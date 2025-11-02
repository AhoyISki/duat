//! The [`Window`]s of Duat
//!
//! In Duat, there is a [`Windows`] struct (accessible via
//! [`context::windows`]), which holds [`Window`]s. Each `Window`
//! represents a set of [`Widget`]s that should be displayed at the
//! same time, like a browser tab.
//!
//! These [`Widget`]s will be contained in [`Node`]s, which are an
//! anonymized version of [`Handle`]s, which the end user interacts
//! with through the [`Pass`] struct, allowing for massively
//! desynchronized global state accessibility.
use std::{
    any::type_name,
    sync::{Arc, Mutex},
};

use super::{Node, Widget, layout::Layout};
use crate::{
    buffer::{Buffer, PathKind},
    context::{self, Cache, Handle},
    data::{Pass, RwData},
    hook::{self, BufferClosed, WidgetCreated, WindowCreated},
    mode,
    opts::PrintOpts,
    text::{SpawnId, Text, txt},
    ui::{PushSpecs, RwArea, SpawnSpecs, Ui},
};

/// A list of all [`Window`]s in Duat
pub struct Windows {
    inner: RwData<InnerWindows>,
    spawns_to_remove: Mutex<Vec<SpawnId>>,
    ui: Ui,
}

/// Inner holder of [`Window`]s
struct InnerWindows {
    layout: Box<Mutex<dyn Layout>>,
    list: Vec<Window>,
    new_additions: Arc<Mutex<Option<Vec<(usize, Node)>>>>,
    cur_buffer: RwData<Handle>,
    cur_widget: RwData<Node>,
    cur_win: usize,
    buffer_history: BufferHistory,
}

impl Windows {
    /// Initializes the `Windows`, returning a [`Node`] for the first
    /// [`Buffer`]
    pub(crate) fn initialize(
        pa: &mut Pass,
        buffer: Buffer,
        layout: Box<Mutex<dyn Layout>>,
        ui: Ui,
    ) {
        let new_additions = Arc::new(Mutex::default());
        let (window, node) = Window::new(0, pa, ui, buffer, new_additions.clone());

        context::set_windows(Self {
            inner: RwData::new(InnerWindows {
                layout,
                list: vec![window.clone()],
                new_additions,
                cur_buffer: RwData::new(node.try_downcast().unwrap()),
                cur_widget: RwData::new(node.clone()),
                cur_win: 0,
                buffer_history: BufferHistory::default(),
            }),
            spawns_to_remove: Mutex::new(Vec::new()),
            ui,
        });

        hook::trigger(
            pa,
            WidgetCreated(node.handle().try_downcast::<Buffer>().unwrap()),
        );
        hook::trigger(pa, WindowCreated(window));
    }

    ////////// Functions for new Widgets

    /// Creates a new list of [`Window`]s, with a main one
    /// initialiazed
    pub(crate) fn new_window(&self, pa: &mut Pass, buffer: Buffer) -> Node {
        let win = self.inner.read(pa).list.len();
        let new_additions = self.inner.read(pa).new_additions.clone();
        let (window, node) = Window::new(win, pa, self.ui, buffer, new_additions);

        let inner = self.inner.write(pa);
        inner.list.push(window);

        hook::trigger(
            pa,
            WidgetCreated(node.handle().try_downcast::<Buffer>().unwrap()),
        );
        hook::trigger(pa, WindowCreated(self.inner.read(pa).list[win].clone()));

        node
    }

    /// Push a [`Widget`] to [`Handle`]
    pub(crate) fn push_widget<W: Widget>(
        &self,
        pa: &mut Pass,
        (target, on_buffers, specs): (&RwArea, Option<bool>, PushSpecs),
        widget: W,
        master: Option<&RwArea>,
    ) -> Option<Handle<W>> {
        self.push(pa, (target, on_buffers, specs), widget, master)?
            .handle()
            .try_downcast()
    }

    /// Spawn a [`Widget`] on a [`Handle`]
    ///
    /// Can fail if the `Handle` in question was already removed.
    pub(crate) fn spawn_on_widget<W: Widget>(
        &self,
        pa: &mut Pass,
        (target, specs): (&RwArea, SpawnSpecs),
        widget: W,
    ) -> Option<Handle<W>> {
        let (win, cluster_master, master) =
            self.inner
                .read(pa)
                .list
                .iter()
                .enumerate()
                .find_map(|(win, window)| {
                    let inner = window.0.read(pa);
                    let master = window.nodes(pa).find_map(|node| {
                        node.area()
                            .area_is_eq(pa, target)
                            .then(|| node.handle().clone())
                    });

                    if inner.master_area.is_master_of(pa, target) {
                        Some((win, None, master))
                    } else if let Some((_, node)) = inner
                        .spawned
                        .iter()
                        .find(|(_, node)| node.area().is_master_of(pa, target))
                    {
                        Some((win, node.area().get_cluster_master(pa), master))
                    } else {
                        None
                    }
                })?;

        let widget = RwData::new(widget);
        let id = SpawnId::new();

        let path = widget
            .read_as::<Buffer>(pa)
            .and_then(|buffer| buffer.path_set());
        let spawned = cluster_master.as_ref().unwrap_or(target).spawn(
            pa,
            path.as_ref().map(|p| p.as_ref()),
            id,
            specs,
        )?;

        let node = Node::new(widget, spawned, master);

        let window = self.inner.write(pa).list.remove(win);
        window.add(pa, node.clone(), None, Location::Spawned(id));
        self.inner.write(pa).list.insert(win, window);

        hook::trigger(
            pa,
            WidgetCreated(node.handle().try_downcast::<W>().unwrap()),
        );

        node.handle().try_downcast()
    }

    /// Spawns a [`Widget`]
    pub(crate) fn spawn_on_text<W: Widget>(
        &self,
        pa: &mut Pass,
        (id, specs): (SpawnId, SpawnSpecs),
        widget: W,
        win: usize,
        master: Handle<dyn Widget>,
    ) -> Handle<W> {
        let widget = RwData::new(widget);
        let path = widget
            .read_as::<Buffer>(pa)
            .and_then(|buffer| buffer.path_set());
        let spawned = self
            .ui
            .new_spawned(path.as_ref().map(|p| p.as_ref()), id, specs, win);

        let node = Node::new(widget, spawned, Some(master));

        let window = self.inner.write(pa).list.remove(win);
        window.add(pa, node.clone(), None, Location::Spawned(id));
        self.inner.write(pa).list.insert(win, window);

        hook::trigger(
            pa,
            WidgetCreated(node.handle().try_downcast::<W>().unwrap()),
        );

        node.handle().try_downcast().unwrap()
    }

    /// Pushes a [`Buffer`] to the buffer's parent
    ///
    /// This function will push to the edge of `self.buffers_parent`
    /// This is an area, usually in the center, that contains all
    /// [`Buffer`]s, and their associated [`Widget`]s,
    /// with others being at the perifery of this area.
    pub(crate) fn new_buffer(&self, pa: &mut Pass, buffer: Buffer) -> Node {
        let inner = self.inner.read(pa);
        let (handle, specs) = inner.layout.lock().unwrap().new_buffer(pa, &inner.list);

        let specs = PushSpecs { cluster: false, ..specs };

        if let Some(master) = handle.area().get_cluster_master(pa) {
            self.push(pa, (&master, Some(true), specs), buffer, None)
                .unwrap()
        } else {
            self.push(pa, (&handle.area, Some(true), specs), buffer, None)
                .unwrap()
        }
    }

    /// Pushes a [`Widget`] to the [`Window`]s
    ///
    /// May return [`None`] if the [`Area`] was already deleted.
    fn push<W: Widget>(
        &self,
        pa: &mut Pass,
        (target, on_buffers, mut specs): (&RwArea, Option<bool>, PushSpecs),
        widget: W,
        master: Option<&RwArea>,
    ) -> Option<Node> {
        let inner = self.inner.read(pa);
        let win = inner
            .list
            .iter()
            .position(|window| {
                window.0.read(pa).master_area.is_master_of(pa, target)
                    || window
                        .nodes(pa)
                        .any(|node| node.area().is_master_of(pa, target))
            })
            .unwrap();

        let inner_window = inner.list[win].0.read(pa);
        let target_is_on_buffers = inner_window.buffers_area.is_master_of(pa, target);

        let on_buffers = on_buffers.unwrap_or(target_is_on_buffers) && target_is_on_buffers;

        if target_is_on_buffers && !on_buffers {
            specs.cluster = false;
        }

        let location = if on_buffers {
            Location::OnBuffers
        } else if let Some((id, _)) = inner_window
            .spawned
            .iter()
            .find(|(_, node)| node.area().area_is_eq(pa, target))
        {
            Location::Spawned(*id)
        } else {
            Location::Regular
        };

        let widget = RwData::new(widget);
        let path = widget
            .read_as::<Buffer>(pa)
            .and_then(|buffer| buffer.path_set());
        let (pushed, parent) =
            target.push(pa, path.as_ref().map(|p| p.as_ref()), specs, on_buffers)?;

        let master = master.and_then(|area| {
            self.entries(pa).find_map(|(.., node)| {
                node.area()
                    .area_is_eq(pa, area)
                    .then(|| node.handle().clone())
            })
        });

        let node = Node::new(widget, pushed, master);

        let window = self.inner.write(pa).list.remove(win);
        window.add(pa, node.clone(), parent, location);
        self.inner.write(pa).list.insert(win, window);

        hook::trigger(
            pa,
            WidgetCreated(node.handle().try_downcast::<W>().unwrap()),
        );

        Some(node)
    }

    ////////// Existing Widget manipulation

    /// Closes a [`Handle`], removing it from the ui
    pub(crate) fn close<W: Widget + ?Sized, S>(
        &self,
        pa: &mut Pass,
        handle: &Handle<W, S>,
    ) -> Result<(), Text> {
        let win = self.handle_window(pa, handle)?;

        // If it's a Buffer, swap all buffers ahead, so this one becomes the
        // last.
        if let Some(buf_handle) = handle.try_downcast::<Buffer>() {
            hook::trigger(pa, BufferClosed((buf_handle.clone(), Cache::new())));

            let buffers_ahead: Vec<Node> = self.inner.read(pa).list[win]
                .nodes(pa)
                .filter(|node| {
                    node.handle().read_as::<Buffer>(pa).is_some_and(|buffer| {
                        buffer.layout_order > buf_handle.read(pa).layout_order
                    })
                })
                .cloned()
                .collect();

            for buffer_ahead in buffers_ahead {
                self.swap(pa, handle, buffer_ahead.handle())?;
            }
        }

        // Actually removing the Handle.
        let mut list = std::mem::take(&mut self.inner.write(pa).list);

        if list[win].close(pa, handle) {
            list.remove(win);
            self.ui.remove_window(win);
            let cur_win = context::current_win_index(pa);
            if cur_win > win {
                self.inner.write(pa).cur_win -= 1;
            }
        }

        let inner = self.inner.write(pa);
        inner.list = list;
        inner.new_additions.lock().unwrap().get_or_insert_default();

        // If this is the active Handle, pick another one to make active.
        let inner = self.inner.read(pa);
        if handle == inner.cur_widget.read(pa).handle() || handle == inner.cur_buffer.read(pa) {
            if let Some(handle) = handle.try_downcast::<Buffer>() {
                self.inner.write(pa).buffer_history.remove(&handle);

                let entry = self
                    .inner
                    .write(pa)
                    .buffer_history
                    .prev()
                    .cloned()
                    .or_else(|| self.buffers(pa).next())
                    .and_then(|handle| {
                        self.entries(pa).find_map(|(win, node)| {
                            (*node.handle() == handle).then(|| (win, node.clone()))
                        })
                    });

                if let Some((_, node)) = entry {
                    crate::mode::reset_to(node.handle().clone());
                } else {
                    // If there is no previous Buffer, just quit.
                    context::sender()
                        .send(crate::session::DuatEvent::Quit)
                        .unwrap();
                    return Ok(());
                }
            } else {
                crate::mode::reset_to(inner.cur_buffer.read(pa).to_dyn());
            }
        }

        Ok(())
    }

    /// Swaps two [`Handle`]'s positions
    pub(crate) fn swap<W1: Widget + ?Sized, S1, W2: Widget + ?Sized, S2>(
        &self,
        pa: &mut Pass,
        lhs: &Handle<W1, S1>,
        rhs: &Handle<W2, S2>,
    ) -> Result<(), Text> {
        let lhs_win = self.handle_window(pa, lhs)?;
        let rhs_win = self.handle_window(pa, rhs)?;

        let [lhs_buffer, rhs_buffer] = [lhs.try_downcast::<Buffer>(), rhs.try_downcast()];

        if let [Some(lhs), Some(rhs)] = [lhs_buffer, rhs_buffer] {
            let lhs_lo = lhs.read(pa).layout_order;
            let rhs_lo = std::mem::replace(&mut rhs.write(pa).layout_order, lhs_lo);
            lhs.write(pa).layout_order = rhs_lo
        }

        let windows = std::mem::take(&mut self.inner.write(pa).list);

        let lhs_nodes = windows[lhs_win].take_with_related_nodes(pa, lhs);
        windows[rhs_win].insert_nodes(pa, lhs_nodes);

        let rhs_nodes = windows[rhs_win].take_with_related_nodes(pa, rhs);
        windows[lhs_win].insert_nodes(pa, rhs_nodes);

        let wins = self.inner.write(pa);
        wins.list = windows;
        wins.new_additions.lock().unwrap().get_or_insert_default();

        lhs.area().swap(pa, rhs.area());

        let cur_buffer = context::current_buffer(pa);
        if lhs_win != rhs_win {
            if lhs == cur_buffer {
                self.inner.write(pa).cur_win = lhs_win;
                self.ui.switch_window(lhs_win);
            } else if rhs == cur_buffer {
                self.inner.write(pa).cur_win = rhs_win;
                self.ui.switch_window(rhs_win);
            }
        }

        Ok(())
    }

    /// Opens a new [`Buffer`] on a new [`Window`], or moves it there,
    /// if it is already open
    pub(crate) fn open_or_move_to_new_window(
        &self,
        pa: &mut Pass,
        pk: PathKind,
        default_buffer_cfg: PrintOpts,
    ) -> Node {
        let node = match self.buffer_entry(pa, pk.clone()) {
            Ok((win, handle)) if self.get(pa, win).unwrap().buffers(pa).len() > 1 => {
                // Take the nodes in the original Window
                handle.write(pa).layout_order = 0;

                let nodes = {
                    let old_window = self.inner.write(pa).list.remove(win);
                    let nodes = old_window.take_with_related_nodes(pa, &handle.to_dyn());
                    self.inner.write(pa).list.insert(win, old_window);

                    nodes
                };

                // Create a new Window Swapping the new root with buffers_area
                let path = handle.read(pa).path_set();
                let new_root = self.ui.new_root(path.as_ref().map(|p| p.as_ref()));
                handle.area().swap(pa, &new_root);
                let window = Window::new_from_raw(
                    pa,
                    win,
                    handle.area.clone(),
                    nodes,
                    self.inner.read(pa).new_additions.clone(),
                );

                self.inner.write(pa).list.push(window.clone());

                hook::trigger(pa, WindowCreated(window));

                // Swap the Buffers ahead of the swapped new_root
                let lo = handle.read(pa).layout_order;

                for handle in &self.inner.read(pa).list[win].buffers(pa)[lo..] {
                    new_root.swap(pa, handle.area());
                }

                // Delete the new_root, which should be the last "Buffer" in the
                // list of the original Window.
                new_root.delete(pa);

                self.inner
                    .write(pa)
                    .new_additions
                    .lock()
                    .unwrap()
                    .get_or_insert_default();

                Node::from_handle(handle)
            }
            // The Handle in question is already in its own window, so no need
            // to move it to another one.
            Ok((.., handle)) => Node::from_handle(handle),
            Err(_) => self.new_window(pa, Buffer::new(pk.as_path(), default_buffer_cfg)),
        };

        if context::current_buffer(pa).read(pa).path_kind() != pk {
            mode::reset_to(node.handle().clone());
        }

        let new_win = self.len(pa) - 1;
        self.inner.write(pa).cur_win = new_win;
        self.ui.switch_window(new_win);

        node
    }

    /// Sets the current active [`Handle`]
    pub(crate) fn set_current_node(&self, pa: &mut Pass, node: Node) -> Result<(), Text> {
        // SAFETY: This Pass is only used when I'm already reborrowing a &mut
        // Pass, and it is known that it only writes to other types.
        let internal_pass = &mut unsafe { Pass::new() };

        let win = self.handle_window(pa, node.handle())?;
        let inner = self.inner.write(pa);

        if let Some(handle) = node.try_downcast::<Buffer>() {
            *inner.cur_buffer.write(internal_pass) = handle;
        }
        *inner.cur_widget.write(internal_pass) = node.clone();
        inner.cur_win = win;
        self.ui.switch_window(win);

        Ok(())
    }

    ////////// Spawned Widget cleanup

    /// Adds a [`SpawnId`] to be removed when a [`Pass`] is available
    pub(crate) fn queue_close_spawned(&self, id: SpawnId) {
        let mut spawns_to_remove = self.spawns_to_remove.lock().unwrap();
        if !spawns_to_remove.contains(&id) {
            spawns_to_remove.push(id)
        }
    }

    /// Removes all [`SpawnId`]'s [`Widget`]s which were queued for
    /// closure
    pub(crate) fn cleanup_despawned(&self, pa: &mut Pass) {
        let spawns_to_remove = std::mem::take(&mut *self.spawns_to_remove.lock().unwrap());
        for id in spawns_to_remove {
            if let Some((_, node)) = self
                .iter(pa)
                .flat_map(|window| &window.0.read(pa).spawned)
                .find(|(other, _)| *other == id)
            {
                self.close(pa, &node.handle().clone()).unwrap();
            }
        }
    }

    ////////// Entry lookup

    /// An entry for a [`Handle`]
    pub(crate) fn handle_window<W: Widget + ?Sized, S>(
        &self,
        pa: &Pass,
        handle: &Handle<W, S>,
    ) -> Result<usize, Text> {
        self.entries(pa)
            .find_map(|(win, node)| (node.handle() == handle).then_some(win))
            .ok_or_else(|| txt!("The Handle was already closed").build())
    }

    /// An entry for a buffer with the given name
    pub(crate) fn buffer_entry(&self, pa: &Pass, pk: PathKind) -> Result<(usize, Handle), Text> {
        self.entries(pa)
            .find_map(|(win, node)| {
                (node.read_as(pa).filter(|f: &&Buffer| f.path_kind() == pk))
                    .and_then(|_| node.try_downcast().map(|handle| (win, handle)))
            })
            .ok_or_else(|| txt!("Buffer {pk} not found").build())
    }

    /// An entry for a buffer with the given name
    pub(crate) fn named_buffer_entry(
        &self,
        pa: &Pass,
        name: &str,
    ) -> Result<(usize, Handle), Text> {
        self.entries(pa)
            .find_map(|(win, node)| {
                (node.read_as(pa).filter(|f: &&Buffer| f.name() == name))
                    .and_then(|_| node.try_downcast().map(|handle| (win, handle)))
            })
            .ok_or_else(|| txt!("Buffer [buffer]{name}[] not found").build())
    }

    /// An entry for a widget of a specific type
    ///
    /// Returns the index of the window, the index of the [`Widget`],
    /// and the [`Widget`]'s [`Node`]
    pub(crate) fn node_of<'a, W: Widget>(&'a self, pa: &'a Pass) -> Result<&'a Node, Text> {
        let handle = context::current_buffer(pa);

        if let Some((handle, _)) = handle.get_related::<W>(pa).next() {
            self.entries(pa)
                .find_map(|(.., node)| node.ptr_eq(handle.widget()).then_some(node))
        } else {
            let cur_win = self.inner.read(pa).cur_win;
            let list = &self.inner.read(pa).list;
            list[cur_win]
                .nodes(pa)
                .chain(list[cur_win + 1..].iter().flat_map(|win| win.nodes(pa)))
                .chain(list[..cur_win].iter().flat_map(|win| win.nodes(pa)))
                .find(|node| node.data_is::<W>())
        }
        .ok_or(txt!("No widget of type [a]{}[] found", type_name::<W>()).build())
    }

    ////////// Entry iterators

    /// Iterates over all widget entries, with window indices
    pub(crate) fn entries<'a>(&'a self, pa: &'a Pass) -> impl Iterator<Item = (usize, &'a Node)> {
        self.inner
            .read(pa)
            .list
            .iter()
            .enumerate()
            .flat_map(|(win, window)| {
                let inner = window.0.read(pa);
                inner
                    .nodes
                    .iter()
                    .chain(inner.spawned.iter().map(|(_, node)| node))
                    .map(move |node| (win, node))
            })
    }

    /// Iterates around a specific widget, going forwards
    pub(crate) fn iter_around<'a>(
        &'a self,
        pa: &'a Pass,
        win: usize,
        wid: usize,
    ) -> impl Iterator<Item = (usize, &'a Node)> + 'a {
        let window_entries =
            |(w, window): (usize, &'a Window)| window.nodes(pa).map(move |node| (w, node));

        let windows = &self.inner.read(pa).list;

        let prev_len: usize = windows
            .iter()
            .take(win)
            .map(|win| win.len_widgets(pa))
            .sum();

        windows
            .iter()
            .enumerate()
            .skip(win)
            .flat_map(window_entries)
            .skip(wid + 1)
            .chain(
                windows
                    .iter()
                    .enumerate()
                    .take(win + 1)
                    .flat_map(window_entries)
                    .take(prev_len + wid),
            )
    }

    /// Iterates around a specific widget, going backwards
    pub(crate) fn iter_around_rev<'a>(
        &'a self,
        pa: &'a Pass,
        win: usize,
        wid: usize,
    ) -> impl Iterator<Item = (usize, &'a Node)> + 'a {
        let entries =
            |(w, window): (usize, &'a Window)| window.nodes(pa).map(move |node| (w, node));

        let windows = &self.inner.read(pa).list;

        let next_len: usize = windows
            .iter()
            .skip(win)
            .map(|win| win.len_widgets(pa))
            .sum();

        windows
            .iter()
            .enumerate()
            .rev()
            .skip(windows.len() - (win + 1))
            .flat_map(move |(w, win)| entries((w, win)).rev().skip(win.len_widgets(pa) - wid))
            .chain(
                windows
                    .iter()
                    .enumerate()
                    .rev()
                    .take(windows.len() - win)
                    .flat_map(move |(i, win)| entries((i, win)).rev())
                    .take(next_len - (wid + 1)),
            )
    }

    ////////// Querying functions

    /// The number of open [`Window`]s
    ///
    /// Should never be 0, as that is not a valid state of affairs.
    pub fn len(&self, pa: &Pass) -> usize {
        self.inner.read(pa).list.len()
    }

    /// get's the `win`th [`Window`]
    pub fn get<'a>(&'a self, pa: &'a Pass, win: usize) -> Option<&'a Window> {
        self.inner.read(pa).list.get(win)
    }

    /// Iterates through every [`Window`]
    pub fn iter<'a>(&'a self, pa: &'a Pass) -> std::slice::Iter<'a, Window> {
        self.inner.read(pa).list.iter()
    }

    /// Returns an [`Iterator`] over the [`Handle`]s of Duat
    pub fn handles<'a>(&'a self, pa: &'a Pass) -> impl Iterator<Item = &'a Handle<dyn Widget>> {
        self.inner
            .read(pa)
            .list
            .iter()
            .flat_map(|w| w.nodes(pa).map(|n| n.handle()))
    }

    /// Iterates over all [`Handle<Buffer>`]s in Duat
    pub fn buffers<'a>(&'a self, pa: &'a Pass) -> impl Iterator<Item = Handle> + 'a {
        self.inner.read(pa).list.iter().flat_map(|w| w.buffers(pa))
    }

    /// The index of the currently active [`Window`]
    pub fn current_window(&self, pa: &Pass) -> usize {
        self.inner.read(pa).cur_win
    }

    /// The [`RwData`] that points to the currently active [`Buffer`]
    pub(crate) fn current_buffer<'a>(&'a self, pa: &'a Pass) -> &'a RwData<Handle> {
        &self.inner.read(pa).cur_buffer
    }

    /// The [`RwData`] that points to the currently active [`Widget`]
    pub(crate) fn current_widget<'a>(&'a self, pa: &'a Pass) -> &'a RwData<Node> {
        &self.inner.read(pa).cur_widget
    }

    /// Gets the new additions to the [`Windows`]
    pub(crate) fn get_additions(&self, pa: &mut Pass) -> Option<Vec<(usize, Node)>> {
        self.inner.write(pa).new_additions.lock().unwrap().take()
    }
}

/// A region containing [`Handle`]s
///
/// This is like a browser tab, it can contain any number of regular
/// and spawned [`Handle`]s.
#[derive(Clone)]
pub struct Window(RwData<InnerWindow>);

struct InnerWindow {
    index: usize,
    nodes: Vec<Node>,
    spawned: Vec<(SpawnId, Node)>,
    buffers_area: RwArea,
    master_area: RwArea,
    new_additions: Arc<Mutex<Option<Vec<(usize, Node)>>>>,
}

impl Window {
    /// Returns a new instance of [`Window`]
    fn new<W: Widget>(
        index: usize,
        pa: &mut Pass,
        ui: Ui,
        widget: W,
        new_additions: Arc<Mutex<Option<Vec<(usize, Node)>>>>,
    ) -> (Self, Node) {
        let widget = RwData::new(widget);
        let path = if let Some(buffer) = widget.write_as::<Buffer>(pa) {
            buffer.layout_order = get_layout_order();
            buffer.path_set()
        } else {
            None
        };

        let area = ui.new_root(path.as_ref().map(|p| p.as_ref()));
        let node = Node::new::<W>(widget, area.clone(), None);

        new_additions
            .lock()
            .unwrap()
            .get_or_insert_default()
            .push((index, node.clone()));

        let window = Self(RwData::new(InnerWindow {
            index,
            nodes: vec![node.clone()],
            spawned: Vec::new(),
            buffers_area: area.clone(),
            master_area: area.clone(),
            new_additions,
        }));

        (window, node)
    }

    /// Returns a new [`Window`] from raw elements
    pub(crate) fn new_from_raw(
        pa: &mut Pass,
        index: usize,
        master_area: RwArea,
        nodes: Vec<Node>,
        new_additions: Arc<Mutex<Option<Vec<(usize, Node)>>>>,
    ) -> Self {
        let master_area = master_area
            .get_cluster_master(pa)
            .unwrap_or(master_area.clone());

        Self(RwData::new(InnerWindow {
            index,
            nodes,
            spawned: Vec::new(),
            buffers_area: master_area.clone(),
            master_area,
            new_additions,
        }))
    }

    ////////// Widget addition/removal

    /// Pushes a widget to the "buffer area" of a `Window`
    ///
    /// If this [`Widget`] is being pushed to a [`Buffer`]'s group,
    /// then this `Widget` will be included in that `Buffer`'s
    /// group. This means that, if that `Buffer` is moved around
    /// or deleted, this `Widget` (and all others in its group)
    /// will follow suit.
    ///
    /// When you push a `Widget`, it is placed on an edge of the
    /// area, and a new parent area may be created to hold both
    /// widgets. If created, that new area will be used for pushing
    /// widgets in the future.
    ///
    /// This means that, if you push widget *A* to the left, then you
    /// push widget *B* to the bottom, you will get this layout:
    ///
    /// ```text
    /// ╭───┬──────────╮
    /// │   │          │
    /// │ A │  Buffer  │
    /// │   │          │
    /// ├───┴──────────┤
    /// │      B       │
    /// ╰──────────────╯
    /// ```
    ///
    /// Here's an example of such a layout:
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// setup_duat!(setup);
    /// use duat::prelude::*;
    ///
    /// fn setup() {
    ///     hook::remove("BufferWidgets");
    ///     hook::add::<Buffer>(|pa, handle| {
    ///         LineNumbers::builder().push_on(pa, handle);
    ///         status!("{name_txt} {selections_txt} {main_txt}").push_on(pa, handle);
    ///         Ok(())
    ///     });
    /// }
    /// ```
    ///
    /// In this case, each buffer will have [`LineNumbers`] with
    /// relative/absolute numbering, and a [`StatusLine`] showing
    /// the buffer's name, how many selections are in it, and its main
    /// selection.
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    /// [`LineNumbers`]: https://docs.rs/duat/latest/duat/widgets/struct.LineNumbers.html
    /// [`StatusLine`]: https://docs.rs/duat/latest/duat/widgets/struct.StatusLine.html
    /// [`WindowCreated`]: crate::hook::WindowCreated
    pub fn push_inner<W: Widget>(
        &self,
        pa: &mut Pass,
        widget: W,
        mut specs: PushSpecs,
    ) -> Handle<W> {
        let target = self.0.read(pa).buffers_area.clone();

        specs.cluster = false;

        context::windows()
            .push_widget(pa, (&target, Some(false), specs), widget, None)
            .unwrap()
    }

    /// Docs: TODO
    pub fn push_outer<W: Widget>(
        &self,
        pa: &mut Pass,
        widget: W,
        mut specs: PushSpecs,
    ) -> Handle<W> {
        let target = self.0.read(pa).master_area.clone();

        specs.cluster = false;

        context::windows()
            .push_widget(pa, (&target, Some(false), specs), widget, None)
            .unwrap()
    }

    /// Adds a [`Widget`] to the list of widgets of this [`Window`]
    fn add(&self, pa: &mut Pass, node: Node, parent: Option<RwArea>, location: Location) {
        match location {
            Location::OnBuffers => {
                self.0.write(pa).nodes.push(node.clone());
                if let Some(parent) = &parent
                    && parent.is_master_of(pa, &self.0.read(pa).buffers_area)
                {
                    self.0.write(pa).buffers_area = parent.clone()
                }
            }
            Location::Regular => self.0.write(pa).nodes.push(node.clone()),
            Location::Spawned(id) => self.0.write(pa).spawned.push((id, node.clone())),
        }

        if let Some(parent) = &parent
            && parent.is_master_of(pa, &self.0.read(pa).master_area)
        {
            self.0.write(pa).master_area = parent.clone()
        }

        let inner = self.0.write(pa);
        inner
            .new_additions
            .lock()
            .unwrap()
            .get_or_insert_default()
            .push((inner.index, node.clone()));
    }

    /// Closes the [`Handle`] and all related ones
    ///
    /// Returns `true` if this `Window` is supposed to be removed.
    fn close<W: Widget + ?Sized, S>(&self, pa: &mut Pass, handle: &Handle<W, S>) -> bool {
        let handle_eq = |node: &mut Node| node.handle() == handle;

        let inner = self.0.write(pa);

        let node = if let Some(node) = inner.nodes.extract_if(.., handle_eq).next() {
            node
        } else if let Some((_, node)) = inner.spawned.extract_if(.., |(_, n)| handle_eq(n)).next() {
            node
        } else {
            unreachable!("This isn't supposed to fail");
        };

        node.handle().declare_closed(pa);

        let (do_rm_window, rm_areas) = node.area().delete(pa);
        if do_rm_window {
            for handle in self.handles(pa).cloned().collect::<Vec<_>>() {
                handle.declare_closed(pa);
            }
            return true;
        }

        let (mut nodes, mut spawned) = {
            let inner = self.0.write(pa);
            let nodes = std::mem::take(&mut inner.nodes);
            let spawned = std::mem::take(&mut inner.spawned);
            (nodes, spawned)
        };

        nodes.retain(|node| {
            if rm_areas
                .iter()
                .any(|a| a.area_is_eq(pa, node.handle().area()))
            {
                node.handle().declare_closed(pa);
                false
            } else {
                true
            }
        });
        spawned.retain(|(_, node)| {
            if rm_areas
                .iter()
                .any(|a| a.area_is_eq(pa, node.handle().area()))
            {
                node.handle().declare_closed(pa);
                false
            } else {
                true
            }
        });

        let inner = self.0.write(pa);
        inner.nodes = nodes;
        inner.spawned = spawned;

        let buffers = self.buffers(pa);
        if buffers.len() == 1 {
            let handle = buffers.first().unwrap();

            let master_area = handle
                .area()
                .get_cluster_master(pa)
                .unwrap_or(handle.area.clone());

            self.0.write(pa).buffers_area = master_area;
        }

        if self.buffers(pa).is_empty() {
            for handle in self.handles(pa).cloned().collect::<Vec<_>>() {
                handle.declare_closed(pa);
            }
            true
        } else {
            false
        }
    }

    /// Takes all [`Node`]s related to a given [`Node`]
    fn take_with_related_nodes<W: Widget + ?Sized, S>(
        &self,
        pa: &mut Pass,
        handle: &Handle<W, S>,
    ) -> Vec<Node> {
        let related = handle.related();

        let (related, inner) = pa.read_and_write(related, &self.0);

        inner
            .nodes
            .extract_if(.., |node| {
                related.iter().any(|(handle, _)| handle == node.handle()) || node.handle() == handle
            })
            .collect()
    }

    /// Inserts [`Buffer`] nodes orderly
    fn insert_nodes(&self, pa: &mut Pass, nodes: Vec<Node>) {
        self.0.write(pa).nodes.extend(nodes);
    }

    ////////// Querying functions

    /// An [`Iterator`] over the [`Node`]s in a [`Window`]
    #[define_opaque(InnerIter)]
    pub(crate) fn nodes<'a>(
        &'a self,
        pa: &'a Pass,
    ) -> impl ExactSizeIterator<Item = &'a Node> + DoubleEndedIterator {
        struct InnerChain<'a>(InnerIter<'a>, usize);

        impl<'a> Iterator for InnerChain<'a> {
            type Item = &'a Node;

            fn next(&mut self) -> Option<Self::Item> {
                self.0.next()
            }
        }

        impl<'a> DoubleEndedIterator for InnerChain<'a> {
            fn next_back(&mut self) -> Option<Self::Item> {
                self.0.next_back()
            }
        }

        impl<'a> ExactSizeIterator for InnerChain<'a> {
            fn len(&self) -> usize {
                self.1
            }
        }

        let inner = self.0.read(pa);
        InnerChain(
            inner
                .nodes
                .iter()
                .chain(inner.spawned.iter().map(|(_, node)| node)),
            inner.nodes.len() + inner.spawned.len(),
        )
    }

    /// An [`Iterator`] over all [`Handle`]s in a `Window`
    ///
    /// Each `Handle` takes care of one [`Widget`], if you want to see
    /// which one exactly is being used, you can use the
    /// [`Handle::try_downcast`] and [`Handle::read_as`] methods.
    ///
    /// If you just want an iterator over the [`Buffer`]s, then check
    /// out [`Window::buffers`].
    pub fn handles<'a>(&'a self, pa: &'a Pass) -> impl Iterator<Item = &'a Handle<dyn Widget>> {
        self.nodes(pa).map(|node| node.handle())
    }

    /// The [`Buffer`]s in a single `Window`
    ///
    /// They will be ordered by where the `"next-buffer"` [command]
    /// focus on. This usually follows the order by which they were
    /// created, but it can be altered if you use the `"swap"`
    /// command.
    ///
    /// If you want an [`Iterator`] over all [`Handle`]s, see
    /// [`Window::handles`].
    ///
    /// [command]: crate::cmd
    pub fn buffers(&self, pa: &Pass) -> Vec<Handle> {
        let inner = self.0.read(pa);
        let mut buffers: Vec<Handle> = inner
            .nodes
            .iter()
            .filter_map(|node| node.try_downcast())
            .collect();

        buffers.sort_unstable_by_key(|buffer| buffer.read(pa).layout_order);

        buffers.extend(
            inner
                .spawned
                .iter()
                .filter_map(|(_, node)| node.try_downcast()),
        );

        buffers
    }

    /// How many [`Widget`]s are in this [`Window`]
    pub(crate) fn len_widgets(&self, pa: &Pass) -> usize {
        let inner = self.0.read(pa);
        inner.nodes.len() + inner.spawned.len()
    }
}

#[derive(Default)]
struct BufferHistory {
    current_i: usize,
    list: Vec<Handle>,
    last_was_fwd: bool,
}

impl BufferHistory {
    /// Returns the previous [`Handle`], if there is one
    fn prev(&mut self) -> Option<&Handle> {
        self.current_i.checked_sub(1).and_then(|prev_i| {
            self.last_was_fwd = true;
            self.current_i = prev_i;
            self.list.get(prev_i)
        })
    }

    /// Returns the next [`Handle`], if there is one
    fn next(&mut self) -> Option<&Handle> {
        self.list.get(self.current_i + 1).inspect(|next| {
            self.last_was_fwd = false;
            self.current_i += 1;
        })
    }

    /// Returns the last [`Handle`]
    ///
    /// Repeatedly calling this function will return the same two
    /// [`Handle`]s.
    fn last(&mut self) -> Option<&Handle> {
        if self.last_was_fwd {
            self.next()
        } else {
            self.prev()
        }
    }

    /// Inserts a new entry, but only if it is different from both of
    /// the surrounding entries
    fn insert(&mut self, handle: Handle) {
        if self
            .current_i
            .checked_sub(1)
            .is_none_or(|prev_i| self.list[prev_i] != handle)
            && self
                .list
                .get(self.current_i)
                .is_none_or(|other| *other != handle)
        {
            self.list.insert(self.current_i, handle);
            self.last_was_fwd = false;
            self.current_i += 1;
        }
    }

    /// Clears all instances of a given [`Handle`], signaling that it
    /// has been closed
    fn remove(&mut self, handle: &Handle) {
        let mut i = 0;
        self.list.retain(|other| {
            if other == handle {
                if i < self.current_i {
                    self.current_i -= 1;
                }
                false
            } else {
                i += 1;
                true
            }
        });
    }
}

/// Returns a new layout order, which will be different from every
/// other
fn get_layout_order() -> usize {
    use std::sync::atomic::{AtomicUsize, Ordering};

    static LAYOUT_ORDER: AtomicUsize = AtomicUsize::new(0);
    LAYOUT_ORDER.fetch_add(1, Ordering::Relaxed)
}

enum Location {
    OnBuffers,
    Regular,
    Spawned(SpawnId),
}

type InnerIter<'a> = impl DoubleEndedIterator<Item = &'a Node>;
