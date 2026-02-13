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
    iter::Chain,
    path::Path,
    sync::{Arc, Mutex},
};

use super::{Node, Widget, layout::Layout};
use crate::{
    buffer::{Buffer, BufferOpts, PathKind},
    context::{self, Handle},
    data::{Pass, RwData},
    hook::{self, BufferClosed, BufferSwitched, WidgetOpened, WindowOpened},
    mode,
    session::UiMouseEvent,
    text::{Text, txt},
    ui::{Coord, DynSpawnSpecs, PushSpecs, RwArea, SpawnId, StaticSpawnSpecs, Ui},
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
            WidgetOpened(node.handle().try_downcast::<Buffer>().unwrap()),
        );
        hook::trigger(pa, WindowOpened(window));
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

        hook::trigger(pa, WindowOpened(self.inner.read(pa).list[win].clone()));
        hook::trigger(
            pa,
            WidgetOpened(node.handle().try_downcast::<Buffer>().unwrap()),
        );

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
        (target, specs): (&RwArea, DynSpawnSpecs),
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

        hook::trigger(pa, WidgetOpened(node.handle().try_downcast::<W>().unwrap()));

        node.handle().try_downcast()
    }

    /// Spawns a [`Widget`] on [`Text`]
    pub(crate) fn spawn_on_text<W: Widget>(
        &self,
        pa: &mut Pass,
        (id, specs): (SpawnId, DynSpawnSpecs),
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
            .new_dyn_spawned(path.as_ref().map(|p| p.as_ref()), id, specs, win);

        let node = Node::new(widget, spawned, Some(master));

        let window = self.inner.write(pa).list.remove(win);
        window.add(pa, node.clone(), None, Location::Spawned(id));
        self.inner.write(pa).list.insert(win, window);

        hook::trigger(pa, WidgetOpened(node.handle().try_downcast::<W>().unwrap()));

        node.handle().try_downcast().unwrap()
    }

    fn spawn_static<W: Widget>(
        &self,
        pa: &mut Pass,
        (specs, win): (StaticSpawnSpecs, usize),
        widget: W,
    ) -> Option<Handle<W>> {
        let id = SpawnId::new();
        let widget = RwData::new(widget);
        let path = widget
            .read_as::<Buffer>(pa)
            .and_then(|buffer| buffer.path_set());
        let spawned = self
            .ui
            .new_static_spawned(path.as_ref().map(|p| p.as_ref()), id, specs, win);

        let node = Node::new(widget, spawned, None);

        let window = self.inner.write(pa).list.remove(win);
        window.add(pa, node.clone(), None, Location::Spawned(id));
        self.inner.write(pa).list.insert(win, window);

        hook::trigger(pa, WidgetOpened(node.handle().try_downcast::<W>().unwrap()));

        node.handle().try_downcast()
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

        hook::trigger(pa, WidgetOpened(node.handle().try_downcast::<W>().unwrap()));

        Some(node)
    }

    ////////// Existing Widget manipulation

    /// Closes a [`Handle`], removing it from the ui
    pub(crate) fn close<W: Widget + ?Sized>(
        &self,
        pa: &mut Pass,
        handle: &Handle<W>,
    ) -> Result<(), Text> {
        let win = self.handle_window(pa, handle)?;

        // If it's a Buffer, swap all buffers ahead, so this one becomes the
        // last.
        if let Some(buf_handle) = handle.try_downcast::<Buffer>() {
            hook::trigger(pa, BufferClosed(buf_handle.clone()));

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
                    .jump_by(handle.clone(), -1)
                    .or_else(|| self.buffers(pa).first().cloned())
                    .and_then(|handle| {
                        self.entries(pa).find_map(|(win, node)| {
                            (*node.handle() == handle).then(|| (win, node.clone()))
                        })
                    });

                if let Some((_, node)) = entry {
                    crate::mode::reset_to(pa, node.handle());
                } else {
                    // If there is no previous Buffer, just quit.
                    context::sender().send(crate::session::DuatEvent::Quit);
                    return Ok(());
                }
            } else {
                crate::mode::reset_to(pa, &inner.cur_buffer.read(pa).clone());
            }
        }

        Ok(())
    }

    /// Swaps two [`Handle`]'s positions
    pub(crate) fn swap<W1: Widget + ?Sized, W2: Widget + ?Sized>(
        &self,
        pa: &mut Pass,
        lhs: &Handle<W1>,
        rhs: &Handle<W2>,
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
            if *lhs == cur_buffer {
                self.inner.write(pa).cur_win = lhs_win;
                self.ui.switch_window(lhs_win);
            } else if *rhs == cur_buffer {
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
        default_buffer_opts: BufferOpts,
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

                hook::trigger(pa, WindowOpened(window));

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
            Err(_) => self.new_window(pa, Buffer::new(pk.as_path(), default_buffer_opts)),
        };

        if context::current_buffer(pa).read(pa).path_kind() != pk {
            mode::reset_to(pa, node.handle());
        }

        node
    }

    /// Sets the current active [`Handle`]
    #[track_caller]
    pub(crate) fn set_current_node(&self, pa: &mut Pass, node: Node) -> Result<(), Text> {
        // SAFETY: This Pass is only used when I'm already reborrowing a &mut
        // Pass, and it is known that it only writes to other types.
        let internal_pass = &mut unsafe { Pass::new() };

        let win = self.handle_window(pa, node.handle())?;
        let inner = self.inner.write(pa);

        if let Some(current) = node.try_downcast::<Buffer>() {
            let former = std::mem::replace(inner.cur_buffer.write(internal_pass), current.clone());
            inner.buffer_history.insert(former.clone(), current.clone());

            hook::trigger(pa, BufferSwitched((former, current)));
        }

        let inner = self.inner.write(pa);
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
    pub(crate) fn handle_window<W: Widget + ?Sized>(
        &self,
        pa: &Pass,
        handle: &Handle<W>,
    ) -> Result<usize, Text> {
        self.entries(pa)
            .find_map(|(win, node)| (node.handle() == handle).then_some(win))
            .ok_or_else(|| txt!("The Handle was already closed"))
    }

    /// An entry for a buffer with the given name
    pub(crate) fn buffer_entry(&self, pa: &Pass, pk: PathKind) -> Result<(usize, Handle), Text> {
        self.entries(pa)
            .find_map(|(win, node)| {
                (node.read_as(pa).filter(|f: &&Buffer| f.path_kind() == pk))
                    .and_then(|_| node.try_downcast().map(|handle| (win, handle)))
            })
            .ok_or_else(|| txt!("Buffer {pk} not found"))
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
            .ok_or_else(|| txt!("Buffer [buffer]{name}[] not found"))
    }

    /// An entry for a buffer with the given name
    pub(crate) fn path_buffer_entry(
        &self,
        pa: &Pass,
        path: &Path,
    ) -> Result<(usize, Handle), Text> {
        self.entries(pa)
            .find_map(|(win, node)| {
                (node
                    .read_as(pa)
                    .filter(|f: &&Buffer| f.path_kind().as_path().is_some_and(|p| p == path)))
                .and_then(|_| node.try_downcast().map(|handle| (win, handle)))
            })
            .ok_or_else(|| txt!("Buffer [buffer]{path}[] not found"))
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
        .ok_or(txt!(
            "No widget of type [a]{}[] found",
            std::any::type_name::<W>()
        ))
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

    ////////// Buffer switching

    /// Jumps around in the buffer history
    ///
    /// This will jump forwards if `number` is positive, backwards
    /// otherwise.
    pub fn jump_buffers_by(&self, pa: &mut Pass, jumps: i32) {
        let current = self.inner.read(pa).cur_buffer.read(pa).clone();
        if let Some(handle) = self.inner.write(pa).buffer_history.jump_by(current, jumps) {
            mode::reset_to(pa, &handle);
        } else {
            context::warn!("No buffer [a]{jumps}[] jumps away from the current one");
        }
    }

    /// Jumps to the last buffer
    ///
    /// Calling this repeatedly
    pub fn last_switched_buffer(&self, pa: &mut Pass) -> Result<Handle, Text> {
        let current = self.inner.read(pa).cur_buffer.read(pa).clone();
        if let Some(handle) = self.inner.write(pa).buffer_history.last(current) {
            mode::reset_to(pa, &handle);
            Ok(handle)
        } else {
            Err(txt!("No last buffer"))
        }
    }

    ////////// Querying functions

    /// The bottom right [`Coord`] on the screen
    ///
    /// Since the top left coord is `Coord { x: 0.0, y: 0.0 }`, this
    /// is also the size of the window.
    pub fn size(&self) -> Coord {
        self.ui.size()
    }

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
    pub fn handles<'a>(&'a self, pa: &'a Pass) -> impl Iterator<Item = Handle<dyn Widget>> + 'a {
        self.inner
            .read(pa)
            .list
            .iter()
            .flat_map(|w| w.nodes(pa).map(|n| n.handle().clone()))
    }

    /// Returns an [`Iterator`] over the [`Handle`]s of Duat
    pub fn handles_of<W: Widget>(&self, pa: &Pass) -> Vec<Handle<W>> {
        self.inner
            .read(pa)
            .list
            .iter()
            .flat_map(|w| w.nodes(pa).filter_map(|n| n.handle().try_downcast()))
            .collect()
    }

    /// Iterates over all [`Handle<Buffer>`]s in Duat
    pub fn buffers(&self, pa: &Pass) -> Vec<Handle> {
        self.inner
            .read(pa)
            .list
            .iter()
            .flat_map(|w| w.buffers(pa))
            .collect()
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

    /// Pushes a [`Widget`] around the "buffer area" of a `Window`
    ///
    /// When you push a `Widget`, it is placed on an edge of the
    /// area, and a new parent area may be created to hold both
    /// widgets. If created, that new area will be used for pushing
    /// widgets in the future.
    ///
    /// This means that, if you push widget *B* to the bottom, then
    /// you push widget *A* to the left, you will get this layout:
    ///
    /// ```text
    /// ╭───┬───────────╮
    /// │   │           │
    /// │ A │  Buffers  │
    /// │   │           │
    /// ├───┴───────────┤
    /// │       B       │
    /// ╰───────────────╯
    /// ```
    ///
    /// The `Buffers` region here represents a central area that all
    /// `Window`s contain, where all the [`Buffer`]s are placed
    /// alongside their [satellite `Widget`s]. When you push `Widget`s
    /// to the `Window`, instead of to a [`Handle`], those widgets are
    /// placed in the outer region, not being associated with any
    /// particular `Buffer`.
    ///
    /// In this case, each `Window` will have a [`LogBook`] on the
    /// left side as well as [`FooterWidgets`] on the bottom.
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    /// [`LogBook`]: https://docs.rs/duat/latest/duat/widgets/struct.LogBook.html
    /// [`FooterWidgets`]: https://docs.rs/duat/latest/duat/widgets/struct.FooterWidgets.html
    /// [`WindowOpened`]: crate::hook::WindowOpened
    /// [satellite `Widget`s]: context::Handle::push_outer_widget
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

    /// Pushes a [`Widget`] to the edges of a `Window`
    ///
    /// When you push a `Widget`, it is placed on an edge of the
    /// area, and a new parent area may be created to hold both
    /// widgets. If created, that new area will be used for pushing
    /// widgets in the future.
    ///
    /// This means that, if you push widget *B* to the bottom, then
    /// you push widget *A* to the left, you will get this layout:
    ///
    /// ```text
    /// ╭───┬───────────╮
    /// │   │           │
    /// │   │  Buffers  │
    /// │ A │           │
    /// │   ├───────────┤
    /// │   │     B     │
    /// ╰───┴───────────╯
    /// ```
    ///
    /// The `Buffers` region here represents a central area that all
    /// `Window`s contain, where all the [`Buffer`]s are placed
    /// alongside their [satellite `Widget`s]. When you push `Widget`s
    /// to the `Window`, instead of to a [`Handle`], those widgets are
    /// placed in the outer region, not being associated with any
    /// particular `Buffer`.
    ///
    /// In this case, each `Window` will have a [`LogBook`] on the
    /// left side as well as [`FooterWidgets`] on the bottom.
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    /// [`LogBook`]: https://docs.rs/duat/latest/duat/widgets/struct.LogBook.html
    /// [`FooterWidgets`]: https://docs.rs/duat/latest/duat/widgets/struct.FooterWidgets.html
    /// [`WindowOpened`]: crate::hook::WindowOpened
    /// [satellite `Widget`s]: context::Handle::push_outer_widget
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

    /// Spawns a new static [`Widget`] on this `Window`
    ///
    /// This `Widget`, unlike all other kinds, does not follow changes
    /// in the layout by the resizing or closure of other `Widget`s.
    /// It instead stays in a single [`Coord`], its width and height
    /// being predefined.
    ///
    /// There is one circumstance in which this `Widget` will move,
    /// however: when the window resizes. In this circumstance, the
    /// `Widget` will be relocated as to be placed in relatively the
    /// same place on screen, in relation to the bottom right corner
    /// of the screen.
    ///
    /// [`Coord`]: super::Coord
    pub fn spawn<W: Widget>(&self, pa: &mut Pass, widget: W, specs: StaticSpawnSpecs) -> Handle<W> {
        context::windows()
            .spawn_static(pa, (specs, self.0.read(pa).index), widget)
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
    fn close<W: Widget + ?Sized>(&self, pa: &mut Pass, handle: &Handle<W>) -> bool {
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
    fn take_with_related_nodes<W: Widget + ?Sized>(
        &self,
        pa: &mut Pass,
        handle: &Handle<W>,
    ) -> Vec<Node> {
        let related = handle.related();

        let (related, inner) = pa.write_many((related, &self.0));

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
    pub(crate) fn nodes<'a>(&'a self, pa: &'a Pass) -> Nodes<'a> {
        let inner = self.0.read(pa);

        let spawned = SpawnedNodes { iter: inner.spawned.iter() };

        Nodes {
            iter: inner.nodes.iter().chain(spawned),
            len: inner.nodes.len() + inner.spawned.len(),
            taken: 0,
        }
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

    pub(crate) fn send_mouse_event(&self, pa: &mut Pass, mouse_event: UiMouseEvent) {
        let inner = self.0.read(pa);
        let node = inner
            .spawned
            .iter()
            .rev()
            .map(|(_, node)| node)
            .chain(&inner.nodes)
            .find(|node| {
                let tl = node.handle().area().top_left(pa);
                let br = node.handle().area().bottom_right(pa);
                (tl.x <= mouse_event.coord.x && tl.y <= mouse_event.coord.y)
                    && (mouse_event.coord.x < br.x && mouse_event.coord.y < br.y)
            })
            .cloned();

        if let Some(node) = node {
            node.on_mouse_event(pa, mouse_event);
        }
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
    last: Option<Handle>,
}

impl BufferHistory {
    /// Returns the previous [`Handle`], if there is one
    fn jump_by(&mut self, current: Handle, by: i32) -> Option<Handle> {
        let new_i = self
            .current_i
            .saturating_add_signed(by as isize)
            .min(self.list.len());
        let new_handle = self.list.get(new_i)?.clone();

        self.last = Some(current);
        self.current_i = new_i;
        Some(new_handle)
    }

    /// Returns the last [`Handle`]
    ///
    /// Repeatedly calling this function will return the same two
    /// [`Handle`]s.
    fn last(&mut self, current: Handle) -> Option<Handle> {
        if let Some(last) = self.last.as_mut() {
            Some(std::mem::replace(last, current))
        } else if let Some(last) = self.list.get(self.current_i.checked_sub(1)?) {
            self.last = Some(current);
            Some(last.clone())
        } else {
            None
        }
    }

    /// Inserts a new entry, but only if it is different from both of
    /// the surrounding entries
    fn insert(&mut self, current: Handle, handle: Handle) {
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
            self.last = Some(current);
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

#[derive(Debug, Clone)]
pub(crate) struct Nodes<'a> {
    iter: Chain<std::slice::Iter<'a, Node>, SpawnedNodes<'a>>,
    len: usize,
    taken: usize,
}

impl<'a> Iterator for Nodes<'a> {
    type Item = &'a Node;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.iter.next();
        self.taken += next.is_some() as usize;
        next
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len - self.taken, Some(self.len - self.taken))
    }
}

impl<'a> DoubleEndedIterator for Nodes<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let next = self.iter.next_back();
        self.taken += next.is_some() as usize;
        next
    }
}

impl ExactSizeIterator for Nodes<'_> {}

#[derive(Debug, Clone)]
struct SpawnedNodes<'a> {
    iter: std::slice::Iter<'a, (SpawnId, Node)>,
}

impl<'a> Iterator for SpawnedNodes<'a> {
    type Item = &'a Node;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(_, node)| node)
    }
}

impl DoubleEndedIterator for SpawnedNodes<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.iter.next_back().map(|(_, node)| node)
    }
}
