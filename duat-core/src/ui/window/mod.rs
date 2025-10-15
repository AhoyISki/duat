use std::{
    any::type_name,
    sync::{Arc, Mutex},
};

pub use self::builder::UiBuilder;
use super::{Node, Widget, layout::Layout};
use crate::{
    cfg::PrintCfg,
    context::{self, Cache, Handle},
    data::{Pass, RwData},
    file::{File, PathKind},
    hook::{self, FileClosed, WidgetCreated, WindowCreated},
    mode,
    text::{SpawnId, Text, txt},
    ui::{Area, PushSpecs, SpawnSpecs, Ui},
};

mod builder;

/// A list of all [`Window`]s in Duat
pub struct Windows {
    inner: RwData<InnerWindows>,
    spawns_to_remove: Mutex<Vec<SpawnId>>,
    ui: Ui,
}

impl Windows {
    /// Initializes the `Windows`, returning a [`Node`] for the first
    /// [`File`]
    pub(crate) fn initialize(pa: &mut Pass, file: File, layout: Box<Mutex<dyn Layout>>, ui: Ui) {
        let new_additions = Arc::new(Mutex::default());
        let (window, node) = Window::new(0, pa, ui, file, new_additions.clone());

        context::set_windows(Self {
            inner: RwData::new(InnerWindows {
                layout,
                list: vec![window],
                new_additions,
                cur_file: RwData::new(node.try_downcast().unwrap()),
                cur_widget: RwData::new(node.clone()),
                cur_win: 0,
            }),
            spawns_to_remove: Mutex::new(Vec::new()),
            ui,
        });

        hook::trigger(
            pa,
            WidgetCreated(node.handle().try_downcast::<File>().unwrap()),
        );

        let builder = UiBuilder::new(0);
        hook::trigger(pa, WindowCreated(builder));
    }

    ////////// Functions for new Widgets

    /// Creates a new list of [`Window`]s, with a main one
    /// initialiazed
    pub(crate) fn new_window(&self, pa: &mut Pass, file: File) -> Node {
        let win = self.inner.read(pa).list.len();
        let new_additions = self.inner.read(pa).new_additions.clone();
        let (window, node) = Window::new(win, pa, self.ui, file, new_additions);

        let inner = self.inner.write(pa);
        inner.list.push(window);

        hook::trigger(
            pa,
            WidgetCreated(node.handle().try_downcast::<File>().unwrap()),
        );
        let builder = UiBuilder::new(win);
        hook::trigger(pa, WindowCreated(builder));

        node
    }

    /// Push a [`Widget`] to [`Handle`]
    pub(crate) fn push_widget<W: Widget>(
        &self,
        pa: &mut Pass,
        (target, on_files, specs): (&Area, Option<bool>, PushSpecs),
        widget: W,
    ) -> Option<Handle<W>> {
        self.push(pa, (target, on_files, specs), widget)?
            .handle()
            .try_downcast()
    }

    /// Spawn a [`Widget`] on a [`Handle`]
    ///
    /// Can fail if the `Handle` in question was already removed.
    pub(crate) fn spawn_on_widget<W: Widget>(
        &self,
        pa: &mut Pass,
        (target, specs): (&Area, SpawnSpecs),
        widget: W,
    ) -> Option<Handle<W>> {
        let (win, cluster_master) =
            self.inner
                .read(pa)
                .list
                .iter()
                .enumerate()
                .find_map(|(win, window)| {
                    if window.master_area.is_master_of(pa, target) {
                        Some((win, None))
                    } else if let Some((_, node)) = window
                        .spawned
                        .iter()
                        .find(|(_, node)| node.area().is_master_of(pa, target))
                    {
                        Some((win, node.area().get_cluster_master(pa)))
                    } else {
                        None
                    }
                })?;

        let widget = RwData::new(widget);
        let id = SpawnId::new();

        let spawned = cluster_master.as_ref().unwrap_or(target).spawn(
            pa,
            &widget.to_dyn_widget(),
            id,
            specs,
        )?;

        let node = Node::new(widget, spawned);

        let mut window = self.inner.write(pa).list.remove(win);
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
    ) -> Handle<W> {
        let widget = RwData::new(widget);
        let spawned = self
            .ui
            .new_spawned(pa, &widget.to_dyn_widget(), id, specs, win);

        let node = Node::new(widget, spawned);

        let mut window = self.inner.write(pa).list.remove(win);
        window.add(pa, node.clone(), None, Location::Spawned(id));
        self.inner.write(pa).list.insert(win, window);

        hook::trigger(
            pa,
            WidgetCreated(node.handle().try_downcast::<W>().unwrap()),
        );

        node.handle().try_downcast().unwrap()
    }

    /// Pushes a [`File`] to the file's parent
    ///
    /// This function will push to the edge of `self.files_parent`
    /// This is an area, usually in the center, that contains all
    /// [`File`]s, and their associated [`Widget`]s,
    /// with others being at the perifery of this area.
    pub(crate) fn new_file(&self, pa: &mut Pass, file: File) -> Node {
        let win = context::cur_window(pa);
        let inner = self.inner.read(pa);
        let (handle, specs) = inner
            .layout
            .lock()
            .unwrap()
            .new_file(pa, win, &file, &inner.list);

        let specs = PushSpecs { cluster: false, ..specs };

        if let Some(master) = handle.area().get_cluster_master(pa) {
            self.push(pa, (&master, Some(true), specs), file).unwrap()
        } else {
            self.push(pa, (&handle.area, Some(true), specs), file)
                .unwrap()
        }
    }

    /// Pushes a [`Widget`] to the [`Window`]s
    ///
    /// May return [`None`] if the [`Area`] was already deleted.
    fn push<W: Widget>(
        &self,
        pa: &mut Pass,
        (target, on_files, mut specs): (&Area, Option<bool>, PushSpecs),
        widget: W,
    ) -> Option<Node> {
        let inner = self.inner.read(pa);
        let win = inner
            .list
            .iter()
            .position(|window| {
                window.master_area.is_master_of(pa, target)
                    || window
                        .nodes()
                        .any(|node| node.area().is_master_of(pa, target))
            })
            .unwrap();

        let target_is_on_files = inner.list[win].files_area.is_master_of(pa, target);
        let on_files = on_files.unwrap_or(target_is_on_files) && target_is_on_files;

        if target_is_on_files && !on_files {
            specs.cluster = false;
        }

        let location = if on_files {
            Location::OnFiles
        } else if let Some((id, _)) = inner.list[win]
            .spawned
            .iter()
            .find(|(_, node)| node.area().area_is_eq(pa, target))
        {
            Location::Spawned(*id)
        } else {
            Location::Regular
        };

        let widget = RwData::new(widget);
        let (pushed, parent) = target.push(pa, &widget.to_dyn_widget(), specs, on_files)?;

        let node = Node::new(widget, pushed);

        let mut window = self.inner.write(pa).list.remove(win);
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
        let (win, wid) = self.handle_entry(pa, handle)?;

        // If this is the active Handle, pick another one to make active.
        let inner = self.inner.read(pa);
        if handle == inner.cur_widget.read(pa).handle() || handle == inner.cur_file.read(pa) {
            // SAFETY: This Pass is only used on known other types.
            let internal_pass = &mut unsafe { Pass::new() };

            if handle.widget().data_is::<File>() {
                let entry = self
                    .iter_around_rev(pa, win, wid)
                    .find_map(|(win, _, node)| node.data_is::<File>().then(|| (win, node.clone())));

                if let Some((win, node)) = entry {
                    *inner.cur_file.write(internal_pass) = node.try_downcast().unwrap();
                    *inner.cur_widget.write(internal_pass) = node.clone();
                    self.inner.write(pa).cur_win = win;
                } else {
                    // If there is no previous File, just quit.
                    context::sender()
                        .send(crate::session::DuatEvent::Quit)
                        .unwrap();
                    return Ok(());
                }
            } else {
                let (win, _) = self
                    .handle_entry(pa, &inner.cur_file.read(pa).to_dyn())
                    .unwrap();

                let node = Node::from_handle(inner.cur_file.read(pa).clone());
                *inner.cur_widget.write(internal_pass) = node;
                self.inner.write(pa).cur_win = win;
            }
        }

        // If it's a File, swap all files ahead, so this one becomes the last.
        if let Some(file_handle) = handle.try_downcast::<File>() {
            hook::trigger(pa, FileClosed((file_handle.clone(), Cache::new())));

            let files_ahead: Vec<Node> = self.inner.read(pa).list[win]
                .nodes()
                .filter(|node| {
                    node.handle()
                        .read_as::<File>(pa)
                        .is_some_and(|file| file.layout_order > file_handle.read(pa).layout_order)
                })
                .cloned()
                .collect();

            for file_ahead in files_ahead {
                self.swap(pa, handle, file_ahead.handle())?;
            }
        }

        // Actually removing the Handle.
        let mut windows = std::mem::take(&mut self.inner.write(pa).list);

        if windows[win].close(pa, handle) {
            windows.remove(win);
            self.ui.remove_window(win);
            let cur_win = context::cur_window(pa);
            if cur_win > win {
                self.inner.write(pa).cur_win -= 1;
            }
        }

        let inner = self.inner.write(pa);
        inner.list = windows;
        inner.new_additions.lock().unwrap().get_or_insert_default();

        Ok(())
    }

    /// Swaps two [`Handle`]'s positions
    pub(crate) fn swap<W1: Widget + ?Sized, S1, W2: Widget + ?Sized, S2>(
        &self,
        pa: &mut Pass,
        lhs: &Handle<W1, S1>,
        rhs: &Handle<W2, S2>,
    ) -> Result<(), Text> {
        let (lhs_win, _) = self.handle_entry(pa, lhs)?;
        let (rhs_win, _) = self.handle_entry(pa, rhs)?;

        let [lhs_file, rhs_file] = [lhs.try_downcast::<File>(), rhs.try_downcast()];
        let lhs_lo = lhs_file.as_ref().map(|handle| handle.read(pa).layout_order);
        let rhs_lo = rhs_file.as_ref().map(|handle| handle.read(pa).layout_order);

        if let [Some(lhs), Some(rhs)] = [lhs_file, rhs_file] {
            let lhs_lo = lhs.read(pa).layout_order;
            let rhs_lo = std::mem::replace(&mut rhs.write(pa).layout_order, lhs_lo);
            lhs.write(pa).layout_order = rhs_lo
        }

        let mut windows = std::mem::take(&mut self.inner.write(pa).list);

        let lhs_nodes = windows[lhs_win].take_with_related_nodes(pa, lhs);
        windows[rhs_win].insert_nodes(pa, rhs_lo, lhs_nodes);

        let rhs_nodes = windows[rhs_win].take_with_related_nodes(pa, rhs);
        windows[lhs_win].insert_nodes(pa, lhs_lo, rhs_nodes);

        let wins = self.inner.write(pa);
        wins.list = windows;
        wins.new_additions.lock().unwrap().get_or_insert_default();

        lhs.area().swap(pa, rhs.area());

        let cur_file = context::cur_file(pa);
        if lhs_win != rhs_win {
            if *lhs == cur_file {
                self.inner.write(pa).cur_win = lhs_win;
                self.ui.switch_window(lhs_win);
            } else if *rhs == cur_file {
                self.inner.write(pa).cur_win = rhs_win;
                self.ui.switch_window(rhs_win);
            }
        }

        Ok(())
    }

    /// Opens a new [`File`] on a new [`Window`], or moves it there,
    /// if it is already open
    pub(crate) fn open_or_move_to_new_window(
        &self,
        pa: &mut Pass,
        pk: PathKind,
        default_file_cfg: PrintCfg,
    ) -> Node {
        let node = match self.file_entry(pa, pk.clone()) {
            Ok((win, _, handle)) if self.get(pa, win).unwrap().file_handles(pa).len() > 1 => {
                // Take the nodes in the original Window
                handle.write(pa).layout_order = 0;

                let nodes = {
                    let mut old_window = self.inner.write(pa).list.remove(win);
                    let nodes = old_window.take_with_related_nodes(pa, &handle.to_dyn());
                    self.inner.write(pa).list.insert(win, old_window);

                    nodes
                };

                // Create a new Window Swapping the new root with files_area
                let new_root = self.ui.new_root(pa, &handle.widget().to_dyn_widget());
                handle.area().swap(pa, &new_root);
                let window = Window::from_raw(
                    pa,
                    win,
                    handle.area.clone(),
                    nodes,
                    self.inner.read(pa).new_additions.clone(),
                );

                self.inner.write(pa).list.push(window);

                let wins = self.inner.write(pa);
                let builder = UiBuilder::new(wins.list.len() - 1);
                hook::trigger(pa, WindowCreated(builder));

                // Swap the Files ahead of the swapped new_root
                let lo = handle.read(pa).layout_order;

                for handle in &self.inner.read(pa).list[win].file_handles(pa)[lo..] {
                    new_root.swap(pa, handle.area());
                }

                // Delete the new_root, which should be the last "File" in the
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
            Err(_) => self.new_window(pa, File::new(pk.as_path(), default_file_cfg)),
        };

        if context::cur_file(pa).read(pa).path_kind() != pk {
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

        let (win, _) = self.handle_entry(pa, node.handle())?;
        let inner = self.inner.write(pa);

        if let Some(handle) = node.try_downcast() {
            *inner.cur_file.write(internal_pass) = handle;
        }
        *inner.cur_widget.write(internal_pass) = node.clone();
        inner.cur_win = win;

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
                .windows(pa)
                .flat_map(|window| &window.spawned)
                .find(|(other, _)| *other == id)
            {
                self.close(pa, &node.handle().clone()).unwrap();
            }
        }
    }

    ////////// Entry lookup

    /// An entry for a [`Handle`]
    pub fn handle_entry<W: Widget + ?Sized, S>(
        &self,
        pa: &Pass,
        handle: &Handle<W, S>,
    ) -> Result<(usize, usize), Text> {
        self.entries(pa)
            .find_map(|(win, wid, node)| (node.handle() == handle).then_some((win, wid)))
            .ok_or_else(|| txt!("The Handle was already closed").build())
    }

    /// An entry for a file with the given name
    pub fn file_entry(
        &self,
        pa: &Pass,
        pk: PathKind,
    ) -> Result<(usize, usize, Handle<File>), Text> {
        self.entries(pa)
            .find_map(|(win, wid, node)| {
                (node.read_as(pa).filter(|f: &&File| f.path_kind() == pk))
                    .and_then(|_| node.try_downcast().map(|handle| (win, wid, handle)))
            })
            .ok_or_else(|| txt!("File {pk} not found").build())
    }

    /// An entry for a file with the given name
    pub fn named_file_entry(
        &self,
        pa: &Pass,
        name: &str,
    ) -> Result<(usize, usize, Handle<File>), Text> {
        self.entries(pa)
            .find_map(|(win, wid, node)| {
                (node.read_as(pa).filter(|f: &&File| f.name() == name))
                    .and_then(|_| node.try_downcast().map(|handle| (win, wid, handle)))
            })
            .ok_or_else(|| txt!("File {name} not found").build())
    }

    /// An entry for a widget of a specific type
    ///
    /// Returns the index of the window, the index of the [`Widget`],
    /// and the [`Widget`]'s [`Node`]
    pub(crate) fn widget_entry<'a, W: Widget>(
        &'a self,
        pa: &'a Pass,
        w: usize,
    ) -> Result<(usize, usize, &'a Node), Text> {
        let handle = context::cur_file(pa);

        if let Some((handle, _)) = handle.get_related::<W>(pa).next() {
            self.entries(pa).find(|(.., n)| n.ptr_eq(handle.widget()))
        } else {
            self.iter_around(pa, w, 0)
                .find(|(.., node)| node.data_is::<W>())
        }
        .ok_or(txt!("No widget of type [a]{}[] found", type_name::<W>()).build())
    }

    ////////// Entry iterators

    /// Iterates over all widget entries, with window and widget
    /// indices, in that order
    pub(crate) fn entries<'a>(
        &'a self,
        pa: &'a Pass,
    ) -> impl Iterator<Item = (usize, usize, &'a Node)> {
        self.inner
            .read(pa)
            .list
            .iter()
            .enumerate()
            .flat_map(|(win, window)| {
                window
                    .nodes
                    .iter()
                    .chain(window.spawned.iter().map(|(_, node)| node))
                    .enumerate()
                    .map(move |(wid, node)| (win, wid, node))
            })
    }

    /// Iterates around a specific widget, going forwards
    pub(crate) fn iter_around<'a>(
        &'a self,
        pa: &'a Pass,
        win: usize,
        wid: usize,
    ) -> impl Iterator<Item = (usize, usize, &'a Node)> + 'a {
        let windows = &self.inner.read(pa).list;

        let prev_len: usize = windows.iter().take(win).map(Window::len_widgets).sum();

        windows
            .iter()
            .enumerate()
            .skip(win)
            .flat_map(window_index_widget)
            .skip(wid + 1)
            .chain(
                windows
                    .iter()
                    .enumerate()
                    .take(win + 1)
                    .flat_map(window_index_widget)
                    .take(prev_len + wid),
            )
    }

    /// Iterates around a specific widget, going backwards
    pub(crate) fn iter_around_rev<'a>(
        &'a self,
        pa: &'a Pass,
        win: usize,
        wid: usize,
    ) -> impl Iterator<Item = (usize, usize, &'a Node)> + 'a {
        let windows = &self.inner.read(pa).list;

        let next_len: usize = windows.iter().skip(win).map(Window::len_widgets).sum();

        windows
            .iter()
            .enumerate()
            .rev()
            .skip(windows.len() - (win + 1))
            .flat_map(move |(i, win)| {
                window_index_widget((i, win))
                    .rev()
                    .skip(win.len_widgets() - wid)
            })
            .chain(
                windows
                    .iter()
                    .enumerate()
                    .rev()
                    .take(windows.len() - win)
                    .flat_map(move |(i, win)| window_index_widget((i, win)).rev())
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

    /// Returns an [`Iterator`] over the [`Handle`]s of Duat
    pub fn handles<'a>(&'a self, pa: &'a Pass) -> impl Iterator<Item = &'a Handle<dyn Widget>> {
        self.inner
            .read(pa)
            .list
            .iter()
            .flat_map(|w| w.nodes().map(|n| n.handle()))
    }

    /// Iterates over all [`Handle<File>`]s in Duat
    pub fn file_handles<'a>(&'a self, pa: &'a Pass) -> impl Iterator<Item = Handle<File>> + 'a {
        self.inner
            .read(pa)
            .list
            .iter()
            .flat_map(|w| w.file_handles(pa))
    }

    /// The [`RwData`] that points to the currently active [`File`]
    pub(crate) fn current_file(&self, pa: &Pass) -> RwData<Handle<File>> {
        self.inner.read(pa).cur_file.clone()
    }

    /// The [`RwData`] that points to the currently active [`Widget`]
    pub(crate) fn current_widget(&self, pa: &Pass) -> RwData<Node> {
        self.inner.read(pa).cur_widget.clone()
    }

    /// The index of the currently active [`Window`]
    pub(crate) fn current_window(&self, pa: &Pass) -> usize {
        self.inner.read(pa).cur_win
    }

    /// Iterates through every [`Window`]
    pub(crate) fn windows<'a>(&'a self, pa: &'a Pass) -> std::slice::Iter<'a, Window> {
        self.inner.read(pa).list.iter()
    }

    /// Gets the new additions to the [`Windows`]
    pub(crate) fn get_additions(&self, pa: &mut Pass) -> Option<Vec<(usize, Node)>> {
        self.inner.write(pa).new_additions.lock().unwrap().take()
    }
}

/// Inner holder of [`Window`]s
struct InnerWindows {
    layout: Box<Mutex<dyn Layout>>,
    list: Vec<Window>,
    new_additions: Arc<Mutex<Option<Vec<(usize, Node)>>>>,
    cur_file: RwData<Handle<File>>,
    cur_widget: RwData<Node>,
    cur_win: usize,
}

/// A container for a master [`Area`] in Duat
pub struct Window {
    index: usize,
    nodes: Vec<Node>,
    spawned: Vec<(SpawnId, Node)>,
    files_area: Area,
    master_area: Area,
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
        if let Some(file) = widget.write_as::<File>(pa) {
            file.layout_order = get_layout_order();
        }

        let area = ui.new_root(pa, &widget.to_dyn_widget());
        let node = Node::new::<W>(widget, area.clone());

        new_additions
            .lock()
            .unwrap()
            .get_or_insert_default()
            .push((index, node.clone()));

        let window = Self {
            index,
            nodes: vec![node.clone()],
            spawned: Vec::new(),
            files_area: area.clone(),
            master_area: area.clone(),
            new_additions,
        };

        (window, node)
    }

    /// Returns a new [`Window`] from raw elements
    pub(crate) fn from_raw(
        pa: &mut Pass,
        index: usize,
        master_area: Area,
        nodes: Vec<Node>,
        new_additions: Arc<Mutex<Option<Vec<(usize, Node)>>>>,
    ) -> Self {
        let master_area = master_area
            .get_cluster_master(pa)
            .unwrap_or(master_area.clone());

        Self {
            index,
            nodes,
            spawned: Vec::new(),
            files_area: master_area.clone(),
            master_area,
            new_additions,
        }
    }

    ////////// Widget addition/removal

    /// Adds a [`Widget`] to the list of widgets of this [`Window`]
    fn add(&mut self, pa: &Pass, node: Node, parent: Option<Area>, location: Location) {
        match location {
            Location::OnFiles => {
                self.nodes.push(node.clone());
                if let Some(parent) = &parent
                    && parent.is_master_of(pa, &self.files_area)
                {
                    self.files_area = parent.clone()
                }
            }
            Location::Regular => self.nodes.push(node.clone()),
            Location::Spawned(id) => self.spawned.push((id, node.clone())),
        }

        if let Some(parent) = &parent
            && parent.is_master_of(pa, &self.master_area)
        {
            self.master_area = parent.clone()
        }

        self.new_additions
            .lock()
            .unwrap()
            .get_or_insert_default()
            .push((self.index, node.clone()));
    }

    /// Closes the [`Handle`] and all related ones
    ///
    /// Returns `true` if this `Window` is supposed to be removed.
    fn close<W: Widget + ?Sized, S>(&mut self, pa: &mut Pass, handle: &Handle<W, S>) -> bool {
        let handle_eq = |node: &mut Node| node.handle() == handle;

        let node = if let Some(node) = self.nodes.extract_if(.., handle_eq).next() {
            node
        } else if let Some((_, node)) = self.spawned.extract_if(.., |(_, n)| handle_eq(n)).next() {
            node
        } else {
            unreachable!("This isn't supposed to fail");
        };

        node.handle().declare_closed(pa);

        let (do_rm_window, rm_areas) = node.area().delete(pa);
        if do_rm_window {
            return true;
        }

        self.nodes.retain(|node| {
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
        self.spawned.retain(|(_, node)| {
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

        let files = self.file_handles(pa);
        if files.len() == 1 {
            let handle = files.first().unwrap();

            let master_area = handle
                .area()
                .get_cluster_master(pa)
                .unwrap_or(handle.area.clone());

            self.files_area = master_area;
        }

        false
    }

    /// Takes all [`Node`]s related to a given [`Node`]
    fn take_with_related_nodes<W: Widget + ?Sized, S>(
        &mut self,
        pa: &mut Pass,
        handle: &Handle<W, S>,
    ) -> Vec<Node> {
        let related = handle.related();

        let related = related.read(pa);
        let nodes = self
            .nodes
            .extract_if(.., |node| {
                related.iter().any(|(handle, _)| handle == node.handle()) || node.handle() == handle
            })
            .collect();

        if let Some(handle) = handle.try_downcast::<File>() {
            let lo = handle.read(pa).layout_order;

            for node in self.nodes.iter() {
                if let Some(file) = node.widget().write_as::<File>(pa) {
                    file.layout_order -= (file.layout_order > lo) as usize;
                }
            }
        }

        nodes
    }

    /// Inserts [`File`] nodes orderly
    fn insert_nodes(&mut self, pa: &mut Pass, layout_order: Option<usize>, nodes: Vec<Node>) {
        if let Some(layout_order) = layout_order
            && let Some(i) = self.nodes.iter().position(|node| {
                node.widget()
                    .read_as(pa)
                    .is_some_and(|f: &File| f.layout_order >= layout_order)
            })
        {
            for node in self.nodes[i..].iter() {
                if let Some(file) = node.widget().write_as::<File>(pa) {
                    file.layout_order += 1;
                }
            }
            self.nodes.splice(i..i, nodes);
        } else {
            self.nodes.extend(nodes);
        }
    }

    ////////// Querying functions

    /// An [`Iterator`] over the [`Node`]s in a [`Window`]
    #[define_opaque(InnerIter)]
    pub(crate) fn nodes(&self) -> impl ExactSizeIterator<Item = &Node> + DoubleEndedIterator {
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

        InnerChain(
            self.nodes
                .iter()
                .chain(self.spawned.iter().map(|(_, node)| node)),
            self.nodes.len() + self.spawned.len(),
        )
    }

    /// Returns an [`Iterator`] over the names of [`File`]s
    /// and their respective [`Widget`] indices
    ///
    /// [`Widget`]: crate::ui::Widget
    pub fn file_names(&self, pa: &Pass) -> Vec<String> {
        self.file_handles(pa)
            .into_iter()
            .map(|handle| handle.read(pa).name())
            .collect()
    }

    /// Returns an [`Iterator`] over the paths of [`File`]s
    /// and their respective [`Widget`] indices
    ///
    /// [`Widget`]: crate::ui::Widget
    pub fn file_paths(&self, pa: &Pass) -> Vec<String> {
        self.file_handles(pa)
            .into_iter()
            .map(|handle| handle.read(pa).path())
            .collect()
    }

    /// An [`Iterator`] over the [`File`] [`Node`]s in a [`Window`]
    pub(crate) fn file_handles(&self, pa: &Pass) -> Vec<Handle<File>> {
        let mut files: Vec<Handle<File>> = self
            .nodes
            .iter()
            .filter_map(|node| node.try_downcast())
            .collect();

        files.sort_unstable_by_key(|file| file.read(pa).layout_order);

        files
    }

    /// How many [`Widget`]s are in this [`Window`]
    pub(crate) fn len_widgets(&self) -> usize {
        self.nodes.len() + self.spawned.len()
    }
}

impl std::fmt::Debug for Window {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Window")
            .field("nodes", &self.nodes)
            .field("floating", &self.spawned)
            .finish_non_exhaustive()
    }
}

/// Iterator over a group of windows, that returns the window's index
fn window_index_widget(
    (index, window): (usize, &Window),
) -> impl ExactSizeIterator<Item = (usize, usize, &Node)> + DoubleEndedIterator {
    window
        .nodes()
        .enumerate()
        .map(move |(i, entry)| (index, i, entry))
}

/// Returns a new layout order, which will be different from every
/// other
fn get_layout_order() -> usize {
    use std::sync::atomic::{AtomicUsize, Ordering};

    static LAYOUT_ORDER: AtomicUsize = AtomicUsize::new(0);
    LAYOUT_ORDER.fetch_add(1, Ordering::Relaxed)
}

enum Location {
    OnFiles,
    Regular,
    Spawned(SpawnId),
}

type InnerIter<'a> = impl DoubleEndedIterator<Item = &'a Node>;
