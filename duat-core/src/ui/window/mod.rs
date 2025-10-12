use std::{
    any::type_name,
    sync::{Arc, Mutex},
};

pub use self::builder::UiBuilder;
use super::{Area, Node, Ui, Widget, layout::Layout};
use crate::{
    cfg::PrintCfg,
    context::{self, Cache, Handle},
    data::{Pass, RwData},
    file::{File, PathKind},
    hook::{self, FileClosed, WidgetCreated, WindowCreated},
    mode,
    text::{SpawnId, Text, txt},
    ui::{MutArea, PushSpecs, SpawnSpecs},
    utils::duat_name,
};

mod builder;

/// A list of all [`Window`]s in Duat
pub struct Windows<U: Ui> {
    inner: RwData<InnerWindows<U>>,
    ms: &'static U::MetaStatics,
}

impl<U: Ui> Windows<U> {
    /// Initializes the `Windows`, returning a [`Node`] for the first
    /// [`File`]
    pub(crate) fn initialize(
        pa: &mut Pass,
        file: File<U>,
        layout: Box<Mutex<dyn Layout<U>>>,
        ms: &'static U::MetaStatics,
    ) {
        let new_additions = Arc::new(Mutex::default());
        let (window, node) = Window::new(0, pa, ms, file, new_additions.clone());

        context::set_windows(Self {
            inner: RwData::new(InnerWindows {
                layout,
                list: vec![window],
                new_additions,
                cur_file: RwData::new(node.try_downcast().unwrap()),
                cur_widget: RwData::new(node.clone()),
                cur_win: 0,
            }),
            ms,
        });

        hook::trigger(
            pa,
            WidgetCreated(node.handle().try_downcast::<File<U>>().unwrap()),
        );
        let builder = UiBuilder::<U>::new(0);
        hook::trigger(pa, WindowCreated(builder));
    }

    /// Creates a new list of [`Window`]s, with a main one
    /// initialiazed
    pub(crate) fn new_window(&self, pa: &mut Pass, file: File<U>) -> Node<U> {
        let win = self.inner.read(pa).list.len();
        let new_additions = self.inner.read(pa).new_additions.clone();
        let (window, node) = Window::new(win, pa, self.ms, file, new_additions);

        let inner = self.inner.write(pa);
        inner.list.push(window);

        hook::trigger(
            pa,
            WidgetCreated(node.handle().try_downcast::<File<U>>().unwrap()),
        );
        let builder = UiBuilder::<U>::new(win);
        hook::trigger(pa, WindowCreated(builder));

        node
    }

    /// Push a [`Widget`] to [`Handle`]
    pub(crate) fn push_widget<W: Widget<U>>(
        &self,
        pa: &mut Pass,
        (target, on_files, specs): (&U::Area, Option<bool>, PushSpecs),
        widget: W,
    ) -> Option<Handle<W, U>> {
        self.push(pa, (target, on_files, specs), widget)?
            .handle()
            .try_downcast()
    }

    /// Spawn a [`Widget`] on a [`Handle`]
    ///
    /// Can fail if the `Handle` in question was already removed.
    pub(crate) fn spawn_on_widget<W: Widget<U>>(
        &self,
        pa: &mut Pass,
        (on, specs): (&U::Area, SpawnSpecs),
        widget: W,
    ) -> Option<Handle<W, U>> {
        let win = window_index_of_area(pa, on, self.inner.read(pa));
        let widget = RwData::new(widget);
        let cache = get_cache(pa, widget.to_dyn_widget(), self, Some(win));
        let id = SpawnId::new();
        let spawned = U::Area::spawn(MutArea(on), id, specs, cache)?;

        let node = Node::new(widget, Arc::new(spawned));

        let wins = self.inner.write(pa);
        wins.list[win].add(node.clone(), None, Location::Spawned(id));

        hook::trigger(
            pa,
            WidgetCreated(node.handle().try_downcast::<W>().unwrap()),
        );

        node.handle().try_downcast()
    }

    /// Spawns a [`Widget`]
    pub(crate) fn spawn_on_text<W: Widget<U>>(
        &self,
        pa: &mut Pass,
        (id, specs): (SpawnId, SpawnSpecs),
        widget: W,
        win: usize,
    ) -> Handle<W, U> {
        let widget = RwData::new(widget);
        let cache = get_cache(pa, widget.to_dyn_widget(), self, None);
        let spawned = U::new_spawned(self.ms, id, specs, cache, win);

        let node = Node::new(widget, Arc::new(spawned));

        let wins = self.inner.write(pa);
        wins.list[win].add(node.clone(), None, Location::Spawned(id));

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
    pub(crate) fn new_file(&self, pa: &mut Pass, file: File<U>) -> Node<U> {
        let win = context::cur_window::<U>(pa);
        let inner = self.inner.read(pa);
        let (handle, specs) = inner
            .layout
            .lock()
            .unwrap()
            .new_file(pa, win, &file, &inner.list);

        let specs = PushSpecs { cluster: false, ..specs };

        if let Some(master) = handle.area(pa).get_cluster_master() {
            self.push(pa, (&master, Some(true), specs), file).unwrap()
        } else {
            self.push(pa, (&handle.area, Some(true), specs), file)
                .unwrap()
        }
    }

    pub(crate) fn close(
        &self,
        pa: &mut Pass,
        handle: Handle<dyn Widget<U>, U>,
    ) -> Result<(), Text> {
        let (win, wid) = self.handle_entry(pa, &handle)?;

        // If this is the active Handle, pick another one to make active.
        let inner = self.inner.read(pa);
        if handle == *inner.cur_widget.read(pa).handle() {
            // SAFETY: This Pass is only used on known other types.
            let internal_pass = &mut unsafe { Pass::new() };

            if handle.widget().data_is::<File<U>>() {
                let entry = self
                    .iter_around_rev(pa, win, wid)
                    .find_map(|(win, _, node)| {
                        node.data_is::<File<U>>().then(|| (win, node.clone()))
                    });

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
        if let Some(file_handle) = handle.try_downcast::<File<U>>() {
            hook::trigger(pa, FileClosed((file_handle.clone(), Cache::new())));

            let files_ahead: Vec<Node<U>> = self.inner.read(pa).list[win]
                .nodes()
                .filter(|node| {
                    node.handle()
                        .read_as::<File<U>>(pa)
                        .is_some_and(|file| file.layout_order > file_handle.read(pa).layout_order)
                })
                .cloned()
                .collect();

            for file_ahead in files_ahead {
                self.swap(pa, &handle, file_ahead.handle())?;
            }
        }

        // Actually removing the Handle.
        let mut windows = std::mem::take(&mut self.inner.write(pa).list);

        if windows[win].close(pa, handle) {
            windows.remove(win);
            U::remove_window(self.ms, win);
            let cur_win = context::cur_window::<U>(pa);
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
    pub(crate) fn swap(
        &self,
        pa: &mut Pass,
        lhs: &Handle<dyn Widget<U>, U>,
        rhs: &Handle<dyn Widget<U>, U>,
    ) -> Result<(), Text> {
        let (lhs_win, _) = self.handle_entry(pa, lhs)?;
        let (rhs_win, _) = self.handle_entry(pa, rhs)?;

        let [lhs_file, rhs_file] = [lhs.try_downcast::<File<U>>(), rhs.try_downcast()];
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

        U::Area::swap(MutArea(lhs.area(pa)), rhs.area(pa));

        let cur_file = context::cur_file::<U>(pa);
        if lhs_win != rhs_win
            && let Some(win) = [lhs, rhs]
                .into_iter()
                .position(|handle| *handle == cur_file)
        {
            self.inner.write(pa).cur_win = win;
            U::switch_window(self.ms, win);
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
    ) -> Node<U> {
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
                let new_root = U::new_root(self.ms, <U::Area as Area>::Cache::default());
                U::Area::swap(MutArea(handle.area(pa)), &new_root);
                let window = Window::<U>::from_raw(
                    win,
                    handle.area.clone(),
                    nodes,
                    self.inner.read(pa).new_additions.clone(),
                );

                self.inner.write(pa).list.push(window);

                let wins = self.inner.write(pa);
                let builder = UiBuilder::<U>::new(wins.list.len() - 1);
                hook::trigger(pa, WindowCreated(builder));

                // Swap the Files ahead of the swapped new_root
                let lo = handle.read(pa).layout_order;

                for handle in &self.inner.read(pa).list[win].file_handles(pa)[lo..] {
                    U::Area::swap(MutArea(&new_root), handle.area(pa));
                }

                // Delete the new_root, which should be the last "File" in the
                // list of the original Window.
                U::Area::delete(MutArea(&new_root));

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

        if context::cur_file::<U>(pa).read(pa).path_kind() != pk {
            mode::reset_to::<U>(node.handle().clone());
        }

        let new_win = context::windows::<U>().len(pa) - 1;
        self.inner.write(pa).cur_win = new_win;
        U::switch_window(self.ms, new_win);

        node
    }

    /// Pushes a [`Widget`] to the [`Window`]s
    ///
    /// May return [`None`] if the [`U::Area`] was already deleted.
    fn push<W: Widget<U>>(
        &self,
        pa: &mut Pass,
        (target, on_files, mut specs): (&U::Area, Option<bool>, PushSpecs),
        widget: W,
    ) -> Option<Node<U>> {
        run_once::<W, U>();

        let inner = self.inner.read(pa);
        let win = inner
            .list
            .iter()
            .position(|window| {
                window.master_area.is_master_of(target)
                    || window
                        .nodes()
                        .any(|node| node.area(pa).is_master_of(target))
            })
            .unwrap();

        let target_is_on_files = inner.list[win].files_area.is_master_of(target);
        let on_files = on_files.unwrap_or(target_is_on_files) && target_is_on_files;

        if target_is_on_files && !on_files {
            specs.cluster = false;
        }

        let location = if on_files {
            Location::OnFiles
        } else if let Some((id, _)) = inner.list[win]
            .spawned
            .iter()
            .find(|(_, node)| node.area(pa) == target)
        {
            Location::Spawned(*id)
        } else {
            Location::Regular
        };

        let widget = RwData::new(widget);
        let cache = get_cache(pa, widget.to_dyn_widget(), self, Some(win));
        let (pushed, parent) = U::Area::push(MutArea(target), specs, on_files, cache)?;

        let node = Node::new(widget, Arc::new(pushed));

        let inner = self.inner.write(pa);
        inner.list[win].add(node.clone(), parent.map(Arc::new), location);

        hook::trigger(
            pa,
            WidgetCreated(node.handle().try_downcast::<W>().unwrap()),
        );

        Some(node)
    }

    /// Sets the current active [`Handle`]
    pub(crate) fn set_current_node(&self, pa: &mut Pass, node: Node<U>) -> Result<(), Text> {
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

    ////////// Entry lookup

    /// An entry for a [`Handle`]
    pub fn handle_entry(
        &self,
        pa: &Pass,
        handle: &Handle<dyn Widget<U>, U>,
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
    ) -> Result<(usize, usize, Handle<File<U>, U>), Text> {
        self.entries(pa)
            .find_map(|(win, wid, node)| {
                (node.read_as(pa).filter(|f: &&File<U>| f.path_kind() == pk))
                    .and_then(|_| node.try_downcast().map(|handle| (win, wid, handle)))
            })
            .ok_or_else(|| txt!("File {pk} not found").build())
    }

    /// An entry for a file with the given name
    pub fn named_file_entry(
        &self,
        pa: &Pass,
        name: &str,
    ) -> Result<(usize, usize, Handle<File<U>, U>), Text> {
        self.entries(pa)
            .find_map(|(win, wid, node)| {
                (node.read_as(pa).filter(|f: &&File<U>| f.name() == name))
                    .and_then(|_| node.try_downcast().map(|handle| (win, wid, handle)))
            })
            .ok_or_else(|| txt!("File {name} not found").build())
    }

    /// An entry for a widget of a specific type
    ///
    /// Returns the index of the window, the index of the [`Widget`],
    /// and the [`Widget`]'s [`Node`]
    pub(crate) fn widget_entry<'a, W: Widget<U>>(
        &'a self,
        pa: &'a Pass,
        w: usize,
    ) -> Result<(usize, usize, &'a Node<U>), Text> {
        let handle = context::cur_file::<U>(pa);

        if let Some((handle, _)) = handle.get_related::<W>(pa).next() {
            self.entries(pa).find(|(.., n)| n.ptr_eq(handle.widget()))
        } else {
            self.iter_around(pa, w, 0)
                .find(|(.., node)| node.data_is::<W>())
        }
        .ok_or(txt!("No widget of type [a]{}[] found", type_name::<W>()).build())
    }

    /// Iterates around a specific widget, going forwards
    pub(crate) fn iter_around<'a>(
        &'a self,
        pa: &'a Pass,
        win: usize,
        wid: usize,
    ) -> impl Iterator<Item = (usize, usize, &'a Node<U>)> + 'a {
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
    ) -> impl Iterator<Item = (usize, usize, &'a Node<U>)> + 'a {
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

    /// Iterates over all widget entries, with window and widget
    /// indices, in that order
    pub(crate) fn entries<'a>(
        &'a self,
        pa: &'a Pass,
    ) -> impl Iterator<Item = (usize, usize, &'a Node<U>)> {
        self.inner
            .read(pa)
            .list
            .iter()
            .enumerate()
            .flat_map(|(win, window)| {
                window
                    .nodes
                    .iter()
                    .enumerate()
                    .map(move |(wid, node)| (win, wid, node))
            })
    }

    ////////// Querying functions

    /// The number of open [`Window`]s
    ///
    /// Should never be 0, as that is not a valid state of affairs.
    pub fn len(&self, pa: &Pass) -> usize {
        self.inner.read(pa).list.len()
    }

    /// get's the `win`th [`Window`]
    pub fn get<'a>(&'a self, pa: &'a Pass, win: usize) -> Option<&'a Window<U>> {
        self.inner.read(pa).list.get(win)
    }

    /// Returns an [`Iterator`] over the [`Handle`]s of Duat
    pub fn handles<'a>(
        &'a self,
        pa: &'a Pass,
    ) -> impl Iterator<Item = &'a Handle<dyn Widget<U>, U>> {
        self.inner
            .read(pa)
            .list
            .iter()
            .flat_map(|w| w.nodes().map(|n| n.handle()))
    }

    /// Iterates over all [`Handle<File>`]s in Duat
    pub fn file_handles<'a>(
        &'a self,
        pa: &'a Pass,
    ) -> impl Iterator<Item = Handle<File<U>, U>> + 'a {
        self.inner
            .read(pa)
            .list
            .iter()
            .flat_map(|w| w.file_handles(pa))
    }

    /// The [`RwData`] that points to the currently active [`File`]
    pub(crate) fn current_file(&self, pa: &Pass) -> RwData<Handle<File<U>, U>> {
        self.inner.read(pa).cur_file.clone()
    }

    /// The [`RwData`] that points to the currently active [`Widget`]
    pub(crate) fn current_widget(&self, pa: &Pass) -> RwData<Node<U>> {
        self.inner.read(pa).cur_widget.clone()
    }

    /// The index of the currently active [`Window`]
    pub(crate) fn current_window(&self, pa: &Pass) -> usize {
        self.inner.read(pa).cur_win
    }

    /// Iterates through every [`Window`]
    pub(crate) fn windows<'a>(&'a self, pa: &'a Pass) -> std::slice::Iter<'a, Window<U>> {
        self.inner.read(pa).list.iter()
    }

    /// Gets the new additions to the [`Windows`]
    pub(crate) fn get_additions(&self, pa: &mut Pass) -> Option<Vec<(usize, Node<U>)>> {
        self.inner.write(pa).new_additions.lock().unwrap().take()
    }
}

/// Inner holder of [`Window`]s
struct InnerWindows<U: Ui> {
    layout: Box<Mutex<dyn Layout<U>>>,
    list: Vec<Window<U>>,
    new_additions: Arc<Mutex<Option<Vec<(usize, Node<U>)>>>>,
    cur_file: RwData<Handle<File<U>, U>>,
    cur_widget: RwData<Node<U>>,
    cur_win: usize,
}

/// A container for a master [`Area`] in Duat
pub struct Window<U: Ui> {
    index: usize,
    nodes: Vec<Node<U>>,
    spawned: Vec<(SpawnId, Node<U>)>,
    files_area: Arc<U::Area>,
    master_area: Arc<U::Area>,
    new_additions: Arc<Mutex<Option<Vec<(usize, Node<U>)>>>>,
}

impl<U: Ui> Window<U> {
    /// Returns a new instance of [`Window`]
    fn new<W: Widget<U>>(
        index: usize,
        pa: &mut Pass,
        ms: &'static U::MetaStatics,
        widget: W,
        new_additions: Arc<Mutex<Option<Vec<(usize, Node<U>)>>>>,
    ) -> (Self, Node<U>) {
        let widget = RwData::new(widget);

        let cache = widget
            .read_as(pa)
            .and_then(|f: &File<U>| Cache::new().load::<<U::Area as Area>::Cache>(f.path()).ok())
            .unwrap_or_default();

        let area = Arc::new(U::new_root(ms, cache));
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
        index: usize,
        master_area: Arc<U::Area>,
        nodes: Vec<Node<U>>,
        new_additions: Arc<Mutex<Option<Vec<(usize, Node<U>)>>>>,
    ) -> Self {
        let master_area = master_area
            .get_cluster_master()
            .map(Arc::new)
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
    fn add(&mut self, node: Node<U>, parent: Option<Arc<U::Area>>, location: Location) {
        match location {
            Location::OnFiles => {
                self.nodes.push(node.clone());
                if let Some(parent) = &parent
                    && parent.is_master_of(&self.files_area)
                {
                    self.files_area = parent.clone()
                }
            }
            Location::Regular => self.nodes.push(node.clone()),
            Location::Spawned(id) => self.spawned.push((id, node.clone())),
        }

        if let Some(parent) = &parent
            && parent.is_master_of(&self.master_area)
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
    fn close(&mut self, pa: &mut Pass, handle: Handle<dyn Widget<U>, U>) -> bool {
        let Some(node) = self
            .nodes
            .extract_if(.., |node| *node.handle() == handle)
            .next()
        else {
            unreachable!("This isn't supposed to fail");
        };

        let (do_rm_window, rm_areas) = U::Area::delete(MutArea(node.area(pa)));
        if do_rm_window {
            return true;
        }

        self.nodes.retain(|node| {
            if rm_areas.contains(node.handle().area(pa)) {
                node.handle().declare_closed(pa);
                false
            } else {
                true
            }
        });
        self.spawned.retain(|(_, node)| {
            if rm_areas.contains(node.handle().area(pa)) {
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
                .area(pa)
                .get_cluster_master()
                .map(Arc::new)
                .unwrap_or(handle.area.clone());

            self.files_area = master_area;
        }

        false
    }

    /// Takes all [`Node`]s related to a given [`Node`]
    fn take_with_related_nodes(
        &mut self,
        pa: &mut Pass,
        handle: &Handle<dyn Widget<U>, U>,
    ) -> Vec<Node<U>> {
        let related = handle.related();

        let related = related.read(pa);
        let nodes = self
            .nodes
            .extract_if(.., |node| {
                related.iter().any(|(handle, _)| handle == node.handle()) || node.handle() == handle
            })
            .collect();

        if let Some(handle) = handle.try_downcast::<File<U>>() {
            let lo = handle.read(pa).layout_order;

            for node in self.nodes.iter() {
                if let Some(file) = node.widget().write_as::<File<U>>(pa) {
                    file.layout_order -= (file.layout_order > lo) as usize;
                }
            }
        }

        nodes
    }

    /// Inserts [`File`] nodes orderly
    fn insert_nodes(&mut self, pa: &mut Pass, layout_order: Option<usize>, nodes: Vec<Node<U>>) {
        if let Some(layout_order) = layout_order
            && let Some(i) = self.nodes.iter().position(|node| {
                node.widget()
                    .read_as(pa)
                    .is_some_and(|f: &File<U>| f.layout_order >= layout_order)
            })
        {
            for node in self.nodes[i..].iter() {
                if let Some(file) = node.widget().write_as::<File<U>>(pa) {
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
    pub(crate) fn nodes(&self) -> impl ExactSizeIterator<Item = &Node<U>> + DoubleEndedIterator {
        struct InnerChain<'a, U: Ui>(InnerIter<'a, U>, usize);

        impl<'a, U: Ui> Iterator for InnerChain<'a, U> {
            type Item = &'a Node<U>;

            fn next(&mut self) -> Option<Self::Item> {
                self.0.next()
            }
        }

        impl<'a, U: Ui> DoubleEndedIterator for InnerChain<'a, U> {
            fn next_back(&mut self) -> Option<Self::Item> {
                self.0.next_back()
            }
        }

        impl<'a, U: Ui> ExactSizeIterator for InnerChain<'a, U> {
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
    pub(crate) fn file_handles(&self, pa: &Pass) -> Vec<Handle<File<U>, U>> {
        let mut files: Vec<Handle<File<U>, U>> = self
            .nodes
            .iter()
            .filter_map(|node| node.try_downcast())
            .collect();

        files.sort_unstable_by_key(|file| file.read(pa).layout_order);

        files
    }

    /// How many [`Widget`]s are in this [`Window`]
    pub(crate) fn len_widgets(&self) -> usize {
        self.nodes.len()
    }
}

impl<U: Ui> std::fmt::Debug for Window<U> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Window")
            .field("nodes", &self.nodes)
            .field("floating", &self.spawned)
            .finish_non_exhaustive()
    }
}

/// Iterator over a group of windows, that returns the window's index
fn window_index_widget<U: Ui>(
    (index, window): (usize, &Window<U>),
) -> impl ExactSizeIterator<Item = (usize, usize, &Node<U>)> + DoubleEndedIterator {
    window
        .nodes()
        .enumerate()
        .map(move |(i, entry)| (index, i, entry))
}

fn window_index_of_area<U: Ui>(pa: &Pass, area: &U::Area, wins: &InnerWindows<U>) -> usize {
    wins.list
        .iter()
        .position(|win| {
            win.master_area.is_master_of(area)
                || win
                    .spawned
                    .iter()
                    .any(|(_, node)| node.area(pa).is_master_of(area))
        })
        .unwrap()
}

/// Runs the [`once`] function of widgets.
///
/// [`once`]: Widget::once
fn run_once<W: Widget<U>, U: Ui>() {
    static ONCE_LIST: Mutex<Vec<&'static str>> = Mutex::new(Vec::new());

    let mut once_list = ONCE_LIST.lock().unwrap();
    if !once_list.contains(&duat_name::<W>()) {
        W::once().unwrap();
        once_list.push(duat_name::<W>());
    }
}

fn get_cache<U: Ui>(
    pa: &mut Pass,
    widget: RwData<dyn Widget<U>>,
    windows: &Windows<U>,
    win: Option<usize>,
) -> <<U as Ui>::Area as Area>::Cache {
    let last_layout_order = if let Some(win) = win {
        windows.inner.read(pa).list[win]
            .file_handles(pa)
            .iter()
            .map(|handle| handle.read(pa).layout_order)
            .max()
    } else {
        windows
            .file_handles(pa)
            .map(|handle| handle.read(pa).layout_order)
            .max()
    };

    if let Some(file) = widget.write_as::<File<U>>(pa) {
        file.layout_order = last_layout_order.map(|lo| lo + 1).unwrap_or(0);
        match Cache::new().load::<<U::Area as Area>::Cache>(file.path()) {
            Ok(cache) => cache,
            Err(err) => {
                context::error!("{err}");
                <U::Area as Area>::Cache::default()
            }
        }
    } else {
        <U::Area as Area>::Cache::default()
    }
}

enum Location {
    OnFiles,
    Regular,
    Spawned(SpawnId),
}

type InnerIter<'a, U: Ui> = impl DoubleEndedIterator<Item = &'a Node<U>>;
