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
    /// Returns an empty [`Windows`], for the purpose of having
    /// [`Windows`] exist before anything is setup
    pub(crate) fn new(layout: Box<Mutex<dyn Layout<U>>>, ms: &'static U::MetaStatics) -> Self {
        Self {
            inner: RwData::new(InnerWindows {
                layout,
                list: Vec::new(),
                new_additions: Arc::default(),
            }),
            ms,
        }
    }

    /// Creates a new list of [`Window`]s, with a main one
    /// initialiazed
    pub(crate) fn new_window(&self, pa: &mut Pass, file: File<U>, set_cur: bool) -> Node<U> {
        let win = self.inner.read(pa).list.len();
        let new_additions = self.inner.read(pa).new_additions.clone();
        let (window, node) = Window::new(win, pa, self.ms, file, new_additions);

        let wins = self.inner.write(pa);
        wins.list.push(window);

        if set_cur {
            context::set_cur(pa, node.try_downcast(), node.clone());
        }

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
        (to, to_file, specs): (&U::Area, bool, PushSpecs),
        widget: W,
    ) -> Handle<W, U> {
        self.push(pa, (to, specs), widget, to_file)
            .handle()
            .try_downcast()
            .unwrap()
    }

    /// Spawn a [`Widget`] on a [`Handle`]
    pub(crate) fn spawn_on_widget<W: Widget<U>>(
        &self,
        pa: &mut Pass,
        (on, specs): (&U::Area, SpawnSpecs),
        widget: W,
    ) -> Handle<W, U> {
        let win = window_index_of_area(on, self.inner.read(pa));
        let widget = RwData::new(widget);
        let cache = get_cache(pa, widget.to_dyn_widget(), self, Some(win));
        let spawned = U::Area::spawn(MutArea(on), specs, cache);

        let node = Node::new(widget, Arc::new(spawned));

        let wins = self.inner.write(pa);
        wins.list[win].add(node.clone(), None, Location::Spawned);

        hook::trigger(
            pa,
            WidgetCreated(node.handle().try_downcast::<File<U>>().unwrap()),
        );
        node.handle().try_downcast().unwrap()
    }

    /// Spawns a [`Widget`]
    pub(crate) fn spawn_on_text<W: Widget<U>>(
        &self,
        pa: &mut Pass,
        (on, specs): (SpawnId, SpawnSpecs),
        widget: W,
        win: usize,
    ) -> Handle<W, U> {
        let widget = RwData::new(widget);
        let cache = get_cache(pa, widget.to_dyn_widget(), self, None);
        let spawned = U::new_floating(self.ms, cache, specs, on);

        let node = Node::new(widget, Arc::new(spawned));

        let wins = self.inner.write(pa);
        wins.list[win].add(node.clone(), None, Location::Spawned);

        hook::trigger(
            pa,
            WidgetCreated(node.handle().try_downcast::<File<U>>().unwrap()),
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
        let win = context::cur_window();
        let wins = self.inner.read(pa);
        let (handle, specs) = wins
            .layout
            .lock()
            .unwrap()
            .new_file(pa, win, &file, &wins.list);

        let specs = PushSpecs { cluster: false, ..specs };

        if let Some(master) = handle.area(pa).get_cluster_master() {
            self.push(pa, (&master, specs), file, true)
        } else {
            self.push(pa, (&handle.area, specs), file, true)
        }
    }

    pub(crate) fn close_file(&self, pa: &mut Pass, pk: PathKind) {
        let (win, lhs, nodes) = {
            let (lhs_win, _, lhs) = self.file_entry(pa, pk.clone()).unwrap();
            let lhs = lhs.clone();

            let lo = lhs.read(pa).layout_order;

            let nodes: Vec<Handle<File<U>, U>> = self.inner.read(pa).list[lhs_win]
                .nodes()
                .filter(|n| n.data_is::<File<U>>())
                .skip(lo + 1)
                .filter_map(Node::try_downcast)
                .collect();

            hook::trigger(pa, FileClosed((lhs.clone(), Cache::new())));

            (lhs_win, lhs, nodes)
        };

        for rhs in nodes {
            self.swap(pa, [win, win], [&lhs, &rhs]);
        }

        let mut windows = std::mem::take(&mut self.inner.write(pa).list);

        windows[win].remove_file(pa, pk);
        if windows[win].file_names(pa).is_empty() {
            windows.remove(win);
            U::remove_window(self.ms, win);
            let cur_win = context::cur_window();
            if cur_win > win {
                context::set_cur_window(cur_win - 1);
            }
        }

        let wins = self.inner.write(pa);
        wins.list = windows;
        wins.new_additions.lock().unwrap().get_or_insert_default();
    }

    /// Swaps two [`File`]s, as well as their related [`Widget`]s
    pub(crate) fn swap_files(&self, pa: &mut Pass, lhs: PathKind, rhs: PathKind) {
        let (wins, [lhs_node, rhs_node]) = {
            let (lhs_win, _, lhs_node) = self.file_entry(pa, lhs.clone()).unwrap();
            let (rhs_win, _, rhs_node) = self.file_entry(pa, rhs.clone()).unwrap();
            let lhs_node = lhs_node.clone();
            let rhs_node = rhs_node.clone();
            ([lhs_win, rhs_win], [lhs_node, rhs_node])
        };

        self.swap(pa, wins, [&lhs_node, &rhs_node]);

        let pk = context::fixed_file::<U>(pa).unwrap().read(pa).path_kind();
        if wins[0] != wins[1]
            && let Some(win) = [lhs, rhs].into_iter().position(|n| n == pk)
        {
            context::set_cur_window(win);
            U::switch_window(self.ms, win);
        }
    }

    /// Opens a new [`File`] on a new [`Window`], or moves it there,
    /// if it is already open
    pub(crate) fn open_or_move_to_new_window(
        &self,
        pa: &mut Pass,
        pk: PathKind,
        default_file_cfg: PrintCfg,
    ) {
        match self.file_entry(pa, pk.clone()) {
            Ok((win, _, handle)) if self.get(pa, win).unwrap().file_handles(pa).len() > 1 => {
                // Take the nodes in the original Window
                handle.write(pa).layout_order = 0;

                let nodes = {
                    let mut old_window = self.inner.write(pa).list.remove(win);
                    let nodes = old_window.take_file_and_related_nodes(pa, &handle);
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
                    MutArea(&new_root).swap(handle.area(pa));
                }

                // Delete the new_root, which should be the last "File" in the
                // list of the original Window.
                MutArea(&new_root).delete();

                self.inner
                    .write(pa)
                    .new_additions
                    .lock()
                    .unwrap()
                    .get_or_insert_default();
            }
            // The Handle in question is already in its own window, so no need
            // to move it to another one.
            Ok(_) => {}
            Err(_) => {
                self.new_window(pa, File::new(pk.as_path(), default_file_cfg), false);
            }
        };

        if context::fixed_file::<U>(pa).unwrap().read(pa).path_kind() != pk {
            mode::reset_to_file::<U>(pk, false);
        }

        let new_win = context::windows::<U>().len(pa) - 1;
        context::set_cur_window(new_win);
        U::switch_window(self.ms, new_win);
    }

    /// Pushes a [`Widget`] to the [`Window`]s
    fn push<W: Widget<U>>(
        &self,
        pa: &mut Pass,
        (to, specs): (&U::Area, PushSpecs),
        widget: W,
        on_file: bool,
    ) -> Node<U> {
        run_once::<W, U>();

        let win = self
            .inner
            .read(pa)
            .list
            .iter()
            .position(|window| window.master_area.is_master_of(to))
            .unwrap();

        let widget = RwData::new(widget);
        let cache = get_cache(pa, widget.to_dyn_widget(), self, Some(win));
        let (pushed, parent) = MutArea(to).push(specs, on_file, cache);

        let node = Node::new(widget, Arc::new(pushed));

        let wins = self.inner.write(pa);
        wins.list[win].add(
            node.clone(),
            parent.map(Arc::new),
            if on_file {
                Location::OnFiles
            } else {
                Location::Regular
            },
        );

        hook::trigger(
            pa,
            WidgetCreated(node.handle().try_downcast::<W>().unwrap()),
        );
        node
    }

    /// Swaps two [`File`] widgets
    fn swap(
        &self,
        pa: &mut Pass,
        [lhs_w, rhs_w]: [usize; 2],
        [lhs, rhs]: [&Handle<File<U>, U>; 2],
    ) {
        let rhs_lo = rhs.widget().read(pa).layout_order;
        let lhs_lo = std::mem::replace(&mut lhs.write(pa).layout_order, rhs_lo);

        rhs.widget().write(pa).layout_order = lhs_lo;

        let mut windows = std::mem::take(&mut self.inner.write(pa).list);

        let lhs_nodes = windows[lhs_w].take_file_and_related_nodes(pa, lhs);
        windows[rhs_w].insert_file_nodes(pa, rhs_lo, lhs_nodes);

        let rhs_nodes = windows[rhs_w].take_file_and_related_nodes(pa, rhs);
        windows[lhs_w].insert_file_nodes(pa, lhs_lo, rhs_nodes);

        let wins = self.inner.write(pa);
        wins.list = windows;
        wins.new_additions.lock().unwrap().get_or_insert_default();

        MutArea(lhs.area(pa)).swap(rhs.area(pa));
    }

    ////////// Entry lookup

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
        let handle = context::fixed_file::<U>(pa).unwrap();

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
}

/// A container for a master [`Area`] in Duat
pub struct Window<U: Ui> {
    index: usize,
    nodes: Vec<Node<U>>,
    floating: Vec<Node<U>>,
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
            floating: Vec::new(),
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
            floating: Vec::new(),
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
            Location::Spawned => self.floating.push(node.clone()),
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

    /// Removes all [`Node`]s whose [`Area`]s where deleted
    pub(crate) fn remove_file(&mut self, pa: &Pass, pk: PathKind) {
        let Some(node) = self
            .nodes
            .extract_if(.., |node| {
                node.read_as(pa).map(|f: &File<U>| f.path_kind() == pk) == Some(true)
            })
            .next()
        else {
            unreachable!("This isn't supposed to fail");
        };

        // If this is the case, this means there is only one File left in this
        // Window, so the files_area should be the cluster master of that
        // File.
        if let Some(parent) = MutArea(node.area(pa)).delete()
            && parent == *self.files_area
        {
            let files = self.file_handles(pa);
            let handle = files.first().unwrap();

            let master_area = handle
                .area(pa)
                .get_cluster_master()
                .map(Arc::new)
                .unwrap_or(handle.area.clone());

            self.files_area = master_area;
        }

        let related = node.related_widgets().read(pa);
        for node in self.nodes.extract_if(.., |node| {
            related.iter().any(|(handle, _)| handle == node.handle())
        }) {
            MutArea(node.area(pa)).delete();
        }
    }

    /// Takes all [`Node`]s related to a given [`Node`]
    pub(crate) fn take_file_and_related_nodes(
        &mut self,
        pa: &mut Pass,
        handle: &Handle<File<U>, U>,
    ) -> Vec<Node<U>> {
        let related = handle.related();
        let lo = handle.widget().read(pa).layout_order;

        let related = related.read(pa);
        let nodes = self
            .nodes
            .extract_if(.., |node| {
                related.iter().any(|(handle, _)| handle == node.handle()) || handle == node.handle()
            })
            .collect();

        for node in self.nodes.iter() {
            if let Some(file) = node.widget().write_as::<File<U>>(pa) {
                file.layout_order -= (file.layout_order > lo) as usize;
            }
        }

        nodes
    }

    /// Inserts [`File`] nodes orderly
    pub(crate) fn insert_file_nodes(
        &mut self,
        pa: &mut Pass,
        layout_order: usize,
        nodes: Vec<Node<U>>,
    ) {
        if let Some(i) = self.nodes.iter().position(|node| {
            node.widget()
                .read_as(pa)
                .is_some_and(|f: &File<U>| f.layout_order >= layout_order)
        }) {
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
    pub(crate) fn nodes(&self) -> impl ExactSizeIterator<Item = &Node<U>> + DoubleEndedIterator {
        use std::slice::Iter;
        struct InnerChain<'a, U: Ui>(
            std::iter::Chain<Iter<'a, Node<U>>, Iter<'a, Node<U>>>,
            usize,
        );

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
            self.nodes.iter().chain(self.floating.iter()),
            self.nodes.len() + self.floating.len(),
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
            .field("floating", &self.floating)
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

fn window_index_of_area<U: Ui>(area: &U::Area, wins: &InnerWindows<U>) -> usize {
    wins.list
        .iter()
        .position(|win| win.master_area.is_master_of(area) || *win.master_area == *area)
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
    Spawned,
}
