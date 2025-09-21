use std::any::type_name;

pub use self::{
    builder::{BuildInfo, BuilderDummy, RawUiBuilder, UiBuilder, WidgetAlias},
    id::{AreaId, GetAreaId},
};
use super::{Area, Node, Ui, Widget, layout::Layout};
use crate::{
    context::{self, Cache, Handle},
    data::{Pass, RwData},
    file::{File, FileCfg, PathKind},
    hook::{self, FileClosed, WidgetCreated, WindowCreated},
    mode,
    text::{Text, txt},
    ui::{MutArea, PushSpecs, WidgetCfg},
};

mod builder;

/// A list of all [`Window`]s in Duat
pub struct Windows<U: Ui>(RwData<InnerWindows<U>>);

impl<U: Ui> Windows<U> {
    /// Returns an empty [`Windows`], for the purpose of having
    /// [`Windows`] exist before anything is setup
    pub(crate) fn new() -> Self {
        Self(RwData::new(InnerWindows {
            windows: Vec::new(),
            areas: Vec::new(),
        }))
    }

    /// Creates a new list of [`Window`]s, with a main one
    /// initialiazed
    pub(crate) fn new_window(
        &self,
        pa: &mut Pass,
        ms: &'static U::MetaStatics,
        file_cfg: FileCfg<U>,
        layout: Box<dyn Layout<U>>,
        set_cur: bool,
    ) -> Node<U> {
        let widget_id = AreaId::new();
        let (file_cfg, builder) = {
            let wc = hook::trigger(
                pa,
                WidgetCreated::<File<U>, U>((
                    Some(file_cfg),
                    UiBuilder::new_main(self.0.read(pa).windows.len(), widget_id),
                )),
            );
            (wc.0.0.unwrap(), wc.0.1)
        };

        let (file, _) = file_cfg.build(pa, BuildInfo::for_main());
        let (window, node) = Window::new(pa, ms, file, widget_id, layout);

        let area = node.area(pa);
        let inner = self.0.write(pa);
        inner.windows.push(window);
        inner.areas.push((node.area_id(), area.clone()));

        if set_cur {
            context::set_cur(pa, node.try_downcast(), node.clone());
        }

        let files_id = builder.finish_around_widget(pa, None, node.handle().clone());

        let inner = self.0.write(pa);
        let builder = UiBuilder::<U>::new_window(inner.windows.len() - 1);
        let WindowCreated(builder) = hook::trigger(pa, WindowCreated(builder));
        builder.finish_around_window(pa, files_id);

        node
    }

    /// Pushes a [`File`] to the file's parent
    ///
    /// This function will push to the edge of `self.files_parent`
    /// This is an area, usually in the center, that contains all
    /// [`File`]s, and their associated [`Widget`]s,
    /// with others being at the perifery of this area.
    pub(crate) fn new_file(
        &self,
        pa: &mut Pass,
        win: usize,
        file_cfg: FileCfg<U>,
    ) -> Result<Node<U>, Text> {
        let widget_id = AreaId::new();
        let (file_cfg, builder) = {
            let wc = hook::trigger(
                pa,
                WidgetCreated::<File<U>, U>((Some(file_cfg), UiBuilder::new_main(win, widget_id))),
            );
            (wc.0.0.unwrap(), wc.0.1)
        };

        let (mut file, _) = file_cfg.build(pa, BuildInfo::for_main());

        let window = &self.0.read(pa).windows[win];
        let file_handles = window.file_handles(pa);
        file.layout_order = file_handles.len();

        let window = &mut self.0.write(pa).windows[win];
        let (handle, specs) = window.layout.new_file(&file, file_handles)?;

        let master_id = {
            let master_area = handle
                .area(pa)
                .get_cluster_master()
                .unwrap_or(handle.area(pa).clone());

            let id_of = |(id, area): &(_, U::Area)| (*area == master_area).then_some(*id);
            self.0.read(pa).areas.iter().find_map(id_of).unwrap()
        };

        let (node, _) = self.push(
            pa,
            win,
            (file, specs),
            (master_id, widget_id),
            (false, true),
        );

        builder.finish_around_widget(pa, None, node.handle().clone());

        Ok(node)
    }

    pub(crate) fn close_file(&self, pa: &mut Pass, pk: PathKind, ms: &'static U::MetaStatics) {
        let (win, lhs, nodes) = {
            let (lhs_win, _, lhs) = self.file_entry(pa, pk.clone()).unwrap();
            let lhs = lhs.clone();

            let lo = lhs.read(pa).layout_order;

            let nodes: Vec<Handle<File<U>, U>> = self.0.read(pa).windows[lhs_win]
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

        let mut windows = std::mem::take(&mut self.0.write(pa).windows);

        windows[win].remove_file(pa, pk);
        if windows[win].file_names(pa).is_empty() {
            windows.remove(win);
            U::remove_window(ms, win);
            let cur_win = context::cur_window();
            if cur_win > win {
                context::set_cur_window(cur_win - 1);
            }
        }

        self.0.write(pa).windows = windows;
    }

    /// Swaps two [`File`]s, as well as their related [`Widget`]s
    pub(crate) fn swap_files(
        &self,
        pa: &mut Pass,
        lhs: PathKind,
        rhs: PathKind,
        ms: &'static U::MetaStatics,
    ) {
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
            U::switch_window(ms, win);
        }
    }

    /// Opens a new [`File`] on a new [`Window`], or moves it there,
    /// if it is already open
    pub(crate) fn open_or_move_to_new_window(
        &self,
        pa: &mut Pass,
        pk: PathKind,
        ms: &'static U::MetaStatics,
        layout: Box<dyn Layout<U>>,
        default_file_cfg: FileCfg<U>,
    ) {
        match self.file_entry(pa, pk.clone()) {
            Ok((win, _, handle)) if self.get(pa, win).unwrap().file_handles(pa).len() > 1 => {
                // Take the nodes in the original Window
                handle.write(pa).layout_order = 0;

                let nodes = {
                    let mut old_window = self.0.write(pa).windows.remove(win);
                    let nodes = old_window.take_file_and_related_nodes(pa, &handle);
                    self.0.write(pa).windows.insert(win, old_window);

                    nodes
                };

                // Create a new Window Swapping the new root with files_area
                let new_root = U::new_root(ms, <U::Area as Area>::Cache::default());
                U::Area::swap(MutArea(handle.area(pa)), &new_root);
                let window = Window::<U>::from_raw(handle.area(pa).clone(), nodes, layout);

                self.0.write(pa).windows.push(window);

                // Swap the Files ahead of the swapped new_root
                let lo = handle.read(pa).layout_order;

                for handle in &self.0.read(pa).windows[win].file_handles(pa)[lo..] {
                    MutArea(&new_root).swap(handle.area(pa));
                }

                // Delete the new_root, which should be the last "File" in the
                // list of the original Window.
                MutArea(&new_root).delete();
            }
            // The Handle in question is already in its own window, so no need
            // to move it to another one.
            Ok(_) => {}
            Err(_) => {
                self.new_window(
                    pa,
                    ms,
                    default_file_cfg.open_path(pk.as_path()),
                    layout,
                    false,
                );
            }
        };

        if context::fixed_file::<U>(pa).unwrap().read(pa).path_kind() != pk {
            mode::reset_to_file::<U>(pk, false);
        }

        let new_win = context::windows::<U>().len(pa) - 1;
        context::set_cur_window(new_win);
        U::switch_window(ms, new_win);
    }

    /// Pushes a [`Widget`] to the [`Window`]s
    fn push<W: Widget<U>>(
        &self,
        pa: &mut Pass,
        win: usize,
        (widget, specs): (W, PushSpecs),
        (to_id, widget_id): (AreaId, AreaId),
        (do_cluster, on_files): (bool, bool),
    ) -> (Node<U>, Option<AreaId>) {
        fn push_and_get_areas<U: Ui>(
            windows: &Windows<U>,
            pa: &mut Pass,
            (widget, specs): (RwData<dyn Widget<U> + 'static>, PushSpecs),
            (to_id, widget_id): (AreaId, AreaId),
            (do_cluster, on_files): (bool, bool),
        ) -> (U::Area, Option<(AreaId, U::Area)>) {
            let area = windows.0.read(pa).find_area(to_id);

            let cache = if let Some(file) = widget.read_as::<File<U>>(pa) {
                match Cache::new().load::<<U::Area as Area>::Cache>(file.path()) {
                    Ok(cache) => cache,
                    Err(err) => {
                        context::error!("{err}");
                        <U::Area as Area>::Cache::default()
                    }
                }
            } else {
                <U::Area as Area>::Cache::default()
            };

            let (widget_area, parent_area) =
                MutArea(area).bisect(specs, do_cluster, on_files, cache);

            let parent = parent_area.map(|a| (AreaId::new(), a));

            let areas = &mut windows.0.write(pa).areas;
            areas.push((widget_id, widget_area.clone()));
            areas.extend(parent.clone());

            (widget_area, parent)
        }

        let widget = RwData::new(widget);

        let (widget_area, parent) = push_and_get_areas(
            self,
            pa,
            (widget.to_dyn_widget(), specs),
            (to_id, widget_id),
            (do_cluster, on_files),
        );

        let (parent_id, parent_area) = parent.unzip();

        let node = self.0.write(pa).windows[win].add::<W>(
            widget,
            parent_area,
            (widget_id, widget_area),
            on_files,
        );

        (node, parent_id)
    }

    fn swap(
        &self,
        pa: &mut Pass,
        [lhs_w, rhs_w]: [usize; 2],
        [lhs, rhs]: [&Handle<File<U>, U>; 2],
    ) {
        let rhs_lo = rhs.widget().read(pa).layout_order;
        let lhs_lo = std::mem::replace(&mut lhs.write(pa).layout_order, rhs_lo);

        rhs.widget().write(pa).layout_order = lhs_lo;

        let mut windows = std::mem::take(&mut self.0.write(pa).windows);

        let lhs_nodes = windows[lhs_w].take_file_and_related_nodes(pa, lhs);
        windows[rhs_w].insert_file_nodes(pa, rhs_lo, lhs_nodes);

        let rhs_nodes = windows[rhs_w].take_file_and_related_nodes(pa, rhs);
        windows[lhs_w].insert_file_nodes(pa, lhs_lo, rhs_nodes);

        self.0.write(pa).windows = windows;

        MutArea(lhs.area(pa)).swap(rhs.area(pa));
    }

    ////////// Querying functions

    /// The number of open [`Window`]s
    ///
    /// Should never be 0, as that is not a valid state of affairs.
    pub fn len(&self, pa: &Pass) -> usize {
        self.0.read(pa).windows.len()
    }

    /// get's the `win`th [`Window`]
    pub fn get<'a>(&'a self, pa: &'a Pass, win: usize) -> Option<&'a Window<U>> {
        self.0.read(pa).windows.get(win)
    }

    /// An entry for a file with the given name
    pub fn file_entry(
        &self,
        pa: &Pass,
        pk: PathKind,
    ) -> Result<(usize, usize, Handle<File<U>, U>), Text> {
        self.0
            .read(pa)
            .windows
            .iter()
            .enumerate()
            .flat_map(window_index_widget)
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
        self.0
            .read(pa)
            .windows
            .iter()
            .enumerate()
            .flat_map(window_index_widget)
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

        if let Some(handle) = handle.get_related::<W>(pa) {
            self.0
                .read(pa)
                .windows
                .iter()
                .enumerate()
                .flat_map(window_index_widget)
                .find(|(.., n)| n.ptr_eq(handle.widget()))
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
        let windows = &self.0.read(pa).windows;

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
        let windows = &self.0.read(pa).windows;

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

    /// Returns an [`Iterator`] over the [`Handle`]s of Duat
    pub fn handles<'a>(
        &'a self,
        pa: &'a Pass,
    ) -> impl Iterator<Item = &'a Handle<dyn Widget<U>, U>> {
        self.0
            .read(pa)
            .windows
            .iter()
            .flat_map(|w| w.nodes().map(|n| n.handle()))
    }

    /// Iterates over all [`Handle<File>`]s in Duat
    pub fn file_handles<'a>(
        &'a self,
        pa: &'a Pass,
    ) -> impl Iterator<Item = Handle<File<U>, U>> + 'a {
        self.0
            .read(pa)
            .windows
            .iter()
            .flat_map(|w| w.file_handles(pa))
    }

    /// Iterates over all widget entries, with window and widget
    /// indices, in that order
    pub(crate) fn entries<'a>(
        &'a self,
        pa: &'a Pass,
    ) -> impl Iterator<Item = (usize, usize, &'a Node<U>)> {
        self.0
            .read(pa)
            .windows
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
}

/// Inner holder of [`Window`]s
struct InnerWindows<U: Ui> {
    windows: Vec<Window<U>>,
    areas: Vec<(AreaId, U::Area)>,
}

impl<U: Ui> InnerWindows<U> {
    /// Finds the [`Ui::Area`] of a given [`AreaId`]
    fn find_area(&self, id: AreaId) -> &U::Area {
        self.areas
            .iter()
            .find_map(|(lhs, area)| (*lhs == id).then_some(area))
            .unwrap()
    }
}

/// A container for a master [`Area`] in Duat
pub struct Window<U: Ui> {
    nodes: Vec<Node<U>>,
    _floating: Vec<(U::Area, Node<U>)>,
    files_area: U::Area,
    layout: Box<dyn Layout<U>>,
}

impl<U: Ui> Window<U> {
    /// Returns a new instance of [`Window`]
    fn new<W: Widget<U>>(
        pa: &mut Pass,
        ms: &'static U::MetaStatics,
        widget: W,
        widget_id: AreaId,
        layout: Box<dyn Layout<U>>,
    ) -> (Self, Node<U>) {
        let widget = RwData::new(widget);

        let cache = widget
            .read_as(pa)
            .and_then(|f: &File<U>| Cache::new().load::<<U::Area as Area>::Cache>(f.path()).ok())
            .unwrap_or_default();

        let area = U::new_root(ms, cache);

        let node = Node::new::<W>(widget, area.clone(), widget_id);

        let window = Self {
            nodes: vec![node.clone()],
            _floating: Vec::new(),
            files_area: area.clone(),
            layout,
        };

        (window, node)
    }

    /// Returns a new [`Window`] from raw elements
    pub(crate) fn from_raw(
        files_area: U::Area,
        nodes: Vec<Node<U>>,
        layout: Box<dyn Layout<U>>,
    ) -> Self {
        let files_area = files_area.get_cluster_master().unwrap_or(files_area);
        Self {
            nodes,
            _floating: Vec::new(),
            files_area,
            layout,
        }
    }

    ////////// Widget pushing functions

    /// Pushes a [`Widget`] onto an existing one
    fn add<W: Widget<U>>(
        &mut self,
        widget: RwData<W>,
        parent_area: Option<U::Area>,
        (widget_id, widget_area): (AreaId, U::Area),
        on_files: bool,
    ) -> Node<U> {
        self.nodes
            .push(Node::new::<W>(widget, widget_area, widget_id));

        if let Some(parent) = &parent_area
            && on_files
            && parent.is_master_of(&self.files_area)
        {
            self.files_area = parent.clone();
        }

        self.nodes.last().unwrap().clone()
    }

    pub(crate) fn _spawn<W: Widget<U>>(&mut self, _pa: &mut Pass, _widget: W) {}

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
            && parent == self.files_area
        {
            let files = self.file_handles(pa);
            let handle = files.first().unwrap();

            let master_area = handle
                .area(pa)
                .get_cluster_master()
                .unwrap_or(handle.area(pa).clone());

            self.files_area = master_area;
        }

        let related = node.related_widgets().read(pa);
        for node in self
            .nodes
            .extract_if(.., |node| related.contains(node.handle()))
        {
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
            .extract_if(.., |n| related.contains(n.handle()) || n == handle)
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
        self.nodes.iter()
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

pub(super) mod id {
    use std::sync::atomic::{AtomicUsize, Ordering};

    use crate::{context, data::Pass, ui::Ui};

    /// An id for uniquely identifying a [`Ui::Area`]
    ///
    /// > [!NOTE]
    /// >
    /// > If you acquired an [`AreaId`] within a [`WidgetCreated`] or
    /// > [`WindowCreated`] [hook], by calling [`UiBuilder::push`] or
    /// > [`UiBuilder::push_to`], *the id's area doesn't currently
    /// > exist*. The [`Ui::Area`] will be created _after_ the [hook]
    /// > takes place, but you can still use the [`AreaId`] for
    /// > comparison purpuses with [`Handle`]s and other such things
    ///
    /// [`Ui::Area`]: super::Ui::Area
    /// [`WidgetCreated`]: crate::hook::WidgetCreated
    /// [`WindowCreated`]: crate::hook::WindowCreated
    /// [hook]: crate::hook
    /// [`UiBuilder::push`]: super::UiBuilder::push
    /// [`UiBuilder::push_to`]: super::UiBuilder::push_to
    /// [`Handle`]: crate::context::Handle
    #[derive(Clone, Copy, Debug, Eq)]
    pub struct AreaId(usize);

    impl AreaId {
        /// Returns a new [`AreaId`]
        pub(in crate::ui) fn new() -> Self {
            static AREA_COUNT: AtomicUsize = AtomicUsize::new(0);
            AreaId(AREA_COUNT.fetch_add(1, Ordering::Relaxed))
        }

        /// The [`Ui::Area`] associated with this [`AreaId`]
        pub fn area<'a, U: Ui>(&self, pa: &'a Pass) -> Option<&'a U::Area> {
            context::windows::<U>()
                .entries(pa)
                .find_map(|(.., node)| (node.area_id() == *self).then(|| node.area(pa)))
        }
    }

    impl<T: GetAreaId> PartialEq<T> for AreaId {
        fn eq(&self, other: &T) -> bool {
            self.0 == other.area_id().0
        }
    }

    /// A generalized method for comparison between types that have
    /// [`AreaId`]s
    pub trait GetAreaId {
        /// The [`AreaId`] of this type
        fn area_id(&self) -> AreaId;
    }

    impl GetAreaId for AreaId {
        fn area_id(&self) -> AreaId {
            *self
        }
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
