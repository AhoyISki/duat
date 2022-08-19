use std::fmt::Display;

use crossterm::style::{Attribute, Attributes, Color, ContentStyle};

use crate::{layout::OutputPos, tags::Form};

#[derive(Clone, Copy)]
pub enum Split {
    Dynamic(f32),
    Static,
}

#[derive(Clone, Copy)]
pub enum Axis {
    Horizontal,
    Vertical,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct ParentId(usize);
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct ChildId(usize);
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum NodeId {
    Parent(ParentId),
    Child(ChildId),
}

pub trait Area {
    fn new() -> Self;

    fn set_form(&mut self, form: Form);

    fn place_main_cursor(&mut self);

    fn place_secondary_cursor(&mut self);

    fn get_len(&mut self, ch: char) -> usize;

    fn print<D>(&mut self, display: D)
    where
        D: Display;

    fn move_to(&mut self, pos: OutputPos);

    fn width(&self) -> usize;

    fn height(&self) -> usize;
}

pub struct ParentNode<A>
where
    A: Area,
{
    area: A,
    parent: Option<ParentId>,
    len: Option<usize>,
    class: String,
    master: bool,

    id: ParentId,
    children: (NodeId, NodeId),
    axis: Axis,
    split: Split,
}

pub struct ChildNode<A>
where
    A: Area,
{
    area: A,
    parent: Option<ParentId>,
    len: Option<usize>,
    class: String,
    master: bool,

    id: ChildId,
    form_stack: Vec<(Form, u16)>,
}

pub enum AreaNode<A>
where
    A: Area,
{
    Child(ChildNode<A>),
    Parent(ParentNode<A>),
}

impl<A> AreaNode<A>
where
    A: Area,
{
    fn len(&mut self) -> &mut Option<usize> {
        match self {
            AreaNode::Child(child) => &mut child.len,
            AreaNode::Parent(parent) => &mut parent.len,
        }
    }

    fn parent(&mut self) -> &mut Option<ParentId> {
        match self {
            AreaNode::Child(child) => &mut child.parent,
            AreaNode::Parent(parent) => &mut parent.parent,
        }
    }

    fn id(&self) -> NodeId {
        match self {
            AreaNode::Child(child) => NodeId::Child(child.id),
            AreaNode::Parent(parent) => NodeId::Parent(parent.id),
        }
    }
}

impl<A> ChildNode<A>
where
    A: Area,
{
    fn get_final_form(&self) -> Form {
        let style = ContentStyle {
            foreground_color: Some(Color::Reset),
            background_color: Some(Color::Reset),
            underline_color: Some(Color::Reset),
            attributes: Attributes::from(Attribute::Reset),
        };

        let mut form = Form { style, is_final: false };

        let (mut fg_done, mut bg_done, mut ul_done, mut attr_done) = (false, false, false, false);

        for &(Form { style, is_final, .. }, _) in &self.form_stack {
            if let Some(color) = style.foreground_color {
                if !fg_done {
                    form.style.foreground_color = Some(color);
                    if is_final {
                        fg_done = true
                    }
                }
            }
            if let Some(color) = style.background_color {
                if !bg_done {
                    form.style.background_color = Some(color);
                    if is_final {
                        bg_done = true
                    }
                }
            }
            if let Some(color) = style.foreground_color {
                if !ul_done {
                    form.style.underline_color = Some(color);
                    if is_final {
                        ul_done = true
                    }
                }
            }
            if !attr_done && !style.attributes.is_empty() {
                form.style.attributes = style.attributes;
                if is_final {
                    attr_done = true
                }
            }

            if fg_done && bg_done && ul_done && attr_done {
                break;
            }
        }

        form
    }

    fn push_form(&mut self, forms: &Vec<Form>, id: u16) {
        self.form_stack.push((forms[id as usize], id));

        let form = self.get_final_form();

        self.area.set_form(form);
    }

    fn pop_form(&mut self, index: u16) {
        if let Some(element) = self.form_stack.iter().enumerate().rfind(|(_, &(_, i))| i == index) {
            self.form_stack.remove(element.0);

            let form = self.get_final_form();

            self.area.set_form(form);
        }
    }
}

#[derive(Clone, Copy)]
pub enum Length {
    Static(usize),
    Ratio(f32),
}

pub trait AreaManager {
    type Area: Area;

    /// Updates a specific area and its decendants.
    fn update_area(&mut self, areas: &mut Vec<AreaNode<Self::Area>>, id: NodeId);

    /// Updates all areas.
    fn update(&mut self, areas: &mut Vec<AreaNode<Self::Area>>);
}

pub enum Direction {
    Top,
    Right,
    Bottom,
    Left,
}

pub struct AreaNodeTree<M>
where
    M: AreaManager,
{
    handler: M,
    areas: Vec<AreaNode<M::Area>>,
    last_id: usize,
}

impl<M> AreaNodeTree<M>
where
    M: AreaManager,
{
    pub fn new(handler: M, area: M::Area, class: String) -> (AreaNodeTree<M>, ChildId) {
        let area_node_tree = AreaNodeTree {
            handler,
            areas: vec![AreaNode::Child(ChildNode {
                area,
                parent: None,
                len: None,
                id: ChildId(0),
                class,
                master: true,
                form_stack: Vec::new(),
            })],
            last_id: 0,
        };

        (area_node_tree, ChildId(0))
    }

    /// Creates a new parent area, containing the old area and another newly created area.
    pub fn push(
        &mut self, id: NodeId, direction: Direction, len: Length, child_class: String,
        parent_class: Option<String>,
    ) -> (ParentId, ChildId) {
        let old_node = self.areas.iter_mut().find(|n| n.id() == id).expect("AreaId doesn't exist!");
        *old_node.parent() = Some(ParentId(self.last_id + 2));
        let (old_id, old_parent) = (old_node.id(), old_node.parent());

        assert!(old_node.len().is_none(), "The original area must be dinamically sized!");

        self.last_id += 1;
        self.areas.push(AreaNode::Child(ChildNode {
            len: if let Length::Static(len) = len { Some(len) } else { None },
            id: ChildId(self.last_id),
            master: true,
            class: child_class,
            area: M::Area::new(),
            parent: Some(ParentId(self.last_id + 1)),
            form_stack: Vec::new(),
        }));

        let (first, second, axis) = match direction {
            Direction::Top => (NodeId::Child(ChildId(self.last_id)), old_id, Axis::Vertical),
            Direction::Right => (old_id, NodeId::Child(ChildId(self.last_id)), Axis::Horizontal),
            Direction::Bottom => (old_id, NodeId::Child(ChildId(self.last_id)), Axis::Vertical),
            Direction::Left => (NodeId::Child(ChildId(self.last_id)), old_id, Axis::Horizontal),
        };

        self.areas.push(AreaNode::Parent(ParentNode {
            len: None,
            id: ParentId(self.last_id + 1),
            master: false,
            class: parent_class.unwrap_or(String::from("parsec-parent")),
            area: M::Area::new(),
            parent: *old_parent,
            axis,
            children: (first, second),
            split: match len {
                Length::Static(_) => Split::Static,
                Length::Ratio(ratio) => Split::Dynamic(ratio),
            },
        }));
        self.last_id += 1;

        self.handler.update(&mut self.areas);

        (ParentId(self.last_id), ChildId(self.last_id - 1))
    }

    pub fn resize(&mut self, id: NodeId, new_len: usize) {
        let node = self.areas.iter_mut().find(|n| n.id() == id).expect("AreaId does not exist!");

        match node.len() {
            Some(ref mut len) => *len = new_len,
            None => panic!("You can only resize areas of static size!"),
        };

        match node.parent() {
            Some(id) => self.handler.update_area(&mut self.areas, NodeId::Parent(*id)),
            None => self.handler.update(&mut self.areas),
        }
    }

    pub fn set_ratio(&mut self, id: ParentId, new_ratio: f32) {
        let node = self.areas.iter_mut().find(|n| n.id() == NodeId::Parent(id)).unwrap();

		if let AreaNode::Parent(parent) = node {
            match parent.split {
                Split::Dynamic(ref mut ratio) => *ratio = new_ratio,
                Split::Static => panic!("You can't set a ratio to a static split!"),
            };        
		}

        self.handler.update_area(&mut self.areas, NodeId::Parent(id));
    }
}
