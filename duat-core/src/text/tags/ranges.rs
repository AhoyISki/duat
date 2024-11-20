use super::RawTag;

#[derive(Clone, PartialEq, Eq)]
pub enum TagRange {
    From(RawTag, u32),
    Until(RawTag, u32),
}

impl TagRange {
    pub fn tag(&self) -> RawTag {
        match self {
            TagRange::From(tag, _) => *tag,
            TagRange::Until(tag, _) => *tag,
        }
    }

    pub fn get_start(&self) -> Option<u32> {
        match self {
            TagRange::From(_, from) => Some(*from),
            TagRange::Until(..) => None,
        }
    }

    pub fn get_end(&self) -> Option<u32> {
        match self {
            TagRange::Until(_, until) => Some(*until),
            TagRange::From(..) => None,
        }
    }

    pub fn start(&self) -> u32 {
        self.get_start().unwrap_or(0)
    }

    pub fn end(&self) -> u32 {
        self.get_end().unwrap_or(u32::MAX)
    }

    pub fn starts_with(&self, (b, s_tag): &(u32, RawTag)) -> bool {
        match self {
            TagRange::From(tag, from) => from == b && *tag == *s_tag,
            TagRange::Until(..) => false,
        }
    }

    pub fn ends_with(&self, (b, e_tag): &(u32, RawTag)) -> bool {
        match self {
            TagRange::Until(s_tag, until) => until == b && *s_tag == *e_tag,
            TagRange::From(..) => false,
        }
    }

    pub fn can_or_does_start_with(&self, (b, s_tag): &(u32, RawTag)) -> bool {
        match self {
            TagRange::Until(e_tag, until) => b <= until && s_tag.ends_with(e_tag),
            TagRange::From(..) => false,
        }
    }

    pub fn can_or_does_end_with(&self, (b, e_tag): &(u32, RawTag)) -> bool {
        match self {
            TagRange::From(s_tag, from) => from <= b && s_tag.ends_with(e_tag),
            TagRange::Until(..) => false,
        }
    }

    pub fn entry(&self) -> (u32, RawTag) {
        match self {
            TagRange::From(tag, from) => (*from, *tag),
            TagRange::Until(tag, until) => (*until, *tag),
        }
    }
}

impl std::fmt::Debug for TagRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TagRange::From(tag, from) => write!(f, "From({tag:?}, {from:?})"),
            TagRange::Until(tag, until) => write!(f, "Until({tag:?}, {until:?})"),
        }
    }
}
