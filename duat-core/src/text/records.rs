use std::fmt::Debug;

const LEN_PER_RECORD: usize = 150;

pub trait Record: Debug + 'static {
    fn bytes(&self) -> usize;

    fn add(self, other: Self) -> Self;

    fn sub(self, other: Self) -> Self;

    fn is_zero_len(&self) -> bool;
}

impl Record for (usize, usize) {
    fn bytes(&self) -> usize {
        self.1
    }

    fn add(self, other: Self) -> Self {
        (self.0 + other.0, self.1 + other.1)
    }

    fn sub(self, other: Self) -> Self {
        (self.0 - other.0, self.1 - other.1)
    }

    fn is_zero_len(&self) -> bool {
        self.0 == 0
    }
}

impl Record for (usize, usize, usize) {
    fn bytes(&self) -> usize {
        self.0
    }

    fn add(self, other: Self) -> Self {
        (self.0 + other.0, self.1 + other.1, self.2 + other.2)
    }

    fn sub(self, other: Self) -> Self {
        (self.0 - other.0, self.1 - other.1, self.2 - other.2)
    }

    fn is_zero_len(&self) -> bool {
        false
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Records<R>
where
    R: Default + Debug + Clone + Copy + Eq + Ord + Record + 'static,
{
    last: (usize, R),
    max: R,
    pub stored: Vec<R>,
}

impl<R> Records<R>
where
    R: Default + Debug + Clone + Copy + Eq + Ord + Record + 'static,
{
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_max(max: R) -> Self {
        Self {
            max,
            stored: vec![max],
            ..Self::default()
        }
    }

    pub fn insert(&mut self, new: R) {
        // For internal functions, I assume that I'm not
        // going over self.max.
        let (i, prev) = self.search_from((0, R::default()), new.bytes(), Record::bytes);
        let len = *self.stored.get(i.min(self.stored.len() - 1)).unwrap();

        // If the recrds would be too close, don't add any
        if [prev, prev.add(len)]
            .iter()
            .any(|rec| rec.bytes().abs_diff(new.bytes()) < LEN_PER_RECORD)
        {
            return;
        }

        if let Some(rec) = self.stored.get_mut(i) {
            *rec = new.sub(prev);
            self.stored.insert(i + 1, len.sub(new.sub(prev)));
            self.last = (i + 1, new);
        }
    }

    pub fn transform(&mut self, start: R, old_len: R, new_len: R) {
        let (s_i, s_rec) = self.search_from((0, R::default()), start.bytes(), Record::bytes);
        let (e_i, e_rec) =
            self.search_from((s_i, s_rec), start.bytes() + old_len.bytes(), Record::bytes);
        let e_len = self.stored.get(e_i).cloned().unwrap_or_default();

        if s_i < e_i {
            self.stored
                .drain((s_i + 1)..(e_i + 1).min(self.stored.len()));
        }

        // Transformation of the beginning len.
        let (len, trans_i) = if let Some(len) = self.stored.get_mut(s_i) {
            *len = new_len.add(e_rec.add(e_len).sub(old_len)).sub(s_rec);

            (*len, s_i)
        } else {
            let last = self.stored.last_mut().unwrap();
            *last = last.add(new_len).sub(old_len);
            
            let trans_i = self.stored.len() - 1;
            (*self.stored.last_mut().unwrap(), trans_i)
        };

        // Removing it if's len is zero.
        if let Some(s_i) = trans_i.checked_sub(1)
            && new_len.is_zero_len()
        {
            let prev = self.stored.get_mut(s_i).unwrap();

            self.last = (s_i, s_rec.sub(*prev));

            *prev = prev.add(len);
            self.stored.remove(s_i + 1);
        } else {
            self.last = (trans_i + 1, s_rec.add(len));
        }

        self.max = self.max.add(new_len).sub(old_len);
    }

    pub fn append(&mut self, r: R) {
        self.transform(self.max, R::default(), r)
    }

    pub fn extend(&mut self, other: Records<R>) {
        self.stored.extend(other.stored);
        self.max = self.max.add(other.max);
    }

    pub fn clear(&mut self) {
        self.last = (0, R::default());
        self.max = R::default();
        self.stored = vec![R::default()];
    }

    pub fn max(&self) -> R {
        self.max
    }

    pub fn closest_to(&self, b: usize) -> R {
        let (i, rec) = self.search_from((0, R::default()), b, Record::bytes);
        let len = self.stored.get(i).cloned().unwrap_or(R::default());

        if rec.bytes().abs_diff(b) > len.add(rec).bytes().abs_diff(b) {
            len.add(rec)
        } else {
            rec
        }
    }

    pub fn closest_to_by(&self, at: usize, by: impl Fn(&R) -> usize + Copy) -> R {
        let (i, rec) = self.search_from((0, R::default()), at, by);
        let len = self.stored.get(i).cloned().unwrap_or(R::default());

        if by(&rec).abs_diff(at) > by(&len.add(rec)).abs_diff(at) {
            len.add(rec)
        } else {
            rec
        }
    }

    fn search_from(
        &self,
        from: (usize, R),
        at: usize,
        by: impl Fn(&R) -> usize + Copy,
    ) -> (usize, R) {
        let (n, mut prev) = {
            [
                (0, R::default()),
                (self.stored.len(), self.max),
                from,
                self.last,
            ]
            .iter()
            .min_by(|(_, lhs), (_, rhs)| by(lhs).abs_diff(at).cmp(&by(rhs).abs_diff(at)))
            .cloned()
            .unwrap()
        };

        if at >= by(&prev) {
            self.stored[n..].iter().enumerate().find_map(|(i, len)| {
                prev = prev.add(*len);
                (by(&prev) > at).then_some((n + i, prev.sub(*len)))
            })
        } else {
            self.stored[..n]
                .iter()
                .enumerate()
                .rev()
                .find_map(|(i, len)| {
                    prev = prev.sub(*len);
                    (by(&prev) <= at).then_some((i, prev))
                })
        }
        .unwrap_or((self.stored.len(), self.max))
    }
}

impl<R: Default> Default for Records<R>
where
    R: Default + Debug + Clone + Copy + Eq + Ord + Record,
{
    fn default() -> Self {
        Self {
            last: (0, R::default()),
            max: R::default(),
            stored: vec![R::default()],
        }
    }
}

impl<R: Debug> Debug for Records<R>
where
    R: Default + Debug + Clone + Copy + Eq + Ord + Record,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Records")
            .field("last", &format!("{:?}", self.last))
            .field("max", &format!("{:?}", self.max))
            .field("stored", &format!("{:?}", self.stored))
            .finish()
    }
}
