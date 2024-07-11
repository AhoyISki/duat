use std::fmt::Debug;

const LEN_PER_RECORD: usize = 150;

pub trait Record {
    fn bytes(&self) -> usize;

    fn add(self, other: Self) -> Self;

    fn sub(self, other: Self) -> Self;
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
}

#[derive(Default, Clone, PartialEq, Eq)]
pub struct Records<R>
where
    R: Default + Debug + Clone + Copy + Eq + Ord + Record,
{
    last: R,
    max: R,
    pub stored: Vec<R>,
}

impl<R> Records<R>
where
    R: Default + Debug + Clone + Copy + Eq + Ord + Record,
{
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_max(max: R) -> Self {
        Self {
            max,
            ..Self::default()
        }
    }

    pub fn insert(&mut self, new: R) {
        let Err(i) = self.stored.binary_search(&new) else {
            return;
        };
        let prev = i.checked_sub(1).and_then(|i| self.stored.get(i));
        let next = self.stored.get(i + 1);

        if [prev, next]
            .iter()
            .flatten()
            .all(|r| r.bytes().abs_diff(new.bytes()) >= LEN_PER_RECORD)
        {
            self.stored.insert(i, new)
        }

        self.last = new;
    }

    pub fn transform(&mut self, start: R, old_len: R, new_len: R) {
        let (Ok(start_i) | Err(start_i)) = self.stored.binary_search(&start);
        let (Ok(old_end_i) | Err(old_end_i)) = self.stored.binary_search(&start.add(old_len));

        for r in self.stored.iter_mut().skip(old_end_i) {
            *r = r.sub(old_len).add(new_len);
        }

        self.stored.splice(start_i..old_end_i, []);

        self.last = start.add(new_len);
        self.max = self.max.add(new_len).sub(old_len);
    }

    pub fn append(&mut self, r: R) {
        self.transform(self.max, R::default(), r)
    }

    pub fn extend(&mut self, mut other: Records<R>) {
        for r in other.stored.iter_mut() {
            *r = r.add(self.max);
        }

        self.stored.extend(other.stored);
        self.max = self.max.add(other.max);
    }

    pub fn clear(&mut self) {
        self.last = R::default();
        self.max = R::default();
        self.stored = Vec::default();
    }

    pub fn max(&self) -> R {
        self.max
    }

    pub fn closest_to(&self, b: usize) -> R {
        let i = match self.stored.binary_search_by(|r| r.bytes().cmp(&b)) {
            Err(i) => i,
            Ok(i) => return self.stored[i],
        };

        let canditates = {
            let prev = i.checked_sub(1).and_then(|i| self.stored.get(i)).cloned();
            let next = self.stored.get(i).cloned();
            let max = Some(self.max);

            [Some(R::default()), prev, next, max].into_iter().flatten()
        };

        canditates
            .min_by(|lhs, rhs| lhs.bytes().abs_diff(b).cmp(&rhs.bytes().abs_diff(b)))
            .unwrap()
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
