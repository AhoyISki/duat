use std::{borrow::BorrowMut, cell::RefCell, fmt::Debug};

const LEN_PER_RECORD: usize = 150;

pub trait Record {
    fn bytes(&self) -> usize;

    fn add(self, other: Self) -> Self;

    fn sub(self, other: Self) -> Self;
}

impl Record for (usize, usize) {
    fn bytes(&self) -> usize {
        self.0
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

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Records<T>
where
    T: Default + Debug + Clone + Copy + Eq + Ord + Record,
{
    last: RefCell<T>,
    max: RefCell<T>,
    stored: RefCell<Vec<T>>,
}

impl<T> Records<T>
where
    T: Default + Debug + Clone + Copy + Eq + Ord + Record,
{
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_max(max: T) -> Self {
        Self {
            max: RefCell::new(max),
            ..Self::default()
        }
    }

    pub fn insert(&self, new: T) {
        let mut stored = self.stored.borrow_mut();

        let Err(i) = stored.binary_search(&new) else {
            return;
        };
        let prev = i.checked_sub(1).and_then(|i| stored.get(i));
        let next = stored.get(i + 1);

        if [prev, next]
            .iter()
            .flatten()
            .all(|t| t.bytes().abs_diff(new.bytes()) >= LEN_PER_RECORD)
        {
            stored.insert(i, new)
        }

        self.last.replace(new);
    }

    pub fn transform(&self, start: T, old_end: T, new_end: T) {
        let mut stored = self.stored.borrow_mut();

        let (Ok(start_i) | Err(start_i)) = stored.binary_search(&start);
        let (Ok(old_end_i) | Err(old_end_i)) = stored.binary_search(&old_end);

        stored.splice(start_i..old_end_i, []);

        for t in &mut stored[old_end_i..] {
            *t = t.sub(old_end).add(old_end);
        }

        self.last.replace(new_end);
        self.max.replace_with(|t| t.sub(old_end).add(old_end));
    }

    pub fn append(&self, t: T) {
        self.transform(*self.max.borrow(), T::default(), t)
    }

    pub fn extend(&self, other: Records<T>) {
        let max = *self.max.borrow();

        for r in other.stored.borrow_mut().iter_mut() {
            *r = r.add(max);
        }

        self.stored.borrow_mut().extend(other.stored.take());
        self.max.replace(max.add(*other.max.borrow()));
    }

    pub fn clear(&self) {
        self.last.take();
        self.max.take();
        self.stored.take();
    }

    pub fn max(&self) -> T {
        *self.max.borrow()
    }

    pub fn closest_to(&self, b: usize) -> T {
        let stored = self.stored.take();

        let i = match stored.binary_search_by(|t| t.bytes().cmp(&b)) {
            Err(i) => i,
            Ok(i) => return stored[i],
        };

        let canditates = {
            let prev = i.checked_sub(1).and_then(|i| stored.get(i)).cloned();
            let next = stored.get(i + 1).cloned();
            let max = Some(*self.max.borrow());

            [Some(T::default()), prev, next, max].into_iter().flatten()
        };

        let closest = canditates
            .min_by(|lhs, rhs| lhs.bytes().abs_diff(b).cmp(&rhs.bytes().abs_diff(b)))
            .unwrap();

        self.stored.replace(stored);

        closest
    }
}
