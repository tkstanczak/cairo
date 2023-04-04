use std::borrow::Borrow;
use std::collections::{hash_map, HashSet};
use std::hash::Hash;
use std::ops::Sub;

use crate::unordered_hash_map::UnorderedHashMap;

/// A hash set that does not care about the order of insertion.
/// In particular, it does not support iterating, in order to guarantee deterministic compilation.
/// For an iterable version see [OrderedHashSet](crate::ordered_hash_set::OrderedHashSet).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UnorderedHashSet<Key: Hash + Eq>(HashSet<Key>);

impl<Key: Hash + Eq> UnorderedHashSet<Key> {
    /// Inserts the value into the set.
    ///
    /// If an equivalent item already exists in the set, returns `false`. Otherwise, returns `true`.
    pub fn insert(&mut self, key: Key) -> bool {
        self.0.insert(key)
    }

    /// Extends the set with the content of the given iterator.
    pub fn extend<I: IntoIterator<Item = Key>>(&mut self, iter: I) {
        self.0.extend(iter)
    }

    /// Extends the set with the content of another set.
    pub fn extend_unordered(&mut self, other: Self) {
        self.0.extend(other.0)
    }

    /// Returns true if an equivalent to value exists in the set.
    pub fn contains<Q: ?Sized + Hash + Eq>(&self, value: &Q) -> bool
    where
        Key: Borrow<Q>,
    {
        self.0.contains(value)
    }

    /// Returns the number of elements in the set.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns true if the set contains no elements.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Clears the set, removing all values.
    pub fn clear(&mut self) {
        self.0.clear()
    }

    /// Returns the intersection of two sets.
    pub fn intersect(&self, other: &Self) -> Self
    where
        Key: Clone,
    {
        Self(self.0.intersection(&other.0).map(|x| x.clone()).collect())
    }

    /// Returns the union of two sets.
    pub fn union(&self, other: &Self) -> Self
    where
        Key: Clone,
    {
        Self(self.0.union(&other.0).map(|x| x.clone()).collect())
    }

    /// Verifies that the two sets do not intersect (panics otherwise) and returns their union.
    pub fn disjoint_union(&self, other: &Self) -> Self
    where
        Key: Clone,
    {
        assert!(
            self.0.intersection(&other.0).next().is_none(),
            "disjoint_union: The input sets must not intersect."
        );
        self.union(other)
    }

    /// Returns the intersection of all sets in the given iterator. Returns `None` if the iterator
    /// is empty.
    pub fn intersection<'a>(mut sets: impl Iterator<Item = &'a Self>) -> Option<Self>
    where
        Key: Clone,
        Self: 'a,
    {
        if let Some(first_set) = sets.next() {
            Some(itertools::fold(sets, first_set.clone(), |acc, b| acc.intersect(b)))
        } else {
            None
        }
    }

    /// Returns a map from the union of all sets to the number of sets that contain each element.
    pub fn counts<'a>(sets: impl Iterator<Item = &'a Self>) -> UnorderedHashMap<Key, usize>
    where
        Key: Clone,
        Self: 'a,
    {
        let mut res = UnorderedHashMap::default();
        for set in sets {
            for elm in &set.0 {
                match res.entry(elm.clone()) {
                    hash_map::Entry::Occupied(mut entry) => {
                        entry.insert(*entry.get() + 1);
                    }
                    hash_map::Entry::Vacant(entry) => {
                        entry.insert(1);
                    }
                }
            }
        }
        res
    }
}

impl<Key: Hash + Eq> Default for UnorderedHashSet<Key> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<Key: Hash + Eq> FromIterator<Key> for UnorderedHashSet<Key> {
    fn from_iter<T: IntoIterator<Item = Key>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<'a, Key: Hash + Eq + Clone> Sub<&'a UnorderedHashSet<Key>> for &'a UnorderedHashSet<Key> {
    type Output = UnorderedHashSet<Key>;

    fn sub(self, rhs: Self) -> Self::Output {
        UnorderedHashSet::<Key>(&self.0 - &rhs.0)
    }
}
