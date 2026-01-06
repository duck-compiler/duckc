use std::{borrow::Borrow, collections::HashMap, hash::Hash};

#[derive(Clone, Debug)]
pub struct MultiMap<K: Eq + Hash, V> {
    inner: HashMap<K, Vec<V>>,
}

impl<K: Eq + Hash, V> Default for MultiMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Eq + Hash, V> MultiMap<K, V> {
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    pub fn insert(&mut self, k: K, v: V) {
        self.inner.entry(k).or_default().push(v);
    }

    pub fn get<'a, Q>(&'a self, k: &Q) -> Vec<&'a V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.inner
            .get(k)
            .map(|v| v.iter().collect())
            .unwrap_or_default()
    }

    pub fn get_mut<'a, Q>(&'a mut self, k: &Q) -> Vec<&'a mut V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.inner
            .get_mut(k)
            .map(|v| v.iter_mut().collect())
            .unwrap_or_default()
    }

    pub fn contains_key<Q>(&self, k: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.inner.contains_key(k.borrow())
    }

    pub fn iter(&self) -> impl Iterator<Item = (&K, Vec<&V>)> {
        self.inner.iter().map(|(k, v)| (k, v.iter().collect()))
    }

    pub fn iter_flat(&self) -> impl Iterator<Item = (&K, &V)> {
        self.inner
            .iter()
            .flat_map(|(k, v)| v.iter().map(move |v| (k, v)))
    }
}

#[cfg(test)]
mod tests {
    use crate::multi_map::MultiMap;

    #[test]
    fn test_multi_map() {
        let mut m = MultiMap::new();
        m.insert("A", "1");
        m.insert("A", "2");

        assert_eq!(
            m.get(&"A")
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>(),
            vec!["1".to_string(), "2".to_string()]
        );

        let expected = vec![("A", "1"), ("A", "2")];
        for pair in m.iter_flat().map(|(k, v)| (*k, *v)) {
            assert!(expected.contains(&pair), "{pair:?}");
        }
    }
}
