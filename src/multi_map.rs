use std::{borrow::Borrow, collections::HashMap, hash::Hash};

#[derive(Clone, Debug)]
pub struct MultiMap<K: Eq + Hash, V> {
    inner: HashMap<K, Vec<V>>,
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
        let v = self.inner.get(k);
        match v {
            Some(v) => unsafe {
                let ptr = v.as_ptr();
                let mut result = Vec::with_capacity(v.len());
                for i in 0..v.len() {
                    result.push(ptr.add(i).as_ref().unwrap());
                }
                result
            },
            None => Vec::new(),
        }
    }

    pub fn get_mut<'a, Q>(&'a mut self, k: &Q) -> Vec<&'a mut V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let v = self.inner.get_mut(k);
        match v {
            Some(v) => unsafe {
                let ptr = v.as_mut_ptr();
                let mut result = Vec::with_capacity(v.len());
                for i in 0..v.len() {
                    result.push(ptr.add(i).as_mut().unwrap());
                }
                result
            },
            None => Vec::new(),
        }
    }

    pub fn contains_key<Q>(&self, k: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.inner.contains_key(k.borrow())
    }

    pub fn iter(&self) -> impl Iterator<Item = (&K, Vec<&V>)> {
        MultiMapIterator {
            multi_map: self,
            key_idx: 0,
        }
    }

    pub fn iter_flat(&self) -> impl Iterator<Item = (&K, &V)> {
        self.inner
            .iter()
            .flat_map(|(k, v)| v.iter().map(move |v| (k, v)))
    }
}

struct MultiMapIterator<'a, K: Eq + Hash, V> {
    multi_map: &'a MultiMap<K, V>,
    key_idx: usize,
}

impl<'a, K: Eq + Hash, V> Iterator for MultiMapIterator<'a, K, V> {
    type Item = (&'a K, Vec<&'a V>);

    fn next(&mut self) -> Option<Self::Item> {
        let key = self.multi_map.inner.keys().nth(self.key_idx)?;
        let values = self.multi_map.get(key);
        self.key_idx += 1;
        Some((key, values))
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
