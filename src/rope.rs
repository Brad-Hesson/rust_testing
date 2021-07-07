use std::{fmt::Debug, str, usize};

pub struct Rope {
    inner: RopeInner,
}
impl Debug for Rope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}
impl From<RopeInner> for Rope {
    fn from(ri: RopeInner) -> Self {
        Self { inner: ri }
    }
}
impl From<String> for Rope {
    fn from(string: String) -> Self {
        Self::from(RopeInner::Leaf(Box::new(Leaf::from(string))))
    }
}
impl From<&str> for Rope {
    fn from(string: &str) -> Self {
        Self::from(string.to_string())
    }
}
impl Rope {
    pub fn new() -> Self {
        Self::from(RopeInner::new())
    }
    pub fn len(&self) -> usize {
        self.inner.len()
    }
    pub fn get_string(&self) -> String {
        self.inner.get_string()
    }
    pub fn concat(left: Self, right: Self) -> Self {
        Self::from(RopeInner::concat(left.inner, right.inner))
    }
    pub fn split(self, i: usize) -> (Self, Self) {
        let (left, right) = self.inner.split(i);
        (Self::from(left), Self::from(right))
    }
    pub fn insert(&mut self, rope: Self, i: usize) {
        let old = std::mem::replace(self, Self::new());
        let (left, right) = old.split(i);
        let new = Self::concat(Self::concat(left, rope), right);
        drop(std::mem::replace(self, new));
    }
    pub fn delete(&mut self, i: usize, j: usize) {
        let old = std::mem::replace(self, Self::new());
        let (left, rest) = old.split(i);
        let (_, right) = rest.split(j - i);
        let new = Self::concat(left, right);
        drop(std::mem::replace(self, new));
    }
    pub fn index(&self, index: usize) -> &str {
        self.inner.index(index)
    }
}

#[derive(Debug)]
struct Node {
    weight: usize,
    left: RopeInner,
    right: RopeInner,
}

#[derive(Debug)]
struct Leaf {
    string: String,
    weight: usize,
}
impl From<String> for Leaf {
    fn from(string: String) -> Self {
        Self {
            weight: string.len(),
            string,
        }
    }
}

enum RopeInner {
    Node(Box<Node>),
    Leaf(Box<Leaf>),
    None,
}
impl Default for RopeInner {
    fn default() -> Self {
        Self::new()
    }
}
impl Debug for RopeInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RopeInner::Node(node) => node.fmt(f),
            RopeInner::Leaf(leaf) => leaf.fmt(f),
            RopeInner::None => f.debug_struct("None").finish(),
        }
    }
}
impl RopeInner {
    fn new() -> Self {
        Self::None
    }
    fn len(&self) -> usize {
        match self {
            Self::None => 0,
            Self::Leaf(leaf) => leaf.weight,
            Self::Node(node) => node.weight + node.right.len(),
        }
    }
    fn get_string(&self) -> String {
        match self {
            &Self::None => String::new(),
            Self::Leaf(leaf) => leaf.string.clone(),
            Self::Node(node) => {
                let mut left_str = node.left.get_string();
                let right_str = node.right.get_string();
                left_str.push_str(&right_str);
                left_str
            }
        }
    }
    fn concat(left: Self, right: Self) -> Self {
        if let Self::None = left {
            return right;
        }
        if let Self::None = right {
            return left;
        }
        Self::Node(Box::new(Node {
            weight: left.len(),
            left,
            right,
        }))
    }
    fn split(self, index: usize) -> (Self, Self) {
        assert!(
            index <= self.len(),
            "index {} out of bounds of Rope with length {}",
            index,
            self.len()
        );
        if index == 0 {
            return (Self::None, self);
        }
        if index == self.len() {
            return (self, Self::None);
        }
        match self {
            RopeInner::Leaf(leaf) => {
                assert!(index < leaf.string.len());
                (
                    RopeInner::Leaf(Box::new(Leaf::from(leaf.string[..index].to_string()))),
                    RopeInner::Leaf(Box::new(Leaf::from(leaf.string[index..].to_string()))),
                )
            }
            RopeInner::Node(node) => {
                if index <= node.weight {
                    let (split_left, split_right) = node.left.split(index);
                    (split_left, Self::concat(split_right, node.right))
                } else {
                    let (split_left, split_right) = node.right.split(index - node.weight);
                    (Self::concat(node.left, split_left), split_right)
                }
            }
            RopeInner::None => unreachable!(),
        }
    }
    fn index(&self, index: usize) -> &str {
        assert!(
            index < self.len(),
            "index {} out of bounds of Rope with length {}",
            index,
            self.len()
        );
        match self {
            RopeInner::Leaf(leaf) => &leaf.string[index..index + 1],
            RopeInner::Node(node) => {
                if index < node.weight {
                    node.left.index(index)
                } else {
                    node.right.index(index - node.weight)
                }
            }
            RopeInner::None => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn setup_rope() -> (Rope, String) {
        let r1 = Rope::from("Hello ");
        let r2 = Rope::from("my ");
        let r3 = Rope::from("na");
        let r4 = Rope::from("me i");
        let r5 = Rope::from("s");
        let r6 = Rope::from(" Simon");

        let r_1 = Rope::concat(r1, r2);
        let r_2 = Rope::concat(r3, r4);
        let r_3 = Rope::concat(r5, r6);
        let r = Rope::concat(r_2, r_3);
        (Rope::concat(r_1, r), "Hello my name is Simon".to_string())
    }

    #[test]
    fn rope_leaf_split_test() {
        let r = Rope::from("Hello, World!");
        let (left, right) = r.split(5);
        assert_eq!(left.get_string(), "Hello");
        assert_eq!(right.get_string(), ", World!");
    }
    #[test]
    fn rope_concat_and_return_test() {
        let (r, s) = setup_rope();
        assert_eq!(r.len(), 22);
        assert_eq!(r.get_string(), s)
    }
    #[test]
    fn rope_node_split_test() {
        let (_, s) = setup_rope();

        let clsr = |i: usize| {
            let (r, s) = setup_rope();
            assert_eq!(r.split(i).1.split(1).0.get_string(), s[i..i + 1]);

            let (r, _) = setup_rope();
            assert_eq!(r.split(i + 1).0.split(i).1.get_string(), s[i..i + 1]);
        };

        (0..s.len()).for_each(clsr);
    }
    #[test]
    fn rope_insert_test() {
        let mut r = Rope::from("Hello, World!");
        r.insert(Rope::from("Good "), 7);
        assert_eq!(r.get_string(), "Hello, Good World!".to_string());
    }
    #[test]
    fn rope_delete_test() {
        let mut r = Rope::from("Hello, World!");
        r.delete(5, 12);
        assert_eq!(r.get_string(), "Hello!".to_string());
    }
    #[test]
    fn rope_index_test() {
        let (r, s) = setup_rope();
        let clsr = |i: usize| assert_eq!(r.index(i), s[i..(i + 1)].to_string());
        (0..s.len()).for_each(clsr);
    }
}
