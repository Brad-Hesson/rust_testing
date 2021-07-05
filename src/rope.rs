use std::{fmt::Debug, str, usize};

pub struct Rope {
    inner: RopeInner,
}
impl Debug for Rope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

impl Rope {
    pub fn new() -> Self {
        Self {
            inner: RopeInner::new(),
        }
    }
    pub fn len(&self) -> usize {
        self.inner.get_weight()
    }
    pub fn get_string(&self) -> String {
        self.inner.get_string()
    }
    pub fn concat(left: Self, right: Self) -> Self {
        Self {
            inner: RopeInner::concat(left.inner, right.inner),
        }
    }

    pub fn split(self, i: usize) -> (Self, Self) {
        let (left, right) = self.inner.split(i);
        (Self { inner: left }, Self { inner: right })
    }
    pub fn insert(&mut self, rope: Self, i: usize) {
        let old = std::mem::replace(self, Self::new());
        let (left, right) = old.split(i);
        let new = Self::concat(Self::concat(left, rope), right);
        let _ = std::mem::replace(self, new);
    }
    pub fn delete(&mut self, i: usize, j: usize) {
        let old = std::mem::replace(self, Self::new());
        let (left, rest) = old.split(i);
        let (_, right) = rest.split(j - i);
        let new = Self::concat(left, right);
        let _ = std::mem::replace(self, new);
    }
}

enum RopeInner {
    Node(Node),
    Leaf(Leaf),
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
        }
    }
}
impl From<String> for Rope {
    fn from(string: String) -> Self {
        Self {
            inner: RopeInner::Leaf(Leaf {
                weight: string.len(),
                string,
            }),
        }
    }
}
impl From<&str> for Rope {
    fn from(string: &str) -> Self {
        Self {
            inner: RopeInner::Leaf(Leaf {
                weight: string.len(),
                string: string.to_string(),
            }),
        }
    }
}

impl RopeInner {
    fn new() -> Self {
        Self::Node(Node {
            left: None,
            right: None,
            weight: 0,
        })
    }
    fn get_weight(&self) -> usize {
        match self {
            Self::Node(node) => node.weight,
            Self::Leaf(leaf) => leaf.weight,
        }
    }
    fn get_string(&self) -> String {
        match self {
            Self::Leaf(leaf) => leaf.string.clone(),
            Self::Node(node) => {
                let get_string_clsr = |r: &Box<Self>| Some(r.get_string());
                let mut left_str = node
                    .left
                    .as_ref()
                    .and_then(get_string_clsr)
                    .unwrap_or_default();
                let right_str = node
                    .right
                    .as_ref()
                    .and_then(get_string_clsr)
                    .unwrap_or_default();
                left_str.push_str(&right_str);
                left_str
            }
        }
    }
    fn concat(left: Self, right: Self) -> Self {
        if left.get_weight() == 0 {
            return right;
        }
        if right.get_weight() == 0 {
            return left;
        }
        Self::Node(Node {
            weight: left.get_weight(),
            left: Some(Box::new(left)),
            right: Some(Box::new(right)),
        })
    }
    fn split(self, i: usize) -> (Self, Self) {
        match self {
            Self::Leaf(leaf) => {
                assert!(i <= leaf.string.len());
                (
                    Self::Leaf(Leaf::new(leaf.string[..i].to_string())),
                    Self::Leaf(Leaf::new(leaf.string[i..].to_string())),
                )
            }
            Self::Node(node) => {
                let weight = node.weight;
                let new_left: Self;
                let new_right: Self;
                if i <= weight {
                    let get_node_clsr = |n: Box<Self>| Some(n.split(i));
                    let (split_left, split_right) =
                        node.left.and_then(get_node_clsr).unwrap_or_default();
                    let outer = *node.right.unwrap_or_default();
                    new_left = split_left;
                    new_right = Self::concat(split_right, outer);
                } else {
                    let get_node_clsr = |n: Box<Self>| Some(n.split(i - weight));
                    let (split_left, split_right) =
                        node.right.and_then(get_node_clsr).unwrap_or_default();
                    let outer = *node.left.unwrap_or_default();
                    new_left = Self::concat(outer, split_left);
                    new_right = split_right;
                }
                (new_left, new_right)
            }
        }
    }
}

#[derive(Debug)]
struct Node {
    left: Option<Box<RopeInner>>,
    right: Option<Box<RopeInner>>,
    weight: usize,
}

#[derive(Debug)]
struct Leaf {
    string: String,
    weight: usize,
}

impl Leaf {
    fn new(string: String) -> Self {
        Self {
            weight: string.len(),
            string,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rope_leaf_split_test() {
        let r = Rope::from("Hello, World!");
        let (left, right) = r.split(5);
        assert_eq!(left.get_string(), "Hello".to_string());
        assert_eq!(right.get_string(), ", World!".to_string());
    }
    #[test]
    fn rope_concat_test() {
        let r1 = Rope::from("Hello");
        let r2 = Rope::from(", ");
        let r3 = Rope::from("World");
        let r4 = Rope::from("!");
        let mut r = Rope::concat(r2, r3);
        r = Rope::concat(r, r4);
        r = Rope::concat(r1, r);
        assert_eq!(r.get_string(), "Hello, World!".to_string())
    }
    #[test]
    fn rope_node_split_test() {
        let r1 = Rope::from("Hello, ");
        let r2 = Rope::from("World!");
        let r = Rope::concat(r1, r2);
        let (left, right) = r.split(6);
        assert_eq!(left.get_string(), "Hello,".to_string());
        assert_eq!(right.get_string(), " World!".to_string());
        let (left, right) = right.split(3);
        assert_eq!(left.get_string(), " Wo".to_string());
        assert_eq!(right.get_string(), "rld!".to_string());
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
}
