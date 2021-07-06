use std::{fmt::Debug, str, usize};

pub struct Rope {
    inner: Box<RopeInner>,
}
impl Debug for Rope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}
impl From<RopeInner> for Rope {
    fn from(ri: RopeInner) -> Self {
        Self {
            inner: Box::new(ri),
        }
    }
}
impl From<String> for Rope {
    fn from(string: String) -> Self {
        Self::from(RopeInner::Leaf(Leaf::from(string)))
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
        Self::from(RopeInner::concat(*left.inner, *right.inner))
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
    pub fn index(&self, index: usize) -> String {
        self.inner.index(index)
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
impl RopeInner {
    fn new() -> Self {
        Self::Node(Node {
            left: None,
            right: None,
            weight: 0,
        })
    }
    fn len(&self) -> usize {
        match self {
            Self::Leaf(leaf) => leaf.weight,
            Self::Node(node) => {
                node.weight
                    + node
                        .right
                        .as_ref()
                        .and_then(|r| Some(r.len()))
                        .unwrap_or_default()
            }
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
        Self::Node(Node {
            weight: left.len(),
            left: Some(Box::new(left)),
            right: Some(Box::new(right)),
        })
    }
    #[allow(unreachable_code)]
    fn split(self, i: usize) -> (Self, Self) {
        let new_left: Self;
        let new_right: Self;
        match self {
            RopeInner::Leaf(leaf) => {
                assert!(i <= leaf.string.len());
                new_left = RopeInner::Leaf(Leaf::from(leaf.string[..i].to_string()));
                new_right = RopeInner::Leaf(Leaf::from(leaf.string[i..].to_string()));
            }
            RopeInner::Node(node) => {
                let weight = node.weight;
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
            }
        }
        (new_left, new_right)
    }
    fn index(&self, index: usize) -> String {
        match self {
            RopeInner::Leaf(leaf) => leaf.string[index..index + 1].to_string(),
            RopeInner::Node(node) => {
                if index < node.weight {
                    node.left
                        .as_ref()
                        .and_then(|r| Some(r.index(index)))
                        .unwrap_or_default()
                } else {
                    node.right
                        .as_ref()
                        .and_then(|r| Some(r.index(index - node.weight)))
                        .unwrap_or_default()
                }
            }
        }
    }
    fn set_string(&mut self, string: String) {
        if let Self::Leaf(leaf) = self {
            leaf.string = string;
        }
    }
}

#[derive(Debug)]
struct Node {
    weight: usize,
    left: Option<Box<RopeInner>>,
    right: Option<Box<RopeInner>>,
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
    #[test]
    fn rope_index_test() {
        let (r, s) = setup_rope();
        let clsr = |i: usize| assert_eq!(r.index(i), s[i..(i + 1)].to_string());
        (0..s.len()).for_each(clsr);
    }
}
