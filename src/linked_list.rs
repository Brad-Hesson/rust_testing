#![allow(dead_code)]
#[derive(Debug)]
pub struct Node<T> {
    elem: T,
    link: Option<Box<Node<T>>>
}

#[derive(Debug)]
pub struct List<T> {
    head: Option<Box<Node<T>>>
}

impl<T> List<T>{
    pub fn new() -> Self {
        Self { head: Option::None }
    }

    pub fn push(&mut self, item: T) {
        self.head = Some(Box::new(Node{elem: item, link: self.head.take()}));
    }

    pub fn pop(&mut self) -> Option<T> {
        if let Some(node) = self.head.take(){
            self.head = node.link;
            Some(node.elem)
        } else {
            None
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn list_test() {
        let mut l = List::<i32>::new();
        l.push(1);
        l.push(2);
        l.push(3);
        assert_eq!(l.pop(), Some(3));
        assert_eq!(l.pop(), Some(2));
        assert_eq!(l.pop(), Some(1));
        assert_eq!(l.pop(), None);
    }
}