use std::{cell::RefCell, fmt::Debug, rc::Rc};

pub struct Deque<T> {
    length: usize,
    front: Option<Rc<Node<T>>>,
    back: Option<Rc<Node<T>>>,
}
impl<T> Deque<T> {
    pub fn new() -> Self {
        Self {
            length: 0,
            front: None,
            back: None,
        }
    }
    pub fn len(&self) -> usize {
        self.length
    }
    pub fn is_empty(&self) -> bool {
        self.length == 0
    }
    pub fn push_front(&mut self, elem: T) {
        self.length += 1;
        let new_node = Rc::new(Node::from(elem));
        match &self.front {
            None => {
                assert!(self.len() == 1);
                assert!(self.back.is_none());
                self.front = Some(new_node.clone());
                self.back = Some(new_node);
            }
            Some(old_front) => {
                new_node.prev.borrow_mut().insert(old_front.clone());
                old_front.next.borrow_mut().insert(new_node.clone());
                self.front = Some(new_node);
            }
        }
    }
    pub fn push_back(&mut self, elem: T) {
        self.length += 1;
        let new_node = Rc::new(Node::from(elem));
        match &self.back {
            None => {
                assert!(self.len() == 1);
                assert!(self.front.is_none());
                self.front = Some(new_node.clone());
                self.back = Some(new_node);
            }
            Some(old_back) => {
                new_node.next.borrow_mut().insert(old_back.clone());
                old_back.prev.borrow_mut().insert(new_node.clone());
                self.back = Some(new_node);
            }
        }
    }
    pub fn pop_front(&mut self) -> Option<T> {
        match self.front.take() {
            None => {
                assert!(self.is_empty());
                None
            }
            Some(node) => {
                self.length -= 1;
                self.front = node.prev.borrow().clone();
                match node.prev.borrow().as_deref() {
                    // clear the prev node's link to our node
                    Some(prev_node) => prev_node.next.borrow_mut().take(),
                    // if there is no prev node, clear deque's back link to our node
                    None => self.back.take(),
                };
                match Rc::try_unwrap(node) {
                    Ok(n) => Some(n.elem),
                    Err(_) => unreachable!(),
                }
            }
        }
    }
    pub fn pop_back(&mut self) -> Option<T> {
        match self.back.take() {
            None => {
                assert!(self.is_empty());
                None
            }
            Some(node) => {
                self.length -= 1;
                self.back = node.next.borrow().clone();
                match node.next.borrow().as_deref() {
                    // clear the next node's link to our node
                    Some(next_node) => next_node.prev.borrow_mut().take(),
                    // if there is no next node, clear deque's front link to our node
                    None => self.front.take(),
                };
                match Rc::try_unwrap(node) {
                    Ok(n) => Some(n.elem),
                    Err(_) => unreachable!(),
                }
            }
        }
    }
    pub fn peek_front(&self) -> Option<&T> {
        self.peek_front_nth(0)
    }
    pub fn peek_back(&self) -> Option<&T> {
        self.peek_back_nth(0)
    }
    pub fn peek_front_nth(&self, index: usize) -> Option<&T> {
        self.front.as_deref()?.peek_front_nth(index)
    }
    pub fn peek_back_nth(&self, index: usize) -> Option<&T> {
        self.back.as_deref()?.peek_back_nth(index)
    }
}
impl<T: Debug> Debug for Deque<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.back {
            Some(l) => l.fmt(f),
            None => Option::<T>::None.fmt(f),
        }
    }
}
impl<T> Iterator for Deque<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.pop_front()
    }
}
impl<T> DoubleEndedIterator for Deque<T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.pop_back()
    }
}
impl<T> Default for Deque<T> {
    fn default() -> Self {
        Self::new()
    }
}

struct Node<T> {
    elem: T,
    next: RefCell<Option<Rc<Node<T>>>>,
    prev: RefCell<Option<Rc<Node<T>>>>,
}
impl<T> Node<T> {
    fn peek_front_nth(&self, index: usize) -> Option<&T> {
        if index == 0 {
            return Some(&self.elem);
        }
        let ptr = self.prev.as_ptr();
        let ptr_ref = unsafe { ptr.as_ref() };
        ptr_ref
            .unwrap_or_else(|| panic!("Referenced null pointer at index {}", index))
            .as_deref()?
            .peek_front_nth(index - 1)
    }
    fn peek_back_nth(&self, index: usize) -> Option<&T> {
        if index == 0 {
            return Some(&self.elem);
        }
        let ptr = self.next.as_ptr();
        let ptr_ref = unsafe { ptr.as_ref() };
        ptr_ref
            .unwrap_or_else(|| panic!("Referenced null pointer at index {}", index))
            .as_deref()?
            .peek_back_nth(index - 1)
    }
}
impl<T> From<T> for Node<T> {
    fn from(elem: T) -> Self {
        Self {
            elem,
            next: RefCell::new(None),
            prev: RefCell::new(None),
        }
    }
}
impl<T: Debug> Debug for Node<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("")
            .field("elem", &self.elem)
            .field("next", &self.next.borrow())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn prepare_deque() -> Deque<usize> {
        let mut d: Deque<usize> = Deque::new();
        d.push_back(3);
        d.push_back(4);
        d.push_back(5);
        d.push_front(2);
        d.push_front(1);
        d.push_front(0);
        d
    }

    #[test]
    fn deque_push_front_test() {
        let mut d = Deque::<usize>::new();
        d.push_front(1);
        d.push_front(2);
    }
    #[test]
    fn deque_pop_front_test() {
        let mut d = Deque::<usize>::new();
        d.push_front(1);
        d.push_front(2);
        assert_eq!(d.pop_front(), Some(2));
        assert_eq!(d.pop_front(), Some(1));
        assert_eq!(d.pop_front(), None);
    }
    #[test]
    fn deque_push_back_test() {
        let mut d = Deque::<usize>::new();
        d.push_back(1);
        d.push_back(2);
    }
    #[test]
    fn deque_pop_back_test() {
        let mut d = Deque::<usize>::new();
        d.push_back(1);
        d.push_back(2);
        assert_eq!(d.pop_back(), Some(2));
        assert_eq!(d.pop_back(), Some(1));
        assert_eq!(d.pop_back(), None);
    }
    #[test]
    fn deque_pop_front_back_test() {
        let mut d = prepare_deque();
        assert_eq!(d.pop_back(), Some(5));
        assert_eq!(d.pop_front(), Some(0));
        assert_eq!(d.pop_back(), Some(4));
        assert_eq!(d.pop_front(), Some(1));
        assert_eq!(d.pop_back(), Some(3));
        d.push_front(9);
        assert_eq!(d.pop_back(), Some(2));
        assert_eq!(d.pop_back(), Some(9));
        assert_eq!(d.pop_back(), None);
        assert_eq!(d.pop_front(), None);
    }
    #[test]
    fn deque_peek_test() {
        let mut d = Deque::<usize>::new();
        d.push_front(1);
        assert_eq!(d.peek_front(), Some(&1));
        assert_eq!(d.peek_back(), Some(&1));
        d.push_front(2);
        assert_eq!(d.peek_front(), Some(&2));
        assert_eq!(d.peek_back(), Some(&1));
        d.pop_back();
        assert_eq!(d.peek_front(), Some(&2));
        assert_eq!(d.peek_back(), Some(&2));
        d.pop_back();
        assert_eq!(d.peek_front(), None);
        assert_eq!(d.peek_back(), None);
    }
    #[test]
    fn deque_peek_front_nth_test() {
        let d = prepare_deque();
        assert_eq!(d.peek_front_nth(0), Some(&0));
        assert_eq!(d.peek_front_nth(1), Some(&1));
        assert_eq!(d.peek_front_nth(2), Some(&2));
        assert_eq!(d.peek_front_nth(3), Some(&3));
        assert_eq!(d.peek_front_nth(4), Some(&4));
        assert_eq!(d.peek_front_nth(5), Some(&5));
        assert_eq!(d.peek_front_nth(6), None);
    }
    #[test]
    fn deque_peek_back_nth_test() {
        let d = prepare_deque();
        assert_eq!(d.peek_back_nth(0), Some(&5));
        assert_eq!(d.peek_back_nth(1), Some(&4));
        assert_eq!(d.peek_back_nth(2), Some(&3));
        assert_eq!(d.peek_back_nth(3), Some(&2));
        assert_eq!(d.peek_back_nth(4), Some(&1));
        assert_eq!(d.peek_back_nth(5), Some(&0));
        assert_eq!(d.peek_back_nth(6), None);
    }
}
