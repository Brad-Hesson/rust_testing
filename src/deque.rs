use std::{cell::RefCell, fmt::Debug, rc::Rc};

pub struct Deque<T> {
    front: Option<Rc<RefCell<Node<T>>>>,
    back: Option<Rc<RefCell<Node<T>>>>,
}
impl<T> Deque<T> {
    fn new() -> Self {
        Self {
            front: None,
            back: None,
        }
    }
    pub fn push_front(&mut self, elem: T) {
        // construct a node from the given element
        let mut node = Node::from(elem);
        match &self.front {
            // if the front link is empty
            None => {
                // confirm that the back link is also empty
                assert!(self.back.is_none());
                // construct a new link for the new node
                let link = Rc::new(RefCell::new(node));
                // assign a copy of the link to self's front and back fields
                self.front = Some(link.clone());
                self.back = Some(link);
            }
            // if the deque is not empty
            Some(old_front) => {
                // set the node's "prev link" to a copy of the old front node's link
                node.prev = Some(old_front.clone());
                // construct a link for the new node
                let link = Rc::new(RefCell::new(node));
                // assign a copy of the link to the old front node's next field
                old_front.as_ref().borrow_mut().next = Some(link.clone());
                // assign the link to self's front field
                self.front = Some(link);
            }
        }
    }
    pub fn push_back(&mut self, elem: T) {
        // construct a node from the given element
        let mut node = Node::from(elem);
        match &self.back {
            // if the back link is empty
            None => {
                // confirm that the front link is also empty
                assert!(self.front.is_none());
                // construct a new link for the new node
                let link = Rc::new(RefCell::new(node));
                // assign a copy of the link to self's front and back fields
                self.back = Some(link.clone());
                self.front = Some(link);
            }
            // if the deque is not empty
            Some(old_back) => {
                // set the node's "next link" to a copy of the old back node's link
                node.next = Some(old_back.clone());
                // construct a link for the new node
                let link = Rc::new(RefCell::new(node));
                // assign a copy of the link to the old back node's next field
                old_back.as_ref().borrow_mut().prev = Some(link.clone());
                // assign the link to self's back field
                self.back = Some(link);
            }
        }
    }
    pub fn pop_front(&mut self) -> Option<T> {
        match self.front.take() {
            // if the deque is empty
            None => None,
            // if the deque is not empty
            Some(link) => {
                // clear the previous node's link to us
                match link.as_ref().borrow_mut().prev.as_ref() {
                    // if a previous node exists, set it's "next link" to None
                    Some(prev) => prev.as_ref().borrow_mut().next = None,
                    // otherwise we are the only node, set self's "back link" to None
                    None => self.back = None,
                };
                // set self's "front link" to whatever our "prev link" is
                self.front = link.as_ref().borrow_mut().prev.clone();
                // deconstruct the link into the element it contains and return
                match Rc::try_unwrap(link) {
                    Ok(refcell) => Some(refcell.into_inner().elem),
                    // if Err, we don't have the only reference to link
                    // and something has gone very wrong
                    Err(_) => unreachable!(),
                }
            }
        }
    }
    pub fn pop_back(&mut self) -> Option<T> {
        match self.back.take() {
            // if the deque is empty
            None => None,
            // if the deque is not empty
            Some(link) => {
                // clear the next node's link to us
                match link.as_ref().borrow_mut().next.as_ref() {
                    // if a next node exists, set it's "prev link" to None
                    Some(next) => next.as_ref().borrow_mut().prev = None,
                    // otherwise we are the only node, set self's "front link" to None
                    None => self.front = None,
                };
                // set self's "back link" to whatever our "next link" is
                self.back = link.as_ref().borrow_mut().next.clone();
                // deconstruct the link into the element it contains and return
                match Rc::try_unwrap(link) {
                    Ok(elem) => Some(elem.into_inner().elem),
                    // if Err, we don't have the only reference to link
                    //  and something has gone very wrong
                    Err(_) => unreachable!(),
                }
            }
        }
    }
}
impl<T: Debug> Debug for Deque<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.back {
            Some(l) => l.borrow().fmt(f),
            None => Option::<T>::None.fmt(f),
        }
    }
}
impl<T> Iterator for Deque<T>{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.pop_front()
    }
}
impl<T> DoubleEndedIterator for Deque<T>{
    fn next_back(&mut self) -> Option<Self::Item> {
        self.pop_back()
    }
}

struct Node<T> {
    elem: T,
    next: Option<Rc<RefCell<Node<T>>>>,
    prev: Option<Rc<RefCell<Node<T>>>>,
}
impl<T> From<T> for Node<T> {
    fn from(elem: T) -> Self {
        Self {
            elem,
            next: None,
            prev: None,
        }
    }
}
impl<T: Debug> Debug for Node<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let next = match &self.next {
            Some(l) => Some(l.borrow()),
            None => None,
        };
        f.debug_struct("")
            .field("elem", &self.elem)
            .field("next", &next)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
    }
    #[test]
    fn deque_front_back_pop_test() {
        let mut deq: Deque<i32> = Deque::new();
        deq.push_back(3);
        deq.push_back(4);
        deq.push_back(5);
        deq.push_front(2);
        deq.push_front(1);
        deq.push_front(0);
        assert_eq!(deq.pop_back(), Some(5));
        assert_eq!(deq.pop_front(), Some(0));
        assert_eq!(deq.pop_back(), Some(4));
        assert_eq!(deq.pop_front(), Some(1));
        assert_eq!(deq.pop_back(), Some(3));
        deq.push_front(9);
        assert_eq!(deq.pop_back(), Some(2));
        assert_eq!(deq.pop_back(), Some(9));
        assert_eq!(deq.pop_back(), None);
        assert_eq!(deq.pop_front(), None);
    }
}
