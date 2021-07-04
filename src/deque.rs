#![allow(dead_code)]
use std::cell::RefCell;
use std::rc::Rc;


type Link<T> = Rc<RefCell<Box<T>>>;

pub struct Deque<T>{
    front: Option<Link<Node<T>>>,
    back: Option<Link<Node<T>>>
}

impl<T: std::fmt::Debug> std::fmt::Debug for Deque<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Deque")
        .field("node", &self.front)
         .finish()
    }
}


struct Node<T>{
    elem: Option<T>,
    next: Option<Link<Node<T>>>,
    prev: Option<Link<Node<T>>>,
}

impl<T: std::fmt::Debug> std::fmt::Debug for Node<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Node")
        .field("elem", &self.elem)
         .field("next", &self.next)
         .finish()
    }
}

impl<T> Deque<T>{
    pub fn new() -> Self {
        Self {
            front: None,
            back: None
        }
    }

    pub fn push_front(&mut self, elem: T){
        let link = Rc::new(RefCell::new(Box::new(Node{
            elem: Some(elem),
            next: None,
            prev: self.front.take()
        })));
        self.front = Some(link.clone());
        if let Some(inner) = &link.borrow().prev{
            inner.borrow_mut().next = Some(link.clone());
        } else {
            self.back = Some(link.clone());
        };
    }

    pub fn push_back(&mut self, elem: T){
        let link = Rc::new(RefCell::new(Box::new(Node{
            elem: Some(elem),
            next: self.back.take(),
            prev: None
        })));
        self.back = Some(link.clone());
        if let Some(inner) = &link.borrow().next{
            inner.borrow_mut().prev = Some(link.clone());
        } else {
            self.front = Some(link.clone());
        };
    }

    pub fn pop_front(&mut self) -> Option<T>{
        if let Some(front) = self.front.take(){
            if let Some(inner) = &front.borrow().prev{
                inner.borrow_mut().next = None;
            } else {
                self.back = None;
            }
            self.front = front.borrow_mut().prev.take();
            front.borrow_mut().elem.take()
        }else{
            None
        }
    }

    pub fn pop_back(&mut self) -> Option<T>{
        if let Some(back) = self.back.take(){
            if let Some(inner) = &back.borrow().next{
                inner.borrow_mut().prev = None;
            } else {
                self.front = None;
            }
            self.back = back.borrow_mut().next.take();
            back.borrow_mut().elem.take()
        }else{
            None
        }
    }
}

#[cfg(test)]
mod tests{
    use super::Deque;
    #[test]
    fn test_deque(){
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