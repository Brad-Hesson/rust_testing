#[allow(dead_code)]
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
        let node = Node{
            elem: Some(elem),
            next: None,
            prev: self.front.take()
        };
        let link = Rc::new(RefCell::new(Box::new(node)));
        self.front = Some(link.clone());
        if let Some(prev) = &link.borrow().prev{
            prev.borrow_mut().next = Some(link.clone());
        } else {
            self.back = Some(link.clone());
        };
    }

    pub fn pop_front(&mut self) -> Option<T>{
        if let Some(front) = self.front.take(){
            if let Some(prev) = &front.borrow().prev{
                prev.borrow_mut().next = None;
            } else {
                self.back = None;
            }
            self.front = front.borrow_mut().prev.take();
            front.borrow_mut().elem.take()
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
        deq.push_front(3);
        deq.push_front(4);
        deq.push_front(5);
        assert_eq!(deq.pop_front(), Some(5));
        assert_eq!(deq.pop_front(), Some(4));
        println!("{:?}", deq)
    }
}