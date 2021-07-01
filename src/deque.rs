#[allow(dead_code)]
use std::cell::RefCell;
use std::rc::Rc;


type Link<T> = Rc<RefCell<Box<T>>>;

#[derive(Debug)]
pub struct Deque<T>{
    front: Option<Link<Node<T>>>,
    back: Option<Link<Node<T>>>
}


struct Node<T>{
    elem: T,
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
            elem,
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
        println!("{:?}", deq)
    }
}