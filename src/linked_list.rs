pub struct List<T> {
    object: Option<T>,
    next: Option<Box<List<T>>>
}

impl<T> List<T>{
    pub fn new() -> Self{
        Self {
            object: Option::None,
            next: Option::None
        }
    }

    pub fn push(&mut self, item: T) {
        self.object = Some(item);
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test1() {
        let mut l = List::<i32>::new();
        l.push(3);
        println!("{:?}", l.object);
    }
}