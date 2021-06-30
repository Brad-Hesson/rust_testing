pub struct ThreadPool {}

impl ThreadPool {
    pub fn new() -> Self {
        Self {}
    }

    pub fn execute() {}
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2+2, 4)
    }
}