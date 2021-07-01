use std::sync::{
    mpsc::{channel, Sender},
    Arc, Mutex,
};

pub trait SendableClosure: FnMut() + Send + 'static {}
impl<T: FnMut() + Send + 'static> SendableClosure for T {}

pub struct ThreadPool {
    tx: Sender<Box<dyn SendableClosure>>,
}

impl ThreadPool {
    pub fn new(num_threads: usize) -> Self {
        let (tx, rx) = channel::<Box<dyn SendableClosure>>();
        let rx_ref = Arc::new(Mutex::new(rx));
        let clsr = move || loop {
            let recv = rx_ref.lock().unwrap().recv();
            if let Ok(mut work) = recv {
                work();
            } else {
                break;
            }
        };
        (0..num_threads).for_each(|_| {
            std::thread::spawn(clsr.clone());
        });

        Self { tx }
    }

    pub fn execute(&self, work: impl SendableClosure) {
        self.tx.send(Box::new(work)).unwrap();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicI32, Ordering};

    #[test]
    fn multiple_tasks_test() {
        let pool = ThreadPool::new(5);
        let foo = || {
            println!("Start thread");
            std::thread::sleep(std::time::Duration::from_secs(1));
            println!("End thread");
        };

        pool.execute(foo.clone());
        pool.execute(foo.clone());
        pool.execute(foo);

        std::thread::sleep(std::time::Duration::from_secs(2));
    }

    #[test]
    fn atomic_int_test() {
        let sum = Arc::new(AtomicI32::new(0));
        let sum_ref = sum.clone();

        let pool = ThreadPool::new(5);
        let foo = move || {
            sum_ref.fetch_add(1, Ordering::SeqCst);
        };

        pool.execute(foo.clone());
        pool.execute(foo.clone());
        pool.execute(foo);

        std::thread::sleep(std::time::Duration::from_secs(2));

        let sum = sum.load(Ordering::SeqCst);
        assert_eq!(sum, 3);
        println!("Sum: {}", sum);
    }
}
