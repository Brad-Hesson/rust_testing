use std::sync::{
    mpsc::{channel, Sender},
    Arc, Mutex,
};

pub trait SendableClosure: FnMut() + Send + 'static {}
impl<T> SendableClosure for T where T: FnMut() + Send + 'static {}

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

    #[test]
    fn atomic_int_test() {
        use std::sync::atomic::{AtomicI32, Ordering};

        let sum = Arc::new(AtomicI32::new(0));
        let sum_ref = sum.clone();

        let pool = ThreadPool::new(5);
        let foo = move || {
            sum_ref.fetch_add(1, Ordering::SeqCst);
        };

        (0..20).for_each(|_| pool.execute(foo.clone()));

        std::thread::sleep(std::time::Duration::from_millis(100));

        let sumf = sum.load(Ordering::SeqCst);
        assert_eq!(sumf, 20);
    }
}
