#![allow(dead_code)]
use std::sync::{
    mpsc::{channel, Sender},
    Arc, Mutex,
};

pub struct ThreadPool {
    handles: Vec<std::thread::JoinHandle<()>>,
    tx: Sender<Box<dyn Fn() + Send>>,
}

impl ThreadPool {
    pub fn new(num_threads: usize) -> Self {
        let (tx, rx) = channel::<Box<dyn Fn() + Send>>();
        let rx = Arc::new(Mutex::new(rx));
        let mut handles = vec![];

        for _ in 0..num_threads {
            let rxc = rx.clone();
            let clsr = move || loop {
                let recv = rxc.lock().unwrap().recv();
                if let Ok(work) = recv {
                    work();
                } else {
                    break;
                }
            };
            handles.push(std::thread::spawn(clsr));
        }

        Self { handles, tx }
    }

    pub fn execute<T: Fn() + Send + 'static>(&self, work: T) {
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
