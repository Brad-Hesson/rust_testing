( begin
    (define fib (lambda (n) (
        if (< n 3) 
            1 
            (+ (fib (- n 1)) (fib (- n 2)))
    )))
    (define zip (lambda (l1 l2) (
        map list l1 l2
    )))
    (zip '(1 2 3) '(1 2 3))
)