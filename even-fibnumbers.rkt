#lang racket

(define even-fib-counter
  (lambda (upper start1 start2)
    (if (> start1 upper)
        0
        (if (even? start1)
            (+ start1 (even-fib-counter upper start2 (+ start1 start2)))
            (even-fib-counter upper start2 (+ start1 start2))
            ))
    ))

(even-fib-counter 4000000 1 2)