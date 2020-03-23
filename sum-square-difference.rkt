#lang racket

;;; generates a list of integers from start to 'one' less than 'end'.
(define range
  (lambda (start end)
    (if (>= start end)
        null
        (cons start (range (+ start 1) end)))))

(define sum-of-squares
  (lambda (lst)
    (if (null? lst)
        0
        (+ (expt (car lst) 2) (sum-of-squares (cdr lst))))))

(define square-of-sum
  (lambda (lst)
    (expt (apply + lst) 2)))

(define main
  (lambda (lst)
    (- (square-of-sum lst) (sum-of-squares lst))))

(main (range 0 11))
(main (range 0 101))