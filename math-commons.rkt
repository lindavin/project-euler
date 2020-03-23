#lang racket

;;; generates a list of integers from start to 'one' less than 'end'.
(define range
  (lambda (start end)
    (if (>= start end)
        null
        (cons start (range (+ start 1) end)))))

;;; reduces a list of two or more elements to
;;; one value using the two-value function 'fun'.
(define reduce
  (lambda (fun lst)
    (if (or (null? lst) (singleton? lst))
        null
        (reduce-helper fun lst))))

;;; helper procedure for the function 'reduce'
(define reduce-helper
  (lambda (fun lst)
    (if (singleton? lst)
        (car lst)
        (reduce-helper fun (cons (fun (car lst) (cadr lst)) (cddr lst))))))

;;; finds the lcm of n-arguments
(define lcm
  (lambda xs
    (if (null? xs)
        null
        (if (singleton? xs)
            (car xs)
            (reduce lcm-a-b xs)))))

;;; finds the lcm of two arguments
(define lcm-a-b
  (lambda (a b)
    (/ (* a b) (gcd a b) )))

;;; finds the gcd of two arguments
(define gcd-a-b
  (lambda (a b)
    (if (eq? (remainder a b) 0)
        b
        (gcd-a-b b (remainder a b)))))

;;; checks if a list is a singleton
(define singleton?
  (lambda (lst)
    (if (null? lst)
        #f
        (null? (cdr lst)))))

;;; shallow-maps a function over a lst
(define map
  (lambda (fun lst)
    (if (null? lst)
        null
        (cons (fun (car lst)) (map fun (cdr lst))))))
