#lang racket


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

;;; checks if a divides n
(define divides?
  (lambda (n a)
    (eq? 0 (remainder n a))))
