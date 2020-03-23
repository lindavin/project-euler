#lang racket
;Expand counter to take any number of elements
;implement map use map
(define singleton?
  (lambda (lst)
    (if (null? lst)
        #f
        (null? (cdr lst)))))

(define map
  (lambda (fun lst)
    (if (null? lst)
        null
        (cons (fun (car lst)) (map fun (cdr lst))))))

;implement reduce
;;;reduce
(define reduce-helper
  (lambda (fun lst)
    (if (singleton? lst)
        (car lst)
        (reduce-helper fun (cons (fun (car lst) (cadr lst)) (cddr lst))))))

(define reduce
  (lambda (fun lst)
    (if (or (null? lst) (singleton? lst))
        null
        (reduce-helper fun lst))))

;;;log is either 'and' 'or'
;;; can't really take 'and' 'or' as params
;;; didn't use
(define reduce-boolean
  (lambda (fun? log lst)
    (if (null? lst)
        #f
        (log (fun? (car lst)) (reduce-boolean fun? log (cdr lst)))))) 

(define divides?
  (lambda (n a)
    (if (eq? 0 (remainder n a))
        #t
        #f)))

(define divisible-by?
  (lambda (n lst)
    (if (null? lst)
        #f
        (or (divides? n (car lst)) (divisible-by? n (cdr lst))))))


;;; Sums the multiples of two given numbers up to an upper bound
(define multiples-of-a-b
  (lambda (upper a b)
    (if (<= upper 0)
        0
        (if (or (divides? upper a) (divides? upper b))
            (+ upper (multiples-of-a-b (- upper 1) a b))
            (multiples-of-a-b (- upper 1) a b)
            ))))


;Expand counter to take a list
;;; Sums the multiples of the integers in a given list up to an upper bound
(define multiples-of-lst-helper
  (lambda (upper lst)
    (if (<= upper 0)
        0
        (if (divisible-by? upper lst)
            (+ upper (multiples-of-lst-helper (- upper 1) lst))
            (multiples-of-lst-helper (- upper 1) lst)))))

(define multiples-of-lst
  (lambda (upper lst)
    (if (null? lst)
        0
        (multiples-of-lst-helper upper lst))))

;;;Use map
(define or-reduce
  (lambda (lst)
    (if (null? lst)
        #f
        (or (car lst) (cdr lst)))))

(define multiples-of-lst2
  (lambda (upper lst)
    (if (<= upper 0)
        0
        (if (or-reduce (map (lambda (x)
                      (divides? upper x)) lst))
            (+ upper (multiples-of-lst2 (- upper 1) lst))
            (multiples-of-lst2 (- upper 1) lst)))))

(multiples-of-lst 10 '(3 5))
(map singleton? (list '(1) '(1 2)))
(multiples-of-lst2 3 '(1 3))