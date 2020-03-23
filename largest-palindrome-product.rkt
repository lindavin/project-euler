#lang racket

(define largest-palindrome-product-helper
  (lambda (upper b max)
    (cond
      [(<= upper 0)
       max]
      [(<= b 0)
       (largest-palindrome-product-helper (- upper 1) (- upper 1) max)]
      [(eq? upper 0)
       (largest-palindrome-product-helper (- upper 1) (- upper 1) max)]
      [(and (> (* upper b) max) (palindrome-int? (* upper b)))
       (largest-palindrome-product-helper upper (- b 1) (* upper b))]
      [else
       (largest-palindrome-product-helper upper (- b 1) max)]
      )
  ))

(define largest-palindrome-product
  (lambda (upper)
    (largest-palindrome-product-helper upper upper 0)))

(define palindrome-int?
  (lambda (int)
    (cond
     [(eq? 0 int)
      #t]
     [(eq? 0 (remainder int 10))
      #f]
     [else
      (eq? (invert-int int) int)])))

(define invert-lst-helper
  (lambda (lst so-far)
    (if (null? lst)
        so-far
        (invert-lst-helper (cdr lst) (cons (car lst) so-far)))))

(define invert-lst
  (lambda (lst)
    (invert-lst-helper lst null)))

(define invert-int
  (lambda (int)
    (string->number (list->string (invert-lst (string->list (number->string int)))))))

(largest-palindrome-product 999)

