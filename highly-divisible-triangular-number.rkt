#lang racket


;;; checks if a divides n
(define divides?
  (lambda (n a)
    (eq? 0 (remainder n a))))

;;; finds the number of divisors
(define divisors
  (lambda (n)
    (divisors-helper n 2)))

(define prime?
  (lambda (n)
    (prime-helper n 2)))

(define prime-helper
  (lambda (n start)
    (if (> start (sqrt n))
        #t
        (if (divides? n start)
            #f
            (prime-helper n (+ 1 start))))))

(define greatest-prime-power
  (lambda (n p)
    (if (divides? n p)
        (+ 1 (greatest-prime-power (/ n p) p))
        0)))

(define divisors-helper
  (lambda (n p)
    (if (= n 1)
        1
        (if (divides? n p)
            (if (prime? p)
                (* (+ 1 (greatest-prime-power n p)) (divisors-helper (/ n (expt p (greatest-prime-power n p))) 2))
                (divisors-helper n (+ p 1)))
            (divisors-helper n (+ p 1))))))

(define divisors-helper-1
  (lambda (n b so-far)
    (if (<= b 0)
        so-far
        (if (divides? n b)
            (divisors-helper n (- b 1) (+ 1 so-far))
            (divisors-helper n (- b 1) so-far)))))



;;; finds the nth triangular number
(define triangular-number-n
  (lambda (n)
    (apply + (range (+ 1 n)))))

(define main
  (lambda (upper)
    (main-helper 1 upper)))

(define main-helper
  (lambda (start upper)
    (if (> (divisors (triangular-number-n start)) upper)
        (triangular-number-n start)
        (main-helper (+ start 1) upper))))

(main 500)