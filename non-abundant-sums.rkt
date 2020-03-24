#lang racket

(define sum-k-powers
  (lambda (n k)
    (/ (- (expt n (+ k 1)) 1) (- n 1))))

(define prime-helper
  (lambda (n start)
    (if (> start (sqrt n))
        #t
        (if (divides? n start)
            #f
            (prime-helper n (+ 1 start))))))

(define prime?
  (lambda (n)
    (prime-helper n 2)))

;;; checks if a divides n
(define divides?
  (lambda (n a)
    (eq? 0 (remainder n a))))

(define greatest-prime-power
  (lambda (n p)
    (if (divides? n p)
        (+ 1 (greatest-prime-power (/ n p) p))
        0)))

(define abundant?
  (lambda (n mesh)
    (if (hash-has-key? mesh n)
        (hash-ref mesh n)
        (let
            ([sum-of-proper-divisors (- (abundant-helper n 2) n)])
          (hash-set! mesh n (> sum-of-proper-divisors n)
            )
          (hash-ref mesh n)))))

(define abundant-helper
  (lambda (n p)
    (if (= n 1)
        1
        (if (divides? n p)
            (if (prime? p)
                (* (sum-k-powers p (greatest-prime-power n p)) (abundant-helper (/ n (expt p (greatest-prime-power n p))) 2))
                (abundant-helper n (+ p 1)))
            (abundant-helper n (+ p 1))))))

(define abundant-loop
  (lambda (start)
    (if (> start 28123)
        0
        (if (abundant? start)
            (+ start (abundant-loop (+ 1 start)))
            (abundant-loop (+ 1 start))))))

(define sum-of-abundants?
  (lambda (n mesh)
    (sum-of-abundants-helper n 1 mesh)))

(define sum-of-abundants-helper
  (lambda (n start mesh)
    (if (> start (/ n 2))
        #f
        (if (and (abundant? start mesh) (abundant? (- n start) mesh))
            #t
            (sum-of-abundants-helper n (+ 1 start) mesh)))))

(define sum-of-abundants-loop
  (lambda (start mesh)
    (if (> start 28123)
        0
        (if (sum-of-abundants? start mesh)
            (sum-of-abundants-loop (+ 1 start) mesh)
            (+ start (sum-of-abundants-loop (+ 1 start) mesh))))))

(sum-of-abundants-loop 1 (make-hash))
(sum-of-abundants? 28124 (make-hash))