#lang planet neil/sicp

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter i result)
    (if (> i b)
        result
        (iter (next i)
              (let ((ti (term i)))
                (if (filter ti)
                    (combiner (term i) result)
                    result)))))
  (iter a null-value))

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n(+ test-divisor 1)))))

(define (divides? a b )
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum-prime a b)
  (filtered-accumulate prime? + 0 (lambda (x) x) a (lambda (x) (+ 1 x)) b))

(define (GCD a b)
  (if (= 0 b)
      a
      (GCD b (remainder a b))))

(define (sum-prime-n n)
  (filtered-accumulate (lambda (x) (= (GCD x n) 1))
                       + 
                       0 
                       (lambda (x) x) 
                       1 
                       (lambda (x) (+ 1 x)) 
                       n))

(display (sum-prime 2 8))
(newline)
(display (GCD 45 15))
(newline)
(display (sum-prime-n 8))