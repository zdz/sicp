#lang planet neil/sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter i func)
    (cond ((= i n) func)
          (else (iter (+ i 1) (compose f func)))))
  (iter 1 f))


(define (square x) (* x x))

(display ((repeated square 2) 5))