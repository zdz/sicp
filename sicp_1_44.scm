#lang planet neil/sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter i func)
    (cond ((= i n) func)
          (else (iter (+ i 1) (compose f func)))))
  (iter 1 f))

(define (average a b c)
  (/ (+ a b c) 3))

(define (smooth f)
  (define dx 0.00001)
  (lambda (x)
    (average (f (- x dx))
             (f x)
             (f (+ x dx)))))

(define (smooth-repeated f n)
  (repeated (smooth f) n))

(display ((smooth-repeated (lambda (x) (* 3 x)) 5) 1))

