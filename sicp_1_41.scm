#lang planet neil/sicp

(define (double g)
  (lambda (x) (g (g x))))

(define (inc x) (+ x 1))

(display (((double (double double)) inc) 5))