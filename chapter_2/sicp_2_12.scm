#lang planet neil/sicp

(define (make-interval a b) (cons a b))
(define (upper-bound z) (max (car z) (cdr z)))
(define (lower-bound z) (min (car z) (cdr z)))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (* c p)))

(define (percent z) 
  (/ (width z) (center z)))