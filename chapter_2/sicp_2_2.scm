#lang planet neil/sicp


(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (make-segment s e) (cons s e))
(define (start-segment l) (car l))
(define (end-segment l) (cdr l))
(define (midpoint-segment l)
  (let ((s (start-segment l))
        (e (end-segment l)))
    (make-point (/ (+ (x-point s) (x-point e)) 2)
                (/ (+ (y-point s) (y-point e)) 2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define a (make-point 2 5))
(define b (make-point 6 4))
(print-point a)
(print-point b)

(print-point (midpoint-segment (make-segment a b)))