#lang planet neil/sicp

(define tolerance 0.0001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(display (fixed-point cos 1.0))
(newline)
(display (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0))
(newline)
(define (sqrt x) (fixed-point (lambda (y) (* 0.5 (+ y (/ x y)))) 1.0))
(display (sqrt 5))
(newline)
(display (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.5))