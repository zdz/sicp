#lang planet neil/sicp

(define (iterative-improve good-enough? improve-func)
  (define (iter)
    (lambda (guess)
      (let ((next (improve-func guess)))
        (if (good-enough? guess next)
            next
            ((iter) next)))))
  (iter))

(define (fixed-point f first-guess)
  (define tolerance 0.0001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)) 
  ((iterative-improve close-enough? f) first-guess))


(define (sqrt x) (fixed-point (lambda (y) (* 0.5 (+ y (/ x y)))) 1.0))
(display (sqrt 5))
(newline)

(define (sqrt-new x)
  (define (average x y)
    (/ (+ x y) 2))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (good-enough? v1 v2)
    (< (abs (- v1 v2)) 0.0001))
  ((iterative-improve good-enough? 
                      (lambda (v) (improve v x)))
   1.0))

(display (sqrt-new 5))
(newline)