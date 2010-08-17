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

(define (sqrt x) (fixed-point (lambda (y) (* 0.5 (+ y (/ x y)))) 1.0))
(display (sqrt 5))
(newline)

(define (square x) (* x x))

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10)

(define (sqrt-new x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(display (sqrt-new 5))
(newline)

(define (cube x) (* x x x))
(define dx 0.00001)
(define (deriv g)
  (lambda (x) 
    (/ (- (g (+ x dx)) (g x))
    dx)))

(display ((deriv cube) 5))
(newline)

(define (newtons-method g guess)
  (define (newton-transform g)
    (lambda (x)
      (- x (/ (g x) ((deriv g) x)))))
  (fixed-point (newton-transform g) guess))

(define (sqrt-newton x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

(display (sqrt-newton 5))
(newline)

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a x x)
       (* b x)
       c)))

(display (newtons-method (cubic 1 1 1) 1))