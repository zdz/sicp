#lang planet neil/sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter i func)
    (cond ((= i n) func)
          (else (iter (+ i 1) (compose f func)))))
  (iter 1 f))

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

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

;power
(define (power base exp)
  (cond ((zero? exp) 1)
        ((zero? (remainder exp 2)) (* (power base (quotient exp 2))
                                      (power base (quotient exp 2))))
         (else (* base
                  (power base (quotient exp 2))
                  (power base (quotient exp 2))))))

(define (sqrt-new x n)
  (let ((rp (repeated (average-damp (lambda (y) 
                                      (/ x (power y (- n 1)))))
                      (* 3 n))))
    (fixed-point rp 1.0)))

(define n 16)
(define q 4)
(let ((t (sqrt-new n q)))
  (display t)
  (newline)
  (display (power t q)))