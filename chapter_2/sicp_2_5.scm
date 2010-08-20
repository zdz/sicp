#lang planet neil/sicp

(define (square x) (* x x))
;pow
(define (power base exp)
  (cond ((zero? exp) 1)
        ((zero? (remainder exp 2)) (square (power base (quotient exp 2))))
         (else (* base
                  (square (power base (quotient exp 2)))))))


(define (fact-n t fact)
  (define (iter a result)
    (cond ((= 0 (remainder a fact)) (iter (/ a fact) (+ result 1)))
          (else result)))
  (iter t 0))

(define (make-p a b) (* (power 2 a) (power 3 b)))

(define (car z) (fact-n z 2))
(define (cdr z) (fact-n z 3))

(define a 10)
(define b 11)
(define z (* (power 2 a) (power 3 b)))
(display (car z))
(newline)
(display (cdr z))