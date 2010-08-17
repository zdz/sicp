#lang planet neil/sicp

;iterative
(define (accumulate combiner null-value term a next b)
  (define (iter i result)
    (if (> i b)
        result
        (iter (next i)
              (combiner (term i) result))))
  (iter a null-value))

;recursive
(define (accumulate-recursive combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-recursive combiner null-value term (next a) next b))))
  
(define (sum term a next b)
  (accumulate + 0 term a next b))
                
(define (product n)
  (accumulate * 
              1
              (lambda (x) x)
              1
              (lambda (x) (+ x 1))
              (+ 1 n)))

;test
(display (product 5))
(newline)
(display (sum (lambda (x) (* x x x))
              1
              (lambda (x) (+ x 1))
              10))