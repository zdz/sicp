#lang planet neil/sicp

;recursive
(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursive term (next a) next b))))

;iterative
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product (lambda (x) x)
           1
           (lambda (x) (+ x 1))
           (+ n 1)))

(display (factorial 5))
(newline)

(define (square x) (* x x))

(define (pi-cal n)
  (define (iter i result)
    (if (> i n)
        result
        (iter (+ i 1)
              (* result
                 (/ (square (* i 2))
                    (square (+ 1 (* i 2))))))))
  (* 4 n (iter 1 1.0)))

(define (pi-cal-2 n)
  (define (pi-term i)
    (/ (square (* i 2))
       (square (+ 1 (* i 2)))))
  (* 4.0 n
     (product pi-term
              1
              (lambda (x) (+ x 1))
              n)))

(display (pi-cal 1000))
(newline)
(display (pi-cal-2 1000))
    
    
    
    
    
    