#lang planet neil/sicp

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((fact (if (< d 0) -1 1)))
    (let ((n (* fact n))
          (d (* fact d)))
      (let ((g (gcd n d)))
        (cons (/ n g) (/ d g))))))
(define numer car)
(define denom cdr)

(define (print-rat r)
  (display (numer r))
  (display "/")
  (display (denom r))
  (newline))
  
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
(print-rat one-half)
(print-rat one-third)
(print-rat (add-rat one-half one-third))
(print-rat (sub-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (div-rat one-half one-third))

;test
(print-rat (make-rat -9 -21))
(print-rat (make-rat -9 21))
(print-rat (make-rat 9 21))
(print-rat (make-rat 9 -21))
