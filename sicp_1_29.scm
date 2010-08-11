#lang planet neil/sicp

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson n)
  (define (simpson-cal a b n)
    (let ((h (/ (- b a) n)))
      (define (yk f a k h)
        (f (+ a (* k h))))
      (define (ele-fac f a k h)
        (let ((fe (* 2 (yk f a k h))))
          (if (even? k)
              fe
              (* 2 fe))))      
      (* (/ h 3)
         (+ (yk cube a 0 h)
            (yk cube a n h)
            (sum (lambda (x) (ele-fac cube a x h)) 
                 1 
                 (lambda (x) (+ x 1)) 
                 (- n 1))))))
  (simpson-cal 0 1 n))

(display (integral cube 0 1 0.01))
(newline)
(display (integral cube 0 1 0.001))
(newline)
(display (simpson 100))
(newline)
(display (simpson 1000))