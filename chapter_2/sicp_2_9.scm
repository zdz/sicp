#lang planet neil/sicp

(define (make-interval a b) (cons a b))
(define (upper-bound z) (max (car z) (cdr z)))
(define (lower-bound z) (min (car z) (cdr z)))
(define (interval-w z) (/ (- (upper-bound z) (lower-bound z)) 2))

;[a,b]   w1 = (b-a)/2
;[c,d]   w2 = (d-c)/2
;add
;[a+c,b+d] w3=(b+d-a-c)/2=w1+w2
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;sub
;[a-d,b-c] w3 = (b-c-a+d)/2 = w1+w2
;[c-b,d-a] w3 = (d-a-c+b)/2 = w1+w2
(define (sub-interval x y)
  (add-interval x
                (make-interval (- (lower-bound y))
                               (- (upper-bound y)))))

;mul
;not linearly dependent on w
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
    
;div
(define (div-interval x y)
  (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))))



(define (display-interval z)
  (display "[")
  (display (lower-bound z))
  (display ",")
  (display (upper-bound z))
  (display "]")
  (newline))

;
(define a (make-interval 12 45))
(define b (make-interval 10 30))
(display-interval a)
(display-interval b)
(display-interval (add-interval a b))
(display-interval (sub-interval a b))
(display-interval (mul-interval a b))
(display-interval (div-interval a b))