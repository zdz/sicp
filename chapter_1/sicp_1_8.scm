
(define (cube-root x)
  
  (define (square x) (* x x))

  (define (improve guess y)
    (/ (+ (/ y (square guess)) (* 2 guess)) 3))
  
  (define (good-enough? guess y)
    (< (abs (- (improve guess y) guess)) 0.0000000001))
  
  (define (cube-iter guess y)
    (if (good-enough? guess y) 
        guess
        (cube-iter (improve guess y)
                   y)))
  
  (cube-iter 1.0 x))

;test
(cube-root 27.0)

