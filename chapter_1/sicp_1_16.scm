
(define (square x) (* x x))
  
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (fast-expt-new b n)
  (define (fast-expt-new-iter b n a)
    (if (zero? n)
        a
        (if (even? n)
            (fast-expt-new-iter (square b) (/ n 2) a)
            (fast-expt-new-iter b (- n 1) (* b a)))))
  (fast-expt-new-iter  b n 1))

;test
(fast-expt-new 2 6) 
(fast-expt-new 3 3)
(fast-expt 2 3)