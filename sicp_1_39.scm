#lang planet neil/sicp

(define (tan-cf x k)
  ;iterative
  (define (cont-frac-new nf df k)
    (define (cont-frac-iter i result)
      (cond ((= i 0) result)
            (else (cont-frac-iter (- i 1)
                                  (/ (nf i)
                                     (- (df i) result))))))
    (cont-frac-iter k 0))
  
  (define (nf x k)
    (cond ((< k 1) 0)
          ((= k 1) x)
          (else (* x x))))
  
  (define (df k)
    (cond ((< k 1) 0)
          (else (- (* 2 k) 1))))
  
  (cont-frac-new (lambda (v) (nf x v)) df  k))


(display (tan-cf 30.0 100))