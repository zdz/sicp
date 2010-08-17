#lang planet neil/sicp

;iterative
(define (cont-frac nf df k)
  (define (cont-frac-iter i result)
    (cond ((= i 0) result)
          (else (cont-frac-iter (- i 1)
                                (/ (nf i)
                                   (+ (df i) result))))))
  (cont-frac-iter k 0))
  
;recursive
(define (cont-frac-recursive nf df k)
  (define (c-f-r i)
    (cond ((> i k) 0)
          (else (/ (nf i)
                   (+ (df i)
                      (c-f-r (+ i 1)))))))
  (c-f-r 1))



(let ((tt (cont-frac (lambda (i) 1.0) 
                     (lambda (i) 1.0) 
                     100)))
  (display tt)
  (newline)
  (display (/ 1 tt)))