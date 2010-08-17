#lang planet neil/sicp

;iterative
(define (cont-frac nf df k)
  (define (cont-frac-iter i result)
    (cond ((= i 0) result)
          (else (cont-frac-iter (- i 1)
                                (/ (nf i)
                                   (+ (df i) result))))))
  (cont-frac-iter k 0))

(define (df k)
  (let ((r (remainder k 3)))
    (cond ((< k 1) 0)
          ((< k 3) k)
          ((or (= r 0) (= r 1)) 1)
          (else (* 2 (+ 1 (floor (/ k 3))))))))

(display (+ 2 (cont-frac (lambda (i) 1.0) df 1000)))