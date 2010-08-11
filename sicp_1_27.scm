#lang planet neil/sicp

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test-new n)
  (define (fermat-test-iter n a)
    (cond ((= a 1) #t)
          (else (and (= (expmod a n n) a)
                     (fermat-test-iter n (- a 1))))))
  (fermat-test-iter n (- n 1)))


(display (fermat-test-new 561))
(display (fermat-test-new 1105))

;561 1105 1729 2465 2821 6601