#lang planet neil/sicp

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let ((sr (expmod base (/ exp 2) m))
               (sq (square (expmod base (/ exp 2) m))))
           (if (and (not (= sr 1)) 
                    (not (= sr (- m 1)) ) 
                    (= (remainder sq m) 1))
               0
               (remainder sq m))))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) (remainder 1 n)))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else #f)))


;561 1105 1729 2465 2821 6601

(display (fast-prime? 1105 5))