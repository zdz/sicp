
#lang planet neil/sicp

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b )
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time) #t)

(define (search-for-prime n cc)
  (cond ((> cc 0) (search-for-prime (+ n 1) 
                                    (if (timed-prime-test n) 
                                        (- cc 1) 
                                        cc)))
        (else (newline)
              (display "***finish***"))))


(search-for-prime 1000 3)
(search-for-prime 10000 3)
(search-for-prime 100000 3)
(search-for-prime 1000000 3)