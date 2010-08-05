
#lang planet neil/sicp

(define (next-test-divisor n)
  (cond ((<= n 2) 3)
        (else (if (divides? 2 n) (+ n 1) (+ n 2)))))

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-test-divisor test-divisor)))))

(define (divides? a b)
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

(define (search-for-prime n count)
  (let ((sn (if (divides? 2 n) (+ n 1) n)))
    (define (iter-search-for-prime nn cc)
      (cond ((> cc 0) 
             (let ((f (timed-prime-test nn)))
               (search-for-prime (+ nn 2)
                                 (if f (- cc 1) cc))))
            (else (newline)
                  (display "***finish***")
                  (newline))))
    (iter-search-for-prime sn count)))

(search-for-prime 1000 3)
;(search-for-prime 10000 3)
;(search-for-prime 100000 3)
;(search-for-prime 1000000 3)
;(search-for-prime 10000000 3)