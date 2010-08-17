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

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))


(display (fast-prime? 1009 5))(newline)
(display (fast-prime? 1013 5))(newline)
(display (fast-prime? 1019 5))(newline)

(display (fast-prime? 10007 5))(newline)
(display (fast-prime? 10009 5))(newline)
(display (fast-prime? 10037 5))(newline)

(display (fast-prime? 100003 5))(newline)
(display (fast-prime? 100019 5))(newline)
(display (fast-prime? 100043 5))(newline)

(display (fast-prime? 1000003 5))(newline)
(display (fast-prime? 1000033 5))(newline)
(display (fast-prime? 1000037 5))(newline)


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
  (start-prime-test-new n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      #f))

(define (start-prime-test-new n start-time)
  (if (fast-prime? n 10)
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