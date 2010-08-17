
;recursive
(define (f-recursive n)
  (cond ((< n 3) n)
        (else (+ (f-recursive (- n 1))
                 (f-recursive (- n 2))
                 (f-recursive (- n 3))))))


;iterative
(define (f-iterative n)
  (define (f-iter a b c count)
    (if (= count 0)
        a
        (f-iter b c (+ a b c) (- count 1))))
  (f-iter 0 1 2 n))
  
  
(define n 10)
(f-recursive n)
(f-iterative n)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (fib-recursive n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-recursive (- n 1))
                 (fib-recursive (- n 2))))))

(define (fib-iterative n)
  (define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))


(fib-recursive n)
(fib-iterative n)