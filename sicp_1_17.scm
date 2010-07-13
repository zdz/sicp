
(define (mul a b)
  (if (zero? b)
      0
      (+ a (mul a (- b 1)))))

(define (halve n)
  (/ n 2))

(define (double n)
  (+ n n))

(define (fast-mul a b)
  (if (zero? b)
      0
      (if (even? b)
          (double (fast-mul a (halve b)))
          (+ a (fast-mul a (- b 1))))))

(define (fast-mul-new a b)
  (define (fast-mul-iter a b t)
    (cond ((zero? b) t)
          ((even? b) (fast-mul-iter (double a) (halve b) t))
          (else (fast-mul-iter a (- b 1) (+ t a)))))
  (fast-mul-iter a b 0))

;test
(mul 3 7)
(fast-mul 3 7)
(fast-mul-new 3 7)
