;power
(define (power base exp)
  (cond ((zero? exp) 1)
        ((zero? (remainder exp 2)) (* (power base (quotient exp 2))
                                      (power base (quotient exp 2))))
         (else (* base
                  (power base (quotient exp 2))
                  (power base (quotient exp 2))))))


;computing square roots with newton's method
(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define DEVIATION 0.000000001)

;good-enough-1
(define (good-enough? guess x)
  (< (abs (- (square 
              guess) 
             x))
     DEVIATION))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt_func guess x)
  (sqrt-iter guess x))

(define (sqrt_func_1 n)
  (sqrt_func (sqrt n) n))
(define (sqrt_func_1_1 n)
  (sqrt_func 1.0 n))

;good-enough-2
(define (good-enough-2? guess x)
  (< (/ (abs  (- guess (improve guess x)))
        guess)
     DEVIATION))

(define (sqrt-iter-2 guess x)
  (if (good-enough-2? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt_func-2 guess x)
  (sqrt-iter-2 guess x))

(define (sqrt_func_2 n)
  (sqrt_func-2 (sqrt n) n))
(define (sqrt_func_2_1 n)
  (sqrt_func-2 1.0 n))


;;test
(define n1 9999999999999)
;;1
(printf "======good enough 1======~n") (newline)
(define a1 (sqrt_func_1 n1))
(define b1 (sqrt_func_1_1 n1))

a1 (power a1 2)
b1 (power b1 2)

;;2
(printf "======good enough 2======~n") (newline)
(define a2 (sqrt_func_2 n1))
(define b2 (sqrt_func_2_1 n1))

a2 (power a2 2)
b2 (power b2 2)


(define (sqrt3 x)
  (define (average x y)
    (/ (+ x y) 2))

  (define (improve guess x)
    (average guess (/ x guess)))

  ;; New version of good-enough? that is relative to the size of the
  ;; guess.
  (define (good-enough? guess x)
    (< (abs (- guess (/ x guess)))
       (/ guess 1000000000)) )

  (define (try guess)
    (if (good-enough? guess x)
        guess
        (try (improve guess x))))

  (try 1.0))



(printf "~a ~a ~a ~n" b1 b2 (sqrt3 n1))
(power (sqrt3 n1) 2)
'done

