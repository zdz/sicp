
(define (pascal-cell r c)
  (cond ((< r c) 0)
        ((or (zero? r) (zero? c) (= r c)) 1)        
        (else (+ (pascal-cell (- r 1) c)
                 (pascal-cell (- r 1) (- c 1))))))

;(pascal-cell 4 1)

(define (pascal-func r)
  (define (row-sum r cc rn)
    (if (< r cc) 
        rn 
        (row-sum r 
                 (+ cc 1) 
                 (+ rn (pascal-cell r cc)))))
  
  (define (pascal-iter cr r n)
    (if (> cr r) 
        n 
        (pascal-iter (+ cr 1)
                     r
                     (+ n (row-sum cr 0 0)))))        
  
  (pascal-iter 0 (- r 1) 0))

;cal row[ 0 1 2 3 ]
(pascal-func 4)
