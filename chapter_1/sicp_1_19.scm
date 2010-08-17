
;T(pq)[0] (a,b)
;T(pq)[1] (bq+aq+ap, bp+aq)
;T(pq)[2] ((bp+aq)q + (bq+aq+ap)q + (bq+aq+ap)p,(bp+aq)p + (bq+aq+ap)q)
;         ==>>(b(2pq+qq)+a(2pq+qq)+a(qq+pp), b(qq+pp)+a(2pq+qq))
;q' = 2pq+qq
;p' = qq+pp
;
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count) 
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q ) (* q q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(fib 9)