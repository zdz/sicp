#lang planet neil/sicp

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (make-segment s e) (cons s e))
(define (start-segment l) (car l))
(define (end-segment l) (cdr l))
(define (midpoint-segment l)
  (let ((s (start-segment l))
        (e (end-segment l)))
    (make-point (/ (+ (x-point s) (x-point e)) 2)
                (/ (+ (y-point s) (y-point e)) 2))))


(define (length-seg l)
  (sqrt (+ (square (- (x-point s) (x-point e)))
           (square (- (y-point s) (y-point e))))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;h w
(define (make-rect seg-a seg-b)
  (cons seg-a seg-b))
(define (rect-h r) (length-seg (car r)))
(define (rect-w r) (length-seg (cdr r)))

;anticlockwise four point
(define (make-rect-2 point-a point-b point-c point-d)
  (make-rect (make-segment point-a point-b) 
             (make-segment point-b point-c)))

(define (rect-area r)
  (* (rect-h r) (rect-w r)))

(define (perimeter-rect r)
  (* (+ (rect-h r) (rect-w r)) 2))