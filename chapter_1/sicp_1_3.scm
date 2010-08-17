;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sicp_1_3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (f a b c)
  (cond ((and (< a b) (< a c)) (+ b c))
        ((and (< b a) (< b c)) (+ a c))
        (else (+ a b))))

(f 5 2 3)
      