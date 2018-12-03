#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Winter 2018
;;
;; Lab #2
;;
;; Thomas Jones-Moore
;; W01223690
;;
;; The purpose of this program is to
;; calculate the integral of a function
;; when given a function, the upper bound,
;; and the lower bound. This is done using
;; recursion, and two functions/procedures.
;; Also supplied is the derivative function,
;; provided in the lab already. This is hel-
;; pful in the test cases provided.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide D)
(provide I)

(define D
  (lambda (f)
    (let* ((delta 0.00001)
           (two-delta (* 2 delta)))
      (lambda (x)
        (/ (- (f (+ x delta))
              (f (- x delta)))
           two-delta)))))

;I needs to take in a function, output the integral of that function which then
;can take two arguments, lower and upper bounds. Then passes 'Idef' the function,
;count, upper, lower, and a total. 
(define I
  (lambda (f)
    (lambda (upper lower) (Idef f 0.0 upper lower 0.0))))

;Spits out total by using recursion to sum every time.
(define (Idef f count a b total)
               (let ((delta .0001))
               (if (<= (+ a (* count delta)) b)
                   (Idef f (+ 1.0 count) a b (+ total (f (+ a (* count delta)))))
                   (/ total 10000))))


                   
               
               