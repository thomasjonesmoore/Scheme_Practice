#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Winter 2018
;;
;; Lab #1
;;
;; Thomas Jones-Moore
;; W01223690
;;
;; The purpose of this program is to
;; calculate the value of pie using
;; a very slowly converging series.
;; The algorithm looks like: 
;; 4-(4/3)+(4/5)-(4/7)+... 
;; This is done by using the procedure 
;; called 'check' recursively until the
;; next fraction in the series is smaller
;; than the inputed tolerance
;; (from the user).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-pi)

;x being the tolerance passed from user. then passes num, denom, tolerance, total to the check function
(define make-pi (lambda (x) (check  4.0 1.0 x -4.0)))

;recursive function
(define (check num den tol sum) 
     (if (< (/ num (countdenom den)) tol)
       sum
     (check 4.0 (+ den 2.0) tol (- (* -1.0 sum) (/ num (countdenom den))))))

;calculated denom by adding two everytime
(define countdenom (lambda (denom) (+ 2.0 denom)))
     
     
         
         
         
         

