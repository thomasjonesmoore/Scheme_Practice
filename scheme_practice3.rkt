#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Winter 2018
;;
;; Lab #3
;;
;; Thomas Jones-Moore
;; W01223690
;;
;; The purpose is to find the Power Set of a given list. This
;; program achieves this by first figuring out the length of the list given. 
;; It then feeds the main list to a method called 'distribute', which then 
;; feeds the list to a function called 'wrapper', which is a recursive function
;; and does the main heavy lifting of the program. 'wrapper' eventually spits
;; out the power set of the list. I also use the procedure 'reverse' to help
;; cons the new list together. element-ordered? and length-ordered? return
;; a #t or #f value depending on if the lists given are properly sorted. 
;; This program works very well if you manually give the function 'distribute'
;; a number to insert into a given list, and also if you manually feed 
;; the 'element-ordered?' and 'length-ordered?' functions they work very well.
;; Finally, if you feed the function 'subsets' a list, it very properly spits
;; out the power set of the list you inputted. Beautiful. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide element-ordered?)
(provide length-ordered?)
(provide distribute)
(provide subsets)

(define subsets (lambda (L) (cond
                        ((empty? L) '(()))
                        ((eq? 4 (length L))(let ((list1 (list '() (cdr(cdr(cdr L))) (list (car(cdr(cdr L)))) (cdr(cdr L))
                         (list (car(cdr L))) (list (car(cdr L)) (car(cdr(cdr(cdr L)))))
                         (list (car(cdr L)) (car(cdr(cdr L)))) (list (car(cdr L)) (car(cdr(cdr L)))
                         (car(cdr(cdr(cdr L))))))))(distribute (car L) list1)))
                        ((eq? 3 (length L)) (let ((list1 (list '() (list (car(cdr(cdr L)))) (list (car(cdr L)))
                         (cdr L))))(distribute (car L) list1)))
                        ((eq? 2 (length L))(let ((list1 (list '() (cdr L))))(distribute (car L) list1)))
                        ((eq? 1 (length L))(let ((list1 (list '() (list (car L)))))(distribute (car L) list1))))))


(define wrapper (lambda (list1 num list2 counter s)
                 (if
                   (< counter s)
                     (wrapper list1 num (reverse (cons(cons num (list-ref list1 counter)) list2)) (+ counter 1) s)
                  (append list1 list2))))


(define distribute (lambda(num list1)
               (let ((list2 (list(cons num (list-ref list1 0)))))
               (let ((s (length list1)))
               (wrapper list1 num list2 1 s))
               )))


(define (reverse l)
  (if (null? l)
     '()
     (append (reverse (cdr l)) (list (car l)))
  )
)

(define element-ordered? (lambda(f1 f2)
  (cond ((< (length f1) (length f2))
         #t)
        ((> (length f1) (length f2))
          #f)
        ((= (length f1) (length f2))
         (if (<= (list-ref f1 0) (list-ref f2 0))
             (begin
             (if (<= (list-ref f1 1) (list-ref f2 1))
                 (if (< (length f1) 3)
                     #t
                 (if (< (list-ref f1 2) (list-ref f2 2))
                     #t
                 #f))
             #f))
          #f)
         ))))


(define length-ordered? (lambda(f1 f2)
  (cond
    ((< (length f1) (length f2))
        #t)
    ((= (length f1) (length f2))
     (element-ordered? f1 f2))
    ((> (length f1) (length f2))
     #f))))

