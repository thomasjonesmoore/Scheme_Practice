#lang racket

(provide lookup)
(provide looker)

;takes in symbol and environment, looks through environment to see if symbol is in it
(define lookup (lambda(sym env)
  (cond
    ((equal? (symbol? sym) (#f))
      exn:fail)
    ((equal? (looker sym env) (#f)
      exn:fail))
    ;(else (looker sym env))
    )))


;looks to see if symbol matches environment, if it does then return symbol; if not then
;pass the cdr of environment back to looker. if it goes through the whole environment and
;sym is not in it, returns #f.
(define looker (lambda(sym env) 
  (if (equal? sym car(env))
      cadar(env))
  (looker sym cdr(env))))


(define evaluate (lambda(exp env)
  (cond
    ((equal? (number? exp) (#t)
             exp)
    ((equal? (symbol? exp) (#t)
             (looker exp env)
    ((equal? (list? exp) (#t)
             (if (equal? (looker car(exp) env) (#f)
                         exn:fail)
                 (looker