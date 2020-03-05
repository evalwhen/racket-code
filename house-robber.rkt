#lang eopl

(define (f l i)
  (cond
    [(<= i 0) 0]
    [(= i 1) (car l)]
    [(= i 2) (max (car l) (cadr l))]
    [else (max (f (cdr l) (- i 1))
               (+ (car l) (f (cddr l) (- i 2))))]))

(define (rob l)
  (f l (length l)))


(define (f-c l i cont)
  (cond
    [(<= i 0) (cont 0)]
    [(= i 1) (cont (car l))]
    [(= i 2) (cont (max (car l) (cadr l)))]
    [else (f-c (cdr l) (- i 1) (lambda (v1)
                                 (f-c (cddr l) (- i 2) (lambda (v2)
                                                         (cont (max v1 (+ (car l) v2)))))))]))
