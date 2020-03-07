#lang racket

;; l is a sorted
(define (rm-dup l)
  (cond
    [(null? l) '()]
    [else
     (cond
       [(null? (cdr l)) l]
       [(= (car l) (cadr l)) (rm-dup (cdr l))]
       [else (cons (car l) (rm-dup (cdr l)))])]))
