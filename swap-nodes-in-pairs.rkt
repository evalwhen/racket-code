#lang racket

(define (swap-nodes l)
  (cond
    [(null? l) '()]
    [(null? (cdr l)) l]
    [else
     (cons (cadr l)
           (cons (car l)
                 (swap-nodes (cddr l))))]))
