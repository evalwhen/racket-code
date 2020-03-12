#lang racket

(define (trav t)
  (cont
   [(null? t) '()]
   [else
    (append (trav (node-left t))
            (trav (node-right t))
            (list (node-value t)))]))

(define (node-value nd)
  (car nd))

(define (node-left nd)
  (cadr nd))

(define (node-right nd)
  (caddr nd))

(define (is-leaf nd)
  (and (null? (node-left nd))
       (null? (node-right nd))))

(define (is-internal nd)
  (not (is-leaf nd)))

(define a-tree
  '(3
    (5 (6 () ())
       (2 (7 () ())
          (4 () ())))
    (1 (0 () ())
       (8 () ()))))
