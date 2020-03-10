#lang racket

(define (trav t)
  (cond
    [(null? t) '()]
    [else (append (trav (node-left t))
                  (list (node-value t))
                  (trav (node-right t)))]))
(define (trav-left t res)
  (cond
    [(null? t) res]
    [(is-leaf t) (cons t res)]
    [else (trav-left (node-left t) (cons (node-value t) res))]))

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
    (1 ()
       (2 ()
          ()))
    (4 ()
       ())))

