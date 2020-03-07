#lang racket

(define (sum-root t cur res)
  (cond
    [(null? t) res]
    [else
     (cond
       [(is-internal t) (sum-root (node-left t)
                                   (+ (* cur 10)
                                      (node-value t))
                                   (sum-root (node-right t)
                                             (+ (* cur 10)
                                                (node-value t))
                                             res))]
       [(is-leaf t) (+ (+ (* cur 10)
                           (node-value t))
                        res)])]))

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
  '(1 (2 () ()) (3 () ())))
