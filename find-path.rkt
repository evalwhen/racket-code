#lang racket

(define (find t v res)
  (cond
    [(null? t) (reverse res)]
    [else
     (cond
       [(is-internal t) (if (= (node-value t) v)
                            (cons (node-value t) res)
                            (let ([l (find (node-left t)
                                           v
                                           (cons (node-value t) res))])
                              (if (null? l)
                                  (find (node-right t)
                                        v
                                        (cons (node-value t) res ))
                                  l)))]
       [(is-leaf t) (if (= (node-value t) v)
                        (cons (node-value t) res)
                        '())])]))

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
