#lang racket

(define (invert-tree t)
  (cond
    [(null? t) '()]
    [else
     (list (car t)
           (invert-tree (cadr t))
           (invert-tree (caddr t)))]))
