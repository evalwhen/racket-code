#lang racket

(define (path-nums t n)
  (define (helper t n)
    (cond
      [(null? t) 0]
      [else
       (+ (if (= n (node-value t)) 1 0)
          (helper (node-left-tree t) (- n (node-value t)))
          (helper (node-right-tree t) (- n (node-value t))))]))
  (cond
    [(null? t) 0]
    [else
     (+ (helper t n)
        (path-nums (node-left-tree t) n)
        (path-nums (node-right-tree t) n))]))

;; tail call
(define (path-nums2 t n)
  (define (helper t n counter)
    (cond
      [(null? t) counter]
      [else
       (helper (node-left-tree t)
               (- n (node-value t))
               (helper (node-right-tree t)
                       (- n (node-value t))
                       (if (= n (node-value t))
                           (+ counter 1)
                           counter)))]))
  (cond
    [(null? t) 0]
    [else
     (+ (helper t n 0)
        (path-nums (node-left-tree t) n)
        (path-nums (node-right-tree t) n))]))

(define (node-value nd)
  (car nd))

(define (node-left-tree nd)
  (cadr nd))

(define (node-right-tree nd)
  (caddr nd))

(define (is-leaf nd)
  (and (null? (node-left-tree nd))
       (null? (node-right-tree nd))))

(define a-tree
  '(5 (4 (11 (7 ()
                ())
             (2 ()
                ()))
         ())
      (8 (13 ()
             ())
         (4 ()
            (5 ()
               ())))))
