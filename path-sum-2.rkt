(define (sum-path t n res)
  (cond
    [(null? t ) res]
    [else
     (cond
       [(and (is-leaf t) (= (node-value t) n)) (cons (list n) res)]
       [else (append (prefix-list (node-value t) (sum-path (node-left-tree t) (- n (node-value t)) res))
                     (prefix-list (node-value t) (sum-path (node-right-tree t) (- n (node-value t)) res)))])]))

(define (sum-path2 t n cur res)
  (cond
    [(null? t) res]
    [else
     (cond
       [(not (is-leaf t)) (sum-path2 (node-left-tree t)
                                     (- n (node-value t))
                                     (cons (node-value t) cur)
                                     (sum-path2 (node-right-tree t)
                                                (- n (node-value t))
                                                (cons (node-value t) cur)
                                                res))]
       [(is-leaf t)
        (if (= n (node-value t))
            (sum-path2 '() n '() (cons (reverse (cons (node-value t) cur))
                                       res))
            res)])]))
(define (prefix-list n ll)
  (cond
    [(null? ll) '()]
    [else
     (cons (cons n (car ll))
           (prefix-list n (cdr ll)))]))
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

(has-path-sum a-tree 5)
