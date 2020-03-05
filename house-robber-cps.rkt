#lang eopl

(define-datatype continuation continuation?
  (end-cont)
  (f-c-1
   (l (list-of integer?))
   (i integer?)
   (cont continuation?))
  (f-c-2
   (v1 integer?)
   (a integer?)
   (cont continuation?)))

(define (f-c2 l i cont)
  (cond
    [(<= i 0) (apply-cont cont 0)]
    [(= i 1) (apply-cont cont (car l))]
    [(= i 2) (apply-cont cont (max (car l) (cadr l)))]
    [else (f-c2 (cdr l) (- i 1) (f-c-1 l i cont))]))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
           (end-cont () val)
           (f-c-1 (l i cont)
                  (f-c2 (cddr l) (- i 2) (f-c-2 val (car l) cont)))
           (f-c-2 (v1 a cont)
                  (apply-cont cont (max v1 (+ a val)))))))
(f-c2 '(1 2 3 4) 4 (end-cont))
(f-c2 '(2 3 4) 3 (f-c-1 '(1 2 3 4) 4 (end-cont)))
(f-c2 '(3 4) 2 (f-c-1 '(2 3 4) 3 (f-c-1 '(1 2 3 4) 4 (end-cont))))
(apply-cont c 4)
