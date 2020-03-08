#lang racket

(define (lco l1 l2)
  (define (single n l2)
    (cond
      [(null? l2) '()]
      [else (cons (list n (car l2))
                  (single n (cdr l2)))]))
  (cond
    [(or (null? l1) (null? l2)) '()]
    [else (append (single (car l1) l2) (lco (cdr l1) l2))]))

(define (lco-list l s)
  (cond
    [(= s 0) '()]
    [(= s 1) (mklist (car l))]
    [else (fll (lco (car l) (cadr l)) (lco-list (cddr l) (- s 2)))]))

(define (fll ll1 ll2)
  (define (single l ll2)
    (cond
      [(null? ll2) '()]
      [else (cons (append l (car ll2))
                  (single l (cdr ll2)))]))
  (cond
    [(null? ll1) '()]
    [(null? ll2) ll1]
    ;; [(and (null? ll2) (not (null? ll1))) ll1]
    [else (append (single (car ll1) ll2) (fll (cdr ll1) ll2))]))

(define (mklist l)
  (cond
    [(null? l) '()]
    [else (cons (list (car l)) (mklist (cdr l)))]))
