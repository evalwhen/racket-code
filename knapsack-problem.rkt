#lang racket

;; ((2 . 3) (4 . 6))
(define (max-value l s)
  (cond
    [(or (null? l) (<= s 0)) 0]
    [(> (item-size (car l)) s) (max-value (cdr l) s)]
    [else
     (max
      (+ (item-value (car l))
         (max-value (cdr l) (- s (item-size (car l)))))
      (max-value (cdr l) s))]))

(define (item-size i)
  (car i))

(define (item-value i)
  (cdr i))

(module+ test
  (require rackunit)
  (check-= 10 (max-value '((2 . 3) (4 . 6) (2 . 7)) 4) 0 "wrong"))
