#lang racket

(define (rem-nth l n)
  (cond
    [(= n 0) (cdr l)]
    [else (cons (car l)
                (rem-nth (cdr l) (- n 1)))]))

(define (rem-nth-loop l n)
  (let loop ([l l] [i 0] [res '()])
    (cond
      [(null? l) (reverse res)]
      [else
       (cond
         [(= i n) (loop (cdr l) (add1 i) res)]
         [else (loop (cdr l) (add1 i) (cons (car l) res))])])))
