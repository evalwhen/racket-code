#lang racket

(define (rev-list l res)
  (cond
    [(null? l) res]
    [else (rev-list (cdr l) (cons (car l) res))]))

(define (rev-list-loop l)
  (let loop ([l l] [res '()])
    (cond
      [(null? l) res]
      [else (loop (cdr l) (cons (car l) res))])))
