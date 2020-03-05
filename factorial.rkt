#lang racket

(define (fact-r n)
  (cond
    [(= n 0) 1]
    [else (* n (fact-r (- n 1)))]))
