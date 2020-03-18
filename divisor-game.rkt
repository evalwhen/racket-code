#lang racket

(define (divisor-game n)
  (cond
    [(<= n 1) false]
    [else (not (divisor-game (- n 1)))]))
