#lang racket

(define counting-bits
  (lambda (n)
    (cond
      [(= n 0) 0]
      [(= n 1) 1]
      [else
       (+ (counting-bits (floor (/ n 2)))
          (modulo n 2))])))
