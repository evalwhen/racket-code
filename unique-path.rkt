#lang racket

(define (unique-path m n)
  (define (helper x y)
    (cond
      [(or (= x m) (= y n)) 1]
      [else (+
             (helper x (add1 y))
             (helper (add1 x) y))]))
  (helper 1 1))

(define (unique-path-2 m n)
  (cond
    [(or (= m 1) (= n 1)) 1]
    [else (+ (unique-path-2 (- m 1) n)
             (unique-path-2 m (- n 1)))]))
