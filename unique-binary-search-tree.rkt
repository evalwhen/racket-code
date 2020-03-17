#lang racket

(define (ubs n)
  (cond
    [(<= n 1) 1]
    [else (let loop ([i 1] [res 0])
            (cond
              [(> i n) res]
              [else (loop (+ i 1)
                          (+ res (* (ubs (- i 1))
                                    (ubs (- n i)))))]))]))
