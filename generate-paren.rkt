#lang racket

(require racket/mpair)

(define (ff n)
  (define res (list))
  (define (f s l r n)
    (cond
      [(= (string-length s) (* 2 n)) (set! res (cons s res))]
      [else
       (if (< l n)
           (f (string-append s "(")
              (+ l 1)
              r
              n
              )
           '())

       (if (< r l)
           (f (string-append s ")")
              l
              (+ r 1)
              n
              )
           null)
       ]))
  (f "" 0 0 n)
  res)
