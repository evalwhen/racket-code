#lang racket

(define (climbing-stairs n)
  (cond
    [(< n 1) 0]
    [(= n 1) 1]
    [(= n 2) 2]
    [else (+ (climbing-stairs (- n 1))
             (climbing-stairs (- n 2)))]))

(module+ test
  (require rackunit)
  (check-equal? (climbing-stairs 2) 2 "2 stairs case")
  (check-equal? (climbing-stairs 3) 3 "3 stairs case")
  (check-equal? (climbing-stairs 4) 5 "4 stairs case")
  )
