#lang racket

;; https://leetcode.com/problems/container-with-most-water/description/
(define f
  (lambda (h l)
    (let loop ([h h] [l l] [i 1] [max 0])
      (cond
        [(null? l) max]
        [else
         (let ([area (* i (min h (car l)))])
           (cond
             [(> area max) (loop h
                                 (rest l)
                                 (add1 i)
                                 area)]
             [else (loop h
                         (rest l)
                         (add1 i)
                         max)]))]))))

(define f2
  (lambda (l)
    (let loop ([l l] [max 0])
      (cond
        [(< (length l) 2) max]
        [else
         (let ([a (f (car l) (rest l))])
           (cond
             [(> a max) (loop (rest l) a)]
             [else (loop (rest l) max)]))]))))

(f2 '(1 8 6 2 5 4 8 3 7))
