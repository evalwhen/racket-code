#lang racket

;; https://leetcode.com/problems/add-two-numbers/description/
(define (add-two-sum l1 l2 c)
  (cond
    [(null? l1) (if (= c 0) '() '(1))]
    [else
     (let ([sum (+ (car l1) (car l2) c)])
       (cons (if (>= sum 10)
                 (- sum 10)
                 sum)
             (add-two-sum (cdr l1)
                          (cdr l2)
                          1)))]))
