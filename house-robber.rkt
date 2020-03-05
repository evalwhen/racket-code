#lang eopl

;; https://leetcode.com/problems/house-robber/description/

(define (robe l)
  (helper l (length l)))

(define (helper l i)
  (cond
    [(<= i 0) 0]
    [(= i 1) (car l)]
    [(= i 2) (max (car l) (cadr l))]
    [else (max (helper (cdr l) (- i 1))                  ;; 不偷第一家的情况
               (+ (car l) (helper (cddr l) (- i 2))))])) ;; 偷第一家的情况, (偷第一家就不能偷第二家)

(define (f-iter l c)
  (let loop ([a 0] [b 0] [l l] [c c])
    (cond
      [(= c 0) b]
      [else (loop b (max b (+ a (car l))) (cdr l) (- c 1))])))

(define (f-c l i cont)
  (cond
    [(<= i 0) (cont 0)]
    [(= i 1) (cont (car l))]
    [(= i 2) (cont (max (car l) (cadr l)))]
    [else (f-c (cdr l) (- i 1) (lambda (v1)
                                 (f-c (cddr l) (- i 2) (lambda (v2)
                                                         (cont (max v1 (+ (car l) v2)))))))]))

(robe '(2 7 9 3 1))

(define (fib n)
  (cond
    [(<= n 1) 1]
    [else (+ (fib (- n 1))
             (fib (- n 2)))]))