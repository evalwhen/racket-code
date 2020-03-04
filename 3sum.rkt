#lang racket

;; 找出 l 中值为 x 的下标
(define (eq-v x l i)
  (cond
    [(null? l) '()]
    [(= x (car l)) (cons i (eq-v x (cdr l) (+ i 1)))]
    [else (eq-v x (cdr l) (+ i 1))]))

;; 找出 l 中和为 v 的二元组
(define (2sum-eq-v v l j)
  (cond
    [(< (length l) 2) '()]
    [else
     (append
      (make-tuble j (eq-v (- v (car l)) (cdr l) (+ j 1)))
      (2sum-eq-v v (cdr l) (+ j 1)))]))

;; (f3 2 '(1 2 3)) => '((2 1) (2 2) (2 3))
(define (make-tuble i l)
  (cond
    [(null? l) '()]
    [else
     (cons (list i (car l))
           (make-tuble i (cdr l)))]))

;; 找出和为零的三元组
(define (3sum l k)
  (cond
    [(< (length l) 3) '()]
    [else
     (append
      (make-triple k
              (2sum-eq-v (- (car l))
                 (rest l)
                 (+ k 1)))
      (3sum (rest l) (+ k 1)))]))

;; (cons-v 2 '((1 2) (3 4) (5 6))) => '((2 1 2) (2 3 4) (2 5 6))
(define (make-triple v l)
  (cond
    [(null? l) '()]
    [else
     (cons (cons v (car l))
           (make-triple v (cdr l)))]))
