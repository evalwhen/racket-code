#lang racket

;; https://leetcode.com/problems/add-two-numbers/description/
;; l1 length = l2 length
(define (add-two-sum l1 l2 c)
  (cond
    [(null? l1) (if (= c 0) '() '(1))]
    [else
     (let ([sum (+ (car l1) (car l2) c)])
       (cond
         [(>= sum 10) (cons (- sum 10)
                            (add-two-sum (cdr l1)
                                         (cdr l2)
                                         1))]
         [else (cons sum
                     (add-two-sum (cdr l1)
                                  (cdr l2)
                                  0))]))]))

;; l1 length != l2 length
(define (add-two-sum3 l1 l2 c)
  (cond
    [(and (null? l1) (null? l2)) (if (= c 0) '() '(1))]
    [else
     (let ([sum (+ (if (null? l1) 0 (car l1))
                   (if (null? l2) 0 (car l2))
                   c)])
       (cond
         [(>= sum 10) (cons (- sum 10)
                           (add-two-sum3 (if (null? l1) l1 (cdr l1))
                                         (if (null? l2) l2 (cdr l2))
                                         1))]
         [else (cons sum
                     (add-two-sum3 (if (null? l1) l1 (cdr l1))
                                   (if (null? l1) l1 (cdr l1))
                                  0))]))]))

(define (add-two-sum2 l1 l2 c r)
  (cond
    [(null? l1) (reverse (if (= c 0) r (cons 1 r)))]
    [else
     (let ([sum (+ (car l1) (car l2) c)])
       (if (>= sum 10)
           (add-two-sum2 (cdr l1)
                        (cdr l2)
                        1
                        (cons (- sum 10) r))
           (add-two-sum2 (cdr l1)
                        (cdr l2)
                        0
                        (cons sum r))))]))

(add-two-sum '(1 3 4) '(2 4 6) 0)
(add-two-sum2 '(1 3 4) '(2 4 6) 0 '())
