#lang racket


(define (decode l)
  (cond
    [(null? l) '()]
    [(null? (cdr l)) (cons l '())]
    [(null? (cddr l)) (append (cons l (decode (cddr l)))
                              (cons (list (list (car l) (cadr l)))
                                    '()))]
    [else (append (append-first (car l) (decode (cdr l)))
                  (append-first  (list (car l) (cadr l)) (decode (cddr l))))]))

(define (append-first a l)
  (cond
    [(null? l) '()]
    [else (cons (cons a (car l))
                (append-first a (cdr l)))]))

(define (decode-n l)
  (cond
    [(null? l) 0]
    [(null? (cdr l)) 1]
    [(null? (cddr l)) 2]
    [else (+ (decode-n (cdr l))
             (decode-n (cddr l)))]))

(define (decode-iter l prev1 prev2)
  (cond
    [(null? l) prev1]
    [(null? (cdr l)) prev1]
    [else (decode-iter (cdr l) (+ prev1 prev2) prev1)]))

(define (decode-s s z)
  (cond
    [(= z 0) 0]
    [(= z 1) (if (check-valid s 1 9) 1 0)]
    [(= z 2) (if (check-valid s 10 26) 2 0)]
    [else (+ (decode-s (substring s 1 z) (- z 1))
             (decode-s (substring s 2 z) (- z 2)))]))

(define (check-valid s l h)
  (let ([n (string->number s)])
    (and (>= n l)
         (<= n h))))
