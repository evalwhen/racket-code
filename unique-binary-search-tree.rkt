#lang racket

(define (ubs-counter n)
  (cond
    [(<= n 1) 1]
    [else (let loop ([i 1] [res 0])
            (cond
              [(> i n) res]
              [else (loop (+ i 1)
                          (+ res (* (ubs-counter (- i 1))
                                    (ubs-counter (- n i)))))]))]))

;; 输出以 start 开始，连续的 n 个整数构成的树的集合
;; (ubs 1 2) => '((2 (1 () ()) ()) (1 () (2 () ())))
(define (ubs start n)
  (cond
    [(< n 1) '()]
    [(= n 1) (list (list start (list) (list)))]
    [else (let loop ([i 0] [res '()])
            (cond
              [(>= i n) res]
              [else (loop (+ i 1)
                          (append (append-tree (+ start i)
                                             (ubs start i)
                                             (ubs (+ start i 1) (- n i 1)))
                                  res))]))]))

(define (append-tree mid left right)
  (cond
    [(and (null? left) (null? right)) '()]
    [(and (null? left) (not (null? right))) (cons (list mid
                                                        (list)
                                                        (car right))
                                                  (append-tree mid left (cdr right)))]
    [(and (not (null? left)) (null? right)) (cons (list mid
                                                        (car left)
                                                        (list))
                                                  (append-tree mid (cdr left) right))]
    [else (cons (list mid
                      (car left)
                      (car right))
                (append-tree mid (cdr left) (cdr right)))]))

(module+ test
  (require rackunit)
  (check-equal? 5 (ubs-counter 3) "n = 3")
  (ubs 1 3))
