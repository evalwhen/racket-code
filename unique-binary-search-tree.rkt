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
    [(< n 1) '(())]
    [(= n 1) (list (list start (list) (list)))]
    [else (let loop ([i 0] [res '()])
            (cond
              [(>= i n) res]
              [else (loop (+ i 1)
                          (append (append-tree (+ start i)
                                             (ubs start i)
                                             (ubs (+ start i 1) (- n i 1)))
                                  res))]))]))

;; 以 mid 作为 value, left 节点列表以及 right 节点列表的组合作为左右子树，输出所有可能的树。
(define (append-tree mid left right)
  (cond
    [(or (null? left) (null? right)) '()]
    [else (append (add-left-node-to-tree mid (car left) right)
                  (append-tree mid (cdr left) right))]))

(define (add-left-node-to-tree mid left-node right)
  (cond
    [(null? right) '()]
    [else (cons  (list mid left-node (car right))
                 (add-left-node-to-tree mid left-node (cdr right)))]))

(module+ test
  (require rackunit)
  (check-equal? 5 (ubs-counter 3) "n = 3")
  (ubs 1 3))
