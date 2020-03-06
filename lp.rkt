#lang racket

(define (is-sp s i j)
  (cond
    [(= j 0) false]
    [(= j 1) true]
    [(= j 2) (char=? (string-ref s i)
                     (string-ref s (- j 1)))]
    [else (and (char=? (string-ref s i)
                       (string-ref s (- j 1)))
               (is-sp s (+ i 1) (- j 1)))]))

(define (all-substring s)
  (let loop1 ([s s]
             [len (string-length s)]
             [res '()])
    (cond
      [(= len 0) res]
      [else (loop1 s
                   (sub1 len)
                   (substring-of-same-length s 0 len res))])))

;; 返回 s 的所有长度为 （j - i) 的字串, (j-i) 不能大于 s 的长度
(define (substring-of-same-length s i j res)
  (cond
    [(> j (string-length s)) res]
    [else (substring-of-same-length s (add1 i) (add1 j) (cons (substring s i j) res))]))
