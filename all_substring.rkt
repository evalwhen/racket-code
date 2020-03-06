#lang racket

;; 找到 s 的所有字串
(define (all-substring s)
  (let loop1 ([s s]
              [len (string-length s)]
              [res '()])
    (cond
      [(= len 0) res]
      [else (loop1 s
                   (sub1 len)
                   (substring-of-same-length s 0 len res))])))

;; 返回 s 的所有长度为 （j - i) 的字串
(define (substring-of-same-length s i j res)
  (cond
    [(> j (string-length s)) res]
    [else (substring-of-same-length s (add1 i) (add1 j) (cons (substring s i j) res))]))
