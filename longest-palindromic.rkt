#lang racket

;; 是 all-substring 的改进版
;; 思路：从 s 的最长字串开始扫描，一旦发现回文串，立马返回。
(define (longest-p s)
  (let loop1 ([s s]
              [len (string-length s)])
    (cond
      [(= len 0) ""]
      [else
       (let ([sub (f s 0 len)])
         (cond
           [(non-empty-string? sub) sub]
           [else (loop1 s (sub1 len))]))])))

;; 找到所有 s 中长度为 j - i 的字串中的第一个回文串
;; 是 substring-of-same-length 的改进版
(define (f s i j)
  (cond
    [(> j (string-length s)) ""]
    [else
     (let ([sub (substring s i j)])
       (cond
        [(is-sp sub 0 (- j i)) sub]
        [else (f s (add1 i) (add1 j))]))]))


(define (is-sp s i j)
  (cond
    [(= j 0) false]
    [(= j 1) true]
    [(= j 2) (char=? (string-ref s i)
                     (string-ref s (- j 1)))]
    [else (and (char=? (string-ref s i)
                       (string-ref s (- j 1)))
               (is-sp s (+ i 1) (- j 1)))]))

