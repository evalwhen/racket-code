#lang racket

;; 是 all-substring 的改进版
;; 思路：从 s 的最长字串开始扫描，一旦发现字串无重复，立马返回。
(define (longest-sub-no-dup s)
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
         [(no-dup sub) sub]
         [else (f s (add1 i) (add1 j))]))]))



;; slowly
(define (no-dup s)
  (no-dup-char-list? (string->list s) (string-length s)))

(define (no-dup-char-list? s i)
  (cond
    [(= i 1) true]
    [(= i 2) (not (char=? (car s)
                          (cadr s)))]
    [else (and (not (member (car s) (cdr s)))
               (no-dup-char-list? (cdr s) (- i 1)))]))
