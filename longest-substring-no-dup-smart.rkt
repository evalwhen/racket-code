#lang racket


;; j, i 始终指向一个没有重复的字串
;; i 的更新操作是 add1
;; 当 i 发现当前字符在之前存在过，
;; 那么只需要查看之前存在的位置是否在 j, i 的范围内
;; 如果是，那么说明 j, i 之间出现了重复字符，更新 j 的位置保证<没有重复>。
;; 如果否，那么 j 的位置不需要更新
(define (lsnd s)
  (let ([v '()])
    (let loop ([i 0] [j 0] [m 0] [v '()])
      (cond
        [(= i (string-length s)) m]
        [else
         (let ([existed (assoc (string-ref s i) v)])
           (cond
             [(not existed)
              (loop (add1 i)
                    j
                    (max m (add1 (- i j)))
                    (cons (list (string-ref s i) i) v))]
             [else (let* ([new-j (if (>= (cadr existed) j) (add1 (cadr existed)) j)] ;; 查看 i, j 之间是否出现了重复字符
                          [new-m (max m (add1 (- i new-j)))])
                     (loop (add1 i)
                           new-j
                           new-m
                           (cons (list (string-ref s i) i) v)))]))]))))
