#lang racket
;; compute the length of longest common subsequence of two string s and t,
;; the contiguous is not required for subsequence.
(define (lcs-length s t)
  (define (helper s t sl tl)
    (cond
      [(or (= sl 0) (= tl 0)) 0]
      [else (cond
              [(char=? (string-ref s 0)
                       (string-ref t 0))
               (+ 1 (helper (substring s 1 sl)
                            (substring t 1 tl)
                            (sub1 sl)
                            (sub1 tl)))]
              [else
               (max (helper s
                            (substring t 1 tl)
                            sl
                            (sub1 tl))
                    (helper (substring s 1 sl)
                            t
                            (sub1 sl)
                            tl))])]))
  (helper s t (string-length s) (string-length t)))

(define (lcs s t)
  (define (helper s t sl tl)
    (cond
      [(or (= sl 0) (= tl 0)) ""]
      [else (cond
              [(char=? (string-ref s 0)
                      (string-ref t 0))
               (format "~a~a"
                       (string-ref s 0)
                       (helper (substring s 1 sl)
                               (substring t 1 tl)
                               (sub1 sl)
                               (sub1 tl)))]
              [else (let ([str1 (helper s
                                        (substring t 1 tl)
                                        sl
                                        (sub1 tl))]
                          [str2 (helper (substring s 1 sl)
                                        t
                                        (sub1 sl)
                                        tl)])
                      (cond
                        ;; (max str1 str2)
                        [(> (string-length str1) (string-length str2)) str1]
                        [else str2]))])]))
  (helper s t (string-length s) (string-length t)))

(module+ test
  (lcs "abc" "ac"))
