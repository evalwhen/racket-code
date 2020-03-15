#lang racket

(define (longest-palind-length s)
  (define (longest s i j)
    (let ([slen (+ (- j i) 1)])
      (cond
        [(= 0 slen) 0]
        [(= 1 slen) 1]
        [(char=? (string-ref s i)
                 (string-ref s j))
         (let ([v (longest s
                           (+ i 1)
                           (- j 1))])
           (if (= v (- slen 2))
               slen
               (max
                v
                (longest s (+ i 1) j)
                (longest s i (- j 1)))))]
        [else (max (longest s (add1 i) j)
                   (longest s i (sub1 j)))])))
  (longest s
           0
           (- (string-length s) 1)))


(define (longest-palindromic-string s)
  (define longest
    (lambda (s i j)
      (let ([slen (add1 (- j i))])
        (cond
          [(= 0 slen) ""]
          [(= 1 slen) (substring s i (add1 j))]
          [(char=? (string-ref s i)
                   (string-ref s j))
           (let ([v (longest s
                             (+ i 1)
                             (- j 1))])
             (if (= (string-length v) (- slen 2))
                 (format "~a~a~a"
                         (string-ref s i)
                         v
                         (string-ref s j))
                 (max-string
                  (list v
                        (longest s (+ i 1) j)
                        (longest s i (- j 1))))))]
          [else (max-string (list (longest s (add1 i) j)
                                  (longest s i (sub1 j))))]))))
  (longest s 0 (sub1 (string-length s))))


(define max-string
  (lambda (args)
    (cond
      [(null? args) ""]
      [else (let ([str1 (car args)]
                  [str2 (max-string (cdr args))])
              (if (> (string-length str1)
                     (string-length str2))
                  str1
                  str2))])))
