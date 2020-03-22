#lang racket

(require sicp-pict)

(define wave-painter-segments
  (list (make-segment (make-vect 0.2 0.0) (make-vect 0.4 0.4))
        (make-segment (make-vect 0.4 0.4) (make-vect 0.3 0.5))
        (make-segment (make-vect 0.3 0.5) (make-vect 0.1 0.3))
        (make-segment (make-vect 0.1 0.3) (make-vect 0.0 0.6))
        (make-segment (make-vect 0.0 0.8) (make-vect 0.1 0.5))
        (make-segment (make-vect 0.1 0.5) (make-vect 0.3 0.6))
        (make-segment (make-vect 0.3 0.6) (make-vect 0.4 0.6))
        (make-segment (make-vect 0.4 0.6) (make-vect 0.3 0.8))
        (make-segment (make-vect 0.3 0.8) (make-vect 0.4 1.0))
        (make-segment (make-vect 0.6 1.0) (make-vect 0.7 0.8))
        (make-segment (make-vect 0.7 0.8) (make-vect 0.6 0.6))
        (make-segment (make-vect 0.6 0.6) (make-vect 0.8 0.6))
        (make-segment (make-vect 0.8 0.6) (make-vect 1.0 0.4))
        (make-segment (make-vect 1.0 0.2) (make-vect 0.6 0.4))
        (make-segment (make-vect 0.6 0.4) (make-vect 0.8 0.0))
        (make-segment (make-vect 0.7 0.0) (make-vect 0.5 0.3))
        (make-segment (make-vect 0.5 0.3) (make-vect 0.3 0.0))))
(define wave (segments->painter wave-painter-segments))

;(define (flipped-pairs painter)
;  (let ([painter2 (beside painter (flip-vert painter))])
;    (below painter2 painter2)))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) 
                       (tr painter)))
          (bottom (beside (bl painter) 
                          (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 
         (square-of-four identity 
                         flip-vert
                         identity 
                         flip-vert)))
    (combine4 painter)))

(define wave4 (flipped-pairs wave))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter 
                                  (- n 1))))
        (beside painter 
                (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter
               (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right 
                                   right))
              (corner (corner-split painter 
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right 
                         corner))))))

;(define (square-limit painter n)
;  (let ((quarter (corner-split painter n)))
;    (let ((half (beside (flip-horiz quarter) 
;                        quarter)))
;      (below (flip-vert half) half))))


(define (square-limit painter n)
  (let ((combine4 
         (square-of-four flip-horiz 
                         identity
                         rotate180 
                         flip-vert)))
    (combine4 (corner-split painter n))))
(define my-painter (bitmap->painter "/home/melp/Downloads/1496705311.jpg"))
(define my-painter2 (bitmap->painter "/home/melp/Downloads/1691619715.jpg"))
(define my-painter3 (bitmap->painter "/home/melp/Downloads/121613413.jpg"))
(define test-painter (square-limit my-painter3 2))
(paint test-painter #:width 400 #:height 400)
