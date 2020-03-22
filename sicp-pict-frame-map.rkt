#lang racket

(require sicp-pict)

(define a-frame (make-frame (make-vect 0.0 0.0) (make-vect 1.0 0.0) (make-vect 0.0 1.0)))

(define m (frame-coord-map a-frame))

(m (make-vect 0.1 0.2))
