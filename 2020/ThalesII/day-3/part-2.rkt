#!/usr/bin/env racket
#lang racket

(define (vector-cycle-ref vec pos)
  (define new-pos (remainder pos (vector-length vec)))
  (vector-ref vec new-pos))

(define (grid-tree? grid x y)
  (vector-cycle-ref (vector-ref grid y) x))

(define (count-slope-trees grid sx sy)
  (define height (vector-length grid))
  (for/sum ([i (in-naturals)]
            #:break (>= (* i sy) height)
            #:when (grid-tree? grid (* i sx) (* i sy)))
    1))

(define (port->grid)
  (for/vector ([line (in-lines)])
    (for/vector ([char (in-string line)])
      (char=? char #\#))))

(let ([grid (port->grid)])
  (* (count-slope-trees grid 1 1)
	 (count-slope-trees grid 3 1)
	 (count-slope-trees grid 5 1)
	 (count-slope-trees grid 7 1)
	 (count-slope-trees grid 1 2)))
