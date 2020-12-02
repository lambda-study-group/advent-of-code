#lang racket

(define (tails lst)
  (cond
    [(empty? lst) '(())]
    [else (cons lst (tails (rest lst)))]))

(define (solve lst)
  (for/or ([xs (tails lst)]
           #:unless (empty? xs)
           [ys (tails (rest xs))]
           #:unless (empty? ys)
           [z (rest ys)]
           #:when (= (+ (first xs) (first ys) z) 2020))
    (* (first xs) (first ys) z)))

(define (port->numbers [port (current-input-port)])
  (map string->number (port->lines port)))

(let ([input (port->numbers)])
  (displayln (solve input)))
