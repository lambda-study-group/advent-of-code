#lang racket

(define (tails lst)
  (cond
    [(empty? lst) '(())]
    [else (cons lst (tails (rest lst)))]))

(define (solve lst)
  (for/or ([xs (tails lst)]
           #:unless (empty? xs)
           [y (rest xs)]
           #:when (= (+ (first xs) y) 2020))
    (* (first xs) y)))

(define (port->numbers [port (current-input-port)])
  (map string->number (port->lines port)))

(let [(input (port->numbers))]
    (displayln (solve input)))
