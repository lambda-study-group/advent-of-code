#!/usr/bin/env racket
#lang racket

(define (solve lst)
  (for*/first ([x lst]
               [y lst]
               #:when (= (+ x y) 2020))
      (* x y)))

(define (port->numbers [port (current-input-port)])
  (map string->number (port->lines port)))

(let [(input (port->numbers))]
    (solve input))
