#!/usr/bin/env racket
#lang racket

(define (string-map proc str)
  (list->string (map proc (string->list str))))

(define (string->seat-id str)
  (define/match (char->digit char)
    [((or #\F #\L)) #\0]
    [((or #\B #\R)) #\1])
  (string->number (string-map char->digit str) 2))

(define (port->input)
  (map string->seat-id (port->lines)))

(let ([seat-ids (port->input)])
  (apply max seat-ids))
