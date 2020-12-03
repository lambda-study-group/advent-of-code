#!/usr/bin/env racket
#lang racket

(define (string-get str k [default #f])
  (if (< k (string-length str)) (string-ref str k) default))

(define (valid-entry? entry)
  (match-define (list lo hi char pass) entry)
  (xor (char=? (string-get pass (sub1 lo)) char)
       (char=? (string-get pass (sub1 hi)) char)))

(define (string->entry str)
  (match-define (list _ lo-str hi-str char-str pass)
    (regexp-match #px"^(\\d+)-(\\d+) (.): (.+)$" str))
  (list (string->number lo-str)
        (string->number hi-str)
        (string-ref char-str 0)
        pass))

(define (port->entries)
  (map string->entry (port->lines)))

(let ([entries (port->entries)])
  (count valid-entry? entries))
