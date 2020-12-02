#!/usr/bin/env racket
#lang racket

(define (string-count str char)
  (count (λ (c) (char=? c char)) (string->list str)))

(define (valid-entry? entry)
  (match-define (list lo hi char pass) entry)
  (<= lo (string-count pass char) hi))

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
