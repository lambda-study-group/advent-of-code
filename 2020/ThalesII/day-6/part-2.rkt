#!/usr/bin/env racket
#lang racket

(define (string->group str)
  (map (compose1 list->set string->list)
       (string-split str "\n")))

(define (port->input)
  (map string->group
       (string-split (port->string) "\n\n")))

(for/sum ([group (port->input)])
  (set-count (apply set-intersect group)))
