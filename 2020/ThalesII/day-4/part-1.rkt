#!/usr/bin/env racket
#lang racket

(define (passport-valid? passport)
  (define required-fields '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))
  (andmap (λ (key) (hash-has-key? passport key)) required-fields))

(define (string->passport str)
  (for/hash ([field-str (string-split str)])
    (match-define (list key value) (string-split field-str ":"))
    (values key value)))

(define (port->input)
  (map string->passport (string-split (port->string) "\n\n")))

(count passport-valid? (port->input))
