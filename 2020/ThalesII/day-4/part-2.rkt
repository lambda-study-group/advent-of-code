#!/usr/bin/env racket
#lang racket

(define (required-field-validator key [pred (const #t)])
  (lambda (hash)
    (and (hash-has-key? hash key)
         (pred (hash-ref hash key)))))

(define (regexp-field-validator key pattern [pred (const #t)])
  (required-field-validator key
    (lambda (str)
      (and (regexp-match-exact? pattern str)
           (pred (regexp-match pattern str))))))

(define (year-field-validator key min-year max-year)
  (regexp-field-validator key #px"\\d{4}"
    (match-lambda
      [(list year-str) (<= min-year (string->number year-str) max-year)])))

(define (height-field-validator key)
  (regexp-field-validator key #px"(\\d+)(cm|in)"
    (match-lambda
      [(list _ height-str "cm") (<= 150 (string->number height-str) 193)]
      [(list _ height-str "in") (<= 59 (string->number height-str) 76)])))

(define passport-valid?
  (conjoin (year-field-validator "byr" 1920 2002)
           (year-field-validator "iyr" 2010 2020)
           (year-field-validator "eyr" 2020 2030)
           (height-field-validator "hgt")
           (regexp-field-validator "hcl" #px"#[a-f\\d]{6}")
           (regexp-field-validator "ecl" #px"(amb|blu|brn|gry|grn|hzl|oth)")
           (regexp-field-validator "pid" #px"\\d{9}")))

(define (string->passport str)
  (for/hash ([field-str (string-split str)])
    (match-define (list key value) (string-split field-str ":"))
    (values key value)))

(define (port->input)
  (map string->passport (string-split (port->string) "\n\n")))

(count passport-valid? (port->input))
