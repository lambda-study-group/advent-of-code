#lang racket

; receives a function and a list of integers
; the function f determines if it's gonna be a sum of two or three numbers
(define (find-2020 f xs)
  (let ([value (f (first xs) (rest xs))])
    (if (number? value) value (find-2020 f (rest xs)))))
   
; returns the multiplication of two numbers which the sum is equal to 2020
; if there is no such a pair, returns false
(define (check-two x xs)
  (cond
    [(null? xs) #f]
    [(equal? (+ x (first xs)) 2020)
     (* x (first xs))]
    [else
     (check-two x (rest xs))]))


; same as check-two, but for three numbers
(define (check-three x xs)
  (cond
    [(null? xs) #f]
    [else 
     (let ([value (compare-three x (first xs) xs)])
       (if (number? value) value (check-three x (rest xs))))]))


; if there's some number from xs that x + y + number is equal to 2020, returns their multiplication
; otherwise returns false
(define (compare-three x y xs)
  (cond
    [(null? xs) #f]
    [(equal? (+ x y (first xs)) 2020)
     (* x y (first xs))]
    [else (compare-three x y (rest xs))]))



