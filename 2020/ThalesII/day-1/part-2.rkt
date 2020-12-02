#lang racket

(define (port->numbers [port (current-input-port)])
  (map string->number (port->lines port)))

(define (find-two lst k)
  (define ((pred x) y)
	(= (+ x y) k))
  (match lst
	['() #f]
	[(cons x xs)
	 (define y (findf (pred x) xs))
	 (cond
	   [y (* x y)]
	   [else (find-two xs k)])]))

(define (find-three lst k)
  (match lst
	['() #f]
	[(cons x xs)
	 (define yz (find-two xs (- k x)))
	 (cond
	   [yz (* x yz)]
	   [else (find-three xs k)])]))

(define (main)
  (let [(nums (port->numbers))]
	(displayln (find-three nums 2020))))

(main)
