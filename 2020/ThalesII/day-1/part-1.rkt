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

(define (main)
  (let [(nums (port->numbers))]
	(displayln (find-two nums 2020))))

(main)
