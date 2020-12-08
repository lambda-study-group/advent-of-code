#!/usr/bin/env racket
#lang racket

(define (halted? prog state)
  (= (car state) (hash-count prog)))

(define/match (run-instruction inst state)
  [((cons 'nop imm) (cons pos acc)) (cons (add1 pos) acc)]
  [((cons 'acc imm) (cons pos acc)) (cons (add1 pos) (+ acc imm))]
  [((cons 'jmp imm) (cons pos acc)) (cons (+ pos imm) acc)])

(define (run-program prog [state (cons 0 0)] [seen (set)])
  (match-define (cons pos acc) state)
  (cond
    [(halted? prog state) state]
    [(set-member? seen pos) state]
    [else (define new-state (run-instruction (hash-ref prog pos) state))
          (define new-seen (set-add seen pos))
          (run-program prog new-state new-seen)]))

(define (string->instruction str)
  (match-define (list opcode-str imm-str) (string-split str))
  (cons (string->symbol opcode-str) (string->number imm-str)))

(define (port->program)
  (for/hash ([(inst-str pos) (in-indexed (port->lines))])
    (values pos (string->instruction inst-str))))

(cdr (run-program (port->program)))
