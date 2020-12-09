#!/usr/bin/env racket
#lang racket

(define (program-ref prog pos)
  (hash-ref prog pos '(hlt)))

(define (program-set prog pos inst)
  (hash-set prog pos inst))

(define (program-step prog state)
  (match-define (cons pos acc) state)
  (match (program-ref prog pos)
    [`(nop ,imm) (cons (add1 pos) acc)]
    [`(acc ,imm) (cons (add1 pos) (+ acc imm))]
    [`(jmp ,imm) (cons (+ pos imm) acc)]
    ['(hlt) state]))

(define (program-run prog [state (cons 0 0)] [seen (set)])
  (match-define (cons pos acc) state)
  (cond
    [(set-member? seen pos) state]
    [else (program-run prog (program-step prog state) (set-add seen pos))]))

(define (string->instruction str)
  (match-define (list opcode-str imm-str) (string-split str))
  (list (string->symbol opcode-str) (string->number imm-str)))

(define (port->program)
  (for/hash ([(inst-str pos) (in-indexed (port->lines))])
    (values pos (string->instruction inst-str))))

(cdr (program-run (port->program)))
