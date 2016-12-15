#lang racket

(require "manipulacao-wire.rkt"
         "blocos-logicos.rkt"
         "implementacao-simulacao.rkt")
(provide half-adder
         full-adder
         ripple-carry-adder)

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder list-a list-b list-s c) 
  (let ((c-out (make-wire))) 
    (if (null? (cdr list-a))
        (set-signal! c 0)
        (ripple-carry-adder (cdr list-a) (cdr list-b) (cdr list-s) c-out)) 
    (full-adder (car list-a) (car list-b) c-out (car list-s) c)))