#lang racket

(require "manipulacao-wire.rkt"
         "implementacao-simulacao.rkt"
         "implementacao-adders.rkt")

(define fa-1 (make-wire))
(define fa-2 (make-wire))
(define fa-c-in (make-wire))
(define fa-s (make-wire))
(define fa-c-out (make-wire))

(full-adder fa-1 fa-2 fa-c-in fa-s fa-c-out)

(display "FA - Start values:")
(probe 'sum fa-s)
(probe 'carry fa-c-out)
(display "\n")

;soma 1+1 com carry: (1+1)+1
(set-signal! fa-1 1)
(display "\nFA - Changed values:")
(propagate)
(display "\n")

(set-signal! fa-2 1)
(display "\nFA - Changed values:")
(propagate)
(display "\n")

(set-signal! fa-c-in 1)
(display "\nFA - Changed values:")
(propagate)