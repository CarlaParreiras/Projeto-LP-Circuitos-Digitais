#lang racket

(require "manipulacao-wire.rkt"
         "implementacao-simulacao.rkt"
         "implementacao-adders.rkt")

(define ha-1 (make-wire))
(define ha-2 (make-wire))
(define ha-s (make-wire))
(define ha-c (make-wire))

(half-adder ha-1 ha-2 ha-s ha-c)

(display "HA - Start values:")
(probe 'sum ha-s)
(probe 'carry ha-c)
(display "\n")

;soma 1+1
(set-signal! ha-1 1)
(display "\nHA - Changed values:")
(propagate)
(display "\n")

(set-signal! ha-2 1)
(display "\nHA - Changed values:")
(propagate)