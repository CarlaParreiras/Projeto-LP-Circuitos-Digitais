#lang racket

(require "manipulacao-wire.rkt"
         "implementacao-simulacao.rkt"
         "implementacao-adders.rkt")

(define rca-a1 (make-wire))
(define rca-a2 (make-wire))
(define rca-a (list rca-a1 rca-a2))

(define rca-b1 (make-wire))
(define rca-b2 (make-wire))
(define rca-b (list rca-b1 rca-b2))

(define rca-s1 (make-wire))
(define rca-s2 (make-wire))
(define rca-s (list rca-s1 rca-s2))

(define rca-c (make-wire))

(ripple-carry-adder rca-a rca-b rca-s rca-c)

(display "RCA - Start values:")
(probe 'sum1 rca-s1)
(probe 'sum2 rca-s2)
(probe 'carry rca-c)

(display "\n")

;soma 11+01
(set-signal! rca-a1 1)
(display "\nRCA - Changed values:")
(propagate)
(display "\n")

(set-signal! rca-a2 1)
(display "\nRCA - Changed values:")
(propagate)
(display "\n")

(set-signal! rca-b2 1)
(display "\nRCA - Changed values:")
(propagate)
