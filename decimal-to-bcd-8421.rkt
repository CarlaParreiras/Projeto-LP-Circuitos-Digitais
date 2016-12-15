#lang racket

(require "manipulacao-wire.rkt"
         "blocos-logicos.rkt"
         "implementacao-simulacao.rkt")

(define (dec->bcd-8421 in9 in8 in7 in6 in5 in4 in3 in2 in1 in0 out3 out2 out1 out0)
  (let ((c1 (make-wire))
        (c2 (make-wire))
        (c3 (make-wire))
        (c4 (make-wire))
        (c5 (make-wire))
        (c6 (make-wire)))
    (or-gate in4 in5 c1)
    (or-gate in6 in7 c2)
    (or-gate in2 in3 c3)
    (or-gate in7 in9 c4)
    (or-gate in5 in3 c5)
    (or-gate in1 c4 c6)
    (or-gate c6 c5 out0)
    (or-gate c1 c2 out2)
    (or-gate c2 c3 out1)
    (or-gate in8 in9 out3)
    'ok))

(define i0 (make-wire))
(define i1 (make-wire))
(define i2 (make-wire))
(define i3 (make-wire))
(define i4 (make-wire))
(define i5 (make-wire))
(define i6 (make-wire))
(define i7 (make-wire))
(define i8 (make-wire))
(define i9 (make-wire))
(define o0 (make-wire))
(define o1 (make-wire))
(define o2 (make-wire))
(define o3 (make-wire))

(dec->bcd-8421 i9 i8 i7 i6 i5 i4 i3 i2 i1 i0 o3 o2 o1 o0) ;ok

; tracking
(display "\nStart signals:")
(probe 'two-three o3)
(probe 'two-two o2)
(probe 'two-one o1)
(probe 'two-zero o0)
(display "\n")

;exemplo: simulação do codificador para o algarismo 9
(set-signal! i9 1)
(display "\nChanged signals:")
(propagate)
