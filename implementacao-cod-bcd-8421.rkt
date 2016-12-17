#lang racket

(require "manipulacao-wire.rkt"
         "blocos-logicos.rkt")
(provide cod-bcd-8421)

(define (cod-bcd-8421 in out3 out2 out1 out0) ;codificador BCD-8421
  (let ((in0 (list-ref in 0))
        (in1 (list-ref in 1))
        (in2 (list-ref in 2))
        (in3 (list-ref in 3))
        (in4 (list-ref in 4))
        (in5 (list-ref in 5))
        (in6 (list-ref in 6))
        (in7 (list-ref in 7))
        (in8 (list-ref in 8))
        (in9 (list-ref in 9))
        (c1 (make-wire))
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


