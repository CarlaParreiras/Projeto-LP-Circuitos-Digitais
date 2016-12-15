#lang racket

(require "manipulacao-wire.rkt"
         "implementacao-agenda.rkt")
(provide inverter
         and-gate
         or-gate
         xor-gate
         nand-gate
         nor-gate)

(define inverter-delay 1) ;os valores para inverter-delay, and-gate-delay e or-gate-delay podem ser escolhidos arbitrariamente para cada circuito

(define and-gate-delay 2)

(define or-gate-delay 3)

(define xor-gate-delay 4)

(define nand-gate-delay 5)

(define nor-gate-delay 6)

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)
(define (logical-and s1 s2)
  (if (= s1 s2 1)
      1
      0))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
(define (logical-or s1 s2)
  (if (= s1 s2 0)
      0
      1))

(define (xor-gate a1 a2 output)
  (define (xor-action-procedure)
    (let ((new-value
           (logical-xor (get-signal a1) (get-signal a2))))
      (after-delay xor-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 xor-action-procedure)
  (add-action! a2 xor-action-procedure)
  'ok)
(define (logical-xor s1 s2)
  (if (or (= s1 s2 0)(= s1 s2 1))
      0
      1))

(define (nand-gate a1 a2 output)
  (define (nand-action-procedure)
    (let ((new-value
           (logical-nand (get-signal a1) (get-signal a2))))
      (after-delay nand-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 nand-action-procedure)
  (add-action! a2 nand-action-procedure)
  'ok)
(define (logical-nand s1 s2)
  (if (= s1 s2 1)
      0
      1))

(define (nor-gate a1 a2 output)
  (define (nor-action-procedure)
    (let ((new-value
           (logical-nor (get-signal a1) (get-signal a2))))
      (after-delay nor-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 nor-action-procedure)
  (add-action! a2 nor-action-procedure)
  'ok)
(define (logical-nor s1 s2)
  (if (= s1 s2 0)
      1
      0))