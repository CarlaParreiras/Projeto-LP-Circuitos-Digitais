#lang racket

(require "manipulacao-wire.rkt"
         "blocos-logicos.rkt"
         "implementacao-simulacao.rkt"
         "implementacao-cod-bcd-8421.rkt")

(define (dec-to-list n list-n)
  (if (= n 0)
      list-n
      (dec-to-list (quotient n 10) (cons (remainder n 10) list-n))))

(define (bcd-8421 n) ;retorna o n (decimal) codificado em BCD-8421
  ;wires
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
  (define in (list i0 i1 i2 i3 i4 i5 i6 i7 i8 i9))  
  ;start circuit
  (cod-bcd-8421 in o3 o2 o1 o0)
  ;tracking
  (display "\nSimulation - Start signals:")
  (probe 'two-three o3)
  (probe 'two-two o2)
  (probe 'two-one o1)
  (probe 'two-zero o0)
  (display "\n\n")
  ;iteration on numbers
  (define (iter numbers lista)
    (if (empty? numbers)
        (reverse lista)
        (let ((item (car numbers)))
          (begin (display (format "Simulation - Number ~a:" item))
                 (set-signal! (list-ref in item) 1)
                 (propagate)
                 (display "\n\n")
                 (set-signal! (list-ref in item) 0)
                 (iter (cdr numbers) (cons (list (get-signal o3)
                                                 (get-signal o2)
                                                 (get-signal o1)
                                                 (get-signal o0))
                                           lista))))))
  (iter (dec-to-list n '()) '()))