#lang racket

(require "manipulacao-wire.rkt"
         "implementacao-agenda.rkt")
(provide probe
         propagate)

(define (probe name wire) ;faz a "sonda" de um wire, passa a acompanhar as mudanças no mesmo
  (add-action! wire ;quando o sinal de wire mudar, o segundo argumento será executado 
               (lambda ()        
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

(define (propagate) ;executor da agenda: realiza cada procedimento da agenda em sequência
  (if (empty-agenda? the-agenda)
      (display "")
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))