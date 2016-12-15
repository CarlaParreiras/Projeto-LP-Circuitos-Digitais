#lang racket

(require "manipulacao-queue.rkt")
(provide get-signal
         set-signal!
         add-action!
         make-wire)

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (call-each procedures)
  (if (null? procedures)
      (display "")
      (begin
        ((mcar procedures))
        (call-each (mcdr procedures)))))

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value) ;toda vez que o wire muda de sinal, chama as ações.
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          (display "")))    
    (define (accept-action-procedure! proc)
      (set! action-procedures (mcons proc action-procedures))
      (proc))    
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation - WIRE" m))))
    dispatch))