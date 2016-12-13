#lang racket

; -- 3.3.2 Manipulação de queue --

(define (make-queue) (mcons '() '()))

(define (front-ptr queue) (mcar queue))

(define (rear-ptr queue) (mcdr queue))

(define (set-front-ptr! queue item) (set-mcar! queue item))

(define (set-rear-ptr! queue item) (set-mcdr! queue item))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (mcar (front-ptr queue))))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-mcdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (mcdr (front-ptr queue)))
         queue)))

; --//--

; MANIPULAÇÃO DE WIRES

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((mcar procedures))
        (call-each (mcdr procedures)))))

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value) ;toda vez que o wire muda de sinal, chama as ações.
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))    
    (define (accept-action-procedure! proc)
      (set! action-procedures (mcons proc action-procedures))
      (proc))    
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation - WIRE" m))))
    dispatch))

; AGENDA DE AÇÕES

(define (make-agenda) (mcons 0 '()))

(define (current-time agenda) (mcar agenda)) ;o primeiro valor indica o tempo passado

(define (segments agenda) (mcdr agenda)) ;o segundo valor carrega os segments, que indica as ações que devem ser feitas num determinado tempo

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (set-current-time! agenda time)
  (set-mcar! agenda time))

(define (set-segments! agenda segments)
  (set-mcdr! agenda segments))

(define (make-time-segment time queue) ;cada segment possui um tempo e as ações a serem realizadas nesse tempo
  (mcons time queue))

(define (segment-time s) (mcar s))

(define (segment-queue s) (mcdr s))

(define (first-segment agenda) (mcar (segments agenda)))

(define (rest-segments agenda) (mcdr (segments agenda)))

(define (add-to-agenda! time action agenda) ;adição de ações na agenda
  (define (belongs-before? segments) ;verifica se a ação a ser adicionada está num tempo anterior ao primeiro segment da agenda
    (or (null? segments)
        (< time (segment-time (mcar segments)))))
  (define (make-new-time-segment time action) ;cria um novo segment
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments) ;adiciona um segment na agenda, no tempo determinado por time
    (if (= (segment-time (mcar segments)) time)
        (insert-queue! (segment-queue (mcar segments))
                       action)
        (let ((rest (mcdr segments)))
          (if (belongs-before? rest)
              (set-mcdr!
               segments
               (mcons (make-new-time-segment time action)
                     (mcdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! ;se a ação pertence a um tempo anterior, ela é adicionada antes dos segments da agenda
         agenda
         (mcons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (begin (delete-queue! q)
           (if (empty-queue? q)
               (set-segments! agenda (rest-segments agenda))
               'pass))))

(define (after-delay delay action) ;adiciona a ação na agenda posteriormente a current-time, num tempo delay
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

; BLOCOS LÓGICOS

(define inverter-delay 2) ;os valores para inverter-delay, and-gate-delay e or-gate-delay podem ser escolhidos arbitrariamente para cada circuito

(define and-gate-delay 3)

(define or-gate-delay 5)

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

; SIMULAÇÃO

(define the-agenda (make-agenda))

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
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))


; Exemplo - CIRCUITO HALF-ADDER

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define input-1 (make-wire))

(define input-2 (make-wire))

(define sum (make-wire))

(define carry (make-wire))

(half-adder input-1 input-2 sum carry)

; Tracking

(probe 'sum sum) ;sum e carry iniciam com time 0 e signal 0
(probe 'carry carry)
(set-signal! input-1 1) ;modificamos o sistema e executamos a agenda
(propagate) ;ao propagarmos, temos que o sinal de carry será 0 e sum 1, como previsto no circuito
