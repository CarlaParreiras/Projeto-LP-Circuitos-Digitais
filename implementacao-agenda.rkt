#lang racket

(require "manipulacao-queue.rkt")
(provide the-agenda
         current-time
         empty-agenda?
         first-agenda-item
         remove-first-agenda-item!
         after-delay)

(define (make-agenda) (mcons 0 '()))

(define the-agenda (make-agenda))

(define (current-time agenda) (mcar agenda)) ;o primeiro valor indica o tempo passado

(define (segments agenda) (mcdr agenda)) ;o segundo valor carrega os segments, os quais indicam as ações que devem ser feitas num determinado tempo

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
  (define (belongs-before? segments) ;verifica se a agenda está vazia ou se a ação a ser adicionada está num tempo anterior ao primeiro segment da agenda
    (or (null? segments)
        (< time (segment-time (mcar segments)))))
  (define (make-new-time-segment time action) ;cria um novo segment
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments) ;quando o time é o mesmo que algum segment na agenda, adiciona a ação a este segment
    (if (= (segment-time (mcar segments)) time)
        (insert-queue! (segment-queue (mcar segments))
                       action)
        (let ((rest (mcdr segments))) ;se o time não é o mesmo do segment, realiza-se a recursão até esvasiar a agenda ou chegar/passar de time
          (if (belongs-before? rest)
              (set-mcdr!
               segments
               (mcons (make-new-time-segment time action)
                     (mcdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! ;se a ação pertence a um tempo anterior, é criado um segment antes dos demais
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
