#lang racket

(require "controller.rkt")

; Main function
(define (main)
  (define prompt "Piece: ")
  (define read-line)
  
  ; Create the controller and start it
  (define controller (new chess-controller%))
  (send controller start))

; Call the main function
(main)
