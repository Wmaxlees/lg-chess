#lang racket

(require "board.rkt")

(provide game%)

(define game%
  (class object%
    (init gametype)

    (define board (new board%))

    (super-new)))