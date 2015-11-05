#lang racket

(require "game.rkt")

(provide chess-controller)

(define chess-controller%
  (class object%
    (init)

    (define game (new game%
                      [gametype "RRetiEndgame"]))

    (super-new)))