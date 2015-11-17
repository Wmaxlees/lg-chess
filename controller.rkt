#lang racket

(require "game.rkt"
         "gui.rkt"
         "board.rkt")

(provide chess-controller%)

(define chess-controller%
  (class object%
    (init)

    ; GUI member
    (define gui (new gui%
                     [width-in-chars 500]
                     [height-in-chars 500]
                     [board (new board%)]))

    ; Game mode
    (define game (new game%
                      [gametype "RRetiEndgame"]))

    (define/public (start)
      ; Initialize the game
      (send game initializeBoards)

      ; Initialize the GUI
      (send gui update-board (send game get-board))
      (send gui init))

    (define/public (update)
      '())

    (super-new)))