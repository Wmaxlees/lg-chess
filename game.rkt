#lang racket

(require "board.rkt"
         "pieces.rkt")

(provide game%)

(define game%
  (class object%
    (init-field gametype)

    (define/public (initializeBoards)
      (case gametype
        [("RRetiEndgame") (init-r-reti-endgame)]
        [else (init-default)]))

    (define board (new board%))

    ;(define/public (generate-moves)

      ; TEMPORARY: Get the move bundle for the white king
      ;(define move-bundle
        ;(convert-matrix-to-bitboard
         ;(get-shortest-trajectory
          ;(send board get-bit-board white-king)
          ;(send board get-bit-board black-pawn) 0 KING)))

      ; Create the first move
      ;(define first-move-cloud
        ;(if (> startOffsetY 0)
            ;(shift-matrix-up (if (> startOffsetX 0)
                                 ;(shift-matrix-right pieceMoves startOffsetX)
                                 ;(shift-matrix-left pieceMoves (- 0 startOffsetX))) startOffsetY)
            ;(shift-matrix-down (if (> startOffsetX 0)
                                   ;(shift-matrix-right pieceMoves startOffsetX)
                                   ;(shift-matrix-left pieceMoves (- 0 startOffsetX))) (- 0 startOffsetY))))
      
      ;elipse-matrix)

    (define (init-r-reti-endgame)
      (send board set-bit-board white-pawn #b0000000000000000000001000000000000000000000000000000000000000000)
      (send board set-bit-board white-king #b1000000000000000000000000000000000000000000000000000000000000000)
      (send board set-bit-board black-pawn #b0000000000000000000000001000000000000000000000000000000000000000)
      (send board set-bit-board black-king #b0000000000000000000000010000000000000000000000000000000000000000))

    (define (init-default)
      ; White pieces
      (send board set-bit-board white-pawn   #b0000000000000000000000000000000000000000000000001111111100000000)
      (send board set-bit-board white-rook   #b0000000000000000000000000000000000000000000000000000000010000001)
      (send board set-bit-board white-knight #b0000000000000000000000000000000000000000000000000000000001000010)
      (send board set-bit-board white-bishop #b0000000000000000000000000000000000000000000000000000000000100100)
      (send board set-bit-board white-queen  #b0000000000000000000000000000000000000000000000000000000000010000)
      (send board set-bit-board white-king   #b0000000000000000000000000000000000000000000000000000000000001000)

      ; Black pieces
      (send board set-bit-board black-pawn   #b0000000011111111000000000000000000000000000000000000000000000000)
      (send board set-bit-board black-rook   #b1000000100000000000000000000000000000000000000000000000000000000)
      (send board set-bit-board black-knight #b0100001000000000000000000000000000000000000000000000000000000000)
      (send board set-bit-board black-bishop #b0010010000000000000000000000000000000000000000000000000000000000)
      (send board set-bit-board black-queen  #b0000100000000000000000000000000000000000000000000000000000000000)
      (send board set-bit-board black-king   #b0001000000000000000000000000000000000000000000000000000000000000))

    (define/public (get-board) board)

    (super-new)))