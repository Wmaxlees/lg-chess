#lang racket

(require racket/gui
         racket/draw
         "board.rkt"
         "bitmasks.rkt"
         "logging.rkt")

(provide gui%)

(define white-pawn-ascii "♙")
(define white-rook-ascii "♖")
(define white-knight-ascii "♘")
(define white-bishop-ascii "♗")
(define white-queen-ascii "♕")
(define white-king-ascii "♔")

(define black-pawn-ascii "♟")
(define black-rook-ascii "♜")
(define black-knight-ascii "♞")
(define black-bishop-ascii "♝")
(define black-queen-ascii "♛")
(define black-king-ascii "♚")

(define gui%
  (class object%
    (init-field width-in-chars
                height-in-chars
                board)

    (define frame
      (new frame%
           [label "LG Chess"]
           [style '(no-resize-border)]))

    ; Define the canvas the GUI will be using
    (define my-canvas% (class canvas% (super-new)))
    (define canvas (new my-canvas%
                        [style '(no-autoclear)]
                        [parent frame]
                        [min-width width-in-chars]
                        [min-height height-in-chars]
                        [paint-callback(lambda (canvas dc) (set! my-dc dc) (draw-board) (draw-pieces board))]))
    (define my-dc (send canvas get-dc))

    ; Draw the pieces on the board based on the bitboards passed to it
    (define (draw-pieces board)
      ; Draw the white pawns
      (draw-piece-of-type white-pawn-ascii (send board get-bit-board white-pawn))
      (draw-piece-of-type white-rook-ascii (send board get-bit-board white-rook))
      (draw-piece-of-type white-knight-ascii (send board get-bit-board white-knight))
      (draw-piece-of-type white-bishop-ascii (send board get-bit-board white-bishop))
      (draw-piece-of-type white-queen-ascii (send board get-bit-board white-queen))
      (draw-piece-of-type white-king-ascii (send board get-bit-board white-king))
      (draw-piece-of-type black-pawn-ascii (send board get-bit-board black-pawn))
      (draw-piece-of-type black-rook-ascii (send board get-bit-board black-rook))
      (draw-piece-of-type black-knight-ascii (send board get-bit-board black-knight))
      (draw-piece-of-type black-bishop-ascii (send board get-bit-board black-bishop))
      (draw-piece-of-type black-queen-ascii (send board get-bit-board black-queen))
      (draw-piece-of-type black-king-ascii (send board get-bit-board black-king)))

    (define (draw-board)
      (send my-dc set-pen (make-object color% 0 0 0) 1 'solid)
      (send my-dc set-brush (make-object color% 255 255 255) 'solid)
      (send my-dc draw-rectangle 50 50 400 400)
      
      (send my-dc set-pen (make-object color% 255 255 255) 0 'transparent)
      (send my-dc set-brush (make-object color% 100 100 100) 'solid)
      
      (send my-dc draw-rectangle 50 100 50 50)
      (send my-dc draw-rectangle 50 200 50 50)
      (send my-dc draw-rectangle 50 300 50 50)
      (send my-dc draw-rectangle 50 400 50 50)
      (send my-dc draw-rectangle 100 50 50 50)
      (send my-dc draw-rectangle 100 150 50 50)
      (send my-dc draw-rectangle 100 250 50 50)
      (send my-dc draw-rectangle 100 350 50 50)
      (send my-dc draw-rectangle 150 100 50 50)
      (send my-dc draw-rectangle 150 200 50 50)
      (send my-dc draw-rectangle 150 300 50 50)
      (send my-dc draw-rectangle 150 400 50 50)
      (send my-dc draw-rectangle 200 50 50 50)
      (send my-dc draw-rectangle 200 150 50 50)
      (send my-dc draw-rectangle 200 250 50 50)
      (send my-dc draw-rectangle 200 350 50 50)
      (send my-dc draw-rectangle 250 100 50 50)
      (send my-dc draw-rectangle 250 200 50 50)
      (send my-dc draw-rectangle 250 300 50 50)
      (send my-dc draw-rectangle 250 400 50 50)
      (send my-dc draw-rectangle 300 50 50 50)
      (send my-dc draw-rectangle 300 150 50 50)
      (send my-dc draw-rectangle 300 250 50 50)
      (send my-dc draw-rectangle 300 350 50 50)
      (send my-dc draw-rectangle 350 100 50 50)
      (send my-dc draw-rectangle 350 200 50 50)
      (send my-dc draw-rectangle 350 300 50 50)
      (send my-dc draw-rectangle 350 400 50 50)
      (send my-dc draw-rectangle 400 50 50 50)
      (send my-dc draw-rectangle 400 150 50 50)
      (send my-dc draw-rectangle 400 250 50 50)
      (send my-dc draw-rectangle 400 350 50 50))
      
    
    (define/public (get-canvas) canvas)

    (define/public (init)
      (send frame show #t)
      (set! my-dc (send canvas get-dc)))

    (define/public (update-board new-board)
      (set! board new-board))

    (define (draw-piece-of-type piece-ascii bit-board)
      ; Logging
      (logMessage "Beginning to draw pieces " piece-ascii " on board")

      ; Set the font size
      (send my-dc set-font (make-object font% 35 'default))
      
      (cond [(equal? (bitwise-and bit-board QR1) QR1) (send my-dc draw-text piece-ascii 58 398)])
      (cond [(equal? (bitwise-and bit-board QR2) QR2) (send my-dc draw-text piece-ascii 58 348)])
      (cond [(equal? (bitwise-and bit-board QR3) QR3) (send my-dc draw-text piece-ascii 58 298)])
      (cond [(equal? (bitwise-and bit-board QR4) QR4) (send my-dc draw-text piece-ascii 58 248)])
      (cond [(equal? (bitwise-and bit-board QR5) QR5) (send my-dc draw-text piece-ascii 58 198)])
      (cond [(equal? (bitwise-and bit-board QR6) QR6) (send my-dc draw-text piece-ascii 58 148)])
      (cond [(equal? (bitwise-and bit-board QR7) QR7) (send my-dc draw-text piece-ascii 58 98)])
      (cond [(equal? (bitwise-and bit-board QR8) QR8) (send my-dc draw-text piece-ascii 58 48)])
      
      (cond [(equal? (bitwise-and bit-board QN1) QN1) (send my-dc draw-text piece-ascii 108 398)])
      (cond [(equal? (bitwise-and bit-board QN2) QN2) (send my-dc draw-text piece-ascii 108 348)])
      (cond [(equal? (bitwise-and bit-board QN3) QN3) (send my-dc draw-text piece-ascii 108 298)])
      (cond [(equal? (bitwise-and bit-board QN4) QN4) (send my-dc draw-text piece-ascii 108 248)])
      (cond [(equal? (bitwise-and bit-board QN5) QN5) (send my-dc draw-text piece-ascii 108 198)])
      (cond [(equal? (bitwise-and bit-board QN6) QN6) (send my-dc draw-text piece-ascii 108 148)])
      (cond [(equal? (bitwise-and bit-board QN7) QN7) (send my-dc draw-text piece-ascii 108 98)])
      (cond [(equal? (bitwise-and bit-board QN8) QN8) (send my-dc draw-text piece-ascii 108 48)])

      (cond [(equal? (bitwise-and bit-board QB1) QB1) (send my-dc draw-text piece-ascii 158 398)])
      (cond [(equal? (bitwise-and bit-board QB2) QB2) (send my-dc draw-text piece-ascii 158 348)])
      (cond [(equal? (bitwise-and bit-board QB3) QB3) (send my-dc draw-text piece-ascii 158 298)])
      (cond [(equal? (bitwise-and bit-board QB4) QB4) (send my-dc draw-text piece-ascii 158 248)])
      (cond [(equal? (bitwise-and bit-board QB5) QB5) (send my-dc draw-text piece-ascii 158 198)])
      (cond [(equal? (bitwise-and bit-board QB6) QB6) (send my-dc draw-text piece-ascii 158 148)])
      (cond [(equal? (bitwise-and bit-board QB7) QB7) (send my-dc draw-text piece-ascii 158 98)])
      (cond [(equal? (bitwise-and bit-board QB8) QB8) (send my-dc draw-text piece-ascii 158 48)])

      (cond [(equal? (bitwise-and bit-board Q1) Q1) (send my-dc draw-text piece-ascii 208 398)])
      (cond [(equal? (bitwise-and bit-board Q2) Q2) (send my-dc draw-text piece-ascii 208 348)])
      (cond [(equal? (bitwise-and bit-board Q3) Q3) (send my-dc draw-text piece-ascii 208 298)])
      (cond [(equal? (bitwise-and bit-board Q4) Q4) (send my-dc draw-text piece-ascii 208 248)])
      (cond [(equal? (bitwise-and bit-board Q5) Q5) (send my-dc draw-text piece-ascii 208 198)])
      (cond [(equal? (bitwise-and bit-board Q6) Q6) (send my-dc draw-text piece-ascii 208 148)])
      (cond [(equal? (bitwise-and bit-board Q7) Q7) (send my-dc draw-text piece-ascii 208 98)])
      (cond [(equal? (bitwise-and bit-board Q8) Q8) (send my-dc draw-text piece-ascii 208 48)])

      (cond [(equal? (bitwise-and bit-board K1) K1) (send my-dc draw-text piece-ascii 258 398)])
      (cond [(equal? (bitwise-and bit-board K2) K2) (send my-dc draw-text piece-ascii 258 348)])
      (cond [(equal? (bitwise-and bit-board K3) K3) (send my-dc draw-text piece-ascii 258 298)])
      (cond [(equal? (bitwise-and bit-board K4) K4) (send my-dc draw-text piece-ascii 258 248)])
      (cond [(equal? (bitwise-and bit-board K5) K5) (send my-dc draw-text piece-ascii 258 198)])
      (cond [(equal? (bitwise-and bit-board K6) K6) (send my-dc draw-text piece-ascii 258 148)])
      (cond [(equal? (bitwise-and bit-board K7) K7) (send my-dc draw-text piece-ascii 258 98)])
      (cond [(equal? (bitwise-and bit-board K8) K8) (send my-dc draw-text piece-ascii 258 48)])

      (cond [(equal? (bitwise-and bit-board KB1) KB1) (send my-dc draw-text piece-ascii 308 398)])
      (cond [(equal? (bitwise-and bit-board KB2) KB2) (send my-dc draw-text piece-ascii 308 348)])
      (cond [(equal? (bitwise-and bit-board KB3) KB3) (send my-dc draw-text piece-ascii 308 298)])
      (cond [(equal? (bitwise-and bit-board KB4) KB4) (send my-dc draw-text piece-ascii 308 248)])
      (cond [(equal? (bitwise-and bit-board KB5) KB5) (send my-dc draw-text piece-ascii 308 198)])
      (cond [(equal? (bitwise-and bit-board KB6) KB6) (send my-dc draw-text piece-ascii 308 148)])
      (cond [(equal? (bitwise-and bit-board KB7) KB7) (send my-dc draw-text piece-ascii 308 98)])
      (cond [(equal? (bitwise-and bit-board KB8) KB8) (send my-dc draw-text piece-ascii 308 48)])

      (cond [(equal? (bitwise-and bit-board KN1) KN1) (send my-dc draw-text piece-ascii 358 398)])
      (cond [(equal? (bitwise-and bit-board KN2) KN2) (send my-dc draw-text piece-ascii 358 348)])
      (cond [(equal? (bitwise-and bit-board KN3) KN3) (send my-dc draw-text piece-ascii 358 298)])
      (cond [(equal? (bitwise-and bit-board KN4) KN4) (send my-dc draw-text piece-ascii 358 248)])
      (cond [(equal? (bitwise-and bit-board KN5) KN5) (send my-dc draw-text piece-ascii 358 198)])
      (cond [(equal? (bitwise-and bit-board KN6) KN6) (send my-dc draw-text piece-ascii 358 148)])
      (cond [(equal? (bitwise-and bit-board KN7) KN7) (send my-dc draw-text piece-ascii 358 98)])
      (cond [(equal? (bitwise-and bit-board KN8) KN8) (send my-dc draw-text piece-ascii 358 48)])
      
      (cond [(equal? (bitwise-and bit-board KR1) KR1) (send my-dc draw-text piece-ascii 408 398)])
      (cond [(equal? (bitwise-and bit-board KR2) KR2) (send my-dc draw-text piece-ascii 408 348)])
      (cond [(equal? (bitwise-and bit-board KR3) KR3) (send my-dc draw-text piece-ascii 408 298)])
      (cond [(equal? (bitwise-and bit-board KR4) KR4) (send my-dc draw-text piece-ascii 408 248)])
      (cond [(equal? (bitwise-and bit-board KR5) KR5) (send my-dc draw-text piece-ascii 408 198)])
      (cond [(equal? (bitwise-and bit-board KR6) KR6) (send my-dc draw-text piece-ascii 408 148)])
      (cond [(equal? (bitwise-and bit-board KR7) KR7) (send my-dc draw-text piece-ascii 408 98)])
      (cond [(equal? (bitwise-and bit-board KR8) KR8) (send my-dc draw-text piece-ascii 408 48)]))

    (define (setRReti)
      (send my-dc set-font (make-object font% 35 'default))
      (send my-dc draw-text "♔" 408 48)
      (send my-dc draw-text "♙" 158 148)
      
      (send my-dc draw-text "♚" 58 148)
      (send my-dc draw-text "♟" 408 198))

    (super-new)))
