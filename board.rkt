#lang racket

(provide board%)

(define board-holes 0)
(define white-pawn 1)
(define white-rook 2)
(define white-knight 3)
(define white-bishop 4)
(define white-queen 5)
(define white-king 6)
(define black-pawn 7)
(define black-rook 8)
(define black-knight 9)
(define black-bishop 10)
(define black-queen 11)
(define black-king 12)

(define board%
  (class object%
    (init)

    (define bit-boards (vector 0 0 0 0 0 0 0 0 0 0 0 0 0))

    (define/public (get-bit-board board)
      (vector-ref bit-boards board))

    (define/public (set-bit-board board value)
      (vector-set! bit-boards board value))

    (super-new)))

(define board (new board%))