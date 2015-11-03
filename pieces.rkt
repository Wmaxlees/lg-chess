#lang racket/base

(require math/matrix)

(provide PAWN
         ROOK
         KNIGHT
         BISHOP
         QUEEN
         KING
         getName
         getValue
         getMoves)

(define PAWN 0)
(define ROOK 1)
(define KNIGHT 2)
(define BISHOP 3)
(define QUEEN 4)
(define KING 5)

(define (getName piece)
  (case piece
    [(0) "Pawn"]
    [(1) "Rook"]
    [(2) "Knight"]
    [(3) "Bishop"]
    [(4) "Queen"]
    [(5) "King"]
    [else "FAIL"]))

(define (getValue piece)
  (case piece
    [(0) 1]
    [(1) 5]
    [(2 3) 3]
    [(4) 9]
    [(5) 255]
    [else "FAIL"]))

(define (getShortestTrajectory startX startY goalX goalY board)
  (define startOffsetX (- startX 8))
  (define startOffsetY (- startY 8))
  (define goalOffsetX (- goalX 8))
  (define goalOffsetY (- goalY 8))

  ; Create the shifted start matrix
  (define startMatrix
    (if (> startOffsetY 0)
        (shiftMatrixUp (if (> startOffsetX 0)
                           (shiftMatrixRight kingMoves startOffsetX)
                           (shiftMatrixLeft kingMoves (- 0 startOffsetX))) startOffsetY)
        (shiftMatrixDown (if (> startOffsetX 0)
                             (shiftMatrixRight kingMoves startOffsetX)
                             (shiftMatrixLeft kingMoves (- 0 startOffsetX))) (- 0 startOffsetY))))

  ; Create the shifted goal matrix
  (define goalMatrix
    (if (> goalOffsetY 0)
        (shiftMatrixUp (if (> goalOffsetX 0)
                           (shiftMatrixRight kingMoves goalOffsetX)
                           (shiftMatrixLeft kingMoves (- 0 goalOffsetX))) goalOffsetY)
        (shiftMatrixDown (if (> startOffsetX 0)
                             (shiftMatrixRight kingMoves goalOffsetX)
                             (shiftMatrixLeft kingMoves (- 0 goalOffsetX))) (- 0 goalOffsetY))))

  (matrix+ startMatrix goalMatrix))

(define pawnMoves
  (matrix [[7 7 7 7 7 7 7 7 7 7 7 7 7 7 7]
           [0 6 6 6 6 6 6 6 6 6 6 6 6 6 0]
           [0 0 5 5 5 5 5 5 5 5 5 5 5 0 0]
           [0 0 0 4 4 4 4 4 4 4 4 4 0 0 0]
           [0 0 0 0 3 3 3 3 3 3 3 0 0 0 0]
           [0 0 0 0 0 2 2 2 2 2 0 0 0 0 0]
           [0 0 0 0 0 0 1 1 1 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]]))

(define rookMoves
  (matrix [[2 2 2 2 2 2 2 1 2 2 2 2 2 2 2]
           [2 2 2 2 2 2 2 1 2 2 2 2 2 2 2]
           [2 2 2 2 2 2 2 1 2 2 2 2 2 2 2]
           [2 2 2 2 2 2 2 1 2 2 2 2 2 2 2]
           [2 2 2 2 2 2 2 1 2 2 2 2 2 2 2]
           [2 2 2 2 2 2 2 1 2 2 2 2 2 2 2]
           [2 2 2 2 2 2 2 1 2 2 2 2 2 2 2]
           [1 1 1 1 1 1 1 2 1 1 1 1 1 1 1]
           [2 2 2 2 2 2 2 1 2 2 2 2 2 2 2]
           [2 2 2 2 2 2 2 1 2 2 2 2 2 2 2]
           [2 2 2 2 2 2 2 1 2 2 2 2 2 2 2]
           [2 2 2 2 2 2 2 1 2 2 2 2 2 2 2]
           [2 2 2 2 2 2 2 1 2 2 2 2 2 2 2]
           [2 2 2 2 2 2 2 1 2 2 2 2 2 2 2]
           [2 2 2 2 2 2 2 1 2 2 2 2 2 2 2]]))

(define knightMoves
  (matrix [[6 5 4 5 4 5 4 5 4 5 4 5 4 5 6]
           [5 4 5 4 3 4 3 4 3 4 3 4 5 4 5]
           [4 5 4 3 4 3 4 3 4 3 4 3 4 5 4]
           [5 4 3 4 3 2 3 2 3 2 3 4 3 4 5]
           [4 3 4 3 2 3 2 3 2 3 2 3 4 3 4]
           [5 4 3 2 3 4 1 2 1 4 3 2 3 4 5]
           [4 3 4 3 2 1 2 3 2 1 2 3 4 3 4]
           [5 4 3 2 3 2 3 2 3 2 3 2 3 4 5]
           [4 3 4 3 2 1 2 3 2 1 2 3 4 3 4]
           [5 4 3 2 3 4 1 2 1 4 3 2 3 4 5]
           [4 3 4 3 2 3 2 3 2 3 2 3 4 3 4]
           [5 4 3 4 3 2 3 2 3 2 3 4 3 4 5]
           [4 5 4 3 4 3 4 3 4 3 4 3 4 5 4]
           [5 4 5 4 3 4 3 4 3 4 3 4 5 4 5]
           [6 5 4 5 4 5 4 5 4 5 4 5 4 5 6]]))

(define bishopMoves
  (matrix [[1 0 2 0 2 0 2 0 2 0 2 0 2 0 1]
           [0 1 0 2 0 2 0 2 0 2 0 2 0 1 0]
           [2 0 1 0 2 0 2 0 2 0 2 0 1 0 2]
           [0 2 0 1 0 2 0 2 0 2 0 1 0 2 0]
           [2 0 2 0 1 0 2 0 2 0 1 0 2 0 2]
           [0 2 0 2 0 1 0 2 0 1 0 2 0 2 0]
           [2 0 2 0 2 0 1 0 1 0 2 0 2 0 2]
           [0 2 0 2 0 2 0 2 0 2 0 2 0 2 0]
           [2 0 2 0 2 0 1 0 1 0 2 0 2 0 2]
           [0 2 0 2 0 1 0 2 0 1 0 2 0 2 0]
           [2 0 2 0 1 0 2 0 2 0 1 0 2 0 2]
           [0 2 0 1 0 2 0 2 0 2 0 1 0 2 0]
           [2 0 1 0 2 0 2 0 2 0 2 0 1 0 2]
           [0 1 0 2 0 2 0 2 0 2 0 2 0 1 0]
           [1 0 2 0 2 0 2 0 2 0 2 0 2 0 1]]))

(define queenMoves
  (matrix [[1 2 2 2 2 2 2 1 2 2 2 2 2 2 1]
           [2 1 2 2 2 2 2 1 2 2 2 2 2 1 2]
           [2 2 1 2 2 2 2 1 2 2 2 2 1 2 2]
           [2 2 2 1 2 2 2 1 2 2 2 1 2 2 2]
           [2 2 2 2 1 2 2 1 2 2 1 2 2 2 2]
           [2 2 2 2 2 1 2 1 2 1 2 2 2 2 2]
           [2 2 2 2 2 2 1 1 1 2 2 2 2 2 2]
           [1 1 1 1 1 1 1 2 1 1 1 1 1 1 1]
           [2 2 2 2 2 2 1 1 1 2 2 2 2 2 2]
           [2 2 2 2 2 1 2 1 2 1 2 2 2 2 2]
           [2 2 2 2 1 2 2 1 2 2 1 2 2 2 2]
           [2 2 2 1 2 2 2 1 2 2 2 1 2 2 2]
           [2 2 1 2 2 2 2 1 2 2 2 2 1 2 2]
           [2 1 2 2 2 2 2 1 2 2 2 2 2 1 2]
           [1 2 2 2 2 2 2 1 2 2 2 2 2 2 1]]))

(define kingMoves
  (matrix [[7 7 7 7 7 7 7 7 7 7 7 7 7 7 7]
           [7 6 6 6 6 6 6 6 6 6 6 6 6 6 7]
           [7 6 5 5 5 5 5 5 5 5 5 5 5 6 7]
           [7 6 5 4 4 4 4 4 4 4 4 4 5 6 7]
           [7 6 5 4 3 3 3 3 3 3 3 4 5 6 7]
           [7 6 5 4 3 2 2 2 2 2 3 4 5 6 7]
           [7 6 5 4 3 2 1 1 1 2 3 4 5 6 7]
           [7 6 5 4 3 2 1 2 1 2 3 4 5 6 7]
           [7 6 5 4 3 2 1 1 1 2 3 4 5 6 7]
           [7 6 5 4 3 2 2 2 2 2 3 4 5 6 7]
           [7 6 5 4 3 3 3 3 3 3 3 4 5 6 7]
           [7 6 5 4 4 4 4 4 4 4 4 4 5 6 7]
           [7 6 5 5 5 5 5 5 5 5 5 5 5 6 7]
           [7 6 6 6 6 6 6 6 6 6 6 6 6 6 7]
           [7 7 7 7 7 7 7 7 7 7 7 7 7 7 7]]))

(define (getMoves piece)
  (case piece
    [(0) pawnMoves]
    [(1) rookMoves]
    [(2) knightMoves]
    [(3) bishopMoves]
    [(4) queenMoves]
    [(5) kingMoves]
    [else "FAIL"]))

; A*shiftLeftOrDown will shift A left
; shiftLeftOrDown*A will shift A down
(define shiftLeftOrDown
  (matrix [[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 1 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 1 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 1 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 1 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 1 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 1 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 1 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 1 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 1 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 1 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 1 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 1 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 1 0]]))

; A*shiftRightOrUp will shift A right
; shiftRightOrUp*A will shift A up
(define shiftRightOrUp
  (matrix [[0 1 0 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 1 0 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 1 0 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 1 0 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 1 0 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 1 0 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 1 0 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 1 0 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 1 0 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 1 0 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 1 0 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 1 0 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 1 0]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 1]
           [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]]))

; Shift the matrix up x row
(define (shiftMatrixUp m x)
  (if (equal? x 0)
      m
      (matrix* shiftRightOrUp (shiftMatrixUp m (- x 1)))))

; Shift the matrix down x rows
(define (shiftMatrixDown m x)
  (if (equal? x 0)
      m
      (matrix* shiftLeftOrDown (shiftMatrixDown m (- x 1)))))

; Shift the matrix right x columns
(define (shiftMatrixRight m x)
  (if (equal? x 0)
      m
      (matrix* (shiftMatrixRight m (- x 1)) shiftRightOrUp)))

; Shift the matrix left x columns
(define (shiftMatrixLeft m x)
  (if (equal? x 0)
      m
      (matrix* (shiftMatrixLeft m (- x 1)) shiftLeftOrDown)))