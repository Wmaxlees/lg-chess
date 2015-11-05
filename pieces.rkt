#lang racket/base

(require math/matrix
         racket/fixnum
         "moves.rkt"
         "logging.rkt")

(provide PAWN ROOK KNIGHT BISHOP QUEEN KING getName getValue getMoves)

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

(define (convert-bitboard-to-x-y bitboard)
  (define temp (/ (log bitboard) (log 2)))

  (define x (+ 1 (modulo temp 8)))
  (define y (+ 1 (floor (/ temp 8))))
  
  (vector
   (if (fixnum? x) x (fl->fx x))
   (if (fixnum? y) y (fl->fx y))))

(define (get-shortest-trajectory startBoard goalBoard holesBoard pieceType)
  (define start (convert-bitboard-to-x-y startBoard))
  (logMessage "Start: " (vector-ref start 0) "," (vector-ref start 1))
  (define goal (convert-bitboard-to-x-y goalBoard))
  (logMessage "Goal: " (vector-ref goal 0) "," (vector-ref goal 1))
  
  (getShortestTrajectory (vector-ref start 0) (vector-ref start 1) (vector-ref goal 0) (vector-ref goal 1) holesBoard pieceType))

(define (getShortestTrajectory startX startY goalX goalY board pieceType)
  (define startOffsetX (- startX 8))
  (define startOffsetY (- startY 8))
  (define goalOffsetX (- goalX 8))
  (define goalOffsetY (- goalY 8))

  ; TEMPORARY: define the piece we're looking at
  (define pieceMoves (case pieceType
                       [(0) pawnMovesNorth]
                       [(1) rookMoves]
                       [(2) knightMoves]
                       [(3) bishopMoves]
                       [(4) queenMoves]
                       [(5) kingMoves]))

  ; Create the shifted start matrix
  (define startMatrix
    (if (> startOffsetY 0)
        (shiftMatrixUp (if (> startOffsetX 0)
                           (shiftMatrixRight pieceMoves startOffsetX)
                           (shiftMatrixLeft pieceMoves (- 0 startOffsetX))) startOffsetY)
        (shiftMatrixDown (if (> startOffsetX 0)
                             (shiftMatrixRight pieceMoves startOffsetX)
                             (shiftMatrixLeft pieceMoves (- 0 startOffsetX))) (- 0 startOffsetY))))

  ; Create the shifted goal matrix
  (define goalMatrix
    (if (> goalOffsetY 0)
        (shiftMatrixUp (if (> goalOffsetX 0)
                           (shiftMatrixRight pieceMoves goalOffsetX)
                           (shiftMatrixLeft pieceMoves (- 0 goalOffsetX))) goalOffsetY)
        (shiftMatrixDown (if (> startOffsetX 0)
                             (shiftMatrixRight pieceMoves goalOffsetX)
                             (shiftMatrixLeft pieceMoves (- 0 goalOffsetX))) (- 0 goalOffsetY))))

  ;(printMatrix 8 8 startMatrix)
  ;(printMatrix 8 8 goalMatrix)

  (shrinkToBoard (matrix+ startMatrix goalMatrix)))

(define (getMoves piece)
  (case piece
    [(0) pawnMovesNorth]
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

; Strip out unneeded #'s for shortest path
(define (stripUnneeded m)
  (define min (matrix-min m))
  
  (logMessage "Minimum moves to goal: " min)

  (define processVector (matrix->vector m))

  (for ([i (vector-length processVector)])
    (if (equal? (vector-ref processVector i) min)
        null
        (vector-set! processVector i 0)))

  (vector->matrix 8 8 processVector))

; Get minimum value in matrix besides 0
(define (matrix-min m)
  (define min 200)
  (for ([i 8])
    (for ([j 8])
      (if (and (< (matrix-ref m i j) min) (> (matrix-ref m i j) 0))
          (set! min (matrix-ref m i j))
          null)))
  min)

; Convert a 15x15 matrix into a 8x8 matrix
(define (shrinkToBoard m)
  (matrix [[(matrix-ref m 7 0) (matrix-ref m 7 1) (matrix-ref m 7 2) (matrix-ref m 7 3) (matrix-ref m 7 4) (matrix-ref m 7 5) (matrix-ref m 7 6) (matrix-ref m 7 7)]
           [(matrix-ref m 8 0) (matrix-ref m 8 1) (matrix-ref m 8 2) (matrix-ref m 8 3) (matrix-ref m 8 4) (matrix-ref m 8 5) (matrix-ref m 8 6) (matrix-ref m 8 7)]
           [(matrix-ref m 9 0) (matrix-ref m 9 1) (matrix-ref m 9 2) (matrix-ref m 9 3) (matrix-ref m 9 4) (matrix-ref m 9 5) (matrix-ref m 9 6) (matrix-ref m 9 7)]
           [(matrix-ref m 10 0) (matrix-ref m 10 1) (matrix-ref m 10 2) (matrix-ref m 10 3) (matrix-ref m 10 4) (matrix-ref m 10 5) (matrix-ref m 10 6) (matrix-ref m 10 7)]
           [(matrix-ref m 11 0) (matrix-ref m 11 1) (matrix-ref m 11 2) (matrix-ref m 11 3) (matrix-ref m 11 4) (matrix-ref m 11 5) (matrix-ref m 11 6) (matrix-ref m 11 7)]
           [(matrix-ref m 12 0) (matrix-ref m 12 1) (matrix-ref m 12 2) (matrix-ref m 12 3) (matrix-ref m 12 4) (matrix-ref m 12 5) (matrix-ref m 12 6) (matrix-ref m 12 7)]
           [(matrix-ref m 13 0) (matrix-ref m 13 1) (matrix-ref m 13 2) (matrix-ref m 13 3) (matrix-ref m 13 4) (matrix-ref m 13 5) (matrix-ref m 13 6) (matrix-ref m 13 7)]
           [(matrix-ref m 14 0) (matrix-ref m 14 1) (matrix-ref m 14 2) (matrix-ref m 14 3) (matrix-ref m 14 4) (matrix-ref m 14 5) (matrix-ref m 14 6) (matrix-ref m 14 7)]]))

;(printMatrix 8 8 (getShortestTrajectory 1 8 8 6 0))
;(printMatrix 8 8 (stripUnneeded (getShortestTrajectory 1 8 8 8 0)))
(printMatrix 8 8 (stripUnneeded (get-shortest-trajectory 128 1 0 KING)))