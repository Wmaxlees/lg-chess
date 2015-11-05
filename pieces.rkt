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

;(printMatrix 8 8 (getShortestTrajectory 1 8 8 6 0))
;(printMatrix 8 8 (stripUnneeded (getShortestTrajectory 1 8 8 8 0)))
(printMatrix 8 8 (stripUnneeded (get-shortest-trajectory 128 1 0 KING)))