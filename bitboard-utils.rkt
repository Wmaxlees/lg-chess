#lang racket

(require math/matrix)

(provide convert-matrix-to-bitboard
         convert-bitboard-to-x-y)

; Convert an 8x8 matrix to a 64bit bitboard with bits set where the item is > 0
(define (convert-matrix-to-bitboard m)
  (define processVector (matrix->vector m))

  (define result 0)
  
  (for ([i (vector-length processVector)])
    (cond [(> (vector-ref processVector i) 0) (set! result (bitwise-ior result (expt 2 (- 63 i))))]))

  result)

; Determine the x,y value on the chessboard of a given bit board with a single set bit
(define (convert-bitboard-to-x-y bitboard)
  (define temp (/ (log bitboard) (log 2)))

  (define x (+ 1 (modulo temp 8)))
  (define y (+ 1 (floor (/ temp 8))))
  
  (vector
   (if (fixnum? x) x (fl->fx x))
   (if (fixnum? y) y (fl->fx y))))