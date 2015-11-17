#lang racket

(require math/matrix
         "logging.rkt")

(provide shift-matrix-up
         shift-matrix-down
         shift-matrix-left
         shift-matrix-right
         strip-unneeded
         shrink-to-board
         calculate-matrix-min)

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
(define (shift-matrix-up m x)
  (if (equal? x 0)
      m
      (matrix* shiftRightOrUp (shift-matrix-up m (- x 1)))))

; Shift the matrix down x rows
(define (shift-matrix-down m x)
  (if (equal? x 0)
      m
      (matrix* shiftLeftOrDown (shift-matrix-down m (- x 1)))))

; Shift the matrix right x columns
(define (shift-matrix-right m x)
  (if (equal? x 0)
      m
      (matrix* (shift-matrix-right m (- x 1)) shiftRightOrUp)))

; Shift the matrix left x columns
(define (shift-matrix-left m x)
  (if (equal? x 0)
      m
      (matrix* (shift-matrix-left m (- x 1)) shiftLeftOrDown)))

; Strip out unneeded #'s for shortest path
(define (strip-unneeded m)
  (define min (calculate-matrix-min m))
  
  (logMessage "Minimum moves to goal: " min)

  (define processVector (matrix->vector m))

  (for ([i (vector-length processVector)])
    (if (equal? (vector-ref processVector i) min)
        null
        (vector-set! processVector i 0)))

  (vector->matrix 8 8 processVector))

; Get minimum value in matrix besides 0
(define (calculate-matrix-min m)
  (define min 200)
  (for ([i 8])
    (for ([j 8])
      (if (and (< (matrix-ref m i j) min) (> (matrix-ref m i j) 0))
          (set! min (matrix-ref m i j))
          null)))
  min)

; Convert a 15x15 matrix into a 8x8 matrix
(define (shrink-to-board m)
  (matrix [[(matrix-ref m 7 0) (matrix-ref m 7 1) (matrix-ref m 7 2) (matrix-ref m 7 3) (matrix-ref m 7 4) (matrix-ref m 7 5) (matrix-ref m 7 6) (matrix-ref m 7 7)]
           [(matrix-ref m 8 0) (matrix-ref m 8 1) (matrix-ref m 8 2) (matrix-ref m 8 3) (matrix-ref m 8 4) (matrix-ref m 8 5) (matrix-ref m 8 6) (matrix-ref m 8 7)]
           [(matrix-ref m 9 0) (matrix-ref m 9 1) (matrix-ref m 9 2) (matrix-ref m 9 3) (matrix-ref m 9 4) (matrix-ref m 9 5) (matrix-ref m 9 6) (matrix-ref m 9 7)]
           [(matrix-ref m 10 0) (matrix-ref m 10 1) (matrix-ref m 10 2) (matrix-ref m 10 3) (matrix-ref m 10 4) (matrix-ref m 10 5) (matrix-ref m 10 6) (matrix-ref m 10 7)]
           [(matrix-ref m 11 0) (matrix-ref m 11 1) (matrix-ref m 11 2) (matrix-ref m 11 3) (matrix-ref m 11 4) (matrix-ref m 11 5) (matrix-ref m 11 6) (matrix-ref m 11 7)]
           [(matrix-ref m 12 0) (matrix-ref m 12 1) (matrix-ref m 12 2) (matrix-ref m 12 3) (matrix-ref m 12 4) (matrix-ref m 12 5) (matrix-ref m 12 6) (matrix-ref m 12 7)]
           [(matrix-ref m 13 0) (matrix-ref m 13 1) (matrix-ref m 13 2) (matrix-ref m 13 3) (matrix-ref m 13 4) (matrix-ref m 13 5) (matrix-ref m 13 6) (matrix-ref m 13 7)]
           [(matrix-ref m 14 0) (matrix-ref m 14 1) (matrix-ref m 14 2) (matrix-ref m 14 3) (matrix-ref m 14 4) (matrix-ref m 14 5) (matrix-ref m 14 6) (matrix-ref m 14 7)]]))