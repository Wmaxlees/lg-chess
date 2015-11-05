#lang racket

(require math/matrix)

(provide printMatrix logMessage)

; Enable logging
(define loggingEnabled #t)

; Log a message to the console
(define (logMessage . input)
  (if loggingEnabled
      (printLogging input)
      null))

(define (printLogging input)
  (for/list ([i input])
    (display i))
  (display "\n"))

; Print the contents of a matrix
(define (printMatrix height width m)
  (if loggingEnabled
      (for ([i height])
         (for ([j width])
           (if (< (matrix-ref m i j) 10)
               (display " ") null)
           (display (matrix-ref m i j))
           (display " "))
         (display "\n"))
      null))
