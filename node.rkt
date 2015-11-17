#lang racket

(define node%
  (class object%
    (init-field location)

    (define children '#())    

    (define/public (add-child child)
      (vector-append children '#(child)))

    (define/public (get-location) location)

    (super-new)))