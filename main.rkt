#lang racket

(require racket/gui racket/draw)

(define gui%
  (class object%
    (init-field width-in-chars
                height-in-chars)

    (define frame
      (new frame%
           [label "LG Chess"]
           [style '(no-resize-border)]))

    (define my-canvas%
      (class canvas%
        (super-new)))

    (define canvas (new my-canvas%
         [parent frame]
         [min-width 500]
         [min-height 500]
         [paint-callback
          (lambda (canvas dc)
            (send dc set-pen (make-object color% 0 0 0) 1 'solid)
            (send dc set-brush (make-object color% 255 255 255) 'solid)
            (send dc draw-rectangle 50 50 400 400)

            (send dc set-pen (make-object color% 255 255 255) 0 'transparent)
            (send dc set-brush (make-object color% 100 100 100) 'solid)

            (send dc draw-rectangle 50 100 50 50)
            (send dc draw-rectangle 50 200 50 50)
            (send dc draw-rectangle 50 300 50 50)
            (send dc draw-rectangle 50 400 50 50)
            (send dc draw-rectangle 100 50 50 50)
            (send dc draw-rectangle 100 150 50 50)
            (send dc draw-rectangle 100 250 50 50)
            (send dc draw-rectangle 100 350 50 50)
            (send dc draw-rectangle 150 100 50 50)
            (send dc draw-rectangle 150 200 50 50)
            (send dc draw-rectangle 150 300 50 50)
            (send dc draw-rectangle 150 400 50 50)
            (send dc draw-rectangle 200 50 50 50)
            (send dc draw-rectangle 200 150 50 50)
            (send dc draw-rectangle 200 250 50 50)
            (send dc draw-rectangle 200 350 50 50)
            (send dc draw-rectangle 250 100 50 50)
            (send dc draw-rectangle 250 200 50 50)
            (send dc draw-rectangle 250 300 50 50)
            (send dc draw-rectangle 250 400 50 50)
            (send dc draw-rectangle 300 50 50 50)
            (send dc draw-rectangle 300 150 50 50)
            (send dc draw-rectangle 300 250 50 50)
            (send dc draw-rectangle 300 350 50 50)
            (send dc draw-rectangle 350 100 50 50)
            (send dc draw-rectangle 350 200 50 50)
            (send dc draw-rectangle 350 300 50 50)
            (send dc draw-rectangle 350 400 50 50)
            (send dc draw-rectangle 400 50 50 50)
            (send dc draw-rectangle 400 150 50 50)
            (send dc draw-rectangle 400 250 50 50)
            (send dc draw-rectangle 400 350 50 50)

            (setRReti dc)

            )]))

    (define/public (get-canvas) canvas)

    (send frame show #t)

    (super-new)))

(define (setRReti dc)
  (send dc set-font (make-object font% 35 'default))
  (send dc draw-text "♔" 408 48)
  (send dc draw-text "♙" 158 148)

  (send dc draw-text "♚" 58 148)
  (send dc draw-text "♟" 408 198))

(define gui (new gui%
     [width-in-chars 500]
     [height-in-chars 400]))
