#lang racket 
(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)


;;Zeichnen einer Welt
;;Hilfsfunktionen
(define field (rectangle 400 400 "solid" "PeachPuff"))

(define field2 (add-line field 380 20 20 20 "black"))

(define field3 (add-line field2 380 380 20 380 "black"))

(define field4 (add-line field3 20 380 20 20 "black"))

(define fieldBasic (add-line field4 380 380 380 20 "black"))

(define (yLines field count y)
  (if (= count 18)
      field
      (yLines (add-line field 380 y 20 y "black") (+ count 1) (+ y 20))
      )
  )

(define (xLines field count x)
  (if (= count 18)
      field
      (xLines (add-line field x 380 x 20 "black") (+ count 1) (+ x 20))
      )
  )

(define game-board (xLines (yLines fieldBasic 1 40) 1 40))

(define (set-stone field x y color)
  (overlay/offset (circle 10 "solid" color)
                (- 180 (* 20 x))
                (- 180 (* 20 y))
                field)
  )
