#lang racket 
(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

(provide draw-board-with-score)

(provide start-field)

(provide choose-color-field)

(provide choose-killed-black-stones)

(provide choose-killed-white-stones)

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
  (if (= (string-length color) 0 )
      field
      (overlay/offset (circle 10 "solid" color)
                      (- 180 (* 20 x))
                      (- 180 (* 20 y))
                      field)
      )
  )

(define (board-state->board county field list)
  (if (empty? list)
      field
      (board-state->board (+ 1 county)
                          (board-list->board county 0 field (car list))
                          (cdr list))
      )
  )

(define (board-list->board county countx field list)
  (if (empty? list)
      field
      (board-list->board county
                         (+ 1 countx)
                         (set-stone field countx county (number->color (car list)))
                         (cdr list))
      )
  )

(define (number->color num)
  (cond
    [(= num 1) "white"]
    [(= num -1) "black"]
    [(= num 0) ""]
  ))


;;Spielfeld mit gesetzen Steinen + geschlagenen Steine
;;Hilfsfunktionen
(define (text-for-killed-stones killed_stones)
  (above (text-for-black-killed (car killed_stones))
         (text-for-white-killed (cadr killed_stones))))

(define (text-for-black-killed black_killed)
  (above (text "Geschlagene schwarze Steine" 14 'blue)
         (text (number->string black_killed) 14 'blue)))

(define (text-for-white-killed white_killed)
  (above (text "Geschlagene weiße Steine" 14 'blue)
         (text (number->string white_killed) 14 'blue)))



(define (draw-board-with-score game_score killed_stones)
 (beside (board-state->board 0 game-board game_score)
         (text-for-killed-stones killed_stones)))

;;Startfeld zur Auswahl vom Spielstart aus Bilddatei oder als neues Spiel
;;Hilfsfunktionen
(define two-areas (add-line field 0 200 400 200  "black"))

(define text-for-image
  (above (text "Klicke hier um ein Spiel" 22 'blue)
         (text "aus einer Bilddatei zu laden" 22 'blue)))

(define text-for-new-game
  (text "Klicke hier um ein neues Spiel zu starten" 22 'blue))

;;Startfeld
(define start-field
  (overlay/offset text-for-image 0 100
                 (overlay/offset text-for-new-game 0 -100 two-areas)))

;;Neues Spiel mit Auswahl der Farbe
;;Hilfsfunktionen
(define text-for-black
  (above(text "Klicke hier um die Farbe" 22 'blue)
        (text " schwarz zu wählen" 22 'blue)))

(define text-for-white
  (above(text "Klicke hier um die Farbe" 22 'blue)
        (text " weiß zu wählen" 22 'blue)))
;;Farbwahlfeld
(define choose-color-field
    (overlay/offset text-for-black 0 100
                 (overlay/offset text-for-white 0 -100 two-areas)))

;;Start aus BV-Datei
;; Hilfsfunktionen
(define text-for-black-stones
  (above(text "Gebe die Anzahl der" 22 'blue)
        (text "geschlagenen schwarzen Steine" 22 'blue)
        (text "mit den Nummertasten ein." 22 'blue)
        (text "Bestätige mit der Entertaste" 22 'blue)))

(define text-for-white-stones
  (above(text "Gebe die Anzahl der" 22 'blue)
        (text "geschlagenen weißen Steine" 22 'blue)
        (text "mit den Nummertasten ein." 22 'blue)
        (text "Bestätige mit der Entertaste" 22 'blue)))

(define (input-for-killed-stones killed-stones)
  (text (number->string killed-stones) 22 'blue))

;Feld zur Eingabe der geschlagenen Steine
;;Schwarz
(define (choose-killed-black-stones killed-stones)
    (overlay/offset text-for-black-stones 0 100
                 (overlay/offset (input-for-killed-stones killed-stones) 0 -100 two-areas)))

;;Weiß
(define (choose-killed-white-stones killed-stones)
    (overlay/offset text-for-white-stones 0 100
                 (overlay/offset (input-for-killed-stones killed-stones) 0 -100 two-areas)))

