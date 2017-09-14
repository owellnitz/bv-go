#lang racket 
(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

(provide draw-board-with-score)

(provide start-field)

(provide choose-color-field)

(provide choose-killed-black-stones)

(provide choose-killed-white-stones)

(provide result-field)

(provide choose-handicap)

(provide set-handicap)

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

;;Anzeige der geschlagenen Steine mit Schwarz über Weiß
(define (text-for-killed-stones killed_stones)
  (overlay/offset (text-for-black-killed (car killed_stones))
                  0 200
                  (text-for-white-killed (cadr killed_stones))))

(define (text-for-black-killed black_killed)
  (above (text "Geschlagene schwarze Steine" 14 'blue)
         (text (number->string black_killed) 14 'blue)))

(define (text-for-white-killed white_killed)
  (above (text "Geschlagene weiße Steine" 14 'blue)
         (text (number->string white_killed) 14 'blue)))


;;Anzeige des Spielfelds mit den geschlagenen Steinen und Pass-Status am rechten Rand.
(define (draw-board-with-score world)
  (overlay/offset (beside (above (board-state->board 0 game-board (second world))
                                 (if (equal? (first world) 'wait)
                                     (if (equal? (fourth world) 'passed)
                                         (above (text "warte auf Gegner..." 16 'darkgreen)
                                                (text "Du hast gepasst!" 16 'red))
                                         (text "warte auf Gegner..." 16 'red))
                                     (if (equal? (fourth world) 'passed)
                                         (above (text "bitte Zelle markieren!" 16 'darkgreen)
                                                (text "Gegner hat gepasst!" 16 'red))
                                         (text "bitte Zelle markieren!" 16 'darkgreen))))
                          (text-for-killed-stones (third world)))
                  155 215
                  (overlay (text "Passen" 16 'black)
                           (rectangle 100 50 "solid" "gray"))))


;;Hilfsfunktion für alle Auswahlfelder

;;Teilung des Feldes in zwei horizontale Bereiche
(define two-areas (add-line field 0 200 400 200  "black"))

;;Startfeld zur Auswahl vom Spielstart aus Bilddatei oder als neues Spiel
;;Hilfsfunktionen  
(define text-for-image
  (above (text "Klicke hier um ein Spiel" 22 'blue)
         (text "aus einer Bilddatei zu laden" 22 'blue)))

(define text-for-new-game
  (text "Klicke hier um ein neues Spiel zu starten" 22 'blue))

;;Startfeld
(define start-field
  (overlay/offset text-for-image 0 100
                  (overlay/offset text-for-new-game 0 -100 two-areas)))

;;Neues Spiel mit Auswahl der Farbe und Vorgabe
;;Hilfsfunktionen für Farbwahl
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

;;Hilfsfunktionen für Vorgabe
(define text-for-sethandicap
  (above(text "Gebe die Höhe der Vorgabe" 22 'blue)
        (text "mit den Nummerntasten ein." 22 'blue)
        (text "Bestätige mit der Entertaste" 22 'blue)))

(define (input-for-handicap handicap)
  (text (if (equal? 'handicap handicap)
            "0"
            (number->string handicap)) 22 'blue))

(define (text-for-handicap handicap)
  (above (text "Verbliebende Vorgabe" 14 'blue)
         (text (if (equal? 'handicap handicap)
                   "0"
                   (number->string handicap)) 14 'blue)))

;Feld zur Eingabe der Vorgabe
(define (choose-handicap handicap)
  (overlay/offset text-for-sethandicap 0 100
                  (overlay/offset (input-for-handicap handicap) 0 -100 two-areas)))

;;Feld zum Setzen der Vorgabe
(define (set-handicap world)
  (beside (above (board-state->board 0 game-board (second world))
                 (text "bitte Zelle markieren!" 16 'darkgreen))            
          (text-for-handicap (fourth world))))

;;Start aus BV-Datei
;; Hilfsfunktionen
(define text-for-black-stones
  (above(text "Gebe die Anzahl der" 22 'blue)
        (text "geschlagenen schwarzen Steine" 22 'blue)
        (text "mit den Nummerntasten ein." 22 'blue)
        (text "Bestätige mit der Entertaste" 22 'blue)))

(define text-for-white-stones
  (above(text "Gebe die Anzahl der" 22 'blue)
        (text "geschlagenen weißen Steine" 22 'blue)
        (text "mit den Nummerntasten ein." 22 'blue)
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

;;Auswertung Placeholder
(define result-field
  (overlay/offset (text "Auswertung" 22 'blue) 0 100
                  field))

