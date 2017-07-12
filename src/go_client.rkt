#lang racket 
(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)
(require "go_draw_board.rkt")


;;Zustände:
;; 1. w=(wait   )      - Abwarten
;; 2. w=(play   )      - An der Reihe
;; 3. w=(won    )      - Gewonnen
;; 4. w=(lost   )      - Verloren
;; 5. w=(remis  )      - Unentschieden
;; 6. w=(started)      -Farbwahl und Zugreihenfolge


;;Startzustand
(define WORLD0 (list 'wait (make-list 19 (make-list 19 0)) (list 0 0)))

;;Falls ein korrekter Weltzustand empfangen wird
;;   --> setze die Welt auf die empfangene Nachricht
;;sonst: verharre im alten Weltzustand
(define (receive w m)
  (if (and (list? m) (= (length m) 3)
           (symbol? (first m)) 
           (list? (second m)) (= (length (second m)) 19))
      m
      w))



;;Zeichnen einer Welt
;;Hilfsfunktionen

(define new_game_text (above(text "Für ein neues Spiel" 16 'black)
                            (text "bitte klicken" 16 'black)))

;;Eigentliches Zeichen mit go_draw_board Schnittstelle
(define (draw name)
    (lambda (w)
      (cond ;;Won
             [(equal? (car w) 'won)
              (above
               (text "Gewonnen" 22 'darkgreen)
               new_game_text)]
             ;;Lost
             [(equal? (car w) 'lost)
              (above 
               (text "Verloren" 22 'red)
                new_game_text)]
             ;;Remis
             [(equal? (car w) 'remis)
              (above 
               (text "Remis" 22 'blue) 
               new_game_text)]
             ;;Wahl des Spielstarts
             [(equal? (car w) 'started)
              start-field]
             ;;Eingabe der geschlagenen Steine
             ;;Zuerst schwarz
             [(equal? (car w) 'setkilledblack)
              (choose-killed-black-stones (car (third w))) ]
             ;;dann weiß
             [(equal? (car w) 'setkilledwhite)
              (choose-killed-white-stones (cadr (third w))) ]
             ;;Wahl der Farbe
             [(equal? (car w) 'choosecolor)
              choose-color-field]
             ;;Sonst wird normal gespielt
             [else
                (above
                 (draw-board-with-score (second w) (third w))
                 (if (equal? (car w) 'wait)
                     (text "warte auf Gegner..." 16 'red)
                     (text "bitte Zelle markieren!" 16 'darkgreen)))
                 ])))


;;Maus-Interaktionen, Restart noch unverändert. Ansonsten wird eine Liste '( 'set  Y X ) verschickt.
(define (handle-mouse name)
  (lambda (w x_pos y_pos mouse_event)
    (if(mouse=? mouse_event "button-up")
       (if (not (equal? (car w) 'wait))
           (cond
             ;Restartzeug
             [(or (equal? (car w) 'won)
                   (equal? (car w) 'lost)
                   (equal? (car w) 'remis))
               (make-package w 'restart)]
               ;Start aus BV oder neues Spiel
               [(equal? (car w) 'started)
                   (if (< y_pos 200)
                       ;TODO Start aus BV
                   (make-package w 'newbvgame)
                       ;Start eines neuen Spiels
                   (make-package w 'newgame))]
               ;Farbwahl
               [(equal? (car w) 'choosecolor)
                   (if (< y_pos 200)
                       ;Wahl von Schwarz
                       (make-package w 'black)
                       ;Wahl von Weiß
                       (make-package w 'white))]
               ;Stein setzen
               [(let* ((column (quotient (- x_pos 10)  20))
                      (row    (quotient (- y_pos 10) 20))
                      (index (list row column)))
                 (if (and (< -1 column 19) (< -1 row 19))
                     (make-package w (cons 'set index))
                     w))])
           w)
       w)))

;;Tastatureingabe zur Eingabe der geschlagenen Steine

(define (handle-key name)
  (lambda (w key_event)
     (if (or (equal? (car w) 'setkilledblack)
             (equal? (car w) 'setkilledwhite))
(cond
     [(member? '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0") key_event)
        (make-package w (list 'set key_event))]
     [(key=? key_event "\r")
        (make-package w 'confirm)]
     [(key=? key_event "\b")
        (make-package w 'delete)]
     
     [else w])
w)))

;;Hilfsfunktion ob ein Key in der Liste ist
(define (member? list key)
  (if (list? (member key list))
      #t
      #f
   )
  )

;;Erstelle eine Welt und verbinde sie mit dem LOCALHOST Server
(define (create-world n)
    (big-bang WORLD0
             (on-receive receive)
             (to-draw (draw n))
             (on-mouse (handle-mouse n))
             (on-release (handle-key n))
             (name n)
             (state #f)
             (register LOCALHOST)))

;;Macht zwei Welten auf
(launch-many-worlds 
  (create-world "Spieler1")
  (create-world "Spieler2"))
  ;(create-world "B"))