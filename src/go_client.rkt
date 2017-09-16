#lang racket 
(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)
(require "go_draw_board.rkt")

;;Der go_client hat in jeder Instanz eine eigene Welt, welche ihm vom Server mitgeteilt wird.
;;Eine Welt setzt sich zusammen aus: (Zustand Spielfeldbelegung Geschlagene_Steine Passstatus)
;;Mögliche Zustände:
;; 1. w=(wait          ) - Abwarten
;; 2. w=(play          ) - An der Reihe
;; 3. w=(won           ) - Gewonnen
;; 4. w=(lost          ) - Verloren
;; 5. w=(remis         ) - Unentschieden
;; 6. w=(started       ) - Wahl des Spielstarts als neues Spiel oder aus einer Bilddatei
;; 7. w=(setkilledblack) - Eingabe der geschlagenen schwarzen Steine bei Spielstart
;; 8. w=(setkilledwhite) - Eingabe der geschlagenen weißen Steine bei Spielstart
;; 9. w=(choosecolor   ) - Farbwahl
 

;;Startzustand als wartender Zustand mit leerem Spielfeld und keinen geschlagenen Steinen
(define WORLD0 (list 'wait (make-list 19 (make-list 19 0)) (list 0 0) 'passstatus))

;;Falls ein korrekter Weltzustand empfangen wird
;;   --> setze die Welt auf die empfangene Nachricht
;;sonst: verharre im alten Weltzustand
(define (receive w m)
  (if (and (list? m) (= (length m) 4)
           (symbol? (first m)) 
           (list? (second m)) (= (length (second m)) 19))
      m
      w))



;;Zeichnen einer Welt mit go_draw_board Schnittstelle
;;Aufrufen der jeweiligen Zeichenfunktion aus go_draw_board abhängig vom Zustand der Welt.
(define (draw name)
  (lambda (w)
    (cond 
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
      ;;Wahl der Zugreihenfolge bei Start aus BV
      [(equal? (car w) 'choosedraw)
       choose-draworder-field]
      ;;Eingabe der Vorgabe
      [(equal? (car w) 'choosehandicap)
       (choose-handicap (fourth w))]
      ;;Setzen der Vorgabe
      [(equal? (car w) 'usehandicap)
       (set-handicap w)]
      ;;Auswertung
      [(equal? (car w) 'result)
       draw-final-score w]
      ;;Sonst wird normal gespielt
      [else
       (draw-board-with-score w)])))


;;Maus-Interaktionen, Restart noch unverändert.
;;Funktion abhängig vom Zustand
;; 1. w=(wait          ) - Welt wird nicht verändert
;; 2. w=(play          ) - Sendet den gesetzen Stein an den Server ('set y-Koordinate x-Koordinate
;; 3. w=(won           ) - ToDO
;; 4. w=(lost          ) - ToDO
;; 5. w=(remis         ) - ToDO
;; 6. w=(started       ) - Sendet Start aus Bild ('newbvgame) oder Start eines neuen Spiels ('newgame) an den Server, abhängig vom gewählten Spielstart
;; 7. w=(setkilledblack) - Wird nicht von handle-mouse behandelt
;; 8. w=(setkilledwhite) - Wird nicht von handle-mouse behandelt
;; 9. w=(choosecolor   ) - Sendet die Farbwahl an den Server, für schwarz ('black) und für weiß ('white)
;;10. w=(choosehandicap) - Setzen der Vorgabe für Schwarz
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
             ;Wahl der Zugreihenfolge
             [(equal? (car w) 'choosedraw)
              (if (< y_pos 200)
                  ;Wahl von Schwarz
                  (make-package w 'black)
                  ;Wahl von Weiß
                  (make-package w 'white))]
             ;;Vorgabe setzen 
             [(equal? (car w) 'usehandicap)
              (let* ((column (quotient (- x_pos 10)  20))
                     (row    (quotient (- y_pos 10) 20))
                     (index (list row column)))
                (if (and (< -1 column 19) (< -1 row 19))
                    (make-package w (cons 'set index))
                    w))]
             ;Stein setzen
             [(equal? (car w) 'play)
              (let* ((column (quotient (- x_pos 10)  20))
                     (row    (quotient (- y_pos 10) 20))
                     (index (list row column)))
                (if (and (< -1 column 19) (< -1 row 19))
                    (make-package w (cons 'set index))
                    (if (and (> y_pos 400)
                             (< y_pos 450)
                             (> x_pos 400)
                             (< x_pos 500))
                        (make-package w 'passed)
                        w)))
              ]
             [else w])
           w)
       w)))

;;Tastatureingabe, wird zur Eingabe der geschlagenen Steine und der Vorgabe beim Spielstart benötigt.
;;Für die Zustände
;; 7. w=(setkilledblack) - Setzen der geschlagenen schwarzen Steine
;; 8. w=(setkilledwhite) - Setzen der geschlagenen weißen Steine
;;10. w=(choosehandicap) - Setzen der Vorgabe für Schwarz

;;Ansonsten wird die Welt nicht verändert und Tastatureingaben ignoriert.

(define (handle-key name)
  (lambda (w key_event)
    
    (cond
      ;;Farbwahl
      [(or (equal? (car w) 'setkilledblack)
           (equal? (car w) 'setkilledwhite))
       (cond
         ;;Wird eine Zahl zwischen 0 und 1 gedrückt, wird diese an den Server gesendet.
         ;;Der Server baut so die Zahl der geschlagenen Steine zusammen.
         ;;Beispiel: Wird erst die "1" und anschließend die "3" gedrückt, ergibt sich 13 geschlagenen Steine für den Server.
         [(member? '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0") key_event)
          (make-package w (list 'setkilled key_event))]
         ;;Die Entertaste bestätigt die geschlagenen Steine.
         [(key=? key_event "\r")
          (make-package w 'confirm)]
         ;;Mit der Backspace-Taste lassen sich Eingaben korrigieren.
         [(key=? key_event "\b")
          (make-package w 'delete)]
         ;;Andere Tastatureingaben werden ignoriert und verändern die Welt nicht.     
         [else w])]
      ;;Vorgabe
      [(equal? (car w) 'choosehandicap)
       (cond
         ;;Wird eine Zahl zwischen 0 und 1 gedrückt, wird diese an den Server gesendet.
         ;;Der Server baut so die Höhe der Vorgabe zusammen.
         ;;Beispiel: Wird erst die "1" und anschließend die "3" gedrückt, ergibt sich 13 als Vorgabe für den Server.
         [(member? '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0") key_event)
          (make-package w (list 'sethandicap key_event))]
         ;;Die Entertaste bestätigt die Vorgabe.
         [(key=? key_event "\r")
          (make-package w 'confirm)]
         ;;Mit der Backspace-Taste lassen sich Eingaben korrigieren.
         [(key=? key_event "\b")
          (make-package w 'delete)]
         ;;Andere Tastatureingaben werden ignoriert und verändern die Welt nicht.     
         [else w])]
      [else w])))

;;Hilfsfunktion ob ein Key in der Liste der erlaubten Tasten ist.
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