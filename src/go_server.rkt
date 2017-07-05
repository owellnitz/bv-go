#lang racket
(require 2htdp/universe)

;;Maximale Anzahl an Spieler
(define NUM_PLAYERS 2)

;;Board-Representation
(define player_color (list "playername" -1 "playername" 1))
(define empty_board  (make-list 19 (make-list 19 0)))
(define UNIVERSE0 
  (list '() 'wait empty_board player_color))

;;Quick accessors for the universe
(define (current_worlds univ)
  (first univ))
(define (world1 univ)
  (first (current_worlds univ)))
(define (world2 univ)
  (second (current_worlds univ)))

(define (current_state univ)
  (second univ))

(define (current_board univ)
  (third univ))

(define (current_color univ)
  (fourth univ))

;;Repräsentation eines Universums
;; '((iworld_active iworld_inactive) status (make-list 19 (make-list 19 0) )
;; 
;;wobei status: 'wait
;;Nachrichten an das Universum
;; (universe world '(set 9)) --> trägt X in den Zustandsraum ein, 
;;                                   informiert die andere Welt,
;;                                   vertauscht active/inactive_world
;; (universe world 'reset)   -->  nur möglich, falls (third universe) == 'finished, 
;;                                startet ein neues Spiel
;;                                vertauscht active/inactive_world


;;Funktion, die herausfindet, ob jemand gewonnen hat
;;TODO

;;Fügt eine neue Welt hinzu 
(define (add-world univ wrld)
  (cond 
         ;;Maximale Anzahl an Spielern erreicht
         ;; --> Weise diese Welt ab
         [(= (length (current_worlds univ)) NUM_PLAYERS)
          (make-bundle univ
                       (list (make-mail wrld (list 'rejected empty_board)))
                       (list wrld))]
         
         ;;Maximale Anzahl an Spielern mit dieser Welt erreicht
         ;; --> Füge die Welt zu den bekannten hinzu
         ;; --> Starte das Spiel
         [(= (length (current_worlds univ)) (- NUM_PLAYERS 1))
          (make-bundle (list
                        (append (current_worlds univ) (list wrld))
                        'started
                        empty_board player_color)
                       (list (make-mail (world1 univ) (list 'started empty_board))
                             (make-mail wrld   (list 'wait empty_board)))
                       '())]
         
         ;;Maximale Anzahl an Spielern noch nicht erreicht
         ;; --> Füge die Welt zu den bekannten hinzu
         [else 
          (make-bundle (list 
                        (append (current_worlds univ) (list wrld))
                        'wait 
                        empty_board player_color)
                       (list (make-mail wrld (list 'wait empty_board)))
                       '())]))
 
;;Nachrichtenaustausch zwischen den Welten 
(define (handle-messages univ wrld m)
  (cond 
    ;;Das Spiel gilt als beendet -> Anfrage eines Neustarts durch eine Welt
    [(and (equal? (current_state univ) 'finished) 
              (equal? m 'restart))
         (make-bundle (list
                       (reverse (current_worlds univ))
                       'play
                       empty_board player_color)
                      (list (make-mail (world1 univ) (list 'wait empty_board))
                            (make-mail (world2 univ) (list 'play empty_board)))
                      '())]
    ;;Das Spiel startet -> Spieler wählt Spielstart aus BV oder neues Spiel
[(and (equal? (current_state univ) 'started) 
              (equal? m 'newgame))
         (make-bundle (list
                       (current_worlds univ)
                       'newgame
                       empty_board player_color)
                      (list (make-mail (world1 univ) (list 'newgame empty_board))
                            (make-mail (world2 univ) (list 'wait empty_board)))
                      '())]
    
    ;Farbwahl bei Neustart
    ;;Der Spieler wählt schwarz
    [(and (equal? (current_state univ) 'newgame) 
              (equal? m 'black))
     (let* ([choosen_color (list (iworld-name (world1 univ)) -1 (iworld-name (world2 univ)) 1)])
         (make-bundle (list
                       (current_worlds univ)
                       'play
                       empty_board choosen_color)
                      (list (make-mail (world1 univ) (list 'play empty_board))
                            (make-mail (world2 univ) (list 'wait empty_board)))
                      '()))]

      ;;Der Spieler wählt weiß
    [(and (equal? (current_state univ) 'newgame) 
              (equal? m 'white))
     (let* ([choosen_color (list (iworld-name (world2 univ)) -1 (iworld-name (world1 univ)) 1)])
         (make-bundle (list
                       (reverse (current_worlds univ))
                       'play
                       empty_board choosen_color)
                      (list (make-mail (world1 univ) (list 'wait empty_board))
                            (make-mail (world2 univ) (list 'play empty_board)))
                      '()))]
    
    
    ;;Eine Welt möchte ein Feld markieren, dazu schickt sie (set FELD_NR) an das Universum
    ;;Prüfe ob: 1. Nachricht im richtigen Format
    ;;          2. Welt gerade spielen darf
    ;;          3. Feld noch frei ist
    ;;TODO      4. Ob Zug gültig ist 
    [(and (list? m) (= (length m) 3) (equal? (first m) 'set)      ;; 1.
          (iworld=? wrld (world1 univ))                           ;; 2.
          (equal? (get-field-state univ (second m) (third m)) 0)  ;;3.
          (check-turn univ (get-color univ (iworld-name wrld)) (second m) (third m))) ;; 4.
     (let* ([new_board (append (take (current_board univ) (second m))
                               (list (list-set (list-ref (current_board univ) (second m)) (third m) (get-color univ (iworld-name wrld))))
                              (take-right (current_board univ) (- 18 (second m))))])
       ;;Haben beide Spieler gepasst?    
                      
               ;;Falls nein, ist der andere Spieler dran - alles geht einfach weiter
              (make-bundle (list 
                             (reverse (current_worlds univ))
                             'play 
                             new_board (current_color univ))
                            (list (make-mail (world1 univ) (list 'wait new_board))
                                  (make-mail (world2 univ) (list 'play new_board)))
                            '()))]
    ;;Sonstige Anfragen verändern das Universum nicht
    [else (make-bundle univ '() '())]))

;;Hilfsfunktion um Farbe des Spielers auszugeben
(define (get-color univ player)
  (second (member player (current_color univ))))

;;Zugprüfung auf Gültigkeit
(define (check-turn univ player y x)
  [not (and
        (or (equal? x 0) (equal? (inverse-player player) (get-field-state univ y (- x 1))))
        (or (equal? x 18) (equal? (inverse-player player) (get-field-state univ y (+ x 1))))
        (or (equal? y 0) (equal? (inverse-player player) (get-field-state univ (- y 1) x)))
        (or (equal? y 18) (equal? (inverse-player player) (get-field-state univ (+ y 1) x))))]
  )

;;Gebe anderen Spieler zurück
(define (inverse-player player)
  (if(equal? player -1)
     1
     -1
     )
  )

;Liefert den Stein der Koordinaten zurück
(define (get-field-state univ y x)
  (list-ref (list-ref (current_board univ) y) x)
  )

;;Überprüfe Freiheiten
(define (check-freedom univ player proofed proof)
  (let ((y (car (first proof)))
    (x (cdr (first proof))))
  (if (and
       (or (equal? x 0) (equal? (inverse-player player) (get-field-state univ y (- x 1))) (member (cons y (- x 1)) proofed))
       (or (equal? x 0) (equal? (inverse-player player) (get-field-state univ y (+ x 1))) (member (cons y (+ x 1)) proofed))
       (or (equal? x 0) (equal? (inverse-player player) (get-field-state univ (- y 1) x)) (member (cons (- y 1) x) proofed))
       (or (equal? x 0) (equal? (inverse-player player) (get-field-state univ (+ y 1) x)) (member (cons (+ y 1) x) proofed)))
      1;;TODO: lösche proofed
      (check-freedom univ player (cons (first proof) proofed) proof))
    )
  )

;;Erschafft ein Universum
(universe UNIVERSE0
          (on-new add-world)
          (on-msg handle-messages)
          (state #t))
 