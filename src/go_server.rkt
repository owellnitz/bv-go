#lang racket
(require 2htdp/universe)
(require "go_bv.rkt")


;;Maximale Anzahl an Spieler
(define NUM_PLAYERS 2)

;;Board-Representation
(define player_color (list "playername" -1 "playername" 1))
;Geschlagene Steine (schwarz weiß)
(define killed_empty (list 0 0))
(define empty_board  (make-list 19 (make-list 19 0)))
(define UNIVERSE0 
  (list '() 'wait empty_board player_color killed_empty))

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

  (define (current_killed univ)
  (fifth univ))
  
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
                        empty_board player_color killed_empty)
                       (list (make-mail (world1 univ) (list 'started empty_board (current_killed univ)))
                             (make-mail wrld   (list 'wait empty_board (current_killed univ))))
                       '())]
         
         ;;Maximale Anzahl an Spielern noch nicht erreicht
         ;; --> Füge die Welt zu den bekannten hinzu
         [else 
          (make-bundle (list 
                        (append (current_worlds univ) (list wrld))
                        'wait 
                        empty_board player_color killed_empty)
                       (list (make-mail wrld (list 'wait empty_board (current_killed univ))))
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
                       empty_board player_color killed_empty)
                      (list (make-mail (world1 univ) (list 'wait empty_board (current_killed univ)))
                            (make-mail (world2 univ) (list 'play empty_board (current_killed univ))))
                      '())]
    ;;Das Spiel startet -> Spieler wählt Spielstart aus BV oder neues Spiel
    [(and (equal? (current_state univ) 'started) 
              (equal? m 'newgame))
         (make-bundle (list
                       (current_worlds univ)
                       'newgame
                       empty_board player_color killed_empty)
                      (list (make-mail (world1 univ) (list 'choosecolor empty_board (current_killed univ)))
                            (make-mail (world2 univ) (list 'wait empty_board (current_killed univ))))
                      '())]

    ;Start aus BV
    [(and (equal? (current_state univ) 'started)
      (equal? m 'newbvgame))
     (let* ([new_board bord-state])
       (make-bundle (list
               (current_worlds univ)
               'setkilledblack
               new_board player_color killed_empty)
              (list (make-mail (world1 univ) (list 'setkilledblack new_board (current_killed univ)))
                            (make-mail (world2 univ) (list 'wait new_board (current_killed univ))))
                      '()))]

    ;Eingabe der geschlagenen Steine
    ;Zuerst schwarze Steine eingeben
    [(and (equal? (current_state univ) 'setkilledblack)
          (pair? m)
          (equal? (car m) 'set))
     (let* ([killed_stones (list (+ (* 10 (car (current_killed univ))) (string->number(cadr m))) (cadr(current_killed univ)))])
     (make-bundle (list
               (current_worlds univ)
               'setkilledblack
               (current_board univ) player_color killed_stones)
              (list (make-mail (world1 univ) (list 'setkilledblack (current_board univ) killed_stones))
                            (make-mail (world2 univ) (list 'wait (current_board univ) killed_stones)))
                      '()))]
    ;Löschen der Eingabe
        [(and (equal? (current_state univ) 'setkilledblack)
          (equal? m 'delete))
     (let* ([killed_stones (list (/ 10 (- (car (current_killed univ)) (remainder (car (current_killed univ)) 10))) (cadr (current_killed univ)))])
     (make-bundle (list
               (current_worlds univ)
               'setkilledblack
               (current_board univ) player_color killed_stones)
              (list (make-mail (world1 univ) (list 'setkilledblack (current_board univ) killed_stones))
                            (make-mail (world2 univ) (list 'wait (current_board univ) killed_stones)))
              '()))]
    
    ;Bestätigen der geschlagenen Schwarzen Steine
        [(and (equal? (current_state univ) 'setkilledblack)
          (equal? m 'confirm))
     (make-bundle (list
               (current_worlds univ)
               'setkilledwhite
               (current_board univ) player_color (current_killed univ))
              (list (make-mail (world1 univ) (list 'setkilledwhite (current_board univ) (current_killed univ)))
                            (make-mail (world2 univ) (list 'wait (current_board univ) (current_killed univ))))
                      '())]
        
    ;Dann weiße Steine eingeben
    [(and (equal? (current_state univ) 'setkilledwhite)
          (pair? m)
          (equal? (car m) 'set))
     (let* ([killed_stones (list (car(current_killed univ)) (+ (* 10 (cadr (current_killed univ))) (string->number(cadr m))))])
     (make-bundle (list
               (current_worlds univ)
               'setkilledwhite
               (current_board univ) player_color killed_stones)
              (list (make-mail (world1 univ) (list 'setkilledwhite (current_board univ) killed_stones))
                            (make-mail (world2 univ) (list 'wait (current_board univ) killed_stones)))
                      '()))]
    ;Löschen der Eingabe
        [(and (equal? (current_state univ) 'setkilledwhite)
          (equal? m 'delete))
     (let* ([killed_stones (list (cadr (current_killed univ)) (/ 10 (- (car (current_killed univ)) (remainder (car (current_killed univ)) 10))))])
     (make-bundle (list
               (current_worlds univ)
               'setkilledwhite
               (current_board univ) player_color killed_stones)
              (list (make-mail (world1 univ) (list 'setkilledwhite (current_board univ) killed_stones))
                            (make-mail (world2 univ) (list 'wait (current_board univ) killed_stones)))
              '()))]
    
    ;Bestätigen der geschlagenen weißen Steine
        [(and (equal? (current_state univ) 'setkilledwhite)
          (equal? m 'confirm))
     (make-bundle (list
               (current_worlds univ)
               'newgame
               (current_board univ) player_color (current_killed univ))
              (list (make-mail (world1 univ) (list 'choosecolor (current_board univ) (current_killed univ)))
                            (make-mail (world2 univ) (list 'wait (current_board univ) (current_killed univ))))
                      '())]
        
      
    ;Farbwahl bei Spielstart
    ;;Der Spieler wählt schwarz
    [(and (equal? (current_state univ) 'newgame) 
              (equal? m 'black))
     (let* ([choosen_color (list (iworld-name (world1 univ)) -1 (iworld-name (world2 univ)) 1)])
         (make-bundle (list
                       (current_worlds univ)
                       'play
                       (current_board univ) choosen_color (current_killed univ))
                      (list (make-mail (world1 univ) (list 'play (current_board univ) (current_killed univ)))
                            (make-mail (world2 univ) (list 'wait (current_board univ) (current_killed univ))))
                      '()))]

      ;;Der Spieler wählt weiß
    [(and (equal? (current_state univ) 'newgame) 
              (equal? m 'white))
     (let* ([choosen_color (list (iworld-name (world2 univ)) -1 (iworld-name (world1 univ)) 1)])
         (make-bundle (list
                       (reverse (current_worlds univ))
                       'play
                       (current_board univ) choosen_color (current_killed univ)) 
                      (list (make-mail (world1 univ) (list 'wait (current_board univ) (current_killed univ)))
                            (make-mail (world2 univ) (list 'play (current_board univ) (current_killed univ))))
                      '()))]
    
    
    ;;Eine Welt möchte ein Feld markieren, dazu schickt sie (set FELD_NR) an das Universum
    ;;Prüfe ob: 1. Nachricht im richtigen Format
    ;;          2. Welt gerade spielen darf
    ;;          3. Feld noch frei ist
    ;;          4. Ob Zug gültig ist
    ;;          5. Prüfe Freiheiten
    [(and (list? m) (= (length m) 3) (equal? (first m) 'set)      ;; 1.
          (iworld=? wrld (world1 univ))                           ;; 2.
          (equal? (get-field-state (current_board univ) (second m) (third m)) 0)  ;;3.
          (check-turn univ (get-color univ (iworld-name wrld)) (second m) (third m))) ;;4.
     (let* ([temp_board (set-stone (current_board univ) (second m) (third m) (get-color univ (iworld-name wrld)))];;Setze neuen Stein
            [new_board_state (find-freedoms temp_board (get-color univ (iworld-name wrld)) '()
                                            (find-opposing-stones
                                             temp_board (second m) (third m)
                                             (get-color univ (iworld-name wrld))
                                             route-list '()))];;Spielbrett und Anzahl geschlagenen Steinen
            [new_board (cdr new_board_state)];;Spielbrett nach entfernen der geschlagenen Steine
            [killed_stones (car new_board_state)]);;Geschlagene Steine im Spielzug
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


;;Zugprüfung auf Gültigkeit.
;;Verhindert den Selbstmord eines Spielers, außer der Selbstmord schlägt Steine des Gegners.
;;
;;Parameter
;;univ: Das Universum des Spieles.
;;player: Der Spieler, der am Zug ist.
;;y/x:Die y- und x-Koordinaten des neuen Steines.
;;
;;return: Die Zulässigkeit des Zuges.
(define (check-turn univ player y x)
  (let ((new_board (set-stone (current_board univ) y x player)))
    (if(> (car (find-freedoms new_board player '()
                                      (find-opposing-stones new_board y x player route-list '()))) 0)
       #t
       (if(equal?
           (get-field-state (cdr (find-freedoms new_board (inverse-player player) '() (list (cons y x)))) y x)
           0)
          #f
          #t
          ))
    )
  )

;;Gibt den gegnerischen Spieler zurück.
;;
;;Parameter
;;player: Der aktuelle Spieler.
;;
;;return: Der gegnerische Spieler.
(define (inverse-player player)
  (if(equal? player -1)
     1
     -1
     )
  )

;Liefert den Zustand des Feldes Zurück
;
;Paramter
;board: Das Spielbrett mit Belegung.
;y: y-Koordinate des gesuchten Feldes.
;x: x-Koordinate des gesuchten Feldes.
;
;Return: Zustand (Schwarz, weiß, leer) des gesuchten Feldes.
(define (get-field-state board y x)
  (list-ref (list-ref board y) x)
  )

;;Dient zur Berechnung der umliegenden Koordinaten eines Feldes.
;;        3
;;      1 o 2
;;        4
(define route-list (list (cons 0 -1)  ;1
                         (cons 0 1)   ;2
                         (cons -1 0)  ;3
                         (cons 1 0))) ;4

;;Prüft jeden gegnerischen Stein (= Start des möglichen Pfades) um den gesetzten Stein herum.
;;
;;Parameter
;;board: Das Spielbrett mit Belegung.
;;player: Der Spieler, der am Zug ist.
;;beaten_stones: Die bereits geschlagenen Steine
;;start_stones: Die umliegenden Steine um den neu gesetzten Stein, die geprüft werden müssen.
;;
;;return: Die Anzahl der geschlagene Steine und die neue Belegung des Spielfeldes
(define (find-freedoms board player beaten_stones start_stones)
  (if(empty? start_stones)
  (cons (length beaten_stones) (kill-stones board beaten_stones));kill
  (find-freedoms board player (append beaten_stones (check-freedom-path board player '() (list (car start_stones)))) (cdr start_stones))
  ))

;;Sucht ab des Startsteines eine Freiheit
;;
;;Parameter
;;board: Das Spielbrett mit Belegung.
;;player: Der Spieler, der am Zug ist.
;;proofed: Die bereits geprüften Steine.
;;proof: Die noch zu prüfende Steine.
;;
;;return: Wenn keine Freiheit gefunden wurde, die Liste mit den geschlagenen Steinen. Sonst die leere Liste.
(define (check-freedom-path board player proofed proof)
  (if (empty? proof)
      proofed
  (let* ((y (car (first proof)))
    (x (cdr (first proof)))
    (free? (check-coordinate board x y player proof proofed route-list)))
  (if (equal? free? 'free)
      '()
      (check-freedom-path board player (cons (cons y x) proofed) (cdr free?)))
    )
  ))

;;Prüfe um eine Koordinate herum, ob sie Eingeschlossen ist, Freiheiten oder befreundete Steine hat.
;;board: Das Spielbrett mit Belegung.
;;x-pos: Die x-Koordinate des zu prüfenden Feldes.
;;y-pos: Die y-Koordinate des zu prüfenden Feldes.
;;proofed: Die bereits geprüften Steine.
;;proof: Die noch zu prüfende Steine.
;;player: Der Spieler, der am Zug ist.
;;pos-list: Hilfsliste für das Finden der umliegenden Koordinaten.
;;
;;return: Die Liste mit den neu zu prüfenden Steinen.
;;        Es können Steine enthalten sein, die Information, dass eine Freiheit gefunden wurde oder die Liste ist leer,
;;        weil der Stein eingeschlossen ist.
(define (check-coordinate board x-pos y-pos player proof proofed pos-list)
    (if(or (empty? pos-list) (equal? proof 'free))
       proof
       (let ((y (+ y-pos (car (first pos-list))))
             (x (+ x-pos (cdr (first pos-list)))))
         (cond
         ;Eingeschlossen
         [(or (< x 0) (> x 18) (< y 0) (> y 18) (member? proofed y x) (equal? player (get-field-state board y x)))
          (check-coordinate board x-pos y-pos player proof proofed (cdr pos-list))]
         ;Nächsten Stein eigener Farbe gefunden --> proof
         [(equal? (inverse-player player) (get-field-state board y x))
          (check-coordinate board x-pos y-pos player (append proof (list (cons y x))) proofed (cdr pos-list))]
         ;Freiheit
         [(equal? 0 (get-field-state board y x))
          (check-coordinate board x-pos y-pos player 'free proofed (cdr pos-list))]
         )
       )))

;;Prüft ob eine Koordinate in der Liste vorhanden ist
;;
;;Parameter
;;list: Die Liste, die durchsucht werden soll.
;;y/x: Die Koordinaten, die in der Liste gesucht werden.
;;
;;return: Koordinate in der Liste?
(define (member? list y x)
  (if (list? (member (cons y x) list))
      #t
      #f
   )
  )

;;Sucht gegnerische Steine, um den neu gesetzten Stein.
;;
;;Parameter
;;board: Das Spielbrett mit Belegung.
;;player: Der Spieler, der den Stein gesetzt hat.
;;x-pos: Die x-Koordinate des zu prüfenden Feldes.
;;y-pos: Die y-Koordinate des zu prüfenden Feldes.
;;pos-list: Hilfsliste für das Finden der umliegenden Koordinaten.
;;proof: Die gegnerischen Steinen, um den neuen Stein herum.
;;
;;return: Die Liste mit den gegnerischen Steinen, um den neuen Stein herum.
;;        Ab diesen Steine muss überprüft werden, ob Steine geschalgen wurden. 
(define (find-opposing-stones board y-pos x-pos player pos-list proof)
    (if(empty? pos-list)
       ;TRUE
       proof
       ;FALSE
       (let ((y (+ y-pos (car (first pos-list))))
             (x (+ x-pos (cdr (first pos-list)))))
         (cond
           ;Eingeschlossen
           [(or (< x 0) (> x 18) (< y 0) (> y 18) (equal? player (get-field-state board y x)))
            (find-opposing-stones board y-pos x-pos player (cdr pos-list) proof)]
           ;Stein von Gegner gefunden --> proof
           [(equal? (inverse-player player) (get-field-state board y x))
            (find-opposing-stones board y-pos x-pos player (cdr pos-list) (cons (cons y x) proof))]
           ;Freiheit
           [(equal? 0 (get-field-state board y x))
            (find-opposing-stones board y-pos x-pos player (cdr pos-list) proof)]
           )
         )
       )
  )

;;Entfernt die geschlagenen Steine von dem Spielfeld.
;;
;;Parameter
;;board: Das Spielbrett mit Belegung.
;;beaten_stones: Die geschlagenen Steine.
;;
;;return: Die Spielfeldbelegung ohne die geschlagenen Steine
(define (kill-stones board beaten_stones)
  (if (empty? beaten_stones)
      board
      (let* ((y (car (first beaten_stones)))
             (x (cdr (first beaten_stones))))
        (kill-stones (set-stone board y x 0) (cdr beaten_stones)))
  ))

;;Setzt einen Stein auf dem Spielfeld.
;;
;;Parameter
;;board: Das Spielbrett mit Belegung.
;;y/x: Die x- und y-Koordinaten des neu zu setzenden Steines.
;;color: Die Farbe des zu setzenden Steines.
;;
;;return: Die Belegung Spielfeldes mit dem neuen Stein.
(define (set-stone board y x color)
(append (take board y)
        (list (list-set (list-ref board y) x color))
                              (take-right board (- 18 y)))
  )

;;Erschafft ein Universum
(universe UNIVERSE0
          (on-new add-world)
          (on-msg handle-messages)
          (state #f))
 