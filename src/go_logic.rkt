#lang racket
(require 2htdp/universe)

(provide do_set)
(provide do_handicap)
(provide calc-score)

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

;;Vorgabe setzen, wird vom Server bei 'set aufgerufen, wenn Serverstatus 'usehandicap ist.
(define (do_handicap  univ wrld m)
  [if (and (equal? (get-field-state (current_board univ) (second m) (third m)) 0)  ;;Prüfung ob Feld frei ist.
          (check-turn univ (get-color univ (iworld-name wrld)) (second m) (third m))) ;;Prüfung ob Spieler setzen darf
     (let* ([new_board (set-stone (current_board univ) (second m) (third m) (get-color univ (iworld-name wrld)))];;Setze neuen Stein
            [new_handicap (- (sixth univ) 1)]) ;;Vorgabe um 1 verringern.
       (if (< new_handicap 1) ;;Wenn die Vorgabe vollständig gesetzt wurde, wird mit dem Spiel begonnen
          (make-bundle (list
                     (current_worlds univ)
                     'play
                     new_board (current_color univ) (current_killed univ))
                    (list (make-mail (world1 univ) (list 'play new_board (current_killed univ)  'passsatus))
                          (make-mail (world2 univ) (list 'wait new_board (current_killed univ)  'passsatus)))
                    '()) 
       ;;Ansonsten setzt Schwarz und die Vorgabe wird um 1 verringert.
       (make-bundle (list
                     (current_worlds univ)
                     'usehandicap
                     new_board (current_color univ) (current_killed univ) new_handicap)
                    (list (make-mail (world1 univ) (list 'usehandicap new_board (current_killed univ)  new_handicap))
                          (make-mail (world2 univ) (list 'wait new_board (current_killed univ) 'passsatus)))
                    '())))
    ;;Sonstige Anfragen verändern das Universum nicht
    [make-bundle univ '() '()]])

;;Zugdurchführung, wird vom Server bei 'set aufgerufen, wenn Serverstatus 'play ist.
(define (do_set  univ wrld m)
  [if (and (equal? (get-field-state (current_board univ) (second m) (third m)) 0)  ;;;Prüfung ob Feld frei ist.
          (check-turn univ (get-color univ (iworld-name wrld)) (second m) (third m))) ;;Prüfung ob Spieler setzen darf
     (let* ([temp_board (set-stone (current_board univ) (second m) (third m) (get-color univ (iworld-name wrld)))];;Setze neuen Stein
            [new_board_state (find-freedoms temp_board (get-color univ (iworld-name wrld)) '()
                                            (find-opposing-stones
                                             temp_board (second m) (third m)
                                             (get-color univ (iworld-name wrld))
                                             route-list '()))];;Spielbrett und Anzahl geschlagenen Steinen
            [new_board (cdr new_board_state)];;Spielbrett nach entfernen der geschlagenen Steine
            [killed_stones (cond
                               [(= 0 (car new_board_state))
                                (current_killed univ)]
                               [(equal? 1 (get-color univ (iworld-name wrld)));;Geschlagene Steine im Spielzug
                                (list (+ (car new_board_state) (car (current_killed univ))) (cadr (current_killed univ)))]
                               [(equal? -1 (get-color univ (iworld-name wrld)))
                                (list (car (current_killed univ))(+ (car new_board_state) (cadr (current_killed univ))))]
                               )])
       ;;Senden des geänderten Spielbretts
       (make-bundle (list
                     (reverse (current_worlds univ))
                     'play
                     new_board (current_color univ) killed_stones)
                    (list (make-mail (world1 univ) (list 'wait new_board killed_stones  'passsatus))
                          (make-mail (world2 univ) (list 'play new_board killed_stones  'passsatus)))
                    '()))
    ;;Sonstige Anfragen verändern das Universum nicht
    [make-bundle univ '() '()]])

;;Hilfsfunktion um Farbe des Spielers auszugeben
;;
;;univ: Das Universum des Spieles.
;;player: Der Spieler, der am Zug ist.
;;
;;return: Die Farbe (-1 oder 1) des Spielers.
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

;;Prüft während des Auffüllens bei der Auswertung, ob das Auffüllen zulässig ist.
;;Ist das Auffüllen nicht mehr zulässig, hat der Gegener das gebiet erobert
;;
;;Parameter
;;board Die aktuelle Spielfeldbelegung
;;player: Der Spieler, der am Zug ist.
;;y/x:Die y- und x-Koordinaten des neuen Steines.
;;
;;return: Die Zulässigkeit des Zuges.
(define (check-turn-board board player y x)
  (let ((new_board (set-stone board y x player)))
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
      (check-freedom-path board player (cons (cons y x) proofed) (remove-list proofed (cdr free?))))
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
                              (take-right board (- 18 y))))


;;Wertet das übergebene Board aus und ermittelt die eroberten und geschlagenen Steine
;;
;;Parameter
;;board: Das Spielbrett, das ausgewertet werden soll.
;;
;;return: Das Ergebnis der Auswertung als Pair (Punkte für Schwarz . Punkte für Weiß)
(define (calc-score board)
  (let ((empty_areas (find-all-empty-areas (find-empty-position board 0 0 '()) '() route-list))) 
    (cond
      [(= (length empty_areas) 1) '(0 . 0)] ;Wenn es nur ein leere Feld gibt, dann hat niemand eins erobert, also 0 . 0
      ;[(= (length empty_areas) 2) '(0 . 0)];TODO: Es gibt zwei Felder und einst ist erobert. Dann kriegt der, der das eine Gebiet hält, auch das neutrale Gebiet
      [else (check-all-empty-areas board empty_areas '() player-list 0 '(0 . 0))]))
  )

;;Liste mit den beiden Spielern (für Iterationen)
(define player-list '(-1 1))

;;Iteriert über alle leeren Flächen und addiert die erzielten Punkte.
;;
;;Parameter
;;board: Das Spielbrett, das ausgewertet werden soll.
;;empty_areas: Die freien Flächen auf dem Spielbrett.
;;last_area: Die zuletzt ausgewertete Fläche.
;;player-list:Liste mit den beiden Spielern (nur für iteration).
;;player-won: Der Spieler, der die letzte Fläche gewonnen hat. (0 = neutrale Fläche)
;;points: Die Punkte als Pair (Punkte für Schwarz . Punkte für Weiß) gespeichert. 
;;
;;return: Das Ergebnis der Auswertung als Pair (Punkte für Schwarz . Punkte für Weiß)
(define (check-all-empty-areas board empty_areas last_area player-list player-won points)
  (let ((new_points
           (cond
             [(eqv? player-won -1) (cons (calc-points (car points) last_area board player-won)
                                         (cdr points))]
             [(eqv? player-won 1)  (cons (car points)
                                         (calc-points (cdr points) last_area board player-won))]
             [else points])))
  (cond
    [(empty? empty_areas) new_points]
    [else (check-all-empty-areas board
                                 (cdr empty_areas)
                                 (car empty_areas)
                                 player-list
                                 (check-empty-area-for-each-player board (car empty_areas) player-list #t)
                                 new_points)]
  )))

;;Berechnet die neue Punktzahl für den Spieler anhand der freien Positionen
;;und der geschlagenen Steine in der Fläche.
;;
;;Parameter
;;points: Die alte Punktzahl.
;;last_area: Die zuletzt ausgewertete Fläche.
;;board: Das Spielbrett nach Beendigung des Spieles.
;;player-won: Der Spieler, der die letzte Fläche gewonnen hat. (0 = neutrale Fläche)
;;
;;return: Die neue Punktzahl für den Spieler, der die letzte Fläche gewonnen hat.
(define (calc-points points last_area board player-won)
  (let ((area-points (length last_area))
        (killed-stones (count-killed-stones-in-area board last_area player-won 0)))
        (+ points
           area-points
           killed-stones))
  )

;;Besetzt alle leeren Flächen mit der Farbe des Spielers, der die Fläche gewonnen hat.
;;Dabei wird überprüft, ob währenddessen Steine des Gegners geschlagen werden.
;;Ist dies der Fall, werden diese während der Füllung des Gebietes aufaddiert und zurückgegeben.
;;
;;Parameter
;;board: Das Spielbrett, das ausgewertet werden soll.
;;empty_area: Die freie Fläche.
;;last_area: Die zuletzt ausgewertete Fläche.
;;player-won: Der Spieler, der die letzte Fläche gewonnen hat. (0 = neutrale Fläche)
;;result: Die aufaddierten Punkte.
;;
;;return: Liefert die aufaddierten Punkte (geschlagene Steine) zurück,
;;die während des Spielfeld Befüllens aufaddiert wurden.
(define (count-killed-stones-in-area board empty_area player-won result)
  (if (empty? empty_area)
      result
      (let ((new-board (find-freedoms (set-stone board (caar empty_area) (cdar empty_area) player-won)
                                      player-won
                                      '()
                                      (find-opposing-stones
                                       (set-stone board (caar empty_area) (cdar empty_area) player-won)
                                       (caar empty_area) (cdar empty_area)
                                       player-won
                                       route-list '()))))
        (count-killed-stones-in-area (cdr new-board)
                                     (cdr empty_area)
                                     player-won
                                     (+ (car new-board)
                                        result))
        )
      ))


;;Prüft wer die übergebene Fläche gewonnen hat.
;;Wenn beide Spieler die Fläche voll besetzten durften, handelt es sich um eine neutrale Fläche.
;;Wenn ein Spieler sie nicht voll besetzten darf, hat der gegnerische Spieler diese Fläche erobert.
;;
;;Parameter
;;board: Das Spielbrett, das ausgewertet werden soll.
;;empty_area: Die zu prüfende Fläche.
;;last_area: Die zuletzt ausgewertete Fläche.
;;player-list: Liste mit den beiden Spielern (nur für iteration).
;;result: Status, ob dem Spieler zuvor die Fläche gehört.
;;(Falls nicht, gehört dem Gegner die Fläche. Wenn beide das Feld voll ausfüllen dürfen, dann handelt es sich um eine neutrale Fläche.)
;;
;;return: Der Spieler, der die Fläche erobert hat. 0 = neutrale Fläche.
(define (check-empty-area-for-each-player board empty_area player-list result)
  (cond
    [(not result) (if (= (length player-list) 1) 1 -1)]
   [(empty? player-list) 0]
   [else (check-empty-area-for-each-player board empty_area (cdr player-list) (set-empty-area board empty_area (car player-list) #t))])
  )

;;Füllt die leere Fläche vollständig mit der Farbe des übergebenen Spielers.
;;Prüft, ob der Spieler die Fläche bis zum letzten Stein besetzen darf, darf er es nicht, hat der Gegner diese Fläche erobert.
;;Es wird dazu die Methode genutzt, die während des Spieles den Selbstmord verhinder. Denn wenn ein Spieler beim ausfüllen der Fläche
;;einen Selbstmord begehen würde, hat der Gegener diese Fläche erobert, weil der setzende Spieler keine Freiheiten mehr hat.
;;
;;Parameter
;;board: Das Spielbrett, das ausgewertet werden soll.
;;empty_area: Die zu prüfende Fläche.
;;last_area: Die zuletzt ausgewertete Fläche.
;;player: Der zu prüfende player.
;;result: Ob der Stein gesetzt werden darf, ist nur wichtig beim letzten zu setzenden Stein.
;;
;;return: Boolean-Wert, ob der Spieler die Fläche vollständig besetzen darf.
(define (set-empty-area board empty_area player result)
  (if (or (empty? empty_area)
          (not result))
      result
      (let ((result (if (= (length empty_area) 1)
                        (check-turn-board board player (caar empty_area) (cdar empty_area));;letzten beiden Paramenter sind y, x
                        #t )))
      (if result
          (let ((new-board (set-stone board (caar empty_area) (cdar empty_area) player)))
          (set-empty-area new-board
                          (cdr empty_area)
                          player
                          result))
          (set-empty-area board (cdr empty_area) player result)
          )
      ))
  )

;;Entfernt die Elemente einer Liste aus einer anderen liste
;;
;;Parameter
;;list1: Liste mit den Elementen, die aus der list2 entfernt werden sollen.
;;list2: Liste aus denen die Elemente aus list1 entfernt sollen.
;;
;;return: Die neue Liste aus denen die Elemente entfernt werden sollen.
(define (remove-list list1 list2)
  (if(empty? list1)
     list2
     (remove-list (cdr list1) (remove (car list1) list2))
     )
  )

;;Findet alle zusammenhängende leere Flächen.
;;
;;Parameter
;;empty_list: Liste mit Positionen ohne Stein.
;;area_list: Die Liste mit den leeren Flächen.
;;route-list: 
;;
;;return: Liste mit den leeren Fächen, die als Liste mit Koordinaten dargestellt werden.
;;TODO: Felder, die durch Steiner derselben Farbe getrennt sind und eingeschlossen sind, zusammenfassen.
(define (find-all-empty-areas empty_list area_list route-list)
  (let ((new_empty_list (if (= (length area_list) 0) empty_list (remove-list (car area_list) empty_list))))
  (if (empty? new_empty_list)
      area_list
      (find-all-empty-areas new_empty_list (cons (find-area (cons (car new_empty_list) '()) '() new_empty_list route-list) area_list) route-list)
      )
  ))

;;Sucht anhand einer Ausgangsposition nach leeren Flächen
;;
;;Parameter
;;list_to_check: Die Liste mit den noch zu prüfenden Positionen.
;;area: Liste mit den leeren zusammenhängenden Positionen.
;;empty_list: Liste mit den noch nicht geprüften leeren Positionen.
;;
;;return: Boolean-Wert, ob Positionen benachbart sind.
(define (find-area list_to_check area empty_list route-list)
  (if (empty? list_to_check)
      area
      (find-area (append (find-all-empty-neighbors empty_list (car list_to_check) route-list '())
                 (cdr list_to_check))
                 (cons (car list_to_check) area)
                 (remove-list list_to_check empty_list)
                 route-list)
      )
  )

;;Findet alle benachbarten leeren Positionen
;;
;;Parameter
;;empty_list: Liste mit den noch nicht geprüften leeren Positionen.
;;empty_pos: Die Positionen, die auf leere Nachbarn untersucht werden soll.
;;neighbors: Die gefundenen leeren Nachbarn.
;;
;;return: Boolean-Wert, ob Positionen benachbart sind.
(define (find-all-empty-neighbors empty_list empty_pos route-list neighbors)
  (if(empty? route-list)
     neighbors
     (let ((potential_neighbor_pos (cons (+ (car empty_pos) (car (first route-list)))
                                         (+ (cdr empty_pos) (cdr (first route-list))))))
     (if(member? empty_list (car potential_neighbor_pos) (cdr potential_neighbor_pos))
     (find-all-empty-neighbors empty_list empty_pos (cdr route-list) (cons potential_neighbor_pos neighbors))
     (find-all-empty-neighbors empty_list empty_pos (cdr route-list) neighbors)
     )
     ))
  )

;;Prüft, ob zwei Positionen benachbart sind.
;;
;;Parameter
;;pos1/2: Die Position, die überprüft werden sollen.
;;
;;return: Boolean-Wert, ob Positionen benachbart sind.
(define (positions-neighbouring? pos1 pos2)
  (if (= (+ (abs (- (car pos1) (car pos2)))
            (abs (- (cdr pos1) (cdr pos2))))
         1
         )   
      #t
      #f
      )
  )

;;Sammelt alle leere Positionen auf dem Spielfeld.
;;
;;Parameter
;;board: Das Spielbrett mit Belegung.
;;y/x: Die x- und y-Koordinaten der prüfenden Position.
;;
;;return: Liste mit allen leeren Positionen auf dem Feld.
(define (find-empty-position board x y empty_positions)
  (let* ((inc_X (+ 1 x))
         (new_X (if (= inc_X 19) 0 inc_X))
         (new_Y (if (= inc_X 19) (+ y 1) y)))
  (if (and (= x 0) (= y 19))
      empty_positions
      (if (= (position-empty? board x y) 1)
      (find-empty-position board new_X new_Y (cons (cons y x) empty_positions))
      (find-empty-position board new_X new_Y empty_positions)
      )))
  )

;;Prüft ob ein Feld leer ist.
;;
;;Parameter
;;board: Das Spielbrett mit Belegung.
;;y/x: Die x- und y-Koordinaten des neu zu prüfenden Steines.
;;
;;return: Boolean-Wert, ob das Feld leer ist.
(define (position-empty? board x y)
  (if (= (get-field-state board y x) 0)
      1
      0
      )
  )

(define board1 '(
(0 0 0 0 0 -1 0 0 0 0 0 0 0 0 0 0 -1 0 0) 
(0 -1 0 0 0 -1 0 0 0 0 0 0 0 0 0 0 -1 0 0) 
(0 0 -1 -1 1 -1 0 0 0 0 0 0 0 0 0 0 -1 -1 0) 
(0 0 1 1 -1 0 0 0 0 0 0 0 0 0 0 0 -1 -1 -1) 
(-1 -1 -1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
(0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 -1 -1) 
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 0 0) 
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 -1 0) 
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 1) 
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 1) 
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1) 
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
(0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 0 0) 
(1 1 1 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0) 
(0 0 1 0 0 0 0 0 0 0 0 1 1 1 1 1 1 0 0) 
(0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))