#lang racket
(require 2htdp/universe)

;;Maximale Anzahl an Spieler
(define NUM_PLAYERS 2)

;;Board-Representation
(define empty_board  (make-list 19 (make-list 19 0)))
(define UNIVERSE0 
  (list '() 'wait empty_board))

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
                        'play
                        empty_board)
                       (list (make-mail (world1 univ) (list 'play empty_board))
                             (make-mail wrld   (list 'wait empty_board)))
                       '())]
         
         ;;Maximale Anzahl an Spielern noch nicht erreicht
         ;; --> Füge die Welt zu den bekannten hinzu
         [else 
          (make-bundle (list 
                        (append (current_worlds univ) (list wrld))
                        'wait 
                        empty_board)
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
                       empty_board)
                      (list (make-mail (world1 univ) (list 'wait empty_board))
                            (make-mail (world2 univ) (list 'play empty_board)))
                      '())]
    ;;Eine Welt möchte ein Feld markieren, dazu schickt sie (set FELD_NR) an das Universum
    ;;Prüfe ob: 1. Nachricht im richtigen Format
    ;;          2. Welt gerade spielen darf
    ;;          3. Feld noch frei ist
    ;;          4. Ob Zug gültig ist
    ;;          5. Prüfe Freiheiten
    [(and (list? m) (= (length m) 3) (equal? (first m) 'set)      ;; 1.
          (iworld=? wrld (world1 univ))                           ;; 2.
          (equal? (get-field-state (current_board univ) (second m) (third m)) 0)  ;;3.
          (check-turn univ (string->number (iworld-name wrld)) (second m) (third m))) ;;4.
     (let* ([temp_board (set-stone (current_board univ) (second m) (third m) (string->number (iworld-name wrld)))];;Neuer Stein
            [new_board (check-freedom temp_board (string->number (iworld-name wrld)) '()
                                      (find-first-proofs temp_board (second m) (third m) (string->number (iworld-name wrld)) route-list '()))]);;Check auf Freiheiten
       ;;Haben beide Spieler gepasst?           
               ;;Falls nein, ist der andere Spieler dran - alles geht einfach weiter
              (make-bundle (list 
                             (reverse (current_worlds univ))
                             'play 
                             new_board)
                            (list (make-mail (world1 univ) (list 'wait new_board))
                                  (make-mail (world2 univ) (list 'play new_board)))
                            '()))]
    ;;Sonstige Anfragen verändern das Universum nicht
    [else (make-bundle univ '() '())]))

;;Zugprüfung auf Gültigkeit
(define (check-turn univ player y x)
  [not (and
        (or (equal? x 0) (equal? (inverse-player player) (get-field-state (current_board univ) y (- x 1))))
        (or (equal? x 18) (equal? (inverse-player player) (get-field-state (current_board univ) y (+ x 1))))
        (or (equal? y 0) (equal? (inverse-player player) (get-field-state (current_board univ) (- y 1) x)))
        (or (equal? y 18) (equal? (inverse-player player) (get-field-state (current_board univ) (+ y 1) x))))]
  )

;;Gebe anderen Spieler zurück
(define (inverse-player player)
  (if(equal? player -1)
     1
     -1
     )
  )

;Liefert den Stein der Koordinaten zurück
(define (get-field-state board y x)
  (list-ref (list-ref board y) x)
  )

;;Koordinaten um eine Steinposition herum
(define route-list (list (cons 0 -1)
                         (cons 0 1)
                         (cons -1 0)
                         (cons 1 0)))

;;Überprüfe Freiheiten der gegnerischen Steine, um den gesetzten Stein herum
(define (check-freedom board player proofed proof)
  (if (empty? proof)
      (kill-stones board proofed);kill
  (let* ((y (car (first proof)))
    (x (cdr (first proof)))
    (free? (check-coordinate board x y player proof proofed route-list)))
  (if (equal? free? 'free)
      board
      (check-freedom board player (cons (cons y x) proofed) (cdr free?)))
    )
  ))

;;Prüfe um eine Koordinate herum, ob sie Eingeschlossen ist, Freiheiten oder befreundete Steine hat
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
(define (member? list y x)
  (if (list? (member (cons y x) list))
      #t
      #f
   )
  )

;;Finde erste zu prüfende Steine (Steine des Gegners, die geschlagen sein können)
(define (find-first-proofs board y-pos x-pos player pos-list proof)
    (if(empty? pos-list)
       ;TRUE
       proof
       ;FALSE
       (let ((y (+ y-pos (car (first pos-list))))
             (x (+ x-pos (cdr (first pos-list)))))
         (cond
           ;Eingeschlossen
           [(or (< x 0) (> x 18) (< y 0) (> y 18) (equal? player (get-field-state board y x)))
            (find-first-proofs board y-pos x-pos player (cdr pos-list) proof)]
           ;Stein von Gegner gefunden --> proof
           [(equal? (inverse-player player) (get-field-state board y x))
            (find-first-proofs board y-pos x-pos player (cdr pos-list) (cons (cons y x) proof))]
           ;Freiheit
           [(equal? 0 (get-field-state board y x))
            (find-first-proofs board y-pos x-pos player (cdr pos-list) proof)]
           )
         )
       )
  )

(define (kill-stones board proofed)
  (if (empty? proofed)
      board
      (let* ((y (car (first proofed)))
             (x (cdr (first proofed))))
        (kill-stones (set-stone board y x 0) (cdr proofed)))
  ))

(define (set-stone board y x color)
(append (take board y)
        (list (list-set (list-ref board y) x color))
                              (take-right board (- 18 y)))
  )

;;Erschafft ein Universum
(universe UNIVERSE0
          (on-new add-world)
          (on-msg handle-messages))
 