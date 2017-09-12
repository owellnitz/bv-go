#lang racket
(require 2htdp/universe)
(require "go_bv.rkt")
(require "go_logic.rkt")


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

;;Hilfsfunktion um Farbe des Spielers auszugeben
(define (get-color univ player)
  (second (member player (current_color univ))))


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
                       (list (make-mail (world1 univ) (list 'started empty_board (current_killed univ) 'passsatus))
                             (make-mail wrld   (list 'wait empty_board (current_killed univ) 'passsatus)))
                       '())]
         
         ;;Maximale Anzahl an Spielern noch nicht erreicht
         ;; --> Füge die Welt zu den bekannten hinzu
         [else 
          (make-bundle (list 
                        (append (current_worlds univ) (list wrld))
                        'wait 
                        empty_board player_color killed_empty)
                       (list (make-mail wrld (list 'wait empty_board (current_killed univ) 'passsatus)))
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
                      (list (make-mail (world1 univ) (list 'wait empty_board (current_killed univ) 'passsatus))
                            (make-mail (world2 univ) (list 'play empty_board (current_killed univ) 'passsatus)))
                      '())]
    ;;Das Spiel startet -> Spieler wählt Spielstart aus BV oder neues Spiel
    [(and (equal? (current_state univ) 'started) 
              (equal? m 'newgame))
         (make-bundle (list
                       (current_worlds univ)
                       'newgame
                       empty_board player_color killed_empty)
                      (list (make-mail (world1 univ) (list 'choosecolor empty_board (current_killed univ) 'passsatus))
                            (make-mail (world2 univ) (list 'wait empty_board (current_killed univ) 'passsatus)))
                      '())]

    ;Start aus BV
    [(and (equal? (current_state univ) 'started)
      (equal? m 'newbvgame))
 ;;(let* ([new_board bord-state])
 (let* ([new_board '((0 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
(0 0 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
(-1 -1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
(0 1 0 0 0 -1 -1 1 0 0 0 0 0 0 0 0 0 0 0) 
(0 0 0 0 -1 1 1 -1 0 0 0 0 0 0 0 0 1 0 0) 
(0 0 0 -1 1 1 -1 0 0 0 0 0 0 0 0 1 0 1 0) 
(0 0 0 -1 1 -1 0 0 0 0 0 0 0 0 0 1 1 0 0) 
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 -1 1 0) 
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0) 
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
(0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0) 
(0 0 0 0 0 0 0 0 0 1 0 1 -1 0 0 0 -1 0 0) 
(0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0 0 0) 
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
(0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 0) 
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))])
 (make-bundle (list
               (current_worlds univ)
               'setkilledblack
               new_board player_color killed_empty)
              (list (make-mail (world1 univ) (list 'setkilledblack new_board (current_killed univ) 'passsatus))
                            (make-mail (world2 univ) (list 'wait new_board (current_killed univ) 'passsatus)))
                      '()))]

    ;Eingabe der geschlagenen Steine
    ;Zuerst schwarze Steine eingeben
    [(and (equal? (current_state univ) 'setkilledblack)
          (pair? m)
          (equal? (car m) 'setkilled))
     (let* ([killed_stones (list (+ (* 10 (car (current_killed univ))) (string->number(cadr m))) (cadr(current_killed univ)))])
     (make-bundle (list
               (current_worlds univ)
               'setkilledblack
               (current_board univ) player_color killed_stones)
              (list (make-mail (world1 univ) (list 'setkilledblack (current_board univ) killed_stones 'passsatus))
                            (make-mail (world2 univ) (list 'wait (current_board univ) killed_stones 'passsatus)))
                      '()))]
    ;Löschen der Eingabe
        [(and (equal? (current_state univ) 'setkilledblack)
          (equal? m 'delete))
     (let* ([killed_stones (list (/ 10 (- (car (current_killed univ)) (remainder (car (current_killed univ)) 10))) (cadr (current_killed univ)))])
     (make-bundle (list
               (current_worlds univ)
               'setkilledblack
               (current_board univ) player_color killed_stones)
              (list (make-mail (world1 univ) (list 'setkilledblack (current_board univ) killed_stones 'passsatus))
                            (make-mail (world2 univ) (list 'wait (current_board univ) killed_stones 'passsatus)))
              '()))]
    
    ;Bestätigen der geschlagenen Schwarzen Steine
        [(and (equal? (current_state univ) 'setkilledblack)
          (equal? m 'confirm))
     (make-bundle (list
               (current_worlds univ)
               'setkilledwhite
               (current_board univ) player_color (current_killed univ))
              (list (make-mail (world1 univ) (list 'setkilledwhite (current_board univ) (current_killed univ) 'passsatus))
                            (make-mail (world2 univ) (list 'wait (current_board univ) (current_killed univ) 'passsatus)))
                      '())]
        
    ;Dann weiße Steine eingeben
    [(and (equal? (current_state univ) 'setkilledwhite)
          (pair? m)
          (equal? (car m) 'setkilled))
     (let* ([killed_stones (list (car(current_killed univ)) (+ (* 10 (cadr (current_killed univ))) (string->number(cadr m))))])
     (make-bundle (list
               (current_worlds univ)
               'setkilledwhite
               (current_board univ) player_color killed_stones)
              (list (make-mail (world1 univ) (list 'setkilledwhite (current_board univ) killed_stones 'passsatus))
                            (make-mail (world2 univ) (list 'wait (current_board univ) killed_stones 'passsatus)))
                      '()))]
    ;Löschen der Eingabe
        [(and (equal? (current_state univ) 'setkilledwhite)
          (equal? m 'delete))
     (let* ([killed_stones (list (cadr (current_killed univ)) (/ 10 (- (car (current_killed univ)) (remainder (car (current_killed univ)) 10))))])
     (make-bundle (list
               (current_worlds univ)
               'setkilledwhite
               (current_board univ) player_color killed_stones)
              (list (make-mail (world1 univ) (list 'setkilledwhite (current_board univ) killed_stones 'passsatus))
                            (make-mail (world2 univ) (list 'wait (current_board univ) killed_stones 'passsatus)))
              '()))]
    
    ;Bestätigen der geschlagenen weißen Steine
        [(and (equal? (current_state univ) 'setkilledwhite)
          (equal? m 'confirm))
     (make-bundle (list
               (current_worlds univ)
               'newgame
               (current_board univ) player_color (current_killed univ))
              (list (make-mail (world1 univ) (list 'choosecolor (current_board univ) (current_killed univ) 'passsatus))
                            (make-mail (world2 univ) (list 'wait (current_board univ) (current_killed univ) 'passsatus)))
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
                      (list (make-mail (world1 univ) (list 'play (current_board univ) (current_killed univ) 'passsatus))
                            (make-mail (world2 univ) (list 'wait (current_board univ) (current_killed univ) 'passsatus)))
                      '()))]

      ;;Der Spieler wählt weiß
    [(and (equal? (current_state univ) 'newgame) 
              (equal? m 'white))
     (let* ([choosen_color (list (iworld-name (world2 univ)) -1 (iworld-name (world1 univ)) 1)])
         (make-bundle (list
                       (reverse (current_worlds univ))
                       'play
                       (current_board univ) choosen_color (current_killed univ)) 
                      (list (make-mail (world1 univ) (list 'wait (current_board univ) (current_killed univ) 'passsatus))
                            (make-mail (world2 univ) (list 'play (current_board univ) (current_killed univ) 'passsatus)))
                      '()))]
    
    
    ;;Eine Welt möchte ein Feld markieren, dazu schickt sie (set Y-Koordinate X-Koordinate) an das Universum
    ;;Prüfe ob: 1. Nachricht im richtigen Format
    ;;          2. Welt gerade spielen darf
    ;;;;
    ;;univ: Das Universum
    ;;wrld: Die Welt des Spielers.
    ;;m: Die Nachricht des Clients (set Y-Koordinate X-Koordinate).
    ;;
    ;;return: Bei gültigem Zug das veränderte Universum nach dem Zug.
    ;;        Bei ungültigem Zug das unveränderte Universum.
    ;;Aufruf von "do_set" aus "go_logic". Dort wird Zuggültigkeit und Folgen des Zuges geprüft und verarbeitet.
    
    [(and (list? m) (= (length m) 3) (equal? (first m) 'set)      ;; 1.
          (iworld=? wrld (world1 univ)))                           ;; 2.
     (do_set univ wrld m)]

    ;;Ein Spieler passt. Der Client schickt 'passed
    ;;Prüfen ob es das erste Passen ist oder der andere Spieler im Zug davor schon gepasst hat.
    ;;
    ;;Ansonsten ist der andere Spieler am Zug
    [(and (equal? (current_state univ) 'play) 
              (equal? m 'passed))
            (make-bundle (list
                     (reverse (current_worlds univ))
                     'passed
                     (current_board univ) (current_color univ) (current_killed univ))
                    (list (make-mail (world1 univ) (list 'wait (current_board univ) (current_killed univ)  'passed))
                          (make-mail (world2 univ) (list 'play (current_board univ) (current_killed univ)  'passed)))
                    '())]
    ;;Haben beide Spieler hintereinander gepasst -> Auswertung
    [(and (equal? (current_state univ) 'passed) 
              (equal? m 'passed))
     ;;ToDO Auswertung
     (make-bundle (list
                     (current_worlds univ)
                     'result
                     (current_board univ) (current_color univ) (current_killed univ))
                    (list (make-mail (world1 univ) (list 'result (current_board univ) (current_killed univ)  'passed))
                          (make-mail (world2 univ) (list 'result (current_board univ) (current_killed univ)  'passed)))
                    '())]
    ;;Sonstige Anfragen verändern das Universum nicht
    [else (make-bundle univ '() '())]))


;;Erschafft ein Universum
(universe UNIVERSE0
          (on-new add-world)
          (on-msg handle-messages)
          (state #f))
 