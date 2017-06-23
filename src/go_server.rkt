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
    ;;TODO      4. Ob Zug gültig ist 
    [(and (list? m) (= (length m) 2) (equal? (first m) 'set)      ;; 1.
          (iworld=? wrld (world1 univ))                           ;; 2.
          (equal? (list-ref (current_board univ) (second m)) "")) ;; 3.
     (let* ([new_board (append (take (current_board univ) (second m))
                               (list (iworld-name wrld))
                               (take-right (current_board univ) (- 8 (second m))))])
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
             
    
;;Erschafft ein Universum
(universe UNIVERSE0
          (on-new add-world)
          (on-msg handle-messages))
 