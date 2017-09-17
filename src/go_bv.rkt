#lang racket
(require vigracket)
(require (rename-in 2htdp/image
                    (save-image save-plt-image)
                    (image-width  plt-image-width)
                    (image-height plt-image-height)))

(provide bord-state)

;Directories
(define src_dir  (current-directory))
(define base_dir (build-path src_dir 'up))
(define img_dir  (build-path base_dir "images"))
(define res_dir  (build-path base_dir "results"))

;;Lädt das Bild aus dem Pfad.
;;
;;Parameter
;;rel-path: Der Dateipfad des Bildes.
;;
;;Return: Das geladene Bild.
(define (load-image-rel rel-path) ;rel-path: filename in "images" folder
    (load-image  (build-path img_dir rel-path)))
  
;;Speichert das Bild im results-Ordner
;;
;;Parameter
;;img: Das geladene Bild.
;;rel-path: Der Speicherpfad des Bildes.
(define (save-image-rel img rel-path) ;rel-path: filename in “images" folder
    (save-image img (build-path res_dir rel-path)))

;;Speichert einen Racket-Ausdruch in den "results" Ordner
(define (save-racket expr rel-path)
  (let* ([out (open-output-file (build-path res_dir rel-path) #:exists 'replace)])
    (begin
      (pretty-print expr out)
      (close-output-port out))))

;;Lädt einen Racket-Ausdruch aus dem "results" Ordner
(define (load-racket rel-path)
  (let* ([in (open-input-file (build-path res_dir rel-path))]
         [res (read in)])
    (begin
      (close-input-port in)
      (cdr res) ;;To remove the leading quote
      )))

;Zeige Originalbild
(define present-image (lambda (img name)
                        (save-image-rel img name)
                        (show-image img name)))

;Laden des Bildes aus dem der Spielstand ausgelesen werden soll
(define img (load-image-rel "IMG_0986.jpg"))

;Bild in Schwarz/Weiß
(define img_gray (list (car img)))

;Das Canny-Image
(define canny_img (cannyedgeimage img 1.0 6.0 255.0))

;; BBox: 0 - left, 1 - upper, 2 - right, 3 - lower
(define (findBBox x y pixel bbox)
  (when (>  (car pixel) 0.0)
    (begin
      (when (> x (vector-ref bbox 2))    (vector-set! bbox 2 x))
      (when (< x (vector-ref bbox 0))    (vector-set! bbox 0 x))
      (when (> y (vector-ref bbox 3))    (vector-set! bbox 3 y))
      (when (< y (vector-ref bbox 1))    (vector-set! bbox 1 y)))))

;;Zeichnet die Canny-Box
;;
;;Parameter
;;img: Das Bild
;;bboxes: Die Koordinaten der Box.
;;colors. Die Farbe der Box.
;;
;;Return: Bild mit gezeichneter Box.
(define (overlay-bboxes img bboxes colors)
  (if (empty? bboxes)
       (image->bitmap img)
       (let ((bbox  (car bboxes))
             (color (car colors)))
         (underlay/xy (overlay-bboxes img (cdr bboxes) (cdr colors))
                      (- (vector-ref bbox 0) 1)
                      (- (vector-ref bbox 1) 1)
                      (rectangle (+ (- (vector-ref bbox 2) (vector-ref bbox 0)) 2)
                                 (+ (- (vector-ref bbox 3) (vector-ref bbox 1)) 2)
                                 'outline color)))))

;Erstellen des Canny-Box
(define canny_bbox (vector (image-width img) (image-height img) 0 0))

;Box zum Bild hinzufügen
(void (image-for-each-pixel (curryr findBBox canny_bbox)  canny_img))

;Ausschneiden des Spielfeldes
(define canny_crop
  (subimage img
            (vector-ref canny_bbox 0)
            (vector-ref canny_bbox 1)
            (vector-ref canny_bbox 2)
            (vector-ref canny_bbox 3)))


;Spielstand auslesen
;Setzen des Smooth-Faktors
(define smoothFactor 2.5)

;"Verwischen" des Bildes, um den Spielstand besser auslesen zu könenn
(define smoothedImage (image->blue (gsmooth canny_crop smoothFactor)))

;;Berechnet den Offset in x-Richtung
;;
;;Parameter
;;pic Das Bild.
;;
;;Return: Der Offset in x-Richtung.
(define (stepsizeX pic)
  (value->pixel (+ 0 (/ (image-width pic) 19.5))))

;;Berechnet den Offset in y-Richtung
;;
;;Parameter
;;pic Das Bild.
;;
;;Return: Der Offset iny-Richtung.
(define (stepsizeY pic)
  (value->pixel ( + 0 (/ (image-height pic) 19.5))))

;;Iteriert über alle Listen in y-Richtung
;;
;;Parameter
;;start x/y: Startpositionen im Bild.
;;list: y-Listen mit den x-Koordinaten
;;count: Aktuelle y-Position
;;pic: Das bild.
;;
;;Return: Der Offset iny-Richtung.
(define (check-board-state startX startY list count pic)
  (if(= count 19)
     list
     (cons (check-x-coordiantes startX startY '() 0 pic)
           (check-board-state startX (+ startY (stepsizeY pic)) list (+ 1 count) pic))
  ))

;;Iteriert über alle Positionen in x-Richtung
;;
;;Parameter
;;start x/y: Startpositionen im Bild.
;;list: x-Listen mit den x-Koordinaten
;;count: Aktuelle y-Position
;;pic: Das bild.
;;
;;Return: Der Offset iny-Richtung.
(define (check-x-coordiantes startX startY list count pic)
  (if(= count 19)
  list
  (cons (checkpixel startX startY pic)
        (check-x-coordiantes (+ startX (stepsizeX pic)) startY list (+ count 1) pic))
  ))

;Einen Wert in einen Pixel-Wert konvertieren
(define (value->pixel value)
  (inexact->exact (round value))
  )

;Definition des Suchradius
(define search (value->pixel (/ (stepsizeX smoothedImage) 7)))

;Definition des Schwellenwertes für weiße Steine
(define border-white 148)

;Definition des Schwellenwertes für schwarze Steine
(define border-black 40)

;Überprüfen einer Position, quadratisch um den ermittelten Pixel.
;Prüfen, welche Farbe auf dem Stein liegt.
; 1: Weiß
;-1: Schwarz
; 0: Leer
;
;x  x  x
;x  x  x
;x  x  x
(define (checkpixel x y pic)
  (cond
   [(> (car (image-ref pic (value->pixel x) (value->pixel y))) border-white) 1]
   [(> (car (image-ref pic (value->pixel x) (- (value->pixel y) search))) border-white) 1]
   [(> (car (image-ref pic (value->pixel x) (+ search (value->pixel y)))) border-white) 1]
   [(> (car (image-ref pic (+ (value->pixel x) search) (value->pixel y))) border-white) 1]
   [(> (car (image-ref pic (- (value->pixel x) search) (value->pixel y))) border-white) 1]
   [(> (car (image-ref pic (- (value->pixel x) search) (- (value->pixel y) search))) border-white) 1]
   [(> (car (image-ref pic (- (value->pixel x) search) (+ (value->pixel y) search))) border-white) 1]
   [(> (car (image-ref pic (+ (value->pixel x) search) (- (value->pixel y) search))) border-white) 1]
   [(> (car (image-ref pic (+ (value->pixel x) search) (+ (value->pixel y) search))) border-white) 1]

   [(< (car (image-ref pic (value->pixel x) (value->pixel y))) border-black) -1]
   [(< (car (image-ref pic (value->pixel x) (- (value->pixel y) search))) border-black) -1]
   [(< (car (image-ref pic (value->pixel x) (+ search (value->pixel y)))) border-black) -1]
   [(< (car (image-ref pic (+ (value->pixel x) search) (value->pixel y))) border-black) -1]
   [(< (car (image-ref pic (- (value->pixel x) search) (value->pixel y))) border-black) -1]
   [(< (car (image-ref pic (- (value->pixel x) search) (- (value->pixel y) search))) border-black) -1]
   [(< (car (image-ref pic (- (value->pixel x) search) (+ (value->pixel y) search))) border-black) -1]
   [(< (car (image-ref pic (+ (value->pixel x) search) (- (value->pixel y) search))) border-black) -1]
   [(< (car (image-ref pic (+ (value->pixel x) search) (+ (value->pixel y) search))) border-black) -1]
   [else 0])
  )

;Ermitteln der Startposition des Spielfeldes
(define (start pic)
  (cons
  (value->pixel (* (stepsizeX pic) 0.75))
  (value->pixel (* (stepsizeY pic) 0.75))))

;Ermitteln des Spielstandes
(define bord-state (check-board-state (car (start smoothedImage)) (cdr (start smoothedImage)) '() 0 smoothedImage))
