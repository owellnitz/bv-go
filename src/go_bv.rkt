#lang racket
(require vigracket)
(require (rename-in 2htdp/image
                    (save-image save-plt-image)
                    (image-width  plt-image-width)
                    (image-height plt-image-height)))


(define src_dir  (current-directory))
(define base_dir (build-path src_dir 'up))
(define img_dir  (build-path base_dir "images"))
(define res_dir  (build-path base_dir "results"))

;;Lädt ein Bild aus dem "images" Ordner
(define (load-image-rel rel-path) ;rel-path: filename in "images" folder
    (load-image  (build-path img_dir rel-path)))
  
;;Speichert ein Bild in den "results" Ordner
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

;(define present-image void)
(define present-image (lambda (img name)
                        (save-image-rel img name)
                        (show-image img name)))

(define img (load-image-rel "IMG_1006.jpg"))
(define img_gray (list (car img)))
(present-image img_gray "image_gray.png")


(present-image (cannyedgeimage img_gray 1.0 6.0 255.0) "go-canny.png")

(define canny_img (cannyedgeimage img 1.0 6.0 255.0))

;; BBox: 0 - left, 1 - upper, 2 - right, 3 - lower
(define (findBBox x y pixel bbox)
  (when (>  (car pixel) 0.0)
    (begin
      (when (> x (vector-ref bbox 2))    (vector-set! bbox 2 x))
      (when (< x (vector-ref bbox 0))    (vector-set! bbox 0 x))
      (when (> y (vector-ref bbox 3))    (vector-set! bbox 3 y))
      (when (< y (vector-ref bbox 1))    (vector-set! bbox 1 y)))))

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

(define canny_bbox (vector (image-width img) (image-height img) 0 0))

(void (image-for-each-pixel (curryr findBBox canny_bbox)  canny_img))

(present-image (plt-image->image (overlay-bboxes img (list canny_bbox) '(green))) "go-canny-bbox.png")

(define canny_crop (subimage img  (vector-ref canny_bbox 0) (vector-ref canny_bbox 1) (vector-ref canny_bbox 2) (vector-ref canny_bbox 3)))
                  
(present-image canny_crop  "go-canny-crop.png")

;Spielstand auslesen

(define stoneradiusthrough3 2.5)

(define smoothedImage (image->blue (gsmooth canny_crop stoneradiusthrough3)))

(present-image smoothedImage "go-smoothed.png")

(define (stepsizeX pic)
  (cons
  ( + 1 (value->pixel (/ (image-width pic) 20)))
    0))

(define (stepsizeY pic)
  (cons
   0
  (+ 1 (value->pixel (/ (image-height pic) 20)))))

(define empty_board_state (make-list 19 (make-list 19 'empty)))

(define (check-board-state startX startY list count pic)
  (if(= count 19)
     list
     (cons (check-x-coordiantes startX startY '() 0 pic)
           (check-board-state startX (+ startY (cdr (stepsizeY pic))) list (+ 1 count) pic))
  ))

(define (check-x-coordiantes startX startY list count pic)
  (if(= count 19)
  list
  (cons (checkpixel (+ startX (* count (car (stepsizeX pic)))) (+ startY (* count (cdr (stepsizeX pic)))) pic)
        (check-x-coordiantes startX startY list (+ count 1) pic))
  ))
(define (value->pixel value)
  (inexact->exact (round value))
  )
(define search (value->pixel (/ (car (stepsizeX smoothedImage)) 6)))
(define border-white 148)
(define border-black 40)

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


