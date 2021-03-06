; picture language

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (edge1-frame frame)
			   (xcor-vect v))
	       (scale-vect (edge2-frame frame)
			   (ycor-vect v))))))

;;; vector representation (ex 2.46)
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v w)
  (make-vect (+ (xcor-vect v) (xcor-vect w))
	     (+ (ycor-vect v) (ycor-vect w))))
(define (sub-vect v w)
  (make-vect (- (xcor-vect v) (xcor-vect w))
	     (- (ycor-vect v) (ycor-vect w))))
(define (scale-vect v s)
  (make-vect (* s (xcor-vect v))
	     (* s (ycor-vect v))))

;;; two representations of frames (ex 2.47)
(define (make-frame origin edge1 edge2 device)
  (list origin edge1 edge2 device))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (cadr (cdr frame)))
(define (device-frame frame)
  (cadr (cdr (cdr frame))))

(define (make-frame-alt origin edge1 edge2 device)
  (cons (cons origin (cons edge1 edge2)) device))
(define (origin-frame-alt frame)
  (car (car frame)))
(define (edge1-frame-alt frame)
  (cadr (car frame)))
(define (edge2-frame-alt frame)
  (cdr (cdr (car frame))))
(define (device-alt frame)
  (cdr frame))

;;; line segment representation (ex 2.48)
(define (make-segment v w)
  (cons v w))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	(device-frame frame)
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (draw-line device v w)
  (graphics-draw-line device (xcor-vect v) (ycor-vect v) (xcor-vect w) (ycor-vect w)))

;;; some painters (ex 2.49)
(define outline-painter 
  (segments->painter
   (list (make-segment
	  (make-vect 0.0 0.0)
	  (make-vect 1.0 0.0))
	 (make-segment
	  (make-vect 0.0 0.0)
	  (make-vect 0.0 1.0))
	 (make-segment
	  (make-vect 1.0 0.0)
	  (make-vect 1.0 1.0))
	 (make-segment
	  (make-vect 0.0 1.0)
	  (make-vect 1.0 1.0)))))

(define X-painter 
  (segments->painter
   (list (make-segment
	  (make-vect 1.0 0.0)
	  (make-vect 0.0 1.0))
	 (make-segment
	  (make-vect 0.0 0.0)
	  (make-vect 1.0 1.0)))))

(define diamond-painter
  (segments->painter
   (list (make-segment
	  (make-vect 0.5 0.0)
	  (make-vect 1.0 0.5))
	 (make-segment
	  (make-vect 1.0 0.5)
	  (make-vect 0.5 1.0))
	 (make-segment
	  (make-vect 0.0 0.5)
	  (make-vect 0.5 0.0))
	 (make-segment
	  (make-vect 0.0 0.5)
	  (make-vect 0.5 1.0)))))

(define wave-painter
  (segments->painter
   (list 
    ;head
    (make-segment
     (make-vect 0.4  1.0)
     (make-vect 0.35 0.9))
    (make-segment
     (make-vect 0.35 0.9)
     (make-vect 0.4  0.8))
    (make-segment
     (make-vect 0.6  1.0)
     (make-vect 0.65 0.9))
    (make-segment
     (make-vect 0.65 0.9)
     (make-vect 0.6  0.8))
    ;left arm
    (make-segment
     (make-vect 0.4 0.8)
     (make-vect 0.3 0.8))
    (make-segment
     (make-vect 0.3  0.8)
     (make-vect 0.2  0.75))
    (make-segment
     (make-vect 0.2  0.75)
     (make-vect 0.0  0.9))
    (make-segment
     (make-vect 0.0 0.8)
     (make-vect 0.2 0.65))
    (make-segment
     (make-vect 0.2 0.65)
     (make-vect 0.3 0.75))
    (make-segment
     (make-vect 0.3  0.75)
     (make-vect 0.35 0.70))
    ;left leg
    (make-segment
     (make-vect 0.35 0.7)
     (make-vect 0.3  0.0))
    (make-segment
     (make-vect 0.4 0.0)
     (make-vect 0.5 0.4))
    ;right leg
    (make-segment
     (make-vect 0.5 0.4)
     (make-vect 0.6 0.0))
    (make-segment
     (make-vect 0.65 0.7)
     (make-vect 0.7  0.0))
    ;right arm
    (make-segment
     (make-vect 0.6 0.8)
     (make-vect 0.7 0.8))
    (make-segment
     (make-vect 0.7 0.8)
     (make-vect 1.0 0.6))
    (make-segment
     (make-vect 0.65 0.7)
     (make-vect 1.0  0.5)))))
			
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Painter Transformations
;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter
	 (make-frame new-origin
		     (sub-vect (m corner1) new-origin)
		     (sub-vect (m corner2) new-origin)
		     (device-frame frame)))))))

;returns a function that takes a painter and a number.
;the function splits the painter so that n painters are
;placed op1 of the main painter op2 to each other
(define (split op1 op2)
  (lambda (painter n)
    (if (= n 0)
	painter
	(let ((smaller ((split op1 op2) painter (- n 1))))
	  (op1 painter (op2 smaller smaller))))))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      split-point
			      (make-vect 0.0 1.0)))
	  (paint-right
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.0)
			      (make-vect 0.5 1.0))))
      (lambda (frame)
	(paint-left frame)
	(paint-right frame)))))

; exercise 2.51
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-down
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      (make-vect 1.0 0.0)
			      split-point))
	  (paint-up
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.5)
			      (make-vect 0.0 1.0))))
      (lambda (frame)
	(paint-down frame)
	(paint-up frame)))))

(define (below-alt painter1 painter2)
    (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

(define right-split (split beside below))
(define up-split (split below beside))

(define (flip-vert painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (rotate90 painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
		     (make-vect 0.0 0.0)
		     (make-vect 0.65 0.35)
		     (make-vect 0.35 0.65)))

; exercise 2.50
(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))
(define (rotate180 painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 0.0)))
(define (rotate270 painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

#|
USAGE EXAMPLE:

(define device (make-graphics-device (car (enumerate-graphics-types))))
(graphics-set-coordinate-limits device 0 0 500 500)
(define frame (make-frame (make-vect 0 0)
			  (make-vect 500 0)
			  (make-vect 0 500)
			  device))
((beside X-painter (below X-painter diamond-painter)) frame)
(graphics-clear device)
((corner-split wave-painter 4) frame)
(graphics-clear device)
((square-limit wave-painter 4) frame)
(graphics-close device)
|#
