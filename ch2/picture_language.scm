; picture language

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

;returns a function that takes a painter and a number.
;the function splits the painter so that n painters are
;placed op1 of the main painter op2 to each other
(define (split op1 op2)
  (lambda (painter n)
    (if (= n 0)
	painter
	(let ((smaller ((split op1 op2) painter (- n 1))))
	  (op1 painter (op2 smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))

;;; vector representation (ex 2.46)
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v w)
  (cons (+ (xcor-vect v) (xcor-vect w))
	(+ (ycor-vect v) (ycor-vect w))))
(define (sub-vect v w)
  (cons (- (xcor-vect v) (xcor-vect w))
	(- (ycor-vect v) (ycor-vect w))))
(define (scale-vect v s)
  (cons (* s (xcor-vect v))
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
(define (edge2-frame frame)
  (cdr (cdr (car frame))))
(define (device frame)
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
  (graphics-draw-line device (xcor-vect v) (xcor-vect w) (ycor-vect v) (ycor-vect w)))

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
			
