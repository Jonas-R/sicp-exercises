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

;;; vector representation
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
