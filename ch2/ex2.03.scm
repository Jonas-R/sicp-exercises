;; import segments and points
(load "ex2.02.scm")

;; rectangle defined by two opposite corner points
(define (make-rect p1 p2) (cons p1 p2))
(define (width rect) 
  (abs (- (x-point (car rect))
	  (x-point (cdr rect)))))
(define (height rect)
  (abs (- (y-point (car rect))
	  (y-point (cdr rect)))))

;; rectangle defined by two perpendicular line segments
(define (make-rect s1 s2) (cons s1 s2))
(define (width rect)
  (seg-length (car rect)))
(define (height rect)
  (seg-length (cdr rect)))

(define (seg-length seg)
  (sqrt (+ 
	 (expt (- (x-point (end-segment seg)) (x-point (start-segment seg))) 2)
	 (expt (- (y-point (end-segment seg)) (y-point (start-segment seg))) 2))))

;; perimeter and area of rectangle
(define (perimeter rect)
  (+ (* 2 (width rect)) (* 2 (height rect))))
(define (area rect)
  (* (width rect) (height rect)))
