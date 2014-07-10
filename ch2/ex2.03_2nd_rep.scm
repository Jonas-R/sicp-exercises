(load "ex2.03.scm")
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
