(load "accumulate.scm")

(define (count-leaves t)
  (accumulate (lambda (x y) (+ (if (pair? x) (count-leaves x) 1) y))
	      0 
	      (map (lambda (x) (if (pair? x) x 1)) t)))
