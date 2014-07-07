;; function to compute the element in a certain row and column of the pascal triangle
;; returns 1 for elements outside the triangle

(define (pascal-triangle row column)
  (cond ((or (< column 1) (< row 1) (= row column)) 1)
	(else (+ (pascal-triangle (- row 1) (- column 1)) (pascal-triangle (- row 1) column))))) 
