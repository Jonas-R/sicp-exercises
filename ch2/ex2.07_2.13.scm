;; intervals

(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

;; ex 2.12
(define (make-center-percent center tolerance)
  (cons (- center (* center tolerance)) (+ center (* center tolerance))))

(define (center interval)
  (/ (+ (lower-bound interval) (upper-bound interval)) 2))

(define (percent interval)
  (/ (- (upper-bound interval) (center interval)) (center interval)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

(define (width i)
  (/ (abs (- (upper-bound i) (lower-bound i))) 2)) 

;; exercise 2.9:
;    The width of the sum of two intervals is the sum of their widths. (same for subtraction)
;    The width of the product of two intervals on the other hand 
;    also depends on the actual bounds of the intervals. (same for division)

(define (mul-interval x y)
  (cond ((and (>= (upper-bound x) 0) (>= (upper-bound y) 0)
	      (>= (lower-bound x) 0) (>= (lower-bound y) 0))
	 (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
	
	((and (>= (upper-bound x) 0) (>= (upper-bound y) 0)
	      (<  (lower-bound x) 0) (>= (lower-bound y) 0))
	 (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (upper-bound y))))
	
	((and (>= (upper-bound x) 0) (>= (upper-bound y) 0)
	      (>= (lower-bound x) 0) (<  (lower-bound y) 0))
	 (make-interval (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
	
	((and (< (upper-bound x) 0) (>= (upper-bound y) 0)
	      (< (lower-bound x) 0) (>= (lower-bound y) 0))
	 (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))))
	
	((and (>= (upper-bound x) 0) (< (upper-bound y) 0)
	      (>= (lower-bound x) 0) (< (lower-bound y) 0))
	 (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y))))
	
	((and (>= (upper-bound x) 0) (>= (upper-bound y) 0)
	      (<  (lower-bound x) 0) (<  (lower-bound y) 0))
	 (let ((p1 (* (lower-bound x) (lower-bound y)))
	       (p2 (* (lower-bound x) (upper-bound y)))
	       (p3 (* (upper-bound x) (lower-bound y)))
	       (p4 (* (upper-bound x) (upper-bound y))))
	   (make-interval (min p1 p2 p3 p4)
			  (max p1 p2 p3 p4))))
	
	((and (< (upper-bound x) 0) (>= (upper-bound y) 0)
	      (< (lower-bound x) 0) (<  (lower-bound y) 0))
	 (make-interval (* (lower-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
	
	((and (>= (upper-bound x) 0) (< (upper-bound y) 0)
	      (<  (lower-bound x) 0) (< (lower-bound y) 0))
	 (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (lower-bound y))))
	
	((and (< (upper-bound x) 0) (< (upper-bound y) 0)
	      (< (lower-bound x) 0) (< (lower-bound y) 0))
	 (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
	
	((#t) (error "not a valid interval!"))))
	 

(define (div-interval x y)
  (if (= (width y) 0)
      (error "division by interval with 0 width!")
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))
  
