;; Monte Carlo integration

;; predicate to test if point (x,y) is in circle of radius r centered at (cx, cy)
(define (in-circle? x y r cx cy)
  (<= (+ (expt (- x cx) 2) (expt (- y cy) 2)) (expt r 2)))

(define (test-circle? x y)
  (in-circle? x y 3 5 7))
(define (test-unit-circle? x y)
  (in-circle? x y 1 0 0))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
	  (y (random-in-range y1 y2)))
      (P x y)))
  (define (area-of-rect x1 x2 y1 y2)
    (abs (* (- x2 x1) (- y2 y1))))
  (* (monte-carlo  trials experiment) (area-of-rect x1 x2 y1 y2)))


(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))



#|
USAGE EXAMPLE:
(estimate-integral test-unit-circle? -2.0 2.0 -2.0 2.0 1000000)
|#

