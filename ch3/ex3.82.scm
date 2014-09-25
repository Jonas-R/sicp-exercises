;; Monte Carlo integration

;; predicate to test if point (x,y) is in circle of radius r centered at (cx, cy)
(define (in-circle? x y r cx cy)
  (<= (+ (expt (- x cx) 2) (expt (- y cy) 2)) (expt r 2)))

(define (test-circle? x y)
  (in-circle? x y 3 5 7))
(define (test-unit-circle? x y)
  (in-circle? x y 1 0 0))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define experiment-stream
    (let ((xs (random-stream-in-range x1 x2))
	  (ys (random-stream-in-range y1 y2)))
      (stream-map P xs ys)))
  (define (area-of-rect x1 x2 y1 y2)
    (abs (* (- x2 x1) (- y2 y1))))
  (* (stream-ref (monte-carlo experiment-stream 0 0) trials) (area-of-rect x1 x2 y1 y2)))

(define (random-stream-in-range low high)
  (let ((range (- high low)))
    (cons-stream (+ low (random range)) (random-stream-in-range low high))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))


#|
USAGE EXAMPLE:
(estimate-integral test-unit-circle? -2.0 2.0 -2.0 2.0 1000000)
|#
