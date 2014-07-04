;;modified fixed-point that displays guesses
(define tolerance 0.00001)
(define (average a b) (/ (+ a b) 2))

(define (fixed-point f first-guess dampen?)
  (newline)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display "trying ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (if (and dampen?)
	      (try (average guess next))
	      (try next)))))
  (try first-guess))

;;find solution to x^x = 1000 without damping
(fixed-point (lambda (x) (/ (log 1000) (log x)))
	     2
	     #f)

;; find solution with damping
(fixed-point (lambda (x) (/ (log 1000) (log x)))
	     2
	     #t)
