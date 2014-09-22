(define (stream-limit s tolerance)
  (let ((fst (stream-car s))
	(snd (stream-car (stream-cdr s))))
    (if (< (abs (- fst snd)) tolerance)
	snd
	(stream-limit (stream-cdr s) tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average a b) (/ (+ a b) 2))
