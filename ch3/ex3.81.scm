(define random-init 4)	 ;; random value chosen by fair dice roll

(define (random-numbers message-stream)
  (define (apply-messages ms initial)
    (cond ((eq? (stream-car ms) 'generate) 
	   (let ((new-val (rand-update initial)))
		 (cons-stream new-val 
			      (apply-messages (stream-cdr ms) new-val))))
	  ((and (pair? (stream-car ms)) (eq? (car (stream-car ms)) 'reset))
	   (apply-messages (stream-cdr ms) (cdr (stream-car ms))))
	  (else (error "Not a valid message -- RANDOM_NUMBERS" (stream-car ms)))))
  (apply-messages message-stream random-init))
	   


;; taken from ch3support.scm available on sicp website
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

#|
EXAMPLE USAGE:

(define messages (cons-stream 'generate (cons-stream 'generate (cons-stream (cons 'reset 4)
		 (cons-stream 'generate (cons-stream 'generate (cons-stream (cons 'reset 4)
		 (cons-stream 'generate (cons-stream 'generate (cons-stream 'null 'null))))))))))
(display-stream (random-numbers messages) 5)


|#
