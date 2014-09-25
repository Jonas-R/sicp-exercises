(define (make-zero-crossings input-stream last-value) 
  (let ((smoothed-data (smooth input-stream)))
    (stream-map sign-change-detector smoothed-data (cons-stream last-value smoothed-data))))

;; returns a stream in which each element is the average of two elements in the input stream
(define (smooth s)
  (let ((avpt (/ (+ (stream-car s) (stream-car (stream-cdr s))) 2)))
    (cons-stream avpt
		 (smooth (stream-cdr s)))))
