(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define (sign-change-detector v1 v2)
  (cond ((and (< 0 v1) (> 0 v2)) 1)
	((and (> 0 v1) (< 0 v2)) -1)
	(else 0)))

(define sense-data (cons-stream 1
		   (cons-stream 1.5
		   (cons-stream 1
		   (cons-stream 0.5
		   (cons-stream -0.1
		   (cons-stream -2
		   (cons-stream -0.5
		   (cons-stream 0.2 0)))))))))

(define zero-crossings (make-zero-crossings sense-data 0))

(define zero-crossings-alt
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))
