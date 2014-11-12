(define (uniquely-asserted query frame-stream)
    (stream-flatmap
     (lambda (frame)
       (let ((extensions (qeval (car query) (singleton-stream frame))))
	 (if (is-singleton-stream? extensions)
	     extensions
	     the-empty-stream)))
     frame-stream))


(define (is-singleton-stream? stream)
  (and (not (stream-null? stream)) (stream-null? (stream-cdr stream))))

(put 'unique 'qeval uniquely-asserted)
