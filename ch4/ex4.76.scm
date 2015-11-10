(load "ch4-query.scm")

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (merge (qeval (first-conjunct conjuncts)
		    frame-stream)
	     (conjoin (rest-conjuncts conjuncts)
			  frame-stream))))
(define (merge s1 s2) 
   (cond ((stream-null? s1) s2) 
         ((stream-null? s2) s1) 
         (else 
          (stream-flatmap (lambda (frame1) 
                            (stream-filter
			     (lambda (f) (not (eq? f 'failed)))
			     (stream-map (lambda (frame2) 
					   (merge-frames frame1 frame2)) 
					 s2)))
			    s1))))

(define (merge-frames f1 f2)
  (if (null? f1)
    f2
    (let* ((var (caar f1))
          (val (cdar f1))
	  (extension (extend-if-possible var val f2)))
        (if (eq? extension 'failed)
          'failed
          (merge-frames (cdr f1) extension)))))
