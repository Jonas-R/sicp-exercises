;; alternative queue representation

(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr  '()))
    (define (set-front-ptr! item) 
      (set! front-ptr item)
      'done)
    (define (set-rear-ptr! item) 
      (set! rear-ptr item)
      'done)
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue) (if (empty-queue?)
			      (error "FRONT called with an empty queue")
			      (car front-ptr)))
    (define (insert-queue! item)
       (let ((new-pair (cons item '())))
	 (cond ((empty-queue?)
		(set-front-ptr! new-pair)
		(set-rear-ptr! new-pair)
		'done)
	       (else
		(set-cdr! rear-ptr new-pair)
		(set-rear-ptr! new-pair)
		'done))))
    (define (delete-queue!)
      (cond ((empty-queue?)
	     (error "DELETE! called with an empty queue" queue))
	    (else
	     (set-front-ptr! (cdr front-ptr))
	     'done)))
    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
	    ((eq? m 'rear-ptr) rear-ptr)
	    ((eq? m 'set-front-ptr!) set-front-ptr!)
	    ((eq? m 'set-rear-ptr!) set-rear-ptr!)
	    ((eq? m 'empty-queue?) (empty-queue?))
	    ((eq? m 'front-queue) (front-queue))
	    ((eq? m 'insert-queue!) insert-queue!)
	    ((eq? m 'delete-queue!) (delete-queue!))
	    (else (error "Undefined operation -- QUEUE" m))))
    dispatch))
