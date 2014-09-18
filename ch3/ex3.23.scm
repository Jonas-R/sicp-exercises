;; deque (represented as doubly linked list)

(define (make-deque-elem data prev next) (list data prev next))
(define (get-data elem) (car elem))
(define (get-prev elem) (cadr elem))
(define (get-next elem) (caddr elem))
(define (set-prev! elem new-prev) (set-cdr! elem (cons new-prev (list (get-next elem)))))
(define (set-next! elem new-next) (set-cdr! (cdr elem) (list new-next)))

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque) (null? (front-ptr deque)))
(define (make-deque) (cons '() '()))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (get-data (front-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-elem (make-deque-elem item '() (front-ptr deque))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-elem)
           (set-rear-ptr! deque new-elem)
           (print-deque deque))
          (else
           (set-prev! (front-ptr deque) new-elem)
           (set-front-ptr! deque new-elem)
           (print-deque deque))))) 

(define (rear-insert-deque! deque item)
  (let ((new-elem (make-deque-elem item (rear-ptr deque) '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-elem)
           (set-rear-ptr! deque new-elem)
           (print-deque deque))
          (else
           (set-next! (rear-ptr deque) new-elem)
           (set-rear-ptr! deque new-elem)
           (print-deque deque))))) 

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (set-front-ptr! deque (get-next (front-ptr deque)))
         (print-deque deque)))) 

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (set-rear-ptr! deque (get-prev (rear-ptr deque)))
         (set-next! (rear-ptr deque) '())
	 (print-deque deque))))

(define (print-deque deque)
  (define (print-elems deque)
    (if (eq? '() deque)
	(display "")
	(begin (display (get-data deque))
	       (display " ")
	       (print-elems (get-next deque)))))
  (display "(")
  (print-elems (front-ptr deque))
  (display ")")
  (newline)
  '())
	       
