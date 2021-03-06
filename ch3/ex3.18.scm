;; determine whether a list has a loop
(define (has-loop? x)
  (define (has-loop-helper x)
    (cond ((not (pair? x)) #f)
	  ((memq x seen) #t)
	  (else (begin (set! seen (append! seen (list x)))
		       (or (has-loop-helper (car x)) 
			   (has-loop-helper (cdr x))))))) 
  (define seen '())
  (has-loop-helper x))
