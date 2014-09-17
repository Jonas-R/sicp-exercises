;; determine whether a list has a loop using a constant amount of space
;; implements Floyd's cycle-finding algorithm 
;; cf https://en.wikipedia.org/wiki/Cycle_detection#Tortoise_and_hare

(define (has-loop? x)
  (define (has-loop-helper tortoise hare)
    (cond ((eq? tortoise hare) #t)
	  ((or (null? hare) (null? (cdr hare))) #f)
	  (else (has-loop-helper (cdr tortoise) (cddr hare)))))

  (if (not (pair? x)) 
      #f
      (has-loop-helper x (cdr x))))
