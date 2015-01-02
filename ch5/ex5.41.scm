(load "ex5.40.scm")

(define (find-variable var comp-env)
  (define (find-in-frame frame pos)
    (if (null? frame)
	-1
	(if (eq? (car frame) var)
	    pos
	    (find-in-frame (cdr frame) (+ pos 1)))))
  (define (find-variable-iter frame env)
    (if (null? env)
	'not-found
	(let ((pos (find-in-frame (car env) 0)))
	  (if (= pos -1)
	      (find-variable-iter (+ frame 1) (cdr env))
	      (list frame pos)))))
  (find-variable-iter 0 comp-env))
