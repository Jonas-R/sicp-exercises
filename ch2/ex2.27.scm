(define (deep-reverse l)
  (cond ((null? l) '())
	((list? (car l)) (append (deep-reverse (cdr l)) (list (deep-reverse (car l)))))
	(#t (append (deep-reverse (cdr l)) (list (car l))))))
