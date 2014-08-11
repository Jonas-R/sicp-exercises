;equality test for lists

(define (equal? a b)
  (cond ((eq? '() a) (eq? '() b))
	((eq? '() b) (eq? '() a))
	((and (list? a) (list? b) (equal? (car a) (car b))) (equal? (cdr a) (cdr b)))
	((and (symbol? a) (symbol? b)) (eq? a b))
	(else #f)))
