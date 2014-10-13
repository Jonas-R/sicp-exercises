;; load let functionality
(load "ex4.06.scm")

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
	     (if (eq? '*unassigned* (car vals))
		 (error "Accessing an unassigned variable!" (car vars))
		 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (scan-out-defines body)
  (let* ((defs (filter definition? body))
	 (non-defs (filter (lambda (exp) (not (definition? exp)))
			   body))
	 (lets (map (lambda (def) (list (definition-variable def)
					'(quote *unassigned*)))
		    defs))
	 (sets (map (lambda (def) (list 'set! 
					(definition-variable def)
					(definition-value def)))
		    defs)))
    (if (null? defs)
	body
	(list (append (list 'let)
		      (list lets)
		      sets
		      non-defs)))))
		       
(define (make-procedure parameters body env)
  (list 'procedure 
	parameters 
	(scan-out-defines body) 
	env))
