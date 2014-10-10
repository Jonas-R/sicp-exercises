(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let* ((frame (first-frame env))
	        (lookup (lookup-variable var
					(frame-variables frame)
					(frame-values frame))))
          (if lookup
	      (car (cdr lookup))
	      (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let* ((frame (first-frame env))
	       (lookup (lookup-variable var
					(frame-variables frame)
					(frame-values frame))))
	  (if lookup
	      (begin (set-car! (cdr lookup) val)
		     'ok)
	      (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (define-variable! var val env)
  (let* ((frame (first-frame env))
	 (lookup (lookup-variable var 
				  (frame-variables frame)
				  (frame-values frame))))
    (if lookup
	(begin (set-car! (cdr lookup) val)
	       'ok)
	(add-binding-to-frame! var val frame))))

(define (lookup-variable var vars vals)
  (cond ((null? vars)
	 false)
	((eq? var (car vars))
	 (cons vars vals))
	(else (lookup-variable var (cdr vars) (cdr vals)))))
