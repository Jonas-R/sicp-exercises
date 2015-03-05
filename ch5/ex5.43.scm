(load "ex5.42.scm")

(define (compile-lambda-body exp proc-entry comp-env)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (scan-out-defines (lambda-body exp)) 'val 'return (cons formals comp-env)))))

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
