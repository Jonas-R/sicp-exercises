(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
	((unbind? exp) (eval-unbind exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (unbind? exp) (tagged-list? exp 'make-unbound!))
(define (unbind-variable exp) (cadr exp))

(define (eval-unbind exp env)
  (unbind-variable! (unbind-variable exp)  env)
  'ok)

(define (unbind-variable! var env)
  (define (env-loop env)
    (define (scan vars vals prev-vals prev-vars frame)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
	     (if (null? prev-vals)
		 (begin (display (cdr vals))
			(newline)
			(set! vals (cdr vals))
			(set! vars (cdr vars))
			(set! frame (make-frame vars vals))
			'case1)
		 (begin (display prev-vals)
			(newline)
			(display prev-vars)
			(newline)
			(set-cdr! prev-vals (cdr vals))
			(set-cdr! prev-vars (cdr vars))
			(set! vals prev-vals)
			(set! vars prev-vars)
			(set! frame (make-frame vars vals))
			'case2)))
            (else (scan (cdr vars) 
			(cdr vals) 
			(append prev-vals (list (car vals))) 
			(append prev-vars (list (car vars)))))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- UNBIND!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)
		'()
		'()
		frame))))
  (env-loop env))
