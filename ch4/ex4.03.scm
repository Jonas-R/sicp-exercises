;; function application with call

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((not (null? (get 'eval (operator exp))))
	 ((get 'eval (operator exp)) exp env))
	((application? exp) (apply (eval (operator exp) env)
				   (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (install-eval-package)
  ;; internal procedures
  (define (quoted exp env) (text-of-quotation exp))
  (define (assignment exp env) (eval-assignment exp env))
  (define (definition exp env) (eval-definition exp env))
  (define (if-disp exp env) (eval-if exp env))
  (define (lambda-disp exp env)
    (make-procedure (lambda-parameters exp)
		    (lambda-body exp)
		    env))
  (define (begin-disp exp env) 
    (eval-sequence (begin-actions exp) env))
  (define (cond-disp exp env) (eval (cond->if exp) env))
  ;; interface to rest of system
  (put 'eval 'quote quoted)
  (put 'eval 'set! assignment)
  (put 'eval 'define definition)
  (put 'eval 'if if-disp)  
  (put 'eval 'lambda lambda-disp)
  (put 'eval 'begin begin-disp)
  (put 'eval 'cond cond-disp)

  'done)
  
;; dispatch table implementation
(define dispatch-table (make-equal-hash-table))

(define (put op type-tag function)
  (hash-table/put! dispatch-table (list op type-tag) function))

(define (get op type-tag)
  (hash-table/get dispatch-table (list op type-tag) '()))

(install-eval-package)
