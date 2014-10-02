;; eval with 'and' and 'or' as special forms

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
	((and? exp) (eval-and (operands exp) env))
	((or? exp) (eval-or (operands exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

(define (eval-and operands env)
  (if (null? operands)
      true
      (if (eval (car operands) env)
	  (eval-and (cdr operands) env)
	  false)))

(define (eval-or operands env)
  (if (null? operands)
      false
      (if (eval (car operands) env)
	  true
	  (eval-or (cdr operands) env))))
