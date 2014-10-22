;; load let expressions
(load "ex4.16.scm")

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
	((letrec? exp) (eval (letrec->let exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))


(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-definitions exp) (cadr exp))
(define (letrec-body exp) (caddr exp))


(define (letrec->let exp)
  (let ((defs (letrec-definitions exp))
	(body (letrec-body exp)))
    (list 'let
	  (map (lambda (def) (list (car def) '*unassigned*))
	       defs)
	  (append (list 'begin)
		  (map (lambda (def) (list 'set! (car def) (cadr def)))
		       defs)
		  body))))
		

  
