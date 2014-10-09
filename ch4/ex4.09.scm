;; very clunky implementations of for and while loops

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
	((for? exp) (eval-for exp env))
	((while? exp) (eval-while exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (for? exp) (tagged-list? exp 'for))
(define (while? exp) (tagged-list? exp 'while))

(define (for-variable exp) (cadr exp))
(define (for-list exp) (cadr (cadddr exp)))
(define (for-body exp) (car (cddddr exp)))

(define (eval-for exp env)
  (define (eval-for-iter var values body)
    (if (null? values)
	'ok
	(begin (eval (list 'define var (car values)) env)
	       (eval body env)
	       (eval-for-iter var (cdr values) body))))
  (eval-for-iter (for-variable exp)
		 (for-list exp)
		 (for-body exp)))

(define (while-cond exp) (cadr exp))
(define (while-body exp) (caddr exp))
(define (while-update exp) (cadddr exp))

(define (eval-while exp env)
  (define (eval-while-iter cond body update)
    (if (eval cond env)
	(begin (eval body env)
	       (eval update env)
	       (eval-while-iter cond body update))
	'ok))
  (eval-while-iter (while-cond exp) 
		   (while-body exp)
		   (while-update exp)))

#|
USAGE EXAMPLES:
(define y 0)
(for x in '(1 2 3 4 5) (define y (+ y x)))
(define x 0)
(define y 0)
(while (< x 6) (define y (+ y x)) (define x (+ x 1)))

|#
