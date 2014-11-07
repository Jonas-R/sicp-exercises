(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
	((permanent-assignment? exp) (analyze-permaassignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (permanent-assignment? exp) (tagged-list? exp 'permanent-set!))
(define (permaassignment-variable exp) (cadr exp))
(define (permaassignment-value exp) (caddr exp))

(define (analyze-permaassignment exp)
  (let ((var (permaassignment-variable exp))
        (vproc (analyze (permaassignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        
                 (set-variable-value! var val env)
                 (succeed 'ok fail2))
             fail))))

#|
Example from book, copy into amb evaluator:

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (require p)
  (if (not p) (amb)))

(define count 0)

(let ((x (an-element-of '(a b c)))
      (y (an-element-of '(a b c))))
  (permanent-set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))

|#
