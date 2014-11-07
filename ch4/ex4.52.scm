(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
	((if-fail? exp) (analyze-if-fail exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (if-fail-success exp) (cadr exp))
(define (if-fail-failure exp) (caddr exp))

(define (analyze-if-fail exp)
  (let ((success (analyze (if-fail-success exp)))
        (failure (analyze (if-fail-failure exp))))
    (lambda (env succeed fail)
      (success env
	       (lambda (value fail2)
		 (if (true? value)
		     value
		     (fail)))
	       (lambda () 
		 (failure env succeed fail))))))

#|
Example from book, load into amb evaluator:
(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (require p)
  (if (not p) (amb)))

(define (even? n)
  (if (= n 0)
      true
      (odd? (- n 1))))
(define (odd? n)
  (if (= n 0)
      false
      (even? (- n 1))))

(if-fail (let ((x (an-element-of '(1 3 5))))
	   (require (even? x))
	   x)
	 'all-odd)

(if-fail (let ((x (an-element-of '(1 3 5 8))))
	   (require (even? x))
	   x)
	 'all-odd)
|#
