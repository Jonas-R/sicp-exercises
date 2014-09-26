;; list-of-values with guaranteed order of evaluation

;; left-to-right
(define (list-of-values-l-to-r exps env)
  (if (no-operands? exps)
      '()
      (let* ((first-exp (eval (first-operand exps) env))
	     (rest-exps (list-of-values (rest-operands exps) env)))
	(cons first-exp rest-exps))))

;; right-to-left
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let* ((rest-exps (list-of-values (rest-operands exps) env))
	     (first-exp (eval (first-operand exps) env)))
	(cons first-exp rest-exps))))

;; Function to test evaluation-order
(define (f l val r) val)

;; yields "l-to-r" when evaluated left-to-right, "r-to-l" when evaluated right-to-left
;; (define x "")
;; (f (set! x "l-to-r") x (set! x "r-to-l"))
