;; load the lazy evaluator before using this

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters-no-tags procedure)
           (list-of-delayed-args (procedure-parameters procedure) arguments env)
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

;; returns just the parameter names without lazy tag
(define (procedure-parameters-no-tags p) 
  (map (lambda (x) (if (pair? x) (car x) x)) 
       (procedure-parameters p)))

(define (list-of-delayed-args arg-defs arguments env)
  (if (no-operands? arguments)
      '()
      (cons (delay-it (car arg-defs) (first-operand arguments) env)
            (list-of-delayed-args (cdr arg-defs) (rest-operands arguments)
                                  env))))

;; thunks
(define (lazy? op) (eq? (cadr op) 'lazy))
(define (lazy-memo? op) (eq? (cadr op) 'lazy-memo))

(define (delay-it def exp env)
  (cond ((not (pair? def)) (eval exp env))
	((lazy? def) (list 'thunk exp env))
	((lazy-memo? def) (list 'memo-thunk exp env))
	(else (eval exp env))))

(define (thunk? obj)
  (tagged-list? obj 'thunk))
(define (memo-thunk? obj)
  (tagged-list? obj 'memo-thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

;; "thunk" that has been forced and is storing its (memoized) value
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))


(define (force-it obj)
  (cond ((memo-thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
	((thunk? obj)
	 (actual-value (thunk-exp obj) (thunk-env obj)))
        (else obj)))
