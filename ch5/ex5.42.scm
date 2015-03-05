(load "ex5.41.scm")

(define (compile-assignment exp target linkage comp-env)
  (let* ((var (assignment-variable exp))
	 (get-value-code
	  (compile (assignment-value exp) 'val 'next))
	 (address (find-variable var comp-env)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence 
       '(env val) 
       (list target)
       (if (eq? address 'not-found)
	   `((perform (op set-variable-value!)
		      (const ,var)
		      (reg val)
		      (reg env))
	     (assign ,target (const ok)))
	   `((perform (op lexical-address-set!)
		      (const ,address)
		      (reg env)
		      (reg val)))))))))

(define (compile-variable exp target linkage comp-env)
  (let ((address (find-variable exp comp-env))) 
    (end-with-linkage linkage
		      (make-instruction-sequence 
		       '(env) (list target)
		       (if (eq? address 'not-found)
			   `((assign ,target
				     (op lookup-variable-value)
				     (const ,exp)
				     (reg env)))
			   `((assign ,target
				     (op lexical-address-lookup)
				     (const ,address)
				     (reg env))))))))
