(load "ch5-regsim.scm")

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
		(if (symbol? e)
		    (error "Operation applied to a symbol! -- MAKE_OPERATION_EXP" e)
		    (make-primitive-exp e machine labels)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))
