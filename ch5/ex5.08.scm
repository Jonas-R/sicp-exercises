(load "ch5-regsim.scm")

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
	       (if (assoc next-inst labels)
		   (error "Label defined multiple times! -- EXTRACT-LABELS" next-inst)
		   (receive insts
		       (cons (make-label-entry next-inst
					       insts)
			     labels)))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))

#|
Machine for testing

(define test-machine
  (make-machine
   '(a)
   '()
   '( 
     start
       (goto (label here))
     here
       (assign a (const 3))
       (goto (label there))
     here 
       (assign a (const 4))
       (goto (label there))
     there)))
|#
