(load "ch5-regsim.scm")

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
	(inst-list '())
	(goto-regs '())
	(stack-regs '())
	(source-regs '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (set-aux-info new-inst-list new-goto-regs new-stack-regs new-source-regs)
	(set! inst-list new-inst-list)
	(set! goto-regs new-goto-regs)
	(set! stack-regs new-stack-regs)
	(set! source-regs new-source-regs))
      (define (print-instruction-list)
	(display inst-list)
	(newline))
      (define (print-entry-points)
	(display goto-regs)
	(newline))
      (define (print-stack-registers)
	(display stack-regs)
	(newline))
      (define (print-source-registers)
	(display source-regs)
	(newline))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
	      ((eq? message 'set-aux-info) set-aux-info)
	      ((eq? message 'print-instruction-list) (print-instruction-list))
	      ((eq? message 'print-entry-points) (print-entry-points))
	      ((eq? message 'print-stack-registers) (print-stack-registers))
	      ((eq? message 'print-source-registers) (print-source-registers))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (update-insts! insts labels inst-list goto-regs stack-regs source-regs machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc! 
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)
    ((machine 'set-aux-info) inst-list goto-regs stack-regs source-regs)))
    

(define (assemble controller-text machine)
  (extract-information machine controller-text
    (lambda (insts labels inst-list goto-regs stack-regs source-regs all-regs)
      (update-insts! insts labels inst-list goto-regs stack-regs source-regs machine)
      insts)))

(define (extract-information machine text receive)
  (if (null? text)
      (receive '() '() '() '() '() '() (list 'flag 'pc))
      (extract-information 
       machine
       (cdr text)
       (lambda (insts labels inst-list goto-regs stack-regs source-regs all-regs)
	 (let ((next-inst (car text)))
	   (cond ((symbol? next-inst)
		  (receive insts
		           (cons (make-label-entry next-inst
						   insts)
				 labels)
		           inst-list goto-regs stack-regs source-regs all-regs))
		 ((eq? (car next-inst) 'assign)
		  (receive (cons (make-instruction next-inst)
				 insts)
		           labels
		           (add-entry inst-list 'assign next-inst)
			   goto-regs
			   stack-regs
			   (add-entry source-regs (cadr next-inst) (cddr next-inst))
			   (add-registers all-regs machine next-inst)))
		 ((eq? (car next-inst) 'perform)
		  (receive (cons (make-instruction next-inst)
				 insts)
		           labels
		           (add-entry inst-list 'perform next-inst)
			   goto-regs
			   stack-regs
			   source-regs
			   all-regs))
		 ((eq? (car next-inst) 'test)
		  (receive (cons (make-instruction next-inst)
				 insts)
		           labels
		           (add-entry inst-list 'test next-inst)
			   goto-regs
			   stack-regs
			   source-regs
			   all-regs))
		 ((eq? (car next-inst) 'branch)
		  (receive (cons (make-instruction next-inst)
				 insts)
		           labels
		           (add-entry inst-list 'branch next-inst)
			   goto-regs
			   stack-regs
			   source-regs
			   all-regs))
		 ((eq? (car next-inst) 'goto)
		  (receive (cons (make-instruction next-inst)
				 insts)
		           labels
		           (add-entry inst-list 'goto next-inst)
			   (if (eq? (caadr next-inst) 'reg)
			       (adjoin-set (cadadr next-inst) goto-regs)
			       goto-regs)
			   stack-regs
			   source-regs
			   all-regs))
		 ((eq? (car next-inst) 'save)
		  (receive (cons (make-instruction next-inst)
				 insts)
		           labels
		           (add-entry inst-list 'save next-inst)
			   goto-regs
			   (adjoin-set (cadr next-inst) stack-regs)
			   source-regs
			   all-regs))
		 ((eq? (car next-inst) 'restore)
		  (receive (cons (make-instruction next-inst)
				 insts)
		           labels
		           (add-entry inst-list 'restore next-inst)
			   goto-regs
			   (adjoin-set (cadr next-inst) stack-regs)
			   source-regs
			   all-regs))
		 (else (error "invalid-instruction -- ASSEMBLE" next-inst))))))))

(define (add-registers regs machine inst)
  (display inst)
  (newline)
  (display regs)
  (newline)
  (map (lambda (subinst)
	 (cond ((and (pair? subinst) 
		     (eq? (car subinst) 'reg)
		     (not (element-of-set? (cadr subinst) regs)))
		(begin ((machine 'allocate-register) (cadr subinst))
		       (set! regs (cons (cadr subinst) regs))))
	       ((and (symbol? subinst) (not (element-of-set? subinst regs)))
		(begin ((machine 'allocate-register) subinst)
		       (set! regs (cons subinst regs))))
	       (else '())))
       (cdr inst))
  regs)
	 


(define (add-entry l tag entry)
  (if (null? l)
      (list (list tag (list entry)))
      (if (eq? (caar l) tag)
	  (cons (list tag (adjoin-set entry 
				      (cadar l)))
		(cdr l))
	  (cons (car l)
		(add-entry (cdr l)
			   tag
			   entry)))))


;; (inefficient) set implementation
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))


#|
USAGE EXAMPLE 

(define expt-rec-machine
  (make-machine
   (list (list '* *) (list '= =) (list '- -))
   '( (assign continue (label expt-done))
     test-b
      (test (op =) (reg n) (const 0))
      (branch (label base-case))
      (save continue)
      (save n)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-expt))
      (goto (label test-b))
     after-expt
      (restore n)
      (restore continue)
      (assign val (op *) (reg b) (reg val))
      (goto (reg continue))
     base-case
      (assign val (const 1))
      (goto (reg continue))
    expt-done)))

(set-register-contents! expt-rec-machine 'b 2)
(set-register-contents! expt-rec-machine 'n 9)
(start expt-rec-machine)
(display (get-register-contents expt-rec-machine 'val))
(newline)

|#
