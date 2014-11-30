(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
	(breakpoints (make-strong-eq-hash-table)))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
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
		(if trace
		    (begin 
		      (if (not (eq? 'dummy (instruction-prec-label (car insts))))
			  ()
		      (display (caar insts))
		      (newline)))
		(set! instruction-count (+ instruction-count 1))
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (set-breakpoint label n)
	;; TODO
	)
      (define (cancel-breakpoint label n)
	;; TODO
	)
      (define (cancel-all-breakpoint)
	;; TODO
	)
      
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
	      ((eq? message 'set-breakpoint) set-breakpoint)
	      ((eq? message 'proceed) (execute))
	      ((eq? message 'cancel-breakpoint) cancel-breakpoint)
	      ((eq? message 'cancel-all-breakpoints) (cancel-all-breakpoints))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))

(define (proceed-machine machine)
  (machine 'proceed))

(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n))

(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))

(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels prec-instr)
      (update-insts! insts labels machine)
      insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '() '())
      (extract-labels (cdr text)
       (lambda (insts labels prec-instr)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (begin 
		 (if (not (null? prec-instr))
		     (instruction-set-prec-label! prec-instr next-inst))
		 (receive insts
		     (cons (make-label-entry next-inst
					     insts)
			   labels)
		   '()))
               (let ((new-inst (make-instruction next-inst 'dummy)))
		 (receive 
		     (cons new-inst insts)
		     labels
		     new-inst))))))))

(define (update-insts! insts labels machine)
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
     insts)))

(define (make-instruction text preceding-label)
  (cons text (cons preceding-label '())))

(define (instruction-text inst)
  (car inst))

(define (instruction-prec-label inst)
  (cadr inst))

(define (instruction-set-prec-label! inst preceding-label)
  (set-cdr! inst (cons preceding-label 
		       (instruction-execution-proc inst))))

(define (instruction-execution-proc inst)
  (cddr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! (cdr inst) proc))

