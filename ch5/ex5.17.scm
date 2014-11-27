(load "ch5-regsim.scm")

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
	(instruction-count 0)
	(trace #f))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (initialize-instruction-count) (set! instruction-count 0) 'done)
      (define (print-instruction-count)
	(display instruction-count)
	(newline))
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
			  (begin (display (instruction-prec-label (car insts)))
				 (newline)))
		      (display (caar insts))
		      (newline)))
		(set! instruction-count (+ instruction-count 1))
                ((instruction-execution-proc (car insts)))
                (execute)))))
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
	      ((eq? message 'init-instruction-count) (initialize-instruction-count))
	      ((eq? message 'print-instruction-count) (print-instruction-count))
	      ((eq? message 'trace-on) (begin (set! trace #t) 'trace-on))
	      ((eq? message 'trace-off) (begin (set! trace #f) 'trace-off))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

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


(define fact-machine
  (make-machine
   '(n continue val)
   (list (list '- -) (list '* *) (list '= =))
  '(
   (assign continue (label fact-done))     ; set up final return address
fact-loop
   (test (op =) (reg n) (const 1))
   (branch (label base-case))
   ;; Set up for the recursive call by saving n and continue.
   ;; Set up continue so that the computation will continue
   ;; at after-fact when the subroutine returns.
   (save continue)
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-fact))
   (goto (label fact-loop))
 after-fact
   (restore n)
   (restore continue)
   (assign val (op *) (reg n) (reg val))   ; val now contains n(n-1)!
   (goto (reg continue))                   ; return to caller
 base-case
   (assign val (const 1))                  ; base case: 1!=1
   (goto (reg continue))                   ; return to caller
 fact-done
)))

(fact-machine 'trace-on)
(fact-machine 'init-instruction-count)
(set-register-contents! fact-machine 'n 4)
(set-register-contents! fact-machine 'val 1)
(start fact-machine)
(display (get-register-contents fact-machine 'val))
(newline)
(fact-machine 'print-instruction-count)
