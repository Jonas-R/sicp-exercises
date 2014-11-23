(load "ch5-regsim.scm")

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            ((eq? message 'name) name)
	    (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-name register) (register 'name))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (push stack (get-name reg))
      (advance-pc pc))))


(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
			    (stack-inst-reg-name inst))))
    (lambda ()
      (if (eq? (get-name reg) (pop stack))
	  (set-contents! reg (pop stack))
	  (error "Trying to restore stack to wrong register -- MAKE_RESTORE" (get-name reg)))
      (advance-pc pc))))
