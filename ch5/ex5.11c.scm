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
      (push stack (get-contents reg) (get-name reg))
      (advance-pc pc))))


(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
			    (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack (get-name reg)))
      (advance-pc pc))))

(define (make-stack)
  (let ((s (make-strong-eq-hash-table)))
    (define (push x reg-name)
      (let ((tmp (hash-table/get s reg-name '())))
	(if (null? tmp)
	    (hash-table/put! s reg-name (list x))
	    (hash-table/put! s reg-name (cons x tmp)))))
    (define (pop reg-name)
      (let ((tmp (hash-table/get s reg-name #())))
	(if (null? tmp)
	    (error "Empty stack -- POP")
	    (let ((top (car tmp)))
            (hash-table/put! s reg-name (cdr tmp))
            top))))
    (define (initialize)
      (set! s (make-strong-eq-hash-table))
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) pop)
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK"
                         message))))
    dispatch))

(define (pop stack reg-name)
  ((stack 'pop) reg-name))

(define (push stack value reg-name)
  ((stack 'push) value reg-name))
