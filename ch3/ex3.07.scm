;; load password-protected account
(load "ex3.03.scm")

;; does not signal an error if pwd is not the password to the original account
(define (make-joint acc pwd new-pwd)
  (define (dispatch pwd-attempt m)
    (if (eq? new-pwd pwd-attempt)
	(acc pwd m)
	(acc pwd-attempt m)))
  dispatch)
