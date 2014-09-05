;; make-from-mag-ang in message passing style
(define (make-from-mag-ang mag angle)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* mag (cos angle)))
	  ((eq? op 'imag-part) (* mag (sin angle)))
	  ((eq? op 'magnitude) mag)
	  ((eq? op 'angle) angle)
	  (else
	   (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)
