;; resetable random number generator
;; does not work because mit-scheme does not seem to let you set the random seed

(define (rand command arg)
  (cond ((eq? command 'generate) (random arg))
	((eq? command 'reset) (set! *random-state* (make-random-state arg)))
	(else (error "Unknown command -- " command))))
