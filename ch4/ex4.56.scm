;; a)
(and (supervisor ?x (bitdiddle ben))
     (address ?x ?address))

;; b)
(and (salary (bitdiddle ben) ?ben-salary)
     (salary ?person ?other-salary)
     (lisp-value > ?ben-salary ?other-salary))

;; c)
(and (supervisor ?bottom ?top)
     (not (job ?top (computer . ?rest)))
     (job ?top ?job))
