(define reverse-database
'(
  (rule (append-to-form () ?y ?y))

  (rule (append-to-form (?u . ?v) ?y (?u . ?z))
	(append-to-form ?v ?y ?z))
  
  ;; use queries of form (reverse ?x (a b c)), enters infinite loop if arguments are switched
  (rule (reverse () ()))
  (rule (reverse ?a ?b)
	(and (append-to-form (?fst) ?rest ?a)
	     (append-to-form ?brest (?bfst) ?b)
	     (reverse ?rest ?brest)))
))

(initialize-data-base reverse-database)
