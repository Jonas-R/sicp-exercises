;; run (initialize-data-base bible-database) in regular scheme to initialize

(define bible-database 
  '((son Adam Cain)
    (son Cain Enoch)
    (son Enoch Irad)
    (son Irad Mehujael)
    (son Mehujael Methushael)
    (son Methushael Lamech)
    (wife Lamech Ada)
    (son Ada Jabal)
    (son Ada Jubal)

    (rule (append-to-form () ?y ?y))

    (rule (append-to-form (?u . ?v) ?y (?u . ?z))
	  (append-to-form ?v ?y ?z))

    (rule (grandson ?gparent ?gson)
	  (and (son ?gparent ?parent)
	       (son ?parent ?gson)))
    (rule (son ?p ?son)
	  (and (wife ?p ?wife)
	       (son ?wife ?son)))
 ;   (rule ((great . ?rel) ?x ?y)
;	  (and
;	   (append-to-form ?rel2 (grandson) ?rel)
;	   (?rel ?btw ?y)
;	   (son ?x ?btw)))

    (rule (end-in-grandson (grandson))) 
    (rule (end-in-grandson (?x . ?rest)) 
       (end-in-grandson ?rest)) 
  
    (rule ((grandson) ?x ?y) 
       (grandson ?x ?y)) 
    (rule ((great . ?rel) ?x ?y) 
	  (and (end-in-grandson ?rel) 
	       (son ?x ?z) 
	       (?rel ?z ?y))) 
))
