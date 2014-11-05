;; load into amb evaluator
;; added adjectives and adverbs (only one each per noun/verb, adverbs can only be appended to verbs)
;; generate random sentence
;; EXAMPLE:
;; (parse '(article adjective noun verb adverb))
;; EXAMPLE OUTPUT:
;; (sentence (adj-noun-phrase (article a) (adjective lugubrious) (noun class)) (verb-phrase (verb sleeps) (adv-phrase (adverb intensely))))


(define nouns '(noun student professor cat class))

(define verbs '(verb studies lectures eats sleeps))

(define articles '(article the a))

(define prepositions '(prep for to in by with))

(define adjectives '(adjective lugubrious delicious irate wobbly wierd))

(define adverbs '(adverb intensely furtively fiercely worryingly wierdly))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) word-list))
  (let ((found-word (list-ref (cdr word-list) (random (length (cdr word-list)))))) ;; only change
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-verb-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))
	 (maybe-extend (list 'verb-phrase
			     verb-phrase
			     (list 'adv-phrase
				   (parse-word adverbs))))))
  (maybe-extend (parse-word verbs)))

(define (parse-simple-noun-phrase)
  (amb (list 'simple-noun-phrase
	     (parse-word articles)
	     (parse-word nouns))
       (list 'adj-noun-phrase
	     (parse-word articles)
	     (parse-word adjectives)
	     (parse-word nouns))))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (require p)
  (if (not p) (amb)))



;; load this into regular scheme evaluator
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list 'memq memq)
        (list 'member member)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '= =)
        (list '> >)
        (list '>= >=)
	(list '< <)
	(list '<= <=)
        (list 'abs abs)
        (list 'remainder remainder)
        (list 'integer? integer?)
        (list 'sqrt sqrt)
        (list 'eq? eq?)
	(list 'display display)
	(list 'newline (newline))
	(list 'list-ref list-ref)
	(list 'random random)
	(list 'length length)
;;      more primitives
        ))
