;; load into amb evaluator
;; added adjectives and adverbs (only one each per noun/verb, adverbs can only be appended to verbs)

(define nouns '(noun student professor cat class))

(define verbs '(verb studies lectures eats sleeps))

(define articles '(article the a))

(define prepositions '(prep for to in by with))

(define adjectives '(adjective lugubrious delicious irate wobbly wierd))

(define adverbs '(adverb intensely furtively fiercely worryingly wierdly))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
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
