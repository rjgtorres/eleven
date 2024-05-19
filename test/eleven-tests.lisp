(in-package #:eleven.test)

;; (define-test test-eleven)

(define-test sequences)

(define-test lay-down-sequence
  :parent sequences
    (fail (let ((seq (make-seq)))
	    (lay-down (list (make-card :ace :suite :clubs)
			    (make-card 2 :suite :clubs)
			    (make-card 2 :suite :clubs)
			    (make-card 4 :suite :clubs))
			 seq)
	       seq)
	simple-error "sequence with a card repeated"))

(define-test trios)


(define-test add-card-trio
  :parent trios
  (let* ((trio (make-trio)))
    (lay-down (list (make-card 2 :suite :clubs)
		    (make-card :joker)
		    (make-card 2 :suite :diamonds))
	      trio)
    (fail (add-card (make-card 2 :suite :spades) 0 trio) simple-error
          "The card added has a different face/suite.")
    (fail (add-card (make-card 2 :suite :diamonds) 1 trio) simple-error
          "The card already exists in another pile")
    (true (add-card (make-card :joker) 0 trio))
    (fail (add-card (make-card :joker) 1 trio) simple-error
          "Cannot add a joker on top of another.")
    (fail (add-card (make-card :joker) 2 trio) simple-error
          "Cannot have more than 2 jokers in a trio.")
    (true (add-card (make-card 2 :suite :diamonds) 2 trio))
    (true (add-card (make-card 2 :suite :spades) 1 trio))))

(define-test add-card-sequence
  (true (let ((seq (make-seq)))
	  (lay-down (list (make-card 2 :suite :clubs)
			  (make-card 3 :suite :clubs)
			  (make-card 4 :suite :clubs)
			  (make-card 5 :suite :clubs))
		    seq)
	  (add-card (make-card :ace :suite :clubs) :begin seq) seq)
	))
