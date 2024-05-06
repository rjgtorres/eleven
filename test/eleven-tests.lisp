(in-package #:eleven.test)

;; (define-test test-eleven)

(define-test sequences)

(define-test lay-down-sequence
  :parent sequences
    (fail (let ((seq (make-seq)))
	    (lay-down (list (eleven::make-card :ace :suite :clubs)
			    (eleven::make-card 2 :suite :clubs)
			    (eleven::make-card 2 :suite :clubs)
			    (eleven::make-card 4 :suite :clubs))
			 seq)
	       seq)
	simple-error "sequence with a card repeated"))

(define-test trios)


(define-test add-card-trio
  :parent trios
  (let* ((trio (eleven::make-trio)))
    (lay-down (list (eleven::make-card 2 :suite :clubs)
		    (eleven::make-card :joker)
		    (eleven::make-card 2 :suite :diamonds))
	      trio)
    (fail (eleven::add-card (eleven::make-card 2 :suite :spades) 0 trio) simple-error
          "The card added has a different face/suite.")
    (fail (eleven::add-card (eleven::make-card 2 :suite :diamonds) 1 trio) simple-error
          "The card already exists in another pile")
    (true (eleven::add-card (eleven::make-card :joker) 0 trio))
    (fail (eleven::add-card (eleven::make-card :joker) 1 trio) simple-error
          "Cannot add a joker on top of another.")
    (fail (eleven::add-card (eleven::make-card :joker) 2 trio) simple-error
          "Cannot have more than 2 jokers in a trio.")
    (true (eleven::add-card (eleven::make-card 2 :suite :diamonds) 2 trio))
    (true (eleven::add-card (eleven::make-card 2 :suite :spades) 1 trio))))
