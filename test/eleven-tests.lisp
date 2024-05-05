(in-package #:eleven.test)

;; (define-test test-eleven)

(define-test sequences)

(define-test lay-down-sequence
  :parent sequences
    (fail (let ((seq (make-seq)))
	    (lay-down (list (eleven::make-card :ace 25 :clubs)
			    (eleven::make-card 2 2 :clubs)
			    (eleven::make-card 2 2 :clubs)
			    (eleven::make-card 4 4 :clubs))
			 seq)
	       seq)
	simple-error "sequence with a card repeated"))

(define-test trios)


(define-test add-card-trio
  :parent trios
  (let* ((trio (eleven::make-trio)))
    (lay-down (list (eleven::make-card 2 2 :clubs)
		    (eleven::make-card :joker 50)
		    (eleven::make-card 2 2 :diamonds))
	      trio)
    (fail (eleven::add-card (eleven::make-card 2 2 :spades) 0 trio))
    (true (eleven::add-card (eleven::make-card :joker 50) 0 trio))))
