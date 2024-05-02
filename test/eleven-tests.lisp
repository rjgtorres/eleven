(in-package #:eleven.test)

;; (define-test test-eleven)

(define-test sequences)

(define-test lay-down-sequence
  :parent sequences
    (fail (let ((seq (make-seq)))
	       (lay-down '((:ace :clubs 2) (2 :clubs 2) (2 :clubs 2) (4 :clubs 2)) seq)
	       seq)
	simple-error "sequence with a card repeated"))
