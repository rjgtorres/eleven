(in-package #:eleven.test)

(define-test stack)

(define-test (stack make)
  (let ((my-stack (eleven::make-stack)))
    (true (eleven::stack-empty-p my-stack) "Empty stack")
    (is equal (eleven::stack-see-all my-stack) '() "Has no elements to see")
    (fail (eleven::stack-top my-stack) simple-error "top fails when stack is empty")
    (fail (eleven::stack-pop my-stack) simple-error "pop fails when stack is empty")))

(define-test (stack push)
  (let ((my-stack (eleven::make-stack)))
    (eleven::stack-push 1 my-stack)
    (is = (eleven::stack-top my-stack) 1)
    (eleven::stack-pop my-stack)
    (is equal (eleven::stack-see-all my-stack) '() "After poping the only value, the stack is empty")
    (loop for i from 1 to 3
	  do (eleven::stack-push i my-stack))
    (is = (eleven::stack-top my-stack) 3)
    (is equal (eleven::stack-see-all my-stack) '(3 2 1))))

(define-test (stack shuffle)
  (let ((my-stack (eleven::make-stack)))
    (loop for i from 1 to 5
	  do (eleven::stack-push i my-stack))
    (is = (eleven::stack-top my-stack) 5)
    (is equal (eleven::stack-see-all my-stack) '(5 4 3 2 1))
    (eleven::shuffle-stack my-stack)
    (is-values (eleven::stack-see-all my-stack) (values 5 4 3 2 1) "after shuffling we only know that the same numbers should be there")))
;; (describe "Shuffle Stack"
;; 		    (it "shuffles the stack"
;; 				  (let ((my-stack (make-stack)))
;; 				    (stack-push 1 my-stack)
;; 				    (stack-push 2 my-stack)
;; 				    (stack-push 3 my-stack)
;; 				    (shuffle-stack my-stack)
;; 				    (expect (sort (stack-see-all my-stack) '<) :to-equal '(1 2 3)))))
