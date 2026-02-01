(in-package #:eleven.test)

;;; ============================================================================
;;; STACK TESTS
;;; ============================================================================

(define-test stack)

(define-test (stack make)
  (let ((my-stack (make-stack)))
    (true (stack-empty-p my-stack) "Empty stack should return true")
    (is equal (stack-see-all my-stack) '() "Has no elements to see")
    (fail (stack-top my-stack) simple-error "top fails when stack is empty")
    (fail (stack-pop my-stack) simple-error "pop fails when stack is empty")))

(define-test (stack push-pop)
  (let ((my-stack (make-stack)))
    (stack-push 1 my-stack)
    (is = (stack-top my-stack) 1 "Top should be 1")
    (false (stack-empty-p my-stack) "Stack should not be empty after push")
    (is = (stack-pop my-stack) 1 "Pop should return 1")
    (true (stack-empty-p my-stack) "Stack should be empty after popping the only value")))

(define-test (stack multiple-push)
  (let ((my-stack (make-stack)))
    (loop for i from 1 to 3
	  do (stack-push i my-stack))
    (is = (stack-top my-stack) 3 "Top should be 3 (LIFO)")
    (is equal (stack-see-all my-stack) '(3 2 1) "Stack elements in order")))

(define-test (stack shuffle)
  (let ((my-stack (make-stack)))
    (loop for i from 1 to 10
	  do (stack-push i my-stack))
    (shuffle-stack my-stack)
    (let ((all-elements (stack-see-all my-stack)))
      (is equal (sort (copy-list all-elements) #'<) 
                '(1 2 3 4 5 6 7 8 9 10) 
                "After shuffling, all elements should still be present"))))

(define-test (stack empty)
  (let ((my-stack (make-stack)))
    (loop for i from 1 to 5
	  do (stack-push i my-stack))
    (false (stack-empty-p my-stack) "Stack should not be empty")
    (empty-stack my-stack)
    (true (stack-empty-p my-stack) "Stack should be empty after empty-stack")
    (is equal (stack-see-all my-stack) '() "Stack elements should be gone")))
