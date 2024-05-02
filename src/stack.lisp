(in-package #:eleven)

(defclass stack ()
  ((elements :initform '()
             :accessor stack-elements)))

(defmethod stack-empty-p ((stack stack))
  (null (stack-elements stack)))

(defmethod stack-push (element (stack stack))
  (push element (stack-elements stack)))

(defmethod stack-pop ((stack stack))
  (when (stack-empty-p stack)
    (error "Stack is empty"))
  (pop (stack-elements stack)))

(defmethod stack-top ((stack stack))
  (when (stack-empty-p stack)
    (error "Stack is empty"))
  (first (stack-elements stack)))

(defmethod stack-see-all ((stack stack))
  (stack-elements stack))

(defmethod shuffle-stack ((stack stack))
  (setf (stack-elements stack)
	(sort (stack-elements stack)
              #'(lambda (x y)
		  (declare (ignore x y))
		  (zerop (random 2))))))

(defun make-stack ()
  (make-instance 'stack))

