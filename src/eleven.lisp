(in-package #:eleven)


(defclass player ()
  ((hand :initarg :hand :accessor hand :type list)
   (board :initarg :board :accessor board :type list)
   (points :type int :initform 0 :accessor points)
   (name :initarg :name :initform "" :accessor name :type string)))

(defmethod count-points ((player player))
 (reduce #'+ (mapcar #'first (mapcar #'last (hand player)))))

(defun make-player (&optional name)
  (make-instance 'player :hand nil :board nil :name name))

(defmethod draw-card ((obj player) (deck stack))
  (push (stack-pop deck) (hand obj)))

(defclass card ()
  ((face :initarg :face :accessor face)
   (suite :initarg :suite :accessor suite)
   (points :initarg :points :accessor points)))

(defun make-card (face points &optional suite)
  (make-instance 'card :face face :suite suite :points points))

(defmethod jokerp ((card card))
  (equal (face card) :joker))

(defun make-deck ()
  (let ((deck))
    (loop repeat 2
	  do (push (make-card :joker 50) deck))
    (dolist (suite '(:spades :hearts :diamonds :clubs))
      (loop for number from 2 upto 10
            do (push (make-card number number suite) deck))
      (dolist (face '(:jack :queen :king :ace))
        (push (make-card face (if (equal face :ace) 25 10) suite) deck)))
    deck))

(defun setup-game (numberofplayers)
  (assert (and (> numberofplayers 0)
	       (< numberofplayers 7)))
  (let ((players)
	(deck (make-stack))
	(discard-pile (make-stack)))
    (loop repeat 2
	  do (dolist (card (make-deck))
	       (stack-push card deck)))
    (shuffle-stack deck)
    (loop for i from 1 upto numberofplayers
	  do (let ((pl (make-player)))
	       (loop repeat 11
		     do (draw-card pl deck))
	       (push pl players)))
    (values players deck discard-pile)))

(defclass goal ()
  ((content :accessor content)))

(defmethod view-goal ((goal goal))
  (content goal))

(defgeneric emptyp (goal))

(defgeneric lay-down (lst goal))

(defgeneric add-card (card pos goal))

(defclass trio (goal)
  ((content :accessor content :initform (list nil nil nil))))

(defun make-trio ()
  (make-instance 'trio))

(defmethod emptyp ((trio trio))
  (zerop (count nil (mapcar #'null (content trio)))))

(defmethod lay-down (lst (trio trio))
  (assert (typep lst 'list) nil "lst has to be a list. lst in of type ~a." (type-of lst))
  (assert (emptyp trio) nil "you cannot lay down on a goal that is already full")
  (assert (= (length lst) 3) nil "a trio needs to have three cards. you are trying to lay down only this cards ~a" lst)
  (let ((nojokers (remove-if #'jokerp lst)))
    (assert (> (length nojokers) 0) nil "a trio can contain at most two jokers")
    (assert (= (count t (mapcar #'(lambda (x) (equal (face x) (face (first nojokers)))) nojokers))
	       (length nojokers))
	    nil "all cards in a trio need to have the same number: ~a" lst)
    (assert (if (= (length nojokers) 1)
		t
		(= (length nojokers)
		   (length (remove-duplicates (mapcar #'suite nojokers)))))
	    nil "each card of a trio needs to be of a different suite: ~a" lst))
  (loop for i from 0 below 3
	do (push (nth i lst) (nth i (content trio)))))

;; rules to add a card to a trio
;; jokers cannot be removed from trios
;; a card added has to be equal to one of the ones that are present.
;; the maximum number of cards in each pile of the trio is 2
;; if there is a joker, any of the cards that are not present can be put on top of it
;; a joker can go on top of any card

(defmethod add-card ((card card) pos (trio trio))
  (assert (not (emptyp trio)))
  (assert (and (>= pos 0) (< pos 3)))
  (let* ((pile (nth pos (content trio)))
	 (cardofpile (first pile)))
    (assert (= (length pile) 1))
    ;; (assert (zerop (length (remove-if (lambda (x) (equal x (face card))) (mapcar #'face (content trio))))))
    (cond ((jokerp card)
	   (assert (not (jokerp cardofpile)))
	   (push card (nth pos (content trio))))
	  (t
	   (if (jokerp cardofpile)
	       (assert (zerop (length (remove-if-not (lambda (x) (equal (face x) (face card))) (content trio)))))
	       (assert (and
			(equal (face card) (face cardofpile))
			(equal (suite card) (suite cardofpile)))))
	   (push card (nth pos (content trio)))))))

(defclass seq (goal)
  ((content :accessor content :initform nil)))

(defun make-seq ()
  (make-instance 'seq))

(defmethod lay-down (lst (seq seq))
  (assert (typep lst 'list) nil "lst has to be a list. lst in of type ~a." (type-of lst))
  (assert (> (length lst) 3) nil "a sequence has a minimum of four cards. you are trying to lay down only ~a cards." (length lst))
  (let ((nojokers (remove-if #'jokerp lst)))
    (assert (<= (- (length lst) (length nojokers)) 2) nil "a sequence can only have two jokers.")
    (assert (= 1 (length (remove-duplicates (mapcar #'suite nojokers)))) nil "All cards in a sequence have to be of the same suite.")
    (assert (= (length nojokers) (length (remove-duplicates (mapcar #'face nojokers)))) nil "All non joker cards need to be different.")
    (assert  (not (equal :fail
			 (loop named tk for a on lst
			       for x = (face (first a))
			       for y = (face (second a))
			       for z = (face (third a))
			       with seq-order = (if (> (length (intersection (list x y z) '(2 3))) 0)
						    (butlast sequence-order)
						    (rest sequence-order))
			       do (cond ((not z) (return-from tk :success))
					  ((and (jokerp y) (or (jokerp x) (jokerp z)))
					   (return-from tk :fail))
					  ((jokerp y)
					   (when (not (equal (+ (position x seq-order) 2)
							     (position z seq-order)))
					     (return-from tk :fail)))
					  ((= (length a) (length lst))
					   (when (equal y :ace)
							 (return-from tk :fail)))
					  ((zerop (count :joker (list x y z)))
					   (unless (< (position x seq-order)
						      (position y seq-order)
						      (position z seq-order))
					     (return-from tk :fail)))))))
	     nil "Rules for sequence are not followed."))
  (setf (content seq) lst))

(defun sort-sequence (lst)
  (let ((seq-order))
    (if (> (length (intersection lst '(2 3))) 0)
	(setf seq-order (butlast sequence-order))
	(setf seq-order (rest sequence-order)))
  (sort lst (lambda (a b)
               (let ((a-index (position (face a) seq-order))
                     (b-index (position (face b) seq-order)))
                 (if (and a-index b-index)
                     (< a-index b-index)
                     (< (face (last a)) (face (last b)))))))))

(defparameter sequence-order '(:ace 2 3 4 5 6 7 8 9 10 :jack :queen :king :ace))



;; (assert (not (let ((lastcard))
;; 	       (loop named loo for i in (rest lst)
;; 			 do (if lastcard
;; 				(if (equal (first i) :joker))

;; 				()
;; 			     (and (> localpos lastpos)
;; 					    (<= localpos (+ lastpos 2)))
;; 					 (setf lastpos localpos)
;; 				       (return-from loo t)))
;; 			 finally (when (> (1+ (- lastpos diff)) (length lst))
;; 				   (return-from loo t))))
;; 	    nil "Rules for sequence are not followed.")


;; (loop named tk for a on lst
;;       for x = (first (first a))
;;       for y = (first (second a))
;;       for z = (first (third a))
;;       do (let ((seq-order (if (> (length (intersection (list x y z) '(2 3))) 0)
;; 			      (butlast sequence-order)
;; 			      (rest sequence-order))))
;; 	   (cond ((not z) (return-from tk :success))
;; 	       ((and (equal y :joker) (or (equal x :joker) (equal z :joker)))
;; 		(return-from tk :fail))
;; 	       ((equal y :joker)
;; 		(when (not (equal (+ (position x sequence-order) 2)
;; 				  (position z sequence)))))
;; 	       ((= (length a) (length lst))
;; 		(when (equal y :ace)
;; 		  (return-from tk :fail)))
;; 	       ((not (find :joker (list x y z))) (unless (> (position x sequence-order)
;; 							    (position y sequence-order)
;; 							    (position z sequence-order))
;; 						   (return-from tk :fail))))))

;; (let ((lastpos (position (first lst) sequence-order)))
;;   (loop check for i in (rest lst)
;; 	do(let ((localpos (position i sequence-order)))
;; 	    (if (and (> localpos lastpos)
;; 		     (<= localpos (+ lastpos 2)))
;; 		(setf (1+ lastpos))
;; 		(return-from check nil)))))





;; (handler-case
;;     (lay-down list trio)
;;   (simple-error (condition)
;;     (format t "Assertion failed: ~A~%" condition)))


;; (defmethod shuffle (lst)
;;   (sort lst
;;         #'(lambda (x y)
;; 	    (declare (ignore x y))
;; 	    (zerop (random 2)))))
