(in-package #:eleven)


(defclass player ()
  ((hand :initarg :hand :accessor hand :type list)
   (board :initarg :board :accessor board :type list)
   (points :type int :initform 0 :accessor points)
   (name :initarg :name :initform "" :accessor name :type string)))

(defmethod count-points ((player player))
 (reduce #'+ (mapcar #'points (hand player))))

(defun make-player (&optional name)
  (make-instance 'player :hand nil :board nil :name name))

(defmethod draw-card ((obj player) (deck stack))
  (push (stack-pop deck) (hand obj)))

(defclass card ()
  ((face :initarg :face :accessor face)
   (suite :initarg :suite :accessor suite)
   (points :initarg :points :accessor points)))

(defun make-card (face &key points suite)
  (make-instance 'card :face face :suite suite :points points))

(defmethod jokerp ((card card))
  (equal (face card) :joker))

(defmethod iscardp (cface (card card))
  (equal (face card) cface))

(defun make-deck ()
  (let ((deck))
    (loop repeat 2
	  do (push (make-card :joker :points 50) deck))
    (dolist (suite '(:spades :hearts :diamonds :clubs))
      (loop for number from 2 upto 10
            do (push (make-card number :points number :suite suite) deck))
      (dolist (face '(:jack :queen :king :ace))
        (push (make-card face :points (if (iscardp :ace face) 25 10) :suite suite) deck)))
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
  (let ((nojokers (remove-if #'(lambda (x) (iscardp :joker x)) lst)))
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
;; a joker can go on top of any card, but maybe put it in the second position

(defmethod list_all_of_card ((card card) trio)
  (reduce #'append
          (mapcar (lambda (inner-lst)
                    (remove-if-not
                     #'(lambda (x) (and
                               (equal (face x) (face card))
                               (if (iscardp :joker card)
                                   t
                                   (equal (suite x) (suite card)))))
                     inner-lst))
                  (content trio))))

(defmethod add-card ((card card) pos (trio trio))
  (assert (not (emptyp trio)))
  (assert (and (>= pos 0) (< pos 3)))
  (let* ((pile (nth pos (content trio)))
	 (cardofpile (first pile)))
    (assert (= (length pile) 1))
    (cond ((iscardp :joker card)
	   (assert (not (iscardp :joker cardofpile)))
           (assert (< (length (list_all_of_card (make-card :joker) trio))
                      2))
	   (push card (nth pos (content trio))))
	  (t
	   (if (iscardp :joker cardofpile)
	       (assert (zerop (length (list_all_of_card card trio))))
	       (assert (and
			(equal (face card) (face cardofpile))
			(equal (suite card) (suite cardofpile)))))
	   (if (iscardp :joker card)
               (nconc (nth pos (content trio)) card)
               (push card (nth pos (content trio))))))))

(defclass seq (goal)
  ((content :accessor content :initform nil)))

(defun make-seq ()
  (make-instance 'seq))

(defmethod lay-down (lst (seq seq))
  (assert (typep lst 'list) nil "lst has to be a list. lst in of type ~a." (type-of lst))
  (assert (> (length lst) 3) nil "a sequence has a minimum of four cards. you are trying to lay down only ~a cards." (length lst))
  (let ((nojokers (remove-if #'(lambda (x) (iscardp :joker x)) lst)))
    (assert (<= (- (length lst) (length nojokers)) 2) nil "a sequence can only have two jokers.")
    (assert (= 1 (length (remove-duplicates (mapcar #'suite nojokers)))) nil "All cards in a sequence have to be of the same suite.")
    (assert (= (length nojokers) (length (remove-duplicates (mapcar #'face nojokers)))) nil "All non joker cards need to be different.")
    (assert  (not (equal :fail
			 (let ((seq-order (if (intersection '(2 3) (mapcar #'face (content seq)))
					      (butlast sequence-order)
					      (rest sequence-order))))
			     (loop named tk for a on lst
			       for x = (first a)
			       for y = (second a)
			       for z = (third a)
			       do (cond ((not z)
                                         (return-from tk :success))
					((and (iscardp :joker y) (or (iscardp :joker x) (iscardp :joker z)))
					 (return-from tk :fail))
					((and (iscardp :joker y)
                                              (not (equal (+ (position (face x) seq-order) 2)
							  (position (face z) seq-order))))
					 (return-from tk :fail))
					((and (= (length a) (length lst))
					      (iscardp :ace y))
					 (return-from tk :fail))
					((zerop (count :joker (list (face x) (face y) (face z))))
					 (unless (< (position (face x) seq-order)
						    (position (face y) seq-order)
						    (position (face z) seq-order))
					   (return-from tk :fail))))))))
	     nil "Rules for sequence are not followed."))
  (setf (content seq) lst))

;; cards can only be put in each extremity of the sequence or substitute a joker
;; only an ace can be added to the sequence, either before the 2 or after the king
;; when substituting a joker, the joker goes to the hand of the player
;; pos can be :begin, :end or number

(defmethod add-card ((card card) pos (seq seq))
  (let* ((seq-order (if (intersection '(2 3) (mapcar #'face (content seq)))
		       (butlast sequence-order)
		       (rest sequence-order)))
	 (card-order (position (face card) seq-order)))
   (when (numberp pos)
    (assert (not (iscardp :joker card)))
    (assert (iscardp :joker (nth pos (content seq))))
    (assert (cond ((zerop pos)
		   (= (1+ card-order)
		      (position (face (nth (1+ pos) (content seq))) seq-order)))
		  ((equal (1+ pos) (length (content seq)))
		   (= (1- card-order)
		      (position (face (nth (1- pos) (content seq))) seq-order)))
		  (t (and (= (1+ card-order)
			     (position (face (nth (1+ pos) (content seq))) seq-order))
			  (= (1- card-order)
			     (position (face (nth (1- pos) (content seq))) seq-order)))))))
    (when (iscardp :ace (face card))
      (assert (intersection '(2 3 :queen :king) (mapcar #'face (content seq))))
      (assert (zerop (length (remove-if-not #'(lambda (x) (iscardp :ace (face x))) (content seq))))))
    (when (iscardp :joker (face card))
      (assert (< (length (remove-if-not #'(lambda (x) (iscardp :joker (face x))) (content seq))) 2))
      (assert (not (or (when (> pos 0)
			  (iscardp :joker (face (nth (1- pos) (content seq)))))
		       (when (< (1+ pos) (length (content seq)))
			  (iscardp :joker (face (nth (1+ pos) (content seq)))))
		       (when (equal pos :begin)
			 (iscardp :joker (face (first (content seq)))))
		       (when (equal pos :end)
			 (iscardp :joker (face (first (last (content seq))))))))))
    (when (equal :begin pos)
      (assert (if (iscardp :joker (first (content seq)))
		  (= (+ card-order 2)
		      (position (face (second (content seq))) seq-order))
		  (= (1+ card-order)
		      (position (face (first (content seq))) seq-order)))))
    (when (equal :end pos)
      (assert (if (iscardp :joker (first (content seq)))
		  (= (- card-order 2)
		      (position (face (nth (- (length (content seq)) 2) (content seq))) seq-order))
		  (= (1- card-order)
		      (position (face (first (last (content seq)))) seq-order)))))))

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

;; (setf ls (append (subseq ls 0 2) (list 3) (subseq ls 2)))
;; (push 3 (rest (nthcdr 2 lk)))

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
