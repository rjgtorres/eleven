(in-package #:eleven)


 ;; #############################################################################

;; rules of the Eleven game
;; two decks of cards
;; each player starts with 11 cards in hand
;; at least two players
;; the goal is to make trios and sequences acording to the following table:
;; |trio|sequence|
;; |  2 |    0   |
;; |  1 |    1   |
;; |  0 |    2   |
;; |  3 |    0   |
;; |  2 |    1   |
;; |  1 |    2   |
;; |  0 |    3   |

;; a trio is a group of three cards with the same number/figure
;; a sequence is a sequence of at least 4 consecutive cards of the same suite

;; the goal of each game is to complete the defined task and empty ones hand
;; the game processes the following way:
;; in the start of a player turn, draw a card either from the deck or from the discard pile.
;; after that the other players have the option to draw cards from the discard pile
;; at the end of its turn, a card must be discarded to the discard pile
;; In a players turn, if he has the goal completed, he can put the cards that complete the goal into its board.
;; then he can proceed to put cards from his hand that fit in other players boards
;; the jokers can represent any card, but a sequence cannot have two consecutive jokers, and a trio cannot be composed of three jockers.
;;

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

(defun make-deck ()
  (let ((deck)
        (index 0))
    (loop repeat 2
	  do (push (list :joker 50) deck))
    (dolist (suite '(:spades :hearts :diamonds :clubs))
      (loop for number from 2 upto 10
            do (push (list number suite number) deck)
               (setf index (1+ index)))
      (dolist (face '(:jack :queen :king :ace))
        (push (list face suite (if (equal face :ace) 25 10)) deck)
        (setf index (1+ index))))
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

;; now I need to defne the trios and sequences, a trio can be a list with three lists inside, each with a maximum lenght of 2, a sequence is a list with a minimum size of 4

(defclass goal ()
  ((content :accessor content)))

(defmethod view-goal ((goal goal))
  (content goal))

(defgeneric lay-down (lst goal))

(defgeneric add-card (card goal))

(defclass trio (goal)
  ((content :accessor content :initform (list nil nil nil))))

(defun make-trio ()
  (make-instance 'trio))

(defmethod lay-down (lst (trio trio))
  (assert (typep lst 'list) nil "lst has to be a list. lst in of type ~a." (type-of lst))
  (assert (= (length lst) 3) nil "a trio needs to have three cards. you are trying to lay down only this cards ~a" lst)
  (let ((nojokers (remove-if #'(lambda (x) (equal (first x) :joker)) lst)))
    (assert (> (length nojokers) 0) nil "a trio can contain at most two jokers")
    (assert (= (count t (mapcar #'(lambda (x) (equal (first x) (first (first nojokers)))) nojokers))
	       (length nojokers))
	    nil "all cards in a trio need to have the same number: ~a" lst)
    (assert (if (= (length nojokers) 1)
		t
		(= (length nojokers)
		   (length (remove-duplicates (mapcar #'second nojokers)))))
	    nil "each card of a trio needs to be of a different suite: ~a" lst))
  (loop for i from 0 below 3
	do (push (nth i lst) (nth i (content trio)))))

(defclass seq (goal)
  ((content :accessor content :initform nil)))

(defun make-seq ()
  (make-instance 'seq))

(defmethod lay-down (lst (seq seq))
  (assert (typep lst 'list) nil "lst has to be a list. lst in of type ~a." (type-of lst))
  (assert (> (length lst) 3) nil "a sequence has a minimum of four cards. you are trying to lay down only ~a cards." (length lst))
  (let ((nojokers (remove-if #'(lambda (x) (equal (first x) :joker)) lst)))
    (assert (<= (- (length lst) (length nojokers)) 2) nil "a sequence can only have two jokers.")
    (assert (= 1 (length (remove-duplicates (mapcar #'second nojokers)))) nil "All cards in a sequence have to be of the same suite.")
    (assert (= (length nojokers) (length (remove-duplicates (mapcar #'first nojokers)))) nil "All non joker cards need to be different.")
    (assert  (not (equal :fail
			 (loop named tk for a on lst
			       for x = (first (first a))
			       for y = (first (second a))
			       for z = (first (third a))
			       with seq-order = (if (> (length (intersection (list x y z) '(2 3))) 0)
						    (butlast sequence-order)
						    (rest sequence-order))
			       do (progn
				    (cond ((not z) (return-from tk :success))
					  ((and (equal y :joker) (or (equal x :joker) (equal z :joker)))
					   (return-from tk :fail))
					  ((equal y :joker)
					   (when (not (equal (+ (position x seq-order) 2)
									 (position z seq-order)))))
					  ((= (length a) (length lst))
					   (when (equal y :ace)
							 (return-from tk :fail)))
					  ((not (find :joker (list x y z)))
					   (unless (< (position x seq-order)
						      (position y seq-order)
						      (position z seq-order))
					     (return-from tk :fail))))))))
	     nil "Rules for sequence are not followed."))
  (setf (content seq) lst))

(defun sort-sequence (lst)
  (let ((seq-order))
    (if (> (length (intersection lst '(2 3))) 0)
	(setf seq-order (butlast sequence-order))
	(setf seq-order (rest sequence-order)))
  (sort lst (lambda (a b)
               (let ((a-index (position (first a) seq-order))
                     (b-index (position (first b) seq-order)))
                 (if (and a-index b-index)
                     (< a-index b-index)
                     (< (first (last a)) (first (last b)))))))))

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
