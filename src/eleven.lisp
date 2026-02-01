(in-package #:eleven)

;;; ============================================================================
;;; CARD CLASS AND METHODS
;;; ============================================================================

(defclass card ()
  ((face :initarg :face :accessor face)
   (suite :initarg :suite :accessor suite)
   (points :initarg :points :accessor points)))

(defun make-card (face &key points suite)
  "Create a card with FACE, POINTS, and SUITE."
  (make-instance 'card :face face :suite suite :points points))

(defmethod jokerp ((card card))
  "Check if CARD is a joker."
  (equal (face card) :joker))

(defmethod iscardp (cface (card card))
  "Check if CARD has face CFACE."
  (equal (face card) cface))

;;; ============================================================================
;;; PLAYER CLASS AND METHODS
;;; ============================================================================

(defclass player ()
  ((hand :initarg :hand :accessor hand :type list)
   (board :initarg :board :accessor board :type list)
   (points :type integer :initform 0 :accessor points)
   (playername :initarg :playername :initform "" :accessor playername :type string)))

(defun make-player (&optional playername)
  "Create a new player with optional NAME."
  (make-instance 'player :hand nil :board nil :playername (or playername "")))

(defmethod reset ((player player))
  "Reset player's hand and board."
  (empty-hand player)
  (empty-board player))

(defmethod draw-card ((obj player) (deck stack))
  "Draw a card from DECK and add it to player's HAND."
  (push (stack-pop deck) (hand obj)))

(defmethod discard-card ((player player) (card card) (discard-pile stack))
  "Remove CARD from player's HAND and push to DISCARD-PILE."
  (setf (hand player) (remove card (hand player) :count 1))
  (stack-push card discard-pile))

(defmethod empty-hand ((player player))
  "Empty the player's hand."
  (setf (hand player) '()))

(defmethod empty-board ((player player))
  "Empty the player's board."
  (setf (board player) '()))

(defmethod count-points ((player player))
  "Count and add points from cards in player's HAND to total POINTS."
  (setf (points player)
	(+ (points player)
	   (reduce #'+ (mapcar #'points (hand player)) :initial-value 0))))

(defmethod see-points ((player player))
  "Return current POINTS of player."
  (points player))

;;; ============================================================================
;;; DECK CREATION
;;; ============================================================================

(defparameter sequence-order '(:ace 2 3 4 5 6 7 8 9 10 :jack :queen :king :ace))

(defun make-deck ()
  "Create a standard deck with two jokers and all other cards."
  (let ((deck '()))
    (loop repeat 2
	  do (push (make-card :joker :points 50) deck))
    (dolist (suite '(:spades :hearts :diamonds :clubs))
      (loop for number from 2 upto 10
            do (push (make-card number :points number :suite suite) deck))
      (dolist (face '(:jack :queen :king :ace))
        (push (make-card face 
                        :points (if (equal :ace face) 25 10) 
                        :suite suite) 
              deck)))
    deck))

;;; ============================================================================
;;; GOAL CLASSES AND METHODS
;;; ============================================================================

(defclass goal ()
  ((content :accessor content)))

(defmethod view-goal ((goal goal))
  "View the CONTENT of a GOAL."
  (content goal))

(defgeneric emptyp (goal)
  (:documentation "Check if GOAL is empty and ready for lay-down."))

(defgeneric lay-down (lst goal)
  (:documentation "Lay down cards LST into GOAL."))

(defgeneric add-card (card pos goal)
  (:documentation "Add CARD to GOAL at position POS."))

;;; ============================================================================
;;; TRIO CLASS AND METHODS
;;; ============================================================================

(defclass trio (goal)
  ((content :accessor content :initform (list nil nil nil))))

(defun make-trio ()
  "Create a new empty TRIO."
  (make-instance 'trio))

(defmethod emptyp ((trio trio))
  "Check if TRIO is ready for lay-down (all positions empty)."
  (every #'null (content trio)))

(defmethod lay-down (lst (trio trio))
  "Lay down three cards LST into TRIO."
  (assert (typep lst 'list) nil 
          "lst has to be a list. lst is of type ~a." (type-of lst))
  (assert (emptyp trio) nil 
          "you cannot lay down on a goal that is already full")
  (assert (= (length lst) 3) nil 
          "a trio needs to have three cards. you are trying to lay down ~a cards" 
          (length lst))
  
  (let ((nojokers (remove-if #'jokerp lst)))
    (assert (> (length nojokers) 0) nil 
            "a trio can contain at most two jokers")
    (assert (= (count t (mapcar #'(lambda (x) 
                                     (equal (face x) (face (first nojokers)))) 
                                nojokers))
	       (length nojokers)) nil 
            "all cards in a trio need to have the same face: ~a" lst)
    (assert (if (= (length nojokers) 1)
		t
		(= (length nojokers)
		   (length (remove-duplicates (mapcar #'suite nojokers)))))
	    nil 
            "each card of a trio needs to be of a different suite: ~a" lst))
  
  (loop for i from 0 below 3
	do (setf (nth i (content trio)) (nth i lst))))

(defmethod list-all-of-card ((card card) (trio trio))
  "List all cards in TRIO matching CARD's face and suite."
  (reduce #'append
          (mapcar (lambda (inner-lst)
                    (remove-if-not
                     #'(lambda (x) (and
                                     (equal (face x) (face card))
                                     (if (jokerp card)
                                         t
                                         (equal (suite x) (suite card)))))
                     (if (listp inner-lst) inner-lst (list inner-lst))))
                  (content trio))))

(defmethod add-card ((card card) pos (trio trio))
  "Add CARD to TRIO at position POS."
  (assert (not (emptyp trio)) nil "Cannot add to empty trio")
  (assert (and (>= pos 0) (< pos 3)) nil "Position ~a out of range" pos)
  
  (let* ((pile (nth pos (content trio)))
	 (pile-list (if (listp pile) pile (list pile)))
	 (cardofpile (first pile-list)))
    (assert (= (length pile-list) 1) nil "Pile already has multiple cards")
    
    (cond ((jokerp card)
	   (assert (not (jokerp cardofpile)) nil "Cannot add joker on top of joker")
           (assert (< (length (list-all-of-card (make-card :joker) trio)) 2) nil
                   "Cannot have more than 2 jokers in a trio")
	   (setf (nth pos (content trio)) (append pile-list (list card))))
	  (t
	   (if (jokerp cardofpile)
	       (assert (zerop (length (list-all-of-card card trio))) nil
                       "Card already exists in another pile")
	       (assert (and
			(equal (face card) (face cardofpile))
			(equal (suite card) (suite cardofpile))) nil
                       "Card must match existing card"))
	   (setf (nth pos (content trio)) (append pile-list (list card)))))))

;;; ============================================================================
;;; SEQUENCE CLASS AND METHODS
;;; ============================================================================

(defclass seq (goal)
  ((content :accessor content :initform nil)))

(defun make-seq ()
  "Create a new empty SEQUENCE."
  (make-instance 'seq))

(defun get-sequence-order (cards)
  "Get appropriate sequence order based on card faces."
  (if (intersection '(2 3) (mapcar #'face cards))
      (butlast sequence-order)
      (rest sequence-order)))

(defmethod emptyp ((seq seq))
  "Check if SEQUENCE is empty."
  (null (content seq)))

(defmethod lay-down (lst (seq seq))
  "Lay down cards LST into SEQUENCE.
   Jokers stay in their positions as substitutes for missing cards.
   Ace can be at beginning (A-2-3...) or end (...K-A) but never wrapping.
   At most 2 non-consecutive jokers allowed."
  (assert (typep lst 'list) nil 
          "lst has to be a list. lst is of type ~a." (type-of lst))
  (assert (>= (length lst) 4) nil 
          "a sequence has a minimum of four cards. you are trying to lay down ~a cards." 
          (length lst))
  
  (let ((nojokers (remove-if #'jokerp lst)))
    (assert (< (- (length lst) (length nojokers)) 3) nil 
            "a sequence can only have two jokers.")
    (assert (= 1 (length (remove-duplicates (mapcar #'suite nojokers)))) nil 
            "All cards in a sequence have to be of the same suite.")
    (assert (= (length nojokers) (length (remove-duplicates (mapcar #'face nojokers)))) nil 
            "All non joker cards need to be different.")
    
    ;; Check for valid ace positioning (no wrapping)
    (let ((has-ace (some #'(lambda (c) (equal (face c) :ace)) nojokers))
          (has-2-or-3 (some #'(lambda (c) (member (face c) '(2 3))) nojokers))
          (has-q-or-k (some #'(lambda (c) (equal (face c) '(:queen :king))) nojokers)))
      
      ;; If ace exists with king and 2/3, it would be wrapping
      (when (and has-ace has-q-or-k has-2-or-3)
        (error "Cannot have ace wrapping around (K-A-2 is invalid)")))
    
    ;; Validate consecutiveness and joker placement
    (validate-sequence-consecutiveness lst))
  
  ;; Store the sequence as a normalized but position-preserving list
  (setf (content seq) (normalize-sequence-order lst)))

(defun normalize-sequence-order (lst)
  "Normalize sequence order while preserving joker positions."
  (let ((nojokers (remove-if #'jokerp lst)))
    (let ((seq-order (get-sequence-order nojokers)))
      ;; Sort non-joker cards
      (let ((sorted-nojokers (sort (copy-list nojokers)
                                   (lambda (a b)
                                     (< (position (face a) seq-order)
                                        (position (face b) seq-order))))))
        
        ;; Rebuild list with sorted non-joker cards and jokers in original positions
        (let ((result '()))
          (loop for card in lst
                if (jokerp card)
                  do (push card result)
                else
                  do (push (pop sorted-nojokers) result))
          (reverse result))))))

(defun validate-sequence-consecutiveness (lst)
  "Validate that cards in LST form a valid sequence.
   - Must have at least 4 cards total
   - Jokers cannot be consecutive
   - Jokers can substitute for any card
   - All gaps must be filled with jokers"
  ;; First, check for consecutive jokers in the original list
  (loop for i from 0 below (1- (length lst))
        do (when (and (jokerp (nth i lst))
                      (jokerp (nth (1+ i) lst)))
             (error "Sequence cannot have consecutive jokers")))
  
  ;; Get non-joker cards and sort them to check for gaps
  (let ((nojokers (remove-if #'jokerp lst))
        (total-jokers (count-if #'jokerp lst)))
    ;; Need at least 2 non-joker cards to validate sequence
    (when (< (length nojokers) 2)
      (error "Need at least 2 non-joker cards to form a valid sequence"))
    
    ;; Sort non-joker cards by sequence order to find gaps
    (let* ((seq-order (get-sequence-order nojokers))
          (sorted-nojokers (sort (copy-list nojokers)
                                 (lambda (a b)
                                   (< (position (face a) seq-order)
                                      (position (face b) seq-order))))))
      
      ;; Check each gap between consecutive non-joker cards
      (loop for i from 0 below (1- (length sorted-nojokers))
            do (let* ((curr (nth i sorted-nojokers))
                     (next (nth (1+ i) sorted-nojokers))
                     (curr-pos (position (face curr) seq-order))
                     (next-pos (position (face next) seq-order))
                     (gap (- next-pos curr-pos)))
                 
                 ;; Gap of 1 means consecutive (no gap, no joker needed)
                 ;; Gap of 2 means 1 card missing (need 1 joker)
                 ;; Gap of 3 means 2 cards missing (need 2 jokers)
                 ;; Gap > 3 means too many missing cards
                 (when (>= gap 3)
                   (error "Gap too large between cards - cannot fill with jokers"))
                 
                 ;; If there's a gap, verify jokers exist
                 (when (> gap 1)
                   (let ((jokers-needed (- gap 1)))
                     ;; Just verify we have enough jokers total
                     ;; The "no consecutive jokers" check at the top ensures proper placement
                     (unless (>= total-jokers jokers-needed)
                       (error "Not enough jokers to fill gaps"))
                     (setf total-jokers (1- total-jokers))))))))
  
  t)

(defun sort-sequence (lst)
  "Sort cards in sequence LST according to sequence order.
   Ace can be at the beginning (A-2-3...) or end (...Q-K-A) but not wrapping."
  (let ((nojokers (remove-if #'jokerp lst)))
    ;; Check if this is an ace-low sequence (contains 2 or 3)
    (let ((is-ace-low (some #'(lambda (card)
                                (member (face card) '(2 3)))
                            nojokers))
          (has-ace (some #'(lambda (card)
                             (equal (face card) :ace))
                         nojokers)))
      
      ;; Select the appropriate sequence order
      (let ((seq-order (cond
                         ;; Ace-low sequence: A-2-3-4...K
                         ((and is-ace-low has-ace)
                          (butlast sequence-order))
                         ;; Regular sequence: 2-3-4...K-A
                         (t
                          (rest sequence-order)))))
        
        ;; Sort the list maintaining jokers at proper positions
        (sort (copy-list lst)
              (lambda (a b)
                (cond ((jokerp a) nil)
                      ((jokerp b) t)
                      (t (< (position (face a) seq-order)
                            (position (face b) seq-order))))))))))

(defmethod add-card ((card card) pos (seq seq))
  "Add CARD to SEQUENCE at position POS or replace joker at numeric index.
   - Numeric position: replace joker at that index
   - :begin or :end: add card to beginning or end
   - When adding to :begin/:end with joker at that position:
     If joker is at position, card must be 2 steps behind/ahead in sequence"
  (assert (not (emptyp seq)) nil "Cannot add to empty sequence")
  
  (let* ((content-seq (content seq))
         (nojokers (remove-if #'jokerp content-seq)))
    
    ;; Handle numeric position (replace joker)
    (cond ((numberp pos)
           (assert (< pos (length content-seq)) nil "Position out of range")
           (let ((target-card (nth pos content-seq)))
             (assert (jokerp target-card) nil
                     "Position must contain a joker to replace")
             ;; Replace the joker at this position
             (setf (nth pos content-seq) card)))
          
          ;; Handle :begin position
          ((equal pos :begin)
           (assert (and (not (jokerp card))
                       (equal (suite card) (suite (first nojokers))))
                   nil "Can only add regular cards with matching suite")
           (let ((seq-order (get-sequence-order nojokers)))
             (let ((first-card (first content-seq)))
               ;; If first position has a joker
               (if (jokerp first-card)
                   ;; Card must be 2 positions before the second card
                   (let ((second-card (second nojokers)))
                     (assert (= (position (face card) seq-order)
                               (- (position (face second-card) seq-order) 2))
                             nil "Card must be 2 steps before second card"))
                   ;; If first position has regular card, must be consecutive
                   (assert (= (position (face card) seq-order)
                             (1- (position (face first-card) seq-order)))
                           nil "Card must be consecutive before first card"))))
           (setf (content seq) (cons card content-seq)))
          
          ;; Handle :end position
          ((equal pos :end)
           (assert (and (not (jokerp card))
                       (equal (suite card) (suite (first nojokers))))
                   nil "Can only add regular cards with matching suite")
           (let ((seq-order (get-sequence-order nojokers)))
             (let ((last-card (first (last content-seq))))
               ;; If last position has a joker
               (if (jokerp last-card)
                   ;; Card must be 2 positions after the second-to-last card
                   (let ((second-to-last-card (first (last nojokers))))
                     (assert (= (position (face card) seq-order)
                               (+ (position (face second-to-last-card) seq-order) 2))
                             nil "Card must be 2 steps after second-to-last card"))
                   ;; If last position has regular card, must be consecutive
                   (assert (= (position (face card) seq-order)
                             (1+ (position (face last-card) seq-order)))
                           nil "Card must be consecutive after last card"))))
           (setf (content seq) (append content-seq (list card))))
          
          (t (error "Invalid position ~a" pos)))))

;;; ============================================================================
;;; GAME SETUP AND FLOW
;;; ============================================================================

(defun setup-game (players)
  "Setup a new game with PLAYERS."
  (let ((deck (make-stack))
	(discard-pile (make-stack)))
    (loop repeat 2
	  do (dolist (card (make-deck))
	       (stack-push card deck)))
    (shuffle-stack deck)
    (loop for pl in players
	  do (progn
               (reset pl)
	       (setf (points pl) 0)
	       (loop repeat 11
		     do (draw-card pl deck))))
    (values deck discard-pile)))

(defparameter round-goals '((2 0) (1 1) (0 2) (3 0) (2 1) (1 2) (0 3)))

(defun end-round (players deck discard-pile)
  "End current round: count points and reset players."
  (dolist (player players)
    (count-points player)
    (empty-hand player)
    (empty-board player))
  (empty-stack deck)
  (empty-stack discard-pile))

(defun draw-phase (player others deck discard-pile)
  "Handle draw phase for PLAYER."
  (declare (ignore others))
  (force-output)
  (format t "~a's turn. Draw from [1] Deck or [2] Discard pile? " (playername player))
  (force-output)
  (let ((choice (read)))
    (cond ((= choice 1)
           (unless (stack-empty-p deck)
             (draw-card player deck)))
          ((= choice 2)
           (unless (stack-empty-p discard-pile)
             (draw-card player (make-stack))))
          (t (progn
               (format t "Invalid choice~%")
               (draw-phase player others deck discard-pile))))))

(defun discard-phase (player discard-pile)
  "Handle discard phase for PLAYER."
  (format t "~a's cards: ~a~%" (playername player) (hand player))
  (format t "Select card index to discard (0-~a): " (1- (length (hand player))))
  (let ((idx (read-line)))
    (cond ((and (integerp idx) (>= idx 0) (< idx (length (hand player))))
           (discard-card player (nth idx (hand player)) discard-pile))
          (t (format t "Invalid selection~%")
             (discard-phase player discard-pile)))))

(defun turn (player others deck discard-pile)
  "Execute a turn for PLAYER."
  (format t "~%=== ~a's Turn ===~%" (playername player))
  
  ;; Draw phase
  (draw-phase player others deck discard-pile)
  
  ;; Lay down phase (if they have completed their goal)
  (format t "~a, do you want to lay down cards? (y/n) " (playername player))
  (when (equal (read) 'y)
    (format t "Lay down implementation needed~%"))
  
  ;; Add to existing goals
  (format t "~a, do you want to add cards to goals? (y/n) " (playername player))
  (when (equal (read) 'y)
    (format t "Add cards implementation needed~%"))
  
  ;; Discard phase
  (discard-phase player discard-pile))

;;; ============================================================================
;;; MAIN GAME FUNCTION
;;; ============================================================================

(defun thegame (nplayers)
  "Play a complete game with NPLAYERS players."
  (assert (and (> nplayers 1) (< nplayers 7)) nil
          "Number of players must be between 2 and 6, got ~a" nplayers)
  
  (let ((start-player 0)
        (players (loop for i from 0 below nplayers
                       collect (make-player (format nil "Player~a" (1+ i))))))
    
    (loop for round-idx from 0
          for goal in round-goals
          do (format t "~%========== Round ~a: ~a Trios, ~a Sequences =========~%"
                     (1+ round-idx) (first goal) (second goal))
             
             (multiple-value-bind (deck discard-pile)
                 (setup-game players)
               
               ;; Play turns until someone empties their hand
               (loop named turns
                     for turn-count from 0
                     do (let* ((active-idx (mod (+ turn-count start-player) nplayers))
                              (active-player (nth active-idx players))
                              (others (remove active-player players)))
                          
                          (turn active-player others deck discard-pile)
                          
                          ;; Check if player emptied hand
                          (when (null (hand active-player))
                            (format t "~a emptied their hand!~%" (playername active-player))
                            (return-from turns))))
               
               ;; Count points and reset
               (loop for player in players
                     do (count-points player))
               
               (format t "~%Round ~a scores: ~{~a~^, ~}~%"
                       (1+ round-idx)
                       (mapcar #'(lambda (p) (format nil "~a: ~a" (playername p) (points p))) 
                               players))
               
               (end-round players deck discard-pile)
               (setf start-player (mod (1+ start-player) nplayers))))
    
    ;; Final scores
    (format t "~%========== Final Scores ==========~%")
    (loop for player in (sort (copy-list players)
                              #'<
                              :key #'points)
          do (format t "~a: ~a~%" (playername player) (points player)))))
