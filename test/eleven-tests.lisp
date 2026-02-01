(in-package #:eleven.test)

;;; ============================================================================
;;; CARD TESTS
;;; ============================================================================

(define-test cards)

(define-test card.creation
  :parent cards
  (let ((card (make-card 5 :points 5 :suite :clubs)))
    (is equal (face card) 5 "Card face should be 5")
    (is = (points card) 5 "Card points should be 5")
    (is equal (suite card) :clubs "Card suite should be clubs")
    (false (jokerp card) "Regular card should not be joker")))

(define-test joker
  :parent cards
  (let ((joker (make-card :joker :points 50)))
    (is equal (face joker) :joker "Joker face should be :joker")
    (is = (points joker) 50 "Joker points should be 50")
    (true (jokerp joker) "Joker should be identified as joker")
    (is equal (suite joker) nil "Joker suite should be nil")))

(define-test face-values
  :parent cards
  (let ((jack (make-card :jack :points 10 :suite :hearts))
        (queen (make-card :queen :points 10 :suite :diamonds))
        (king (make-card :king :points 10 :suite :spades))
        (ace (make-card :ace :points 25 :suite :clubs)))
    (is = (points jack) 10 "Jack should have 10 points")
    (is = (points queen) 10 "Queen should have 10 points")
    (is = (points king) 10 "King should have 10 points")
    (is = (points ace) 25 "Ace should have 25 points")))

;;; ============================================================================
;;; PLAYER TESTS
;;; ============================================================================

(define-test players)

(define-test player.creation
  :parent players
  (let ((p (make-player "TestPlayer")))
    (is equal (playername p) "TestPlayer" "Player name should be set")
    (is equal (hand p) nil "Hand should be empty initially")
    (is equal (board p) nil "Board should be empty initially")
    (is = (points p) 0 "Points should be 0 initially")))

(define-test unnamed
  :parent players
  (let ((p (make-player)))
    (is equal (playername p) "" "Unnamed player should have empty name")))

(define-test hand-management
  :parent players
  (let ((p (make-player "PlayerA"))
        (card1 (make-card 5 :points 5 :suite :clubs))
        (card2 (make-card 6 :points 6 :suite :hearts)))
    (push card1 (hand p))
    (push card2 (hand p))
    (is = (length (hand p)) 2 "Hand should have 2 cards")
    (empty-hand p)
    (is equal (hand p) nil "Hand should be empty after empty-hand")))

(define-test board-management
  :parent players
  (let ((p (make-player "PlayerA"))
        (trio (make-trio)))
    (push trio (board p))
    (is = (length (board p)) 1 "Board should have 1 goal")
    (empty-board p)
    (is equal (board p) nil "Board should be empty after empty-board")))

(define-test reset
  :parent players
  (let ((p (make-player "PlayerA")))
    (push (make-card 5 :points 5 :suite :clubs) (hand p))
    (push (make-trio) (board p))
    (setf (points p) 10)
    (reset p)
    (is equal (hand p) nil "Hand should be empty after reset")
    (is equal (board p) nil "Board should be empty after reset")
    (is = (points p) 10 "Points should not change on reset")))

(define-test draw-card
  :parent players
  (let ((p (make-player "PlayerA"))
        (deck (make-stack))
        (card (make-card 5 :points 5 :suite :clubs)))
    (stack-push card deck)
    (draw-card p deck)
    (is = (length (hand p)) 1 "Hand should have 1 card")
    (is equal (first (hand p)) card "Card should be in hand")))

(define-test discard-card
  :parent players
  (let ((p (make-player "PlayerA"))
        (discard (make-stack))
        (card (make-card 5 :points 5 :suite :clubs)))
    (push card (hand p))
    (discard-card p card discard)
    (is equal (hand p) nil "Card should be removed from hand")
    (is equal (stack-top discard) card "Card should be in discard pile")))

(define-test count-points
  :parent players
  (let ((p (make-player "PlayerA")))
    (loop repeat 3
          do (push (make-card 5 :points 5 :suite :clubs) (hand p)))
    (loop repeat 2
          do (push (make-card 10 :points 10 :suite :hearts) (hand p)))
    (count-points p)
    (is = (points p) 35 "Points should be sum of all card points")))

(define-test see-points
  :parent players
  (let ((p (make-player "PlayerA")))
    (setf (points p) 42)
    (is = (see-points p) 42 "see-points should return current points")))

;;; ============================================================================
;;; DECK TESTS
;;; ============================================================================

(define-test deck)

(define-test deck.creation
  :parent deck
  (let ((deck-list (make-deck)))
    (is = (length deck-list) 54 "Single deck should have 54 cards")
    (let ((jokers (remove-if-not #'jokerp deck-list)))
      (is = (length jokers) 2 "Single deck should have 2 joker"))))

(define-test deck.double-deck
  :parent deck
  (let ((double-deck (loop repeat 2
                           append (make-deck))))
    (is = (length double-deck) 108 "Double deck should have 108 cards (2 × 54)")
    (let ((jokers (remove-if-not #'jokerp double-deck)))
      (is = (length jokers) 4 "Double deck should have 2 jokers"))))

(define-test deck.composition
  :parent deck
  (let ((deck-list (make-deck)))
    (let ((twos (remove-if-not #'(lambda (c) (equal (face c) 2)) deck-list)))
      (is = (length twos) 4 "Single deck should have 4 twos (one per suite)"))))

(define-test deck.all-suites-present
  :parent deck
  (let ((deck-list (make-deck)))
    (let ((suites (remove-duplicates (mapcar #'suite (remove-if #'jokerp deck-list)))))
      (is = (length suites) 4 "Deck should have all 4 suites")
      (is equal suites
                '(:clubs :diamonds :hearts :spades) 
                "Deck should contain clubs, diamonds, hearts, and spades"))))

(define-test deck.all-face-values
  :parent deck
  (let ((deck-list (make-deck)))
    (let ((faces (remove-duplicates (mapcar #'face (remove-if #'jokerp deck-list)))))
      (is = (length faces) 13 "Deck should have 13 different face values")
      (is equal (sort (copy-list faces) #'(lambda (a b)
                                            (string< (format nil "~a" a)
                                                     (format nil "~a" b))))
                '(10 2 3 4 5 6 7 8 9 :ace :jack :king :queen)
                "Deck should contain all face values from 2 to 10, J, Q, K, A"))))

(define-test deck.point-values
  :parent deck
  (let ((deck-list (make-deck)))
    ;; Check aces
    (let ((aces (remove-if-not #'(lambda (c) (equal (face c) :ace)) deck-list)))
      (is = (length aces) 4 "Should have 4 aces")
      (loop for ace in aces
            do (is = (points ace) 25 "All aces should have 25 points")))
    ;; Check face cards (J, Q, K)
    (let ((faces (remove-if-not #'(lambda (c) (member (face c) '(:jack :queen :king))) deck-list)))
      (is = (length faces) 12 "Should have 12 face cards (4 per J, Q, K)")
      (loop for face in faces
            do (is = (points face) 10 "All face cards should have 10 points")))
    ;; Check number cards (2-10)
    (let ((numbers (remove-if-not #'(lambda (c) (and (numberp (face c)) (>= (face c) 2) (<= (face c) 10))) deck-list)))
      (is = (length numbers) 36 "Should have 36 number cards (9 per suite)")
      (loop for num-card in numbers
            do (is = (points num-card) (face num-card) 
                     (format nil "Card ~a should have ~a points" (face num-card) (face num-card)))))))

(define-test deck.joker-properties
  :parent deck
  (let ((deck-list (make-deck)))
    (let ((jokers (remove-if-not #'jokerp deck-list)))
      (is = (length jokers) 2 "Should have 2 joker in single deck")
      (loop for joker in jokers
            do (progn
                 (is = (points joker) 50 "Joker should have 50 points")
                 (is equal (suite joker) nil "Joker should have no suite"))))))

(define-test deck.in-stack
  :parent deck
  (let ((stack (make-stack)))
    (loop repeat 2
          do (dolist (card (make-deck))
               (stack-push card stack)))
    (is = (length (stack-see-all stack)) 108 "Stack with double deck should have 108 cards")
    (shuffle-stack stack)
    (is = (length (stack-see-all stack)) 108 "Shuffling should preserve all cards")))

;;; ============================================================================
;;; TRIO TESTS
;;; ============================================================================

(define-test trios)

(define-test trio.creation
  :parent trios
  (let ((trio (make-trio)))
    (true (emptyp trio) "New trio should be empty")))

(define-test lay-down-valid
  :parent trios
  (let ((trio (make-trio)))
    (lay-down (list (make-card 2 :suite :clubs)
                    (make-card 2 :suite :hearts)
                    (make-card 2 :suite :diamonds))
              trio)
    (false (emptyp trio) "Trio should not be empty after lay-down")
    (is = (length (view-goal trio)) 3 "Trio should have 3 cards")))

(define-test lay-down-with-jokers
  :parent trios
  (let ((trio (make-trio)))
    (lay-down (list (make-card 5 :suite :clubs)
                    (make-card :joker)
                    (make-card 5 :suite :diamonds))
              trio)
    (false (emptyp trio) "Trio with joker should be valid")))

(define-test lay-down-two-jokers
  :parent trios
  (let ((trio (make-trio)))
    (lay-down (list (make-card 7 :suite :clubs)
                    (make-card :joker)
                    (make-card :joker))
              trio)
    (false (emptyp trio) "Trio with two jokers should be valid")))

(define-test lay-down-invalid-all-jokers
  :parent trios
  (let ((trio (make-trio)))
    (fail (lay-down (list (make-card :joker)
                          (make-card :joker)
                          (make-card :joker))
                    trio)
          simple-error "Trio cannot have three jokers")))

(define-test lay-down-invalid-different-faces
  :parent trios
  (let ((trio (make-trio)))
    (fail (lay-down (list (make-card 2 :suite :clubs)
                          (make-card 3 :suite :hearts)
                          (make-card 2 :suite :diamonds))
                    trio)
          simple-error "All cards in trio must have same face")))

(define-test lay-down-invalid-duplicate-suite
  :parent trios
  (let ((trio (make-trio)))
    (fail (lay-down (list (make-card 2 :suite :clubs)
                          (make-card 2 :suite :hearts)
                          (make-card 2 :suite :clubs))
                    trio)
          simple-error "Trio cards must have different suites")))

(define-test lay-down-invalid-wrong-size
  :parent trios
  (let ((trio (make-trio)))
    (fail (lay-down (list (make-card 2 :suite :clubs)
                          (make-card 2 :suite :hearts))
                    trio)
          simple-error "Trio must have exactly 3 cards")))

(define-test lay-down-invalid-already-full
  :parent trios
  (let ((trio (make-trio)))
    (lay-down (list (make-card 2 :suite :clubs)
                    (make-card 2 :suite :hearts)
                    (make-card 2 :suite :diamonds))
              trio)
    (fail (lay-down (list (make-card 3 :suite :clubs)
                          (make-card 3 :suite :hearts)
                          (make-card 3 :suite :diamonds))
                    trio)
          simple-error "Cannot lay down on already full trio")))

(define-test add-card-valid
  :parent trios
  (let ((trio (make-trio)))
    (lay-down (list (make-card 2 :suite :clubs)
                    (make-card 2 :suite :hearts)
                    (make-card 2 :suite :diamonds))
              trio)
    (true (add-card (make-card 2 :suite :clubs) 0 trio))
    (is = (length (first (view-goal trio))) 2 "Position 0 should have 2 cards now")))

(define-test add-card-joker
  :parent trios
  (let ((trio (make-trio)))
    (lay-down (list (make-card 5 :suite :clubs)
                    (make-card :joker)
                    (make-card 5 :suite :diamonds))
              trio)
    (true (add-card (make-card :joker) 0 trio))
    (is = (length (first (view-goal trio))) 2 "Should add joker on top")))

(define-test add-card-invalid-joker-on-joker
  :parent trios
  (let ((trio (make-trio)))
    (lay-down (list (make-card 5 :suite :clubs)
                    (make-card :joker)
                    (make-card 5 :suite :diamonds))
              trio)
    (fail (add-card (make-card :joker) 1 trio)
          simple-error "Cannot add joker on top of joker")))

(define-test add-card-invalid-wrong-face
  :parent trios
  (let ((trio (make-trio)))
    (lay-down (list (make-card 2 :suite :clubs)
                    (make-card 2 :suite :hearts)
                    (make-card 2 :suite :diamonds))
              trio)
    (fail (add-card (make-card 3 :suite :spades) 0 trio)
          simple-error "Card must match existing card")))

(define-test add-card-invalid-empty-trio
  :parent trios
  (let ((trio (make-trio)))
    (fail (add-card (make-card 2 :suite :clubs) 0 trio)
          simple-error "Cannot add to empty trio")))

;;; ============================================================================
;;; SEQUENCE TESTS
;;; ============================================================================

(define-test sequences)

(define-test sequence.creation
  :parent sequences
  (let ((seq (make-seq)))
    (true (emptyp seq) "New sequence should be empty")))

(define-test lay-down-valid-basic
  :parent sequences
  (let ((seq (make-seq)))
    (lay-down (list (make-card 2 :suite :clubs)
                    (make-card 3 :suite :clubs)
                    (make-card 4 :suite :clubs)
                    (make-card 5 :suite :clubs))
              seq)
    ;; (false (emptyp seq) "Sequence should not be empty after lay-down")
    ;; (is = (length (view-goal seq)) 4 "Sequence should have 4 cards")
    ))

(define-test lay-down-valid-long
  :parent sequences
  (let ((seq (make-seq)))
    (lay-down (list (make-card 5 :suite :hearts)
                    (make-card 6 :suite :hearts)
                    (make-card 7 :suite :hearts)
                    (make-card 8 :suite :hearts)
                    (make-card 9 :suite :hearts))
              seq)
    (is = (length (view-goal seq)) 5 "Long sequence should be valid")))

(define-test lay-down-with-joker
  :parent sequences
  (let ((seq (make-seq)))
    (lay-down (list (make-card 5 :suite :diamonds)
                    (make-card :joker)
                    (make-card 7 :suite :diamonds)
                    (make-card 8 :suite :diamonds))
              seq)
    (false (emptyp seq) "Sequence with joker should be valid")))

(define-test lay-down-with-two-jokers
  :parent sequences
  (let ((seq (make-seq)))
    (lay-down (list (make-card :joker)
                    (make-card 6 :suite :spades)
                    (make-card :joker)
                    (make-card 8 :suite :spades))
              seq)
    (false (emptyp seq) "Sequence with two jokers should be valid")))

(define-test lay-down-invalid-three-jokers
  :parent sequences
  (let ((seq (make-seq)))
    (fail (lay-down (list (make-card :joker)
                          (make-card :joker)
                          (make-card :joker)
                          (make-card 8 :suite :spades))
                    seq)
          simple-error "Sequence cannot have more than 2 jokers")))

(define-test lay-down-invalid-too-short
  :parent sequences
  (let ((seq (make-seq)))
    (fail (lay-down (list (make-card 2 :suite :clubs)
                          (make-card 3 :suite :clubs)
                          (make-card 4 :suite :clubs))
                    seq)
          simple-error "Sequence must have at least 4 cards")))

(define-test lay-down-invalid-different-suites
  :parent sequences
  (let ((seq (make-seq)))
    (fail (lay-down (list (make-card 2 :suite :clubs)
                          (make-card 3 :suite :hearts)
                          (make-card 4 :suite :diamonds)
                          (make-card 5 :suite :spades))
                    seq)
          simple-error "All cards must be of same suite")))

(define-test lay-down-invalid-duplicate-face
  :parent sequences
  (let ((seq (make-seq)))
    (fail (lay-down (list (make-card 2 :suite :clubs)
                          (make-card 3 :suite :clubs)
                          (make-card 2 :suite :clubs)
                          (make-card 5 :suite :clubs))
                    seq)
          simple-error "Sequence cannot have duplicate faces")))

(define-test lay-down-invalid-non-consecutive
  :parent sequences
  (let ((seq (make-seq)))
    (fail (lay-down (list (make-card 2 :suite :clubs)
                          (make-card 3 :suite :clubs)
                          (make-card 5 :suite :clubs)
                          (make-card 6 :suite :clubs))
                    seq)
          simple-error "Cards must be consecutive")))

(define-test lay-down-invalid-jokers-adjacent
  :parent sequences
  (let ((seq (make-seq)))
    (fail (lay-down (list (make-card :joker)
                          (make-card :joker)
                          (make-card 7 :suite :clubs)
                          (make-card 8 :suite :clubs))
                    seq)
          simple-error "Sequence cannot have two consecutive jokers")))

(define-test add-card-end
  :parent sequences
  (let ((seq (make-seq)))
    (lay-down (list (make-card 2 :suite :clubs)
                    (make-card 3 :suite :clubs)
                    (make-card 4 :suite :clubs)
                    (make-card 5 :suite :clubs))
              seq)
    (true (add-card (make-card 6 :suite :clubs) :end seq))
    (is = (length (view-goal seq)) 5 "Sequence should have 5 cards")))

(define-test add-card-begin
  :parent sequences
  (let ((seq (make-seq)))
    (lay-down (list (make-card 3 :suite :diamonds)
                    (make-card 4 :suite :diamonds)
                    (make-card 5 :suite :diamonds)
                    (make-card 6 :suite :diamonds))
              seq)
    (true (add-card (make-card 2 :suite :diamonds) :begin seq))
    (is = (length (view-goal seq)) 5 "Sequence should have 5 cards")))

(define-test add-card-replace-joker
  :parent sequences
  (let ((seq (make-seq)))
    (lay-down (list (make-card 5 :suite :hearts)
                    (make-card :joker)
                    (make-card 7 :suite :hearts)
                    (make-card 8 :suite :hearts))
              seq)
    (true (add-card (make-card 6 :suite :hearts) 1 seq))
    (is = (length (view-goal seq)) 4 "Sequence length should remain 4")))

(define-test add-card-invalid-empty
  :parent sequences
  (let ((seq (make-seq)))
    (fail (add-card (make-card 5 :suite :clubs) :end seq)
          simple-error "Cannot add to empty sequence")))

(define-test add-card-invalid-duplicate
  :parent sequences
  (let ((seq (make-seq)))
    (lay-down (list (make-card 2 :suite :clubs)
                    (make-card 3 :suite :clubs)
                    (make-card 4 :suite :clubs)
                    (make-card 5 :suite :clubs))
              seq)
    (fail (add-card (make-card 3 :suite :clubs) :end seq)
          simple-error "Card already exists in sequence")))

(define-test add-card-invalid-suite
  :parent sequences
  (let ((seq (make-seq)))
    (lay-down (list (make-card 2 :suite :clubs)
                    (make-card 3 :suite :clubs)
                    (make-card 4 :suite :clubs)
                    (make-card 5 :suite :clubs))
              seq)
    (fail (add-card (make-card 6 :suite :hearts) :end seq)
          simple-error "Card must be of same suite")))

(define-test add-card-invalid-joker-placement
  :parent sequences
  (let ((seq (make-seq)))
    (lay-down (list (make-card 2 :suite :clubs)
                    (make-card :joker)
                    (make-card 4 :suite :clubs)
                    (make-card 5 :suite :clubs))
              seq)
    (fail (add-card (make-card :joker) :begin seq)
          simple-error "Cannot add joker next to another joker")))

(define-test lay-down-valid-ace-low
  :parent sequences
  (let ((seq (make-seq)))
    (lay-down (list (make-card :ace :suite :clubs)
                    (make-card 2 :suite :clubs)
                    (make-card 3 :suite :clubs)
                    (make-card 4 :suite :clubs))
              seq)
    (false (emptyp seq) "Sequence with ace at beginning should be valid")
    (is = (length (view-goal seq)) 4 "Ace-low sequence should have 4 cards")))

(define-test lay-down-valid-ace-high
  :parent sequences
  (let ((seq (make-seq)))
    (lay-down (list (make-card 10 :suite :hearts)
                    (make-card :jack :suite :hearts)
                    (make-card :queen :suite :hearts)
                    (make-card :king :suite :hearts)
                    (make-card :ace :suite :hearts))
              seq)
    (false (emptyp seq) "Sequence with ace at end should be valid")
    (is = (length (view-goal seq)) 5 "Ace-high sequence should have 5 cards")))

(define-test lay-down-invalid-ace-wrap
  :parent sequences
  (let ((seq (make-seq)))
    (fail (lay-down (list (make-card :king :suite :clubs)
                          (make-card :ace :suite :clubs)
                          (make-card 2 :suite :clubs)
                          (make-card 3 :suite :clubs))
                    seq)
          simple-error "Sequence cannot wrap around with ace")))

(define-test lay-down-invalid-ace-wrap-middle
  :parent sequences
  (let ((seq (make-seq)))
    (fail (lay-down (list (make-card 10 :suite :spades)
                          (make-card :jack :suite :spades)
                          (make-card :ace :suite :spades)
                          (make-card 2 :suite :spades))
                    seq)
          simple-error "Ace cannot be in middle of sequence")))

(define-test add-card-ace-extend-low
  :parent sequences
  (let ((seq (make-seq)))
    (lay-down (list (make-card :ace :suite :diamonds)
                    (make-card 2 :suite :diamonds)
                    (make-card 3 :suite :diamonds)
                    (make-card 4 :suite :diamonds))
              seq)
    (fail (add-card (make-card 5 :suite :diamonds) :begin seq)
          simple-error "Cannot extend before ace in low sequence")))

(define-test add-card-ace-extend-high
  :parent sequences
  (let ((seq (make-seq)))
    (lay-down (list (make-card 10 :suite :spades)
                    (make-card :jack :suite :spades)
                    (make-card :queen :suite :spades)
                    (make-card :king :suite :spades)
                    (make-card :ace :suite :spades))
              seq)
    (fail (add-card (make-card 9 :suite :spades) :end seq)
          simple-error "Cannot extend after ace in high sequence")))

(define-test lay-down-valid-ace-2-3-sequence
  :parent sequences
  (let ((seq (make-seq)))
    (lay-down (list (make-card :ace :suite :clubs)
                    (make-card 2 :suite :clubs)
                    (make-card 3 :suite :clubs)
                    (make-card 4 :suite :clubs)
                    (make-card 5 :suite :clubs))
              seq)
    (false (emptyp seq) "Ace-2-3 sequence should be valid")
    (is = (length (view-goal seq)) 5 "Should have 5 cards")))

(define-test lay-down-valid-high-sequence-no-wrap
  :parent sequences
  (let ((seq (make-seq)))
    (lay-down (list (make-card 9 :suite :hearts)
                    (make-card 10 :suite :hearts)
                    (make-card :jack :suite :hearts)
                    (make-card :queen :suite :hearts)
                    (make-card :king :suite :hearts))
              seq)
    (false (emptyp seq) "High sequence without ace should be valid")))

;;; ============================================================================
;;; GAME SETUP TESTS
;;; ============================================================================

(define-test game-setup)

(define-test (game-setup valid)
  (let ((players (list (make-player "P1") (make-player "P2"))))
    (multiple-value-bind (deck discard-pile)
        (setup-game players)
      (loop for player in players
            do (is = (length (hand player)) 11 
                     (format nil "Player ~a should have 11 cards" (playername player))))
      (true (typep deck 'eleven::stack) "Deck should be a stack")
      (true (typep discard-pile 'eleven::stack) "Discard pile should be a stack"))))

(define-test (game-setup reset-players)
  (let ((players (list (make-player "P1") (make-player "P2"))))
    ;; Simulate previous state
    (loop for p in players
          do (progn
               (push (make-card 5 :points 5 :suite :clubs) (hand p))
               (push (make-trio) (board p))
               (setf (points p) 100)))
    ;; Setup new game
    (multiple-value-bind (deck discard-pile)
        (setup-game players)
      (declare (ignore deck discard-pile))
      (loop for player in players
            do (progn
                 (is = (length (hand player)) 11 "Hand should be reset to 11 cards")
                 (is equal (board player) nil "Board should be empty")
                 (is = (points player) 0 "Points should be reset to 0"))))))

(define-test (game-setup end-round)
  (let ((players (list (make-player "P1") (make-player "P2"))))
    (loop for p in players
          do (progn
               (loop repeat 5
                     do (push (make-card 5 :points 5 :suite :clubs) (hand p)))
               (push (make-trio) (board p))
               (setf (points p) 10)))
    (let ((deck (make-stack))
          (discard-pile (make-stack)))
      (stack-push (make-card 5 :points 5 :suite :clubs) deck)
      (end-round players deck discard-pile)
      (loop for player in players
            do (progn
                 (is = (points player) 35 
                      (format nil "Player ~a should have accumulated points" (playername player)))
                 (is equal (hand player) nil "Hand should be empty after round end")
                 (is equal (board player) nil "Board should be empty after round end")))
      (true (stack-empty-p deck) "Deck should be empty")
      (true (stack-empty-p discard-pile) "Discard pile should be empty"))))

(define-test (game-setup invalid-player-count)
  (fail (thegame 1) simple-error "Game with 1 player should fail")
  (fail (thegame 7) simple-error "Game with 7 players should fail")
  (fail (thegame 0) simple-error "Game with 0 players should fail"))

;;; ============================================================================
;;; HELPER FUNCTION TESTS
;;; ============================================================================

(define-test helpers)

(define-test sequence-order-high-low
  :parent helpers
  (let ((cards (list (make-card 2 :suite :clubs)
                     (make-card 5 :suite :clubs))))
    (let ((order (get-sequence-order cards)))
      (is equal (first order) :ace "2 and 3 present should exclude highest ace"))))

(define-test sequence-order-normal
  :parent helpers
  (let ((cards (list (make-card 5 :suite :clubs)
                     (make-card 7 :suite :clubs))))
    (let ((order (get-sequence-order cards)))
      (is equal (first order) 2 "Normal sequence should start at 2"))))

(define-test iscardp
  :parent helpers
  (let ((card (make-card 5 :suite :clubs)))
    (true (iscardp 5 card) "Should identify card face")
    (false (iscardp 6 card) "Should not match different face")))

;;; ============================================================================
;;; INTEGRATION TESTS
;;; ============================================================================

(define-test integration)

(define-test (integration game-flow)
  (let* ((p1 (make-player "Alice"))
         (p2 (make-player "Bob"))
         (players (list p1 p2)))
    (multiple-value-bind (deck discard-pile)
        (setup-game players)
      ;; Both players should have cards
      (is = (length (hand p1)) 11 "Player 1 should have 11 cards")
      (is = (length (hand p2)) 11 "Player 2 should have 11 cards")
      
      ;; Simulate a draw
      (let ((initial-deck-size (length (stack-see-all deck)))
            (initial-p1-size (length (hand p1))))
        (draw-card p1 deck)
        (is = (length (hand p1)) (1+ initial-p1-size) "Player 1 should have one more card")))))

(define-test (integration player-trio-play)
  (let ((player (make-player "TestPlayer")))
    ;; Create three matching cards
    (let ((cards (list (make-card 8 :suite :clubs)
                       (make-card 8 :suite :hearts)
                       (make-card 8 :suite :diamonds))))
      (loop for card in cards
            do (push card (hand player)))
      
      ;; Create and lay down trio
      (let ((trio (make-trio)))
        (lay-down cards trio)
        (push trio (board player))
        
        ;; Add to board and remove from hand
        (loop for card in cards
              do (setf (hand player) (remove card (hand player) :count 1)))
        
        (is = (length (hand player)) 0 "Hand should be empty")
        (is = (length (board player)) 1 "Board should have one goal")))))

(define-test (integration player-sequence-play)
  (let ((player (make-player "TestPlayer")))
    ;; Create four consecutive cards
    (let ((cards (list (make-card 3 :suite :spades)
                       (make-card 4 :suite :spades)
                       (make-card 5 :suite :spades)
                       (make-card 6 :suite :spades))))
      (loop for card in cards
            do (push card (hand player)))
      
      ;; Create and lay down sequence
      (let ((seq (make-seq)))
        (lay-down cards seq)
        (push seq (board player))
        
        ;; Remove from hand
        (loop for card in cards
              do (setf (hand player) (remove card (hand player) :count 1)))
        
        (is = (length (hand player)) 0 "Hand should be empty")
        (is = (length (board player)) 1 "Board should have one goal")
        
        ;; Add to sequence
        (add-card (make-card 7 :suite :spades) :end seq)
        (is = (length (view-goal seq)) 5 "Sequence should have 5 cards")))))
