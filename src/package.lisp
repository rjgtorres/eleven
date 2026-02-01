;;;; package.lisp

(defpackage #:eleven
  (:use #:cl)
  (:export
   ;; Stack operations
   #:make-stack
   #:stack-push
   #:stack-pop
   #:stack-top
   #:stack-empty-p
   #:stack-see-all
   #:empty-stack
   #:shuffle-stack

   ;; Card operations
   #:card
   #:make-card
   #:face
   #:suite
   #:points
   #:jokerp
   #:iscardp

   ;; Player operations
   #:player
   #:make-player
   #:hand
   #:board
   #:playername
   #:reset
   #:draw-card
   #:discard-card
   #:empty-hand
   #:empty-board
   #:count-points
   #:see-points

   ;; Goal operations
   #:goal
   #:trio
   #:seq
   #:make-trio
   #:make-seq
   #:lay-down
   #:add-card
   #:emptyp
   #:view-goal
   #:list-all-of-card

   ;; Game operations
   #:make-deck
   #:setup-game
   #:end-round
   #:turn
   #:draw-phase
   #:discard-phase
   #:thegame
   #:sequence-order
   #:get-sequence-order
   #:sort-sequence))
