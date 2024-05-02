(asdf:defsystem #:eleven.test
  :description "Tests for eleven"
  :author "RDutschTorres"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (#:eleven #:parachute)
  :pathname "test"
  :serial t
  :perform (asdf:test-op (op c) (uiop:symbol-call '#:parachute '#:test '#:eleven.test))
  :components ((:file "package")
	       (:file "eleven-tests")
	       (:file "stack-tests")))

