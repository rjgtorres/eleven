;;;; eleven.asd

(asdf:defsystem #:eleven
  :name "eleven"
  :description "Describe eleven here"
  :author "RDutschTorres"
  :license  "Specify license here"
  :version "0.0.1"
  :encoding :utf-8
  :depends-on (#:clog)
  :in-order-to ((asdf:test-op (asdf:test-op #:eleven.test)))
  :pathname "src"
  :serial t
  :components ((:file "package")
	       (:file "stack")
               (:file "eleven")))
