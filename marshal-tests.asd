;;; ***********************************************************
;;;
;;; Project: marshal
;;; Simple (de)serialization of Lisp datastructures.
;;;
;;; ***********************************************************

(asdf:defsystem #:marshal-tests
  :description "marshal: Simple (de)serialization of Lisp datastructures."
  :author "Michael Wolber <mwolber@gmx.de>"
  :version "1.3"
  :licence "MIT"
  :depends-on (#:xlunit #:marshal)
  :serial t
  :components ((:file "tests")))
