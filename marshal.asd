;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10; indent-tabs-mode: nil -*-

;;; ***********************************************************
;;;
;;; Project: marshal
;;; Simple (de)serialization of Lisp datastructures.
;;;
;;; File: package.lsp
;;;
;;; ***********************************************************

(asdf:defsystem :marshal
  :description "marshal: Simple (de)serialization of Lisp datastructures."
  :author "Michael Wolber <mwolber@gmx.de>"
  :version "1.3"
  :licence "MIT"
  :serial t
  :components ((:file "package")
	       (:file "utils")
	       (:file "serialization-format")
               (:file "coding-idiom")
               (:file "marshal")
               (:file "unmarshal")))

; (asdf:load-system "marshal")
