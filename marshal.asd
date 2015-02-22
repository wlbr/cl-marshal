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
  :version "1.0.1"
  :licence "MIT"
  :serial t
  :components ((:file "package")
               (:file "coding-idiom")
               (:file "marshal")
               (:file "unmarshal")))

