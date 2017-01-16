;;; -*- Mode:LISP; Syntax: COMMON-LISP; Package: MARSHAL; Base: 10; indent-tabs-mode: nil -*-

;;; ***********************************************************
;;;
;;; Project: marshal
;;; Simple (de)serialization of Lisp datastructures.
;;;
;;; File: coding-idiom.lsp
;;;
;;; ***********************************************************

;;; exported symbols: ---

(in-package :marshal)

(defconstant +reference-placeholder+ :placeholder)

(defparameter *idiom-table* '(:array             :array
                              :hash-table        :hash-table
                              :coding-identifier :pcode
                              :list              :list
                              :dlist             :dlist
                              :circular-list     :clist
                              :object            :object
                              :string            :string
                              :simple-string     :simple-string
                              :reference         :reference
                              :coding-release-no 1)
     "Definition of the vocabulary of the generated serialization. You can
increase verbosity or compactness by choosing your own 'language'.
key= access code used inside the programs source code
value= generated identifier.")

(defun coding-idiom (key)
  "Definition of the vocabulary of the generated serialization.You can
increase verbosity or compactness by choosing your own 'language'. Simply
define your own vocabulary and redefine the variable ms:*idiom-table*."
  ;;; key= access code used inside the programs source code
  ;;; value= generated identifier.
  (getf *idiom-table* key))


;;(defparameter *idiom-table* '(:array             :a
;;                              :hash-table        :h
;;                              :coding-identifier :pcode
;;                              :list              :l
;;                              :dlist             :d
;;                              :circular-list     :i
;;                              :object            :o
;;                              :string            :s
;;                              :simple-string     :c
;;                              :reference         :r
;;                              :coding-release-no 1.1)
;;     "Definition of the vocabulary of the generated serialization. You can
;;increase verbosity or compactness by choosing your own 'language'.
;;key= access code used inside the programs source code
;;value= generated identifier.")
