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


(defmacro coding-idiom (key)
   "Definition of the vocabulary of the generated serialization. You can 
increase verbosity or compactness by choosing your own 'language'"
   ;;; key= access code used inside the programs source code
   ;;; value= generated identifier.
   (getf '(:array :array
           :hash-table :hash-table
           :coding-identifier :pcode
           :list :list
           :dlist :dlist
           :object :object
           :string :string
           :simple-string :simple-string
           :reference :reference
           :coding-release-no 1)
     key))


;(defmacro coding-idiom (key)
;      "Definition of the vocabulary of the generated serialization. You can 
;increase verbosity or compactness by choosing your own 'language'. 
;This is a version with shorter keys, that saves an significant amount of 
;of traffic. Anyway - using NGINX e.g. in front of Hunchentoot to provide an 
;on the fly gzip compression. may be the better solution, depending on your client.
;key= access code used inside the programs source code
;value= generated identifier."
;   (getf '(:array :a
;           :hash-table :h
;           :coding-identifier :pcode
;           :list :l
;           :dlist :d
;           :object :o
;           :string :s
;           :simple-string :c
;           :reference :r
;           :coding-release-no 1.1
;           )
;     key))
