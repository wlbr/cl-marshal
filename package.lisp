;;; -*- Mode:LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10; indent-tabs-mode: nil -*-

;;; ***********************************************************
;;;
;;; Project: marshal
;;; Simple (de)serialization of Lisp datastructures.
;;;
;;; File: package.lisp
;;;
;;; ***********************************************************
(in-package :cl-user)

(defpackage :utils
  (:use :cl)
  (:export
   :circular-list-p
   :dotted-list-p
   :proper-list-p))

(defpackage :serialization-format
  (:use :cl)
  (:nicknames :fmt)
  (:export
   :id
   :reference-id
   :data-type
   :class-slots-values
   :list-values
   :array-values
   :array-sizes
   :array-elements-type
   :ht-values
   :ht-size
   :ht-rehash-size
   :ht-rehash-threshold
   :ht-test-fn
   :ht-hash-fn
   :simple-string-value
   :string-value
   :string-adjustable-p
   :string-fill-pointer
   :object-class-name
   :object-package-name))

(defpackage :marshal
  (:use :cl)
  (:nicknames :ms)
  (:export
   :*idiom-table*
   :marshal
   :unmarshal
   :class-persistent-slots
   :class-persistant-slots
   :initialize-unmarshalled-instance))
