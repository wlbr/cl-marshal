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

(require 'asdf)

(defpackage :utils
  (:use :cl)
  (:export
   :circular-list-p
   :dotted-list-p
   :proper-list-p))

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
