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

(defpackage :marshal
  (:use :cl)
  (:nicknames :ms)
  (:export :marshal
           :unmarshal
           :class-persistant-slots
	   :initialize-unmarshalled-instance))

