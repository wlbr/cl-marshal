;;; -*- Mode:LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10; indent-tabs-mode: nil -*-

;;; ***********************************************************
;;;
;;; Project: marshal
;;; Simple (de)serialization of Lisp datastructures.
;;; 
;;; File: tests.lisp
;;;
;;; Provides a decent test suite.
;;; Not included in asdf system definition.
;;; Requires unit test suite xpunit (available via quicklisp e.g.)
;;; 
;;; ***********************************************************


(in-package :cl-user)

(require :marshal)
(require :xlunit)

(use-package :xlunit)
(use-package :marshal)



;;; ***********************************************************
;; definition of test classes
(defclass ship () 
  ((name :initform "" :initarg :name :accessor name)
   (dimensions :initform '(:width 0 :length 0) :initarg :dimensions :accessor dimensions)
   (course :initform 0 :initarg :course :accessor course)
   (cruise :initform 0 :initarg :cruise :accessor cruise) ; shall be transient
   (dinghy :initform NIL :initarg :dinghy :accessor dinghy :initarg :dinghy)) ; another ship -> ref
  (:documentation "A democlass. Some 'persistant slots', one transient. 
Some numbers, string, lists and object references."))
 
(defgeneric ttostring (ship))

(defmethod ttostring ((self ship))
  (format nil "Vessel: ~a~%Fitting ~a degrees at ~a knots~%  Length: ~a m~%  Width: ~a m" 
	   (name self) (course self) (cruise self)
	   (getf (dimensions self):length)
	   (getf (dimensions self):width)))

(defmethod ms:class-persistant-slots ((self ship))
  '(name dimensions course dinghy))


(defclass sailingship (ship)
  ((sailarea :initform 0 :initarg :sailarea :accessor sailarea))
  )

(defmethod ttostring ((self sailingship))
  (format nil "~a~%  Sail area: ~a sqm" (call-next-method) (sailarea self))
)


(defclass motorship (ship)
  ((enginepower :initform 0 :initarg :enginepower :accessor enginepower))
)

(defmethod ttostring ((self motorship))
  (format nil "~a~%  Engine power: ~a hp" (call-next-method) (enginepower self))
)

(defclass motorsailor (motorship sailingship)
  ()
) 

(defclass dinghy (ship)
  ((aboard :initform NIL :initarg :aboard :accessor aboard)) ; another ship -> circular ref
) 

(defmethod ms:class-persistant-slots ((self dinghy))
  (append (call-next-method) '(aboard)))


;;; ***********************************************************


(defclass basetest (test-case)
  ((ship1 :accessor ship1 :initarg :ship1)
   (ship2 :accessor ship2 :initarg :ship2)
    (ship3 :accessor ship3 :initarg :ship3)
    (ship4 :accessor ship4 :initarg :ship4)
    (ship5 :accessor ship5 :initarg :ship5)
    (ship6 :accessor ship6 :initarg :ship6)
    (ships :accessor ships :initarg :ships)) 
 )


(defmethod set-up ((self basetest))
  (setf (ship1 self) (make-instance 'ship :name "Ark" :course 360 :dimensions '(:width 30 :length 90)))
  (setf (ship2 self) (make-instance 'sailingship :name "Pinta" :course 270 :cruise 9 
				   :dimensions '(:width 7 :length 21) :sailarea 400))
  (setf (ship3 self) (make-instance 'motorship :name "Titanic" :course 320 :cruise 21 
				   :dimensions '(:width 28 :length 269) :enginepower 51000))
  (setf (ship4 self) (make-instance 'motorsailor  :name "Gorch Fock" :course 180
				   :cruise 18 :dimensions '(:width 12 :length 82) 
				   :sailarea 2037 :enginepower 1660))
  (setf (ship5 self) (make-instance 'dinghy :name "Gig" :course 320 :cruise 5 
				:dimensions '(:width 2 :length 6)))
  (setf (ship6 self) (make-instance 'dinghy :name "Gig" :course 320 :cruise 5 
				:dimensions '(:width 2 :length 6) :aboard (ship4 self)))
  
  (setf (dinghy (ship3 self)) (ship5 self))  ; ref only
  (setf (dinghy (ship4 self)) (ship6 self))  ; -> circle
  (setf (ships self) (list (ship1 self) (ship2 self) (ship4 self) (ship6 self)))
)



(defclass objecttest (basetest)
 ()
 )



(def-test-method test-objectref ((self objecttest) :run nil)
  (assert-equal '(:PCODE 1
                  (:OBJECT 1 MOTORSHIP (:SIMPLE-STRING 2 "Titanic")
                   (:LIST 3 :WIDTH 28 :LENGTH 269) 320
                   (:OBJECT 4 DINGHY (:SIMPLE-STRING 5 "Gig") (:LIST 6 :WIDTH 2 :LENGTH 6)
                    320 (:LIST 7) (:REFERENCE 7))))
		(marshal (ship3 self)))
  )

(def-test-method test-objectcircle ((self objecttest) :run nil)
  (let ((uma (unmarshal (marshal (ship4 self)))))
    (assert-eql uma 
	       (aboard (dinghy uma)))
  ))

(def-test-method test-listednestedobjects ((self objecttest) :run nil)
  (let ((umshs (unmarshal (marshal (ships self)))))
     (assert-eql (fourth umshs)
                (dinghy (third umshs)))
    (assert-eql (third umshs)
                (aboard (fourth umshs))))
)





(defclass typestest (basetest)
  ()
  )

(def-test-method simplearraytest ((self typestest) :run nil)
  (let* ((arr (make-array '(10) :initial-contents '(0 1 2 3 4 5 6 7 8 9)))
         (umarr (unmarshal (marshal arr))))
    (assert-eql 2 (aref umarr 2))
    (assert-eql 8 (aref umarr 8))
    (assert-eql 9 (aref umarr 9))
    (assert-not-eql arr umarr)
    ))


(def-test-method complexarraytest ((self typestest) :run nil)
  (let* ((arr (make-array '(4 3 2) :initial-contents '(((1 2) (2 3) (3 4))
                                                       ((11 12) (12 13) (13 14))
                                                       ((21 22) (22 23) (23 24))
                                                       ((31 32) (32 33) (33 34)) )))
         (umarr (unmarshal (marshal arr))))
    (assert-eql 24 (aref umarr 2 2 1))
    (assert-eql 1 (aref umarr 0 0 0))
    (assert-eql 12 (aref umarr 1 1 0))
    (assert-not-eql arr umarr)
    ))

(def-test-method objectarraytest ((self typestest) :run nil)
  (let* ((arr (make-array '(2 3) :initial-contents (list (list (ship1 self) (ship2 self) (ship3 self))
                                                     (list (ship4 self) (ship5 self) (ship6 self)) )))
         (umarr (unmarshal (marshal arr))))
    (assert-eql (dinghy (aref umarr 1 0)) (aref umarr 1 2))
    (assert-eql (aref umarr 1 0) (aboard (aref umarr 1 2)))
    (assert-not-eql arr umarr)
    ))

(def-test-method hashtabletest ((self typestest) :run nil)
  (let* ((ht (make-hash-table))
         (umht (unmarshal (marshal ht))))
    (setf (gethash 1 ht) (ship1 self))
    (setf (gethash 2 ht) (ship2 self))
    (setf (gethash 3 ht) (ship3 self))
    (setf (gethash 4 ht) (ship4 self))
    (setf (gethash 5 ht) (ship5 self))
    (setf (gethash 6 ht) (ship6 self))
   
    (assert-eql (dinghy (gethash 4 ht)) (gethash 6 ht))
    (assert-eql (gethash 4 ht) (aboard (gethash 6 ht)))
    (assert-not-eql ht umht)
    ))



(progn
  (print "Testcase Objecttest")
  (textui-test-run (get-suite objecttest))
  (print "Testcase Typestest")
  (textui-test-run (get-suite typestest)))
