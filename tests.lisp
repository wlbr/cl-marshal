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

#-allegro
(require :marshal)

#-allegro
(require :xlunit)

(eval-when (:compile-toplevel)
  (use-package :xlunit))

(eval-when (:compile-toplevel)
  (use-package :marshal))

;;; ***********************************************************
;; definition of test classes
(defclass ship ()
  ((name :initform "" :initarg :name :accessor name)
   (dimensions :initform '(:width 0 :length 0) :initarg :dimensions :accessor dimensions)
   (course :initform 0 :initarg :course :accessor course)
   (cruise :initform 0 :initarg :cruise :accessor cruise) ; shall be transient
   (dinghy :initform nil :initarg :dinghy :accessor dinghy)) ; another ship -> ref
  (:documentation "A democlass. Some 'persistent slots', one transient.
Some numbers, string, lists and object references."))

(defgeneric ttostring (ship))

(defmethod ttostring ((self ship))
  (format nil "Vessel: ~a~%Fitting ~a degrees at ~a knots~%  Length: ~a m~%  Width: ~a m"
          (name self) (course self) (cruise self)
          (getf (dimensions self):length)
          (getf (dimensions self):width)))


(defmethod ms:class-persistent-slots ((self ship))
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
  ((aboard :initform nil :initarg :aboard :accessor aboard)) ; another ship -> circular ref
  )

;; note: intentionally misspelled
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
  (assert-equal '(:pcode 1
		  (:object 1 motorship :common-lisp-user (:simple-string 2 "Titanic")
		   (:list 3 :width 28 :length 269) 320
		   (:object 4 dinghy :common-lisp-user (:simple-string 5 "Gig")
		    (:list 6 :width 2 :length 6) 320 (:list 7) (:reference 7))))
		(marshal (ship3 self))))

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
    (assert-not-eql ht umht)))

(def-test-method dotlisttest ((self typestest) :run nil)
  (let* ((dl '((3 #(1 2) . 4) ((5 . #(1 2)))))
         (umdl (unmarshal (marshal dl))))
    (assert-equal (cddr (first umdl)) 4)
    (assert-equal (caar (second umdl)) 5)
    (assert-true (equalp dl umdl))))

(def-test-method dotlisttest-2 ((self typestest) :run nil)
  (let* ((dl (cons 1 (cons 2 3)))
         (umdl (unmarshal (marshal dl))))
    (assert-equal (cddr umdl) 3)
    (assert-equal (cadr umdl) 2)))

(def-test-method simple-circular-list ((self typestest) :run nil)
  (let ((orig (list 2 2 3 4 5 6)))
    (setf (cdr (last orig)) orig)
    (let ((restored (unmarshal (marshal orig))))
      (assert-true  (utils:circular-list-p restored))
      (assert-equal (elt restored 8) 3))))

(def-test-method simple-circular-list-2 ((self typestest) :run nil)
  (let ((orig (list (vector 1.0 2.0) (vector 3.0 4.0) 2 2 3 4 5 6)))
    (setf (cdr (last orig)) orig)
    (let ((restored (unmarshal (marshal orig))))
      (assert-true (utils:circular-list-p restored))
      (assert-true (vectorp (elt restored 0)))
      (assert-true (vectorp (elt restored 1)))
      (assert-true (floatp  (elt (elt restored 0) 0))))))

(def-test-method object-circular-list ((self typestest) :run nil)
  (let ((orig (list (make-instance 'ship :name "a")
		    (make-instance 'ship :name "b")
		    (make-instance 'ship :name "c"))))
    (setf (cdr (last orig)) orig)
    (let ((restored (unmarshal (marshal orig))))
      (assert-true (utils:circular-list-p restored))
      (assert-true (string= (name (elt restored 0)) "a")))))

(def-test-method object-circular-list-w-reference ((self typestest) :run nil)
  (let* ((object (list (make-instance 'ship :name "a")))
	 (orig     (list object 2 object 3)))
    (setf (cdr (last orig)) orig)
    (let ((restored (unmarshal (marshal orig))))
      (assert-true (utils:circular-list-p restored))
      (assert-true (eq (elt restored 0) (elt restored 2))))))

(def-test-method nested-circular-list ((self typestest) :run nil)
  (let* ((orig     '#1=(2 2 #2=(a b) (#1# #1# (#1# #2#)) 5 #2# c . #1#))
	 (restored (unmarshal (marshal orig))))
    (assert-true (utils:circular-list-p restored))
    (assert-true (eq (elt (elt orig     2) 0)
		     (elt (elt restored 2) 0)))
    (assert-true (eq (elt (elt restored 2) 0)
		     'a))
    (assert-true (eq (elt (elt restored 3) 1)
		     restored))))

(def-test-method string-vector-fill-pointer-nil ((self typestest) :run nil)
  (let* ((test-string (make-array 8
				  :element-type    'character
				  :initial-element #\a
				  :adjustable      t
				  :fill-pointer    nil))
	 (restored (unmarshal (marshal test-string))))
    (assert-true (string= restored "aaaaaaaa"))))

(def-test-method string-vector-fill-pointer-t ((self typestest) :run nil)
  (let* ((test-string (make-array 8
				  :element-type    'character
				  :initial-element #\a
				  :adjustable      t
				  :fill-pointer    t))
	 (restored (unmarshal (marshal test-string))))
    (assert-true (string= restored "aaaaaaaa"))))

(progn
  (print "Testcase Objecttest")
  (textui-test-run (get-suite objecttest))
  (print "Testcase Typestest")
  (textui-test-run (get-suite typestest)))
