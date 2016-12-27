;;; -*- Mode:LISP; Syntax: COMMON-LISP; Package: MARSHAL; Base: 10; indent-tabs-mode: nil -*-


;;; ***********************************************************
;;;
;;; Project: marshal
;;; Simple (de)serialization of Lisp datastructures.
;;;
;;; File: marshal.lisp
;;;
;;; ***********************************************************

(in-package :marshal)

;;; =============================================================

(defgeneric initialize-unmarshalled-instance (object)
  (:documentation  "Called as the last step of the deserialization of an object.
!Must return the object!!"))

(defmethod initialize-unmarshalled-instance (object)
   "Called as the last step of the deserialization of an object.
!Must return the object!!"
   object)

;;; =============================================================

(defgeneric unmarshal (thing)
  (:documentation "Returns an object when called with a wellformed marshal sexp."))

(defmethod unmarshal (thing)
  (if (and (not (null thing))
	   (listp thing))
      (if (eq (coding-idiom :coding-identifier) (first thing))
	  (if (and (not (null (third thing))) (listp (third thing)))
	      (unmarshal-fn (second thing) (first (third thing)) (third thing))
	      (unmarshal-fn (second thing) T (third thing)))
	  thing)
      thing))

(defgeneric unmarshal-fn (version type token &optional circle-hash))

(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no))) type token
			 &optional (circle-hash nil))
  (declare (ignore type circle-hash))
  token)

(defmethod unmarshal-fn :around ((version (eql (coding-idiom :coding-release-no))) type token
				 &optional (circle-hash nil))
   (let ((result nil))
     (if circle-hash
        (progn
          (setq result (call-next-method version type token circle-hash))
          (if (listp token)
	      (setf (gethash (second token) circle-hash) result)
	      result))
        (progn
          (setq circle-hash (make-hash-table :test #'eq :size 50 :rehash-size 1.5))
          (call-next-method version type token circle-hash)))))

(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
			 (type (eql (coding-idiom :reference))) token
			 &optional (circle-hash nil))
   (gethash (second token) circle-hash))

;;;  07.07.98 cjo: LOOP
(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
			 (type (eql (coding-idiom :object))) token
			 &optional (circle-hash nil))
   (let* ((out    (make-instance (third token)))
          (slots  (class-persistant-slots out))
          (values (cdddr token)))
      (setf (gethash (second token) circle-hash) out)
      (loop
	 for slot in slots
	 for value in values
	 do (if (listp value)
		(setf (slot-value out slot)
		      (unmarshal-fn version (first value) value circle-hash))
		(setf (slot-value out slot)
		      (unmarshal-fn version T value circle-hash))))
      (initialize-unmarshalled-instance out)))

;;;  07.07.98 cjo: LOOP (Faktor 3 schneller :)
;;; 12.02.99 cjo: aufgespalten wegen dotted lists
(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
			 (type (eql (coding-idiom :list))) token
			 &optional (circle-hash nil))
   (let ((out nil))
     (setf (gethash (second token) circle-hash)
	   out)
     (loop for walker in (cddr token) collect
	  (if (listp walker)
	      (unmarshal-fn version (first walker) walker circle-hash)
	      (unmarshal-fn version T walker circle-hash)))))

(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                      (type (eql (coding-idiom :dlist))) token
                      &optional (circle-hash nil))
   (let ((out nil))
     (setf (gethash (second token) circle-hash) out)
     (let* ((rest-liste (cddr token)))
       (flet ((unmarshal-it (item)
		(if (listp item)
                    (unmarshal-fn version (first item) item circle-hash)
                    (unmarshal-fn version T item circle-hash))))
	 (cons (unmarshal-it (first rest-liste))
	       (first (loop for walker in (rest rest-liste)
			 collect (unmarshal-it walker))))))))

;;;  04.01.99 cjo: weswegen ein neues coding-idom eingefuehrt wurde, um alte array "richtig"
;;;                einlesen zu koennen.
;;;  18.08.98 cjo: encode von array wurde umgedreht
;;;  10.11.11 mw: removed the old (wrong) array method
(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
			 (type (eql (coding-idiom :array))) token
			 &optional (circle-hash nil))
  (let ((out      (make-array (third token) :element-type (fourth token)))
	(elements (fifth token)))
    (setf (gethash (second token) circle-hash) out)
    (loop
       for walker in elements
       for e from 0 to (1- (length elements))
       do (if (listp walker)
	      (setf (row-major-aref out e)
		    (unmarshal-fn version (first walker) walker circle-hash))
	      (setf (row-major-aref out e)
		    (unmarshal-fn version T walker circle-hash))))
    out))

;;;  15.01.99 cjo: make-hash-table abgeaendert
;;;  07.07.98 cjo: LOOP
(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
			 (type (eql (coding-idiom :hash-table))) token
			 &optional (circle-hash nil))
  (let* ((hash-function (seventh token))
	 (out           (if hash-function
			    (make-hash-table :size             (third token)
					     :rehash-size      (fourth token)
					     :rehash-threshold (fifth token)
					     :test             (sixth token)
					     :hash-function    hash-function)
			    (make-hash-table :size             (third token)
					     :rehash-size      (fourth token)
					     :rehash-threshold (fifth token)
					     :test             (sixth token))))
	 (elements      (eighth token)))
    (setf (gethash (second token) circle-hash) out)
    (loop
       for key in elements by #'cddr
       for value in (rest elements) by #'cddr  do
	 (if (listp key)
	     (setf (gethash (unmarshal-fn version (first key) key circle-hash) out)
		   (if (listp value) (unmarshal-fn version (first value) value circle-hash)
		       (unmarshal-fn version t value circle-hash)))
	     (setf (gethash (unmarshal-fn version t key circle-hash) out)
		   (if (listp value) (unmarshal-fn version (first value) value circle-hash)
		       (unmarshal-fn version t value circle-hash)))))
    out))

;;;  04.01.99 cjo: simple-strings
(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
			 (type (eql (coding-idiom :simple-string))) token
			 &optional (circle-hash nil))
   (declare (ignore circle-hash))
   (third token))

; (unmarshal (marshal "huhu"))

;;;  04.01.99 cjo: strings
(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
			 (type (eql (coding-idiom :string))) token
			 &optional (circle-hash nil))
  (declare (ignore circle-hash))
  (let ((string (nth 4 token)))
    (make-array (length string)
		:element-type     'character
		:initial-contents string
		:adjustable       (nth 3 token)
		:fill-pointer     (nth 2 token))))

;;; =============================================================

(pushnew :marshal *features*)
