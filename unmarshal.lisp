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
!Must return the object!!")
  )

(defmethod initialize-unmarshalled-instance (object)
  "Called as the last step of the deserialization of an object.
!Must return the object!!"
  (shared-initialize object t))


;;; =============================================================
(defgeneric unmarshal (thing)
  (:documentation "Returns an object when called with a wellformed marshal sexp.")
  )


(defmethod unmarshal (thing)
  (if (and (not (null thing)) (listp thing))
      (if (eq (coding-idiom :coding-identifier) (first thing))
          (if (and (not (null (third thing))) (listp (third thing)))
              (unmarshal-fn (second thing) (first (third thing)) (third thing))
              (unmarshal-fn (second thing) T (third thing)))
          thing)
      thing)
  )



(defgeneric unmarshal-fn (version type token &optional circle-hash)
  )


(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                         type token &optional (circle-hash NIL))
  (declare (ignore type circle-hash))
  token)

(defun make-circle-hash ()
  (make-hash-table :test #'eq :size 50 :rehash-size 1.5))

(defmethod unmarshal-fn :around ((version (eql (coding-idiom :coding-release-no)))
                                 type token &optional (circle-hash NIL))
  (let ((result NIL))
    (if circle-hash
        (progn
          (setq result (call-next-method version type token circle-hash))
          (if (listp token)
              (setf (gethash (second token) circle-hash) result)
              result))
        (progn
          (setq circle-hash (make-circle-hash))
          (call-next-method version type token circle-hash))
        )))

(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                         (type (eql (coding-idiom :reference))) token &optional (circle-hash NIL))
  (let ((reference (gethash (second token) circle-hash)))
    (if (eq reference :placeholder)
	token
	reference)))

;;;  07.07.98 cjo: LOOP
(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                         (type (eql (coding-idiom :object))) token &optional (circle-hash NIL))
  (let* ((out (allocate-instance (find-class (third token))))
         (slots (class-persistent-slots out))
         (values (cdddr token)))

    (setf (gethash (second token) circle-hash) out)

    (LOOP
      FOR slot IN slots
      FOR value IN values
      DO (if (listp value)
             (setf (slot-value out slot) (unmarshal-fn version (first value) value circle-hash))
             (setf (slot-value out slot) (unmarshal-fn version T value circle-hash))))
    (initialize-unmarshalled-instance out)))

(defun token-reference-p (token)
  (and
   (listp token)
   (not (utils:dotted-list-p token))
   (= (length token) 2)
   (eq (first token) :reference)
   (numberp (second token))))

(defun second-pass-list (version token circle-hash &optional (max-depth 4))
  (when (> max-depth 0)
    (LOOP
       FOR walker IN token
       FOR i from 0        do
	 (cond
	   ((and (utils:proper-list-p walker)
		 (token-reference-p walker))
	    (multiple-value-bind (value existsp)
		(gethash (second walker) circle-hash)
	      (when existsp
		(setf (elt token i) value))))
	   ((utils:proper-list-p walker)
	    (second-pass-list version walker circle-hash (1- max-depth)))
	   (t ;; do-nothing
	    ))))
  token)

;;;  07.07.98 cjo: LOOP (Faktor 3 schneller :)
;;; 12.02.99 cjo: aufgespalten wegen dotted lists
(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                         (type (eql (coding-idiom :list))) token
                         &optional (circle-hash NIL))
  (let ((out (if (subseq token 2)
		 (coding-idiom :placeholder)
		 nil))
	(local-circle-hash (make-circle-hash)))
    (setf (gethash (second token) circle-hash) out)
    (let ((first-pass (LOOP FOR walker IN (cddr token) COLLECT
			   (if (listp walker)
			       (progn
				 (setf (gethash (second walker) local-circle-hash)
				       (unmarshal-fn version (first walker) walker circle-hash))
				 (gethash (second walker) local-circle-hash))
			       (unmarshal-fn version T walker circle-hash)))))
      (if (not (token-reference-p token))
	  (setf (gethash (second token) local-circle-hash) first-pass))
      (second-pass-list  version first-pass local-circle-hash))))

(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                         (type (eql (coding-idiom :dlist))) token
                         &optional (circle-hash NIL))
    (let ((out (if (subseq token 2)
		   (coding-idiom :placeholder)
		   nil)))
      (setf (gethash (second token) circle-hash) out)
      (let* ((rest-liste (cddr token)))
	(flet ((unmarshal-it (item)
		 (if (listp item)
		     (unmarshal-fn version (first item) item circle-hash)
		     (unmarshal-fn version T item circle-hash))))
	  (cons (unmarshal-it (first rest-liste))
		(first (loop for walker in (rest rest-liste)
			  collect (unmarshal-it walker))))))))

(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                         (type (eql (coding-idiom :circular-list))) token
                         &optional (circle-hash NIL))
  (let ((flat (unmarshal-fn version :list token circle-hash)))
    (setf (cdr (last flat)) flat)
    flat))

;;;  04.01.99 cjo: weswegen ein neues coding-idom eingefuehrt wurde, um alte array "richtig"
;;;                einlesen zu koennen.
;;;  18.08.98 cjo: encode von array wurde umgedreht
;;;  10.11.11 mw: removed the old (wrong) array method
(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                         (type (eql (coding-idiom :array))) token &optional (circle-hash NIL))
  (let ((out (make-array (third token) :element-type (fourth token)))
        (elements (fifth token)))

    (setf (gethash (second token) circle-hash) out)

    (LOOP
      FOR walker IN elements
      FOR e FROM 0 TO (1- (length elements))
      DO (if (listp walker)
             (setf (row-major-aref out e) (unmarshal-fn version (first walker) walker circle-hash))
             (setf (row-major-aref out e) (unmarshal-fn version T walker circle-hash))))
    out))


;;;  15.01.99 cjo: make-hash-table abgeaendert
;;;  07.07.98 cjo: LOOP
(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                         (type (eql (coding-idiom :hash-table))) token &optional (circle-hash NIL))
  (let* ((hash-function (seventh token))
         (out (if hash-function
                  (make-hash-table :size (third token) :rehash-size (fourth token)
                                   :rehash-threshold (fifth token) :test (sixth token) :hash-function hash-function)
                  (make-hash-table :size (third token) :rehash-size (fourth token)
                                   :rehash-threshold (fifth token) :test (sixth token))))
         (elements (eighth token)))

    (setf (gethash (second token) circle-hash) out)
    (LOOP
      FOR key IN elements BY #'cddr
      FOR value IN (rest elements) BY #'cddr
      DO (if (listp key)
             (setf (gethash (unmarshal-fn version (first key) key circle-hash) out)
                   (if (listp value) (unmarshal-fn version (first value) value circle-hash)
                       (unmarshal-fn version T value circle-hash)))
             (setf (gethash (unmarshal-fn version T key circle-hash) out)
                   (if (listp value) (unmarshal-fn version (first value) value circle-hash)
                       (unmarshal-fn version T value circle-hash)))))
    out))



;;;  04.01.99 cjo: simple-strings
(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                         (type (eql (coding-idiom :simple-string))) token
                         &optional (circle-hash NIL))
  (declare (ignore circle-hash))
  (third token))

; (unmarshal (marshal "huhu"))

;;;  04.01.99 cjo: strings
(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                         (type (eql (coding-idiom :string))) token
                         &optional (circle-hash NIL))
  (declare (ignore circle-hash))
  (let ((string (nth 4 token)))
    (make-array (length string)
                :element-type 'character
                :initial-contents string
                :adjustable (nth 3 token)
                :fill-pointer (nth 2 token))))

;;; =============================================================

(pushnew :marshal *features*)
