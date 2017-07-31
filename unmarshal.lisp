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
              (unmarshal-fn (second thing) t (third thing)))
          thing)
      thing)
  )



(defgeneric unmarshal-fn (version type token &optional circle-hash)
  )


(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                         type token &optional (circle-hash nil))
  (declare (ignore type circle-hash))
  token)

(defun make-circle-hash ()
  (make-hash-table :test #'eq :size 50 :rehash-size 1.5))

(defmethod unmarshal-fn :around ((version (eql (coding-idiom :coding-release-no)))
                                 type token &optional (circle-hash nil))
  (let ((result nil))
    (if circle-hash
        (progn
          (setq result (call-next-method version type token circle-hash))
          (if (listp token)
              (setf (gethash (fmt:id token) circle-hash) result)
              result))
        (progn
          (setq circle-hash (make-circle-hash))
          (call-next-method version type token circle-hash))
        )))

(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                         (type (eql (coding-idiom :reference))) token &optional (circle-hash nil))
  (let ((reference (gethash (fmt:id token) circle-hash)))
    (if (eq reference
            +reference-placeholder+)
        token
        reference)))

;;;  07.07.98 cjo: LOOP
(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                         (type (eql (coding-idiom :object))) token &optional (circle-hash nil))
  (let* ((package    (find-package (fmt:object-package-name token)))
         (values     (fmt:class-slots-values  token))
	 (class-out  (find-class (intern (symbol-name (fmt:object-class-name token))
					 package)))
	 (out        (allocate-instance class-out))
	 (slots      (class-persistent-slots  out)))

    (setf (gethash (fmt:id token) circle-hash) out)

    (loop
      for slot in slots
      for value in values
      do (if (listp value)
             (setf (slot-value out slot)
                   (unmarshal-fn version
                                 (fmt:data-type value)
                                 value
                                 circle-hash))
             (setf (slot-value out slot) (unmarshal-fn version t value circle-hash))))
    (initialize-unmarshalled-instance out)))

(defun token-reference-p (token)
  (and
   (listp token)
   (not (utils:dotted-list-p token))
   (= (length token) 2)
   (eq (fmt:data-type token) :reference)
   (numberp (fmt:reference-id token))))

(defun second-pass-list (version token circle-hash &optional (max-depth 4))
  (when (> max-depth 0)
    (loop
       for walker in token
       for i from 0        do
         (cond
           ((and (utils:proper-list-p walker)
                 (token-reference-p walker))
            (multiple-value-bind (value existsp)
                (gethash (fmt:id walker) circle-hash)
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
                         &optional (circle-hash nil))
  (let ((out (if (subseq token 2)
                 +reference-placeholder+
                 nil))
        (local-circle-hash (make-circle-hash)))
    (setf (gethash (fmt:id token) circle-hash) out)
    (let ((first-pass (loop for walker in (fmt:list-values token) collect
                           (if (listp walker)
                               (progn
                                 (setf (gethash (fmt:id walker) local-circle-hash)
                                       (unmarshal-fn version
                                                     (fmt:data-type walker)
                                                     walker
                                                     circle-hash))
                                 (gethash (fmt:id walker) local-circle-hash))
                               (unmarshal-fn version t walker circle-hash)))))
      (if (not (token-reference-p token))
          (setf (gethash (fmt:id token) local-circle-hash) first-pass))
      (second-pass-list  version first-pass local-circle-hash))))

(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                         (type (eql (coding-idiom :dlist))) token
                         &optional (circle-hash nil))
    (let ((out (if (subseq token 2)
                   +reference-placeholder+
                   nil)))
      (setf (gethash (fmt:id token) circle-hash) out)
      (let* ((rest-liste (fmt:list-values token)))
        (flet ((unmarshal-it (item)
                 (if (listp item)
                     (unmarshal-fn version (fmt:data-type item) item circle-hash)
                     (unmarshal-fn version t item circle-hash))))
          (cons (unmarshal-it (first rest-liste))
                (first (loop for walker in (rest rest-liste)
                          collect (unmarshal-it walker))))))))

(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                         (type (eql (coding-idiom :circular-list))) token
                         &optional (circle-hash nil))
  (let ((flat (unmarshal-fn version :list token circle-hash)))
    (setf (cdr (last flat)) flat)
    flat))

;;;  04.01.99 cjo: weswegen ein neues coding-idom eingefuehrt wurde, um alte array "richtig"
;;;                einlesen zu koennen.
;;;  18.08.98 cjo: encode von array wurde umgedreht
;;;  10.11.11 mw: removed the old (wrong) array method
(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                         (type (eql (coding-idiom :array))) token &optional (circle-hash nil))
  (let ((out (make-array (fmt:array-sizes token)
                         :element-type (fmt:array-elements-type token)))
        (elements (fmt:array-values token)))

    (setf (gethash (fmt:id token) circle-hash) out)

    (loop
      for walker in elements
      for e from 0 to (1- (length elements))
      do (if (listp walker)
             (setf (row-major-aref out e)
                   (unmarshal-fn version
                                 (fmt:data-type walker)
                                 walker
                                 circle-hash))
             (setf (row-major-aref out e) (unmarshal-fn version t walker circle-hash))))
    out))


;;;  15.01.99 cjo: make-hash-table abgeaendert
;;;  07.07.98 cjo: LOOP
(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                         (type (eql (coding-idiom :hash-table))) token &optional (circle-hash nil))
  (let* ((hash-function    (fmt:ht-hash-fn          token))
         (size             (fmt:ht-size             token))
         (rehash-size      (fmt:ht-rehash-size      token))
         (rehash-threshold (fmt:ht-rehash-threshold token))
         (test             (fmt:ht-test-fn          token))
         (out              (if hash-function
                               (make-hash-table :size             size
                                                :rehash-size      rehash-size
                                                :rehash-threshold rehash-threshold
                                                :test             test
                                                :hash-function    hash-function)
                               (make-hash-table :size             size
                                                :rehash-size      rehash-size
                                                :rehash-threshold rehash-threshold
                                                :test             test)))
         (elements         (fmt:ht-values token)))
    (setf (gethash (fmt:id token) circle-hash) out)
    (loop
      for key   in elements        by #'cddr
      for value in (rest elements) by #'cddr
      do (if (listp key)
             (setf (gethash (unmarshal-fn version (fmt:data-type key) key circle-hash) out)
                   (if (listp value)
                       (unmarshal-fn version (fmt:data-type value) value circle-hash)
                       (unmarshal-fn version t value circle-hash)))
             (setf (gethash (unmarshal-fn version t key circle-hash) out)
                   (if (listp value)
                       (unmarshal-fn version (fmt:data-type value) value circle-hash)
                       (unmarshal-fn version t value circle-hash)))))
    out))



;;;  04.01.99 cjo: simple-strings
(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                         (type (eql (coding-idiom :simple-string))) token
                         &optional (circle-hash nil))
  (declare (ignore circle-hash))
  (fmt:simple-string-value token))

; (unmarshal (marshal "huhu"))

;;;  04.01.99 cjo: strings
(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                         (type (eql (coding-idiom :string))) token
                         &optional (circle-hash nil))
  (declare (ignore circle-hash))
  (let ((string (fmt:string-value token)))
    (make-array (length string)
                :element-type     'character
                :initial-contents string
                :adjustable       (fmt:string-adjustable-p token)
                :fill-pointer     (fmt:string-fill-pointer token))))

;;; =============================================================

(pushnew :marshal *features*)
