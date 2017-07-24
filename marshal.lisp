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

(eval-when  (:execute :load-toplevel :compile-toplevel)
  (defgeneric class-persistent-slots (class)
    (:documentation
     "Defines the slots that will be serialized. Has to return list of valid slotnames.
If this is a nested list, then the elements of the second level
need to be pairs of slot and accessors."))

  (defmethod class-persistent-slots ((class standard-object))
    nil)

  (setf (symbol-function 'class-persistant-slots) #'class-persistent-slots))

;;; =============================================================

(defclass persist-hashtable ()
  ((hashtable :initform nil :accessor hashtable)
   (next-key :initform 0 :accessor next-key)))

(defmethod initialize-instance :after ((self persist-hashtable) &rest initargs)
  (declare (ignore initargs))
  (setf (hashtable self) (make-hash-table :test #'eq :size 50 :rehash-size 1.5))
  )


(defgeneric genkey (self))


(defmethod genkey ((self persist-hashtable))
  (incf (next-key self)))
;   (setf (next-key self) (1+ (next-key self))))


(defgeneric getvalue (self key))


(defmethod getvalue ((self persist-hashtable) key)
  (gethash key (hashtable self)))


(defgeneric setvalue (self key value))


(defmethod setvalue ((self persist-hashtable) key value)
  (setf (gethash key (hashtable self)) value))

;;; =============================================================

(defgeneric marshal (thing &optional circle-hash)
  (:documentation "Generates an sexp when called with an object. The sexp can be used
to send it over a network or to store it in a database etc.")
  )


(defmethod marshal (thing &optional (circle-hash nil))
  (declare (ignore circle-hash))
  thing)


(defmethod marshal :around (thing &optional (circle-hash nil))
  (if circle-hash
      (call-next-method thing circle-hash)
      (progn
        (setq circle-hash (make-instance 'persist-hashtable))
        (list (coding-idiom :coding-identifier) (coding-idiom :coding-release-no) (call-next-method thing circle-hash)))
      ))


(defmethod marshal ((object standard-object) &optional (circle-hash nil))
  (let* ((class   (class-of object))
         (pslots  (class-persistent-slots object))
         (dummy   nil)
         (outlist nil))
    (setq dummy (getvalue circle-hash object))
    (if dummy
        (setq outlist (list (coding-idiom :reference) dummy))
        (progn
          (when pslots
            (setq dummy (genkey circle-hash))
            (setvalue circle-hash object dummy)
            (setf outlist (list (coding-idiom :object)
				dummy
				(class-name class)
				(intern (package-name (symbol-package (class-name class)))
					:keyword)))
            (dolist (walker pslots)
              (setq outlist
		    (nconc outlist
			   (list (marshal (slot-value object walker)
					  circle-hash))))))))
    outlist))

(defun %walk-list (sequence output ckey key-idiom circle-hash)
  (loop for walker in sequence
     do (setf output (nconc output (list (marshal walker circle-hash)))))
  (push ckey output)
  (push (coding-idiom key-idiom) output))

;;; 12.02.99 cjo: auch dotted lists werden korrekt behandelt
(defmethod marshal ((list list) &optional (circle-hash nil))
  (let* ((ckey nil)
         (output nil)
	 (circular-list-p (utils:circular-list-p list))
         (dotted-list-p   (and (not circular-list-p)
			       (rest (last list)))))
    ; ========= circle-stuff
    (setf ckey (getvalue circle-hash list))
    (if ckey
        (setq output (list (coding-idiom :reference) ckey))
        (progn
          (setq ckey (genkey circle-hash))
          (setvalue circle-hash list ckey)
          (cond
	    (dotted-list-p
	     (setf output (nconc output (list (marshal (car list) circle-hash)
					      (marshal (cdr list) circle-hash))))
	     (push ckey output)
	     (push (coding-idiom :dlist) output))
	    (circular-list-p
	     (let* ((*print-circle* t)
		    (flat (do* ((stopper list)
				(tail    (rest list) (rest tail))
				(results (list (first stopper))))
			       ((eq stopper tail) (reverse results))
			    ;;(format t "~a stopper ~a ~%"  tail stopper)
			    (push (first tail) results))))

	       (loop for walker in flat
		  do (setf output (nconc output (list (marshal walker circle-hash)))))
	       (push ckey output)
	       (push (coding-idiom :circular-list) output)))
	    (t ;; proper list
	     (loop for walker in list
                do (setf output (nconc output (list (marshal walker circle-hash)))))
	     (push ckey output)
	     (push (coding-idiom :list) output)))))
    output))


;;;  04.01.99 cjo: wird jetzt als :array2 rausgeschrieben, dann ist eine unterscheidung zum alten
;;;                :array moeglich
;;;  10.08.98 cjo: nreverse vergessen! push dreht die liste um. wenn es bloede laeuft hat man so
;;;                :reference, bevor die nummer ueberhaupt existiert!
(defmethod marshal ((array array) &optional (circle-hash nil))
  (let* ((ckey nil)
         (output nil)
         (dummy nil))
    (setf ckey (getvalue circle-hash array))
    (if ckey
        (setq output (list (coding-idiom :reference) ckey))
        (progn
          (setq ckey (genkey circle-hash))
          (setvalue circle-hash array ckey)
          (setq output (list (coding-idiom :array) ckey
                             (array-dimensions array) (array-element-type array)))
          (dotimes (walker (array-total-size array))
            (push (marshal (row-major-aref array walker) circle-hash) dummy))
          (setq output (nconc output (list (nreverse dummy))))))
    output))

(defgeneric marshal-simple-string (object circle-hash))

(defmethod marshal-simple-string (object circle-hash)
  (let* ((ckey nil)
         (output nil))
    (setf ckey (getvalue circle-hash object))
    (if ckey
        (setq output (list (coding-idiom :reference) ckey))
        (progn
          (setq ckey (genkey circle-hash))
          (setvalue circle-hash object ckey)
          (setq output (list (coding-idiom :simple-string) ckey
                             object))))
    output))

(defun marshal-string (object circle-hash)
  (let* ((ckey nil)
         (output nil))
    (setf ckey (getvalue circle-hash object))
    (if ckey
        (setq output (list (coding-idiom :reference) ckey))
        (let ((fill-pointer (if (array-has-fill-pointer-p object) (fill-pointer object) nil))
              (adjustable-array-p (adjustable-array-p object)))
          (setq ckey (genkey circle-hash))
          (setvalue circle-hash object ckey)
          (when fill-pointer (setf (fill-pointer object) (array-dimension object 0))) ; was 0, was: nil
          (setq output (list (coding-idiom :string) ckey
                             fill-pointer
                             adjustable-array-p
                             (princ-to-string object)))
          (when fill-pointer (setf (fill-pointer object) fill-pointer))))
    output))

(defmethod marshal ((object string) &optional (circle-hash nil))
  (typecase object
    (simple-string (marshal-simple-string object circle-hash))
    (t             (marshal-string object circle-hash))))

;;; cjo 15.1.1999 hash-function kann man nicht mehr auslesen!!!
(defmethod marshal ((hash-table hash-table) &optional (circle-hash nil))
  (let* ((ckey nil)
         (output nil)
         (dummy nil))
    (setf ckey (getvalue circle-hash hash-table))
    (if ckey
        (setq output (list (coding-idiom :reference) ckey))
        (progn
          (setq ckey (genkey circle-hash))
          (setvalue circle-hash hash-table ckey)
          (setq output (list (coding-idiom :hash-table) ckey
                             (hash-table-size hash-table) (hash-table-rehash-size hash-table)
                             (hash-table-rehash-threshold hash-table) (hash-table-test hash-table)
                             ;;(hash-table-hash-function hash-table)
                             nil
                             ))
          (maphash #'(lambda (key value)
                       (setq dummy
			     (nconc dummy
				    (list (marshal key circle-hash)
					  (marshal value circle-hash)))))
                   hash-table)
	  (setq output (nconc output (list dummy)))))
    output))
