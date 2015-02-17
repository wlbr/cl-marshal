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


(defmethod class-persistant-slots ((class standard-object))
   "Defines the slots that will be serialized. Has to return 
list of valid slotnames.
If this is a nested list, then this the element of the second level 
need to be pairs of slot and accessors."
   NIL)


;;; =============================================================

(defclass persist-hashtable ()
    ((hashtable :initform NIL :accessor hashtable)
     (next-key :initform 0 :accessor next-key)))

(defmethod initialize-instance :after ((self persist-hashtable) &rest initargs)
   (declare (ignore initargs))
   (setf (hashtable self) (make-hash-table :test #'eq :size 50 :rehash-size 1.5))
   )

(defmethod genkey ((self persist-hashtable))
   (incf (next-key self)))
   ;   (setf (next-key self) (1+ (next-key self))))
   

(defmethod getvalue ((self persist-hashtable) key)
   (gethash key (hashtable self)))


(defmethod setvalue ((self persist-hashtable) key value)
   (setf (gethash key (hashtable self)) value))



;;; =============================================================


(defgeneric marshal (thing &optional circle-hash)
  (:documentation "Generates an sexp when called with an object. The sexp can be used 
to send it over a ntowrk or to store it in a database etc.")
  )


(defmethod marshal (thing &optional (circle-hash NIL))
  (declare (ignore circle-hash))
   thing)


(defmethod marshal :around (thing &optional (circle-hash NIL))
   (if circle-hash 
      (call-next-method thing circle-hash)
      (progn
        (setq circle-hash (make-instance 'persist-hashtable))
        (list (coding-idiom :coding-identifier) (coding-idiom :coding-release-no) (call-next-method thing circle-hash)))
        ))


(defmethod marshal ((object standard-object) &optional (circle-hash NIL))
   (let* ((class (class-of object))
          (pslots (class-persistant-slots object))
          (dummy NIL)
          (outlist NIL))
      
      (setq dummy (getvalue circle-hash object))
      (if dummy
         (setq outlist (list (coding-idiom :reference) dummy))                               
         (progn
           (when pslots
              (setq dummy (genkey circle-hash))
              (setvalue circle-hash object dummy)
              (setq outlist (list (coding-idiom :object) dummy (class-name class)))
              (dolist (walker pslots)
                 (setq outlist (nconc outlist (list (marshal (slot-value object walker) circle-hash))))
                 )))
           )
      outlist))


;;; 12.02.99 cjo: auch dotted lists werden korrekt behandelt
(defmethod marshal ((list list) &optional (circle-hash NIL))
   (let* ((ckey NIL)
          (output NIL)
          (dotted-list (rest (last list))))
      
      ; ========= circle-stuff
      (setf ckey (getvalue circle-hash list))
      (if ckey
         (setq output (list (coding-idiom :reference) ckey))
         (progn
           (setq ckey (genkey circle-hash))
           (setvalue circle-hash list ckey)
           (when dotted-list
              (setf output (nconc output (list (marshal dotted-list circle-hash)))))
           (LOOP FOR walker IN list
             DO (setf output (nconc output (list (marshal walker circle-hash)))))
           (push ckey output)
           (push (if dotted-list
                    (coding-idiom :dlist)
                    (coding-idiom :list)) 
             output)))
      output))


;;;  04.01.99 cjo: wird jetzt als :array2 rausgeschrieben, dann ist eine unterscheidung zum alten 
;;;                :array moeglich
;;;  10.08.98 cjo: nreverse vergessen! push dreht die liste um. wenn es bloede laeuft hat man so
;;;                :reference, bevor die nummer ueberhaupt existiert!
(defmethod marshal ((array array) &optional (circle-hash NIL))
   (let* ((ckey NIL)
          (output NIL) 
          (dummy NIL))
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

(defmethod marshal-simple-string (object circle-hash)
   (let* ((ckey NIL)
          (output NIL))
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
   (let* ((ckey NIL)
          (output NIL))
      (setf ckey (getvalue circle-hash object))
      (if ckey
         (setq output (list (coding-idiom :reference) ckey))
         (let ((fill-pointer (fill-pointer object))
               (adjustable-array-p (adjustable-array-p object)))
            (setq ckey (genkey circle-hash))
            (setvalue circle-hash object ckey) 
            (setf (fill-pointer object) (array-dimension object 0)) ; was 0, was: NIL
            (setq output (list (coding-idiom :string) ckey
                           fill-pointer
                           adjustable-array-p
                           (princ-to-string object)))
            (setf (fill-pointer object) fill-pointer)))
      output))

(defmethod marshal ((object string) &optional (circle-hash NIL))
   (typecase object
     (simple-string (marshal-simple-string object circle-hash))
     (T (marshal-string object circle-hash))))

;;; cjo 15.1.1999 hash-function kann man nicht mehr auslesen!!!
(defmethod marshal ((hash-table hash-table) &optional (circle-hash NIL))
   (let* ((ckey NIL)
          (output NIL) 
          (dummy NIL))
      (setf ckey (getvalue circle-hash hash-table))
      (if ckey
         (setq output (list (coding-idiom :reference) ckey))
         (progn 
           (setq ckey (genkey circle-hash))
           (setvalue circle-hash hash-table ckey)                      
           (setq output (list (coding-idiom :hash-table) ckey
                          (hash-table-size hash-table) (hash-table-rehash-size hash-table)
                          (hash-table-rehash-threshold hash-table) (hash-table-test hash-table)
                          ;(hash-table-hash-function hash-table)
                          NIL
                          ))
           (maphash #'(lambda (key value)
                        (setq dummy (nconc dummy (list (marshal key circle-hash) (marshal value circle-hash)))))
                        hash-table)
           (when dummy 
              (setq output (nconc output (list dummy))))))
      output))


