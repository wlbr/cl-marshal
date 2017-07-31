(in-package :serialization-format)

(defun id (expr)
  (elt expr 1))

(defun reference-id (expr)
  (elt expr 1))

(defun data-type (expr)
  (elt expr 0))

(defun list-values (expr)
  (subseq expr 2))

(defun array-values (expr)
  (elt expr 4))

(defun array-sizes (expr)
  (elt expr 2))

(defun array-elements-type (expr)
  (elt expr 3))

(defun ht-values (expr)
  (elt expr 7))

(defun ht-size (expr)
  (elt expr 2))

(defun ht-rehash-size (expr)
  (elt expr 3))

(defun ht-rehash-threshold (expr)
  (elt expr 4))

(defun ht-test-fn (expr)
  (elt expr 5))

(defun ht-hash-fn (expr)
  (elt expr 6))

(defun simple-string-value (expr)
  (elt expr 2))

(defun string-value (expr)
  (elt expr 4))

(defun string-adjustable-p (expr)
  (elt expr 3))

(defun string-fill-pointer (expr)
  (elt expr 2))

(defun object-class-name (expr)
  (elt expr 2))

(defun object-package-name (expr)
  (elt expr 3))

(defun class-slots-values (expr)
  (subseq expr 4))
