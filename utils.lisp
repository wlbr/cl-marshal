(in-package :utils)

(defun circular-list-p (a-list)
  (if (null a-list) ;; an empty list is not a circular list
      nil
      (do* ((stopper a-list)
	    (tail    (rest a-list) (rest tail)))
	   (nil)
	(cond
	  ((not (consp tail))
	   (return-from circular-list-p nil))
	  ((eq stopper tail)
	   (return-from circular-list-p t))
	  ((null tail)
	   (return-from circular-list-p nil))))))

(defun dotted-list-p (l)
  (and (not (circular-list-p l))
       (not (null (cdr (last l))))))

(defun proper-list-p (l)
  (and (listp l)
       (not (utils:circular-list-p l))
       (not (utils:dotted-list-p   l))))
