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
