;; -*- lexical-binding:t -*-
(eval-and-compile
  (defmacro increase ()
    `(let ((foo ,(point-max)))
       (cond
	((consp foo)
	 (message "consp %s" foo)
	 foo)
	((numberp foo)
	 (1+ fooo))			; Note the misspelling.
	(t (message "Something else: %s" foo))))))

(defun call-increase (bar)
  (cond
   ((not (or (consp bar)
	     (numberp bar)))
    bar)
   (t (increase))))
