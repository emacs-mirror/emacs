(defun add-timeout (secs function object &optional resignal)
  (run-at-time secs resignal function object))

(defun disable-timeout (timeout)
  (cancel-timer timeout))

(defun copy-tree (tree)
  (if (consp tree)
      (cons (copy-tree (car tree))
	    (copy-tree (cdr tree)))
    (if (vectorp tree)
	(let ((new (copy-sequence tree))
	      (i (1- (length new))))
	  (while (>= i 0)
	    (aset new i (copy-tree (aref new i)))
	    (setq i (1- i)))
	  new)
      tree)))

(fset 'current-time-seconds 'current-time)

(defun keymap-parent (keymap)
  (let ((tail (cdr keymap)))
    (while (and tail (not (eq (car tail) 'keymap)))
      (setq tail (cdr tail)))
    tail))

(defun set-keymap-parent (keymap new-parent)
  (let ((tail (cdr keymap)))
    (while (and tail (cdr tail) (not (eq (car (cdr tail)) 'keymap)))
      (setq tail (cdr tail)))
    (if tail
	(setcdr tail new-parent))))

(defun remove-hook (hook-var function)
  (if (boundp 'hook-var)
      (set hook-var (delq function (symbol-value hook-var)))))

(defun remprop (symbol prop)
  (let ((plist (symbol-plist symbol)))
    (while (eq (car plist) prop)
      (setplist symbol (setq plist (cdr (cdr plist)))))
    (while plist
      (if (eq (nth 2 plist) prop)
	  (setcdr (cdr plist) (nthcdr 4 plist)))
      (setq plist (cdr (cdr plist))))))

(defun map-keymap (function keymap)
  "Call FUNCTION for every binding in KEYMAP.
This includes bindings inherited from a parent keymap.
FUNCTION receives two arguments each time it is called:
the character (more generally, the event type) that is bound,
and the binding it has."
  (while (consp keymap)
    (if (consp (car keymap))
	(funcall function (car (car keymap)) (cdr (car keymap)))
      (if (vectorp (car keymap))
	  (let ((i (length (car keymap)))
		(vector (car keymap)))
	    (while (>= i 0)
	      (funcall function i (aref vector i))
	      (setq i (1- i))))))
    (setq keymap (cdr keymap))))
