;;; profiler.el --- UI and helper functions for Emacs's native profiler -*- lexical-binding: t -*-

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: Tomohiro Matsuyama <tomo@cx4a.org>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defgroup profiler nil
  "Emacs profiler."
  :group 'lisp
  :prefix "profiler-")



;;; Utilities

(defun profiler-ensure-string (object)
  (if (stringp object)
      object
    (format "%s" object)))

(defun profiler-format (fmt &rest args)
  (cl-loop for (width align subfmt) in fmt
	   for arg in args
	   for str = (cond
		      ((consp subfmt)
		       (apply 'profiler-format subfmt arg))
		      ((stringp subfmt)
		       (format subfmt arg))
		      ((and (symbolp subfmt)
			    (fboundp subfmt))
		       (funcall subfmt arg))
		      (t
		       (profiler-ensure-string arg)))
	   for len = (length str)
	   if (< width len)
	   collect (substring str 0 width) into frags
	   else
	   collect
	   (let ((padding (make-string (- width len) ?\s)))
	     (cl-ecase align
	       (left (concat str padding))
	       (right (concat padding str))))
	   into frags
	   finally return (apply #'concat frags)))

(defun profiler-format-nbytes (nbytes)
  (if (and (integerp nbytes) (> nbytes 0))
      (cl-loop with i = (% (1+ (floor (log10 nbytes))) 3)
	       for c in (append (number-to-string nbytes) nil)
	       if (= i 0)
	       collect ?, into s
	       and do (setq i 3)
	       collect c into s
	       do (cl-decf i)
	       finally return
	       (apply 'string (if (eq (car s) ?,) (cdr s) s)))
    (profiler-ensure-string nbytes)))



;;; Backtrace data structure

(defun profiler-backtrace-reverse (backtrace)
  (cl-case (car backtrace)
    ((t gc)
     (cons (car backtrace)
	   (reverse (cdr backtrace))))
    (t (reverse backtrace))))



;;; Slot data structure

(cl-defstruct (profiler-slot (:type list)
			     (:constructor profiler-make-slot))
  backtrace count elapsed)



;;; Log data structure

(cl-defstruct (profiler-log (:type list)
			    (:constructor profiler-make-log))
  type diff-p timestamp slots)

(defun profiler-log-diff (log1 log2)
  ;; FIXME zeros
  (unless (eq (profiler-log-type log1)
	      (profiler-log-type log2))
    (error "Can't compare different type of logs"))
  (let ((slots (profiler-log-slots log2)))
    (dolist (slot (profiler-log-slots log1))
      (push (profiler-make-slot :backtrace (profiler-slot-backtrace slot)
				:count (- (profiler-slot-count slot))
				:elapsed (- (profiler-slot-elapsed slot)))
	    slots))
    (profiler-make-log :type (profiler-log-type log1)
		       :diff-p t
		       :timestamp (current-time)
		       :slots slots)))

(defun profiler-log-fixup (log)
  "Fixup LOG so that the log could be serialized into file."
  (let ((fixup-entry
	 (lambda (entry)
	   (cond
	    ((and (consp entry)
		  (or (eq (car entry) 'lambda)
		      (eq (car entry) 'closure)))
	     (format "#<closure 0x%x>" (sxhash entry)))
	    ((eq (type-of entry) 'compiled-function)
	     (format "#<compiled 0x%x>" (sxhash entry)))
	    ((subrp entry)
	     (subr-name entry))
	    ((or (symbolp entry) (stringp entry))
	     entry)
	    (t
	     (format "#<unknown 0x%x>" (sxhash entry)))))))
    (dolist (slot (profiler-log-slots log))
      (setf (profiler-slot-backtrace slot)
	    (mapcar fixup-entry (profiler-slot-backtrace slot))))))



;;; Calltree data structure

(cl-defstruct (profiler-calltree (:constructor profiler-make-calltree))
  entry
  (count 0) count-percent
  (elapsed 0) elapsed-percent
  parent children)

(defun profiler-calltree-leaf-p (tree)
  (null (profiler-calltree-children tree)))

(defun profiler-calltree-count< (a b)
  (cond ((eq (profiler-calltree-entry a) t) t)
	((eq (profiler-calltree-entry b) t) nil)
	((eq (profiler-calltree-entry a) 'gc) t)
	((eq (profiler-calltree-entry b) 'gc) nil)
	(t (< (profiler-calltree-count a)
	      (profiler-calltree-count b)))))

(defun profiler-calltree-count> (a b)
  (not (profiler-calltree-count< a b)))

(defun profiler-calltree-elapsed< (a b)
  (cond ((eq (profiler-calltree-entry a) t) t)
	((eq (profiler-calltree-entry b) t) nil)
	((eq (profiler-calltree-entry a) 'gc) t)
	((eq (profiler-calltree-entry b) 'gc) nil)
	(t (< (profiler-calltree-elapsed a)
	      (profiler-calltree-elapsed b)))))

(defun profiler-calltree-elapsed> (a b)
  (not (profiler-calltree-elapsed< a b)))

(defun profiler-calltree-depth (tree)
  (let ((parent (profiler-calltree-parent tree)))
    (if (null parent)
	0
      (1+ (profiler-calltree-depth parent)))))

(defun profiler-calltree-find (tree entry)
  (cl-dolist (child (profiler-calltree-children tree))
    (when (equal (profiler-calltree-entry child) entry)
      (cl-return child))))

(defun profiler-calltree-walk (calltree function)
  (funcall function calltree)
  (dolist (child (profiler-calltree-children calltree))
    (profiler-calltree-walk child function)))

(defun profiler-calltree-build-1 (tree log &optional reverse)
  (dolist (slot (profiler-log-slots log))
    (let ((backtrace (profiler-slot-backtrace slot))
	  (count (profiler-slot-count slot))
	  (elapsed (profiler-slot-elapsed slot))
	  (node tree))
      (dolist (entry (if reverse
			 backtrace
		       (profiler-backtrace-reverse backtrace)))
	(let ((child (profiler-calltree-find node entry)))
	  (unless child
	    (setq child (profiler-make-calltree :entry entry :parent node))
	    (push child (profiler-calltree-children node)))
	  (cl-incf (profiler-calltree-count child) count)
	  (cl-incf (profiler-calltree-elapsed child) elapsed)
	  (setq node child))))))

(defun profiler-calltree-compute-percentages (tree)
  (let ((total-count 0)
	(total-elapsed 0))
    (dolist (child (profiler-calltree-children tree))
      (if (eq (profiler-calltree-entry child) 'gc)
	  (profiler-calltree-compute-percentages child)
	(cl-incf total-count (profiler-calltree-count child))
	(cl-incf total-elapsed (profiler-calltree-elapsed child))))
    (dolist (child (profiler-calltree-children tree))
      (if (eq (profiler-calltree-entry child) 'gc)
	  (setf (profiler-calltree-count-percent child) ""
		(profiler-calltree-elapsed-percent child) "")
	(profiler-calltree-walk
	 child
	 (lambda (node)
	   (unless (zerop total-count)
	     (setf (profiler-calltree-count-percent node)
		   (format "%s%%"
			   (/ (* (profiler-calltree-count node) 100)
			      total-count))))
	   (unless (zerop total-elapsed)
	     (setf (profiler-calltree-elapsed-percent node)
		   (format "%s%%"
			   (/ (* (profiler-calltree-elapsed node) 100)
			      total-elapsed))))))))))

(cl-defun profiler-calltree-build (log &key reverse)
  (let ((tree (profiler-make-calltree)))
    (profiler-calltree-build-1 tree log reverse)
    (profiler-calltree-compute-percentages tree)
    tree))

(defun profiler-calltree-sort (tree predicate)
  (let ((children (profiler-calltree-children tree)))
    (setf (profiler-calltree-children tree) (sort children predicate))
    (dolist (child (profiler-calltree-children tree))
      (profiler-calltree-sort child predicate))))



;;; Report rendering

(defcustom profiler-report-closed-mark "+"
  "An indicator of closed calltrees."
  :type 'string
  :group 'profiler)

(defcustom profiler-report-open-mark "-"
  "An indicator of open calltrees."
  :type 'string
  :group 'profiler)

(defcustom profiler-report-leaf-mark " "
  "An indicator of calltree leaves."
  :type 'string
  :group 'profiler)

(defvar profiler-report-sample-line-format
  '((60 left)
    (14 right ((9 right)
	       (5 right)))))

(defvar profiler-report-memory-line-format
  '((55 left)
    (19 right ((14 right profiler-format-nbytes)
	       (5 right)))))

(defvar profiler-report-log nil)
(defvar profiler-report-reversed nil)
(defvar profiler-report-order nil)

(defun profiler-report-make-entry-part (entry)
  (let ((string
	 (cond
	  ((eq entry t)
	   "Others")
	  ((eq entry 'gc)
	   "Garbage Collection")
	  ((and (symbolp entry)
		(fboundp entry))
	   (propertize (symbol-name entry)
		       'face 'link
		       'mouse-face 'highlight
		       'help-echo "mouse-2 or RET jumps to definition"))
	  (t
	   (profiler-ensure-string entry)))))
    (propertize string 'entry entry)))

(defun profiler-report-make-name-part (tree)
  (let* ((entry (profiler-calltree-entry tree))
	 (depth (profiler-calltree-depth tree))
	 (indent (make-string (* (1- depth) 2) ?\s))
	 (mark (if (profiler-calltree-leaf-p tree)
		   profiler-report-leaf-mark
		 profiler-report-closed-mark))
	 (entry (profiler-report-make-entry-part entry)))
    (format "%s%s %s" indent mark entry)))

(defun profiler-report-header-line-format (fmt &rest args)
  (let* ((header (apply 'profiler-format fmt args))
	 (escaped (replace-regexp-in-string "%" "%%" header)))
    (concat " " escaped)))

(defun profiler-report-line-format (tree)
  (let ((diff-p (profiler-log-diff-p profiler-report-log))
	(name-part (profiler-report-make-name-part tree))
	(elapsed (profiler-calltree-elapsed tree))
	(elapsed-percent (profiler-calltree-elapsed-percent tree))
	(count (profiler-calltree-count tree))
	(count-percent (profiler-calltree-count-percent tree)))
    (cl-ecase (profiler-log-type profiler-report-log)
      (sample
       (if diff-p
	   (profiler-format profiler-report-sample-line-format
			    name-part
			    (list (if (> elapsed 0)
				      (format "+%s" elapsed)
				    elapsed)
				  ""))
	 (profiler-format profiler-report-sample-line-format
			  name-part (list elapsed elapsed-percent))))
      (memory
       (if diff-p
	   (profiler-format profiler-report-memory-line-format
			  name-part
			  (list (if (> count 0)
				      (format "+%s" count)
				    count)
				""))
	 (profiler-format profiler-report-memory-line-format
			  name-part (list count count-percent)))))))

(defun profiler-report-insert-calltree (tree)
  (let ((line (profiler-report-line-format tree)))
    (insert (propertize (concat line "\n") 'calltree tree))))

(defun profiler-report-insert-calltree-children (tree)
  (mapc 'profiler-report-insert-calltree
	(profiler-calltree-children tree)))



;;; Report mode

(defvar profiler-report-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n"	    'profiler-report-next-entry)
    (define-key map "p"	    'profiler-report-previous-entry)
    (define-key map [down]  'profiler-report-next-entry)
    (define-key map [up]    'profiler-report-previous-entry)
    (define-key map "\r"    'profiler-report-toggle-entry)
    (define-key map "\t"    'profiler-report-toggle-entry)
    (define-key map "i"     'profiler-report-toggle-entry)
    (define-key map "f"     'profiler-report-find-entry)
    (define-key map "j"     'profiler-report-find-entry)
    (define-key map [mouse-2] 'profiler-report-find-entry)
    (define-key map "d"	    'profiler-report-describe-entry)
    (define-key map "C"	    'profiler-report-render-calltree)
    (define-key map "B"	    'profiler-report-render-reversed-calltree)
    (define-key map "A"	    'profiler-report-ascending-sort)
    (define-key map "D"	    'profiler-report-descending-sort)
    (define-key map "="	    'profiler-report-compare-log)
    (define-key map (kbd "C-x C-w") 'profiler-report-write-log)
    (define-key map "q"     'quit-window)
    map))

(defun profiler-report-make-buffer-name (log)
  (let ((time (format-time-string "%Y-%m-%d %T" (profiler-log-timestamp log))))
    (cl-ecase (profiler-log-type log)
      (sample (format "*CPU-Profiler-Report %s*" time))
      (memory (format "*Memory-Profiler-Report %s*" time)))))

(defun profiler-report-setup-buffer (log)
  (let* ((buf-name (profiler-report-make-buffer-name log))
	 (buffer (get-buffer-create buf-name)))
    (with-current-buffer buffer
      (profiler-report-mode)
      (setq profiler-report-log log
	    profiler-report-reversed nil
	    profiler-report-order 'descending))
    buffer))

(define-derived-mode profiler-report-mode special-mode "Profiler-Report"
  "Profiler Report Mode."
  (make-local-variable 'profiler-report-log)
  (make-local-variable 'profiler-report-reversed)
  (make-local-variable 'profiler-report-order)
  (use-local-map profiler-report-mode-map)
  (setq buffer-read-only t
	buffer-undo-list t
	truncate-lines t))



;;; Report commands

(defun profiler-report-calltree-at-point ()
  (get-text-property (point) 'calltree))

(defun profiler-report-move-to-entry ()
  (let ((point (next-single-property-change (line-beginning-position) 'entry)))
    (if point
	(goto-char point)
      (back-to-indentation))))

(defun profiler-report-next-entry ()
  "Move cursor to next profile entry."
  (interactive)
  (forward-line)
  (profiler-report-move-to-entry))

(defun profiler-report-previous-entry ()
  "Move cursor to previous profile entry."
  (interactive)
  (forward-line -1)
  (profiler-report-move-to-entry))

(defun profiler-report-expand-entry ()
  "Expand profile entry at point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (search-forward (concat profiler-report-closed-mark " ")
			  (line-end-position) t)
      (let ((tree (profiler-report-calltree-at-point)))
	(when tree
	  (let ((buffer-read-only nil))
	    (replace-match (concat profiler-report-open-mark " "))
	    (forward-line)
	    (profiler-report-insert-calltree-children tree)
	    t))))))

(defun profiler-report-collapse-entry ()
  "Collpase profile entry at point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (search-forward (concat profiler-report-open-mark " ")
			  (line-end-position) t)
      (let* ((tree (profiler-report-calltree-at-point))
	     (depth (profiler-calltree-depth tree))
	     (start (line-beginning-position 2))
	     d)
	(when tree
	  (let ((buffer-read-only nil))
	    (replace-match (concat profiler-report-closed-mark " "))
	    (while (and (eq (forward-line) 0)
			(let ((child (get-text-property (point) 'calltree)))
			  (and child
			       (numberp (setq d (profiler-calltree-depth child)))))
			(> d depth)))
	    (delete-region start (line-beginning-position)))))
      t)))

(defun profiler-report-toggle-entry ()
  "Expand profile entry at point if the tree is collapsed,
otherwise collapse the entry."
  (interactive)
  (or (profiler-report-expand-entry)
      (profiler-report-collapse-entry)))

(defun profiler-report-find-entry (&optional event)
  "Find profile entry at point."
  (interactive (list last-nonmenu-event))
  (if event (posn-set-point (event-end event)))
  (let ((tree (profiler-report-calltree-at-point)))
    (when tree
      (let ((entry (profiler-calltree-entry tree)))
	(find-function entry)))))

(defun profiler-report-describe-entry ()
  "Describe profile entry at point."
  (interactive)
  (let ((tree (profiler-report-calltree-at-point)))
    (when tree
      (let ((entry (profiler-calltree-entry tree)))
	(require 'help-fns)
	(describe-function entry)))))

(cl-defun profiler-report-render-calltree-1 (log &key reverse (order 'descending))
  (let ((calltree (profiler-calltree-build profiler-report-log
					   :reverse reverse)))
    (cl-ecase (profiler-log-type log)
      (sample
       (setq header-line-format
	     (profiler-report-header-line-format
	      profiler-report-sample-line-format
	      "Function" (list "Time (ms)" "%")))
       (let ((predicate (cl-ecase order
			  (ascending 'profiler-calltree-elapsed<)
			  (descending 'profiler-calltree-elapsed>))))
	 (profiler-calltree-sort calltree predicate)))
      (memory
       (setq header-line-format
	     (profiler-report-header-line-format
	      profiler-report-memory-line-format
	      "Function" (list "Bytes" "%")))
       (let ((predicate (cl-ecase order
			  (ascending 'profiler-calltree-count<)
			  (descending 'profiler-calltree-count>))))
	 (profiler-calltree-sort calltree predicate))))
    (let ((buffer-read-only nil))
      (erase-buffer)
      (profiler-report-insert-calltree-children calltree)
      (goto-char (point-min))
      (profiler-report-move-to-entry))))

(defun profiler-report-rerender-calltree ()
  (profiler-report-render-calltree-1 profiler-report-log
				     :reverse profiler-report-reversed
				     :order profiler-report-order))

(defun profiler-report-render-calltree ()
  "Render calltree view of the current profile."
  (interactive)
  (setq profiler-report-reversed nil)
  (profiler-report-rerender-calltree))

(defun profiler-report-render-reversed-calltree ()
  "Render reversed calltree view of the current profile."
  (interactive)
  (setq profiler-report-reversed t)
  (profiler-report-rerender-calltree))

(defun profiler-report-ascending-sort ()
  "Sort calltree view in ascending order."
  (interactive)
  (setq profiler-report-order 'ascending)
  (profiler-report-rerender-calltree))

(defun profiler-report-descending-sort ()
  "Sort calltree view in descending order."
  (interactive)
  (setq profiler-report-order 'descending)
  (profiler-report-rerender-calltree))

(defun profiler-report-log (log)
  (let ((buffer (profiler-report-setup-buffer log)))
    (with-current-buffer buffer
      (profiler-report-render-calltree))
    (pop-to-buffer buffer)))

(defun profiler-report-compare-log (buffer)
  "Compare current profiler log with another profiler log."
  (interactive (list (read-buffer "Compare to: ")))
  (let ((log1 (with-current-buffer buffer profiler-report-log))
	(log2 profiler-report-log))
    (profiler-report-log (profiler-log-diff log1 log2))))

(defun profiler-report-write-log (filename &optional confirm)
  "Write current profiler log into FILENAME."
  (interactive
   (list (read-file-name "Write log: " default-directory)
	 (not current-prefix-arg)))
  (let ((log profiler-report-log))
    (with-temp-buffer
      (let (print-level print-length)
	(print log (current-buffer)))
      (write-file filename confirm))))



;;; Profiler commands

(defcustom profiler-sample-interval 10
  "Default sample interval in millisecond."
  :type 'integer
  :group 'profiler)

;;;###autoload
(defun profiler-start (mode)
  (interactive
   (list (intern (completing-read "Mode: " '("cpu" "mem" "cpu+mem")
				  nil t nil nil "cpu"))))
  (cl-ecase mode
    (cpu
     (sample-profiler-start profiler-sample-interval)
     (message "CPU profiler started"))
    (mem
     (memory-profiler-start)
     (message "Memory profiler started"))
    (cpu+mem
     (sample-profiler-start profiler-sample-interval)
     (memory-profiler-start)
     (message "CPU and memory profiler started"))))

(defun profiler-stop ()
  (interactive)
  (cond
   ((and (sample-profiler-running-p)
	 (memory-profiler-running-p))
    (sample-profiler-stop)
    (memory-profiler-stop)
    (message "CPU and memory profiler stopped"))
   ((sample-profiler-running-p)
    (sample-profiler-stop)
    (message "CPU profiler stopped"))
   ((memory-profiler-running-p)
    (memory-profiler-stop)
    (message "Memory profiler stopped"))
   (t
    (error "No profilers started"))))

(defun profiler-reset ()
  (interactive)
  (sample-profiler-reset)
  (memory-profiler-reset)
  t)

(defun sample-profiler-report ()
  (let ((sample-log (sample-profiler-log)))
    (when sample-log
      (profiler-log-fixup sample-log)
      (profiler-report-log sample-log))))

(defun memory-profiler-report ()
  (let ((memory-log (memory-profiler-log)))
    (when memory-log
      (profiler-log-fixup memory-log)
      (profiler-report-log memory-log))))

(defun profiler-report ()
  (interactive)
  (sample-profiler-report)
  (memory-profiler-report))

;;;###autoload
(defun profiler-find-log (filename)
  (interactive
   (list (read-file-name "Find log: " default-directory)))
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (let ((log (read (current-buffer))))
      (profiler-report-log log))))



;;; Profiling helpers

(cl-defmacro with-sample-profiling ((&key (interval profiler-sample-interval)) &rest body)
  `(progn
     (sample-profiler-start ,interval)
     (sample-profiler-reset)
     (unwind-protect
	 (progn ,@body)
       (sample-profiler-stop)
       (sample-profiler-report)
       (sample-profiler-reset))))

(cl-defmacro with-memory-profiling (() &rest body)
  `(progn
     (memory-profiler-start)
     (memory-profiler-reset)
     (unwind-protect
	 (progn ,@body)
       (memory-profiler-stop)
       (memory-profiler-report)
       (memory-profiler-reset))))

(provide 'profiler)
;;; profiler.el ends here
