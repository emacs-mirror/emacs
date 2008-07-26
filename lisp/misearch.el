;;; misearch.el --- isearch extensions for multi-buffer search

;; Copyright (C) 2007, 2008  Free Software Foundation, Inc.

;; Author: Juri Linkov <juri@jurta.org>
;; Keywords: matching

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file adds more dimensions to the search space.  It implements
;; various features that extend isearch.  One of them is an ability to
;; search through multiple buffers.

;;; Code:

;;; Search multiple buffers

;;;###autoload (add-hook 'isearch-mode-hook 'multi-isearch-setup)

(defgroup multi-isearch nil
  "Using isearch to search through multiple buffers."
  :version "23.1"
  :group 'isearch)

(defcustom multi-isearch-search t
  "Non-nil enables searching multiple related buffers, in certain modes."
  :type 'boolean
  :version "23.1"
  :group 'multi-isearch)

(defcustom multi-isearch-pause t
  "A choice defining where to pause the search.
If the value is nil, don't pause before going to the next buffer.
If the value is `initial', pause only after a failing search in the
initial buffer.
If t, pause in all buffers that contain the search string."
  :type '(choice
	  (const :tag "Don't pause" nil)
	  (const :tag "Only in initial buffer" initial)
	  (const :tag "All buffers" t))
  :version "23.1"
  :group 'multi-isearch)

;;;###autoload
(defvar multi-isearch-next-buffer-function nil
  "Function to call to get the next buffer to search.

When this variable is set to a function that returns a buffer, then
after typing another \\[isearch-forward] or \\[isearch-backward] \
at a failing search, the search goes
to the next buffer in the series and continues searching for the
next occurrence.

The first argument of this function is the current buffer where the
search is currently searching.  It defines the base buffer relative to
which this function should find the next buffer.  When the isearch
direction is backward (when `isearch-forward' is nil), this function
should return the previous buffer to search.  If the second argument of
this function WRAP is non-nil, then it should return the first buffer
in the series; and for the backward search, it should return the last
buffer in the series.")

;;;###autoload
(defvar multi-isearch-next-buffer-current-function nil
  "The currently active function to get the next buffer to search.
Initialized from `multi-isearch-next-buffer-function' when
Isearch starts.")

;;;###autoload
(defvar multi-isearch-current-buffer nil
  "The buffer where the search is currently searching.
The value is nil when the search still is in the initial buffer.")

(defvar multi-isearch-orig-search-fun nil)
(defvar multi-isearch-orig-wrap nil)
(defvar multi-isearch-orig-push-state nil)


;;;###autoload
(defun multi-isearch-setup ()
  "Set up isearch to search multiple buffers.
Intended to be added to `isearch-mode-hook'."
  (when (and multi-isearch-search
	     multi-isearch-next-buffer-function)
    (setq multi-isearch-current-buffer nil
	  multi-isearch-next-buffer-current-function
	  multi-isearch-next-buffer-function
	  multi-isearch-orig-search-fun
	  (default-value 'isearch-search-fun-function)
	  multi-isearch-orig-wrap
	  (default-value 'isearch-wrap-function)
	  multi-isearch-orig-push-state
	  (default-value 'isearch-push-state-function))
    (setq-default isearch-search-fun-function 'multi-isearch-search-fun
		  isearch-wrap-function       'multi-isearch-wrap
		  isearch-push-state-function 'multi-isearch-push-state)
    (add-hook 'isearch-mode-end-hook  'multi-isearch-end)))

(defun multi-isearch-end ()
  "Clean up the multi-buffer search after terminating isearch."
  (setq multi-isearch-current-buffer nil
	multi-isearch-next-buffer-current-function nil)
  (setq-default isearch-search-fun-function multi-isearch-orig-search-fun
		isearch-wrap-function       multi-isearch-orig-wrap
		isearch-push-state-function multi-isearch-orig-push-state)
  (remove-hook 'isearch-mode-end-hook  'multi-isearch-end))

(defun multi-isearch-search-fun ()
  "Return the proper search function, for isearch in multiple buffers."
  (lambda (string bound noerror)
    (let ((search-fun
	   ;; Use standard functions to search within one buffer
	   (cond
	    (isearch-word
	     (if isearch-forward 'word-search-forward 'word-search-backward))
	    (isearch-regexp
	     (if isearch-forward 're-search-forward 're-search-backward))
	    (t
	     (if isearch-forward 'search-forward 'search-backward))))
	  found buffer)
      (or
       ;; 1. First try searching in the initial buffer
       (let ((res (funcall search-fun string bound noerror)))
	 ;; Reset wrapping for all-buffers pause after successful search
	 (if (and res (eq multi-isearch-pause t))
	     (setq multi-isearch-current-buffer nil))
	 res)
       ;; 2. If the above search fails, start visiting next/prev buffers
       ;; successively, and search the string in them.  Do this only
       ;; when bound is nil (i.e. not while lazy-highlighting search
       ;; strings in the current buffer).
       (when (and (not bound) multi-isearch-search)
	 ;; If no-pause or there was one attempt to leave the current buffer
	 (if (or (null multi-isearch-pause)
		 (and multi-isearch-pause multi-isearch-current-buffer))
	     (condition-case nil
		 (progn
		   (while (not found)
		     ;; Find the next buffer to search
		     (setq buffer (funcall multi-isearch-next-buffer-current-function
					   buffer))
		     (with-current-buffer buffer
		       (goto-char (if isearch-forward (point-min) (point-max)))
		       (setq isearch-barrier (point) isearch-opoint (point))
		       ;; After visiting the next/prev buffer search the
		       ;; string in it again, until the function in
		       ;; multi-isearch-next-buffer-current-function raises
		       ;; an error at the beginning/end of the buffer sequence.
		       (setq found (funcall search-fun string bound noerror))))
		   ;; Set buffer for isearch-search-string to switch
		   (if buffer (setq multi-isearch-current-buffer buffer))
		   ;; Return point of the new search result
		   found)
	       ;; Return nil when multi-isearch-next-buffer-current-function fails
	       (error nil))
	   (signal 'search-failed (list string "Repeat for next buffer"))))))))

(defun multi-isearch-wrap ()
  "Wrap the multiple buffers search when search is failed.
Switch buffer to the first buffer for a forward search,
or to the last buffer for a backward search.
Set `multi-isearch-current-buffer' to the current buffer to display
the isearch suffix message [initial buffer] only when isearch leaves
the initial buffer."
  (if (or (null multi-isearch-pause)
	  (and multi-isearch-pause multi-isearch-current-buffer))
      (progn
	(switch-to-buffer
	 (setq multi-isearch-current-buffer
	       (funcall multi-isearch-next-buffer-current-function
			(current-buffer) t)))
	(goto-char (if isearch-forward (point-min) (point-max))))
    (setq multi-isearch-current-buffer (current-buffer))
    (setq isearch-wrapped nil)))

(defun multi-isearch-push-state ()
  "Save a function restoring the state of multiple buffers search.
Save the current buffer to the additional state parameter in the
search status stack."
  `(lambda (cmd)
     (multi-isearch-pop-state cmd ,(current-buffer))))

(defun multi-isearch-pop-state (cmd buffer)
  "Restore the multiple buffers search state.
Switch to the buffer restored from the search status stack."
  (unless (equal buffer (current-buffer))
    (switch-to-buffer (setq multi-isearch-current-buffer buffer))))


;;; Global multi-buffer search invocations

(defvar multi-isearch-buffer-list nil)

(defun multi-isearch-next-buffer-from-list (&optional buffer wrap)
  "Return the next buffer in the series of ChangeLog file buffers.
This function is used for multiple buffers isearch.
A sequence of buffers is formed by ChangeLog files with decreasing
numeric file name suffixes in the directory of the initial ChangeLog
file were isearch was started."
  (let ((buffers (if isearch-forward
		     multi-isearch-buffer-list
		   (reverse multi-isearch-buffer-list))))
    (if wrap
	(car buffers)
      (cadr (member (or buffer (current-buffer)) buffers)))))

;;;###autoload
(defun multi-isearch-buffers (buffers)
  "Start multi-buffer Isearch on a list of BUFFERS."
  (let ((multi-isearch-next-buffer-function
	 'multi-isearch-next-buffer-from-list)
	(multi-isearch-buffer-list buffers))
    (switch-to-buffer (car buffers))
    (goto-char (if isearch-forward (point-min) (point-max)))
    (isearch-forward)))

;;;###autoload
(defun multi-isearch-buffers-regexp (buffers)
  "Start multi-buffer regexp Isearch on a list of BUFFERS."
  (let ((multi-isearch-next-buffer-function
	 'multi-isearch-next-buffer-from-list)
	(multi-isearch-buffer-list buffers))
    (switch-to-buffer (car buffers))
    (goto-char (if isearch-forward (point-min) (point-max)))
    (isearch-forward-regexp)))


;;; Global multi-file search invocations

(defvar multi-isearch-file-list nil)

(defun multi-isearch-next-file-buffer-from-list (&optional buffer wrap)
  "Return the next buffer in the series of ChangeLog file buffers.
This function is used for multiple buffers isearch.
A sequence of buffers is formed by ChangeLog files with decreasing
numeric file name suffixes in the directory of the initial ChangeLog
file were isearch was started."
  (let ((files (if isearch-forward
		   multi-isearch-file-list
		 (reverse multi-isearch-file-list))))
    (find-file-noselect
     (if wrap
	 (car files)
       (cadr (member (buffer-file-name buffer) files))))))

;;;###autoload
(defun multi-isearch-files (files)
  "Start multi-buffer Isearch on a list of FILES."
  (let ((multi-isearch-next-buffer-function
	 'multi-isearch-next-file-buffer-from-list)
	(multi-isearch-file-list files))
    (find-file (car files))
    (goto-char (if isearch-forward (point-min) (point-max)))
    (isearch-forward)))

;;;###autoload
(defun multi-isearch-files-regexp (files)
  "Start multi-buffer regexp Isearch on a list of FILES."
  (let ((multi-isearch-next-buffer-function
	 'multi-isearch-next-file-buffer-from-list)
	(multi-isearch-file-list files))
    (find-file (car files))
    (goto-char (if isearch-forward (point-min) (point-max)))
    (isearch-forward-regexp)))


(provide 'multi-isearch)

;; arch-tag: a6d38ffa-4d14-4e39-8ac6-46af9d6a6773
;;; misearch.el ends here
