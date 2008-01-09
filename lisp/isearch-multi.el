;;; isearch-multi.el --- isearch extensions for multi-buffer search

;; Copyright (C) 2007, 2008  Free Software Foundation, Inc.

;; Author: Juri Linkov <juri@jurta.org>
;; Keywords: matching

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file adds more dimensions to the search space.  It implements
;; various features that extend isearch.  One of them is an ability to
;; search through multiple buffers.

;;; Code:

;;; Search multiple buffers

(defgroup isearch-buffers nil
  "Using isearch to search through multiple buffers."
  :version "23.1"
  :group 'isearch)

(defcustom isearch-buffers-multi t
  "Non-nil enables searching multiple related buffers, in certain modes."
  :type 'boolean
  :version "23.1"
  :group 'isearch-buffers)

(defcustom isearch-buffers-pause t
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
  :group 'isearch-buffers)

;;;###autoload
(defvar isearch-buffers-current-buffer nil
  "The buffer where the search is currently searching.
The value is nil when the search still is in the initial buffer.")

;;;###autoload
(defvar isearch-buffers-next-buffer-function nil
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
(define-minor-mode isearch-buffers-minor-mode
  "Minor mode for using isearch to search through multiple buffers.
With arg, turn isearch-buffers minor mode on if arg is positive, off otherwise."
  :group 'isearch-buffers ;; :lighter " X"
  (if isearch-buffers-minor-mode
      (progn
	(add-hook 'isearch-mode-hook 'isearch-buffers-init nil t)
	(set (make-local-variable 'isearch-search-fun-function)
	     'isearch-buffers-search-fun)
	(set (make-local-variable 'isearch-wrap-function)
	     'isearch-buffers-wrap)
	(set (make-local-variable 'isearch-push-state-function)
	     'isearch-buffers-push-state))
    (remove-hook 'isearch-mode-hook 'isearch-buffers-init t)
    (kill-local-variable 'isearch-search-fun-function)
    (kill-local-variable 'isearch-wrap-function)
    (kill-local-variable 'isearch-push-state-function)))

(defun isearch-buffers-init ()
  "Set up isearch to search multiple buffers.
Intended to be added to `isearch-mode-hook'."
  (setq isearch-buffers-current-buffer nil))

(defun isearch-buffers-search-fun ()
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
	 (if (and res (eq isearch-buffers-pause t))
	     (setq isearch-buffers-current-buffer nil))
	 res)
       ;; 2. If the above search fails, start visiting next/prev buffers
       ;; successively, and search the string in them.  Do this only
       ;; when bound is nil (i.e. not while lazy-highlighting search
       ;; strings in the current buffer).
       (when (and (not bound) isearch-buffers-multi)
	 ;; If no-pause or there was one attempt to leave the current buffer
	 (if (or (null isearch-buffers-pause)
		 (and isearch-buffers-pause isearch-buffers-current-buffer))
	     (condition-case nil
		 (progn
		   (while (not found)
		     ;; Find the next buffer to search
		     (setq buffer (funcall isearch-buffers-next-buffer-function
					   buffer))
		     (with-current-buffer buffer
		       (goto-char (if isearch-forward (point-min) (point-max)))
		       (setq isearch-barrier (point) isearch-opoint (point))
		       ;; After visiting the next/prev buffer search the
		       ;; string in it again, until the function in
		       ;; isearch-buffers-next-buffer-function raises an error
		       ;; at the beginning/end of the buffer sequence.
		       (setq found (funcall search-fun string bound noerror))))
		   ;; Set buffer for isearch-search-string to switch
		   (if buffer (setq isearch-buffers-current-buffer buffer))
		   ;; Return point of the new search result
		   found)
	       ;; Return nil when isearch-buffers-next-buffer-function fails
	       (error nil))
	   (signal 'search-failed (list string "Repeat for next buffer"))))))))

(defun isearch-buffers-wrap ()
  "Wrap the multiple buffers search when search is failed.
Switch buffer to the first buffer for a forward search,
or to the last buffer for a backward search.
Set `isearch-buffers-current-buffer' to the current buffer to display
the isearch suffix message [initial buffer] only when isearch leaves
the initial buffer."
  (if (or (null isearch-buffers-pause)
	  (and isearch-buffers-pause isearch-buffers-current-buffer))
      (progn
	(switch-to-buffer
	 (setq isearch-buffers-current-buffer
	       (funcall isearch-buffers-next-buffer-function
			(current-buffer) t)))
	(goto-char (if isearch-forward (point-min) (point-max))))
    (setq isearch-buffers-current-buffer (current-buffer))
    (setq isearch-wrapped nil)))

(defun isearch-buffers-push-state ()
  "Save a function restoring the state of multiple buffers search.
Save the current buffer to the additional state parameter in the
search status stack."
  `(lambda (cmd)
     (isearch-buffers-pop-state cmd ,(current-buffer))))

(defun isearch-buffers-pop-state (cmd buffer)
  "Restore the multiple buffers search state.
Switch to the buffer restored from the search status stack."
  (unless (equal buffer (current-buffer))
    (switch-to-buffer (setq isearch-buffers-current-buffer buffer))))

(provide 'isearch-multi)

;; arch-tag: a6d38ffa-4d14-4e39-8ac6-46af9d6a6773
;;; isearch-multi.el ends here
