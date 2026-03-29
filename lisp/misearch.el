;;; misearch.el --- isearch extensions for multi-buffer search  -*- lexical-binding: t; -*-

;; Copyright (C) 2007-2026 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file adds more dimensions to the search space.  It implements
;; various features that extend isearch.  One of them is an ability to
;; search through multiple buffers.

;;; Code:

(require 'cl-lib)

;;; Search multiple buffers

;;;###autoload (add-hook 'isearch-mode-hook 'multi-isearch-setup)

(defgroup multi-isearch nil
  "Using isearch to search through multiple buffers."
  :version "23.1"
  :group 'isearch)

(defcustom multi-isearch-search t
  "Non-nil enables searching multiple related buffers, in certain modes."
  :type 'boolean
  :version "23.1")

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
  :version "23.1")

;;;###autoload
(defvar multi-isearch-next-buffer-function nil
  "Function to call to get the next buffer to search.

When this variable is set to a function that returns a buffer, then
after typing another \\[isearch-forward] or \\[isearch-backward] \
at a failing search, the search goes
to the next buffer in the series and continues searching for the
next occurrence.

This function should return the next buffer (it doesn't need to switch
to it), or nil if it can't find the next buffer (when it reaches the
end of the search space).

The first argument of this function is the current buffer where the
search is currently searching.  It defines the base buffer relative to
which this function should find the next buffer.  When the isearch
direction is backward (when option `isearch-forward' is nil), this function
should return the previous buffer to search.

If the second argument of this function WRAP is non-nil, then it
should return the first buffer in the series; and for the backward
search, it should return the last buffer in the series.")

;;;###autoload
(defvar multi-isearch-next-buffer-current-function nil
  "The currently active function to get the next buffer to search.
Initialized from `multi-isearch-next-buffer-function' when
Isearch starts.")

;;;###autoload
(defvar multi-isearch-current-buffer nil
  "The buffer where the search is currently searching.
The value is nil when the search still is in the initial buffer.")

;;;###autoload
(defvar multi-isearch-buffer-list nil
  "Sequence of buffers visited by multiple buffers Isearch.
This is nil if Isearch is not currently searching more than one buffer.")
;;;###autoload
(defvar multi-isearch-file-list nil
  "Sequence of files visited by multiple file buffers Isearch.")

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
    (setq-default isearch-search-fun-function #'multi-isearch-search-fun
		  isearch-wrap-function       #'multi-isearch-wrap
		  isearch-push-state-function #'multi-isearch-push-state)
    (add-hook 'isearch-mode-end-hook #'multi-isearch-end)))

(defun multi-isearch-end ()
  "Clean up the multi-buffer search after terminating isearch."
  (setq multi-isearch-current-buffer nil
	multi-isearch-next-buffer-current-function nil
	multi-isearch-buffer-list nil
	multi-isearch-file-list nil)
  (setq-default isearch-search-fun-function multi-isearch-orig-search-fun
		isearch-wrap-function       multi-isearch-orig-wrap
		isearch-push-state-function multi-isearch-orig-push-state)
  (remove-hook 'isearch-mode-end-hook #'multi-isearch-end))

(defun multi-isearch-search-fun ()
  "Return the proper search function, for isearch in multiple buffers."
  (lambda (string bound noerror)
    (let ((search-fun
	   ;; Use standard functions to search within one buffer
	   (isearch-search-fun-default))
	  found buffer)
      (or
       ;; 1. First try searching in the initial buffer
       (let ((res (funcall search-fun string bound noerror)))
	 ;; Reset wrapping for all-buffers pause after successful search
	 (if (and res (not bound) (eq multi-isearch-pause t))
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
					   (or buffer (current-buffer)) nil))
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
	       ;; (`with-current-buffer' raises an error for nil returned from it).
	       (error (signal 'search-failed (list string "end of multi"))))
	   (signal 'search-failed (list string "repeat for next buffer"))))))))

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
	(setq multi-isearch-current-buffer
	      (funcall multi-isearch-next-buffer-current-function
		       (current-buffer) t))
	(multi-isearch-switch-buffer)
	(goto-char (if isearch-forward (point-min) (point-max))))
    (setq multi-isearch-current-buffer (current-buffer))
    (setq isearch-wrapped nil)))

(defun multi-isearch-push-state ()
  "Save a function restoring the state of multiple buffers search.
Save the current buffer to the additional state parameter in the
search status stack."
  (let ((buf (current-buffer)))
    (lambda (cmd)
      (multi-isearch-pop-state cmd buf))))

(defun multi-isearch-pop-state (_cmd buffer)
  "Restore the multiple buffers search state in BUFFER.
Switch to the buffer restored from the search status stack."
  (unless (eq buffer (current-buffer))
    (setq multi-isearch-current-buffer buffer)
    (multi-isearch-switch-buffer)))

;;;###autoload
(defun multi-isearch-switch-buffer ()
  "Switch to the next buffer in multi-buffer search."
  (when (and (buffer-live-p multi-isearch-current-buffer)
             (not (eq multi-isearch-current-buffer (current-buffer))))
    (setq isearch-mode nil)
    (switch-to-buffer multi-isearch-current-buffer)
    (setq isearch-mode " M-Isearch")))


;;; Global multi-buffer search invocations

(defun multi-isearch-next-buffer-from-list (&optional buffer wrap)
  "Return the next buffer in the series of buffers.
This function is used for multiple buffers Isearch.  A sequence of
buffers is defined by the variable `multi-isearch-buffer-list'
set in `multi-isearch-buffers' or `multi-isearch-buffers-regexp'."
  (let ((buffers (if isearch-forward
		     multi-isearch-buffer-list
		   (reverse multi-isearch-buffer-list))))
    (if wrap
	(car buffers)
      (cadr (member buffer buffers)))))

(defvar ido-ignore-item-temp-list)  ; from ido.el

(defun multi-isearch-read-buffers ()
  "Return a list of buffers specified interactively, one by one."
  ;; Most code from `multi-occur'.
  (let* ((bufs (list (read-buffer "First buffer to search: "
				  (current-buffer) t)))
	 (buf nil)
	 (ido-ignore-item-temp-list bufs))
    (while (not (string-equal
		 (setq buf (read-buffer (multi-occur--prompt) nil t))
		 ""))
      (cl-pushnew buf bufs :test #'equal)
      (setq ido-ignore-item-temp-list bufs))
    (nreverse bufs)))

(defun multi-isearch-read-matching-buffers ()
  "Return a list of buffers whose names match specified regexp.
Uses `read-regexp' to read the regexp."
  ;; Most code from `multi-occur-in-matching-buffers'
  ;; and `kill-matching-buffers'.
  (let ((bufregexp
	 (read-regexp "Search in buffers whose names match regexp")))
    (when bufregexp
      (delq nil (mapcar (lambda (buf)
			  (when (string-match bufregexp (buffer-name buf))
			    buf))
			(buffer-list))))))

;;;###autoload
(defun multi-isearch-buffers (buffers)
  "Start multi-buffer Isearch on a list of BUFFERS.
This list can contain live buffers or their names.
Interactively read buffer names to search, one by one, ended with RET.
With a prefix argument, ask for a regexp, and search in buffers
whose names match the specified regexp."
  (interactive
   (list (if current-prefix-arg
	     (multi-isearch-read-matching-buffers)
	   (multi-isearch-read-buffers))))
  (let ((multi-isearch-next-buffer-function
	 'multi-isearch-next-buffer-from-list))
    (setq multi-isearch-buffer-list (mapcar #'get-buffer buffers))
    (switch-to-buffer (car multi-isearch-buffer-list))
    (goto-char (if isearch-forward (point-min) (point-max)))
    (isearch-forward nil t)))

;;;###autoload
(defun multi-isearch-buffers-regexp (buffers)
  "Start multi-buffer regexp Isearch on a list of BUFFERS.
This list can contain live buffers or their names.
Interactively read buffer names to search, one by one, ended with RET.
With a prefix argument, ask for a regexp, and search in buffers
whose names match the specified regexp."
  (interactive
   (list (if current-prefix-arg
	     (multi-isearch-read-matching-buffers)
	   (multi-isearch-read-buffers))))
  (let ((multi-isearch-next-buffer-function
	 'multi-isearch-next-buffer-from-list))
    (setq multi-isearch-buffer-list (mapcar #'get-buffer buffers))
    (switch-to-buffer (car multi-isearch-buffer-list))
    (goto-char (if isearch-forward (point-min) (point-max)))
    (isearch-forward-regexp nil t)))


;;; Global multi-file search invocations

(defun multi-isearch-next-file-buffer-from-list (&optional buffer wrap)
  "Return the next buffer in the series of file buffers.
This function is used for multiple file buffers Isearch.  A sequence
of files is defined by the variable `multi-isearch-file-list' set in
`multi-isearch-files' or `multi-isearch-files-regexp'.
Every next/previous file in the defined sequence is visited by
`find-file-noselect' that returns the corresponding file buffer."
  (let ((files (if isearch-forward
		   multi-isearch-file-list
		 (reverse multi-isearch-file-list))))
    (find-file-noselect
     (if wrap
	 (car files)
       (cadr (member (buffer-file-name buffer) files))))))

(defun multi-isearch-read-files ()
  "Return a list of files specified interactively, one by one."
  ;; Most code from `multi-occur'.
  (let* ((files (list (read-file-name "First file to search: "
				      default-directory
				      buffer-file-name)))
	 (file nil))
    (while (not (file-equal-p
		 (setq file (read-file-name
			     "Next file to search (RET to end): "
			     default-directory
			     default-directory))
		 default-directory))
      (cl-pushnew file files :test #'equal))
    (nreverse files)))

;; A regexp is not the same thing as a file glob - does this matter?
(defun multi-isearch-read-matching-files ()
  "Return a list of files whose names match specified wildcard.
Uses `read-regexp' to read the wildcard."
  ;; Most wildcard code from `find-file-noselect'.
  (let ((filename (read-regexp "Search in files whose names match wildcard")))
    (when (and filename
	       (not (string-match "\\`/:" filename))
	       (string-match "[[*?]" filename))
      (condition-case nil
	  (file-expand-wildcards filename t)
	(error (list filename))))))

;;;###autoload
(defun multi-isearch-files (files)
  "Start multi-buffer Isearch on a list of FILES.
Relative file names in this list are expanded to absolute
file names using the current buffer's value of `default-directory'.
Interactively read file names to search, one by one, ended with RET.
With a prefix argument, ask for a wildcard, and search in file buffers
whose file names match the specified wildcard."
  (interactive
   (list (if current-prefix-arg
	     (multi-isearch-read-matching-files)
	   (multi-isearch-read-files))))
  (let ((multi-isearch-next-buffer-function
	 'multi-isearch-next-file-buffer-from-list))
    (setq multi-isearch-file-list (mapcar #'expand-file-name files))
    (find-file (car multi-isearch-file-list))
    (goto-char (if isearch-forward (point-min) (point-max)))
    (isearch-forward nil t)))

;;;###autoload
(defun multi-isearch-files-regexp (files)
  "Start multi-buffer regexp Isearch on a list of FILES.
Relative file names in this list are expanded to absolute
file names using the current buffer's value of `default-directory'.
Interactively read file names to search, one by one, ended with RET.
With a prefix argument, ask for a wildcard, and search in file buffers
whose file names match the specified wildcard."
  (interactive
   (list (if current-prefix-arg
	     (multi-isearch-read-matching-files)
	   (multi-isearch-read-files))))
  (let ((multi-isearch-next-buffer-function
	 'multi-isearch-next-file-buffer-from-list))
    (setq multi-isearch-file-list (mapcar #'expand-file-name files))
    (find-file (car multi-isearch-file-list))
    (goto-char (if isearch-forward (point-min) (point-max)))
    (isearch-forward-regexp nil t)))


;;; Global multi-file replacements as diff

(defcustom multi-file-diff-unsaved 'save-buffers
  "What to do with unsaved edits when showing multi-file replacements as diffs.
If the value is `save-buffers', save unsaved buffers before creating diff.
If the value is `use-file', use text from the file even when the
file-visiting buffer is modified.
If the value is `use-modified-buffer', use text from the file-visiting
modified buffer to be able to use unsaved changes."
  :type '(choice
          (const :tag "Save buffers" save-buffers)
          (const :tag "Use file" use-file)
          (const :tag "Use modified buffer" use-modified-buffer))
  :version "30.1")

(declare-function diff-setup-whitespace "diff-mode" ())
(declare-function diff-setup-buffer-type "diff-mode" ())

(defvar diff--coding-system-for-buffer) ; from diff.el

;;;###autoload
(defun multi-file-replace-as-diff (files from-string replacements regexp-flag delimited-flag)
  "Show as diffs replacements of FROM-STRING with REPLACEMENTS.
FILES is a list of file names.  Also it's possible to provide a list of
buffers in FILES.  REGEXP-FLAG and DELIMITED-FLAG have the same meaning
as in `perform-replace'."
  (require 'diff)
  (let ((inhibit-message t)
        (diff-buffer (get-buffer-create "*replace-diff*")))
    (when (eq multi-file-diff-unsaved 'save-buffers)
      (save-some-buffers t (lambda ()
                             (seq-some (lambda (f-or-b)
                                         (equal f-or-b buffer-file-name))
                                       files))))
    (with-current-buffer diff-buffer
      (buffer-disable-undo (current-buffer))
      (let ((inhibit-read-only t))
        (erase-buffer))
      ;; Make the *vc-diff* buffer read only, the diff-mode key
      ;; bindings are nicer for read only buffers.
      (setq buffer-read-only t)
      (diff-mode))
    (dolist (file-name files)
      (let* ((non-file-buffer (and (buffer-live-p file-name)
                                   (not (buffer-local-value
                                         'buffer-file-name file-name))))
             (file-exists (unless non-file-buffer
                            (file-exists-p file-name)))
             (file-buffer
              (if non-file-buffer
                  file-name
                (when (or (not file-exists)
                          (eq multi-file-diff-unsaved 'use-modified-buffer))
                  (find-buffer-visiting file-name))))
             ;; Make sure any supported characters can be written to a
             ;; file without asking the user to select a safe
             ;; coding-system.
             (diff--coding-system-for-buffer 'utf-8-emacs))
        (when non-file-buffer (setq file-name (buffer-name file-name)))
        (when (or file-exists file-buffer)
          (with-temp-buffer
            (if (and file-buffer
                     (or (not file-exists)
                         (buffer-modified-p file-buffer)))
                (insert-buffer-substring file-buffer)
              (insert-file-contents file-name))
            (goto-char (point-min))
            (perform-replace from-string replacements nil regexp-flag delimited-flag)
            (multi-file-diff-no-select
             (if file-exists file-name file-buffer)
             (current-buffer) nil diff-buffer
             (concat file-name "~") file-name)))))
    (with-current-buffer diff-buffer
      (diff-setup-whitespace)
      (diff-setup-buffer-type)
      (buffer-enable-undo (current-buffer))
      (setq-local revert-buffer-function
                  (lambda (_ignore-auto _noconfirm)
                    (multi-file-replace-as-diff
                     files from-string replacements regexp-flag delimited-flag)))
      (goto-char (point-min)))
    (pop-to-buffer diff-buffer)))

;;;###autoload
(defun multi-file-replace-regexp-as-diff (files regexp to-string &optional delimited)
  "Show as diffs replacements of REGEXP with TO-STRING in FILES.
DELIMITED has the same meaning as in `replace-regexp'.
The replacements are displayed in the buffer *replace-diff* that
you can later apply as a patch after reviewing the changes."
  (interactive
   (let ((files (multi-isearch-read-files))
         (common
          (query-replace-read-args
           (concat "Replace"
                   (if current-prefix-arg " word" "")
                   " regexp as diff in files")
           t t)))
     (list files (nth 0 common) (nth 1 common) (nth 2 common))))
  (multi-file-replace-as-diff files regexp to-string t delimited))

;;;###autoload
(defun replace-regexp-as-diff (regexp to-string &optional delimited)
  "Show as diffs replacements of REGEXP with TO-STRING in the current buffer.
DELIMITED has the same meaning as in `replace-regexp'.
The replacements are displayed in the buffer *replace-diff* that
you can later apply as a patch after reviewing the changes."
  (interactive
   (let ((common
          (query-replace-read-args
           (concat "Replace"
                   (if current-prefix-arg " word" "")
                   " regexp as diff")
           t t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common))))
  (multi-file-replace-as-diff
   (list (or buffer-file-name (current-buffer)))
   regexp to-string t delimited))

(defvar diff-use-labels)
(declare-function diff-check-labels "diff" (&optional force))
(declare-function diff-file-local-copy "diff" (file-or-buf))

(defun multi-file-diff-no-select (old new &optional switches buf label-old label-new)
  ;; Based on `diff-no-select' tailored to multi-file diffs.
  "Compare the OLD and NEW file/buffer.
If the optional SWITCHES is nil, the switches specified in the
variable `diff-switches' are passed to the diff command,
otherwise SWITCHES is used.  SWITCHES can be a string or a list
of strings.  BUF should be non-nil.  LABEL-OLD and LABEL-NEW
specify labels to use for file names."
  (require 'diff)
  (unless (bufferp new) (setq new (expand-file-name new)))
  (unless (bufferp old) (setq old (expand-file-name old)))
  (or switches (setq switches diff-switches)) ; If not specified, use default.
  (setq switches (ensure-list switches))
  (diff-check-labels)
  (let* ((old-alt (diff-file-local-copy old))
         (new-alt (diff-file-local-copy new))
         (command
          (mapconcat #'identity
                     `(,diff-command
                       ;; Use explicitly specified switches
                       ,@switches
                       ,@(mapcar #'shell-quote-argument
                                 (nconc
                                  (and (or old-alt new-alt)
                                       (eq diff-use-labels t)
                                       (list "--label"
                                             (cond ((stringp label-old) label-old)
                                                   ((stringp old) old)
                                                   ((prin1-to-string old)))
                                             "--label"
                                             (cond ((stringp label-new) label-new)
                                                   ((stringp new) new)
                                                   ((prin1-to-string new)))))
                                  (list (or old-alt old)
                                        (or new-alt new)))))
                     " ")))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (coding-system-for-read (or diff--coding-system-for-buffer
                                        coding-system-for-read)))
        (insert command "\n")
        (call-process shell-file-name nil buf nil
                      shell-command-switch command))
      (if old-alt (delete-file old-alt))
      (if new-alt (delete-file new-alt)))))


(defvar unload-function-defs-list)

(defun multi-isearch-unload-function ()
  "Remove autoloaded variables from `unload-function-defs-list'.
Also prevent the feature from being reloaded via `isearch-mode-hook'."
  (remove-hook 'isearch-mode-hook #'multi-isearch-setup)
  (let ((defs (list (car unload-function-defs-list)))
	(auto '(multi-isearch-next-buffer-function
		multi-isearch-next-buffer-current-function
		multi-isearch-current-buffer
		multi-isearch-buffer-list multi-isearch-file-list)))
    (dolist (def (cdr unload-function-defs-list))
      (unless (and (symbolp def)
		   (memq def auto))
	(push def defs)))
    (setq unload-function-defs-list (nreverse defs))
    ;; .
    nil))

(defalias 'misearch-unload-function #'multi-isearch-unload-function)


(provide 'multi-isearch)
(provide 'misearch)
;;; misearch.el ends here
