;;; saveplace.el --- automatically save place in files  -*- lexical-binding:t -*-

;; Copyright (C) 1993-1994, 2001-2025 Free Software Foundation, Inc.

;; Author: Karl Fogel <kfogel@red-bean.com>
;; Maintainer: emacs-devel@gnu.org
;; Created: July, 1993
;; Keywords: bookmarks, placeholders

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

;; Automatically save place in files, so that visiting them later
;; (even during a different Emacs session) automatically moves point
;; to the saved position, when the file is first found.  Uses the
;; value of buffer-local variable save-place-mode to determine whether to
;; save position or not.
;;
;; Thanks to Stefan Schoef, who sent a patch with the
;; `save-place-version-control' stuff in it.

;;; Code:

(require 'cl-lib)

;; this is what I was using during testing:
;; (keymap-set ctl-x-map "p" 'toggle-save-place-globally)

(defgroup save-place nil
  "Automatically save place in files."
  :group 'data)

(defvar save-place-alist nil
  "Alist of saved places to go back to when revisiting files.
Each element looks like (FILENAME . POSITION);
visiting file FILENAME goes automatically to position POSITION
rather than the beginning of the buffer.
A list element can also have the form

   (DIRECTORY (dired-filename . FILENAME))

where DIRECTORY is the name of a directory ending in a slash,
and FILENAME is the name of a file in that directory.  This
format is used for saving places in Dired buffers, see the
function `save-place-dired-hook'; the FILENAME is the file
where point was located in the Dired listing of DIRECTORY
when the place in that buffer was recorded.

This alist is saved between Emacs sessions.")

(defcustom save-place-file
  (locate-user-emacs-file '("places.eld" "places") ".emacs-places")
  "Name of the file that records `save-place-alist' value."
  :version "31.1"
  :type 'file)

(defcustom save-place-version-control nil
  "Controls whether to make numbered backups of master `save-place' file.
It can have four values: t, nil, `never', and `nospecial'.  The first
three have the same meaning that they do for the variable
`version-control', and the final value `nospecial' means just use the
value of `version-control'."
  :type '(radio (const :tag "Unconditionally" t)
		(const :tag "For VC Files" nil)
		(const never)
		(const :tag "Use value of `version-control'" nospecial)))

(defvar save-place-loaded nil
  "Non-nil means that the `save-place-file' has been loaded.")

(defcustom save-place-limit 400
  "Maximum number of entries to retain in the list; nil means no limit."
  :version "24.1"                       ; nil -> 400
  :type '(choice (integer :tag "Entries" :value 1)
		 (const :tag "No Limit" nil)))

(defcustom save-place-forget-unreadable-files t
  "Non-nil means forget place in unreadable files.

The filenames in `save-place-alist' that do not match
`save-place-skip-check-regexp' are filtered through
`file-readable-p'.  If nil, their alist entries are removed.

You may do this anytime by calling the complementary function,
`save-place-forget-unreadable-files'.  When this option is turned on,
this happens automatically before saving `save-place-alist' to
`save-place-file'."
  :type 'boolean)

(defun save-place-load-alist-from-file ()
  (unless save-place-loaded
    (setq save-place-loaded t)
    ;; FIXME: Obey `save-place-abbreviate-file-names'?
    (let ((file (expand-file-name save-place-file)))
      ;; make sure that the alist does not get overwritten, and then
      ;; load it if it exists:
      (when (file-readable-p file)
        ;; don't want to use find-file because we have been
        ;; adding hooks to it.
        (with-temp-buffer
          ;; Make sure our 'coding:' cookie in the save-place
          ;; file will take effect, in case the caller binds
          ;; coding-system-for-read.
          (let (coding-system-for-read)
            (insert-file-contents file))
          (goto-char (point-min))
          (setq save-place-alist
                (with-demoted-errors "Error reading save-place-file: %S"
                  (car (read-from-string
                        (buffer-substring (point-min) (point-max))))))

          ;; If there is a limit, and we're over it, then we'll
          ;; have to truncate the end of the list:
          (if save-place-limit
              (if (<= save-place-limit 0)
                  ;; Zero gets special cased.  I'm not thrilled
                  ;; with this, but the loop for >= 1 is tight.
                  (setq save-place-alist nil)
                ;; Else the limit is >= 1, so enforce it by
                ;; counting and then `setcdr'ing.
                (let ((s save-place-alist)
                      (count 1))
                  (while s
                    (if (>= count save-place-limit)
                        (setcdr s nil)
                      (setq count (1+ count)))
                    (setq s (cdr s))))))))
      (save-place--normalize-alist))))

(defcustom save-place-abbreviate-file-names nil
  "If non-nil, abbreviate file names before saving them.
This can simplify sharing the `save-place-file' file across
different hosts.

Changing this option requires rewriting `save-place-alist' with
corresponding file name format, therefore setting this option
just using `setq' may cause out-of-sync problems.  You should use
either `setopt' or \\[customize-variable] to set this option."
  :type 'boolean
  :set (lambda (sym val)
         (let ((old (if (default-boundp sym) (default-value sym))))
           (set-default sym val)
           (if (or (equal old val) (not save-place-loaded))
               nil                      ;Nothing to do.
             (save-place--normalize-alist))))
  :version "28.1")

(defun save-place--normalize-alist ()
  (let ((fun (if save-place-abbreviate-file-names
                 #'abbreviate-file-name #'expand-file-name))
        ;; Don't expand file names for non-existing remote connections.
        (non-essential t))
    (setq save-place-alist
          (cl-delete-duplicates
           (cl-loop for (k . v) in save-place-alist
                    collect
                    (cons (funcall fun k)
                          (if (listp v)
                              (cl-loop for (k1 . v1) in v
                                       collect
                                       (cons k1 (funcall fun v1)))
                            v)))
           :key #'car
           :from-end t
           :test #'equal))))

(defcustom save-place-save-skipped t
  "If non-nil, remember files matching `save-place-skip-check-regexp'.

When filtering `save-place-alist' for unreadable files, some will not
be checked, based on said regexp, and instead saved or forgotten based
on this flag."
  :type 'boolean)

(defcustom save-place-skip-check-regexp
  ;; thanks to ange-ftp-name-format
  "\\`/\\(?:cdrom\\|floppy\\|mnt\\|\\(?:[^@/:]*@\\)?[^@/:]*[^@/:.]:\\)"
  "Regexp whose file names shall not be checked for readability.

When forgetting unreadable files, file names matching this regular
expression shall not be checked for readability, but instead be
subject to `save-place-save-skipped'.

Files for which such a check may be inconvenient include those on
removable and network volumes."
  :type 'regexp)

(defcustom save-place-ignore-files-regexp
  "\\(?:COMMIT_EDITMSG\\|hg-editor-[[:alnum:]]+\\.txt\\|svn-commit\\.tmp\\|bzr_log\\.[[:alnum:]]+\\)$"
  "Regexp matching files whose positions should not be recorded.
Useful to exclude temporary files, such as commit message files that are
automatically created by VCSes.  If set to nil, this feature is
disabled, i.e., no files are excluded."
  :version "24.1"
  :type '(choice (const :tag "Don't exclude any files" nil)
                 regexp))

(declare-function dired-current-directory "dired" (&optional localp))

(defvar save-place--autosave-timer nil)

(defun save-place--cancel-timer ()
  "Cancel `save-place-autosave' timer, if set."
  (when (timerp save-place--autosave-timer)
    (cancel-timer save-place--autosave-timer))
  (setq save-place--autosave-timer nil))

(defvar save-place-autosave-interval)

(defun save-place--manage-timer ()
  "Set or cancel an invocation of `save-place--autosave' on a timer.
If `save-place-mode' is enabled, set the timer, otherwise cancel the timer."
  (if (and save-place-mode
           save-place-autosave-interval
           (null save-place--autosave-timer))
      (setq save-place--autosave-timer
	    (run-with-timer
             save-place-autosave-interval
	     save-place-autosave-interval #'save-place--autosave))
    (save-place--cancel-timer)))

(defcustom save-place-autosave-interval nil
  "The interval between auto saves of buffer places.
If set to nil, disables timer-based auto saving."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :version "31.1"
  :set (lambda (sym val)
         (set-default sym val)
         (save-place--cancel-timer)
         (save-place--manage-timer)))

(defun save-place--autosave ()
  "Called by `save-place--autosave-timer'."
  (save-places-to-alist)
  (save-place-alist-to-file))

(defun save-place--setup-hooks (add)
  (cond
   (add
    (add-hook 'find-file-hook #'save-place-find-file-hook t)
    (add-hook 'dired-initial-position-hook #'save-place-dired-hook)
    (unless noninteractive
      (add-hook 'kill-emacs-hook #'save-place-kill-emacs-hook))
    (add-hook 'kill-buffer-hook #'save-place-to-alist))
   (t
    ;; We should remove the hooks, but only if save-place-mode
    ;; is nil everywhere.  Is it worth the trouble, tho?
    ;; (unless (or (default-value 'save-place-mode)
    ;;             (cl-some <save-place-local-mode-p> (buffer-list)))
    ;;   (remove-hook 'find-file-hook #'save-place-find-file-hook)
    ;;   (remove-hook 'dired-initial-position-hook #'save-place-dired-hook)
    ;;   (remove-hook 'kill-emacs-hook #'save-place-kill-emacs-hook)
    ;;   (remove-hook 'kill-buffer-hook #'save-place-to-alist))
    )))

(define-obsolete-variable-alias 'save-place 'save-place-mode "25.1")
;;;###autoload
(define-minor-mode save-place-mode
  "Non-nil means automatically save place in each file.
This means when you visit a file, point goes to the last place
where it was when you previously visited the same file."
  :global t
  (save-place--setup-hooks save-place-mode)
  (save-place--manage-timer))

(make-variable-buffer-local 'save-place-mode)

(define-obsolete-function-alias 'toggle-save-place
  #'save-place-local-mode "25.1")
;;;###autoload
(define-minor-mode save-place-local-mode
  "Toggle whether to save your place in this file between sessions.
If this mode is enabled, point is recorded when you kill the buffer
or exit Emacs.  Visiting this file again will go to that position,
even in a later Emacs session.

To save places automatically in all files, put this in your init
file:

\(save-place-mode 1)"
  :variable save-place-mode
  (if (not (or buffer-file-name (and (derived-mode-p 'dired-mode)
                                     (boundp 'dired-subdir-alist)
				     dired-subdir-alist
				     (dired-current-directory))))
      (message "Buffer `%s' not visiting a file or directory" (buffer-name))
    (save-place--setup-hooks save-place-mode)
    (save-place--manage-timer)))

(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))

(defun save-place-to-alist ()
  "Add current buffer filename and position to `save-place-alist'.
Put filename and point in a cons box and then cons that onto the
front of the `save-place-alist', if `save-place-mode' is non-nil.
Otherwise, just delete that file from the alist.

If `save-place-abbreviate-file-names' is non-nil, abbreviate the
file names."
  ;; First check to make sure alist has been loaded in from the master
  ;; file.  If not, do so, then feel free to modify the alist.  It
  ;; will be saved again when Emacs is killed.
  (or save-place-loaded (save-place-load-alist-from-file))
  (let* ((directory (and (derived-mode-p 'dired-mode)
                         (boundp 'dired-subdir-alist)
			 dired-subdir-alist
			 (dired-current-directory)))
	 (item (or buffer-file-name
		   (and directory
			(expand-file-name (if (consp directory)
					      (car directory)
					    directory))))))
    (when (and item
               (or (not save-place-ignore-files-regexp)
                   (not (string-match save-place-ignore-files-regexp
                                      item))))
      (when save-place-abbreviate-file-names
        (setq item (abbreviate-file-name item)))
      (let ((cell (assoc item save-place-alist))
            (position (cond ((eq major-mode 'hexl-mode)
			     (with-no-warnings
			       (1+ (hexl-current-address))))
			    ((and (derived-mode-p 'dired-mode) directory)
			     (let ((filename (dired-get-filename nil t)))
			       (if filename
                                   (list
                                    (cons 'dired-filename
                                          (if save-place-abbreviate-file-names
                                              (abbreviate-file-name filename)
                                            filename)))
				 (point))))
			    (t (point)))))
        (if cell
            (setq save-place-alist (delq cell save-place-alist)))
        (if (and save-place
                 (not (and (integerp position)
			   (= position 1)))) ;; Optimize out the degenerate case.
            (setq save-place-alist
                  (cons (cons item position)
                        save-place-alist)))))))

(defun save-place-forget-unreadable-files ()
  "Remove unreadable files from `save-place-alist'.
For each entry in the alist, if `file-readable-p' returns nil for the
filename, remove the entry.  Save the new alist (as the first pair
may have changed) back to `save-place-alist'."
  (interactive)
  ;; the following was adapted from an in-place filtering function,
  ;; `filter-mod', used in the original.
  (unless (null save-place-alist)	;says it better than `when'
    ;; first, check all except first
    (let ((fmprev save-place-alist) (fmcur (cdr save-place-alist)))
      (while fmcur			;not null
	;; a value is only saved when it becomes FMPREV.
	(if (if (string-match save-place-skip-check-regexp (caar fmcur))
		save-place-save-skipped
	      (file-readable-p (caar fmcur)))
	    (setq fmprev fmcur)
	  (setcdr fmprev (cdr fmcur)))
	(setq fmcur (cdr fmcur))))
    ;; test first pair, keep it if OK, otherwise 2nd element, which
    ;; may be '()
    (unless (if (string-match save-place-skip-check-regexp
			      (caar save-place-alist))
		save-place-save-skipped
	      (file-readable-p (caar save-place-alist)))
      (setq save-place-alist (cdr save-place-alist)))))

(defun save-place-alist-to-file ()
  (let ((file (expand-file-name save-place-file))
        (coding-system-for-write 'utf-8))
    (with-temp-buffer
      (when save-place-forget-unreadable-files
	(save-place-forget-unreadable-files))
      (insert (format ";;; -*- coding: %s; mode: lisp-data -*-\n"
                      coding-system-for-write))
      (let ((print-length nil)
            (print-level nil))
        (pp save-place-alist (current-buffer)))
      (let ((version-control
             (cond
              ((null save-place-version-control) nil)
              ((eq 'never save-place-version-control) 'never)
              ((eq 'nospecial save-place-version-control) version-control)
              (t
               t))))
	(condition-case nil
	    ;; Don't use write-file; we don't want this buffer to visit it.
            (write-region (point-min) (point-max) file nil
			  (unless (called-interactively-p 'interactive) 'quiet))
	  (file-error (message "Saving places: can't write %s" file)))))))

(defun save-places-to-alist ()
  "Save all buffer filenames and positions to `save-place-alist'.
See `save-place-to-alist'."
  (let ((buf-list (buffer-list)))
    (while buf-list
      ;; put this into a save-excursion in case someone is counting on
      ;; another function in kill-emacs-hook to act on the last buffer
      ;; they were in:
      (with-current-buffer (car buf-list)
	;; save-place checks buffer-file-name too, but we can avoid
	;; overhead of function call by checking here too.
	(when (and (or buffer-file-name
                       (and (derived-mode-p 'dired-mode)
                            (boundp 'dired-subdir-alist)
		            dired-subdir-alist
		            (dired-current-directory)))
                   ;; Don't save place in literally-visited file
                   ;; because this will commonly differ from the place
                   ;; when visiting literally (and
                   ;; `find-file-literally' always places point at the
                   ;; start of the buffer).
                   (not find-file-literally))
	  (save-place-to-alist))
	(setq buf-list (cdr buf-list))))))

(defvar save-place-after-find-file-hook nil
  "Hook run at the end of `save-place-find-file-hook'.")

(defun save-place-find-file-hook ()
  "Function added to `find-file-hook' by `save-place-mode'.
It runs the hook `save-place-after-find-file-hook'."
  (or save-place-loaded (save-place-load-alist-from-file))
  (let ((cell (and (stringp buffer-file-name)
                   (assoc (if save-place-abbreviate-file-names
                              (abbreviate-file-name buffer-file-name)
                            buffer-file-name)
                          save-place-alist))))
    (if cell
	(progn
	  (or revert-buffer-in-progress
	      (and (integerp (cdr cell))
		   (goto-char (cdr cell))))
          ;; and make sure it will be saved again for later
          (setq save-place-mode t))))
  (run-hooks 'save-place-after-find-file-hook))

(declare-function dired-goto-file "dired" (file))

(defun save-place-dired-hook ()
  "Position point in a Dired buffer according to its saved place.
This is run via `dired-initial-position-hook', which see."
  (or save-place-loaded (save-place-load-alist-from-file))
  (when-let* ((directory (and (derived-mode-p 'dired-mode)
                              (boundp 'dired-subdir-alist)
			      dired-subdir-alist
			      (dired-current-directory)))
              (item (expand-file-name (if (consp directory)
					  (car directory)
				        directory)))
	      (cell (assoc (if save-place-abbreviate-file-names
                               (abbreviate-file-name item) item)
		           save-place-alist)))
    (or revert-buffer-in-progress
        (cond
	 ((integerp (cdr cell))
	  (goto-char (cdr cell)))
	 ((listp (cdr cell))
          (when-let* ((elt (assq 'dired-filename (cdr cell))))
            (dired-goto-file (expand-file-name (cdr elt)))))))
    ;; and make sure it will be saved again for later
    (setq save-place-mode t)))

(defun save-place-kill-emacs-hook ()
  ;; First update the alist.  This loads the old save-place-file if nec.
  (save-places-to-alist)
  ;; Now save the alist in the file, if we have ever loaded the file
  ;; (including just now).
  (if save-place-loaded
      (save-place-alist-to-file)))

(define-obsolete-function-alias 'load-save-place-alist-from-file
  #'save-place-load-alist-from-file "29.1")

(provide 'saveplace)
;;; saveplace.el ends here
