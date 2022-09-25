;;; fileloop.el --- Operations on multiple files  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>

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

;; Support functions for operations like search or query&replace applied to
;; several files.  This code was largely inspired&extracted from an earlier
;; version of etags.el.

;; TODO:
;; - Maybe it would make sense to replace the fileloop--* vars with a single
;;   global var holding a struct, and then stash those structs into a history
;;   of past operations, so you can perform a fileloop-search while in the
;;   middle of a fileloop-replace and later go back to that
;;   fileloop-replace.
;; - Make multi-isearch work on top of this library (might require changes
;;   to this library, of course).

;;; Code:

(require 'generator)

(defgroup fileloop nil
  "Operations on multiple files."
  :group 'tools)

(defcustom fileloop-revert-buffers 'silent
  "Whether to revert files during fileloop operation.
This can be one of:
  `silent' means to only do it if `revert-without-query' is applicable;
  t        means to offer to do it for all applicable files;
  nil      means never to do it"
  :type '(choice (const silent) (const t) (const nil)))

;; FIXME: This already exists in GNU ELPA's iterator.el.  Maybe it should move
;; to generator.el?
(iter-defun fileloop--list-to-iterator (list)
  (while list (iter-yield (pop list))))

(defvar fileloop--iterator iter-empty)
(defvar fileloop--scan-function
  (lambda () (user-error "No operation in progress")))
;; If the default value below is changed, the :enable form of
;; tags-continue and tags-repl-continue in menu-bar.el will have to be
;; updated accordingly.
(defvar fileloop--operate-function #'ignore)
(defvar fileloop--freshly-initialized nil)

;;;###autoload
(defun fileloop-initialize (files scan-function operate-function)
  "Initialize a new round of operation on several files.
FILES can be either a list of file names, or an iterator (used with `iter-next')
which returns a file name at each step.
SCAN-FUNCTION is a function called with no argument inside a buffer
and it should return non-nil if that buffer has something on which to operate.
OPERATE-FUNCTION is a function called with no argument; it is expected
to perform the operation on the current file buffer and when done
should return non-nil to mean that we should immediately continue
operating on the next file and nil otherwise."
  (setq fileloop--iterator
        (if (and (listp files) (not (functionp files)))
            (fileloop--list-to-iterator files)
          files))
  (setq fileloop--scan-function scan-function)
  (setq fileloop--operate-function operate-function)
  (setq fileloop--freshly-initialized t))

(defun fileloop-next-file (&optional novisit)
  ;; FIXME: Should we provide an interactive command, like tags-next-file?
  (let ((next (condition-case nil
                  (iter-next fileloop--iterator)
                (iter-end-of-sequence nil))))
    (unless next
      (and novisit
	   (get-buffer " *next-file*")
	   (kill-buffer " *next-file*"))
      (user-error "All files processed"))
    (let* ((buffer (get-file-buffer next))
	   (new (not buffer)))
      ;; Optionally offer to revert buffers
      ;; if the files have changed on disk.
      (and buffer fileloop-revert-buffers
	   (not (verify-visited-file-modtime buffer))
           (if (eq fileloop-revert-buffers 'silent)
               (and (not (buffer-modified-p buffer))
                    (let ((revertible nil))
                      (dolist (re revert-without-query)
                        (when (string-match-p re next)
                          (setq revertible t)))
                      revertible))
	     (y-or-n-p
	      (format
	       (if (buffer-modified-p buffer)
	           "File %s changed on disk.  Discard your edits? "
	         "File %s changed on disk.  Reread from disk? ")
	       next)))
	   (with-current-buffer buffer
	     (revert-buffer t t)))
      (if (not (and new novisit))
	  (set-buffer (find-file-noselect next))
        ;; Like find-file, but avoids random warning messages.
        (set-buffer (get-buffer-create " *next-file*"))
        (kill-all-local-variables)
        (erase-buffer)
        (setq new next)
        (condition-case nil
            (insert-file-contents new nil)
          (file-missing
           (fileloop-next-file novisit))))
      new)))

(defun fileloop-continue ()
  "Continue last multi-file operation."
  (interactive)
  (let (new
	;; Non-nil means we have finished one file
	;; and should not scan it again.
	file-finished
	original-point
	(messaged nil))
    (while
	(progn
	  ;; Scan files quickly for the first or next interesting one.
	  ;; This starts at point in the current buffer.
	  (while (or fileloop--freshly-initialized file-finished
		     (save-restriction
		       (widen)
		       (not (funcall fileloop--scan-function))))
	    ;; If nothing was found in the previous file, and
	    ;; that file isn't in a temp buffer, restore point to
	    ;; where it was.
	    (when original-point
	      (goto-char original-point))

	    (setq file-finished nil)
	    (setq new (fileloop-next-file t))

	    ;; If NEW is non-nil, we got a temp buffer,
	    ;; and NEW is the file name.
	    (when (or messaged
		      (and (not fileloop--freshly-initialized)
			   (> baud-rate search-slow-speed)
			   (setq messaged t)))
	      (message "Scanning file %s..." (or new buffer-file-name)))

	    (setq fileloop--freshly-initialized nil)
	    (setq original-point (if new nil (point)))
	    (goto-char (point-min)))

	  ;; If we visited it in a temp buffer, visit it now for real.
	  (if new
	      (let ((pos (point)))
		(erase-buffer)
		(set-buffer (find-file-noselect new))
		(setq new nil)		;No longer in a temp buffer.
		(widen)
		(goto-char pos))
	    (push-mark original-point t))

          (let (switch-to-buffer-preserve-window-point)
	    (switch-to-buffer (current-buffer)))

	  ;; Now operate on the file.
	  ;; If value is non-nil, continue to scan the next file.
          (save-restriction
            (widen)
            (funcall fileloop--operate-function)))
      (setq file-finished t))))

;;;###autoload
(defun fileloop-initialize-search (regexp files case-fold)
  (let ((last-buffer (current-buffer)))
    (fileloop-initialize
     files
     (lambda ()
       (let ((case-fold-search (fileloop--case-fold regexp case-fold)))
         (re-search-forward regexp nil t)))
     (lambda ()
       (unless (eq last-buffer (current-buffer))
         (setq last-buffer (current-buffer))
         (message "Scanning file %s...found" buffer-file-name))
       nil))))

(defun fileloop--case-fold (regexp case-fold)
  (let ((value
         (if (memql case-fold '(nil t))
             case-fold
           case-fold-search)))
    (if (and value search-upper-case)
        (isearch-no-upper-case-p regexp t)
      value)))

;;;###autoload
(defun fileloop-initialize-replace (from to files case-fold &optional delimited)
  "Initialize a new round of query&replace on several files.
FROM is a regexp and TO is the replacement to use.
FILES describes the files, as in `fileloop-initialize'.
CASE-FOLD can be t, nil, or `default':
  if it is nil, matching of FROM is case-sensitive.
  if it is t, matching of FROM is case-insensitive, except
     when `search-upper-case' is non-nil and FROM includes
     upper-case letters.
  if it is `default', the function uses the value of
     `case-fold-search' instead.
DELIMITED if non-nil means replace only word-delimited matches."
  ;; FIXME: Not sure how the delimited-flag interacts with the regexp-flag in
  ;; `perform-replace', so I just try to mimic the old code.
  (let ((mstart (make-hash-table :test 'eq)))
    (fileloop-initialize
     files
     (lambda ()
       (let ((case-fold-search (fileloop--case-fold from case-fold)))
         (when (re-search-forward from nil t)
           ;; When we find a match, save its beginning for
           ;; `perform-replace' (we used to just set point, but this
           ;; is unreliable in the face of
           ;; `switch-to-buffer-preserve-window-point').
           (puthash (current-buffer) (match-beginning 0) mstart))))
     (lambda ()
       (let ((case-fold-search (fileloop--case-fold from case-fold)))
         (perform-replace from to t t delimited nil multi-query-replace-map
                          (gethash (current-buffer) mstart (point-min))
                          (point-max)))))))

(provide 'fileloop)
;;; fileloop.el ends here
