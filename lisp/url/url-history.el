;;; url-history.el --- Global history tracking for URL package
;; Author: $Author: fx $
;; Created: $Date: 2001/05/05 16:49:52 $
;; Version: $Revision: 1.6 $
;; Keywords: comm, data, processes, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993 - 1996 by William M. Perry <wmperry@cs.indiana.edu>
;;; Copyright (c) 1996 - 1999 Free Software Foundation, Inc.
;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This can get a recursive require.
;;(require 'url)
(eval-when-compile (require 'cl))
(require 'url-parse)
(autoload 'url-do-setup "url")

(defgroup url-history nil
  "History variables in the URL package"
  :prefix "url-history"
  :group 'url)

(defcustom url-history-track nil
  "*Controls whether to keep a list of all the URLS being visited.
If non-nil, url will keep track of all the URLS visited.
If eq to `t', then the list is saved to disk at the end of each emacs
session."
  :type 'boolean
  :group 'url-history)

(defcustom url-history-file nil
  "*The global history file for the URL package.
This file contains a list of all the URLs you have visited.  This file
is parsed at startup and used to provide URL completion."
  :type '(choice (const :tag "Default" :value nil) file)
  :group 'url-history)

(defcustom url-history-save-interval 3600
  "*The number of seconds between automatic saves of the history list.
Default is 1 hour.  Note that if you change this variable outside of
the `customize' interface after `url-do-setup' has been run, you need
to run the `url-history-setup-save-timer' function manually."
  :set (function (lambda (var val)
		   (set-default var val)
		   (and (featurep 'url)
			(fboundp 'url-history-setup-save-timer)
                        (let ((def (symbol-function
                                    'url-history-setup-save-timer)))
                          (not (and (listp def) (eq 'autoload (car def)))))
			(url-history-setup-save-timer))))
  :type 'integer
  :group 'url-history)

(defvar url-history-timer nil)

(defvar url-history-list nil
  "List of urls visited this session.")

(defvar url-history-changed-since-last-save nil
  "Whether the history list has changed since the last save operation.")

(defvar url-history-hash-table nil
  "Hash table for global history completion.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun url-history-setup-save-timer ()
  "Reset the history list timer."
  (interactive)
  (cond
   ((featurep 'itimer)
    (ignore-errors (delete-itimer url-history-timer))
    (setq url-history-timer nil)
    (if url-history-save-interval
	(setq url-history-timer
	      (start-itimer "url-history-saver" 'url-history-save-history
			    url-history-save-interval
			    url-history-save-interval))))
   ((fboundp 'run-at-time)
    (ignore-errors (cancel-timer url-history-timer))
    (setq url-history-timer nil)
    (if url-history-save-interval
	(setq url-history-timer
	      (run-at-time url-history-save-interval
			   url-history-save-interval
			   'url-history-save-history))))
   (t nil)))

;;;###autoload
(defun url-history-parse-history (&optional fname)
  "Parse a history file stored in FNAME."
  ;; Parse out the mosaic global history file for completions, etc.
  (or fname (setq fname (expand-file-name url-history-file)))
  (cond
   ((not (file-exists-p fname))
    (message "%s does not exist." fname))
   ((not (file-readable-p fname))
    (message "%s is unreadable." fname))
   (t
    (condition-case nil
	(load fname nil t)
      (error (message "Could not load %s" fname)))))
  (if (not url-history-hash-table)
      (setq url-history-hash-table (make-hash-table :size 31 :test 'equal))))

(defun url-history-update-url (url time)
  (setq url-history-changed-since-last-save t)
  (puthash (if (vectorp url) (url-recreate-url url) url) time url-history-hash-table))

;;;###autoload
(defun url-history-save-history (&optional fname)
  "Write the global history file into `url-history-file'.
The type of data written is determined by what is in the file to begin
with.  If the type of storage cannot be determined, then prompt the
user for what type to save as."
  (interactive)
  (or fname (setq fname (expand-file-name url-history-file)))
  (cond
   ((not url-history-changed-since-last-save) nil)
   ((not (file-writable-p fname))
    (message "%s is unwritable." fname))
   (t
    (let ((make-backup-files nil)
	  (version-control nil)
	  (require-final-newline t))
      (save-excursion
	(set-buffer (get-buffer-create " *url-tmp*"))
	(erase-buffer)
	(let ((count 0))
	  (maphash (function
		       (lambda (key value)
			 (while (string-match "[\r\n]+" key)
			   (setq key (concat (substring key 0 (match-beginning 0))
					     (substring key (match-end 0) nil))))
			 (setq count (1+ count))
			 (insert "(puthash \"" key "\""
				 (if (not (stringp value)) " '" "")
				 (prin1-to-string value)
				 " url-history-hash-table)\n")))
		      url-history-hash-table)
	  (goto-char (point-min))
	  (insert (format
		   "(setq url-history-hash-table (make-hash-table :size %d :test 'equal))\n"
		   (/ count 4)))
	  (goto-char (point-max))
	  (insert "\n")
	  (write-file fname))
	(kill-buffer (current-buffer))))))
  (setq url-history-changed-since-last-save nil))

(defun url-have-visited-url (url)
  (url-do-setup)
  (gethash url url-history-hash-table nil))

(defun url-completion-function (string predicate function)
  (url-do-setup)
  (cond
   ((eq function nil)
    (let ((list nil))
      (maphash (function (lambda (key val)
			      (setq list (cons (cons key val)
					       list))))
		  url-history-hash-table)
      (try-completion string (nreverse list) predicate)))
   ((eq function t)
    (let ((stub (concat "^" (regexp-quote string)))
	  (retval nil))
      (maphash
       (function
	(lambda (url time)
	  (if (string-match stub url)
	      (setq retval (cons url retval)))))
       url-history-hash-table)
      retval))
   ((eq function 'lambda)
    (and url-history-hash-table
	 (gethash string url-history-hash-table)
	 t))
   (t
    (error "url-completion-function very confused."))))

(provide 'url-history)
