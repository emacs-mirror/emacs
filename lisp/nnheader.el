;;; nnheader.el --- header access macros for Gnus and its backends
;; Copyright (C) 1987,88,89,90,93,94,95 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@flab.flab.fujitsu.junet>
;; 	Lars Magne Ingebrigtsen <larsi@ifi.uio.no>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; These macros may look very much like the ones in GNUS 4.1. They
;; are, in a way, but you should note that the indices they use have
;; been changed from the internal GNUS format to the NOV format. Makes
;; it possible to read headers from XOVER much faster.
;;
;; The format of a header is now:
;; [number subject from date id references chars lines xref]
;;
;; (That last entry is defined as "misc" in the NOV format, but Gnus
;; uses it for xrefs.)

;;; Code:

(defalias 'nntp-header-number 'mail-header-number)
(defmacro mail-header-number (header)
  "Return article number in HEADER."
  (` (aref (, header) 0)))

(defalias 'nntp-set-header-number 'mail-header-set-number)
(defmacro mail-header-set-number (header number)
  "Set article number of HEADER to NUMBER."
  (` (aset (, header) 0 (, number))))

(defalias 'nntp-header-subject 'mail-header-subject)
(defmacro mail-header-subject (header)
  "Return subject string in HEADER."
  (` (aref (, header) 1)))

(defalias 'nntp-set-header-subject 'mail-header-set-subject)
(defmacro mail-header-set-subject (header subject)
  "Set article subject of HEADER to SUBJECT."
  (` (aset (, header) 1 (, subject))))

(defalias 'nntp-header-from 'mail-header-from)
(defmacro mail-header-from (header)
  "Return author string in HEADER."
  (` (aref (, header) 2)))

(defalias 'nntp-set-header-from 'mail-header-set-from)
(defmacro mail-header-set-from (header from)
  "Set article author of HEADER to FROM."
  (` (aset (, header) 2 (, from))))

(defalias 'nntp-header-date 'mail-header-date)
(defmacro mail-header-date (header)
  "Return date in HEADER."
  (` (aref (, header) 3)))

(defalias 'nntp-set-header-date 'mail-header-set-date)
(defmacro mail-header-set-date (header date)
  "Set article date of HEADER to DATE."
  (` (aset (, header) 3 (, date))))

(defalias 'nntp-header-id 'mail-header-id)
(defmacro mail-header-id (header)
  "Return Id in HEADER."
  (` (aref (, header) 4)))

(defalias 'nntp-set-header-id 'mail-header-set-id)
(defmacro mail-header-set-id (header id)
  "Set article Id of HEADER to ID."
  (` (aset (, header) 4 (, id))))

(defalias 'nntp-header-references 'mail-header-references)
(defmacro mail-header-references (header)
  "Return references in HEADER."
  (` (aref (, header) 5)))

(defalias 'nntp-set-header-references 'mail-header-set-references)
(defmacro mail-header-set-references (header ref)
  "Set article references of HEADER to REF."
  (` (aset (, header) 5 (, ref))))

(defalias 'nntp-header-chars 'mail-header-chars)
(defmacro mail-header-chars (header)
  "Return number of chars of article in HEADER."
  (` (aref (, header) 6)))

(defalias 'nntp-set-header-chars 'mail-header-set-chars)
(defmacro mail-header-set-chars (header chars)
  "Set number of chars in article of HEADER to CHARS."
  (` (aset (, header) 6 (, chars))))

(defalias 'nntp-header-lines 'mail-header-lines)
(defmacro mail-header-lines (header)
  "Return lines in HEADER."
  (` (aref (, header) 7)))

(defalias 'nntp-set-header-lines 'mail-header-set-lines)
(defmacro mail-header-set-lines (header lines)
  "Set article lines of HEADER to LINES."
  (` (aset (, header) 7 (, lines))))

(defalias 'nntp-header-xref 'mail-header-xref)
(defmacro mail-header-xref (header)
  "Return xref string in HEADER."
  (` (aref (, header) 8)))

(defalias 'nntp-set-header-xref 'mail-header-set-xref)
(defmacro mail-header-set-xref (header xref)
  "Set article xref of HEADER to xref."
  (` (aset (, header) 8 (, xref))))


;; Various cruft the backends and Gnus need to communicate.

(defvar nntp-server-buffer nil)
(defvar gnus-verbose-backends t
  "*If non-nil, Gnus backends will generate lots of comments.")
(defvar gnus-nov-is-evil nil
  "If non-nil, Gnus backends will never output headers in the NOV format.")
(defvar news-reply-yank-from nil)
(defvar news-reply-yank-message-id nil)

;; All backends use this function, so I moved it to this file.

(defun nnheader-init-server-buffer ()
  (save-excursion
    (setq nntp-server-buffer (get-buffer-create " *nntpd*"))
    (set-buffer nntp-server-buffer)
    (buffer-disable-undo (current-buffer))
    (erase-buffer)
    (kill-all-local-variables)
    (setq case-fold-search t)		;Should ignore case.
    t))

(defun nnheader-set-init-variables (server defs)
  (let ((s server)
	val)
    ;; First we set the server variables in the sequence required.  We
    ;; use the definitions from the `defs' list where that is
    ;; possible. 
    (while s
      (set (car (car s)) 
	   (if (setq val (assq (car (car s)) defs))
	       (nth 1 val)
	     (nth 1 (car s))))
      (setq s (cdr s)))
    ;; The we go through the defs list and set any variables that were
    ;; not set in the first sweep.
    (while defs
      (if (not (assq (car (car defs)) server))
	  (set (car (car defs)) 
	       (if (and (symbolp (nth 1 (car defs)))
			(not (boundp (nth 1 (car defs)))))
		   (nth 1 (car defs))
		 (eval (nth 1 (car defs))))))
      (setq defs (cdr defs)))))

(defun nnheader-save-variables (server)
  (let (out)
    (while server
      (setq out (cons (list (car (car server)) 
			    (symbol-value (car (car server))))
		      out))
      (setq server (cdr server)))
    (nreverse out)))

(defun nnheader-restore-variables (state)
  (while state
    (set (car (car state)) (nth 1 (car state)))
    (setq state (cdr state))))

;; Read the head of an article.
(defun nnheader-insert-head (file)
  (let ((beg 0)
	(chop 1024))
    (while (and (eq chop (nth 1 (nnheader-insert-file-contents-literally
				 file nil beg (setq beg (+ chop beg)))))
		(prog1 (not (search-backward "\n\n" nil t)) 
		  (goto-char (point-max)))))))

(defun nnheader-article-p ()
  (goto-char (point-min))
  (if (not (search-forward "\n\n" nil t))
      nil
    (narrow-to-region (point-min) (1- (point)))
    (goto-char (point-min))
    (while (looking-at "[A-Z][^ \t]+:.*\n\\([ \t].*\n\\)*\\|From .*\n")
      (goto-char (match-end 0)))
    (prog1
	(eobp)
      (widen))))    

;; Written by Erik Naggum <erik@naggum.no>.
(defun nnheader-insert-file-contents-literally (filename &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but only reads in the file.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
find-file-hooks, etc.
  This function ensures that none of these modifications will take place."
  (let (				; (file-name-handler-alist nil)
	(format-alist nil)
	(after-insert-file-functions nil)
	(find-buffer-file-type-function 
	 (if (fboundp 'find-buffer-file-type)
	     (symbol-function 'find-buffer-file-type)
	   nil)))
    (unwind-protect
	(progn
	  (fset 'find-buffer-file-type (lambda (filename) t))
	  (insert-file-contents filename visit beg end replace))
      (if find-buffer-file-type-function
	  (fset 'find-buffer-file-type find-buffer-file-type-function)
	(fmakunbound 'find-buffer-file-type)))))

(defun nnheader-find-file-noselect (filename &optional nowarn rawfile)
  "Read file FILENAME into a buffer and return the buffer.
If a buffer exists visiting FILENAME, return that one, but
verify that the file has not changed since visited or saved.
The buffer is not selected, just returned to the caller."
  (setq filename
	(abbreviate-file-name
	 (expand-file-name filename)))
  (if (file-directory-p filename)
      (if find-file-run-dired
	  (dired-noselect filename)
	(error "%s is a directory." filename))
    (let* ((buf (get-file-buffer filename))
	   (truename (abbreviate-file-name (file-truename filename)))
	   (number (nthcdr 10 (file-attributes truename)))
	   ;; Find any buffer for a file which has same truename.
	   (other (and (not buf) 
		       (if (fboundp 'find-buffer-visiting)
			   (find-buffer-visiting filename)
			 (get-file-buffer filename))))
	   error)
      ;; Let user know if there is a buffer with the same truename.
      (if other
	  (progn
	    (or nowarn
		(string-equal filename (buffer-file-name other))
		(message "%s and %s are the same file"
			 filename (buffer-file-name other)))
	    ;; Optionally also find that buffer.
	    (if (or (and (boundp 'find-file-existing-other-name)
			 find-file-existing-other-name)
		    find-file-visit-truename)
		(setq buf other))))
      (if buf
	  (or nowarn
	      (verify-visited-file-modtime buf)
	      (cond ((not (file-exists-p filename))
		     (error "File %s no longer exists!" filename))
		    ((yes-or-no-p
		      (if (string= (file-name-nondirectory filename)
				   (buffer-name buf))
			  (format
			   (if (buffer-modified-p buf)
			       "File %s changed on disk.  Discard your edits? "
			     "File %s changed on disk.  Reread from disk? ")
			   (file-name-nondirectory filename))
			(format
			 (if (buffer-modified-p buf)
			     "File %s changed on disk.  Discard your edits in %s? "
			   "File %s changed on disk.  Reread from disk into %s? ")
			 (file-name-nondirectory filename)
			 (buffer-name buf))))
		     (save-excursion
		       (set-buffer buf)
		       (revert-buffer t t)))))
	(save-excursion
;;; The truename stuff makes this obsolete.
;;;	  (let* ((link-name (car (file-attributes filename)))
;;;		 (linked-buf (and (stringp link-name)
;;;				  (get-file-buffer link-name))))
;;;	    (if (bufferp linked-buf)
;;;		(message "Symbolic link to file in buffer %s"
;;;			 (buffer-name linked-buf))))
	  (setq buf (create-file-buffer filename))
	  ;;	  (set-buffer-major-mode buf)
	  (set-buffer buf)
	  (erase-buffer)
	  (if rawfile
	      (condition-case ()
		  (nnheader-insert-file-contents-literally filename t)
		(file-error
		 ;; Unconditionally set error
		 (setq error t)))
	    (condition-case ()
		(insert-file-contents filename t)
	      (file-error
	       ;; Run find-file-not-found-hooks until one returns non-nil.
	       (or t			; (run-hook-with-args-until-success 'find-file-not-found-hooks)
		   ;; If they fail too, set error.
		   (setq error t)))))
	  ;; Find the file's truename, and maybe use that as visited name.
	  (setq buffer-file-truename truename)
	  (setq buffer-file-number number)
	  ;; On VMS, we may want to remember which directory in a search list
	  ;; the file was found in.
	  (and (eq system-type 'vax-vms)
	       (let (logical)
		 (if (string-match ":" (file-name-directory filename))
		     (setq logical (substring (file-name-directory filename)
					      0 (match-beginning 0))))
		 (not (member logical find-file-not-true-dirname-list)))
	       (setq buffer-file-name buffer-file-truename))
	  (if find-file-visit-truename
	      (setq buffer-file-name
		    (setq filename
			  (expand-file-name buffer-file-truename))))
	  ;; Set buffer's default directory to that of the file.
	  (setq default-directory (file-name-directory filename))
	  ;; Turn off backup files for certain file names.  Since
	  ;; this is a permanent local, the major mode won't eliminate it.
	  (and (not (funcall backup-enable-predicate buffer-file-name))
	       (progn
		 (make-local-variable 'backup-inhibited)
		 (setq backup-inhibited t)))
	  (if rawfile
	      nil
	    (after-find-file error (not nowarn)))))
      buf)))

(defun nnheader-insert-references (references message-id)
  (if (and (not references) (not message-id)) 
      () ; This is illegal, but not all articles have Message-IDs.
    (mail-position-on-field "References")
    ;; Fold long references line to follow RFC1036.
    (let ((begin (gnus-point-at-bol))
	  (fill-column 78)
	  (fill-prefix "\t"))
      (if references (insert references))
      (if (and references message-id) (insert " "))
      (if message-id (insert message-id))
      ;; The region must end with a newline to fill the region
      ;; without inserting extra newline.
      (fill-region-as-paragraph begin (1+ (point))))))

(provide 'nnheader)

;;; nnheader.el ends here
