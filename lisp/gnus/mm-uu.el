;;; mm-uu.el -- Return uu stuff as mm handles
;; Copyright (c) 1998, 1999, 2000 Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: postscript uudecode binhex shar forward news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:


;;; Code:

(eval-when-compile (require 'cl))
(require 'mail-parse)
(require 'nnheader)
(require 'mm-decode)
(require 'mailcap)
(require 'uudecode)

(eval-and-compile
  (autoload 'binhex-decode-region "binhex")
  (autoload 'binhex-decode-region-external "binhex"))

(defun mm-uu-copy-to-buffer (from to)
  "Copy the contents of the current buffer to a fresh buffer.
Return that buffer."
  (save-excursion
    (let ((obuf (current-buffer)))
      (set-buffer (generate-new-buffer " *mm-uu*"))
      (insert-buffer-substring obuf from to)
      (current-buffer))))

;;; postscript

(defconst mm-uu-postscript-begin-line "^%!PS-")
(defconst mm-uu-postscript-end-line "^%%EOF$")

(defconst mm-uu-uu-begin-line "^begin[ \t]+[0-7][0-7][0-7][ \t]+")
(defconst mm-uu-uu-end-line "^end[ \t]*$")

;; This is not the right place for this.  uudecode.el should decide
;; whether or not to use a program with a single interface, but I
;; guess it's too late now.  Also the default should depend on a test
;; for the program.  -- fx
(defcustom mm-uu-decode-function 'uudecode-decode-region
  "*Function to uudecode.
Internal function is done in Lisp by default, therefore decoding may
appear to be horribly slow.  You can make Gnus use an external
decoder, such as uudecode."
  :type '(choice
	  (function-item :tag "Internal" uudecode-decode-region)
	  (function-item :tag "External" uudecode-decode-region-external))
  :group 'gnus-article-mime)

(defconst mm-uu-binhex-begin-line
  "^:...............................................................$")
(defconst mm-uu-binhex-end-line ":$")

(defcustom mm-uu-binhex-decode-function 'binhex-decode-region
  "*Function to binhex decode.
Internal function is done in Lisp by default, therefore decoding may
appear to be horribly slow.  You can make Gnus use an external
decoder, such as hexbin."
  :type '(choice
	  (function-item :tag "Internal" binhex-decode-region)
	  (function-item :tag "External" binhex-decode-region-external))
  :group 'gnus-article-mime)

(defconst mm-uu-shar-begin-line "^#! */bin/sh")
(defconst mm-uu-shar-end-line "^exit 0\\|^$")

;;; Thanks to Edward J. Sabol <sabol@alderaan.gsfc.nasa.gov> and
;;; Peter von der Ah\'e <pahe@daimi.au.dk>
(defconst mm-uu-forward-begin-line "^-+ \\(Start of \\)?Forwarded message")
(defconst mm-uu-forward-end-line "^-+ End \\(of \\)?forwarded message")

(defvar mm-uu-begin-line nil)

(defconst mm-uu-identifier-alist
  '((?% . postscript) (?b . uu) (?: . binhex) (?# . shar)
    (?- . forward)))

(defvar mm-dissect-disposition "inline"
  "The default disposition of uu parts.
This can be either \"inline\" or \"attachment\".")

(defun mm-uu-configure-p  (key val)
  (member (cons key val) mm-uu-configure-list))

(defun mm-uu-configure (&optional symbol value)
  (if symbol (set-default symbol value))
  (setq mm-uu-begin-line nil)
  (mapcar (lambda (type)
	    (if (mm-uu-configure-p type 'disabled)
		nil
	      (setq mm-uu-begin-line
		    (concat mm-uu-begin-line
			    (if mm-uu-begin-line "\\|")
			    (symbol-value
			     (intern (concat "mm-uu-" (symbol-name type)
					     "-begin-line")))))))
	  '(uu postscript binhex shar forward)))

;; Needs to come after mm-uu-configure.
(defcustom mm-uu-configure-list nil
  "Alist of mm-uu configurations to disable.
To disable dissecting shar codes, for instance, add
`(shar . disabled)' to this list."
  :type '(repeat (choice (const :tag "postscript" (postscript . disabled))
			 (const :tag "uu" (uu . disabled))
			 (const :tag "binhex" (binhex . disabled))
			 (const :tag "shar" (shar . disabled))
			 (const :tag "forward" (forward . disabled))))
  :group 'gnus-article-mime
  :set 'mm-uu-configure)

(mm-uu-configure)

;;;### autoload

(defun mm-uu-dissect ()
  "Dissect the current buffer and return a list of uu handles."
  (let (text-start start-char end-char
		   type file-name end-line result text-plain-type
		   start-char-1 end-char-1
		   (case-fold-search t))
    (save-excursion
      (save-restriction
	(mail-narrow-to-head)
	(goto-char (point-max)))
      (forward-line)
      ;;; gnus-decoded is a fake charset, which means no further
      ;;; decoding.
      (setq text-start (point)
	    text-plain-type '("text/plain"  (charset . gnus-decoded)))
      (while (re-search-forward mm-uu-begin-line nil t)
	(setq start-char (match-beginning 0))
	(setq type (cdr (assq (aref (match-string 0) 0)
			      mm-uu-identifier-alist)))
	(setq file-name
	      (if (and (eq type 'uu)
		       (looking-at "\\(.+\\)$"))
		  (and (match-string 1)
		       (let ((nnheader-file-name-translation-alist
			      '((?/ . ?,) (? . ?_) (?* . ?_) (?$ . ?_))))
			 (nnheader-translate-file-chars (match-string 1))))))
	(forward-line);; in case of failure
	(setq start-char-1 (point))
	(setq end-line (symbol-value
			(intern (concat "mm-uu-" (symbol-name type)
					"-end-line"))))
	(when (and (re-search-forward end-line nil t)
		   (not (eq (match-beginning 0) (match-end 0))))
	  (setq end-char-1 (match-beginning 0))
	  (forward-line)
	  (setq end-char (point))
	  (when (cond
		 ((eq type 'binhex)
		  (setq file-name
			(ignore-errors
			  (binhex-decode-region start-char end-char t))))
		 ((eq type 'forward)
		  (save-excursion
		    (goto-char start-char-1)
		    (looking-at "[\r\n]*[a-zA-Z][a-zA-Z0-9-]*:")))
		 (t t))
	    (if (> start-char text-start)
		(push
		 (mm-make-handle (mm-uu-copy-to-buffer text-start start-char)
				 text-plain-type)
		 result))
	    (push
	     (cond
	      ((eq type 'postscript)
	       (mm-make-handle (mm-uu-copy-to-buffer start-char end-char)
			       '("application/postscript")))
	      ((eq type 'forward)
	       (mm-make-handle (mm-uu-copy-to-buffer start-char-1 end-char-1)
			       '("message/rfc822" (charset . gnus-decoded))))
	      ((eq type 'uu)
	       (mm-make-handle (mm-uu-copy-to-buffer start-char end-char)
			       (list (or (and file-name
					      (string-match "\\.[^\\.]+$"
							    file-name)
					      (mailcap-extension-to-mime
					       (match-string 0 file-name)))
					 "application/octet-stream"))
			       'x-uuencode nil
			       (if (and file-name (not (equal file-name "")))
				   (list mm-dissect-disposition
					 (cons 'filename file-name)))))
	      ((eq type 'binhex)
	       (mm-make-handle (mm-uu-copy-to-buffer start-char end-char)
			       (list (or (and file-name
					      (string-match "\\.[^\\.]+$" file-name)
					      (mailcap-extension-to-mime
					       (match-string 0 file-name)))
					 "application/octet-stream"))
			       'x-binhex nil
			       (if (and file-name (not (equal file-name "")))
				   (list mm-dissect-disposition
					 (cons 'filename file-name)))))
	      ((eq type 'shar)
	       (mm-make-handle (mm-uu-copy-to-buffer start-char end-char)
			       '("application/x-shar"))))
	     result)
	    (setq text-start end-char))))
      (when result
	(if (> (point-max) (1+ text-start))
	    (push
	     (mm-make-handle (mm-uu-copy-to-buffer text-start (point-max))
			     text-plain-type)
	     result))
	(setq result (cons "multipart/mixed" (nreverse result))))
      result)))

;;;### autoload
(defun mm-uu-test ()
  "Check whether the current buffer contains uu stuff."
  (save-excursion
    (goto-char (point-min))
    (let (type end-line result
	       (case-fold-search t))
      (while (and mm-uu-begin-line
		  (not result) (re-search-forward mm-uu-begin-line nil t))
	(forward-line)
	(setq type (cdr (assq (aref (match-string 0) 0)
			      mm-uu-identifier-alist)))
	(setq end-line (symbol-value
			(intern (concat "mm-uu-" (symbol-name type)
					"-end-line"))))
	(if (and (re-search-forward end-line nil t)
		 (not (eq (match-beginning 0) (match-end 0))))
	    (setq result t)))
      result)))

(provide 'mm-uu)

;;; mm-uu.el ends here
