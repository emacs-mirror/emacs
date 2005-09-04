;;; mm-uu.el --- Return uu stuff as mm handles

;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: postscript uudecode binhex shar forward gnatsweb pgp

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))
(require 'mail-parse)
(require 'nnheader)
(require 'mm-decode)
(require 'mailcap)
(require 'mml2015)

(autoload 'uudecode-decode-region "uudecode")
(autoload 'uudecode-decode-region-external "uudecode")
(autoload 'uudecode-decode-region-internal "uudecode")

(autoload 'binhex-decode-region "binhex")
(autoload 'binhex-decode-region-external "binhex")
(autoload 'binhex-decode-region-internal "binhex")

(autoload 'yenc-decode-region "yenc")
(autoload 'yenc-extract-filename "yenc")

(defcustom mm-uu-decode-function 'uudecode-decode-region
  "*Function to uudecode.
Internal function is done in Lisp by default, therefore decoding may
appear to be horribly slow.  You can make Gnus use an external
decoder, such as uudecode."
  :type '(choice
	  (function-item :tag "Auto detect" uudecode-decode-region)
	  (function-item :tag "Internal" uudecode-decode-region-internal)
	  (function-item :tag "External" uudecode-decode-region-external))
  :group 'gnus-article-mime)

(defcustom mm-uu-binhex-decode-function 'binhex-decode-region
  "*Function to binhex decode.
Internal function is done in elisp by default, therefore decoding may
appear to be horribly slow . You can make Gnus use the external Unix
decoder, such as hexbin."
  :type '(choice (function-item :tag "Auto detect" binhex-decode-region)
		 (function-item :tag "Internal" binhex-decode-region-internal)
		 (function-item :tag "External" binhex-decode-region-external))
  :group 'gnus-article-mime)

(defvar mm-uu-yenc-decode-function 'yenc-decode-region)

(defvar mm-uu-pgp-beginning-signature
     "^-----BEGIN PGP SIGNATURE-----")

(defvar mm-uu-beginning-regexp nil)

(defvar mm-dissect-disposition "inline"
  "The default disposition of uu parts.
This can be either \"inline\" or \"attachment\".")

(defvar mm-uu-emacs-sources-regexp "gnu\\.emacs\\.sources"
  "The regexp of Emacs sources groups.")

(defcustom mm-uu-diff-groups-regexp "gnus\\.commits"
  "*Regexp matching diff groups."
  :version "22.1"
  :type 'regexp
  :group 'gnus-article-mime)

(defvar mm-uu-type-alist
  '((postscript
     "^%!PS-"
     "^%%EOF$"
     mm-uu-postscript-extract
     nil)
    (uu
     "^begin[ \t]+0?[0-7][0-7][0-7][ \t]+"
     "^end[ \t]*$"
     mm-uu-uu-extract
     mm-uu-uu-filename)
    (binhex
     "^:...............................................................$"
     ":$"
     mm-uu-binhex-extract
     nil
     mm-uu-binhex-filename)
    (yenc
     "^=ybegin.*size=[0-9]+.*name=.*$"
     "^=yend.*size=[0-9]+"
     mm-uu-yenc-extract
     mm-uu-yenc-filename)
    (shar
     "^#! */bin/sh"
     "^exit 0$"
     mm-uu-shar-extract)
    (forward
     ;; Thanks to Edward J. Sabol <sabol@alderaan.gsfc.nasa.gov> and
     ;; Peter von der Ah\'e <pahe@daimi.au.dk>
     "^-+ \\(Start of \\)?Forwarded message"
     "^-+ End \\(of \\)?forwarded message"
     mm-uu-forward-extract
     nil
     mm-uu-forward-test)
    (gnatsweb
     "^----gnatsweb-attachment----"
     nil
     mm-uu-gnatsweb-extract)
    (pgp-signed
     "^-----BEGIN PGP SIGNED MESSAGE-----"
     "^-----END PGP SIGNATURE-----"
     mm-uu-pgp-signed-extract
     nil
     nil)
    (pgp-encrypted
     "^-----BEGIN PGP MESSAGE-----"
     "^-----END PGP MESSAGE-----"
     mm-uu-pgp-encrypted-extract
     nil
     nil)
    (pgp-key
     "^-----BEGIN PGP PUBLIC KEY BLOCK-----"
     "^-----END PGP PUBLIC KEY BLOCK-----"
     mm-uu-pgp-key-extract
     mm-uu-gpg-key-skip-to-last
     nil)
    (emacs-sources
     "^;;;?[ \t]*[^ \t]+\\.el[ \t]*--"
     "^;;;?[ \t]*\\([^ \t]+\\.el\\)[ \t]+ends here"
     mm-uu-emacs-sources-extract
     nil
     mm-uu-emacs-sources-test)
    (diff
     "^Index: "
     nil
     mm-uu-diff-extract
     nil
     mm-uu-diff-test)))

(defcustom mm-uu-configure-list '((shar . disabled))
  "A list of mm-uu configuration.
To disable dissecting shar codes, for instance, add
`(shar . disabled)' to this list."
  :type 'alist
  :options (mapcar (lambda (entry)
		     (list (car entry) '(const disabled)))
		   mm-uu-type-alist)
  :group 'gnus-article-mime)

;; functions

(defsubst mm-uu-type (entry)
  (car entry))

(defsubst mm-uu-beginning-regexp (entry)
  (nth 1 entry))

(defsubst mm-uu-end-regexp (entry)
  (nth 2 entry))

(defsubst mm-uu-function-extract (entry)
  (nth 3 entry))

(defsubst mm-uu-function-1 (entry)
  (nth 4 entry))

(defsubst mm-uu-function-2 (entry)
  (nth 5 entry))

(defun mm-uu-copy-to-buffer (&optional from to)
  "Copy the contents of the current buffer to a fresh buffer.
Return that buffer."
  (let ((obuf (current-buffer))
        (coding-system
         ;; Might not exist in non-MULE XEmacs
         (when (boundp 'buffer-file-coding-system)
           buffer-file-coding-system)))
    (with-current-buffer (generate-new-buffer " *mm-uu*")
      (setq buffer-file-coding-system coding-system)
      (insert-buffer-substring obuf from to)
      (current-buffer))))

(defun mm-uu-configure-p  (key val)
  (member (cons key val) mm-uu-configure-list))

(defun mm-uu-configure (&optional symbol value)
  (if symbol (set-default symbol value))
  (setq mm-uu-beginning-regexp nil)
  (mapcar (lambda (entry)
	     (if (mm-uu-configure-p (mm-uu-type entry) 'disabled)
		 nil
	       (setq mm-uu-beginning-regexp
		     (concat mm-uu-beginning-regexp
			     (if mm-uu-beginning-regexp "\\|")
			     (mm-uu-beginning-regexp entry)))))
	  mm-uu-type-alist))

(mm-uu-configure)

(eval-when-compile
  (defvar file-name)
  (defvar start-point)
  (defvar end-point)
  (defvar entry))

(defun mm-uu-uu-filename ()
  (if (looking-at ".+")
      (setq file-name
	    (let ((nnheader-file-name-translation-alist
		   '((?/ . ?,) (?\  . ?_) (?* . ?_) (?$ . ?_))))
	      (nnheader-translate-file-chars (match-string 0))))))

(defun mm-uu-binhex-filename ()
  (setq file-name
	(ignore-errors
	  (binhex-decode-region start-point end-point t))))

(defun mm-uu-yenc-filename ()
  (goto-char start-point)
  (setq file-name
	(ignore-errors
	  (yenc-extract-filename))))

(defun mm-uu-forward-test ()
  (save-excursion
    (goto-char start-point)
    (forward-line)
    (looking-at "[\r\n]*[a-zA-Z][a-zA-Z0-9-]*:")))

(defun mm-uu-postscript-extract ()
  (mm-make-handle (mm-uu-copy-to-buffer start-point end-point)
		  '("application/postscript")))

(defun mm-uu-emacs-sources-extract ()
  (mm-make-handle (mm-uu-copy-to-buffer start-point end-point)
		  '("application/emacs-lisp")
		  nil nil
		  (list mm-dissect-disposition
			(cons 'filename file-name))))

(eval-when-compile
  (defvar gnus-newsgroup-name))

(defun mm-uu-emacs-sources-test ()
  (setq file-name (match-string 1))
  (and gnus-newsgroup-name
       mm-uu-emacs-sources-regexp
       (string-match mm-uu-emacs-sources-regexp gnus-newsgroup-name)))

(defun mm-uu-diff-extract ()
  (mm-make-handle (mm-uu-copy-to-buffer start-point end-point)
		  '("text/x-patch")))

(defun mm-uu-diff-test ()
  (and gnus-newsgroup-name
       mm-uu-diff-groups-regexp
       (string-match mm-uu-diff-groups-regexp gnus-newsgroup-name)))

(defun mm-uu-forward-extract ()
  (mm-make-handle (mm-uu-copy-to-buffer
		   (progn (goto-char start-point) (forward-line) (point))
		   (progn (goto-char end-point) (forward-line -1) (point)))
		  '("message/rfc822" (charset . gnus-decoded))))

(defun mm-uu-uu-extract ()
  (mm-make-handle (mm-uu-copy-to-buffer start-point end-point)
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

(defun mm-uu-binhex-extract ()
  (mm-make-handle (mm-uu-copy-to-buffer start-point end-point)
		  (list (or (and file-name
				 (string-match "\\.[^\\.]+$" file-name)
				 (mailcap-extension-to-mime
				  (match-string 0 file-name)))
			    "application/octet-stream"))
		  'x-binhex nil
		  (if (and file-name (not (equal file-name "")))
		      (list mm-dissect-disposition
			    (cons 'filename file-name)))))

(defun mm-uu-yenc-extract ()
  (mm-make-handle (mm-uu-copy-to-buffer start-point end-point)
		  (list (or (and file-name
				 (string-match "\\.[^\\.]+$" file-name)
				 (mailcap-extension-to-mime
				  (match-string 0 file-name)))
			    "application/octet-stream"))
		  'x-yenc nil
		  (if (and file-name (not (equal file-name "")))
		      (list mm-dissect-disposition
			    (cons 'filename file-name)))))


(defun mm-uu-shar-extract ()
  (mm-make-handle (mm-uu-copy-to-buffer start-point end-point)
		  '("application/x-shar")))

(defun mm-uu-gnatsweb-extract ()
  (save-restriction
    (goto-char start-point)
    (forward-line)
    (narrow-to-region (point) end-point)
    (mm-dissect-buffer t)))

(defun mm-uu-pgp-signed-test (&rest rest)
  (and
   mml2015-use
   (mml2015-clear-verify-function)
   (cond
    ((eq mm-verify-option 'never) nil)
    ((eq mm-verify-option 'always) t)
    ((eq mm-verify-option 'known) t)
    (t (y-or-n-p "Verify pgp signed part? ")))))

(eval-when-compile
  (defvar gnus-newsgroup-charset))

(defun mm-uu-pgp-signed-extract-1 (handles ctl)
  (let ((buf (mm-uu-copy-to-buffer (point-min) (point-max))))
    (with-current-buffer buf
      (if (mm-uu-pgp-signed-test)
	  (progn
	    (mml2015-clean-buffer)
	    (let ((coding-system-for-write (or gnus-newsgroup-charset
					       'iso-8859-1)))
	      (funcall (mml2015-clear-verify-function))))
	(when (and mml2015-use (null (mml2015-clear-verify-function)))
	  (mm-set-handle-multipart-parameter
	   mm-security-handle 'gnus-details
	   (format "Clear verification not supported by `%s'.\n" mml2015-use))))
      (goto-char (point-min))
      (if (search-forward "\n\n" nil t)
	  (delete-region (point-min) (point)))
      (if (re-search-forward mm-uu-pgp-beginning-signature nil t)
	  (delete-region (match-beginning 0) (point-max)))
      (goto-char (point-min))
      (while (re-search-forward "^- " nil t)
	(replace-match "" t t)
	(forward-line 1)))
    (list (mm-make-handle buf '("text/plain" (charset . gnus-decoded))))))

(defun mm-uu-pgp-signed-extract ()
  (let ((mm-security-handle (list (format "multipart/signed"))))
    (mm-set-handle-multipart-parameter
     mm-security-handle 'protocol "application/x-gnus-pgp-signature")
    (save-restriction
      (narrow-to-region start-point end-point)
      (add-text-properties 0 (length (car mm-security-handle))
			   (list 'buffer (mm-uu-copy-to-buffer))
			   (car mm-security-handle))
      (setcdr mm-security-handle
	      (mm-uu-pgp-signed-extract-1 nil
					  mm-security-handle)))
    mm-security-handle))

(defun mm-uu-pgp-encrypted-test (&rest rest)
  (and
   mml2015-use
   (mml2015-clear-decrypt-function)
   (cond
    ((eq mm-decrypt-option 'never) nil)
    ((eq mm-decrypt-option 'always) t)
    ((eq mm-decrypt-option 'known) t)
    (t (y-or-n-p "Decrypt pgp encrypted part? ")))))

(defun mm-uu-pgp-encrypted-extract-1 (handles ctl)
  (let ((buf (mm-uu-copy-to-buffer (point-min) (point-max))))
    (if (mm-uu-pgp-encrypted-test)
	(with-current-buffer buf
	  (mml2015-clean-buffer)
	  (funcall (mml2015-clear-decrypt-function))))
    (list
     (mm-make-handle buf
		     '("text/plain"  (charset . gnus-decoded))))))

(defun mm-uu-pgp-encrypted-extract ()
  (let ((mm-security-handle (list (format "multipart/encrypted"))))
    (mm-set-handle-multipart-parameter
     mm-security-handle 'protocol "application/x-gnus-pgp-encrypted")
    (save-restriction
      (narrow-to-region start-point end-point)
      (add-text-properties 0 (length (car mm-security-handle))
			   (list 'buffer (mm-uu-copy-to-buffer))
			   (car mm-security-handle))
      (setcdr mm-security-handle
	      (mm-uu-pgp-encrypted-extract-1 nil
					     mm-security-handle)))
    mm-security-handle))

(defun mm-uu-gpg-key-skip-to-last ()
  (let ((point (point))
	(end-regexp (mm-uu-end-regexp entry))
	(beginning-regexp (mm-uu-beginning-regexp entry)))
    (when (and end-regexp
	       (not (mm-uu-configure-p (mm-uu-type entry) 'disabled)))
      (while (re-search-forward end-regexp nil t)
	(skip-chars-forward " \t\n\r")
	(if (looking-at beginning-regexp)
	    (setq point (match-end 0)))))
    (goto-char point)))

(defun mm-uu-pgp-key-extract ()
  (let ((buf (mm-uu-copy-to-buffer start-point end-point)))
    (mm-make-handle buf
		    '("application/pgp-keys"))))

;;;###autoload
(defun mm-uu-dissect ()
  "Dissect the current buffer and return a list of uu handles."
  (let ((case-fold-search t)
	text-start start-point end-point file-name result
	text-plain-type entry func)
    (save-excursion
      (goto-char (point-min))
      (cond
       ((looking-at "\n")
	(forward-line))
       ((search-forward "\n\n" nil t)
	t)
       (t (goto-char (point-max))))
      ;;; gnus-decoded is a fake charset, which means no further
      ;;; decoding.
      (setq text-start (point)
	    text-plain-type '("text/plain"  (charset . gnus-decoded)))
      (while (re-search-forward mm-uu-beginning-regexp nil t)
	(setq start-point (match-beginning 0))
	(let ((alist mm-uu-type-alist)
	      (beginning-regexp (match-string 0)))
	  (while (not entry)
	    (if (string-match (mm-uu-beginning-regexp (car alist))
			      beginning-regexp)
		(setq entry (car alist))
	      (pop alist))))
	(if (setq func (mm-uu-function-1 entry))
	    (funcall func))
	(forward-line);; in case of failure
	(when (and (not (mm-uu-configure-p (mm-uu-type entry) 'disabled))
		   (let ((end-regexp (mm-uu-end-regexp entry)))
		     (if (not end-regexp)
			 (or (setq end-point (point-max)) t)
		       (prog1
			   (re-search-forward end-regexp nil t)
			 (forward-line)
			 (setq end-point (point)))))
		   (or (not (setq func (mm-uu-function-2 entry)))
		       (funcall func)))
	  (if (and (> start-point text-start)
		   (progn
		     (goto-char text-start)
		     (re-search-forward "." start-point t)))
	      (push
	       (mm-make-handle (mm-uu-copy-to-buffer text-start start-point)
			       text-plain-type)
	       result))
	  (push
	   (funcall (mm-uu-function-extract entry))
	   result)
	  (goto-char (setq text-start end-point))))
      (when result
	(if (and (> (point-max) (1+ text-start))
		 (save-excursion
		   (goto-char text-start)
		   (re-search-forward "." nil t)))
	    (push
	     (mm-make-handle (mm-uu-copy-to-buffer text-start (point-max))
			     text-plain-type)
	     result))
	(setq result (cons "multipart/mixed" (nreverse result))))
      result)))

(provide 'mm-uu)

;; arch-tag: 7db076bf-53db-4320-aa19-ca76a1d2ab2c
;;; mm-uu.el ends here
