;;;; dired-lisp.el - emulate ls completely in Emacs Lisp. $Revision: 1.2 $
;;;; Copyright (C) 1991 Sebastian Kremer <sk@thp.uni-koeln.de>

;;;; READ THE WARNING BELOW BEFORE USING THIS PROGRAM!

;;;; Useful if you cannot afford to fork Emacs on a real memory UNIX,
;;;; under VMS, or if you don't have the ls program.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;;; WARNING:

;;;; Sometimes I get an internal Emacs error:

;;;;   Signalling: (wrong-type-argument natnump #<EMACS BUG: ILLEGAL
;;;;   DATATYPE (#o37777777727) Save your buffers immediately and please
;;;;   report this bug>)

;;;;  Sometimes emacs just crashes with a fatal error.

;;;  RESTRICTIONS:
;;;; Always sorts by name (ls switches are completely ignored for now)
;;;; Cannot display date of file, displays a fake date "Jan 00 00:00" instead
;;;; Only numeric uid/gid
;;;; Loading ange-ftp breaks it

;;;; It is surprisingly fast, though!

;;;; TODO:
;;;; Recognize at least some ls switches: l R g F i

(require 'dired)			; we will redefine this function:

(defun dired-ls (file &optional switches wildcard full-directory-p)
  "dired-lisp.el's version of dired-ls."
;  "Insert ls output of FILE, optionally formatted with SWITCHES.
;Optional third arg WILDCARD means treat FILE as shell wildcard.
;Optional fourth arg FULL-DIRECTORY-P means file is a directory and
;switches do not contain `d'.
;
;SWITCHES default to dired-listing-switches."
  (or switches (setq switches dired-listing-switches))
  (if wildcard
      (error "Cannot handle wildcards in lisp emulation of `ls'."))
  (if full-directory-p
      (let* ((dir (file-name-as-directory file))
	     (start (length dir))
	     (sum 0))
	(insert "total \007\n")		; fill in afterwards
	(insert
	 (mapconcat 
	  (function (lambda (short)
		      (let* ((fil (concat dir short))
			     (attr (file-attributes fil))
			     (size (nth 7 attr)))
			;;(debug)
			(setq sum (+ sum size))
			(dired-lisp-format
			 ;;(file-name-nondirectory fil)
			 ;;(dired-make-relative fil dir)
			 ;;(substring fil start)
			 short
			 attr
			 switches))))
	  (directory-files dir)
	  ""))
	(save-excursion
	  (search-backward "total \007")
	  (goto-char (match-end 0))
	  (delete-char -1)
	  (insert (format "%d" sum)))
	)
    ;; if not full-directory-p, FILE *must not* end in /, as
    ;; file-attributes will not recognize a symlink to a directory
    ;; must make it a relative filename as ls does:
    (setq file (file-name-nondirectory file))
    (insert (dired-lisp-format file (file-attributes file) switches)))
  )

(defun dired-lisp-format (file-name file-attr &optional switches)
  (let ((file-type (nth 0 file-attr)))
    (concat (nth 8 file-attr)		; permission bits
	    " "
	    (dired-lisp-pad (nth 1 file-attr) -3) ; no. of links
	    ;; numeric uid/gid are more confusing than helpful
	    ;; Emacs should be able to make strings of them
	    " " (dired-lisp-pad (nth 2 file-attr) -6)		; uid
	    " " (dired-lisp-pad (nth 3 file-attr) -6)		; gid
	    " "
	    (dired-lisp-pad (nth 7 file-attr) -8) ; size in bytes
	    ;; file-attributes's time is in a braindead format
	    ;; Emacs should have a ctime function
	    " " "Jan 00 00:00 "		; fake time
	    file-name
	    (if (stringp file-type)	; is a symbolic link
		(concat " -> " file-type)
	      "")
	    "\n"
	    )))

;; format should really do anything printf can!!
(defun dired-lisp-pad (arg width &optional pad-char)
  "Pad ARG to WIDTH, from left if WIDTH < 0.
Non-nil third arg optional PAD-CHAR defaults to a space."
  (or pad-char (setq pad-char ?\040))
  (if (integerp arg)
      (setq arg (int-to-string arg)))
  (let (l pad reverse)
    (if (< width 0)
	(setq reverse t
	      width (- width)))
    (setq l (length arg)
	  pad (- width l))
    (if (> pad 0)
	(if reverse
	    (concat (make-string pad pad-char) arg)
	  (concat arg (make-string pad pad-char)))
      arg)))
