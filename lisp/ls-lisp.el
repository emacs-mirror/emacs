;;;; dired-lisp.el - emulate ls completely in Emacs Lisp. $Revision: 1.4 $
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

;;;; With earlier version of this program I sometimes got an internal
;;;; Emacs error:

;;;;   Signalling: (wrong-type-argument natnump #<EMACS BUG: ILLEGAL
;;;;   DATATYPE (#o37777777727) Save your buffers immediately and please
;;;;   report this bug>)

;;;; The datatype differs (I also got #o67 once).

;;;; Sometimes emacs just crashed with a fatal error.

;;;; After I've avoided using directory-files and file-attributes
;;;; together inside a mapcar, the bug didn't surface any longer.

;;;  RESTRICTIONS:
;;;; ls switches are mostly ignored
;;;; Cannot display date of file, displays a fake date "Jan 00 00:00" instead
;;;; Only numeric uid/gid
;;;; Loading ange-ftp breaks it

;;;; It is surprisingly fast, though!

;;;; TODO:
;;;; Recognize at some more ls switches: R F

(require 'dired)			; we will redefine dired-ls:
(or (fboundp 'dired-lisp-unix-ls)
    (fset 'dired-lisp-unix-ls (symbol-function 'dired-ls)))

(fset 'dired-ls 'dired-lisp-ls)

(defun dired-lisp-ls (file &optional switches wildcard full-directory-p)
  "dired-lisp.el's version of dired-ls.
Known switches: A a S r i s
Others are ignored.

  Insert ls output of FILE, optionally formatted with SWITCHES.
Optional third arg WILDCARD means treat non-directory part of FILE
as emacs regexp (_not_ a shell wildcard).

Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d'.

SWITCHES default to dired-listing-switches."
  (or switches (setq switches dired-listing-switches))
  (or (consp switches)			; convert to list of chars
      (setq switches (mapcar 'identity switches))) 
  (if wildcard
      (setq wildcard (file-name-nondirectory file) ; actually emacs regexp
	    ;; perhaps convert it from shell to emacs syntax?
	    file (file-name-directory file)))
  (if (or wildcard
	  full-directory-p)
      (let* ((dir (file-name-as-directory file))
	     (default-directory dir);; so that file-attributes works
	     (sum 0)
	     elt
	     (file-list (directory-files dir nil wildcard))
	     file-alist 
	     ;; do all bindings here for speed
	     fil attr)
	(cond ((memq ?A switches)
	       (setq file-list
		     (dired-lisp-delete-matching "^\\.\\.?$" file-list)))
	      ((not (memq ?a switches))
	       ;; if neither -A  nor -a, flush . files
	       (setq file-list
		     (dired-lisp-delete-matching "^\\." file-list))))
	(setq file-alist
	      (mapcar
	       (function
		(lambda (x)
		  ;; file-attributes("~bogus") bombs
		  (cons x (file-attributes (expand-file-name x)))))
	       ;; inserting the call to directory-files right here
	       ;; seems to stimulate an Emacs bug
	       ;; ILLEGAL DATATYPE (#o37777777727) or #o67
	       file-list))
	(insert "total \007\n")		; filled in afterwards
	(setq file-alist
	      (dired-lisp-handle-switches file-alist switches))
	(while file-alist
	  (setq elt (car file-alist)
		short (car elt)
		attr  (cdr elt)
		file-alist (cdr file-alist)
		fil (concat dir short)
		sum (+ sum (nth 7 attr)))
	  (insert (dired-lisp-format short attr switches)))
	(save-excursion
	  (search-backward "total \007")
	  (goto-char (match-end 0))
	  (delete-char -1)
	  (insert (format "%d" (1+ (/ sum 1024)))))
	)
    ;; if not full-directory-p, FILE *must not* end in /, as
    ;; file-attributes will not recognize a symlink to a directory
    ;; must make it a relative filename as ls does:
    (setq file (file-name-nondirectory file))
    (insert (dired-lisp-format file (file-attributes file) switches))))

(defun dired-lisp-delete-matching (regexp list)
  ;; Delete all elements matching REGEXP from LIST, return new list.
  ;; Should perhaps use setcdr for efficiency.
  (let (result)
    (while list
      (or (string-match regexp (car list))
	  (setq result (cons (car list) result)))
      (setq list (cdr list)))
    result))

(defun dired-lisp-handle-switches (file-alist switches)
  ;; FILE-ALIST's elements are (FILE . FILE-ATTRIBUTES).
  ;; Return new alist sorted according to switches.
  ;; Default sorting is alphabetically.
  (setq file-alist
	(sort file-alist
	      (cond ((memq ?S switches)
		     (function
		      (lambda (x y)
			;; 7th file attribute is file size
			;; Make largest file come first
			(< (nth 7 (cdr y)) (nth 7 (cdr x))))))
		    (t			; sorted alphabetically
		     (function
		      (lambda (x y)
			(string-lessp (car x) (car y))))))))
  (if (memq ?r switches)		; reverse sort order
      (setq file-alist (nreverse file-alist)))
  file-alist)

(defun dired-lisp-format (file-name file-attr &optional switches)
  (let ((file-type (nth 0 file-attr)))
    (concat (if (memq ?i switches)	; inode number
		(format "%6d " (nth 10 file-attr)))
	    ;; nil is treated like "" in concat
	    (if (memq ?s switches)	; size in K
		(format "%4d " (1+ (/ (nth 7 file-attr) 1024))))
	    (nth 8 file-attr)		; permission bits
	    ;; numeric uid/gid are more confusing than helpful
	    ;; Emacs should be able to make strings of them.
	    ;; user-login-name and user-full-name could take an
	    ;; optional arg.
	    (format " %3d %-8d %-8d %8d "
		    (nth 1 file-attr)	; no. of links
		    (nth 2 file-attr)	; uid
		    (nth 3 file-attr)	; gid
		    (nth 7 file-attr)	; size in bytes
		    )
	    ;; file-attributes's time is in a braindead format
	    ;; Emacs should have a ctime function
	    ;; current-time-string could take an optional arg.
	    "Jan 00 00:00 "		; fake time
	    file-name
	    (if (stringp file-type)	; is a symbolic link
		(concat " -> " file-type)
	      "")
	    "\n"
	    )))

