;;; dos-fns.el --- MS-Dos specific functions

;; Copyright (C) 1991, 1993, 1995, 1996 Free Software Foundation, Inc.

;; Maintainer: Morten Welinder <terra@diku.dk>
;; Keywords: internal

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Part of this code is taken from (or derived from) demacs.

;;; Code:

;; This overrides a trivial definition in files.el.
(defun convert-standard-filename (filename)
  "Convert a standard file's name to something suitable for the current OS.
This function's standard definition is trivial; it just returns the argument.
However, on some systems, the function is redefined
with a definition that really does change some file names."
  (if (or (not (stringp filename))
	  ;; This catches the case where FILENAME is "x:" or "x:/" or
	  ;; "/", thus preventing infinite recursion.
	  (string-match "\\`\\([a-zA-Z]:\\)?[/\\]?\\'" filename))
      filename
    (let ((flen (length filename)))
      ;; If FILENAME has a trailing slash, remove it and recurse.
      (if (memq (aref filename (1- flen)) '(?/ ?\\))
	  (concat (convert-standard-filename 
		   (substring filename 0 (1- flen)))
		  "/")
	(let* (;; ange-ftp gets in the way for names like "/foo:bar".
	       ;; We need to inhibit all magic file names, because
	       ;; remote file names should never be passed through
	       ;; this function, as they are not meant for the local
	       ;; filesystem!
	       (file-name-handler-alist nil)
	       (dir
		;; If FILENAME is "x:foo", file-name-directory returns
		;; "x:/bar/baz", substituting the current working
		;; directory on drive x:.  We want to be left with "x:"
		;; instead.
		(if (and (< 1 flen)
			 (eq (aref filename 1) ?:)
			 (null (string-match "[/\\]" filename)))
		    (substring filename 0 2)
		  (file-name-directory filename)))
	       (dlen-m-1 (1- (length dir)))
	       (string (copy-sequence (file-name-nondirectory filename)))
	       (lastchar (aref string (1- (length string))))
	       i firstdot)
	  (cond
	   ((msdos-long-file-names)
	    ;; Replace characters that are invalid even on Windows.
	    (while (setq i (string-match "[?*:<>|\"\000-\037]" string))
	      (aset string i ?!)))
	   ((not (member string '("" "." "..")))
	    ;; Change a leading period to a leading underscore.
	    (if (= (aref string 0) ?.)
		(aset string 0 ?_))
	    ;; If the name is longer than 8 chars, and doesn't have a
	    ;; period, and we have a dash or underscore that isn't too
	    ;; close to the beginning, change that to a period.  This
	    ;; is so we could salvage more characters of the original
	    ;; name by pushing them into the extension.
	    (if (and (not (string-match "\\." string))
		     (> (length string) 8)
		     ;; We don't gain anything if we put the period closer
		     ;; than 5 chars from the beginning (5 + 3 = 8).
		     (setq i (string-match "[-_]" string 5)))
		(aset string i ?\.))
	    ;; Get rid of invalid characters.
	    (while (setq i (string-match
			    "[^-a-zA-Z0-9_.%~^$!#&{}@`'()\200-\376]"
			    string))
	      (aset string i ?_))
	    ;; If we don't have a period in the first 8 chars, insert one.
	    ;; This enables to have 3 more characters from the original
	    ;; name in the extension.
	    (if (> (or (string-match "\\." string) (length string))
		   8)
		(setq string
		      (concat (substring string 0 8)
			      "."
			      (substring string 8))))
	    (setq firstdot (or (string-match "\\." string)
			       (1- (length string))))
	    ;; Truncate to 3 chars after the first period.
	    (if (> (length string) (+ firstdot 4))
		(setq string (substring string 0 (+ firstdot 4))))
	    ;; Change all periods except the first one into underscores.
	    ;; (DOS doesn't allow more than one period.)
	    (while (string-match "\\." string (1+ firstdot))
	      (setq i (string-match "\\." string (1+ firstdot)))
	      (aset string i ?_))
	    ;; If the last character of the original filename was `~' or `#',
	    ;; make sure the munged name ends with it also.  This is so that
	    ;; backup and auto-save files retain their telltale form.
	    (if (memq lastchar '(?~ ?#))
		(aset string (1- (length string)) lastchar))))
	  (concat (if (and (stringp dir)
			   (memq (aref dir dlen-m-1) '(?/ ?\\)))
		      (concat (convert-standard-filename
			       (substring dir 0 dlen-m-1))
			      "/")
		    (convert-standard-filename dir))
		  string))))))

(defun dos-8+3-filename (filename)
  "Truncate FILENAME to DOS 8+3 limits."
  (if (or (not (stringp filename))
	  (< (length filename) 5))	; too short to give any trouble
      filename
    (let ((flen (length filename)))
      ;; If FILENAME has a trailing slash, remove it and recurse.
      (if (memq (aref filename (1- flen)) '(?/ ?\\))
	  (concat (dos-8+3-filename (substring filename 0 (1- flen)))
		  "/")
	(let* (;; ange-ftp gets in the way for names like "/foo:bar".
	       ;; We need to inhibit all magic file names, because
	       ;; remote file names should never be passed through
	       ;; this function, as they are not meant for the local
	       ;; filesystem!
	       (file-name-handler-alist nil)
	       (dir
		;; If FILENAME is "x:foo", file-name-directory returns
		;; "x:/bar/baz", substituting the current working
		;; directory on drive x:.  We want to be left with "x:"
		;; instead.
		(if (and (< 1 flen)
			 (eq (aref filename 1) ?:)
			 (null (string-match "[/\\]" filename)))
		    (substring filename 0 2)
		  (file-name-directory filename)))
	       (dlen-m-1 (1- (length dir)))
	       (string (copy-sequence (file-name-nondirectory filename)))
	       (strlen (length string))
	       (lastchar (aref string (1- strlen)))
	       i firstdot)
	  (setq firstdot (string-match "\\." string))
	  (cond
	   (firstdot
	    ;; Truncate the extension to 3 characters.
	    (if (> strlen (+ firstdot 4))
		(setq string (substring string 0 (+ firstdot 4))))
	    ;; Truncate the basename to 8 characters.
	    (if (> firstdot 8)
		(setq string (concat (substring string 0 8)
				     "."
				     (substring string (1+ firstdot))))))
	   ((> strlen 8)
	    ;; No dot; truncate file name to 8 characters.
	    (setq string (substring string 0 8))))
	  ;; If the last character of the original filename was `~',
	  ;; make sure the munged name ends with it also.  This is so
	  ;; a backup file retains its final `~'.
	  (if (equal lastchar ?~)
	      (aset string (1- (length string)) lastchar))
	  (concat (if (and (stringp dir)
			   (memq (aref dir dlen-m-1) '(?/ ?\\)))
		      (concat (dos-8+3-filename (substring dir 0 dlen-m-1))
			      "/")
		    ;; Recurse to truncate the leading directories.
		    (dos-8+3-filename dir))
		  string))))))

;; See dos-vars.el for defcustom.
(defvar msdos-shells)

;;; Override setting chosen at startup.
(defun set-default-process-coding-system ()
  (setq default-process-coding-system
	(if default-enable-multibyte-characters
	    '(undecided-dos . undecided-dos)
	  '(raw-text-dos . raw-text-dos))))

(add-hook 'before-init-hook 'set-default-process-coding-system)

(defvar register-name-alist
  '((ax . 0) (bx . 1) (cx . 2) (dx . 3) (si . 4) (di . 5)
    (cflag . 6) (flags . 7)
    (al . (0 . 0)) (bl . (1 . 0)) (cl . (2 . 0)) (dl . (3 . 0))
    (ah . (0 . 1)) (bh . (1 . 1)) (ch . (2 . 1)) (dh . (3 . 1))))

(defun make-register ()
  (make-vector 8 0))

(defun register-value (regs name)
  (let ((where (cdr (assoc name register-name-alist))))
    (cond ((consp where)
	   (let ((tem (aref regs (car where))))
	     (if (zerop (cdr where))
		 (% tem 256)
	       (/ tem 256))))
	  ((numberp where)
	   (aref regs where))
	  (t nil))))

(defun set-register-value (regs name value)
  (and (numberp value)
       (>= value 0)
       (let ((where (cdr (assoc name register-name-alist))))
	 (cond ((consp where)
		(let ((tem (aref regs (car where)))
		      (value (logand value 255)))
		  (aset regs
			(car where)
			(if (zerop (cdr where))
			    (logior (logand tem 65280) value)
			  (logior (logand tem 255) (lsh value 8))))))
	       ((numberp where)
		(aset regs where (logand value 65535))))))
  regs)

(defsubst intdos (regs)
  (int86 33 regs))

;; Backward compatibility for obsolescent functions which
;; set screen size.

(defun mode25 ()
  "Changes the number of screen rows to 25."
  (interactive)
  (set-frame-size (selected-frame) 80 25))

(defun mode4350 ()
  "Changes the number of rows to 43 or 50.
Emacs always tries to set the screen height to 50 rows first.
If this fails, it will try to set it to 43 rows, on the assumption
that your video hardware might not support 50-line mode."
  (interactive)
  (set-frame-size (selected-frame) 80 50)
  (if (eq (frame-height (selected-frame)) 50)
      nil  ; the original built-in function returned nil
    (set-frame-size (selected-frame) 80 43)))

(provide 'dos-fns)

;;; dos-fns.el ends here
