;;; ls-lisp.el --- emulate insert-directory completely in Emacs Lisp

;; Author: Sebastian Kremer <sk@thp.uni-koeln.de>
;; Keywords: unix

;; Copyright (C) 1992, 1994 by Sebastian Kremer <sk@thp.uni-koeln.de>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; INSTALLATION =======================================================
;; 
;; Put this file into your load-path.  To use it, load it
;; with (load "ls-lisp").

;; OVERVIEW ===========================================================

;; This file overloads the function insert-directory to implement it
;; directly from Emacs lisp, without running `ls' in a subprocess.

;; It is useful if you cannot afford to fork Emacs on a real memory UNIX,
;; under VMS, or if you don't have the ls program, or if you want
;; different format from what ls offers.

;; This function uses regexps instead of shell
;; wildcards.  If you enter regexps remember to double each $ sign.
;; For example, to include files *.el, enter `.*\.el$$',
;; resulting in the regexp `.*\.el$'.

;;  RESTRICTIONS =====================================================

;; * many ls switches are ignored, see docstring of `insert-directory'.

;; * Only numeric uid/gid

;; TODO ==============================================================

;; Recognize some more ls switches: R F

;;; Code:

(defun insert-directory (file &optional switches wildcard full-directory-p)
  "Insert directory listing for of FILE, formatted according to SWITCHES.
Leaves point after the inserted text.
Optional third arg WILDCARD means treat FILE as shell wildcard.
Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d', so that a full listing is expected.

This version of the function comes from `ls-lisp.el'.
It does not support ordinary shell wildcards; instead, it allows
regular expressions to match file names.

The switches that work are: A a c i r S s t u"
  (let ((handler (find-file-name-handler file 'insert-directory)))
    (if handler
	(funcall handler 'insert-directory file switches
		 wildcard full-directory-p)
      ;; Convert SWITCHES to a list of characters.
      (setq switches (append switches nil))
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
		 short
		 (file-list (directory-files dir nil wildcard))
		 file-alist 
		 ;; do all bindings here for speed
		 fil attr)
	    (cond ((memq ?A switches)
		   (setq file-list
			 (ls-lisp-delete-matching "^\\.\\.?$" file-list)))
		  ((not (memq ?a switches))
		   ;; if neither -A  nor -a, flush . files
		   (setq file-list
			 (ls-lisp-delete-matching "^\\." file-list))))
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
	    (insert "total \007\n")	; filled in afterwards
	    (setq file-alist
		  (ls-lisp-handle-switches file-alist switches))
	    (while file-alist
	      (setq elt (car file-alist)
		    file-alist (cdr file-alist)
		    short (car elt)
		    attr (cdr elt))
	      (and attr
		   (setq sum (+ sum (nth 7 attr)))
		   (insert (ls-lisp-format short attr switches))))
	    ;; Fill in total size of all files:
	    (save-excursion
	      (search-backward "total \007")
	      (goto-char (match-end 0))
	      (delete-char -1)
	      (insert (format "%d" (1+ (/ sum 1024))))))
	;; if not full-directory-p, FILE *must not* end in /, as
	;; file-attributes will not recognize a symlink to a directory
	;; must make it a relative filename as ls does:
	(setq file (file-name-nondirectory file))
	(insert (ls-lisp-format file (file-attributes file) switches))))))

(defun ls-lisp-delete-matching (regexp list)
  ;; Delete all elements matching REGEXP from LIST, return new list.
  ;; Should perhaps use setcdr for efficiency.
  (let (result)
    (while list
      (or (string-match regexp (car list))
	  (setq result (cons (car list) result)))
      (setq list (cdr list)))
    result))

(defun ls-lisp-handle-switches (file-alist switches)
  ;; FILE-ALIST's elements are (FILE . FILE-ATTRIBUTES).
  ;; Return new alist sorted according to SWITCHES which is a list of
  ;; characters.  Default sorting is alphabetically.
  (let (index)
    (setq file-alist
	  (sort file-alist
		(cond ((memq ?S switches) ; sorted on size
		       (function
			(lambda (x y)
			  ;; 7th file attribute is file size
			  ;; Make largest file come first
			  (< (nth 7 (cdr y))
			     (nth 7 (cdr x))))))
		      ((memq ?t switches) ; sorted on time
		       (setq index (ls-lisp-time-index switches))
		       (function
			(lambda (x y)
			  (ls-lisp-time-lessp (nth index (cdr y))
					      (nth index (cdr x))))))
		      (t		; sorted alphabetically
		       (function
			(lambda (x y)
			  (string-lessp (car x)
					(car y)))))))))
  (if (memq ?r switches)		; reverse sort order
      (setq file-alist (nreverse file-alist)))
  file-alist)

;; From Roland McGrath.  Can use this to sort on time.
(defun ls-lisp-time-lessp (time0 time1)
  (let ((hi0 (car time0))
	(hi1 (car time1))
	(lo0 (car (cdr time0)))
	(lo1 (car (cdr time1))))
    (or (< hi0 hi1)
	(and (= hi0 hi1)
	     (< lo0 lo1)))))


(defun ls-lisp-format (file-name file-attr &optional switches)
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
	    (format " %3d %8s %8s %8d "
		    (nth 1 file-attr)	; no. of links
		    (if (= (user-uid) (nth 2 file-attr))
			(user-login-name)
		      (int-to-string (nth 2 file-attr)))	; uid
		    (if (eq system-type 'ms-dos)
			"root"		; everything is root on MSDOS.
		      (int-to-string (nth 3 file-attr)))	; gid
		    (nth 7 file-attr)	; size in bytes
		    )
	    (ls-lisp-format-time file-attr switches)
	    " "
	    file-name
	    (if (stringp file-type)	; is a symbolic link
		(concat " -> " file-type)
	      "")
	    "\n"
	    )))

(defun ls-lisp-time-index (switches)
  ;; Return index into file-attributes according to ls SWITCHES.
  (cond
   ((memq ?c switches) 6)		; last mode change
   ((memq ?u switches) 4)		; last access
   ;; default is last modtime
   (t 5)))

(defun ls-lisp-format-time (file-attr switches)
  ;; Format time string for file with attributes FILE-ATTR according
  ;; to SWITCHES (a list of ls option letters of which c and u are recognized).
  ;; file-attributes's time is in a braindead format
  ;; Emacs 19 can format it using a new optional argument to
  ;; current-time-string, for Emacs 18 we just return the faked fixed
  ;; date "Jan 00 00:00 ".
  (condition-case error-data
      (let* ((time (current-time-string
		    (nth (ls-lisp-time-index switches) file-attr)))
	     (date (substring time 4 11)) ; "Apr 30 "
	     (clock (substring time 11 16)) ; "11:27"
	     (year (substring time 19 24)) ; " 1992"
	     (same-year (equal year (substring (current-time-string) 19 24))))
	(concat date			; has trailing SPC
		(if same-year
		    ;; this is not exactly the same test used by ls
		    ;; ls tests if the file is older than 6 months
		    ;; but we can't do time differences easily
		    clock
		  year)))
    (error
     "Jan 00 00:00")))

(provide 'ls-lisp)

;;; ls-lisp.el ends here
