;;;; dired-lisp.el - emulate Tree Dired's ls completely in Emacs Lisp

;;;; READ THE WARNING BELOW BEFORE USING THIS PROGRAM!

(defconst dired-lisp-version (substring "$Revision: 1.8 $" 11 -2)
  "$Id: dired-lisp.el,v 1.8 1992/05/01 17:50:56 sk Exp sk $")

;; Copyright (C) 1992 by Sebastian Kremer <sk@thp.uni-koeln.de>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
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

;; LISPDIR ENTRY for the Elisp Archive ===============================
;;    LCD Archive Entry:
;;    dired-lisp|Sebastian Kremer|sk@thp.uni-koeln.de
;;    |emulate Tree Dired's ls completely in Emacs Lisp 
;;    |$Date: 1992/05/01 17:50:56 $|$Revision: 1.8 $|

;; INSTALLATION =======================================================
;; 
;; Put this file into your load-path.  Loading it will result in
;; redefining function dired-ls to not call ls.

;; You need tree dired from ftp.cs.buffalo.edu:pub/Emacs/diredall.tar.Z,
;; classic (e.g. 18.57) dired.el will not work.

;; OVERVIEW ===========================================================

;; This file overloads tree dired so that all fileinfo is retrieved
;; directly from Emacs lisp, without using an ls subprocess.

;; Useful if you cannot afford to fork Emacs on a real memory UNIX,
;; under VMS, or if you don't have the ls program, or if you want
;; different format from what ls offers.

;; Beware that if you change the output format of dired-ls, you'll
;; have to change dired-move-to-filename and
;; dired-move-to-end-of-filename as well.

;; With this package is loaded, dired uses regexps instead of shell
;; wildcards. If you enter regexps remember to double each $ sign.
;; For example, to dired all elisp (*.el) files, enter `.*\.el$$',
;; resulting in the regexp `.*\.el$'.

;; WARNING ===========================================================

;; With earlier version of this program I sometimes got an internal
;; Emacs error:

;;   Signalling: (wrong-type-argument natnump #<EMACS BUG: ILLEGAL
;;   DATATYPE (#o37777777727) Save your buffers immediately and please
;;   report this bug>)

;; The datatype differs (I also got #o67 once).

;; Sometimes emacs just crashed with a fatal error.

;; After I've avoided using directory-files and file-attributes
;; together inside a mapcar, the bug didn't surface any longer.

;;  RESTRICTIONS =====================================================

;; * many ls switches are ignored, see docstring of `dired-ls'.

;; * In Emacs 18: cannot display date of file, displays a fake date
;;   "Jan 00 00:00" instead (dates do work in Emacs 19)

;; * Only numeric uid/gid

;; * if you load dired-lisp after ange-ftp, remote listings look
;;   really strange: 
;;
;;   total 1
;;   d?????????  -1 -1       -1             -1 Jan  1  1970 .
;;   d?????????  -1 -1       -1             -1 Jan  1  1970 ..
;;
;;   This is because ange-ftp's file-attributes does not return much
;;   useful information.
;;
;;   If you load dired-lisp first, there seem to be no problems.

;; TODO ==============================================================

;; Recognize some more ls switches: R F


(require 'dired)			; we will redefine dired-ls:
(or (fboundp 'dired-lisp-unix-ls)
    (fset 'dired-lisp-unix-ls (symbol-function 'dired-ls)))

(fset 'dired-ls 'dired-lisp-ls)

(defun dired-lisp-ls (file &optional switches wildcard full-directory-p)
  "dired-lisp.el's version of dired-ls.
Known switches: A a S r i s t
In Emacs 19, additional known switches are: c u
Others are ignored.

  Insert ls output of FILE, optionally formatted with SWITCHES.
Optional third arg WILDCARD means treat non-directory part of FILE as
emacs regexp (_not_ a shell wildcard).  If you enter regexps remember
to double each $ sign.

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
	     short
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
		       (setq index (dired-lisp-time-index switches))
		       (function
			(lambda (x y)
			  (time-lessp (nth index (cdr y))
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
(defun time-lessp (time0 time1)
  (let ((hi0 (car time0))
	(hi1 (car time1))
	(lo0 (car (cdr time0)))
	(lo1 (car (cdr time1))))
    (or (< hi0 hi1)
	(and (= hi0 hi1)
	     (< lo0 lo1)))))


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
	    (dired-lisp-format-time file-attr switches)
	    " "
	    file-name
	    (if (stringp file-type)	; is a symbolic link
		(concat " -> " file-type)
	      "")
	    "\n"
	    )))

(defun dired-lisp-time-index (switches)
  ;; Return index into file-attributes according to ls SWITCHES.
  (cond
   ((memq ?c switches) 6)		; last mode change
   ((memq ?u switches) 4)		; last access
   ;; default is last modtime
   (t 5)))

(defun dired-lisp-format-time (file-attr switches)
  ;; Format time string for file with attributes FILE-ATTR according
  ;; to SWITCHES (a list of ls option letters of which c and u are recognized).
  ;; file-attributes's time is in a braindead format
  ;; Emacs 19 can format it using a new optional argument to
  ;; current-time-string, for Emacs 18 we just return the faked fixed
  ;; date "Jan 00 00:00 ".
  (condition-case error-data
      (let* ((time (current-time-string
		    (nth (dired-lisp-time-index switches) file-attr)))
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

(provide 'dired-lisp)

; eof
