;;; ls-lisp.el --- emulate insert-directory completely in Emacs Lisp

;; Copyright (C) 1992, 1994 Free Software Foundation, Inc.

;; Author: Sebastian Kremer <sk@thp.uni-koeln.de>
;; Maintainer: FSF
;; Keywords: unix

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

;;;###autoload
(defvar ls-lisp-support-shell-wildcards t
  "*Non-nil means file patterns are treated as shell wildcards.
nil means they are treated as Emacs regexps (for backward compatibility).
This variable is checked by \\[insert-directory] only when `ls-lisp.el'
package is used.")

(defvar ls-lisp-dired-ignore-case nil
  "Non-nil causes dired buffers to sort alphabetically regardless of case.")

(defvar ls-lisp-use-insert-directory-program nil
  "Non-nil causes ls-lisp to revert back to using `insert-directory-program'.
This is useful on platforms where ls-lisp is dumped into Emacs, such as
Microsoft Windows, but you would still like to use a program to list
the contents of a directory.")

;; Remember the original insert-directory function.
(fset 'original-insert-directory (symbol-function 'insert-directory))

(defun insert-directory (file switches &optional wildcard full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.
SWITCHES may be a string of options, or a list of strings.
Optional third arg WILDCARD means treat FILE as shell wildcard.
Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d', so that a full listing is expected.

This version of the function comes from `ls-lisp.el'.  Depending upon
the value of `ls-lisp-use-insert-directory-program', it will use an
external program if non-nil or the lisp function `ls-lisp-insert-directory'
otherwise."
  (if ls-lisp-use-insert-directory-program
      (original-insert-directory file switches wildcard full-directory-p)
    (ls-lisp-insert-directory file switches wildcard full-directory-p)))

(defun ls-lisp-insert-directory (file switches &optional wildcard full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.
Optional third arg WILDCARD means treat FILE as shell wildcard.
Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d', so that a full listing is expected.

This version of the function comes from `ls-lisp.el'.  It does not
run any external programs or shells.  It supports ordinary shell
wildcards if `ls-lisp-support-shell-wildcards' variable is non-nil;
otherwise, it interprets wildcards as regular expressions to match
file names.

Not all `ls' switches are supported.  The switches that work
are: A a c i r S s t u"
  (let ((handler (find-file-name-handler file 'insert-directory)))
    (if handler
	(funcall handler 'insert-directory file switches
		 wildcard full-directory-p)
      ;; Sometimes we get ".../foo*/" as FILE.  While the shell and
      ;; `ls' don't mind, we certainly do, because it makes us think
      ;; there is no wildcard, only a directory name.
      (if (and ls-lisp-support-shell-wildcards
	       (string-match "[[?*]" file))
	  (progn
	    (or (not (eq (aref file (1- (length file))) ?/))
		(setq file (substring file 0 (1- (length file)))))
	    (setq wildcard t)))
      ;; Convert SWITCHES to a list of characters.
      (setq switches (append switches nil))
      (if wildcard
	  (setq wildcard
		(if ls-lisp-support-shell-wildcards
		    (wildcard-to-regexp (file-name-nondirectory file))
		  (file-name-nondirectory file))
		file (file-name-directory file)))
      (if (or wildcard
	      full-directory-p)
	  (let* ((dir (file-name-as-directory file))
		 (default-directory dir);; so that file-attributes works
		 (sum 0)
		 elt
		 short
		 (file-alist (directory-files-and-attributes dir nil wildcard))
		 (now (current-time))
		 ;; do all bindings here for speed
		 file-size
		 fil attr)
	    (cond ((memq ?A switches)
		   (setq file-alist
			 (ls-lisp-delete-matching "^\\.\\.?$" file-alist)))
		  ((not (memq ?a switches))
		   ;; if neither -A  nor -a, flush . files
		   (setq file-alist
			 (ls-lisp-delete-matching "^\\." file-alist))))
	    ;; ``Total'' line (filled in afterwards).
	    (insert (if (car-safe file-alist)
			"total \007\n"
		      ;; Shell says ``No match'' if no files match
		      ;; the wildcard; let's say something similar.
		      "(No match)\ntotal \007\n"))
	    (setq file-alist
		  (ls-lisp-handle-switches file-alist switches))
	    (while file-alist
	      (setq elt (car file-alist)
		    file-alist (cdr file-alist)
		    short (car elt)
		    attr (cdr elt)
		    file-size (nth 7 attr))
	      (and attr
		   (setq sum
			 ;; Even if neither SUM nor file's size
			 ;; overflow, their sum could.
			 (if (or (< sum (- 134217727 file-size))
				 (floatp sum)
				 (floatp file-size))
			     (+ sum file-size)
			   (+ (float sum) file-size)))
		   (insert (ls-lisp-format short attr file-size switches now))
		   ))
	    ;; Fill in total size of all files:
	    (save-excursion
	      (search-backward "total \007")
	      (goto-char (match-end 0))
	      (delete-char -1)
	      (insert (format "%.0f" (fceiling (/ sum 1024.0))))))
	;; if not full-directory-p, FILE *must not* end in /, as
	;; file-attributes will not recognize a symlink to a directory
	;; must make it a relative filename as ls does:
	(setq file (file-name-nondirectory file))
	(insert (ls-lisp-format file (file-attributes file)
				(nth 7 (file-attributes file)) switches
				(current-time)))))))

(defun ls-lisp-delete-matching (regexp list)
  ;; Delete all elements matching REGEXP from LIST, return new list.
  ;; Should perhaps use setcdr for efficiency.
  (let (result)
    (while list
      (or (string-match regexp (car (car list)))
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
		       (if ls-lisp-dired-ignore-case
			   (function
			    (lambda (x y)
			      (string-lessp (upcase (car x))
					    (upcase (car y)))))
			 (function
			  (lambda (x y)
			    (string-lessp (car x)
					  (car y))))))))))
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


(defun ls-lisp-format (file-name file-attr file-size switches now)
  (let ((file-type (nth 0 file-attr)))
    (concat (if (memq ?i switches)	; inode number
		(format "%6d " (nth 10 file-attr)))
	    ;; nil is treated like "" in concat
	    (if (memq ?s switches)	; size in K
		(format "%4.0f " (fceiling (/ file-size 1024.0))))
	    (nth 8 file-attr)		; permission bits
	    ;; numeric uid/gid are more confusing than helpful
	    ;; Emacs should be able to make strings of them.
	    ;; user-login-name and user-full-name could take an
	    ;; optional arg.
	    (format (if (floatp file-size)
 			" %3d %-8s %-8s %8.0f "
 		      " %3d %-8s %-8s %8d ")
		    (nth 1 file-attr)	; no. of links
		    (if (= (user-uid) (nth 2 file-attr))
			(user-login-name)
		      (int-to-string (nth 2 file-attr)))	; uid
		    (if (eq system-type 'ms-dos)
			"root"		; everything is root on MSDOS.
		      (int-to-string (nth 3 file-attr)))	; gid
		    file-size
		    )
	    (ls-lisp-format-time file-attr switches now)
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

(defun ls-lisp-format-time (file-attr switches now)
  ;; Format time string for file with attributes FILE-ATTR according
  ;; to SWITCHES (a list of ls option letters of which c and u are recognized).
  ;; Use the same method as `ls' to decide whether to show time-of-day or year,
  ;; depending on distance between file date and NOW.
  (let* ((time (nth (ls-lisp-time-index switches) file-attr))
	 (diff16 (- (car time) (car now)))
	 (diff (+ (ash diff16 16) (- (car (cdr time)) (car (cdr now)))))
	 (past-cutoff (- (* 6 30 24 60 60)))	; 6 30-day months
	 (future-cutoff (* 60 60)))		; 1 hour
    (condition-case nil
	(format-time-string
	 (if (and
	      (<= past-cutoff diff) (<= diff future-cutoff)
	      ;; Sanity check in case `diff' computation overflowed.
	      (<= (1- (ash past-cutoff -16)) diff16)
	      (<= diff16 (1+ (ash future-cutoff -16))))
	     "%b %e %H:%M"
	   "%b %e  %Y")
	 time)
      (error "Unk  0  0000"))))

(provide 'ls-lisp)

;;; ls-lisp.el ends here
