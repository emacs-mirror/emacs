;;; ls-lisp.el --- emulate insert-directory completely in Emacs Lisp

;; Copyright (C) 1992, 1994, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007 Free Software Foundation, Inc.

;; Author: Sebastian Kremer <sk@thp.uni-koeln.de>
;; Modified by: Francis J. Wright <F.J.Wright@maths.qmw.ac.uk>
;; Maintainer: FSF
;; Keywords: unix, dired

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; OVERVIEW ==========================================================

;; This file redefines the function `insert-directory' to implement it
;; directly from Emacs lisp, without running ls in a subprocess.  It
;; is useful if you cannot afford to fork Emacs on a real memory UNIX,
;; under VMS or other non-UNIX platforms if you don't have the ls
;; program, or if you want a different format from what ls offers.

;; This function can use regexps instead of shell wildcards.  If you
;; enter regexps remember to double each $ sign.  For example, to
;; include files *.el, enter `.*\.el$$', resulting in the regexp
;; `.*\.el$'.

;; RESTRICTIONS ======================================================

;; * A few obscure ls switches are still ignored: see the docstring of
;; `insert-directory'.

;; TO DO =============================================================

;; Complete handling of F switch (if/when possible).

;; FJW: May be able to sort much faster by consing the sort key onto
;; the front of each list element, sorting and then stripping the key
;; off again!

;;; History:

;; Written originally by Sebastian Kremer <sk@thp.uni-koeln.de>
;; Revised by Andrew Innes and Geoff Volker (and maybe others).

;; Modified by Francis J. Wright <F.J.Wright@maths.qmw.ac.uk>, mainly
;; to support many more ls options, "platform emulation" and more
;; robust sorting.

;;; Code:

(eval-when-compile (require 'cl))

(defgroup ls-lisp nil
  "Emulate the ls program completely in Emacs Lisp."
  :version "21.1"
  :group 'dired)

(defcustom ls-lisp-emulation
  (cond ((eq system-type 'macos) 'MacOS)
	;; ((eq system-type 'windows-nt) 'MS-Windows)
	((memq system-type
	       '(hpux dgux usg-unix-v unisoft-unix rtu irix berkeley-unix))
	 'UNIX))			; very similar to GNU
  ;; Anything else defaults to nil, meaning GNU.
  "*Platform to emulate: GNU (default), MacOS, MS-Windows, UNIX.
Corresponding value is one of the atoms: nil, MacOS, MS-Windows, UNIX.
Sets default values for: `ls-lisp-ignore-case', `ls-lisp-dirs-first',
`ls-lisp-verbosity'.  Need not match actual platform.  Changing this
option will have no effect until you restart Emacs."
  :type '(choice (const :tag "GNU" nil)
		 (const MacOS)
		 (const MS-Windows)
		 (const UNIX))
  :group 'ls-lisp)

(defcustom ls-lisp-ignore-case
  ;; Name change for consistency with other option names.
  (or (memq ls-lisp-emulation '(MS-Windows MacOS))
      (and (boundp 'ls-lisp-dired-ignore-case) ls-lisp-dired-ignore-case))
  "*Non-nil causes ls-lisp alphabetic sorting to ignore case."
  :type 'boolean
  :group 'ls-lisp)

(defcustom ls-lisp-dirs-first (eq ls-lisp-emulation 'MS-Windows)
  "*Non-nil causes ls-lisp to sort directories first in any ordering.
\(Or last if it is reversed.)  Follows Microsoft Windows Explorer."
  ;; Functionality suggested by Chris McMahan <cmcmahan@one.net>
  :type 'boolean
  :group 'ls-lisp)

(defcustom ls-lisp-verbosity
  (cond ((eq ls-lisp-emulation 'MacOS) nil)
	((eq ls-lisp-emulation 'MS-Windows)
	 (if (and (fboundp 'w32-using-nt) (w32-using-nt))
	     '(links)))			; distinguish NT/2K from 9x
	((eq ls-lisp-emulation 'UNIX) '(links uid)) ; UNIX ls
	(t '(links uid gid)))		; GNU ls
  "*A list of optional file attributes that ls-lisp should display.
It should contain none or more of the symbols: links, uid, gid.
A value of nil (or an empty list) means display none of them.

Concepts come from UNIX: `links' means count of names associated with
the file\; `uid' means user (owner) identifier\; `gid' means group
identifier.

If emulation is MacOS then default is nil\;
if emulation is MS-Windows then default is `(links)' if platform is
Windows NT/2K, nil otherwise\;
if emulation is UNIX then default is `(links uid)'\;
if emulation is GNU then default is `(links uid gid)'."
  ;; Functionality suggested by Howard Melman <howard@silverstream.com>
  :type '(set (const :tag "Show Link Count" links)
	      (const :tag "Show User" uid)
	      (const :tag "Show Group" gid))
  :group 'ls-lisp)

(defcustom ls-lisp-use-insert-directory-program
  (not (memq system-type '(macos ms-dos windows-nt)))
  "*Non-nil causes ls-lisp to revert back to using `insert-directory-program'.
This is useful on platforms where ls-lisp is dumped into Emacs, such as
Microsoft Windows, but you would still like to use a program to list
the contents of a directory."
  :type 'boolean
  :group 'ls-lisp)

;;; Autoloaded because it is let-bound in `recover-session', `mail-recover-1'.
;;;###autoload
(defcustom ls-lisp-support-shell-wildcards t
  "*Non-nil means ls-lisp treats file patterns as shell wildcards.
Otherwise they are treated as Emacs regexps (for backward compatibility)."
  :type 'boolean
  :group 'ls-lisp)

(defcustom ls-lisp-format-time-list
  '("%b %e %H:%M"
    "%b %e  %Y")
  "*List of `format-time-string' specs to display file time stamps.
These specs are used ONLY if a valid locale can not be determined.

If `ls-lisp-use-localized-time-format' is non-nil, these specs are used
regardless of whether the locale can be determined.

Syntax:  (EARLY-TIME-FORMAT OLD-TIME-FORMAT)

The EARLY-TIME-FORMAT is used if file has been modified within the
current year. The OLD-TIME-FORMAT is used for older files.  To use ISO
8601 dates, you could set:

\(setq ls-lisp-format-time-list
       '(\"%Y-%m-%d %H:%M\"
         \"%Y-%m-%d      \"))"
  :type '(list (string :tag "Early time format")
	       (string :tag "Old time format"))
  :group 'ls-lisp)

(defcustom ls-lisp-use-localized-time-format nil
  "*Non-nil causes ls-lisp to use `ls-lisp-format-time-list' even if
a valid locale is specified.

WARNING: Using localized date/time format might cause Dired columns
to fail to lign up, e.g. if month names are not all of the same length."
  :type 'boolean
  :group 'ls-lisp)

(defvar original-insert-directory nil
  "This holds the original function definition of `insert-directory'.")

;; Remember the original insert-directory function
(or (featurep 'ls-lisp)  ; FJW: unless this file is being reloaded!
    (setq original-insert-directory (symbol-function 'insert-directory)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-directory (file switches &optional wildcard full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.
SWITCHES may be a string of options, or a list of strings.
Optional third arg WILDCARD means treat FILE as shell wildcard.
Optional fourth arg FULL-DIRECTORY-P means file is a directory and
switches do not contain `d', so that a full listing is expected.

This version of the function comes from `ls-lisp.el'.
If the value of `ls-lisp-use-insert-directory-program' is non-nil then
it works exactly like the version from `files.el' and runs a directory
listing program whose name is in the variable
`insert-directory-program'; if also WILDCARD is non-nil then it runs
the shell specified by `shell-file-name'.  If the value of
`ls-lisp-use-insert-directory-program' is nil then it runs a Lisp
emulation.

The Lisp emulation does not run any external programs or shells.  It
supports ordinary shell wildcards if `ls-lisp-support-shell-wildcards'
is non-nil; otherwise, it interprets wildcards as regular expressions
to match file names.  It does not support all `ls' switches -- those
that work are: A a c i r S s t u U X g G B C R and F partly."
  (if ls-lisp-use-insert-directory-program
      (funcall original-insert-directory
	       file switches wildcard full-directory-p)
    ;; We need the directory in order to find the right handler.
    (let ((handler (find-file-name-handler (expand-file-name file)
					   'insert-directory))
	  wildcard-regexp)
      (if handler
	  (funcall handler 'insert-directory file switches
		   wildcard full-directory-p)
	;; Remove --dired switch
	(if (string-match "--dired " switches)
	    (setq switches (replace-match "" nil nil switches)))
	;; Convert SWITCHES to a list of characters.
	(setq switches (delete ?- (append switches nil)))
	;; Sometimes we get ".../foo*/" as FILE.  While the shell and
	;; `ls' don't mind, we certainly do, because it makes us think
	;; there is no wildcard, only a directory name.
	(if (and ls-lisp-support-shell-wildcards
		 (string-match "[[?*]" file))
	    (progn
	      (or (not (eq (aref file (1- (length file))) ?/))
		  (setq file (substring file 0 (1- (length file)))))
	      (setq wildcard t)))
	(if wildcard
	    (setq wildcard-regexp
		  (if ls-lisp-support-shell-wildcards
		      (wildcard-to-regexp (file-name-nondirectory file))
		    (file-name-nondirectory file))
		  file (file-name-directory file))
	  (if (memq ?B switches) (setq wildcard-regexp "[^~]\\'")))
	(ls-lisp-insert-directory
	 file switches (ls-lisp-time-index switches)
	 wildcard-regexp full-directory-p)
	;; Try to insert the amount of free space.
	(save-excursion
	  (goto-char (point-min))
	  ;; First find the line to put it on.
	  (when (re-search-forward "^total" nil t)
	    (let ((available (get-free-disk-space ".")))
	      (when available
		;; Replace "total" with "total used", to avoid confusion.
		(replace-match "total used in directory")
		(end-of-line)
		(insert " available " available)))))))))

(defun ls-lisp-insert-directory
  (file switches time-index wildcard-regexp full-directory-p)
  "Insert directory listing for FILE, formatted according to SWITCHES.
Leaves point after the inserted text.  This is an internal function
optionally called by the `ls-lisp.el' version of `insert-directory'.
It is called recursively if the -R switch is used.
SWITCHES is a *list* of characters.  TIME-INDEX is the time index into
file-attributes according to SWITCHES.  WILDCARD-REGEXP is nil or an *Emacs
regexp*.  FULL-DIRECTORY-P means file is a directory and SWITCHES does
not contain `d', so that a full listing is expected."
  (if (or wildcard-regexp full-directory-p)
      (let* ((dir (file-name-as-directory file))
	     (default-directory dir)	; so that file-attributes works
	     (file-alist
	      (directory-files-and-attributes dir nil wildcard-regexp t 'string))
	     (now (current-time))
	     (sum 0)
	     ;; do all bindings here for speed
	     total-line files elt short file-size fil attr)
	(cond ((memq ?A switches)
	       (setq file-alist
		     (ls-lisp-delete-matching "^\\.\\.?$" file-alist)))
	      ((not (memq ?a switches))
	       ;; if neither -A  nor -a, flush . files
	       (setq file-alist
		     (ls-lisp-delete-matching "^\\." file-alist))))
	(setq file-alist
	      (ls-lisp-handle-switches file-alist switches))
	(if (memq ?C switches)		; column (-C) format
	    (ls-lisp-column-format file-alist)
	  (setq total-line (cons (point) (car-safe file-alist)))
	  (setq files file-alist)
	  (while files			; long (-l) format
	    (setq elt (car files)
		  files (cdr files)
		  short (car elt)
		  attr (cdr elt)
		  file-size (nth 7 attr))
	    (and attr
		 (setq sum (+ file-size
			      ;; Even if neither SUM nor file's size
			      ;; overflow, their sum could.
			      (if (or (< sum (- 134217727 file-size))
				      (floatp sum)
				      (floatp file-size))
				  sum
				(float sum))))
		 (insert (ls-lisp-format short attr file-size
					 switches time-index now))))
	  ;; Insert total size of all files:
	  (save-excursion
	    (goto-char (car total-line))
	    (or (cdr total-line)
		;; Shell says ``No match'' if no files match
		;; the wildcard; let's say something similar.
		(insert "(No match)\n"))
	    (insert (format "total %.0f\n" (fceiling (/ sum 1024.0))))))
	(if (memq ?R switches)
	    ;; List the contents of all directories recursively.
	    ;; cadr of each element of `file-alist' is t for
	    ;; directory, string (name linked to) for symbolic
	    ;; link, or nil.
	    (while file-alist
	      (setq elt (car file-alist)
		    file-alist (cdr file-alist))
	      (when (and (eq (cadr elt) t) ; directory
			 (not (string-match "\\`\\.\\.?\\'" (car elt))))
		(setq elt (expand-file-name (car elt) dir))
		(insert "\n" elt ":\n")
		(ls-lisp-insert-directory
		 elt switches time-index wildcard-regexp full-directory-p)))))
    ;; If not full-directory-p, FILE *must not* end in /, as
    ;; file-attributes will not recognize a symlink to a directory,
    ;; so must make it a relative filename as ls does:
    (if (eq (aref file (1- (length file))) ?/)
	(setq file (substring file 0 -1)))
    (let ((fattr (file-attributes file 'string)))
      (if fattr
	  (insert (ls-lisp-format file fattr (nth 7 fattr)
				  switches time-index (current-time)))
	(message "%s: doesn't exist or is inaccessible" file)
	(ding) (sit-for 2)))))		; to show user the message!

(defun ls-lisp-column-format (file-alist)
  "Insert the file names (only) in FILE-ALIST into the current buffer.
Format in columns, sorted vertically, following GNU ls -C.
Responds to the window width as ls should but may not!"
  (let (files fmt ncols collen (nfiles 0) (colwid 0))
    ;; Count number of files as `nfiles', build list of filenames as
    ;; `files', and find maximum filename length as `colwid':
    (let (file len)
      (while file-alist
	(setq nfiles (1+ nfiles)
	      file (caar file-alist)
	      files (cons file files)
	      file-alist (cdr file-alist)
	      len (length file))
	(if (> len colwid) (setq colwid len))))
    (setq files (nreverse files)
	  colwid (+ 2 colwid)		; 2 character column gap
	  fmt (format "%%-%ds" colwid)	; print format
	  ncols (/ (window-width) colwid) ; no of columns
	  collen (/ nfiles ncols))	; floor of column length
    (if (> nfiles (* collen ncols)) (setq collen (1+ collen)))
    ;; Output the file names in columns, sorted vertically:
    (let ((i 0) j)
      (while (< i collen)
	(setq j i)
	(while (< j nfiles)
	  (insert (format fmt (nth j files)))
	  (setq j (+ j collen)))
	;; FJW: This is completely unnecessary, but I don't like
	;; trailing white space...
	(delete-region (point) (progn (skip-chars-backward " \t") (point)))
	(insert ?\n)
	(setq i (1+ i))))))

(defun ls-lisp-delete-matching (regexp list)
  "Delete all elements matching REGEXP from LIST, return new list."
  ;; Should perhaps use setcdr for efficiency.
  (let (result)
    (while list
      (or (string-match regexp (caar list))
	  (setq result (cons (car list) result)))
      (setq list (cdr list)))
    result))

(defsubst ls-lisp-string-lessp (s1 s2)
  "Return t if string S1 is less than string S2 in lexicographic order.
Case is significant if `ls-lisp-ignore-case' is nil.
Unibyte strings are converted to multibyte for comparison."
  (let ((u (compare-strings s1 0 nil s2 0 nil ls-lisp-ignore-case)))
    (and (numberp u) (< u 0))))

(defun ls-lisp-handle-switches (file-alist switches)
  "Return new FILE-ALIST sorted according to SWITCHES.
SWITCHES is a list of characters.  Default sorting is alphabetic."
  ;; FILE-ALIST's elements are (FILE . FILE-ATTRIBUTES).
  (or (memq ?U switches)		; unsorted
      ;; Catch and ignore unexpected sorting errors
      (condition-case err
	  (setq file-alist
		(let (index)
		  ;; Copy file-alist in case of error
		  (sort (copy-sequence file-alist) ; modifies its argument!
			(cond ((memq ?S switches)
			       (lambda (x y) ; sorted on size
				 ;; 7th file attribute is file size
				 ;; Make largest file come first
				 (< (nth 7 (cdr y))
				    (nth 7 (cdr x)))))
			      ((setq index (ls-lisp-time-index switches))
			       (lambda (x y) ; sorted on time
				 (ls-lisp-time-lessp (nth index (cdr y))
						     (nth index (cdr x)))))
			      ((memq ?X switches)
			       (lambda (x y) ; sorted on extension
				 (ls-lisp-string-lessp
				  (ls-lisp-extension (car x))
				  (ls-lisp-extension (car y)))))
			      (t
			       (lambda (x y) ; sorted alphabetically
				 (ls-lisp-string-lessp (car x) (car y))))))))
	(error (message "Unsorted (ls-lisp sorting error) - %s"
			(error-message-string err))
	       (ding) (sit-for 2))))	; to show user the message!
  (if (memq ?F switches)		; classify switch
      (setq file-alist (mapcar 'ls-lisp-classify file-alist)))
  (if ls-lisp-dirs-first
  ;; Re-sort directories first, without otherwise changing the
  ;; ordering, and reverse whole list.  cadr of each element of
  ;; `file-alist' is t for directory, string (name linked to) for
  ;; symbolic link, or nil.
      (let (el dirs files)
	(while file-alist
	  (if (or (eq (cadr (setq el (car file-alist))) t) ; directory
                  (and (stringp (cadr el))
                       (file-directory-p (cadr el)))) ; symlink to a directory
	      (setq dirs (cons el dirs))
	    (setq files (cons el files)))
	  (setq file-alist (cdr file-alist)))
	(setq file-alist
	      (if (memq ?U switches)	; unsorted order is reversed
		  (nconc dirs files)
		(nconc files dirs)
		))))
  ;; Finally reverse file alist if necessary.
  ;; (eq below MUST compare `(not (memq ...))' to force comparison of
  ;; `t' or `nil', rather than list tails!)
  (if (eq (eq (not (memq ?U switches))	; unsorted order is reversed
	      (not (memq ?r switches)))	; reversed sort order requested
	  ls-lisp-dirs-first)		; already reversed
      (nreverse file-alist)
    file-alist))

(defun ls-lisp-classify (filedata)
  "Append a character to each file name indicating the file type.
Also, for regular files that are executable, append `*'.
The file type indicators are `/' for directories, `@' for symbolic
links, `|' for FIFOs, `=' for sockets, and nothing for regular files.
\[But FIFOs and sockets are not recognized.]
FILEDATA has the form (filename . `file-attributes').  Its `cadr' is t
for directory, string (name linked to) for symbolic link, or nil."
  (let ((file-name (car filedata))
        (type (cadr filedata)))
    (cond (type
	   (cons
	    (concat file-name (if (eq type t) "/" "@"))
	    (cdr filedata)))
	  ((string-match "x" (nth 9 filedata))
	   (cons
	    (concat file-name "*")
	    (cdr filedata)))
	  (t filedata))))

(defun ls-lisp-extension (filename)
  "Return extension of FILENAME (ignoring any version extension)
FOLLOWED by null and full filename, SOLELY for full alpha sort."
  ;; Force extension sort order: `no ext' then `null ext' then `ext'
  ;; to agree with GNU ls.
  (concat
   (let* ((i (length filename)) end)
     (if (= (aref filename (1- i)) ?.) ; null extension
	 "\0"
       (while (and (>= (setq i (1- i)) 0)
		   (/= (aref filename i) ?.)))
       (if (< i 0) "\0\0"		; no extension
	 (if (/= (aref filename (1+ i)) ?~)
	     (substring filename (1+ i))
	   ;; version extension found -- ignore it
	   (setq end i)
	   (while (and (>= (setq i (1- i)) 0)
		       (/= (aref filename i) ?.)))
	   (if (< i 0) "\0\0"	; no extension
	     (substring filename (1+ i) end))))
       )) "\0" filename))

;; From Roland McGrath.  Can use this to sort on time.
(defun ls-lisp-time-lessp (time0 time1)
  "Return t if time TIME0 is earlier than time TIME1."
  (let ((hi0 (car time0)) (hi1 (car time1)))
    (or (< hi0 hi1)
	(and (= hi0 hi1)
	     (< (cadr time0) (cadr time1))))))

(defun ls-lisp-format (file-name file-attr file-size switches time-index now)
  "Format one line of long ls output for file FILE-NAME.
FILE-ATTR and FILE-SIZE give the file's attributes and size.
SWITCHES, TIME-INDEX and NOW give the full switch list and time data."
  (let ((file-type (nth 0 file-attr))
	;; t for directory, string (name linked to)
	;; for symbolic link, or nil.
	(drwxrwxrwx (nth 8 file-attr)))	; attribute string ("drwxrwxrwx")
    (concat (if (memq ?i switches)	; inode number
		(format " %6d" (nth 10 file-attr)))
	    ;; nil is treated like "" in concat
	    (if (memq ?s switches)	; size in K
		(format " %4.0f" (fceiling (/ file-size 1024.0))))
	    drwxrwxrwx			; attribute string
	    (if (memq 'links ls-lisp-verbosity)
		(format " %3d" (nth 1 file-attr))) ; link count
	    ;; Numeric uid/gid are more confusing than helpful;
	    ;; Emacs should be able to make strings of them.
	    ;; They tend to be bogus on non-UNIX platforms anyway so
	    ;; optionally hide them.
	    (if (memq 'uid ls-lisp-verbosity)
		;; uid can be a sting or an integer
		(let ((uid (nth 2 file-attr)))
                  (format (if (stringp uid) " %-8s" " %-8d") uid)))
	    (if (not (memq ?G switches)) ; GNU ls -- shows group by default
		(if (or (memq ?g switches) ; UNIX ls -- no group by default
			(memq 'gid ls-lisp-verbosity))
                    (let ((gid (nth 3 file-attr)))
                      (format (if (stringp gid) " %-8s" " %-8d") gid))))
	    (ls-lisp-format-file-size file-size (memq ?h switches))
	    " "
	    (ls-lisp-format-time file-attr time-index now)
	    " "
	    (propertize file-name 'dired-filename t)
	    (if (stringp file-type)	; is a symbolic link
		(concat " -> " file-type))
	    "\n"
	    )))

(defun ls-lisp-time-index (switches)
  "Return time index into file-attributes according to ls SWITCHES list.
Return nil if no time switch found."
  ;; FJW: Default of nil is IMPORTANT and used in `ls-lisp-handle-switches'!
  (cond ((memq ?c switches) 6)		; last mode change
	((memq ?t switches) 5)		; last modtime
	((memq ?u switches) 4)))	; last access

(defun ls-lisp-time-to-seconds (time)
  "Convert TIME to a floating point number."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (nth 2 time) 0) 1000000.0)))

(defun ls-lisp-format-time (file-attr time-index now)
  "Format time for file with attributes FILE-ATTR according to TIME-INDEX.
Use the same method as ls to decide whether to show time-of-day or year,
depending on distance between file date and NOW.
All ls time options, namely c, t and u, are handled."
  (let* ((time (nth (or time-index 5) file-attr)) ; default is last modtime
	 (diff (- (ls-lisp-time-to-seconds time)
		  (ls-lisp-time-to-seconds now)))
	 ;; Consider a time to be recent if it is within the past six
	 ;; months.  A Gregorian year has 365.2425 * 24 * 60 * 60 ==
	 ;; 31556952 seconds on the average, and half of that is 15778476.
	 ;; Write the constant explicitly to avoid roundoff error.
	 (past-cutoff -15778476)) ; half a Gregorian year
    (condition-case nil
	;; Use traditional time format in the C or POSIX locale,
	;; ISO-style time format otherwise, so columns line up.
	(let ((locale system-time-locale))
	  (if (not locale)
	      (let ((vars '("LC_ALL" "LC_TIME" "LANG")))
		(while (and vars (not (setq locale (getenv (car vars)))))
		  (setq vars (cdr vars)))))
	  (if (member locale '("C" "POSIX"))
	      (setq locale nil))
	  (format-time-string
	   (if (and (<= past-cutoff diff) (<= diff 0))
	       (if (and locale (not ls-lisp-use-localized-time-format))
		   "%m-%d %H:%M"
		 (nth 0 ls-lisp-format-time-list))
	     (if (and locale (not ls-lisp-use-localized-time-format))
		 "%Y-%m-%d "
	       (nth 1 ls-lisp-format-time-list)))
	   time))
      (error "Unk  0  0000"))))

(defun ls-lisp-format-file-size (file-size human-readable)
  (if (or (not human-readable)
          (< file-size 1024))
      (format (if (floatp file-size) " %9.0f" " %9d") file-size)
    (do ((file-size (/ file-size 1024.0) (/ file-size 1024.0))
         ;; kilo, mega, giga, tera, peta, exa
         (post-fixes (list "k" "M" "G" "T" "P" "E") (cdr post-fixes)))
        ((< file-size 1024) (format " %8.0f%s"  file-size (car post-fixes))))))

(provide 'ls-lisp)

;;; arch-tag: e55f399b-05ec-425c-a6d5-f5e349c35ab4
;;; ls-lisp.el ends here
