;;; shadow.el --- locate Emacs Lisp file shadowings

;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Terry Jones <terry@santafe.edu>
;; Keywords: lisp
;; Created: 15 December 1995

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

;; The functions in this file detect (`find-emacs-lisp-shadows')
;; and display (`list-load-path-shadows') potential load-path
;; problems that arise when Emacs Lisp files "shadow" each other.
;;
;; For example, a file XXX.el early in one's load-path will shadow
;; a file with the same name in a later load-path directory.  When
;; this is unintentional, it may result in problems that could have
;; been easily avoided.  This occurs often (to me) when installing a
;; new version of emacs and something in the site-lisp directory
;; has been updated and added to the emacs distribution.  The old
;; version, now outdated, shadows the new one. This is obviously
;; undesirable.
;;
;; The `list-load-path-shadows' function was run when you installed
;; this version of emacs. To run it by hand in emacs:
;;
;;     M-x load-library RET shadow RET
;;     M-x list-load-path-shadows
;;
;; or run it non-interactively via:
;;
;;     emacs -batch -l shadow.el -f list-load-path-shadows
;;
;; Thanks to Francesco Potorti` <pot@cnuce.cnr.it> for suggestions,
;; rewritings & speedups.

;;; Code:

(defgroup shadow nil
  "Locate Emacs Lisp file shadowings."
  :prefix "shadows-"
  :group 'lisp)

(defcustom shadows-compare-text-p nil
  "*If non-nil, then shadowing files are reported only if their text differs.
This is slower, but filters out some innocuous shadowing."
  :type 'boolean
  :group 'shadow)

(defun find-emacs-lisp-shadows (&optional path)
  "Return a list of Emacs Lisp files that create shadows.
This function does the work for `list-load-path-shadows'.

We traverse PATH looking for shadows, and return a \(possibly empty\)
even-length list of files.  A file in this list at position 2i shadows
the file in position 2i+1.  Emacs Lisp file suffixes \(.el and .elc\)
are stripped from the file names in the list.

See the documentation for `list-load-path-shadows' for further information."
  
  (or path (setq path load-path))

  (let (true-names			; List of dirs considered.
	shadows				; List of shadowings, to be returned.
	files				; File names ever seen, with dirs.
	dir				; The dir being currently scanned.
	curr-files			; This dir's Emacs Lisp files.
	orig-dir			; Where the file was first seen.
	files-seen-this-dir		; Files seen so far in this dir.
	file)				; The current file.

  
    (while path

      (setq dir (directory-file-name (file-truename (or (car path) "."))))
      (if (member dir true-names)
	  ;; We have already considered this PATH redundant directory.
	  ;; Show the redundancy if we are interactiver, unless the PATH
	  ;; dir is nil or "." (these redundant directories are just a
	  ;; result of the current working directory, and are therefore
	  ;; not always redundant).
	  (or noninteractive
	      (and (car path)
		   (not (string= (car path) "."))
		   (message "Ignoring redundant directory %s" (car path))))

	(setq true-names (append true-names (list dir)))
	(setq dir (directory-file-name (or (car path) ".")))
	(setq curr-files (if (file-accessible-directory-p dir)
			     (directory-files dir nil ".\\.elc?$" t)))
	(and curr-files
	     (not noninteractive)
	     (message "Checking %d files in %s..." (length curr-files) dir))

	(setq files-seen-this-dir nil)

	(while curr-files

	  (setq file (car curr-files))
	  (setq file (substring
		      file 0 (if (string= (substring file -1) "c") -4 -3)))

	  ;; FILE now contains the current file name, with no suffix.
	  (unless (or (member file files-seen-this-dir)
		      ;; Ignore these files.
		      (member file '("subdirs")))
	    ;; File has not been seen yet in this directory.
	    ;; This test prevents us declaring that XXX.el shadows
	    ;; XXX.elc (or vice-versa) when they are in the same directory.
	    (setq files-seen-this-dir (cons file files-seen-this-dir))
	      
	    (if (setq orig-dir (assoc file files))
		;; This file was seen before, we have a shadowing.
		;; Report it unless the files are identical.
		(let ((base1 (concat (cdr orig-dir) "/" file))
		      (base2 (concat dir "/" file)))
		  (if (not (and shadows-compare-text-p
				(shadow-same-file-or-nonexistent
				 (concat base1 ".el") (concat base2 ".el"))
				;; This is a bit strict, but safe.
				(shadow-same-file-or-nonexistent
				 (concat base1 ".elc") (concat base2 ".elc"))))
		      (setq shadows
			    (append shadows (list base1 base2)))))

	      ;; Not seen before, add it to the list of seen files.
	      (setq files (cons (cons file dir) files))))

	  (setq curr-files (cdr curr-files))))
	(setq path (cdr path)))

    ;; Return the list of shadowings.
    shadows))

;; Return true if neither file exists, or if both exist and have identical
;; contents.
(defun shadow-same-file-or-nonexistent (f1 f2)
  (let ((exists1 (file-exists-p f1))
	(exists2 (file-exists-p f2)))
    (or (and (not exists1) (not exists2))
	(and exists1 exists2
	     (or (equal (file-truename f1) (file-truename f2))
		 ;; As a quick test, avoiding spawning a process, compare file
		 ;; sizes.
		 (and (= (nth 7 (file-attributes f1))
			 (nth 7 (file-attributes f2)))
		      (zerop (call-process "cmp" nil nil nil "-s" f1 f2))))))))

;;;###autoload
(defun list-load-path-shadows ()
  "Display a list of Emacs Lisp files that shadow other files.

This function lists potential load-path problems.  Directories in the
`load-path' variable are searched, in order, for Emacs Lisp
files.  When a previously encountered file name is found again, a
message is displayed indicating that the later file is \"hidden\" by
the earlier.

For example, suppose `load-path' is set to

\(\"/usr/gnu/emacs/site-lisp\" \"/usr/gnu/emacs/share/emacs/19.30/lisp\"\)

and that each of these directories contains a file called XXX.el.  Then
XXX.el in the site-lisp directory is referred to by all of:
\(require 'XXX\), \(autoload .... \"XXX\"\), \(load-library \"XXX\"\) etc.

The first XXX.el file prevents emacs from seeing the second \(unless
the second is loaded explicitly via load-file\).

When not intended, such shadowings can be the source of subtle
problems.  For example, the above situation may have arisen because the
XXX package was not distributed with versions of emacs prior to
19.30.  An emacs maintainer downloaded XXX from elsewhere and installed
it.  Later, XXX was updated and included in the emacs distribution.
Unless the emacs maintainer checks for this, the new version of XXX
will be hidden behind the old \(which may no longer work with the new
emacs version\).

This function performs these checks and flags all possible
shadowings.  Because a .el file may exist without a corresponding .elc
\(or vice-versa\), these suffixes are essentially ignored.  A file
XXX.elc in an early directory \(that does not contain XXX.el\) is
considered to shadow a later file XXX.el, and vice-versa.

When run interactively, the shadowings \(if any\) are displayed in a
buffer called `*Shadows*'.  Shadowings are located by calling the
\(non-interactive\) companion function, `find-emacs-lisp-shadows'."
  
  (interactive)
  (let* ((path (copy-sequence load-path))
	(tem path)
	toplevs)
    ;; If we can find simple.el in two places,
    (while tem
      (if (file-exists-p (expand-file-name "simple.el" (car tem)))
	  (setq toplevs (cons (car tem) toplevs)))
      (setq tem (cdr tem)))
    (if (> (length toplevs) 1)
	;; Cut off our copy of load-path right before
	;; the last directory which has simple.el in it.
	;; This avoids loads of duplications between the source dir
	;; and the dir where these files were copied by installation.
	(let ((break (car toplevs)))
	  (setq tem path)
	  (while tem
	    (if (eq (nth 1 tem) break)
		(progn
		  (setcdr tem nil)
		  (setq tem nil)))
	    (setq tem (cdr tem)))))

    (let* ((shadows (find-emacs-lisp-shadows path))
	   (n (/ (length shadows) 2))
	   (msg (format "%s Emacs Lisp load-path shadowing%s found"
			(if (zerop n) "No" (concat "\n" (number-to-string n)))
			(if (= n 1) " was" "s were"))))
      (if (interactive-p)
	  (save-excursion
	    ;; We are interactive.
	    ;; Create the *Shadows* buffer and display shadowings there.
	    (let ((output-buffer (get-buffer-create "*Shadows*")))
	      (display-buffer output-buffer)
	      (set-buffer output-buffer)
	      (erase-buffer)
	      (while shadows
		(insert (format "%s hides %s\n" (car shadows)
				(car (cdr shadows))))
		(setq shadows (cdr (cdr shadows))))
	      (insert msg "\n")))
	;; We are non-interactive, print shadows via message.
	(when shadows
	  (message "This site has duplicate Lisp libraries with the same name.
If a locally-installed Lisp library overrides a library in the Emacs release,
that can cause trouble, and you should probably remove the locally-installed
version unless you know what you are doing.\n")
	  (while shadows
	    (message "%s hides %s" (car shadows) (car (cdr shadows)))
	    (setq shadows (cdr (cdr shadows))))
	  (message "%s" msg))))))

(provide 'shadow)

;;; shadow.el ends here
