;;; lisp-mnt.el --- minor mode for Emacs Lisp maintainers

;; Copyright (C) 1992, 1994 Free Software Foundation, Inc.

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Maintainer: Eric S. Raymond <esr@snark.thyrsus.com>
;; Created: 14 Jul 1992
;; Version: $Id: lisp-mnt.el,v 1.10 1995/03/16 04:37:48 rms Exp kwzh $
;; Keywords: docs
;; X-Bogus-Bureaucratic-Cruft: Gruad will get you if you don't watch out!

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This minor mode adds some services to Emacs-Lisp editing mode.
;;
;; First, it knows about the header conventions for library packages.
;; One entry point supports generating synopses from a library directory.
;; Another can be used to check for missing headers in library files.
;; 
;; Another entry point automatically addresses bug mail to a package's
;; maintainer or author.

;; This file can be loaded by your lisp-mode-hook.  Have it (require 'lisp-mnt)

;; This file is an example of the header conventions.  Note the following
;; features:
;; 
;;    * Header line --- makes it possible to extract a one-line summary of
;; the package's uses automatically for use in library synopses, KWIC
;; indexes and the like.
;; 
;;    Format is three semicolons, followed by the filename, followed by
;; three dashes, followed by the summary.  All fields space-separated.
;; 
;;    * Author line --- contains the name and net address of at least
;; the principal author.
;; 
;;    If there are multiple authors, they should be listed on continuation
;; lines led by ;;<TAB>, like this:
;; 
;; ;; Author: Ashwin Ram <Ram-Ashwin@cs.yale.edu>
;; ;;	Dave Sill <de5@ornl.gov>
;; ;;	David Lawrence <tale@pawl.rpi.edu>
;; ;;	Noah Friedman <friedman@ai.mit.edu>
;; ;;	Joe Wells <jbw@maverick.uswest.com>
;; ;;	Dave Brennan <brennan@hal.com>
;; ;;	Eric Raymond <esr@snark.thyrsus.com>
;; 
;; This field may have some special values; notably "FSF", meaning
;; "Free Software Foundation".
;; 
;;    * Maintainer line --- should be a single name/address as in the Author
;; line, or an address only, or the string "FSF".  If there is no maintainer
;; line, the person(s) in the Author field are presumed to be it.  The example
;; in this file is mildly bogus because the maintainer line is redundant.
;;    The idea behind these two fields is to be able to write a lisp function
;; that does "send mail to the author" without having to mine the name out by
;; hand. Please be careful about surrounding the network address with <> if
;; there's also a name in the field.
;; 
;;    * Created line --- optional, gives the original creation date of the
;; file.  For historical interest, basically.
;; 
;;    * Version line --- intended to give the reader a clue if they're looking
;; at a different version of the file than the one they're accustomed to.  This
;; may be an RCS or SCCS header.
;; 
;;    * Adapted-By line --- this is for FSF's internal use.  The person named
;; in this field was the one responsible for installing and adapting the
;; package for the distribution.  (This file doesn't have one because the
;; author *is* one of the maintainers.)
;; 
;;    * Keywords line --- used by the finder code (now under construction)
;; for finding Emacs Lisp code related to a topic.
;;
;;    * X-Bogus-Bureaucratic-Cruft line --- this is a joke and an example
;; of a comment header.  Headers starting with `X-' should never be used
;; for any real purpose; this is the way to safely add random headers
;; without invoking the wrath of any program.
;;
;;    * Commentary line --- enables lisp code to find the developer's and
;; maintainers' explanations of the package internals.
;; 
;;    * Change log line --- optional, exists to terminate the commentary
;; section and start a change-log part, if one exists.
;; 
;;    * Code line --- exists so Lisp can know where commentary and/or
;; change-log sections end.
;; 
;;    * Footer line --- marks end-of-file so it can be distinguished from
;; an expanded formfeed or the results of truncation.

;;; Change Log:

;; Tue Jul 14 23:44:17 1992	ESR
;;	* Created.

;;; Code:

(require 'picture)		; provides move-to-column-force
(require 'emacsbug)

;; These functions all parse the headers of the current buffer

(defun lm-section-mark (hd &optional after)
  ;; Return the buffer location of a given section start marker
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-min))
      (if (re-search-forward (concat "^;;;;* " hd ":[ \t]*$") nil t)
	  (progn
	    (beginning-of-line)
	    (if after (forward-line 1))
	    (point))
	nil))))

(defun lm-code-mark ()
  ;; Return the buffer location of the code start marker
  (lm-section-mark "Code"))

(defun lm-header (hd)
  ;; Return the contents of a named header
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (re-search-forward
	   (concat "^;; " hd ": \\(.*\\)") (lm-code-mark) t)
	  (buffer-substring (match-beginning 1) (match-end 1))
	nil)))

(defun lm-header-multiline (hd)
  ;; Return the contents of a named header, with possible continuation lines.
  ;; Note -- the returned value is a list of strings, one per line.
  (save-excursion
    (goto-char (point-min))
    (let ((res (save-excursion (lm-header hd))))
      (if res
	  (progn
	    (forward-line 1)
	    (setq res (list res))
	    (while (looking-at "^;;\t\\(.*\\)")
	      (setq res (cons (buffer-substring
			       (match-beginning 1)
			       (match-end 1))
			      res))
	      (forward-line 1))
	    ))
      res)))

;; These give us smart access to the header fields and commentary

(defun lm-summary (&optional file)
  ;; Return the buffer's or FILE's one-line summary.
  (save-excursion
    (if file
	(find-file file))
    (goto-char (point-min))
    (prog1
      (if (looking-at "^;;; [^ ]+ --- \\(.*\\)")
	  (buffer-substring (match-beginning 1) (match-end 1)))
      (if file
	  (kill-buffer (current-buffer)))
      )))


(defun lm-crack-address (x)
  ;; Given a string containing a human and email address, parse it
  ;; into a cons pair (name . address).
  (cond ((string-match "\\(.+\\) [(<]\\(\\S-+@\\S-+\\)[>)]" x)
	 (cons (substring x (match-beginning 1) (match-end 1))
	       (substring x (match-beginning 2) (match-end 2))))
	((string-match "\\(\\S-+@\\S-+\\) [(<]\\(.*\\)[>)]" x)
	 (cons (substring x (match-beginning 2) (match-end 2))
	       (substring x (match-beginning 1) (match-end 1))))
	((string-match "\\S-+@\\S-+" x)
	 (cons nil x))
	(t
	 (cons x nil))))

(defun lm-authors (&optional file)
  ;; Return the buffer's or FILE's author list.  Each element of the
  ;; list is a cons; the car is a name-aming-humans, the cdr an email
  ;; address.
  (save-excursion
    (if file
	(find-file file))
    (let ((authorlist (lm-header-multiline "author")))
      (prog1
	 (mapcar 'lm-crack-address authorlist)
	  (if file
	      (kill-buffer (current-buffer)))
	))))

(defun lm-maintainer (&optional file)
  ;; Get a package's bug-report & maintenance address.  Parse it out of FILE,
  ;; or the current buffer if FILE is nil.
  ;; The return value is a (name . address) cons.
  (save-excursion
    (if file
	(find-file file))
    (prog1
	(let ((maint (lm-header "maintainer")))
	  (if maint
	      (lm-crack-address maint)
	    (car (lm-authors))))
      (if file
	  (kill-buffer (current-buffer)))
      )))

(defun lm-creation-date (&optional file)
  ;; Return a package's creation date, if any.  Parse it out of FILE,
  ;; or the current buffer if FILE is nil.
  (save-excursion
    (if file
	(find-file file))
    (prog1
	(lm-header "created")
      (if file
	  (kill-buffer (current-buffer)))
      )))


(defun lm-last-modified-date (&optional file)
  ;; Return a package's last-modified date, if you can find one.
  (save-excursion 
    (if file
	(find-file file))
    (prog1
	(if (progn
	      (goto-char (point-min))
	      (re-search-forward
	       "\\$Id: [^ ]+ [^ ]+ \\([^/]+\\)/\\([^/]+\\)/\\([^ ]+\\) "
	       (lm-code-mark) t))
	    (format "%s %s %s"
		    (buffer-substring (match-beginning 3) (match-end 3))
		    (nth (string-to-int 
			  (buffer-substring (match-beginning 2) (match-end 2)))
			 '("" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
			   "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
		    (buffer-substring (match-beginning 1) (match-end 1))
		    ))
      (if file
	  (kill-buffer (current-buffer)))
      )))

(defun lm-version (&optional file)
  ;; Return the package's version field.
  ;; If none, look for an RCS or SCCS header to crack it out of.
  (save-excursion 
    (if file
	(find-file file))
    (prog1
	(or
	 (lm-header "version")
	 (let ((header-max (lm-code-mark)))
	   (goto-char (point-min))
	   (cond
	    ;; Look for an RCS header
	    ((re-search-forward "\\$Id: [^ ]+ \\([^ ]+\\) " header-max t)
	     (buffer-substring (match-beginning 1) (match-end 1)))

	    ;; Look for an SCCS header
	    ((re-search-forward 
	      (concat
	       (regexp-quote "@(#)")
	       (regexp-quote (file-name-nondirectory (buffer-file-name)))
	       "\t\\([012345679.]*\\)")
	      header-max t)
	     (buffer-substring (match-beginning 1) (match-end 1)))

	    (t nil))))
      (if file
	  (kill-buffer (current-buffer)))
      )))

(defun lm-keywords (&optional file)
  ;; Return the header containing the package's topic keywords.
  ;; Parse them out of FILE, or the current buffer if FILE is nil.
  (save-excursion
    (if file
	(find-file file))
    (prog1
	(let ((keywords (lm-header "keywords")))
	  (and keywords (downcase keywords)))
      (if file
	  (kill-buffer (current-buffer)))
      )))

(defun lm-adapted-by (&optional file)
  ;; Return the name or code of the person who cleaned up this package
  ;; for distribution.  Parse it out of FILE, or the current buffer if
  ;; FILE is nil.
  (save-excursion
    (if file
	(find-file file))
    (prog1
	(lm-header "adapted-by")
      (if file
	  (kill-buffer (current-buffer)))
      )))

(defun lm-commentary (&optional file)
  ;; Return the commentary region of a file, as a string.
  (save-excursion
    (if file
	(find-file file))
    (prog1
	(let ((commentary (lm-section-mark "Commentary" t))
	      (change-log (lm-section-mark "Change Log"))
	      (code (lm-section-mark "Code")))
	  (and commentary
	      (if change-log
		  (buffer-substring commentary change-log)
		(buffer-substring commentary code)))
	  )
      (if file
	  (kill-buffer (current-buffer)))
      )))

;;; Verification and synopses

(defun insert-at-column (col &rest pieces)
   (if (> (current-column) col) (insert "\n"))
   (move-to-column-force col)
   (apply 'insert pieces))

(defconst lm-comment-column 16)

(defun lm-verify (&optional file showok)
  "Check that the current buffer (or FILE if given) is in proper format.
If FILE is a directory, recurse on its files and generate a report into
a temporary buffer."
  (if (and file (file-directory-p file))
      (progn
	(switch-to-buffer (get-buffer-create "*lm-verify*"))
	(erase-buffer)
	(mapcar
	 '(lambda (f)
	    (if (string-match ".*\\.el$" f)
		(let ((status (lm-verify f)))
		  (if status
		      (progn
			(insert f ":")
			(insert-at-column lm-comment-column status "\n"))
		    (and showok
			 (progn
			   (insert f ":")
			   (insert-at-column lm-comment-column "OK\n")))))))
	(directory-files file))
    )
  (save-excursion
    (if file
	(find-file file))
    (prog1
	(cond
	 ((not (lm-summary))
	  "Can't find a package summary")
	 ((not (lm-code-mark))
	  "Can't find a code section marker")
	 ((progn
	    (goto-char (point-max))
	    (forward-line -1)
	    (looking-at (concat ";;; " file "ends here")))
	  "Can't find a footer line")
	 )
      (if file
	  (kill-buffer (current-buffer)))
      ))))

(defun lm-synopsis (&optional file showall)
  "Generate a synopsis listing for the buffer or the given FILE if given.
If FILE is a directory, recurse on its files and generate a report into
a temporary buffer.  If SHOWALL is on, also generate a line for files
which do not include a recognizable synopsis."
  (if (and file (file-directory-p file))
      (progn
	(switch-to-buffer (get-buffer-create "*lm-verify*"))
	(erase-buffer)
	(mapcar
	 '(lambda (f)
	    (if (string-match ".*\\.el$" f)
		(let ((syn (lm-synopsis f)))
		  (if syn
		      (progn
			(insert f ":")
			(insert-at-column lm-comment-column syn "\n"))
		    (and showall
			 (progn
			   (insert f ":")
			   (insert-at-column lm-comment-column "NA\n")))))))
	 (directory-files file))
	)
    (save-excursion
      (if file
	  (find-file file))
      (prog1
	  (lm-summary)
	(if file
	    (kill-buffer (current-buffer)))
	))))

(defun lm-report-bug (topic)
  "Report a bug in the package currently being visited to its maintainer.
Prompts for bug subject.  Leaves you in a mail buffer."
  (interactive "sBug Subject: ")
  (let ((package (buffer-name))
	(addr (lm-maintainer))
	(version (lm-version)))
    (mail nil
	  (if addr
	      (concat (car addr) " <" (cdr addr) ">")
	    bug-gnu-emacs)
	  topic)
    (goto-char (point-max))
    (insert "\nIn "
	    package
	    (if version (concat " version " version) "")
	    "\n\n")
    (message
     (substitute-command-keys "Type \\[mail-send] to send bug report."))))

(provide 'lisp-mnt)

;;; lisp-mnt.el ends here
