;;; cc-mode.el --- major mode for editing C, C++, Objective-C, and Java code

;; Copyright (C) 1985,87,92,93,94,95,96,97 Free Software Foundation, Inc.

;; Authors:    1992-1997 Barry A. Warsaw
;;             1987 Dave Detlefs and Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: cc-mode-help@python.org
;; Created:    a long, long, time ago. adapted from the original c-mode.el
;; Version:    5.13
;; Keywords:   c languages oop

;; NOTE: Read the commentary below for the right way to submit bug reports!
;; NOTE: See the accompanying texinfo manual for details on using this mode!

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

;; This package provides GNU Emacs major modes for editing C, C++,
;; Objective-C, and Java code.  As of the latest Emacs and XEmacs
;; releases, it is the default package for editing these languages.
;; This package is called "CC Mode", and should be spelled exactly
;; this way.  It supports K&R and ANSI C, ANSI C++, Objective-C, and
;; Java, with a consistent indentation model across all modes.  This
;; indentation model is intuitive and very flexible, so that almost
;; any desired style of indentation can be supported.  Installation,
;; usage, and programming details are contained in an accompanying
;; texinfo manual.

;; CC Mode's immediate ancestors were, c++-mode.el, cplus-md.el, and
;; cplus-md1.el..

;; NOTE: This mode does not perform font-locking (a.k.a syntactic
;; coloring, keyword highlighting, etc.) for any of the supported
;; modes.  Typically this is done by a package called font-lock.el
;; which I do *not* maintain.  You should contact the Emacs
;; maintainers for questions about coloring or highlighting in any
;; language mode.

;; To submit bug reports, type "C-c C-b".  These will be sent to
;; bug-gnu-emacs@prep.ai.mit.edu as well as cc-mode-help@python.org,
;; and I'll read about them there (the former is mirrored as the
;; Usenet newsgroup gnu.emacs.bug).  Questions can sent to
;; help-gnu-emacs@prep.ai.mit.edu (mirrored as gnu.emacs.help) and/or
;; cc-mode-help@python.org.  Please do not send bugs or questions to
;; my personal account.

;; YOU CAN IGNORE ALL BYTE-COMPILER WARNINGS. They are the result of
;; the cross-Emacsen support.  GNU Emacs 19 (from the FSF), GNU XEmacs
;; 19 (formerly Lucid Emacs), and GNU Emacs 18 all do things
;; differently and there's no way to shut the byte-compiler up at the
;; necessary granularity.  Let me say this again: YOU CAN IGNORE ALL
;; BYTE-COMPILER WARNINGS (you'd be surprised at how many people don't
;; follow this advice :-).

;; Many, many thanks go out to all the folks on the beta test list.
;; Without their patience, testing, insight, code contributions, and
;; encouragement CC Mode would be a far inferior package.

;; You can get the latest version of CC Mode, including PostScript
;; documentation and separate individual files from:
;;
;;     http://www.python.org/ftp/emacs/

;; Or if you don't have access to the World Wide Web, through
;; anonymous ftp from:
;;
;;    ftp://ftp.python.org/pub/emacs

;;; Code:

(eval-when-compile
  (require 'cc-menus))
(require 'cc-defs)


;; Other modes and packages which depend on CC Mode should do the
;; following to make sure everything is loaded and available for their
;; use:
;;
;; (require 'cc-mode)
;; (c-initialize-cc-mode)

(defun c-initialize-cc-mode ()
  ;; make sure all necessary components of CC Mode are loaded in.
  (require 'cc-vars)
  (require 'cc-engine)
  (require 'cc-langs)
  (require 'cc-menus)
  (require 'cc-align)
  (require 'cc-styles)
  (require 'cc-cmds))



;;;###autoload
(defun c-mode ()
  "Major mode for editing K&R and ANSI C code.
To submit a problem report, enter `\\[c-submit-bug-report]' from a
c-mode buffer.  This automatically sets up a mail buffer with version
information already added.  You just need to add a description of the
problem, including a reproducible test case and send the message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `c-mode-hook' is run with no args, if that value is
bound and has a non-nil value.  Also the hook `c-mode-common-hook' is
run first.

Key bindings:
\\{c-mode-map}"
  (interactive)
  (c-initialize-cc-mode)
  (kill-all-local-variables)
  (set-syntax-table c-mode-syntax-table)
  (setq major-mode 'c-mode
	mode-name "C"
	local-abbrev-table c-mode-abbrev-table)
  (use-local-map c-mode-map)
  (c-common-init)
  (setq comment-start "/* "
	comment-end   " */"
	comment-multi-line t
	c-conditional-key c-C-conditional-key
	c-class-key c-C-class-key
	c-baseclass-key nil
	c-comment-start-regexp c-C-comment-start-regexp
	imenu-generic-expression cc-imenu-c-generic-expression)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'c-mode-hook)
  (c-update-modeline))


;;;###autoload
(defun c++-mode ()
  "Major mode for editing C++ code.
To submit a problem report, enter `\\[c-submit-bug-report]' from a
c++-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case, and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `c++-mode-hook' is run with no args, if that
variable is bound and has a non-nil value.  Also the hook
`c-mode-common-hook' is run first.

Key bindings:
\\{c++-mode-map}"
  (interactive)
  (c-initialize-cc-mode)
  (kill-all-local-variables)
  (set-syntax-table c++-mode-syntax-table)
  (setq major-mode 'c++-mode
	mode-name "C++"
	local-abbrev-table c++-mode-abbrev-table)
  (use-local-map c++-mode-map)
  (c-common-init)
  (setq comment-start "// "
	comment-end ""
	comment-multi-line nil
	c-conditional-key c-C++-conditional-key
	c-comment-start-regexp c-C++-comment-start-regexp
	c-class-key c-C++-class-key
	c-access-key c-C++-access-key
	c-double-slash-is-comments-p t
	c-recognize-knr-p nil
	imenu-generic-expression cc-imenu-c++-generic-expression)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'c++-mode-hook)
  (c-update-modeline))


;;;###autoload
(defun objc-mode ()
  "Major mode for editing Objective C code.
To submit a problem report, enter `\\[c-submit-bug-report]' from an
objc-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case, and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `objc-mode-hook' is run with no args, if that value
is bound and has a non-nil value.  Also the hook `c-mode-common-hook'
is run first.

Key bindings:
\\{objc-mode-map}"
  (interactive)
  (c-initialize-cc-mode)
  (kill-all-local-variables)
  (set-syntax-table objc-mode-syntax-table)
  (setq major-mode 'objc-mode
	mode-name "ObjC"
	local-abbrev-table objc-mode-abbrev-table)
  (use-local-map objc-mode-map)
  (c-common-init)
  (setq comment-start "// "
	comment-end   ""
	comment-multi-line nil
	c-conditional-key c-C-conditional-key
	c-comment-start-regexp c-C++-comment-start-regexp
 	c-class-key c-ObjC-class-key
	c-baseclass-key nil
	c-access-key c-ObjC-access-key
	c-double-slash-is-comments-p t
	c-method-key c-ObjC-method-key)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'objc-mode-hook)
  (c-update-modeline))


;;;###autoload
(defun java-mode ()
  "Major mode for editing Java code.
To submit a problem report, enter `\\[c-submit-bug-report]' from an
java-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `java-mode-hook' is run with no args, if that value
is bound and has a non-nil value.  Also the common hook
`c-mode-common-hook' is run first.  Note that this mode automatically
sets the \"java\" style before calling any hooks so be careful if you
set styles in `c-mode-common-hook'.

Key bindings:
\\{java-mode-map}"
  (interactive)
  (c-initialize-cc-mode)
  (kill-all-local-variables)
  (set-syntax-table java-mode-syntax-table)
  (setq major-mode 'java-mode
 	mode-name "Java"
 	local-abbrev-table java-mode-abbrev-table)
  (use-local-map java-mode-map)
  (c-common-init)
  (setq comment-start "// "
 	comment-end   ""
 	comment-multi-line nil
 	c-conditional-key c-Java-conditional-key
 	c-comment-start-regexp c-Java-comment-start-regexp
  	c-class-key c-Java-class-key
	c-method-key c-Java-method-key
	c-double-slash-is-comments-p t
 	c-baseclass-key nil
	c-recognize-knr-p nil
 	c-access-key c-Java-access-key
	;defun-prompt-regexp c-Java-defun-prompt-regexp
	imenu-generic-expression cc-imenu-java-generic-expression
	)
  (c-set-style "java")
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'java-mode-hook)
  (c-update-modeline))


;; defuns for submitting bug reports
(defconst c-version "5.13"
  "CC Mode version number.")

(defconst c-mode-help-address
  "bug-gnu-emacs@prep.ai.mit.edu, cc-mode-help@python.org"
  "Address for CC Mode bug reports.")

(defun c-version ()
  "Echo the current version of CC Mode in the minibuffer."
  (interactive)
  (message "Using CC Mode version %s" c-version)
  (c-keep-region-active))

;; Get reporter-submit-bug-report when byte-compiling
(eval-when-compile
  (require 'reporter))

(defun c-submit-bug-report ()
  "Submit via mail a bug report on CC Mode."
  (interactive)
  (require 'cc-vars)
  ;; load in reporter
  (let ((reporter-prompt-for-summary-p t)
	(reporter-dont-compact-list '(c-offsets-alist))
	(style c-indentation-style)
	(hook c-special-indent-hook)
	(c-features c-emacs-features))
    (and
     (if (y-or-n-p "Do you want to submit a report on CC Mode? ")
	 t (message "") nil)
     (require 'reporter)
     (reporter-submit-bug-report
      c-mode-help-address
      (concat "CC Mode " c-version " ("
	      (cond ((eq major-mode 'c++-mode)  "C++")
		    ((eq major-mode 'c-mode)    "C")
		    ((eq major-mode 'objc-mode) "ObjC")
		    ((eq major-mode 'java-mode) "Java")
		    )
	      ")")
      (let ((vars (list
		   ;; report only the vars that affect indentation
		   'c-basic-offset
		   'c-offsets-alist
		   'c-cleanup-list
		   'c-comment-only-line-offset
		   'c-backslash-column
		   'c-delete-function
		   'c-electric-pound-behavior
		   'c-hanging-braces-alist
		   'c-hanging-colons-alist
		   'c-hanging-comment-starter-p
		   'c-hanging-comment-ender-p
		   'c-indent-comments-syntactically-p
		   'c-tab-always-indent
		   'c-recognize-knr-p
		   'c-label-minimum-indentation
		   'defun-prompt-regexp
		   'tab-width
		   )))
	(if (not (boundp 'defun-prompt-regexp))
	    (delq 'defun-prompt-regexp vars)
	  vars))
      (function
       (lambda ()
	 (insert
	  "Buffer Style: " style "\n\n"
	  (if hook
	      (concat "\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n"
		      "c-special-indent-hook is set to '"
		      (format "%s" hook)
		      ".\nPerhaps this is your problem?\n"
		      "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\n")
	    "\n")
	  (format "c-emacs-features: %s\n" c-features)
	  )))
      nil
      "Dear Barry,"
      ))))


(provide 'cc-mode)
;;; cc-mode.el ends here
