;;; cc-mode.el --- major mode for editing C, C++, Objective-C, and Java code

;; Copyright (C) 1985,1987,1992-1999 Free Software Foundation, Inc.

;; Authors:    1998-1999 Barry A. Warsaw and Martin Stjernholm
;;             1992-1997 Barry A. Warsaw
;;             1987 Dave Detlefs and Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    a long, long, time ago. adapted from the original c-mode.el
;; Keywords:   c languages oop

(defconst c-version "5.26e"
  "CC Mode version number.")

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
;; Objective-C, Java, IDL and Pike code.  As of the latest Emacs and
;; XEmacs releases, it is the default package for editing these
;; languages.  This package is called "CC Mode", and should be spelled
;; exactly this way.

;; CC Mode supports K&R and ANSI C, ANSI C++, Objective-C, Java,
;; CORBA's IDL, and Pike with a consistent indentation model across
;; all modes.  This indentation model is intuitive and very flexible,
;; so that almost any desired style of indentation can be supported.
;; Installation, usage, and programming details are contained in an
;; accompanying texinfo manual.

;; CC Mode's immediate ancestors were, c++-mode.el, cplus-md.el, and
;; cplus-md1.el..

;; NOTE: This mode does not perform font-locking (a.k.a syntactic
;; coloring, keyword highlighting, etc.) for any of the supported
;; modes.  Typically this is done by a package called font-lock.el
;; which we do *not* maintain.  You should contact the Emacs or XEmacs
;; maintainers for questions about coloring or highlighting in any
;; language mode.

;; To submit bug reports, type "C-c C-b".  These will be sent to
;; bug-gnu-emacs@gnu.org (mirrored as the Usenet newsgroup
;; gnu.emacs.bug) as well as bug-cc-mode@gnu.org, which directly
;; contacts the CC Mode maintainers.  Questions can sent to
;; help-gnu-emacs@gnu.org (mirrored as gnu.emacs.help) and/or
;; bug-cc-mode@gnu.org.  The old CC Mode contact address,
;; cc-mode-help@python.org is currently still active, but its use is
;; discouraged.  Please use bug-cc-mode@gnu.org instead.  Please do
;; not send bugs or questions to our personal accounts; we reserve the
;; right to ignore such email!

;; Many, many thanks go out to all the folks on the beta test list.
;; Without their patience, testing, insight, code contributions, and
;; encouragement CC Mode would be a far inferior package.

;; You can get the latest version of CC Mode, including PostScript
;; documentation and separate individual files from:
;;
;;     http://www.python.org/emacs/cc-mode/
;;
;; You can join a moderated CC Mode announcement-only mailing list by
;; visiting
;;
;;    http://www.python.org/mailman/listinfo/cc-mode-announce

;;; Code:

(eval-when-compile
  (let ((load-path
	 ;; Try to make sure the source directory is at the front of
	 ;; load-path when we load cc-defs.
	 (if (and (boundp 'byte-compile-current-file)
		  (stringp byte-compile-current-file))
	     ;; byte-compile-current-file is set by the byte compiler
	     ;; to the full path to this file.
	     (cons (file-name-directory byte-compile-current-file)
		   load-path)
	   load-path)))
    ;; Load our version of cc-defs unconditionally, since an older
    ;; version might very well be dumped in or already loaded.  This
    ;; way we ensure that the code is compiled with the correct macros
    ;; and defsubsts.  The same problem affects the subpackages that's
    ;; require'd below, but that doesn't harm the compiler; it can
    ;; only cause some bogus warnings.
    (load "cc-defs" nil t)))

(require 'cc-defs) ; Not meaningless; this passes on require's from cc-defs.
(require 'cc-menus)
(require 'cc-vars)
(require 'cc-styles)
(require 'cc-langs)
(require 'cc-engine)
(require 'cc-align)
(require 'cc-cmds)


;; Other modes and packages which depend on CC Mode should do the
;; following to make sure everything is loaded and available for their
;; use:
;;
;; (require 'cc-mode)
;; (c-initialize-cc-mode)

;;;###autoload
(defun c-initialize-cc-mode ()
  (setq c-buffer-is-cc-mode t)
  (let ((initprop 'cc-mode-is-initialized)
	c-initialization-ok)
    (unless (get 'c-initialize-cc-mode initprop)
      (unwind-protect
	  (progn
	    (put 'c-initialize-cc-mode initprop t)
	    (c-initialize-builtin-style)
	    (run-hooks 'c-initialization-hook)
	    ;; Fix obsolete variables.
	    (if (boundp 'c-comment-continuation-stars)
		(setq c-block-comment-prefix c-comment-continuation-stars))
	    (setq c-initialization-ok t))
	;; Will try initialization hooks again if they failed.
	(put 'c-initialize-cc-mode initprop c-initialization-ok)))
    ))


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
	c-conditional-key c-C-conditional-key
	c-class-key c-C-class-key
	c-baseclass-key nil
	c-comment-start-regexp c-C-comment-start-regexp
	c-bitfield-key c-C-bitfield-key
	)
  (cc-imenu-init cc-imenu-c-generic-expression)
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
	c-conditional-key c-C++-conditional-key
	c-comment-start-regexp c-C++-comment-start-regexp
	c-class-key c-C++-class-key
	c-extra-toplevel-key c-C++-extra-toplevel-key
	c-access-key c-C++-access-key
	c-recognize-knr-p nil
	c-bitfield-key c-C-bitfield-key
	)
  (cc-imenu-init cc-imenu-c++-generic-expression)
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
	c-conditional-key c-ObjC-conditional-key
	c-comment-start-regexp c-ObjC-comment-start-regexp
 	c-class-key c-ObjC-class-key
	c-baseclass-key nil
	c-access-key c-ObjC-access-key
	c-method-key c-ObjC-method-key
	)
  (cc-imenu-init cc-imenu-objc-generic-expression)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'objc-mode-hook)
  (c-update-modeline))


;;;###autoload
(defun java-mode ()
  "Major mode for editing Java code.
To submit a problem report, enter `\\[c-submit-bug-report]' from a
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
	paragraph-start (concat paragraph-start
				"\\("
				c-Java-javadoc-paragraph-start
				"\\|$\\)")
 	c-conditional-key c-Java-conditional-key
 	c-comment-start-regexp c-Java-comment-start-regexp
  	c-class-key c-Java-class-key
	c-method-key nil
 	c-baseclass-key nil
	c-recognize-knr-p nil
 	c-access-key c-Java-access-key
	c-inexpr-class-key c-Java-inexpr-class-key
	;defun-prompt-regexp c-Java-defun-prompt-regexp
	)
  (cc-imenu-init cc-imenu-java-generic-expression)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'java-mode-hook)
  (c-update-modeline))


;;;###autoload
(defun idl-mode ()
  "Major mode for editing CORBA's IDL code.
To submit a problem report, enter `\\[c-submit-bug-report]' from an
idl-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case, and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `idl-mode-hook' is run with no args, if that
variable is bound and has a non-nil value.  Also the hook
`c-mode-common-hook' is run first.

Key bindings:
\\{idl-mode-map}"
  (interactive)
  (c-initialize-cc-mode)
  (kill-all-local-variables)
  (set-syntax-table idl-mode-syntax-table)
  (setq major-mode 'idl-mode
	mode-name "IDL"
	local-abbrev-table idl-mode-abbrev-table)
  (use-local-map idl-mode-map)
  (c-common-init)
  (setq comment-start "// "
	comment-end ""
	c-conditional-key c-IDL-conditional-key
	c-comment-start-regexp c-IDL-comment-start-regexp
	c-class-key c-IDL-class-key
	c-method-key nil
	c-baseclass-key nil
	c-extra-toplevel-key c-IDL-extra-toplevel-key
	c-access-key c-IDL-access-key
	c-recognize-knr-p nil
	)
  ;;(cc-imenu-init cc-imenu-idl-generic-expression) ;FIXME
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'idl-mode-hook)
  (c-update-modeline))


;;;###autoload
(defun pike-mode ()
  "Major mode for editing Pike code.
To submit a problem report, enter `\\[c-submit-bug-report]' from an
idl-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case, and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `pike-mode-hook' is run with no args, if that value
is bound and has a non-nil value.  Also the common hook
`c-mode-common-hook' is run first.

Key bindings:
\\{pike-mode-map}"
  (interactive)
  (c-initialize-cc-mode)
  (kill-all-local-variables)
  (set-syntax-table pike-mode-syntax-table)
  (setq major-mode 'pike-mode
 	mode-name "Pike"
 	local-abbrev-table pike-mode-abbrev-table)
  (use-local-map pike-mode-map)
  (c-common-init)
  (setq comment-start "// "
 	comment-end   ""
 	c-conditional-key c-Pike-conditional-key
	c-comment-start-regexp c-Pike-comment-start-regexp
  	c-class-key c-Pike-class-key
	c-method-key nil
 	c-baseclass-key nil
	c-recognize-knr-p nil
 	c-access-key c-Pike-access-key
	c-lambda-key c-Pike-lambda-key
	c-inexpr-block-key c-Pike-inexpr-block-key
	c-special-brace-lists c-Pike-special-brace-lists
	)
  ;;(cc-imenu-init cc-imenu-pike-generic-expression) ;FIXME
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'pike-mode-hook)
  (c-update-modeline))


(defun c-setup-filladapt ()
  "Convenience function to configure Kyle E. Jones' Filladapt mode for
CC Mode by making sure the proper entries are present on
`filladapt-token-table', `filladapt-token-match-table', and
`filladapt-token-conversion-table'.  This is intended to be used on
`c-mode-common-hook' or similar."
  ;; This function is intended to be used explicitly by the end user
  ;; only.
  ;;
  ;; The default configuration already handles C++ comments, but we
  ;; need to add handling of C block comments.  A new filladapt token
  ;; `c-comment' is added for that.
  (let (p)
    (setq p filladapt-token-table)
    (while (and p (not (eq (car-safe (cdr-safe (car-safe p))) 'c-comment)))
      (setq p (cdr-safe p)))
    (if p
	(setcar (car p) c-comment-prefix-regexp)
      (setq filladapt-token-table
	    (append (list (car filladapt-token-table)
			  (list c-comment-prefix-regexp 'c-comment))
		    (cdr filladapt-token-table)))))
  (unless (assq 'c-comment filladapt-token-match-table)
    (setq filladapt-token-match-table
	  (append '((c-comment c-comment))
		  filladapt-token-match-table)))
  (unless (assq 'c-comment filladapt-token-conversion-table)
    (setq filladapt-token-conversion-table
	  (append '((c-comment . exact))
		  filladapt-token-conversion-table))))


;; bug reporting

(defconst c-mode-help-address
  "bug-gnu-emacs@gnu.org, bug-cc-mode@gnu.org"
  "Addresses for CC Mode bug reports.")

(defun c-version ()
  "Echo the current version of CC Mode in the minibuffer."
  (interactive)
  (message "Using CC Mode version %s" c-version)
  (c-keep-region-active))

(defun c-submit-bug-report ()
  "Submit via mail a bug report on CC Mode."
  (interactive)
  (require 'reporter)
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
		    ((eq major-mode 'idl-mode)  "IDL")
		    ((eq major-mode 'pike-mode) "Pike")
		    )
	      ")")
      (let ((vars (append
		   ;; report only the vars that affect indentation
		   c-style-variables
		   '(c-delete-function
		     c-electric-pound-behavior
		     c-indent-comments-syntactically-p
		     c-tab-always-indent
		     defun-prompt-regexp
		     tab-width
		     comment-column
		     parse-sexp-ignore-comments
		     ;; A brain-damaged XEmacs only variable that, if
		     ;; set to nil can cause all kinds of chaos.
		     signal-error-on-buffer-boundary
		     ;; Variables that affect line breaking and comments.
		     auto-fill-mode
		     filladapt-mode
		     comment-multi-line
		     comment-start-skip
		     fill-prefix
		     paragraph-start
		     adaptive-fill-mode
		     adaptive-fill-regexp)
		   nil)))
	(delq 'c-special-indent-hook vars)
	(unless (boundp 'defun-prompt-regexp)
	  (delq 'defun-prompt-regexp vars))
	(unless (boundp 'filladapt-mode)
	  (delq 'filladapt-mode vars))
	vars)
      (function
       (lambda ()
	 (insert
	  "Buffer Style: " style "\n\n"
	  (if (and hook
		   (or (/= (length hook) 1)
		       (not (eq (car hook) 'c-gnu-impose-minimum))
		       ))
	      (concat "\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n"
		      "c-special-indent-hook is set to '"
		      (format "%s" hook)
		      ".\nPerhaps this is your problem?\n"
		      "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\n")
	    "\n")
	  (format "c-emacs-features: %s\n" c-features)
	  )))
      nil
      "Dear Barry and Martin,"
      ))))


(provide 'cc-mode)
;;; cc-mode.el ends here
