;;; cc-mode.el --- major mode for editing C, C++, Objective-C, and Java code

;; Copyright (C) 1985,1987,1992-2001 Free Software Foundation, Inc.

;; Authors:    2000- Martin Stjernholm
;;	       1998-1999 Barry A. Warsaw and Martin Stjernholm
;;             1992-1997 Barry A. Warsaw
;;             1987 Dave Detlefs and Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    a long, long, time ago. adapted from the original c-mode.el
;; Keywords:   c languages oop

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(defconst c-version "5.29"
  "CC Mode version number.")

;; NOTE: Read the commentary below for the right way to submit bug reports!
;; NOTE: See the accompanying texinfo manual for details on using this mode!

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
;; bug-cc-mode@gnu.org.  Please do not send bugs or questions to our
;; personal accounts; we reserve the right to ignore such email!

;; Many, many thanks go out to all the folks on the beta test list.
;; Without their patience, testing, insight, code contributions, and
;; encouragement CC Mode would be a far inferior package.

;; You can get the latest version of CC Mode, including PostScript
;; documentation and separate individual files from:
;;
;;     http://cc-mode.sourceforge.net/
;;
;; You can join a moderated CC Mode announcement-only mailing list by
;; visiting
;;
;;    http://lists.sourceforge.net/mailman/listinfo/cc-mode-announce

;;; Code:

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (require 'cc-bytecomp)))

(cc-require 'cc-defs)
(cc-require 'cc-menus)
(cc-require 'cc-vars)
(cc-require 'cc-langs)
(cc-require 'cc-styles)
(cc-require 'cc-engine)
(cc-require 'cc-cmds)
(cc-require 'cc-align)

;; Silence the compiler.
(cc-bytecomp-defvar comment-line-break-function) ; (X)Emacs 20+
(cc-bytecomp-defvar adaptive-fill-first-line-regexp) ; Emacs 20+
(cc-bytecomp-defun set-keymap-parents)	; XEmacs

;; Menu support for both XEmacs and Emacs.  If you don't have easymenu
;; with your version of Emacs, you are incompatible!
(require 'easymenu)


;; Other modes and packages which depend on CC Mode should do the
;; following to make sure everything is loaded and available for their
;; use:
;;
;; (require 'cc-mode)
;;
;; And in the major mode function:
;;
;; (c-initialize-cc-mode)

(defun c-leave-cc-mode-mode ()
  (setq c-buffer-is-cc-mode nil))

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
	    (add-hook 'change-major-mode-hook 'c-leave-cc-mode-mode)
	    (setq c-initialization-ok t))
	;; Will try initialization hooks again if they failed.
	(put 'c-initialize-cc-mode initprop c-initialization-ok)))
    ))


;; Common routines
(defvar c-mode-base-map ()
  "Keymap shared by all CC Mode related modes.")

(defun c-make-inherited-keymap ()
  (let ((map (make-sparse-keymap)))
    (cond
     ;; XEmacs 19 & 20
     ((fboundp 'set-keymap-parents)
      (set-keymap-parents map c-mode-base-map))
     ;; Emacs 19
     ((fboundp 'set-keymap-parent)
      (set-keymap-parent map c-mode-base-map))
     ;; incompatible
     (t (error "CC Mode is incompatible with this version of Emacs")))
    map))

(defun c-define-abbrev-table (name defs)
  ;; Compatibility wrapper for `define-abbrev' which passes a non-nil
  ;; sixth argument for SYSTEM-FLAG in emacsen that support it
  ;; (currently only Emacs 21.2).
  (define-abbrev-table name nil)
  (let ((table (symbol-value name)))
    (while defs
      (condition-case nil
	  (apply 'define-abbrev table (append (car defs) '(t)))
	(wrong-number-of-arguments
	 (apply 'define-abbrev table (car defs))))
      (setq defs (cdr defs)))))

(if c-mode-base-map
    nil
  ;; TBD: should we even worry about naming this keymap. My vote: no,
  ;; because Emacs and XEmacs do it differently.
  (setq c-mode-base-map (make-sparse-keymap))
  ;; put standard keybindings into MAP
  ;; the following mappings correspond more or less directly to BOCM
  (define-key c-mode-base-map "{"         'c-electric-brace)
  (define-key c-mode-base-map "}"         'c-electric-brace)
  (define-key c-mode-base-map ";"         'c-electric-semi&comma)
  (define-key c-mode-base-map "#"         'c-electric-pound)
  (define-key c-mode-base-map ":"         'c-electric-colon)
  (define-key c-mode-base-map "("         'c-electric-paren)
  (define-key c-mode-base-map ")"         'c-electric-paren)
  ;; Separate M-BS from C-M-h.  The former should remain
  ;; backward-kill-word.
  (define-key c-mode-base-map [(control meta h)] 'c-mark-function)
  (define-key c-mode-base-map "\e\C-q"    'c-indent-exp)
  (substitute-key-definition 'backward-sentence
			     'c-beginning-of-statement
			     c-mode-base-map global-map)
  (substitute-key-definition 'forward-sentence
			     'c-end-of-statement
			     c-mode-base-map global-map)
  (substitute-key-definition 'indent-new-comment-line
			     'c-indent-new-comment-line
			     c-mode-base-map global-map)
  (when (fboundp 'comment-indent-new-line)
    ;; indent-new-comment-line has changed name to
    ;; comment-indent-new-line in Emacs 21.
    (substitute-key-definition 'comment-indent-new-line
			       'c-indent-new-comment-line
			       c-mode-base-map global-map))
  ;; RMS says don't make these the default.
;;  (define-key c-mode-base-map "\e\C-a"    'c-beginning-of-defun)
;;  (define-key c-mode-base-map "\e\C-e"    'c-end-of-defun)
  (define-key c-mode-base-map "\C-c\C-n"  'c-forward-conditional)
  (define-key c-mode-base-map "\C-c\C-p"  'c-backward-conditional)
  (define-key c-mode-base-map "\C-c\C-u"  'c-up-conditional)
  (substitute-key-definition 'indent-for-tab-command
			     'c-indent-command
			     c-mode-base-map global-map)
  ;; It doesn't suffice to put c-fill-paragraph on
  ;; fill-paragraph-function due to the way it works.
  (substitute-key-definition 'fill-paragraph 'c-fill-paragraph
			     c-mode-base-map global-map)
  ;; In XEmacs the default fill function is called
  ;; fill-paragraph-or-region.
  (substitute-key-definition 'fill-paragraph-or-region 'c-fill-paragraph
			     c-mode-base-map global-map)
  ;; Bind the electric deletion functions to C-d and DEL.  Emacs 21
  ;; automatically maps the [delete] and [backspace] keys to these two
  ;; depending on window system and user preferences.  (In earlier
  ;; versions it's possible to do the same by using `function-key-map'.)
  (define-key c-mode-base-map "\C-d" 'c-electric-delete-forward)
  (define-key c-mode-base-map "\177" 'c-electric-backspace)
  (when (boundp 'delete-key-deletes-forward)
    ;; In XEmacs 20 and later we fix the forward and backward deletion
    ;; behavior by binding the keysyms for the [delete] and
    ;; [backspace] keys directly, and use `delete-forward-p' or
    ;; `delete-key-deletes-forward' to decide what [delete] should do.
    (define-key c-mode-base-map [delete]    'c-electric-delete)
    (define-key c-mode-base-map [backspace] 'c-electric-backspace))
  ;; these are new keybindings, with no counterpart to BOCM
  (define-key c-mode-base-map ","         'c-electric-semi&comma)
  (define-key c-mode-base-map "*"         'c-electric-star)
  (define-key c-mode-base-map "/"         'c-electric-slash)
  (define-key c-mode-base-map "\C-c\C-q"  'c-indent-defun)
  (define-key c-mode-base-map "\C-c\C-\\" 'c-backslash-region)
  ;; TBD: where if anywhere, to put c-backward|forward-into-nomenclature
  (define-key c-mode-base-map "\C-c\C-a"  'c-toggle-auto-state)
  (define-key c-mode-base-map "\C-c\C-b"  'c-submit-bug-report)
  (define-key c-mode-base-map "\C-c\C-c"  'comment-region)
  (define-key c-mode-base-map "\C-c\C-d"  'c-toggle-hungry-state)
  (define-key c-mode-base-map "\C-c\C-o"  'c-set-offset)
  (define-key c-mode-base-map "\C-c\C-s"  'c-show-syntactic-information)
  (define-key c-mode-base-map "\C-c\C-t"  'c-toggle-auto-hungry-state)
  (define-key c-mode-base-map "\C-c."     'c-set-style)
  ;; conflicts with OOBR
  ;;(define-key c-mode-base-map "\C-c\C-v"  'c-version)
  )

(defvar c-c-menu nil)
(defvar c-c++-menu nil)
(defvar c-objc-menu nil)
(defvar c-java-menu nil)
(defvar c-pike-menu nil)

(defun c-mode-menu (modestr)
  (let ((m
	 '(["Comment Out Region"     comment-region (c-fn-region-is-active-p)]
	   ["Uncomment Region"
	    (comment-region (region-beginning) (region-end) '(4))
	    (c-fn-region-is-active-p)]
	   ["Fill Comment Paragraph" c-fill-paragraph t]
	   "----"
	   ["Indent Expression"      c-indent-exp
	    (memq (char-after) '(?\( ?\[ ?\{))]
	   ["Indent Line or Region"  c-indent-line-or-region t]
	   ["Up Conditional"         c-up-conditional t]
	   ["Backward Conditional"   c-backward-conditional t]
	   ["Forward Conditional"    c-forward-conditional t]
	   ["Backward Statement"     c-beginning-of-statement t]
	   ["Forward Statement"      c-end-of-statement t]
	   "----"
	   ["Macro Expand Region"    c-macro-expand (c-fn-region-is-active-p)]
	   ["Backslashify"	     c-backslash-region
	    (c-fn-region-is-active-p)]
	   "----"
	   ("Toggle..."
	    ["Syntactic indentation" c-toggle-syntactic-indentation t]
	    ["Auto newline"          c-toggle-auto-state t]
	    ["Hungry delete"         c-toggle-hungry-state t])
	   )))
    (cons modestr m)))

;; We don't require the outline package, but we configure it a bit anyway.
(cc-bytecomp-defvar outline-level)

(defun c-common-init (mode)
  ;; Common initializations for all modes.
  (setq c-buffer-is-cc-mode mode)

  ;; these variables should always be buffer local; they do not affect
  ;; indentation style.
  (make-local-variable 'require-final-newline)
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'outline-regexp)
  (make-local-variable 'outline-level)
  (make-local-variable 'normal-auto-fill-function)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-multi-line)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (make-local-variable 'adaptive-fill-mode)
  (make-local-variable 'adaptive-fill-regexp)
  (make-local-variable 'imenu-generic-expression) ;set in the mode functions

  ;; X/Emacs 20 only
  (and (boundp 'comment-line-break-function)
       (progn
	 (make-local-variable 'comment-line-break-function)
	 (setq comment-line-break-function
	       'c-indent-new-comment-line)))

  ;; now set their values
  (setq require-final-newline t
	parse-sexp-ignore-comments t
	indent-line-function 'c-indent-line
	indent-region-function 'c-indent-region
	outline-regexp "[^#\n\^M]"
	outline-level 'c-outline-level
	normal-auto-fill-function 'c-do-auto-fill
	comment-column 32
	comment-start-skip "/\\*+ *\\|//+ *"
	comment-multi-line t)

  ;; Fix keyword regexps.
  (c-init-language-vars)

  ;; now set the mode style based on c-default-style
  (let ((style (if (stringp c-default-style)
		   c-default-style
		 (or (cdr (assq mode c-default-style))
		     (cdr (assq 'other c-default-style))
		     "gnu"))))
    ;; Override style variables if `c-old-style-variable-behavior' is
    ;; set.  Also override if we are using global style variables,
    ;; have already initialized a style once, and are switching to a
    ;; different style.  (It's doubtful whether this is desirable, but
    ;; the whole situation with nonlocal style variables is a bit
    ;; awkward.  It's at least the most compatible way with the old
    ;; style init procedure.)
    (c-set-style style (not (or c-old-style-variable-behavior
				(and (not c-style-variables-are-local-p)
				     c-indentation-style
				     (not (string-equal c-indentation-style
							style)))))))
  (c-setup-paragraph-variables)

  ;; we have to do something special for c-offsets-alist so that the
  ;; buffer local value has its own alist structure.
  (setq c-offsets-alist (copy-alist c-offsets-alist))

  ;; setup the comment indent variable in a Emacs version portable way
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'c-comment-indent)

  ;; add menus to menubar
  (easy-menu-add (c-mode-menu mode-name))

  ;; put auto-hungry designators onto minor-mode-alist, but only once
  (or (assq 'c-auto-hungry-string minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(c-auto-hungry-string c-auto-hungry-string)
		  minor-mode-alist)))

  ;; Install the function that ensures `c-state-cache' doesn't become
  ;; invalid.
  (make-local-variable 'after-change-functions)
  (add-hook 'after-change-functions 'c-check-state-cache))

(defun c-postprocess-file-styles ()
  "Function that post processes relevant file local variables.
Currently, this function simply applies any style and offset settings
found in the file's Local Variable list.  It first applies any style
setting found in `c-file-style', then it applies any offset settings
it finds in `c-file-offsets'.

Note that the style variables are always made local to the buffer."
  ;; apply file styles and offsets
  (if (or c-file-style c-file-offsets)
      (c-make-styles-buffer-local t))
  (and c-file-style
       (c-set-style c-file-style))
  (and c-file-offsets
       (mapcar
	(lambda (langentry)
	  (let ((langelem (car langentry))
		(offset (cdr langentry)))
	    (c-set-offset langelem offset)))
	c-file-offsets)))

(add-hook 'hack-local-variables-hook 'c-postprocess-file-styles)


;; Support for C

(defvar c-mode-abbrev-table nil
  "Abbreviation table used in c-mode buffers.")
(c-define-abbrev-table 'c-mode-abbrev-table
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)))

(defvar c-mode-map ()
  "Keymap used in c-mode buffers.")
(if c-mode-map
    nil
  (setq c-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for C
  (define-key c-mode-map "\C-c\C-e"  'c-macro-expand)
  )

(easy-menu-define c-c-menu c-mode-map "C Mode Commands"
		  (c-mode-menu "C"))

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
  (kill-all-local-variables)
  (c-initialize-cc-mode)
  (set-syntax-table c-mode-syntax-table)
  (setq major-mode 'c-mode
	mode-name "C"
	local-abbrev-table c-mode-abbrev-table
	abbrev-mode t)
  (use-local-map c-mode-map)
  (c-common-init 'c-mode)
  (cc-imenu-init cc-imenu-c-generic-expression)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'c-mode-hook)
  (c-update-modeline))


;; Support for C++

(defvar c++-mode-abbrev-table nil
  "Abbreviation table used in c++-mode buffers.")
(c-define-abbrev-table 'c++-mode-abbrev-table
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)))

(defvar c++-mode-map ()
  "Keymap used in c++-mode buffers.")
(if c++-mode-map
    nil
  (setq c++-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for C++
  (define-key c++-mode-map "\C-c\C-e" 'c-macro-expand)
  (define-key c++-mode-map "\C-c:"    'c-scope-operator)
  (define-key c++-mode-map "<"        'c-electric-lt-gt)
  (define-key c++-mode-map ">"        'c-electric-lt-gt))

(easy-menu-define c-c++-menu c++-mode-map "C++ Mode Commands"
		  (c-mode-menu "C++"))

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
  (kill-all-local-variables)
  (c-initialize-cc-mode)
  (set-syntax-table c++-mode-syntax-table)
  (setq major-mode 'c++-mode
	mode-name "C++"
	local-abbrev-table c++-mode-abbrev-table
	abbrev-mode t)
  (use-local-map c++-mode-map)
  (c-common-init 'c++-mode)
  (cc-imenu-init cc-imenu-c++-generic-expression)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'c++-mode-hook)
  (c-update-modeline))


;; Support for Objective-C

(defvar objc-mode-abbrev-table nil
  "Abbreviation table used in objc-mode buffers.")
(c-define-abbrev-table 'objc-mode-abbrev-table
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)))

(defvar objc-mode-map ()
  "Keymap used in objc-mode buffers.")
(if objc-mode-map
    nil
  (setq objc-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for Objective-C
  (define-key objc-mode-map "\C-c\C-e" 'c-macro-expand))

(easy-menu-define c-objc-menu objc-mode-map "ObjC Mode Commands"
		  (c-mode-menu "ObjC"))

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
  (kill-all-local-variables)
  (c-initialize-cc-mode)
  (set-syntax-table objc-mode-syntax-table)
  (setq major-mode 'objc-mode
	mode-name "ObjC"
	local-abbrev-table objc-mode-abbrev-table
	abbrev-mode t)
  (use-local-map objc-mode-map)
  (c-common-init 'objc-mode)
  (cc-imenu-init cc-imenu-objc-generic-expression)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'objc-mode-hook)
  (c-update-modeline))


;; Support for Java

(defvar java-mode-abbrev-table nil
  "Abbreviation table used in java-mode buffers.")
(c-define-abbrev-table 'java-mode-abbrev-table
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)))

(defvar java-mode-map ()
  "Keymap used in java-mode buffers.")
(if java-mode-map
    nil
  (setq java-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for Java
  )

(easy-menu-define c-java-menu java-mode-map "Java Mode Commands"
		  (c-mode-menu "Java"))

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
  (kill-all-local-variables)
  (c-initialize-cc-mode)
  (set-syntax-table java-mode-syntax-table)
  (setq major-mode 'java-mode
 	mode-name "Java"
 	local-abbrev-table java-mode-abbrev-table
	abbrev-mode t)
  (use-local-map java-mode-map)
  (c-common-init 'java-mode)
  (cc-imenu-init cc-imenu-java-generic-expression)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'java-mode-hook)
  (c-update-modeline))


;; Support for CORBA's IDL language

(defvar idl-mode-abbrev-table nil
  "Abbreviation table used in idl-mode buffers.")
(c-define-abbrev-table 'idl-mode-abbrev-table nil)

(defvar idl-mode-map ()
  "Keymap used in idl-mode buffers.")
(if idl-mode-map
    nil
  (setq idl-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for IDL
  )

(easy-menu-define c-idl-menu idl-mode-map "IDL Mode Commands"
		  (c-mode-menu "IDL"))

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
  (kill-all-local-variables)
  (c-initialize-cc-mode)
  (set-syntax-table idl-mode-syntax-table)
  (setq major-mode 'idl-mode
	mode-name "IDL"
	local-abbrev-table idl-mode-abbrev-table)
  (use-local-map idl-mode-map)
  (c-common-init 'idl-mode)
  ;;(cc-imenu-init cc-imenu-idl-generic-expression) ;FIXME
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'idl-mode-hook)
  (c-update-modeline))


;; Support for Pike

(defvar pike-mode-abbrev-table nil
  "Abbreviation table used in pike-mode buffers.")
(c-define-abbrev-table 'pike-mode-abbrev-table
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)))

(defvar pike-mode-map ()
  "Keymap used in pike-mode buffers.")
(if pike-mode-map
    nil
  (setq pike-mode-map (c-make-inherited-keymap))
  ;; additional bindings
  (define-key pike-mode-map "\C-c\C-e" 'c-macro-expand))

(easy-menu-define c-pike-menu pike-mode-map "Pike Mode Commands"
		  (c-mode-menu "Pike"))

;;;###autoload
(defun pike-mode ()
  "Major mode for editing Pike code.
To submit a problem report, enter `\\[c-submit-bug-report]' from a
pike-mode buffer.  This automatically sets up a mail buffer with
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
  (kill-all-local-variables)
  (c-initialize-cc-mode)
  (set-syntax-table pike-mode-syntax-table)
  (setq major-mode 'pike-mode
 	mode-name "Pike"
 	local-abbrev-table pike-mode-abbrev-table
	abbrev-mode t)
  (use-local-map pike-mode-map)
  (c-common-init 'pike-mode)
  ;;(cc-imenu-init cc-imenu-pike-generic-expression) ;FIXME
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'pike-mode-hook)
  (c-update-modeline))


;; bug reporting

(defconst c-mode-help-address
  "bug-cc-mode@gnu.org"
  "Address(es) for CC Mode bug reports.")

(defun c-version ()
  "Echo the current version of CC Mode in the minibuffer."
  (interactive)
  (message "Using CC Mode version %s" c-version)
  (c-keep-region-active))

(defvar c-prepare-bug-report-hooks nil)

;; Dynamic variables used by reporter.
(defvar reporter-prompt-for-summary-p)
(defvar reporter-dont-compact-list)

(defun c-submit-bug-report ()
  "Submit via mail a bug report on CC Mode."
  (interactive)
  (require 'reporter)
  ;; load in reporter
  (let ((reporter-prompt-for-summary-p t)
	(reporter-dont-compact-list '(c-offsets-alist))
	(style c-indentation-style)
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
		     auto-fill-function
		     filladapt-mode
		     comment-multi-line
		     comment-start-skip
		     fill-prefix
		     paragraph-start
		     adaptive-fill-mode
		     adaptive-fill-regexp)
		   nil)))
	(delq 'c-special-indent-hook vars)
	(mapcar (lambda (var) (unless (boundp var) (delq var vars)))
		'(signal-error-on-buffer-boundary
		  filladapt-mode
		  defun-prompt-regexp))
	vars)
      (lambda ()
	(run-hooks 'c-prepare-bug-report-hooks)
	(insert (format "Buffer Style: %s\n\nc-emacs-features: %s\n"
			style c-features)))))))


(cc-provide 'cc-mode)
;;; cc-mode.el ends here
