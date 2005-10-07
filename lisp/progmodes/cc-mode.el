;;; cc-mode.el --- major mode for editing C and similar languages

;; Copyright (C) 1985,1987,1992-2003, 2004, 2005 Free Software Foundation, Inc.

;; Authors:    2003- Alan Mackenzie
;;             1998- Martin Stjernholm
;;             1992-1999 Barry A. Warsaw
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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; NOTE: Read the commentary below for the right way to submit bug reports!
;; NOTE: See the accompanying texinfo manual for details on using this mode!
;; Note: The version string is in cc-defs.

;; This package provides GNU Emacs major modes for editing C, C++,
;; Objective-C, Java, CORBA's IDL, Pike and AWK code.  As of the
;; latest Emacs and XEmacs releases, it is the default package for
;; editing these languages.  This package is called "CC Mode", and
;; should be spelled exactly this way.

;; CC Mode supports K&R and ANSI C, ANSI C++, Objective-C, Java,
;; CORBA's IDL, Pike and AWK with a consistent indentation model
;; across all modes.  This indentation model is intuitive and very
;; flexible, so that almost any desired style of indentation can be
;; supported.  Installation, usage, and programming details are
;; contained in an accompanying texinfo manual.

;; CC Mode's immediate ancestors were, c++-mode.el, cplus-md.el, and
;; cplus-md1.el..

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
    (load "cc-bytecomp" nil t)))

(cc-require 'cc-defs)
(cc-require-when-compile 'cc-langs)
(cc-require 'cc-vars)
(cc-require 'cc-engine)
(cc-require 'cc-styles)
(cc-require 'cc-cmds)
(cc-require 'cc-align)
(cc-require 'cc-menus)

;; Silence the compiler.
(cc-bytecomp-defvar comment-line-break-function) ; (X)Emacs 20+
(cc-bytecomp-defvar adaptive-fill-first-line-regexp) ; Emacs 20+
(cc-bytecomp-defun set-keymap-parents)	; XEmacs
(cc-bytecomp-defun run-mode-hooks)	; Emacs 21.1+
(cc-bytecomp-obsolete-fun make-local-hook) ; Marked obsolete in Emacs 21.1.

;; We set these variables during mode init, yet we don't require
;; font-lock.
(cc-bytecomp-defvar font-lock-defaults)
(cc-bytecomp-defvar font-lock-syntactic-keywords)

;; Menu support for both XEmacs and Emacs.  If you don't have easymenu
;; with your version of Emacs, you are incompatible!
(cc-external-require 'easymenu)

;; Load cc-fonts first after font-lock is loaded, since it isn't
;; necessary until font locking is requested.
(eval-after-load "font-lock"
  '(require 'cc-fonts))

;; cc-langs isn't loaded when we're byte compiled, so add autoload
;; directives for the interface functions.
(autoload 'c-make-init-lang-vars-fun "cc-langs")
(autoload 'c-init-language-vars "cc-langs" nil nil 'macro)


;; Other modes and packages which depend on CC Mode should do the
;; following to make sure everything is loaded and available for their
;; use:
;;
;; (require 'cc-mode)
;;
;; And in the major mode function:
;;
;; (c-initialize-cc-mode t)
;; (c-init-language-vars some-mode)
;; (c-common-init 'some-mode) ; Or perhaps (c-basic-common-init 'some-mode)
;;
;; If you're not writing a derived mode using the language variable
;; system, then some-mode is one of the language modes directly
;; supported by CC Mode.  You can then use (c-init-language-vars-for
;; 'some-mode) instead of `c-init-language-vars'.
;; `c-init-language-vars-for' is a function that avoids the rather
;; large expansion of `c-init-language-vars'.
;;
;; If you use `c-basic-common-init' then you might want to call
;; `c-font-lock-init' too to set up CC Mode's font lock support.
;;
;; See cc-langs.el for further info.  A small example of a derived mode
;; is also available at <http://cc-mode.sourceforge.net/
;; derived-mode-ex.el>.

(defun c-leave-cc-mode-mode ()
  (setq c-buffer-is-cc-mode nil))

(defun c-init-language-vars-for (mode)
  "Initialize the language variables for one of the language modes
directly supported by CC Mode.  This can be used instead of the
`c-init-language-vars' macro if the language you want to use is one of
those, rather than a derived language defined through the language
variable system (see \"cc-langs.el\")."
  ;; This function does not do any hidden buffer changes.
  (cond ((eq mode 'c-mode)    (c-init-language-vars c-mode))
	((eq mode 'c++-mode)  (c-init-language-vars c++-mode))
	((eq mode 'objc-mode) (c-init-language-vars objc-mode))
	((eq mode 'java-mode) (c-init-language-vars java-mode))
	((eq mode 'idl-mode)  (c-init-language-vars idl-mode))
	((eq mode 'pike-mode) (c-init-language-vars pike-mode))
	((eq mode 'awk-mode)  (c-init-language-vars awk-mode))
	(t (error "Unsupported mode %s" mode))))

;;;###autoload
(defun c-initialize-cc-mode (&optional new-style-init)
  "Initialize CC Mode for use in the current buffer.
If the optional NEW-STYLE-INIT is nil or left out then all necessary
initialization to run CC Mode for the C language is done.  Otherwise
only some basic setup is done, and a call to `c-init-language-vars' or
`c-init-language-vars-for' is necessary too (which gives more
control).  See \"cc-mode.el\" for more info."
  ;;
  ;; This function does not do any hidden buffer changes.

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
	(put 'c-initialize-cc-mode initprop c-initialization-ok))))

  (unless new-style-init
    (c-init-language-vars-for 'c-mode)))


;;; Common routines.

(defvar c-mode-base-map ()
  "Keymap shared by all CC Mode related modes.")

(defun c-make-inherited-keymap ()
  (let ((map (make-sparse-keymap)))
    ;; Necessary to use `cc-bytecomp-fboundp' below since this
    ;; function is called from top-level forms that are evaluated
    ;; while cc-bytecomp is active when one does M-x eval-buffer.
    (cond
     ;; XEmacs
     ((cc-bytecomp-fboundp 'set-keymap-parents)
      (set-keymap-parents map c-mode-base-map))
     ;; Emacs
     ((cc-bytecomp-fboundp 'set-keymap-parent)
      (set-keymap-parent map c-mode-base-map))
     ;; incompatible
     (t (error "CC Mode is incompatible with this version of Emacs")))
    map))

(defun c-define-abbrev-table (name defs)
  ;; Compatibility wrapper for `define-abbrev' which passes a non-nil
  ;; sixth argument for SYSTEM-FLAG in emacsen that support it
  ;; (currently only Emacs 21.2).
  (let ((table (or (symbol-value name)
		   (progn (define-abbrev-table name nil)
			  (symbol-value name)))))
    (while defs
      (condition-case nil
	  (apply 'define-abbrev table (append (car defs) '(t)))
	(wrong-number-of-arguments
	 (apply 'define-abbrev table (car defs))))
      (setq defs (cdr defs)))))
(put 'c-define-abbrev-table 'lisp-indent-function 1)

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

  ;; It doesn't suffice to put `c-fill-paragraph' on
  ;; `fill-paragraph-function' since `c-fill-paragraph' must be called
  ;; before any fill prefix adaption is done.  E.g. `filladapt-mode'
  ;; replaces `fill-paragraph' and does the adaption before calling
  ;; `fill-paragraph-function', and we have to mask comments etc
  ;; before that.  Also, `c-fill-paragraph' chains on to
  ;; `fill-paragraph' and the value on `fill-parapgraph-function' to
  ;; do the actual filling work.
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
  (define-key c-mode-base-map ","         'c-electric-semi&comma)
  (define-key c-mode-base-map "*"         'c-electric-star)
  (define-key c-mode-base-map "/"         'c-electric-slash)
  (define-key c-mode-base-map "\C-c\C-q"  'c-indent-defun)
  (define-key c-mode-base-map "\C-c\C-\\" 'c-backslash-region)
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

;; We don't require the outline package, but we configure it a bit anyway.
(cc-bytecomp-defvar outline-level)

(defun c-mode-menu (modestr)
  "Return a menu spec suitable for `easy-menu-define' that is exactly
like the C mode menu except that the menu bar item name is MODESTR
instead of \"C\".

This function is provided for compatibility only; derived modes should
preferably use the `c-mode-menu' language constant directly."
  (cons modestr (c-lang-const c-mode-menu c)))

;; Ugly hack to pull in the definition of `c-populate-syntax-table'
;; from cc-langs to make it available at runtime.  It's either this or
;; moving the definition for it to cc-defs, but that would mean to
;; break up the syntax table setup over two files.
(defalias 'c-populate-syntax-table
  (cc-eval-when-compile
    (let ((f (symbol-function 'c-populate-syntax-table)))
      (if (byte-code-function-p f) f (byte-compile f)))))

(defun c-after-change (beg end len)
  ;; Function put on `after-change-functions' to adjust various
  ;; caches.  Prefer speed to finesse here, since there will be an
  ;; order of magnitude more calls to this function than any of the
  ;; functions that use the caches.
  ;;
  ;; Note that care must be taken so that this is called before any
  ;; font-lock callbacks since we might get calls to functions using
  ;; these caches from inside them, and we must thus be sure that this
  ;; has already been executed.
  ;;
  ;; This function does not do any hidden buffer changes.

  (c-save-buffer-state ()
    (when (> end (point-max))
      ;; Some emacsen might return positions past the end. This has been
      ;; observed in Emacs 20.7 when rereading a buffer changed on disk
      ;; (haven't been able to minimize it, but Emacs 21.3 appears to
      ;; work).
      (setq end (point-max))
      (when (> beg end)
	(setq beg end)))

    (c-invalidate-sws-region-after beg end)
    (c-invalidate-state-cache beg)
    (c-invalidate-find-decl-cache beg)))

(defun c-basic-common-init (mode default-style)
  "Do the necessary initialization for the syntax handling routines
and the line breaking/filling code.  Intended to be used by other
packages that embed CC Mode.

MODE is the CC Mode flavor to set up, e.g. 'c-mode or 'java-mode.
DEFAULT-STYLE tells which indentation style to install.  It has the
same format as `c-default-style'.

Note that `c-init-language-vars' must be called before this function.
This function cannot do that since `c-init-language-vars' is a macro
that requires a literal mode spec at compile time."
  ;;
  ;; This function does not do any hidden buffer changes.

  (setq c-buffer-is-cc-mode mode)

  ;; these variables should always be buffer local; they do not affect
  ;; indentation style.
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'normal-auto-fill-function)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-multi-line)

  ;; now set their values
  (setq parse-sexp-ignore-comments t
	indent-line-function 'c-indent-line
	indent-region-function 'c-indent-region
	normal-auto-fill-function 'c-do-auto-fill
	comment-multi-line t)

  ;; Install `c-fill-paragraph' on `fill-paragraph-function' so that a
  ;; direct call to `fill-paragraph' behaves better.  This still
  ;; doesn't work with filladapt but it's better than nothing.
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'c-fill-paragraph)

  ;; (X)Emacs 20 and later.
  (when (boundp 'comment-line-break-function)
    (make-local-variable 'comment-line-break-function)
    (setq comment-line-break-function
	  'c-indent-new-comment-line))

  ;; Emacs 20 and later.
  (when (boundp 'parse-sexp-lookup-properties)
    (make-local-variable 'parse-sexp-lookup-properties)
    (setq parse-sexp-lookup-properties t))

  ;; Same as above for XEmacs 21 (although currently undocumented).
  (when (boundp 'lookup-syntax-properties)
    (make-local-variable 'lookup-syntax-properties)
    (setq lookup-syntax-properties t))

  ;; Use this in Emacs 21 to avoid meddling with the rear-nonsticky
  ;; property on each character.
  (when (boundp 'text-property-default-nonsticky)
    (make-local-variable 'text-property-default-nonsticky)
    (let ((elem (assq 'syntax-table text-property-default-nonsticky)))
      (if elem
	  (setcdr elem t)
	(setq text-property-default-nonsticky
	      (cons '(syntax-table . t)
		    text-property-default-nonsticky))))
    (setq text-property-default-nonsticky
	  (cons '(c-type . t)
		text-property-default-nonsticky)))

  ;; In Emacs 21 and later it's possible to turn off the ad-hoc
  ;; heuristic that open parens in column 0 are defun starters.  Since
  ;; we have c-state-cache that isn't useful and only causes trouble
  ;; so turn it off.
  (when (memq 'col-0-paren c-emacs-features)
    (make-local-variable 'open-paren-in-column-0-is-defun-start)
    (setq open-paren-in-column-0-is-defun-start nil))

  ;; The `c-type' text property with `c-decl-end' is used to mark the
  ;; ends of access keys to make interactive refontification work
  ;; better.
  (when c-opt-access-key
    (setq c-type-decl-end-used t))

  (c-clear-found-types)

  ;; now set the mode style based on default-style
  (let ((style (if (stringp default-style)
		   default-style
		 (or (cdr (assq mode default-style))
		     (cdr (assq 'other default-style))
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

  ;; put auto-hungry designators onto minor-mode-alist, but only once
  (or (assq 'c-auto-hungry-string minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(c-auto-hungry-string c-auto-hungry-string)
		  minor-mode-alist)))

  ;; Install the functions that ensure that various internal caches
  ;; don't become invalid due to buffer changes.
  (add-hook 'after-change-functions 'c-after-change nil t))

(defun c-after-font-lock-init ()
  ;; Put on `font-lock-mode-hook'.
  (remove-hook 'after-change-functions 'c-after-change t)
  (add-hook 'after-change-functions 'c-after-change nil t))

(defun c-font-lock-init ()
  "Set up the font-lock variables for using the font-lock support in CC Mode.
This does not load the font-lock package.  Use after
`c-basic-common-init' and after cc-fonts has been loaded."

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	`(,(if (c-mode-is-new-awk-p)
	       ;; awk-mode currently has only one font lock level.
	       'awk-font-lock-keywords
	     (mapcar 'c-mode-symbol
		     '("font-lock-keywords" "font-lock-keywords-1"
		       "font-lock-keywords-2" "font-lock-keywords-3")))
	  nil nil
	  ,c-identifier-syntax-modifications
	  c-beginning-of-syntax
	  (font-lock-lines-before . 1)
	  (font-lock-mark-block-function
	   . c-mark-function)))
  (add-hook 'font-lock-mode-hook 'c-after-font-lock-init nil t))

(defun c-setup-doc-comment-style ()
  "Initialize the variables that depend on the value of `c-doc-comment-style'."
  (when (and (featurep 'font-lock)
	     (symbol-value 'font-lock-mode))
    ;; Force font lock mode to reinitialize itself.
    (font-lock-mode 0)
    (font-lock-mode 1)))

(defun c-common-init (&optional mode)
  "Common initialization for all CC Mode modes.
In addition to the work done by `c-basic-common-init' and
`c-font-lock-init', this function sets up various other things as
customary in CC Mode modes but which aren't strictly necessary for CC
Mode to operate correctly.

MODE is the symbol for the mode to initialize, like 'c-mode.  See
`c-basic-common-init' for details.  It's only optional to be
compatible with old code; callers should always specify it.

This function does not do any hidden buffer changes."

  (unless mode
    ;; Called from an old third party package.  The fallback is to
    ;; initialize for C.
    (c-init-language-vars-for 'c-mode))

  (c-basic-common-init mode c-default-style)
  (when mode
    ;; Only initialize font locking if we aren't called from an old package.
    (c-font-lock-init))

  (make-local-variable 'outline-regexp)
  (make-local-variable 'outline-level)
  (setq outline-regexp "[^#\n\^M]"
	outline-level 'c-outline-level)

  (let ((rfn (assq mode c-require-final-newline)))
    (when rfn
      (make-local-variable 'require-final-newline)
      (and (cdr rfn)
	   (setq require-final-newline mode-require-final-newline)))))

(defun c-postprocess-file-styles ()
  "Function that post processes relevant file local variables in CC Mode.
Currently, this function simply applies any style and offset settings
found in the file's Local Variable list.  It first applies any style
setting found in `c-file-style', then it applies any offset settings
it finds in `c-file-offsets'.

Note that the style variables are always made local to the buffer."
  ;;
  ;; This function does not do any hidden buffer changes.

  ;; apply file styles and offsets
  (when c-buffer-is-cc-mode
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
	  c-file-offsets))))

(add-hook 'hack-local-variables-hook 'c-postprocess-file-styles)

(defmacro c-run-mode-hooks (&rest hooks)
  ;; Emacs 21.1 has introduced a system with delayed mode hooks that
  ;; require the use of the new function `run-mode-hooks'.
  (if (cc-bytecomp-fboundp 'run-mode-hooks)
      `(run-mode-hooks ,@hooks)
    `(progn ,@(mapcar (lambda (hook) `(run-hooks ,hook)) hooks))))


;; Support for C

;;;###autoload
(defvar c-mode-syntax-table nil
  "Syntax table used in c-mode buffers.")
(or c-mode-syntax-table
    (setq c-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table c))))

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
		  (cons "C" (c-lang-const c-mode-menu c)))

;; In XEmacs >= 21.5 modes should add their own entries to
;; `auto-mode-alist'.  The comment form of autoload is used to avoid
;; doing this on load.  That since `add-to-list' prepends the value
;; which could cause it to clobber user settings.  Later emacsen have
;; an append option, but it's not safe to use.

;; The the extension ".C" is associated to C++ while the lowercase
;; variant goes to C.  On case insensitive file systems, this means
;; that ".c" files also might open C++ mode if the C++ entry comes
;; first on `auto-mode-alist'.  Thus we try to ensure that ".C" comes
;; after ".c", and since `add-to-list' adds the entry first we have to
;; add the ".C" entry first.
;;;###autoload (add-to-list 'auto-mode-alist '("\\.\\(cc\\|hh\\)\\'" . c++-mode))
;;;###autoload (add-to-list 'auto-mode-alist '("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . c++-mode))
;;;###autoload (add-to-list 'auto-mode-alist '("\\.\\(CC?\\|HH?\\)\\'" . c++-mode))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.[ch]\\'" . c-mode))

;; NB: The following two associate yacc and lex files to C Mode, which
;; is not really suitable for those formats.  Anyway, afaik there's
;; currently no better mode for them, and besides this is legacy.
;;;###autoload (add-to-list 'auto-mode-alist '("\\.y\\(acc\\)?\\'" . c-mode))
;;;###autoload (add-to-list 'auto-mode-alist '("\\.lex\\'" . c-mode))

;;;###autoload
(defun c-mode ()
  "Major mode for editing K&R and ANSI C code.
To submit a problem report, enter `\\[c-submit-bug-report]' from a
c-mode buffer.  This automatically sets up a mail buffer with version
information already added.  You just need to add a description of the
problem, including a reproducible test case, and send the message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `c-mode-hook'.

Key bindings:
\\{c-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table c-mode-syntax-table)
  (setq major-mode 'c-mode
	mode-name "C"
	local-abbrev-table c-mode-abbrev-table
	abbrev-mode t)
  (use-local-map c-mode-map)
  (c-init-language-vars-for 'c-mode)
  (c-common-init 'c-mode)
  (easy-menu-add c-c-menu)
  (cc-imenu-init cc-imenu-c-generic-expression)
  (c-run-mode-hooks 'c-mode-common-hook 'c-mode-hook)
  (c-update-modeline))


;; Support for C++

;;;###autoload
(defvar c++-mode-syntax-table nil
  "Syntax table used in c++-mode buffers.")
(or c++-mode-syntax-table
    (setq c++-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table c++))))

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
		  (cons "C++" (c-lang-const c-mode-menu c++)))

;;;###autoload
(defun c++-mode ()
  "Major mode for editing C++ code.
To submit a problem report, enter `\\[c-submit-bug-report]' from a
c++-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case, and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `c++-mode-hook'.

Key bindings:
\\{c++-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table c++-mode-syntax-table)
  (setq major-mode 'c++-mode
	mode-name "C++"
	local-abbrev-table c++-mode-abbrev-table
	abbrev-mode t)
  (use-local-map c++-mode-map)
  (c-init-language-vars-for 'c++-mode)
  (c-common-init 'c++-mode)
  (easy-menu-add c-c++-menu)
  (cc-imenu-init cc-imenu-c++-generic-expression)
  (c-run-mode-hooks 'c-mode-common-hook 'c++-mode-hook)
  (c-update-modeline))


;; Support for Objective-C

;;;###autoload
(defvar objc-mode-syntax-table nil
  "Syntax table used in objc-mode buffers.")
(or objc-mode-syntax-table
    (setq objc-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table objc))))

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
		  (cons "ObjC" (c-lang-const c-mode-menu objc)))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.m\\'" . objc-mode))

;;;###autoload
(defun objc-mode ()
  "Major mode for editing Objective C code.
To submit a problem report, enter `\\[c-submit-bug-report]' from an
objc-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case, and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `objc-mode-hook'.

Key bindings:
\\{objc-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table objc-mode-syntax-table)
  (setq major-mode 'objc-mode
	mode-name "ObjC"
	local-abbrev-table objc-mode-abbrev-table
	abbrev-mode t)
  ;; The `c-type' text property with `c-decl-end' is used to mark the
  ;; end of the @-style directives.
  (setq c-type-decl-end-used t)
  (use-local-map objc-mode-map)
  (c-init-language-vars-for 'objc-mode)
  (c-common-init 'objc-mode)
  (easy-menu-add c-objc-menu)
  (cc-imenu-init nil 'cc-imenu-objc-function)
  (c-run-mode-hooks 'c-mode-common-hook 'objc-mode-hook)
  (c-update-modeline))


;; Support for Java

;;;###autoload
(defvar java-mode-syntax-table nil
  "Syntax table used in java-mode buffers.")
(or java-mode-syntax-table
    (setq java-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table java))))

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

;; Regexp trying to describe the beginning of a Java top-level
;; definition.  This is not used by CC Mode, nor is it maintained
;; since it's practically impossible to write a regexp that reliably
;; matches such a construct.  Other tools are necessary.
(defconst c-Java-defun-prompt-regexp
  "^[ \t]*\\(\\(\\(public\\|protected\\|private\\|const\\|abstract\\|synchronized\\|final\\|static\\|threadsafe\\|transient\\|native\\|volatile\\)\\s-+\\)*\\(\\(\\([[a-zA-Z][][_$.a-zA-Z0-9]*[][_$.a-zA-Z0-9]+\\|[[a-zA-Z]\\)\\s-*\\)\\s-+\\)\\)?\\(\\([[a-zA-Z][][_$.a-zA-Z0-9]*\\s-+\\)\\s-*\\)?\\([_a-zA-Z][^][ \t:;.,{}()=]*\\|\\([_$a-zA-Z][_$.a-zA-Z0-9]*\\)\\)\\s-*\\(([^);{}]*)\\)?\\([] \t]*\\)\\(\\s-*\\<throws\\>\\s-*\\(\\([_$a-zA-Z][_$.a-zA-Z0-9]*\\)[, \t\n\r\f\v]*\\)+\\)?\\s-*")

(easy-menu-define c-java-menu java-mode-map "Java Mode Commands"
		  (cons "Java" (c-lang-const c-mode-menu java)))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))

;;;###autoload
(defun java-mode ()
  "Major mode for editing Java code.
To submit a problem report, enter `\\[c-submit-bug-report]' from a
java-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case, and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `java-mode-hook'.

Key bindings:
\\{java-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table java-mode-syntax-table)
  (setq major-mode 'java-mode
 	mode-name "Java"
 	local-abbrev-table java-mode-abbrev-table
	abbrev-mode t)
  (use-local-map java-mode-map)
  (c-init-language-vars-for 'java-mode)
  (c-common-init 'java-mode)
  (easy-menu-add c-java-menu)
  (cc-imenu-init cc-imenu-java-generic-expression)
  (c-run-mode-hooks 'c-mode-common-hook 'java-mode-hook)
  (c-update-modeline))


;; Support for CORBA's IDL language

;;;###autoload
(defvar idl-mode-syntax-table nil
  "Syntax table used in idl-mode buffers.")
(or idl-mode-syntax-table
    (setq idl-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table idl))))

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
		  (cons "IDL" (c-lang-const c-mode-menu idl)))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.idl\\'" . idl-mode))

;;;###autoload
(defun idl-mode ()
  "Major mode for editing CORBA's IDL, PSDL and CIDL code.
To submit a problem report, enter `\\[c-submit-bug-report]' from an
idl-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case, and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `idl-mode-hook'.

Key bindings:
\\{idl-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table idl-mode-syntax-table)
  (setq major-mode 'idl-mode
	mode-name "IDL"
	local-abbrev-table idl-mode-abbrev-table)
  (use-local-map idl-mode-map)
  (c-init-language-vars-for 'idl-mode)
  (c-common-init 'idl-mode)
  (easy-menu-add c-idl-menu)
  ;;(cc-imenu-init cc-imenu-idl-generic-expression) ;TODO
  (c-run-mode-hooks 'c-mode-common-hook 'idl-mode-hook)
  (c-update-modeline))


;; Support for Pike

;;;###autoload
(defvar pike-mode-syntax-table nil
  "Syntax table used in pike-mode buffers.")
(or pike-mode-syntax-table
    (setq pike-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table pike))))

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
		  (cons "Pike" (c-lang-const c-mode-menu pike)))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.\\(u?lpc\\|pike\\|pmod\\(.in\\)?\\)\\'" . pike-mode))
;;;###autoload (add-to-list 'interpreter-mode-alist '("pike" . pike-mode))

;;;###autoload
(defun pike-mode ()
  "Major mode for editing Pike code.
To submit a problem report, enter `\\[c-submit-bug-report]' from a
pike-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case, and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `pike-mode-hook'.

Key bindings:
\\{pike-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table pike-mode-syntax-table)
  (setq major-mode 'pike-mode
 	mode-name "Pike"
 	local-abbrev-table pike-mode-abbrev-table
	abbrev-mode t)
  (use-local-map pike-mode-map)
  (c-init-language-vars-for 'pike-mode)
  (c-common-init 'pike-mode)
  (easy-menu-add c-pike-menu)
  ;;(cc-imenu-init cc-imenu-pike-generic-expression) ;TODO
  (c-run-mode-hooks 'c-mode-common-hook 'pike-mode-hook)
  (c-update-modeline))


;; Support for awk.  This is purposely disabled for older (X)Emacsen which
;; don't support syntax-table properties.

;;;###autoload (add-to-list 'auto-mode-alist '("\\.awk\\'" . awk-mode))
;;;###autoload (add-to-list 'interpreter-mode-alist '("awk" . awk-mode))
;;;###autoload (add-to-list 'interpreter-mode-alist '("mawk" . awk-mode))
;;;###autoload (add-to-list 'interpreter-mode-alist '("nawk" . awk-mode))
;;;###autoload (add-to-list 'interpreter-mode-alist '("gawk" . awk-mode))

;;; Autoload directives must be on the top level, so we construct an
;;; autoload form instead.
;;;###autoload (autoload 'awk-mode "cc-mode" "Major mode for editing AWK code." t)

(if (not (memq 'syntax-properties c-emacs-features))
    (autoload 'awk-mode "awk-mode" "Major mode for editing AWK code."  t)

  (defvar awk-mode-abbrev-table nil
    "Abbreviation table used in awk-mode buffers.")
  (c-define-abbrev-table 'awk-mode-abbrev-table
    '(("else" "else" c-electric-continued-statement 0)
      ("while" "while" c-electric-continued-statement 0)))

  (defvar awk-mode-map ()
    "Keymap used in awk-mode buffers.")
  (if awk-mode-map
      nil
    (setq awk-mode-map (c-make-inherited-keymap))
    ;; add bindings which are only useful for awk.
    (define-key awk-mode-map "#" 'self-insert-command)
    (define-key awk-mode-map "/" 'self-insert-command)
    (define-key awk-mode-map "*" 'self-insert-command)
    (define-key awk-mode-map "\C-c\C-n" 'undefined) ; #if doesn't exist in awk.
    (define-key awk-mode-map "\C-c\C-p" 'undefined)
    (define-key awk-mode-map "\C-c\C-u" 'undefined)
    (define-key awk-mode-map "\M-a" 'undefined) ; c-awk-beginning-of-statement isn't yet implemented.
    (define-key awk-mode-map "\M-e" 'undefined) ; c-awk-end-of-statement isn't yet implemented.
    (define-key awk-mode-map "\C-\M-a" 'c-awk-beginning-of-defun)
    (define-key awk-mode-map "\C-\M-e" 'c-awk-end-of-defun))

  (easy-menu-define c-awk-menu awk-mode-map "AWK Mode Commands"
    (cons "AWK" (c-lang-const c-mode-menu awk)))

  (defun awk-mode ()
    "Major mode for editing AWK code.
To submit a problem report, enter `\\[c-submit-bug-report]' from an
awk-mode buffer.  This automatically sets up a mail buffer with version
information already added.  You just need to add a description of the
problem, including a reproducible test case, and send the message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `awk-mode-hook'.

Key bindings:
\\{awk-mode-map}"
    (interactive)
    (require 'cc-awk)                   ; Added 2003/6/10.
    (kill-all-local-variables)
    (c-initialize-cc-mode t)
    (set-syntax-table awk-mode-syntax-table)
    (setq major-mode 'awk-mode
          mode-name "AWK"
          local-abbrev-table awk-mode-abbrev-table
          abbrev-mode t)
    (use-local-map awk-mode-map)
    (c-init-language-vars-for 'awk-mode)
    (c-common-init 'awk-mode)
    ;; The rest of CC Mode does not (yet) use `font-lock-syntactic-keywords',
    ;; so it's not set by `c-font-lock-init'.
    (make-local-variable 'font-lock-syntactic-keywords)
    (setq font-lock-syntactic-keywords
          '((c-awk-set-syntax-table-properties
             0 (0)                      ; Everything on this line is a dummy.
             nil t)))
    (c-awk-unstick-NL-prop)
    (add-hook 'before-change-functions 'c-awk-before-change nil t)
    (add-hook 'after-change-functions 'c-awk-after-change nil t)
    (c-save-buffer-state nil
      (save-restriction
        (widen)
        (c-awk-clear-NL-props (point-min) (point-max))
        (c-awk-after-change (point-min) (point-max) 0))) ; Set syntax-table props.

    ;; Prevent Xemacs's buffer-syntactic-context being used.  See the comment
    ;; in cc-engine.el, just before (defun c-fast-in-literal ...
    (defalias 'c-in-literal 'c-slow-in-literal)

    (c-run-mode-hooks 'c-mode-common-hook 'awk-mode-hook)
    (c-update-modeline))
) ;; closes the (if (not (memq 'syntax-properties c-emacs-features))


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
     (reporter-submit-bug-report
      c-mode-help-address
      (concat "CC Mode " c-version " (" mode-name ")")
      (let ((vars (append
		   c-style-variables
		   '(c-buffer-is-cc-mode
		     c-tab-always-indent
		     c-syntactic-indentation
		     c-syntactic-indentation-in-macros
		     c-ignore-auto-fill
		     c-auto-align-backslashes
		     c-backspace-function
		     c-delete-function
		     c-electric-pound-behavior
		     c-default-style
		     c-enable-xemacs-performance-kludge-p
		     c-old-style-variable-behavior
		     defun-prompt-regexp
		     tab-width
		     comment-column
		     parse-sexp-ignore-comments
		     parse-sexp-lookup-properties
		     lookup-syntax-properties
		     ;; A brain-damaged XEmacs only variable that, if
		     ;; set to nil can cause all kinds of chaos.
		     signal-error-on-buffer-boundary
		     ;; Variables that affect line breaking and comments.
		     auto-fill-mode
		     auto-fill-function
		     filladapt-mode
		     comment-multi-line
		     comment-start-skip
		     fill-prefix
		     fill-column
		     paragraph-start
		     adaptive-fill-mode
		     adaptive-fill-regexp)
		   nil)))
	(mapcar (lambda (var) (unless (boundp var)
				(setq vars (delq var vars))))
		'(signal-error-on-buffer-boundary
		  filladapt-mode
		  defun-prompt-regexp
		  font-lock-mode
		  font-lock-maximum-decoration
		  parse-sexp-lookup-properties
		  lookup-syntax-properties))
	vars)
      (lambda ()
	(run-hooks 'c-prepare-bug-report-hooks)
	(insert (format "Buffer Style: %s\nc-emacs-features: %s\n"
			style c-features)))))))


(cc-provide 'cc-mode)

;; arch-tag: 7825e5c4-fd09-439f-a04d-4c13208ba3d7
;;; cc-mode.el ends here
