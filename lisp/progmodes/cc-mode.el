;;; cc-mode.el --- major mode for editing C and similar languages

;; Copyright (C) 1985, 1987, 1992-2020 Free Software Foundation, Inc.

;; Authors:    2003- Alan Mackenzie
;;             1998- Martin Stjernholm
;;             1992-1999 Barry A. Warsaw
;;             1987 Dave Detlefs
;;             1987 Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    a long, long, time ago. adapted from the original c-mode.el
;; Keywords:   c languages
;; The version header below is used for ELPA packaging.
;; Version: 5.33.1

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

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

;; Externally maintained major modes which use CC-mode's engine include:
;; - cuda-mode
;; - csharp-mode (https://github.com/josteink/csharp-mode)
;; - haxe-mode
;; - d-mode
;; - dart-mode
;; - cc-php-js-cs.el
;; - php-mode
;; - yang-mode
;; - math-mode (mathematica)
;; - unrealscript-mode
;; - groovy-mode

;;; Code:

;; For Emacs < 22.2.
(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest _r))))

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (load "cc-bytecomp" nil t)))

(cc-require 'cc-defs)
(cc-require 'cc-vars)
(cc-require-when-compile 'cc-langs)
(cc-require 'cc-engine)
(cc-require 'cc-styles)
(cc-require 'cc-cmds)
(cc-require 'cc-align)
(cc-require 'cc-menus)
(cc-require 'cc-guess)

;; Silence the compiler.
(cc-bytecomp-defvar adaptive-fill-first-line-regexp) ; Emacs
(cc-bytecomp-defun run-mode-hooks)	; Emacs 21.1

;; We set this variable during mode init, yet we don't require
;; font-lock.
(cc-bytecomp-defvar font-lock-defaults)

;; Menu support for both XEmacs and Emacs.  If you don't have easymenu
;; with your version of Emacs, you are incompatible!
(cc-external-require 'easymenu)

;; Load cc-fonts first after font-lock is loaded, since it isn't
;; necessary until font locking is requested.
; (eval-after-load "font-lock" ; 2006-07-09: font-lock is now preloaded.
;   '
(require 'cc-fonts) ;)

;; The following three really belong to cc-fonts.el, but they are required
;; even when cc-fonts.el hasn't been loaded (this happens in XEmacs when
;; font-lock-mode is nil).

(defvar c-doc-line-join-re regexp-unmatchable)
;; Matches a join of two lines in a doc comment.
;; This should not be changed directly, but instead set by
;; `c-setup-doc-comment-style'.  This variable is used in `c-find-decl-spots'
;; in (e.g.) autodoc style comments to bridge the gap between a "@\n" at an
;; EOL and the token following "//!" on the next line.

(defvar c-doc-bright-comment-start-re regexp-unmatchable)
;; Matches the start of a "bright" comment, one whose contents may be
;; fontified by, e.g., `c-font-lock-declarations'.

(defvar c-doc-line-join-end-ch nil)
;; A list of characters, each being a last character of a doc comment marker,
;; e.g. the ! from pike autodoc's "//!".


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
  (when c-buffer-is-cc-mode
    (save-restriction
      (widen)
      (c-save-buffer-state ()
	(c-clear-char-properties (point-min) (point-max) 'category)
	(c-clear-char-properties (point-min) (point-max) 'syntax-table)
	(c-clear-char-properties (point-min) (point-max) 'c-fl-syn-tab)
	(c-clear-char-properties (point-min) (point-max) 'c-is-sws)
	(c-clear-char-properties (point-min) (point-max) 'c-in-sws)
	(c-clear-char-properties (point-min) (point-max) 'c-type)
	(if (c-major-mode-is 'awk-mode)
	    (c-clear-char-properties (point-min) (point-max) 'c-awk-NL-prop))))
    (setq c-buffer-is-cc-mode nil)))

(defun c-init-language-vars-for (mode)
  "Initialize the language variables for one of the language modes
directly supported by CC Mode.  This can be used instead of the
`c-init-language-vars' macro if the language you want to use is one of
those, rather than a derived language defined through the language
variable system (see \"cc-langs.el\")."
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
	    ;; Connect up with Emacs's electric-pair-mode
	    (eval-after-load "elec-pair"
	      '(when (boundp 'electric-pair-inhibit-predicate)
		 (dolist (buf (buffer-list))
		   (with-current-buffer buf
		     (when c-buffer-is-cc-mode
		       (make-local-variable 'electric-pair-inhibit-predicate)
		       (setq electric-pair-inhibit-predicate
			     #'c-electric-pair-inhibit-predicate))))))
	    (setq c-initialization-ok t)
	    ;; Connect up with Emacs's electric-indent-mode, for >= Emacs 24.4
            (when (fboundp 'electric-indent-local-mode)
	      (add-hook 'electric-indent-mode-hook 'c-electric-indent-mode-hook)
              (add-hook 'electric-indent-local-mode-hook
                        'c-electric-indent-local-mode-hook)))
	;; Will try initialization hooks again if they failed.
	(put 'c-initialize-cc-mode initprop c-initialization-ok))))

  (unless new-style-init
    (c-init-language-vars-for 'c-mode)))


;;; Common routines.

(defvar c-mode-base-map ()
  "Keymap shared by all CC Mode related modes.")

(defun c-make-inherited-keymap ()
  (let ((map (make-sparse-keymap)))
    (c-set-keymap-parent map c-mode-base-map)
    map))

(defun c-define-abbrev-table (name defs &optional doc)
  ;; Compatibility wrapper for `define-abbrev' which passes a non-nil
  ;; sixth argument for SYSTEM-FLAG in emacsen that support it
  ;; (currently only Emacs >= 21.2).
  (let ((table (or (and (boundp name) (symbol-value name))
		   (progn (condition-case nil
                              (define-abbrev-table name nil doc)
                            (wrong-number-of-arguments ;E.g. Emacs<23.
                             (eval `(defvar ,name nil ,doc))
                             (define-abbrev-table name nil)))
			  (symbol-value name)))))
    (while defs
      (condition-case nil
	  (apply 'define-abbrev table (append (car defs) '(t)))
	(wrong-number-of-arguments
	 (apply 'define-abbrev table (car defs))))
      (setq defs (cdr defs)))))
(put 'c-define-abbrev-table 'lisp-indent-function 1)

(defun c-bind-special-erase-keys ()
  ;; Only used in Emacs to bind C-c C-<delete> and C-c C-<backspace>
  ;; to the proper keys depending on `normal-erase-is-backspace'.
  (if normal-erase-is-backspace
      (progn
	(define-key c-mode-base-map (kbd "C-c C-<delete>")
	  'c-hungry-delete-forward)
	(define-key c-mode-base-map (kbd "C-c C-<backspace>")
	  'c-hungry-delete-backwards))
    (define-key c-mode-base-map (kbd "C-c C-<delete>")
      'c-hungry-delete-backwards)
    (define-key c-mode-base-map (kbd "C-c C-<backspace>")
      'c-hungry-delete-forward)))

(if c-mode-base-map
    nil

  (setq c-mode-base-map (make-sparse-keymap))
  (when (boundp 'prog-mode-map)
    (c-set-keymap-parent c-mode-base-map prog-mode-map))

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
  (substitute-key-definition 'indent-for-tab-command
			     ;; XXX Is this the right thing to do
			     ;; here?
			     'c-indent-line-or-region
			     c-mode-base-map global-map)
  (when (fboundp 'comment-indent-new-line)
    ;; indent-new-comment-line has changed name to
    ;; comment-indent-new-line in Emacs 21.
    (substitute-key-definition 'comment-indent-new-line
			       'c-indent-new-comment-line
			       c-mode-base-map global-map))

  ;; RMS says don't make these the default.
  ;; (April 2006): RMS has now approved these commands as defaults.
  (unless (memq 'argumentative-bod-function c-emacs-features)
    (define-key c-mode-base-map "\e\C-a"    'c-beginning-of-defun)
    (define-key c-mode-base-map "\e\C-e"    'c-end-of-defun))

  (define-key c-mode-base-map "\C-c\C-n"  'c-forward-conditional)
  (define-key c-mode-base-map "\C-c\C-p"  'c-backward-conditional)
  (define-key c-mode-base-map "\C-c\C-u"  'c-up-conditional)

  ;; It doesn't suffice to put `c-fill-paragraph' on
  ;; `fill-paragraph-function' since `c-fill-paragraph' must be called
  ;; before any fill prefix adaption is done.  E.g. `filladapt-mode'
  ;; replaces `fill-paragraph' and does the adaption before calling
  ;; `fill-paragraph-function', and we have to mask comments etc
  ;; before that.  Also, `c-fill-paragraph' chains on to
  ;; `fill-paragraph' and the value on `fill-paragraph-function' to
  ;; do the actual filling work.
  (substitute-key-definition 'fill-paragraph 'c-fill-paragraph
			     c-mode-base-map global-map)
  ;; In XEmacs the default fill function is called
  ;; fill-paragraph-or-region.
  (substitute-key-definition 'fill-paragraph-or-region 'c-fill-paragraph
			     c-mode-base-map global-map)

  ;; We bind the forward deletion key and (implicitly) C-d to
  ;; `c-electric-delete-forward', and the backward deletion key to
  ;; `c-electric-backspace'.  The hungry variants are bound to the
  ;; same keys but prefixed with C-c.  This implies that C-c C-d is
  ;; `c-hungry-delete-forward'.  For consistency, we bind not only C-c
  ;; <backspace> to `c-hungry-delete-backwards' but also
  ;; C-c C-<backspace>, so that the Ctrl key can be held down during
  ;; the whole sequence regardless of the direction.  This in turn
  ;; implies that we bind C-c C-<delete> to `c-hungry-delete-forward',
  ;; for the same reason.

  ;; Bind the electric deletion functions to C-d and DEL.  Emacs 21
  ;; automatically maps the [delete] and [backspace] keys to these two
  ;; depending on window system and user preferences.  (In earlier
  ;; versions it's possible to do the same by using `function-key-map'.)
  (define-key c-mode-base-map "\C-d" 'c-electric-delete-forward)
  (define-key c-mode-base-map "\177" 'c-electric-backspace)
  (define-key c-mode-base-map "\C-c\C-d"     'c-hungry-delete-forward)
  (define-key c-mode-base-map [?\C-c ?\d]    'c-hungry-delete-backwards)
  (define-key c-mode-base-map [?\C-c ?\C-\d] 'c-hungry-delete-backwards)
  (define-key c-mode-base-map [?\C-c deletechar] 'c-hungry-delete-forward) ; C-c <delete> on a tty.
  (define-key c-mode-base-map [?\C-c (control deletechar)] ; C-c C-<delete> on a tty.
    'c-hungry-delete-forward)
  (when (boundp 'normal-erase-is-backspace)
    ;; The automatic C-d and DEL mapping functionality doesn't extend
    ;; to special combinations like C-c C-<delete>, so we have to hook
    ;; into the `normal-erase-is-backspace' system to bind it directly
    ;; as appropriate.
    (add-hook 'normal-erase-is-backspace-mode-hook 'c-bind-special-erase-keys)
    (c-bind-special-erase-keys))

  (when (fboundp 'delete-forward-p)
    ;; In XEmacs we fix the forward and backward deletion behavior by
    ;; binding the keysyms for the [delete] and [backspace] keys
    ;; directly, and use `delete-forward-p' to decide what [delete]
    ;; should do.  That's done in the XEmacs specific
    ;; `c-electric-delete' and `c-hungry-delete' functions.
    (define-key c-mode-base-map [delete]    'c-electric-delete)
    (define-key c-mode-base-map [backspace] 'c-electric-backspace)
    (define-key c-mode-base-map (kbd "C-c <delete>") 'c-hungry-delete)
    (define-key c-mode-base-map (kbd "C-c C-<delete>") 'c-hungry-delete)
    (define-key c-mode-base-map (kbd "C-c <backspace>")
      'c-hungry-delete-backwards)
    (define-key c-mode-base-map (kbd "C-c C-<backspace>")
      'c-hungry-delete-backwards))

  (define-key c-mode-base-map "#"         'c-electric-pound)
  (define-key c-mode-base-map "{"         'c-electric-brace)
  (define-key c-mode-base-map "}"         'c-electric-brace)
  (define-key c-mode-base-map "/"         'c-electric-slash)
  (define-key c-mode-base-map "*"         'c-electric-star)
  (define-key c-mode-base-map ";"         'c-electric-semi&comma)
  (define-key c-mode-base-map ","         'c-electric-semi&comma)
  (define-key c-mode-base-map ":"         'c-electric-colon)
  (define-key c-mode-base-map "("         'c-electric-paren)
  (define-key c-mode-base-map ")"         'c-electric-paren)

  (define-key c-mode-base-map "\C-c\C-\\" 'c-backslash-region)
  (define-key c-mode-base-map "\C-c\C-a"  'c-toggle-auto-newline)
  (define-key c-mode-base-map "\C-c\C-b"  'c-submit-bug-report)
  (define-key c-mode-base-map "\C-c\C-c"  'comment-region)
  (define-key c-mode-base-map "\C-c\C-l"  'c-toggle-electric-state)
  (define-key c-mode-base-map "\C-c\C-o"  'c-set-offset)
  (define-key c-mode-base-map "\C-c\C-q"  'c-indent-defun)
  (define-key c-mode-base-map "\C-c\C-s"  'c-show-syntactic-information)
  ;; (define-key c-mode-base-map "\C-c\C-t"  'c-toggle-auto-hungry-state)  Commented out by ACM, 2005-03-05.
  (define-key c-mode-base-map "\C-c."     'c-set-style)
  ;; conflicts with OOBR
  ;;(define-key c-mode-base-map "\C-c\C-v"  'c-version)
  ;; (define-key c-mode-base-map "\C-c\C-y"  'c-toggle-hungry-state)  Commented out by ACM, 2005-11-22.
  (define-key c-mode-base-map "\C-c\C-w" 'c-subword-mode)
  (define-key c-mode-base-map "\C-c\C-k" 'c-toggle-comment-style)
  (define-key c-mode-base-map "\C-c\C-z" 'c-display-defun-name))

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

;; CAUTION: Try to avoid installing things on
;; `before-change-functions'.  The macro `combine-after-change-calls'
;; is used and it doesn't work if there are things on that hook.  That
;; can cause font lock functions to run in inconvenient places during
;; temporary changes in some font lock support modes, causing extra
;; unnecessary work and font lock glitches due to interactions between
;; various text properties.
;;
;; (2007-02-12): The macro `combine-after-change-calls' ISN'T used any
;; more.

(defun c-unfind-enclosing-token (pos)
  ;; If POS is wholly inside a token, remove that id from
  ;; `c-found-types', should it be present.  Return t if we were in an
  ;; id, else nil.
  (save-excursion
    (let ((tok-beg (progn (goto-char pos)
			  (and (c-beginning-of-current-token) (point))))
	  (tok-end (progn (goto-char pos)
			  (and (c-end-of-current-token) (point)))))
      (when (and tok-beg tok-end)
	(c-unfind-type (buffer-substring-no-properties tok-beg tok-end))
	t))))

(defun c-unfind-coalesced-tokens (beg end)
  ;; If removing the region (beg end) would coalesce an identifier ending at
  ;; beg with an identifier (fragment) beginning at end, or an identifier
  ;; fragment ending at beg with an identifier beginning at end, remove the
  ;; pertinent identifier(s) from `c-found-types'.
  (save-excursion
    (when (< beg end)
      (goto-char beg)
      (when
	  (and (not (bobp))
	       (progn (c-backward-syntactic-ws) (eq (point) beg))
	       (/= (skip-chars-backward c-symbol-chars (1- (point))) 0)
	       (progn (goto-char beg) (c-forward-syntactic-ws) (<= (point) end))
	       (> (point) beg)
	       (goto-char end)
	       (looking-at c-symbol-char-key))
	(goto-char beg)
	(c-simple-skip-symbol-backward)
	(c-unfind-type (buffer-substring-no-properties (point) beg)))

      (goto-char end)
      (when
	  (and (not (eobp))
	       (progn (c-forward-syntactic-ws) (eq (point) end))
	       (looking-at c-symbol-char-key)
	       (progn (c-backward-syntactic-ws) (>= (point) beg))
	       (< (point) end)
	       (/= (skip-chars-backward c-symbol-chars (1- (point))) 0))
	(goto-char (1+ end))
	(c-end-of-current-token)
	(c-unfind-type (buffer-substring-no-properties end (point)))))))

;; c-maybe-stale-found-type records a place near the region being
;; changed where an element of `found-types' might become stale.  It
;; is set in c-before-change and is either nil, or has the form:
;;
;;   (c-decl-id-start "foo" 97 107  " (* ooka) " "o"), where
;;
;; o - `c-decl-id-start' is the c-type text property value at buffer
;;   pos 96.
;;
;; o - 97 107 is the region potentially containing the stale type -
;;   this is delimited by a non-nil c-type text property at 96 and
;;   either another one or a ";", "{", or "}" at 107.
;;
;; o - " (* ooka) " is the (before change) buffer portion containing
;;   the suspect type (here "ooka").
;;
;; o - "o" is the buffer contents which is about to be deleted.  This
;;   would be the empty string for an insertion.
(defvar c-maybe-stale-found-type nil)
(make-variable-buffer-local 'c-maybe-stale-found-type)

(defvar c-just-done-before-change nil)
(make-variable-buffer-local 'c-just-done-before-change)
;; This variable is set to t by `c-before-change' and to nil by
;; `c-after-change'.  It is used for two purposes: (i) to detect a spurious
;; invocation of `before-change-functions' directly following on from a
;; correct one.  This happens in some Emacsen, for example when
;; `basic-save-buffer' does (insert ?\n) when `require-final-newline' is
;; non-nil; (ii) to detect when Emacs fails to invoke
;; `before-change-functions'.  This can happen when reverting a buffer - see
;; bug #24094.  It seems these failures happen only in GNU Emacs; XEmacs seems
;; to maintain the strict alternation of calls to `before-change-functions'
;; and `after-change-functions'.  Note that this variable is not set when
;; `c-before-change' is invoked by a change to text properties.

(defun c-basic-common-init (mode default-style)
  "Do the necessary initialization for the syntax handling routines
and the line breaking/filling code.  Intended to be used by other
packages that embed CC Mode.

MODE is the CC Mode flavor to set up, e.g. `c-mode' or `java-mode'.
DEFAULT-STYLE tells which indentation style to install.  It has the
same format as `c-default-style'.

Note that `c-init-language-vars' must be called before this function.
This function cannot do that since `c-init-language-vars' is a macro
that requires a literal mode spec at compile time."

  (setq c-buffer-is-cc-mode mode)

  ;; these variables should always be buffer local; they do not affect
  ;; indentation style.
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)

  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (make-local-variable 'adaptive-fill-mode)
  (make-local-variable 'adaptive-fill-regexp)
  (make-local-variable 'fill-paragraph-handle-comment)

  (setq c-buffer-is-cc-mode mode)

  ;; Prepare for the use of `electric-pair-mode'.  Note: if this mode is not
  ;; yet loaded, `electric-pair-inhibit-predicate' will get set from an
  ;; `eval-after-load' form in `c-initialize-cc-mode' when elec-pair.elc is
  ;; loaded.
  (when (boundp 'electric-pair-inhibit-predicate)
    (make-local-variable 'electric-pair-inhibit-predicate)
    (setq electric-pair-inhibit-predicate
	  #'c-electric-pair-inhibit-predicate))

  ;; now set their values
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'indent-line-function) 'c-indent-line)
  (set (make-local-variable 'indent-region-function) 'c-indent-region)
  (set (make-local-variable 'normal-auto-fill-function) 'c-do-auto-fill)
  (set (make-local-variable 'comment-multi-line) t)
  (set (make-local-variable 'comment-line-break-function)
       'c-indent-new-comment-line)

  ;; Prevent time-wasting activity on C-y.
  (when (boundp 'yank-handled-properties)
    (make-local-variable 'yank-handled-properties)
    (let ((yank-cat-handler (assq 'category yank-handled-properties)))
      (when yank-cat-handler
	(setq yank-handled-properties (remq yank-cat-handler
					    yank-handled-properties)))))

  ;; For the benefit of adaptive fill, which otherwise mis-fills.
  (setq fill-paragraph-handle-comment nil)

  ;; Install `c-fill-paragraph' on `fill-paragraph-function' so that a
  ;; direct call to `fill-paragraph' behaves better.  This still
  ;; doesn't work with filladapt but it's better than nothing.
  (set (make-local-variable 'fill-paragraph-function) 'c-fill-paragraph)

  ;; Initialize the three literal sub-caches.
  (c-truncate-lit-pos-cache 1)
  ;; Initialize the cache of brace pairs, and opening braces/brackets/parens.
  (c-state-cache-init)
  ;; Initialize the "brace stack" cache.
  (c-init-bs-cache)

  (when (or c-recognize-<>-arglists
	    (c-major-mode-is 'awk-mode)
	    (c-major-mode-is '(java-mode c-mode c++-mode objc-mode pike-mode)))
    ;; We'll use the syntax-table text property to change the syntax
    ;; of some chars for this language, so do the necessary setup for
    ;; that.
    ;;
    ;; Note to other package developers: It's ok to turn this on in CC
    ;; Mode buffers when CC Mode doesn't, but it's not ok to turn it
    ;; off if CC Mode has turned it on.

    ;; Emacs.
    (when (boundp 'parse-sexp-lookup-properties)
      (set (make-local-variable 'parse-sexp-lookup-properties) t))

    ;; Same as above for XEmacs.
    (when (boundp 'lookup-syntax-properties)
      (set (make-local-variable 'lookup-syntax-properties) t)))

  ;; Use this in Emacs 21+ to avoid meddling with the rear-nonsticky
  ;; property on each character.
  (when (boundp 'text-property-default-nonsticky)
    (make-local-variable 'text-property-default-nonsticky)
    (mapc (lambda (tprop)
	    (unless (assq tprop text-property-default-nonsticky)
	      (setq text-property-default-nonsticky
                    (cons `(,tprop . t) text-property-default-nonsticky))))
	  '(syntax-table c-fl-syn-tab category c-type)))

  ;; In Emacs 21 and later it's possible to turn off the ad-hoc
  ;; heuristic that open parens in column 0 are defun starters.  Since
  ;; we have c-state-cache, that heuristic isn't useful and only causes
  ;; trouble, so turn it off.
;; 2006/12/17: This facility is somewhat confused, and doesn't really seem
;; helpful.  Comment it out for now.
;;   (when (memq 'col-0-paren c-emacs-features)
;;     (make-local-variable 'open-paren-in-column-0-is-defun-start)
;;     (setq open-paren-in-column-0-is-defun-start nil))

  (c-clear-found-types)

  ;; now set the mode style based on default-style
  (let ((style (cc-choose-style-for-mode mode default-style)))
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

  ;; setup the comment indent variable in an Emacs version portable way
  (set (make-local-variable 'comment-indent-function) 'c-comment-indent)
  ;; What sort of comments are default for M-;?
  (setq c-block-comment-flag c-block-comment-is-default)

  ;; In Emacs 24.4 onwards, prevent Emacs's built in electric indentation from
  ;; messing up CC Mode's, and set `c-electric-flag' if `electric-indent-mode'
  ;; has been called by the user.
  (when (boundp 'electric-indent-inhibit) (setq electric-indent-inhibit t))
  ;; CC-mode should obey Emacs's generic preferences, tho only do it if
  ;; Emacs's generic preferences can be set per-buffer (Emacs>=24.4).
  (when (fboundp 'electric-indent-local-mode)
    (setq c-electric-flag electric-indent-mode))

;;   ;; Put submode indicators onto minor-mode-alist, but only once.
;;   (or (assq 'c-submode-indicators minor-mode-alist)
;;       (setq minor-mode-alist
;; 	    (cons '(c-submode-indicators c-submode-indicators)
;; 		  minor-mode-alist)))
  (c-update-modeline)

  ;; Install the functions that ensure that various internal caches
  ;; don't become invalid due to buffer changes.
  (when (featurep 'xemacs)
    (make-local-hook 'before-change-functions)
    (make-local-hook 'after-change-functions))
  (add-hook 'before-change-functions 'c-before-change nil t)
  (setq c-just-done-before-change nil)
  ;; FIXME: We should use the new `depth' arg in Emacs-27 (e.g. a depth of -10
  ;; would do since font-lock uses a(n implicit) depth of 0) so we don't need
  ;; c-after-font-lock-init.
  (add-hook 'after-change-functions 'c-after-change nil t)
  (when (boundp 'font-lock-extend-after-change-region-function)
    (set (make-local-variable 'font-lock-extend-after-change-region-function)
         'c-extend-after-change-region))) ; Currently (2009-05) used by all
                          ; languages with #define (C, C++,; ObjC), and by AWK.

(defun c-setup-doc-comment-style ()
  "Initialize the variables that depend on the value of `c-doc-comment-style'."
  (when (and (featurep 'font-lock)
	     (symbol-value 'font-lock-mode))
    ;; Force font lock mode to reinitialize itself.
    (font-lock-mode 0)
    (font-lock-mode 1)))

;; Buffer local variables defining the region to be fontified by a font lock
;; after-change function.  They are initialized in c-before-change to
;; before-change-functions' BEG and END.  `c-new-END' is amended in
;; c-after-change with after-change-functions' BEG, END, and OLD-LEN.  These
;; variables may be modified by any before/after-change function, in
;; particular by functions in `c-get-state-before-change-functions' and
;; `c-before-font-lock-functions'.
(defvar c-new-BEG 0)
(make-variable-buffer-local 'c-new-BEG)
(defvar c-new-END 0)
(make-variable-buffer-local 'c-new-END)

;; Buffer local variable which notes the value of calling `c-in-literal' just
;; before a change.  It is one of 'string, 'c, 'c++ (for the two sorts of
;; comments), or nil.
(defvar c-old-END-literality nil)
(make-variable-buffer-local 'c-old-END-literality)

(defun c-common-init (&optional mode)
  "Common initialization for all CC Mode modes.
In addition to the work done by `c-basic-common-init' and
`c-font-lock-init', this function sets up various other things as
customary in CC Mode modes but which aren't strictly necessary for CC
Mode to operate correctly.

MODE is the symbol for the mode to initialize, like `c-mode'.  See
`c-basic-common-init' for details.  It's only optional to be
compatible with old code; callers should always specify it."

  (unless mode
    ;; Called from an old third party package.  The fallback is to
    ;; initialize for C.
    (c-init-language-vars-for 'c-mode))

  (c-basic-common-init mode c-default-style)
  (when mode
    ;; Only initialize font locking if we aren't called from an old package.
    (c-font-lock-init))

  ;; Starting a mode is a sort of "change".  So call the change functions...
  (save-restriction
    (widen)
    (setq c-new-BEG (point-min))
    (setq c-new-END (point-max))
    (save-excursion
      (let (before-change-functions after-change-functions)
	(mapc (lambda (fn)
		(funcall fn (point-min) (point-max)))
	      c-get-state-before-change-functions)
	(mapc (lambda (fn)
		(funcall fn (point-min) (point-max)
			 (- (point-max) (point-min))))
	      c-before-font-lock-functions))))

  (set (make-local-variable 'outline-regexp) "[^#\n\^M]")
  (set (make-local-variable 'outline-level) 'c-outline-level)
  (set (make-local-variable 'add-log-current-defun-function)
       (lambda ()
	 (or (c-cpp-define-name) (car (c-defun-name-and-limits nil)))))
  (let ((rfn (assq mode c-require-final-newline)))
    (when rfn
      (if (boundp 'mode-require-final-newline)
          (and (cdr rfn)
               (set (make-local-variable 'require-final-newline)
                    mode-require-final-newline))
        (set (make-local-variable 'require-final-newline) (cdr rfn))))))

(defun c-count-cfss (lv-alist)
  ;; LV-ALIST is an alist like `file-local-variables-alist'.  Count how many
  ;; elements with the key `c-file-style' there are in it.
  (let ((elt-ptr lv-alist) elt (cownt 0))
    (while elt-ptr
      (setq elt (car elt-ptr)
	    elt-ptr (cdr elt-ptr))
      (when (eq (car elt) 'c-file-style)
	(setq cownt (1+ cownt))))
    cownt))

(defun c-before-hack-hook ()
  "Set the CC Mode style and \"offsets\" when in the buffer's local variables.
They are set only when, respectively, the pseudo variables
`c-file-style' and `c-file-offsets' are present in the list.

This function is called from the hook `before-hack-local-variables-hook'."
  (when c-buffer-is-cc-mode
    (let ((mode-cons (assq 'mode file-local-variables-alist))
	  (stile (cdr (assq 'c-file-style file-local-variables-alist)))
	  (offsets (cdr (assq 'c-file-offsets file-local-variables-alist))))
      (when mode-cons
	(hack-one-local-variable (car mode-cons) (cdr mode-cons))
	(setq file-local-variables-alist
	      (delq mode-cons file-local-variables-alist)))
      (when stile
	(or (stringp stile) (error "c-file-style is not a string"))
	(if (boundp 'dir-local-variables-alist)
	    ;; Determine whether `c-file-style' was set in the file's local
	    ;; variables or in a .dir-locals.el (a directory setting).
	    (let ((cfs-in-file-and-dir-count
		   (c-count-cfss file-local-variables-alist))
		  (cfs-in-dir-count (c-count-cfss dir-local-variables-alist)))
	      (c-set-style stile
			   (and (= cfs-in-file-and-dir-count cfs-in-dir-count)
				'keep-defaults)))
	  (c-set-style stile)))
      (when offsets
	(mapc
	 (lambda (langentry)
	   (let ((langelem (car langentry))
		 (offset (cdr langentry)))
	     (c-set-offset langelem offset)))
	 offsets)))))

(defun c-remove-any-local-eval-or-mode-variables ()
  ;; If the buffer specifies `mode' or `eval' in its File Local Variable list
  ;; or on the first line, remove all occurrences.  See
  ;; `c-postprocess-file-styles' for justification.  There is no need to save
  ;; point here, or even bother too much about the buffer contents.  However,
  ;; DON'T mess up the kill-ring.
  ;;
  ;; Most of the code here is derived from Emacs 21.3's `hack-local-variables'
  ;; in files.el.
  (goto-char (point-max))
  (search-backward "\n\^L" (max (- (point-max) 3000) (point-min)) 'move)
  (let (lv-point (prefix "") (suffix ""))
    (when (let ((case-fold-search t))
	    (search-forward "Local Variables:" nil t))
      (setq lv-point (point))
      ;; The prefix is what comes before "local variables:" in its line.
      ;; The suffix is what comes after "local variables:" in its line.
      (skip-chars-forward " \t")
      (or (eolp)
	  (setq suffix (buffer-substring (point)
					 (progn (end-of-line) (point)))))
      (goto-char (match-beginning 0))
      (or (bolp)
	  (setq prefix
		(buffer-substring (point)
				  (progn (beginning-of-line) (point)))))

      (while (search-forward-regexp
	      (concat "^[ \t]*"
		      (regexp-quote prefix)
		      "\\(mode\\|eval\\):.*"
		      (regexp-quote suffix)
		      "$")
	      nil t)
	(forward-line 0)
	(delete-region (point) (progn (forward-line) (point)))))

    ;; Delete the first line, if we've got one, in case it contains a mode spec.
    (unless (and lv-point
		 (progn (goto-char lv-point)
			(forward-line 0)
			(bobp)))
      (goto-char (point-min))
      (unless (eobp)
	(delete-region (point) (progn (forward-line) (point)))))))

(defun c-postprocess-file-styles ()
  "Function that post processes relevant file local variables in CC Mode.
Currently, this function simply applies any style and offset settings
found in the file's Local Variable list.  It first applies any style
setting found in `c-file-style', then it applies any offset settings
it finds in `c-file-offsets'.

Note that the style variables are always made local to the buffer."

  ;; apply file styles and offsets
  (when c-buffer-is-cc-mode
    (if (or c-file-style c-file-offsets)
	(c-make-styles-buffer-local t))
    (when c-file-style
      (or (stringp c-file-style)
	  (error "c-file-style is not a string"))
      (c-set-style c-file-style))

    (and c-file-offsets
	 (mapc
	  (lambda (langentry)
	    (let ((langelem (car langentry))
		  (offset (cdr langentry)))
	      (c-set-offset langelem offset)))
	  c-file-offsets))
    ;; Problem: The file local variable block might have explicitly set a
    ;; style variable.  The `c-set-style' or `mapcar' call might have
    ;; overwritten this.  So we run `hack-local-variables' again to remedy
    ;; this.  There are no guarantees this will work properly, particularly as
    ;; we have no control over what the other hook functions on
    ;; `hack-local-variables-hook' would have done.  We now (2006/2/1) remove
    ;; any `eval' or `mode' expressions before we evaluate again (see below).
    ;; ACM, 2005/11/2.
    ;;
    ;; Problem (bug reported by Gustav Broberg): if one of the variables is
    ;; `mode', this will invoke c-mode (etc.) again, setting up the style etc.
    ;; We prevent this by temporarily removing `mode' from the Local Variables
    ;; section.
    (if (or c-file-style c-file-offsets)
	(let ((hack-local-variables-hook nil) (inhibit-read-only t))
	  (c-tentative-buffer-changes
	    (c-remove-any-local-eval-or-mode-variables)
	    (hack-local-variables))
	  nil))))

(if (boundp 'before-hack-local-variables-hook)
    (add-hook 'before-hack-local-variables-hook 'c-before-hack-hook)
  (add-hook 'hack-local-variables-hook 'c-postprocess-file-styles))

(defmacro c-run-mode-hooks (&rest hooks)
  ;; Emacs 21.1 has introduced a system with delayed mode hooks that
  ;; requires the use of the new function `run-mode-hooks'.
  (if (cc-bytecomp-fboundp 'run-mode-hooks)
      `(run-mode-hooks ,@hooks)
    `(progn ,@(mapcar (lambda (hook) `(run-hooks ,hook)) hooks))))


;;; Change hooks, linking with Font Lock and electric-indent-mode.
(defun c-called-from-text-property-change-p ()
  ;; Is the primitive which invoked `before-change-functions' or
  ;; `after-change-functions' one which merely changes text properties?  This
  ;; function must be called directly from a member of one of the above hooks.
  ;;
  ;; In the following call, frame 0 is `backtrace-frame', frame 1 is
  ;; `c-called-from-text-property-change-p', frame 2 is
  ;; `c-before/after-change', frame 3 is the primitive invoking the change
  ;; hook.
  (memq (cadr (backtrace-frame 3))
	'(put-text-property remove-list-of-text-properties)))

(defun c-depropertize-CPP (beg end)
  ;; Remove the punctuation syntax-table text property from the CPP parts of
  ;; (c-new-BEG c-new-END), and remove all syntax-table properties from any
  ;; raw strings within these CPP parts.
  ;;
  ;; This function is in the C/C++/ObjC values of
  ;; `c-get-state-before-change-functions' and is called exclusively as a
  ;; before change function.
  (c-save-buffer-state (m-beg ss-found)
    (goto-char c-new-BEG)
    (while (and (< (point) beg)
		(search-forward-regexp c-anchored-cpp-prefix beg 'bound))
      (goto-char (match-beginning 1))
      (setq m-beg (point))
      (c-end-of-macro)
      (when (c-major-mode-is 'c++-mode)
	(save-excursion (c-depropertize-raw-strings-in-region m-beg (point))))
      (c-clear-char-property-with-value m-beg (point) 'syntax-table '(1)))

    (while (and (< (point) end)
		(setq ss-found
		      (search-forward-regexp c-anchored-cpp-prefix end 'bound)))
      (goto-char (match-beginning 1))
      (setq m-beg (point))
      (c-end-of-macro))
    (when (and ss-found (> (point) end))
      (when (c-major-mode-is 'c++-mode)
	(save-excursion (c-depropertize-raw-strings-in-region m-beg (point))))
      (c-clear-char-property-with-value m-beg (point) 'syntax-table '(1)))

    (while (and (< (point) c-new-END)
		(search-forward-regexp c-anchored-cpp-prefix c-new-END 'bound))
      (goto-char (match-beginning 1))
      (setq m-beg (point))
      (c-end-of-macro)
      (when (c-major-mode-is 'c++-mode)
	(save-excursion (c-depropertize-raw-strings-in-region m-beg (point))))
      (c-clear-char-property-with-value
       m-beg (point) 'syntax-table '(1)))))

(defun c-extend-region-for-CPP (_beg _end)
  ;; Adjust `c-new-BEG', `c-new-END' respectively to the beginning and end of
  ;; any preprocessor construct they may be in.
  ;;
  ;; Point is undefined both before and after this function call; the buffer
  ;; has already been widened, and match-data saved.  The return value is
  ;; meaningless.
  ;;
  ;; This function is in the C/C++/ObjC values of
  ;; `c-get-state-before-change-functions' and is called exclusively as a
  ;; before change function.
  (goto-char c-new-BEG)
  (c-beginning-of-macro)
  (when (< (point) c-new-BEG)
    (setq c-new-BEG (max (point) (c-determine-limit 500 c-new-BEG))))

  (goto-char c-new-END)
  (when (c-beginning-of-macro)
    (c-end-of-macro)
    (or (eobp) (forward-char)))	 ; Over the terminating NL which may be marked
				 ; with a c-cpp-delimiter category property
  (when (> (point) c-new-END)
    (setq c-new-END (min (point) (c-determine-+ve-limit 500 c-new-END)))))

(defun c-depropertize-new-text (beg end _old-len)
  ;; Remove from the new text in (BEG END) any and all text properties which
  ;; might interfere with CC Mode's proper working.
  ;;
  ;; This function is called exclusively as an after-change function.  It
  ;; appears in the value (for all languages) of
  ;; `c-before-font-lock-functions'.  The value of point is undefined both on
  ;; entry and exit, and the return value has no significance.  The parameters
  ;; BEG, END, and OLD-LEN are the standard ones supplied to all after-change
  ;; functions.
  (c-save-buffer-state ()
    (when (> end beg)
      (c-clear-char-properties beg end 'syntax-table)
      (c-clear-char-properties beg end 'c-fl-syn-tab)
      (c-clear-char-properties beg end 'category)
      (c-clear-char-properties beg end 'c-is-sws)
      (c-clear-char-properties beg end 'c-in-sws)
      (c-clear-char-properties beg end 'c-type)
      (c-clear-char-properties beg end 'c-awk-NL-prop))))

(defun c-extend-font-lock-region-for-macros (_begg endd _old-len)
  ;; Extend the region (c-new-BEG c-new-END) to cover all (possibly changed)
  ;; preprocessor macros; The return value has no significance.
  ;;
  ;; Point is undefined on both entry and exit to this function.  The buffer
  ;; will have been widened on entry.
  ;;
  ;; c-new-BEG has already been extended in `c-extend-region-for-CPP' so we
  ;; don't need to repeat the exercise here.
  ;;
  ;; This function is in the C/C++/ObjC value of `c-before-font-lock-functions'.
  (goto-char endd)
  (when (c-beginning-of-macro)
    (c-end-of-macro)
    ;; Determine the region, (c-new-BEG c-new-END), which will get font
    ;; locked.  This restricts the region should there be long macros.
    (setq c-new-END (min (max c-new-END (point))
			 (c-determine-+ve-limit 500 c-new-END)))))

(defun c-neutralize-CPP-line (beg end)
  ;; BEG and END bound a region, typically a preprocessor line.  Put a
  ;; "punctuation" syntax-table property on syntactically obtrusive
  ;; characters, ones which would interact syntactically with stuff outside
  ;; this region.
  ;;
  ;; These are unmatched parens/brackets/braces.  An unclosed comment is
  ;; regarded as valid, NOT obtrusive.  Unbalanced strings are handled
  ;; elsewhere.
  (save-excursion
    (let (s)
      (while
	  (progn
	    (setq s (parse-partial-sexp beg end -1))
	    (cond
	     ((< (nth 0 s) 0)		; found an unmated ),},]
	      (c-put-char-property (1- (point)) 'syntax-table '(1))
	      t)
	     ;; Unbalanced strings are now handled by
	     ;; `c-before-change-check-unbalanced-strings', etc.
	     ;; ((nth 3 s)			; In a string
	     ;;  (c-put-char-property (nth 8 s) 'syntax-table '(1))
	     ;;  t)
	     ((> (nth 0 s) 0)		; In a (,{,[
	      (c-put-char-property (nth 1 s) 'syntax-table '(1))
	      t)
	     (t nil)))))))

(defun c-neutralize-syntax-in-CPP (_begg _endd _old-len)
  ;; "Neutralize" every preprocessor line wholly or partially in the changed
  ;; region.  "Restore" lines which were CPP lines before the change and are
  ;; no longer so.
  ;;
  ;; That is, set syntax-table properties on characters that would otherwise
  ;; interact syntactically with those outside the CPP line(s).
  ;;
  ;; This function is called from an after-change function, BEGG ENDD and
  ;; OLD-LEN being the standard parameters.  It prepares the buffer for font
  ;; locking, hence must get called before `font-lock-after-change-function'.
  ;;
  ;; Point is undefined both before and after this function call, the buffer
  ;; has been widened, and match-data saved.  The return value is ignored.
  ;;
  ;; This function is in the C/C++/ObjC value of `c-before-font-lock-functions'.
  ;;
  ;; Note: SPEED _MATTERS_ IN THIS FUNCTION!!!
  ;;
  ;; This function might make hidden buffer changes.
  (c-save-buffer-state (limits)
    ;; Clear 'syntax-table properties "punctuation":
    ;; (c-clear-char-property-with-value c-new-BEG c-new-END 'syntax-table '(1))
    ;; The above is now done in `c-depropertize-CPP'.

    ;; Add needed properties to each CPP construct in the region.
    (goto-char c-new-BEG)
    (if (setq limits (c-literal-limits)) ; Go past any literal.
	(goto-char (cdr limits)))
    (skip-chars-backward " \t")
    (let ((pps-position (point))  pps-state mbeg)
      (while (and (< (point) c-new-END)
		  (search-forward-regexp c-anchored-cpp-prefix c-new-END t))
	;; If we've found a "#" inside a macro/string/comment, ignore it.
	(unless
	    (or (save-excursion
		  (goto-char (match-beginning 0))
		  (let ((here (point)))
		    (and (save-match-data (c-beginning-of-macro))
			 (< (point) here))))
		(progn
		  (setq pps-state
			(parse-partial-sexp pps-position (point) nil nil pps-state)
			pps-position (point))
		  (or (nth 3 pps-state)	   ; in a string?
		      (and (nth 4 pps-state)
			   (not (eq (nth 7 pps-state) 'syntax-table)))))) ; in a comment?
	  (goto-char (match-beginning 1))
	  (setq mbeg (point))
	  (if (> (c-no-comment-end-of-macro) mbeg)
	      (c-neutralize-CPP-line mbeg (point)) ; "punctuation" properties
	    (forward-line))	      ; no infinite loop with, e.g., "#//"
	  )))))

(defun c-unescaped-nls-in-string-p (&optional quote-pos)
  ;; Return whether unescaped newlines can be inside strings.
  ;;
  ;; QUOTE-POS, if present, is the position of the opening quote of a string.
  ;; Depending on the language, there might be a special character before it
  ;; signifying the validity of such NLs.
  (cond
   ((null c-multiline-string-start-char) nil)
   ((c-characterp c-multiline-string-start-char)
    (and quote-pos
	 (eq (char-before quote-pos) c-multiline-string-start-char)))
   (t t)))

(defun c-multiline-string-start-is-being-detached (end)
  ;; If (e.g.), the # character in Pike is being detached from the string
  ;; opener it applies to, return t.  Else return nil.  END is the argument
  ;; supplied to every before-change function.
  (and (memq (char-after end) c-string-delims)
       (c-characterp c-multiline-string-start-char)
       (eq (char-before end) c-multiline-string-start-char)))

(defun c-pps-to-string-delim (end)
  ;; parse-partial-sexp forward to the next string quote, which is deemed to
  ;; be a closing quote.  Return nil.
  ;;
  ;; We remove string-fence syntax-table text properties from characters we
  ;; pass over.
  (let* ((start (point))
	 (no-st-s `(0 nil nil ?\" nil nil 0 nil ,start nil nil))
	 (st-s `(0 nil nil t nil nil 0 nil ,start nil nil))
	 no-st-pos st-pos
	 )
    (parse-partial-sexp start end nil nil no-st-s 'syntax-table)
    (setq no-st-pos (point))
    (goto-char start)
    (while (progn
	     (parse-partial-sexp (point) end nil nil st-s 'syntax-table)
	     (unless (bobp)
	       (c-clear-char-property (1- (point)) 'syntax-table))
	     (setq st-pos (point))
	     (and (< (point) end)
		  (not (eq (char-before) ?\")))))
    (goto-char (min no-st-pos st-pos))
    nil))

(defun c-multiline-string-check-final-quote ()
  ;; Check that the final quote in the buffer is correctly marked or not with
  ;; a string-fence syntax-table text propery.  The return value has no
  ;; significance.
  (let (pos-ll pos-lt)
    (save-excursion
      (goto-char (point-max))
      (skip-chars-backward "^\"")
      (while
	  (and
	   (not (bobp))
	   (cond
	    ((progn
	       (setq pos-ll (c-literal-limits)
		     pos-lt (c-literal-type pos-ll))
	       (memq pos-lt '(c c++)))
	     ;; In a comment.
	     (goto-char (car pos-ll)))
	    ((save-excursion
	       (backward-char)	; over "
	       (c-is-escaped (point)))
	     ;; At an escaped string.
	     (backward-char)
	     t)
	    (t
	     ;; At a significant "
	     (c-clear-char-property (1- (point)) 'syntax-table)
	     (setq pos-ll (c-literal-limits)
		   pos-lt (c-literal-type pos-ll))
	     nil)))
	(skip-chars-backward "^\""))
      (cond
       ((bobp))
       ((eq pos-lt 'string)
	(c-put-char-property (1- (point)) 'syntax-table '(15)))
       (t nil)))))

(defvar c-fl-syn-tab-region nil)
  ;; Non-nil when a `c-restore-string-fences' is "in force".  It's value is a
  ;; cons of the BEG and END of the region currently "mirroring" the
  ;; c-fl-syn-tab properties as syntax-table properties.

(defun c-clear-string-fences ()
  ;; Clear syntax-table text properties in the region defined by
  ;; `c-cl-syn-tab-region' which are "mirrored" by c-fl-syn-tab text
  ;; properties.  However, any such " character which ends up not being
  ;; balanced by another " is left with a '(1) syntax-table property.
  (when c-fl-syn-tab-region
    (let ((beg (car c-fl-syn-tab-region))
	  (end (cdr c-fl-syn-tab-region))
	  s pos)
      (setq pos beg)
      (while
	  (and
	   (< pos end)
	   (setq pos
		 (c-min-property-position pos end 'c-fl-syn-tab))
	   (< pos end))
	(c-clear-char-property pos 'syntax-table)
	(setq pos (1+ pos)))
      ;; Check we haven't left any unbalanced "s.
      (save-excursion
	(setq pos beg)
	;; Is there already an unbalanced " before BEG?
	(setq pos (c-min-property-position pos end 'c-fl-syn-tab))
	(when (< pos end) (goto-char pos))
	(when (and (save-match-data
		     (c-search-backward-char-property-with-value-on-char
		      'c-fl-syn-tab '(15) ?\"
		      (max (- (point) 500) (point-min))))
		   (not (equal (c-get-char-property (point) 'syntax-table) '(1))))
	  (setq pos (1+ pos)))
	(while (< pos end)
	  (setq pos
		(c-min-property-position pos end 'c-fl-syn-tab))
	  (when (< pos end)
	    (if (memq (char-after pos) c-string-delims)
		(progn
		  ;; Step over the ".
		  (setq s (parse-partial-sexp pos end nil nil nil
					      'syntax-table))
		  ;; Seek a (bogus) matching ".
		  (setq s (parse-partial-sexp (point) end nil nil s
					      'syntax-table))
		  ;; When a bogus matching " is found, do nothing.
		  ;; Otherwise mark the " with 'syntax-table '(1).
		  (unless
		      (and		;(< (point) end)
		       (not (nth 3 s))
		       (c-get-char-property (1- (point)) 'c-fl-syn-tab))
		    (c-put-char-property pos 'syntax-table '(1)))
		  (setq pos (point)))
	      (setq pos (1+ pos))))))
      (setq c-fl-syn-tab-region nil))))

(defun c-restore-string-fences (beg end)
  ;; Restore any syntax-table text properties in the region (BEG END) which
  ;; are "mirrored" by c-fl-syn-tab text properties.
  (let ((pos beg))
    (while
	(and
	 (< pos end)
	 (setq pos
	       (c-min-property-position pos end 'c-fl-syn-tab))
	 (< pos end))
      (c-put-char-property pos 'syntax-table
			   (c-get-char-property pos 'c-fl-syn-tab))
      (setq pos (1+ pos)))
    (setq c-fl-syn-tab-region (cons beg end))))

(defvar c-bc-changed-stringiness nil)
;; Non-nil when, in a before-change function, the deletion of a range of text
;; will change the "stringiness" of the subsequent text.  Only used when
;; `c-multiline-sting-start-char' is a non-nil value which isn't a character.

(defun c-remove-string-fences (&optional here)
  ;; The character after HERE (default point) is either a string delimiter or
  ;; a newline, which is marked with a string fence text property for both
  ;; syntax-table and c-fl-syn-tab.  Remove these properties from that
  ;; character and its matching newline or string delimiter, if any (there may
  ;; not be one if there is a missing newline at EOB).
  (save-excursion
    (if here
	(goto-char here)
      (setq here (point)))
    (cond
     ((memq (char-after) c-string-delims)
      (save-excursion
	(save-match-data
	  (forward-char)
	  (if (and (c-search-forward-char-property 'syntax-table '(15))
		   (memq (char-before) '(?\n ?\r)))
	      (c-clear-syn-tab (1- (point))))))
      (c-clear-syn-tab (point)))
     ((memq (char-after) '(?\n ?\r))
      (save-excursion
	(save-match-data
	  (when (and (c-search-backward-char-property 'syntax-table '(15))
		     (memq (char-after) c-string-delims))
	    (c-clear-syn-tab (point)))))
      (c-clear-syn-tab (point)))
     (t (c-benign-error "c-remove-string-fences: wrong position")))))

(defun c-before-change-check-unbalanced-strings (beg end)
  ;; If BEG or END is inside an unbalanced string, remove the syntax-table
  ;; text property from respectively the start or end of the string.  Also
  ;; extend the region (c-new-BEG c-new-END) as necessary to cope with the
  ;; coming change involving the insertion or deletion of an odd number of
  ;; quotes.
  ;;
  ;; POINT is undefined both at entry to and exit from this function, the
  ;; buffer will have been widened, and match data will have been saved.
  ;;
  ;; This function is called exclusively as a before-change function via
  ;; `c-get-state-before-change-functions'.
  (c-save-buffer-state
      ((end-limits
	(progn
	  (goto-char (if (c-multiline-string-start-is-being-detached end)
			 (1+ end)
		       end))
	  (c-literal-limits)))
       (end-literal-type (and end-limits
                                      (c-literal-type end-limits)))
       (beg-limits
	(progn
	  (goto-char beg)
	  (c-literal-limits)))
       (beg-literal-type (and beg-limits
                                      (c-literal-type beg-limits))))

    ;; It is possible the buffer change will include inserting a string quote.
    ;; This could have the effect of flipping the meaning of any following
    ;; quotes up until the next unescaped EOL.  Also guard against the change
    ;; being the insertion of \ before an EOL, escaping it.
    (cond
     ((c-characterp c-multiline-string-start-char)
      ;; The text about to be inserted might contain a multiline string
      ;; opener.  Set c-new-END after anything which might be affected.
      ;; Go to the end of the putative multiline string.
      (goto-char end)
      (c-pps-to-string-delim (point-max))
      (when (< (point) (point-max))
	(while
	    (and
	     (progn
	       (while
		   (and
		    (c-syntactic-re-search-forward
		     (if c-single-quotes-quote-strings
			 "[\"']\\|\\s|"
		       "\"\\|\\s|")
		     (point-max) t t)
		    (progn
		      (c-clear-syn-tab (1- (point)))
		      (not (memq (char-before) c-string-delims)))))
	       (memq (char-before) c-string-delims))
	     (progn
	       (c-pps-to-string-delim (point-max))
	       (< (point) (point-max))))))
      (setq c-new-END (max (point) c-new-END)))

     (c-multiline-string-start-char
      (setq c-bc-changed-stringiness
	    (not (eq (eq end-literal-type 'string)
		     (eq beg-literal-type 'string))))
      ;; Deal with deletion of backslashes before "s.
      (goto-char end)
      (if (and (looking-at (if c-single-quotes-quote-strings
			       "\\\\*[\"']"
			     "\\\\*\""))
	       (c-is-escaped (point)))
	  (setq c-bc-changed-stringiness (not c-bc-changed-stringiness)))
      (if (eq beg-literal-type 'string)
	  (setq c-new-BEG (min (car beg-limits) c-new-BEG))))

     ((< end (point-max))
      ;; Have we just escaped a newline by deleting characters?
      (if (and (eq end-literal-type 'string)
	       (memq (char-after end) '(?\n ?\r)))
	  (cond
	   ;; Are we escaping a newline by deleting stuff between \ and \n?
	   ((and (> end beg)
		 (c-will-be-escaped end beg end))
	    (c-remove-string-fences end)
	    (goto-char (1+ end)))
	   ;; Are we unescaping a newline by inserting stuff between \ and \n?
	   ((and (eq end beg)
		 (c-is-escaped end))
	    (goto-char (1+ end))) ; To after the NL which is being unescaped.
	   (t
	    (goto-char end)))
	(goto-char end))

      ;; Move to end of logical line (as it will be after the change, or as it
      ;; was before unescaping a NL.)
      (re-search-forward "\\(\\\\\\(.\\|\n\\)\\|[^\\\n\r]\\)*" nil t)
      ;; We're at an EOLL or point-max.
      (if (equal (c-get-char-property (point) 'syntax-table) '(15))
	  (if (memq (char-after) '(?\n ?\r))
	      ;; Normally terminated invalid string.
	      (c-remove-string-fences)
	    ;; Opening " at EOB.
	    (c-clear-syn-tab (1- (point))))
	(when (and (c-search-backward-char-property 'syntax-table '(15) c-new-BEG)
		   (memq (char-after) c-string-delims)) ; Ignore an unterminated raw string's (.
	  ;; Opening " on last line of text (without EOL).
	  (c-remove-string-fences)
	  (setq c-new-BEG (min c-new-BEG (point))))))

     (t (goto-char end)			; point-max
	(when
	    (and
	     (c-search-backward-char-property 'syntax-table '(15) c-new-BEG)
	     (memq (char-after) c-string-delims))
	  (c-remove-string-fences))))

    (unless
	(or (and
	     ;; Don't set c-new-BEG/END if we're in a raw string.
	     (eq beg-literal-type 'string)
	     (c-at-c++-raw-string-opener (car beg-limits)))
	    (and c-multiline-string-start-char
		 (not (c-characterp c-multiline-string-start-char))))
      (when (and (eq end-literal-type 'string)
		 (not (eq (char-before (cdr end-limits)) ?\())
		 (memq (char-after (car end-limits)) c-string-delims)
		 (equal (c-get-char-property (car end-limits) 'syntax-table)
			'(15)))
	(c-remove-string-fences (car end-limits))
	(setq c-new-END (max c-new-END (cdr end-limits))))

      (when (and (eq beg-literal-type 'string)
		 (memq (char-after (car beg-limits)) c-string-delims))
	(c-remove-string-fences (car beg-limits))
	(setq c-new-BEG (min c-new-BEG (car beg-limits)))))))

(defun c-after-change-mark-abnormal-strings (beg end _old-len)
  ;; Mark any unbalanced strings in the region (c-new-BEG c-new-END) with
  ;; string fence syntax-table text properties.
  ;;
  ;; POINT is undefined both at entry to and exit from this function, the
  ;; buffer will have been widened, and match data will have been saved.
  ;;
  ;; This function is called exclusively as an after-change function via
  ;; `c-before-font-lock-functions'.
  (if (and c-multiline-string-start-char
	   (not (c-characterp c-multiline-string-start-char)))
      ;; Only the last " might need to be marked.
      (c-save-buffer-state
	  ((beg-literal-limits
	    (progn (goto-char beg) (c-literal-limits)))
	   (beg-literal-type (c-literal-type beg-literal-limits))
	   end-literal-limits end-literal-type)
	(when (and (eq beg-literal-type 'string)
		   (c-get-char-property (car beg-literal-limits) 'syntax-table))
	  (c-clear-syn-tab (car beg-literal-limits))
	  (setq c-bc-changed-stringiness (not c-bc-changed-stringiness)))
	(setq end-literal-limits (progn (goto-char end) (c-literal-limits))
	      end-literal-type (c-literal-type end-literal-limits))
	;; Deal with the insertion of backslashes before a ".
	(goto-char end)
	(if (and (looking-at "\\\\*\"")
		 (eq (logand (skip-chars-backward "\\\\" beg) 1) 1))
	    (setq c-bc-changed-stringiness (not c-bc-changed-stringiness)))
	(when (eq (eq (eq beg-literal-type 'string)
		      (eq end-literal-type 'string))
		  c-bc-changed-stringiness)
	  (c-multiline-string-check-final-quote)))
    ;; There could be several "s needing marking.
    (c-save-buffer-state
	((cll (progn (goto-char c-new-BEG)
		     (c-literal-limits)))
	 (beg-literal-type (and cll (c-literal-type cll)))
	 (beg-limits
	  (cond
	   ((and (eq beg-literal-type 'string)
		 (c-unescaped-nls-in-string-p (car cll)))
	    (cons
	     (car cll)
	     (progn
	       (goto-char (1+ (car cll)))
	       (search-forward-regexp
		(cdr (assq (char-after (car cll)) c-string-innards-re-alist))
		nil t)
	       (min (1+ (point)) (point-max)))))
	   ((and (null beg-literal-type)
		 (goto-char beg)
		 (and (not (bobp))
		      (eq (char-before) c-multiline-string-start-char))
		 (memq (char-after) c-string-delims))
	    (cons (point)
		  (progn
		    (forward-char)
		    (search-forward-regexp
		     (cdr (assq (char-before) c-string-innards-re-alist)) nil t)
		    (1+ (point)))))
	   (cll)))
	 (end-hwm ; the highest position which could possibly be affected by
		   ; insertion/deletion of string delimiters.
	  (max
	   (progn
	     (goto-char (min (1+ end)	; 1+, in case a NL has become escaped.
			     (point-max)))
	     (re-search-forward "\\(\\\\\\(.\\|\n\\)\\|[^\\\n\r]\\)*"
				nil t)
	     (point))
	   c-new-END))
	 s)
      (goto-char
       (cond ((null beg-literal-type)
	      c-new-BEG)
	     ((eq beg-literal-type 'string)
	      (car beg-limits))
	     (t				; comment
	      (cdr beg-limits))))
      ;; Handle one string each time around the next while loop.
      (while
	  (and
	   (< (point) end-hwm)
	   (progn
	     ;; Skip over any comments before the next string.
	     (while (progn
		      (setq s (parse-partial-sexp (point) end-hwm nil
						  nil s 'syntax-table))
		      (and (< (point) end-hwm)
			   (or (not (nth 3 s))
			       (not (memq (char-before) c-string-delims))))))
	     ;; We're at the start of a string.
	     (and (memq (char-before) c-string-delims)
		  (not (nth 4 s)))))	; Check we're actually out of the
					; comment. not stuck at EOB
	(unless (and (c-major-mode-is 'c++-mode)
		     (c-maybe-re-mark-raw-string))
	  (if (c-unescaped-nls-in-string-p (1- (point)))
	      (looking-at "\\(\\\\\\(.\\|\n\\)\\|[^\"]\\)*")
	    (looking-at (cdr (assq (char-before) c-string-innards-re-alist))))
	  (cond
	   ((memq (char-after (match-end 0)) '(?\n ?\r))
	    (c-put-syn-tab (1- (point)) '(15))
	    (c-put-syn-tab (match-end 0) '(15))
	    (setq c-new-BEG (min c-new-BEG (point))
		  c-new-END (max c-new-END (match-end 0))))
	   ((or (eq (match-end 0) (point-max))
		(eq (char-after (match-end 0)) ?\\)) ; \ at EOB
	    (c-put-syn-tab (1- (point)) '(15))
	    (setq c-new-BEG (min c-new-BEG (point))
		  c-new-END (max c-new-END (match-end 0))) ; Do we need c-new-END?
	    ))
	  (goto-char (min (1+ (match-end 0)) (point-max))))
	(setq s nil)))))

(defun c-after-change-escape-NL-in-string (beg end _old_len)
  ;; If a backslash has just been inserted into a string, and this quotes an
  ;; existing newline, remove the string fence syntax-table text properties
  ;; on what has become the tail of the string.
  ;;
  ;; POINT is undefined both at entry to and exit from this function, the
  ;; buffer will have been widened, and match data will have been saved.
  ;;
  ;; This function is called exclusively as an after-change function via
  ;; `c-before-font-lock-functions'.  In C++ Mode, it should come before
  ;; `c-after-change-unmark-raw-strings' in that lang variable.
  (let (lit-start		       ; Don't calculate this till we have to.
	lim)
    (when
	(and (> end beg)
	     (memq (char-after end) '(?\n ?\r))
	     (c-is-escaped end)
	     (progn (goto-char end)
		    (setq lit-start (c-literal-start)))
	     (memq (char-after lit-start) c-string-delims)
	     (or (not (c-major-mode-is 'c++-mode))
		 (progn
		   (goto-char lit-start)
		   (and (not (and (eq (char-before) ?R)
				  (looking-at c-c++-raw-string-opener-1-re)))
			(not (and (eq (char-after) ?\()
				  (equal (c-get-char-property
					  (point) 'syntax-table)
					 '(15))))))
		 (save-excursion
		   (c-beginning-of-macro))))
      (goto-char (1+ end))		; After the \
      ;; Search forward for EOLL
      (setq lim (re-search-forward "\\(\\\\\\(.\\|\n\\)\\|[^\\\n\r]\\)*"
				   nil t))
      (goto-char (1+ end))
      (when (c-search-forward-char-property-with-value-on-char
	     'syntax-table '(15) ?\" lim)
	(c-remove-string-fences end)
	(c-remove-string-fences (1- (point)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing of quotes.
;;
;; Valid digit separators in numbers will get the syntax-table "punctuation"
;; property, '(1), and also the text property `c-digit-separator' value t.
;;
;; Invalid other quotes (i.e. those not validly bounding a single character,
;; or escaped character) will get the syntax-table "punctuation" property,
;; '(1), too.
;;
;; Note that, for convenience, these properties are applied even inside
;; comments and strings.

(defconst c-maybe-quoted-number-head
  (concat
   "\\(0\\("
       "\\([Xx]\\([[:xdigit:]]\\('[[:xdigit:]]\\|[[:xdigit:]]\\)*'?\\)?\\)"
       "\\|"
       "\\([Bb]\\([01]\\('[01]\\|[01]\\)*'?\\)?\\)"
       "\\|"
       "\\('[0-7]\\|[0-7]\\)*'?"
       "\\)"
   "\\|"
       "[1-9]\\('[0-9]\\|[0-9]\\)*'?"
   "\\)")
  "Regexp matching the head of a numeric literal, including with digit separators.")

(defun c-quoted-number-head-before-point ()
  ;; Return non-nil when the head of a possibly quoted number is found
  ;; immediately before point.  The value returned in this case is the buffer
  ;; position of the start of the head.  That position is also in
  ;; (match-beginning 0).
  (when c-has-quoted-numbers
    (save-excursion
      (let ((here (point))
	    found)
	(skip-chars-backward "[:xdigit:]'")
	(if (and (memq (char-before) '(?x ?X))
		 (eq (char-before (1- (point))) ?0))
	    (backward-char 2))
	(while
	    (and
	     (setq found
		   (search-forward-regexp c-maybe-quoted-number-head here t))
	     (< found here)))
	(and (eq found here) (match-beginning 0))))))

(defconst c-maybe-quoted-number-tail
  (concat
   "\\("
       "\\([xX']?[[:xdigit:]]\\('[[:xdigit:]]\\|[[:xdigit:]]\\)*\\)"
   "\\|"
       "\\([bB']?[01]\\('[01]\\|[01]\\)*\\)"
   "\\|"
       "\\('?[0-9]\\('[0-9]\\|[0-9]\\)*\\)"
   "\\)")
  "Regexp matching the tail of a numeric literal, including with digit separators.
Note that this is a strict tail, so won't match, e.g. \"0x....\".")

(defun c-quoted-number-tail-after-point ()
  ;; Return non-nil when a proper tail of a possibly quoted number is found
  ;; immediately after point.  The value returned in this case is the buffer
  ;; position of the end of the tail.  That position is also in (match-end 0).
  (when c-has-quoted-numbers
    (and (looking-at c-maybe-quoted-number-tail)
	 (match-end 0))))

(defconst c-maybe-quoted-number
  (concat
   "\\(0\\("
       "\\([Xx][[:xdigit:]]\\('[[:xdigit:]]\\|[[:xdigit:]]\\)*\\)"
       "\\|"
       "\\([Bb][01]\\('[01]\\|[01]\\)*\\)"
       "\\|"
       "\\('[0-7]\\|[0-7]\\)*"
       "\\)"
   "\\|"
       "[1-9]\\('[0-9]\\|[0-9]\\)*"
   "\\)")
  "Regexp matching a numeric literal, including with digit separators.")

(defun c-quoted-number-straddling-point ()
  ;; Return non-nil if a definitely quoted number starts before point and ends
  ;; after point.  In this case the number is bounded by (match-beginning 0)
  ;; and (match-end 0).
  (when c-has-quoted-numbers
    (save-excursion
      (let ((here (point))
	    (bound (progn (skip-chars-forward "[:xdigit:]'") (point))))
	(goto-char here)
	(when (< (skip-chars-backward "[:xdigit:]'") 0)
	  (if (and (memq (char-before) '(?x ?X))
		   (eq (char-before (1- (point))) ?0))
	      (backward-char 2))
	  (while (and (search-forward-regexp c-maybe-quoted-number bound t)
		      (<= (match-end 0) here)))
	  (and (< (match-beginning 0) here)
	       (> (match-end 0) here)
	       (save-match-data
		 (goto-char (match-beginning 0))
		 (save-excursion (search-forward "'" (match-end 0) t)))))))))

(defun c-parse-quotes-before-change (_beg _end)
  ;; This function analyzes 's near the region (c-new-BEG c-new-END), amending
  ;; those two variables as needed to include 's into that region when they
  ;; might be syntactically relevant to the change in progress.
  ;;
  ;; Having amended that region, the function removes pertinent text
  ;; properties (syntax-table properties with value '(1) and c-digit-separator
  ;; props with value t) from 's in it.  This operation is performed even
  ;; within strings and comments.
  ;;
  ;; This function is called exclusively as a before-change function via the
  ;; variable `c-get-state-before-change-functions'.
  (c-save-buffer-state (case-fold-search)
    (goto-char c-new-BEG)
    ;; We need to scan for 's from the BO (logical) line.
    (beginning-of-line)
    (while (eq (char-before (1- (point))) ?\\)
      (beginning-of-line 0))
    (while (and (< (point) c-new-BEG)
		(search-forward "'" c-new-BEG t))
      (cond
       ((c-quoted-number-straddling-point)
	(goto-char (match-end 0))
	(if (> (match-end 0) c-new-BEG)
	    (setq c-new-BEG (match-beginning 0))))
       ((c-quoted-number-head-before-point)
	(if (>= (point) c-new-BEG)
	    (setq c-new-BEG (match-beginning 0))))
       ((looking-at
	 "\\([^'\\]\\|\\\\\\([0-7]\\{1,3\\}\\|[xuU][[:xdigit:]]+\\|.\\)\\)'")
	(goto-char (match-end 0))
	(if (> (match-end 0) c-new-BEG)
	    (setq c-new-BEG (1- (match-beginning 0)))))
       ((looking-at "\\\\'")
	(setq c-new-BEG (min c-new-BEG (1- (point))))
	(goto-char (match-end 0)))
       ((save-excursion
	  (not (search-forward "'" c-new-BEG t)))
	(setq c-new-BEG (min c-new-BEG (1- (point)))))
       (t nil)))

    (goto-char c-new-END)
    ;; We will scan from the BO (logical) line.
    (beginning-of-line)
    (while (eq (char-before (1- (point))) ?\\)
      (beginning-of-line 0))
    (while (and (< (point) c-new-END)
		(search-forward "'" c-new-END t))
      (cond
       ((c-quoted-number-straddling-point)
	(goto-char (match-end 0))
	(if (> (match-end 0) c-new-END)
	    (setq c-new-END (match-end 0))))
       ((c-quoted-number-tail-after-point)
	(goto-char (match-end 0))
	(if (> (match-end 0) c-new-END)
	    (setq c-new-END (match-end 0))))
       ((looking-at
	 "\\([^'\\]\\|\\\\\\([0-7]\\{1,3\\}\\|[xuU][[:xdigit:]]+\\|.\\)\\)'")
	(goto-char (match-end 0))
	(if (> (match-end 0) c-new-END)
	    (setq c-new-END (match-end 0))))
       ((looking-at "\\\\'")
	(goto-char (match-end 0))
	(setq c-new-END (max c-new-END (point))))
       ((equal (c-get-char-property (1- (point)) 'syntax-table) '(1))
	(when (c-search-forward-char-property-with-value-on-char
	       'syntax-table '(1) ?\' (c-point 'eoll))
	  (setq c-new-END (max (point) c-new-END))))
       (t nil)))
    ;; Having reached c-new-END, handle any 's after it whose context may be
    ;; changed by the current buffer change.  The idea is to catch
    ;; monstrosities like ',',',',',' changing "polarity".
    (goto-char c-new-END)
    (cond
     ((c-quoted-number-tail-after-point)
      (setq c-new-END (match-end 0)))
     ((looking-at
       "\\(\\\\\\([0-7]\\{1,3\\}\\|[xuU][[:xdigit:]]+\\|.\\)\\|.\\)?\
\\('\\([^'\\]\\|\\\\\\([0-7]\\{1,3\\}\\|[xuU][[:xdigit:]]+\\|.\\)\\)\\)*'")
      (setq c-new-END (match-end 0))))

    ;; Remove the '(1) syntax-table property from any "'"s within (c-new-BEG
    ;; c-new-END).
    (goto-char c-new-BEG)
    (when (c-search-forward-char-property-with-value-on-char
	   'syntax-table '(1) ?\' c-new-END)
      (c-invalidate-state-cache (1- (point)))
      (c-truncate-lit-pos-cache (1- (point)))
      (c-clear-char-property-with-value-on-char
       (1- (point)) c-new-END
       'syntax-table '(1)
       ?')
      ;; Remove the c-digit-separator text property from the same "'"s.
      (when c-has-quoted-numbers
	(c-clear-char-property-with-value-on-char
	 (1- (point)) c-new-END
	 'c-digit-separator t
	 ?')))))

(defun c-parse-quotes-after-change (_beg _end _old-len)
  ;; This function applies syntax-table properties (value '(1)) and
  ;; c-digit-separator properties as needed to 's within the range (c-new-BEG
  ;; c-new-END).  This operation is performed even within strings and
  ;; comments.
  ;;
  ;; This function is called exclusively as an after-change function via the
  ;; variable `c-before-font-lock-functions'.
  (c-save-buffer-state (num-beg num-end case-fold-search)
    ;; Apply the needed syntax-table and c-digit-separator text properties to
    ;; quotes.
    (save-restriction
      (goto-char c-new-BEG)
      (while (and (< (point) c-new-END)
		  (search-forward "'" c-new-END 'limit))
	(cond ((c-is-escaped (1- (point)))) ; not a real '.
	      ((c-quoted-number-straddling-point)
	       (setq num-beg (match-beginning 0)
		     num-end (match-end 0))
	       (c-invalidate-state-cache num-beg)
	       (c-truncate-lit-pos-cache num-beg)
	       (c-put-char-properties-on-char num-beg num-end
					      'syntax-table '(1) ?')
	       (c-put-char-properties-on-char num-beg num-end
					      'c-digit-separator t ?')
	       (goto-char num-end))
	      ((looking-at
		"\\([^\\']\\|\\\\\\([0-7]\\{1,3\\}\\|[xuU][[:xdigit:]]+\\|.\\)\
\\)'") ; balanced quoted expression.
	       (goto-char (match-end 0)))
	      ((looking-at "\\\\'")	; Anomalous construct.
	       (c-invalidate-state-cache (1- (point)))
	       (c-truncate-lit-pos-cache (1- (point)))
	       (c-put-char-properties-on-char (1- (point)) (+ (point) 2)
					      'syntax-table '(1) ?')
	       (goto-char (match-end 0)))
	      (t
	       (c-invalidate-state-cache (1- (point)))
	       (c-truncate-lit-pos-cache (1- (point)))
	       (c-put-char-property (1- (point)) 'syntax-table '(1))))
	;; Prevent the next `c-quoted-number-straddling-point' getting
	;; confused by already processed single quotes.
	(narrow-to-region (point) (point-max))))))

(defun c-before-change (beg end)
  ;; Function to be put on `before-change-functions'.  Primarily, this calls
  ;; the language dependent `c-get-state-before-change-functions'.  It is
  ;; otherwise used only to remove stale entries from the `c-found-types'
  ;; cache, and to record entries which a `c-after-change' function might
  ;; confirm as stale.
  ;;
  ;; Note that this function must be FAST rather than accurate.  Note
  ;; also that it only has any effect when font locking is enabled.
  ;; We exploit this by checking for font-lock-*-face instead of doing
  ;; rigorous syntactic analysis.

  ;; If either change boundary is wholly inside an identifier, delete
  ;; it/them from the cache.  Don't worry about being inside a string
  ;; or a comment - "wrongly" removing a symbol from `c-found-types'
  ;; isn't critical.
  (unless (c-called-from-text-property-change-p)
    (save-restriction
      (widen)
      (if c-just-done-before-change
	  ;; We have two consecutive calls to `before-change-functions' without
	  ;; an intervening `after-change-functions'.  An example of this is bug
	  ;; #38691.  To protect CC Mode, assume that the entire buffer has
	  ;; changed.
	  (setq beg (point-min)
		end (point-max)
		c-just-done-before-change 'whole-buffer)
	(setq c-just-done-before-change t))
      ;; (c-new-BEG c-new-END) will be the region to fontify.
      (setq c-new-BEG beg  c-new-END end)
      (setq c-maybe-stale-found-type nil)
      ;; A workaround for syntax-ppss's failure to notice syntax-table text
      ;; property changes.
      (when (fboundp 'syntax-ppss)
	(setq c-syntax-table-hwm most-positive-fixnum))
      (save-match-data
	(widen)
	(unwind-protect
	    (progn
	      (c-restore-string-fences (point-min) (point-max))
	      (save-excursion
		;; Are we inserting/deleting stuff in the middle of an
		;; identifier?
		(c-unfind-enclosing-token beg)
		(c-unfind-enclosing-token end)
		;; Are we coalescing two tokens together, e.g. "fo o"
		;; -> "foo"?
		(when (< beg end)
		  (c-unfind-coalesced-tokens beg end))
		(c-invalidate-sws-region-before beg end)
		;; Are we (potentially) disrupting the syntactic
		;; context which makes a type a type?  E.g. by
		;; inserting stuff after "foo" in "foo bar;", or
		;; before "foo" in "typedef foo *bar;"?
		;;
		;; We search for appropriate c-type properties "near"
		;; the change.  First, find an appropriate boundary
		;; for this property search.
		(let (lim
		      type type-pos
		      marked-id term-pos
		      (end1
		       (or (and (eq (get-text-property end 'face)
				    'font-lock-comment-face)
				(previous-single-property-change end 'face))
			   end)))
		  (when (>= end1 beg) ; Don't hassle about changes entirely in
					; comments.
		    ;; Find a limit for the search for a `c-type' property
		    (while
			(and (/= (skip-chars-backward "^;{}") 0)
			     (> (point) (point-min))
			     (memq (c-get-char-property (1- (point)) 'face)
				   '(font-lock-comment-face font-lock-string-face))))
		    (setq lim (max (point-min) (1- (point))))

		    ;; Look for the latest `c-type' property before end1
		    (when (and (> end1 (point-min))
			       (setq type-pos
				     (if (get-text-property (1- end1) 'c-type)
					 end1
				       (previous-single-property-change end1 'c-type
									nil lim))))
		      (setq type (get-text-property (max (1- type-pos) lim) 'c-type))

		      (when (memq type '(c-decl-id-start c-decl-type-start))
			;; Get the identifier, if any, that the property is on.
			(goto-char (1- type-pos))
			(setq marked-id
			      (when (looking-at "\\(\\sw\\|\\s_\\)")
				(c-beginning-of-current-token)
				(buffer-substring-no-properties (point) type-pos)))

			(goto-char end1)
			(skip-chars-forward "^;{}") ; FIXME!!!  loop for
					; comment, maybe
			(setq lim (point))
			(setq term-pos
			      (or (c-next-single-property-change end 'c-type nil lim) lim))
			(setq c-maybe-stale-found-type
			      (list type marked-id
				    type-pos term-pos
				    (buffer-substring-no-properties type-pos
								    term-pos)
				    (buffer-substring-no-properties beg end)))))))

		(if c-get-state-before-change-functions
		    (mapc (lambda (fn)
			    (funcall fn beg end))
			  c-get-state-before-change-functions))))
	  (c-clear-string-fences))))
    (c-truncate-lit-pos-cache beg)
    ;; The following must be done here rather than in `c-after-change'
    ;; because newly inserted parens would foul up the invalidation
    ;; algorithm.
    (c-invalidate-state-cache beg)))

(defvar c-in-after-change-fontification nil)
(make-variable-buffer-local 'c-in-after-change-fontification)
;; A flag to prevent region expanding stuff being done twice for after-change
;; fontification.

(defun c-after-change (beg end old-len)
  ;; Function put on `after-change-functions' to adjust various caches
  ;; etc.  Prefer speed to finesse here, since there will be an order
  ;; of magnitude more calls to this function than any of the
  ;; functions that use the caches.
  ;;
  ;; Note that care must be taken so that this is called before any
  ;; font-lock callbacks since we might get calls to functions using
  ;; these caches from inside them, and we must thus be sure that this
  ;; has already been executed.
  ;;
  ;; This calls the language variable c-before-font-lock-functions, if non-nil.
  ;; This typically sets `syntax-table' properties.

  ;; We can sometimes get two consecutive calls to `after-change-functions'
  ;; without an intervening call to `before-change-functions' when reverting
  ;; the buffer (see bug #24094).  Whatever the cause, assume that the entire
  ;; buffer has changed.

  ;; Note: c-just-done-before-change is nil, t, or 'whole-buffer.
  (unless (c-called-from-text-property-change-p)
    (unless (eq c-just-done-before-change t)
      (save-restriction
	(widen)
	(when (null c-just-done-before-change)
	  (c-before-change (point-min) (point-max)))
	(setq beg (point-min)
	      end (point-max)
	      old-len (- end beg)
	      c-new-BEG (point-min)
	      c-new-END (point-max)))))

  ;; (c-new-BEG c-new-END) will be the region to fontify.  It may become
  ;; larger than (beg end).
  (setq c-new-END (- (+ c-new-END (- end beg)) old-len))

  (unless (c-called-from-text-property-change-p)
    (setq c-just-done-before-change nil)
    (c-save-buffer-state (case-fold-search)
      ;; When `combine-after-change-calls' is used we might get calls
      ;; with regions outside the current narrowing.  This has been
      ;; observed in Emacs 20.7.
      (save-restriction
	(save-match-data	  ; c-recognize-<>-arglists changes match-data
	  (widen)
	  (unwind-protect
	      (progn
		(c-restore-string-fences (point-min) (point-max))
		(when (> end (point-max))
		  ;; Some emacsen might return positions past the end. This
		  ;; has been observed in Emacs 20.7 when rereading a buffer
		  ;; changed on disk (haven't been able to minimize it, but
		  ;; Emacs 21.3 appears to work).
		  (setq end (point-max))
		  (when (> beg end)
		    (setq beg end)))

		;; C-y is capable of spuriously converting category
		;; properties c-</>-as-paren-syntax and
		;; c-cpp-delimiter into hard syntax-table properties.
		;; Remove these when it happens.
		(when (eval-when-compile (memq 'category-properties c-emacs-features))
		  (c-save-buffer-state ()
		    (c-clear-char-property-with-value beg end 'syntax-table
						      c-<-as-paren-syntax)
		    (c-clear-char-property-with-value beg end 'syntax-table
						      c->-as-paren-syntax)
		    (c-clear-char-property-with-value beg end 'syntax-table nil)))

		(c-trim-found-types beg end old-len) ; maybe we don't
						     ; need all of these.
		(c-invalidate-sws-region-after beg end old-len)
		;; (c-invalidate-state-cache beg) ; moved to
		;; `c-before-change'.
		(c-invalidate-find-decl-cache beg)

		(when c-recognize-<>-arglists
		  (c-after-change-check-<>-operators beg end))

		(setq c-in-after-change-fontification t)
		(save-excursion
		  (mapc (lambda (fn)
			  (funcall fn beg end old-len))
			c-before-font-lock-functions)))
	    (c-clear-string-fences))))))
  ;; A workaround for syntax-ppss's failure to notice syntax-table text
  ;; property changes.
  (when (fboundp 'syntax-ppss)
    (syntax-ppss-flush-cache c-syntax-table-hwm)))

(defun c-doc-fl-decl-start (pos)
  ;; If the line containing POS is in a doc comment continued line (as defined
  ;; by `c-doc-line-join-re'), return the position of the first line of the
  ;; sequence.  Otherwise, return nil.  Point has no significance at entry to
  ;; and exit from this function.
  (when (not (equal c-doc-line-join-re regexp-unmatchable))
    (goto-char pos)
    (back-to-indentation)
    (and (or (looking-at c-comment-start-regexp)
	     (memq (c-literal-type (c-literal-limits)) '(c c++)))
	 (progn
	   (end-of-line)
	   (let ((here (point)))
	     (while (re-search-backward c-doc-line-join-re (c-point 'bopl) t))
	     (and (not (eq (point) here))
		  (c-point 'bol)))))))

(defun c-doc-fl-decl-end (pos)
  ;; If the line containing POS is continued by a doc comment continuation
  ;; marker (as defined by `c-doc-line-join-re), return the position of
  ;; the BOL at the end of the sequence.  Otherwise, return nil.  Point has no
  ;; significance at entry to and exit from this function.
  (when (not (equal c-doc-line-join-re regexp-unmatchable))
    (goto-char pos)
    (back-to-indentation)
    (let ((here (point)))
      (while (re-search-forward c-doc-line-join-re (c-point 'eonl) t))
      (and (not (eq (point) here))
	   (c-point 'bonl)))))

(defun c-fl-decl-start (pos)
  ;; If the beginning of the line containing POS is in the middle of a "local"
  ;; declaration, return the beginning of that declaration.  Otherwise return
  ;; nil.  Note that declarations, in this sense, can be nested.  (A local
  ;; declaration is one which does not start outside of struct braces (and
  ;; similar) enclosing POS.  Brace list braces here are not "similar".
  ;;
  ;; POS being in a literal does not count as being in a declaration (on
  ;; pragmatic grounds).
  ;;
  ;; This function is called indirectly from font locking stuff - either from
  ;; c-after-change (to prepare for after-change font-locking) or from font
  ;; lock context (etc.) fontification.
  (goto-char pos)
  (let ((lit-start (c-literal-start))
	old-pos
	(new-pos pos)
	capture-opener
	bod-lim bo-decl)
    (goto-char (c-point 'bol new-pos))
    (unless lit-start
      (setq bod-lim (c-determine-limit 500))

      ;; In C++ Mode, first check if we are within a (possibly nested) lambda
      ;; form capture list.
      (when (c-major-mode-is 'c++-mode)
	(save-excursion
	  (while (and (c-go-up-list-backward nil bod-lim)
		      (c-looking-at-c++-lambda-capture-list))
	    (setq capture-opener (point)))))

      (while
	  ;; Go to a less nested declaration each time round this loop.
	  (and
	   (setq old-pos (point))
	   (let (pseudo)
	     (while
		 (progn
		   (c-syntactic-skip-backward "^;{}" bod-lim t)
		   (and (eq (char-before) ?})
			(save-excursion
			  (backward-char)
			  (setq pseudo (c-cheap-inside-bracelist-p (c-parse-state))))))
	       (goto-char pseudo))
	     t)
	   (> (point) bod-lim)
	   (progn (c-forward-syntactic-ws)
		  ;; Have we got stuck in a comment at EOB?
		  (not (and (eobp)
			    (c-literal-start))))
	   (< (point) old-pos)
	   (progn (setq bo-decl (point))
		  (or (not (looking-at c-protection-key))
		      (c-forward-keyword-clause 1)))
	   (progn
	     ;; Are we looking at a keyword such as "template" or
	     ;; "typedef" which can decorate a type, or the type itself?
	     (when (or (looking-at c-prefix-spec-kwds-re)
		       (c-forward-type t))
	       ;; We've found another candidate position.
	       (setq new-pos (min new-pos bo-decl))
	       (goto-char bo-decl))
	     t)
	   ;; Try and go out a level to search again.
	   (progn
	     (c-backward-syntactic-ws bod-lim)
	     (and (> (point) bod-lim)
		  (or (memq (char-before) '(?\( ?\[))
		      (and (eq (char-before) ?\<)
			   (eq (c-get-char-property
				(1- (point)) 'syntax-table)
			       c-<-as-paren-syntax))
		      (and (eq (char-before) ?{)
			   (save-excursion
			     (backward-char)
			     (consp (c-looking-at-or-maybe-in-bracelist))))
		      )))
	   (not (bobp)))
	(backward-char))		; back over (, [, <.
      (when (and capture-opener (< capture-opener new-pos))
	(setq new-pos capture-opener))
      (and (/= new-pos pos) new-pos))))

(defun c-fl-decl-end (pos)
  ;; If POS is inside a declarator, return the end of the token that follows
  ;; the declarator, otherwise return nil.  POS being in a literal does not
  ;; count as being in a declarator (on pragmatic grounds).
  (goto-char pos)
  (let ((lit-start (c-literal-start))
	enclosing-attribute pos1)
    (unless lit-start
      (c-backward-syntactic-ws)
      (when (setq enclosing-attribute (c-slow-enclosing-c++-attribute))
	(goto-char (car enclosing-attribute))) ; Only happens in C++ Mode.
      (when (setq pos1 (c-on-identifier))
	(goto-char pos1)
	(let ((lim (save-excursion
		     (and (c-beginning-of-macro)
			  (progn (c-end-of-macro) (point))))))
	  (when (and (c-forward-declarator lim)
		     (or (not (eq (char-after) ?\())
			 (c-go-list-forward nil lim))
		     (eq (c-forward-token-2 1 nil lim) 0))
	    (c-backward-syntactic-ws)
	    (point)))))))

(defun c-change-expand-fl-region (_beg _end _old-len)
  ;; Expand the region (c-new-BEG c-new-END) to an after-change font-lock
  ;; region.  This will usually be the smallest sequence of whole lines
  ;; containing `c-new-BEG' and `c-new-END', but if `c-new-BEG' is in a
  ;; "local" declaration (see `c-fl-decl-start') the beginning of this is used
  ;; as the lower bound.
  ;;
  ;; This is called from an after-change-function, but the parameters BEG END
  ;; and OLD-LEN are not used.
  (if font-lock-mode
      (setq c-new-BEG
	    (or (c-fl-decl-start c-new-BEG) (c-doc-fl-decl-start c-new-BEG)
		(c-point 'bol c-new-BEG))
	    c-new-END
	    (or (c-fl-decl-end c-new-END) (c-doc-fl-decl-end c-new-END)
		(c-point 'bonl c-new-END)))))

(defun c-context-expand-fl-region (beg end)
  ;; Return a cons (NEW-BEG . NEW-END), where NEW-BEG is the beginning of a
  ;; "local" declaration containing BEG (see `c-fl-decl-start') or BOL BEG is
  ;; in.  NEW-END is beginning of the line after the one END is in.
  (c-save-buffer-state ()
    (cons (or (c-fl-decl-start beg) (c-doc-fl-decl-start beg)
	      (c-point 'bol beg))
	  (or (c-fl-decl-end end) (c-doc-fl-decl-end end)
	      (c-point 'bonl (1- end))))))

(defun c-before-context-fl-expand-region (beg end)
  ;; Expand the region (BEG END) as specified by
  ;; `c-before-context-fontification-functions'.  Return a cons of the bounds
  ;; of the new region.
  (save-restriction
    (widen)
    (save-excursion
      (let ((new-beg beg) (new-end end)
	    (new-region (cons beg end)))
	(mapc (lambda (fn)
		(setq new-region (funcall fn new-beg new-end))
		(setq new-beg (car new-region) new-end (cdr new-region)))
	      c-before-context-fontification-functions)
	new-region))))

(defun c-font-lock-fontify-region (beg end &optional verbose)
  ;; Effectively advice around `font-lock-fontify-region' which extends the
  ;; region (BEG END), for example, to avoid context fontification chopping
  ;; off the start of the context.  Do not extend the region if it's already
  ;; been done (i.e. from an after-change fontification.  An example (C++)
  ;; where the chopping off used to happen is this:
  ;;
  ;;     template <typename T>
  ;;
  ;;
  ;;     void myfunc(T* p) {}
  ;;
  ;; Type a space in the first blank line, and the fontification of the next
  ;; line was fouled up by context fontification.
  (save-restriction
    (widen)
    (let (new-beg new-end new-region case-fold-search string-fence-beg lim)
      ;; Check how far back we need to extend the region where we reapply the
      ;; string fence syntax-table properties.  These must be in place for the
      ;; coming fontification operations.
      (save-excursion
	(goto-char (if c-in-after-change-fontification
		       (min beg c-new-BEG)
		     beg))
	(setq lim (max (- (point) 500) (point-min)))
	(while
	    (progn
	      (skip-chars-backward "^\"" lim)
	      (or (bobp) (backward-char))
	      (save-excursion
		(eq (logand (skip-chars-backward "\\\\") 1) 1))))
	(setq string-fence-beg
	      (cond ((c-get-char-property (point) 'c-fl-syn-tab)
		     (point))
		    (c-in-after-change-fontification
		     c-new-BEG)
		    (t beg)))
	(c-save-buffer-state nil
	  ;; Temporarily reapply the string fence syntax-table properties.
	  (c-with-extended-string-fences
	   string-fence-beg (if c-in-after-change-fontification
				(max end c-new-END)
			      end)

	   (if (and c-in-after-change-fontification
		    (< beg c-new-END) (> end c-new-BEG))
	       ;; Region and the latest after-change fontification region overlap.
	       ;; Determine the upper and lower bounds of our adjusted region
	       ;; separately.
	       (progn
		 (if (<= beg c-new-BEG)
		     (setq c-in-after-change-fontification nil))
		 (setq new-beg
		       (if (and (>= beg (c-point 'bol c-new-BEG))
				(<= beg c-new-BEG))
			   ;; Either jit-lock has accepted `c-new-BEG', or has
			   ;; (probably) extended the change region spuriously
			   ;; to BOL, which position likely has a
			   ;; syntactically different position.  To ensure
			   ;; correct fontification, we start at `c-new-BEG',
			   ;; assuming any characters to the left of
			   ;; `c-new-BEG' on the line do not require
			   ;; fontification.
			   c-new-BEG
			 (setq new-region (c-before-context-fl-expand-region beg end)
			       new-end (cdr new-region))
			 (car new-region)))
		 (setq new-end
		       (if (and (>= end (c-point 'bol c-new-END))
				(<= end c-new-END))
			   c-new-END
			 (or new-end
			     (cdr (c-before-context-fl-expand-region beg end))))))
	     ;; Context (etc.) fontification.
	     (setq new-region (c-before-context-fl-expand-region beg end)
		   new-beg (car new-region)  new-end (cdr new-region)))
	   ;; Finally invoke font lock's functionality.
	   (funcall (default-value 'font-lock-fontify-region-function)
		    new-beg new-end verbose)))))))

(defun c-after-font-lock-init ()
  ;; Put on `font-lock-mode-hook'.  This function ensures our after-change
  ;; function will get executed before the font-lock one.
  (when (memq #'c-after-change after-change-functions)
    (remove-hook 'after-change-functions #'c-after-change t)
    (add-hook 'after-change-functions #'c-after-change nil t)))

(defun c-font-lock-init ()
  "Set up the font-lock variables for using the font-lock support in CC Mode.
This does not load the font-lock package.  Use after
`c-basic-common-init' and after cc-fonts has been loaded.
This function is called from `c-common-init', once per mode initialization."

  (set (make-local-variable 'font-lock-defaults)
	`(,(if (c-major-mode-is 'awk-mode)
	       ;; awk-mode currently has only one font lock level.
	       'awk-font-lock-keywords
	     (mapcar 'c-mode-symbol
		     '("font-lock-keywords" "font-lock-keywords-1"
		       "font-lock-keywords-2" "font-lock-keywords-3")))
	  nil nil
	  ,c-identifier-syntax-modifications
	  c-beginning-of-syntax
	  (font-lock-mark-block-function
	   . c-mark-function)))

  ;; Prevent `font-lock-default-fontify-region' extending the region it will
  ;; fontify to whole lines by removing `font-lock-extend-region-wholelines'
  ;; from `font-lock-extend-region-functions'.  (Emacs only).  This fixes
  ;; Emacs bug #19669.
  (when (boundp 'font-lock-extend-region-functions)
    (setq font-lock-extend-region-functions
	  (delq 'font-lock-extend-region-wholelines
		font-lock-extend-region-functions)))

  (make-local-variable 'font-lock-fontify-region-function)
  (setq font-lock-fontify-region-function 'c-font-lock-fontify-region)

  (if (featurep 'xemacs)
      (make-local-hook 'font-lock-mode-hook))
  (add-hook 'font-lock-mode-hook 'c-after-font-lock-init nil t))

;; Emacs 22 and later.
(defun c-extend-after-change-region (beg end _old-len)
  "Extend the region to be fontified, if necessary."
  ;; Note: the parameter OLD-LEN is ignored here.  This somewhat indirect
  ;; implementation exists because it is minimally different from the
  ;; stand-alone CC Mode which, lacking
  ;; font-lock-extend-after-change-region-function, is forced to use advice
  ;; instead.
  ;;
  ;; Of the seven CC Mode languages, currently (2009-05) only C, C++, Objc
  ;; (the languages with #define) and AWK Mode make non-null use of this
  ;; function.
  (when (eq font-lock-support-mode 'jit-lock-mode)
    (save-restriction
      (widen)
      (c-save-buffer-state () ; Protect the undo-list from put-text-property.
	(if (< c-new-BEG beg)
	    (put-text-property c-new-BEG beg 'fontified nil))
	(if (> c-new-END end)
	    (put-text-property end c-new-END 'fontified nil)))))
  (cons c-new-BEG c-new-END))

;; Emacs < 22 and XEmacs
(defmacro c-advise-fl-for-region (function)
  `(defadvice ,function (before get-awk-region activate)
     ;; Make sure that any string/regexp is completely font-locked.
     (when c-buffer-is-cc-mode
       (save-excursion
	 (ad-set-arg 1 c-new-END)   ; end
	 (ad-set-arg 0 c-new-BEG)))))	; beg

(unless (boundp 'font-lock-extend-after-change-region-function)
  (c-advise-fl-for-region font-lock-after-change-function)
  (c-advise-fl-for-region jit-lock-after-change)
  (c-advise-fl-for-region lazy-lock-defer-rest-after-change)
  (c-advise-fl-for-region lazy-lock-defer-line-after-change))

;; Connect up to `electric-indent-mode' (Emacs 24.4 and later).
(defun c-electric-indent-mode-hook ()
  ;; Emacs has en/disabled `electric-indent-mode'.  Propagate this through to
  ;; each CC Mode buffer.
  (mapc (lambda (buf)
          (with-current-buffer buf
            (when c-buffer-is-cc-mode
              ;; Don't use `c-toggle-electric-state' here due to recursion.
              (setq c-electric-flag electric-indent-mode)
              (c-update-modeline))))
        (buffer-list)))

(defun c-electric-indent-local-mode-hook ()
  ;; Emacs has en/disabled `electric-indent-local-mode' for this buffer.
  ;; Propagate this through to this buffer's value of `c-electric-flag'
  (when c-buffer-is-cc-mode
    (setq c-electric-flag electric-indent-mode)
    (c-update-modeline)))


;; Connection with Emacs's electric-pair-mode
(defun c-electric-pair-inhibit-predicate (char)
  "Return t to inhibit the insertion of a second copy of CHAR.

At the time of call, point is just after the newly inserted CHAR.

When CHAR is \", t will be returned unless the \" is marked with
a string fence syntax-table text property.  For other characters,
the default value of `electric-pair-inhibit-predicate' is called
and its value returned.

This function is the appropriate value of
`electric-pair-inhibit-predicate' for CC Mode modes, which mark
invalid strings with such a syntax table text property on the
opening \" and the next unescaped end of line."
  (if (eq char ?\")
      (not (equal (get-text-property (1- (point)) 'c-fl-syn-tab) '(15)))
    (funcall (default-value 'electric-pair-inhibit-predicate) char)))


;; Support for C

(defvar c-mode-syntax-table
  (funcall (c-lang-const c-make-mode-syntax-table c))
  "Syntax table used in c-mode buffers.")

(c-define-abbrev-table 'c-mode-abbrev-table
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0))
  "Abbreviation table used in c-mode buffers.")

(defvar c-mode-map
  (let ((map (c-make-inherited-keymap)))
    map)
  "Keymap used in c-mode buffers.")
;; Add bindings which are only useful for C.
(define-key c-mode-map "\C-c\C-e"  'c-macro-expand)


(easy-menu-define c-c-menu c-mode-map "C Mode Commands"
		  (cons "C" (c-lang-const c-mode-menu c)))

;; In XEmacs >= 21.5 modes should add their own entries to
;; `auto-mode-alist'.  The comment form of autoload is used to avoid
;; doing this on load.  That since `add-to-list' prepends the value
;; which could cause it to clobber user settings.  Later emacsen have
;; an append option, but it's not safe to use.

;; The extension ".C" is associated with C++ while the lowercase
;; variant goes with C.  On case insensitive file systems, this means
;; that ".c" files also might open C++ mode if the C++ entry comes
;; first on `auto-mode-alist'.  Thus we try to ensure that ".C" comes
;; after ".c", and since `add-to-list' adds the entry first we have to
;; add the ".C" entry first.
;;;###autoload (add-to-list 'auto-mode-alist '("\\.\\(cc\\|hh\\)\\'" . c++-mode))
;;;###autoload (add-to-list 'auto-mode-alist '("\\.[ch]\\(pp\\|xx\\|\\+\\+\\)\\'" . c++-mode))
;;;###autoload (add-to-list 'auto-mode-alist '("\\.\\(CC?\\|HH?\\)\\'" . c++-mode))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
;;;###autoload (add-to-list 'auto-mode-alist '("\\.h\\'" . c-or-c++-mode))

;; NB: The following two associate yacc and lex files to C Mode, which
;; is not really suitable for those formats.  Anyway, afaik there's
;; currently no better mode for them, and besides this is legacy.
;;;###autoload (add-to-list 'auto-mode-alist '("\\.y\\(acc\\)?\\'" . c-mode))
;;;###autoload (add-to-list 'auto-mode-alist '("\\.lex\\'" . c-mode))

;; Preprocessed files generated by C and C++ compilers.
;;;###autoload (add-to-list 'auto-mode-alist '("\\.i\\'" . c-mode))
;;;###autoload (add-to-list 'auto-mode-alist '("\\.ii\\'" . c++-mode))

(unless (fboundp 'prog-mode) (defalias 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode c-mode prog-mode "C"
  "Major mode for editing C code.

To submit a problem report, enter `\\[c-submit-bug-report]' from a
c-mode buffer.  This automatically sets up a mail buffer with version
information already added.  You just need to add a description of the
problem, including a reproducible test case, and send the message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `c-mode-hook'.

Key bindings:
\\{c-mode-map}"
  :after-hook (progn (c-make-noise-macro-regexps)
		     (c-make-macro-with-semi-re)
		     (c-update-modeline))
  (c-initialize-cc-mode t)
  (setq abbrev-mode t)
  (c-init-language-vars-for 'c-mode)
  (c-common-init 'c-mode)
  (easy-menu-add c-c-menu)
  (cc-imenu-init cc-imenu-c-generic-expression)
  (add-hook 'flymake-diagnostic-functions 'flymake-cc nil t)
  (c-run-mode-hooks 'c-mode-common-hook))

(defconst c-or-c++-mode--regexp
  (eval-when-compile
    (let ((id "[a-zA-Z0-9_]+") (ws "[ \t\r]+") (ws-maybe "[ \t\r]*"))
      (concat "^" ws-maybe "\\(?:"
                    "using"     ws "\\(?:namespace" ws "std;\\|std::\\)"
              "\\|" "namespace" "\\(:?" ws id "\\)?" ws-maybe "{"
              "\\|" "class"     ws id ws-maybe "[:{\n]"
              "\\|" "template"  ws-maybe "<.*>"
              "\\|" "#include"  ws-maybe "<\\(?:string\\|iostream\\|map\\)>"
              "\\)")))
  "A regexp applied to C header files to check if they are really C++.")

;;;###autoload
(defun c-or-c++-mode ()
  "Analyze buffer and enable either C or C++ mode.

Some people and projects use .h extension for C++ header files
which is also the one used for C header files.  This makes
matching on file name insufficient for detecting major mode that
should be used.

This function attempts to use file contents to determine whether
the code is C or C++ and based on that chooses whether to enable
`c-mode' or `c++-mode'."
  (if (save-excursion
        (save-restriction
          (save-match-data
            (widen)
            (goto-char (point-min))
            (re-search-forward c-or-c++-mode--regexp
                               (+ (point) c-guess-region-max) t))))
      (c++-mode)
    (c-mode)))


;; Support for C++

(defvar c++-mode-syntax-table
  (funcall (c-lang-const c-make-mode-syntax-table c++))
  "Syntax table used in c++-mode buffers.")

(c-define-abbrev-table 'c++-mode-abbrev-table
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0))
  "Abbreviation table used in c++-mode buffers.")

(defvar c++-mode-map
  (let ((map (c-make-inherited-keymap)))
    map)
  "Keymap used in c++-mode buffers.")
;; Add bindings which are only useful for C++.
(define-key c++-mode-map "\C-c\C-e" 'c-macro-expand)
(define-key c++-mode-map "\C-c:"    'c-scope-operator)
(define-key c++-mode-map "<"        'c-electric-lt-gt)
(define-key c++-mode-map ">"        'c-electric-lt-gt)

(easy-menu-define c-c++-menu c++-mode-map "C++ Mode Commands"
		  (cons "C++" (c-lang-const c-mode-menu c++)))

;;;###autoload
(define-derived-mode c++-mode prog-mode "C++"
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
  :after-hook (progn (c-make-noise-macro-regexps)
		     (c-make-macro-with-semi-re)
		     (c-update-modeline))
  (c-initialize-cc-mode t)
  (setq abbrev-mode t)
  (c-init-language-vars-for 'c++-mode)
  (c-common-init 'c++-mode)
  (easy-menu-add c-c++-menu)
  (cc-imenu-init cc-imenu-c++-generic-expression)
  (add-hook 'flymake-diagnostic-functions 'flymake-cc nil t)
  (c-run-mode-hooks 'c-mode-common-hook))


;; Support for Objective-C

(defvar objc-mode-syntax-table
  (funcall (c-lang-const c-make-mode-syntax-table objc))
  "Syntax table used in objc-mode buffers.")

(c-define-abbrev-table 'objc-mode-abbrev-table
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0))
  "Abbreviation table used in objc-mode buffers.")

(defvar objc-mode-map
  (let ((map (c-make-inherited-keymap)))
    map)
  "Keymap used in objc-mode buffers.")
;; Add bindings which are only useful for Objective-C.
(define-key objc-mode-map "\C-c\C-e" 'c-macro-expand)

(easy-menu-define c-objc-menu objc-mode-map "ObjC Mode Commands"
		  (cons "ObjC" (c-lang-const c-mode-menu objc)))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.m\\'" . objc-mode))

;;;###autoload
(define-derived-mode objc-mode prog-mode "ObjC"
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
  :after-hook (progn (c-make-noise-macro-regexps)
		     (c-make-macro-with-semi-re)
		     (c-update-modeline))
  (c-initialize-cc-mode t)
  (setq abbrev-mode t)
  (c-init-language-vars-for 'objc-mode)
  (c-common-init 'objc-mode)
  (easy-menu-add c-objc-menu)
  (cc-imenu-init nil 'cc-imenu-objc-function)
  (c-run-mode-hooks 'c-mode-common-hook))


;; Support for Java

(defvar java-mode-syntax-table
  (funcall (c-lang-const c-make-mode-syntax-table java))
  "Syntax table used in java-mode buffers.")

(c-define-abbrev-table 'java-mode-abbrev-table
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0))
  "Abbreviation table used in java-mode buffers.")

(defvar java-mode-map
  (let ((map (c-make-inherited-keymap)))
    map)
  "Keymap used in java-mode buffers.")
;; Add bindings which are only useful for Java.

;; Regexp trying to describe the beginning of a Java top-level
;; definition.  This is not used by CC Mode, nor is it maintained
;; since it's practically impossible to write a regexp that reliably
;; matches such a construct.  Other tools are necessary.
(defconst c-Java-defun-prompt-regexp
  "^[ \t]*\\(\\(\\(public\\|protected\\|private\\|const\\|abstract\\|synchronized\\|final\\|static\\|threadsafe\\|transient\\|native\\|volatile\\)\\s-+\\)*\\(\\(\\([[a-zA-Z][][_$.a-zA-Z0-9]*[][_$.a-zA-Z0-9]+\\|[[a-zA-Z]\\)\\s-*\\)\\s-+\\)\\)?\\(\\([[a-zA-Z][][_$.a-zA-Z0-9]*\\s-+\\)\\s-*\\)?\\([_a-zA-Z][^][ \t:;.,{}()\^?=]*\\|\\([_$a-zA-Z][_$.a-zA-Z0-9]*\\)\\)\\s-*\\(([^);{}]*)\\)?\\([] \t]*\\)\\(\\s-*\\<throws\\>\\s-*\\(\\([_$a-zA-Z][_$.a-zA-Z0-9]*\\)[, \t\n\r\f\v]*\\)+\\)?\\s-*")

(easy-menu-define c-java-menu java-mode-map "Java Mode Commands"
		  (cons "Java" (c-lang-const c-mode-menu java)))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))

;;;###autoload
(define-derived-mode java-mode prog-mode "Java"
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
  :after-hook (c-update-modeline)
  (c-initialize-cc-mode t)
  (setq abbrev-mode t)
  (c-init-language-vars-for 'java-mode)
  (c-common-init 'java-mode)
  (easy-menu-add c-java-menu)
  (cc-imenu-init cc-imenu-java-generic-expression)
  (c-run-mode-hooks 'c-mode-common-hook))


;; Support for CORBA's IDL language

(defvar idl-mode-syntax-table
  (funcall (c-lang-const c-make-mode-syntax-table idl))
  "Syntax table used in idl-mode buffers.")

(c-define-abbrev-table 'idl-mode-abbrev-table nil
  "Abbreviation table used in idl-mode buffers.")

(defvar idl-mode-map
  (let ((map (c-make-inherited-keymap)))
    map)
  "Keymap used in idl-mode buffers.")
;; Add bindings which are only useful for IDL.

(easy-menu-define c-idl-menu idl-mode-map "IDL Mode Commands"
		  (cons "IDL" (c-lang-const c-mode-menu idl)))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.idl\\'" . idl-mode))

;;;###autoload
(define-derived-mode idl-mode prog-mode "IDL"
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
  :after-hook (c-update-modeline)
  (c-initialize-cc-mode t)
  (c-init-language-vars-for 'idl-mode)
  (c-common-init 'idl-mode)
  (easy-menu-add c-idl-menu)
  ;;(cc-imenu-init cc-imenu-idl-generic-expression) ;TODO
  (c-run-mode-hooks 'c-mode-common-hook))


;; Support for Pike

(defvar pike-mode-syntax-table
  (funcall (c-lang-const c-make-mode-syntax-table pike))
  "Syntax table used in pike-mode buffers.")

(c-define-abbrev-table 'pike-mode-abbrev-table
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0))
  "Abbreviation table used in pike-mode buffers.")

(defvar pike-mode-map
  (let ((map (c-make-inherited-keymap)))
    map)
  "Keymap used in pike-mode buffers.")
;; Additional bindings.
(define-key pike-mode-map "\C-c\C-e" 'c-macro-expand)

(easy-menu-define c-pike-menu pike-mode-map "Pike Mode Commands"
		  (cons "Pike" (c-lang-const c-mode-menu pike)))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.\\(u?lpc\\|pike\\|pmod\\(\\.in\\)?\\)\\'" . pike-mode))
;;;###autoload (add-to-list 'interpreter-mode-alist '("pike" . pike-mode))

;;;###autoload
(define-derived-mode pike-mode prog-mode "Pike"
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
  :after-hook (c-update-modeline)
  (c-initialize-cc-mode t)
  (setq	abbrev-mode t)
  (c-init-language-vars-for 'pike-mode)
  (c-common-init 'pike-mode)
  (easy-menu-add c-pike-menu)
  ;;(cc-imenu-init cc-imenu-pike-generic-expression) ;TODO
  (c-run-mode-hooks 'c-mode-common-hook))


;; Support for AWK

;;;###autoload (add-to-list 'auto-mode-alist '("\\.awk\\'" . awk-mode))
;;;###autoload (add-to-list 'interpreter-mode-alist '("awk" . awk-mode))
;;;###autoload (add-to-list 'interpreter-mode-alist '("mawk" . awk-mode))
;;;###autoload (add-to-list 'interpreter-mode-alist '("nawk" . awk-mode))
;;;###autoload (add-to-list 'interpreter-mode-alist '("gawk" . awk-mode))

(c-define-abbrev-table 'awk-mode-abbrev-table
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0))
  "Abbreviation table used in awk-mode buffers.")

(defvar awk-mode-map
  (let ((map (c-make-inherited-keymap)))
    map)
  "Keymap used in awk-mode buffers.")
;; Add bindings which are only useful for awk.
(define-key awk-mode-map "#" 'self-insert-command);Overrides electric parent binding.
(define-key awk-mode-map "/" 'self-insert-command);Overrides electric parent binding.
(define-key awk-mode-map "*" 'self-insert-command);Overrides electric parent binding.
(define-key awk-mode-map "\C-c\C-n" 'undefined) ; #if doesn't exist in awk.
(define-key awk-mode-map "\C-c\C-p" 'undefined)
(define-key awk-mode-map "\C-c\C-u" 'undefined)
(define-key awk-mode-map "\M-a" 'c-beginning-of-statement) ; 2003/10/7
(define-key awk-mode-map "\M-e" 'c-end-of-statement)       ; 2003/10/7
(define-key awk-mode-map "\C-\M-a" 'c-awk-beginning-of-defun)
(define-key awk-mode-map "\C-\M-e" 'c-awk-end-of-defun)

(easy-menu-define c-awk-menu awk-mode-map "AWK Mode Commands"
		  (cons "AWK" (c-lang-const c-mode-menu awk)))

;; (require 'cc-awk) brings these in.
(defvar awk-mode-syntax-table)
(declare-function c-awk-unstick-NL-prop "cc-awk" ())

;;;###autoload
(define-derived-mode awk-mode prog-mode "AWK"
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
  :after-hook (c-update-modeline)
  ;; We need the next line to stop the macro defining
  ;; `awk-mode-syntax-table'.  This would mask the real table which is
  ;; declared in cc-awk.el and hasn't yet been loaded.
  :syntax-table nil
  (require 'cc-awk)			; Added 2003/6/10.
  (c-initialize-cc-mode t)
  (set-syntax-table awk-mode-syntax-table)
  (setq	abbrev-mode t)
  (c-init-language-vars-for 'awk-mode)
  (c-common-init 'awk-mode)
  (c-awk-unstick-NL-prop)
  (c-run-mode-hooks 'c-mode-common-hook))


;; bug reporting

(defconst c-mode-help-address
  "submit@debbugs.gnu.org"
  "Address(es) for CC Mode bug reports.")

(defun c-version ()
  "Echo the current version of CC Mode in the minibuffer."
  (interactive)
  (message "Using CC Mode version %s" c-version)
  (c-keep-region-active))

(define-obsolete-variable-alias 'c-prepare-bug-report-hooks
  'c-prepare-bug-report-hook "24.3")
(defvar c-prepare-bug-report-hook nil)

;; Dynamic variables used by reporter.
(defvar reporter-prompt-for-summary-p)
(defvar reporter-dont-compact-list)

;; This could be "emacs,cc-mode" in the version included in Emacs.
(defconst c-mode-bug-package "cc-mode"
  "The package to use in the bug submission.")

;; reporter-submit-bug-report requires sendmail.
(declare-function mail-position-on-field "sendmail" (field &optional soft))
(declare-function mail-text "sendmail" ())

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
	(mapc (lambda (var) (unless (boundp var)
                         (setq vars (delq var vars))))
	      '(signal-error-on-buffer-boundary
		filladapt-mode
		defun-prompt-regexp
		font-lock-mode
		auto-fill-mode
		font-lock-maximum-decoration
		parse-sexp-lookup-properties
		lookup-syntax-properties))
	vars)
      (lambda ()
	(run-hooks 'c-prepare-bug-report-hook)
	(let ((hook (get mail-user-agent 'hookvar)))
	  (if hook
	      (add-hook hook
			(lambda ()
			  (save-excursion
			    (mail-text)
			    (unless (looking-at "Package: ")
			      (insert "Package: " c-mode-bug-package "\n\n"))))
			nil t)))
	(save-excursion
	  (or (mail-position-on-field "X-Debbugs-Package")
	      (insert c-mode-bug-package))
	  ;; For mail clients that do not support X- headers.
	  ;; Sadly reporter-submit-bug-report unconditionally adds
	  ;; a blank line before SALUTATION, so we can't use that.
	  ;; It is also sad that reporter offers no way to leave point
	  ;; after this line we are now inserting.
	  (mail-text)
	  (or (looking-at "Package:")
	      (insert "Package: " c-mode-bug-package)))
	(insert (format "Buffer Style: %s\nc-emacs-features: %s\n"
			style c-features)))))))


(cc-provide 'cc-mode)

;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:
;;; cc-mode.el ends here
