;;; font-core.el --- Core interface to font-lock

;; Copyright (C) 1992, 93, 94, 95, 96, 97, 98, 1999, 2000, 2001, 2002
;;  Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: languages, faces

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

;;; Code:

;; This variable is used by mode packages that support Font Lock mode by
;; defining their own keywords to use for `font-lock-keywords'.  (The mode
;; command should make it buffer-local and set it to provide the set up.)
(defvar font-lock-defaults nil
  "Defaults for Font Lock mode specified by the major mode.
Defaults should be of the form:

 (KEYWORDS KEYWORDS-ONLY CASE-FOLD SYNTAX-ALIST SYNTAX-BEGIN ...)

KEYWORDS may be a symbol (a variable or function whose value is the keywords to
use for fontification) or a list of symbols.  If KEYWORDS-ONLY is non-nil,
syntactic fontification (strings and comments) is not performed.
If CASE-FOLD is non-nil, the case of the keywords is ignored when fontifying.
If SYNTAX-ALIST is non-nil, it should be a list of cons pairs of the form
\(CHAR-OR-STRING . STRING) used to set the local Font Lock syntax table, for
keyword and syntactic fontification (see `modify-syntax-entry').

If SYNTAX-BEGIN is non-nil, it should be a function with no args used to move
backwards outside any enclosing syntactic block, for syntactic fontification.
Typical values are `beginning-of-line' (i.e., the start of the line is known to
be outside a syntactic block), or `beginning-of-defun' for programming modes or
`backward-paragraph' for textual modes (i.e., the mode-dependent function is
known to move outside a syntactic block).  If nil, the beginning of the buffer
is used as a position outside of a syntactic block, in the worst case.

These item elements are used by Font Lock mode to set the variables
`font-lock-keywords', `font-lock-keywords-only',
`font-lock-keywords-case-fold-search', `font-lock-syntax-table' and
`font-lock-beginning-of-syntax-function', respectively.

Further item elements are alists of the form (VARIABLE . VALUE) and are in no
particular order.  Each VARIABLE is made buffer-local before set to VALUE.

Currently, appropriate variables include `font-lock-mark-block-function'.
If this is non-nil, it should be a function with no args used to mark any
enclosing block of text, for fontification via \\[font-lock-fontify-block].
Typical values are `mark-defun' for programming modes or `mark-paragraph' for
textual modes (i.e., the mode-dependent function is known to put point and mark
around a text block relevant to that mode).

Other variables include that for syntactic keyword fontification,
`font-lock-syntactic-keywords'
and those for buffer-specialised fontification functions,
`font-lock-fontify-buffer-function', `font-lock-unfontify-buffer-function',
`font-lock-fontify-region-function', `font-lock-unfontify-region-function',
`font-lock-inhibit-thing-lock' and `font-lock-maximum-size'.")
(make-variable-buffer-local 'font-lock-defaults)

;; This variable is used where font-lock.el itself supplies the
;; keywords.  Really, this shouldn't need to be in font-core.el, but
;; we can't avoid it.  In the future, this stuff will hopefully be
;; moved to cc-mode itself.
(defvar font-lock-defaults-alist
  (let (;; We use `beginning-of-defun', rather than nil, for SYNTAX-BEGIN.
	;; Thus the calculation of the cache is usually faster but not
	;; infallible, so we risk mis-fontification.  sm.
	(c-mode-defaults
	 '((c-font-lock-keywords c-font-lock-keywords-1
	    c-font-lock-keywords-2 c-font-lock-keywords-3)
	   nil nil ((?_ . "w")) beginning-of-defun
	   (font-lock-syntactic-face-function
	    . c-font-lock-syntactic-face-function)
	   (font-lock-mark-block-function . mark-defun)))
	(c++-mode-defaults
	 '((c++-font-lock-keywords c++-font-lock-keywords-1
	    c++-font-lock-keywords-2 c++-font-lock-keywords-3)
	   nil nil ((?_ . "w")) beginning-of-defun
	   (font-lock-syntactic-face-function
	    . c-font-lock-syntactic-face-function)
	   (font-lock-mark-block-function . mark-defun)))
	(objc-mode-defaults
	 '((objc-font-lock-keywords objc-font-lock-keywords-1
	    objc-font-lock-keywords-2 objc-font-lock-keywords-3)
	   nil nil ((?_ . "w") (?$ . "w")) nil
	   (font-lock-syntactic-face-function
	    . c-font-lock-syntactic-face-function)
	   (font-lock-mark-block-function . mark-defun)))
	(java-mode-defaults
	 '((java-font-lock-keywords java-font-lock-keywords-1
	    java-font-lock-keywords-2 java-font-lock-keywords-3)
	   nil nil ((?_ . "w") (?$ . "w")) nil
	   (font-lock-syntactic-face-function
	    . java-font-lock-syntactic-face-function)
	   (font-lock-mark-block-function . mark-defun))))
    (list
     (cons 'c-mode			c-mode-defaults)
     (cons 'c++-mode			c++-mode-defaults)
     (cons 'objc-mode			objc-mode-defaults)
     (cons 'java-mode			java-mode-defaults)))
  "Alist of fall-back Font Lock defaults for major modes.

This variable should not be used any more.
Set the buffer-local `font-lock-keywords' in the major mode instead.

Each item should be a list of the form:

 (MAJOR-MODE . FONT-LOCK-DEFAULTS)

where MAJOR-MODE is a symbol and FONT-LOCK-DEFAULTS is a list of default
settings.  See the variable `font-lock-defaults', which takes precedence.")
(make-obsolete-variable 'font-lock-defaults-alist 'font-lock-defaults)

(defvar font-lock-multiline nil
  "Whether font-lock should cater to multiline keywords.
If nil, don't try to handle multiline patterns.
If t, always handle multiline patterns.
If `undecided', don't try to handle multiline patterns until you see one.
Major/minor modes can set this variable if they know which option applies.")

(defvar font-lock-fontified nil)	; Whether we have fontified the buffer.

(defvar font-lock-category-alist nil
  "An alist of (CATEGORY-SYMBOL . FACE-PROP) controlled by Font Lock.
This variable is intended to be used by special modes which construct
buffer text for display to the user (i.e. buffer-menu, occur), but
wish to have fontification turned on and off by Font Lock.  If this
variable is non-nil, then calling `font-lock-mode' will simply toggle
the symbol property `face' of CATEGORY-SYMBOL.")

(defvar font-lock-function 'font-lock-default-function
  "A function which is called when `font-lock-mode' is toggled.
It will be passed one argument, which is the current value of
`font-lock-mode'.")
(make-variable-buffer-local 'font-lock-default-function)

(define-minor-mode font-lock-mode
  "Toggle Font Lock mode.
With arg, turn Font Lock mode off if and only if arg is a non-positive
number; if arg is nil, toggle Font Lock mode; anything else turns Font
Lock on.
\(Font Lock is also known as \"syntax highlighting\".)

When Font Lock mode is enabled, text is fontified as you type it:

 - Comments are displayed in `font-lock-comment-face';
 - Strings are displayed in `font-lock-string-face';
 - Certain other expressions are displayed in other faces according to the
   value of the variable `font-lock-keywords'.

To customize the faces (colors, fonts, etc.) used by Font Lock for
fontifying different parts of buffer text, use \\[customize-face].

You can enable Font Lock mode in any major mode automatically by turning on in
the major mode's hook.  For example, put in your ~/.emacs:

 (add-hook 'c-mode-hook 'turn-on-font-lock)

Alternatively, you can use Global Font Lock mode to automagically turn on Font
Lock mode in buffers whose major mode supports it and whose major mode is one
of `font-lock-global-modes'.  For example, put in your ~/.emacs:

 (global-font-lock-mode t)

There are a number of support modes that may be used to speed up Font Lock mode
in various ways, specified via the variable `font-lock-support-mode'.  Where
major modes support different levels of fontification, you can use the variable
`font-lock-maximum-decoration' to specify which level you generally prefer.
When you turn Font Lock mode on/off the buffer is fontified/defontified, though
fontification occurs only if the buffer is less than `font-lock-maximum-size'.

For example, to specify that Font Lock mode use use Lazy Lock mode as a support
mode and use maximum levels of fontification, put in your ~/.emacs:

 (setq font-lock-support-mode 'lazy-lock-mode)
 (setq font-lock-maximum-decoration t)

To add your own highlighting for some major mode, and modify the highlighting
selected automatically via the variable `font-lock-maximum-decoration', you can
use `font-lock-add-keywords'.

To fontify a buffer, without turning on Font Lock mode and regardless of buffer
size, you can use \\[font-lock-fontify-buffer].

To fontify a block (the function or paragraph containing point, or a number of
lines around point), perhaps because modification on the current line caused
syntactic change on other lines, you can use \\[font-lock-fontify-block].

See the variable `font-lock-defaults-alist' for the Font Lock mode default
settings.  You can set your own default settings for some mode, by setting a
buffer local value for `font-lock-defaults', via its mode hook.

The above is the default behavior of `font-lock-mode'; you may specify
your own function which is called when `font-lock-mode' is toggled via
`font-lock-function'. "
  nil nil nil
  ;; Don't turn on Font Lock mode if we don't have a display (we're running a
  ;; batch job) or if the buffer is invisible (the name starts with a space).
  (when (or noninteractive (eq (aref (buffer-name) 0) ?\ ))
    (setq font-lock-mode nil))
  (funcall font-lock-function font-lock-mode))

(defun font-lock-default-function (font-lock-mode)
  ;; Turn on Font Lock mode.
  (when font-lock-mode
    (font-lock-set-defaults)
    (dolist (elt font-lock-category-alist)
      (put (car elt) 'face (cdr elt)))
    (when font-lock-defaults
      (add-hook 'after-change-functions 'font-lock-after-change-function t t)
      (font-lock-turn-on-thing-lock)
      ;; Fontify the buffer if we have to.
      (let ((max-size (font-lock-value-in-major-mode font-lock-maximum-size)))
	(cond (font-lock-fontified
	       nil)
	      ((or (null max-size) (> max-size (buffer-size)))
	       (font-lock-fontify-buffer))
	      (font-lock-verbose
	       (message "Fontifying %s...buffer size greater than font-lock-maximum-size"
			(buffer-name)))))))
  ;; Turn off Font Lock mode.
  (unless font-lock-mode
    (dolist (elt font-lock-category-alist)
      (put (car elt) 'face nil))
    (when font-lock-defaults
      (remove-hook 'after-change-functions 'font-lock-after-change-function t)
      (font-lock-unfontify-buffer)
      (font-lock-turn-off-thing-lock))))
  
(defun turn-on-font-lock ()
  "Turn on Font Lock mode (only if the terminal can display it)."
  (unless font-lock-mode
    (font-lock-mode)))

(defvar font-lock-set-defaults nil)	; Whether we have set up defaults.

(defun font-lock-set-defaults ()
  "Set fontification defaults appropriately for this mode.
Sets various variables using `font-lock-defaults' (or, if nil, using
`font-lock-defaults-alist') and `font-lock-maximum-decoration'."
  (unless font-lock-set-defaults
    (set (make-local-variable 'font-lock-set-defaults) t)
    (make-local-variable 'font-lock-fontified)
    (make-local-variable 'font-lock-multiline)
    ;; Detect if this is a simple mode, which doesn't use any
    ;; syntactic fontification functions.
    (when (or font-lock-defaults
	      (assq major-mode font-lock-defaults-alist))
      (require 'font-lock)
      (font-lock-set-defaults-1))))

;;; Global Font Lock mode.

;; A few people have hassled in the past for a way to make it easier to turn on
;; Font Lock mode, without the user needing to know for which modes s/he has to
;; turn it on, perhaps the same way hilit19.el/hl319.el does.  I've always
;; balked at that way, as I see it as just re-moulding the same problem in
;; another form.  That is; some person would still have to keep track of which
;; modes (which may not even be distributed with Emacs) support Font Lock mode.
;; The list would always be out of date.  And that person might have to be me.

;; Implementation.
;;
;; In a previous discussion the following hack came to mind.  It is a gross
;; hack, but it generally works.  We use the convention that major modes start
;; by calling the function `kill-all-local-variables', which in turn runs
;; functions on the hook variable `change-major-mode-hook'.  We attach our
;; function `font-lock-change-major-mode' to that hook.  Of course, when this
;; hook is run, the major mode is in the process of being changed and we do not
;; know what the final major mode will be.  So, `font-lock-change-major-mode'
;; only (a) notes the name of the current buffer, and (b) adds our function
;; `turn-on-font-lock-if-enabled' to the hook variables `find-file-hooks' and
;; `post-command-hook' (for buffers that are not visiting files).  By the time
;; the functions on the first of these hooks to be run are run, the new major
;; mode is assumed to be in place.  This way we get a Font Lock function run
;; when a major mode is turned on, without knowing major modes or their hooks.
;;
;; Naturally this requires that (a) major modes run `kill-all-local-variables',
;; as they are supposed to do, and (b) the major mode is in place after the
;; file is visited or the command that ran `kill-all-local-variables' has
;; finished, whichever the sooner.  Arguably, any major mode that does not
;; follow the convension (a) is broken, and I can't think of any reason why (b)
;; would not be met (except `gnudoit' on non-files).  However, it is not clean.
;;
;; Probably the cleanest solution is to have each major mode function run some
;; hook, e.g., `major-mode-hook', but maybe implementing that change is
;; impractical.  I am personally against making `setq' a macro or be advised,
;; or have a special function such as `set-major-mode', but maybe someone can
;; come up with another solution?

;; User interface.
;;
;; Although Global Font Lock mode is a pseudo-mode, I think that the user
;; interface should conform to the usual Emacs convention for modes, i.e., a
;; command to toggle the feature (`global-font-lock-mode') with a variable for
;; finer control of the mode's behaviour (`font-lock-global-modes').
;;
;; The feature should not be enabled by loading font-lock.el, since other
;; mechanisms for turning on Font Lock mode, such as M-x font-lock-mode RET or
;; (add-hook 'c-mode-hook 'turn-on-font-lock), would cause Font Lock mode to be
;; turned on everywhere.  That would not be intuitive or informative because
;; loading a file tells you nothing about the feature or how to control it.  It
;; would also be contrary to the Principle of Least Surprise.  sm.

(defcustom font-lock-global-modes t
  "*Modes for which Font Lock mode is automagically turned on.
Global Font Lock mode is controlled by the command `global-font-lock-mode'.
If nil, means no modes have Font Lock mode automatically turned on.
If t, all modes that support Font Lock mode have it automatically turned on.
If a list, it should be a list of `major-mode' symbol names for which Font Lock
mode should be automatically turned on.  The sense of the list is negated if it
begins with `not'.  For example:
 (c-mode c++-mode)
means that Font Lock mode is turned on for buffers in C and C++ modes only."
  :type '(choice (const :tag "none" nil)
		 (const :tag "all" t)
		 (set :menu-tag "mode specific" :tag "modes"
		      :value (not)
		      (const :tag "Except" not)
		      (repeat :inline t (symbol :tag "mode"))))
  :group 'font-lock)

(defun turn-on-font-lock-if-enabled ()
  (when (and (or font-lock-defaults
		 font-lock-category-alist
		 (assq major-mode font-lock-defaults-alist))
	     (or (eq font-lock-global-modes t)
		 (if (eq (car-safe font-lock-global-modes) 'not)
		     (not (memq major-mode (cdr font-lock-global-modes)))
		   (memq major-mode font-lock-global-modes))))
    (let (inhibit-quit)
      (turn-on-font-lock))))

(easy-mmode-define-global-mode
 global-font-lock-mode font-lock-mode turn-on-font-lock-if-enabled
 :extra-args (dummy))

;;; End of Global Font Lock mode.

(provide 'font-core)

;;; font-core.el ends here

