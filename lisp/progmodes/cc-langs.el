;;; cc-langs.el --- specific language support for CC Mode

;; Copyright (C) 1985,1987,1992-1999 Free Software Foundation, Inc.

;; Authors:    1998-1999 Barry A. Warsaw and Martin Stjernholm
;;             1992-1997 Barry A. Warsaw
;;             1987 Dave Detlefs and Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Version:    See cc-mode.el
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-current-file)
		  (stringp byte-compile-current-file))
	     (cons (file-name-directory byte-compile-current-file)
		   load-path)
	   load-path)))
    (load "cc-defs" nil t)))
(require 'cc-styles)

;; Pull in some other packages.
(eval-when-compile
  (condition-case nil
      ;; Not required and only needed during compilation to shut up
      ;; the compiler.
      (require 'outline)
    (error nil)))
;; menu support for both XEmacs and Emacs.  If you don't have easymenu
;; with your version of Emacs, you are incompatible!
(require 'easymenu)


(defvar c-buffer-is-cc-mode nil
  "Non-nil for all buffers with a `major-mode' derived from CC Mode.
Otherwise, this variable is nil. I.e. this variable is non-nil for
`c-mode', `c++-mode', `objc-mode', `java-mode', `idl-mode',
`pike-mode', and any other non-CC Mode mode that calls
`c-initialize-cc-mode' (e.g. `awk-mode').")
(make-variable-buffer-local 'c-buffer-is-cc-mode)
(put 'c-buffer-is-cc-mode 'permanent-local t)


;; Regular expressions and other values which must be parameterized on
;; a per-language basis.

;; Keywords defining protection levels
(defconst c-protection-key "\\<\\(public\\|protected\\|private\\)\\>")

;; Regex describing a `symbol' in all languages.  We cannot use just
;; `word' syntax class since `_' cannot be in word class.  Putting
;; underscore in word class breaks forward word movement behavior that
;; users are familiar with.  Besides, this runs counter to Emacs
;; convention.
;;
;; I suspect this definition isn't correct in light of Java's
;; definition of a symbol as being Unicode.  I know so little about
;; I18N (except how to sound cool and say I18N :-) that I'm willing to
;; punt on this for now.

(defconst c-symbol-key "[_a-zA-Z]\\(\\w\\|\\s_\\)*")


;; keywords introducing class definitions.  language specific
(defconst c-C-class-key "\\(struct\\|union\\)")
(defconst c-C++-class-key "\\(class\\|struct\\|union\\)")
(defconst c-IDL-class-key "\\(interface\\|struct\\|union\\|valuetype\\)")
(defconst c-C-extra-toplevel-key "\\(extern\\)")
(defconst c-C++-extra-toplevel-key "\\(extern\\|namespace\\)")
(defconst c-IDL-extra-toplevel-key "\\(module\\)")

(defconst c-ObjC-class-key
  (concat
   "@\\(interface\\|implementation\\)\\s +"
   c-symbol-key				;name of the class
   "\\(\\s *:\\s *" c-symbol-key "\\)?"	;maybe followed by the superclass
   "\\(\\s *<[^>]+>\\)?"		;and maybe the adopted protocols list
   ))

(defconst c-Java-class-key
  (concat
   "\\(" c-protection-key "\\s +\\)?"
   "\\(interface\\|class\\)\\s +"
   c-symbol-key				      ;name of the class
   "\\(\\s *extends\\s *" c-symbol-key "\\)?" ;maybe followed by superclass
   ;;"\\(\\s *implements *[^{]+{\\)?"	      ;maybe the adopted protocols list
   ))

(defconst c-Pike-class-key "class")

(defvar c-class-key c-C-class-key)
(make-variable-buffer-local 'c-class-key)

(defvar c-extra-toplevel-key c-C-extra-toplevel-key)
(make-variable-buffer-local 'c-extra-toplevel-key)

;; Keywords that can introduce bitfields in the languages that supports that.
(defconst c-C-bitfield-key "\\(char\\|int\\|long\\|signed\\|unsigned\\)")

(defvar c-bitfield-key nil)
(make-variable-buffer-local 'c-bitfield-key)


;; regexp describing access protection clauses.  language specific
(defvar c-access-key nil)
(make-variable-buffer-local 'c-access-key)
(defconst c-C++-access-key (concat c-protection-key "[ \t]*:"))
(defconst c-IDL-access-key nil)
(defconst c-ObjC-access-key (concat "@" c-protection-key))
(defconst c-Java-access-key nil)
(defconst c-Pike-access-key nil)


;; keywords introducing conditional blocks
(defconst c-C-conditional-key nil)
(defconst c-C++-conditional-key nil)
(defconst c-IDL-conditional-key nil)
(defconst c-ObjC-conditional-key nil)
(defconst c-Java-conditional-key nil)
(defconst c-Pike-conditional-key nil)

(let ((all-kws "for\\|if\\|do\\|else\\|while\\|switch")
      (exc-kws "\\|try\\|catch")
      (thr-kws "\\|finally\\|synchronized")
      (front   "\\b\\(")
      (back    "\\)\\b[^_]"))
  (setq c-C-conditional-key (concat front all-kws back)
	c-C++-conditional-key (concat front all-kws exc-kws back)
	;; c-IDL-conditional-key is nil.
	c-ObjC-conditional-key c-C-conditional-key
	c-Java-conditional-key (concat front all-kws exc-kws thr-kws back)
	c-Pike-conditional-key (concat front all-kws "\\|foreach" back)))

(defvar c-conditional-key c-C-conditional-key)
(make-variable-buffer-local 'c-conditional-key)


;; keywords describing method definition introductions
(defvar c-method-key nil)
(make-variable-buffer-local 'c-method-key)

(defconst c-ObjC-method-key
  (concat
   "^\\s *[+-]\\s *"
   "\\(([^)]*)\\)?"			; return type
   ;; \\s- in objc syntax table does not include \n
   ;; since it is considered the end of //-comments.
   "[ \t\n]*" c-symbol-key))



;; comment starter definitions for various languages.  language specific
(defconst c-C++-comment-start-regexp "/[/*]")
(defconst c-C-comment-start-regexp c-C++-comment-start-regexp)
(defconst c-IDL-comment-start-regexp c-C++-comment-start-regexp)
(defconst c-ObjC-comment-start-regexp c-C++-comment-start-regexp)
(defconst c-Pike-comment-start-regexp c-C++-comment-start-regexp)
;; We need to match all 3 Java style comments
;; 1) Traditional C block; 2) javadoc /** ...; 3) C++ style
(defconst c-Java-comment-start-regexp "/\\(/\\|[*][*]?\\)")
(defvar c-comment-start-regexp c-C++-comment-start-regexp)
(make-variable-buffer-local 'c-comment-start-regexp)



;; Regexp describing a switch's case or default label for all languages
(defconst c-switch-label-key "\\(\\(case[( \t]+\\S .*\\)\\|default[ \t]*\\):")
;; Regexp describing any label.
(defconst c-label-key (concat c-symbol-key ":\\([^:]\\|$\\)"))

;; Regexp describing class inheritance declarations.  TBD: this should
;; be language specific, and only makes sense for C++
(defconst c-inher-key
  (concat "\\(\\<static\\>\\s +\\)?"
	  c-C++-class-key "[ \t]+" c-symbol-key
	  "\\([ \t]*:[ \t]*\\)\\s *[^;]"))

;; Regexp describing C++ base classes in a derived class definition.
;; TBD: this should be language specific, and only makes sense for C++
(defvar c-baseclass-key
  (concat
   ":?[ \t]*\\(virtual[ \t]+\\)?\\("
   c-protection-key "[ \t]+\\)" c-symbol-key))
(make-variable-buffer-local 'c-baseclass-key)

;; Regexp describing friend declarations in C++ classes.
(defconst c-C++-friend-key
  "friend[ \t]+\\|template[ \t]*<.+>[ \t]*friend[ \t]+")

;; Regexp describing Java inheritance and throws clauses.
(defconst c-Java-special-key "\\(implements\\|extends\\|throws\\)[^_]")

;; Regexp describing the beginning of a Java top-level definition.
(defconst c-Java-defun-prompt-regexp
  "^[ \t]*\\(\\(\\(public\\|protected\\|private\\|const\\|abstract\\|synchronized\\|final\\|static\\|threadsafe\\|transient\\|native\\|volatile\\)\\s-+\\)*\\(\\(\\([[a-zA-Z][][_$.a-zA-Z0-9]*[][_$.a-zA-Z0-9]+\\|[[a-zA-Z]\\)\\s-*\\)\\s-+\\)\\)?\\(\\([[a-zA-Z][][_$.a-zA-Z0-9]*\\s-+\\)\\s-*\\)?\\([_a-zA-Z][^][ \t:;.,{}()=]*\\|\\([_$a-zA-Z][_$.a-zA-Z0-9]*\\)\\)\\s-*\\(([^);{}]*)\\)?\\([] \t]*\\)\\(\\s-*\\<throws\\>\\s-*\\(\\([_$a-zA-Z][_$.a-zA-Z0-9]*\\)[, \t\n\r\f]*\\)+\\)?\\s-*")

;; Regexp describing Javadoc markup that always starts paragraphs.
(defconst c-Java-javadoc-paragraph-start
  "@\\(author\\|exception\\|param\\|return\\|see\\|throws\\|version\\)")

;; Regexp that starts lambda constructs.
(defvar c-lambda-key nil)
(make-variable-buffer-local 'c-lambda-key)
(defconst c-Pike-lambda-key "\\<lambda\\>")

;; Regexp that are followed by a statement block in expressions.
(defvar c-inexpr-block-key nil)
(make-variable-buffer-local 'c-inexpr-block-key)
(defconst c-Pike-inexpr-block-key "\\<\\(catch\\|gauge\\)\\>")

;; Regexp that may be followed by an anonymous class in expressions.
(defvar c-inexpr-class-key nil)
(make-variable-buffer-local 'c-inexpr-class-key)
(defconst c-Java-inexpr-class-key "\\<new\\>")

;; List of open- and close-chars that makes up a pike-style brace
;; list, ie for a `([ ])' list there should be a cons (?\[ . ?\]) in
;; this list.
(defvar c-special-brace-lists nil)
(make-variable-buffer-local 'c-special-brace-lists)
(defconst c-Pike-special-brace-lists '((?{ . ?})
				       (?\[ . ?\])
				       (?< . ?>)))



;; internal state variables

;; Internal state of hungry delete key feature
(defvar c-hungry-delete-key nil)
(make-variable-buffer-local 'c-hungry-delete-key)

;; Internal state of auto newline feature.
(defvar c-auto-newline nil)
(make-variable-buffer-local 'c-auto-newline)

;; Internal auto-newline/hungry-delete designation string for mode line.
(defvar c-auto-hungry-string nil)
(make-variable-buffer-local 'c-auto-hungry-string)

;; Non-nil means K&R style argument declarations are valid.
(defvar c-recognize-knr-p t)
(make-variable-buffer-local 'c-recognize-knr-p)



(defun c-common-init ()
  ;; Common initializations for all modes.
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
  ;; now set the mode style based on c-default-style
  (let ((style (if (stringp c-default-style)
		   (if (c-major-mode-is 'java-mode)
		       "java"
		     c-default-style)
		 (or (cdr (assq major-mode c-default-style))
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
  ;; Fix things up for paragraph recognition and filling inside
  ;; comments by using c-comment-prefix-regexp in the relevant places.
  ;; We use adaptive filling for this to make it possible to use
  ;; filladapt or some other fancy package.
  (let ((comment-line-prefix
	 (concat "[ \t]*\\(" c-comment-prefix-regexp "\\)?[ \t]*")))
    (setq paragraph-start (concat comment-line-prefix "$\\|"
				  page-delimiter)
	  paragraph-separate paragraph-start
	  paragraph-ignore-fill-prefix t
	  adaptive-fill-mode t
	  adaptive-fill-regexp
	  (concat comment-line-prefix
		  (if adaptive-fill-regexp
		      (concat "\\(" adaptive-fill-regexp "\\)")
		    "")))
    (when (boundp 'adaptive-fill-first-line-regexp)
      ;; XEmacs (20.x) adaptive fill mode doesn't have this.
      (make-local-variable 'adaptive-fill-first-line-regexp)
      (setq adaptive-fill-first-line-regexp
	    (concat "\\`" comment-line-prefix
		    ;; Maybe we should incorporate the old value here,
		    ;; but then we have to do all sorts of kludges to
		    ;; deal with the \` and \' it probably contains.
		    "\\'"))))
  ;; we have to do something special for c-offsets-alist so that the
  ;; buffer local value has its own alist structure.
  (setq c-offsets-alist (copy-alist c-offsets-alist))
  ;; setup the comment indent variable in a Emacs version portable way
  ;; ignore any byte compiler warnings you might get here
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'c-comment-indent)
  ;; add menus to menubar
  (easy-menu-add (c-mode-menu mode-name))
  ;; put auto-hungry designators onto minor-mode-alist, but only once
  (or (assq 'c-auto-hungry-string minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(c-auto-hungry-string c-auto-hungry-string)
		  minor-mode-alist)))
  )


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
	(function
	 (lambda (langentry)
	   (let ((langelem (car langentry))
		 (offset (cdr langentry)))
	     (c-set-offset langelem offset)
	     )))
	c-file-offsets)))

(add-hook 'hack-local-variables-hook 'c-postprocess-file-styles)


(defvar c-mode-base-map ()
  "Keymap shared by all CC Mode related modes.")

;; Common routines
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

(defun c-populate-syntax-table (table)
  ;; Populate the syntax TABLE
  ;; DO NOT TRY TO SET _ (UNDERSCORE) TO WORD CLASS!
  (modify-syntax-entry ?_  "_"     table)
  (modify-syntax-entry ?\\ "\\"    table)
  (modify-syntax-entry ?+  "."     table)
  (modify-syntax-entry ?-  "."     table)
  (modify-syntax-entry ?=  "."     table)
  (modify-syntax-entry ?%  "."     table)
  (modify-syntax-entry ?<  "."     table)
  (modify-syntax-entry ?>  "."     table)
  (modify-syntax-entry ?&  "."     table)
  (modify-syntax-entry ?|  "."     table)
  (modify-syntax-entry ?\' "\""    table)
  ;; Set up block and line oriented comments.  The new C standard
  ;; mandates both comment styles even in C, so since all languages
  ;; now require dual comments, we make this the default.
  (cond
   ;; XEmacs 19 & 20
   ((memq '8-bit c-emacs-features)
    (modify-syntax-entry ?/  ". 1456" table)
    (modify-syntax-entry ?*  ". 23"   table))
   ;; Emacs 19 & 20
   ((memq '1-bit c-emacs-features)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table))
   ;; incompatible
   (t (error "CC Mode is incompatible with this version of Emacs"))
   )
  (modify-syntax-entry ?\n "> b"  table)
  ;; Give CR the same syntax as newline, for selective-display
  (modify-syntax-entry ?\^m "> b" table))


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
  ;; Caution!  Enter here at your own risk.  We are trying to support
  ;; several behaviors and it gets disgusting. :-(
  ;;
  (if (boundp 'delete-key-deletes-forward)
      (progn
	;; In XEmacs 20 it is possible to sanely define both backward
	;; and forward deletion behavior under X separately (TTYs are
	;; forever beyond hope, but who cares?  XEmacs 20 does the
	;; right thing with these too).
	(define-key c-mode-base-map [delete]    'c-electric-delete)
	(define-key c-mode-base-map [backspace] 'c-electric-backspace))
    ;; In XEmacs 19, Emacs 19, and Emacs 20, we use this to bind
    ;; backwards deletion behavior to DEL, which both Delete and
    ;; Backspace get translated to.  There's no way to separate this
    ;; behavior in a clean way, so deal with it!  Besides, it's been
    ;; this way since the dawn of BOCM.
    (define-key c-mode-base-map "\177" 'c-electric-backspace))
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
	 '(["Comment Out Region"     comment-region (c-region-is-active-p)]
	   ["Uncomment Region"
	    (comment-region (region-beginning) (region-end) '(4))
	    (c-region-is-active-p)]
	   ["Fill Comment Paragraph" c-fill-paragraph t]
	   "---"
	   ["Indent Expression"      c-indent-exp
	    (memq (char-after) '(?\( ?\[ ?\{))]
	   ["Indent Line or Region"  c-indent-line-or-region t]
	   ["Up Conditional"         c-up-conditional t]
	   ["Backward Conditional"   c-backward-conditional t]
	   ["Forward Conditional"    c-forward-conditional t]
	   ["Backward Statement"     c-beginning-of-statement t]
	   ["Forward Statement"      c-end-of-statement t]
	   "---"
	   ["Macro Expand Region"    c-macro-expand (c-region-is-active-p)]
	   ["Backslashify"           c-backslash-region (c-region-is-active-p)]
	   )))
    (cons modestr m)))



;; Support for C

(defvar c-mode-abbrev-table nil
  "Abbreviation table used in c-mode buffers.")
(define-abbrev-table 'c-mode-abbrev-table ())

(defvar c-mode-map ()
  "Keymap used in c-mode buffers.")
(if c-mode-map
    nil
  (setq c-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for C
  (define-key c-mode-map "\C-c\C-e"  'c-macro-expand)
  )

;;;###autoload
(defvar c-mode-syntax-table nil
  "Syntax table used in c-mode buffers.")
(if c-mode-syntax-table
    ()
  (setq c-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table c-mode-syntax-table))

(easy-menu-define c-c-menu c-mode-map "C Mode Commands"
		  (c-mode-menu "C"))


;; Support for C++

(defvar c++-mode-abbrev-table nil
  "Abbreviation table used in c++-mode buffers.")
(define-abbrev-table 'c++-mode-abbrev-table ())

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

;;;###autoload
(defvar c++-mode-syntax-table nil
  "Syntax table used in c++-mode buffers.")
(if c++-mode-syntax-table
    ()
  (setq c++-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table c++-mode-syntax-table)
  ;; TBD: does it make sense for colon to be symbol class in C++?
  ;; I'm not so sure, since c-label-key is busted on lines like:
  ;; Foo::bar( i );
  ;; maybe c-label-key should be fixed instead of commenting this out,
  ;; but it also bothers me that this only seems appropriate for C++
  ;; and not C.
  ;;(modify-syntax-entry ?: "_" c++-mode-syntax-table)
  )

(defvar c++-template-syntax-table nil
  "A variant of `c++-mode-syntax-table' that defines `<' and `>' as
parenthesis characters.  Used temporarily when template argument lists
are parsed.")
(if c++-template-syntax-table
    ()
  (setq c++-template-syntax-table
	(copy-syntax-table c++-mode-syntax-table))
  (modify-syntax-entry ?< "(>" c++-template-syntax-table)
  (modify-syntax-entry ?> ")<" c++-template-syntax-table))

(easy-menu-define c-c++-menu c++-mode-map "C++ Mode Commands"
		  (c-mode-menu "C++"))


;; Support for Objective-C

(defvar objc-mode-abbrev-table nil
  "Abbreviation table used in objc-mode buffers.")
(define-abbrev-table 'objc-mode-abbrev-table ())

(defvar objc-mode-map ()
  "Keymap used in objc-mode buffers.")
(if objc-mode-map
    nil
  (setq objc-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for Objective-C
  (define-key objc-mode-map "\C-c\C-e" 'c-macro-expand))

;;;###autoload
(defvar objc-mode-syntax-table nil
  "Syntax table used in objc-mode buffers.")
(if objc-mode-syntax-table
    ()
  (setq objc-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table objc-mode-syntax-table)
  ;; add extra Objective-C only syntax
  (modify-syntax-entry ?@ "_" objc-mode-syntax-table))

(easy-menu-define c-objc-menu objc-mode-map "ObjC Mode Commands"
		  (c-mode-menu "ObjC"))


;; Support for Java

(defvar java-mode-abbrev-table nil
  "Abbreviation table used in java-mode buffers.")
(define-abbrev-table 'java-mode-abbrev-table ())

(defvar java-mode-map ()
  "Keymap used in java-mode buffers.")
(if java-mode-map
    nil
  (setq java-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for Java
  )

;;;###autoload
(defvar java-mode-syntax-table nil
  "Syntax table used in java-mode buffers.")
(if java-mode-syntax-table
    ()
  (setq java-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table java-mode-syntax-table))

(easy-menu-define c-java-menu java-mode-map "Java Mode Commands"
		  (c-mode-menu "Java"))


;; Support for CORBA's IDL language

(defvar idl-mode-abbrev-table nil
  "Abbreviation table used in idl-mode buffers.")
(define-abbrev-table 'idl-mode-abbrev-table ())

(defvar idl-mode-map ()
  "Keymap used in idl-mode buffers.")
(if idl-mode-map
    nil
  (setq idl-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for IDL
  )

;;;###autoload
(defvar idl-mode-syntax-table nil
  "Syntax table used in idl-mode buffers.")
(if idl-mode-syntax-table
    nil
  (setq idl-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table idl-mode-syntax-table))

(easy-menu-define c-idl-menu idl-mode-map "IDL Mode Commands"
		  (c-mode-menu "IDL"))


;; Support for Pike

(defvar pike-mode-abbrev-table nil
  "Abbreviation table used in pike-mode buffers.")
(define-abbrev-table 'pike-mode-abbrev-table ())

(defvar pike-mode-map ()
  "Keymap used in pike-mode buffers.")
(if pike-mode-map
    nil
  (setq pike-mode-map (c-make-inherited-keymap))
  ;; additional bindings
  (define-key pike-mode-map "\C-c\C-e" 'c-macro-expand))

;;;###autoload
(defvar pike-mode-syntax-table nil
  "Syntax table used in pike-mode buffers.")
(if pike-mode-syntax-table
    ()
  (setq pike-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table pike-mode-syntax-table)
  (modify-syntax-entry ?@ "." pike-mode-syntax-table))

(easy-menu-define c-pike-menu pike-mode-map "Pike Mode Commands"
		  (c-mode-menu "Pike"))



(provide 'cc-langs)
;;; cc-langs.el ends here
