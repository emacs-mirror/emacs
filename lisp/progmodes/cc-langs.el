;;; cc-langs.el --- specific language support for CC Mode

;; Copyright (C) 1985,87,92,93,94,95,96,97 Free Software Foundation, Inc.

;; Authors:    1992-1997 Barry A. Warsaw
;;             1987 Dave Detlefs and Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: cc-mode-help@python.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Version:    5.12
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


;; Regular expressions and other values which must be parameterized on
;; a per-language basis.

;; Keywords defining protection levels
(defconst c-protection-key "\\<\\(public\\|protected\\|private\\)\\>")

;; Regex describing a `symbol' in all languages We cannot use just
;; `word' syntax class since `_' cannot be in word class.  Putting
;; underscore in word class breaks forward word movement behavior that
;; users are familiar with.
(defconst c-symbol-key "\\(\\w\\|\\s_\\)+")


;; keywords introducing class definitions.  language specific
(defconst c-C-class-key "\\(struct\\|union\\)")
(defconst c-C++-class-key "\\(class\\|struct\\|union\\)")

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

(defvar c-class-key c-C-class-key)
(make-variable-buffer-local 'c-class-key)


;; regexp describing access protection clauses.  language specific
(defvar c-access-key nil)
(make-variable-buffer-local 'c-access-key)
(defconst c-C++-access-key (concat c-protection-key "[ \t]*:"))
(defconst c-ObjC-access-key (concat "@" c-protection-key))
(defconst c-Java-access-key nil)


;; keywords introducing conditional blocks
(defconst c-C-conditional-key nil)
(defconst c-C++-conditional-key nil)
(defconst c-Java-conditional-key nil)

(let ((all-kws "for\\|if\\|do\\|else\\|while\\|switch")
      (exc-kws "\\|try\\|catch")
      (thr-kws "\\|finally\\|synchronized")
      (front   "\\b\\(")
      (back    "\\)\\b[^_]"))
  (setq c-C-conditional-key (concat front all-kws back)
	c-C++-conditional-key (concat front all-kws exc-kws back)
	c-Java-conditional-key (concat front all-kws exc-kws thr-kws back)))

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

(defconst c-Java-method-key
  (concat
   "^\\s *[+-]\\s *"
   "\\(([^)]*)\\)?"			; return type
   ;; \\s- in java syntax table does not include \n
   ;; since it is considered the end of //-comments.
   "[ \t\n]*" c-symbol-key))


;; comment starter definitions for various languages.  language specific
(defconst c-C-comment-start-regexp "/[*]")
(defconst c-C++-comment-start-regexp "/[/*]")
;; We need to match all 3 Java style comments
;; 1) Traditional C block; 2) javadoc /** ...; 3) C++ style
(defconst c-Java-comment-start-regexp "/\\(/\\|[*][*]?\\)")
(defvar c-comment-start-regexp c-C-comment-start-regexp)
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

;; Buffer local language-specific comment style flag.
(defvar c-double-slash-is-comments-p nil)
(make-variable-buffer-local 'c-double-slash-is-comments-p)

;; Non-nil means K&R style argument declarations are valid.
(defvar c-recognize-knr-p t)
(make-variable-buffer-local 'c-recognize-knr-p)



(defun c-use-java-style ()
  "Institutes `java' indentation style.
For use with the variable `java-mode-hook'."
  (c-set-style "java"))

(defvar c-styles-are-initialized nil)

(defun c-common-init ()
  ;; Common initializations for all modes.
  (if c-styles-are-initialized
      nil
    (require 'cc-styles)
    (c-initialize-builtin-style)
    (if c-style-variables-are-local-p
	(c-make-styles-buffer-local))
    (setq c-styles-are-initialized t))
  ;; these variables should always be buffer local; they do not affect
  ;; indentation style.
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-multi-line)
  (make-local-variable 'outline-regexp)
  (make-local-variable 'outline-level)
  (make-local-variable 'adaptive-fill-regexp)
  (make-local-variable 'imenu-generic-expression) ;set in the mode functions
  ;; Emacs 19.30 and beyond only, AFAIK
  (if (boundp 'fill-paragraph-function)
      (progn
	(make-local-variable 'fill-paragraph-function)
	(setq fill-paragraph-function 'c-fill-paragraph)))
  ;; now set their values
  (setq paragraph-start (concat page-delimiter "\\|$")
	paragraph-separate paragraph-start
	paragraph-ignore-fill-prefix t
	require-final-newline t
	parse-sexp-ignore-comments t
	indent-line-function 'c-indent-line
	indent-region-function 'c-indent-region
	outline-regexp "[^#\n\^M]"
	outline-level 'c-outline-level
	comment-column 32
	comment-start-skip "/\\*+ *\\|// *"
	adaptive-fill-regexp nil)
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
		  minor-mode-alist))))

(defun c-postprocess-file-styles ()
  "Function that post processes relevant file local variables.
Currently, this function simply applies any style and offset settings
found in the file's Local Variable list.  It first applies any style
setting found in `c-file-style', then it applies any offset settings
it finds in `c-file-offsets'."
  ;; apply file styles and offsets
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


;; Common routines
(defsubst c-make-inherited-keymap ()
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
  (modify-syntax-entry ?\' "\""    table))

(defun c-setup-dual-comments (table)
  ;; Set up TABLE to handle block and line style comments
  (cond
   ;; XEmacs 19 & 20
   ((memq '8-bit c-emacs-features)
    (modify-syntax-entry ?/  ". 1456" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    ;; Give CR the same syntax as newline, for selective-display
    (modify-syntax-entry ?\^m "> b"    table))
   ;; Emacs 19
   ((memq '1-bit c-emacs-features)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    ;; Give CR the same syntax as newline, for selective-display
    (modify-syntax-entry ?\^m "> b"   table))
   ;; incompatible
   (t (error "CC Mode is incompatible with this version of Emacs"))
   ))

(defvar c-mode-base-map ()
  "Keymap shared by all CC Mode related modes.")

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
  ;; Lucid Emacs 19.9 defined these two, the second of which was
  ;; commented out...
  ;; (define-key c-mode-base-map "\e{" 'c-insert-braces)
  ;; Commented out electric square brackets because nobody likes them.
  ;; (define-key c-mode-base-map "[" 'c-insert-brackets)
  (define-key c-mode-base-map "\C-c\C-m"  'c-mark-function)
  (define-key c-mode-base-map "\e\C-q"    'c-indent-exp)
  (define-key c-mode-base-map "\ea"       'c-beginning-of-statement)
  (define-key c-mode-base-map "\ee"       'c-end-of-statement)
  (define-key c-mode-base-map "\C-c\C-n"  'c-forward-conditional)
  (define-key c-mode-base-map "\C-c\C-p"  'c-backward-conditional)
  (define-key c-mode-base-map "\C-c\C-u"  'c-up-conditional)
  (define-key c-mode-base-map "\t"        'c-indent-command)
  ;; Caution!  Enter here at your own risk.  We are trying to support
  ;; several behaviors and it gets disgusting. :-(
  ;;
  ;; In XEmacs 19, Emacs 19, and Emacs 20, we use this to bind
  ;; backwards deletion behavior to DEL, which both Delete and
  ;; Backspace get translated to.  There's no way to separate this
  ;; behavior in a clean way, so deal with it!  Besides, it's been
  ;; this way since the dawn of BOCM.
  (if (not (boundp 'delete-key-deletes-forward))
      (define-key c-mode-base-map "\177" 'c-electric-backspace)
    ;; However, XEmacs 20 actually achieved enlightenment.  It is
    ;; possible to sanely define both backward and forward deletion
    ;; behavior under X separately (TTYs are forever beyond hope, but
    ;; who cares?  XEmacs 20 does the right thing with these too).
    (define-key c-mode-base-map [delete]    'c-electric-delete)
    (define-key c-mode-base-map [backspace] 'c-electric-backspace))
  ;; these are new keybindings, with no counterpart to BOCM
  (define-key c-mode-base-map ","         'c-electric-semi&comma)
  (define-key c-mode-base-map "*"         'c-electric-star)
  (define-key c-mode-base-map "\C-c\C-q"  'c-indent-defun)
  (define-key c-mode-base-map "\C-c\C-\\" 'c-backslash-region)
  ;; TBD: where if anywhere, to put c-backward|forward-into-nomenclature
  (define-key c-mode-base-map "\C-c\C-a"  'c-toggle-auto-state)
  (define-key c-mode-base-map "\C-c\C-b"  'c-submit-bug-report)
  (define-key c-mode-base-map "\C-c\C-c"  'comment-region)
  (define-key c-mode-base-map "\C-c\C-d"  'c-toggle-hungry-state)
  (define-key c-mode-base-map "\C-c\C-e"  'c-macro-expand)
  (define-key c-mode-base-map "\C-c\C-o"  'c-set-offset)
  (define-key c-mode-base-map "\C-c\C-s"  'c-show-syntactic-information)
  (define-key c-mode-base-map "\C-c\C-t"  'c-toggle-auto-hungry-state)
  (define-key c-mode-base-map "\C-c."     'c-set-style)
  ;; conflicts with OOBR
  ;;(define-key c-mode-base-map "\C-c\C-v"  'c-version)
  )

;; menu support for both XEmacs and Emacs.  If you don't have easymenu
;; with your version of Emacs, you are incompatible!
(require 'easymenu)

(defvar c-c-menu nil)
(defvar c-c++-menu nil)
(defvar c-objc-menu nil)
(defvar c-java-menu nil)

(defun c-mode-menu (modestr)
  (let ((m
	 '(["Comment Out Region"     comment-region (mark)]
	   ["Macro Expand Region"    c-macro-expand (mark)]
	   ["Backslashify"           c-backslash-region (mark)]
	   ["Indent Expression"      c-indent-exp
	    (memq (char-after) '(?\( ?\[ ?\{))]
	   ["Indent Line"            c-indent-command t]
	   ["Fill Comment Paragraph" c-fill-paragraph t]
	   ["Up Conditional"         c-up-conditional t]
	   ["Backward Conditional"   c-backward-conditional t]
	   ["Forward Conditional"    c-forward-conditional t]
	   ["Backward Statement"     c-beginning-of-statement t]
	   ["Forward Statement"      c-end-of-statement t]
	   )))
    (cons modestr m)))



;; Support for C

(defvar c-mode-abbrev-table nil
  "Abbrev table in use in c-mode buffers.")
(define-abbrev-table 'c-mode-abbrev-table ())

(defvar c-mode-map ()
  "Keymap used in c-mode buffers.")
(if c-mode-map
    nil
  (setq c-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for C
  )

;;;###autoload
(defvar c-mode-syntax-table nil
  "Syntax table used in c-mode buffers.")
(if c-mode-syntax-table
    ()
  (setq c-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table c-mode-syntax-table)
  ;; add extra comment syntax
  (modify-syntax-entry ?/  ". 14"  c-mode-syntax-table)
  (modify-syntax-entry ?*  ". 23"  c-mode-syntax-table))

(defun c-enable-//-in-c-mode ()
  "Enables // as a comment delimiter in `c-mode'.
ANSI C currently does *not* allow this, although many C compilers
support optional C++ style comments.  To use, call this function from
your `.emacs' file before you visit any C files.  The changes are
global and affect all future `c-mode' buffers."
  (c-setup-dual-comments c-mode-syntax-table)
  (setq-default c-C-comment-start-regexp c-C++-comment-start-regexp))

(easy-menu-define c-c-menu c-mode-map "C Mode Commands"
		  (c-mode-menu "C"))


;; Support for C++

(defvar c++-mode-abbrev-table nil
  "Abbrev table in use in c++-mode buffers.")
(define-abbrev-table 'c++-mode-abbrev-table ())

(defvar c++-mode-map ()
  "Keymap used in c++-mode buffers.")
(if c++-mode-map
    nil
  (setq c++-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for C++
  (define-key c++-mode-map "\C-c:"  'c-scope-operator)
  (define-key c++-mode-map "/"      'c-electric-slash)
  (define-key c++-mode-map "<"      'c-electric-lt-gt)
  (define-key c++-mode-map ">"      'c-electric-lt-gt))

(defvar c++-mode-syntax-table nil
  "Syntax table used in c++-mode buffers.")
(if c++-mode-syntax-table
    ()
  (setq c++-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table c++-mode-syntax-table)
  ;; add extra comment syntax
  (c-setup-dual-comments c++-mode-syntax-table)
  ;; TBD: does it make sense for colon to be symbol class in C++?
  ;; I'm not so sure, since c-label-key is busted on lines like:
  ;; Foo::bar( i );
  ;; maybe c-label-key should be fixed instead of commenting this out,
  ;; but it also bothers me that this only seems appropriate for C++
  ;; and not C.
  ;;(modify-syntax-entry ?: "_" c++-mode-syntax-table)
  )

(easy-menu-define c-c++-menu c++-mode-map "C++ Mode Commands"
		  (c-mode-menu "C++"))


;; Support for Objective-C

(defvar objc-mode-abbrev-table nil
  "Abbrev table in use in objc-mode buffers.")
(define-abbrev-table 'objc-mode-abbrev-table ())

(defvar objc-mode-map ()
  "Keymap used in objc-mode buffers.")
(if objc-mode-map
    nil
  (setq objc-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for Objective-C
  (define-key objc-mode-map "/"      'c-electric-slash))

(defvar objc-mode-syntax-table nil
  "Syntax table used in objc-mode buffers.")
(if objc-mode-syntax-table
    ()
  (setq objc-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table objc-mode-syntax-table)
  ;; add extra comment syntax
  (c-setup-dual-comments objc-mode-syntax-table)
  ;; everyone gets these
  (modify-syntax-entry ?@ "_" objc-mode-syntax-table)
  )

(easy-menu-define c-objc-menu objc-mode-map "ObjC Mode Commands"
		  (c-mode-menu "ObjC"))


;; Support for Java

(defvar java-mode-abbrev-table nil
  "Abbrev table in use in java-mode buffers.")
(define-abbrev-table 'java-mode-abbrev-table ())

(defvar java-mode-map ()
  "Keymap used in java-mode buffers.")
(if java-mode-map
    nil
  (setq java-mode-map (c-make-inherited-keymap))
  ;; add bindings which are only useful for Java
  (define-key java-mode-map "/"      'c-electric-slash))

(defvar java-mode-syntax-table nil
  "Syntax table used in java-mode buffers.")
(if java-mode-syntax-table
    ()
  (setq java-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table java-mode-syntax-table)
  ;; add extra comment syntax
  (c-setup-dual-comments java-mode-syntax-table)
  ;; everyone gets these
  (modify-syntax-entry ?@ "_" java-mode-syntax-table)
  )

(easy-menu-define c-java-menu java-mode-map "Java Mode Commands"
		  (c-mode-menu "Java"))


(provide 'cc-langs)
;;; cc-langs.el ends here
