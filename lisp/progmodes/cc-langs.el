;;; cc-langs.el --- language specific settings for CC Mode

;; Copyright (C) 1985,1987,1992-2001 Free Software Foundation, Inc.

;; Authors:    2000- Martin Stjernholm
;;	       1998-1999 Barry A. Warsaw and Martin Stjernholm
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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (require 'cc-bytecomp)))

(cc-require 'cc-defs)
(cc-require 'cc-vars)


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

;; HELPME: Many of the following keyword lists are more or less bogus
;; for some languages (notably ObjC and IDL).  The effects of the
;; erroneous values in the language handling is miniscule since these
;; constants are not used very much (yet, anyway) in the actual syntax
;; detection code, but I'd still appreciate help to get them correct.

;; Primitive type keywords.
(defconst c-C-primitive-type-kwds
  "char\\|double\\|float\\|int\\|long\\|short\\|signed\\|unsigned\\|void")
(defconst c-C++-primitive-type-kwds c-C-primitive-type-kwds)
(defconst c-ObjC-primitive-type-kwds c-C-primitive-type-kwds)
(defconst c-Java-primitive-type-kwds
  "boolean\\|byte\\|char\\|double\\|float\\|int\\|long\\|short\\|void")
(defconst c-IDL-primitive-type-kwds c-C-primitive-type-kwds)
(defconst c-Pike-primitive-type-kwds
  (concat "constant\\|float\\|int\\|mapping\\|multiset\\|object\\|"
	  "program\\|string\\|void"))

;; Declaration specifier keywords.
(defconst c-C-specifier-kwds
  "auto\\|const\\|extern\\|register\\|static\\|volatile")
(defconst c-C++-specifier-kwds
  (concat c-C-specifier-kwds "\\|friend\\|inline\\|virtual"))
(defconst c-ObjC-specifier-kwds c-C++-specifier-kwds)
(defconst c-Java-specifier-kwds
  ;; Note: `const' is not used, but it's still a reserved keyword.
  (concat "abstract\\|const\\|final\\|native\\|private\\|protected\\|"
	  "public\\|static\\|synchronized\\|transient\\|volatile"))
(defconst c-IDL-specifier-kwds c-C++-specifier-kwds)
(defconst c-Pike-specifier-kwds
  (concat "final\\|inline\\|local\\|nomask\\|optional\\|private\\|"
	  "protected\\|static\\|variant"))

;; Class/struct declaration keywords.
(defconst c-C-class-kwds "struct\\|union")
(defconst c-C++-class-kwds (concat c-C-class-kwds "\\|class"))
(defconst c-ObjC-class-kwds "interface\\|implementation")
(defconst c-Java-class-kwds "class\\|interface")
(defconst c-IDL-class-kwds
  (concat c-C++-class-kwds "\\|interface\\|valuetype"))
(defconst c-Pike-class-kwds "class")

;; Keywords introducing other declaration-level blocks.
(defconst c-C-extra-toplevel-kwds "extern")
(defconst c-C++-extra-toplevel-kwds
  (concat c-C-extra-toplevel-kwds "\\|namespace"))
;;(defconst c-ObjC-extra-toplevel-kwds nil)
;;(defconst c-Java-extra-toplevel-kwds nil)
(defconst c-IDL-extra-toplevel-kwds "module")
;;(defconst c-Pike-extra-toplevel-kwds nil)

;; Keywords introducing other declaration-level constructs.
(defconst c-C-other-decl-kwds "enum\\|typedef")
(defconst c-C++-other-decl-kwds (concat c-C-other-decl-kwds "\\|template"))
;;(defconst c-ObjC-other-decl-kwds nil)
(defconst c-Java-other-decl-kwds "import\\|package")
;;(defconst c-IDL-other-decl-kwds nil)
(defconst c-Pike-other-decl-kwds "import\\|inherit")

;; Keywords that occur in declaration-level constructs.
;;(defconst c-C-decl-level-kwds nil)
;;(defconst c-C++-decl-level-kwds nil)
;;(defconst c-ObjC-decl-level-kwds nil)
(defconst c-Java-decl-level-kwds "extends\\|implements\\|throws")
;;(defconst c-IDL-decl-level-kwds nil)
;;(defconst c-Pike-decl-level-kwds nil)

;; Protection label keywords in classes.
;;(defconst c-C-protection-kwds nil)
(defconst c-C++-protection-kwds "private\\|protected\\|public")
(defconst c-ObjC-protection-kwds c-C++-protection-kwds)
;;(defconst c-Java-protection-kwds nil)
;;(defconst c-IDL-protection-kwds nil)
;;(defconst c-Pike-protection-kwds nil)

;; Statement keywords followed directly by a block.
(defconst c-C-block-stmt-1-kwds "do\\|else")
(defconst c-C++-block-stmt-1-kwds
  (concat c-C-block-stmt-1-kwds "\\|asm\\|try"))
(defconst c-ObjC-block-stmt-1-kwds c-C++-block-stmt-1-kwds)
(defconst c-Java-block-stmt-1-kwds
  (concat c-C-block-stmt-1-kwds "\\|finally\\|try"))
;;(defconst c-IDL-block-stmt-1-kwds nil)
(defconst c-Pike-block-stmt-1-kwds c-C-block-stmt-1-kwds)

;; Statement keywords followed by a paren sexp and then by a block.
(defconst c-C-block-stmt-2-kwds "for\\|if\\|switch\\|while")
(defconst c-C++-block-stmt-2-kwds (concat c-C-block-stmt-2-kwds "\\|catch"))
(defconst c-ObjC-block-stmt-2-kwds c-C++-block-stmt-2-kwds)
(defconst c-Java-block-stmt-2-kwds
  (concat c-C++-block-stmt-2-kwds "\\|synchronized"))
;;(defconst c-IDL-block-stmt-2-kwds nil)
(defconst c-Pike-block-stmt-2-kwds c-C-block-stmt-2-kwds)

;; Statement keywords followed by an expression or nothing.
(defconst c-C-simple-stmt-kwds "break\\|continue\\|goto\\|return")
(defconst c-C++-simple-stmt-kwds c-C-simple-stmt-kwds)
(defconst c-ObjC-simple-stmt-kwds c-C-simple-stmt-kwds)
(defconst c-Java-simple-stmt-kwds
  ;; Note: `goto' is not a valid statement, but the keyword is still reserved.
  (concat c-C-simple-stmt-kwds "\\|throw"))
;;(defconst c-IDL-simple-stmt-kwds nil)
(defconst c-Pike-simple-stmt-kwds "break\\|continue\\|return")

;; Keywords introducing labels in blocks.
(defconst c-C-label-kwds "case\\|default")
(defconst c-C++-label-kwds c-C-label-kwds)
(defconst c-ObjC-label-kwds c-C-label-kwds)
(defconst c-Java-label-kwds c-C-label-kwds)
;;(defconst c-IDL-label-kwds nil)
(defconst c-Pike-label-kwds c-C-label-kwds)

;; Keywords that can occur anywhere in expressions.
(defconst c-C-expr-kwds "sizeof")
(defconst c-C++-expr-kwds
  (concat c-C-expr-kwds "\\|delete\\|new\\|operator\\|this\\|throw"))
(defconst c-ObjC-expr-kwds c-C-expr-kwds)
(defconst c-Java-expr-kwds "instanceof\\|new\\|super\\|this")
;;(defconst c-IDL-expr-kwds nil)
(defconst c-Pike-expr-kwds
  (concat c-C-expr-kwds "\\|catch\\|class\\|gauge\\|lambda\\|predef"))

;; All keywords.
(defconst c-C-keywords
  (concat c-C-primitive-type-kwds "\\|" c-C-specifier-kwds
	  "\\|" c-C-class-kwds "\\|" c-C-extra-toplevel-kwds
	  "\\|" c-C-other-decl-kwds
	  ;; "\\|" c-C-decl-level-kwds "\\|" c-C-protection-kwds
	  "\\|" c-C-block-stmt-1-kwds "\\|" c-C-block-stmt-2-kwds
	  "\\|" c-C-simple-stmt-kwds "\\|" c-C-label-kwds
	  "\\|" c-C-expr-kwds))
(defconst c-C++-keywords
  (concat c-C++-primitive-type-kwds "\\|" c-C++-specifier-kwds
	  "\\|" c-C++-class-kwds "\\|" c-C++-extra-toplevel-kwds
	  "\\|" c-C++-other-decl-kwds
	  ;; "\\|" c-C++-decl-level-kwds
	  "\\|" c-C++-protection-kwds
	  "\\|" c-C++-block-stmt-1-kwds "\\|" c-C++-block-stmt-2-kwds
	  "\\|" c-C++-simple-stmt-kwds "\\|" c-C++-label-kwds
	  "\\|" c-C++-expr-kwds))
(defconst c-ObjC-keywords
  (concat c-ObjC-primitive-type-kwds "\\|" c-ObjC-specifier-kwds
	  "\\|" c-ObjC-class-kwds
	  ;; "\\|" c-ObjC-extra-toplevel-kwds
	  ;; "\\|" c-ObjC-other-decl-kwds "\\|" c-ObjC-decl-level-kwds
	  "\\|" c-ObjC-protection-kwds
	  "\\|" c-ObjC-block-stmt-1-kwds "\\|" c-ObjC-block-stmt-2-kwds
	  "\\|" c-ObjC-simple-stmt-kwds "\\|" c-ObjC-label-kwds
	  "\\|" c-ObjC-expr-kwds))
(defconst c-Java-keywords
  (concat c-Java-primitive-type-kwds "\\|" c-Java-specifier-kwds
	  "\\|" c-Java-class-kwds
	  ;; "\\|" c-Java-extra-toplevel-kwds
	  "\\|" c-Java-other-decl-kwds "\\|" c-Java-decl-level-kwds
	  ;; "\\|" c-Java-protection-kwds
	  "\\|" c-Java-block-stmt-1-kwds "\\|" c-Java-block-stmt-2-kwds
	  "\\|" c-Java-simple-stmt-kwds "\\|" c-Java-label-kwds
	  "\\|" c-Java-expr-kwds))
(defconst c-IDL-keywords
  (concat c-IDL-primitive-type-kwds "\\|" c-IDL-specifier-kwds
	  "\\|" c-IDL-class-kwds "\\|" c-IDL-extra-toplevel-kwds
	  ;; "\\|" c-IDL-other-decl-kwds "\\|" c-IDL-decl-level-kwds
	  ;; "\\|" c-IDL-protection-kwds
	  ;; "\\|" c-IDL-block-stmt-1-kwds "\\|" c-IDL-block-stmt-2-kwds
	  ;; "\\|" c-IDL-simple-stmt-kwds "\\|" c-IDL-label-kwds
	  ;; "\\|" c-IDL-expr-kwds)
	  ))
(defconst c-Pike-keywords
  (concat c-Pike-primitive-type-kwds "\\|" c-Pike-specifier-kwds
	  "\\|" c-Pike-class-kwds
	  ;; "\\|" c-Pike-extra-toplevel-kwds
	  "\\|" c-Pike-other-decl-kwds
	  ;; "\\|" c-Pike-decl-level-kwds "\\|" c-Pike-protection-kwds
	  "\\|" c-Pike-block-stmt-1-kwds "\\|" c-Pike-block-stmt-2-kwds
	  "\\|" c-Pike-simple-stmt-kwds "\\|" c-Pike-label-kwds
	  "\\|" c-Pike-expr-kwds))

(defvar c-keywords nil)
(make-variable-buffer-local 'c-keywords)

;; Keywords defining protection levels
(defconst c-protection-key "\\<\\(public\\|protected\\|private\\)\\>")

;; Regexps introducing class definitions.
(defconst c-C-class-key (c-paren-re c-C-class-kwds))
(defconst c-C++-class-key (c-paren-re c-C++-class-kwds))
(defconst c-IDL-class-key (c-paren-re c-IDL-class-kwds))
(defconst c-ObjC-class-key
  (concat
   "@\\(" c-ObjC-class-kwds "\\)\\s +"
   c-symbol-key				;name of the class
   "\\(\\s *:\\s *" c-symbol-key "\\)?"	;maybe followed by the superclass
   "\\(\\s *<[^>]+>\\)?"		;and maybe the adopted protocols list
   ))
(defconst c-Java-class-key
  (concat
   "\\(" c-protection-key "\\s +\\)?"
   "\\(" c-Java-class-kwds "\\)\\s +"
   c-symbol-key				      ;name of the class
   "\\(\\s *extends\\s *" c-symbol-key "\\)?" ;maybe followed by superclass
   ;;"\\(\\s *implements *[^{]+{\\)?"	      ;maybe the adopted protocols list
   ))
(defconst c-Pike-class-key (c-paren-re c-Pike-class-kwds))

(defvar c-class-key c-C-class-key)
(make-variable-buffer-local 'c-class-key)

(defconst c-C-extra-toplevel-key (c-paren-re c-C-extra-toplevel-kwds))
(defconst c-C++-extra-toplevel-key (c-paren-re c-C++-extra-toplevel-kwds))
(defconst c-IDL-extra-toplevel-key (c-paren-re c-IDL-extra-toplevel-kwds))

(defvar c-extra-toplevel-key c-C-extra-toplevel-key)
(make-variable-buffer-local 'c-extra-toplevel-key)

;; Keywords that can introduce bitfields in the languages that supports that.
(defconst c-C-bitfield-key "\\(char\\|int\\|long\\|signed\\|unsigned\\)")

(defvar c-bitfield-key nil)
(make-variable-buffer-local 'c-bitfield-key)

;; regexp describing access protection clauses.  language specific
(defvar c-access-key nil)
(make-variable-buffer-local 'c-access-key)
(defconst c-C++-access-key
  (concat "\\<\\(" c-C++-protection-kwds "\\)\\>[ \t]*:"))
;;(defconst c-IDL-access-key nil)
(defconst c-ObjC-access-key (concat "@" c-protection-key))
;;(defconst c-Java-access-key nil)
;;(defconst c-Pike-access-key nil)

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
      (front   "\\<\\(")
      (back    "\\)\\>[^_]"))
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

;; Regexp to append to paragraph-start.
(defvar c-append-paragraph-start "$")
(make-variable-buffer-local 'c-append-paragraph-start)
(defconst c-Java-javadoc-paragraph-start
  "\\(@[a-zA-Z]+\\>\\|$\\)")
(defconst c-Pike-pikedoc-paragraph-start
  "\\(@[a-zA-Z]+\\>\\([^{]\\|$\\)\\|$\\)")

;; Regexp to append to paragraph-separate.
(defvar c-append-paragraph-separate "$")
(make-variable-buffer-local 'c-append-paragraph-separate)
(defconst c-Pike-pikedoc-paragraph-separate c-Pike-pikedoc-paragraph-start)

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
(defconst c-Pike-inexpr-class-key "\\<class\\>")

;; List of open- and close-chars that makes up a pike-style brace
;; list, ie for a `([ ])' list there should be a cons (?\[ . ?\]) in
;; this list.
(defvar c-special-brace-lists nil)
(make-variable-buffer-local 'c-special-brace-lists)
(defconst c-Pike-special-brace-lists '((?{ . ?})
				       (?\[ . ?\])
				       (?< . ?>)))


;; Syntax tables.

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

;;;###autoload
(defvar c-mode-syntax-table nil
  "Syntax table used in c-mode buffers.")
(if c-mode-syntax-table
    ()
  (setq c-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table c-mode-syntax-table))

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

;;;###autoload
(defvar objc-mode-syntax-table nil
  "Syntax table used in objc-mode buffers.")
(if objc-mode-syntax-table
    ()
  (setq objc-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table objc-mode-syntax-table)
  ;; add extra Objective-C only syntax
  (modify-syntax-entry ?@ "_" objc-mode-syntax-table))

;;;###autoload
(defvar java-mode-syntax-table nil
  "Syntax table used in java-mode buffers.")
(if java-mode-syntax-table
    ()
  (setq java-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table java-mode-syntax-table))

;;;###autoload
(defvar idl-mode-syntax-table nil
  "Syntax table used in idl-mode buffers.")
(if idl-mode-syntax-table
    nil
  (setq idl-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table idl-mode-syntax-table))

;;;###autoload
(defvar pike-mode-syntax-table nil
  "Syntax table used in pike-mode buffers.")
(if pike-mode-syntax-table
    ()
  (setq pike-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table pike-mode-syntax-table)
  (modify-syntax-entry ?@ "." pike-mode-syntax-table))


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


(cc-provide 'cc-langs)
;;; cc-langs.el ends here
