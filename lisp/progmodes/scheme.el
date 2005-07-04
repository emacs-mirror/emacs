;;; scheme.el --- Scheme (and DSSSL) editing mode

;; Copyright (C) 1986, 1987, 1988, 1997, 1998, 2005
;;           Free Software Foundation, Inc.

;; Author: Bill Rozas <jinx@martigny.ai.mit.edu>
;; Adapted-by: Dave Love <d.love@dl.ac.uk>
;; Keywords: languages, lisp

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The major mode for editing Scheme-type Lisp code, very similar to
;; the Lisp mode documented in the Emacs manual.  `dsssl-mode' is a
;; variant of scheme-mode for editing DSSSL specifications for SGML
;; documents.  [As of Apr 1997, some pointers for DSSSL may be found,
;; for instance, at <URL:http://www.sil.org/sgml/related.html#dsssl>.]
;; All these Lisp-ish modes vary basically in details of the language
;; syntax they highlight/indent/index, but dsssl-mode uses "^;;;" as
;; the page-delimiter since ^L isn't normally a legal SGML character.
;;
;; For interacting with a Scheme interpreter See also `run-scheme' in
;; the `cmuscheme' package and also the implementation-specific
;; `xscheme' package.

;; Here's a recipe to generate a TAGS file for DSSSL, by the way:
;; etags --lang=scheme --regex='/[ \t]*(\(mode\|element\)[ \t
;; ]+\([^ \t(
;; ]+\)/\2/' --regex='/[ \t]*(element[ \t
;; ]*([^)]+[ \t
;; ]+\([^)]+\)[ \t
;; ]*)/\1/' --regex='/(declare[^ \t
;; ]*[ \t
;; ]+\([^ \t
;; ]+\)/\1/' "$@"

;;; Code:

(require 'lisp-mode)

(defvar scheme-mode-syntax-table
  (let ((st (make-syntax-table))
	(i 0))

    ;; Default is atom-constituent.
    (while (< i 256)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))

    ;; Word components.
    (setq i ?0)
    (while (<= i ?9)
      (modify-syntax-entry i "w   " st)
      (setq i (1+ i)))
    (setq i ?A)
    (while (<= i ?Z)
      (modify-syntax-entry i "w   " st)
      (setq i (1+ i)))
    (setq i ?a)
    (while (<= i ?z)
      (modify-syntax-entry i "w   " st)
      (setq i (1+ i)))

    ;; Whitespace
    (modify-syntax-entry ?\t "    " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\f "    " st)
    (modify-syntax-entry ?\r "    " st)
    (modify-syntax-entry ?\s "    " st)

    ;; These characters are delimiters but otherwise undefined.
    ;; Brackets and braces balance for editing convenience.
    (modify-syntax-entry ?\[ "(]  " st)
    (modify-syntax-entry ?\] ")[  " st)
    (modify-syntax-entry ?{ "(}  " st)
    (modify-syntax-entry ?} "){  " st)
    (modify-syntax-entry ?\| "\" 23b" st)

    ;; Other atom delimiters
    (modify-syntax-entry ?\( "()  " st)
    (modify-syntax-entry ?\) ")(  " st)
    (modify-syntax-entry ?\; "<   " st)
    (modify-syntax-entry ?\" "\"    " st)
    (modify-syntax-entry ?' "'   " st)
    (modify-syntax-entry ?` "'   " st)

    ;; Special characters
    (modify-syntax-entry ?, "'   " st)
    (modify-syntax-entry ?@ "'   " st)
    (modify-syntax-entry ?# "' 14bn" st)
    (modify-syntax-entry ?\\ "\\   " st)
    st))

(defvar scheme-mode-abbrev-table nil)
(define-abbrev-table 'scheme-mode-abbrev-table ())

(defvar scheme-imenu-generic-expression
      '((nil
	 "^(define\\(\\|-\\(generic\\(\\|-procedure\\)\\|method\\)\\)*\\s-+(?\\(\\sw+\\)" 4)
	("Types"
	 "^(define-class\\s-+(?\\(\\sw+\\)" 1)
	("Macros"
	 "^(\\(defmacro\\|define-macro\\|define-syntax\\)\\s-+(?\\(\\sw+\\)" 2))
  "Imenu generic expression for Scheme mode.  See `imenu-generic-expression'.")

(defun scheme-mode-variables ()
  (set-syntax-table scheme-mode-syntax-table)
  (setq local-abbrev-table scheme-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil)
  (make-local-variable 'normal-auto-fill-function)
  (setq normal-auto-fill-function 'lisp-mode-auto-fill)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lisp-indent-line)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp ";;; \\|(....")
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (set (make-local-variable 'comment-add) 1)
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+[ \t]*")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'lisp-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'lisp-indent-function)
  (set lisp-indent-function 'scheme-indent-function)
  (setq mode-line-process '("" scheme-mode-line-process))
  (set (make-local-variable 'imenu-case-fold-search) t)
  (setq imenu-generic-expression scheme-imenu-generic-expression)
  (set (make-local-variable 'imenu-syntax-alist)
	'(("+-*/.<>=?!$%_&~^:" . "w")))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((scheme-font-lock-keywords
           scheme-font-lock-keywords-1 scheme-font-lock-keywords-2)
          nil t (("+-*/.<>=!?$%_&~^:#" . "w")) beginning-of-defun
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function . lisp-font-lock-syntactic-face-function))))

(defvar scheme-mode-line-process "")

(defvar scheme-mode-map
  (let ((smap (make-sparse-keymap))
	(map (make-sparse-keymap "Scheme")))
    (set-keymap-parent smap lisp-mode-shared-map)
    (define-key smap [menu-bar scheme] (cons "Scheme" map))
    (define-key map [run-scheme] '("Run Inferior Scheme" . run-scheme))
    (define-key map [uncomment-region]
      '("Uncomment Out Region" . (lambda (beg end)
                                   (interactive "r")
                                   (comment-region beg end '(4)))))
    (define-key map [comment-region] '("Comment Out Region" . comment-region))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . lisp-indent-line))
    (put 'comment-region 'menu-enable 'mark-active)
    (put 'uncomment-region 'menu-enable 'mark-active)
    (put 'indent-region 'menu-enable 'mark-active)
    smap)
  "Keymap for Scheme mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")

;; Used by cmuscheme
(defun scheme-mode-commands (map)
  ;;(define-key map "\t" 'indent-for-tab-command) ; default
  (define-key map "\177" 'backward-delete-char-untabify)
  (define-key map "\e\C-q" 'indent-sexp))

;;;###autoload
(defun scheme-mode ()
  "Major mode for editing Scheme code.
Editing commands are similar to those of `lisp-mode'.

In addition, if an inferior Scheme process is running, some additional
commands will be defined, for evaluating expressions and controlling
the interpreter, and the state of the process will be displayed in the
modeline of all Scheme buffers.  The names of commands that interact
with the Scheme process start with \"xscheme-\" if you use the MIT
Scheme-specific `xscheme' package; for more information see the
documentation for `xscheme-interaction-mode'.  Use \\[run-scheme] to
start an inferior Scheme using the more general `cmuscheme' package.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{scheme-mode-map}
Entry to this mode calls the value of `scheme-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map scheme-mode-map)
  (setq major-mode 'scheme-mode)
  (setq mode-name "Scheme")
  (scheme-mode-variables)
  (run-mode-hooks 'scheme-mode-hook))

(defgroup scheme nil
  "Editing Scheme code."
  :group 'lisp)

(defcustom scheme-mit-dialect t
  "If non-nil, scheme mode is specialized for MIT Scheme.
Set this to nil if you normally use another dialect."
  :type 'boolean
  :group 'scheme)

(defcustom dsssl-sgml-declaration
  "<!DOCTYPE style-sheet PUBLIC \"-//James Clark//DTD DSSSL Style Sheet//EN\">
"
  "*An SGML declaration for the DSSSL file.
If it is defined as a string this will be inserted into an empty buffer
which is in `dsssl-mode'.  It is typically James Clark's style-sheet
doctype, as required for Jade."
  :type '(choice (string :tag "Specified string")
                 (const :tag "None" :value nil))
  :group 'scheme)

(defcustom scheme-mode-hook nil
  "Normal hook run when entering `scheme-mode'.
See `run-hooks'."
  :type 'hook
  :group 'scheme)

(defcustom dsssl-mode-hook nil
  "Normal hook run when entering `dsssl-mode'.
See `run-hooks'."
  :type 'hook
  :group 'scheme)

;; This is shared by cmuscheme and xscheme.
(defcustom scheme-program-name "scheme"
  "*Program invoked by the `run-scheme' command."
  :type 'string
  :group 'scheme)

(defvar dsssl-imenu-generic-expression
  ;; Perhaps this should also look for the style-sheet DTD tags.  I'm
  ;; not sure it's the best way to organize it; perhaps one type
  ;; should be at the first level, though you don't see this anyhow if
  ;; it gets split up.
  '(("Defines"
     "^(define\\s-+(?\\(\\sw+\\)" 1)
    ("Modes"
     "^\\s-*(mode\\s-+\\(\\(\\sw\\|\\s-\\)+\\)" 1)
    ("Elements"
     ;; (element foo ...) or (element (foo bar ...) ...)
     ;; Fixme: Perhaps it should do `root'.
     "^\\s-*(element\\s-+(?\\(\\(\\sw\\|\\s-\\)+\\))?" 1)
    ("Declarations"
     "^(declare\\(-\\sw+\\)+\\>\\s-+\\(\\sw+\\)" 2))
  "Imenu generic expression for DSSSL mode.  See `imenu-generic-expression'.")

(defconst scheme-font-lock-keywords-1
  (eval-when-compile
    (list
     ;;
     ;; Declarations.  Hannes Haug <hannes.haug@student.uni-tuebingen.de> says
     ;; this works for SOS, STklos, SCOOPS, Meroon and Tiny CLOS.
     (list (concat "(\\(define\\*?\\("
		   ;; Function names.
		   "\\(\\|-public\\|-method\\|-generic\\(-procedure\\)?\\)\\|"
		   ;; Macro names, as variable names.  A bit dubious, this.
		   "\\(-syntax\\|-macro\\)\\|"
		   ;; Class names.
		   "-class"
                   ;; Guile modules.
                   "\\|-module"
		   "\\)\\)\\>"
		   ;; Any whitespace and declared object.
		   "[ \t]*(?"
		   "\\(\\sw+\\)?")
	   '(1 font-lock-keyword-face)
	   '(6 (cond ((match-beginning 3) font-lock-function-name-face)
		     ((match-beginning 5) font-lock-variable-name-face)
		     (t font-lock-type-face))
	       nil t))
     ))
  "Subdued expressions to highlight in Scheme modes.")

(defconst scheme-font-lock-keywords-2
  (append scheme-font-lock-keywords-1
   (eval-when-compile
     (list
      ;;
      ;; Control structures.
      (cons
       (concat
	"(" (regexp-opt
	     '("begin" "call-with-current-continuation" "call/cc"
	       "call-with-input-file" "call-with-output-file" "case" "cond"
	       "do" "else" "for-each" "if" "lambda"
	       "let" "let*" "let-syntax" "letrec" "letrec-syntax"
	       ;; Hannes Haug <hannes.haug@student.uni-tuebingen.de> wants:
	       "and" "or" "delay" "force"
	       ;; Stefan Monnier <stefan.monnier@epfl.ch> says don't bother:
	       ;;"quasiquote" "quote" "unquote" "unquote-splicing"
	       "map" "syntax" "syntax-rules") t)
	"\\>") 1)
      ;;
      ;; It wouldn't be Scheme w/o named-let.
      '("(let\\s-+\\(\\sw+\\)"
        (1 font-lock-function-name-face))
      ;;
      ;; David Fox <fox@graphics.cs.nyu.edu> for SOS/STklos class specifiers.
      '("\\<<\\sw+>\\>" . font-lock-type-face)
      ;;
      ;; Scheme `:' and `#:' keywords as builtins.
      '("\\<#?:\\sw+\\>" . font-lock-builtin-face)
      )))
  "Gaudy expressions to highlight in Scheme modes.")

(defvar scheme-font-lock-keywords scheme-font-lock-keywords-1
  "Default expressions to highlight in Scheme modes.")

;;;###autoload
(define-derived-mode dsssl-mode scheme-mode "DSSSL"
  "Major mode for editing DSSSL code.
Editing commands are similar to those of `lisp-mode'.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{scheme-mode-map}
Entering this mode runs the hooks `scheme-mode-hook' and then
`dsssl-mode-hook' and inserts the value of `dsssl-sgml-declaration' if
that variable's value is a string."
  (make-local-variable 'page-delimiter)
  (setq page-delimiter "^;;;" ; ^L not valid SGML char
	major-mode 'dsssl-mode
	mode-name "DSSSL")
  ;; Insert a suitable SGML declaration into an empty buffer.
  ;; FIXME: This should use `auto-insert-alist' instead.
  (and (zerop (buffer-size))
       (stringp dsssl-sgml-declaration)
       (not buffer-read-only)
       (insert dsssl-sgml-declaration))
  (setq font-lock-defaults '(dsssl-font-lock-keywords
			     nil t (("+-*/.<>=?$%_&~^:" . "w"))
			     beginning-of-defun
			     (font-lock-mark-block-function . mark-defun)))
  (set (make-local-variable 'imenu-case-fold-search) nil)
  (setq imenu-generic-expression dsssl-imenu-generic-expression)
  (set (make-local-variable 'imenu-syntax-alist)
       '(("+-*/.<>=?$%_&~^:" . "w"))))

;; Extra syntax for DSSSL.  This isn't separated from Scheme, but
;; shouldn't cause much trouble in scheme-mode.
(put 'element 'scheme-indent-function 1)
(put 'mode 'scheme-indent-function 1)
(put 'with-mode 'scheme-indent-function 1)
(put 'make 'scheme-indent-function 1)
(put 'style 'scheme-indent-function 1)
(put 'root 'scheme-indent-function 1)

(defvar dsssl-font-lock-keywords
  (eval-when-compile
    (list
     ;; Similar to Scheme
     (list "(\\(define\\(-\\w+\\)?\\)\\>[ 	]*\\\((?\\)\\(\\sw+\\)\\>"
	   '(1 font-lock-keyword-face)
	   '(4 font-lock-function-name-face))
     (cons
      (concat "(\\("
	      ;; (make-regexp '("case" "cond" "else" "if" "lambda"
	      ;; "let" "let*" "letrec" "and" "or" "map" "with-mode"))
	      "and\\|c\\(ase\\|ond\\)\\|else\\|if\\|"
	      "l\\(ambda\\|et\\(\\|*\\|rec\\)\\)\\|map\\|or\\|with-mode"
	      "\\)\\>")
      1)
     ;; DSSSL syntax
     '("(\\(element\\|mode\\|declare-\\w+\\)\\>[ 	]*\\(\\sw+\\)"
       (1 font-lock-keyword-face)
       (2 font-lock-type-face))
     '("(\\(element\\)\\>[ 	]*(\\(\\S)+\\))"
       (1 font-lock-keyword-face)
       (2 font-lock-type-face))
     '("\\<\\sw+:\\>" . font-lock-constant-face) ; trailing `:' c.f. scheme
     ;; SGML markup (from sgml-mode) :
     '("<\\([!?][-a-z0-9]+\\)" 1 font-lock-keyword-face)
     '("<\\(/?[-a-z0-9]+\\)" 1 font-lock-function-name-face)))
  "Default expressions to highlight in DSSSL mode.")


(defvar calculate-lisp-indent-last-sexp)

;; Copied from lisp-indent-function, but with gets of
;; scheme-indent-{function,hook}.
(defun scheme-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
					 calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
					(progn (forward-sexp 1) (point))))
	    method)
	(setq method (or (get (intern-soft function) 'scheme-indent-function)
			 (get (intern-soft function) 'scheme-indent-hook)))
	(cond ((or (eq method 'defun)
		   (and (null method)
			(> (length function) 3)
			(string-match "\\`def" function)))
	       (lisp-indent-defform state indent-point))
	      ((integerp method)
	       (lisp-indent-specform method state
				     indent-point normal-indent))
	      (method
		(funcall method state indent-point normal-indent)))))))


;;; Let is different in Scheme

(defun would-be-symbol (string)
  (not (string-equal (substring string 0 1) "(")))

(defun next-sexp-as-string ()
  ;; Assumes that it is protected by a save-excursion
  (forward-sexp 1)
  (let ((the-end (point)))
    (backward-sexp 1)
    (buffer-substring (point) the-end)))

;; This is correct but too slow.
;; The one below works almost always.
;;(defun scheme-let-indent (state indent-point)
;;  (if (would-be-symbol (next-sexp-as-string))
;;      (scheme-indent-specform 2 state indent-point)
;;      (scheme-indent-specform 1 state indent-point)))

(defun scheme-let-indent (state indent-point normal-indent)
  (skip-chars-forward " \t")
  (if (looking-at "[-a-zA-Z0-9+*/?!@$%^&_:~]")
      (lisp-indent-specform 2 state indent-point normal-indent)
    (lisp-indent-specform 1 state indent-point normal-indent)))

;; (put 'begin 'scheme-indent-function 0), say, causes begin to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

(put 'begin 'scheme-indent-function 0)
(put 'case 'scheme-indent-function 1)
(put 'delay 'scheme-indent-function 0)
(put 'do 'scheme-indent-function 2)
(put 'lambda 'scheme-indent-function 1)
(put 'let 'scheme-indent-function 'scheme-let-indent)
(put 'let* 'scheme-indent-function 1)
(put 'letrec 'scheme-indent-function 1)
(put 'sequence 'scheme-indent-function 0) ; SICP, not r4rs
(put 'let-syntax 'scheme-indent-function 1)
(put 'letrec-syntax 'scheme-indent-function 1)
(put 'syntax-rules 'scheme-indent-function 1)
(put 'syntax-case 'scheme-indent-function 2) ; not r5rs

(put 'call-with-input-file 'scheme-indent-function 1)
(put 'with-input-from-file 'scheme-indent-function 1)
(put 'with-input-from-port 'scheme-indent-function 1)
(put 'call-with-output-file 'scheme-indent-function 1)
(put 'with-output-to-file 'scheme-indent-function 1)
(put 'with-output-to-port 'scheme-indent-function 1)
(put 'call-with-values 'scheme-indent-function 1) ; r5rs?
(put 'dynamic-wind 'scheme-indent-function 3) ; r5rs?

;;;; MIT Scheme specific indentation.

(if scheme-mit-dialect
    (progn
      (put 'fluid-let 'scheme-indent-function 1)
      (put 'in-package 'scheme-indent-function 1)
      (put 'local-declare 'scheme-indent-function 1)
      (put 'macro 'scheme-indent-function 1)
      (put 'make-environment 'scheme-indent-function 0)
      (put 'named-lambda 'scheme-indent-function 1)
      (put 'using-syntax 'scheme-indent-function 1)

      (put 'with-input-from-string 'scheme-indent-function 1)
      (put 'with-output-to-string 'scheme-indent-function 0)
      (put 'with-values 'scheme-indent-function 1)

      (put 'syntax-table-define 'scheme-indent-function 2)
      (put 'list-transform-positive 'scheme-indent-function 1)
      (put 'list-transform-negative 'scheme-indent-function 1)
      (put 'list-search-positive 'scheme-indent-function 1)
      (put 'list-search-negative 'scheme-indent-function 1)

      (put 'access-components 'scheme-indent-function 1)
      (put 'assignment-components 'scheme-indent-function 1)
      (put 'combination-components 'scheme-indent-function 1)
      (put 'comment-components 'scheme-indent-function 1)
      (put 'conditional-components 'scheme-indent-function 1)
      (put 'disjunction-components 'scheme-indent-function 1)
      (put 'declaration-components 'scheme-indent-function 1)
      (put 'definition-components 'scheme-indent-function 1)
      (put 'delay-components 'scheme-indent-function 1)
      (put 'in-package-components 'scheme-indent-function 1)
      (put 'lambda-components 'scheme-indent-function 1)
      (put 'lambda-components* 'scheme-indent-function 1)
      (put 'lambda-components** 'scheme-indent-function 1)
      (put 'open-block-components 'scheme-indent-function 1)
      (put 'pathname-components 'scheme-indent-function 1)
      (put 'procedure-components 'scheme-indent-function 1)
      (put 'sequence-components 'scheme-indent-function 1)
      (put 'unassigned\?-components 'scheme-indent-function 1)
      (put 'unbound\?-components 'scheme-indent-function 1)
      (put 'variable-components 'scheme-indent-function 1)))

(provide 'scheme)

;; arch-tag: a8f06bc1-ad11-42d2-9e36-ce651df37a90
;;; scheme.el ends here
