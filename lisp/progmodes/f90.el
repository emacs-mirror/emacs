;; f90.el --- Fortran-90 mode (free format) for GNU Emacs 19.x.
;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Torbj\"orn Einarsson <tfkte@fy.chalmers.se>
;; Created: Jan 21, 1995
;; Version: 1.03
;; Keywords: fortran, f90, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; Smart mode for editing F90 programs in FREE FORMAT.
;; Knows about continuation lines, named structured statements, and other
;; new features in F90 including HPF (High Performance Fortran) structures.
;; The basic feature is to provide an accurate indentation of F90 programs.
;; In addition, there are many more features like automatic matching of all
;; end statements, an auto-fill function to break long lines, a join-lines
;; function which joins continued lines etc etc.
;;  To facilitate typing, a fairly complete list of abbreviations is provided.
;;    For example, `i is short-hand for integer (if abbrev-mode is on).

;; There are two separate features for highlighting the code.
;;   1) Upcasing or capitalizing of all keywords.
;;   2) Different colors/fonts for different structures. (X-windows)
;;         (using either font-lock-mode or hilit19)
;;  Automatic upcase of downcase of keywords is controlled by the parameter
;;  f90-auto-keyword-case.
;;  To use hilit19 it must be loaded before this file. Then the parameter
;;  f90-auto-hilit19 determines if each line should be updated automatically.

;; The indentations of lines starting with ! is determined by the first of the
;; following matches:
;; start-string   indent          variable holding start-string
;;    !!!            0
;;    !hpf$          0            f90-directive-comment (default nil)
;;    !!$            0            f90-comment-region    (default !!$)
;;    !           as code         f90-indented-comment  (a regexp, default !)
;;    default     comment-column
;; Ex: Here is the result of three different settings of f90-indented-comment
;;     f90-indented-comment  !-indentation      !!-indentation
;;          !                  as code             as code
;;          !!                 comment-column      as code
;;          ![^!]              as code             comment-column
;; Trailing comments are indented to comment-column with indent-for-comment M-;
;; f90-comment-region (C-c;) toggles insertion of f90-comment-region in region.

;; One common convention for free vs. fixed format is that free-format files
;; have the ending .f90 while the fixed format files have the ending .f. 
;; To make f90-mode work, put this file in, for example, your directory
;;  ~/lisp, and be sure that you have the following in your .emacs-file
;;     (setq load-path (append load-path '("~/lisp")))
;;     (autoload 'f90-mode "f90"
;;       "Major mode for editing Fortran 90 code in free format." t)
;;     (setq auto-mode-alist (append auto-mode-alist 
;;                           (list '("\\.f90$" . f90-mode))))
;; Once you have entered f90-mode, you may get more info by using
;; the command describe-mode (C-h m). For online help describing various
;; functions use  C-h f <Name of function you want described>

;; To customize the f90-mode for your taste, use, for example:
;;    (you don't have to specify values for all the parameters below)
;;(setq f90-mode-hook
;;      '(lambda () (setq f90-do-indent 3
;;                        f90-if-indent 3
;;                        f90-type-indent 3
;;                        f90-program-indent 2
;;                        f90-continuation-indent 5
;;                        f90-comment-region "!!$"
;;                        f90-directive-comment nil
;;                        f90-indented-comment "!"
;;                        f90-break-delimiters "[-+\\*/,><=% \t]"
;;                        f90-break-before-delimiters t
;;                        f90-beginning-ampersand t
;;                        f90-smart-end 'blink
;;                        f90-auto-keyword-case nil
;;                        f90-auto-hilit19 t
;;                        f90-leave-line-no  nil
;;                        f90-startup-message t)
;;       ;;The rest is not default.
;;       (abbrev-mode 1)             ; turn on abbreviation mode
;;       (f90-auto-fill-mode 1)      ; turn on auto-filling
;;       (if f90-auto-keyword-case   ; change case of all keywords on startup
;;           (f90-change-keywords f90-auto-keyword-case))
;;	 ))
;; in your .emacs file (the shown values are the defaults). You can also
;; change the values of the lists f90-keywords etc.
;; The auto-fill and abbreviation minor modes are accessible from the menu,
;; or by using M-x f90-auto-fill-mode and M-x abbrev-mode, respectively.

;; Remarks
;; 1) Line numbers are by default left-justified. If f90-leave-line-no is
;;    non-nil, the line numbers are never touched.
;; 2) Multi-; statements like > do i=1,20 ; j=j+i ; end do < are not handled
;;    correctly, but I imagine them to be rare.
;; 3) You can use either hilit19 or font-lock-mode for your highlighting
;;   a) To use hilit19, be sure that hilit19 is loaded before this file
;;   b) To use font-lock-mode, nothing special is needed.
;; 4) For FIXED FORMAT code, use the ordinary fortran mode.
;; 5) This mode does not work under emacs-18.x.

;; List of user commands
;;   f90-previous-statement         f90-next-statement
;;   f90-beginning-of-subprogram    f90-end-of-subprogram   f90-mark-subprogram
;;   f90-comment-region
;;   f90-indent-line                f90-indent-new-line
;;   f90-indent-region    (can be called by calling indent-region)
;;   f90-indent-subprogram
;;   f90-break-line                 f90-join-lines
;;   f90-auto-fill-mode
;;   f90-fill-region
;;   f90-insert-end
;;   f90-upcase-keywords            f90-upcase-region-keywords
;;   f90-downcase-keywords          f90-downcase-region-keywords
;;   f90-capitalize-keywords        f90-capitalize-region-keywords

;; Thanks to all the people who have tested the mode. Special thanks to Jens
;; Bloch Helmers for encouraging me to write this code, for creative
;; suggestions as well as for the lists of hpf-commands.
;; Also thanks to the authors of the fortran and pascal modes, on which some
;; of this code is built.

;;; Code:
(defconst f90-mode-version "version 1.03")
(defconst bug-f90-mode "tfkte@fy.chalmers.se"
  "Address of mailing list for F90 mode bugs.")

;; User options
(defvar f90-do-indent 3
  "*Extra indentation applied to DO blocks.")

(defvar f90-if-indent 3
  "*Extra indentation applied to IF, SELECT CASE, WHERE and FORALL blocks.")

(defvar f90-type-indent 3
  "*Extra indentation applied to TYPE, INTERFACE and BLOCK DATA blocks.")

(defvar f90-program-indent 2
  "*Extra indentation applied to PROGRAM/MODULE/SUBROUTINE/FUNCTION blocks.")

(defvar f90-continuation-indent 5
  "*Extra indentation applied to F90 continuation lines.")

(defvar f90-comment-region "!!$"
  "*String inserted by \\[f90-comment-region]\
 at start of each line in region.")

(defvar f90-indented-comment "!"
  "*Comments to be indented like code.")

(defvar f90-directive-comment nil
  "*String of comment-like directive like \"!HPF$\", not to be indented.")

(defvar f90-beginning-ampersand t
  "*t makes automatic insertion of \& at beginning of continuation line.")

(defvar f90-smart-end 'blink
  "*From an END statement, check and fill the end using matching block start.
Allowed values are 'blink, 'no-blink, and nil, which determine
whether to blink the matching beginning.")

(defvar f90-break-delimiters "[-+\\*/><=,% \t]"
  "*Regexp holding list of delimiters at which lines may be broken.")

(defvar f90-break-before-delimiters t
  "*Non-nil causes `f90-do-auto-fill' to break lines before delimiters.")

(defvar f90-auto-keyword-case nil
  "*Automatic case conversion of keywords.
  The options are 'downcase-word, 'upcase-word, 'capitalize-word and nil")

(defvar f90-auto-hilit19 t
  "*Automatic highlight of line at every indent or newline (for hilit19).")

(defvar f90-leave-line-no nil
  "*If nil, left-justify linenumbers.")

(defvar f90-startup-message t
  "*Non-nil displays a startup message when F90 mode is first called.")

(defvar f90-keywords
  '("allocate" "allocatable" "assign" "assignment" "backspace" "block"
    "call" "case" "character" "close" "common" "complex" "contains"
    "continue" "cycle" "data" "deallocate" "dimension" "do" "double" "else"
    "elseif" "elsewhere" "end" "enddo" "endfile" "endif" "entry" "equivalence"
    "exit" "external" "forall" "format" "function" "goto" "if" "implicit"
    "include" "inquire" "integer" "intent" "interface" "intrinsic" "logical"
    "module" "namelist" "none" "nullify" "open" "optional" "parameter"
    "pause" "pointer" "precision" "print" "private" "procedure" "program"
    "public" "read" "real" "recursive" "return" "rewind" "save" "select"
    "sequence" "stop" "subroutine" "target" "then" "type" "use" "where"
    "while" "write")
  "*List of f90-keywords.")

(defvar f90-intrinsic-procedures
  '("abs" "achar" "acos" "adjustl" "adjustr" "aimag" "aint" "all" "allocated"
    "anint" "any" "asin" "associated" "atan" "atan2" "bit_size" "btest"
    "ceiling" "char" "cmplx" "conjg" "cos" "cosh" "count" "cshift"
    "date_and_time" "dble" "digits" "dim" "dot_product" "dprod" "eoshift"
    "epsilon" "exp" "exponent" "floor" "fraction" "huge" "iachar" "iand"
    "ibclr" "ibits" "ibset" "ichar" "ieor" "index" "int" "ior" "ishft"
    "ishftc" "kind" "lbound" "len" "len_trim" "lge" "lgt" "lle" "llt" "log"
    "logical" "log10" "matmul" "max" "maxexponent" "maxloc" "maxval" "merge"
    "min" "minexponent" "minloc" "minval" "mod" "modulo" "mvbits" "nearest"
    "nint" "not" "pack" "precision" "present" "product" "radix" "random_number"
    "random_seed" "range" "real" "repeat" "reshape" "rrspacing" "scale"
    "scan" "selected_int_kind" "selected_real_kind" "set_exponent" "shape"
    "sign" "sin" "sinh" "size" "spacing" "spread" "sqrt" "sum"
    "system_clock" "tan" "tanh" "tiny" "transfer" "transpose" "trim"
    "ubound" "unpack" "verify")
  "*List of F90 intrinsic procedures.")

(defvar f90-hpf-procedures
  '("all_prefix" "all_scatter" "all_suffix" "any_prefix" "any_scatter"
    "any_suffix" "copy_prefix" "copy_scatter" "copy_suffix" "count_prefix"
    "count_scatter" "count_suffix" "grade_down" "grade_up" "hpf_alignment"
    "hpf_template" "hpf_distribution" "iall" "iall_prefix" "iall_scatter"
    "iall_suffix" "iany" "iany_prefix" "iany_scatter" "iany_suffix" "iparity"
    "iparity_prefix" "iparity_scatter" "iparity_suffix" "leadz" "maxval_prefix"
    "maxval_scatter" "maxval_suffix" "minval_prefix" "minval_scatter" 
    "minval_suffix" "parity" "parity_prefix" "parity_scatter" "parity_suffix"
    "popcnt" "poppar" "product_prefix" "product_scatter" "product_suffix"
    "sum_prefix" "sum_scatter" "sum_suffix" "ilen" "number_of_processors"
    "processors_shape")
  "*List of hpf intrinsic procedures.")

(defvar f90-hpf-directives
  '("align" "distribute" "dynamic" "inherit" "template" "processors"
    "realign" "redistribute" "independent")
  "*List of hpf directives.")

(defvar f90-hpf-keywords
  '("pure" "extrinsic" "new" "with" "onto" "block" "cyclic")
  "*List of hpf keywords.")

;; Highlighting patterns

(defconst f90-font-lock-keywords-1
  (purecopy
   (list
    ;; Subroutine and function declarations
    '("^[ \t]*\\(program\\|module\\)[ \t]+\\sw+" 1 font-lock-keyword-face)
    '("^[ \t]*\\(program\\|module\\)[ \t]+\\(\\sw+\\)" 2
      font-lock-function-name-face)
    '("\\(^.*\\(function\\|subroutine\\)\\)[ \t]+\\sw+" 1
      font-lock-keyword-face)
    '("^.*\\(function\\|subroutine\\)[ \t]+\\(\\sw+\\)" 2
      font-lock-function-name-face)
    '("^[ \t]*end[ \t]*\\(program\\|module\\|function\\|subroutine\\|type\\)"
      . font-lock-keyword-face)
    (list (concat "^[ \t]*end[ \t]*\\(program\\|module\\|function\\|"
		  "subroutine\\|type\\)[ \t]+\\(\\sw+\\)") 2 
		  'font-lock-function-name-face)
    '("^[ \t]*\\(type\\)[ \t]+\\sw+" 1 font-lock-keyword-face)
    '("^[ \t]*type[ \t]+\\(\\sw+\\)" 1 font-lock-function-name-face)
    '("^[ \t]*\\(end[ \t]*\\)?interface\\>" . font-lock-keyword-face)
    '("^[ \t]*contains\\>" . font-lock-keyword-face)))
  "For consideration as a value of `f90-font-lock-keywords-1'.
This does fairly subdued highlighting of comments and function names.")

(defconst f90-font-lock-keywords-2
  (purecopy
   (append f90-font-lock-keywords-1
    (list
     ;; Variable declarations
     '("\\(\\(real\\|integer\\|character\\|complex\\|logical\\|type\\).*\\)::"
       1 font-lock-type-face)
     '("implicit[ \t]*none" . font-lock-keyword-face)
     '("^[ \t]*\\(\\sw+[ \t]*:[ \t]*\\)?\\(do\\([ \t]*while\\)?\\)\\>"
       2 font-lock-keyword-face)
     '("^[ \t]*\\(\\sw+\\)[ \t]*:[ \t]*\\(do\\([ \t]*while\\)?\\)\\>" 1
       font-lock-function-name-face)
     '("^[ \t]*\\(end[ \t]*do\\)\\>" 1 font-lock-keyword-face)
     '("^[ \t]*end[ \t]*do[ \t]+\\(\\sw+\\)" 1 font-lock-function-name-face)
     '("^[ \t]*\\(\\sw+[ \t]*:[ \t]*\\)?\\(if\\)\\>" 2 
       font-lock-keyword-face)
     '("^[ \t]*\\(\\sw+\\)[ \t]*:[ \t]*if\\>" 1 font-lock-function-name-face)
     '("^[ \t]*\\(end[ \t]*if\\)\\>" 1 font-lock-keyword-face)
     '("^[ \t]*end[ \t]*if[ \t]+\\(\\sw+\\)" 1 font-lock-function-name-face)
     '("^[ \t]*\\(\\sw+[ \t]*:[ \t]*\\)?\\(select[ \t]*case\\)\\>" 2
       font-lock-keyword-face)
     '("^[ \t]*\\(\\sw+\\)[ \t]*:[ \t]*\\(select[ \t]*case\\)\\>" 1
       font-lock-function-name-face)
     '("^[ \t]*end[ \t]*select\\>" . font-lock-keyword-face)
     '("^[ \t]*end[ \t]*select\\>[ \t]+\\(\\sw+\\)" 1
       font-lock-function-name-face)
     '("\\(where\\|forall\\)[ \t]*(" 1 font-lock-keyword-face)
     '("\\<\\(elsewhere\\|else\\|else[ \t]*if\\)\\>" . font-lock-keyword-face)
     '("\\<end[ \t]*\\(where\\|forall\\)\\>" . font-lock-keyword-face)
     '("\\<then\\>" . font-lock-keyword-face)
     '("\\<\\(exit\\|cycle\\)\\>" . font-lock-keyword-face)
     '("\\<\\(exit\\|cycle\\)[ \t]*\\(\\sw+\\)\\>" 2
       font-lock-function-name-face)
     '("\\<\\(stop\\|return\\)\\>" . font-lock-keyword-face)
     '("^[ \t]*\\(case\\)[ \t]*\\((\\|default\\)" 1 font-lock-keyword-face)
     (concat "\\<\\("(mapconcat 'identity f90-keywords "\\|") "\\)\\>")
     (concat "\\<\\("(mapconcat 'identity f90-intrinsic-procedures "\\|")
	     "\\)\\>")
     (concat "\\<\\("(mapconcat 'identity f90-hpf-procedures "\\|")
	     "\\)\\>")
     (concat "\\<\\("(mapconcat 'identity f90-hpf-directives "\\|")
	     "\\)\\>")
     (concat "\\<\\("(mapconcat 'identity f90-hpf-keywords "\\|")
	     "\\)\\>"))))
  "For consideration as a value of `f90-font-lock-keywords'.
This highlights variable types, \"keywords,\" etc.")

(defvar f90-font-lock-keywords f90-font-lock-keywords-2
  "*Additional expressions to highlight in F90 mode.")

;; hilit19 customization and expressions
(defvar f90-face-string 'named-param "*Face for strings.")
(defvar f90-face-comment 'comment "*Face for comments.")
(defvar f90-face-decl 'include "*Face for declarations.")
(defvar f90-face-prog 'defun "*Face for program blocks.")
(defvar f90-face-label 'Tomato-bold "*Face for labels.")
(defvar f90-face-type 'defun "*Face for type blocks.")
(defvar f90-face-interface 'defun "*Face for interface blocks.")
(defvar f90-face-contains 'defun "*Face for contains statement.")
(defvar f90-face-do 'SteelBlue-bold "*Face for do-structure.")
(defvar f90-face-if 'define "*Face for if-structure.")
(defvar f90-face-select 'define "*Face for select-case structure.")
(defvar f90-face-stop 'defun "*Face for stop and return.")
(defvar f90-face-exit 'SteelBlue-bold "*Face for exit and cycle.")
(defvar f90-face-keyword 'struct "*Face for keywords.")
(defvar f90-face-intrinsics 'struct "*Face for intrinsic procedures.")
;; Highlighting for HPF (High-Peformance Fortran)
(defvar f90-face-hpf-procedures 'struct "*Face for hpf procedures.")
(defvar f90-face-hpf-directives 'struct "*Face for hpf directives.")
(defvar f90-face-hpf-keywords   'struct "*Face for hpf keywords.")

(if (fboundp 'hilit-set-mode-patterns)
    (hilit-set-mode-patterns
     'f90-mode
     (list
      ;; Allow for strings delimited by ' and by " and for multirow strings.
      ;; A multi-row string includes &\n& (+ possible whitespace and comments)
      (list (concat
	     "\\(\"[^\"\n]*\\(&[ \t]*\\(![^\n]*\\)?\n[ \t]*&[^\"\n]*\\)*\""
	     "\\|'[^'\n]*\\(&[ \t]*\\(![^\n]*\\)?\n[ \t]*&[^'\n]*\\)*'\\)")
	    nil f90-face-string)
      (list "!" "$" f90-face-comment)
      (list "\\(\\(real\\|integer\\|character\\|complex\\|logical\
\\|type\\).*\\)::" 1 f90-face-decl)
      (list "implicit[ \t]*none" nil f90-face-decl)
      (list "^[ \t]*\\(program\\|module\\)[ \t]+\\sw+" 1 f90-face-prog)
      (list "^[ \t]*\\(program\\|module\\)[ \t]+\\(\\sw+\\)" 2 f90-face-label)
      (list "\\(^.*\\(function\\|subroutine\\)\\)[ \t]+\\sw+" 1
	    f90-face-prog)
      (list "^.*\\(function\\|subroutine\\)[ \t]+\\(\\sw+\\)" 2
	    f90-face-label)
      (list "^[ \t]*end[ \t]*\\(program\\|module\\|function\
\\|subroutine\\|type\\)" nil f90-face-prog)
      (list (concat "^[ \t]*end[ \t]*\\(program\\|module\\|function\\|"
		    "subroutine\\|type\\)[ \t]+\\(\\sw+\\)") 2 f90-face-label)
      (list "^[ \t]*\\(type\\)[ \t]+\\sw+" 1 f90-face-type)
      (list "^[ \t]*type[ \t]+\\(\\sw+\\)" 1 f90-face-label)
      (list "^[ \t]*\\(end[ \t]*\\)?interface\\>" nil f90-face-interface)
      (list "^[ \t]*contains\\>" nil f90-face-contains)
      (list "^[ \t]*\\(\\sw+[ \t]*:[ \t]*\\)?\\(do\\([ \t]*while\\)?\\)\\>"
	    2 f90-face-do)
      (list "^[ \t]*\\(\\sw+\\)[ \t]*:[ \t]*\\(do\\([ \t]*while\\)?\\)\\>" 1
	    f90-face-label)
      (list "^[ \t]*\\(end[ \t]*do\\)\\>" 1 f90-face-do)
      (list "^[ \t]*end[ \t]*do[ \t]+\\(\\sw+\\)" 1 f90-face-label)
      (list "^[ \t]*\\(\\sw+[ \t]*:[ \t]*\\)?\\(if\\)\\>" 2 f90-face-if)
      (list "^[ \t]*\\(\\sw+\\)[ \t]*:[ \t]*if\\>" 1 f90-face-label)
      (list "^[ \t]*\\(end[ \t]*if\\)\\>" 1 f90-face-if)
      (list "^[ \t]*end[ \t]*if[ \t]+\\(\\sw+\\)" 1 f90-face-label)
      (list "^[ \t]*\\(\\sw+[ \t]*:[ \t]*\\)?\\(select[ \t]*case\\)\\>" 2
	    f90-face-select)
      (list "^[ \t]*\\(\\sw+\\)[ \t]*:[ \t]*\\(select[ \t]*case\\)\\>" 1
	    f90-face-label)
      (list "^[ \t]*end[ \t]*select\\>" nil f90-face-select)
      (list "^[ \t]*end[ \t]*select\\>[ \t]+\\(\\sw+\\)" 1 f90-face-label)
      (list "\\(where\\|forall\\)[ \t]*(" 1 f90-face-if)
      (list "\\<\\(elsewhere\\|else\\|else[ \t]*if\\)\\>" nil f90-face-if)
      (list "\\<end[ \t]*\\(where\\|forall\\)\\>" nil f90-face-if)
      (list "\\<then\\>" nil f90-face-if)
      (list "\\<\\(exit\\|cycle\\)\\>" nil f90-face-exit)
      (list "\\<\\(exit\\|cycle\\)[ \t]*\\sw+\\>" nil f90-face-label)
      (list "\\<\\(stop\\|return\\)\\>" nil f90-face-stop)
      (list "^[ \t]*\\(case\\)[ \t]*\\((\\|default\\)" 1 f90-face-select)
      (list (concat "\\<\\("(mapconcat 'identity f90-keywords "\\|")
		    "\\)\\>") nil f90-face-keyword)
    (list (concat "\\<\\("(mapconcat 'identity f90-intrinsic-procedures "\\|")
		  "\\)\\>") nil f90-face-intrinsics)
    (list (concat "\\<\\("(mapconcat 'identity f90-hpf-procedures "\\|")
		  "\\)\\>") nil f90-face-hpf-procedures)
    (list (concat "\\<\\("(mapconcat 'identity f90-hpf-directives "\\|")
		  "\\)\\>") nil f90-face-hpf-directives)
    (list (concat "\\<\\("(mapconcat 'identity f90-hpf-keywords "\\|")
		  "\\)\\>") nil f90-face-hpf-keywords))
   nil 'case-insensitive))

;; syntax table
(defvar f90-mode-syntax-table nil
  "Syntax table in use in F90 mode buffers.")

(if f90-mode-syntax-table
    ()
  (setq f90-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\! "<" f90-mode-syntax-table)  ; beg. comment
  (modify-syntax-entry ?\n ">" f90-mode-syntax-table)  ; end comment
  (modify-syntax-entry ?_ "w" f90-mode-syntax-table)   ; underscore in names
  (modify-syntax-entry ?\' "\"" f90-mode-syntax-table) ; string quote
  (modify-syntax-entry ?\" "\"" f90-mode-syntax-table) ; string quote
  (modify-syntax-entry ?\` "w" f90-mode-syntax-table)  ; for abbrevs
  (modify-syntax-entry ?\r " " f90-mode-syntax-table)  ; return is whitespace
  (modify-syntax-entry ?+ "." f90-mode-syntax-table)  
  (modify-syntax-entry ?- "." f90-mode-syntax-table)
  (modify-syntax-entry ?= "." f90-mode-syntax-table)
  (modify-syntax-entry ?* "." f90-mode-syntax-table)
  (modify-syntax-entry ?/ "." f90-mode-syntax-table)
  (modify-syntax-entry ?\\ "/" f90-mode-syntax-table)) ; escape chars

;; keys
(defvar f90-mode-map ()
  "Keymap used in F90 mode.")
(if f90-mode-map
    ()
  (setq f90-mode-map (make-sparse-keymap))
  (define-key f90-mode-map "`"        'f90-abbrev-start)
  (define-key f90-mode-map "\C-c;"    'f90-comment-region)
  (define-key f90-mode-map "\C-\M-a"  'f90-beginning-of-subprogram)
  (define-key f90-mode-map "\C-\M-e"  'f90-end-of-subprogram)
  (define-key f90-mode-map "\C-\M-h"  'f90-mark-subprogram)
  (define-key f90-mode-map "\C-\M-q"  'f90-indent-subprogram)
  (define-key f90-mode-map "\C-j"     'f90-indent-new-line) ; LFD equals C-j
  (define-key f90-mode-map "\r"       'newline)
  (define-key f90-mode-map "\C-c\r"   'f90-break-line)
  ;;  (define-key f90-mode-map [M-return] 'f90-break-line)
  (define-key f90-mode-map "\C-c\C-d" 'f90-join-lines)
  (define-key f90-mode-map "\C-c\C-f" 'f90-fill-region)
  (define-key f90-mode-map "\C-c\C-p" 'f90-previous-statement)
  (define-key f90-mode-map "\C-c\C-n" 'f90-next-statement)
  (define-key f90-mode-map "\C-c\C-w" 'f90-insert-end)
  (define-key f90-mode-map "\t"       'f90-indent-line))
;; menus
(if (string-match "Lucid" emacs-version)
    ;; XEmacs
    (progn
      (add-menu nil "F90"
		'(
		  ["Indent subprogram"       f90-indent-subprogram t]
		  ["Mark subprogram"         f90-mark-subprogram t]
		  ["Beginning of subprogram" f90-beginning-of-subprogram t]
		  ["End of subprogram"       f90-end-of-subprogram t]
		  "-----"
		  ["(Un)Comment region"      f90-comment-region t]
		  ["Indent region"           indent-region t]
		  ["Fill region"             f90-fill-region t]
		  "-----"
		  ["Break line at point"     f90-break-line t]
		  ["Join with next line"     f90-join-lines t]
		  ["Insert newline."         newline t]
		  ["Insert end."             f90-insert-end t]
		  "-----"
		  ["UPCASE keywords (buffer)"      f90-upcase-keywords t]
		  ["UPCASE keywords (region)"      f90-upcase-region-keywords
		   t]
		  ["Capitalize keywords (buffer)"  f90-capitalize-keywords t]
		  ["Capitalize keywords (region)" 
		   f90-capitalize-region-keywords t]
		  ["downcase keywords (buffer)"    f90-downcase-keywords t]
		  ["downcase keywords (region)"   
		   f90-downcase-region-keywords t]
		  "-----"
		  ["Toggle abbrev-mode"   abbrev-mode             t]
		  ["Toggle auto-fill"     f90-auto-fill-mode      t])))
  ;; FSF Emacs
  (define-key f90-mode-map [menu-bar] (make-sparse-keymap))
  (define-key f90-mode-map [menu-bar f90] 
    (cons "F90" (make-sparse-keymap "f90"))) 
  (define-key f90-mode-map [menu-bar f90 abbrev-mode]
    '("Toggle abbrev-mode" . abbrev-mode))
  (define-key f90-mode-map [menu-bar f90 f90-auto-fill-mode]
    '("Toggle auto-fill" . f90-auto-fill-mode))
  (define-key f90-mode-map [menu-bar f90 f90-downcase-region-keywords]
    '("downcase keywords (region)" . f90-downcase-region-keywords))
  (define-key f90-mode-map [menu-bar f90 f90-downcase-keywords]
    '("downcase keywords (buffer)" . f90-downcase-keywords))
  (define-key f90-mode-map [menu-bar f90 f90-capitalize-keywords]
    '("Capitalize keywords (region)" . f90-capitalize-region-keywords))
  (define-key f90-mode-map [menu-bar f90 f90-capitalize-region-keywords]
    '("Capitalize keywords (buffer)" . f90-capitalize-keywords))
  (define-key f90-mode-map [menu-bar f90 f90-upcase-region-keywords]
    '("UPCASE keywords (region)" . f90-upcase-region-keywords))
  (define-key f90-mode-map [menu-bar f90 f90-upcase-keywords]
    '("UPCASE keywords (buffer)" . f90-upcase-keywords))
  (define-key f90-mode-map [menu-bar f90 f90-insert-end]
    '("Insert end." . f90-insert-end))
  (define-key f90-mode-map [menu-bar f90 f90-join-lines]
    '("Join with next line." . f90-join-lines))
  (define-key f90-mode-map [menu-bar f90 f90-break-line]
    '("Break line at point" . f90-break-line))
  (define-key f90-mode-map [menu-bar f90 f90-fill-region]
    '("Fill Region" . f90-fill-region))
  (define-key f90-mode-map [menu-bar f90 indent-region]
    '("Indent Region" . indent-region))
  (define-key f90-mode-map [menu-bar f90 f90-comment-region]
    '("(Un)Comment Region" . f90-comment-region))
  (define-key f90-mode-map [menu-bar f90 f90-end-of-subprogram]
    '("End of Subprogram" . f90-end-of-subprogram))
  (define-key f90-mode-map [menu-bar f90 f90-beginning-of-subprogram]
    '("Beginning of Subprogram" . f90-beginning-of-subprogram))
  (define-key f90-mode-map [menu-bar f90 f90-mark-subprogram]
    '("Mark Subprogram" . f90-mark-subprogram))
  (define-key f90-mode-map [menu-bar f90 f90-indent-subprogram]
    '("Indent Subprogram" . f90-indent-subprogram)))
  
(defconst f90-symbol-re "[a-z_][a-z_0-9]*")
(defconst f90-blocks-re 
  "\\(block[ \t]*data\\|do\\|if\\|interface\\|function\\|module\\|\
program\\|select\\|subroutine\\|type\\|where\\|forall\\)\\>")
(defconst f90-program-block-re 
  "\\(program\\|module\\|subroutine\\|function\\)")
(defconst f90-else-like-re 
  "\\(else\\|else[ \t]*if\\|elsewhere\\|case[ \t]*(\\|case[ \t]*default\\)")
(defconst f90-end-if-re 
  "end[ \t]*\\(if\\|select\\|where\\|forall\\)\\>")
(defconst f90-end-type-re 
  "end[ \t]*\\(type\\|interface\\|block[ \t]*data\\)\\>")
(defconst f90-no-break-re  "\\(\\*\\*\\|//\\|=>\\)")
;; A temporary position to make region operators faster
(defvar f90-cache-position nil)
(make-variable-buffer-local 'f90-cache-position)

;; abbrevs have generally two letters, except standard types `c, `i, `r, `t
(defvar f90-mode-abbrev-table nil)
(if f90-mode-abbrev-table
    ()
  (let ((ac abbrevs-changed))
    (define-abbrev-table 'f90-mode-abbrev-table ())
    (define-abbrev f90-mode-abbrev-table  "`al"  "allocate" nil)
    (define-abbrev f90-mode-abbrev-table  "`ab"  "allocateable" nil)
    (define-abbrev f90-mode-abbrev-table  "`as"  "assignment" nil)
    (define-abbrev f90-mode-abbrev-table  "`ba"  "backspace" nil)
    (define-abbrev f90-mode-abbrev-table  "`bd"  "block data" nil)
    (define-abbrev f90-mode-abbrev-table  "`c"   "character" nil)
    (define-abbrev f90-mode-abbrev-table  "`cl"  "close" nil)
    (define-abbrev f90-mode-abbrev-table  "`cm"  "common" nil)
    (define-abbrev f90-mode-abbrev-table  "`cx"  "complex" nil)
    (define-abbrev f90-mode-abbrev-table  "`cn"  "contains" nil)
    (define-abbrev f90-mode-abbrev-table  "`cy"  "cycle" nil)
    (define-abbrev f90-mode-abbrev-table  "`de"  "deallocate" nil)
    (define-abbrev f90-mode-abbrev-table  "`df"  "define" nil)
    (define-abbrev f90-mode-abbrev-table  "`di"  "dimension" nil)
    (define-abbrev f90-mode-abbrev-table  "`dw"  "do while" nil)
    (define-abbrev f90-mode-abbrev-table  "`el"  "else" nil)
    (define-abbrev f90-mode-abbrev-table  "`eli" "else if" nil)
    (define-abbrev f90-mode-abbrev-table  "`elw" "elsewhere" nil)
    (define-abbrev f90-mode-abbrev-table  "`eq"  "equivalence" nil)
    (define-abbrev f90-mode-abbrev-table  "`ex"  "external" nil)
    (define-abbrev f90-mode-abbrev-table  "`ey"  "entry" nil)
    (define-abbrev f90-mode-abbrev-table  "`fl"  "forall" nil)
    (define-abbrev f90-mode-abbrev-table  "`fo"  "format" nil)
    (define-abbrev f90-mode-abbrev-table  "`fu"  "function" nil)
    (define-abbrev f90-mode-abbrev-table  "`fa"  ".false." nil)
    (define-abbrev f90-mode-abbrev-table  "`im"  "implicit none" nil)
    (define-abbrev f90-mode-abbrev-table  "`in " "include" nil)
    (define-abbrev f90-mode-abbrev-table  "`i"   "integer" nil)
    (define-abbrev f90-mode-abbrev-table  "`it"  "intent" nil)
    (define-abbrev f90-mode-abbrev-table  "`if"  "interface" nil)
    (define-abbrev f90-mode-abbrev-table  "`lo"  "logical" nil)
    (define-abbrev f90-mode-abbrev-table  "`mo"  "module" nil)
    (define-abbrev f90-mode-abbrev-table  "`na"  "namelist" nil)
    (define-abbrev f90-mode-abbrev-table  "`nu"  "nullify" nil)
    (define-abbrev f90-mode-abbrev-table  "`op"  "optional" nil)
    (define-abbrev f90-mode-abbrev-table  "`pa"  "parameter" nil)
    (define-abbrev f90-mode-abbrev-table  "`po"  "pointer" nil)
    (define-abbrev f90-mode-abbrev-table  "`pr"  "print" nil)
    (define-abbrev f90-mode-abbrev-table  "`pi"  "private" nil)
    (define-abbrev f90-mode-abbrev-table  "`pm"  "program" nil)
    (define-abbrev f90-mode-abbrev-table  "`pu"  "public" nil)
    (define-abbrev f90-mode-abbrev-table  "`r"   "real" nil)
    (define-abbrev f90-mode-abbrev-table  "`rc"  "recursive" nil)
    (define-abbrev f90-mode-abbrev-table  "`rt"  "return" nil)
    (define-abbrev f90-mode-abbrev-table  "`rw"  "rewind" nil)
    (define-abbrev f90-mode-abbrev-table  "`se"  "select" nil)
    (define-abbrev f90-mode-abbrev-table  "`sq"  "sequence" nil)
    (define-abbrev f90-mode-abbrev-table  "`su"  "subroutine" nil)
    (define-abbrev f90-mode-abbrev-table  "`ta"  "target" nil)
    (define-abbrev f90-mode-abbrev-table  "`tr"  ".true." nil)
    (define-abbrev f90-mode-abbrev-table  "`t"   "type" nil)
    (define-abbrev f90-mode-abbrev-table  "`wh"  "where" nil)
    (define-abbrev f90-mode-abbrev-table  "`wr"  "write" nil)
    (setq abbrevs-changed ac)))

;;;###autoload
(defun f90-mode ()
  "Major mode for editing Fortran 90 code in free format.

\\[f90-indent-new-line] corrects current indentation and creates new\
 indented line.
\\[f90-indent-line] indents the current line correctly. 
\\[f90-indent-subprogram] indents the current subprogram. 

Type `? or `\\[help-command] to display a list of built-in\
 abbrevs for F90 keywords.

Key definitions:
\\{f90-mode-map}

Variables controlling indentation style and extra features:

 f90-do-indent
    Extra indentation within do blocks.  (default 3)
 f90-if-indent
    Extra indentation within if/select case/where/forall blocks. (default 3)
 f90-type-indent
    Extra indentation within type/interface/block-data blocks.  (default 3)
 f90-program-indent
    Extra indentation within program/module/subroutine/function blocks.
      (default 2)
 f90-continuation-indent
    Extra indentation applied to continuation lines.  (default 5)
 f90-comment-region
    String inserted by \\[f90-comment-region] at start of each line in 
    region.  (default \"!!!$\")
 f90-indented-comment
    String holding the type of comment to be intended like code.
    This is a regular expression. (default \"!\")
 f90-directive-comment
    String of comment-like directive like \"!HPF$\", not to be indented.
    (default nil)
 f90-break-delimiters
    Regexp holding list of delimiters at which lines may be broken.
    (default \"[-+*/><=,% \\t]\")
 f90-break-before-delimiters
    Non-nil causes `f90-do-auto-fill' to break lines before delimiters.
    (default t)
 f90-beginning-ampersand 
    Automatic insertion of \& at beginning of continuation lines. (default t)
 f90-smart-end 
    From an END statement, check and fill the end using matching block start.
    Allowed values are 'blink, 'no-blink, and nil, which determine
    whether to blink the matching beginning.) (default 'blink)
 f90-auto-keyword-case
    Automatic change of case of keywords. (default nil)
    The possibilities are 'downcase-word, 'upcase-word, 'capitalize-word.
 f90-auto-hilit19  (default nil)
    Automatic highlighting (if hilit19 is used) at every indent or newline.
 f90-leave-line-no
    Do not left-justify line numbers. (default nil)
 f90-startup-message
    Set to nil to inhibit message first time F90 mode is used. (default t)
 f90-keywords
    List of keywords used for highlighting/upcase-keywords etc.

Turning on F90 mode calls the value of the variable `f90-mode-hook'
with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'f90-mode)
  (setq mode-name "F90")
  (setq local-abbrev-table f90-mode-abbrev-table)
  (set-syntax-table f90-mode-syntax-table)
  (use-local-map f90-mode-map)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'f90-indent-line)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'f90-indent-region)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "!")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "!+ *")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'f90-comment-indent)
  (make-local-variable 'abbrev-all-caps)
  (setq abbrev-all-caps t)
  ;; Setting up things for font-lock
  (if (string-match "Lucid" emacs-version)
      (put 'f90-mode 'font-lock-keywords-case-fold-search t)
    ;; (make-local-variable 'font-lock-keywords) ; for FSF version <= 19.28 
    ;; (setq font-lock-keywords f90-font-lock-keywords)
    (make-local-variable 'font-lock-defaults) ; for FSF version > 19.28
    (setq font-lock-defaults '(f90-font-lock-keywords t))
    )
  (make-local-variable 'font-lock-keywords-case-fold-search)
  (setq font-lock-keywords-case-fold-search t)
  (run-hooks 'f90-mode-hook)
  (if f90-startup-message
      (message "Emacs F90 mode %s. Bugs to %s" f90-mode-version bug-f90-mode))
  (setq f90-startup-message nil))

;; inline-functions
(defsubst f90-get-beg-of-line ()
  (save-excursion (beginning-of-line) (point)))

(defsubst f90-get-end-of-line ()
  (save-excursion (end-of-line) (point)))

(defsubst f90-in-string ()
  (let ((beg-pnt
	 (if (and f90-cache-position (> (point) f90-cache-position))
	     f90-cache-position
	   (point-min))))
    (nth 3 (parse-partial-sexp beg-pnt (point)))))
	    
(defsubst f90-in-comment ()
  (let ((beg-pnt
	 (if (and f90-cache-position (> (point) f90-cache-position))
	     f90-cache-position
	   (point-min))))
    (nth 4 (parse-partial-sexp beg-pnt (point)))))

(defsubst f90-line-continued ()
  (save-excursion
    (let ((bol (f90-get-beg-of-line)))
      (end-of-line)
      (while (f90-in-comment)
	(search-backward "!" bol)
	(skip-chars-backward "!"))
      (skip-chars-backward " \t")
      (= (preceding-char) ?&))))

(defsubst f90-current-indentation ()
  "Return indentation of current line.
Line-numbers are considered whitespace characters."
  (save-excursion
    (beginning-of-line) (skip-chars-forward " \t0-9")
    (current-column)))

(defsubst f90-indent-to (col &optional no-line-number)
  "Indent current line to column COL.
If no-line-number nil, jump over a possible line-number."
  (beginning-of-line)
  (if (not no-line-number)
      (skip-chars-forward " \t0-9"))
  (delete-horizontal-space)
  (if (zerop (current-column))
      (indent-to col)
    (indent-to col 1)))

(defsubst f90-match-piece (arg)
  (if (match-beginning arg)
      (buffer-substring (match-beginning arg) (match-end arg))))

(defsubst f90-get-present-comment-type ()
  (save-excursion
    (let ((type nil) (eol (f90-get-end-of-line)))
      (if (f90-in-comment)
	  (progn
	    (beginning-of-line)
	    (re-search-forward "[!]+" eol)
	    (while (f90-in-string)
	      (re-search-forward "[!]+" eol))
	    (setq type (buffer-substring (match-beginning 0) (match-end 0)))))
      type)))

(defsubst f90-equal-symbols (a b)
  "Compare strings neglecting case and allowing for nil value."
  (let ((a-local (if a (downcase a) nil))
	(b-local (if b (downcase b) nil)))
    (equal a-local b-local)))

;; There seems to be a bug in XEmacs matching of regular expressions.
;; One cannot extract the label directly for do,if, and select case.
;; Therefore, the following functions are longer than necessary.

(defsubst f90-looking-at-do ()
  "Return (\"do\" name) if a do statement starts after point.
Name is nil if the statement has no label."
  (let (struct (label nil))
    (if (looking-at (concat "\\(\\(" f90-symbol-re "\\)[ \t]*\:\\)?"
			    "[ \t]*\\(do\\)\\b"))
	(progn
	  (setq struct (f90-match-piece 3))
	  (if (looking-at (concat "\\(" f90-symbol-re "\\)[ \t]*\:"))
	      (setq label (f90-match-piece 1)))
	  (list struct label)))))

(defsubst f90-looking-at-if-then ()
  "Return (\"if\" name) if an if () then statement starts after point.
Name is nil if the statement has no label."
  (save-excursion
    (let (struct (label nil))
      (if (looking-at (concat "\\(\\(" f90-symbol-re "\\)[ \t]*\:\\)?"
			      "[ \t]*\\(if\\)\\b"))
	  (progn
	    (setq struct (f90-match-piece 3))
	    (if (looking-at (concat "\\(" f90-symbol-re "\\)[ \t]*\:"))
		(setq label (f90-match-piece 1)))
	    (goto-char (scan-lists (point) 1 0))
	    (skip-chars-forward " \t")
	    (if (or (looking-at "then\\b")
		    (if (f90-line-continued)
			(progn
			  (f90-next-statement)
			  (skip-chars-forward " \t0-9&")
			  (looking-at "then\\b"))))
		(list struct label)))))))

(defsubst f90-looking-at-select-case ()
  "Return (\"select\" name) if a select-case statement starts after point.
Name is nil if the statement has no label."
  (let (struct (label nil))
    (if (looking-at (concat "\\(\\(" f90-symbol-re "\\)[ \t]*\:\\)?"
			    "[ \t]*\\(select\\)[ \t]*case[ \t]*("))
	(progn
	  (setq struct (f90-match-piece 3))
	  (if (looking-at (concat "\\(" f90-symbol-re "\\)[ \t]*\:"))
	      (setq label (f90-match-piece 1)))
	  (list struct label)))))

(defsubst f90-looking-at-where-or-forall ()
  "Return (kind nil) if where/forall...end starts after point."
  (save-excursion
    (let (command)
      (if (looking-at "\\(where\\|forall\\)[ \t]*(")
	  (progn
	    (setq command (list (f90-match-piece 1) nil))
	    (goto-char (scan-lists (point) 1 0))
	    (skip-chars-forward " \t")
	    (if (looking-at "\\(!\\|$\\)")
		command))))))

(defsubst f90-looking-at-type-like ()
  "Return (kind name) at the start of a type/interface/block-data block.
Name is non-nil only for type."
  (cond 
   ((looking-at (concat "\\(type\\)[ \t]+\\(" f90-symbol-re "\\)\\>"))
    (list (f90-match-piece 1) (f90-match-piece 2)))
   ((looking-at "\\(interface\\)\\>")
    (list (f90-match-piece 1) nil))
   ((looking-at "\\(block[ \t]*data\\)\\>")
    (list (f90-match-piece 1) nil))))

(defsubst f90-looking-at-program-block-start ()
  "Return (kind name) if a program block with name name starts after point."
  (cond
   ((looking-at (concat "\\(program\\)[ \t]+\\(" f90-symbol-re "\\)\\b"))
    (list (f90-match-piece 1) (f90-match-piece 2)))
   ((and (not (looking-at "module[ \t]*procedure\\>"))
	 (looking-at (concat "\\(module\\)[ \t]+\\(" 
			     f90-symbol-re "\\)\\b")))
    (list (f90-match-piece 1) (f90-match-piece 2)))
   ((looking-at (concat "\\(recursive[ \t]*\\)?\\(subroutine\\)[ \t]+\\("
			f90-symbol-re "\\)"))
    (list (f90-match-piece 2) (f90-match-piece 3)))
   ((looking-at (concat "[a-z0-9()_ \t]*\\(function\\)[ \t]+\\("
			f90-symbol-re "\\)[ \t]*("))
    (list (f90-match-piece 1) (f90-match-piece 2)))))

(defsubst f90-looking-at-program-block-end ()
  "Return list of type and name of end of block."
  (if (looking-at (concat "end[ \t]*" f90-blocks-re "?\\([ \t]+\\("
			  f90-symbol-re  "\\)\\)?\\>"))
      (list (f90-match-piece 1) (f90-match-piece 3))))

(defsubst f90-comment-indent ()
  (cond ((looking-at "!!!") 0)
	((and f90-directive-comment
	      (looking-at (regexp-quote f90-directive-comment))) 0)
	((looking-at (regexp-quote f90-comment-region)) 0)
	((looking-at f90-indented-comment)
	 (f90-calculate-indent))
	(t (skip-chars-backward " \t")
	   (max (if (bolp) 0 (1+ (current-column))) comment-column))))

(defsubst f90-present-statement-cont ()
  "Return continuation properties of present statement."
  (let (pcont cont)
    (save-excursion
      (setq pcont (if (f90-previous-statement) (f90-line-continued) nil)))
    (setq cont (f90-line-continued))
    (cond ((and (not pcont) (not cont)) 'single)
 	  ((and (not pcont) cont)       'begin)
 	  ((and pcont       (not cont)) 'end)
 	  ((and pcont       cont)       'middle)
 	  (t (error)))))

(defsubst f90-indent-line-no ()
  (if f90-leave-line-no
      ()
    (if (and (not (zerop (skip-chars-forward " \t")))
	     (looking-at "[0-9]"))
	(delete-horizontal-space)))
  (skip-chars-forward " \t0-9"))

(defsubst f90-no-block-limit ()
  (let ((eol (f90-get-end-of-line)))
    (save-excursion
      (not (or (looking-at "end")
	       (looking-at "\\(do\\|if\\|else\\|select[ \t]*case\\|\
case\\|where\\|forall\\)\\>")
	       (looking-at "\\(program\\|module\\|interface\\|\
block[ \t]*data\\)\\>")
	       (looking-at "\\(contains\\|continue\\|\\sw+[ \t]*:\\)")
	       (looking-at "type[ \t]+\\sw+")
	       (re-search-forward "\\(function\\|subroutine\\)" eol t))))))

(defsubst f90-update-line ()
  (let (bol eol)
    (if (or f90-auto-keyword-case f90-auto-hilit19)
	(progn (setq bol (f90-get-beg-of-line)
		     eol (f90-get-end-of-line))
	       (if f90-auto-keyword-case
		   (f90-change-keywords f90-auto-keyword-case bol eol))
	       (if (and f90-auto-hilit19 (fboundp 'hilit-rehighlight-region))
		   (hilit-rehighlight-region bol eol t))))))

(defun f90-get-correct-indent ()
  "Get correct indent for a line starting with line number.
Does not check type and subprogram indentation."
  (let ((epnt (f90-get-end-of-line)) icol cont)
    (save-excursion
      (while (and (f90-previous-statement)
		  (or (progn
			(setq cont (f90-present-statement-cont))
			(or (eq cont 'end) (eq cont 'middle)))
		      (looking-at "[ \t]*[0-9]"))))
      (setq icol (current-indentation))
      (beginning-of-line)
      (if (re-search-forward "\\(if\\|do\\|select\\|where\\|forall\\)"
			     (f90-get-end-of-line) t)
	  (progn
	    (beginning-of-line) (skip-chars-forward " \t")
	    (cond ((f90-looking-at-do)
		   (setq icol (+ icol f90-do-indent)))
		  ((or (f90-looking-at-if-then)
		       (f90-looking-at-where-or-forall)
		       (f90-looking-at-select-case))
		   (setq icol (+ icol f90-if-indent))))
	    (end-of-line)))
      (while (re-search-forward
	      "\\(if\\|do\\|select\\|where\\|forall\\|continue\\)" epnt t)
	(beginning-of-line) (skip-chars-forward " \t0-9")
	(cond  ((f90-looking-at-do)
		(setq icol (+ icol f90-do-indent)))
	       ((or (f90-looking-at-if-then)
		    (f90-looking-at-where-or-forall)
		    (f90-looking-at-select-case))
		(setq icol (+ icol f90-if-indent)))
	       ((looking-at f90-end-if-re)
		(setq icol (- icol f90-if-indent)))
	       ((looking-at "\\(end[ \t]*do\\|continue\\)\\>")
		(setq icol (- icol f90-do-indent))))
	(end-of-line))
      icol)))
	       
	  
(defun f90-calculate-indent ()
  "Calculate the indent column based on previous statements."
  (interactive)
  (let (icol cont (case-fold-search t) (pnt (point)))
    (save-excursion
      (if (not (f90-previous-statement))
	  (setq icol 0)
	(setq cont (f90-present-statement-cont))
	(if (eq cont 'end)
	    (while (not (eq 'begin (f90-present-statement-cont)))
	      (f90-previous-statement)))
	(cond ((eq cont 'begin)
	       (setq icol (+ (f90-current-indentation)
			     f90-continuation-indent)))
	      ((eq cont 'middle) (setq icol(current-indentation)))
	      (t (setq icol (f90-current-indentation))
		 (skip-chars-forward " \t")
		 (if (looking-at "[0-9]")
		     (setq icol (f90-get-correct-indent))
		   (cond ((or (f90-looking-at-if-then)
			      (f90-looking-at-where-or-forall)
			      (f90-looking-at-select-case)
			      (looking-at f90-else-like-re))		       
			  (setq icol (+ icol f90-if-indent)))
			 ((f90-looking-at-do)
			  (setq icol (+ icol f90-do-indent)))
			 ((f90-looking-at-type-like)
			  (setq icol (+ icol f90-type-indent)))
			 ((or (f90-looking-at-program-block-start)
			      (looking-at "contains[ \t]*\\($\\|!\\)"))
			  (setq icol (+ icol f90-program-indent)))))
		 (goto-char pnt)
		 (beginning-of-line)
		 (cond ((looking-at "[ \t]*$"))
		       ((looking-at "[ \t]*#") ; Check for cpp directive.
			(setq icol 0))
		       (t
			(skip-chars-forward " \t0-9")
			(cond ((or (looking-at f90-else-like-re)
				   (looking-at f90-end-if-re))
			       (setq icol (- icol f90-if-indent)))
			      ((looking-at "\\(end[ \t]*do\\|continue\\)\\>")
			       (setq icol (- icol f90-do-indent)))
			      ((looking-at f90-end-type-re)
			       (setq icol (- icol f90-type-indent)))
			      ((or (looking-at "contains[ \t]*\\(!\\|$\\)")
				   (f90-looking-at-program-block-end))
			       (setq icol (- icol f90-program-indent))))))
		 ))))
    icol))

;; Statement = statement line, a line which is neither blank, nor a comment.
(defun f90-previous-statement ()
  "Move point to beginning of the previous F90 statement.
Return nil if no previous statement is found."
  (interactive)
  (let (not-first-statement)
    (beginning-of-line)
    (while (and (setq not-first-statement (zerop (forward-line -1)))
		(looking-at "[ \t0-9]*\\(!\\|$\\)")))
    not-first-statement))

(defun f90-next-statement ()
  "Move point to beginning of the next F90 statement.
Return nil if no later statement is found."
  (interactive)
  (let (not-last-statement)
    (beginning-of-line)
    (while (and (setq not-last-statement
		      (and (zerop (forward-line 1))
			   (not (eobp))))
 		(looking-at "[ \t0-9]*\\(!\\|$\\)")))
    not-last-statement))

(defun f90-beginning-of-subprogram ()
  "Move point to the beginning of subprogram.
Return (type name) or nil if not found."
  (interactive)
  (let ((count 1) (case-fold-search t) matching-beg)
    (beginning-of-line) (skip-chars-forward " \t0-9")
    (if (setq matching-beg (f90-looking-at-program-block-start)) 
	(setq count (- count 1)))
    (while (and (not (zerop count))
		(re-search-backward f90-program-block-re nil 'move))
      (beginning-of-line) (skip-chars-forward " \t0-9")
      (cond 
       ((setq matching-beg (f90-looking-at-program-block-start))
	(setq count (- count 1)))
       ((f90-looking-at-program-block-end)
	(setq count (+ count 1)))))
    (beginning-of-line)
    (if (zerop count)
	matching-beg
      (message "No beginning-found.")
      nil)))

(defun f90-end-of-subprogram ()
  "Move point to the end of subprogram.
Return (type name) or nil if not found."
  (interactive)
  (let ((count 1) (case-fold-search t) matching-end)
    (beginning-of-line) (skip-chars-forward " \t0-9")
    (if (setq matching-end (f90-looking-at-program-block-end))
	(setq count (1- count)))
    (end-of-line)
    (while (and (not (zerop count))
		(re-search-forward f90-program-block-re nil 'move))
      (beginning-of-line) (skip-chars-forward " \t0-9")
      (cond ((f90-looking-at-program-block-start)
	     (setq count (+ count 1)))
	    ((setq matching-end (f90-looking-at-program-block-end))
	     (setq count (1- count ))))
      (end-of-line))
    (forward-line 1)
    (if (zerop count)
	matching-end
      (message "No end found.")
      nil)))

(defun f90-mark-subprogram ()
  "Put mark at end of F90 subprogram, point at beginning.
Marks are pushed and highlight (grey shadow) is turned on."
  (interactive)
  (let ((pos (point)) program)
    (f90-end-of-subprogram)
    (push-mark (point) t)
    (goto-char pos)
    (setq program (f90-beginning-of-subprogram))
    ;; The keywords in the preceding lists assume case-insensitivity.
    (if (string-match "Lucid" emacs-version)
	(zmacs-activate-region)
      (setq mark-active t)
      (setq deactivate-mark nil))
    program))

(defun f90-comment-region (beg-region end-region)
  "Comment/uncomment every line in the region.
Insert f90-comment-region at the beginning of every line in the region
or, if already present, remove it."
  (interactive "*r")
  (let ((end (make-marker)))
    (set-marker end end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (if (looking-at (regexp-quote f90-comment-region))
	(delete-region (point) (match-end 0))
      (insert f90-comment-region))
    (while (and  (zerop (forward-line 1))
		 (< (point) (marker-position end)))
      (if (looking-at (regexp-quote f90-comment-region))
	  (delete-region (point) (match-end 0))
	(insert f90-comment-region)))
    (set-marker end nil)))

(defun f90-indent-line (&optional no-update)
  "Indent current line as F90 code."
  (interactive)
  (let (indent (no-line-number nil) (pos (make-marker)) (case-fold-search t))
    (set-marker pos (point))
    (beginning-of-line)			; Digits after & \n are not line-no
    (if (save-excursion (and (f90-previous-statement) (f90-line-continued)))
	(progn (setq no-line-number t) (skip-chars-forward " \t"))
      (f90-indent-line-no))
    (if (looking-at "!")
	(setq indent (f90-comment-indent))
      (if (and (looking-at "end") f90-smart-end) (f90-match-end))
      (setq indent (f90-calculate-indent)))
    (if (zerop (- indent (current-column)))
	nil
      (f90-indent-to indent no-line-number))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (< (point) (marker-position pos))
	(goto-char (marker-position pos)))
    (if (not no-update) (f90-update-line))
    (if (and auto-fill-function
	     (> (save-excursion (end-of-line) (current-column)) fill-column))
	(save-excursion (f90-do-auto-fill)))
    (set-marker pos nil)))

(defun f90-indent-new-line ()
  "Reindent the current F90 line, insert a newline and indent the newline.
An abbrev before point is expanded if `abbrev-mode' is non-nil.
If run in the middle of a line, the line is not broken."
  (interactive)
  (let (string cont (case-fold-search t))
    (if abbrev-mode (expand-abbrev))
    (beginning-of-line)			; Reindent where likely to be needed.
    (f90-indent-line-no)
    (if (or (looking-at "\\(end\\|else\\|!\\)"))
	(f90-indent-line 'no-update))
    (end-of-line)
    (delete-horizontal-space)		;Destroy trailing whitespace
    (setq string (f90-in-string))
    (setq cont (f90-line-continued))
    (if (and string (not cont)) (insert "&"))
    (f90-update-line)
    (newline)
    (if (or string (and cont f90-beginning-ampersand)) (insert "&"))
    (f90-indent-line 'no-update)))


(defun f90-indent-region (beg-region end-region)
  "Indent every line in region by forward parsing."
  (interactive "*r")
  (let ((end-region-mark (make-marker)) (save-point (point-marker))
	(block-list nil) ind-lev ind-curr ind-b cont
	struct beg-struct end-struct)
    (set-marker end-region-mark end-region)
    (goto-char beg-region)
    ;; first find a line which is not a continuation line or comment
    (beginning-of-line)
    (while (and (looking-at "[ \t]*[0-9]*\\(!\\|[ \t]*$\\)")
		(progn (f90-indent-line 'no-update)
		       (zerop (forward-line 1)))
		(< (point) end-region-mark)))
    (setq cont (f90-present-statement-cont))
    (while (and (or (eq cont 'middle) (eq cont 'end))
		(f90-previous-statement))
      (setq cont (f90-present-statement-cont)))
    ;; process present line for beginning of block
    (setq f90-cache-position (point))
    (f90-indent-line 'no-update)
    (setq ind-lev (f90-current-indentation))
    (setq ind-curr ind-lev)
    (beginning-of-line) (skip-chars-forward " \t0-9")
    (setq struct nil)
    (setq ind-b (cond ((setq struct (f90-looking-at-do)) f90-do-indent)
		      ((or (setq struct (f90-looking-at-if-then))
			   (setq struct (f90-looking-at-select-case))
			   (setq struct (f90-looking-at-where-or-forall))
			   (looking-at f90-else-like-re))
		       f90-if-indent)
		      ((setq struct (f90-looking-at-type-like))
		       f90-type-indent)
		      ((or(setq struct (f90-looking-at-program-block-start))
			  (looking-at "contains[ \t]*\\($\\|!\\)"))
		       f90-program-indent)))
    (if ind-b (setq ind-lev (+ ind-lev ind-b)))
    (if struct (setq block-list (cons struct block-list)))
    (while (and (f90-line-continued) (zerop (forward-line 1))
		(< (point) end-region-mark))
      (if (not (zerop (- (current-indentation) 
			 (+ ind-curr f90-continuation-indent))))
	  (f90-indent-to (+ ind-curr f90-continuation-indent) 'no-line-no)))
    ;; process all following lines
    (while (and  (zerop (forward-line 1)) (< (point) end-region-mark))
      (beginning-of-line)
      (f90-indent-line-no)
      (setq f90-cache-position (point))
      (cond ((looking-at "[ \t]*$") (setq ind-curr 0))
	    ((looking-at "[ \t]*#") (setq ind-curr 0))
	    ((looking-at "!") (setq ind-curr (f90-comment-indent)))
	    ((f90-no-block-limit) (setq ind-curr ind-lev))
	    ((looking-at f90-else-like-re) (setq ind-curr
						 (- ind-lev f90-if-indent)))
	    ((looking-at "contains[ \t]*\\($\\|!\\)")
	     (setq ind-curr (- ind-lev f90-program-indent)))
	    ((setq ind-b
		   (cond ((setq struct (f90-looking-at-do)) f90-do-indent)
			 ((or (setq struct (f90-looking-at-if-then))
			      (setq struct (f90-looking-at-select-case))
			      (setq struct (f90-looking-at-where-or-forall)))
			  f90-if-indent)
			 ((setq struct (f90-looking-at-type-like))
			  f90-type-indent)
			 ((setq struct (f90-looking-at-program-block-start))
			  f90-program-indent)))
	     (setq ind-curr ind-lev)
	     (if ind-b (setq ind-lev (+ ind-lev ind-b)))
	     (setq block-list (cons struct block-list)))
	    ((setq end-struct (f90-looking-at-program-block-end))
	     (setq beg-struct (car block-list)
		   block-list (cdr block-list))
	     (if f90-smart-end 
		 (save-excursion
		   (f90-block-match (car beg-struct)(car (cdr beg-struct))
				    (car end-struct)(car (cdr end-struct)))))
	     (setq ind-b
		   (cond ((looking-at f90-end-if-re) f90-if-indent)
			 ((looking-at "end[ \t]*do\\>")  f90-do-indent)
			 ((looking-at f90-end-type-re) f90-type-indent)
			 ((f90-looking-at-program-block-end)
			  f90-program-indent)))
	     (if ind-b (setq ind-lev (- ind-lev ind-b)))
	     (setq ind-curr ind-lev))
	    ((looking-at "continue\\>")
	     (setq ind-lev (- ind-lev f90-do-indent))
	     (setq ind-curr ind-lev)
	     (setq block-list (cdr block-list)))
	    (t (setq ind-curr ind-lev)))
      ;; do the indentation if necessary
      (if (not (zerop (- ind-curr (current-column))))
	  (f90-indent-to ind-curr))
      (while (and (f90-line-continued) (zerop (forward-line 1))
		  (< (point) end-region-mark))
	(if (not (zerop (- (current-indentation) 
			   (+ ind-curr f90-continuation-indent))))
	    (f90-indent-to (+ ind-curr f90-continuation-indent) 'no-line-no))))
    ;; restore point etc
    (setq f90-cache-position nil)
    (goto-char save-point)
    (set-marker end-region-mark nil)
    (set-marker save-point nil)
    (if (string-match "Lucid" emacs-version)
	(zmacs-deactivate-region)
      (deactivate-mark))))

(defun f90-indent-subprogram ()
  "Properly indent the subprogram which contains point."
  (interactive)
  (save-excursion
    (let (program)
      (setq program (f90-mark-subprogram))
      (if program
	  (progn
	    (message (concat "Indenting " (car program) " "
			     (car (cdr program))"."))
	    (f90-indent-region (point) (mark))
	    (message (concat "Indenting " (car program) " "
			     (car (cdr program)) "...done.")))
	(message "Indenting the whole file.")
	(f90-indent-region (point) (mark))
	(message (concat "Indenting the whole file...done."))))))

;; autofill and break-line
(defun f90-break-line (&optional no-update)
  "Break line at point, insert continuation marker(s) and indent."
  (interactive)
  (let (ctype)
    (cond ((f90-in-string)
	   (insert "&") (newline) (insert "&"))
	  ((f90-in-comment)
	   (delete-horizontal-space)
	   (setq ctype (f90-get-present-comment-type))
	   (newline) (insert (concat ctype " ")))
	  (t (delete-horizontal-space)
	     (insert "&")
	     (if (not no-update) (f90-update-line))
	     (newline)
	     (if f90-beginning-ampersand (insert "& ")))))
  (if (not no-update) (f90-indent-line)))
  
(defun f90-find-breakpoint ()
  "From fill-column, search backward for break-delimiter."
  (let ((bol (f90-get-beg-of-line)))
    (re-search-backward f90-break-delimiters bol)
    (if f90-break-before-delimiters
	(progn (backward-char)
	       (if (not (looking-at f90-no-break-re))
		   (forward-char)))
      (if (looking-at f90-no-break-re)
	  (forward-char 2)
	(forward-char)))))

(defun f90-auto-fill-mode (arg)
  "Toggle f90-auto-fill mode.
With ARG, turn `f90-auto-fill' mode on iff ARG is positive.
In `f90-auto-fill' mode, inserting a space at a column beyond `fill-column'
automatically breaks the line at a previous space."
  (interactive "P")
  (prog1 (setq auto-fill-function
	       (if (if (null arg)
		       (not auto-fill-function)
		     (> (prefix-numeric-value arg) 0))
		   'f90-do-auto-fill))
    ;; update mode-line
    (set-buffer-modified-p (buffer-modified-p))))

(defun f90-do-auto-fill ()
  "Break line if non-white characters beyond fill-column."
  (interactive)
  ;; Break the line before or after the last delimiter (non-word char).
  ;; Will not break **, //, or => (specified by f90-no-break-re).
  (move-to-column fill-column)
  (if (and (looking-at "[ \t]*$") (not (f90-in-string)))
      (delete-horizontal-space)
    (f90-find-breakpoint)
    (f90-break-line)
    (end-of-line)))

(defun f90-join-lines ()
  "Join present line with next line, if this line ends with \&."
  (interactive)
  (let (pos (oldpos (point)))
    (end-of-line)
    (skip-chars-backward " \t")
    (cond ((= (preceding-char) ?&)
	   (delete-char -1)
	   (setq pos (point))
	   (forward-line 1)
	   (skip-chars-forward " \t")
	   (if (looking-at "\&") (delete-char 1))
	   (delete-region pos (point))
	   (if (not (f90-in-string))
	       (progn (delete-horizontal-space) (insert " ")))
	   (if (and auto-fill-function
		    (> (save-excursion (end-of-line)
				       (current-column))
		       fill-column))
	       (f90-do-auto-fill))
	   (goto-char oldpos)
	   t))))

(defun f90-fill-region (beg-region end-region)
  "Fill every line in region by forward parsing. Join lines if possible."
  (interactive "*r")
  (let ((end-region-mark (make-marker))
	(f90-smart-end nil) (f90-auto-keyword-case nil)
	(f90-auto-hilit19 nil) indent (go-on t)
	(af-function auto-fill-function) (auto-fill-function nil))
    (set-marker end-region-mark end-region)
    (goto-char beg-region)
    (while go-on
      ;; join as much as possible
      (while (f90-join-lines));
      (setq indent (+ (f90-current-indentation) f90-continuation-indent))
      ;; chop the line if necessary
      (while (> (save-excursion (end-of-line) (current-column))
		fill-column)
	(move-to-column fill-column)
	(if (and (looking-at "[ \t]*$") (not (f90-in-string)))
	    (delete-horizontal-space)
	  (f90-find-breakpoint)
	  (f90-break-line 'no-update)
	  (f90-indent-to indent 'no-line-no)))
      (setq go-on (and  (< (point) (marker-position end-region-mark))
			(zerop (forward-line 1))))
      (setq f90-cache-position (point)))
    (setq auto-fill-function af-function)
    (setq f90-cache-position nil)
    (if (string-match "Lucid" emacs-version)
	(zmacs-deactivate-region)
      (deactivate-mark))))

(defun f90-block-match (beg-block beg-name end-block end-name)
  "Match end-struct with beg-struct and complete end-block if possible.
Leave point at the end of line."
  (search-forward "end" (f90-get-end-of-line))
  (catch 'no-match
    (if (not (f90-equal-symbols beg-block end-block))
	(if end-block
	    (progn
	      (message "END %s does not match %s." end-block beg-block)
	      (end-of-line) 
	      (throw 'no-match nil))
	  (message "Inserting %s." beg-block)
	  (insert (concat " " beg-block)))
      (search-forward end-block))
    (if (not (f90-equal-symbols beg-name end-name))
	(cond ((and beg-name (not end-name)) 
	       (message "Inserting %s." beg-name)
	       (insert (concat " " beg-name)))
	      ((and beg-name end-name) 
	       (message "Replacing %s with %s." end-name beg-name)
	       (search-forward end-name)
	       (replace-match beg-name))
	      ((and (not beg-name) end-name) 
	       (message "Deleting %s." end-name)
	       (search-forward end-name)
	       (replace-match "")))
      (if end-name (search-forward end-name)))
    (if (not (looking-at "!")) (delete-horizontal-space))))

(defun f90-match-end ()
  "From an end foo statement, find the corresponding foo including name."
  (interactive)
  (let ((count 1) (top-of-window (window-start)) (matching-beg nil)
	(end-point (point)) (case-fold-search t)
	beg-name end-name beg-block end-block end-struct)
    (if (save-excursion (beginning-of-line) (skip-chars-forward " \t0-9")
			(setq end-struct (f90-looking-at-program-block-end)))
	(progn
	  (setq end-block (car end-struct))
	  (setq end-name  (car (cdr end-struct)))
	  (save-excursion
	    (beginning-of-line)
	    (while (and (not (zerop count))
			(re-search-backward 
			 (concat "\\(" f90-blocks-re "\\|continue\\)") nil t))
	      (beginning-of-line) (skip-chars-forward " \t0-9")
	      (cond ((setq matching-beg
			   (cond
			    ((f90-looking-at-do))
			    ((f90-looking-at-if-then))
			    ((f90-looking-at-where-or-forall))
			    ((f90-looking-at-select-case))
			    ((f90-looking-at-type-like))
			    ((f90-looking-at-program-block-start))))
		     (setq count (- count 1)))
		    ((looking-at (concat "end[ \t]*" f90-blocks-re "\\b"))
		     (setq count (+ count 1)))
		    ((looking-at "continue\\>") (setq count (+ count 1)))))
	    (if (not (zerop count))
		(message "No matching beginning.")
	      (f90-update-line)
	      (if (eq f90-smart-end 'blink)
		  (if (< (point) top-of-window)
		      (message (concat 
				"Matches " (what-line) ": "
				(buffer-substring
				 (progn (beginning-of-line) (point))
				 (progn (end-of-line) (point)))))
		    (sit-for 1)))
	      (setq beg-block (car matching-beg))
	      (setq beg-name (car (cdr matching-beg)))
	      (goto-char end-point)
	      (beginning-of-line)
	      (f90-block-match beg-block beg-name end-block end-name)))))))

(defun f90-insert-end ()
  "Inserts an complete end statement matching beginning of present block."
  (interactive)
  (let ((f90-smart-end (if f90-smart-end f90-smart-end 'blink)))
    (insert "end")
    (f90-indent-new-line)))

;; abbrevs and keywords

(defun f90-abbrev-start ()
  "Typing `\\[help-command] or `? lists all the F90 abbrevs. 
Any other key combination is executed normally."
  (interactive)
  (let (c)
    (insert last-command-char)
    (if (or (eq (setq c (if (string-match "Lucid" emacs-version)
			    (event-to-character (next-command-event))
			  (read-event)))
		??) ;insert char if not equal to `?'
	    (eq c help-char))
	(f90-abbrev-help)
      (if (string-match "Lucid" emacs-version)
	  (setq unread-command-event c)
	(setq unread-command-events (list c))))))

(defun f90-abbrev-help ()
  "List the currently defined abbrevs in F90 mode."
  (interactive)
  (message "Listing abbrev table...")
  (display-buffer (f90-prepare-abbrev-list-buffer))
  (message "Listing abbrev table...done"))

(defun f90-prepare-abbrev-list-buffer ()
  (save-excursion
    (set-buffer (get-buffer-create "*Abbrevs*"))
    (erase-buffer)
    (insert-abbrev-table-description 'f90-mode-abbrev-table t)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (edit-abbrevs-mode))
  (get-buffer-create "*Abbrevs*"))

(defun f90-upcase-keywords ()
  "Upcase all F90 keywords in the buffer."
  (interactive)
  (f90-change-keywords 'upcase-word))

(defun f90-capitalize-keywords ()
  "Capitalize all F90 keywords in the buffer."
  (interactive)
  (f90-change-keywords 'capitalize-word))

(defun f90-downcase-keywords ()
  "Downcase all F90 keywords in the buffer."
  (interactive)
  (f90-change-keywords 'downcase-word))

(defun f90-upcase-region-keywords (beg end)
  "Upcase all F90 keywords in the region."
  (interactive "*r")
  (f90-change-keywords 'upcase-word beg end))

(defun f90-capitalize-region-keywords (beg end)
  "Capitalize all F90 keywords in the region."
  (interactive "*r")
  (f90-change-keywords 'capitalize-word beg end))

(defun f90-downcase-region-keywords (beg end)
  "Downcase all F90 keywords in the region."
  (interactive "*r")
  (f90-change-keywords 'downcase-word beg end))

;; Change the keywords according to argument.
(defun f90-change-keywords (change-word &optional beg end)
  (save-excursion
    (setq beg (if beg beg (point-min)))
    (setq end (if end end (point-max)))
    (let ((keyword-re 
	   (concat "\\<\\("
		   (mapconcat 'identity f90-keywords "\\|")            "\\|"
		   (mapconcat 'identity f90-intrinsic-procedures"\\|") "\\|"
		   (mapconcat 'identity f90-hpf-procedures "\\|")      "\\|"
		   (mapconcat 'identity f90-hpf-directives "\\|")      "\\|"
		   (mapconcat 'identity f90-hpf-keywords "\\|")
		   "\\)\\>")) (ref-point (point-min)) state)
      (goto-char beg)
      (while (re-search-forward keyword-re end t)
	(if (progn
	      (setq state (parse-partial-sexp ref-point (point)))
	      (and (not (nth 3 state)) (not (nth 4 state))))
	    (progn
	      (setq ref-point (point))
	      (funcall change-word -1)))))))

(provide 'f90)
;;; f90.el ends here
