;;; f90.el --- Fortran-90 mode (free format)

;; Copyright (C) 1995, 1996, 1997, 2000 Free Software Foundation, Inc.

;; Author: Torbj\"orn Einarsson <Torbjorn.Einarsson@era.ericsson.se>
;; Maintainer: Glenn Morris <gmorris@ast.cam.ac.uk>
;; Keywords: fortran, f90, languages

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

;; Major mode for editing F90 programs in FREE FORMAT.
;; The minor language revision F95 is also supported (with font-locking).

;; Knows about continuation lines, named structured statements, and other
;; features in F90 including HPF (High Performance Fortran) structures.
;; The basic feature provides accurate indentation of F90 programs.
;; In addition, there are many more features like automatic matching of all
;; end statements, an auto-fill function to break long lines, a join-lines
;; function which joins continued lines, etc.

;; To facilitate typing, a fairly complete list of abbreviations is provided.
;; All abbreviations begin with the backquote character "`"
;; (this requires modification of the syntax-table).
;; For example, `i expands to integer (if abbrev-mode is on).

;; There are two separate features for altering the appearance of code:
;;   1) Upcasing or capitalizing of all keywords.
;;   2) Colors/fonts using font-lock-mode.
;; Automatic upcase or downcase of keywords is controlled by the variable
;; f90-auto-keyword-case.

;; The indentations of lines starting with ! is determined by the first of the
;; following matches (values in the left column are the defaults):

;; start-string/regexp  indent         variable holding start-string/regexp
;;    !!!                  0
;;    !hpf\\$ (re)         0              f90-directive-comment-re
;;    !!$                  0              f90-comment-region
;;    !      (re)        as code          f90-indented-comment-re
;;    default            comment-column

;; Ex: Here is the result of 3 different settings of f90-indented-comment-re
;;     f90-indented-comment-re  !-indentation      !!-indentation
;;          !                    as code             as code
;;          !!                   comment-column      as code
;;          ![^!]                as code             comment-column
;; Trailing comments are indented to comment-column with indent-for-comment.
;; The function f90-comment-region toggles insertion of
;; the variable f90-comment-region in every line of the region.

;; One common convention for free vs. fixed format is that free-format files
;; have the ending .f90 or .f95 while fixed format files have the ending .f.
;; Emacs automatically loads Fortran files in the appropriate mode based
;; on extension. You can modify this by adjusting the variable auto-mode-alist.
;; For example:
;; (add-to-list 'auto-mode-alist '("\\.f\\'" . f90-mode))

;; Once you have entered f90-mode, you may get more info by using
;; the command describe-mode (C-h m). For online help use
;; C-h f <Name of function you want described>, or
;; C-h v <Name of variable you want described>.

;; To customize f90-mode for your taste, use, for example:
;; (you don't have to specify values for all the parameters below)
;;
;;(add-hook 'f90-mode-hook
;;      ;; These are the default values.
;;      '(lambda () (setq f90-do-indent 3
;;                        f90-if-indent 3
;;                        f90-type-indent 3
;;                        f90-program-indent 2
;;                        f90-continuation-indent 5
;;                        f90-comment-region "!!$"
;;                        f90-directive-comment-re "!hpf\\$"
;;                        f90-indented-comment-re "!"
;;                        f90-break-delimiters "[-+\\*/><=,% \t]"
;;                        f90-break-before-delimiters t
;;                        f90-beginning-ampersand t
;;                        f90-smart-end 'blink
;;                        f90-auto-keyword-case nil
;;                        f90-leave-line-no nil
;;                        indent-tabs-mode nil
;;                        f90-font-lock-keywords f90-font-lock-keywords-2
;;                  )
;;       ;; These are not default.
;;       (abbrev-mode 1)             ; turn on abbreviation mode
;;       (f90-add-imenu-menu)        ; extra menu with functions etc.
;;       (if f90-auto-keyword-case   ; change case of all keywords on startup
;;           (f90-change-keywords f90-auto-keyword-case))
;;	 ))
;;
;; in your .emacs file. You can also customize the lists
;; f90-font-lock-keywords, etc.
;;
;; The auto-fill and abbreviation minor modes are accessible from the F90 menu,
;; or by using M-x auto-fill-mode and M-x abbrev-mode, respectively.

;; Remarks
;; 1) Line numbers are by default left-justified. If f90-leave-line-no is
;;    non-nil, the line numbers are never touched.
;; 2) Multi-; statements like "do i=1,20 ; j=j+i ; end do" are not handled
;;    correctly, but I imagine them to be rare.
;; 3) Regexps for hilit19 are no longer supported.
;; 4) For FIXED FORMAT code, use fortran mode.
;; 5) This mode does not work under emacs-18.x.
;; 6) Preprocessor directives, i.e., lines starting with # are left-justified
;;    and are untouched by all case-changing commands. There is, at present, no
;;    mechanism for treating multi-line directives (continued by \ ).
;; 7) f77 do-loops do 10 i=.. ; ; 10 continue are not correctly indented.
;;    You are urged to use f90-do loops (with labels if you wish).
;; 8) The highlighting mode under XEmacs is not as complete as under Emacs.

;; List of user commands
;;   f90-previous-statement         f90-next-statement
;;   f90-beginning-of-subprogram    f90-end-of-subprogram   f90-mark-subprogram
;;   f90-comment-region
;;   f90-indent-line                f90-indent-new-line
;;   f90-indent-region    (can be called by calling indent-region)
;;   f90-indent-subprogram
;;   f90-break-line                 f90-join-lines
;;   f90-fill-region
;;   f90-insert-end
;;   f90-upcase-keywords            f90-upcase-region-keywords
;;   f90-downcase-keywords          f90-downcase-region-keywords
;;   f90-capitalize-keywords        f90-capitalize-region-keywords
;;   f90-add-imenu-menu
;;   f90-font-lock-1, f90-font-lock-2, f90-font-lock-3, f90-font-lock-4

;; Original author's thanks
;; Thanks to all the people who have tested the mode. Special thanks to Jens
;; Bloch Helmers for encouraging me to write this code, for creative
;; suggestions as well as for the lists of hpf-commands.
;; Also thanks to the authors of the fortran and pascal modes, on which some
;; of this code is built.

;; TODO
;; Support for hideshow, align.
;; OpenMP, preprocessor highlighting.

;;; Code:

;; User options

(defgroup f90 nil
  "Major mode for editing Fortran 90,95 code."
  :group 'languages)

(defgroup f90-indent nil
  "Indentation in free-format Fortran."
  :prefix "f90-"
  :group 'f90)


(defcustom f90-do-indent 3
  "*Extra indentation applied to DO blocks."
  :type 'integer
  :group 'f90-indent)

(defcustom f90-if-indent 3
  "*Extra indentation applied to IF, SELECT CASE, WHERE and FORALL blocks."
  :type 'integer
  :group 'f90-indent)

(defcustom f90-type-indent 3
  "*Extra indentation applied to TYPE, INTERFACE and BLOCK DATA blocks."
  :type 'integer
  :group 'f90-indent)

(defcustom f90-program-indent 2
  "*Extra indentation applied to PROGRAM/MODULE/SUBROUTINE/FUNCTION blocks."
  :type 'integer
  :group 'f90-indent)

(defcustom f90-continuation-indent 5
  "*Extra indentation applied to F90 continuation lines."
  :type 'integer
  :group 'f90-indent)

(defcustom f90-comment-region "!!$"
  "*String inserted by \\[f90-comment-region] at start of each line in region."
  :type 'string
  :group 'f90-indent)

(defcustom f90-indented-comment-re "!"
  "*Regexp saying which comments to indent like code."
  :type 'regexp
  :group 'f90-indent)

(defcustom f90-directive-comment-re "!hpf\\$"
  "*Regexp of comment-like directive like \"!HPF\\\\$\", not to be indented."
  :type 'regexp
  :group 'f90-indent)

(defcustom f90-beginning-ampersand t
  "*Non-nil gives automatic insertion of \& at start of continuation line."
  :type 'boolean
  :group 'f90)

(defcustom f90-smart-end 'blink
  "*From an END statement, check and fill the end using matching block start.
Allowed values are 'blink, 'no-blink, and nil, which determine
whether to blink the matching beginning."
  :type '(choice (const blink) (const no-blink) (const nil))
  :group 'f90)

(defcustom f90-break-delimiters "[-+\\*/><=,% \t]"
  "*Regexp holding list of delimiters at which lines may be broken."
  :type 'regexp
  :group 'f90)

(defcustom f90-break-before-delimiters t
  "*Non-nil causes `f90-do-auto-fill' to break lines before delimiters."
  :type 'boolean
  :group 'f90)

(defcustom f90-auto-keyword-case nil
  "*Automatic case conversion of keywords.
The options are 'downcase-word, 'upcase-word, 'capitalize-word and nil."
  :type '(choice (const downcase-word) (const upcase-word)
		 (const capitalize-word) (const nil))
  :group 'f90)

(defcustom f90-leave-line-no nil
  "*If non-nil, line numbers are not left justified."
  :type 'boolean
  :group 'f90)

(defconst f90-xemacs-flag (string-match "XEmacs\\|Lucid" emacs-version)
  "Non-nil means F90 mode thinks it is running under XEmacs.")

(defconst f90-keywords-re
  (regexp-opt '("allocatable" "allocate" "assign" "assignment" "backspace"
		"block" "call" "case" "character" "close" "common" "complex"
		"contains" "continue" "cycle" "data" "deallocate"
		"dimension" "do" "double" "else" "elseif" "elsewhere" "end"
		"enddo" "endfile" "endif" "entry" "equivalence" "exit"
		"external" "forall" "format" "function" "goto" "if"
		"implicit" "include" "inquire" "integer" "intent"
		"interface" "intrinsic" "logical" "module" "namelist" "none"
		"nullify" "only" "open" "operator" "optional" "parameter"
		"pause" "pointer" "precision" "print" "private" "procedure"
		"program" "public" "read" "real" "recursive" "result" "return"
		"rewind" "save" "select" "sequence" "stop" "subroutine"
		"target" "then" "type" "use" "where" "while" "write"
		;; F95 keywords.
		"elemental" "pure") 'words)
  "Regexp for F90 keywords.")

(defconst f90-keywords-level-3-re
  (regexp-opt
   '("allocatable" "allocate" "assign" "assignment" "backspace"
     "close" "deallocate" "dimension" "endfile" "entry" "equivalence"
     "external" "inquire" "intent" "intrinsic" "nullify" "only" "open"
     "operator" "optional" "parameter" "pause" "pointer" "print" "private"
     "public" "read" "recursive" "result" "rewind" "save" "select"
     "sequence" "target" "write"
     ;; F95 keywords.
     "elemental" "pure") 'words)
  "Keyword-regexp for font-lock level >= 3.")

(defconst f90-procedures-re
  (concat "\\<"
          (regexp-opt
           '("abs" "achar" "acos" "adjustl" "adjustr" "aimag" "aint"
             "all" "allocated" "anint" "any" "asin" "associated"
             "atan" "atan2" "bit_size" "btest" "ceiling" "char" "cmplx"
             "conjg" "cos" "cosh" "count" "cshift" "date_and_time" "dble"
             "digits" "dim" "dot_product" "dprod" "eoshift" "epsilon"
             "exp" "exponent" "floor" "fraction" "huge" "iachar" "iand"
             "ibclr" "ibits" "ibset" "ichar" "ieor" "index" "int" "ior"
             "ishft" "ishftc" "kind" "lbound" "len" "len_trim" "lge" "lgt"
             "lle" "llt" "log" "log10" "logical" "matmul" "max"
             "maxexponent" "maxloc" "maxval" "merge" "min" "minexponent"
             "minloc" "minval" "mod" "modulo" "mvbits" "nearest" "nint"
             "not" "pack" "precision" "present" "product" "radix"
             ;; Real is taken out here to avoid highlighting declarations.
             "random_number" "random_seed" "range" ;; "real"
             "repeat" "reshape" "rrspacing" "scale" "scan"
             "selected_int_kind" "selected_real_kind" "set_exponent"
             "shape" "sign" "sin" "sinh" "size" "spacing" "spread" "sqrt"
             "sum" "system_clock" "tan" "tanh" "tiny" "transfer"
             "transpose" "trim" "ubound" "unpack" "verify"
             ;; F95 intrinsic functions.
             "null" "cpu_time") t)
          ;; A left parenthesis to avoid highlighting non-procedures.
          "[ \t]*(")
  "Regexp whose first part matches F90 intrinsic procedures.")

(defconst f90-operators-re
  (concat "\\."
          (regexp-opt '("and" "eq" "eqv" "false" "ge" "gt" "le" "lt" "ne"
                        "neqv" "not" "or" "true") t)
          "\\.")
  "Regexp matching intrinsic operators.")

(defconst f90-hpf-keywords-re
  (regexp-opt
   ;; Intrinsic procedures.
   '("all_prefix" "all_scatter" "all_suffix" "any_prefix"
     "any_scatter" "any_suffix" "copy_prefix" "copy_scatter"
     "copy_suffix" "count_prefix" "count_scatter" "count_suffix"
     "grade_down" "grade_up"
     "hpf_alignment" "hpf_distribution" "hpf_template" "iall" "iall_prefix"
     "iall_scatter" "iall_suffix" "iany" "iany_prefix" "iany_scatter"
     "iany_suffix" "ilen" "iparity" "iparity_prefix"
     "iparity_scatter" "iparity_suffix" "leadz" "maxval_prefix"
     "maxval_scatter" "maxval_suffix" "minval_prefix" "minval_scatter"
     "minval_suffix" "number_of_processors" "parity"
     "parity_prefix" "parity_scatter" "parity_suffix" "popcnt" "poppar"
     "processors_shape" "product_prefix" "product_scatter"
     "product_suffix" "sum_prefix" "sum_scatter" "sum_suffix"
     ;; Directives.
     "align" "distribute" "dynamic" "independent" "inherit" "processors"
     "realign" "redistribute" "template"
     ;; Keywords.
     "block" "cyclic" "extrinsic" "new" "onto" "pure" "with") 'words)
  "Regexp for all HPF keywords, procedures and directives.")

;; Highlighting patterns.

(defvar f90-font-lock-keywords-1
  (list
   ;; Special highlighting of "module procedure".
   '("\\<\\(module[ \t]*procedure\\)\\>" (1 font-lock-keyword-face))
   ;; Highlight declaration of derived type.
;;;   '("\\<\\(type\\)[ \t]*\\(.*::[ \t]*\\|[ \t]+\\)\\(\\sw+\\)"
;;;     (1 font-lock-keyword-face) (3 font-lock-function-name-face))
   ;; Other functions and declarations.
   '("\\<\\(\\(?:end[ \t]*\\)?\\(program\\|module\\|function\\|\
subroutine\\|type\\)\\|use\\|call\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-keyword-face) (3 font-lock-function-name-face nil t))
   "\\<\\(\\(end[ \t]*\\)?\\(interface\\|block[ \t]*data\\)\\|contains\\)\\>")
  "This does fairly subdued highlighting of comments and function calls.")

(defvar f90-font-lock-keywords-2
  (append
   f90-font-lock-keywords-1
   (list
    ;; Variable declarations (avoid the real function call).
    '("^[ \t0-9]*\\(real\\|integer\\|c\\(haracter\\|omplex\\)\\|\
logical\\|type[ \t]*(\\sw+)\\)\\(.*::\\|[ \t]*(.*)\\)?\\([^!\n]*\\)"
      (1 font-lock-type-face t) (4 font-lock-variable-name-face))
    ;; do, if, select, where, and forall constructs.
    '("\\<\\(end[ \t]*\\(do\\|if\\|select\\|forall\\|where\\)\\)\\>\
\\([ \t]+\\(\\sw+\\)\\)?"
      (1 font-lock-keyword-face) (3 font-lock-constant-face nil t))
    '("^[ \t0-9]*\\(\\(\\sw+\\)[ \t]*:[ \t]*\\)?\\(\\(if\\|\
do\\([ \t]*while\\)?\\|select[ \t]*case\\|where\\|forall\\)\\)\\>"
      (2 font-lock-constant-face nil t) (3 font-lock-keyword-face))
    ;; Implicit declaration.
    '("\\<\\(implicit\\)[ \t]*\\(real\\|integer\\|c\\(haracter\\|omplex\\)\
\\|logical\\|type[ \t]*(\\sw+)\\|none\\)\\>"
      (1 font-lock-keyword-face) (2 font-lock-type-face))
    '("\\<\\(namelist\\|common\\)[ \t]*\/\\(\\sw+\\)?\/"
      (1 font-lock-keyword-face) (2 font-lock-constant-face nil t))
    "\\<else\\([ \t]*if\\|where\\)?\\>"
    "\\<\\(then\\|continue\\|format\\|include\\|stop\\|return\\)\\>"
    '("\\<\\(exit\\|cycle\\)[ \t]*\\(\\sw+\\)?\\>"
      (1 font-lock-keyword-face) (2 font-lock-constant-face nil t))
    '("\\<\\(case\\)[ \t]*\\(default\\|(\\)" . 1)
    '("\\<\\(do\\|go *to\\)\\>[ \t]*\\([0-9]+\\)"
      (1 font-lock-keyword-face) (2 font-lock-constant-face))
    ;; line numbers (lines whose first character after number is letter)
    '("^[ \t]*\\([0-9]+\\)[ \t]*[a-z]+" (1 font-lock-constant-face t))))
  "Highlights declarations, do-loops and other constructs.")

(defvar f90-font-lock-keywords-3
  (append f90-font-lock-keywords-2
          (list
           f90-keywords-level-3-re
           f90-operators-re
           (list f90-procedures-re '(1 font-lock-keyword-face keep))
           "\\<real\\>"			; avoid overwriting real defs
           ))
  "Highlights all F90 keywords and intrinsic procedures.")

(defvar f90-font-lock-keywords-4
  (append f90-font-lock-keywords-3
          (list f90-hpf-keywords-re))
  "Highlights all F90 and HPF keywords.")

(defvar f90-font-lock-keywords
  f90-font-lock-keywords-2
  "*Default expressions to highlight in F90 mode.")


(defvar f90-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\! "<"  table) ; begin comment
    (modify-syntax-entry ?\n ">"  table) ; end comment
    (modify-syntax-entry ?_  "w"  table) ; underscore in names
    (modify-syntax-entry ?\' "\"" table) ; string quote
    (modify-syntax-entry ?\" "\"" table) ; string quote
    (modify-syntax-entry ?\` "w"  table) ; for abbrevs
    (modify-syntax-entry ?\r " "  table) ; return is whitespace
    (modify-syntax-entry ?+  "."  table) ; punctuation
    (modify-syntax-entry ?-  "."  table)
    (modify-syntax-entry ?=  "."  table)
    (modify-syntax-entry ?*  "."  table)
    (modify-syntax-entry ?/  "."  table)
    (modify-syntax-entry ?\\ "\\" table) ; escape chars
    table)
  "Syntax table used in F90 mode.")

(defvar f90-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "`"        'f90-abbrev-start)
    (define-key map "\C-c;"    'f90-comment-region)
    (define-key map "\C-\M-a"  'f90-beginning-of-subprogram)
    (define-key map "\C-\M-e"  'f90-end-of-subprogram)
    (define-key map "\C-\M-h"  'f90-mark-subprogram)
    (define-key map "\C-\M-q"  'f90-indent-subprogram)
    (define-key map "\C-j"     'f90-indent-new-line) ; LFD equals C-j
    (define-key map "\r"       'newline)
    (define-key map "\C-c\r"   'f90-break-line)
;;;  (define-key map [M-return] 'f90-break-line)
    (define-key map "\C-c\C-d" 'f90-join-lines)
    (define-key map "\C-c\C-f" 'f90-fill-region)
    (define-key map "\C-c\C-p" 'f90-previous-statement)
    (define-key map "\C-c\C-n" 'f90-next-statement)
    (define-key map "\C-c\C-w" 'f90-insert-end)
    (define-key map "\t"       'f90-indent-line)
    (define-key map ","        'f90-electric-insert)
    (define-key map "+"        'f90-electric-insert)
    (define-key map "-"        'f90-electric-insert)
    (define-key map "*"        'f90-electric-insert)
    (define-key map "/"        'f90-electric-insert)
    map)
  "Keymap used in F90 mode.")

;; Menu bar support.
(if f90-xemacs-flag
    (defvar f90-xemacs-menu
      '("F90"
	["Indent Subprogram"       f90-indent-subprogram t]
	["Mark Subprogram"         f90-mark-subprogram t]
	["Beginning of Subprogram" f90-beginning-of-subprogram t]
	["End of Subprogram"       f90-end-of-subprogram t]
	"-----"
	["(Un)Comment Region"      f90-comment-region t]
	["Indent Region"           indent-region t]
	["Fill Region"             f90-fill-region t]
	"-----"
	["Break Line at Point"     f90-break-line t]
	["Join with Next Line"     f90-join-lines t]
	["Insert Newline"          newline t]
	["Insert Block End"        f90-insert-end t]
	"-----"
	["Upcase Keywords (buffer)"      f90-upcase-keywords t]
	["Upcase Keywords (region)"      f90-upcase-region-keywords t]
	["Capitalize Keywords (buffer)"  f90-capitalize-keywords t]
	["Capitalize Keywords (region)"  f90-capitalize-region-keywords t]
	["Downcase Keywords (buffer)"    f90-downcase-keywords t]
	["Downcase Keywords (region)"    f90-downcase-region-keywords t]
	"-----"
	["Toggle abbrev-mode"   abbrev-mode t]
	["Toggle auto-fill"     auto-fill-mode t])
      "XEmacs menu for F90 mode.")

  ;; Emacs.
  (defvar f90-menu-bar-menu
    (let ((map (make-sparse-keymap "F90")))
      (define-key map [f90-imenu-menu]
        '("Add imenu Menu" . f90-add-imenu-menu))
      (define-key map [abbrev-mode]
        '("Toggle abbrev-mode" . abbrev-mode))
      (define-key map [auto-fill-mode]
        '("Toggle auto-fill" . auto-fill-mode))
      (define-key map [line1] '("--"))
      (define-key map [f90-change-case-menu]
        '("Change Keyword Case" . f90-change-case-menu))
      (define-key map [f90-font-lock-menu]
        '("Highlighting" . f90-font-lock-menu))
      (define-key map [line2] '("--"))
      (define-key map [f90-insert-end]
        '("Insert Block End" . f90-insert-end))
      (define-key map [f90-join-lines]
        '("Join with Next Line" . f90-join-lines))
      (define-key map [f90-break-line]
        '("Break Line at Point" . f90-break-line))
      (define-key map [line3] '("--"))
      (define-key map [f90-fill-region]
        '("Fill Region" . f90-fill-region))
      (put 'f90-fill-region 'menu-enable 'mark-active)
      (define-key map [indent-region]
        '("Indent Region" . indent-region))
      (define-key map [f90-comment-region]
        '("(Un)Comment Region" . f90-comment-region))
      (put 'f90-comment-region 'menu-enable 'mark-active)
      (define-key map [line4] '("--"))
      (define-key map [f90-end-of-subprogram]
        '("End of Subprogram" . f90-end-of-subprogram))
      (define-key map [f90-beginning-of-subprogram]
        '("Beginning of Subprogram" . f90-beginning-of-subprogram))
      (define-key map [f90-mark-subprogram]
        '("Mark Subprogram" . f90-mark-subprogram))
      (define-key map [f90-indent-subprogram]
        '("Indent Subprogram" . f90-indent-subprogram))
      map)
    "F90 mode top-level menu bar menu.")

  (define-key f90-mode-map [menu-bar f90-menu]
    (cons "F90" f90-menu-bar-menu))

  (defvar f90-change-case-menu
    (let ((map (make-sparse-keymap "Change Keyword Case")))
      (define-key map [dkr]
        '("Downcase Keywords (region)" . f90-downcase-region-keywords))
      (put 'f90-downcase-region-keywords 'menu-enable 'mark-active)
      (define-key map [ckr]
        '("Capitalize Keywords (region)" . f90-capitalize-region-keywords))
      (put 'f90-capitalize-region-keywords 'menu-enable 'mark-active)
      (define-key map [ukr]
        '("Upcase Keywords (region)" . f90-upcase-region-keywords))
      (put 'f90-upcase-region-keywords 'menu-enable 'mark-active)
      (define-key map [line] '("--"))
      (define-key map [dkb]
        '("Downcase Keywords (buffer)" . f90-downcase-keywords))
      (define-key map [ckb]
        '("Capitalize Keywords (buffer)" . f90-capitalize-keywords))
      (define-key map [ukb]
        '("Upcase Keywords (buffer)" . f90-upcase-keywords))
      map)
    "Submenu for change of case.")

  (defalias 'f90-change-case-menu f90-change-case-menu)

  ;; Font-lock-menu and function calls.
  (defalias 'f90-font-lock-on 'font-lock-mode)
  (put 'f90-font-lock-on 'menu-enable 'font-lock-mode)
  (put 'f90-font-lock-on 'menu-alias t)

  (defalias 'f90-font-lock-off 'font-lock-mode)
  (put 'f90-font-lock-off 'menu-enable '(not font-lock-mode))
  (put 'f90-font-lock-off 'menu-alias t)

  (defun f90-font-lock-1 ()
    "Set `font-lock-keywords' to `f90-font-lock-keywords-1'."
    (interactive)
    (font-lock-mode 1)
    (setq font-lock-keywords f90-font-lock-keywords-1)
    (font-lock-fontify-buffer))

  (defun f90-font-lock-2 ()
    "Set `font-lock-keywords' to `f90-font-lock-keywords-2'."
    (interactive)
    (font-lock-mode 1)
    (setq font-lock-keywords f90-font-lock-keywords-2)
    (font-lock-fontify-buffer))

  (defun f90-font-lock-3 ()
    "Set `font-lock-keywords' to `f90-font-lock-keywords-3'."
    (interactive)
    (font-lock-mode 1)
    (setq font-lock-keywords f90-font-lock-keywords-3)
    (font-lock-fontify-buffer))

  (defun f90-font-lock-4 ()
    "Set `font-lock-keywords' to `f90-font-lock-keywords-4'."
    (interactive)
    (font-lock-mode 1)
    (setq font-lock-keywords f90-font-lock-keywords-4)
    (font-lock-fontify-buffer))

  (defvar f90-font-lock-menu
    (let ((map (make-sparse-keymap "f90-font-lock-menu")))
      (define-key map [h4]
        '("Maximum highlighting (level 4)" . f90-font-lock-4))
      (define-key map [h3]
        '("Heavy highlighting (level 3)" . f90-font-lock-3))
      (define-key map [h2]
        '("Default highlighting (level 2)" . f90-font-lock-2))
      (define-key map [h1]
        '("Light highlighting (level 1)" . f90-font-lock-1))
      (define-key map [line] '("--"))
      (define-key map [floff]
        '("Turn off font-lock-mode" . f90-font-lock-on))
      (define-key map [flon]
        '("Turn on font-lock-mode" . f90-font-lock-off))
      map)
    "Submenu for highlighting using font-lock-mode.")

  (defalias 'f90-font-lock-menu f90-font-lock-menu)

  )

;; Regexps for finding program structures.
(defconst f90-blocks-re
  (concat "\\(block[ \t]*data\\|"
          (regexp-opt '("do" "if" "interface" "function" "module" "program"
                        "select" "subroutine" "type" "where" "forall"))
          "\\)\\>")
  "Regexp potentially indicating a \"block\" of F90 code.")

(defconst f90-program-block-re
  (regexp-opt '("program" "module" "subroutine" "function") 'paren)
  "Regexp used to locate the start/end of a \"subprogram\".")

(defconst f90-else-like-re
  "\\(else\\([ \t]*if\\|where\\)?\\|case[ \t]*\\(default\\|(\\)\\)"
  "Regexp matching an ELSE IF, ELSEWHERE, CASE statement.")

(defconst f90-end-if-re
  (concat "end[ \t]*"
          (regexp-opt '("if" "select" "where" "forall") 'paren)
          "\\>")
  "Regexp matching the end of an IF, SELECT, WHERE, FORALL block.")

(defconst f90-end-type-re
  "end[ \t]*\\(type\\|interface\\|block[ \t]*data\\)\\>"
  "Regexp matching the end of a TYPE, INTERFACE, BLOCK DATA section.")

(defconst f90-type-def-re
  "\\<\\(type\\)\\([^(\n]*\\)\\(::\\)?[ \t]*\\b\\(\\sw+\\)"
  "Regexp matching the declaration of a variable of derived type.")

(defconst f90-no-break-re
  (regexp-opt '("**" "//" "=>") 'paren)
  "Regexp specifying where not to break lines when filling.")

(defvar f90-cache-position nil
  "Temporary position used to speed up region operations.")
(make-variable-buffer-local 'f90-cache-position)

(defvar f90-imenu-flag nil
  "Non-nil means this buffer already has an imenu.")
(make-variable-buffer-local 'f90-imenu-flag)


;; Imenu support.
(defvar f90-imenu-generic-expression
  (let ((good-char "[^!\"\&\n \t]") (not-e "[^e!\n\"\& \t]")
	(not-n "[^n!\n\"\& \t]") (not-d "[^d!\n\"\& \t]"))
    (list
     '(nil "^[ \t0-9]*program[ \t]+\\(\\sw+\\)" 1)
     '("Modules" "^[ \t0-9]*module[ \t]+\\(\\sw+\\)[ \t]*\\(!\\|$\\)" 1)
     '("Types" "^[ \t0-9]*type[ \t]+\\(\\sw+\\)" 1)
     (list
      "Procedures"
      (concat
       "^[ \t0-9]*"
       "\\("
       ;; At least three non-space characters before function/subroutine.
       ;; Check that the last three non-space characters do not spell E N D.
       "[^!\"\&\n]*\\("
       not-e good-char good-char "\\|"
       good-char not-n good-char "\\|"
       good-char good-char not-d "\\)"
       "\\|"
       ;; Less than three non-space characters before function/subroutine.
       good-char "?" good-char "?"
       "\\)"
       "[ \t]*\\(function\\|subroutine\\)[ \t]+\\(\\sw+\\)")
      4)))
  "Generic imenu expression for F90 mode.")

(defun f90-add-imenu-menu ()
  "Add an imenu menu to the menubar."
  (interactive)
  (if f90-imenu-flag
      (message "%s" "F90-imenu already exists.")
    (imenu-add-to-menubar "F90-imenu")
    (redraw-frame (selected-frame))
    (setq f90-imenu-flag t)))

(put 'f90-add-imenu-menu 'menu-enable '(not f90-imenu-flag))


;; When compiling under GNU Emacs, load imenu during compilation.
;; If you have 19.22 or earlier, comment this out, or get imenu.
(or f90-xemacs-flag (eval-when-compile (require 'imenu)))


;; Abbrevs have generally two letters, except standard types `c, `i, `r, `t.
(defvar f90-mode-abbrev-table nil "Abbrev table for F90 mode.")
(if (not f90-mode-abbrev-table)
    (let (abbrevs-changed)
      (define-abbrev-table 'f90-mode-abbrev-table 
        '(("`al"  "allocate"      nil 0 t)
          ("`ab"  "allocatable"   nil 0 t)
          ("`as"  "assignment"    nil 0 t)
          ("`ba"  "backspace"     nil 0 t)
          ("`bd"  "block data"    nil 0 t)
          ("`c"   "character"     nil 0 t)
          ("`cl"  "close"         nil 0 t)
          ("`cm"  "common"        nil 0 t)
          ("`cx"  "complex"       nil 0 t)
          ("`cn"  "contains"      nil 0 t)
          ("`cy"  "cycle"         nil 0 t)
          ("`de"  "deallocate"    nil 0 t)
          ("`df"  "define"        nil 0 t)
          ("`di"  "dimension"     nil 0 t)
          ("`dw"  "do while"      nil 0 t)
          ("`el"  "else"          nil 0 t)
          ("`eli" "else if"       nil 0 t)
          ("`elw" "elsewhere"     nil 0 t)
          ("`eq"  "equivalence"   nil 0 t)
          ("`ex"  "external"      nil 0 t)
          ("`ey"  "entry"         nil 0 t)
          ("`fl"  "forall"        nil 0 t)
          ("`fo"  "format"        nil 0 t)
          ("`fu"  "function"      nil 0 t)
          ("`fa"  ".false."       nil 0 t)
          ("`im"  "implicit none" nil 0 t)
          ("`in " "include"       nil 0 t)
          ("`i"   "integer"       nil 0 t)
          ("`it"  "intent"        nil 0 t)
          ("`if"  "interface"     nil 0 t)
          ("`lo"  "logical"       nil 0 t)
          ("`mo"  "module"        nil 0 t)
          ("`na"  "namelist"      nil 0 t)
          ("`nu"  "nullify"       nil 0 t)
          ("`op"  "optional"      nil 0 t)
          ("`pa"  "parameter"     nil 0 t)
          ("`po"  "pointer"       nil 0 t)
          ("`pr"  "print"         nil 0 t)
          ("`pi"  "private"       nil 0 t)
          ("`pm"  "program"       nil 0 t)
          ("`pu"  "public"        nil 0 t)
          ("`r"   "real"          nil 0 t)
          ("`rc"  "recursive"     nil 0 t)
          ("`rt"  "return"        nil 0 t)
          ("`rw"  "rewind"        nil 0 t)
          ("`se"  "select"        nil 0 t)
          ("`sq"  "sequence"      nil 0 t)
          ("`su"  "subroutine"    nil 0 t)
          ("`ta"  "target"        nil 0 t)
          ("`tr"  ".true."        nil 0 t)
          ("`t"   "type"          nil 0 t)
          ("`wh"  "where"         nil 0 t)
          ("`wr"  "write"         nil 0 t)))))


(defcustom f90-mode-hook nil
  "Hook run when entering F90 mode."
  :type 'hook
  :options '(f90-add-imenu-menu)
  :group 'f90)

;;;###autoload
(defun f90-mode ()
  "Major mode for editing Fortran 90,95 code in free format.

\\[f90-indent-new-line] indents current line and creates a new\
 indented line.
\\[f90-indent-line] indents the current line.
\\[f90-indent-subprogram] indents the current subprogram.

Type `? or `\\[help-command] to display a list of built-in\
 abbrevs for F90 keywords.

Key definitions:
\\{f90-mode-map}

Variables controlling indentation style and extra features:

`f90-do-indent'
  Extra indentation within do blocks (default 3).
`f90-if-indent'
  Extra indentation within if/select case/where/forall blocks (default 3).
`f90-type-indent'
  Extra indentation within type/interface/block-data blocks (default 3).
`f90-program-indent'
  Extra indentation within program/module/subroutine/function blocks
  (default 2).
`f90-continuation-indent'
  Extra indentation applied to continuation lines (default 5).
`f90-comment-region'
  String inserted by function \\[f90-comment-region] at start of each
  line in region (default \"!!!$\").
`f90-indented-comment-re'
  Regexp determining the type of comment to be intended like code
  (default \"!\").
`f90-directive-comment-re'
  Regexp of comment-like directive like \"!HPF\\\\$\", not to be indented
  (default \"!hpf\\\\$\").
`f90-break-delimiters'
  Regexp holding list of delimiters at which lines may be broken
  (default \"[-+*/><=,% \\t]\").
`f90-break-before-delimiters'
  Non-nil causes `f90-do-auto-fill' to break lines before delimiters
  (default t).
`f90-beginning-ampersand'
  Automatic insertion of \& at beginning of continuation lines (default t).
`f90-smart-end'
  From an END statement, check and fill the end using matching block start.
  Allowed values are 'blink, 'no-blink, and nil, which determine
  whether to blink the matching beginning (default 'blink).
`f90-auto-keyword-case'
  Automatic change of case of keywords (default nil).
  The possibilities are 'downcase-word, 'upcase-word, 'capitalize-word.
`f90-leave-line-no'
  Do not left-justify line numbers (default nil).
`f90-keywords-re'
  List of keywords used for highlighting/upcase-keywords etc.

Turning on F90 mode calls the value of the variable `f90-mode-hook'
with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'f90-mode
        mode-name "F90"
        local-abbrev-table f90-mode-abbrev-table)
  (set-syntax-table f90-mode-syntax-table)
  (use-local-map f90-mode-map)
  (set (make-local-variable 'indent-line-function) 'f90-indent-line)
  (set (make-local-variable 'indent-region-function) 'f90-indent-region)
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'comment-start) "!")
  (set (make-local-variable 'comment-start-skip) "!+ *")
  (set (make-local-variable 'comment-indent-function) 'f90-comment-indent)
  (set (make-local-variable 'abbrev-all-caps) t)
  (set (make-local-variable 'normal-auto-fill-function) 'f90-do-auto-fill)
  (setq indent-tabs-mode nil)           ; auto buffer local
  ;; Setting up things for font-lock.
  (when f90-xemacs-flag
    (put 'f90-mode 'font-lock-keywords-case-fold-search t)
    (when (and (featurep 'menubar)
               current-menubar
               (not (assoc "F90" current-menubar)))
      (set-buffer-menubar (copy-sequence current-menubar))
      (add-submenu nil f90-xemacs-menu)))
  ;; XEmacs: Does not need a special case, since both emacsen work alike -sb.
  (set (make-local-variable 'font-lock-defaults)
       '((f90-font-lock-keywords f90-font-lock-keywords-1
                                 f90-font-lock-keywords-2
                                 f90-font-lock-keywords-3
                                 f90-font-lock-keywords-4)
         nil t))
  ;; Tell imenu how to handle f90.
  (set (make-local-variable 'imenu-case-fold-search) t)
  (set (make-local-variable 'imenu-generic-expression)
        f90-imenu-generic-expression)
  (set (make-local-variable 'add-log-current-defun-function)
       #'f90-current-defun)
  (run-hooks 'f90-mode-hook))


;; Inline-functions.
(defsubst f90-in-string ()
  "Return non-nil if point is inside a string.
Checks from `point-min', or `f90-cache-position', if that is non-nil
and lies before point."
  (let ((beg-pnt
	 (if (and f90-cache-position (> (point) f90-cache-position))
	     f90-cache-position
	   (point-min))))
    (nth 3 (parse-partial-sexp beg-pnt (point)))))

(defsubst f90-in-comment ()
  "Return non-nil if point is inside a comment.
Checks from `point-min', or `f90-cache-position', if that is non-nil
and lies before point."
  (let ((beg-pnt
	 (if (and f90-cache-position (> (point) f90-cache-position))
	     f90-cache-position
	   (point-min))))
    (nth 4 (parse-partial-sexp beg-pnt (point)))))

(defsubst f90-line-continued ()
  "Return t if the current line is a continued one.
This includes comment lines embedded in continued lines, but
not the last line of a continued statement."
  (save-excursion
    (beginning-of-line)
    (while (and (looking-at "[ \t]*\\(!\\|$\\)") (zerop (forward-line -1))))
    (end-of-line)
    (while (f90-in-comment)
      (search-backward "!" (line-beginning-position))
      (skip-chars-backward "!"))
    (skip-chars-backward " \t")
    (= (preceding-char) ?&)))

(defsubst f90-current-indentation ()
  "Return indentation of current line.
Line-numbers are considered whitespace characters."
  (save-excursion (beginning-of-line) (skip-chars-forward " \t0-9")))

(defsubst f90-indent-to (col &optional no-line-number)
  "Indent current line to column COL.
If optional argument NO-LINE-NUMBER is nil, jump over a possible
line-number before indenting."
  (beginning-of-line)
  (if (not no-line-number)
      (skip-chars-forward " \t0-9"))
  (delete-horizontal-space)
  (if (zerop (current-column))
      (indent-to col)
    (indent-to col 1)))                 ; leave >= 1 space after line number

(defsubst f90-get-present-comment-type ()
  "If point lies within a comment, return the string starting the comment.
For example, \"!\" or \"!!\"."
  (save-excursion
    (when (f90-in-comment)
      (beginning-of-line)
      (re-search-forward "[!]+" (line-end-position))
      (while (f90-in-string)
        (re-search-forward "[!]+" (line-end-position))
        (match-string 0)))))

(defsubst f90-equal-symbols (a b)
  "Compare strings A and B neglecting case and allowing for nil value."
  (let ((a-local (if a (downcase a) nil))
	(b-local (if b (downcase b) nil)))
    (equal a-local b-local)))

;; XEmacs 19.11 & 19.12 return a single char when matching an empty regexp.
;; The next 2 functions are therefore longer than necessary.
(defsubst f90-looking-at-do ()
  "Return (\"do\" NAME) if a do statement starts after point.
NAME is nil if the statement has no label."
  (if (looking-at "\\(\\(\\sw+\\)[ \t]*\:\\)?[ \t]*\\(do\\)\\>")
      (let (label
	    (struct (match-string 3)))
	(if (looking-at "\\(\\sw+\\)[ \t]*\:")
	    (setq label (match-string 1)))
	(list struct label))))

(defsubst f90-looking-at-select-case ()
  "Return (\"select\" NAME) if a select-case statement starts after point.
NAME is nil if the statement has no label."
  (if (looking-at "\\(\\(\\sw+\\)[ \t]*\:\\)?[ \t]*\
\\(select\\)[ \t]*case[ \t]*(")
      (let (label
	    (struct (match-string 3)))
	(if (looking-at "\\(\\sw+\\)[ \t]*\:")
	    (setq label (match-string 1)))
	(list struct label))))

(defsubst f90-looking-at-if-then ()
  "Return (\"if\" NAME) if an if () then statement starts after point.
NAME is nil if the statement has no label."
  (save-excursion
    (when (looking-at "\\(\\(\\sw+\\)[ \t]*\:\\)?[ \t]*\\(if\\)\\>")
      (let (label
            (struct (match-string 3)))
        (if (looking-at "\\(\\sw+\\)[ \t]*\:")
            (setq label (match-string 1)))
        (let ((pos (scan-lists (point) 1 0)))
          (and pos (goto-char pos)))
        (skip-chars-forward " \t")
        (if (or (looking-at "then\\>")
                (when (f90-line-continued)
                  (f90-next-statement)
                  (skip-chars-forward " \t0-9&")
                  (looking-at "then\\>")))
            (list struct label))))))

(defsubst f90-looking-at-where-or-forall ()
  "Return (KIND NAME) if a where or forall block starts after point.
NAME is nil if the statement has no label."
  (if (looking-at "\\(\\(\\sw+\\)[ \t]*\:\\)?[ \t]*\
\\(where\\|forall\\)[ \t]*(.*)[ \t]*\\(!\\|$\\)")
      (let (label
	    (struct (match-string 3)))
	(if (looking-at "\\(\\sw+\\)[ \t]*\:")
	    (setq label (match-string 1)))
	(list struct label))))

(defsubst f90-looking-at-type-like ()
  "Return (KIND NAME) if a type/interface/block-data block starts after point.
NAME is non-nil only for type."
  (cond
   ((looking-at f90-type-def-re)
    (list (match-string 1) (match-string 4)))
   ((looking-at "\\(interface\\|block[\t]*data\\)\\>")
    (list (match-string 1) nil))))

(defsubst f90-looking-at-program-block-start ()
  "Return (KIND NAME) if a program block with name NAME starts after point."
  (cond
   ((looking-at "\\(program\\)[ \t]+\\(\\sw+\\)\\>")
    (list (match-string 1) (match-string 2)))
   ((and (not (looking-at "module[ \t]*procedure\\>"))
	 (looking-at "\\(module\\)[ \t]+\\(\\sw+\\)\\>"))
    (list (match-string 1) (match-string 2)))
   ((and (not (looking-at "end[ \t]*\\(function\\|subroutine\\)"))
	 (looking-at "[^!'\"\&\n]*\\(function\\|subroutine\\)\
[ \t]+\\(\\sw+\\)"))
    (list (match-string 1) (match-string 2)))))

(defsubst f90-looking-at-program-block-end ()
  "Return (KIND NAME) if a block with name NAME ends after point."
  (if (looking-at (concat "end[ \t]*" f90-blocks-re
			  "?\\([ \t]+\\(\\sw+\\)\\)?\\>"))
      (list (match-string 1) (match-string 3))))

(defsubst f90-comment-indent ()
  "Return the indentation to be used for a comment starting at point.
Used for `comment-indent-function' by F90 mode.
\"!!!\", `f90-directive-comment-re', variable `f90-comment-region' return 0.
`f90-indented-comment-re' (if not trailing code) calls `f90-calculate-indent'.
Any other type return `comment-column', leaving at least one space after code."
  (cond ((looking-at "!!!") 0)
	((and f90-directive-comment-re
	      (looking-at f90-directive-comment-re)) 0)
	((looking-at (regexp-quote f90-comment-region)) 0)
	((and (looking-at f90-indented-comment-re)
	      ;; Don't attempt to indent trailing comment as code.
	      (save-excursion
		(skip-chars-backward " \t")
		(bolp)))
	 (f90-calculate-indent))
	(t (skip-chars-backward " \t")
	   (max (if (bolp) 0 (1+ (current-column))) comment-column))))

(defsubst f90-present-statement-cont ()
  "Return continuation properties of present statement.
Possible return values are:
single - statement is not continued.
begin  - current line is the first in a continued statement.
end    - current line is the last in a continued statement
middle - current line is neither first nor last in a continued statement.
Comment lines embedded amongst continued lines return 'middle."
  (let (pcont cont)
    (save-excursion
      (setq pcont (if (f90-previous-statement) (f90-line-continued))))
    (setq cont (f90-line-continued))
    (cond ((and (not pcont) (not cont)) 'single)
 	  ((and (not pcont) cont)       'begin)
 	  ((and pcont       (not cont)) 'end)
 	  ((and pcont       cont)       'middle)
 	  (t (error "The impossible occurred")))))

(defsubst f90-indent-line-no ()
  "If `f90-leave-line-no' is nil, left-justify a line number.
Leaves point at the first non-blank character after the line number.
Call from beginning of line."
  (if (and (null f90-leave-line-no) (looking-at "[ \t]+[0-9]"))
      (delete-horizontal-space))
  (skip-chars-forward " \t0-9"))

(defsubst f90-no-block-limit ()
  "Return nil if point is at the edge of a code block.
Searches line forward for \"function\" or \"subroutine\",
if all else fails."
  (let ((eol (line-end-position)))
    (save-excursion
      (not (or (looking-at "end")
	       (looking-at "\\(do\\|if\\|else\\(if\\|where\\)?\
\\|select[ \t]*case\\|case\\|where\\|forall\\)\\>")
	       (looking-at "\\(program\\|module\\|interface\\|\
block[ \t]*data\\)\\>")
	       (looking-at "\\(contains\\|\\sw+[ \t]*:\\)")
	       (looking-at f90-type-def-re)
	       (re-search-forward "\\(function\\|subroutine\\)" eol t))))))

(defsubst f90-update-line ()
  "Change case of current line as per `f90-auto-keyword-case'."
  (if f90-auto-keyword-case
      (f90-change-keywords f90-auto-keyword-case
                           (line-beginning-position) (line-end-position))))

(defun f90-electric-insert ()
  "Change keyword case and auto-fill line as operators are inserted."
  (interactive)
  (self-insert-command 1)
  (if auto-fill-function (f90-do-auto-fill) ; also updates line
    (f90-update-line)))


(defun f90-get-correct-indent ()
  "Get correct indent for a line starting with line number.
Does not check type and subprogram indentation."
  (let ((epnt (line-end-position)) icol cont)
    (save-excursion
      (while (and (f90-previous-statement)
		  (or (progn
			(setq cont (f90-present-statement-cont))
			(or (eq cont 'end) (eq cont 'middle)))
		      (looking-at "[ \t]*[0-9]"))))
      (setq icol (current-indentation))
      (beginning-of-line)
      (when (re-search-forward "\\(if\\|do\\|select\\|where\\|forall\\)"
                               (line-end-position) t)
        (beginning-of-line)
        (skip-chars-forward " \t")
        (cond ((f90-looking-at-do)
               (setq icol (+ icol f90-do-indent)))
              ((or (f90-looking-at-if-then)
                   (f90-looking-at-where-or-forall)
                   (f90-looking-at-select-case))
               (setq icol (+ icol f90-if-indent))))
        (end-of-line))
      (while (re-search-forward
	      "\\(if\\|do\\|select\\|where\\|forall\\)" epnt t)
	(beginning-of-line)
        (skip-chars-forward " \t0-9")
	(cond ((f90-looking-at-do)
               (setq icol (+ icol f90-do-indent)))
              ((or (f90-looking-at-if-then)
                   (f90-looking-at-where-or-forall)
                   (f90-looking-at-select-case))
               (setq icol (+ icol f90-if-indent)))
              ((looking-at f90-end-if-re)
               (setq icol (- icol f90-if-indent)))
              ((looking-at "end[ \t]*do\\>")
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
	      ((eq cont 'middle) (setq icol (current-indentation)))
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
		       ((looking-at "[ \t]*#") ; check for cpp directive
			(setq icol 0))
		       (t
			(skip-chars-forward " \t0-9")
			(cond ((or (looking-at f90-else-like-re)
				   (looking-at f90-end-if-re))
			       (setq icol (- icol f90-if-indent)))
			      ((looking-at "end[ \t]*do\\>")
			       (setq icol (- icol f90-do-indent)))
			      ((looking-at f90-end-type-re)
			       (setq icol (- icol f90-type-indent)))
			      ((or (looking-at "contains[ \t]*\\(!\\|$\\)")
				   (f90-looking-at-program-block-end))
			       (setq icol (- icol f90-program-indent))))))
		 ))))
    icol))

(defun f90-previous-statement ()
  "Move point to beginning of the previous F90 statement.
Return nil if no previous statement is found.
A statement is a line which is neither blank nor a comment."
  (interactive)
  (let (not-first-statement)
    (beginning-of-line)
    (while (and (setq not-first-statement (zerop (forward-line -1)))
		(looking-at "[ \t0-9]*\\(!\\|$\\|#\\)")))
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
Return (TYPE NAME), or nil if not found."
  (interactive)
  (let ((count 1) (case-fold-search t) matching-beg)
    (beginning-of-line)
    (skip-chars-forward " \t0-9")
    (if (setq matching-beg (f90-looking-at-program-block-start))
	(setq count (1- count)))
    (while (and (not (zerop count))
		(re-search-backward f90-program-block-re nil 'move))
      (beginning-of-line)
      (skip-chars-forward " \t0-9")
      (cond ((setq matching-beg (f90-looking-at-program-block-start))
             (setq count (1- count)))
            ((f90-looking-at-program-block-end)
             (setq count (1+ count)))))
    (beginning-of-line)
    (if (zerop count)
	matching-beg
      (message "No beginning found.")
      nil)))

(defun f90-end-of-subprogram ()
  "Move point to the end of subprogram.
Return (TYPE NAME), or nil if not found."
  (interactive)
  (let ((count 1) (case-fold-search t) matching-end)
    (beginning-of-line)
    (skip-chars-forward " \t0-9")
    (if (setq matching-end (f90-looking-at-program-block-end))
	(setq count (1- count)))
    (end-of-line)
    (while (and (not (zerop count))
		(re-search-forward f90-program-block-re nil 'move))
      (beginning-of-line)
      (skip-chars-forward " \t0-9")
      (cond ((f90-looking-at-program-block-start)
	     (setq count (1+ count)))
	    ((setq matching-end (f90-looking-at-program-block-end))
	     (setq count (1- count))))
      (end-of-line))
    (forward-line 1)
    (if (zerop count)
	matching-end
      (message "No end found.")
      nil)))

(defvar f90-mark-subprogram-overlay nil
  "Used internally by `f90-mark-subprogram' to highlight the subprogram.")
(make-variable-buffer-local 'f90-mark-subprogram-overlay)

(defun f90-mark-subprogram ()
  "Put mark at end of F90 subprogram, point at beginning, push marks.
If called interactively, highlight the subprogram with the face `highlight'.
Call again to remove the highlighting."
  (interactive)
  (let ((pos (point)) program)
    (f90-end-of-subprogram)
    (push-mark (point) t)
    (goto-char pos)
    (setq program (f90-beginning-of-subprogram))
    ;; The keywords in the preceding lists assume case-insensitivity.
    (if f90-xemacs-flag
	(zmacs-activate-region)
      (setq mark-active t
            deactivate-mark nil)
      (if (interactive-p)
	  (if (overlayp f90-mark-subprogram-overlay)
	      (if (overlay-buffer f90-mark-subprogram-overlay)
		  (delete-overlay f90-mark-subprogram-overlay)
		(move-overlay f90-mark-subprogram-overlay (point) (mark)))
	    (setq f90-mark-subprogram-overlay (make-overlay (point) (mark)))
	    (overlay-put f90-mark-subprogram-overlay 'face 'highlight))))
    program))

(defun f90-comment-region (beg-region end-region)
  "Comment/uncomment every line in the region.
Insert the variable `f90-comment-region' at the start of every line
in the region, or, if already present, remove it."
  (interactive "*r")
  (let ((end (make-marker)))
    (set-marker end end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (if (looking-at (regexp-quote f90-comment-region))
	(delete-region (point) (match-end 0))
      (insert f90-comment-region))
    (while (and (zerop (forward-line 1))
		(< (point) (marker-position end)))
      (if (looking-at (regexp-quote f90-comment-region))
	  (delete-region (point) (match-end 0))
	(insert f90-comment-region)))
    (set-marker end nil)))

(defun f90-indent-line (&optional no-update)
  "Indent current line as F90 code.
Unless optional argument NO-UPDATE is non-nil, call `f90-update-line'
after indenting."
  (interactive)
  (let (indent no-line-number (pos (make-marker)) (case-fold-search t))
    (set-marker pos (point))
    (beginning-of-line)			; digits after & \n are not line-nos
    (if (save-excursion (and (f90-previous-statement) (f90-line-continued)))
	(progn (setq no-line-number t) (skip-chars-forward " \t"))
      (f90-indent-line-no))
    (if (looking-at "!")
	(setq indent (f90-comment-indent))
      (if (and (looking-at "end") f90-smart-end)
          (f90-match-end))
      (setq indent (f90-calculate-indent)))
    (if (not (zerop (- indent (current-column))))
        (f90-indent-to indent no-line-number))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (< (point) (marker-position pos))
	(goto-char (marker-position pos)))
    (if auto-fill-function
        (f90-do-auto-fill)              ; also updates line
      (if (not no-update) (f90-update-line)))
    (set-marker pos nil)))

(defun f90-indent-new-line ()
  "Reindent current line, insert a newline and indent the newline.
An abbrev before point is expanded if the variable `abbrev-mode' is non-nil.
If run in the middle of a line, the line is not broken."
  (interactive)
  (let (string cont (case-fold-search t))
    (if abbrev-mode (expand-abbrev))
    (beginning-of-line)			; reindent where likely to be needed
    (f90-indent-line-no)
    (f90-indent-line 'no-update)
    (end-of-line)
    (delete-horizontal-space)		; destroy trailing whitespace
    (setq string (f90-in-string)
          cont (f90-line-continued))
    (if (and string (not cont)) (insert "&"))
    (f90-update-line)
    (newline)
    (if (or string (and cont f90-beginning-ampersand)) (insert "&"))
    (f90-indent-line 'no-update)))


(defun f90-indent-region (beg-region end-region)
  "Indent every line in region by forward parsing."
  (interactive "*r")
  (let ((end-region-mark (make-marker))
        (save-point (point-marker))
	block-list ind-lev ind-curr ind-b cont
	struct beg-struct end-struct)
    (set-marker end-region-mark end-region)
    (goto-char beg-region)
    ;; First find a line which is not a continuation line or comment.
    (beginning-of-line)
    (while (and (looking-at "[ \t]*[0-9]*\\(!\\|#\\|[ \t]*$\\)")
		(progn (f90-indent-line 'no-update)
		       (zerop (forward-line 1)))
		(< (point) end-region-mark)))
    (setq cont (f90-present-statement-cont))
    (while (and (or (eq cont 'middle) (eq cont 'end))
		(f90-previous-statement))
      (setq cont (f90-present-statement-cont)))
    ;; Process present line for beginning of block.
    (setq f90-cache-position (point))
    (f90-indent-line 'no-update)
    (setq ind-lev (f90-current-indentation)
          ind-curr ind-lev)
    (beginning-of-line)
    (skip-chars-forward " \t0-9")
    (setq struct nil
          ind-b (cond ((setq struct (f90-looking-at-do)) f90-do-indent)
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
      (if (looking-at "[ \t]*!")
          (f90-indent-to (f90-comment-indent))
        (if (not (zerop (- (current-indentation)
                           (+ ind-curr f90-continuation-indent))))
            (f90-indent-to (+ ind-curr f90-continuation-indent) 'no-line-no))))
    ;; Process all following lines.
    (while (and (zerop (forward-line 1)) (< (point) end-region-mark))
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
		   (f90-block-match (car beg-struct) (car (cdr beg-struct))
				    (car end-struct) (car (cdr end-struct)))))
	     (setq ind-b
		   (cond ((looking-at f90-end-if-re) f90-if-indent)
			 ((looking-at "end[ \t]*do\\>")  f90-do-indent)
			 ((looking-at f90-end-type-re) f90-type-indent)
			 ((f90-looking-at-program-block-end)
			  f90-program-indent)))
	     (if ind-b (setq ind-lev (- ind-lev ind-b)))
	     (setq ind-curr ind-lev))
	    (t (setq ind-curr ind-lev)))
      ;; Do the indentation if necessary.
      (if (not (zerop (- ind-curr (current-column))))
	  (f90-indent-to ind-curr))
      (while (and (f90-line-continued) (zerop (forward-line 1))
		  (< (point) end-region-mark))
        (if (looking-at "[ \t]*!")
            (f90-indent-to (f90-comment-indent))
          (if (not (zerop (- (current-indentation)
                             (+ ind-curr f90-continuation-indent))))
              (f90-indent-to
               (+ ind-curr f90-continuation-indent) 'no-line-no)))))
    ;; Restore point, etc.
    (setq f90-cache-position nil)
    (goto-char save-point)
    (set-marker end-region-mark nil)
    (set-marker save-point nil)
    (if f90-xemacs-flag
	(zmacs-deactivate-region)
      (deactivate-mark))))

(defun f90-indent-subprogram ()
  "Properly indent the subprogram containing point."
  (interactive)
  (save-excursion
    (let ((program (f90-mark-subprogram)))
      (if program
	  (progn
	    (message "Indenting %s %s..."
		     (car program) (car (cdr program)))
	    (indent-region (point) (mark) nil)
	    (message "Indenting %s %s...done"
		     (car program) (car (cdr program))))
	(message "Indenting the whole file...")
	(indent-region (point) (mark) nil)
	(message "Indenting the whole file...done")))))

(defun f90-break-line (&optional no-update)
  "Break line at point, insert continuation marker(s) and indent.
Unless in a string or comment, or if the optional argument NO-UPDATE
is non-nil, call `f90-update-line' after inserting the continuation marker."
  (interactive)
  (let (ctype)
    (cond ((f90-in-string)
	   (insert "&") (newline 1) (insert "&"))
	  ((f90-in-comment)
	   (setq ctype (f90-get-present-comment-type))
	   (newline 1)
	   (insert ctype))
	  (t (insert "&")
	     (if (not no-update) (f90-update-line))
	     (newline 1)
	     (if f90-beginning-ampersand (insert "&")))))
  (indent-according-to-mode))

(defun f90-find-breakpoint ()
  "From `fill-column', search backward for break-delimiter."
  (let ((bol (line-beginning-position)))
    (re-search-backward f90-break-delimiters bol)
    (if (not f90-break-before-delimiters)
        (if (looking-at f90-no-break-re)
            (forward-char 2)
          (forward-char))
      (backward-char)
      (if (not (looking-at f90-no-break-re))
          (forward-char)))))

(defun f90-do-auto-fill ()
  "Break line if non-white characters beyond `fill-column'.
Update keyword case first."
  (interactive)
  ;; Break line before or after last delimiter (non-word char) if
  ;; position is beyond fill-column.
  ;; Will not break **, //, or => (as specified by f90-no-break-re).
  (f90-update-line)
  (while (> (current-column) fill-column)
    (let ((pos-mark (point-marker)))
      (move-to-column fill-column)
      (or (f90-in-string) (f90-find-breakpoint))
      (f90-break-line)
      (goto-char pos-mark)
      (set-marker pos-mark nil))))


(defun f90-join-lines ()
  "Join present line with next line, if this line ends with \&."
  (interactive)
  (let (pos (oldpos (point)))
    (end-of-line)
    (skip-chars-backward " \t")
    (when (= (preceding-char) ?&)
      (delete-char -1)
      (setq pos (point))
      (forward-line 1)
      (skip-chars-forward " \t")
      (if (looking-at "\&") (delete-char 1))
      (delete-region pos (point))
      (unless (f90-in-string)
        (delete-horizontal-space)
        (insert " "))
      (if (and auto-fill-function
               (> (line-end-position) fill-column))
          (f90-do-auto-fill))
      (goto-char oldpos)
      t)))                              ; return t if joined something

(defun f90-fill-region (beg-region end-region)
  "Fill every line in region by forward parsing.  Join lines if possible."
  (interactive "*r")
  (let ((end-region-mark (make-marker))
        (go-on t)
	f90-smart-end f90-auto-keyword-case auto-fill-function)
    (set-marker end-region-mark end-region)
    (goto-char beg-region)
    (while go-on
      ;; Join as much as possible.
      (while (f90-join-lines))
      ;; Chop the line if necessary.
      (while (> (save-excursion (end-of-line) (current-column))
		fill-column)
	(move-to-column fill-column)
	(f90-find-breakpoint)
	(f90-break-line 'no-update))
      (setq go-on (and (< (point) (marker-position end-region-mark))
                       (zerop (forward-line 1)))
            f90-cache-position (point)))
    (setq f90-cache-position nil)
    (if f90-xemacs-flag
	(zmacs-deactivate-region)
      (deactivate-mark))))

(defun f90-block-match (beg-block beg-name end-block end-name)
  "Match end-struct with beg-struct and complete end-block if possible.
BEG-BLOCK is the type of block as indicated at the start (e.g., do).
BEG-NAME is the block start name (may be nil).
END-BLOCK is the type of block as indicated at the end (may be nil).
END-NAME is the block end name (may be nil).
Leave point at the end of line."
  (search-forward "end" (line-end-position))
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
    (if (not (looking-at "[ \t]*!")) (delete-horizontal-space))))

(defun f90-match-end ()
  "From an end block statement, find the corresponding block and name."
  (interactive)
  (let ((count 1) (top-of-window (window-start))
	(end-point (point)) (case-fold-search t)
	matching-beg beg-name end-name beg-block end-block end-struct)
    (when (save-excursion (beginning-of-line) (skip-chars-forward " \t0-9")
                          (setq end-struct (f90-looking-at-program-block-end)))
      (setq end-block (car end-struct)
            end-name  (car (cdr end-struct)))
      (save-excursion
        (beginning-of-line)
        (while
            (and (not (zerop count))
                 (let ((stop nil) notexist)
                   (while (not stop)
                     (setq notexist
                           (not (re-search-backward
                                 (concat "\\(" f90-blocks-re "\\)") nil t)))
                     (if notexist
                         (setq stop t)
                       (setq stop
                             (not (or (f90-in-string)
                                      (f90-in-comment))))))
                   (not notexist)))
          (beginning-of-line)
          (skip-chars-forward " \t0-9")
          (cond ((setq matching-beg
                       (or
                        (f90-looking-at-do)
                        (f90-looking-at-if-then)
                        (f90-looking-at-where-or-forall)
                        (f90-looking-at-select-case)
                        (f90-looking-at-type-like)
                        (f90-looking-at-program-block-start)))
                 (setq count (1- count)))
                ((looking-at (concat "end[ \t]*" f90-blocks-re "\\b"))
                 (setq count (1+ count)))))
        (if (not (zerop count))
            (message "No matching beginning.")
          (f90-update-line)
          (if (eq f90-smart-end 'blink)
              (if (< (point) top-of-window)
                  (message "Matches %s: %s"
                           (what-line)
                           (buffer-substring
                            (line-beginning-position)
                            (line-end-position)))
                (sit-for 1)))
          (setq beg-block (car matching-beg)
                beg-name (car (cdr matching-beg)))
          (goto-char end-point)
          (beginning-of-line)
          (f90-block-match beg-block beg-name end-block end-name))))))

(defun f90-insert-end ()
  "Insert a complete end statement matching beginning of present block."
  (interactive)
  (let ((f90-smart-end (or f90-smart-end 'blink)))
    (insert "end")
    (f90-indent-new-line)))

;; Abbrevs and keywords.

(defun f90-abbrev-start ()
  "Typing `\\[help-command] or `? lists all the F90 abbrevs.
Any other key combination is executed normally."
  (interactive)
  (let (e c)
    (insert last-command-char)
    (if (not f90-xemacs-flag)
        (setq c (read-event))
      (setq e (next-command-event)
            c (event-to-character e)))
    ;; Insert char if not equal to `?'.
    (if (or (eq c ??) (eq c help-char))
	(f90-abbrev-help)
      (if f90-xemacs-flag
	  (setq unread-command-event e)
	(setq unread-command-events (list c))))))

(defun f90-abbrev-help ()
  "List the currently defined abbrevs in F90 mode."
  (interactive)
  (message "Listing abbrev table...")
  (display-buffer (f90-prepare-abbrev-list-buffer))
  (message "Listing abbrev table...done"))

(defun f90-prepare-abbrev-list-buffer ()
  "Create a buffer listing the F90 mode abbreviations."
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
  "Change the case of F90 keywords in the region (if specified) or buffer.
CHANGE-WORD should be one of 'upcase-word, 'downcase-word, capitalize-word."
  (save-excursion
    (setq beg (or beg (point-min))
          end (or end (point-max)))
    (let ((keyword-re
	   (concat "\\("
		   f90-keywords-re "\\|" f90-procedures-re "\\|"
		   f90-hpf-keywords-re "\\|" f90-operators-re "\\)"))
	  (ref-point (point-min)) 
	  (modified (buffer-modified-p))
          state saveword back-point)
      (goto-char beg)
      (unwind-protect
	  (while (re-search-forward keyword-re end t)
	    (unless (progn
                      (setq state (parse-partial-sexp ref-point (point)))
                      (or (nth 3 state) (nth 4 state)
                          (save-excursion ; check for cpp directive
                            (beginning-of-line)
                            (skip-chars-forward " \t0-9")
                            (looking-at "#"))))
	      (setq ref-point (point)
		    back-point (save-excursion (backward-word 1) (point))
                    saveword (buffer-substring back-point ref-point))
	      (funcall change-word -1)
	      (or (string= saveword (buffer-substring back-point ref-point))
		  (setq modified t))))
	(or modified (set-buffer-modified-p nil))))))


(defun f90-current-defun ()
  "Function to use for `add-log-current-defun-function' in F90 mode."
  (save-excursion
    (nth 1 (f90-beginning-of-subprogram))))

(provide 'f90)

;;; f90.el ends here
