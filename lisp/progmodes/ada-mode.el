;; @(#) ada-mode.el --- major-mode for editing Ada source.

;; Copyright (C) 1994-1999 Free Software Foundation, Inc.

;; Author: Rolf Ebert      <ebert@inf.enst.fr>
;;      Markus Heritsch <Markus.Heritsch@studbox.uni-stuttgart.de>
;;      Emmanuel Briot  <briot@gnat.com>
;; Maintainer: Emmanuel Briot <briot@gnat.com>
;; Ada Core Technologies's version:   $Revision: 1.70 $
;; Keywords: languages ada

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;; This mode is a major mode for editing Ada83 and Ada95 source code.
;;; This is a major rewrite of the file packaged with Emacs-20.  The
;;; ada-mode is composed of four lisp file, ada-mode.el, ada-xref.el,
;;; ada-prj.el and ada-stmt.el. Only this file (ada-mode.el) is
;;; completly independant from the GNU Ada compiler Gnat, distributed
;;; by Ada Core Technologies. All the other files rely heavily on
;;; features provides only by Gnat.
;;;
;;; Note: this mode will not work with Emacs 19. If you are on a VMS
;;; system, where the latest version of Emacs is 19.28, you will need
;;; another file, called ada-vms.el, that provides some required
;;; functions.

;;; Usage:
;;; Emacs should enter Ada mode automatically when you load an Ada file.
;;; By default, the valid extensions for Ada files are .ads, .adb or .ada
;;; If the ada-mode does not start automatically, then simply type the
;;; following command :
;;;     M-x ada-mode
;;;
;;; By default, ada-mode is configured to take full advantage of the GNAT
;;; compiler (the menus will include the cross-referencing features,...).
;;; If you are using another compiler, you might want to set the following
;;; variable in your .emacs (Note: do not set this in the ada-mode-hook, it
;;; won't work) :
;;;    (setq ada-which-compiler 'generic)
;;;
;;; This mode requires find-file.el to be present on your system.

;;; History:
;;; The first Ada mode for GNU Emacs was written by V. Broman in
;;; 1985. He based his work on the already existing Modula-2 mode.
;;; This was distributed as ada.el in versions of Emacs prior to 19.29.
;;;
;;; Lynn Slater wrote an extensive Ada mode in 1989. It consisted of
;;; several files with support for dired commands and other nice
;;; things. It is currently available from the PAL
;;; (wuarchive.wustl.edu:/languages/ada) as ada-mode-1.06a.tar.Z.
;;;
;;; The probably very first Ada mode (called electric-ada.el) was
;;; written by Steven D. Litvintchouk and Steven M. Rosen for the
;;; Gosling Emacs. L. Slater based his development on ada.el and
;;; electric-ada.el.
;;;
;;; A complete rewrite by M. Heritsch and R. Ebert has been done.
;;; Some ideas from the Ada mode mailing list have been
;;; added.  Some of the functionality of L. Slater's mode has not
;;; (yet) been recoded in this new mode.  Perhaps you prefer sticking
;;; to his version.
;;;
;;; A complete rewrite for Emacs-20 / Gnat-3.11 has been done by Ada Core
;;; Technologies. Please send bugs to  briot@gnat.com

;;; Credits:
;;;   Many thanks to John McCabe <john@assen.demon.co.uk> for sending so
;;;     many patches included in this package.
;;;   Christian Egli <Christian.Egli@hcsd.hac.com>:
;;;     ada-imenu-generic-expression
;;;   Many thanks also to the following persons that have contributed one day
;;;   to the ada-mode
;;;     Philippe Waroquiers (PW) <philippe@cfmu.eurocontrol.be> in particular,
;;;     woodruff@stc.llnl.gov (John Woodruff)
;;;     jj@ddci.dk (Jesper Joergensen)
;;;     gse@ocsystems.com (Scott Evans)
;;;     comar@gnat.com (Cyrille Comar)
;;;     stephen.leake@gsfc.nasa.gov (Stephen Leake)
;;;    and others for their valuable hints.

;;; Code:
;;; Note: Every function is this package is compiler-independent.
;;; The names start with  ada-
;;; The variables that the user can edit can all be modified throught
;;;   the customize mode. They are sorted in alphabetical order in this
;;;   file.


;; this function is needed at compile time
(eval-and-compile
  (defun ada-check-emacs-version (major minor &optional is_xemacs)
    "Returns t if Emacs's version is greater or equal to major.minor.
if IS_XEMACS is non-nil, check for XEmacs instead of Emacs"
    (let ((xemacs_running (or (string-match "Lucid"  emacs-version)
			      (string-match "XEmacs" emacs-version))))
      (and (or (and is_xemacs xemacs_running)
	     (not (or is_xemacs xemacs_running)))
	   (or (> emacs-major-version major)
	       (and (= emacs-major-version major)
		    (>= emacs-minor-version minor)))))))
  

;;  We create a constant for that, for efficiency only
;;  This should not be evaluated at compile time, only a runtime
(defconst ada-xemacs (boundp 'running-xemacs)
  "Return t if we are using XEmacs")

(unless ada-xemacs
  (require 'outline))

(eval-and-compile
  (condition-case nil (require 'find-file) (error nil)))

;;  This call should not be made in the release that is done for the
;;  official FSF Emacs, since it does nothing useful for the latest version
(require 'ada-support)

(defvar ada-mode-hook nil
  "*List of functions to call when Ada mode is invoked.
This hook is automatically executed after the ada-mode is
fully loaded.
This is a good place to add Ada environment specific bindings.")

(defgroup ada nil
  "Major mode for editing Ada source in Emacs"
  :group 'languages)

(defcustom ada-auto-case t
  "*Non-nil means automatically change case of preceding word while typing.
Casing is done according to `ada-case-keyword', `ada-case-identifier'
and `ada-case-attribute'."
  :type 'boolean :group 'ada)

(defcustom ada-broken-decl-indent 0
  "*Number of columns to indent a broken declaration.

An example is :
  declare
     A,
     >>>>>B : Integer;  --  from ada-broken-decl-indent"
  :type 'integer :group 'ada)

(defcustom ada-broken-indent 2
  "*Number of columns to indent the continuation of a broken line.

An example is :
   My_Var : My_Type := (Field1 =>
                        >>>>>>>>>Value);  -- from ada-broken-indent"
  :type 'integer :group 'ada)

(defcustom ada-case-attribute 'ada-capitalize-word
  "*Function to call to adjust the case of Ada attributes.
It may be `downcase-word', `upcase-word', `ada-loose-case-word' or
`ada-capitalize-word'."
  :type '(choice (const downcase-word)
                 (const upcase-word)
                 (const ada-capitalize-word)
                 (const ada-loose-case-word))
  :group 'ada)

(defcustom ada-case-exception-file "~/.emacs_case_exceptions"
  "*Name of the file that contains the list of special casing
exceptions for identifiers.
This file should contain one word per line, that gives the casing
to be used for that words in Ada files"
  :type 'file :group 'ada)

(defcustom ada-case-keyword 'downcase-word
  "*Function to call to adjust the case of Ada keywords.
It may be `downcase-word', `upcase-word', `ada-loose-case-word' or
`ada-capitalize-word'."
  :type '(choice (const downcase-word)
                 (const upcase-word)
                 (const ada-capitalize-word)
                 (const ada-loose-case-word))
  :group 'ada)

(defcustom ada-case-identifier 'ada-loose-case-word
  "*Function to call to adjust the case of an Ada identifier.
It may be `downcase-word', `upcase-word', `ada-loose-case-word' or
`ada-capitalize-word'."
  :type '(choice (const downcase-word)
                 (const upcase-word)
                 (const ada-capitalize-word)
                 (const ada-loose-case-word))
  :group 'ada)

(defcustom ada-clean-buffer-before-saving t
  "*Non-nil means `remove-trailing-spaces' and `untabify' buffer before saving."
  :type 'boolean :group 'ada)

(defcustom ada-indent 3
  "*Size of Ada indentation.

An example is :
procedure Foo is
begin
>>>>>>>>>>null;  --  from ada-indent"
  :type 'integer  :group 'ada)

(defcustom ada-indent-after-return t
  "*Non-nil means automatically indent after RET or LFD."
  :type 'boolean :group 'ada)

(defcustom ada-indent-comment-as-code t
  "*Non-nil means indent comment lines as code"
  :type 'boolean :group 'ada)

(defcustom ada-indent-is-separate t
  "*Non-nil means indent 'is separate' or 'is abstract' if on a single line."
  :type 'boolean :group 'ada)

(defcustom ada-indent-record-rel-type 3
  "*Indentation for 'record' relative to 'type' or 'use'.

An example is:
   type A is
   >>>>>>>>>>>record    --  from ada-indent-record-rel-type"
  :type 'integer :group 'ada)

(defcustom ada-indent-return 0
  "*Indentation for 'return' relative to the matching 'function' statement.
If ada-indent-return is null or negative, the indentation is done relative to
the open parenthesis (if there is no parenthesis, ada-broken-indent is used)

An example is:
   function A (B : Integer)
   >>>>>return C;       --  from ada-indent-return"
  :type 'integer :group 'ada)

(defcustom ada-indent-to-open-paren t
  "*Non-nil means indent according to the innermost open parenthesis."
  :type 'boolean :group 'ada)

(defcustom ada-fill-comment-prefix "-- "
  "*Text inserted in the first columns when filling a comment paragraph.
Note: if you modify this variable, you will have to restart the ada-mode to
reread this variable."
  :type 'string :group 'ada)

(defcustom ada-fill-comment-postfix " --"
  "*Text inserted at the end of each line when filling a comment paragraph.
with `ada-fill-comment-paragraph-postfix'."
  :type 'string :group 'ada)

(defcustom ada-label-indent -4
  "*Number of columns to indent a label.

An example is:
procedure Foo is
begin
>>>>>>>>>>>>Label:  --  from ada-label-indent"
  :type 'integer :group 'ada)

(defcustom ada-language-version 'ada95
  "*Do we program in `ada83' or `ada95'?"
  :type '(choice (const ada83) (const ada95)) :group 'ada)

(defcustom ada-move-to-declaration nil
  "*Non-nil means `ada-move-to-start' moves point to the subprog declaration,
not to 'begin'."
  :type 'boolean :group 'ada)

(defcustom ada-popup-key '[down-mouse-3]
  "*Key used for binding the contextual menu.
if nil, no contextual menu is available")

(defcustom ada-search-directories
  '("." "$ADA_INCLUDE_PATH" "/usr/adainclude" "/usr/local/adainclude"
    "/opt/gnu/adainclude")
  "*List of directories to search for Ada files.  See the description
for the `ff-search-directories' variable.
Emacs will automatically add the paths defined in your project file."
  :type '(repeat (choice :tag "Directory"
                         (const :tag "default" nil)
                         (directory :format "%v")))
  :group 'ada)

(defcustom ada-stmt-end-indent 0
  "*Number of columns to indent a statement end keyword on a separate line.

An example is:
   if A = B
   >>>>>>>>>>>then   --  from ada-stmt-end-indent"
  :type 'integer :group 'ada)

(defcustom ada-tab-policy 'indent-auto
  "*Control the behaviour of the TAB key.
This is used only in the ada-tab and ada-untab functions.
Must be one of :
`indent-rigidly' : always adds ada-indent blanks at the beginning of the line.
`indent-auto'    : use indentation functions in this file.
`always-tab'     : do indent-relative."
  :type '(choice (const indent-auto)
                 (const indent-rigidly)
                 (const always-tab))
  :group 'ada)

(defcustom ada-when-indent 3
  "*Indentation for 'when' relative to 'exception' or 'case'.

An example is:
   case A is
   >>>>>>>>when B =>     --  from ada-when-indentx"
  :type 'integer :group 'ada)

(defcustom ada-which-compiler 'gnat
  "*Name of the compiler we use. This will determine what features are
made available through the ada-mode. The possible choices are :

`gnat': Use Ada Core Technologies' Gnat compiler. Add some cross-referencing
    features
`generic': Use a generic compiler"
  :type '(choice (const gnat)
                 (const generic))
  :group 'ada)


;;; ---- end of user configurable variables


(defvar ada-body-suffixes '(".adb")
  "List of possible suffixes for Ada body files. The extensions should
include a `.' if needed")

(defvar ada-spec-suffixes '(".ads")
  "List of possible suffixes for Ada spec files. The extensions should
include a `.' if needed")

(defvar ada-mode-menu (make-sparse-keymap)
  "Menu for ada-mode")

(defvar ada-mode-map (make-sparse-keymap)
  "Local keymap used for Ada mode.")

(defvar ada-mode-syntax-table nil
  "Syntax table to be used for editing Ada source code.")

(defvar ada-mode-symbol-syntax-table nil
  "Syntax table for Ada, where `_' is a word constituent.")

(eval-when-compile
  (defconst ada-83-string-keywords
    '("abort" "abs" "accept" "access" "all" "and" "array" "at" "begin"
      "body" "case" "constant" "declare" "delay" "delta" "digits" "do"
      "else" "elsif" "end" "entry" "exception" "exit" "for" "function"
      "generic" "goto" "if" "in" "is" "limited" "loop" "mod" "new"
      "not" "null" "of" "or" "others" "out" "package" "pragma" "private"
      "procedure" "raise" "range" "record" "rem" "renames" "return"
      "reverse" "select" "separate" "subtype" "task" "terminate" "then"
      "type" "use" "when" "while" "with" "xor")
    "List of ada keywords  -- This variable is not used instead to define
ada-83-keywords and ada-95-keywords"))

(defvar ada-ret-binding nil
  "Variable to save key binding of RET when casing is activated.")

(defvar ada-case-exception '()
  "Alist of words (entities) that have special casing, and should not
be reindented according to the function `ada-case-identifier'.
Its value is read from the file `ada-case-exception-file'")

(defvar ada-lfd-binding nil
  "Variable to save key binding of LFD when casing is activated.")

(defvar ada-other-file-alist nil
  "Variable used by find-file to find the name of the other package.
See `ff-other-file-alist'"
  )

;;; ---- Below are the regexp used in this package for parsing

(defconst ada-83-keywords
  (eval-when-compile
    (concat "\\<" (regexp-opt ada-83-string-keywords t) "\\>"))
  "Regular expression for looking at Ada83 keywords.")

(defconst ada-95-keywords
  (eval-when-compile
    (concat "\\<" (regexp-opt
                   (append
                    '("abstract" "aliased" "protected" "requeue"
                      "tagged" "until")
                    ada-83-string-keywords) t) "\\>"))
  "Regular expression for looking at Ada95 keywords.")

(defvar ada-keywords ada-95-keywords
  "Regular expression for looking at Ada keywords.")

(defconst ada-ident-re
  "\\(\\sw\\|[_.]\\)+"
  "Regexp matching Ada (qualified) identifiers.")

(defvar ada-procedure-start-regexp
  "^[ \t]*\\(procedure\\|function\\|task\\)[ \t\n]+\\(\\(\\sw\\|[_.]\\)+\\)"
  "Regexp used to find Ada procedures/functions.")

(defvar ada-package-start-regexp
  "^[ \t]*\\(package\\)"
  "Regexp used to find Ada packages")


;;; ---- regexps for indentation functions

(defvar ada-block-start-re
  (eval-when-compile
    (concat "\\<\\(" (regexp-opt '("begin" "declare" "else"
                                   "exception" "generic" "loop" "or"
                                   "private" "select" ))
            "\\|\\(\\(limited\\|abstract\\|tagged\\)[ \t\n]+\\)*record\\)\\>"))
  "Regexp for keywords starting Ada blocks.")

(defvar ada-end-stmt-re
  (eval-when-compile
    (concat "\\("
            ";"                                        "\\|"
            "=>[ \t]*$"                                "\\|"
            "^[ \t]*separate[ \t]*(\\(\\sw\\|[_.]\\)+)"  "\\|"
            "\\<" (regexp-opt '("begin" "declare" "is" "do" "else" "generic" "loop"
                                "private" "record" "select" "then") t) "\\>"  "\\|"
            "^[ \t]*" (regexp-opt '("function" "package" "procedure")
                                  t) "\\>\\(\\sw\\|[ \t_.]\\)+\\<is\\>"        "\\|"
            "^[ \t]*exception\\>"
            "\\)")                      )
  "Regexp of possible ends for a non-broken statement.
A new statement starts after these.")

(defvar ada-matching-start-re
  (eval-when-compile
    (concat "\\<"
            (regexp-opt
             '("end" "loop" "select" "begin" "case" "do"
               "if" "task" "package" "record" "protected") t)
            "\\>"))
  "Regexp used in ada-goto-matching-start")

(defvar ada-matching-decl-start-re
  (eval-when-compile
    (concat "\\<"
            (regexp-opt
             '("is" "separate" "end" "declare" "if" "new" "begin" "generic") t)
            "\\>"))
  "Regexp used in ada-goto-matching-decl-start")


(defvar ada-loop-start-re
  "\\<\\(for\\|while\\|loop\\)\\>"
  "Regexp for the start of a loop.")

(defvar ada-subprog-start-re
  (eval-when-compile
    (concat "\\<" (regexp-opt '("accept" "entry" "function" "package" "procedure"
                                "protected" "task") t) "\\>"))
  "Regexp for the start of a subprogram.")

(defvar ada-named-block-re
  "[ \t]*\\(\\sw\\|_\\)+[ \t]*:[^=]"
  "Regexp of the name of a block or loop.")



;;------------------------------------------------------------------
;; Support for imenu  (see imenu.el)
;;------------------------------------------------------------------

(defvar ada-imenu-generic-expression
  (list
   '(nil "^[ \t]*\\(procedure\\|function\\)[ \t\n]+\\(\\(\\sw\\|_\\)+\\)[ \t\n]*\\([ \t\n]\\|([^)]+)\\)[ \t\n]*\\(return[ \t\n]+\\(\\sw\\|[_.]\\)+[ \t\n]*\\)?is[ \t\n]" 2)
   (list "*Specs*"
         (concat
          "^[ \t]*\\(procedure\\|function\\)[ \t\n]+\\(\\(\\sw\\|_\\)+\\)"
          "\\("
          "\\([ \t\n]+\\|[ \t\n]*([^)]+)\\)";; parameter list or simple space
          "\\([ \t\n]*return[ \t\n]+\\(\\sw\\|[_.]\\)+[ \t\n]*\\)?"
          "\\)?;") 2)
   '("*Tasks*" "^[ \t]*task[ \t]+\\(\\(body\\|type\\)[ \t]+\\)?\\(\\(\\sw\\|_\\)+\\)" 3)
   '("*Type Defs*" "^[ \t]*\\(sub\\)?type[ \t]+\\(\\(\\sw\\|_\\)+\\)" 2)
   '("*Packages*" "^[ \t]*package[ \t]+\\(\\(body[ \t]+\\)?\\(\\sw\\|[_.]\\)+\\)" 1))
  "Imenu generic expression for Ada mode.  See `imenu-generic-expression'.
This variable will create two submenus, one for type and subtype definitions,
the other for subprograms declarations. The main menu will reference the bodies
of the subprograms.")



;;------------------------------------------------------------
;;  Supporte for compile.el
;;------------------------------------------------------------

(defun ada-compile-mouse-goto-error ()
  "mouse interface for ada-compile-goto-error"
  (interactive)
  (mouse-set-point last-input-event)
  (ada-compile-goto-error (point))
  )

(defun ada-compile-goto-error (pos)
  "replaces compile-goto-error from compile.el: if point is on an file and line
location, go to this position. It adds to compile.el the capacity to go to a
reference in an error message.
For instance, on this line:
  foo.adb:61:11: missing argument for parameter set in call to size declared at foo.ads:11
both file locations can be clicked on and jumped to"
  (interactive "d")
  (goto-char pos)

  (skip-chars-backward "-a-zA-Z0-9_:./\\")
  (cond
   ;;  special case: looking at a filename:line not at the beginning of a line
   ((and (not (bolp))
	 (looking-at
	  "\\(\\(\\sw\\|[_-.]\\)+\\):\\([0-9]+\\)\\(:\\([0-9]+\\)\\)?"))
    (let ((line (match-string 3))
          (error-pos (point-marker))
          source)
      (save-excursion
        (save-restriction
          (widen)
          (set-buffer (compilation-find-file (point-marker) (match-string 1)
                                             "./"))
          (if (stringp line)
              (goto-line (string-to-number line)))
          (set 'source (point-marker))))
      (compilation-goto-locus (cons source error-pos))
      ))

   ;; otherwise, default behavior
   (t
    (compile-goto-error))
   )
  (recenter))

;;;-------------
;;;  functions
;;;-------------

(defun ada-create-syntax-table ()
  "Create the syntax table for Ada mode."
  ;; There are two different syntax-tables.  The standard one declares
  ;; `_' as a symbol constituant, in the second one, it is a word
  ;; constituant.  For some search and replacing routines we
  ;; temporarily switch between the two.
  (interactive)
  (set 'ada-mode-syntax-table (make-syntax-table))
  (set-syntax-table  ada-mode-syntax-table)

  ;; define string brackets (`%' is alternative string bracket, but
  ;; almost never used as such and throws font-lock and indentation
  ;; off the track.)
  (modify-syntax-entry ?%  "$" ada-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" ada-mode-syntax-table)

  (modify-syntax-entry ?:  "." ada-mode-syntax-table)
  (modify-syntax-entry ?\; "." ada-mode-syntax-table)
  (modify-syntax-entry ?&  "." ada-mode-syntax-table)
  (modify-syntax-entry ?\|  "." ada-mode-syntax-table)
  (modify-syntax-entry ?+  "." ada-mode-syntax-table)
  (modify-syntax-entry ?*  "." ada-mode-syntax-table)
  (modify-syntax-entry ?/  "." ada-mode-syntax-table)
  (modify-syntax-entry ?=  "." ada-mode-syntax-table)
  (modify-syntax-entry ?<  "." ada-mode-syntax-table)
  (modify-syntax-entry ?>  "." ada-mode-syntax-table)
  (modify-syntax-entry ?$ "." ada-mode-syntax-table)
  (modify-syntax-entry ?\[ "." ada-mode-syntax-table)
  (modify-syntax-entry ?\] "." ada-mode-syntax-table)
  (modify-syntax-entry ?\{ "." ada-mode-syntax-table)
  (modify-syntax-entry ?\} "." ada-mode-syntax-table)
  (modify-syntax-entry ?. "." ada-mode-syntax-table)
  (modify-syntax-entry ?\\ "." ada-mode-syntax-table)
  (modify-syntax-entry ?\' "." ada-mode-syntax-table)

  ;; a single hyphen is punctuation, but a double hyphen starts a comment
  (modify-syntax-entry ?-  ". 12" ada-mode-syntax-table)

  ;; # is set to be a matched-pair, since it is used for based numbers,
  ;; as in 16#3f#. The syntax class will be modifed later when it
  ;; appears at the beginning of a line for gnatprep statements.
  ;; For Emacs, the modification is done in font-lock-syntactic-keywords
  ;; or ada-after-change-function.
  ;; For XEmacs, this is not done correctly for now, based numbers won't
  ;; be handled correctly.
  (if ada-xemacs
      (modify-syntax-entry ?#  "<" ada-mode-syntax-table)
    (modify-syntax-entry ?#  "$" ada-mode-syntax-table))

  ;; and \f and \n end a comment
  (modify-syntax-entry ?\f  ">   " ada-mode-syntax-table)
  (modify-syntax-entry ?\n  ">   " ada-mode-syntax-table)

  ;; define what belongs in Ada symbols
  (modify-syntax-entry ?_ "_" ada-mode-syntax-table)

  ;; define parentheses to match
  (modify-syntax-entry ?\( "()" ada-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" ada-mode-syntax-table)

  (set 'ada-mode-symbol-syntax-table (copy-syntax-table ada-mode-syntax-table))
  (modify-syntax-entry ?_ "w" ada-mode-symbol-syntax-table)
  )

;;
;;  This is to support XEmacs, which does not have the syntax-table attribute
;;  as used in ada-after-change-function
;;  When executing parse-partial-sexp, we simply modify the strings before and
;;  after, so that the special constants '"', '(' and ')' do not interact
;;  with parse-partial-sexp.

(if ada-xemacs
    (defadvice parse-partial-sexp (around parse-partial-sexp-protect-constants)
      (let (change)
        (if (< to from)
            (let ((tmp from))
              (setq from to  to tmp)))
        (save-excursion
          (goto-char from)
          (while (re-search-forward "'\\([(\")#]\\)'" to t)
            (set 'change (cons (list (match-beginning 1)
                                     1
                                     (match-string 1))
                               change))
            (replace-match "'A'"))
          (goto-char from)
          (while (re-search-forward "\\(#[0-9a-fA-F]*#\\)" to t)
            (set 'change (cons (list (match-beginning 1)
                                     (length (match-string 1))
                                     (match-string 1))
                               change))
	    (replace-match (make-string (length (match-string 1)) ?@))))
        ad-do-it
        (save-excursion
          (while change
            (goto-char (caar change))
            (delete-char (cadar change))
            (insert (caddar change))
            (set 'change (cdr change)))))))

;;
;;  The following three functions handle the text properties in the buffer:
;;  the problem in Ada is that ' can be both a constant character delimiter
;;  and an attribute delimiter. To handle this easily (and allowing us to
;;  use the standard Emacs functions for sexp... as in ada-in-string-p), we
;;  change locally the syntax table every time we see a character constant.
;;  The three characters are then said to be part of a string.
;;  This handles nicely the '"' case (" is simply ignored in that case)
;;
;;  The idea for this code was borrowed from font-lock.el, which actually
;;  does the same job thanks to ada-font-lock-syntactic-keywords. No need
;;  to duplicate the work if we already use font-lock
;;
;;  This code is not executed for XEmacs, since the syntax-table attribute is
;;  not known

(defun ada-deactivate-properties ()
  "Deactivate ada-mode's properties handling, since this would be
a duplicate of font-lock"
  (remove-hook 'after-change-functions 'ada-after-change-function t))

(defun ada-initialize-properties ()
  "Initialize some special text properties in the whole buffer.
In particular, character constants that contain string delimiters are said
to be strings.
We also treat  #..# as numbers, instead of gnatprep comments
"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (re-search-forward "'.'" nil t)
        (add-text-properties (match-beginning 0) (match-end 0)
                             '(syntax-table ("'" . ?\"))))
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#" nil t)
        (add-text-properties (match-beginning 0) (match-end 0)
                             '(syntax-table (11 . 10))))
      (set-buffer-modified-p nil)

      ;;  Setting this only if font-lock is not set won't work
      ;;  if the user activates or deactivates font-lock-mode,
      ;;  but will make things faster most of the time
      (make-local-hook 'after-change-functions)
      (add-hook 'after-change-functions 'ada-after-change-function nil t)
      )))

(defun ada-after-change-function (beg end old-len)
  "Called every time a character is changed in the buffer"
  ;; borrowed from font-lock.el
  (let ((inhibit-point-motion-hooks t)
        (eol (point)))
    (save-excursion
      (save-match-data
        (beginning-of-line)
        (remove-text-properties (point) eol '(syntax-table nil))
        (while (re-search-forward "'.'" eol t)
          (add-text-properties (match-beginning 0) (match-end 0)
                               '(syntax-table ("'" . ?\"))))
        (beginning-of-line)
        (if (looking-at "^[ \t]*#")
            (add-text-properties (match-beginning 0) (match-end 0)
                                 '(syntax-table (11 . 10))))
        ))))


(defvar ada-contextual-menu-on-identifier nil)

(defvar ada-contextual-menu
  (if ada-xemacs
      '("Ada"
	["Goto Declaration/Body" ada-goto-declaration
	 :included ada-contextual-menu-on-identifier]
	["Goto Previous Reference" ada-xref-goto-previous-reference]
	["List References" ada-find-references
	 :included ada-contextual-menu-on-identifier]
	["-" nil nil]
	["Other File" ff-find-other-file]
	["Goto Parent Unit" ada-goto-parent]
	)
    
    (let ((map (make-sparse-keymap "Ada")))
      ;; The identifier part
      (if (equal ada-which-compiler 'gnat)
	  (progn
	    (define-key-after map [Ref]
	      '(menu-item "Goto Declaration/Body"
			  ada-point-and-xref
			  :visible ada-contextual-menu-on-identifier
			  ) t)
	    (define-key-after map [Prev]
	      '("Goto Previous Reference" .ada-xref-goto-previous-reference) t)
	    (define-key-after map [List]
	      '(menu-item "List References"
			  ada-find-references
			  :visible ada-contextual-menu-on-identifier) t)
	    (define-key-after map [-] '("-" nil) t)
	    ))
      (define-key-after map [Other] '("Other file" . ff-find-other-file) t)
      (define-key-after map [Parent] '("Goto Parent Unit" . ada-goto-parent)t)
      map)))

(defun ada-popup-menu (position)
  "Pops up a contextual menu, depending on where the user clicked"
  (interactive "e")
  (mouse-set-point last-input-event)

  (setq ada-contextual-menu-on-identifier
	(and (or (= (char-syntax (char-after)) ?w)
		 (= (char-after) ?_))
	     (not (ada-in-string-or-comment-p))
	     (save-excursion (skip-syntax-forward "w")
			      (not (ada-after-keyword-p)))
	     ))
  (let (choice)
    (if ada-xemacs
	(set 'choice (popup-menu ada-contextual-menu))
      (set 'choice (x-popup-menu position ada-contextual-menu)))
    (if choice
	(funcall (lookup-key ada-contextual-menu (vector (car choice)))))))

;;;###autoload
(defun ada-add-extensions (spec body)
  "Add a new set of extensions to the ones recognized by ada-mode.
The addition is done so that `goto-other-file' works as expected"
  
  (let* ((reg (concat (regexp-quote body) "$"))
	 (tmp (assoc reg ada-other-file-alist)))
    (if tmp
	(setcdr tmp (list (cons spec (cadr tmp))))
      (add-to-list 'ada-other-file-alist (list reg (list spec)))))
  
  (let* ((reg (concat (regexp-quote spec) "$"))
	 (tmp (assoc reg ada-other-file-alist)))
    (if tmp
	(setcdr tmp (list (cons body (cadr tmp))))
      (add-to-list 'ada-other-file-alist (list reg (list body)))))

  (add-to-list 'auto-mode-alist (cons spec 'ada-mode))
  (add-to-list 'auto-mode-alist (cons body 'ada-mode))

  (add-to-list 'ada-spec-suffixes spec)
  (add-to-list 'ada-body-suffixes body)

  ;; Support for speedbar (Specifies that we want to see these files in
  ;; speedbar)
  (condition-case nil
      (progn
        (require 'speedbar)
        (speedbar-add-supported-extension spec)
        (speedbar-add-supported-extension body)))
  )



;;;###autoload
(defun ada-mode ()
  "Ada mode is the major mode for editing Ada code.

Bindings are as follows: (Note: 'LFD' is control-j.)

 Indent line                                          '\\[ada-tab]'
 Indent line, insert newline and indent the new line. '\\[newline-and-indent]'

 Re-format the parameter-list point is in             '\\[ada-format-paramlist]'
 Indent all lines in region                           '\\[ada-indent-region]'

 Adjust case of identifiers and keywords in region    '\\[ada-adjust-case-region]'
 Adjust case of identifiers and keywords in buffer    '\\[ada-adjust-case-buffer]'

 Fill comment paragraph, justify and append postfix   '\\[fill-paragraph]'

 Next func/proc/task '\\[ada-next-procedure]'  Previous func/proc/task '\\[ada-previous-procedure]'
 Next package        '\\[ada-next-package]'  Previous package        '\\[ada-previous-package]'

 Goto matching start of current 'end ...;'            '\\[ada-move-to-start]'
 Goto end of current block                            '\\[ada-move-to-end]'

Comments are handled using standard GNU Emacs conventions, including:
 Start a comment                                      '\\[indent-for-comment]'
 Comment region                                       '\\[comment-region]'
 Uncomment region                                     '\\[ada-uncomment-region]'
 Continue comment on next line                        '\\[indent-new-comment-line]'

If you use imenu.el:
 Display index-menu of functions & procedures         '\\[imenu]'

If you use find-file.el:
 Switch to other file (Body <-> Spec)                 '\\[ff-find-other-file]'
                                                   or '\\[ff-mouse-find-other-file]
 Switch to other file in other window                 '\\[ada-ff-other-window]'
                                                   or '\\[ff-mouse-find-other-file-other-window]
 If you use this function in a spec and no body is available, it gets created with body stubs.

If you use ada-xref.el:
 Goto declaration:          '\\[ada-point-and-xref]' on the identifier
                         or '\\[ada-goto-declaration]' with point on the identifier
 Complete identifier:       '\\[ada-complete-identifier]'"

  (interactive)
  (kill-all-local-variables)

  (set (make-local-variable 'require-final-newline) t)

  (make-local-variable 'comment-start)
  (if ada-fill-comment-prefix
      (set 'comment-start ada-fill-comment-prefix)
    (set 'comment-start "-- "))

  ;;  Set the paragraph delimiters so that one can select a whole block
  ;;  simply with M-h
  (set (make-local-variable 'paragraph-start) "[ \t\n\f]*$")
  (set (make-local-variable 'paragraph-separate) "[ \t\n\f]*$")

  ;; comment end must be set because it may hold a wrong value if
  ;; this buffer had been in another mode before. RE
  (set (make-local-variable 'comment-end) "")

  ;; used by autofill and indent-new-comment-line
  (set (make-local-variable 'comment-start-skip) "---*[ \t]*")

  ;; used by autofill to break a comment line and continue it on another line.
  ;; The reason we need this one is that the default behavior does not work
  ;; correctly with the definition of paragraph-start above when the comment
  ;; is right after a multiline subprogram declaration (the comments are
  ;; aligned under the latest parameter, not under the declaration start).
  (set (make-local-variable 'comment-line-break-function)
       (lambda (&optional soft) (let ((fill-prefix nil))
  				  (indent-new-comment-line soft))))
  
  (set (make-local-variable 'indent-line-function)
       'ada-indent-current-function)

  (set (make-local-variable 'comment-column) 40)

  ;;  Emacs 20.3 defines a comment-padding to insert spaces between
  ;;  the comment and the text. We do not want any, this is already
  ;;  included in comment-start
  (unless ada-xemacs
    (progn
      (if (ada-check-emacs-version 20 3)
          (progn
            (set (make-local-variable 'parse-sexp-ignore-comments) t)
            (set (make-local-variable 'comment-padding) 0)))
      (set (make-local-variable 'parse-sexp-lookup-properties) t)
      ))

  (set 'case-fold-search t)
  (if (boundp 'imenu-case-fold-search)
      (set 'imenu-case-fold-search t))

  (set (make-local-variable 'fill-paragraph-function)
       'ada-fill-comment-paragraph)

  (set (make-local-variable 'imenu-generic-expression)
       ada-imenu-generic-expression)

  ;;  Support for compile.el
  ;;  We just substitute our own functions to go to the error.
  (add-hook 'compilation-mode-hook
            '(lambda()
	       (set 'compile-auto-highlight 40)
               (define-key compilation-minor-mode-map [mouse-2]
                 'ada-compile-mouse-goto-error)
               (define-key compilation-minor-mode-map "\C-c\C-c"
                 'ada-compile-goto-error)
               (define-key compilation-minor-mode-map "\C-m"
                 'ada-compile-goto-error)
               ))

  ;;  font-lock support :
  ;;  We need to set some properties for Xemacs, and define some variables
  ;;  for Emacs

  (if ada-xemacs
      ;;  XEmacs
      (put 'ada-mode 'font-lock-defaults
           '(ada-font-lock-keywords
             nil t ((?\_ . "w") (?# . ".")) beginning-of-line))
    ;;  Emacs
    (set (make-local-variable 'font-lock-defaults)
         '(ada-font-lock-keywords
           nil t
           ((?\_ . "w") (?# . "."))
           beginning-of-line
           (font-lock-syntactic-keywords . ada-font-lock-syntactic-keywords)))
    )
  
  ;; Set up support for find-file.el.
  (set (make-variable-buffer-local 'ff-other-file-alist)
       'ada-other-file-alist)
  (set (make-variable-buffer-local 'ff-search-directories)
       'ada-search-directories)
  (setq ff-post-load-hooks    'ada-set-point-accordingly
	ff-file-created-hooks 'ada-make-body)
  (add-hook 'ff-pre-load-hooks 'ada-which-function-are-we-in)
  
  ;; Some special constructs for find-file.el
  ;; We do not need to add the construction for 'with', which is in the
  ;; standard find-file.el
  ;; Go to the parent package :
  (make-local-variable 'ff-special-constructs)
  (add-to-list 'ff-special-constructs
	       (cons (eval-when-compile
		       (concat "^\\(private[ \t]\\)?[ \t]*package[ \t]+"
			       "\\(body[ \t]+\\)?"
			       "\\(\\(\\sw\\|[_.]\\)+\\)\\.\\(\\sw\\|_\\)+[ \t\n]+is"))
		     '(lambda ()
			(set 'fname (ff-get-file
				     ff-search-directories
				     (ada-make-filename-from-adaname
				      (match-string 3))
				     ada-spec-suffixes)))))
  ;; Another special construct for find-file.el : when in a separate clause,
  ;; go to the correct package.
  (add-to-list 'ff-special-constructs
	       (cons "^separate[ \t\n]*(\\(\\(\\sw\\|[_.]\\)+\\))"
		     '(lambda ()
			(set 'fname (ff-get-file
				     ff-search-directories
				     (ada-make-filename-from-adaname
				      (match-string 1))
				     ada-spec-suffixes)))))
  ;; Another special construct, that redefines the one in find-file.el. The
  ;; old one can handle only one possible type of extension for Ada files
  (add-to-list 'ff-special-constructs
	       (cons "^with[ \t]+\\([a-zA-Z0-9_\\.]+\\)" 
		     '(lambda ()
			(set 'fname (ff-get-file
				     ff-search-directories
				     (ada-make-filename-from-adaname
				      (match-string 1))
				     ada-spec-suffixes)))))
  
  ;;  Support for outline-minor-mode
  (set (make-local-variable 'outline-regexp)
       "\\([ \t]*\\(procedure\\|function\\|package\\|with\\|use\\)\\|--\\|end\\)")
  (set (make-local-variable 'outline-level) 'ada-outline-level)

  ;;  Support for imenu : We want a sorted index
  (set 'imenu-sort-function 'imenu--sort-by-name)

  ;;  Set up the contextual menu
  (if ada-popup-key
      (define-key ada-mode-map ada-popup-key 'ada-popup-menu))

  ;;  Support for indent-new-comment-line (Especially for XEmacs)
  (set 'comment-multi-line nil)
  (defconst comment-indent-function (lambda () comment-column))

  (set 'major-mode 'ada-mode)
  (set 'mode-name "Ada")

  (use-local-map ada-mode-map)

  (if ada-xemacs
      (easy-menu-add ada-mode-menu ada-mode-map))
  
  (set-syntax-table ada-mode-syntax-table)

  (if ada-clean-buffer-before-saving
      (progn
        ;; remove all spaces at the end of lines in the whole buffer.
        (add-hook 'local-write-file-hooks 'ada-remove-trailing-spaces)
        ;; convert all tabs to the correct number of spaces.
        (add-hook 'local-write-file-hooks
                  '(lambda () (untabify (point-min) (point-max))))))

  (run-hooks 'ada-mode-hook)

  ;;  Run this after the hook to give the users a chance to activate
  ;;  font-lock-mode

  (unless ada-xemacs
    (progn
      (ada-initialize-properties)
      (make-local-hook 'font-lock-mode-hook)
      (add-hook 'font-lock-mode-hook 'ada-deactivate-properties nil t)))

  ;; the following has to be done after running the ada-mode-hook
  ;; because users might want to set the values of these variable
  ;; inside the hook (MH)
  ;; Note that we add the new elements at the end of ada-other-file-alist
  ;; since some user might want to give priority to some other extensions
  ;; first (for instance, a .adb file could be associated with a .ads
  ;; or a .ads.gp (gnatprep)).
  ;; This is why we can't use add-to-list here.

  (cond ((eq ada-language-version 'ada83)
         (set 'ada-keywords ada-83-keywords))
        ((eq ada-language-version 'ada95)
         (set 'ada-keywords ada-95-keywords)))

  (if ada-auto-case
      (ada-activate-keys-for-case)))



;;;--------------------------------------------------------
;;;                      auto-casing
;;;--------------------------------------------------------


(defun ada-create-case-exception (&optional word)
  "Defines WORD as an exception for the casing system. If WORD
is not given, then the current word in the buffer is used instead.
Every time the ada-mode will see the same word, the same casing will
be used.
The new words is added to the file `ada-case-exception-file'"
  (interactive)
  (let ((previous-syntax-table (syntax-table))
	(exception-list '()))
    (set-syntax-table ada-mode-symbol-syntax-table)
    (unless word
      (save-excursion
	(skip-syntax-backward "w")
	(set 'word (buffer-substring-no-properties
                  (point) (save-excursion (forward-word 1) (point))))))

    ;;  Reread the exceptions file, in case it was modified by some other,
    ;;  and to keep the end-of-line comments that may exist in it.
    (if (file-readable-p (expand-file-name ada-case-exception-file))
	(let ((buffer (current-buffer)))
	  (find-file (expand-file-name ada-case-exception-file))
	  (set-syntax-table ada-mode-symbol-syntax-table)
	  (widen)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (add-to-list 'exception-list
			 (list
			  (buffer-substring-no-properties
			   (point) (save-excursion (forward-word 1) (point)))
			  (buffer-substring-no-properties
			   (save-excursion (forward-word 1) (point))
			   (save-excursion (end-of-line) (point)))
			  t))
	    (forward-line 1))
	  (kill-buffer nil)
	  (set-buffer buffer)))
    
    ;;  If the word is already in the list, even with a different casing
    ;;  we simply want to replace it.
    (if (and (not (equal exception-list '()))
	     (assoc-ignore-case word exception-list))
	(setcar (assoc-ignore-case word exception-list)
		word)
      (add-to-list 'exception-list (list word "" t))
      )

    (if (and (not (equal ada-case-exception '()))
	     (assoc-ignore-case word ada-case-exception))
	(setcar (assoc-ignore-case word ada-case-exception)
		word)
      (add-to-list 'ada-case-exception (cons word t))
      )

    ;;  Save the list in the file
    (find-file (expand-file-name ada-case-exception-file))
    (erase-buffer)
    (mapcar '(lambda (x) (insert (car x) (nth 1 x) "\n"))
	    (sort exception-list
		  (lambda(a b) (string< (car a) (car b)))))
    (save-buffer)
    (kill-buffer nil)
    (set-syntax-table previous-syntax-table)
    ))
  
(defun ada-case-read-exceptions ()
  "Read the file `ada-case-exception-file' for the list of identifiers that
have special casing"
  (interactive)
  (set 'ada-case-exception '())
  (if (file-readable-p (expand-file-name ada-case-exception-file))
      (let ((buffer (current-buffer)))
	(find-file (expand-file-name ada-case-exception-file))
	(set-syntax-table ada-mode-symbol-syntax-table)
        (widen)
        (goto-char (point-min))
        (while (not (eobp))
          (add-to-list 'ada-case-exception
                       (cons
                        (buffer-substring-no-properties
                         (point) (save-excursion (forward-word 1) (point)))
                        t))
          (forward-line 1))
        (kill-buffer nil)
        (set-buffer buffer)
	)))

(defun ada-adjust-case-identifier ()
  "Adjust case of the previous identifier. The auto-casing is
done according to the value of `ada-case-identifier' and the
exceptions defined in `ada-case-exception'"

  (if (or (equal ada-case-exception '())
          (equal (char-after) ?_))
      (funcall ada-case-identifier -1)

    (progn
      (let ((end   (point))
            (start (save-excursion (skip-syntax-backward "w")
				   (point)))
            match)
        ;;  If we have an exception, replace the word by the correct casing
        (if (set 'match (assoc-ignore-case (buffer-substring start end)
                                           ada-case-exception))

            (progn
              (delete-region start end)
              (insert (car match)))

          ;;  Else simply recase the word
          (funcall ada-case-identifier -1))))))

(defun ada-after-keyword-p ()
  ;; returns t if cursor is after a keyword.
  (save-excursion
    (forward-word -1)
    (and (not (and (char-before) (= (char-before) ?_)));; unless we have a _
         (looking-at (concat ada-keywords "[^_]")))))

(defun ada-adjust-case (&optional force-identifier)
  "Adjust the case of the word before the just typed character.
Respect options `ada-case-keyword', `ada-case-identifier', and
`ada-case-attribute'.
If FORCE-IDENTIFIER is non-nil then also adjust keyword as identifier." ; (MH)
  (let ((previous-syntax-table (syntax-table)))
    (set-syntax-table ada-mode-symbol-syntax-table)

    (forward-char -1)

    ;;  Do nothing in some cases
    (if (and (> (point) 1)

	     ;;  or if at the end of a character constant
	     (not (and (eq (char-after) ?')
		       (eq (char-before (1- (point))) ?')))

	     ;;  or if the previous character was not part of a word
	     (eq (char-syntax (char-before)) ?w)

	     ;;  if in a string or a comment
	     (not (ada-in-string-or-comment-p))
	     )
	
	(if (save-excursion
	      (forward-word -1)
	      (or (= (point) (point-min))
		  (backward-char 1))
	      (= (char-after) ?'))
	    (funcall ada-case-attribute -1)
	  (if (and
	       (not force-identifier) ; (MH)
	       (ada-after-keyword-p))
	      (funcall ada-case-keyword -1)
	    (ada-adjust-case-identifier))))
    (forward-char 1)
    (set-syntax-table previous-syntax-table)
    )
  )

(defun ada-adjust-case-interactive (arg)
  (interactive "P")
  (let ((lastk last-command-char))
    (cond ((or (eq lastk ?\n)
               (eq lastk ?\r))
           ;; horrible kludge
           (insert " ")
           (ada-adjust-case)
           ;; horrible dekludge
           (delete-backward-char 1)
           ;; some special keys and their bindings
           (cond
            ((eq lastk ?\n)
             (funcall ada-lfd-binding))
            ((eq lastk ?\r)
             (funcall ada-ret-binding))))
          ((eq lastk ?\C-i) (ada-tab))
          ((self-insert-command (prefix-numeric-value arg))))
    ;; if there is a keyword in front of the underscore
    ;; then it should be part of an identifier (MH)
    (if (eq lastk ?_)
        (ada-adjust-case t)
      (ada-adjust-case))))


(defun ada-activate-keys-for-case ()
  (interactive)
  ;; save original keybindings to allow swapping ret/lfd
  ;; when casing is activated
  ;; the 'or ...' is there to be sure that the value will not
  ;; be changed again when Ada mode is called more than once (MH)
  (or ada-ret-binding
      (set 'ada-ret-binding (key-binding "\C-M")))
  (or ada-lfd-binding
      (set 'ada-lfd-binding (key-binding "\C-j")))
  ;; call case modifying function after certain keys.
  (mapcar (function (lambda(key) (define-key
                                   ada-mode-map
                                   (char-to-string key)
                                   'ada-adjust-case-interactive)))
          '( ?` ?~ ?! ?@ ?# ?$ ?% ?^ ?& ?* ?( ?)  ?- ?= ?+ ?[ ?{ ?] ?}
		?\\ ?| ?\; ?: ?' ?\" ?< ?, ?. ?> ?? ?/ ?\n 32 ?\r )))

;;
;; added by MH
;;
(defun ada-loose-case-word (&optional arg)
  "Capitalizes the first letter and the letters following `_' for the following
word. Ignores Arg (its there to conform to capitalize-word parameters)
Does not change other letters"
  (interactive)
  (let ((pos (point))
        (first t))
    (skip-syntax-backward "w")
    (while (or first
               (search-forward "_" pos t))
      (and first
           (set 'first nil))
      (insert-char (upcase (following-char)) 1)
      (delete-char 1))
    (goto-char pos)))

(defun ada-capitalize-word (&optional arg)
  "Capitalizes the first letter and the letters following '_', and
lower case other letters"
  (interactive)
  (let ((pos (point)))
    (skip-syntax-backward "w")
    (modify-syntax-entry ?_ "_")
    (capitalize-region (point) pos)
    (goto-char pos)
    (modify-syntax-entry ?_ "w")))

;;
;; added by MH
;; modified by JSH to handle attributes
;;
(defun ada-adjust-case-region (from to)
  "Adjusts the case of all words in the region.
Attention: This function might take very long for big regions !"
  (interactive "*r")
  (let ((begin nil)
        (end nil)
        (keywordp nil)
        (attribp nil)
        (previous-syntax-table (syntax-table)))
    (message "Adjusting case ...")
    (unwind-protect
        (save-excursion
          (set-syntax-table ada-mode-symbol-syntax-table)
          (goto-char to)
          ;;
          ;; loop: look for all identifiers, keywords, and attributes
          ;;
          (while (re-search-backward "\\<\\(\\sw+\\)\\>" from t)
            (set 'end (match-end 1))
            (set 'attribp
                 (and (> (point) from)
                      (save-excursion
                        (forward-char -1)
                        (set 'attribp (looking-at "'.[^']")))))
            (or
             ;; do nothing if it is a string or comment
             (ada-in-string-or-comment-p)
             (progn
               ;;
               ;; get the identifier or keyword or attribute
               ;;
               (set 'begin (point))
               (set 'keywordp (looking-at ada-keywords))
               (goto-char end)
               ;;
               ;; casing according to user-option
               ;;
               (if attribp
                   (funcall ada-case-attribute -1)
                 (if keywordp
                     (funcall ada-case-keyword -1)
                   (ada-adjust-case-identifier)))
               (goto-char begin))))
          (message "Adjusting case ... Done"))
      (set-syntax-table previous-syntax-table))))


;;
;; added by MH
;;
(defun ada-adjust-case-buffer ()
  "Adjusts the case of all words in the whole buffer.
ATTENTION: This function might take very long for big buffers !"
  (interactive "*")
  (ada-adjust-case-region (point-min) (point-max)))


;;;------------------------;;;
;;; Format Parameter Lists ;;;
;;;------------------------;;;
(defun ada-format-paramlist ()
  "Reformats a parameter list.
ATTENTION:  1) Comments inside the list are killed !
            2) If the syntax is not correct (especially, if there are
               semicolons missing), it can get totally confused !
In such a case, use `undo', correct the syntax and try again."

  (interactive)
  (let ((begin nil)
        (end nil)
        (delend nil)
        (paramlist nil)
        (previous-syntax-table (syntax-table)))
    (unwind-protect
        (progn
          (set-syntax-table ada-mode-symbol-syntax-table)

          ;; check if really inside parameter list
          (or (ada-in-paramlist-p)
              (error "not in parameter list"))
          ;;
          ;; find start of current parameter-list
          ;;
          (ada-search-ignore-string-comment
           (concat ada-subprog-start-re "\\|\\<body\\>" ) t nil)
          (down-list 1)
          (backward-char 1)
          (set 'begin (point))

          ;;
          ;; find end of parameter-list
          ;;
          (forward-sexp 1)
          (set 'delend (point))
          (delete-char -1)

          ;;
          ;; find end of last parameter-declaration
          ;;
          (forward-comment -1000)
          (set 'end (point))

          ;;
          ;; build a list of all elements of the parameter-list
          ;;
          (set 'paramlist (ada-scan-paramlist (1+ begin) end))

          ;;
          ;; delete the original parameter-list
          ;;
          (delete-region begin (1- delend))

          ;;
          ;; insert the new parameter-list
          ;;
          (goto-char begin)
          (ada-insert-paramlist paramlist))

      ;;
      ;; restore syntax-table
      ;;
      (set-syntax-table previous-syntax-table)
      )))


(defun ada-scan-paramlist (begin end)
  ;; Scans a parameter-list  between BEGIN and END and returns a list
  ;; of its contents.
  ;; The list has the following format:
  ;;
  ;;   Name of Param  in? out? access?  Name of Type   Default-Exp or nil
  ;;
  ;; ( ('Name_Param_1' t   nil    t      Type_Param_1   ':= expression')
  ;;   ('Name_Param_2' nil nil    t      Type_Param_2    nil) )

  (let ((paramlist (list))
        (param (list))
        (notend t)
        (apos nil)
        (epos nil)
        (semipos nil)
        (match-cons nil))

    (goto-char begin)
    ;;
    ;; loop until end of last parameter
    ;;
    (while notend

      ;;
      ;; find first character of parameter-declaration
      ;;
      (ada-goto-next-non-ws)
      (set 'apos (point))

      ;;
      ;; find last character of parameter-declaration
      ;;
      (if (set 'match-cons
               (ada-search-ignore-string-comment "[ \t\n]*;" nil end t))
          (progn
            (set 'epos (car match-cons))
            (set 'semipos (cdr match-cons)))
        (set 'epos end))

      ;;
      ;; read name(s) of parameter(s)
      ;;
      (goto-char apos)
      (looking-at "\\(\\(\\sw\\|[_, \t\n]\\)*\\(\\sw\\|_\\)\\)[ \t\n]*:[^=]")

      (set 'param (list (match-string 1)))
      (ada-search-ignore-string-comment ":" nil epos t 'search-forward)

      ;;
      ;; look for 'in'
      ;;
      (set 'apos (point))
      (set 'param
           (append param
                   (list
                    (consp
                     (ada-search-ignore-string-comment
                      "in" nil epos t 'word-search-forward)))))

      ;;
      ;; look for 'out'
      ;;
      (goto-char apos)
      (set 'param
           (append param
                   (list
                    (consp
                     (ada-search-ignore-string-comment
                      "out" nil epos t 'word-search-forward)))))

      ;;
      ;; look for 'access'
      ;;
      (goto-char apos)
      (set 'param
           (append param
                   (list
                    (consp
                     (ada-search-ignore-string-comment
                      "access" nil epos t 'word-search-forward)))))

      ;;
      ;; skip 'in'/'out'/'access'
      ;;
      (goto-char apos)
      (ada-goto-next-non-ws)
      (while (looking-at "\\<\\(in\\|out\\|access\\)\\>")
        (forward-word 1)
        (ada-goto-next-non-ws))

      ;;
      ;; read type of parameter
      ;;
      (looking-at "\\<\\(\\sw\\|[_.']\\)+\\>")
      (set 'param
           (append param
                   (list (match-string 0))))

      ;;
      ;; read default-expression, if there is one
      ;;
      (goto-char (set 'apos (match-end 0)))
      (set 'param
           (append param
                   (list
                    (if (set 'match-cons
                             (ada-search-ignore-string-comment
                              ":=" nil epos t 'search-forward))
                        (buffer-substring (car match-cons) epos)
                      nil))))
      ;;
      ;; add this parameter-declaration to the list
      ;;
      (set 'paramlist (append paramlist (list param)))

      ;;
      ;; check if it was the last parameter
      ;;
      (if (eq epos end)
          (set 'notend nil)
        (goto-char semipos))

      )                                 ; end of loop

    (reverse paramlist)))


(defun ada-insert-paramlist (paramlist)
  ;; Inserts a formatted PARAMLIST in the buffer.
  ;; See doc of `ada-scan-paramlist' for the format.
  (let ((i (length paramlist))
        (parlen 0)
        (typlen 0)
        (temp 0)
        (inp nil)
        (outp nil)
        (accessp nil)
        (column nil)
        (firstcol nil))

    ;;
    ;; loop until last parameter
    ;;
    (while (not (zerop i))
      (set 'i (1- i))

      ;;
      ;; get max length of parameter-name
      ;;
      (set 'parlen
           (if (<= parlen (set 'temp
                               (length (nth 0 (nth i paramlist)))))
               temp
             parlen))

      ;;
      ;; get max length of type-name
      ;;
      (set 'typlen
           (if (<= typlen (set 'temp
                               (length (nth 4 (nth i paramlist)))))
               temp
             typlen))

      ;;
      ;; is there any 'in' ?
      ;;
      (set 'inp
           (or inp
               (nth 1 (nth i paramlist))))

      ;;
      ;; is there any 'out' ?
      ;;
      (set 'outp
           (or outp
               (nth 2 (nth i paramlist))))

      ;;
      ;; is there any 'access' ?
      ;;
      (set 'accessp
           (or accessp
               (nth 3 (nth i paramlist))))) ; end of loop

    ;;
    ;; does paramlist already start on a separate line ?
    ;;
    (if (save-excursion
          (re-search-backward "^.\\|[^ \t]" nil t)
          (looking-at "^."))
        ;; yes => re-indent it
        (progn
          (ada-indent-current)
          (save-excursion
            (if (looking-at "\\(is\\|return\\)")
                (replace-match " \\1"))))
      ;;
      ;; no => insert it where we are after removing any whitespace
      ;;
      (fixup-whitespace)
      (save-excursion
        (cond
         ((looking-at "[ \t]*\\(\n\\|;\\)")
          (replace-match "\\1"))
         ((looking-at "[ \t]*\\(is\\|return\\)")
          (replace-match " \\1"))))
      (insert " "))

    (insert "(")
    (ada-indent-current)

    (set 'firstcol (current-column))
    (set 'i (length paramlist))

    ;;
    ;; loop until last parameter
    ;;
    (while (not (zerop i))
      (set 'i (1- i))
      (set 'column firstcol)

      ;;
      ;; insert parameter-name, space and colon
      ;;
      (insert (nth 0 (nth i paramlist)))
      (indent-to (+ column parlen 1))
      (insert ": ")
      (set 'column (current-column))

      ;;
      ;; insert 'in' or space
      ;;
      (if (nth 1 (nth i paramlist))
          (insert "in ")
        (if (and
             (or inp
                 accessp)
             (not (nth 3 (nth i paramlist))))
            (insert "   ")))

      ;;
      ;; insert 'out' or space
      ;;
      (if (nth 2 (nth i paramlist))
          (insert "out ")
        (if (and
             (or outp
                 accessp)
             (not (nth 3 (nth i paramlist))))
            (insert "    ")))

      ;;
      ;; insert 'access'
      ;;
      (if (nth 3 (nth i paramlist))
          (insert "access "))

      (set 'column (current-column))

      ;;
      ;; insert type-name and, if necessary, space and default-expression
      ;;
      (insert (nth 4 (nth i paramlist)))
      (if (nth 5 (nth i paramlist))
          (progn
            (indent-to (+ column typlen 1))
            (insert (nth 5 (nth i paramlist)))))

      ;;
      ;; check if it was the last parameter
      ;;
      (if (zerop i)
          (insert ")")
        ;; no => insert ';' and newline and indent
        (insert ";")
        (newline)
        (indent-to firstcol))
      )                                 ; end of loop

    ;;
    ;; if anything follows, except semicolon, newline, is or return
    ;; put it in a new line and indent it
    ;;
    (unless (looking-at "[ \t]*\\(;\\|\n\\|is\\|return\\)")
      (ada-indent-newline-indent))

    ))


;;;----------------------------;;;
;;; Move To Matching Start/End ;;;
;;;----------------------------;;;
(defun ada-move-to-start ()
  "Moves point to the matching start of the current Ada structure."
  (interactive)
  (let ((pos (point))
        (previous-syntax-table (syntax-table)))
    (unwind-protect
        (progn
          (set-syntax-table ada-mode-symbol-syntax-table)

          (message "searching for block start ...")
          (save-excursion
            ;;
            ;; do nothing if in string or comment or not on 'end ...;'
            ;;            or if an error occurs during processing
            ;;
            (or
             (ada-in-string-or-comment-p)
             (and (progn
                    (or (looking-at "[ \t]*\\<end\\>")
                        (backward-word 1))
                    (or (looking-at "[ \t]*\\<end\\>")
                        (backward-word 1))
                    (or (looking-at "[ \t]*\\<end\\>")
                        (error "not on end ...;")))
                  (ada-goto-matching-start 1)
                  (set 'pos (point))

                  ;;
                  ;; on 'begin' => go on, according to user option
                  ;;
                  ada-move-to-declaration
                  (looking-at "\\<begin\\>")
                  (ada-goto-matching-decl-start)
                  (set 'pos (point))))

            )                           ; end of save-excursion

          ;; now really move to the found position
          (goto-char pos)
          (message "searching for block start ... done"))

      ;;
      ;; restore syntax-table
      ;;
      (set-syntax-table previous-syntax-table))))

(defun ada-move-to-end ()
  "Moves point to the matching end of the current block around point.
Moves to 'begin' if in a declarative part."
  (interactive)
  (let ((pos (point))
        (previous-syntax-table (syntax-table)))
    (unwind-protect
        (progn
          (set-syntax-table ada-mode-symbol-syntax-table)

          (message "searching for block end ...")
          (save-excursion

            (forward-char 1)
            (cond
             ;; directly on 'begin'
             ((save-excursion
                (ada-goto-previous-word)
                (looking-at "\\<begin\\>"))
              (ada-goto-matching-end 1))
             ;; on first line of defun declaration
             ((save-excursion
                (and (ada-goto-stmt-start)
                     (looking-at "\\<function\\>\\|\\<procedure\\>" )))
              (ada-search-ignore-string-comment "begin" nil nil nil 'word-search-forward))
             ;; on first line of task declaration
             ((save-excursion
                (and (ada-goto-stmt-start)
                     (looking-at "\\<task\\>" )
                     (forward-word 1)
                     (ada-goto-next-non-ws)
                     (looking-at "\\<body\\>")))
              (ada-search-ignore-string-comment "begin" nil nil nil 'word-search-forward))
             ;; accept block start
             ((save-excursion
                (and (ada-goto-stmt-start)
                     (looking-at "\\<accept\\>" )))
              (ada-goto-matching-end 0))
             ;; package start
             ((save-excursion
                (and (ada-goto-matching-decl-start t)
                     (looking-at "\\<package\\>")))
              (ada-goto-matching-end 1))
             ;; inside a 'begin' ... 'end' block
             ((save-excursion
                (ada-goto-matching-decl-start t))
              (ada-search-ignore-string-comment "begin" nil nil nil 'word-search-forward))
             ;; (hopefully ;-) everything else
             (t
              (ada-goto-matching-end 1)))
            (set 'pos (point))

            )                           ; end of save-excursion

          ;; now really move to the found position
          (goto-char pos)
          (message "searching for block end ... done"))

      ;;
      ;; restore syntax-table
      ;;
      (set-syntax-table previous-syntax-table))))


;;;-----------------------------;;;
;;;  Functions For Indentation  ;;;
;;;-----------------------------;;;

;; ---- main functions for indentation
(defun ada-indent-region (beg end)
  "Indents the region using `ada-indent-current' on each line."
  (interactive "*r")
  (goto-char beg)
  (let ((block-done 0)
        (lines-remaining (count-lines beg end))
        (msg (format "indenting %4d lines %%4d lines remaining ..."
                     (count-lines beg end)))
        (endmark (copy-marker end)))
    ;; catch errors while indenting
    (while (< (point) endmark)
      (if (> block-done 39)
          (progn (message msg lines-remaining)
                 (set 'block-done 0)))
      (if (looking-at "^$") nil
        (ada-indent-current))
      (forward-line 1)
      (set 'block-done (1+ block-done))
      (set 'lines-remaining (1- lines-remaining)))
    (message "indenting ... done")))

(defun ada-indent-newline-indent ()
  "Indents the current line, inserts a newline and then indents the new line."
  (interactive "*")
  (ada-indent-current)
  (newline)
  (ada-indent-current))

(defun ada-indent-newline-indent-conditional ()
  "If `ada-indent-after-return' is non-nil, then indents the current line,
insert a newline and indents the newline.
If `ada-indent-after-return' is nil then inserts a newline and indents the
newline.
This function is intended to be bound to the \C-m and \C-j keys"
  (interactive "*")
  (if ada-indent-after-return (ada-indent-current))
  (newline)
  (ada-indent-current))

(defun ada-justified-indent-current ()
  "Indent the current line and explains how it was chosen"
  (interactive)

  (let ((cur-indent (ada-indent-current)))

    (message nil)
    (if (equal (cdr cur-indent) '(0))
	(message "same indentation")
      (message (mapconcat (lambda(x)
			    (cond
			     ((symbolp x)
			      (symbol-name x))
			     ((numberp x)
			      (number-to-string x))
			     ((listp x)
			      (concat "- " (symbol-name (cadr x))))
			     ))
			  (cdr cur-indent)
			  " + ")))
    (save-excursion
      (goto-char (car cur-indent))
      (sit-for 1))))

(defun ada-indent-current ()
  "Indents current line as Ada code.
Each of these steps returns a two element list:
  - position of reference in the buffer
  - offset to indent from this position (can also be a symbol or a list
    that are evaluated"

  (interactive)
  (let ((previous-syntax-table (syntax-table))
	(orgpoint (point-marker))
	cur-indent tmp-indent
	prev-indent)
    
    (set-syntax-table ada-mode-symbol-syntax-table)
    
    ;;  This need to be done here so that the advice is not always activated
    ;;  (this might interact badly with other modes)
    (if ada-xemacs
        (ad-activate 'parse-partial-sexp t))

    (unwind-protect
        (progn

	  (save-excursion
	    (set 'cur-indent
		 ;; Not First line in the buffer ?
		 
		 (if (save-excursion (zerop (forward-line -1)))
		     (progn
		       (back-to-indentation)
		       (ada-get-current-indent))
		   
		   ;; first line in the buffer
		   (list (point-min) 0))))
	    
	  ;; Evaluate the list to get the column to indent to
	  ;; prev-indent contains the column to indent to
	  (set 'prev-indent (save-excursion (goto-char (car cur-indent)) (current-column)))
	  (set 'tmp-indent (cdr cur-indent))
	  (while (not (null tmp-indent))
	    (cond
	     ((numberp (car tmp-indent))
	      (set 'prev-indent (+ prev-indent (car tmp-indent))))
	     (t
	      (set 'prev-indent (+ prev-indent (eval (car tmp-indent)))))
	     )
	    (set 'tmp-indent (cdr tmp-indent)))
	  
	  ;; only reindent if indentation is different then the current
	  (if (= (save-excursion (back-to-indentation) (current-column)) prev-indent)
	      nil
	    (beginning-of-line)
	    (delete-horizontal-space)
	    (indent-to prev-indent))
	  ;;
	  ;; restore position of point
	  ;;
	  (goto-char orgpoint)
	  (if (< (current-column) (current-indentation))
	      (back-to-indentation))))
    ;;
    ;; restore syntax-table
    ;;
    (if ada-xemacs
	(ad-deactivate 'parse-partial-sexp))
    (set-syntax-table previous-syntax-table)
    cur-indent
    ))


(defun ada-get-current-indent ()
  "Returns the column number to indent the current line to.

Returns a list of two elements (same as prev-indent):
  - Position in the cursor that is used as a reference (its columns
    is used)
  - variable used to calculate the indentation from position"

  (let (column
	pos
	match-cons
	(orgpoint (save-excursion
		    (beginning-of-line)
		    (forward-comment -10000)
		    (forward-line 1)
		    (point))))
    (cond
     ;;
     ;; preprocessor line (gnatprep)
     ;;
     ((and (equal ada-which-compiler 'gnat)
           (looking-at "#[ \t]*\\(if\\|else\\|elsif\\|end[ \t]*if\\)"))
      (list (save-excursion (beginning-of-line) (point)) 0))

     ;;
     ;; in open parenthesis, but not in parameter-list
     ;;
     ((and
       ada-indent-to-open-paren
       (not (ada-in-paramlist-p))
       (set 'column (ada-in-open-paren-p)))
      ;; check if we have something like this  (Table_Component_Type =>
      ;;                                          Source_File_Record)
      (save-excursion
        (if (and (skip-chars-backward " \t")
                 (= (char-before) ?\n)
                 (not (forward-comment -10000))
                 (= (char-before) ?>))
	    (list column 'ada-broken-indent);; ??? Could use a different variable
	  (list column 0))))

     ;;
     ;; end
     ;;
     ((looking-at "\\<end\\>")
      (let ((label 0))
        (save-excursion
          (ada-goto-matching-start 1)

          ;;
          ;; found 'loop' => skip back to 'while' or 'for'
          ;;                 if 'loop' is not on a separate line
          ;;
          (if (save-excursion
                (beginning-of-line)
                (looking-at ".+\\<loop\\>"))
              (if (save-excursion
                    (and
                     (set 'match-cons
                          (ada-search-ignore-string-comment ada-loop-start-re t))
                     (not (looking-at "\\<loop\\>"))))
                  (progn
                    (goto-char (car match-cons))
                    (save-excursion
                      (beginning-of-line)
                      (if (looking-at ada-named-block-re)
                          (set 'label (- ada-label-indent)))))))

	  (list (+ (save-excursion (back-to-indentation) (point)) label) 0))))
     ;;
     ;; exception
     ;;
     ((looking-at "\\<exception\\>")
      (save-excursion
        (ada-goto-matching-start 1)
	(list (save-excursion (back-to-indentation) (point)) 0)))
     ;;
     ;; when
     ;;
     ((looking-at "\\<when\\>")
      (save-excursion
        (ada-goto-matching-start 1)
	(list (save-excursion (back-to-indentation) (point)) 'ada-when-indent)))
     ;;
     ;; else
     ;;
     ((looking-at "\\<else\\>")
      (if (save-excursion  (ada-goto-previous-word)
			   (looking-at "\\<or\\>"))
	  (ada-indent-on-previous-lines nil orgpoint orgpoint)
        (save-excursion
          (ada-goto-matching-start 1 nil t)
	  (list (progn (back-to-indentation) (point)) 0))))
     ;;
     ;; elsif
     ;;
     ((looking-at "\\<elsif\\>")
      (save-excursion
        (ada-goto-matching-start 1 nil t)
	(list (progn (back-to-indentation) (point)) 0)))
     ;;
     ;; then
     ;;
     ((looking-at "\\<then\\>")
      (if (save-excursion (ada-goto-previous-word)
			  (looking-at "\\<and\\>"))
	  (ada-indent-on-previous-lines nil orgpoint orgpoint)
        (save-excursion
          ;;  Select has been added for the statement:  "select ... then abort"
          (ada-search-ignore-string-comment "\\<\\(elsif\\|if\\|select\\)\\>" t nil)
	  (list (progn (back-to-indentation) (point)) 'ada-stmt-end-indent))))
     ;;
     ;; loop
     ;;
     ((looking-at "\\<loop\\>")
      (set 'pos (point))
      (save-excursion
        (goto-char (match-end 0))
        (ada-goto-stmt-start)
        (if (looking-at "\\<\\(loop\\|if\\)\\>")
	    (ada-indent-on-previous-lines nil orgpoint orgpoint)
	  (unless (looking-at ada-loop-start-re)
	    (ada-search-ignore-string-comment ada-loop-start-re
					      nil pos))
	  (if (looking-at "\\<loop\\>")
	      (ada-indent-on-previous-lines nil orgpoint orgpoint)
	    (list (progn (back-to-indentation) (point)) 'ada-stmt-end-indent)))))
     ;;
     ;; begin
     ;;
     ((looking-at "\\<begin\\>")
      (save-excursion
        (if (ada-goto-matching-decl-start t)
	    (list (progn (back-to-indentation) (point)) 0)
	  (ada-indent-on-previous-lines nil orgpoint orgpoint))))
     ;;
     ;; is
     ;;
     ((looking-at "\\<is\\>")
      (if (and ada-indent-is-separate
	       (save-excursion
		 (goto-char (match-end 0))
		 (ada-goto-next-non-ws (save-excursion (end-of-line)
						       (point)))
		 (looking-at "\\<abstract\\>\\|\\<separate\\>")))
          (save-excursion
            (ada-goto-stmt-start)
	    (list (progn (back-to-indentation) (point)) 'ada-indent))
        (save-excursion
          (ada-goto-stmt-start)
	  (list (progn (back-to-indentation) (point)) 'ada-stmt-end-indent))))
     ;;
     ;; record
     ;;
     ((looking-at "\\<record\\>")
      (save-excursion
        (ada-search-ignore-string-comment
         "\\<\\(type\\|use\\)\\>" t nil)
        (if (looking-at "\\<use\\>")
            (ada-search-ignore-string-comment "for" t nil nil 'word-search-backward))
	(list (progn (back-to-indentation) (point)) 'ada-indent-record-rel-type)))
     ;;
     ;; 'or'      as statement-start
     ;; 'private' as statement-start
     ;;
     ((or (ada-looking-at-semi-or)
	  (ada-looking-at-semi-private))
      (save-excursion
        (ada-goto-matching-start 1)
	(list (progn (back-to-indentation) (point)) 0)))
     ;;
     ;; new/abstract/separate
     ;;
     ((looking-at "\\<\\(new\\|abstract\\|separate\\)\\>")
      (ada-indent-on-previous-lines nil orgpoint orgpoint))
     ;;
     ;; return
     ;;
     ((looking-at "\\<return\\>")
      (save-excursion
	(forward-comment -1000)
	(if (= (char-before) ?\))
	    (forward-sexp -1)
	  (forward-word -1))

	;; If there is a parameter list, and we have a function declaration
        (if (and (= (char-after) ?\()
                 (save-excursion
                   (backward-sexp 2)
                   (looking-at "\\<function\\>")))

	    ;; The indentation depends of the value of ada-indent-return
	    (if (<= ada-indent-return 0)
		(list (point) (- ada-indent-return))
	      (list (progn (backward-sexp 2) (point)) ada-indent-return))

	  ;; Else there is no parameter list, but we have a function
	  ;; Only do something special if the user want to indent relative
	  ;; to the "function" keyword
	  (if (and (> ada-indent-return 0)
		   (save-excursion (forward-word -1)
				   (looking-at "\\<function\\>")))
	      (list (progn (forward-word -1) (point)) ada-indent-return)

	    ;; Else...
	    (ada-indent-on-previous-lines nil orgpoint orgpoint)))))
     ;;
     ;; do
     ;;
     ((looking-at "\\<do\\>")
      (save-excursion
        (ada-goto-stmt-start)
	(list (progn (back-to-indentation) (point)) 'ada-stmt-end-indent)))
     ;;
     ;; package/function/procedure
     ;;
     ((and (looking-at "\\<\\(package\\|function\\|procedure\\)\\>")
           (save-excursion
             (forward-char 1)
             (ada-goto-stmt-start)
             (looking-at "\\<\\(package\\|function\\|procedure\\)\\>")))
      (save-excursion
        ;; look for 'generic'
        (if (and (ada-goto-matching-decl-start t)
                 (looking-at "generic"))
	    (list (progn (back-to-indentation) (point)) 0)
	  (ada-indent-on-previous-lines nil orgpoint orgpoint))))
     ;;
     ;; label
     ;;
     ((looking-at "\\<\\(\\sw\\|_\\)+[ \t\n]*:[^=]")
      (if (ada-in-decl-p)
	  (ada-indent-on-previous-lines nil orgpoint orgpoint)
	(set 'pos (ada-indent-on-previous-lines nil orgpoint orgpoint))
	(list (car pos)
	      (cadr pos)
	      'ada-label-indent)))
     ;;
     ;; identifier and other noindent-statements
     ;;
     ((looking-at "\\<\\(\\sw\\|_\\)+[ \t\n]*")
      (ada-indent-on-previous-lines nil orgpoint orgpoint))
     ;;
     ;; beginning of a parameter list
     ;;
     ((and (not (eobp)) (= (char-after) ?\())
      (ada-indent-on-previous-lines nil orgpoint orgpoint))
     ;;
     ;; end of a parameter list
     ;;
     ((and (not (eobp)) (= (char-after) ?\)))
      (save-excursion
        (forward-char 1)
        (backward-sexp 1)
	(list (point) 0)))
     ;;
     ;; comment
     ;;
     ((looking-at "--")
      (if ada-indent-comment-as-code
	  ;; If previous line is a comment, indent likewise
	  (save-excursion
	    (forward-line -1)
	    (beginning-of-line)
	    (if (looking-at "[ \t]*--")
		(list (progn (back-to-indentation) (point)) 0)
	      (ada-indent-on-previous-lines nil orgpoint orgpoint)))
	(list (save-excursion (back-to-indentation) (point)) 0)))
     ;;
     ;; unknown syntax - maybe this should signal an error ?
     ;;
     (t
      (ada-indent-on-previous-lines nil orgpoint orgpoint)))))

(defun ada-indent-on-previous-lines (&optional nomove orgpoint initial-pos)
  "Calculate the indentation of the current line, based on the previous lines
in the buffer. This function does not pay any attention to the current line,
since this is the role of the second step in the indentation
 (see ada-get-current-indent).

Returns a two element list:
    - position of reference in the buffer
    - offset to indent from this position (can also be a symbol or a list
      that are evaluated)
Moves point to the beginning of the current statement, if NOMOVE is nil."
  (if initial-pos
      (goto-char initial-pos))
  (let ((oldpoint (point))
        result)
    ;;
    ;; Is inside a parameter-list ?
    ;;
    (if (ada-in-paramlist-p)
        (set 'result (ada-get-indent-paramlist orgpoint))

      ;;
      ;; move to beginning of current statement
      ;;
      (unless nomove
        (ada-goto-stmt-start))

      (unless result
        (progn
          ;;
          ;; no beginning found => don't change indentation
          ;;
          (if (and (eq oldpoint (point))
                   (not nomove))
              (set 'result (ada-get-indent-nochange orgpoint))

            (cond
             ;;
             ((and
               ada-indent-to-open-paren
               (ada-in-open-paren-p))
              (set 'result (ada-get-indent-open-paren orgpoint)))
             ;;
             ((looking-at "end\\>")
              (set 'result (ada-get-indent-end orgpoint)))
             ;;
             ((looking-at ada-loop-start-re)
              (set 'result (ada-get-indent-loop orgpoint)))
             ;;
             ((looking-at ada-subprog-start-re)
              (set 'result (ada-get-indent-subprog orgpoint)))
             ;;
             ((looking-at ada-block-start-re)
              (set 'result (ada-get-indent-block-start orgpoint)))
             ;;
             ((looking-at "\\(sub\\)?type\\>")
              (set 'result (ada-get-indent-type orgpoint)))
	     ;;
             ((looking-at "\\(els\\)?if\\>")
              (set 'result (ada-get-indent-if orgpoint)))
             ;;
             ((looking-at "case\\>")
              (set 'result (ada-get-indent-case orgpoint)))
             ;;
             ((looking-at "when\\>")
              (set 'result (ada-get-indent-when orgpoint)))
             ;;
             ((looking-at "\\(\\sw\\|_\\)+[ \t\n]*:[^=]")
              (set 'result (ada-get-indent-label orgpoint)))
             ;;
             ((looking-at "separate\\>")
              (set 'result (ada-get-indent-nochange orgpoint)))
             (t
              (set 'result (ada-get-indent-noindent orgpoint))))))))

    result))


;; ---- functions to return indentation for special cases

(defun ada-get-indent-open-paren (orgpoint)
  "Returns the two element list for the indentation, when point is
behind an open parenthesis not yet closed"
  (list (ada-in-open-paren-p) 0))


(defun ada-get-indent-nochange (orgpoint)
  "Returns the two element list for the indentation of the current line"
  (save-excursion
    (forward-line -1)
    (list (progn (back-to-indentation) (point)) 0)))


(defun ada-get-indent-paramlist (orgpoint)
  "Returns the classical two position list for indentation for the new line
after ORGPOINT.
Assumes point to be inside a parameter list"
  (save-excursion
    (ada-search-ignore-string-comment "[^ \t\n]" t nil t)
    (cond
     ;;
     ;; in front of the first parameter
     ;;
     ((= (char-after) ?\()
      (goto-char (match-end 0))
      (list (point) 0))
     ;;
     ;; in front of another parameter
     ;;
     ((= (char-after) ?\;)
      (goto-char (cdr (ada-search-ignore-string-comment "(\\|;" t nil t)))
      (ada-goto-next-non-ws)
      (list (point) 0))
     ;;
     ;; inside a parameter declaration
     ;;
     (t
      (goto-char (cdr (ada-search-ignore-string-comment "(\\|;" t nil t)))
      (ada-goto-next-non-ws)
      (list (point) 'ada-broken-indent)))))


(defun ada-get-indent-end (orgpoint &optional do-not-check-start)
  ;; Returns the indentation (column #) for the new line after ORGPOINT.
  ;; Assumes point to be at the beginning of an end-statement.
  ;; Therefore it has to find the corresponding start. This can be a little
  ;; slow, if it has to search through big files with many nested blocks.
  ;; Signals an error if the corresponding block-start doesn't match.
  (let ((defun-name nil)
        (label 0)
        (indent nil))
    ;;
    ;; is the line already terminated by ';' ?
    ;;
    (if (save-excursion
          (ada-search-ignore-string-comment ";" nil orgpoint nil 'search-forward))
        ;;
        ;; yes, look what's following 'end'
        ;;
        (progn
          (forward-word 1)
          (ada-goto-next-non-ws)
          (cond
	   ((looking-at "\\<\\(loop\\|select\\|if\\|case\\)\\>")
	    (unless do-not-check-start
	      (save-excursion (ada-check-matching-start (match-string 0))))
	    (list (save-excursion (back-to-indentation) (point)) 0))
	    
           ;;
           ;; loop/select/if/case/record/select
           ;;
           ((looking-at "\\<record\\>")
            (save-excursion
              (ada-check-matching-start (match-string 0))
	      ;;  we are now looking at the matching "record" statement
	      (forward-word 1)
	      (ada-goto-stmt-start)
	      ;;  now on the matching type declaration, or use clause
	      (unless (looking-at "\\(for\\|type\\)\\>")
		(ada-search-ignore-string-comment "\\<type\\>" t))
	      (list (progn (back-to-indentation) (point)) 0)))
           ;;
           ;; a named block end
           ;;
           ((looking-at ada-ident-re)
	    (unless do-not-check-start
	      (progn
		(set 'defun-name (match-string 0))
		(save-excursion
		  (ada-goto-matching-start 0)
		  (ada-check-defun-name defun-name))))
	    (list (progn (back-to-indentation) (point)) 0))
           ;;
           ;; a block-end without name
           ;;
           ((= (char-after) ?\;)
	    (unless do-not-check-start
	      (save-excursion
		(ada-goto-matching-start 0)
		(if (looking-at "\\<begin\\>")
		    (progn
		      (set 'indent (list (point) 0))
		      (if (ada-goto-matching-decl-start t)
			  (list (progn (back-to-indentation) (point)) 0)
			indent))))
	      (list (progn (back-to-indentation) (point)) 0)))
           ;;
           ;; anything else - should maybe signal an error ?
           ;;
           (t
	    (list (save-excursion (back-to-indentation) (point)) 'ada-broken-indent))))

      (list (save-excursion (back-to-indentation) (point)) 'ada-broken-indent))))


(defun ada-get-indent-case (orgpoint)
  ;; Returns the indentation (column #) for the new line after ORGPOINT.
  ;; Assumes point to be at the beginning of a case-statement.
  (let ((match-cons nil)
        (opos (point)))
    (cond
     ;;
     ;; case..is..when..=>
     ;;
     ((save-excursion
        (set 'match-cons (and
                          ;; the `=>' must be after the keyword `is'.
                          (ada-search-ignore-string-comment
                           "is" nil orgpoint nil 'word-search-forward)
                          (ada-search-ignore-string-comment
                           "[ \t\n]+=>" nil orgpoint))))
      (save-excursion
        (goto-char (car match-cons))
        (unless (ada-search-ignore-string-comment "when" t opos)
          (error "missing 'when' between 'case' and '=>'"))
	(list (save-excursion (back-to-indentation) (point)) 'ada-indent)))
     ;;
     ;; case..is..when
     ;;
     ((save-excursion
        (set 'match-cons (ada-search-ignore-string-comment
                          "when" nil orgpoint nil 'word-search-forward)))
      (goto-char (cdr match-cons))
      (list (save-excursion (back-to-indentation) (point)) 'ada-broken-indent))
     ;;
     ;; case..is
     ;;
     ((save-excursion
        (set 'match-cons (ada-search-ignore-string-comment
                          "is" nil orgpoint nil 'word-search-forward)))
      (list (save-excursion (back-to-indentation) (point)) 'ada-when-indent))
     ;;
     ;; incomplete case
     ;;
     (t
      (list (save-excursion (back-to-indentation) (point)) 'ada-broken-indent)))))


(defun ada-get-indent-when (orgpoint)
  ;; Returns the indentation (column #) for the new line after ORGPOINT.
  ;; Assumes point to be at the beginning of an when-statement.
  (let ((cur-indent (save-excursion (back-to-indentation) (point))))
    (if (ada-search-ignore-string-comment
         "[ \t\n]*=>" nil orgpoint)
	(list cur-indent 'ada-indent)
      (list cur-indent 'ada-broken-indent))))


(defun ada-get-indent-if (orgpoint)
  ;; Returns the indentation (column #) for the new line after ORGPOINT.
  ;; Assumes point to be at the beginning of an if-statement.
  (let ((cur-indent (save-excursion (back-to-indentation) (point)))
        (match-cons nil))
    ;;
    ;; Move to the correct then (ignore all "and then")
    ;;
    (while (and (set 'match-cons (ada-search-ignore-string-comment
                                  "\\<\\(then\\|and[ \t]*then\\)\\>"
                                  nil orgpoint))
                (= (char-after (car match-cons)) ?a)))
    ;; If "then" was found (we are looking at it)
    (if match-cons
        (progn
          ;;
          ;; 'then' first in separate line ?
          ;; => indent according to 'then',
	  ;; => else indent according to 'if'
          ;;
          (if (save-excursion
                (back-to-indentation)
                (looking-at "\\<then\\>"))
              (set 'cur-indent (save-excursion (back-to-indentation) (point))))
	  ;; skip 'then'
          (forward-word 1)
	  (list cur-indent 'ada-indent))

      (list cur-indent 'ada-broken-indent))))


(defun ada-get-indent-block-start (orgpoint)
  ;; Returns the indentation (column #) for the new line after
  ;; ORGPOINT.  Assumes point to be at the beginning of a block start
  ;; keyword.
  (let ((pos nil))
    (cond
     ((save-excursion
        (forward-word 1)
        (set 'pos (ada-goto-next-non-ws orgpoint)))
      (goto-char pos)
      (save-excursion
        (ada-indent-on-previous-lines t orgpoint)))
     ;;
     ;; nothing follows the block-start
     ;;
     (t
      (list (save-excursion (back-to-indentation) (point)) 'ada-indent)))))


(defun ada-get-indent-subprog (orgpoint)
  ;; Returns the indentation (column #) for the new line after ORGPOINT.
  ;; Assumes point to be at the beginning of a subprog-/package-declaration.
  (let ((match-cons nil)
        (cur-indent (save-excursion (back-to-indentation) (point)))
        (foundis nil))
    ;;
    ;; is there an 'is' in front of point ?
    ;;
    (if (save-excursion
          (set 'match-cons
               (ada-search-ignore-string-comment
                "\\<\\(is\\|do\\)\\>" nil orgpoint)))
        ;;
        ;; yes, then skip to its end
        ;;
        (progn
          (set 'foundis t)
          (goto-char (cdr match-cons)))
      ;;
      ;; no, then goto next non-ws, if there is one in front of point
      ;;
      (progn
        (unless (ada-goto-next-non-ws orgpoint)
          (goto-char orgpoint))))

    (cond
     ;;
     ;; nothing follows 'is'
     ;;
     ((and
       foundis
       (save-excursion
         (not (ada-search-ignore-string-comment
               "[^ \t\n]" nil orgpoint t))))
      (list cur-indent 'ada-indent))
     ;;
     ;; is abstract/separate/new ...
     ;;
     ((and
       foundis
       (save-excursion
         (set 'match-cons
              (ada-search-ignore-string-comment
               "\\<\\(separate\\|new\\|abstract\\)\\>"
               nil orgpoint))))
      (goto-char (car match-cons))
      (ada-search-ignore-string-comment ada-subprog-start-re t)
      (ada-get-indent-noindent orgpoint))
     ;;
     ;; something follows 'is'
     ;;
     ((and
       foundis
       (save-excursion (set 'match-cons (ada-goto-next-non-ws orgpoint)))
       (goto-char match-cons)
       (ada-indent-on-previous-lines t orgpoint)))
     ;;
     ;; no 'is' but ';'
     ;;
     ((save-excursion
        (ada-search-ignore-string-comment ";" nil orgpoint nil 'search-forward))
      (list cur-indent 0))
     ;;
     ;; no 'is' or ';'
     ;;
     (t
      (list cur-indent 'ada-broken-indent)))))


(defun ada-get-indent-noindent (orgpoint)
  ;; Returns the indentation (column #) for the new line after ORGPOINT.
  ;; Assumes point to be at the beginning of a 'noindent statement'.
  (let ((label 0))
    (save-excursion
      (beginning-of-line)

      (cond

       ;;  This one is called when indenting a line preceded by a multiline
       ;;  subprogram declaration (in that case, we are at this point inside
       ;;  the parameter declaration list)
       ((ada-in-paramlist-p)
        (ada-previous-procedure)
	(list (save-excursion (back-to-indentation) (point)) 0))

       ;;  This one is called when indenting the second line of a multiline
       ;;  declaration section, in a declare block or a record declaration
       ((looking-at "[ \t]*\\(\\sw\\|_\\)*[ \t]*,[ \t]*$")
	(list (save-excursion (back-to-indentation) (point))
	      'ada-broken-decl-indent))

       ;;  This one is called in every over case when indenting a line at the
       ;;  top level
       (t
        (if (looking-at ada-named-block-re)
            (set 'label (- ada-label-indent))

          ;;  "with private" or "null record" cases
          (if (or (and (re-search-forward "\\<private\\>" orgpoint t)
                       (save-excursion (forward-char -7);; skip back "private"
                                       (ada-goto-previous-word)
                                       (looking-at "with")))
		  (and (re-search-forward "\\<record\\>" orgpoint t)
		       (save-excursion (forward-char -6);; skip back "record"
				       (ada-goto-previous-word)
				       (looking-at "null"))))
              (progn
                (re-search-backward "\\<\\(type\\|subtype\\)\\>" nil t)
		(list (save-excursion (back-to-indentation) (point)) 0))))
        (if (save-excursion
              (ada-search-ignore-string-comment ";" nil orgpoint nil 'search-forward))
	    (list (+ (save-excursion (back-to-indentation) (point)) label) 0)
	  (list (+ (save-excursion (back-to-indentation) (point)) label)
		'ada-broken-indent)))))))

(defun ada-get-indent-label (orgpoint)
  ;; Returns the indentation (column #) for the new line after ORGPOINT.
  ;; Assumes point to be at the beginning of a label or variable declaration.
  ;; Checks the context to decide if it's a label or a variable declaration.
  ;; This check might be a bit slow.
  (let ((match-cons nil)
        (cur-indent (save-excursion (back-to-indentation) (point))))
    (ada-search-ignore-string-comment ":" nil)
    (cond
     ;; loop label
     ((save-excursion
        (set 'match-cons (ada-search-ignore-string-comment ada-loop-start-re nil orgpoint)))
      (goto-char (car match-cons))
      (ada-get-indent-loop orgpoint))

     ;; declare label
     ((save-excursion
        (set 'match-cons (ada-search-ignore-string-comment "\\<declare\\|begin\\>" nil orgpoint)))
      (goto-char (car match-cons))
      (list (save-excursion (back-to-indentation) (point)) 'ada-indent))

     ;; variable declaration
     ((ada-in-decl-p)
      (if (save-excursion
            (ada-search-ignore-string-comment ";" nil orgpoint))
          (list cur-indent 0)
	(list cur-indent 'ada-broken-indent)))

     ;; nothing follows colon
     (t
      (list cur-indent '(- ada-label-indent))))))

(defun ada-get-indent-loop (orgpoint)
  "Returns the two-element list for indentation.
Assumes point to be at the beginning of a loop statement
or a for ... use statement."
  (let ((match-cons nil)
        (pos (point))

	;; If looking at a named block, skip the label
        (label (save-excursion
                 (beginning-of-line)
                 (if (looking-at ada-named-block-re)
                     (- ada-label-indent)
                   0))))

    (cond

     ;;
     ;; statement complete
     ;;
     ((save-excursion
        (ada-search-ignore-string-comment ";" nil orgpoint nil 'search-forward))
      (list (+ (save-excursion (back-to-indentation) (point)) label) 0))
     ;;
     ;; simple loop
     ;;
     ((looking-at "loop\\>")
      (set 'pos (ada-get-indent-block-start orgpoint))
      (if (equal label 0)
	  pos
	(list (+ (car pos) label) (cdr pos))))

     ;;
     ;; 'for'- loop (or also a for ... use statement)
     ;;
     ((looking-at "for\\>")
      (cond
       ;;
       ;; for ... use
       ;;
       ((save-excursion
          (and
           (goto-char (match-end 0))
           (ada-goto-next-non-ws orgpoint)
           (forward-word 1)
           (if (= (char-after) ?') (forward-word 1) t)
           (ada-goto-next-non-ws orgpoint)
           (looking-at "\\<use\\>")
           ;;
           ;; check if there is a 'record' before point
           ;;
           (progn
             (set 'match-cons (ada-search-ignore-string-comment
                               "record" nil orgpoint nil 'word-search-forward))
             t)))
        (if match-cons
            (goto-char (car match-cons)))
	(list (save-excursion (back-to-indentation) (point)) 'ada-indent))
       ;;
       ;; for..loop
       ;;
       ((save-excursion
          (set 'match-cons (ada-search-ignore-string-comment
                            "loop" nil orgpoint nil 'word-search-forward)))
        (goto-char (car match-cons))
        ;;
        ;; indent according to 'loop', if it's first in the line;
        ;; otherwise to 'for'
        ;;
        (unless (save-excursion
                  (back-to-indentation)
                  (looking-at "\\<loop\\>"))
          (goto-char pos))
	(list (+ (save-excursion (back-to-indentation) (point)) label) 'ada-indent))
       ;;
       ;; for-statement is broken
       ;;
       (t
	(list (+ (save-excursion (back-to-indentation) (point)) label) 'ada-broken-indent))))

     ;;
     ;; 'while'-loop
     ;;
     ((looking-at "while\\>")
      ;;
      ;; while..loop ?
      ;;
      (if (save-excursion
            (set 'match-cons (ada-search-ignore-string-comment
                              "loop" nil orgpoint nil 'word-search-forward)))

          (progn
            (goto-char (car match-cons))
            ;;
            ;; indent according to 'loop', if it's first in the line;
            ;; otherwise to 'while'.
            ;;
            (unless (save-excursion
                      (back-to-indentation)
                      (looking-at "\\<loop\\>"))
              (goto-char pos))
	    (list (+ (save-excursion (back-to-indentation) (point)) label) 'ada-indent))

	(list (+ (save-excursion (back-to-indentation) (point)) label)
	      'ada-broken-indent))))))


(defun ada-get-indent-type (orgpoint)
  ;; Returns the indentation (column #) for the new line after ORGPOINT.
  ;; Assumes point to be at the beginning of a type statement.
  (let ((match-dat nil))
    (cond
     ;;
     ;; complete record declaration
     ;;
     ((save-excursion
        (and
         (set 'match-dat (ada-search-ignore-string-comment
                          "end" nil orgpoint nil 'word-search-forward))
         (ada-goto-next-non-ws)
         (looking-at "\\<record\\>")
         (forward-word 1)
         (ada-goto-next-non-ws)
         (= (char-after) ?\;)))
      (goto-char (car match-dat))
      (list (save-excursion (back-to-indentation) (point)) 0))
     ;;
     ;; record type
     ;;
     ((save-excursion
        (set 'match-dat (ada-search-ignore-string-comment
                         "record" nil orgpoint nil 'word-search-forward)))
      (goto-char (car match-dat))
      (list (save-excursion (back-to-indentation) (point)) 'ada-indent))
     ;;
     ;; complete type declaration
     ;;
     ((save-excursion
        (ada-search-ignore-string-comment ";" nil orgpoint nil 'search-forward))
      (list (save-excursion (back-to-indentation) (point)) 0))
     ;;
     ;; "type ... is", but not "type ... is ...", which is broken
     ;;
     ((save-excursion
        (and
         (ada-search-ignore-string-comment "is" nil orgpoint nil 'word-search-forward)
         (not (ada-goto-next-non-ws orgpoint))))
      (list (save-excursion (back-to-indentation) (point)) 'ada-broken-indent))
     ;;
     ;; broken statement
     ;;
     (t
      (list (save-excursion (back-to-indentation) (point)) 'ada-broken-indent)))))


;;; ---- support-functions for indentation

;;; ---- searching and matching

(defun ada-goto-stmt-start (&optional limit)
  ;; Moves point to the beginning of the statement that point is in or
  ;; after.  Returns the new position of point.  Beginnings are found
  ;; by searching for 'ada-end-stmt-re' and then moving to the
  ;; following non-ws that is not a comment.  LIMIT is actually not
  ;; used by the indentation functions.
  ;; As a special case, if we are looking back at a closing parenthesis,
  ;; we just skip the parenthesis
  (let ((match-dat nil)
        (orgpoint (point)))

    (set 'match-dat (ada-search-prev-end-stmt limit))
    (if match-dat

        ;;
        ;; found a previous end-statement => check if anything follows
        ;;
        (unless (looking-at "declare")
          (progn
            (unless (save-excursion
                      (goto-char (cdr match-dat))
                      (ada-goto-next-non-ws orgpoint))
              ;;
              ;; nothing follows => it's the end-statement directly in
              ;;                    front of point => search again
              ;;
              (set 'match-dat (ada-search-prev-end-stmt limit)))
            ;;
            ;; if found the correct end-statement => goto next non-ws
            ;;
            (if match-dat
                (goto-char (cdr match-dat)))
            (ada-goto-next-non-ws)
            ))

      ;;
      ;; no previous end-statement => we are at the beginning of the
      ;;                              accessible part of the buffer
      ;;
      (progn
        (goto-char (point-min))
        ;;
        ;; skip to the very first statement, if there is one
        ;;
        (unless (ada-goto-next-non-ws orgpoint)
          (goto-char orgpoint))))

    (point)))


(defun ada-search-prev-end-stmt (&optional limit)
  ;; Moves point to previous end-statement.  Returns a cons cell whose
  ;; car is the beginning and whose cdr the end of the match.
  ;; End-statements are defined by 'ada-end-stmt-re'.  Checks for
  ;; certain keywords if they follow 'end', which means they are no
  ;; end-statement there.
  (let ((match-dat nil)
        (found nil)
        parse)

    ;;
    ;; search until found or beginning-of-buffer
    ;;
    (while
        (and
         (not found)
         (set 'match-dat (ada-search-ignore-string-comment
                          ada-end-stmt-re t limit)))

      (goto-char (car match-dat))
      (unless (ada-in-open-paren-p)
        (if (and (looking-at
                  "\\<\\(record\\|loop\\|select\\|else\\|then\\)\\>")
                 (save-excursion
                   (ada-goto-previous-word)
                   (looking-at "\\<\\(end\\|or\\|and\\)\\>[ \t]*[^;]")))
            (forward-word -1)

          (save-excursion
            (goto-char (cdr match-dat))
            (ada-goto-next-non-ws)
            (looking-at "(")
            ;;  words that can go after an 'is'
            (unless (looking-at
                     (eval-when-compile
                       (concat "\\<"
                               (regexp-opt '("separate" "access" "array" "abstract" "new") t)
                               "\\>\\|(")))
              (set 'found t))))
        ))

    (if found
        match-dat
      nil)))


(defun ada-goto-next-non-ws (&optional limit)
  "Skips whitespaces, newlines and comments to next non-ws
character.  Signals an error if there is no more such character
and limit is nil.
Do not call this function from within a string."
  (unless limit
    (set 'limit (point-max)))
  (while (and (<= (point) limit)
              (progn (forward-comment 10000)
                     (if (and (not (eobp))
                              (save-excursion (forward-char 1)
                                              (ada-in-string-p)))
                         (progn (forward-sexp 1) t)))))
  (if (< (point) limit)
      (point)
    nil)
  )


(defun ada-goto-stmt-end (&optional limit)
  ;; Moves point to the end of the statement that point is in or
  ;; before.  Returns the new position of point or nil if not found.
  (if (ada-search-ignore-string-comment ada-end-stmt-re nil limit)
      (point)
    nil))


(defun ada-goto-next-word (&optional backward)
  ;; Moves point to the beginning of the next word of Ada code.
  ;; If BACKWARD is non-nil, jump to the beginning of the previous word.
  ;; Returns the new position of point or nil if not found.
  (let ((match-cons nil)
        (orgpoint (point))
        (old-syntax (char-to-string (char-syntax ?_))))
    (modify-syntax-entry ?_ "w")
    (unless backward
      (skip-syntax-forward "w"));;  ??? Used to have . too
    (if (set 'match-cons
             (if backward
                 (ada-search-ignore-string-comment "\\w" t nil t)
               (ada-search-ignore-string-comment "\\w" nil nil t)))
        ;;
        ;; move to the beginning of the word found
        ;;
        (progn
          (goto-char (car match-cons))
          (skip-syntax-backward "w")
          (point))
      ;;
      ;; if not found, restore old position of point
      ;;
      (goto-char orgpoint)
      'nil)
    (modify-syntax-entry ?_ old-syntax))
  )


(defsubst ada-goto-previous-word ()
  ;; Moves point to the beginning of the previous word of Ada code.
  ;; Returns the new position of point or nil if not found.
  (ada-goto-next-word t))


(defun ada-check-matching-start (keyword)
  ;; Signals an error if matching block start is not KEYWORD.
  ;; Moves point to the matching block start.
  (ada-goto-matching-start 0)
  (unless (looking-at (concat "\\<" keyword "\\>"))
    (error "matching start is not '%s'" keyword)))


(defun ada-check-defun-name (defun-name)
  ;; Checks if the name of the matching defun really is DEFUN-NAME.
  ;; Assumes point to be already positioned by 'ada-goto-matching-start'.
  ;; Moves point to the beginning of the declaration.

  ;;
  ;; named block without a `declare'
  ;;
  (if (save-excursion
        (ada-goto-previous-word)
        (looking-at (concat "\\<" defun-name "\\> *:")))
      t                                 ; do nothing
    ;;
    ;; 'accept' or 'package' ?
    ;;
    (unless (looking-at "\\<\\(accept\\|package\\|task\\|protected\\)\\>")
      (ada-goto-matching-decl-start))
    ;;
    ;; 'begin' of 'procedure'/'function'/'task' or 'declare'
    ;;
    (save-excursion
      ;;
      ;; a named 'declare'-block ?
      ;;
      (if (looking-at "\\<declare\\>")
          (ada-goto-stmt-start)
        ;;
        ;; no, => 'procedure'/'function'/'task'/'protected'
        ;;
        (progn
          (forward-word 2)
          (backward-word 1)
          ;;
          ;; skip 'body' 'type'
          ;;
          (if (looking-at "\\<\\(body\\|type\\)\\>")
              (forward-word 1))
          (forward-sexp 1)
          (backward-sexp 1)))
      ;;
      ;; should be looking-at the correct name
      ;;
      (unless (looking-at (concat "\\<" defun-name "\\>"))
        (error "matching defun has different name: %s"
               (buffer-substring (point)
                                 (progn (forward-sexp 1) (point))))))))

(defun ada-goto-matching-decl-start (&optional noerror nogeneric)
  ;; Moves point to the matching declaration start of the current 'begin'.
  ;; If NOERROR is non-nil, it only returns nil if no match was found.
  (let ((nest-count 1)
        (first t)
        (flag nil)
        (count-generic nil)
        )

    (if (or
         (looking-at "\\<\\(package\\|procedure\\|function\\)\\>")
         (save-excursion
           (ada-search-ignore-string-comment "\\<\\(package\\|procedure\\|function\\|generic\\)\\>" t)
           (looking-at "generic")))
        (set 'count-generic t))

    ;;
    ;; search backward for interesting keywords
    ;;
    (while (and
            (not (zerop nest-count))
            (ada-search-ignore-string-comment ada-matching-decl-start-re t))
      ;;
      ;; calculate nest-depth
      ;;
      (cond
       ;;
       ((looking-at "end")
        (ada-goto-matching-start 1 noerror)

	;;  In some case, two begin..end block can follow each other closely,
	;;  which we have to detect, as in
	;;     procedure P is
	;;        procedure Q is
	;;        begin
	;;        end;
        ;;     begin    --  here we should go to procedure, not begin
	;;     end

	(let ((loop-again 0))
	  (if (looking-at "begin")
	      (set 'loop-again 1))

	  (save-excursion
	    (while (not (= loop-again 0))
	      
	      ;;  If begin was just there as the beginning of a block (with no
	      ;;  declare) then do nothing, otherwise just register that we
	      ;;  have to find the statement that required the begin
	      
	      (ada-search-ignore-string-comment
	       "declare\\|begin\\|end\\|procedure\\|function\\|task\\|package"
	       t)

	      (if (looking-at "end")
		  (set 'loop-again (1+ loop-again))

		(set 'loop-again (1- loop-again))
		(unless (looking-at "begin")
		    (set 'nest-count (1+ nest-count))))
	      ))
	  ))
       ;;
       ((looking-at "generic")
        (if count-generic
            (progn
              (set 'first nil)
              (set 'nest-count (1- nest-count)))))
       ;;
       ((looking-at "declare\\|generic\\|if")
        (set 'nest-count (1- nest-count))
        (set 'first nil))
       ;;
       ((looking-at "is")
        ;; check if it is only a type definition, but not a protected
        ;; type definition, which should be handled like a procedure.
        (if (or (looking-at "is[ \t]+<>")
                (save-excursion
                  (forward-comment -10000)
                  (forward-char -1)

                  ;; Detect if we have a closing parenthesis (Could be
                  ;; either the end of subprogram parameters or (<>)
                  ;; in a type definition
                  (if (= (char-after) ?\))
                      (progn
                        (forward-char 1)
                        (backward-sexp 1)
                        (forward-comment -10000)
                        ))
                  (skip-chars-backward "a-zA-Z0-9_.'")
                  (ada-goto-previous-word)
                  (and
                   (looking-at "\\<\\(sub\\)?type\\>")
                   (save-match-data
                     (ada-goto-previous-word)
                     (not (looking-at "\\<protected\\>"))))
                  ))                    ; end of `or'
            (goto-char (match-beginning 0))
          (progn
            (set 'nest-count (1- nest-count))
            (set 'first nil))))

       ;;
       ((looking-at "new")
        (if (save-excursion
              (ada-goto-previous-word)
              (looking-at "is"))
            (goto-char (match-beginning 0))))
       ;;
       ((and first
             (looking-at "begin"))
        (set 'nest-count 0)
        (set 'flag t))
       ;;
       (t
        (set 'nest-count (1+ nest-count))
        (set 'first nil)))

      );; end of loop

    ;; check if declaration-start is really found
    (if (and
         (zerop nest-count)
         (not flag)
         (if (looking-at "is")
             (ada-search-ignore-string-comment ada-subprog-start-re t)
           (looking-at "declare\\|generic")))
        t
      (if noerror nil
        (error "no matching proc/func/task/declare/package/protected")))
    ))

(defun ada-goto-matching-start (&optional nest-level noerror gotothen)
  ;; Moves point to the beginning of a block-start.  Which block
  ;; depends on the value of NEST-LEVEL, which defaults to zero.  If
  ;; NOERROR is non-nil, it only returns nil if no matching start was
  ;; found.  If GOTOTHEN is non-nil, point moves to the 'then'
  ;; following 'if'.
  (let ((nest-count (if nest-level nest-level 0))
        (found nil)
        (pos nil))

    ;;
    ;; search backward for interesting keywords
    ;;
    (while (and
            (not found)
            (ada-search-ignore-string-comment ada-matching-start-re t))

      (unless (and (looking-at "\\<record\\>")
                   (save-excursion
                     (forward-word -1)
                     (looking-at "\\<null\\>")))
        (progn
          ;;
          ;; calculate nest-depth
          ;;
          (cond
           ;; found block end => increase nest depth
           ((looking-at "end")
            (set 'nest-count (1+ nest-count)))

           ;; found loop/select/record/case/if => check if it starts or
           ;; ends a block
           ((looking-at "loop\\|select\\|record\\|case\\|if")
            (set 'pos (point))
            (save-excursion
              ;;
              ;; check if keyword follows 'end'
              ;;
              (ada-goto-previous-word)
              (if (looking-at "\\<end\\>[ \t]*[^;]")
                  ;; it ends a block => increase nest depth
                  (progn
                    (set 'nest-count (1+ nest-count))
                    (set 'pos (point)))
                ;; it starts a block => decrease nest depth
                (set 'nest-count (1- nest-count))))
            (goto-char pos))

           ;; found package start => check if it really is a block
           ((looking-at "package")
            (save-excursion
              ;; ignore if this is just a renames statement
              (let ((current (point))
                    (pos (ada-search-ignore-string-comment
                          "\\<\\(is\\|renames\\|;\\)\\>" nil)))
                (if pos
                    (goto-char (car pos))
                  (error (concat
                          "No matching 'is' or 'renames' for 'package' at line "
                          (number-to-string (count-lines (point-min) (1+ current)))))))
              (unless (looking-at "renames")
                (progn
                  (forward-word 1)
                  (ada-goto-next-non-ws)
                  ;; ignore it if it is only a declaration with 'new'
                  (if (not (looking-at "\\<\\(new\\|separate\\)\\>"))
                      (set 'nest-count (1- nest-count)))))))
           ;; found task start => check if it has a body
           ((looking-at "task")
            (save-excursion
              (forward-word 1)
              (ada-goto-next-non-ws)
              (cond
               ((looking-at "\\<body\\>"))
               ((looking-at "\\<type\\>")
                ;;  In that case, do nothing if there is a "is"
                (forward-word 2);; skip "type"
                (ada-goto-next-non-ws);; skip type name

		;; Do nothing if we are simply looking at a simple
		;; "task type name;" statement with no block
		(unless (looking-at ";")
		  (progn
		    ;; Skip the parameters
		    (if (looking-at "(")
			(ada-search-ignore-string-comment ")" nil))
		    (let ((tmp (ada-search-ignore-string-comment
				"\\<\\(is\\|;\\)\\>" nil)))
		      (if tmp
			  (progn
			    (goto-char (car tmp))
			    (if (looking-at "is")
				(set 'nest-count (1- nest-count)))))))))
               (t
		;; Check if that task declaration had a block attached to
		;; it (i.e do nothing if we have just "task name;")
		(unless (progn (forward-word 1)
			       (looking-at "[ \t]*;"))
		  (set 'nest-count (1- nest-count)))))))
           ;; all the other block starts
           (t
            (set 'nest-count (1- nest-count)))) ; end of 'cond'

          ;; match is found, if nest-depth is zero
          ;;
          (set 'found (zerop nest-count))))) ; end of loop

    (if found
        ;;
        ;; match found => is there anything else to do ?
        ;;
        (progn
          (cond
           ;;
           ;; found 'if' => skip to 'then', if it's on a separate line
           ;;                               and GOTOTHEN is non-nil
           ;;
           ((and
             gotothen
             (looking-at "if")
             (save-excursion
               (ada-search-ignore-string-comment "then" nil nil nil 'word-search-forward)
               (back-to-indentation)
               (looking-at "\\<then\\>")))
            (goto-char (match-beginning 0)))
           ;;
           ;; found 'do' => skip back to 'accept'
           ;;
           ((looking-at "do")
            (unless (ada-search-ignore-string-comment "accept" t nil nil 'word-search-backward)
              (error "missing 'accept' in front of 'do'"))))
          (point))

      (if noerror
          nil
        (error "no matching start")))))


(defun ada-goto-matching-end (&optional nest-level noerror)
  ;; Moves point to the end of a block.  Which block depends on the
  ;; value of NEST-LEVEL, which defaults to zero.  If NOERROR is
  ;; non-nil, it only returns nil if found no matching start.
  (let ((nest-count (if nest-level nest-level 0))
        (found nil))

    ;;
    ;; search forward for interesting keywords
    ;;
    (while (and
            (not found)
            (ada-search-ignore-string-comment
             (eval-when-compile
               (concat "\\<"
                       (regexp-opt '("end" "loop" "select" "begin" "case"
                                     "if" "task" "package" "record" "do") t)
                       "\\>")) nil))

      ;;
      ;; calculate nest-depth
      ;;
      (backward-word 1)
      (cond
       ;; found block end => decrease nest depth
       ((looking-at "\\<end\\>")
        (set 'nest-count (1- nest-count))
        ;; skip the following keyword
        (if (progn
              (skip-chars-forward "end")
              (ada-goto-next-non-ws)
              (looking-at "\\<\\(loop\\|select\\|record\\|case\\|if\\)\\>"))
            (forward-word 1)))
       ;; found package start => check if it really starts a block
       ((looking-at "\\<package\\>")
        (ada-search-ignore-string-comment "is" nil nil nil 'word-search-forward)
        (ada-goto-next-non-ws)
        ;; ignore and skip it if it is only a 'new' package
        (if (looking-at "\\<new\\>")
            (goto-char (match-end 0))
          (set 'nest-count (1+ nest-count))))
       ;; all the other block starts
       (t
        (set 'nest-count (1+ nest-count))
        (forward-word 1)))              ; end of 'cond'

      ;; match is found, if nest-depth is zero
      ;;
      (set 'found (zerop nest-count)))  ; end of loop

    (if found
        t
      (if noerror
          nil
        (error "no matching end")))
    ))


(defun ada-search-ignore-string-comment
  (search-re &optional backward limit paramlists search-func )
  ;; Regexp-Search for SEARCH-RE, ignoring comments, strings and
  ;; parameter lists, if PARAMLISTS is nil. Returns a cons cell of
  ;; begin and end of match data or nil, if not found.
  ;; The search is done using search-func, so that we can choose using
  ;; regular expression search, basic search, ...
  ;; Point is moved at the beginning of the search-re
  (let (found
        begin
        end
        parse-result
        (previous-syntax-table (syntax-table)))

    (unless search-func
      (set 'search-func (if backward 're-search-backward 're-search-forward)))

    ;;
    ;; search until found or end-of-buffer
    ;; We have to test that we do not look further than limit
    ;;
    (set-syntax-table ada-mode-symbol-syntax-table)
    (while (and (not found)
                (or (not limit)
                    (or (and backward (<= limit (point)))
                        (>= limit (point))))
                (funcall search-func search-re limit 1))
      (set 'begin (match-beginning 0))
      (set 'end (match-end 0))

      (set 'parse-result (parse-partial-sexp
                          (save-excursion (beginning-of-line) (point))
                          (point)))

      (cond
       ;;
       ;; If inside a string, skip it (and the following comments)
       ;;
       ((ada-in-string-p parse-result)
        (if ada-xemacs
            (search-backward "\"" nil t)
          (goto-char (nth 8 parse-result)))
        (unless backward (forward-sexp 1)))
       ;;
       ;; If inside a comment, skip it (and the following comments)
       ;; There is a special code for comments at the end of the file
       ;;
       ((ada-in-comment-p parse-result)
        (if ada-xemacs
	    (progn
	      (forward-line 1)
	      (beginning-of-line)
	      (forward-comment -1))
          (goto-char (nth 8 parse-result)))
        (unless backward
          ;;  at the end of the file, it is not possible to skip a comment
          ;;  so we just go at the end of the line
          (if (forward-comment 1)
              (progn
                (forward-comment 1000)
                (beginning-of-line))
            (end-of-line))))
       ;;
       ;; directly in front of a comment => skip it, if searching forward
       ;;
       ((and (= (char-after begin) ?-) (= (char-after (1+ begin)) ?-))
        (unless backward (progn (forward-char -1) (forward-comment 1000))))

       ;;
       ;; found a parameter-list but should ignore it => skip it
       ;;
       ((and (not paramlists) (ada-in-paramlist-p))
        (if backward
            (search-backward "(" nil t)
          (search-forward ")" nil t)))
       ;;
       ;; found what we were looking for
       ;;
       (t
        (set 'found t))))               ; end of loop

    (set-syntax-table previous-syntax-table)

    (if found
        (cons begin end)
      nil)))

;; ---- boolean functions for indentation

(defun ada-in-decl-p ()
  ;; Returns t if point is inside a declarative part.
  ;; Assumes point to be at the end of a statement.
  (or
   (ada-in-paramlist-p)
   (save-excursion
     (ada-goto-matching-decl-start t))))


(defun ada-looking-at-semi-or ()
  ;; Returns t if looking-at an 'or' following a semicolon.
  (save-excursion
    (and (looking-at "\\<or\\>")
         (progn
           (forward-word 1)
           (ada-goto-stmt-start)
           (looking-at "\\<or\\>")))))


(defun ada-looking-at-semi-private ()
  "Returns t if looking-at an 'private' following a semicolon.
Returns nil if the private is part of the package name, as in
'private package A is...' (this can only happen at top level)"
  (save-excursion
    (and (looking-at "\\<private\\>")
         (not (looking-at "\\<private[ \t]*\\(package\\|generic\\)"))
         (progn (forward-comment -1000)
                (= (char-before) ?\;)))))

(defsubst ada-in-comment-p (&optional parse-result)
  "Returns t if inside a comment."
  (nth 4 (or parse-result
             (parse-partial-sexp
              (save-excursion (beginning-of-line) (point)) (point)))))

(defsubst ada-in-string-p (&optional parse-result)
  "Returns t if point is inside a string.
if parse-result is non-nil, use is instead of calling parse-partial-sexp"
  (nth 3 (or parse-result
             (parse-partial-sexp
              (save-excursion (beginning-of-line) (point)) (point)))))

(defsubst ada-in-string-or-comment-p (&optional parse-result)
  "Returns t if inside a comment or string"
  (set 'parse-result (or parse-result
                         (parse-partial-sexp
                          (save-excursion (beginning-of-line) (point)) (point))))
  (or (ada-in-string-p parse-result) (ada-in-comment-p parse-result)))

(defun ada-in-paramlist-p ()
  ;; Returns t if point is inside a parameter-list
  ;; following 'function'/'procedure'/'package'.
  (save-excursion
    (and
     (re-search-backward "(\\|)" nil t)
     ;; inside parentheses ?
     (= (char-after) ?\()
     (backward-word 2)
     
     ;; We should ignore the case when the reserved keyword is in a
     ;; comment (for instance, when we have:
     ;;    -- .... package
     ;;    Test (A)
     ;; we should return nil

     (not (ada-in-string-or-comment-p))
     
     ;; right keyword two words before parenthesis ?
     ;; Type is in this list because of discriminants
     (looking-at (eval-when-compile
                   (concat "\\<\\("
                           "procedure\\|function\\|body\\|"
                           "task\\|entry\\|accept\\|"
                           "access[ \t]+procedure\\|"
                           "access[ \t]+function\\|"
			   "pragma\\|"
                           "type\\)\\>"))))))

;; not really a boolean function ...
(defun ada-in-open-paren-p ()
  "If point is somewhere behind an open parenthesis not yet closed,
it returns the position of the first non-ws behind that open parenthesis,
otherwise nil"
  (save-excursion
    (let ((parse (parse-partial-sexp
		  (point)
		  (or (car (ada-search-ignore-string-comment "\\<\\(;\\|is\\|then\\|loop\\|begin\\|else\\)\\>" t))
		      (point-min)))))
      
      (if (nth 1 parse)
          (progn
            (goto-char (1+ (nth 1 parse)))
            (skip-chars-forward " \t")
	    (point))))))


;;;----------------------;;;
;;; Behaviour Of TAB Key ;;;
;;;----------------------;;;
(defun ada-tab ()
  "Do indenting or tabbing according to `ada-tab-policy'.

In Transient Mark mode, if the mark is active, operate on the contents
of the region.  Otherwise, operates only on the current line"
  (interactive)
  (cond ((eq ada-tab-policy 'indent-rigidly) (ada-tab-hard))
        ((eq ada-tab-policy 'indent-auto)
         ;;  transient-mark-mode and mark-active are not defined in XEmacs
         (if (or (and ada-xemacs (region-active-p))
                 (and (not ada-xemacs)
                      transient-mark-mode
                      mark-active))
             (ada-indent-region (region-beginning) (region-end))
           (ada-indent-current)))
        ((eq ada-tab-policy 'always-tab) (error "not implemented"))
        ))

(defun ada-untab (arg)
  "Delete leading indenting according to `ada-tab-policy'."
  (interactive "P")
  (cond ((eq ada-tab-policy 'indent-rigidly) (ada-untab-hard))
        ((eq ada-tab-policy 'indent-auto) (error "not implemented"))
        ((eq ada-tab-policy 'always-tab) (error "not implemented"))
        ))

(defun ada-indent-current-function ()
  "Ada mode version of the indent-line-function."
  (interactive "*")
  (let ((starting-point (point-marker)))
    (beginning-of-line)
    (ada-tab)
    (if (< (point) starting-point)
        (goto-char starting-point))
    (set-marker starting-point nil)
    ))

(defun ada-tab-hard ()
  "Indent current line to next tab stop."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert-char ?  ada-indent))
  (if (save-excursion (= (point) (progn (beginning-of-line) (point))))
      (forward-char ada-indent)))

(defun ada-untab-hard ()
  "indent current line to previous tab stop."
  (interactive)
  (let  ((bol (save-excursion (progn (beginning-of-line) (point))))
         (eol (save-excursion (progn (end-of-line) (point)))))
    (indent-rigidly bol eol  (- 0 ada-indent))))



;;;---------------;;;
;;; Miscellaneous ;;;
;;;---------------;;;

(defun ada-remove-trailing-spaces  ()
  "remove trailing spaces in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+$" (point-max) t)
          (replace-match "" nil nil))))))


;; define a function to support find-file.el if loaded
(defun ada-ff-other-window ()
  "Find other file in other window using `ff-find-other-file'."
  (interactive)
  (and (fboundp 'ff-find-other-file)
       (ff-find-other-file t)))

;; inspired by Laurent.GUERBY@enst-bretagne.fr
(defun ada-gnat-style ()
  "Clean up comments, `(' and `,' for GNAT style checking switch."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "-- ?\\([^ -]\\)" nil t)
      (replace-match "--  \\1"))
    (goto-char (point-min))
    (while (re-search-forward "\\>(" nil t)
      (replace-match " ("))
    (goto-char (point-min))
    (while (re-search-forward ",\\<" nil t)
      (replace-match ", "))
    ))



;;;-------------------------------;;;
;;; Moving To Procedures/Packages ;;;
;;;-------------------------------;;;
(defun ada-next-procedure ()
  "Moves point to next procedure."
  (interactive)
  (end-of-line)
  (if (re-search-forward ada-procedure-start-regexp nil t)
      (goto-char (match-beginning 1))
    (error "No more functions/procedures/tasks")))

(defun ada-previous-procedure ()
  "Moves point to previous procedure."
  (interactive)
  (beginning-of-line)
  (if (re-search-backward ada-procedure-start-regexp nil t)
      (goto-char (match-beginning 1))
    (error "No more functions/procedures/tasks")))

(defun ada-next-package ()
  "Moves point to next package."
  (interactive)
  (end-of-line)
  (if (re-search-forward ada-package-start-regexp nil t)
      (goto-char (match-beginning 1))
    (error "No more packages")))

(defun ada-previous-package ()
  "Moves point to previous package."
  (interactive)
  (beginning-of-line)
  (if (re-search-backward ada-package-start-regexp nil t)
      (goto-char (match-beginning 1))
    (error "No more packages")))


;;;-----------------------
;;; define keymap and menus for Ada
;;;-----------------------

(defun ada-create-keymap ()
  "Create the keymap associated with the Ada mode"

  ;; Indentation and Formatting
  (define-key ada-mode-map "\C-j"     'ada-indent-newline-indent-conditional)
  (define-key ada-mode-map "\C-m"     'ada-indent-newline-indent-conditional)
  (define-key ada-mode-map "\t"       'ada-tab)
  (define-key ada-mode-map "\C-c\t"   'ada-justified-indent-current)
  (define-key ada-mode-map "\C-c\C-l" 'ada-indent-region)
  (if ada-xemacs
      (define-key ada-mode-map '(shift tab)    'ada-untab)
    (define-key ada-mode-map [S-tab]    'ada-untab))
  (define-key ada-mode-map "\C-c\C-f" 'ada-format-paramlist)
  ;; We don't want to make meta-characters case-specific.

  ;; Movement
  (define-key ada-mode-map "\M-\C-e"  'ada-next-procedure)
  (define-key ada-mode-map "\M-\C-a"  'ada-previous-procedure)
  (define-key ada-mode-map "\C-c\C-a" 'ada-move-to-start)
  (define-key ada-mode-map "\C-c\C-e" 'ada-move-to-end)

  ;; Compilation
  (unless (lookup-key ada-mode-map "\C-c\C-c")
    (define-key ada-mode-map "\C-c\C-c" 'compile))

  ;; Casing
  (define-key ada-mode-map "\C-c\C-b" 'ada-adjust-case-buffer)
  (define-key ada-mode-map "\C-c\C-t" 'ada-case-read-exceptions)
  (define-key ada-mode-map "\C-c\C-y" 'ada-create-case-exception)

  (define-key ada-mode-map "\177"     'backward-delete-char-untabify)

  ;; Make body
  (define-key ada-mode-map "\C-c\C-n" 'ada-make-subprogram-body)

  ;; Use predefined function of emacs19 for comments (RE)
  (define-key ada-mode-map "\C-c;"    'comment-region)
  (define-key ada-mode-map "\C-c:"    'ada-uncomment-region)

  )

(defun ada-create-menu ()
  "Create the ada menu as shown in the menu bar.
This function is designed to be extensible, so that each compiler-specific file
can add its own items"

  ;;  Note that the separators must have different length in the submenus
  (autoload 'easy-menu-define "easymenu")
  (autoload 'imenu "imenu")
  (easy-menu-define
   ada-mode-menu ada-mode-map "Menu keymap for Ada mode"
   '("Ada"
     ("Help"
      ["Ada Mode" (info "ada-mode") t])
     ["Customize" (customize-group 'ada)  (>= emacs-major-version 20)]
     ("Goto"
      ["Next compilation error"  next-error t]
      ["Previous Package" ada-previous-package t]
      ["Next Package" ada-next-package t]
      ["Previous Procedure" ada-previous-procedure t]
      ["Next Procedure" ada-next-procedure t]
      ["Goto Start Of Statement" ada-move-to-start t]
      ["Goto End Of Statement" ada-move-to-end t]
      ["-" nil nil]
      ["Other File" ff-find-other-file t]
      ["Other File Other Window" ada-ff-other-window t])
     ("Edit"
      ["Indent Line"  ada-indent-current-function t]
      ["Justify Current Indentation" ada-justified-indent-current t]
      ["Indent Lines in Selection" ada-indent-region t]
      ["Indent Lines in File" (ada-indent-region (point-min) (point-max)) t]
      ["Format Parameter List" ada-format-paramlist t]
      ["-" nil nil]
      ["Comment Selection" comment-region t]
      ["Uncomment Selection" ada-uncomment-region t]
      ["--" nil nil]
      ["Fill Comment Paragraph" fill-paragraph t]
      ["Fill Comment Paragraph Justify" ada-fill-comment-paragraph-justify t]
      ["Fill Comment Paragraph Postfix" ada-fill-comment-paragraph-postfix t]
      ["---" nil nil]
      ["Adjust Case Selection"  ada-adjust-case-region t]
      ["Adjust Case Buffer"     ada-adjust-case-buffer t]
      ["Create Case Exception"  ada-create-case-exception t]
      ["Reload Case Exceptions" ada-case-read-exceptions t]
      ["----" nil nil]
      ["Make body for subprogram" ada-make-subprogram-body t]
      )
     ["Index" imenu t]
     ))

  (if ada-xemacs
      (progn
        (easy-menu-add ada-mode-menu ada-mode-map)
        (define-key ada-mode-map [menu-bar] ada-mode-menu)
        (set 'mode-popup-menu (cons "Ada mode" ada-mode-menu))
	)
    )
  )




;;
;;  The two following calls are provided to enhance the standard
;;  comment-region function, which only allows uncommenting if the
;;  comment is at the beginning of a line. If the line have been reindented,
;;  we are unable to use comment-region, which makes no sense.
;;
(defadvice comment-region (before ada-uncomment-anywhere)
  (if (and arg
           (< arg 0)
           (string= mode-name "Ada"))
      (save-excursion
        (let ((cs (concat "^[ \t]*" (regexp-quote comment-start))))
          (goto-char beg)
          (while (re-search-forward cs end t)
            (replace-match comment-start))
          ))))

;;
;;  Handling of comments
;;

(defun ada-uncomment-region (beg end &optional arg)
  "delete `comment-start' at the beginning of a line in the region."
  (interactive "r\nP")
  (ad-activate 'comment-region)
  (comment-region beg end (- (or arg 1)))
  (ad-deactivate 'comment-region))

(defun ada-fill-comment-paragraph-justify ()
  "Fills current comment paragraph and justifies each line as well."
  (interactive)
  (ada-fill-comment-paragraph 'full))

(defun ada-fill-comment-paragraph-postfix ()
  "Fills current comment paragraph and justifies each line as well.
Adds `ada-fill-comment-postfix' at the end of each line"
  (interactive)
  (ada-fill-comment-paragraph 'full t))

(defun ada-fill-comment-paragraph (&optional justify postfix)
  "Fills the current comment paragraph.
If JUSTIFY is non-nil, each line is justified as well.
If POSTFIX and JUSTIFY are  non-nil, `ada-fill-comment-postfix' is appended
to each filled and justified line.
The paragraph is indented on the first line."
  (interactive "P")

  ;; check if inside comment or just in front a comment
  (if (and (not (ada-in-comment-p))
           (not (looking-at "[ \t]*--")))
      (error "not inside comment"))

  (let* ((indent)
         (from)
         (to)
         (opos             (point-marker))

	 ;; Sets this variable to nil, otherwise it prevents
	 ;; fill-region-as-paragraph to work on Emacs <= 20.2
	 (parse-sexp-lookup-properties nil)
	 
         fill-prefix
         (fill-column (current-fill-column)))

    ;;  Find end of paragraph
    (back-to-indentation)
    (while (and (not (eobp)) (looking-at "--[ \t]*[^ \t\n]"))
      (forward-line 1)
      (back-to-indentation))
    (beginning-of-line)
    (set 'to (point-marker))
    (goto-char opos)

    ;;  Find beginning of paragraph
    (back-to-indentation)
    (while (and (not (bobp)) (looking-at "--[ \t]*[^ \t\n]"))
      (forward-line -1)
      (back-to-indentation))
    (forward-line 1)
    (beginning-of-line)
    (set 'from (point-marker))

    ;;  Calculate the indentation we will need for the paragraph
    (back-to-indentation)
    (set 'indent (current-column))
    ;;  unindent the first line of the paragraph
    (delete-region from (point))

    ;;  Remove the old postfixes
    (goto-char from)
    (while (re-search-forward (concat ada-fill-comment-postfix "\n") to t)
      (replace-match "\n"))

    (goto-char (1- to))
    (set 'to (point-marker))

    ;;  Indent and justify the paragraph
    (set 'fill-prefix ada-fill-comment-prefix)
    (set-left-margin from to indent)
    (if postfix
        (set 'fill-column (- fill-column (length ada-fill-comment-postfix))))

    (fill-region-as-paragraph from to justify)

    ;;  Add the postfixes if required
    (if postfix
        (save-restriction
          (goto-char from)
          (narrow-to-region from to)
          (while (not (eobp))
            (end-of-line)
            (insert-char ?  (- fill-column (current-column)))
            (insert ada-fill-comment-postfix)
            (forward-line))
          ))

    ;;  In Emacs <= 20.2 and XEmacs <=20.4, there is a bug, and a newline is
    ;;  inserted at the end. Delete it
    (if (or ada-xemacs
            (<= emacs-major-version 19)
            (and (= emacs-major-version 20)
                 (<= emacs-minor-version 2)))
        (progn
          (goto-char to)
          (end-of-line)
          (delete-char 1)))

    (goto-char opos)))

;;;---------------------------------------------------
;;; support for find-file.el
;;;---------------------------------------------------

;;; Note : this function is overwritten when we work with GNAT: we then
;;; use gnatkrunch
(defun ada-make-filename-from-adaname (adaname)
  "Determine the filename of a package/procedure from its own Ada name.
This is a generic function, independant from any compiler."
  (while (string-match "\\." adaname)
    (set 'adaname (replace-match "-" t t adaname)))
  adaname
  )

(defun ada-other-file-name ()
  "Return the name of the other file (the body if current-buffer is the spec,
or the spec otherwise."
  (let ((ff-always-try-to-create nil)
        (buffer                  (current-buffer))
        name)
    (ff-find-other-file nil t);; same window, ignore 'with' lines
    (if (equal buffer (current-buffer))

        ;;  other file not found
        ""

      ;; other file found
      (set 'name (buffer-file-name))
      (switch-to-buffer buffer)
      name)))

;;; functions for placing the cursor on the corresponding subprogram
(defun ada-which-function-are-we-in ()
  "Determine whether we are on a function definition/declaration.
If that is the case remember the name of that function.
This function is used in support of the find-file.el package"

  (set 'ff-function-name nil)
  (save-excursion
    (end-of-line);;  make sure we get the complete name
    (if (or (re-search-backward ada-procedure-start-regexp nil t)
            (re-search-backward ada-package-start-regexp nil t))
        (set 'ff-function-name (match-string 0)))
    ))

(defun ada-set-point-accordingly ()
  "Move to the function declaration that was set by `ff-which-function-are-we-in'"
  (if ff-function-name
      (progn
        (goto-char (point-min))
        (unless (ada-search-ignore-string-comment (concat ff-function-name "\\b") nil)
          (goto-char (point-min))))))

;;;---------------------------------------------------
;;; support for font-lock
;;;---------------------------------------------------
;; Strings are a real pain in Ada because a single quote character is
;; overloaded as a string quote and type/instance delimiter.  By default, a
;; single quote is given punctuation syntax in `ada-mode-syntax-table'.
;; So, for Font Lock mode purposes, we mark single quotes as having string
;; syntax when the gods that created Ada determine them to be.  sm.

(defconst ada-font-lock-syntactic-keywords
  ;; Mark single quotes as having string quote syntax in 'c' instances.
  ;; As a special case, ''' will not be hilighted, but if we do not
  ;; set this special case, then the rest of the buffer is hilighted as
  ;; a string
  ;; This sets the properties of the characters, so that ada-in-string-p
  ;; correctly handles '"' too...
  '(("\\('\\)[^'\n]\\('\\)" (1 (7 . ?')) (2 (7 . ?')))
    ("^[ \t]*\\(#\\(if\\|else\\|elsif\\|end\\)\\)" (1 (11 . ?\n)))
    ))

(defvar ada-font-lock-keywords
  (eval-when-compile
    (list
     ;;
     ;; handle "type T is access function return S;"
     (list "\\<\\(function[ \t]+return\\)\\>" '(1 font-lock-keyword-face) )

     ;;  preprocessor line
     (list "^[ \t]*\\(#.*\n\\)"  '(1 font-lock-type-face t))

     ;;
     ;; accept, entry, function, package (body), protected (body|type),
     ;; pragma, procedure, task (body) plus name.
     (list (concat
            "\\<\\("
            "accept\\|"
            "entry\\|"
            "function\\|"
            "package[ \t]+body\\|"
            "package\\|"
            "pragma\\|"
            "procedure\\|"
            "protected[ \t]+body\\|"
            "protected[ \t]+type\\|"
            "protected\\|"
            "task[ \t]+body\\|"
            "task[ \t]+type\\|"
            "task"
            "\\)\\>[ \t]*"
            "\\(\\sw+\\(\\.\\sw*\\)*\\)?")
           '(1 font-lock-keyword-face) '(2 font-lock-function-name-face nil t))
     ;;
     ;; Optional keywords followed by a type name.
     (list (concat                      ; ":[ \t]*"
            "\\<\\(access[ \t]+all\\|access\\|constant\\|in[ \t]+out\\|in\\|out\\)\\>"
            "[ \t]*"
            "\\(\\sw+\\(\\.\\sw*\\)*\\)?")
           '(1 font-lock-keyword-face nil t) '(2 font-lock-type-face nil t))

     ;;
     ;; Main keywords, except those treated specially below.
     (concat "\\<"
             (regexp-opt
              '("abort" "abs" "abstract" "accept" "access" "aliased" "all"
                "and" "array" "at" "begin" "case" "declare" "delay" "delta"
                "digits" "do" "else" "elsif" "entry" "exception" "exit" "for"
                "generic" "if" "in" "is" "limited" "loop" "mod" "not"
                "null" "or" "others" "private" "protected" "raise"
                "range" "record" "rem" "renames" "requeue" "return" "reverse"
                "select" "separate" "tagged" "task" "terminate" "then" "until"
                "when" "while" "xor") t)
             "\\>")
     ;;
     ;; Anything following end and not already fontified is a body name.
     '("\\<\\(end\\)\\>\\([ \t]+\\)?\\(\\(\\sw\\|[_.]\\)+\\)?"
       (1 font-lock-keyword-face) (3 font-lock-function-name-face nil t))
     ;;
     ;; Keywords followed by a type or function name.
     (list (concat "\\<\\("
                   "new\\|of\\|subtype\\|type"
                   "\\)\\>[ \t]*\\(\\sw+\\(\\.\\sw*\\)*\\)?[ \t]*\\((\\)?")
           '(1 font-lock-keyword-face)
           '(2 (if (match-beginning 4)
                   font-lock-function-name-face
                 font-lock-type-face) nil t))
     ;;
     ;; Keywords followed by a (comma separated list of) reference.
     (list (concat "\\<\\(goto\\|raise\\|use\\|with\\)\\>" ; "when" removed
                   "[ \t\n]*\\(\\(\\sw\\|[_.|, \t\n]\\)+\\)\\W")
           '(1 font-lock-keyword-face) '(2 font-lock-reference-face nil t))
     ;;
     ;; Goto tags.
     '("<<\\(\\sw+\\)>>" 1 font-lock-reference-face)
     ))
  "Default expressions to highlight in Ada mode.")

;;
;;  outline-minor-mode support

(defun ada-outline-level ()
  ;; This is so that `current-column` DTRT in otherwise-hidden text
  ;; patch from Dave Love <fx@gnu.org>
  (let (buffer-invisibility-spec)
    (save-excursion
      (back-to-indentation)
      (current-column))))

;;
;;  Body generation
;;

(defun ada-gen-treat-proc (match)
  ;; make dummy body of a procedure/function specification.
  ;; MATCH is a cons cell containing the start and end location of the
  ;; last search for ada-procedure-start-regexp.
  (goto-char (car match))
  (let (func-found procname functype)
    (cond
     ((or (looking-at "^[ \t]*procedure")
          (set 'func-found (looking-at "^[ \t]*function")))
      ;; treat it as a proc/func
      (forward-word 2)
      (forward-word -1)
      (set 'procname (buffer-substring (point) (cdr match))) ; store  proc name

      ;; goto end of procname
      (goto-char (cdr match))

      ;; skip over parameterlist
      (unless (looking-at "[ \t\n]*\\(;\\|return\\)")
        (forward-sexp))

      ;; if function, skip over 'return' and result type.
      (if func-found
          (progn
            (forward-word 1)
            (skip-chars-forward " \t\n")
            (set 'functype (buffer-substring (point)
                                             (progn
                                               (skip-chars-forward
                                                "a-zA-Z0-9_\.")
                                               (point))))))
      ;; look for next non WS
      (cond
       ((looking-at "[ \t]*;")
        (delete-region (match-beginning 0) (match-end 0));; delete the ';'
        (ada-indent-newline-indent)
        (insert "is")
        (ada-indent-newline-indent)
        (if func-found
            (progn
              (insert "Result : " functype ";")
              (ada-indent-newline-indent)))
        (insert "begin")
        (ada-indent-newline-indent)
        (if func-found
            (insert "return Result;")
          (insert "null;"))
        (ada-indent-newline-indent)
        (insert "end " procname ";")
        (ada-indent-newline-indent)
        )
       ;; else
       ((looking-at "[ \t\n]*is")
        ;; do nothing
        )
       ((looking-at "[ \t\n]*rename")
        ;; do nothing
        )
       (t
        (message "unknown syntax"))))
     (t
      (if (looking-at "^[ \t]*task")
          (progn
            (message "Task conversion is not yet implemented")
            (forward-word 2)
            (if (looking-at "[ \t]*;")
                (forward-line)
              (ada-move-to-end))
            ))))))

(defun ada-make-body ()
  "Create an Ada package body in the current buffer.
The potential old buffer contents is deleted first, then we copy the
spec buffer in here and modify it to make it a body.

This function typically is to be hooked into `ff-file-created-hooks'."
  (interactive)
  (delete-region (point-min) (point-max))
  (insert-buffer (car (cdr (buffer-list))))
  (ada-mode)

  (let (found ada-procedure-or-package-start-regexp)
    (if (set 'found
             (ada-search-ignore-string-comment ada-package-start-regexp nil))
        (progn (goto-char (cdr found))
               (insert " body")
               )
      (error "No package"))

    (set 'ada-procedure-or-package-start-regexp
         (concat ada-procedure-start-regexp
                 "\\|"
                 ada-package-start-regexp))

    (while (set 'found
                (ada-search-ignore-string-comment
                 ada-procedure-or-package-start-regexp nil))
      (progn
        (goto-char (car found))
        (if (looking-at ada-package-start-regexp)
            (progn (goto-char (cdr found))
                   (insert " body"))
          (ada-gen-treat-proc found))))))

(defun ada-make-subprogram-body ()
  "make one dummy subprogram body from spec surrounding point"
  (interactive)
  (let* ((found (re-search-backward ada-procedure-start-regexp nil t))
         (spec  (match-beginning 0)))
    (if found
        (progn
          (goto-char spec)
          (if (and (re-search-forward "(\\|;" nil t)
                   (= (char-before) ?\())
              (progn
                (ada-search-ignore-string-comment ")" nil)
                (ada-search-ignore-string-comment ";" nil)))
          (set 'spec (buffer-substring spec (point)))

	  ;; If find-file.el was available, use its functions
	  (if (functionp 'ff-get-file)
	      (find-file (ff-get-file
			  ff-search-directories
			  (ada-make-filename-from-adaname
			   (file-name-nondirectory
			    (file-name-sans-extension (buffer-name))))
			  ada-body-suffixes))
	    ;; Else emulate it very simply
	    (find-file (concat (ada-make-filename-from-adaname
				(file-name-nondirectory
				 (file-name-sans-extension (buffer-name))))
			       ".adb")))
	    
          (save-restriction
            (widen)
            (goto-char (point-max))
            (forward-comment -10000)
            (re-search-backward "\\<end\\>" nil t)
            ;;  Move to the beginning of the elaboration part, if any
            (re-search-backward "^begin" nil t)
            (newline)
            (forward-char -1)
            (insert spec)
            (re-search-backward ada-procedure-start-regexp nil t)
            (ada-gen-treat-proc (cons (match-beginning 0) (match-end 0)))
            ))
      (error "Not in subprogram spec"))))

;;  Create the keymap once and for all. If we do that in ada-mode,
;;  the keys changed in the user's .emacs have to be modified
;;  every time
(ada-create-keymap)
(ada-create-menu)

;;  Create the syntax tables, but do not activate them
(ada-create-syntax-table)

;;  Add the default extensions (and set up speedbar)
(ada-add-extensions ".ads" ".adb")
;; This two files are generated by GNAT when running with -gnatD
(if (equal ada-which-compiler 'gnat)
    (ada-add-extensions ".ads.dg" ".adb.dg"))

;;  Read the special cases for exceptions
(ada-case-read-exceptions)

;; include the other ada-mode files

(if (equal ada-which-compiler 'gnat)
    (progn
      ;; The order here is important: ada-xref defines the Project
      ;; submenu, and ada-prj adds to it.
      (condition-case nil  (require 'ada-prj) (error nil))
      (require 'ada-xref)
      ))
(condition-case nil (require 'ada-stmt) (error nil))

;;; provide ourselves
(provide 'ada-mode)

;;; ada-mode.el ends here

