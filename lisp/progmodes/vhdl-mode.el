;;; vhdl-mode.el --- major mode for editing VHDL code

;; Copyright (C) 1992,93,94,95,96,97,98,99 Free Software Foundation, Inc.

;; Authors:     Reto Zimmermann               <mailto:Reto.Zimmermann@iaeth.ch>
;;                                          <http://www.iis.ee.ethz.ch/~zimmi/>
;;              Rodney J. Whitby                 <mailto:rwhitby@geocities.com>
;;                          <http://www.geocities.com/SiliconValley/Park/8287/>
;; Maintainer:  VHDL Mode Maintainers                 <vhdl-mode@geocities.com>
;;                         <http://www.geocities.com/SiliconValley/Peaks/8287/>
;; Version:     3.29
;; Keywords:    languages vhdl

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This package provides an Emacs major mode for editing VHDL code.
;; It includes the following features:

;;   - Highlighting of VHDL syntax
;;   - Indentation based on versatile syntax analysis
;;   - Template insertion (electrification) for most VHDL constructs
;;   - Insertion of customizable VHDL file headers
;;   - Insertion of user-specified models
;;   - Word completion (dynamic abbreviations)
;;   - Comprehensive menu
;;   - File browser (using Speedbar or index/sources menu)
;;   - Design hierarchy browser (using Speedbar)
;;   - Source file compilation (syntax analysis)
;;   - Postscript printing with fontification
;;   - Lower and upper case keywords
;;   - Hiding code of design units
;;   - Code beautification
;;   - Port translation and test bench generation
;;   - VHDL'87/'93 and VHDL-AMS supported
;;   - Fully customizable
;;   - Works under GNU Emacs (Unix and Windows NT/95) and XEmacs
;;     (GNU Emacs is preferred due to higher robustness and functionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Usage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; see below (comment in `vhdl-mode' function) or type `C-c C-h' in Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Versions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; supported: Emacs 20.X (Unix and Windows NT/95), XEmacs 20.X
;; tested on: Emacs 20.3, XEmacs 20.4 (marginally)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Acknowledgements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Electrification ideas by Bob Pack <rlpst@cislabs.pitt.edu>
;; and Steve Grout.

;; Fontification approach suggested by Ken Wood <ken@eda.com.au>.
;; Ideas about alignment from John Wiegley <johnw@borland.com>.

;; Many thanks to all the users who sent me bug reports and enhancement
;; requests.  Colin Marquardt, will you never stop asking for new features :-?
;; Thanks to Dan Nicolaescu for reviewing the code and for his valuable hints.
;; Thanks to Ulf Klaperski for the indentation speedup hint.

;; Special thanks go to Wolfgang Fichtner and the crew from the Integrated
;; Systems Laboratory, Swiss Federal Institute of Technology Zurich, for
;; giving me the opportunity to develop this code.
;; This work has been funded in part by MICROSWISS, a Microelectronics Program
;; of the Swiss Government.


;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; help function
(defun vhdl-custom-set (variable value &rest functions)
  "Set variables as in `custom-set-default' and call FUNCTIONS afterwards."
  (if (fboundp 'custom-set-default)
      (custom-set-default variable value)
    (set-default variable value))
  (while functions
    (when (fboundp (car functions)) (funcall (car functions)))
    (setq functions (cdr functions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User variables

(defgroup vhdl nil
  "Customizations for VHDL Mode."
  :prefix "vhdl-"
  :group 'languages
  :version "20.4"  ; comment out for XEmacs
  )

(defgroup vhdl-mode nil
  "Customizations for modes."
  :group 'vhdl)

(defcustom vhdl-electric-mode t
  "*Non-nil enables electrification (automatic template generation).
If nil, template generators can still be invoked through key bindings and
menu.  Is indicated in the modeline by `/e' after the mode name and can be
toggled by `\\[vhdl-electric-mode]'."
  :type 'boolean
  :group 'vhdl-mode)

(defcustom vhdl-stutter-mode t
  "*Non-nil enables stuttering.
Is indicated in the modeline by `/s' after the mode name and can be toggled
by `\\[vhdl-stutter-mode]'."
  :type 'boolean
  :group 'vhdl-mode)

(defcustom vhdl-indent-tabs-mode nil
  "*Non-nil means indentation can insert tabs.
Overrides local variable `indent-tabs-mode'."
  :type 'boolean
  :group 'vhdl-mode)


(defgroup vhdl-project nil
  "Customizations for projects."
  :group 'vhdl)

(defcustom vhdl-project-alist
  '(("example 1" "Project with individual source files"
     ("~/example1/vhdl/system.vhd" "~/example1/vhdl/component_*.vhd") "\
-------------------------------------------------------------------------------
-- This is a multi-line project description
-- that can be used as a project dependent part of the file header.
")
    ("example 2" "Project where source files are located in two directories"
     ("$EXAMPLE2/vhdl/components/" "$EXAMPLE2/vhdl/system/") "")
    ("example 3" "Project where source files are located in some directory trees"
     ("-r ~/example3/*/vhdl/") ""))
  "*List of projects and their properties.
  Name       : name of project
  Title      : title of project (one-line string)
  Sources    : a) source files  : path + \"/\" + file name
               b) directory     : path + \"/\"
               c) directory tree: \"-r \" + path + \"/\"
  Description: description of project (multi-line string)

Project name and description are used to insert into the file header (see
variable `vhdl-file-header').

Path and file name can contain wildcards `*' and `?'.  Environment variables
\(e.g. \"$EXAMPLE2\") are resolved.

The hierarchy browser shows the hierarchy of the design units found in
`Sources'.  If no directories or files are specified, the current directory is
shown.

NOTE: Reflect the new setting in the choice list of variable `vhdl-project'
      by restarting Emacs."
  :type '(repeat (list :tag "Project" :indent 2
		       (string :tag "Name ")
		       (string :tag "Title")
		       (repeat :tag "Sources" :indent 4
			       (string :format "%v"))
		       (string :tag "Description: (type `C-j' for newline)"
			       :format "%t\n%v")))
  :set (lambda (variable value)
         (vhdl-custom-set variable value 'vhdl-update-mode-menu))
  :group 'vhdl-project)

(defcustom vhdl-project ""
  "*Specifies the default for the current project.
Select a project name from the ones defined in variable `vhdl-project-alist'.
Is used to determine the project title and description to be inserted in file
headers and the source files/directories to be scanned in the hierarchy
browser.  The current project can also be changed temporarily in the menu."
  :type (let ((project-alist vhdl-project-alist) choice-list)
	  (while project-alist
	    (setq choice-list (cons (list 'const (car (car project-alist)))
				    choice-list))
	    (setq project-alist (cdr project-alist)))
	  (append '(choice (const :tag "None" "") (const :tag "--"))
		  (nreverse choice-list)))
  :group 'vhdl-project)


(defgroup vhdl-compile nil
  "Customizations for compilation."
  :group 'vhdl)

(defcustom vhdl-compiler-alist
  '(
    ;; Cadence Design Systems: cv -file test.vhd
    ;; duluth: *E,430 (test.vhd,13): identifier (POSITIV) is not declared
    ("Cadence" "cv -file" "" "" "./"
     ("duluth: \\*E,[0-9]+ (\\(.+\\),\\([0-9]+\\)):" 1 2) ("" 0))
    ;; Ikos Voyager: analyze test.vhd
    ;; analyze sdrctl.vhd
    ;; E L4/C5:        this library unit is inaccessible
    ("Ikos" "analyze" "" "" "./"
     ("E L\\([0-9]+\\)/C[0-9]+:" 0 1)
     ("^analyze +\\(.+ +\\)*\\(.+\\)$" 2))
    ;; ModelSim, Model Technology: vcom test.vhd
    ;; ERROR: test.vhd(14): Unknown identifier: positiv
    ;; WARNING[2]: test.vhd(85): Possible infinite loop
    ("ModelSim" "vcom" "" "vmake > Makefile" "./"
     ("\\(ERROR\\|WARNING\\)[^:]*: \\(.+\\)(\\([0-9]+\\)):" 2 3) ("" 0))
    ;; QuickHDL, Mentor Graphics: qvhcom test.vhd
    ;; ERROR: test.vhd(24): near "dnd": expecting: END
    ;; WARNING[4]: test.vhd(30): A space is required between ...
    ("QuickHDL" "qvhcom" "" "qhmake >! Makefile" "./"
     ("\\(ERROR\\|WARNING\\)[^:]*: \\(.+\\)(\\([0-9]+\\)):" 2 3) ("" 0))
    ;; Synopsys, VHDL Analyzer: vhdlan test.vhd
    ;; **Error: vhdlan,703 test.vhd(22): OTHERS is not legal in this context.
    ("Synopsys" "vhdlan" "" "" "./"
     ("\\*\\*Error: vhdlan,[0-9]+ \\(.+\\)(\\([0-9]+\\)):" 1 2) ("" 0))
    ;; Vantage: analyze -libfile vsslib.ini -src test.vhd
    ;;     Compiling "pcu.vhd" line 1...
    ;; **Error: LINE 499 *** No aggregate value is valid in this context.
    ("Vantage" "analyze -libfile vsslib.ini -src" "" "" "./"
     ("\\*\\*Error: LINE \\([0-9]+\\) \\*\\*\\*" 0 1)
     ("^ *Compiling \"\\(.+\\)\" " 1))
    ;; Viewlogic: analyze -libfile vsslib.ini -src test.vhd
    ;;     Compiling "pcu.vhd" line 1...
    ;; **Error: LINE 499 *** No aggregate value is valid in this context.
    ("Viewlogic" "analyze -libfile vsslib.ini -src" "" "" "./"
     ("\\*\\*Error: LINE \\([0-9]+\\) \\*\\*\\*" 0 1)
     ("^ *Compiling \"\\(.+\\)\" " 1))
    )
  "*List of available VHDL compilers and their properties.
Each list entry specifies the following items for a compiler:
Compiler:
  Compiler Name    : name used in variable `vhdl-compiler' to choose compiler
  Compile Command  : command including options used for syntax analysis
  Make Command     : command including options used instead of `make' (default)
  Generate Makefile: command to generate a Makefile (used by `make' command)
  From Directory   : directory where compilation is run (must end with '/')
Error Message:
  Regexp           : regular expression to match error messages
  File Subexp Index: index of subexpression that matches the file name
  Line Subexp Index: index of subexpression that matches the line number
File Message:
  Regexp           : regular expression to match a file name message
  File Subexp Index: index of subexpression that matches the file name

See also variable `vhdl-compiler-options' to add options to the compile
command.

Some compilers do not include the file name in the error message, but print
out a file name message in advance.  In this case, set \"File Subexp Index\"
to 0 and fill out the \"File Message\" entries.

A compiler is selected for syntax analysis (`\\[vhdl-compile]') by
assigning its name to variable `vhdl-compiler'.

NOTE: Reflect the new setting in the choice list of variable `vhdl-compiler'
      by restarting Emacs."
  :type '(repeat (list :tag "Compiler" :indent 2
		       (string :tag "Compiler Name    ")
		       (string :tag "Compile Command  ")
		       (string :tag "Make Command     ")
		       (string :tag "Generate Makefile")
		       (string :tag "From Directory   " "./")
		       (list :tag "Error Message" :indent 4
			     (regexp  :tag "Regexp           ")
			     (integer :tag "File Subexp Index")
			     (integer :tag "Line Subexp Index"))
		       (list :tag "File Message" :indent 4
			     (regexp  :tag "Regexp           ")
			     (integer :tag "File Subexp Index"))))
  :set (lambda (variable value)
         (vhdl-custom-set variable value 'vhdl-update-mode-menu))
  :group 'vhdl-compile)

(defcustom vhdl-compiler "ModelSim"
  "*Specifies the VHDL compiler to be used for syntax analysis.
Select a compiler name from the ones defined in variable `vhdl-compiler-alist'."
  :type (let ((compiler-alist vhdl-compiler-alist) choice-list)
	  (while compiler-alist
	    (setq choice-list (cons (list 'const (car (car compiler-alist)))
				    choice-list))
	    (setq compiler-alist (cdr compiler-alist)))
	  (append '(choice) (nreverse choice-list)))
  :group 'vhdl-compile)

(defcustom vhdl-compiler-options ""
 "*Options to be added to the compile command."
  :type 'string
  :group 'vhdl-compile)


(defgroup vhdl-style nil
  "Customizations for code styles."
  :group 'vhdl)

(defcustom vhdl-standard '(87 nil)
  "*VHDL standards used.
Basic standard:
  VHDL'87      : IEEE Std 1076-1987
  VHDL'93      : IEEE Std 1076-1993
Additional standards:
  VHDL-AMS     : IEEE Std 1076.1 (analog-mixed-signal)
  Math Packages: IEEE Std 1076.2 (`math_real', `math_complex')

NOTE: Activate the new setting in a VHDL buffer using the menu entry
      \"Activate New Customizations\"."
  :type '(list (choice :tag "Basic standard"
		       (const :tag "VHDL'87" 87)
		       (const :tag "VHDL'93" 93))
	       (set :tag "Additional standards" :indent 2
		    (const :tag "VHDL-AMS" ams)
		    (const :tag "Math Packages" math)))
  :set (lambda (variable value)
         (vhdl-custom-set variable value
			  'vhdl-template-map-init
			  'vhdl-mode-abbrev-table-init
			  'vhdl-template-construct-alist-init
			  'vhdl-template-package-alist-init
			  'vhdl-update-mode-menu
			  'vhdl-words-init 'vhdl-font-lock-init))
  :group 'vhdl-style)

(defcustom vhdl-basic-offset 2
  "*Amount of basic offset used for indentation.
This value is used by + and - symbols in `vhdl-offsets-alist'."
  :type 'integer
  :group 'vhdl-style)

(defcustom vhdl-upper-case-keywords nil
  "*Non-nil means convert keywords to upper case.
This is done when typed or expanded or by the fix case functions."
  :type 'boolean
  :set (lambda (variable value)
         (vhdl-custom-set variable value 'vhdl-abbrev-list-init))
  :group 'vhdl-style)

(defcustom vhdl-upper-case-types nil
  "*Non-nil means convert standardized types to upper case.
This is done when expanded or by the fix case functions."
  :type 'boolean
  :set (lambda (variable value)
         (vhdl-custom-set variable value 'vhdl-abbrev-list-init))
  :group 'vhdl-style)

(defcustom vhdl-upper-case-attributes nil
  "*Non-nil means convert standardized attributes to upper case.
This is done when expanded or by the fix case functions."
  :type 'boolean
  :set (lambda (variable value)
         (vhdl-custom-set variable value 'vhdl-abbrev-list-init))
  :group 'vhdl-style)

(defcustom vhdl-upper-case-enum-values nil
  "*Non-nil means convert standardized enumeration values to upper case.
This is done when expanded or by the fix case functions."
  :type 'boolean
  :set (lambda (variable value)
         (vhdl-custom-set variable value 'vhdl-abbrev-list-init))
  :group 'vhdl-style)

(defcustom vhdl-upper-case-constants t
  "*Non-nil means convert standardized constants to upper case.
This is done when expanded."
  :type 'boolean
  :set (lambda (variable value)
         (vhdl-custom-set variable value 'vhdl-abbrev-list-init))
  :group 'vhdl-style)


(defgroup vhdl-electric nil
  "Customizations for electrification."
  :group 'vhdl)

(defcustom vhdl-electric-keywords '(vhdl user)
  "*Type of keywords for which electrification is enabled.
  VHDL keywords: invoke built-in templates
  User keywords: invoke user models (see variable `vhdl-model-alist')"
  :type '(set (const :tag "VHDL keywords" vhdl)
	      (const :tag "User keywords" user))
  :set (lambda (variable value)
         (vhdl-custom-set variable value 'vhdl-mode-abbrev-table-init))
  :group 'vhdl-electric)

(defcustom vhdl-optional-labels 'process
  "*Constructs for which labels are to be queried.
Template generators prompt for optional labels for:
  None          : no constructs
  Processes only: processes only (also procedurals in VHDL-AMS)
  All constructs: all constructs with optional labels and keyword END"
  :type '(choice (const :tag "None" none)
		 (const :tag "Processes only" process)
		 (const :tag "All constructs" all))
  :group 'vhdl-electric)

(defcustom vhdl-insert-empty-lines 'unit
  "*Specifies whether to insert empty lines in some templates.
This improves readability of code.  Empty lines are inserted in:
  None             : no constructs
  Design units only: entities, architectures, configurations, packages only
  All constructs   : also all constructs with BEGIN...END parts

Replaces variable `vhdl-additional-empty-lines'."
  :type '(choice (const :tag "None" none)
		 (const :tag "Design units only" unit)
		 (const :tag "All constructs" all))
  :group 'vhdl-electric)

(defcustom vhdl-argument-list-indent nil
  "*Non-nil means indent argument lists relative to opening parenthesis.
That is, argument, association, and port lists start on the same line as the
opening parenthesis and subsequent lines are indented accordingly.
Otherwise, lists start on a new line and are indented as normal code."
  :type 'boolean
  :group 'vhdl-electric)

(defcustom vhdl-association-list-with-formals t
  "*Non-nil means write association lists with formal parameters.
In templates, you are prompted for formal and actual parameters.
If nil, only a list of actual parameters is entered."
  :type 'boolean
  :group 'vhdl-electric)

(defcustom vhdl-conditions-in-parenthesis nil
  "*Non-nil means place parenthesis around condition expressions."
  :type 'boolean
  :group 'vhdl-electric)

(defcustom vhdl-zero-string "'0'"
  "*String to use for a logic zero."
  :type 'string
  :group 'vhdl-electric)

(defcustom vhdl-one-string "'1'"
  "*String to use for a logic one."
  :type 'string
  :group 'vhdl-electric)


(defgroup vhdl-header nil
  "Customizations for file header."
  :group 'vhdl-electric)

(defcustom vhdl-file-header "\
-------------------------------------------------------------------------------
-- Title      : <title string>
-- Project    : <project>
-------------------------------------------------------------------------------
-- File       : <filename>
-- Author     : <author>
-- Company    : <company>
-- Last update: <date>
-- Platform   : <platform>
<projectdesc>-------------------------------------------------------------------------------
-- Description: <cursor>
-------------------------------------------------------------------------------
-- Revisions  :
-- Date        Version  Author  Description
-- <date>  1.0      <login>\tCreated
-------------------------------------------------------------------------------

"
  "*String or file to insert as file header.
If the string specifies an existing file name, the contents of the file is
inserted, otherwise the string itself is inserted as file header.
Type `C-j' for newlines.
If the header contains RCS keywords, they may be written as <RCS>Keyword<RCS>
if the header needs to be version controlled.

The following keywords for template generation are supported:
  <filename>   : replaced by the name of the buffer
  <author>     : replaced by the user name and email address (customize
                 `mail-host-address' or `user-mail-address' if required)
  <login>      : replaced by user login name
  <company>    : replaced by contents of variable `vhdl-company-name'
  <date>       : replaced by the current date
  <project>    : replaced by title of current project (`vhdl-project')
  <projectdesc>: replaced by description of current project (`vhdl-project')
  <platform>   : replaced by contents of variable `vhdl-platform-spec'
  <... string> : replaced by a queried string (... is the prompt word)
  <cursor>     : final cursor position

The (multi-line) project description <projectdesc> can be used as a project
dependent part of the file header and can also contain the above keywords."
  :type 'string
  :group 'vhdl-header)

(defcustom vhdl-file-footer ""
  "*String or file to insert as file footer.
If the string specifies an existing file name, the contents of the file is
inserted, otherwise the string itself is inserted as file footer (i.e. at
the end of the file).
Type `C-j' for newlines."
  :type 'string
  :group 'vhdl-header)

(defcustom vhdl-company-name ""
  "*Name of company to insert in file header."
  :type 'string
  :group 'vhdl-header)

(defcustom vhdl-platform-spec ""
  "*Specification of VHDL platform to insert in file header.
The platform specification should contain names and versions of the
simulation and synthesis tools used."
  :type 'string
  :group 'vhdl-header)

(defcustom vhdl-date-format "%Y/%m/%d"
  "*Specifies the date format to use in the header.
This string is passed as argument to the command `format-time-string'.
For more information on format strings, see the documentation for the
`format-time-string' command (C-h f `format-time-string')."
  :type 'string
  :group 'vhdl-header)

(defcustom vhdl-modify-date-prefix-string "-- Last update: "
  "*Prefix string of modification date in VHDL file header.
If actualization of the modification date is called (menu,
`\\[vhdl-template-modify]'), this string is searched and the rest
of the line replaced by the current date."
  :type 'string
  :group 'vhdl-header)

(defcustom vhdl-modify-date-on-saving t
  "*Non-nil means update the modification date when the buffer is saved.
Calls function `\\[vhdl-template-modify]').

NOTE: Activate the new setting in a VHDL buffer using the menu entry
      \"Activate New Customizations\""
  :type 'boolean
  :group 'vhdl-header)


(defgroup vhdl-sequential-process nil
  "Customizations for sequential processes."
  :group 'vhdl-electric)

(defcustom vhdl-reset-kind 'async
  "*Specifies which kind of reset to use in sequential processes."
  :type '(choice (const :tag "None" none)
		 (const :tag "Synchronous" sync)
		 (const :tag "Asynchronous" async))
  :group 'vhdl-sequential-process)

(defcustom vhdl-reset-active-high nil
  "*Non-nil means reset in sequential processes is active high.
Nil means active low."
  :type 'boolean
  :group 'vhdl-sequential-process)

(defcustom vhdl-clock-rising-edge t
  "*Non-nil means rising edge of clock triggers sequential processes.
Nil means falling edge."
  :type 'boolean
  :group 'vhdl-sequential-process)

(defcustom vhdl-clock-edge-condition 'standard
  "*Syntax of the clock edge condition.
  Standard: \"clk'event and clk = '1'\"
  Function: \"rising_edge(clk)\""
  :type '(choice (const :tag "Standard" standard)
		 (const :tag "Function" function))
  :group 'vhdl-sequential-process)

(defcustom vhdl-clock-name ""
  "*Name of clock signal to use in templates."
  :type 'string
  :group 'vhdl-sequential-process)

(defcustom vhdl-reset-name ""
  "*Name of reset signal to use in templates."
  :type 'string
  :group 'vhdl-sequential-process)


(defgroup vhdl-model nil
  "Customizations for user models."
  :group 'vhdl)

(defcustom vhdl-model-alist
  '(("example model"
     "<label> : process (<clock>, <reset>)
begin  -- process <label>
  if <reset> = '0' then  -- asynchronous reset (active low)
    <cursor>
  elsif <clock>'event and <clock> = '1' then  -- rising clock edge
    if <enable> = '1' then  -- synchronous load
      
    end if;
  end if;
end process <label>;"
     "e" ""))
  "*List of user models.
VHDL models (templates) can be specified by the user in this list.  They can be
invoked from the menu, through key bindings (`C-c C-m ...'), or by keyword
electrification (i.e. overriding existing or creating new keywords, see
variable `vhdl-electric-keywords').
  Name       : name of model (string of words and spaces)
  String     : string or name of file to be inserted as model (newline: `C-j')
  Key Binding: key binding to invoke model, added to prefix `C-c C-m'
                (must be in double-quotes, examples: \"i\", \"\\C-p\", \"\\M-s\")
  Keyword    : keyword to invoke model

The models can contain prompts to be queried.  A prompt is of the form \"<...>\".
A prompt that appears several times is queried once and replaced throughout
the model.  Special prompts are:
  <clock> : name specified in `vhdl-clock-name' (if not empty)
  <reset> : name specified in `vhdl-reset-name' (if not empty)
  <cursor>: final cursor position

If the string specifies an existing file name, the contents of the file is
inserted, otherwise the string itself is inserted.
The code within the models should be correctly indented.
Type `C-j' for newlines.

NOTE: Activate the new setting in a VHDL buffer using the menu entry
      \"Activate New Customizations\""
  :type '(repeat (list :tag "Model" :indent 2
		       (string :tag "Name       ")
		       (string :tag "String     : (type `C-j' for newline)"
			       :format "%t\n%v")
		       (sexp   :tag "Key Binding" x)
		       (string :tag "Keyword    ")))
  :set (lambda (variable value)
         (vhdl-custom-set variable value
			  'vhdl-model-map-init
			  'vhdl-model-defun
			  'vhdl-mode-abbrev-table-init
			  'vhdl-update-mode-menu))
  :group 'vhdl-model)

(defgroup vhdl-port nil
  "Customizations for port transformation functions."
  :group 'vhdl)

(defcustom vhdl-include-port-comments nil
  "*Non-nil means include port comments when a port is pasted."
  :type 'boolean
  :group 'vhdl-port)

(defcustom vhdl-include-direction-comments nil
  "*Non-nil means include signal direction in instantiations as comments."
  :type 'boolean
  :group 'vhdl-port)

(defconst vhdl-name-doc-string "

FROM REGEXP is a regular expression matching the formal port name:
  `.*'       matches the entire name
  `\\(...\\)'  matches a substring
TO STRING specifies the string to be inserted as actual port name:
  `\\&'  means substitute original matched text
  `\\N'  means substitute what matched the Nth `\\(...\\)'
Examples:
  `.*'           `\\&'    leaves name as it is
  `.*'           `\\&_i'  attaches `_i' to original name
  `\\(.*\\)_[io]$' `\\1'    strips off `_i' or `_o' from original name
  `.*'           `'      leaves name empty")

(defcustom vhdl-actual-port-name '(".*" . "\\&_i")
  (concat
   "*Specifies how actual port names are obtained from formal port names.
In a component instantiation, an actual port name can be obtained by
modifying the formal port name (e.g. attaching or stripping off a substring)."
   vhdl-name-doc-string)
  :type '(cons (regexp :tag "From Regexp")
	       (string :tag "To String  "))
  :group 'vhdl-port)

(defcustom vhdl-instance-name '(".*" . "")
  (concat
   "*Specifies how an instance name is obtained.
The instance name can be obtained by modifying the name of the component to be
instantiated (e.g. attaching or stripping off a substring).
If TO STRING is empty, the instance name is queried."
   vhdl-name-doc-string)
  :type '(cons (regexp :tag "From Regexp")
	       (string :tag "To String  "))
  :group 'vhdl-port)

(defcustom vhdl-testbench-entity-name '(".*" . "\\&_tb")
  (concat
   "*Specifies how the test bench entity name is obtained.
The entity name of a test bench can be obtained by modifying the name of
the component to be tested (e.g. attaching or stripping off a substring)."
   vhdl-name-doc-string)
  :type '(cons (regexp :tag "From Regexp")
	       (string :tag "To String  "))
  :group 'vhdl-port)

(defcustom vhdl-testbench-architecture-name '(".*" . "")
  (concat
   "*Specifies how the test bench architecture name is obtained.
The test bench architecture name can be obtained by modifying the name of
the component to be tested (e.g. attaching or stripping off a substring).
If TO STRING is empty, the architecture name is queried."
   vhdl-name-doc-string)
  :type '(cons (regexp :tag "From Regexp")
	       (string :tag "To String  "))
  :group 'vhdl-port)

(defcustom vhdl-testbench-dut-name '(".*" . "DUT")
  (concat
   "*Specifies how a DUT instance name is obtained.
The design-under-test instance name (i.e. the component instantiated in the
test bench) can be obtained by modifying the component name (e.g. attaching
or stripping off a substring)."
   vhdl-name-doc-string)
  :type '(cons (regexp :tag "From Regexp")
	       (string :tag "To String  "))
  :group 'vhdl-port)

(defcustom vhdl-testbench-entity-header ""
  "*String or file to be inserted as test bench entity header.
If the string specifies an existing file name, the contents of the file is
inserted, otherwise the string itself is inserted at the beginning of the test
bench entity template.
Type `C-j' for newlines."
  :type 'string
  :group 'vhdl-port)

(defcustom vhdl-testbench-architecture-header ""
  "*String or file to be inserted as test bench architecture header.
If the string specifies an existing file name, the contents of the file is
inserted, otherwise the string itself is inserted at the beginning of the test
bench architecture template, if a separate file is created for the
architecture.
Type `C-j' for newlines."
  :type 'string
  :group 'vhdl-port)

(defcustom vhdl-testbench-declarations ""
  "*String or file to be inserted in the test bench declarative part.
If the string specifies an existing file name, the contents of the file is
inserted, otherwise the string itself is inserted in the test bench
architecture before the BEGIN keyword.
Type `C-j' for newlines."
  :type 'string
  :group 'vhdl-port)

(defcustom vhdl-testbench-statements ""
  "*String or file to be inserted in the test bench statement part.
If the string specifies an existing file name, the contents of the file is
inserted, otherwise the string itself is inserted in the test bench
architecture before the END keyword.
Type `C-j' for newlines."
  :type 'string
  :group 'vhdl-port)

(defcustom vhdl-testbench-initialize-signals nil
  "*Non-nil means initialize signals with `0' when declared in test bench."
  :type 'boolean
  :group 'vhdl-port)

(defcustom vhdl-testbench-create-files 'single
  "*Specifies whether new files should be created for the test bench.
Test bench entity and architecture are inserted:
  None          : in current buffer
  Single file   : in new single file
  Separate files: in two separate files
Note that the files have the same name as the contained design unit."
  :type '(choice (const :tag "None" none)
		 (const :tag "Single file" single)
		 (const :tag "Separate files" separate))
  :group 'vhdl-port)


(defgroup vhdl-comment nil
  "Customizations for comments."
  :group 'vhdl)

(defcustom vhdl-self-insert-comments t
  "*Non-nil means various templates automatically insert help comments."
  :type 'boolean
  :group 'vhdl-comment)

(defcustom vhdl-prompt-for-comments t
  "*Non-nil means various templates prompt for user definable comments."
  :type 'boolean
  :group 'vhdl-comment)

(defcustom vhdl-inline-comment-column 40
  "*Column to indent inline comments to.
Overrides local variable `comment-column'.

NOTE: Activate the new setting in a VHDL buffer using the menu entry
      \"Activate New Customizations\""
  :type 'integer
  :group 'vhdl-comment)

(defcustom vhdl-end-comment-column 79
  "*End of comment column.
Comments that exceed this column number are wrapped.

NOTE: Activate the new setting in a VHDL buffer using the menu entry
      \"Activate New Customizations\""
  :type 'integer
  :group 'vhdl-comment)

(defvar end-comment-column)


(defgroup vhdl-align nil
  "Customizations for alignment."
  :group 'vhdl)

(defcustom vhdl-auto-align t
  "*Non-nil means align some templates automatically after generation."
  :type 'boolean
  :group 'vhdl-align)

(defcustom vhdl-align-groups t
  "*Non-nil means align groups of code lines separately.
A group of code lines is a region of lines with no empty lines inbetween."
  :type 'boolean
  :group 'vhdl-align)


(defgroup vhdl-highlight nil
  "Customizations for highlighting."
  :group 'vhdl)

(defcustom vhdl-highlight-keywords t
  "*Non-nil means highlight VHDL keywords and other standardized words.
The following faces are used:
  `font-lock-keyword-face'        : keywords
  `font-lock-type-face'           : standardized types
  `vhdl-font-lock-attribute-face' : standardized attributes
  `vhdl-font-lock-enumvalue-face' : standardized enumeration values
  `vhdl-font-lock-function-face'  : standardized function and package names

NOTE: Activate the new setting in a VHDL buffer by re-fontifying it (menu
      entry \"Fontify Buffer\").  XEmacs: turn off and on font locking."
  :type 'boolean
  :set (lambda (variable value)
         (vhdl-custom-set variable value 'vhdl-font-lock-init))
  :group 'vhdl-highlight)

(defcustom vhdl-highlight-names t
  "*Non-nil means highlight declaration names and construct labels.
The following faces are used:
  `font-lock-function-name-face'  : names in declarations of units,
     subprograms, components, as well as labels of VHDL constructs
  `font-lock-type-face'           : names in type/nature declarations
  `vhdl-font-lock-attribute-face' : names in attribute declarations
  `font-lock-variable-name-face'  : names in declarations of signals,
     variables, constants, subprogram parameters, generics, and ports

NOTE: Activate the new setting in a VHDL buffer by re-fontifying it (menu
      entry \"Fontify Buffer\").  XEmacs: turn off and on font locking."
  :type 'boolean
  :set (lambda (variable value)
         (vhdl-custom-set variable value 'vhdl-font-lock-init))
  :group 'vhdl-highlight)

(defcustom vhdl-highlight-special-words nil
  "*Non-nil means highlight words with special syntax.
The words with syntax and color specified in variable
`vhdl-special-syntax-alist' are highlighted accordingly.
Can be used for visual support of naming conventions.

NOTE: Activate the new setting in a VHDL buffer by re-fontifying it (menu
      entry \"Fontify Buffer\").  XEmacs: turn off and on font locking."
  :type 'boolean
  :set (lambda (variable value)
         (vhdl-custom-set variable value 'vhdl-font-lock-init))
  :group 'vhdl-highlight)

(defcustom vhdl-highlight-forbidden-words nil
  "*Non-nil means highlight forbidden words.
The reserved words specified in variable `vhdl-forbidden-words' or having the
syntax specified in variable `vhdl-forbidden-syntax' are highlighted in a
warning color (face `vhdl-font-lock-reserved-words-face') to indicate not to
use them.

NOTE: Activate the new setting in a VHDL buffer by re-fontifying it (menu
      entry \"Fontify Buffer\").  XEmacs: turn off and on font locking."
  :type 'boolean
  :set (lambda (variable value)
         (vhdl-custom-set variable value
			  'vhdl-words-init 'vhdl-font-lock-init))
  :group 'vhdl-highlight)

(defcustom vhdl-highlight-verilog-keywords nil
  "*Non-nil means highlight Verilog keywords as reserved words.
Verilog keywords are highlighted in a warning color (face
`vhdl-font-lock-reserved-words-face') to indicate not to use them.

NOTE: Activate the new setting in a VHDL buffer by re-fontifying it (menu
      entry \"Fontify Buffer\").  XEmacs: turn off and on font locking."
  :type 'boolean
  :set (lambda (variable value)
         (vhdl-custom-set variable value
			  'vhdl-words-init 'vhdl-font-lock-init))
  :group 'vhdl-highlight)

(defcustom vhdl-highlight-translate-off nil
  "*Non-nil means background-highlight code excluded from translation.
That is, all code between \"-- pragma translate_off\" and
\"-- pragma translate_on\" is highlighted using a different background color
\(face `vhdl-font-lock-translate-off-face').
Note: this might slow down on-the-fly fontification (and thus editing).

NOTE: Activate the new setting in a VHDL buffer by re-fontifying it (menu
      entry \"Fontify Buffer\").  XEmacs: turn off and on font locking."
  :type 'boolean
  :set (lambda (variable value)
         (vhdl-custom-set variable value 'vhdl-font-lock-init))
  :group 'vhdl-highlight)

(defcustom vhdl-highlight-case-sensitive nil
  "*Non-nil means consider case for highlighting.
Possible trade-off:
  non-nil  also upper-case VHDL words are highlighted, but case of words with
           special syntax is not considered
  nil      only lower-case VHDL words are highlighted, but case of words with
           special syntax is considered
Overrides local variable `font-lock-keywords-case-fold-search'.

NOTE: Activate the new setting in a VHDL buffer by re-fontifying it (menu
      entry \"Fontify Buffer\").  XEmacs: turn off and on font locking."
  :type 'boolean
  :group 'vhdl-highlight)

(defcustom vhdl-special-syntax-alist nil
  "*List of special syntax to be highlighted.
If variable `vhdl-highlight-special-words' is non-nil, words with the specified
syntax (as regular expression) are highlighted in the corresponding color.

  Name         : string of words and spaces
  Regexp       : regular expression describing word syntax
                  (e.g. \"\\\w+_c\" matches word with suffix \"_c\")
  Color (light): foreground color for light background
                 (matching color examples: Gold3, Grey50, LimeGreen, Tomato,
                 LightSeaGreen, DodgerBlue, Gold, PaleVioletRed)
  Color (dark) : foreground color for dark background
                 (matching color examples: BurlyWood1, Grey80, Green, Coral,
                 AquaMarine2, LightSkyBlue1, Yellow, PaleVioletRed1)

Can be used for visual support of naming conventions, such as highlighting
different kinds of signals (e.g. \"Clk_c\", \"Rst_r\") or objects (e.g.
\"Signal_s\", \"Variable_v\", \"Constant_c\") by distinguishing them using
name suffices.
For each entry, a new face is generated with the specified colors and name
\"vhdl-font-lock-\" + name + \"-face\".

NOTE: Activate a changed regexp in a VHDL buffer by re-fontifying it (menu
      entry \"Fontify Buffer\").  XEmacs: turn off and on font locking.
      All other changes require restarting Emacs."
  :type '(repeat (list :tag "Face" :indent 2
		       (string :tag "Name         ")
		       (regexp :tag "Regexp       " "\\w+_")
		       (string :tag "Color (light)")
		       (string :tag "Color (dark) ")))
  :set (lambda (variable value)
         (vhdl-custom-set variable value 'vhdl-font-lock-init))
  :group 'vhdl-highlight)

(defcustom vhdl-forbidden-words '()
  "*List of forbidden words to be highlighted.
If variable `vhdl-highlight-forbidden-words' is non-nil, these reserved
words are highlighted in a warning color to indicate not to use them.

NOTE: Activate the new setting in a VHDL buffer by re-fontifying it (menu
      entry \"Fontify Buffer\").  XEmacs: turn off and on font locking."
  :type '(repeat (string :format "%v"))
  :set (lambda (variable value)
         (vhdl-custom-set variable value
			  'vhdl-words-init 'vhdl-font-lock-init))
  :group 'vhdl-highlight)

(defcustom vhdl-forbidden-syntax ""
  "*Syntax of forbidden words to be highlighted.
If variable `vhdl-highlight-forbidden-words' is non-nil, words with this
syntax are highlighted in a warning color to indicate not to use them.
Can be used to highlight too long identifiers (e.g. \"\\w\\w\\w\\w\\w\\w\\w\\w\\w\\w+\"
highlights identifiers with 10 or more characters).

NOTE: Activate the new setting in a VHDL buffer by re-fontifying it (menu
      entry \"Fontify Buffer\").  XEmacs: turn off and on font locking."
  :type 'regexp
  :set (lambda (variable value)
         (vhdl-custom-set variable value
			  'vhdl-words-init 'vhdl-font-lock-init))
  :group 'vhdl-highlight)


(defgroup vhdl-menu nil
  "Customizations for speedbar and menues."
  :group 'vhdl)

(defcustom vhdl-speedbar nil
  "*Non-nil means open the speedbar automatically at startup.
Alternatively, the speedbar can be opened from the VHDL menu."
  :type 'boolean
  :group 'vhdl-menu)

(defcustom vhdl-speedbar-show-hierarchy nil
  "*Non-nil means open the speedbar as hierarchy browser at startup.
Otherwise, the speedbar is opened as normal file browser."
  :type 'boolean
  :group 'vhdl-menu)

(defcustom vhdl-speedbar-hierarchy-indent 1
  "*Amount of indentation in hierarchy display of subcomponent."
  :type 'integer
  :group 'vhdl-menu)

(defcustom vhdl-index-menu nil
  "*Non-nil means add an index menu for a source file when loading.
Alternatively, the speedbar can be used.  Note that the index menu scans a file
when it is opened, while speedbar only scans the file upon request.
Does not work under XEmacs."
  :type 'boolean
  :group 'vhdl-menu)

(defcustom vhdl-source-file-menu nil
  "*Non-nil means add a menu of all source files in current directory.
Alternatively, the speedbar can be used."
  :type 'boolean
  :group 'vhdl-menu)

(defcustom vhdl-hideshow-menu nil
  "*Non-nil means add hideshow menu and functionality.
Hideshow allows hiding code of VHDL design units.
Does not work under XEmacs.

NOTE: Activate the new setting in a VHDL buffer using the menu entry
      \"Activate New Customizations\""
  :type 'boolean
  :group 'vhdl-menu)

(defcustom vhdl-hide-all-init nil
  "*Non-nil means hide all design units initially after a file is loaded."
  :type 'boolean
  :group 'vhdl-menu)


(defgroup vhdl-print nil
  "Customizations for printing."
  :group 'vhdl)

(defcustom vhdl-print-two-column t
  "*Non-nil means print code in two columns and landscape format.

NOTE: Activate the new setting by restarting Emacs.
      Overrides `ps-print' settings locally."
  :type 'boolean
  :group 'vhdl-print)

(defcustom vhdl-print-customize-faces t
  "*Non-nil means use an optimized set of faces for postscript printing.

NOTE: Activate the new setting by restarting Emacs.
      Overrides `ps-print' settings locally."
  :type 'boolean
  :group 'vhdl-print)


(defgroup vhdl-misc nil
  "Miscellaneous customizations."
  :group 'vhdl)

(defcustom vhdl-intelligent-tab t
  "*Non-nil means `TAB' does indentation, word completion and tab insertion.
That is, if preceeding character is part of a word then complete word,
else if not at beginning of line then insert tab,
else if last command was a `TAB' or `RET' then dedent one step,
else indent current line (i.e. `TAB' is bound to `vhdl-electric-tab').
If nil, TAB always indents current line (i.e. `TAB' is bound to
`vhdl-indent-line').

NOTE: Activate the new setting in a VHDL buffer using the menu entry
      \"Activate New Customizations\""
  :type 'boolean
  :group 'vhdl-misc)

(defcustom vhdl-word-completion-case-sensitive nil
  "*Non-nil means word completion using `TAB' is case sensitive.
That is, `TAB' completes words that start with the same letters and case.
Otherwise, case is ignored."
  :type 'boolean
  :group 'vhdl-misc)

(defcustom vhdl-word-completion-in-minibuffer t
  "*Non-nil enables word completion in minibuffer (for template prompts).

NOTE: Activate the new setting by restarting Emacs."
  :type 'boolean
  :group 'vhdl-misc)

(defcustom vhdl-underscore-is-part-of-word nil
  "*Non-nil means consider the underscore character `_' as part of word.
An identifier containing underscores is then treated as a single word in
select and move operations.  All parts of an identifier separated by underscore
are treated as single words otherwise.

NOTE: Activate the new setting in a VHDL buffer using the menu entry
      \"Activate New Customizations\""
  :type 'boolean
  :set (lambda (variable value)
         (vhdl-custom-set variable value 'vhdl-mode-syntax-table-init))
  :group 'vhdl-misc)


(defgroup vhdl-related nil
  "Related general customizations."
  :group 'vhdl)

;; add related general customizations
(custom-add-to-group 'vhdl-related 'line-number-mode 'custom-variable)
(if (string-match "XEmacs" emacs-version)
    (custom-add-to-group 'vhdl-related 'paren-mode 'custom-variable)
  (custom-add-to-group 'vhdl-related 'paren-showing 'custom-group))
(unless (string-match "XEmacs" emacs-version)
  (custom-add-to-group 'vhdl-related 'transient-mark-mode 'custom-variable))
(custom-add-to-group 'vhdl-related 'ps-print 'custom-group)
(custom-add-to-group 'vhdl-related 'mail-host-address 'custom-variable)
(custom-add-to-group 'vhdl-related 'user-mail-address 'custom-variable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables

(defconst vhdl-version "3.29"
  "VHDL Mode version number.")

(defvar vhdl-progress-interval 1
  "*Interval used to update progress status during long operations.
If a number, percentage complete gets updated after each interval of
that many seconds.   To inhibit all messages, set this variable to nil.")

(defvar vhdl-inhibit-startup-warnings-p nil
  "*If non-nil, inhibits start up compatibility warnings.")

(defvar vhdl-strict-syntax-p nil
  "*If non-nil, all syntactic symbols must be found in `vhdl-offsets-alist'.
If the syntactic symbol for a particular line does not match a symbol
in the offsets alist, an error is generated, otherwise no error is
reported and the syntactic symbol is ignored.")

(defvar vhdl-echo-syntactic-information-p nil
  "*If non-nil, syntactic info is echoed when the line is indented.")

(defconst vhdl-offsets-alist-default
  '((string                . -1000)
    (block-open            . 0)
    (block-close           . 0)
    (statement             . 0)
    (statement-cont        . vhdl-lineup-statement-cont)
    (statement-block-intro . +)
    (statement-case-intro  . +)
    (case-alternative      . +)
    (comment               . vhdl-lineup-comment)
    (arglist-intro         . +)
    (arglist-cont          . 0)
    (arglist-cont-nonempty . vhdl-lineup-arglist)
    (arglist-close         . vhdl-lineup-arglist)
    (entity                . 0)
    (configuration         . 0)
    (package               . 0)
    (architecture          . 0)
    (package-body          . 0)
    )
  "Default settings for offsets of syntactic elements.
Do not change this constant!  See the variable `vhdl-offsets-alist' for
more information.")

(defvar vhdl-offsets-alist (copy-alist vhdl-offsets-alist-default)
  "*Association list of syntactic element symbols and indentation offsets.
As described below, each cons cell in this list has the form:

    (SYNTACTIC-SYMBOL . OFFSET)

When a line is indented, `vhdl-mode' first determines the syntactic
context of the line by generating a list of symbols called syntactic
elements.  This list can contain more than one syntactic element and
the global variable `vhdl-syntactic-context' contains the context list
for the line being indented.  Each element in this list is actually a
cons cell of the syntactic symbol and a buffer position.  This buffer
position is call the relative indent point for the line.  Some
syntactic symbols may not have a relative indent point associated with
them.

After the syntactic context list for a line is generated, `vhdl-mode'
calculates the absolute indentation for the line by looking at each
syntactic element in the list.  First, it compares the syntactic
element against the SYNTACTIC-SYMBOL's in `vhdl-offsets-alist'.  When it
finds a match, it adds the OFFSET to the column of the relative indent
point.  The sum of this calculation for each element in the syntactic
list is the absolute offset for line being indented.

If the syntactic element does not match any in the `vhdl-offsets-alist',
an error is generated if `vhdl-strict-syntax-p' is non-nil, otherwise
the element is ignored.

Actually, OFFSET can be an integer, a function, a variable, or one of
the following symbols: `+', `-', `++', or `--'.  These latter
designate positive or negative multiples of `vhdl-basic-offset',
respectively: *1, *-1, *2, and *-2.  If OFFSET is a function, it is
called with a single argument containing the cons of the syntactic
element symbol and the relative indent point.  The function should
return an integer offset.

Here is the current list of valid syntactic element symbols:

 string                 -- inside multi-line string
 block-open             -- statement block open
 block-close            -- statement block close
 statement              -- a VHDL statement
 statement-cont         -- a continuation of a VHDL statement
 statement-block-intro  -- the first line in a new statement block
 statement-case-intro   -- the first line in a case alternative block
 case-alternative       -- a case statement alternative clause
 comment                -- a line containing only a comment
 arglist-intro          -- the first line in an argument list
 arglist-cont           -- subsequent argument list lines when no
                           arguments follow on the same line as the
                           the arglist opening paren
 arglist-cont-nonempty  -- subsequent argument list lines when at
                           least one argument follows on the same
                           line as the arglist opening paren
 arglist-close          -- the solo close paren of an argument list
 entity                 -- inside an entity declaration
 configuration          -- inside a configuration declaration
 package                -- inside a package declaration
 architecture           -- inside an architecture body
 package-body           -- inside a package body")

(defvar vhdl-comment-only-line-offset 0
  "*Extra offset for line which contains only the start of a comment.
Can contain an integer or a cons cell of the form:

 (NON-ANCHORED-OFFSET . ANCHORED-OFFSET)

Where NON-ANCHORED-OFFSET is the amount of offset given to
non-column-zero anchored comment-only lines, and ANCHORED-OFFSET is
the amount of offset to give column-zero anchored comment-only lines.
Just an integer as value is equivalent to (<val> . 0)")

(defvar vhdl-special-indent-hook nil
  "*Hook for user defined special indentation adjustments.
This hook gets called after a line is indented by the mode.")

(defvar vhdl-style-alist
  '(("IEEE"
     (vhdl-basic-offset . 4)
     (vhdl-offsets-alist . ())
     )
    )
  "Styles of Indentation.
Elements of this alist are of the form:

  (STYLE-STRING (VARIABLE . VALUE) [(VARIABLE . VALUE) ...])

where STYLE-STRING is a short descriptive string used to select a
style, VARIABLE is any `vhdl-mode' variable, and VALUE is the intended
value for that variable when using the selected style.

There is one special case when VARIABLE is `vhdl-offsets-alist'.  In this
case, the VALUE is a list containing elements of the form:

  (SYNTACTIC-SYMBOL . VALUE)

as described in `vhdl-offsets-alist'.  These are passed directly to
`vhdl-set-offset' so there is no need to set every syntactic symbol in
your style, only those that are different from the default.")

;; dynamically append the default value of most variables
(or (assoc "Default" vhdl-style-alist)
    (let* ((varlist '(vhdl-inhibit-startup-warnings-p
		      vhdl-strict-syntax-p
		      vhdl-echo-syntactic-information-p
		      vhdl-basic-offset
		      vhdl-offsets-alist
		      vhdl-comment-only-line-offset))
	   (default (cons "Default"
			  (mapcar
			   (function
			    (lambda (var)
			      (cons var (symbol-value var))))
			   varlist))))
      (setq vhdl-style-alist (cons default vhdl-style-alist))))

(defvar vhdl-mode-hook nil
  "*Hook called by `vhdl-mode'.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compatibility

(defvar vhdl-startup-warnings nil
  "Warnings to tell the user during start up.")

(defun vhdl-print-warnings ()
  "Print out messages in variable `vhdl-startup-warnings'."
  (let ((warnings vhdl-startup-warnings))
    (while warnings
      (message (concat "WARNING:  " (car warnings)))
      (setq warnings (cdr warnings))))
  (when (> (length vhdl-startup-warnings) 1)
    (message "WARNING:  See warning messages in *Messages* buffer.")))

(defun vhdl-add-warning (string)
  "Add STRING to warning list `vhdl-startup-warnings'."
  (setq vhdl-startup-warnings (cons string vhdl-startup-warnings)))

;; Perform compatibility checks.
(when (not (stringp vhdl-compiler))	; changed format of `vhdl-compiler'
  (setq vhdl-compiler "ModelSim")
  (vhdl-add-warning "Variable `vhdl-compiler' has changed format; customize again"))
(when (not (listp vhdl-standard))	; changed format of `vhdl-standard'
  (setq vhdl-standard '(87 nil))
  (vhdl-add-warning "Variable `vhdl-standard' has changed format; customize again"))
(when (= (length (car vhdl-model-alist)) 3)
  (let ((old-alist vhdl-model-alist)	; changed format of `vhdl-model-alist'
	new-alist)
    (while old-alist
      (setq new-alist (cons (append (car old-alist) '("")) new-alist))
      (setq old-alist (cdr old-alist)))
    (setq vhdl-model-alist (nreverse new-alist))))
(when (= (length (car vhdl-project-alist)) 3)
  (let ((old-alist vhdl-project-alist) ; changed format of `vhdl-project-alist'
	new-alist)
    (while old-alist
      (setq new-alist (cons (append (car old-alist) '("")) new-alist))
      (setq old-alist (cdr old-alist)))
    (setq vhdl-project-alist (nreverse new-alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help functions

(defsubst vhdl-standard-p (standard)
  "Check if STANDARD is specified as used standard."
  (or (eq standard (car vhdl-standard))
       (memq standard (cadr vhdl-standard))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Required packages

(require 'assoc)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs variant handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; active regions

(defun vhdl-keep-region-active ()
  "Do whatever is necessary to keep the region active in XEmacs.
Ignore byte-compiler warnings you might see."
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XEmacs hacks

(unless (fboundp 'wildcard-to-regexp)
  (defun wildcard-to-regexp (wildcard)
    "Simplified version of `wildcard-to-regexp' from Emacs' `files.el'."
    (let* ((i (string-match "[*?]" wildcard))
	   (result (substring wildcard 0 i))
	   (len (length wildcard)))
      (when i
	(while (< i len)
	  (let ((ch (aref wildcard i)))
	    (setq result (concat result
				 (cond ((eq ch ?*)  "[^\000]*")
				       ((eq ch ??)  "[^\000]")
				       (t (char-to-string ch)))))
	    (setq i (1+ i)))))
      (concat "\\`" result "\\'"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

(defvar vhdl-template-map ()
  "Keymap for VHDL templates.")

(defun vhdl-template-map-init ()
  "Initialize `vhdl-template-map'."
  (setq vhdl-template-map (make-sparse-keymap))
  ;; key bindings for VHDL templates
  (define-key vhdl-template-map "al"	'vhdl-template-alias)
  (define-key vhdl-template-map "ar"	'vhdl-template-architecture)
  (define-key vhdl-template-map "at"	'vhdl-template-assert)
  (define-key vhdl-template-map "ad"	'vhdl-template-attribute-decl)
  (define-key vhdl-template-map "as"	'vhdl-template-attribute-spec)
  (define-key vhdl-template-map "bl"	'vhdl-template-block)
  (define-key vhdl-template-map "ca"	'vhdl-template-case-is)
  (define-key vhdl-template-map "cd"	'vhdl-template-component-decl)
  (define-key vhdl-template-map "ci"	'vhdl-template-component-inst)
  (define-key vhdl-template-map "cs"	'vhdl-template-conditional-signal-asst)
  (define-key vhdl-template-map "Cb"	'vhdl-template-block-configuration)
  (define-key vhdl-template-map "Cc"	'vhdl-template-component-conf)
  (define-key vhdl-template-map "Cd"	'vhdl-template-configuration-decl)
  (define-key vhdl-template-map "Cs"	'vhdl-template-configuration-spec)
  (define-key vhdl-template-map "co"	'vhdl-template-constant)
  (define-key vhdl-template-map "di"	'vhdl-template-disconnect)
  (define-key vhdl-template-map "el"	'vhdl-template-else)
  (define-key vhdl-template-map "ei"	'vhdl-template-elsif)
  (define-key vhdl-template-map "en"	'vhdl-template-entity)
  (define-key vhdl-template-map "ex"	'vhdl-template-exit)
  (define-key vhdl-template-map "fi"	'vhdl-template-file)
  (define-key vhdl-template-map "fg"	'vhdl-template-for-generate)
  (define-key vhdl-template-map "fl"	'vhdl-template-for-loop)
  (define-key vhdl-template-map "\C-f"	'vhdl-template-footer)
  (define-key vhdl-template-map "fb"	'vhdl-template-function-body)
  (define-key vhdl-template-map "fd"	'vhdl-template-function-decl)
  (define-key vhdl-template-map "ge"	'vhdl-template-generic)
  (define-key vhdl-template-map "gd"	'vhdl-template-group-decl)
  (define-key vhdl-template-map "gt"	'vhdl-template-group-template)
  (define-key vhdl-template-map "\C-h"	'vhdl-template-header)
  (define-key vhdl-template-map "ig"	'vhdl-template-if-generate)
  (define-key vhdl-template-map "it"	'vhdl-template-if-then)
  (define-key vhdl-template-map "li"	'vhdl-template-library)
  (define-key vhdl-template-map "lo"	'vhdl-template-bare-loop)
  (define-key vhdl-template-map "\C-m"	'vhdl-template-modify)
  (define-key vhdl-template-map "\C-t"	'vhdl-template-insert-date)
  (define-key vhdl-template-map "ma"	'vhdl-template-map)
  (define-key vhdl-template-map "ne"	'vhdl-template-next)
  (define-key vhdl-template-map "ot"	'vhdl-template-others)
  (define-key vhdl-template-map "Pd"	'vhdl-template-package-decl)
  (define-key vhdl-template-map "Pb"	'vhdl-template-package-body)
  (define-key vhdl-template-map "("     'vhdl-template-paired-parens)
  (define-key vhdl-template-map "po"	'vhdl-template-port)
  (define-key vhdl-template-map "pb"	'vhdl-template-procedure-body)
  (define-key vhdl-template-map "pd"	'vhdl-template-procedure-decl)
  (define-key vhdl-template-map "pc"	'vhdl-template-process-comb)
  (define-key vhdl-template-map "ps"	'vhdl-template-process-seq)
  (define-key vhdl-template-map "rp"	'vhdl-template-report)
  (define-key vhdl-template-map "rt"	'vhdl-template-return)
  (define-key vhdl-template-map "ss"	'vhdl-template-selected-signal-asst)
  (define-key vhdl-template-map "si"	'vhdl-template-signal)
  (define-key vhdl-template-map "su"	'vhdl-template-subtype)
  (define-key vhdl-template-map "ty"	'vhdl-template-type)
  (define-key vhdl-template-map "us"	'vhdl-template-use)
  (define-key vhdl-template-map "va"	'vhdl-template-variable)
  (define-key vhdl-template-map "wa"	'vhdl-template-wait)
  (define-key vhdl-template-map "wl"	'vhdl-template-while-loop)
  (define-key vhdl-template-map "wi"	'vhdl-template-with)
  (define-key vhdl-template-map "wc"	'vhdl-template-clocked-wait)
  (define-key vhdl-template-map "\C-pb" 'vhdl-template-package-numeric-bit)
  (define-key vhdl-template-map "\C-pn" 'vhdl-template-package-numeric-std)
  (define-key vhdl-template-map "\C-ps" 'vhdl-template-package-std-logic-1164)
  (define-key vhdl-template-map "\C-pA" 'vhdl-template-package-std-logic-arith)
  (define-key vhdl-template-map "\C-pM" 'vhdl-template-package-std-logic-misc)
  (define-key vhdl-template-map "\C-pS" 'vhdl-template-package-std-logic-signed)
  (define-key vhdl-template-map "\C-pT" 'vhdl-template-package-std-logic-textio)
  (define-key vhdl-template-map "\C-pU" 'vhdl-template-package-std-logic-unsigned)
  (define-key vhdl-template-map "\C-pt" 'vhdl-template-package-textio)
  (define-key vhdl-template-map "\C-dn" 'vhdl-template-directive-translate-on)
  (define-key vhdl-template-map "\C-df" 'vhdl-template-directive-translate-off)
  (define-key vhdl-template-map "\C-dN" 'vhdl-template-directive-synthesis-on)
  (define-key vhdl-template-map "\C-dF" 'vhdl-template-directive-synthesis-off)
  (define-key vhdl-template-map "\C-q"  'vhdl-template-search-prompt)
  (when (vhdl-standard-p 'ams)
    (define-key vhdl-template-map "br"	'vhdl-template-break)
    (define-key vhdl-template-map "cu"	'vhdl-template-case-use)
    (define-key vhdl-template-map "iu"	'vhdl-template-if-use)
    (define-key vhdl-template-map "lm"	'vhdl-template-limit)
    (define-key vhdl-template-map "na"	'vhdl-template-nature)
    (define-key vhdl-template-map "pa"	'vhdl-template-procedural)
    (define-key vhdl-template-map "qf"	'vhdl-template-quantity-free)
    (define-key vhdl-template-map "qb"	'vhdl-template-quantity-branch)
    (define-key vhdl-template-map "qs"	'vhdl-template-quantity-source)
    (define-key vhdl-template-map "sn"	'vhdl-template-subnature)
    (define-key vhdl-template-map "te"	'vhdl-template-terminal)
    )
  (when (vhdl-standard-p 'math)
    (define-key vhdl-template-map "\C-pc" 'vhdl-template-package-math-complex)
    (define-key vhdl-template-map "\C-pr" 'vhdl-template-package-math-real)
    ))

;; initialize template map for VHDL Mode
(vhdl-template-map-init)

(defun vhdl-function-name (prefix string &optional postfix)
  "Generate a Lisp function name.
PREFIX, STRING and optional POSTFIX are concatenated by '-' and spaces in
STRING are replaced by `-' and substrings are converted to lower case."
  (let ((name prefix))
    (while (string-match "\\(\\w+\\)\\s-*\\(.*\\)" string)
      (setq name
	    (concat name "-" (downcase (substring string 0 (match-end 1)))))
      (setq string (substring string (match-beginning 2))))
    (when postfix (setq name (concat name "-" postfix)))
    (intern name)))

(defvar vhdl-model-map ()
  "Keymap for VHDL models.")

(defun vhdl-model-map-init ()
  "Initialize `vhdl-model-map'."
  (setq vhdl-model-map (make-sparse-keymap))
  ;; key bindings for VHDL models
  (let ((model-alist vhdl-model-alist) model)
    (while model-alist
      (setq model (car model-alist))
      (define-key vhdl-model-map (nth 2 model)
	(vhdl-function-name "vhdl-model" (nth 0 model)))
      (setq model-alist (cdr model-alist)))))

;; initialize user model map for VHDL Mode
(vhdl-model-map-init)

(defvar vhdl-mode-map ()
  "Keymap for VHDL Mode.")

(defun vhdl-mode-map-init ()
  "Initialize `vhdl-mode-map'."
  (setq vhdl-mode-map (make-sparse-keymap))
  ;; template key bindings
  (define-key vhdl-mode-map "\C-c\C-t"     vhdl-template-map)
  ;; model key bindings
  (define-key vhdl-mode-map "\C-c\C-m"     vhdl-model-map)
  ;; standard key bindings
  (define-key vhdl-mode-map "\M-a"         'vhdl-beginning-of-statement)
  (define-key vhdl-mode-map "\M-e"         'vhdl-end-of-statement)
  (define-key vhdl-mode-map "\M-\C-f"      'vhdl-forward-sexp)
  (define-key vhdl-mode-map "\M-\C-b"      'vhdl-backward-sexp)
  (define-key vhdl-mode-map "\M-\C-u"      'vhdl-backward-up-list)
  (define-key vhdl-mode-map "\M-\C-a"      'vhdl-beginning-of-defun)
  (define-key vhdl-mode-map "\M-\C-e"      'vhdl-end-of-defun)
  (define-key vhdl-mode-map "\M-\C-h"      'vhdl-mark-defun)
  (define-key vhdl-mode-map "\M-\C-q"      'vhdl-indent-sexp)
  ;; backspace/delete key bindings
  (define-key vhdl-mode-map [backspace]    'backward-delete-char-untabify)
  (define-key vhdl-mode-map [delete]       'delete-char)
  (unless (string-match "XEmacs" emacs-version)
    (define-key vhdl-mode-map [M-delete]   'kill-word))
  ;; mode specific key bindings
  (define-key vhdl-mode-map "\C-c\C-e"     'vhdl-electric-mode)
  (define-key vhdl-mode-map "\C-c\C-s"     'vhdl-stutter-mode)
  (define-key vhdl-mode-map "\C-c\C-k"     'vhdl-compile)
  (define-key vhdl-mode-map "\C-c\M-\C-k"  'vhdl-make)
  (define-key vhdl-mode-map "\C-c\C-p\C-w" 'vhdl-port-copy)
  (define-key vhdl-mode-map "\C-c\C-p\M-w" 'vhdl-port-copy)
  (define-key vhdl-mode-map "\C-c\C-p\C-e" 'vhdl-port-paste-entity)
  (define-key vhdl-mode-map "\C-c\C-p\C-c" 'vhdl-port-paste-component)
  (define-key vhdl-mode-map "\C-c\C-p\C-i" 'vhdl-port-paste-instance)
  (define-key vhdl-mode-map "\C-c\C-p\C-s" 'vhdl-port-paste-signals)
  (define-key vhdl-mode-map "\C-c\C-p\M-c" 'vhdl-port-paste-constants)
  (if (string-match "XEmacs" emacs-version) ; `... C-g' not allowed in XEmacs
      (define-key vhdl-mode-map "\C-c\C-p\M-g" 'vhdl-port-paste-generic-map)
    (define-key vhdl-mode-map "\C-c\C-p\C-g" 'vhdl-port-paste-generic-map))
  (define-key vhdl-mode-map "\C-c\C-p\C-t" 'vhdl-port-paste-testbench)
  (define-key vhdl-mode-map "\C-c\C-p\C-f" 'vhdl-port-flatten)
  (define-key vhdl-mode-map "\C-c\C-c"     'vhdl-comment-uncomment-region)
  (define-key vhdl-mode-map "\C-c-"        'vhdl-comment-append-inline)
  (define-key vhdl-mode-map "\C-c\M--"     'vhdl-comment-display-line)
  (define-key vhdl-mode-map "\C-c\M-\C-i"  'vhdl-indent-line)
  (define-key vhdl-mode-map "\M-\C-\\"     'vhdl-indent-region)
  (define-key vhdl-mode-map "\C-c\C-a"     'vhdl-align-group)
  (define-key vhdl-mode-map "\C-c\C-r\C-a" 'vhdl-align-noindent-region)
  (define-key vhdl-mode-map "\C-c\M-\C-a"  'vhdl-align-inline-comment-group)
  (define-key vhdl-mode-map "\C-c\C-r\M-\C-a" 'vhdl-align-inline-comment-region)
  (define-key vhdl-mode-map "\C-c\C-w"     'vhdl-fixup-whitespace-region)
  (define-key vhdl-mode-map "\C-c\C-l\C-w" 'vhdl-line-kill)
  (define-key vhdl-mode-map "\C-c\C-l\M-w" 'vhdl-line-copy)
  (define-key vhdl-mode-map "\C-c\C-l\C-y" 'vhdl-line-yank)
  (define-key vhdl-mode-map "\C-c\C-l\t"   'vhdl-line-expand)
  (define-key vhdl-mode-map "\C-c\C-l\C-n" 'vhdl-line-transpose-next)
  (define-key vhdl-mode-map "\C-c\C-l\C-p" 'vhdl-line-transpose-previous)
  (define-key vhdl-mode-map "\C-c\C-l\C-o" 'vhdl-line-open)
  (define-key vhdl-mode-map "\C-c\C-l\C-g" 'goto-line)
  (define-key vhdl-mode-map "\C-c\C-l\C-c" 'vhdl-comment-uncomment-line)
  (define-key vhdl-mode-map "\C-c\C-r\C-u" 'vhdl-fix-case-region)
  (define-key vhdl-mode-map "\C-c\C-u"     'vhdl-fix-case-buffer)
  (define-key vhdl-mode-map "\C-c\C-f"     'vhdl-fontify-buffer)
  (define-key vhdl-mode-map "\C-c\C-x"     'vhdl-show-syntactic-information)
  (define-key vhdl-mode-map "\C-c\C-h"     'vhdl-doc-mode)
  (define-key vhdl-mode-map "\C-c\C-v"     'vhdl-version)
  (define-key vhdl-mode-map "\C-c\C-r\C-b" 'vhdl-beautify-region)
  (define-key vhdl-mode-map "\C-c\C-b"     'vhdl-beautify-buffer)
  (define-key vhdl-mode-map "\M-\t"        'tab-to-tab-stop)
  ;; insert commands bindings
  (define-key vhdl-mode-map "\C-c\C-i\C-c" 'vhdl-template-insert-construct)
  (define-key vhdl-mode-map "\C-c\C-i\C-p" 'vhdl-template-insert-package)
  (define-key vhdl-mode-map "\C-c\C-i\C-d" 'vhdl-template-insert-directive)
  (define-key vhdl-mode-map "\C-c\C-i\C-m" 'vhdl-model-insert)
  ;; electric key bindings
  (define-key vhdl-mode-map " "            'vhdl-electric-space)
  (if vhdl-intelligent-tab
      (define-key vhdl-mode-map "\t"       'vhdl-electric-tab)
    (define-key vhdl-mode-map "\t"         'vhdl-indent-line))
  (define-key vhdl-mode-map "\r"           'vhdl-electric-return)
  (define-key vhdl-mode-map "-"            'vhdl-electric-dash)
  (define-key vhdl-mode-map "["            'vhdl-electric-open-bracket)
  (define-key vhdl-mode-map "]"            'vhdl-electric-close-bracket)
  (define-key vhdl-mode-map "'"            'vhdl-electric-quote)
  (define-key vhdl-mode-map ";"            'vhdl-electric-semicolon)
  (define-key vhdl-mode-map ","            'vhdl-electric-comma)
  (define-key vhdl-mode-map "."            'vhdl-electric-period)
  (when (vhdl-standard-p 'ams)
    (define-key vhdl-mode-map "="          'vhdl-electric-equal)))

;; initialize mode map for VHDL Mode
(vhdl-mode-map-init)

;; define special minibuffer keymap for enabling word completion in minibuffer
;; (useful in template generator prompts)
(defvar vhdl-minibuffer-local-map (copy-keymap minibuffer-local-map)
  "Keymap for minibuffer used in VHDL Mode.")

(when vhdl-word-completion-in-minibuffer
  (define-key vhdl-minibuffer-local-map "\t" 'vhdl-minibuffer-tab))

;; set up electric character functions to work with
;; `delete-selection-mode' (Emacs) and `pending-delete-mode' (XEmacs)
(mapcar
 (function
  (lambda (sym)
    (put sym 'delete-selection t)	; for `delete-selection-mode' (Emacs)
    (put sym 'pending-delete t)))	; for `pending-delete-mode' (XEmacs)
 '(vhdl-electric-space
   vhdl-electric-tab
   vhdl-electric-return
   vhdl-electric-dash
   vhdl-electric-open-bracket
   vhdl-electric-close-bracket
   vhdl-electric-quote
   vhdl-electric-semicolon
   vhdl-electric-comma
   vhdl-electric-period
   vhdl-electric-equal))

;; syntax table
(defvar vhdl-mode-syntax-table nil
  "Syntax table used in `vhdl-mode' buffers.")

(defun vhdl-mode-syntax-table-init ()
  "Initialize `vhdl-mode-syntax-table'."
  (setq vhdl-mode-syntax-table (make-syntax-table))
  ;; define punctuation
  (modify-syntax-entry ?\# "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\$ "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\% "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\& "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\' "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\* "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\+ "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\. "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\/ "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\: "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\; "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\< "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\= "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\> "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\\ "."    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\| "."    vhdl-mode-syntax-table)
  ;; define string
  (modify-syntax-entry ?\" "\""   vhdl-mode-syntax-table)
  ;; define underscore
  (when vhdl-underscore-is-part-of-word
    (modify-syntax-entry ?_ "w"   vhdl-mode-syntax-table))
  ;; a single hyphen is punctuation, but a double hyphen starts a comment
  (modify-syntax-entry ?\- ". 12" vhdl-mode-syntax-table)
  ;; and \n and \^M end a comment
  (modify-syntax-entry ?\n ">"    vhdl-mode-syntax-table)
  (modify-syntax-entry ?\^M ">"   vhdl-mode-syntax-table)
  ;; define parentheses to match
  (modify-syntax-entry ?\( "()"   vhdl-mode-syntax-table)
  (modify-syntax-entry ?\) ")("   vhdl-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]"   vhdl-mode-syntax-table)
  (modify-syntax-entry ?\] ")["   vhdl-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}"   vhdl-mode-syntax-table)
  (modify-syntax-entry ?\} "){"   vhdl-mode-syntax-table))

;; initialize syntax table for VHDL Mode
(vhdl-mode-syntax-table-init)

(defmacro vhdl-ext-syntax-table (&rest body)
  "Execute BODY with syntax table that includes `_' in word class."
  `(let (result)
     (modify-syntax-entry ?_ "w" vhdl-mode-syntax-table)
     (setq result (progn ,@body))
     (when (not vhdl-underscore-is-part-of-word)
       (modify-syntax-entry ?_ "_" vhdl-mode-syntax-table))
     result))

(defvar vhdl-syntactic-context nil
  "Buffer local variable containing syntactic analysis list.")
(make-variable-buffer-local 'vhdl-syntactic-context)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abbrev hook bindings

(defvar vhdl-mode-abbrev-table nil
  "Abbrev table to use in `vhdl-mode' buffers.")

(defun vhdl-mode-abbrev-table-init ()
  "Initialize `vhdl-mode-abbrev-table'."
  (when vhdl-mode-abbrev-table (clear-abbrev-table vhdl-mode-abbrev-table))
  (define-abbrev-table 'vhdl-mode-abbrev-table
    (append
     (when (memq 'vhdl vhdl-electric-keywords)
       ;; VHDL'93 keywords
       '(
	 ("--"            "" vhdl-template-display-comment-hook 0)
	 ("abs"           "" vhdl-template-default-hook 0)
	 ("access"        "" vhdl-template-default-hook 0)
	 ("after"         "" vhdl-template-default-hook 0)
	 ("alias"         "" vhdl-template-alias-hook 0)
	 ("all"           "" vhdl-template-default-hook 0)
	 ("and"           "" vhdl-template-default-hook 0)
	 ("arch"          "" vhdl-template-architecture-hook 0)
	 ("architecture"  "" vhdl-template-architecture-hook 0)
	 ("array"         "" vhdl-template-default-hook 0)
	 ("assert"        "" vhdl-template-assert-hook 0)
	 ("attr"          "" vhdl-template-attribute-hook 0)
	 ("attribute"     "" vhdl-template-attribute-hook 0)
	 ("begin"         "" vhdl-template-default-indent-hook 0)
	 ("block"         "" vhdl-template-block-hook 0)
	 ("body"          "" vhdl-template-default-hook 0)
	 ("buffer"        "" vhdl-template-default-hook 0)
	 ("bus"           "" vhdl-template-default-hook 0)
	 ("case"          "" vhdl-template-case-hook 0)
	 ("comp"          "" vhdl-template-component-hook 0)
	 ("component"     "" vhdl-template-component-hook 0)
	 ("cond"          "" vhdl-template-conditional-signal-asst-hook 0)
	 ("conditional"   "" vhdl-template-conditional-signal-asst-hook 0)
	 ("conf"          "" vhdl-template-configuration-hook 0)
	 ("configuration" "" vhdl-template-configuration-hook 0)
	 ("cons"          "" vhdl-template-constant-hook 0)
	 ("constant"      "" vhdl-template-constant-hook 0)
	 ("disconnect"    "" vhdl-template-disconnect-hook 0)
	 ("downto"        "" vhdl-template-default-hook 0)
	 ("else"          "" vhdl-template-else-hook 0)
	 ("elseif"        "" vhdl-template-elsif-hook 0)
	 ("elsif"         "" vhdl-template-elsif-hook 0)
	 ("end"           "" vhdl-template-default-indent-hook 0)
	 ("entity"        "" vhdl-template-entity-hook 0)
	 ("exit"          "" vhdl-template-exit-hook 0)
	 ("file"          "" vhdl-template-file-hook 0)
	 ("for"           "" vhdl-template-for-hook 0)
	 ("func"          "" vhdl-template-function-hook 0)
	 ("function"      "" vhdl-template-function-hook 0)
	 ("generic"       "" vhdl-template-generic-hook 0)
	 ("group"         "" vhdl-template-group-hook 0)
	 ("guarded"       "" vhdl-template-default-hook 0)
	 ("if"            "" vhdl-template-if-hook 0)
	 ("impure"        "" vhdl-template-default-hook 0)
	 ("in"            "" vhdl-template-default-hook 0)
	 ("inertial"      "" vhdl-template-default-hook 0)
	 ("inout"         "" vhdl-template-default-hook 0)
	 ("inst"          "" vhdl-template-instance-hook 0)
	 ("instance"      "" vhdl-template-instance-hook 0)
	 ("is"            "" vhdl-template-default-hook 0)
	 ("label"         "" vhdl-template-default-hook 0)
	 ("library"       "" vhdl-template-library-hook 0)
	 ("linkage"       "" vhdl-template-default-hook 0)
	 ("literal"       "" vhdl-template-default-hook 0)
	 ("loop"          "" vhdl-template-bare-loop-hook 0)
	 ("map"           "" vhdl-template-map-hook 0)
	 ("mod"           "" vhdl-template-default-hook 0)
	 ("nand"          "" vhdl-template-default-hook 0)
	 ("new"           "" vhdl-template-default-hook 0)
	 ("next"          "" vhdl-template-next-hook 0)
	 ("nor"           "" vhdl-template-default-hook 0)
	 ("not"           "" vhdl-template-default-hook 0)
	 ("null"          "" vhdl-template-default-hook 0)
	 ("of"            "" vhdl-template-default-hook 0)
	 ("on"            "" vhdl-template-default-hook 0)
	 ("open"          "" vhdl-template-default-hook 0)
	 ("or"            "" vhdl-template-default-hook 0)
	 ("others"        "" vhdl-template-default-hook 0)
	 ("out"           "" vhdl-template-default-hook 0)
	 ("pack"          "" vhdl-template-package-hook 0)
	 ("package"       "" vhdl-template-package-hook 0)
	 ("port"          "" vhdl-template-port-hook 0)
	 ("postponed"     "" vhdl-template-default-hook 0)
	 ("procedure"     "" vhdl-template-procedure-hook 0)
	 ("process"       "" vhdl-template-process-hook 0)
	 ("pure"          "" vhdl-template-default-hook 0)
	 ("range"         "" vhdl-template-default-hook 0)
	 ("record"        "" vhdl-template-default-hook 0)
	 ("register"      "" vhdl-template-default-hook 0)
	 ("reject"        "" vhdl-template-default-hook 0)
	 ("rem"           "" vhdl-template-default-hook 0)
	 ("report"        "" vhdl-template-report-hook 0)
	 ("return"        "" vhdl-template-return-hook 0)
	 ("rol"           "" vhdl-template-default-hook 0)
	 ("ror"           "" vhdl-template-default-hook 0)
	 ("select"        "" vhdl-template-selected-signal-asst-hook 0)
	 ("severity"      "" vhdl-template-default-hook 0)
	 ("shared"        "" vhdl-template-default-hook 0)
	 ("sig"           "" vhdl-template-signal-hook 0)
	 ("signal"        "" vhdl-template-signal-hook 0)
	 ("sla"           "" vhdl-template-default-hook 0)
	 ("sll"           "" vhdl-template-default-hook 0)
	 ("sra"           "" vhdl-template-default-hook 0)
	 ("srl"           "" vhdl-template-default-hook 0)
	 ("subtype"       "" vhdl-template-subtype-hook 0)
	 ("then"          "" vhdl-template-default-hook 0)
	 ("to"            "" vhdl-template-default-hook 0)
	 ("transport"     "" vhdl-template-default-hook 0)
	 ("type"          "" vhdl-template-type-hook 0)
	 ("unaffected"    "" vhdl-template-default-hook 0)
	 ("units"         "" vhdl-template-default-hook 0)
	 ("until"         "" vhdl-template-default-hook 0)
	 ("use"           "" vhdl-template-use-hook 0)
	 ("var"           "" vhdl-template-variable-hook 0)
	 ("variable"      "" vhdl-template-variable-hook 0)
	 ("wait"          "" vhdl-template-wait-hook 0)
	 ("when"          "" vhdl-template-when-hook 0)
	 ("while"         "" vhdl-template-while-loop-hook 0)
	 ("with"          "" vhdl-template-with-hook 0)
	 ("xnor"          "" vhdl-template-default-hook 0)
	 ("xor"           "" vhdl-template-default-hook 0)
	 ))
     ;; VHDL-AMS keywords
     (when (and (memq 'vhdl vhdl-electric-keywords) (vhdl-standard-p 'ams))
       '(
	 ("across"     "" vhdl-template-default-hook 0)
	 ("break"      "" vhdl-template-break-hook 0)
	 ("limit"      "" vhdl-template-limit-hook 0)
	 ("nature"     "" vhdl-template-nature-hook 0)
	 ("noise"      "" vhdl-template-default-hook 0)
	 ("procedural" "" vhdl-template-procedural-hook 0)
	 ("quantity"   "" vhdl-template-quantity-hook 0)
	 ("reference"  "" vhdl-template-default-hook 0)
	 ("spectrum"   "" vhdl-template-default-hook 0)
	 ("subnature"  "" vhdl-template-subnature-hook 0)
	 ("terminal"   "" vhdl-template-terminal-hook 0)
	 ("through"    "" vhdl-template-default-hook 0)
	 ("tolerance"  "" vhdl-template-default-hook 0)
	 ))
     ;; user model keywords
     (when (memq 'user vhdl-electric-keywords)
       (let ((alist vhdl-model-alist)
	     abbrev-list keyword)
	 (while alist
	   (setq keyword (nth 3 (car alist)))
	   (unless (equal keyword "")
	     (setq abbrev-list
		   (cons (list keyword ""
			       (vhdl-function-name
				"vhdl-model" (nth 0 (car alist)) "hook") 0)
			 abbrev-list)))
	   (setq alist (cdr alist)))
	 abbrev-list)))))

;; initialize abbrev table for VHDL Mode
(vhdl-mode-abbrev-table-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Template completion lists

(defvar vhdl-template-construct-alist nil
  "List of built-in construct templates.")

(defun vhdl-template-construct-alist-init ()
  "Initialize `vhdl-template-construct-alist'."
  (setq
   vhdl-template-construct-alist
   (append
    '(
      ("alias declaration"		   vhdl-template-alias)
      ("architecture body"		   vhdl-template-architecture)
      ("assertion"			   vhdl-template-assert)
      ("attribute declaration"		   vhdl-template-attribute-decl)
      ("attribute specification"	   vhdl-template-attribute-spec)
      ("block configuration"		   vhdl-template-block-configuration)
      ("block statement"		   vhdl-template-block)
      ("case statement"			   vhdl-template-case-is)
      ("component configuration"	   vhdl-template-component-conf)
      ("component declaration"		   vhdl-template-component-decl)
      ("component instantiation statement" vhdl-template-component-inst)
      ("conditional signal assignment"	   vhdl-template-conditional-signal-asst)
      ("configuration declaration"	   vhdl-template-configuration-decl)
      ("configuration specification" 	   vhdl-template-configuration-spec)
      ("constant declaration"		   vhdl-template-constant)
      ("disconnection specification"	   vhdl-template-disconnect)
      ("entity declaration"		   vhdl-template-entity)
      ("exit statement"			   vhdl-template-exit)
      ("file declaration"		   vhdl-template-file)
      ("generate statement"		   vhdl-template-generate)
      ("generic clause"			   vhdl-template-generic)
      ("group declaration"		   vhdl-template-group-decl)
      ("group template declaration" 	   vhdl-template-group-template)
      ("if statement"			   vhdl-template-if-then)
      ("library clause"			   vhdl-template-library)
      ("loop statement"			   vhdl-template-loop)
      ("next statement"			   vhdl-template-next)
      ("package declaration"		   vhdl-template-package-decl)
      ("package body"			   vhdl-template-package-body)
      ("port clause"			   vhdl-template-port)
      ("process statement"	 	   vhdl-template-process)
      ("report statement"		   vhdl-template-report)
      ("return statement"		   vhdl-template-return)
      ("selected signal assignment"	   vhdl-template-selected-signal-asst)
      ("signal declaration"		   vhdl-template-signal)
      ("subprogram declaration"		   vhdl-template-subprogram-decl)
      ("subprogram body"		   vhdl-template-subprogram-body)
      ("subtype declaration"		   vhdl-template-subtype)
      ("type declaration"		   vhdl-template-type)
      ("use clause"			   vhdl-template-use)
      ("variable declaration"	 	   vhdl-template-variable)
      ("wait statement"			   vhdl-template-wait)
      )
    (when (vhdl-standard-p 'ams)
      '(
	("break statement"		     vhdl-template-break)
	("nature declaration"		     vhdl-template-nature)
	("quantity declaration"		     vhdl-template-quantity)
	("simultaneous case statement"	     vhdl-template-case-use)
	("simultaneous if statement"	     vhdl-template-if-use)
	("simultaneous procedural statement" vhdl-template-procedural)
	("step limit specification"	     vhdl-template-limit)
	("subnature declaration"	     vhdl-template-subnature)
	("terminal declaration"		     vhdl-template-terminal)
	)))))

;; initialize for VHDL Mode
(vhdl-template-construct-alist-init)

(defvar vhdl-template-package-alist nil
  "List of built-in package templates.")

(defun vhdl-template-package-alist-init ()
  "Initialize `vhdl-template-package-alist'."
  (setq
   vhdl-template-package-alist
   (append
    '(
      ("numeric_bit"	    vhdl-template-package-numeric-bit)
      ("numeric_std"	    vhdl-template-package-numeric-std)
      ("std_logic_1164"	    vhdl-template-package-std-logic-1164)
      ("std_logic_arith"    vhdl-template-package-std-logic-arith)
      ("std_logic_misc"     vhdl-template-package-std-logic-misc)
      ("std_logic_signed"   vhdl-template-package-std-logic-signed)
      ("std_logic_textio"   vhdl-template-package-std-logic-textio)
      ("std_logic_unsigned" vhdl-template-package-std-logic-unsigned)
      ("textio"		    vhdl-template-package-textio)
      )
    (when (vhdl-standard-p 'math)
      '(
	("math_complex"	vhdl-template-package-math-complex)
	("math_real"	vhdl-template-package-math-real)
	)))))

;; initialize for VHDL Mode
(vhdl-template-package-alist-init)

(defvar vhdl-template-directive-alist
  (append
   '(
     ("translate_on"	vhdl-template-directive-translate-on)
     ("translate_off"	vhdl-template-directive-translate-off)
     ("synthesis_on"	vhdl-template-directive-synthesis-on)
     ("synthesis_off"	vhdl-template-directive-synthesis-off)
     ))
  "List of built-in directive templates.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menues
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VHDL menu (using `easy-menu.el')

(defun vhdl-customize ()
  "Call the customize function with `vhdl' as argument."
  (interactive)
  (customize-browse 'vhdl))

(defun vhdl-create-customize-menu ()
  "Create a full customization menu for VHDL, insert it into the menu."
  (interactive)
  (if (fboundp 'customize-menu-create)
      (easy-menu-change
       '("VHDL") "Customize"
       `(["Browse VHDL Group..." vhdl-customize t]
	 ,(customize-menu-create 'vhdl)
	 "--"
	 ["Activate New Customizations" vhdl-activate-customizations t]))
    (error "Cannot expand menu (outdated version of cus-edit.el)")))

(defun vhdl-create-mode-menu ()
  "Create VHDL Mode menu."
  (list
   "VHDL"
   '("Mode"
     ["Electric" vhdl-electric-mode :style toggle :selected vhdl-electric-mode]
     ["Stutter"	 vhdl-stutter-mode  :style toggle :selected vhdl-stutter-mode]
     )
   "--"
   (append
    '("Project"
      ["None" (vhdl-project-switch "")
       :style radio :selected (equal vhdl-project "")]
      "--"
      )
    ;; add menu entries for defined projects
    (let ((project-alist vhdl-project-alist) menu-alist name)
      (while project-alist
	(setq name (car (car project-alist)))
	(setq menu-alist (cons (vector name (list 'vhdl-project-switch name)
				       :style 'radio :selected
				       (list 'equal 'vhdl-project name))
			       menu-alist))
	(setq project-alist (cdr project-alist)))
      (setq menu-alist (cons '["Add Project..."
			       (customize-variable 'vhdl-project-alist) t]
			     (cons "--" menu-alist)))
      (nreverse menu-alist)))
   "--"
   (list
    "Compile"
    ["Compile Buffer"		vhdl-compile t]
    ["Stop Compilation"		kill-compilation t]
    "--"
    ["Make"			vhdl-make t]
    ["Generate Makefile"	vhdl-generate-makefile t]
    "--"
    ["Next Error"		next-error t]
    ["Previous Error"		previous-error t]
    ["First Error"		first-error t]
    "--"
    (append
     '("Compiler")
     ;; add menu entries for defined compilers
     (let ((comp-alist vhdl-compiler-alist) menu-alist name)
       (while comp-alist
	 (setq name (car (car comp-alist)))
	 (setq menu-alist (cons (vector name (list 'setq 'vhdl-compiler name)
					:style 'radio :selected
					(list 'equal 'vhdl-compiler name))
				menu-alist))
	 (setq comp-alist (cdr comp-alist)))
       (setq menu-alist (cons '["Add Compiler..."
				(customize-variable 'vhdl-compiler-alist) t]
			      (cons "--" menu-alist)))
       (nreverse menu-alist))))
   "--"
   (append
    '("Template"
      ("VHDL Construct 1"
       ["Alias"			vhdl-template-alias t]
       ["Architecture"		vhdl-template-architecture t]
       ["Assert"		vhdl-template-assert t]
       ["Attribute (Decl)"	vhdl-template-attribute-decl t]
       ["Attribute (Spec)"	vhdl-template-attribute-spec t]
       ["Block"			vhdl-template-block t]
       ["Case"			vhdl-template-case-is t]
       ["Component (Decl)"	vhdl-template-component-decl t]
       ["(Component) Instance"	vhdl-template-component-inst t]
       ["Conditional (Signal Asst)" vhdl-template-conditional-signal-asst t]
       ["Configuration (Block)"vhdl-template-block-configuration t]
       ["Configuration (Comp)"	vhdl-template-component-conf t]
       ["Configuration (Decl)"	vhdl-template-configuration-decl t]
       ["Configuration (Spec)"	vhdl-template-configuration-spec t]
       ["Constant"		vhdl-template-constant t]
       ["Disconnect"		vhdl-template-disconnect t]
       ["Else"			vhdl-template-else t]
       ["Elsif"			vhdl-template-elsif t]
       ["Entity"		vhdl-template-entity t]
       ["Exit"			vhdl-template-exit t]
       ["File"			vhdl-template-file t]
       ["For (Generate)"	vhdl-template-for-generate t]
       ["For (Loop)"		vhdl-template-for-loop t]
       ["Function (Body)"	vhdl-template-function-body t]
       ["Function (Decl)"	vhdl-template-function-decl t]
       ["Generic"		vhdl-template-generic t]
       ["Group (Decl)"		vhdl-template-group-decl t]
       ["Group (Template)"	vhdl-template-group-template t]
       )
      ("VHDL Construct 2"
       ["If (Generate)"		vhdl-template-if-generate t]
       ["If (Then)"		vhdl-template-if-then t]
       ["Library"		vhdl-template-library t]
       ["Loop"			vhdl-template-bare-loop t]
       ["Map"			vhdl-template-map t]
       ["Next"			vhdl-template-next t]
       ["(Others)"		vhdl-template-others t]
       ["Package (Decl)"	vhdl-template-package-decl t]
       ["Package (Body)"	vhdl-template-package-body t]
       ["Port"			vhdl-template-port t]
       ["Procedure (Body)"	vhdl-template-procedure-body t]
       ["Procedure (Decl)"	vhdl-template-procedure-decl t]
       ["Process (Comb)"	vhdl-template-process-comb t]
       ["Process (Seq)"		vhdl-template-process-seq t]
       ["Report"		vhdl-template-report t]
       ["Return"		vhdl-template-return t]
       ["Select"		vhdl-template-selected-signal-asst t]
       ["Signal"		vhdl-template-signal t]
       ["Subtype"		vhdl-template-subtype t]
       ["Type"			vhdl-template-type t]
       ["Use"			vhdl-template-use t]
       ["Variable"		vhdl-template-variable t]
       ["Wait"			vhdl-template-wait t]
       ["(Clocked Wait)"	vhdl-template-clocked-wait t]
       ["When"			vhdl-template-when t]
       ["While (Loop)"		vhdl-template-while-loop t]
       ["With"			vhdl-template-with t]
       ))
    (when (vhdl-standard-p 'ams)
      '(("VHDL-AMS Construct"
	 ["Break"		vhdl-template-break t]
	 ["Case (Use)"		vhdl-template-case-use t]
	 ["If (Use)"		vhdl-template-if-use t]
	 ["Limit"		vhdl-template-limit t]
	 ["Nature"		vhdl-template-nature t]
	 ["Procedural"		vhdl-template-procedural t]
	 ["Quantity (Free)"	vhdl-template-quantity-free t]
	 ["Quantity (Branch)"	vhdl-template-quantity-branch t]
	 ["Quantity (Source)"	vhdl-template-quantity-source t]
	 ["Subnature"		vhdl-template-subnature t]
	 ["Terminal"		vhdl-template-terminal t]
	 )))
    '(["Insert Construct"	vhdl-template-insert-construct
       :keys "C-c C-i C-c"]
      "--")
    (list
     (append
      '("Package")
      (when (vhdl-standard-p 'math)
	'(
	  ["math_complex"	vhdl-template-package-math-complex t]
	  ["math_real"		vhdl-template-package-math-real t]
	  ))
      '(
	["numeric_bit"		vhdl-template-package-numeric-bit t]
	["numeric_std"		vhdl-template-package-numeric-std t]
	["std_logic_1164"	vhdl-template-package-std-logic-1164 t]
	["textio"		vhdl-template-package-textio t]
	"--"
	["std_logic_arith"	vhdl-template-package-std-logic-arith t]
	["std_logic_signed"	vhdl-template-package-std-logic-signed t]
	["std_logic_unsigned"	vhdl-template-package-std-logic-unsigned t]
	["std_logic_misc"	vhdl-template-package-std-logic-misc t]
	["std_logic_textio"	vhdl-template-package-std-logic-textio t]
	"--"
	["Insert Package"	vhdl-template-insert-package
	 :keys "C-c C-i C-p"]
	)))
    '(("Directive"
       ["translate_on"		vhdl-template-directive-translate-on t]
       ["translate_off"		vhdl-template-directive-translate-off t]
       ["synthesis_on"		vhdl-template-directive-synthesis-on t]
       ["synthesis_off"		vhdl-template-directive-synthesis-off t]
       "--"
       ["Insert Directive"	vhdl-template-insert-directive
	:keys "C-c C-i C-d"]
       )
      "--"
      ["Insert Header"		vhdl-template-header :keys "C-c C-t C-h"]
      ["Insert Footer"		vhdl-template-footer t]
      ["Insert Date"		vhdl-template-insert-date t]
      ["Modify Date"		vhdl-template-modify :keys "C-c C-t C-m"]
      "--"
      ["Query Next Prompt"	vhdl-template-search-prompt t]
      ))
   (append
    '("Model")
    ;; add menu entries for defined models
    (let ((model-alist vhdl-model-alist) menu-alist model)
      (while model-alist
	(setq model (car model-alist))
	(setq menu-alist
	      (cons (vector
		     (nth 0 model)
		     (vhdl-function-name "vhdl-model" (nth 0 model))
		     :keys (concat "C-c C-m " (key-description (nth 2 model))))
		    menu-alist))
	(setq model-alist (cdr model-alist)))
      (setq menu-alist
	    (append
	     (nreverse menu-alist)
	     '("--"
	       ["Insert Model" vhdl-model-insert  :keys "C-c C-i C-m"]
	       ["Add Model..." (customize-variable 'vhdl-model-alist) t])))
      menu-alist))
   '("Port"
     ["Copy"			vhdl-port-copy t]
     "--"
     ["Paste As Entity"		vhdl-port-paste-entity vhdl-port-list]
     ["Paste As Component"	vhdl-port-paste-component vhdl-port-list]
     ["Paste As Instance"	vhdl-port-paste-instance
				:keys "C-c C-p C-i" :active vhdl-port-list]
     ["Paste As Signals"	vhdl-port-paste-signals vhdl-port-list]
     ["Paste As Constants"	vhdl-port-paste-constants vhdl-port-list]
     ["Paste As Generic Map"	vhdl-port-paste-generic-map vhdl-port-list]
     ["Paste As Test Bench"	vhdl-port-paste-testbench vhdl-port-list]
     "--"
     ["Flatten"			vhdl-port-flatten vhdl-port-list]
     )
   "--"
   '("Comment"
     ["(Un)Comment Out Region"	vhdl-comment-uncomment-region (mark)]
     "--"
     ["Insert Inline Comment"	vhdl-comment-append-inline t]
     ["Insert Horizontal Line"	vhdl-comment-display-line t]
     ["Insert Display Comment"	vhdl-comment-display t]
     "--"
     ["Fill Comment"		fill-paragraph t]
     ["Fill Comment Region"	fill-region (mark)]
     ["Kill Comment Region"	vhdl-comment-kill-region (mark)]
     ["Kill Inline Comment Region" vhdl-comment-kill-inline-region (mark)]
     )
   '("Line"
     ["Kill"			vhdl-line-kill t]
     ["Copy"			vhdl-line-copy t]
     ["Yank"			vhdl-line-yank t]
     ["Expand"			vhdl-line-expand t]
     "--"
     ["Transpose Next"		vhdl-line-transpose-next t]
     ["Transpose Prev"		vhdl-line-transpose-previous t]
     ["Open"			vhdl-line-open t]
     ["Join"			delete-indentation t]
     "--"
     ["Goto"			goto-line t]
     ["(Un)Comment Out"		vhdl-comment-uncomment-line t]
     )
   '("Move"
     ["Forward Statement"	vhdl-end-of-statement t]
     ["Backward Statement"	vhdl-beginning-of-statement t]
     ["Forward Expression"	vhdl-forward-sexp t]
     ["Backward Expression"	vhdl-backward-sexp t]
     ["Forward Function"	vhdl-end-of-defun t]
     ["Backward Function"	vhdl-beginning-of-defun t]
     ["Mark Function"		vhdl-mark-defun t]
     )
   "--"
   '("Indent"
     ["Line"			vhdl-indent-line t]
     ["Region"			vhdl-indent-region (mark)]
     ["Buffer"			vhdl-indent-buffer t]
     )
   '("Align"
     ["Group"			vhdl-align-group t]
     ["Region"			vhdl-align-noindent-region (mark)]
     ["Buffer"			vhdl-align-noindent-buffer t]
     "--"
     ["Inline Comment Group"	vhdl-align-inline-comment-group t]
     ["Inline Comment Region"	vhdl-align-inline-comment-region (mark)]
     ["Inline Comment Buffer"	vhdl-align-inline-comment-buffer t]
     "--"
     ["Fixup Whitespace Region" vhdl-fixup-whitespace-region (mark)]
     ["Fixup Whitespace Buffer" vhdl-fixup-whitespace-buffer t]
     )
   '("Fix Case"
     ["Region"			vhdl-fix-case-region (mark)]
     ["Buffer"			vhdl-fix-case-buffer t]
     )
   '("Beautify"
     ["Beautify Region"		vhdl-beautify-region (mark)]
     ["Beautify Buffer"		vhdl-beautify-buffer t]
     )
   "--"
   ["Fontify Buffer"		vhdl-fontify-buffer t]
   ["Syntactic Info"		vhdl-show-syntactic-information t]
   "--"
   '("Documentation"
     ["VHDL Mode"		vhdl-doc-mode :keys "C-c C-h"]
     ["Reserved Words"		(vhdl-doc-variable 'vhdl-doc-keywords) t]
     ["Coding Style"		(vhdl-doc-variable 'vhdl-doc-coding-style) t]
     )
   ["Version"			vhdl-version t]
   ["Bug Report..."		vhdl-submit-bug-report t]
   "--"
   '("Speedbar"
     ["Open/Close"		vhdl-speedbar t]
     "--"
     ["Show Hierarchy"		vhdl-speedbar-toggle-hierarchy
      :style toggle
      :selected
      (and (boundp 'speedbar-initial-expansion-list-name)
	   (equal speedbar-initial-expansion-list-name "vhdl hierarchy"))
      :active (and (boundp 'speedbar-frame) speedbar-frame)]
     )
   "--"
   '("Customize"
     ["Browse VHDL Group..."	vhdl-customize t]
     ["Build Customize Menu"	vhdl-create-customize-menu
				(fboundp 'customize-menu-create)]
     "--"
     ["Activate New Customizations" vhdl-activate-customizations t])
   ))

(defvar vhdl-mode-menu-list (vhdl-create-mode-menu)
  "VHDL Mode menu.")

(defun vhdl-update-mode-menu ()
  "Update VHDL mode menu."
  (interactive)
  (easy-menu-remove vhdl-mode-menu-list) ; for XEmacs
  (setq vhdl-mode-menu-list (vhdl-create-mode-menu))
  (easy-menu-add vhdl-mode-menu-list)	; for XEmacs
  (easy-menu-define vhdl-mode-menu vhdl-mode-map
		    "Menu keymap for VHDL Mode." vhdl-mode-menu-list))

(require 'easymenu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Index menu (using `imenu.el'), also used for speedbar (using `speedbar.el')

(defvar vhdl-imenu-generic-expression
  '(
    ("Subprogram"
     "^\\s-*\\(\\(\\(impure\\|pure\\)\\s-+\\|\\)function\\|procedure\\)\\s-+\\(\"?\\(\\w\\|\\s_\\)+\"?\\)"
     4)
    ("Instance"
     "^\\s-*\\(\\(\\w\\|\\s_\\)+\\s-*:\\(\\s-\\|\n\\)*\\(\\w\\|\\s_\\)+\\)\\(\\s-\\|\n\\)+\\(generic\\|port\\)\\s-+map\\>"
     1)
    ("Component"
     "^\\s-*\\(component\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)"
     2)
    ("Procedural"
     "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(procedural\\)"
     1)
    ("Process"
     "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(\\(postponed\\s-+\\|\\)process\\)"
     1)
    ("Block"
     "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(block\\)"
     1)
    ("Package"
     "^\\s-*\\(package\\( body\\|\\)\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)"
     3)
    ("Configuration"
     "^\\s-*\\(configuration\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\s-+of\\s-+\\(\\w\\|\\s_\\)+\\)"
     2)
    ("Architecture"
     "^\\s-*\\(architecture\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\s-+of\\s-+\\(\\w\\|\\s_\\)+\\)"
     2)
    ("Entity"
     "^\\s-*\\(entity\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)"
     2)
    )
  "Imenu generic expression for VHDL Mode.  See `imenu-generic-expression'.")

(defun vhdl-index-menu-init ()
  "Initialize index menu."
  (set (make-local-variable 'imenu-case-fold-search) t)
  (set (make-local-variable 'imenu-generic-expression)
       vhdl-imenu-generic-expression)
  (when (and vhdl-index-menu (not (string-match "XEmacs" emacs-version)))
    (if (or (not (boundp 'font-lock-maximum-size))
	    (> font-lock-maximum-size (buffer-size)))
	(imenu-add-to-menubar "Index")
      (message "Scanning buffer for index...buffer too big"))))

;; ############################################################################
;; Source file menu (using `easy-menu.el')

(defvar vhdl-sources-menu nil)

(defun vhdl-directory-files (directory &optional full match)
  "Call `directory-files' if DIRECTORY exists, otherwise generate error
message."
  (if (file-directory-p directory)
      (directory-files directory full match)
    (message "No such directory: \"%s\"" directory)
    nil))

(defun vhdl-get-source-files (&optional full directory)
  "Get list of VHDL source files in DIRECTORY or current directory."
  (let ((mode-alist auto-mode-alist)
	filename-regexp)
    ;; create regular expressions for matching file names
    (setq filename-regexp ".*\\(")
    (while mode-alist
      (when (eq (cdr (car mode-alist)) 'vhdl-mode)
	(setq filename-regexp
	      (concat filename-regexp (car (car mode-alist)) "\\|")))
      (setq mode-alist (cdr mode-alist)))
    (setq filename-regexp
	  (concat (substring filename-regexp 0
			     (string-match "\\\\|$" filename-regexp)) "\\)"))
    ;; find files
    (nreverse (vhdl-directory-files
	       (or directory default-directory) full filename-regexp))))

(defun vhdl-add-source-files-menu ()
  "Scan directory for all VHDL source files and generate menu.
The directory of the current source file is scanned."
  (interactive)
  (message "Scanning directory for source files ...")
  (let ((newmap (current-local-map))
	(mode-alist auto-mode-alist)
	(file-list (vhdl-get-source-files))
	menu-list found)
    ;; Create list for menu
    (setq found nil)
    (while file-list
      (setq found t)
      (setq menu-list (cons (vector (car file-list)
				   (list 'find-file (car file-list)) t)
			   menu-list))
      (setq file-list (cdr file-list)))
    (setq menu-list (vhdl-menu-split menu-list 25))
    (when found (setq menu-list (cons "--" menu-list)))
    (setq menu-list (cons ["*Rescan*" vhdl-add-source-files-menu t] menu-list))
    (setq menu-list (cons "Sources" menu-list))
    ;; Create menu
    (easy-menu-add menu-list)
    (easy-menu-define vhdl-sources-menu newmap
		      "VHDL source files menu" menu-list))
  (message ""))

(defun vhdl-menu-split (list n)
  "Split menu LIST into several submenues, if number of elements > N."
  (if (> (length list) n)
      (let ((remain list)
	    (result '())
	    (sublist '())
	    (menuno 1)
	    (i 0))
	(while remain
	  (setq sublist (cons (car remain) sublist))
	  (setq remain (cdr remain))
	  (setq i (+ i 1))
	  (if (= i n)
	      (progn
		(setq result (cons (cons (format "Sources %s" menuno)
					 (nreverse sublist)) result))
		(setq i 0)
		(setq menuno (+ menuno 1))
		(setq sublist '()))))
	(and sublist
	     (setq result (cons (cons (format "Sources %s" menuno)
				      (nreverse sublist)) result)))
	(nreverse result))
    list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VHDL Mode definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; performs all buffer local initializations

;;;###autoload
(defun vhdl-mode ()
  "Major mode for editing VHDL code.

Usage:
------

- TEMPLATE INSERTION (electrification):  After typing a VHDL keyword and
  entering `\\[vhdl-electric-space]', you are prompted for arguments while a template is generated
  for that VHDL construct.  Typing `\\[vhdl-electric-return]' or `\\[keyboard-quit]' at the first (mandatory)
  prompt aborts the current template generation.  Optional arguments are
  indicated by square brackets and removed if the queried string is left empty.
  Prompts for mandatory arguments remain in the code if the queried string is
  left empty.  They can be queried again by `\\[vhdl-template-search-prompt]'.
  Typing `\\[just-one-space]' after a keyword inserts a space without calling the template
  generator.  Automatic template generation (i.e. electrification) can be
  disabled (enabled) by typing `\\[vhdl-electric-mode]' or by setting custom variable
  `vhdl-electric-mode' (see CUSTOMIZATION).
  Enabled electrification is indicated by `/e' in the modeline.
  Template generators can be invoked from the VHDL menu, by key bindings, by
  typing `C-c C-i C-c' and choosing a construct, or by typing the keyword (i.e.
  first word of menu entry not in parenthesis) and `\\[vhdl-electric-space]'.
  The following abbreviations can also be used:
  arch, attr, cond, conf, comp, cons, func, inst, pack, sig, var.
  Template styles can be customized in customization group `vhdl-electric'
  \(see CUSTOMIZATION).

- HEADER INSERTION:  A file header can be inserted by `\\[vhdl-template-header]'.  A
  file footer (template at the end of the file) can be inserted by
  `\\[vhdl-template-footer]'.  See customization group `vhdl-header'.

- STUTTERING:  Double striking of some keys inserts cumbersome VHDL syntax
  elements.  Stuttering can be disabled (enabled) by typing `\\[vhdl-stutter-mode]' or by
  variable `vhdl-stutter-mode'.  Enabled stuttering is indicated by `/s' in
  the modeline.  The stuttering keys and their effects are:
      ;;   -->  \" : \"         [   -->  (        --    -->  comment
      ;;;  -->  \" := \"        [[  -->  [        --CR  -->  comment-out code
      ..   -->  \" => \"        ]   -->  )        ---   -->  horizontal line
      ,,   -->  \" <= \"        ]]  -->  ]        ----  -->  display comment
      ==   -->  \" == \"        ''  -->  \\\"

- WORD COMPLETION:  Typing `\\[vhdl-electric-tab]' after a (not completed) word looks for a VHDL
  keyword or a word in the buffer that starts alike, inserts it and adjusts
  case.  Re-typing `\\[vhdl-electric-tab]' toggles through alternative word completions.
  This also works in the minibuffer (i.e. in template generator prompts).
  Typing `\\[vhdl-electric-tab]' after `(' looks for and inserts complete parenthesized
  expressions (e.g. for array index ranges).  All keywords as well as standard
  types and subprograms of VHDL have predefined abbreviations (e.g. type \"std\"
  and `\\[vhdl-electric-tab]' will toggle through all standard types beginning with \"std\").

  Typing `\\[vhdl-electric-tab]' after a non-word character indents the line if at the beginning
  of a line (i.e. no preceding non-blank characters),and inserts a tabulator
  stop otherwise.  `\\[tab-to-tab-stop]' always inserts a tabulator stop.

- COMMENTS:
      `--'       puts a single comment.
      `---'      draws a horizontal line for separating code segments.
      `----'     inserts a display comment, i.e. two horizontal lines with a
                 comment in between.
      `--CR'     comments out code on that line.  Re-hitting CR comments out
                 following lines.
      `\\[vhdl-comment-uncomment-region]'  comments out a region if not commented out,
                 uncomments a region if already commented out.

  You are prompted for comments after object definitions (i.e. signals,
  variables, constants, ports) and after subprogram and process specifications
  if variable `vhdl-prompt-for-comments' is non-nil.  Comments are
  automatically inserted as additional labels (e.g. after begin statements) and
  as help comments if `vhdl-self-insert-comments' is non-nil.
  Inline comments (i.e. comments after a piece of code on the same line) are
  indented at least to `vhdl-inline-comment-column'.  Comments go at maximum to
  `vhdl-end-comment-column'.  `\\[vhdl-electric-return]' after a space in a comment will open a
  new comment line.  Typing beyond `vhdl-end-comment-column' in a comment
  automatically opens a new comment line.  `\\[fill-paragraph]' re-fills
  multi-line comments.

- INDENTATION:  `\\[vhdl-electric-tab]' indents a line if at the beginning of the line.
  The amount of indentation is specified by variable `vhdl-basic-offset'.
  `\\[vhdl-indent-line]' always indents the current line (is bound to `TAB' if variable
  `vhdl-intelligent-tab' is nil).  Indentation can be done for an entire region
  \(`\\[vhdl-indent-region]') or buffer (menu).  Argument and port lists are indented normally
  \(nil) or relative to the opening parenthesis (non-nil) according to variable
  `vhdl-argument-list-indent'.  If variable `vhdl-indent-tabs-mode' is nil,
  spaces are used instead of tabs.  `\\[tabify]' and `\\[untabify]' allow
  to convert spaces to tabs and vice versa.

- ALIGNMENT:  The alignment functions align operators, keywords, and inline
  comment to beautify argument lists, port maps, etc.  `\\[vhdl-align-group]' aligns a group
  of consecutive lines separated by blank lines.  `\\[vhdl-align-noindent-region]' aligns an
  entire region.  If variable `vhdl-align-groups' is non-nil, groups of code
  lines separated by empty lines are aligned individually.  `\\[vhdl-align-inline-comment-group]' aligns
  inline comments for a group of lines, and `\\[vhdl-align-inline-comment-region]' for a region.
  Some templates are automatically aligned after generation if custom variable
  `vhdl-auto-align' is non-nil.
  `\\[vhdl-fixup-whitespace-region]' fixes up whitespace in a region.  That is, operator symbols
  are surrounded by one space, and multiple spaces are eliminated.

- PORT TRANSLATION:  Generic and port clauses from entity or component
  declarations can be copied (`\\[vhdl-port-copy]') and pasted as entity and
  component declarations, as component instantiations and corresponding
  internal constants and signals, as a generic map with constants as actual
  parameters, and as a test bench (menu).
  A clause with several generic/port names on the same line can be flattened
  (`\\[vhdl-port-flatten]') so that only one name per line exists.  Names for actual
  ports, instances, test benches, and design-under-test instances can be
  derived from existing names according to variables `vhdl-...-name'.
  Variables `vhdl-testbench-...' allow the insertion of additional templates
  into a test bench.  New files are created for the test bench entity and
  architecture according to variable `vhdl-testbench-create-files'.
  See customization group `vhdl-port'.

- TEST BENCH GENERATION:  See PORT TRANSLATION.

- KEY BINDINGS:  Key bindings (`C-c ...') exist for most commands (see in
  menu).

- VHDL MENU:  All commands can be invoked from the VHDL menu.

- FILE BROWSER:  The speedbar allows browsing of directories and file contents.
  It can be accessed from the VHDL menu and is automatically opened if
  variable `vhdl-speedbar' is non-nil.
  In speedbar, open files and directories with `mouse-2' on the name and
  browse/rescan their contents with `mouse-2'/`S-mouse-2' on the `+'.

- DESIGN HIERARCHY BROWSER:  The speedbar can also be used for browsing the
  hierarchy of design units contained in the source files of the current
  directory or in the source files/directories specified for a project (see
  variable `vhdl-project-alist').
  The speedbar can be switched between file and hierarchy browsing mode in the
  VHDL menu or by typing `f' and `h' in speedbar.
  In speedbar, open design units with `mouse-2' on the name and browse their
  hierarchy with `mouse-2' on the `+'.  The hierarchy can be rescanned and
  ports directly be copied from entities by using the speedbar menu.

- PROJECTS:  Projects can be defined in variable `vhdl-project-alist' and a
  current project be selected using variable `vhdl-project' (permanently) or
  from the menu (temporarily).  For each project, a title string (for the file
  headers) and source files/directories (for the hierarchy browser) can be
  specified.

- SPECIAL MENUES:  As an alternative to the speedbar, an index menu can
  be added (set variable `vhdl-index-menu' to non-nil) or made accessible
  as a mouse menu (e.g. add \"(global-set-key '[S-down-mouse-3] 'imenu)\" to
  your start-up file) for browsing the file contents.  Also, a source file menu
  can be added (set variable `vhdl-source-file-menu' to non-nil) for browsing
  the current directory for VHDL source files.

- SOURCE FILE COMPILATION:  The syntax of the current buffer can be analyzed
  by calling a VHDL compiler (menu, `\\[vhdl-compile]').  The compiler to be used is
  specified by variable `vhdl-compiler'.  The available compilers are listed
  in variable `vhdl-compiler-alist' including all required compilation command,
  destination directory, and error message syntax information.  New compilers
  can be added.  Additional compile command options can be set in variable
  `vhdl-compiler-options'.
  An entire hierarchy of source files can be compiled by the `make' command
  \(menu, `\\[vhdl-make]').  This only works if an appropriate Makefile exists.
  The make command itself as well as a command to generate a Makefile can also
  be specified in variable `vhdl-compiler-alist'.

- VHDL STANDARDS:  The VHDL standards to be used are specified in variable
  `vhdl-standard'.  Available standards are: VHDL'87/'93, VHDL-AMS,
  Math Packages.

- KEYWORD CASE:  Lower and upper case for keywords and standardized types,
  attributes, and enumeration values is supported.  If the variable
  `vhdl-upper-case-keywords' is set to non-nil, keywords can be typed in lower
  case and are converted into upper case automatically (not for types,
  attributes, and enumeration values).  The case of keywords, types,
  attributes,and enumeration values can be fixed for an entire region (menu)
  or buffer (`\\[vhdl-fix-case-buffer]') according to the variables
  `vhdl-upper-case-{keywords,types,attributes,enum-values}'.

- HIGHLIGHTING (fontification):  Keywords and standardized types, attributes,
  enumeration values, and function names (controlled by variable
  `vhdl-highlight-keywords'), as well as comments, strings, and template
  prompts are highlighted using different colors.  Unit, subprogram, signal,
  variable, constant, parameter and generic/port names in declarations as well
  as labels are highlighted if variable `vhdl-highlight-names' is non-nil.

  Additional reserved words or words with a forbidden syntax (e.g. words that
  should be avoided) can be specified in variable `vhdl-forbidden-words' or
  `vhdl-forbidden-syntax' and be highlighted in a warning color (variable
  `vhdl-highlight-forbidden-words').  Verilog keywords are highlighted as
  forbidden words if variable `vhdl-highlight-verilog-keywords' is non-nil.

  Words with special syntax can be highlighted by specifying their syntax and
  color in variable `vhdl-special-syntax-alist' and by setting variable
  `vhdl-highlight-special-words' to non-nil.  This allows to establish some
  naming conventions (e.g. to distinguish different kinds of signals or other
  objects by using name suffices) and to support them visually.

  Variable `vhdl-highlight-case-sensitive' can be set to non-nil in order to
  support case-sensitive highlighting.  However, keywords are then only
  highlighted if written in lower case.

  Code between \"translate_off\" and \"translate_on\" pragmas is highlighted
  using a different background color if variable `vhdl-highlight-translate-off'
  is non-nil.

  All colors can be customized by command `\\[customize-face]'.
  For highlighting of matching parenthesis, see customization group
  `paren-showing' (`\\[customize-group]').

- USER MODELS:  VHDL models (templates) can be specified by the user and made
  accessible in the menu, through key bindings (`C-c C-m ...'), or by keyword
  electrification.  See custom variable `vhdl-model-alist'.

- HIDE/SHOW:  The code of entire VHDL design units can be hidden using the
  `Hide/Show' menu or by pressing `S-mouse-2' within the code (variable
  `vhdl-hideshow-menu').

- PRINTING:  Postscript printing with different faces (an optimized set of
  faces is used if `vhdl-print-customize-faces' is non-nil) or colors
  \(if `ps-print-color-p' is non-nil) is possible using the standard Emacs
  postscript printing commands.  Variable `vhdl-print-two-column' defines
  appropriate default settings for nice landscape two-column printing.  The
  paper format can be set by variable `ps-paper-type'.  Do not forget to
  switch `ps-print-color-p' to nil for printing on black-and-white printers.

- CUSTOMIZATION:  All variables can easily be customized using the `Customize'
  menu entry or `\\[customize-option]' (`\\[customize-group]' for groups).
  Some customizations only take effect after some action (read the NOTE in
  the variable documentation).  Customization can also be done globally (i.e.
  site-wide, read the INSTALL file).

- FILE EXTENSIONS:  As default, files with extensions \".vhd\" and \".vhdl\" are
  automatically recognized as VHDL source files.  To add an extension \".xxx\",
  add the following line to your Emacs start-up file (`.emacs'):
    \(setq auto-mode-alist (cons '(\"\\\\.xxx\\\\'\" . vhdl-mode) auto-mode-alist))

- HINTS:
  - Type `\\[keyboard-quit] \\[keyboard-quit]' to interrupt long operations or if Emacs hangs.


Maintenance:
------------

To submit a bug report, enter `\\[vhdl-submit-bug-report]' within VHDL Mode.
Add a description of the problem and include a reproducible test case.

Questions and enhancement requests can be sent to <vhdl-mode@geocities.com>.

The `vhdl-mode-announce' mailing list informs about new VHDL Mode releases.
The `vhdl-mode-victims' mailing list informs about new VHDL Mode beta releases.
You are kindly invited to participate in beta testing.  Subscribe to above
mailing lists by sending an email to <vhdl-mode@geocities.com>.

VHDL Mode is officially distributed on the Emacs VHDL Mode Home Page
<http://www.geocities.com/SiliconValley/Peaks/8287>, where the latest
version and release notes can be found.


Bugs and Limitations:
---------------------

- Re-indenting large regions or expressions can be slow.
- Indentation bug in simultaneous if- and case-statements (VHDL-AMS).
- Hideshow does not work under XEmacs.
- Index menu and file tagging in speedbar do not work under XEmacs.
- Parsing compilation error messages for Ikos and Viewlogic VHDL compilers
  does not work under XEmacs.


                                                  The VHDL Mode Maintainers
                                                Reto Zimmermann and Rod Whitby

Key bindings:
-------------

\\{vhdl-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'vhdl-mode)
  (setq mode-name "VHDL")

  ;; set maps and tables
  (use-local-map vhdl-mode-map)
  (set-syntax-table vhdl-mode-syntax-table)
  (setq local-abbrev-table vhdl-mode-abbrev-table)

  ;; set local variable values
  (set (make-local-variable 'paragraph-start)
       "\\s-*\\(--+\\s-*$\\|[^ -]\\|$\\)")
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'indent-line-function) 'vhdl-indent-line)
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-column) vhdl-inline-comment-column)
  (set (make-local-variable 'end-comment-column) vhdl-end-comment-column)
  (set (make-local-variable 'comment-start-skip) "--+\\s-*")
  (set (make-local-variable 'comment-multi-line) nil)
  (set (make-local-variable 'indent-tabs-mode) vhdl-indent-tabs-mode)
  (set (make-local-variable 'hippie-expand-only-buffers) '(vhdl-mode))
  (set (make-local-variable 'hippie-expand-verbose) nil)

  ;; setup the comment indent variable in a Emacs version portable way
  ;; ignore any byte compiler warnings you might get here
  (when (boundp 'comment-indent-function)
    (make-local-variable 'comment-indent-function)
    (setq comment-indent-function 'vhdl-comment-indent))

  ;; initialize font locking
  (require 'font-lock)
  (set (make-local-variable 'font-lock-defaults)
       (list
	'vhdl-font-lock-keywords nil
	(not vhdl-highlight-case-sensitive) '((?\_ . "w")) 'beginning-of-line
	'(font-lock-syntactic-keywords . vhdl-font-lock-syntactic-keywords)))
  (set (make-local-variable 'font-lock-support-mode) 'lazy-lock-mode)
  (set (make-local-variable 'lazy-lock-defer-contextually) nil)
  (set (make-local-variable 'lazy-lock-defer-on-the-fly) t)
;  (set (make-local-variable 'lazy-lock-defer-time) 0.1)
  (set (make-local-variable 'lazy-lock-defer-on-scrolling) t)
  (turn-on-font-lock)

  ;; variables for source file compilation
  (require 'compile)
  (set (make-local-variable 'compilation-error-regexp-alist) nil)
  (set (make-local-variable 'compilation-file-regexp-alist) nil)

  ;; add index menu
  (vhdl-index-menu-init)
  ;; add source file menu
  (if vhdl-source-file-menu (vhdl-add-source-files-menu))
  ;; add VHDL menu
  (easy-menu-add vhdl-mode-menu-list)	; for XEmacs
  (easy-menu-define vhdl-mode-menu vhdl-mode-map
		    "Menu keymap for VHDL Mode." vhdl-mode-menu-list)
  ;; initialize hideshow and add menu
  (make-local-variable 'hs-minor-mode-hook)
  (vhdl-hideshow-init)
  (run-hooks 'menu-bar-update-hook)

  ;; add speedbar
  (when (fboundp 'speedbar)
    (condition-case ()			; due to bug in `speedbar-el' v0.7.2a
	(progn
	  (when (and vhdl-speedbar (not (and (boundp 'speedbar-frame)
					     (frame-live-p speedbar-frame))))
	    (speedbar-frame-mode 1)
	    (select-frame speedbar-attached-frame)))
      (error (vhdl-add-warning "Before using Speedbar, install included `speedbar.el' patch"))))

  ;; miscellaneous
  (vhdl-ps-print-init)
  (vhdl-modify-date-init)
  (vhdl-mode-line-update)
  (message "VHDL Mode %s.  Type C-c C-h for documentation."
	   vhdl-version)
  (vhdl-print-warnings)

  ;; run hooks
  (run-hooks 'vhdl-mode-hook))

(defun vhdl-activate-customizations ()
  "Activate all customizations on local variables."
  (interactive)
  (vhdl-mode-map-init)
  (use-local-map vhdl-mode-map)
  (set-syntax-table vhdl-mode-syntax-table)
  (setq comment-column vhdl-inline-comment-column)
  (setq end-comment-column vhdl-end-comment-column)
  (vhdl-modify-date-init)
  (vhdl-update-mode-menu)
  (vhdl-hideshow-init)
  (run-hooks 'menu-bar-update-hook)
  (vhdl-mode-line-update))

(defun vhdl-modify-date-init ()
  "Add/remove hook for modifying date when buffer is saved."
  (if vhdl-modify-date-on-saving
      (add-hook 'local-write-file-hooks 'vhdl-template-modify-noerror)
    (remove-hook 'local-write-file-hooks 'vhdl-template-modify-noerror)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vhdl-doc-keywords nil
  "Reserved words in VHDL:

VHDL'93 (IEEE Std 1076-1993):
  `vhdl-93-keywords'      : keywords
  `vhdl-93-types'         : standardized types
  `vhdl-93-attributes'    : standardized attributes
  `vhdl-93-enum-values'   : standardized enumeration values
  `vhdl-93-functions'     : standardized functions
  `vhdl-93-packages'      : standardized packages and libraries

VHDL-AMS (IEEE Std 1076.1):
  `vhdl-ams-keywords'     : keywords
  `vhdl-ams-types'        : standardized types
  `vhdl-ams-attributes'   : standardized attributes
  `vhdl-ams-enum-values'  : standardized enumeration values
  `vhdl-ams-functions'    : standardized functions

Math Packages (IEEE Std 1076.2):
  `vhdl-math-types'       : standardized types
  `vhdl-math-constants'   : standardized constants
  `vhdl-math-functions'   : standardized functions
  `vhdl-math-packages'    : standardized packages

Forbidden words:
  `vhdl-verilog-keywords' : Verilog reserved words

NOTE: click `mouse-2' on variable names above (not in XEmacs).")

(defvar vhdl-doc-coding-style nil
  "For VHDL coding style and naming convention guidelines, see the following
references:

\[1] Ben Cohen.
    \"VHDL Coding Styles and Methodologies\".
    Kluwer Academic Publishers, 1999.
    http://members.aol.com/vhdlcohen/vhdl/

\[2] Michael Keating and Pierre Bricaud.
    \"Reuse Methodology Manual\".
    Kluwer Academic Publishers, 1998.
    http://www.synopsys.com/products/reuse/rmm.html

\[3] European Space Agency.
    \"VHDL Modelling Guidelines\".
    ftp://ftp.estec.esa.nl/pub/vhdl/doc/ModelGuide.{pdf,ps}

Use variables `vhdl-highlight-special-words' and `vhdl-special-syntax-alist'
to visually support naming conventions.")

(defun vhdl-doc-variable (variable)
  "Display VARIABLE's documentation in *Help* buffer."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ (documentation-property variable 'variable-documentation))
    (unless (string-match "XEmacs" emacs-version)
      (help-setup-xref (list #'vhdl-doc-variable variable) (interactive-p)))
    (save-excursion
      (set-buffer standard-output)
      (help-mode))
    (print-help-return-message)))

(defun vhdl-doc-mode ()
  "Display VHDL mode documentation in *Help* buffer."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ mode-name)
    (princ " mode:\n")
    (princ (documentation 'vhdl-mode))
    (unless (string-match "XEmacs" emacs-version)
      (help-setup-xref (list #'vhdl-doc-mode) (interactive-p)))
    (save-excursion
      (set-buffer standard-output)
      (help-mode))
    (print-help-return-message)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keywords and standardized words
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst vhdl-93-keywords
  '(
    "abs" "access" "after" "alias" "all" "and" "architecture" "array"
    "assert" "attribute"
    "begin" "block" "body" "buffer" "bus"
    "case" "component" "configuration" "constant"
    "disconnect" "downto"
    "else" "elsif" "end" "entity" "exit"
    "file" "for" "function"
    "generate" "generic" "group" "guarded"
    "if" "impure" "in" "inertial" "inout" "is"
    "label" "library" "linkage" "literal" "loop"
    "map" "mod"
    "nand" "new" "next" "nor" "not" "null"
    "of" "on" "open" "or" "others" "out"
    "package" "port" "postponed" "procedure" "process" "pure"
    "range" "record" "register" "reject" "rem" "report" "return"
    "rol" "ror"
    "select" "severity" "shared" "signal" "sla" "sll" "sra" "srl" "subtype"
    "then" "to" "transport" "type"
    "unaffected" "units" "until" "use"
    "variable"
    "wait" "when" "while" "with"
    "xnor" "xor"
    )
  "List of VHDL'93 keywords.")

(defconst vhdl-ams-keywords
  '(
    "across" "break" "limit" "nature" "noise" "procedural" "quantity"
    "reference" "spectrum" "subnature" "terminal" "through"
    "tolerance"
    )
  "List of VHDL-AMS keywords.")

(defconst vhdl-verilog-keywords
  '(
    "`define" "`else" "`endif" "`ifdef" "`include" "`timescale" "`undef"
    "always" "and" "assign" "begin" "buf" "bufif0" "bufif1"
    "case" "casex" "casez" "cmos" "deassign" "default" "defparam" "disable"
    "edge" "else" "end" "endattribute" "endcase" "endfunction" "endmodule"
    "endprimitive" "endspecify" "endtable" "endtask" "event"
    "for" "force" "forever" "fork" "function"
    "highz0" "highz1" "if" "initial" "inout" "input" "integer" "join" "large"
    "macromodule" "makefile" "medium" "module"
    "nand" "negedge" "nmos" "nor" "not" "notif0" "notif1" "or" "output"
    "parameter" "pmos" "posedge" "primitive" "pull0" "pull1" "pulldown"
    "pullup"
    "rcmos" "real" "realtime" "reg" "release" "repeat" "rnmos" "rpmos" "rtran"
    "rtranif0" "rtranif1"
    "scalared" "signed" "small" "specify" "specparam" "strength" "strong0"
    "strong1" "supply" "supply0" "supply1"
    "table" "task" "time" "tran" "tranif0" "tranif1" "tri" "tri0" "tri1"
    "triand" "trior" "trireg"
    "vectored" "wait" "wand" "weak0" "weak1" "while" "wire" "wor" "xnor" "xor"
    )
  "List of Verilog keywords as candidate for additional reserved words.")

(defconst vhdl-93-types
  '(
    "boolean" "bit" "bit_vector" "character" "severity_level" "integer"
    "real" "time" "natural" "positive" "string" "line" "text" "side"
    "unsigned" "signed" "delay_length" "file_open_kind" "file_open_status"
    "std_logic" "std_logic_vector"
    "std_ulogic" "std_ulogic_vector"
    )
  "List of VHDL'93 standardized types.")

(defconst vhdl-ams-types
  '(
    "domain_type" "real_vector"
    )
  "List of VHDL-AMS standardized types.")

(defconst vhdl-math-types
  '(
    "complex" "complex_polar"
    )
  "List of Math Packages standardized types.")

(defconst vhdl-93-attributes
  '(
    "base" "left" "right" "high" "low" "pos" "val" "succ"
    "pred" "leftof" "rightof" "range" "reverse_range"
    "length" "delayed" "stable" "quiet" "transaction"
    "event" "active" "last_event" "last_active" "last_value"
    "driving" "driving_value" "ascending" "value" "image"
    "simple_name" "instance_name" "path_name"
    "foreign"
    )
  "List of VHDL'93 standardized attributes.")

(defconst vhdl-ams-attributes
  '(
    "across" "through"
    "reference" "contribution" "tolerance"
    "dot" "integ" "delayed" "above" "zoh" "ltf" "ztf"
    "ramp" "slew"
    )
  "List of VHDL-AMS standardized attributes.")

(defconst vhdl-93-enum-values
  '(
    "true" "false"
    "note" "warning" "error" "failure"
    "read_mode" "write_mode" "append_mode"
    "open_ok" "status_error" "name_error" "mode_error"
    "fs" "ps" "ns" "us" "ms" "sec" "min" "hr"
    "right" "left"
    )
  "List of VHDL'93 standardized enumeration values.")

(defconst vhdl-ams-enum-values
  '(
    "quiescent_domain" "time_domain" "frequency_domain"
    )
  "List of VHDL-AMS standardized enumeration values.")

(defconst vhdl-math-constants
  '(
    "math_e" "math_1_over_e"
    "math_pi" "math_two_pi" "math_1_over_pi"
    "math_half_pi" "math_q_pi" "math_3_half_pi"
    "math_log_of_2" "math_log_of_10" "math_log2_of_e" "math_log10_of_e"
    "math_sqrt2" "math_sqrt1_2" "math_sqrt_pi"
    "math_deg_to_rad" "math_rad_to_deg"
    "cbase_1" "cbase_j" "czero"
    )
  "List of Math Packages standardized constants.")

(defconst vhdl-93-functions
  '(
    "now" "resolved" "rising_edge" "falling_edge"
    "read" "readline" "write" "writeline" "endfile"
    "resize" "is_X" "std_match"
    "shift_left" "shift_right" "rotate_left" "rotate_right"
    "to_unsigned" "to_signed" "to_integer"
    "to_stdLogicVector" "to_stdULogic" "to_stdULogicVector"
    "to_bit" "to_bitVector" "to_X01" "to_X01Z" "to_UX01" "to_01"
    "conv_unsigned" "conv_signed" "conv_integer" "conv_std_logic_vector"
    "shl" "shr" "ext" "sxt"
    )
  "List of VHDL'93 standardized functions.")

(defconst vhdl-ams-functions
  '(
    "frequency"
    )
  "List of VHDL-AMS standardized functions.")

(defconst vhdl-math-functions
  '(
    "sign" "ceil" "floor" "round" "trunc" "fmax" "fmin" "uniform"
    "sqrt" "cbrt" "exp" "log"
    "sin" "cos" "tan" "arcsin" "arccos" "arctan"
    "sinh" "cosh" "tanh" "arcsinh" "arccosh" "arctanh"
    "cmplx" "complex_to_polar" "polar_to_complex" "arg" "conj"
    )
  "List of Math Packages standardized functions.")

(defconst vhdl-93-packages
  '(
    "std_logic_1164" "numeric_std" "numeric_bit"
    "standard" "textio"
    "std_logic_arith" "std_logic_signed" "std_logic_unsigned"
    "std_logic_misc" "std_logic_textio"
    "ieee" "std" "work"
    )
  "List of VHDL'93 standardized packages and libraries.")

(defconst vhdl-math-packages
  '(
    "math_real" "math_complex"
    )
  "List of Math Packages standardized packages and libraries.")

(defvar vhdl-keywords nil
  "List of VHDL keywords.")

(defvar vhdl-types nil
  "List of VHDL standardized types.")

(defvar vhdl-attributes nil
  "List of VHDL standardized attributes.")

(defvar vhdl-enum-values nil
  "List of VHDL standardized enumeration values.")

(defvar vhdl-constants nil
  "List of VHDL standardized constants.")

(defvar vhdl-functions nil
  "List of VHDL standardized functions.")

(defvar vhdl-packages nil
  "List of VHDL standardized packages and libraries.")

(defvar vhdl-reserved-words nil
  "List of additional reserved words.")

(defvar vhdl-keywords-regexp nil
  "Regexp for VHDL keywords.")

(defvar vhdl-types-regexp nil
  "Regexp for VHDL standardized types.")

(defvar vhdl-attributes-regexp nil
  "Regexp for VHDL standardized attributes.")

(defvar vhdl-enum-values-regexp nil
  "Regexp for VHDL standardized enumeration values.")

(defvar vhdl-functions-regexp nil
  "Regexp for VHDL standardized functions.")

(defvar vhdl-packages-regexp nil
  "Regexp for VHDL standardized packages and libraries.")

(defvar vhdl-reserved-words-regexp nil
  "Regexp for additional reserved words.")

(defun vhdl-words-init ()
  "Initialize reserved words."
  (setq vhdl-keywords
	(append vhdl-93-keywords
		(when (vhdl-standard-p 'ams) vhdl-ams-keywords)))
  (setq vhdl-types
	(append vhdl-93-types
		(when (vhdl-standard-p 'ams) vhdl-ams-types)
		(when (vhdl-standard-p 'math) vhdl-math-types)))
  (setq vhdl-attributes
	(append vhdl-93-attributes
		(when (vhdl-standard-p 'ams) vhdl-ams-attributes)))
  (setq vhdl-enum-values
	(append vhdl-93-enum-values
		(when (vhdl-standard-p 'ams) vhdl-ams-enum-values)))
  (setq vhdl-constants
	(append (when (vhdl-standard-p 'math) vhdl-math-constants)))
  (setq vhdl-functions
	(append vhdl-93-functions
		(when (vhdl-standard-p 'ams) vhdl-ams-functions)
		(when (vhdl-standard-p 'math) vhdl-math-functions)))
  (setq vhdl-packages
	(append vhdl-93-packages
		(when (vhdl-standard-p 'math) vhdl-math-packages)))
  (setq vhdl-reserved-words
	(append (when vhdl-highlight-forbidden-words vhdl-forbidden-words)
		(when vhdl-highlight-verilog-keywords vhdl-verilog-keywords)
		'("")))
  (setq vhdl-keywords-regexp
	(concat "\\<\\(" (regexp-opt vhdl-keywords) "\\)\\>"))
  (setq vhdl-types-regexp
	(concat "\\<\\(" (regexp-opt vhdl-types) "\\)\\>"))
  (setq vhdl-attributes-regexp
	(concat "\\<\\(" (regexp-opt vhdl-attributes) "\\)\\>"))
  (setq vhdl-enum-values-regexp
	(concat "\\<\\(" (regexp-opt vhdl-enum-values) "\\)\\>"))
  (setq vhdl-functions-regexp
	(concat "\\<\\(" (regexp-opt vhdl-functions) "\\)\\>"))
  (setq vhdl-packages-regexp
	(concat "\\<\\(" (regexp-opt vhdl-packages) "\\)\\>"))
  (setq vhdl-reserved-words-regexp
	(concat "\\<\\("
		(unless (equal vhdl-forbidden-syntax "")
		  (concat vhdl-forbidden-syntax "\\|"))
		(regexp-opt vhdl-reserved-words)
		"\\)\\>"))
  (vhdl-abbrev-list-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Words to expand

(defvar vhdl-abbrev-list nil
  "Predefined abbreviations for VHDL.")

(defun vhdl-abbrev-list-init ()
  (setq vhdl-abbrev-list
	(append
	 (list vhdl-upper-case-keywords) vhdl-keywords
	 (list vhdl-upper-case-types) vhdl-types
	 (list vhdl-upper-case-attributes) vhdl-attributes
	 (list vhdl-upper-case-enum-values) vhdl-enum-values
	 (list vhdl-upper-case-constants) vhdl-constants
	 (list nil) vhdl-functions
	 (list nil) vhdl-packages)))

;; initialize reserved words for VHDL Mode
(vhdl-words-init)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax analysis and indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax analysis

;; constant regular expressions for looking at various constructs

(defconst vhdl-symbol-key "\\(\\w\\|\\s_\\)+"
  "Regexp describing a VHDL symbol.
We cannot use just `word' syntax class since `_' cannot be in word
class.  Putting underscore in word class breaks forward word movement
behavior that users are familiar with.")

(defconst vhdl-case-header-key "case[( \t\n][^;=>]+[) \t\n]is"
  "Regexp describing a case statement header key.")

(defconst vhdl-label-key
  (concat "\\(" vhdl-symbol-key "\\s-*:\\)[^=]")
  "Regexp describing a VHDL label.")

;; Macro definitions:

(defmacro vhdl-point (position)
  "Return the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:

bol  -- beginning of line
eol  -- end of line
bod  -- beginning of defun
boi  -- back to indentation
eoi  -- last whitespace on line
ionl -- indentation of next line
iopl -- indentation of previous line
bonl -- beginning of next line
bopl -- beginning of previous line

This function does not modify point or mark."
  (or (and (eq 'quote (car-safe position))
	   (null (cdr (cdr position))))
      (error "Bad buffer position requested: %s" position))
  (setq position (nth 1 position))
  `(let ((here (point)))
     ,@(cond
	((eq position 'bol)  '((beginning-of-line)))
	((eq position 'eol)  '((end-of-line)))
	((eq position 'bod)  '((save-match-data
				 (vhdl-beginning-of-defun))))
	((eq position 'boi)  '((back-to-indentation)))
	((eq position 'eoi)  '((end-of-line)(skip-chars-backward " \t")))
	((eq position 'bonl) '((forward-line 1)))
	((eq position 'bopl) '((forward-line -1)))
	((eq position 'iopl)
	 '((forward-line -1)
	   (back-to-indentation)))
	((eq position 'ionl)
	 '((forward-line 1)
	   (back-to-indentation)))
	(t (error "Unknown buffer position requested: %s" position))
	)
     (prog1
	 (point)
       (goto-char here))
     ;; workaround for an Emacs18 bug -- blech! Well, at least it
     ;; doesn't hurt for v19
     ,@nil
     ))

(defmacro vhdl-safe (&rest body)
  "Safely execute BODY, return nil if an error occurred."
  `(condition-case nil
       (progn ,@body)
     (error nil)))

(defmacro vhdl-add-syntax (symbol &optional relpos)
  "A simple macro to append the syntax in SYMBOL to the syntax list.
Try to increase performance by using this macro."
  `(setq vhdl-syntactic-context
	 (cons (cons ,symbol ,relpos) vhdl-syntactic-context)))

(defmacro vhdl-has-syntax (symbol)
  "A simple macro to return check the syntax list.
Try to increase performance by using this macro."
  `(assoc ,symbol vhdl-syntactic-context))

;; Syntactic element offset manipulation:

(defun vhdl-read-offset (langelem)
  "Read new offset value for LANGELEM from minibuffer.
Return a legal value only."
  (let ((oldoff (format "%s" (cdr-safe (assq langelem vhdl-offsets-alist))))
	(errmsg "Offset must be int, func, var, or one of +, -, ++, --: ")
	(prompt "Offset: ")
	offset input interned)
    (while (not offset)
      (setq input (read-string prompt oldoff)
	    offset (cond ((string-equal "+" input) '+)
			 ((string-equal "-" input) '-)
			 ((string-equal "++" input) '++)
			 ((string-equal "--" input) '--)
			 ((string-match "^-?[0-9]+$" input)
			  (string-to-int input))
			 ((fboundp (setq interned (intern input)))
			  interned)
			 ((boundp interned) interned)
			 ;; error, but don't signal one, keep trying
			 ;; to read an input value
			 (t (ding)
			    (setq prompt errmsg)
			    nil))))
    offset))

(defun vhdl-set-offset (symbol offset &optional add-p)
  "Change the value of a syntactic element symbol in `vhdl-offsets-alist'.
SYMBOL is the syntactic element symbol to change and OFFSET is the new
offset for that syntactic element.  Optional ADD says to add SYMBOL to
`vhdl-offsets-alist' if it doesn't already appear there."
  (interactive
   (let* ((langelem
	   (intern (completing-read
		    (concat "Syntactic symbol to change"
			    (if current-prefix-arg " or add" "")
			    ": ")
		    (mapcar
		     (function
		      (lambda (langelem)
			(cons (format "%s" (car langelem)) nil)))
		     vhdl-offsets-alist)
		    nil (not current-prefix-arg)
		    ;; initial contents tries to be the last element
		    ;; on the syntactic analysis list for the current
		    ;; line
		    (let* ((syntax (vhdl-get-syntactic-context))
			   (len (length syntax))
			   (ic (format "%s" (car (nth (1- len) syntax)))))
		      ic)
		    )))
	  (offset (vhdl-read-offset langelem)))
     (list langelem offset current-prefix-arg)))
  ;; sanity check offset
  (or (eq offset '+)
      (eq offset '-)
      (eq offset '++)
      (eq offset '--)
      (integerp offset)
      (fboundp offset)
      (boundp offset)
      (error "Offset must be int, func, var, or one of +, -, ++, --: %s"
	     offset))
  (let ((entry (assq symbol vhdl-offsets-alist)))
    (if entry
	(setcdr entry offset)
      (if add-p
	  (setq vhdl-offsets-alist
		(cons (cons symbol offset) vhdl-offsets-alist))
	(error "%s is not a valid syntactic symbol" symbol))))
  (vhdl-keep-region-active))

(defun vhdl-set-style (style &optional local)
  "Set `vhdl-mode' variables to use one of several different indentation styles.
STYLE is a string representing the desired style and optional LOCAL is
a flag which, if non-nil, means to make the style variables being
changed buffer local, instead of the default, which is to set the
global variables.  Interactively, the flag comes from the prefix
argument.  The styles are chosen from the `vhdl-style-alist' variable."
  (interactive (list (completing-read "Use which VHDL indentation style? "
				      vhdl-style-alist nil t)
		     current-prefix-arg))
  (let ((vars (cdr (assoc style vhdl-style-alist))))
    (or vars
	(error "Invalid VHDL indentation style `%s'" style))
    ;; set all the variables
    (mapcar
     (function
      (lambda (varentry)
	(let ((var (car varentry))
	      (val (cdr varentry)))
	  (and local
	       (make-local-variable var))
	  ;; special case for vhdl-offsets-alist
	  (if (not (eq var 'vhdl-offsets-alist))
	      (set var val)
	    ;; reset vhdl-offsets-alist to the default value first
	    (setq vhdl-offsets-alist (copy-alist vhdl-offsets-alist-default))
	    ;; now set the langelems that are different
	    (mapcar
	     (function
	      (lambda (langentry)
		(let ((langelem (car langentry))
		      (offset (cdr langentry)))
		  (vhdl-set-offset langelem offset)
		  )))
	     val))
	  )))
     vars))
  (vhdl-keep-region-active))

(defun vhdl-get-offset (langelem)
  "Get offset from LANGELEM which is a cons cell of the form:
\(SYMBOL . RELPOS).  The symbol is matched against
vhdl-offsets-alist and the offset found there is either returned,
or added to the indentation at RELPOS.  If RELPOS is nil, then
the offset is simply returned."
  (let* ((symbol (car langelem))
	 (relpos (cdr langelem))
	 (match  (assq symbol vhdl-offsets-alist))
	 (offset (cdr-safe match)))
    ;; offset can be a number, a function, a variable, or one of the
    ;; symbols + or -
    (cond
     ((not match)
      (if vhdl-strict-syntax-p
	  (error "Don't know how to indent a %s" symbol)
	(setq offset 0
	      relpos 0)))
     ((eq offset '+)  (setq offset vhdl-basic-offset))
     ((eq offset '-)  (setq offset (- vhdl-basic-offset)))
     ((eq offset '++) (setq offset (* 2 vhdl-basic-offset)))
     ((eq offset '--) (setq offset (* 2 (- vhdl-basic-offset))))
     ((and (not (numberp offset))
	   (fboundp offset))
      (setq offset (funcall offset langelem)))
     ((not (numberp offset))
      (setq offset (eval offset)))
     )
    (+ (if (and relpos
		(< relpos (vhdl-point 'bol)))
	   (save-excursion
	     (goto-char relpos)
	     (current-column))
	 0)
       offset)))

;; Syntactic support functions:

;; Returns `comment' if in a comment, `string' if in a string literal,
;; or nil if not in a literal at all.  Optional LIM is used as the
;; backward limit of the search.  If omitted, or nil, (point-min) is
;; used.

(defun vhdl-in-literal (&optional lim)
  "Determine if point is in a VHDL literal."
  (save-excursion
    (let ((state (parse-partial-sexp (vhdl-point 'bol) (point))))
      (cond
       ((nth 3 state) 'string)
       ((nth 4 state) 'comment)
       (t nil)))))

;; This is the best we can do in Win-Emacs.
(defun vhdl-win-il (&optional lim)
  "Determine if point is in a VHDL literal."
  (save-excursion
    (let* ((here (point))
	   (state nil)
	   (match nil)
	   (lim  (or lim (vhdl-point 'bod))))
      (goto-char lim )
      (while (< (point) here)
	(setq match
	      (and (re-search-forward "--\\|[\"']"
				      here 'move)
		   (buffer-substring (match-beginning 0) (match-end 0))))
	(setq state
	      (cond
	       ;; no match
	       ((null match) nil)
	       ;; looking at the opening of a VHDL style comment
	       ((string= "--" match)
		(if (<= here (progn (end-of-line) (point))) 'comment))
	       ;; looking at the opening of a double quote string
	       ((string= "\"" match)
		(if (not (save-restriction
			   ;; this seems to be necessary since the
			   ;; re-search-forward will not work without it
			   (narrow-to-region (point) here)
			   (re-search-forward
			    ;; this regexp matches a double quote
			    ;; which is preceded by an even number
			    ;; of backslashes, including zero
			    "\\([^\\]\\|^\\)\\(\\\\\\\\\\)*\"" here 'move)))
		    'string))
	       ;; looking at the opening of a single quote string
	       ((string= "'" match)
		(if (not (save-restriction
			   ;; see comments from above
			   (narrow-to-region (point) here)
			   (re-search-forward
			    ;; this matches a single quote which is
			    ;; preceded by zero or two backslashes.
			    "\\([^\\]\\|^\\)\\(\\\\\\\\\\)?'"
			    here 'move)))
		    'string))
	       (t nil)))
	) ; end-while
      state)))

(and (string-match "Win-Emacs" emacs-version)
     (fset 'vhdl-in-literal 'vhdl-win-il))

;; Skipping of "syntactic whitespace".  Syntactic whitespace is
;; defined as lexical whitespace or comments.  Search no farther back
;; or forward than optional LIM.  If LIM is omitted, (point-min) is
;; used for backward skipping, (point-max) is used for forward
;; skipping.

(defun vhdl-forward-syntactic-ws (&optional lim)
  "Forward skip of syntactic whitespace."
  (save-restriction
    (let* ((lim (or lim (point-max)))
	   (here lim)
	   (hugenum (point-max)))
      (narrow-to-region lim (point))
      (while (/= here (point))
	(setq here (point))
	(forward-comment hugenum))
      )))

;; This is the best we can do in Win-Emacs.
(defun vhdl-win-fsws (&optional lim)
  "Forward skip syntactic whitespace for Win-Emacs."
  (let ((lim (or lim (point-max)))
	stop)
    (while (not stop)
      (skip-chars-forward " \t\n\r\f" lim)
      (cond
       ;; vhdl comment
       ((looking-at "--") (end-of-line))
       ;; none of the above
       (t (setq stop t))
       ))))

(and (string-match "Win-Emacs" emacs-version)
     (fset 'vhdl-forward-syntactic-ws 'vhdl-win-fsws))

(defun vhdl-backward-syntactic-ws (&optional lim)
  "Backward skip over syntactic whitespace."
  (save-restriction
    (let* ((lim (or lim (point-min)))
	   (here lim)
	   (hugenum (- (point-max))))
      (if (< lim (point))
	  (progn
	    (narrow-to-region lim (point))
	    (while (/= here (point))
	      (setq here (point))
	      (forward-comment hugenum)
	      )))
      )))

;; This is the best we can do in Win-Emacs.
(defun vhdl-win-bsws (&optional lim)
  "Backward skip syntactic whitespace for Win-Emacs."
  (let ((lim (or lim (vhdl-point 'bod)))
	stop)
    (while (not stop)
      (skip-chars-backward " \t\n\r\f" lim)
      (cond
       ;; vhdl comment
       ((eq (vhdl-in-literal lim) 'comment)
	(skip-chars-backward "^-" lim)
	(skip-chars-backward "-" lim)
	(while (not (or (and (= (following-char) ?-)
			     (= (char-after (1+ (point))) ?-))
			(<= (point) lim)))
	  (skip-chars-backward "^-" lim)
	  (skip-chars-backward "-" lim)))
       ;; none of the above
       (t (setq stop t))
       ))))

(and (string-match "Win-Emacs" emacs-version)
    (fset 'vhdl-backward-syntactic-ws 'vhdl-win-bsws))

;; Functions to help finding the correct indentation column:

(defun vhdl-first-word (point)
  "If the keyword at POINT is at boi, then return (current-column) at
that point, else nil."
  (save-excursion
    (and (goto-char point)
	 (eq (point) (vhdl-point 'boi))
	 (current-column))))

(defun vhdl-last-word (point)
  "If the keyword at POINT is at eoi, then return (current-column) at
that point, else nil."
  (save-excursion
    (and (goto-char point)
	 (save-excursion (or (eq (progn (forward-sexp) (point))
				 (vhdl-point 'eoi))
			     (looking-at "\\s-*\\(--\\)?")))
	 (current-column))))

;; Core syntactic evaluation functions:

(defconst vhdl-libunit-re
  "\\b\\(architecture\\|configuration\\|entity\\|package\\)\\b[^_]")

(defun vhdl-libunit-p ()
  (and
   (save-excursion
     (forward-sexp)
     (skip-chars-forward " \t\n")
     (not (looking-at "is\\b[^_]")))
   (save-excursion
     (backward-sexp)
     (and (not (looking-at "use\\b[^_]"))
	  (progn
	    (forward-sexp)
	    (vhdl-forward-syntactic-ws)
	    (/= (following-char) ?:))))
   ))

(defconst vhdl-defun-re
  "\\b\\(architecture\\|block\\|configuration\\|entity\\|package\\|process\\|procedural\\|procedure\\|function\\)\\b[^_]")

(defun vhdl-defun-p ()
  (save-excursion
    (if (looking-at "block\\|process\\|procedural")
	;; "block", "process", "procedural":
	(save-excursion
	  (backward-sexp)
	  (not (looking-at "end\\s-+\\w")))
      ;; "architecture", "configuration", "entity",
      ;; "package", "procedure", "function":
      t)))

(defun vhdl-corresponding-defun ()
  "If the word at the current position corresponds to a \"defun\"
keyword, then return a string that can be used to find the
corresponding \"begin\" keyword, else return nil."
  (save-excursion
    (and (looking-at vhdl-defun-re)
	 (vhdl-defun-p)
	 (if (looking-at "block\\|process\\|procedural")
	     ;; "block", "process". "procedural:
	     (buffer-substring (match-beginning 0) (match-end 0))
	   ;; "architecture", "configuration", "entity", "package",
	   ;; "procedure", "function":
	   "is"))))

(defconst vhdl-begin-fwd-re
  "\\b\\(is\\|begin\\|block\\|component\\|generate\\|then\\|else\\|loop\\|process\\|procedural\\|units\\|record\\|for\\)\\b\\([^_]\\|\\'\\)"
  "A regular expression for searching forward that matches all known
\"begin\" keywords.")

(defconst vhdl-begin-bwd-re
  "\\b\\(is\\|begin\\|block\\|component\\|generate\\|then\\|else\\|loop\\|process\\|procedural\\|units\\|record\\|for\\)\\b[^_]"
  "A regular expression for searching backward that matches all known
\"begin\" keywords.")

(defun vhdl-begin-p (&optional lim)
  "Return t if we are looking at a real \"begin\" keyword.
Assumes that the caller will make sure that we are looking at
vhdl-begin-fwd-re, and are not inside a literal, and that we are not in
the middle of an identifier that just happens to contain a \"begin\"
keyword."
  (cond
   ;; "[architecture|case|configuration|entity|package|
   ;;   procedure|function] ... is":
   ((and (looking-at "i")
	 (save-excursion
	   ;; Skip backward over first sexp (needed to skip over a
	   ;; procedure interface list, and is harmless in other
	   ;; situations).  Note that we need "return" in the
	   ;; following search list so that we don't run into
	   ;; semicolons in the function interface list.
	   (backward-sexp)
	   (let (foundp)
	     (while (and (not foundp)
			 (re-search-backward
			  ";\\|\\b\\(architecture\\|case\\|configuration\\|entity\\|package\\|procedure\\|return\\|is\\|begin\\|process\\|procedural\\|block\\)\\b[^_]"
			  lim 'move))
	       (if (or (= (preceding-char) ?_)
		       (vhdl-in-literal lim))
		   (backward-char)
		 (setq foundp t))))
	   (and (/= (following-char) ?\;)
		(not (looking-at "is\\|begin\\|process\\|procedural\\|block")))))
    t)
   ;; "begin", "then":
   ((looking-at "be\\|t")
    t)
   ;; "else":
   ((and (looking-at "e")
	 ;; make sure that the "else" isn't inside a
	 ;; conditional signal assignment.
	 (save-excursion
	   (re-search-backward ";\\|\\bwhen\\b[^_]" lim 'move)
	   (or (eq (following-char) ?\;)
	       (eq (point) lim))))
    t)
   ;; "block", "generate", "loop", "process", "procedural",
   ;; "units", "record":
   ((and (looking-at "bl\\|[glpur]")
	 (save-excursion
	   (backward-sexp)
	   (not (looking-at "end\\s-+\\w"))))
    t)
   ;; "component":
   ((and (looking-at "c")
	 (save-excursion
	   (backward-sexp)
	   (not (looking-at "end\\s-+\\w")))
	 ;; look out for the dreaded entity class in an attribute
	 (save-excursion
	   (vhdl-backward-syntactic-ws lim)
	   (/= (preceding-char) ?:)))
    t)
   ;; "for" (inside configuration declaration):
   ((and (looking-at "f")
	 (save-excursion
	   (backward-sexp)
	   (not (looking-at "end\\s-+\\w")))
	 (vhdl-has-syntax 'configuration))
    t)
   ))

(defun vhdl-corresponding-mid (&optional lim)
  (cond
   ((looking-at "is\\|block\\|generate\\|process\\|procedural")
    "begin")
   ((looking-at "then")
    "<else>")
   (t
    "end")))

(defun vhdl-corresponding-end (&optional lim)
  "If the word at the current position corresponds to a \"begin\"
keyword, then return a vector containing enough information to find
the corresponding \"end\" keyword, else return nil.  The keyword to
search forward for is aref 0.  The column in which the keyword must
appear is aref 1 or nil if any column is suitable.
Assumes that the caller will make sure that we are not in the middle
of an identifier that just happens to contain a \"begin\" keyword."
  (save-excursion
    (and (looking-at vhdl-begin-fwd-re)
	 (/= (preceding-char) ?_)
	 (not (vhdl-in-literal lim))
	 (vhdl-begin-p lim)
	 (cond
	  ;; "is", "generate", "loop":
	  ((looking-at "[igl]")
	   (vector "end"
		   (and (vhdl-last-word (point))
			(or (vhdl-first-word (point))
			    (save-excursion
			      (vhdl-beginning-of-statement-1 lim)
			      (vhdl-backward-skip-label lim)
			      (vhdl-first-word (point)))))))
	  ;; "begin", "else", "for":
	  ((looking-at "be\\|[ef]")
	   (vector "end"
		   (and (vhdl-last-word (point))
			(or (vhdl-first-word (point))
			    (save-excursion
			      (vhdl-beginning-of-statement-1 lim)
			      (vhdl-backward-skip-label lim)
			      (vhdl-first-word (point)))))))
	  ;; "component", "units", "record":
	  ((looking-at "[cur]")
	   ;; The first end found will close the block
	   (vector "end" nil))
	  ;; "block", "process", "procedural":
	  ((looking-at "bl\\|p")
	   (vector "end"
		   (or (vhdl-first-word (point))
		       (save-excursion
			 (vhdl-beginning-of-statement-1 lim)
			 (vhdl-backward-skip-label lim)
			 (vhdl-first-word (point))))))
	  ;; "then":
	  ((looking-at "t")
	   (vector "elsif\\|else\\|end\\s-+if"
		   (and (vhdl-last-word (point))
			(or (vhdl-first-word (point))
			    (save-excursion
			      (vhdl-beginning-of-statement-1 lim)
			      (vhdl-backward-skip-label lim)
			      (vhdl-first-word (point)))))))
	  ))))

(defconst vhdl-end-fwd-re "\\b\\(end\\|else\\|elsif\\)\\b\\([^_]\\|\\'\\)")

(defconst vhdl-end-bwd-re "\\b\\(end\\|else\\|elsif\\)\\b[^_]")

(defun vhdl-end-p (&optional lim)
  "Return t if we are looking at a real \"end\" keyword.
Assumes that the caller will make sure that we are looking at
vhdl-end-fwd-re, and are not inside a literal, and that we are not in
the middle of an identifier that just happens to contain an \"end\"
keyword."
  (or (not (looking-at "else"))
      ;; make sure that the "else" isn't inside a conditional signal
      ;; assignment.
      (save-excursion
	(re-search-backward ";\\|\\bwhen\\b[^_]" lim 'move)
	(or (eq (following-char) ?\;)
	    (eq (point) lim)))))

(defun vhdl-corresponding-begin (&optional lim)
  "If the word at the current position corresponds to an \"end\"
keyword, then return a vector containing enough information to find
the corresponding \"begin\" keyword, else return nil.  The keyword to
search backward for is aref 0.     The column in which the keyword must
appear is aref 1 or nil if any column is suitable.  The supplementary
keyword to search forward for is aref 2 or nil if this is not
required.  If aref 3 is t, then the \"begin\" keyword may be found in
the middle of a statement.
Assumes that the caller will make sure that we are not in the middle
of an identifier that just happens to contain an \"end\" keyword."
  (save-excursion
    (let (pos)
      (if (and (looking-at vhdl-end-fwd-re)
	       (not (vhdl-in-literal lim))
	       (vhdl-end-p lim))
	  (if (looking-at "el")
	      ;; "else", "elsif":
	      (vector "if\\|elsif" (vhdl-first-word (point)) "then" nil)
	    ;; "end ...":
	    (setq pos (point))
	    (forward-sexp)
	    (skip-chars-forward " \t\n")
	    (cond
	     ;; "end if":
	     ((looking-at "if\\b[^_]")
	      (vector "else\\|elsif\\|if"
		      (vhdl-first-word pos)
		      "else\\|then" nil))
	     ;; "end component":
	     ((looking-at "component\\b[^_]")
	      (vector (buffer-substring (match-beginning 1)
					(match-end 1))
		      (vhdl-first-word pos)
		      nil nil))
	     ;; "end units", "end record":
	     ((looking-at "\\(units\\|record\\)\\b[^_]")
	      (vector (buffer-substring (match-beginning 1)
					(match-end 1))
		      (vhdl-first-word pos)
		      nil t))
	     ;; "end block", "end process", "end procedural":
	     ((looking-at "\\(block\\|process\\|procedural\\)\\b[^_]")
	      (vector "begin" (vhdl-first-word pos) nil nil))
	     ;; "end case":
	     ((looking-at "case\\b[^_]")
	      (vector "case" (vhdl-first-word pos) "is" nil))
	     ;; "end generate":
	     ((looking-at "generate\\b[^_]")
	      (vector "generate\\|for\\|if"
		      (vhdl-first-word pos)
		      "generate" nil))
	     ;; "end loop":
	     ((looking-at "loop\\b[^_]")
	      (vector "loop\\|while\\|for"
		      (vhdl-first-word pos)
		      "loop" nil))
	     ;; "end for" (inside configuration declaration):
	     ((looking-at "for\\b[^_]")
	      (vector "for" (vhdl-first-word pos) nil nil))
	     ;; "end [id]":
	     (t
	      (vector "begin\\|architecture\\|configuration\\|entity\\|package\\|procedure\\|function"
		      (vhdl-first-word pos)
		      ;; return an alist of (statement . keyword) mappings
		      '(
			;; "begin ... end [id]":
			("begin"          . nil)
			;; "architecture ... is ... begin ... end [id]":
			("architecture"   . "is")
			;; "configuration ... is ... end [id]":
			("configuration"  . "is")
			;; "entity ... is ... end [id]":
			("entity"         . "is")
			;; "package ... is ... end [id]":
			("package"        . "is")
			;; "procedure ... is ... begin ... end [id]":
			("procedure"      . "is")
			;; "function ... is ... begin ... end [id]":
			("function"       . "is")
			)
		      nil))
	     ))) ; "end ..."
      )))

(defconst vhdl-leader-re
  "\\b\\(block\\|component\\|process\\|procedural\\|for\\)\\b[^_]")

(defun vhdl-end-of-leader ()
  (save-excursion
    (cond ((looking-at "block\\|process\\|procedural")
	   (if (save-excursion
		 (forward-sexp)
		 (skip-chars-forward " \t\n")
		 (= (following-char) ?\())
	       (forward-sexp 2)
	     (forward-sexp))
	   (point))
	  ((looking-at "component")
	   (forward-sexp 2)
	   (point))
	  ((looking-at "for")
	   (forward-sexp 2)
	   (skip-chars-forward " \t\n")
	   (while (looking-at "[,:(]")
	     (forward-sexp)
	     (skip-chars-forward " \t\n"))
	   (point))
	  (t nil)
	  )))

(defconst vhdl-trailer-re
  "\\b\\(is\\|then\\|generate\\|loop\\)\\b[^_]")

(defconst vhdl-statement-fwd-re
  "\\b\\(if\\|for\\|while\\)\\b\\([^_]\\|\\'\\)"
  "A regular expression for searching forward that matches all known
\"statement\" keywords.")

(defconst vhdl-statement-bwd-re
  "\\b\\(if\\|for\\|while\\)\\b[^_]"
  "A regular expression for searching backward that matches all known
\"statement\" keywords.")

(defun vhdl-statement-p (&optional lim)
  "Return t if we are looking at a real \"statement\" keyword.
Assumes that the caller will make sure that we are looking at
vhdl-statement-fwd-re, and are not inside a literal, and that we are not
in the middle of an identifier that just happens to contain a
\"statement\" keyword."
  (cond
   ;; "for" ... "generate":
   ((and (looking-at "f")
	 ;; Make sure it's the start of a parameter specification.
	 (save-excursion
	   (forward-sexp 2)
	   (skip-chars-forward " \t\n")
	   (looking-at "in\\b[^_]"))
	 ;; Make sure it's not an "end for".
	 (save-excursion
	   (backward-sexp)
	   (not (looking-at "end\\s-+\\w"))))
    t)
   ;; "if" ... "then", "if" ... "generate", "if" ... "loop":
   ((and (looking-at "i")
	 ;; Make sure it's not an "end if".
	 (save-excursion
	   (backward-sexp)
	   (not (looking-at "end\\s-+\\w"))))
    t)
   ;; "while" ... "loop":
   ((looking-at "w")
    t)
   ))

(defconst vhdl-case-alternative-re "when[( \t\n][^;=>]+=>"
  "Regexp describing a case statement alternative key.")

(defun vhdl-case-alternative-p (&optional lim)
  "Return t if we are looking at a real case alternative.
Assumes that the caller will make sure that we are looking at
vhdl-case-alternative-re, and are not inside a literal, and that
we are not in the middle of an identifier that just happens to
contain a \"when\" keyword."
  (save-excursion
    (let (foundp)
      (while (and (not foundp)
		  (re-search-backward ";\\|<=" lim 'move))
	(if (or (= (preceding-char) ?_)
		(vhdl-in-literal lim))
	    (backward-char)
	  (setq foundp t)))
      (or (eq (following-char) ?\;)
	  (eq (point) lim)))
    ))

;; Core syntactic movement functions:

(defconst vhdl-b-t-b-re
  (concat vhdl-begin-bwd-re "\\|" vhdl-end-bwd-re))

(defun vhdl-backward-to-block (&optional lim)
  "Move backward to the previous \"begin\" or \"end\" keyword."
  (let (foundp)
    (while (and (not foundp)
		(re-search-backward vhdl-b-t-b-re lim 'move))
      (if (or (= (preceding-char) ?_)
	      (vhdl-in-literal lim))
	  (backward-char)
	(cond
	 ;; "begin" keyword:
	 ((and (looking-at vhdl-begin-fwd-re)
	       (/= (preceding-char) ?_)
	       (vhdl-begin-p lim))
	  (setq foundp 'begin))
	 ;; "end" keyword:
	 ((and (looking-at vhdl-end-fwd-re)
	       (/= (preceding-char) ?_)
	       (vhdl-end-p lim))
	  (setq foundp 'end))
	 ))
      )
    foundp
    ))

(defun vhdl-forward-sexp (&optional count lim)
  "Move forward across one balanced expression (sexp).
With COUNT, do it that many times."
  (interactive "p")
  (let ((count (or count 1))
	(case-fold-search t)
	end-vec target)
    (save-excursion
      (while (> count 0)
	;; skip whitespace
	(skip-chars-forward " \t\n")
	;; Check for an unbalanced "end" keyword
	(if (and (looking-at vhdl-end-fwd-re)
		 (/= (preceding-char) ?_)
		 (not (vhdl-in-literal lim))
		 (vhdl-end-p lim)
		 (not (looking-at "else")))
	    (error
	     "Containing expression ends prematurely in vhdl-forward-sexp"))
	;; If the current keyword is a "begin" keyword, then find the
	;; corresponding "end" keyword.
	(if (setq end-vec (vhdl-corresponding-end lim))
	    (let (
		  ;; end-re is the statement keyword to search for
		  (end-re
		   (concat "\\b\\(" (aref end-vec 0) "\\)\\b\\([^_]\\|\\'\\)"))
		  ;; column is either the statement keyword target column
		  ;; or nil
		  (column (aref end-vec 1))
		  (eol (vhdl-point 'eol))
		  foundp literal placeholder)
	      ;; Look for the statement keyword.
	      (while (and (not foundp)
			  (re-search-forward end-re nil t)
			  (setq placeholder (match-end 1))
			  (goto-char (match-beginning 0)))
		;; If we are in a literal, or not in the right target
		;; column and not on the same line as the begin, then
		;; try again.
		(if (or (and column
			     (/= (current-indentation) column)
			     (> (point) eol))
			(= (preceding-char) ?_)
			(setq literal (vhdl-in-literal lim)))
		    (if (eq literal 'comment)
			(end-of-line)
		      (forward-char))
		  ;; An "else" keyword corresponds to both the opening brace
		  ;; of the following sexp and the closing brace of the
		  ;; previous sexp.
		  (if (not (looking-at "else"))
		      (goto-char placeholder))
		  (setq foundp t))
		)
	      (if (not foundp)
		  (error "Unbalanced keywords in vhdl-forward-sexp"))
	      )
	  ;; If the current keyword is not a "begin" keyword, then just
	  ;; perform the normal forward-sexp.
	  (forward-sexp)
	  )
	(setq count (1- count))
	)
      (setq target (point)))
    (goto-char target)
    nil))

(defun vhdl-backward-sexp (&optional count lim)
  "Move backward across one balanced expression (sexp).
With COUNT, do it that many times.  LIM bounds any required backward
searches."
  (interactive "p")
  (let ((count (or count 1))
	(case-fold-search t)
	begin-vec target)
    (save-excursion
      (while (> count 0)
	;; Perform the normal backward-sexp, unless we are looking at
	;; "else" - an "else" keyword corresponds to both the opening brace
	;; of the following sexp and the closing brace of the previous sexp.
	(if (and (looking-at "else\\b\\([^_]\\|\\'\\)")
		 (/= (preceding-char) ?_)
		 (not (vhdl-in-literal lim)))
	    nil
	  (backward-sexp)
	  (if (and (looking-at vhdl-begin-fwd-re)
		   (/= (preceding-char) ?_)
		   (not (vhdl-in-literal lim))
		   (vhdl-begin-p lim))
	      (error "Containing expression ends prematurely in vhdl-backward-sexp")))
	;; If the current keyword is an "end" keyword, then find the
	;; corresponding "begin" keyword.
	(if (and (setq begin-vec (vhdl-corresponding-begin lim))
		 (/= (preceding-char) ?_))
	    (let (
		  ;; begin-re is the statement keyword to search for
		  (begin-re
		   (concat "\\b\\(" (aref begin-vec 0) "\\)\\b[^_]"))
		  ;; column is either the statement keyword target column
		  ;; or nil
		  (column (aref begin-vec 1))
		  ;; internal-p controls where the statement keyword can
		  ;; be found.
		  (internal-p (aref begin-vec 3))
		  (last-backward (point)) last-forward
		  foundp literal keyword)
	      ;; Look for the statement keyword.
	      (while (and (not foundp)
			  (re-search-backward begin-re lim t)
			  (setq keyword
				(buffer-substring (match-beginning 1)
						  (match-end 1))))
		;; If we are in a literal or in the wrong column,
		;; then try again.
		(if (or (and column
			     (and (/= (current-indentation) column)
				  ;; possibly accept current-column as
				  ;; well as current-indentation.
				  (or (not internal-p)
				      (/= (current-column) column))))
			(= (preceding-char) ?_)
			(vhdl-in-literal lim))
		    (backward-char)
		  ;; If there is a supplementary keyword, then
		  ;; search forward for it.
		  (if (and (setq begin-re (aref begin-vec 2))
			   (or (not (listp begin-re))
			       ;; If begin-re is an alist, then find the
			       ;; element corresponding to the actual
			       ;; keyword that we found.
			       (progn
				 (setq begin-re
				       (assoc keyword begin-re))
				 (and begin-re
				      (setq begin-re (cdr begin-re))))))
		      (and
		       (setq begin-re
			     (concat "\\b\\(" begin-re "\\)\\b[^_]"))
		       (save-excursion
			 (setq last-forward (point))
			 ;; Look for the supplementary keyword
			 ;; (bounded by the backward search start
			 ;; point).
			 (while (and (not foundp)
				     (re-search-forward begin-re
							last-backward t)
				     (goto-char (match-beginning 1)))
			   ;; If we are in a literal, then try again.
			   (if (or (= (preceding-char) ?_)
				   (setq literal
					 (vhdl-in-literal last-forward)))
			       (if (eq literal 'comment)
				   (goto-char
				    (min (vhdl-point 'eol) last-backward))
				 (forward-char))
			     ;; We have found the supplementary keyword.
			     ;; Save the position of the keyword in foundp.
			     (setq foundp (point)))
			   )
			 foundp)
		       ;; If the supplementary keyword was found, then
		       ;; move point to the supplementary keyword.
		       (goto-char foundp))
		    ;; If there was no supplementary keyword, then
		    ;; point is already at the statement keyword.
		    (setq foundp t)))
		) ; end of the search for the statement keyword
	      (if (not foundp)
		  (error "Unbalanced keywords in vhdl-backward-sexp"))
	      ))
	(setq count (1- count))
	)
      (setq target (point)))
    (goto-char target)
    nil))

(defun vhdl-backward-up-list (&optional count limit)
  "Move backward out of one level of blocks.
With argument, do this that many times."
  (interactive "p")
  (let ((count (or count 1))
	target)
    (save-excursion
      (while (> count 0)
	(if (looking-at vhdl-defun-re)
	    (error "Unbalanced blocks"))
	(vhdl-backward-to-block limit)
	(setq count (1- count)))
      (setq target (point)))
    (goto-char target)))

(defun vhdl-end-of-defun (&optional count)
  "Move forward to the end of a VHDL defun."
  (interactive)
  (let ((case-fold-search t))
    (vhdl-beginning-of-defun)
    (if (not (looking-at "block\\|process\\|procedural"))
	(re-search-forward "\\bis\\b"))
    (vhdl-forward-sexp)))

(defun vhdl-mark-defun ()
  "Put mark at end of this \"defun\", point at beginning."
  (interactive)
  (let ((case-fold-search t))
    (push-mark)
    (vhdl-beginning-of-defun)
    (push-mark)
    (if (not (looking-at "block\\|process\\|procedural"))
	(re-search-forward "\\bis\\b"))
    (vhdl-forward-sexp)
    (exchange-point-and-mark)))

(defun vhdl-beginning-of-libunit ()
  "Move backward to the beginning of a VHDL library unit.
Returns the location of the corresponding begin keyword, unless search
stops due to beginning or end of buffer.
Note that if point is between the \"libunit\" keyword and the
corresponding \"begin\" keyword, then that libunit will not be
recognised, and the search will continue backwards.  If point is
at the \"begin\" keyword, then the defun will be recognised.  The
returned point is at the first character of the \"libunit\" keyword."
  (let ((last-forward (point))
	(last-backward
	 ;; Just in case we are actually sitting on the "begin"
	 ;; keyword, allow for the keyword and an extra character,
	 ;; as this will be used when looking forward for the
	 ;; "begin" keyword.
	 (save-excursion (forward-word 1) (1+ (point))))
	foundp literal placeholder)
    ;; Find the "libunit" keyword.
    (while (and (not foundp)
		(re-search-backward vhdl-libunit-re nil 'move))
      ;; If we are in a literal, or not at a real libunit, then try again.
      (if (or (= (preceding-char) ?_)
	      (vhdl-in-literal (point-min))
	      (not (vhdl-libunit-p)))
	  (backward-char)
	;; Find the corresponding "begin" keyword.
	(setq last-forward (point))
	(while (and (not foundp)
		    (re-search-forward "\\bis\\b[^_]" last-backward t)
		    (setq placeholder (match-beginning 0)))
	  (if (or (= (preceding-char) ?_)
		  (setq literal (vhdl-in-literal last-forward)))
	      ;; It wasn't a real keyword, so keep searching.
	      (if (eq literal 'comment)
		  (goto-char
		   (min (vhdl-point 'eol) last-backward))
		(forward-char))
	    ;; We have found the begin keyword, loop will exit.
	    (setq foundp placeholder)))
	;; Go back to the libunit keyword
	(goto-char last-forward)))
    foundp))

(defun vhdl-beginning-of-defun (&optional count)
  "Move backward to the beginning of a VHDL defun.
With argument, do it that many times.
Returns the location of the corresponding begin keyword, unless search
stops due to beginning or end of buffer."
  ;; Note that if point is between the "defun" keyword and the
  ;; corresponding "begin" keyword, then that defun will not be
  ;; recognised, and the search will continue backwards.  If point is
  ;; at the "begin" keyword, then the defun will be recognised.  The
  ;; returned point is at the first character of the "defun" keyword.
  (interactive "p")
  (let ((count (or count 1))
	(case-fold-search t)
	(last-forward (point))
	foundp)
    (while (> count 0)
      (setq foundp nil)
      (goto-char last-forward)
      (let ((last-backward
	     ;; Just in case we are actually sitting on the "begin"
	     ;; keyword, allow for the keyword and an extra character,
	     ;; as this will be used when looking forward for the
	     ;; "begin" keyword.
	     (save-excursion (forward-word 1) (1+ (point))))
	    begin-string literal)
	(while (and (not foundp)
		    (re-search-backward vhdl-defun-re nil 'move))
	  ;; If we are in a literal, then try again.
	  (if (or (= (preceding-char) ?_)
		  (vhdl-in-literal (point-min)))
	      (backward-char)
	    (if (setq begin-string (vhdl-corresponding-defun))
		;; This is a real defun keyword.
		;; Find the corresponding "begin" keyword.
		;; Look for the begin keyword.
		(progn
		  ;; Save the search start point.
		  (setq last-forward (point))
		  (while (and (not foundp)
			      (search-forward begin-string last-backward t))
		    (if (or (= (preceding-char) ?_)
			    (save-match-data
			      (setq literal (vhdl-in-literal last-forward))))
			;; It wasn't a real keyword, so keep searching.
			(if (eq literal 'comment)
			    (goto-char
			     (min (vhdl-point 'eol) last-backward))
			  (forward-char))
		      ;; We have found the begin keyword, loop will exit.
		      (setq foundp (match-beginning 0)))
		    )
		  ;; Go back to the defun keyword
		  (goto-char last-forward)) ; end search for begin keyword
	      ))
	  ) ; end of the search for the defun keyword
	)
      (setq count (1- count))
      )
    (vhdl-keep-region-active)
    foundp))

(defun vhdl-beginning-of-statement (&optional count lim)
  "Go to the beginning of the innermost VHDL statement.
With prefix arg, go back N - 1 statements.  If already at the
beginning of a statement then go to the beginning of the preceding
one.  If within a string or comment, or next to a comment (only
whitespace between), move by sentences instead of statements.

When called from a program, this function takes 2 optional args: the
prefix arg, and a buffer position limit which is the farthest back to
search."
  (interactive "p")
  (let ((count (or count 1))
	(case-fold-search t)
	(lim (or lim (point-min)))
	(here (point))
	state)
    (save-excursion
      (goto-char lim)
      (setq state (parse-partial-sexp (point) here nil nil)))
    (if (and (interactive-p)
	     (or (nth 3 state)
		 (nth 4 state)
		 (looking-at (concat "[ \t]*" comment-start-skip))))
	(forward-sentence (- count))
      (while (> count 0)
	(vhdl-beginning-of-statement-1 lim)
	(setq count (1- count))))
    ;; its possible we've been left up-buf of lim
    (goto-char (max (point) lim))
    )
  (vhdl-keep-region-active))

(defconst vhdl-e-o-s-re
  (concat ";\\|" vhdl-begin-fwd-re "\\|" vhdl-statement-fwd-re))

(defun vhdl-end-of-statement ()
  "Very simple implementation."
  (interactive)
  (re-search-forward vhdl-e-o-s-re))

(defconst vhdl-b-o-s-re
  (concat ";\\|\(\\|\)\\|\\bwhen\\b[^_]\\|"
	  vhdl-begin-bwd-re "\\|" vhdl-statement-bwd-re))

(defun vhdl-beginning-of-statement-1 (&optional lim)
  "Move to the start of the current statement, or the previous
statement if already at the beginning of one."
  (let ((lim (or lim (point-min)))
	(here (point))
	(pos (point))
	donep)
    ;; go backwards one balanced expression, but be careful of
    ;; unbalanced paren being reached
    (if (not (vhdl-safe (progn (backward-sexp) t)))
	(progn
	  (backward-up-list 1)
	  (forward-char)
	  (vhdl-forward-syntactic-ws here)
	  (setq donep t)))
    (while (and (not donep)
		(not (bobp))
		;; look backwards for a statement boundary
		(re-search-backward vhdl-b-o-s-re lim 'move))
      (if (or (= (preceding-char) ?_)
	      (vhdl-in-literal lim))
	  (backward-char)
	(cond
	 ;; If we are looking at an open paren, then stop after it
	 ((eq (following-char) ?\()
	  (forward-char)
	  (vhdl-forward-syntactic-ws here)
	  (setq donep t))
	 ;; If we are looking at a close paren, then skip it
	 ((eq (following-char) ?\))
	  (forward-char)
	  (setq pos (point))
	  (backward-sexp)
	  (if (< (point) lim)
	      (progn (goto-char pos)
		     (vhdl-forward-syntactic-ws here)
		     (setq donep t))))
	 ;; If we are looking at a semicolon, then stop
	 ((eq (following-char) ?\;)
	  (progn
	    (forward-char)
	    (vhdl-forward-syntactic-ws here)
	    (setq donep t)))
	 ;; If we are looking at a "begin", then stop
	 ((and (looking-at vhdl-begin-fwd-re)
	       (/= (preceding-char) ?_)
	       (vhdl-begin-p nil))
	  ;; If it's a leader "begin", then find the
	  ;; right place
	  (if (looking-at vhdl-leader-re)
	      (save-excursion
		;; set a default stop point at the begin
		(setq pos (point))
		;; is the start point inside the leader area ?
		(goto-char (vhdl-end-of-leader))
		(vhdl-forward-syntactic-ws here)
		(if (< (point) here)
		    ;; start point was not inside leader area
		    ;; set stop point at word after leader
		    (setq pos (point))))
	    (forward-word 1)
	    (vhdl-forward-syntactic-ws here)
	    (setq pos (point)))
	  (goto-char pos)
	  (setq donep t))
	 ;; If we are looking at a "statement", then stop
	 ((and (looking-at vhdl-statement-fwd-re)
	       (/= (preceding-char) ?_)
	       (vhdl-statement-p nil))
	  (setq donep t))
	 ;; If we are looking at a case alternative key, then stop
	 ((and (looking-at vhdl-case-alternative-re)
	       (vhdl-case-alternative-p lim))
	  (save-excursion
	    ;; set a default stop point at the when
	    (setq pos (point))
	    ;; is the start point inside the case alternative key ?
	    (looking-at vhdl-case-alternative-re)
	    (goto-char (match-end 0))
	    (vhdl-forward-syntactic-ws here)
	    (if (< (point) here)
		;; start point was not inside the case alternative key
		;; set stop point at word after case alternative keyleader
		(setq pos (point))))
	  (goto-char pos)
	  (setq donep t))
	 ;; Bogus find, continue
	 (t
	  (backward-char)))))
    ))

;; Defuns for calculating the current syntactic state:

(defun vhdl-get-library-unit (bod placeholder)
  "If there is an enclosing library unit at bod, with it's \"begin\"
keyword at placeholder, then return the library unit type."
  (let ((here (vhdl-point 'bol)))
    (if (save-excursion
	  (goto-char placeholder)
	  (vhdl-safe (vhdl-forward-sexp 1 bod))
	  (<= here (point)))
	(save-excursion
	  (goto-char bod)
	  (cond
	   ((looking-at "e") 'entity)
	   ((looking-at "a") 'architecture)
	   ((looking-at "c") 'configuration)
	   ((looking-at "p")
	    (save-excursion
	      (goto-char bod)
	      (forward-sexp)
	      (vhdl-forward-syntactic-ws here)
	      (if (looking-at "body\\b[^_]")
		  'package-body 'package))))))
    ))

(defun vhdl-get-block-state (&optional lim)
  "Finds and records all the closest opens.
lim is the furthest back we need to search (it should be the
previous libunit keyword)."
  (let ((here (point))
	(lim (or lim (point-min)))
	keyword sexp-start sexp-mid sexp-end
	preceding-sexp containing-sexp
	containing-begin containing-mid containing-paren)
    (save-excursion
      ;; Find the containing-paren, and use that as the limit
      (if (setq containing-paren
		(save-restriction
		  (narrow-to-region lim (point))
		  (vhdl-safe (scan-lists (point) -1 1))))
	  (setq lim containing-paren))
      ;; Look backwards for "begin" and "end" keywords.
      (while (and (> (point) lim)
		  (not containing-sexp))
	(setq keyword (vhdl-backward-to-block lim))
	(cond
	 ((eq keyword 'begin)
	  ;; Found a "begin" keyword
	  (setq sexp-start (point))
	  (setq sexp-mid (vhdl-corresponding-mid lim))
	  (setq sexp-end (vhdl-safe
			  (save-excursion
			    (vhdl-forward-sexp 1 lim) (point))))
	  (if (and sexp-end (<= sexp-end here))
	      ;; we want to record this sexp, but we only want to
	      ;; record the last-most of any of them before here
	      (or preceding-sexp
		  (setq preceding-sexp sexp-start))
	    ;; we're contained in this sexp so put sexp-start on
	    ;; front of list
	    (setq containing-sexp sexp-start)
	    (setq containing-mid sexp-mid)
	    (setq containing-begin t)))
	 ((eq keyword 'end)
	  ;; Found an "end" keyword
	  (forward-sexp)
	  (setq sexp-end (point))
	  (setq sexp-mid nil)
	  (setq sexp-start
		(or (vhdl-safe (vhdl-backward-sexp 1 lim) (point))
		    (progn (backward-sexp) (point))))
	  ;; we want to record this sexp, but we only want to
	  ;; record the last-most of any of them before here
	  (or preceding-sexp
	      (setq preceding-sexp sexp-start)))
	 )))
    ;; Check if the containing-paren should be the containing-sexp
    (if (and containing-paren
	     (or (null containing-sexp)
		 (< containing-sexp containing-paren)))
	(setq containing-sexp containing-paren
	      preceding-sexp nil
	      containing-begin nil
	      containing-mid nil))
    (vector containing-sexp preceding-sexp containing-begin containing-mid)
    ))


(defconst vhdl-s-c-a-re
  (concat vhdl-case-alternative-re "\\|" vhdl-case-header-key))

(defun vhdl-skip-case-alternative (&optional lim)
  "Skip forward over case/when bodies, with optional maximal
limit. If no next case alternative is found, nil is returned and point
is not moved."
  (let ((lim (or lim (point-max)))
	(here (point))
	donep foundp)
    (while (and (< (point) lim)
		(not donep))
      (if (and (re-search-forward vhdl-s-c-a-re lim 'move)
	       (save-match-data
		 (not (vhdl-in-literal)))
	       (/= (match-beginning 0) here))
	  (progn
	    (goto-char (match-beginning 0))
	    (cond
	     ((and (looking-at "case")
		   (re-search-forward "\\bis[^_]" lim t))
	      (backward-sexp)
	      (vhdl-forward-sexp))
	     (t
	      (setq donep t
		    foundp t))))))
    (if (not foundp)
	(goto-char here))
    foundp))

(defun vhdl-backward-skip-label (&optional lim)
  "Skip backward over a label, with optional maximal
limit. If label is not found, nil is returned and point
is not moved."
  (let ((lim (or lim (point-min)))
	placeholder)
    (if (save-excursion
	  (vhdl-backward-syntactic-ws lim)
	  (and (eq (preceding-char) ?:)
	       (progn
		 (backward-sexp)
		 (setq placeholder (point))
		 (looking-at vhdl-label-key))))
	(goto-char placeholder))
    ))

(defun vhdl-forward-skip-label (&optional lim)
  "Skip forward over a label, with optional maximal
limit.  If label is not found, nil is returned and point
is not moved."
  (let ((lim (or lim (point-max))))
    (if (looking-at vhdl-label-key)
	(progn
	  (goto-char (match-end 0))
	  (vhdl-forward-syntactic-ws lim)))
    ))

(defun vhdl-get-syntactic-context ()
  "Guess the syntactic description of the current line of VHDL code."
  (save-excursion
    (save-restriction
      (beginning-of-line)
      (let* ((indent-point (point))
	     (case-fold-search t)
	     vec literal containing-sexp preceding-sexp
	     containing-begin containing-mid containing-leader
	     char-before-ip char-after-ip begin-after-ip end-after-ip
	     placeholder lim library-unit
	    )

	;; Reset the syntactic context
	(setq vhdl-syntactic-context nil)

	(save-excursion
	  ;; Move to the start of the previous library unit, and
	  ;; record the position of the "begin" keyword.
	  (setq placeholder (vhdl-beginning-of-libunit))
	  ;; The position of the "libunit" keyword gives us a gross
	  ;; limit point.
	  (setq lim (point))
	  )

	;; If there is a previous library unit, and we are enclosed by
	;; it, then set the syntax accordingly.
	(and placeholder
	     (setq library-unit (vhdl-get-library-unit lim placeholder))
	     (vhdl-add-syntax library-unit lim))

	;; Find the surrounding state.
	(if (setq vec (vhdl-get-block-state lim))
	    (progn
	      (setq containing-sexp (aref vec 0))
	      (setq preceding-sexp (aref vec 1))
	      (setq containing-begin (aref vec 2))
	      (setq containing-mid (aref vec 3))
	      ))

	;; set the limit on the farthest back we need to search
	(setq lim (if containing-sexp
		      (save-excursion
			(goto-char containing-sexp)
			;; set containing-leader if required
			(if (looking-at vhdl-leader-re)
			    (setq containing-leader (vhdl-end-of-leader)))
			(vhdl-point 'bol))
		    (point-min)))

	;; cache char before and after indent point, and move point to
	;; the most likely position to perform the majority of tests
	(goto-char indent-point)
	(skip-chars-forward " \t")
	(setq literal (vhdl-in-literal lim))
	(setq char-after-ip (following-char))
	(setq begin-after-ip (and
			      (not literal)
			      (looking-at vhdl-begin-fwd-re)
			      (vhdl-begin-p)))
	(setq end-after-ip (and
			    (not literal)
			    (looking-at vhdl-end-fwd-re)
			    (vhdl-end-p)))
	(vhdl-backward-syntactic-ws lim)
	(setq char-before-ip (preceding-char))
	(goto-char indent-point)
	(skip-chars-forward " \t")

	;; now figure out syntactic qualities of the current line
	(cond
	 ;; CASE 1: in a string or comment.
	 ((memq literal '(string comment))
	  (vhdl-add-syntax literal (vhdl-point 'bopl)))
	 ;; CASE 2: Line is at top level.
	 ((null containing-sexp)
	  ;; Find the point to which indentation will be relative
	  (save-excursion
	    (if (null preceding-sexp)
		;; CASE 2X.1
		;; no preceding-sexp -> use the preceding statement
		(vhdl-beginning-of-statement-1 lim)
	      ;; CASE 2X.2
	      ;; if there is a preceding-sexp then indent relative to it
	      (goto-char preceding-sexp)
	      ;; if not at boi, then the block-opening keyword is
	      ;; probably following a label, so we need a different
	      ;; relpos
	      (if (/= (point) (vhdl-point 'boi))
		  ;; CASE 2X.3
		  (vhdl-beginning-of-statement-1 lim)))
	    ;; v-b-o-s could have left us at point-min
	    (and (bobp)
		 ;; CASE 2X.4
		 (vhdl-forward-syntactic-ws indent-point))
	    (setq placeholder (point)))
	  (cond
	   ;; CASE 2A : we are looking at a block-open
	   (begin-after-ip
	    (vhdl-add-syntax 'block-open placeholder))
	   ;; CASE 2B: we are looking at a block-close
	   (end-after-ip
	    (vhdl-add-syntax 'block-close placeholder))
	   ;; CASE 2C: we are looking at a top-level statement
	   ((progn
	      (vhdl-backward-syntactic-ws lim)
	      (or (bobp)
		  (= (preceding-char) ?\;)))
	    (vhdl-add-syntax 'statement placeholder))
	   ;; CASE 2D: we are looking at a top-level statement-cont
	   (t
	    (vhdl-beginning-of-statement-1 lim)
	    ;; v-b-o-s could have left us at point-min
	    (and (bobp)
		 ;; CASE 2D.1
		 (vhdl-forward-syntactic-ws indent-point))
	    (vhdl-add-syntax 'statement-cont (point)))
	   )) ; end CASE 2
	 ;; CASE 3: line is inside parentheses.  Most likely we are
	 ;; either in a subprogram argument (interface) list, or a
	 ;; continued expression containing parentheses.
	 ((null containing-begin)
	  (vhdl-backward-syntactic-ws containing-sexp)
	  (cond
	   ;; CASE 3A: we are looking at the arglist closing paren
	   ((eq char-after-ip ?\))
	    (goto-char containing-sexp)
	    (vhdl-add-syntax 'arglist-close (vhdl-point 'boi)))
	   ;; CASE 3B: we are looking at the first argument in an empty
	   ;; argument list.
	   ((eq char-before-ip ?\()
	    (goto-char containing-sexp)
	    (vhdl-add-syntax 'arglist-intro (vhdl-point 'boi)))
	   ;; CASE 3C: we are looking at an arglist continuation line,
	   ;; but the preceding argument is on the same line as the
	   ;; opening paren.  This case includes multi-line
	   ;; expression paren groupings.
	   ((and (save-excursion
		   (goto-char (1+ containing-sexp))
		   (skip-chars-forward " \t")
		   (not (eolp))
		   (not (looking-at "--")))
		 (save-excursion
		   (vhdl-beginning-of-statement-1 containing-sexp)
		   (skip-chars-backward " \t(")
		   (<= (point) containing-sexp)))
	    (goto-char containing-sexp)
	    (vhdl-add-syntax 'arglist-cont-nonempty (vhdl-point 'boi)))
	   ;; CASE 3D: we are looking at just a normal arglist
	   ;; continuation line
	   (t (vhdl-beginning-of-statement-1 containing-sexp)
	      (vhdl-forward-syntactic-ws indent-point)
	      (vhdl-add-syntax 'arglist-cont (vhdl-point 'boi)))
	   ))
	 ;; CASE 4: A block mid open
	 ((and begin-after-ip
	       (looking-at containing-mid))
	  (goto-char containing-sexp)
	  ;; If the \"begin\" keyword is a trailer, then find v-b-o-s
	  (if (looking-at vhdl-trailer-re)
	      ;; CASE 4.1
	      (progn (forward-sexp) (vhdl-beginning-of-statement-1 nil)))
	  (vhdl-backward-skip-label (vhdl-point 'boi))
	  (vhdl-add-syntax 'block-open (point)))
	 ;; CASE 5: block close brace
	 (end-after-ip
	  (goto-char containing-sexp)
	  ;; If the \"begin\" keyword is a trailer, then find v-b-o-s
	  (if (looking-at vhdl-trailer-re)
	      ;; CASE 5.1
	      (progn (forward-sexp) (vhdl-beginning-of-statement-1 nil)))
	  (vhdl-backward-skip-label (vhdl-point 'boi))
	  (vhdl-add-syntax 'block-close (point)))
	 ;; CASE 6: A continued statement
	 ((and (/= char-before-ip ?\;)
	       ;; check it's not a trailer begin keyword, or a begin
	       ;; keyword immediately following a label.
	       (not (and begin-after-ip
			 (or (looking-at vhdl-trailer-re)
			     (save-excursion
			       (vhdl-backward-skip-label containing-sexp)))))
	       ;; check it's not a statement keyword
	       (not (and (looking-at vhdl-statement-fwd-re)
			 (vhdl-statement-p)))
	       ;; see if the b-o-s is before the indent point
	       (> indent-point
		  (save-excursion
		    (vhdl-beginning-of-statement-1 containing-sexp)
		    ;; If we ended up after a leader, then this will
		    ;; move us forward to the start of the first
		    ;; statement.  Note that a containing sexp here is
		    ;; always a keyword, not a paren, so this will
		    ;; have no effect if we hit the containing-sexp.
		    (vhdl-forward-syntactic-ws indent-point)
		    (setq placeholder (point))))
	       ;; check it's not a block-intro
	       (/= placeholder containing-sexp)
	       ;; check it's not a case block-intro
	       (save-excursion
		 (goto-char placeholder)
		 (or (not (looking-at vhdl-case-alternative-re))
		     (> (match-end 0) indent-point))))
	  ;; Make placeholder skip a label, but only if it puts us
	  ;; before the indent point at the start of a line.
	  (let ((new placeholder))
	    (if (and (> indent-point
			(save-excursion
			  (goto-char placeholder)
			  (vhdl-forward-skip-label indent-point)
			  (setq new (point))))
		     (save-excursion
		       (goto-char new)
		       (eq new (progn (back-to-indentation) (point)))))
		(setq placeholder new)))
	  (vhdl-add-syntax 'statement-cont placeholder)
	  (if begin-after-ip
	      (vhdl-add-syntax 'block-open)))
	 ;; Statement. But what kind?
	 ;; CASE 7: A case alternative key
	 ((and (looking-at vhdl-case-alternative-re)
	       (vhdl-case-alternative-p containing-sexp))
	  ;; for a case alternative key, we set relpos to the first
	  ;; non-whitespace char on the line containing the "case"
	  ;; keyword.
	  (goto-char containing-sexp)
	  ;; If the \"begin\" keyword is a trailer, then find v-b-o-s
	  (if (looking-at vhdl-trailer-re)
	      (progn (forward-sexp) (vhdl-beginning-of-statement-1 nil)))
	  (vhdl-add-syntax 'case-alternative (vhdl-point 'boi)))
	 ;; CASE 8: statement catchall
	 (t
	  ;; we know its a statement, but we need to find out if it is
	  ;; the first statement in a block
	  (if containing-leader
	      (goto-char containing-leader)
	    (goto-char containing-sexp)
	    ;; Note that a containing sexp here is always a keyword,
	    ;; not a paren, so skip over the keyword.
	    (forward-sexp))
	  ;; move to the start of the first statement
	  (vhdl-forward-syntactic-ws indent-point)
	  (setq placeholder (point))
	  ;; we want to ignore case alternatives keys when skipping forward
	  (let (incase-p)
	    (while (looking-at vhdl-case-alternative-re)
	      (setq incase-p (point))
	      ;; we also want to skip over the body of the
	      ;; case/when statement if that doesn't put us at
	      ;; after the indent-point
	      (while (vhdl-skip-case-alternative indent-point))
	      ;; set up the match end
	      (looking-at vhdl-case-alternative-re)
	      (goto-char (match-end 0))
	      ;; move to the start of the first case alternative statement
	      (vhdl-forward-syntactic-ws indent-point)
	      (setq placeholder (point)))
	    (cond
	     ;; CASE 8A: we saw a case/when statement so we must be
	     ;; in a switch statement.  find out if we are at the
	     ;; statement just after a case alternative key
	     ((and incase-p
		   (= (point) indent-point))
	      ;; relpos is the "when" keyword
	      (vhdl-add-syntax 'statement-case-intro incase-p))
	     ;; CASE 8B: any old statement
	     ((< (point) indent-point)
	      ;; relpos is the first statement of the block
	      (vhdl-add-syntax 'statement placeholder)
	      (if begin-after-ip
		  (vhdl-add-syntax 'block-open)))
	     ;; CASE 8C: first statement in a block
	     (t
	      (goto-char containing-sexp)
	      ;; If the \"begin\" keyword is a trailer, then find v-b-o-s
	      (if (looking-at vhdl-trailer-re)
		  (progn (forward-sexp) (vhdl-beginning-of-statement-1 nil)))
	      (vhdl-backward-skip-label (vhdl-point 'boi))
	      (vhdl-add-syntax 'statement-block-intro (point))
	      (if begin-after-ip
		  (vhdl-add-syntax 'block-open)))
	     )))
	 )

	;; now we need to look at any modifiers
	(goto-char indent-point)
	(skip-chars-forward " \t")
	(if (looking-at "--")
	    (vhdl-add-syntax 'comment))
	;; return the syntax
	vhdl-syntactic-context))))

;; Standard indentation line-ups:

(defun vhdl-lineup-arglist (langelem)
  "Lineup the current arglist line with the arglist appearing just
after the containing paren which starts the arglist."
  (save-excursion
    (let* ((containing-sexp
	    (save-excursion
	      ;; arglist-cont-nonempty gives relpos ==
	      ;; to boi of containing-sexp paren. This
	      ;; is good when offset is +, but bad
	      ;; when it is vhdl-lineup-arglist, so we
	      ;; have to special case a kludge here.
	      (if (memq (car langelem) '(arglist-intro arglist-cont-nonempty))
		  (progn
		    (beginning-of-line)
		    (backward-up-list 1)
		    (skip-chars-forward " \t" (vhdl-point 'eol)))
		(goto-char (cdr langelem)))
	      (point)))
	   (cs-curcol (save-excursion
			(goto-char (cdr langelem))
			(current-column))))
      (if (save-excursion
	    (beginning-of-line)
	    (looking-at "[ \t]*)"))
	  (progn (goto-char (match-end 0))
		 (backward-sexp)
		 (forward-char)
		 (vhdl-forward-syntactic-ws)
		 (- (current-column) cs-curcol))
	(goto-char containing-sexp)
	(or (eolp)
	    (let ((eol (vhdl-point 'eol))
		  (here (progn
			  (forward-char)
			  (skip-chars-forward " \t")
			  (point))))
	      (vhdl-forward-syntactic-ws)
	      (if (< (point) eol)
		  (goto-char here))))
	(- (current-column) cs-curcol)
	))))

(defun vhdl-lineup-arglist-intro (langelem)
  "Lineup an arglist-intro line to just after the open paren."
  (save-excursion
    (let ((cs-curcol (save-excursion
		       (goto-char (cdr langelem))
		       (current-column)))
	  (ce-curcol (save-excursion
		       (beginning-of-line)
		       (backward-up-list 1)
		       (skip-chars-forward " \t" (vhdl-point 'eol))
		       (current-column))))
      (- ce-curcol cs-curcol -1))))

(defun vhdl-lineup-comment (langelem)
  "Support old behavior for comment indentation.  We look at
vhdl-comment-only-line-offset to decide how to indent comment
only-lines."
  (save-excursion
    (back-to-indentation)
    ;; at or to the right of comment-column
    (if (>= (current-column) comment-column)
	(vhdl-comment-indent)
      ;; otherwise, indent as specified by vhdl-comment-only-line-offset
      (if (not (bolp))
	  (or (car-safe vhdl-comment-only-line-offset)
	      vhdl-comment-only-line-offset)
	(or (cdr-safe vhdl-comment-only-line-offset)
	    (car-safe vhdl-comment-only-line-offset)
	    -1000                       ;jam it against the left side
	    )))))

(defun vhdl-lineup-statement-cont (langelem)
  "Line up statement-cont after the assignment operator."
  (save-excursion
    (let* ((relpos (cdr langelem))
	   (assignp (save-excursion
		     (goto-char (vhdl-point 'boi))
		     (and (re-search-forward "\\(<\\|:\\)="
					     (vhdl-point 'eol) t)
			  (- (point) (vhdl-point 'boi)))))
	   (curcol (progn
		     (goto-char relpos)
		     (current-column)))
	   foundp)
      (while (and (not foundp)
		  (< (point) (vhdl-point 'eol)))
	(re-search-forward "\\(<\\|:\\)=\\|(" (vhdl-point 'eol) 'move)
	(if (vhdl-in-literal (cdr langelem))
	    (forward-char)
	  (if (= (preceding-char) ?\()
	      ;; skip over any parenthesized expressions
	      (goto-char (min (vhdl-point 'eol)
			      (scan-lists (point) 1 1)))
	    ;; found an assignment operator (not at eol)
	    (setq foundp (not (looking-at "\\s-*$"))))))
      (if (not foundp)
	  ;; there's no assignment operator on the line
	  vhdl-basic-offset
	;; calculate indentation column after assign and ws, unless
	;; our line contains an assignment operator
	(if (not assignp)
	    (progn
	      (forward-char)
	      (skip-chars-forward " \t")
	      (setq assignp 0)))
	(- (current-column) assignp curcol))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation commands

(defsubst vhdl-in-comment-p ()
  "Check if point is to right of beginning comment delimiter."
  (let ((position (point)))
    (save-excursion ; finds an unquoted comment
      (beginning-of-line)
      (re-search-forward "^\\([^\"]*\"[^\"]*\"\\)*[^\"]*--" position t))))

(defsubst vhdl-in-string-p ()
  "Check if point is in a string."
  (let ((position (point)))
    (save-excursion ; preceeded by odd number of string delimiters?
      (beginning-of-line)
      (eq position (re-search-forward "^\\([^\"]*\"[^\"]*\"\\)*[^\"]*\"[^\"]*"
				      position t)))))

(defsubst vhdl-in-comment-or-string-p ()
  "Check if point is in a comment or a string."
  (and (vhdl-in-comment-p)
       (vhdl-in-string-p)))

(defun vhdl-electric-tab (&optional prefix-arg)
  "If preceeding character is part of a word or a paren then hippie-expand,
else if right of non whitespace on line then tab-to-tab-stop,
else if last command was a tab or return then dedent one step,
else indent `correctly'."
  (interactive "*P")
  (vhdl-ext-syntax-table
   (cond ((= (char-syntax (preceding-char)) ?w)
	  (let ((case-fold-search (not vhdl-word-completion-case-sensitive))
		(case-replace nil))
	    (vhdl-expand-abbrev prefix-arg)))
	 ((or (= (preceding-char) ?\() (= (preceding-char) ?\)))
	  (let ((case-fold-search (not vhdl-word-completion-case-sensitive))
		(case-replace nil))
	    (vhdl-expand-paren prefix-arg)))
	 ((> (current-column) (current-indentation))
	  (tab-to-tab-stop))
	 ((and (or (eq last-command 'vhdl-electric-tab)
		   (eq last-command 'vhdl-electric-return))
	       (/= 0 (current-indentation)))
	  (backward-delete-char-untabify vhdl-basic-offset nil))
	 (t (vhdl-indent-line)))
   (setq this-command 'vhdl-electric-tab)))

(defun vhdl-electric-return ()
  "newline-and-indent or indent-new-comment-line if in comment and preceding
character is a space."
  (interactive)
  (if (and (= (preceding-char) ? ) (vhdl-in-comment-p))
      (indent-new-comment-line)
    (newline-and-indent)))

(defvar vhdl-progress-info nil
  "Array variable for progress information: 0 begin, 1 end, 2 time.")

(defun vhdl-indent-line ()
  "Indent the current line as VHDL code.  Returns the amount of
indentation change."
  (interactive)
  (let* ((syntax (vhdl-get-syntactic-context))
	 (pos (- (point-max) (point)))
	 ;; special case: comments at or right of comment-column
	 (indent (if (and (eq (car (car syntax)) 'comment)
			  (>= (vhdl-get-offset (car syntax)) comment-column))
		     (vhdl-get-offset (car syntax))
		   (apply '+ (mapcar 'vhdl-get-offset syntax))))
; 	 (indent (apply '+ (mapcar 'vhdl-get-offset syntax)))
	 (shift-amt  (- indent (current-indentation))))
    (and vhdl-echo-syntactic-information-p
	 (message "syntax: %s, indent= %d" syntax indent))
    (unless (zerop shift-amt)
      (delete-region (vhdl-point 'bol) (vhdl-point 'boi))
      (beginning-of-line)
      (indent-to indent))
    (if (< (point) (vhdl-point 'boi))
	(back-to-indentation)
      ;; If initial point was within line's indentation, position after
      ;; the indentation.  Else stay at same point in text.
      (when (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos))))
    (run-hooks 'vhdl-special-indent-hook)
    ;; update progress status
    (when vhdl-progress-info
      (aset vhdl-progress-info 1 (+ (aref vhdl-progress-info 1)
				    (if (> -500 shift-amt) 0 shift-amt)))
      (when (< vhdl-progress-interval
	       (- (nth 1 (current-time)) (aref vhdl-progress-info 2)))
	(message "Indenting... (%2d%s)"
		 (/ (* 100 (- (point) (aref vhdl-progress-info 0)))
		    (- (aref vhdl-progress-info 1)
		       (aref vhdl-progress-info 0))) "%")
	(aset vhdl-progress-info 2 (nth 1 (current-time)))))
    shift-amt))

(defun vhdl-indent-buffer ()
  "Indent whole buffer as VHDL code.
Calls `indent-region' for whole buffer and adds progress reporting."
  (interactive)
  (when vhdl-progress-interval
    (setq vhdl-progress-info (vector (point-min) (point-max) 0)))
  (indent-region (point-min) (point-max) nil)
  (when vhdl-progress-interval (message "Indenting...done"))
  (setq vhdl-progress-info nil))

(defun vhdl-indent-region (start end column)
  "Indent region as VHDL code.
Adds progress reporting to `indent-region'."
  (interactive "r\nP")
  (when vhdl-progress-interval (setq vhdl-progress-info (vector start end 0)))
  (indent-region start end column)
  (when vhdl-progress-interval (message "Indenting...done"))
  (setq vhdl-progress-info nil))

(defun vhdl-indent-sexp (&optional endpos)
  "Indent each line of the list starting just after point.
If optional arg ENDPOS is given, indent each line, stopping when
ENDPOS is encountered."
  (interactive)
  (save-excursion
    (let ((beg (point))
	  (end (progn (vhdl-forward-sexp nil endpos) (point))))
      (indent-region beg end nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous commands

(defun vhdl-show-syntactic-information ()
  "Show syntactic information for current line."
  (interactive)
  (message "syntactic analysis: %s" (vhdl-get-syntactic-context))
  (vhdl-keep-region-active))

;; Verification and regression functions:

(defun vhdl-regress-line (&optional arg)
  "Check syntactic information for current line."
  (interactive "P")
  (let ((expected (save-excursion
		    (end-of-line)
		    (when (search-backward " -- ((" (vhdl-point 'bol) t)
		      (forward-char 4)
		      (read (current-buffer)))))
	(actual (vhdl-get-syntactic-context))
	(expurgated))
    ;; remove the library unit symbols
    (mapcar
     (function
      (lambda (elt)
	(if (memq (car elt) '(entity configuration package
				     package-body architecture))
	    nil
	  (setq expurgated (append expurgated (list elt))))))
     actual)
    (if (and (not arg) expected (listp expected))
	(if (not (equal expected expurgated))
	    (error "Should be: %s, is: %s" expected expurgated))
      (save-excursion
	(beginning-of-line)
	(when (not (looking-at "^\\s-*\\(--.*\\)?$"))
	  (end-of-line)
	  (if (search-backward " -- ((" (vhdl-point 'bol) t)
	      (kill-line))
	  (insert " -- ")
	  (insert (format "%s" expurgated))))))
  (vhdl-keep-region-active))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Alignment, whitespace fixup, beautifying
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vhdl-align-alist
  '(
    ;; after some keywords
    (vhdl-mode "\\<\\(constant\\|quantity\\|signal\\|terminal\\|variable\\)[ \t]"
	       "\\<\\(constant\\|quantity\\|signal\\|terminal\\|variable\\)\\([ \t]+\\)" 2)
    ;; before ':'
    (vhdl-mode ":[^=]" "\\([ \t]*\\):[^=]")
    ;; after direction specifications
    (vhdl-mode ":[ \t]*\\(in\\|out\\|inout\\|buffer\\|\\)\\>"
	       ":[ \t]*\\(in\\|out\\|inout\\|buffer\\|\\)\\([ \t]+\\)" 2)
    ;; before "==", ":=", "=>", and "<="
    (vhdl-mode "==" "\\([ \t]*\\)==" 1)
    (vhdl-mode ":=" "\\([ \t]*\\):=" 1) ; since ":= ... =>" can occur
    (vhdl-mode "<=" "\\([ \t]*\\)<=" 1) ; since "<= ... =>" can occur
    (vhdl-mode "=>" "\\([ \t]*\\)=>" 1)
    (vhdl-mode ":=" "\\([ \t]*\\):=" 1)	; since "=> ... :=" can occur
    (vhdl-mode "<=" "\\([ \t]*\\)<=" 1) ; since "=> ... <=" can occur
    ;; before some keywords
    (vhdl-mode "[ \t]after\\>" "[^ \t]\\([ \t]+\\)after\\>" 1)
    (vhdl-mode "[ \t]when\\>" "[^ \t]\\([ \t]+\\)when\\>" 1)
    (vhdl-mode "[ \t]else\\>" "[^ \t]\\([ \t]+\\)else\\>" 1)
    )
  "The format of this alist is (MODES [or MODE] REGEXP ALIGN-PATTERN SUBEXP).
It is searched in order.  If REGEXP is found anywhere in the first
line of a region to be aligned, ALIGN-PATTERN will be used for that
region.  ALIGN-PATTERN must include the whitespace to be expanded or
contracted.  It may also provide regexps for the text surrounding the
whitespace.  SUBEXP specifies which sub-expression of
ALIGN-PATTERN matches the white space to be expanded/contracted.")

(defvar vhdl-align-try-all-clauses t
  "If REGEXP is not found on the first line of the region that clause
is ignored.  If this variable is non-nil, then the clause is tried anyway.")

(defun vhdl-align-region (begin end &optional spacing alignment-list indent)
  "Attempt to align a range of lines based on the content of the
lines.  The definition of `alignment-list' determines the matching
order and the manner in which the lines are aligned.  If ALIGNMENT-LIST
is not specified `vhdl-align-alist' is used.  If INDENT is non-nil,
indentation is done before aligning."
  (interactive "r\np")
  (setq alignment-list (or alignment-list vhdl-align-alist))
  (setq spacing (or spacing 1))
  (save-excursion
    (let (bol indent)
      (goto-char end)
      (setq end (point-marker))
      (goto-char begin)
      (setq bol (setq begin (progn (beginning-of-line) (point))))
					;      (untabify bol end)
      (when indent
	(indent-region bol end nil))))
  (let ((case-fold-search t)
	(copy (copy-alist alignment-list)))
    (vhdl-ext-syntax-table
     (while copy
       (save-excursion
	 (goto-char begin)
	 (let (element
	       (eol (save-excursion (progn (end-of-line) (point)))))
	   (setq element (nth 0 copy))
	   (when (and (or (and (listp (car element))
			       (memq major-mode (car element)))
			  (eq major-mode (car element)))
		      (or vhdl-align-try-all-clauses
			  (re-search-forward (car (cdr element)) eol t)))
	     (vhdl-align-region-1 begin end (car (cdr (cdr element)))
				  (car (cdr (cdr (cdr element)))) spacing))
	   (setq copy (cdr copy))))))))

(defun vhdl-align-region-1 (begin end match &optional substr spacing)
  "Align a range of lines from BEGIN to END.  The regular expression
MATCH must match exactly one fields: the whitespace to be
contracted/expanded.  The alignment column will equal the
rightmost column of the widest whitespace block. SPACING is
the amount of extra spaces to add to the calculated maximum required.
SPACING defaults to 1 so that at least one space is inserted after
the token in MATCH."
  (setq spacing (or spacing 1))
  (setq substr (or substr 1))
  (save-excursion
    (let (distance (max 0) (lines 0) bol eol width)
      ;; Determine the greatest whitespace distance to the alignment
      ;; character
      (goto-char begin)
      (setq eol (progn (end-of-line) (point))
	    bol (setq begin (progn (beginning-of-line) (point))))
      (while (< bol end)
	(save-excursion
	  (when (and (re-search-forward match eol t)
		     (not (vhdl-in-comment-p)))
	    (setq distance (- (match-beginning substr) bol))
	    (when (> distance max)
	      (setq max distance))))
	(forward-line)
	(setq bol (point)
	      eol (save-excursion (end-of-line) (point)))
	(setq lines (1+ lines)))
      ;; Now insert enough maxs to push each assignment operator to
      ;; the same column.  We need to use 'lines' as a counter, since
      ;; the location of the mark may change
      (goto-char (setq bol begin))
      (setq eol (save-excursion (end-of-line) (point)))
      (while (> lines 0)
  	(when (and (re-search-forward match eol t)
		   (not (vhdl-in-comment-p)))
	  (setq width (- (match-end substr) (match-beginning substr)))
	  (setq distance (- (match-beginning substr) bol))
	  (goto-char (match-beginning substr))
	  (delete-char width)
	  (insert-char ?  (+ (- max distance) spacing)))
	(beginning-of-line)
	(forward-line)
	(setq bol (point)
	      eol (save-excursion (end-of-line) (point)))
	(setq lines (1- lines))))))

(defun vhdl-align-inline-comment-region-1 (beg end &optional spacing)
  "Align inline comments in region."
  (save-excursion
    (let ((high-start 0)
	  (high-length 0)
	  (case-fold-search t))
      (vhdl-ext-syntax-table
       (goto-char beg)
       ;; search for longest code line and longest inline comment
       (while (< (point) end)
	 (cond
	  ((and (not (looking-at "^\\s-*\\(begin\\|end\\)\\>"))
		(looking-at "^\\(.*[^ \t\n-]+\\)\\s-*\\(--\\s-*.*\\)$"))
	   (setq high-start
		 (max high-start (- (match-end 1) (match-beginning 1))))
	   (setq high-length
		 (max high-length (- (match-end 2) (match-beginning 2)))))
	  ((and (looking-at "^\\(\\s-*\\))\\(--\\s-*.*\\)$")
		(>= (- (match-end 1) (match-beginning 1)) comment-column))
	   (setq high-length
		 (max high-length (- (match-end 2) (match-beginning 2))))))
	 (beginning-of-line 2))
       (goto-char beg)
       (setq spacing (or spacing 2))
       (setq high-start (+ high-start spacing))
       ;; align as nice as possible
       (while (< (point) end)
	 (when (and (not (looking-at "^\\s-*\\(begin\\|end\\)\\>"))
		    (or (looking-at "^.*[^ \t\n-]+\\(\\s-*\\)--")
			(and (looking-at "^\\(\\s-*\\)--")
			     (>= (- (match-end 1) (match-beginning 1))
				 comment-column))))
	   (goto-char (match-end 1))
	   (delete-region (match-beginning 1) (match-end 1))
	   (insert-char ?  spacing)
	   (cond ((<= high-start comment-column)
		  (indent-to comment-column))
		 ((<= (+ high-start high-length) end-comment-column)
		  (indent-to high-start))
		 (t (indent-to comment-column))))
	 (beginning-of-line 2))))))

(defun vhdl-align-noindent-region (beg end &optional spacing no-message)
  "Align region without indentation."
  (interactive "r\nP")
  (save-excursion
    (let (pos)
      (goto-char beg)
      (beginning-of-line)
      (setq beg (point))
      (goto-char end)
      (setq end (point-marker))
      (untabify beg end)
      (unless no-message (message "Aligning..."))
      (vhdl-fixup-whitespace-region beg end t)
      (goto-char beg)
      (if (not vhdl-align-groups)
	  ;; align entire region
	  (progn (vhdl-align-region beg end spacing)
		 (vhdl-align-inline-comment-region-1 beg end))
	;; align groups
	(while (and (< beg end)
		    (re-search-forward "^\\s-*$" end t))
	  (setq pos (point-marker))
	  (vhdl-align-region beg pos spacing)
	  (vhdl-align-inline-comment-region-1 beg pos)
	  (setq beg (1+ pos))
	  (goto-char beg))
	;; align last group
	(when (< beg end)
	  (vhdl-align-region beg end spacing)
	  (vhdl-align-inline-comment-region-1 beg end)))))
  (unless no-message (message "Aligning...done")))

(defun vhdl-align-group (&optional spacing)
  "Align group of lines between empty lines."
  (interactive)
  (save-excursion
    (let ((start (point))
	  beg end)
      (setq end (if (re-search-forward "^\\s-*$" nil t)
		    (point-marker) (point-max)))
      (goto-char start)
      (setq beg (if (re-search-backward "^\\s-*$" nil t) (point) (point-min)))
      (untabify beg end)
      (message "Aligning...")
      (vhdl-fixup-whitespace-region beg end t)
      (vhdl-align-region beg end spacing)
      (vhdl-align-inline-comment-region-1 beg end)
      (message "Aligning...done"))))

(defun vhdl-align-noindent-buffer ()
  "Align buffer without indentation."
  (interactive)
  (vhdl-align-noindent-region (point-min) (point-max)))

(defun vhdl-align-inline-comment-region (beg end &optional spacing no-message)
  "Align inline comments within a region.  Groups of code lines separated by
empty lines are aligned individually, if `vhdl-align-groups' is non-nil."
  (interactive "r\nP")
  (save-excursion
    (let (pos)
      (goto-char beg)
      (beginning-of-line)
      (setq beg (point))
      (goto-char end)
      (setq end (point-marker))
      (untabify beg end)
      (unless no-message (message "Aligning inline comments..."))
      (goto-char beg)
      (if (not vhdl-align-groups)
	  ;; align entire region
	  (vhdl-align-inline-comment-region-1 beg end spacing)
	;; align groups
	(while (and (< beg end) (re-search-forward "^\\s-*$" end t))
	  (setq pos (point-marker))
	  (vhdl-align-inline-comment-region-1 beg pos spacing)
	  (setq beg (1+ pos))
	  (goto-char beg))
	;; align last group
	(when (< beg end)
	  (vhdl-align-inline-comment-region-1 beg end spacing))))
    (unless no-message (message "Aligning inline comments...done"))))

(defun vhdl-align-inline-comment-group (&optional spacing)
  "Align inline comments within a group of lines between empty lines."
  (interactive)
  (save-excursion
    (let ((start (point))
	  beg end)
      (setq end (if (re-search-forward "^\\s-*$" nil t)
		    (point-marker) (point-max)))
      (goto-char start)
      (setq beg (if (re-search-backward "^\\s-*$" nil t) (point) (point-min)))
      (untabify beg end)
      (message "Aligning inline comments...")
      (vhdl-align-inline-comment-region-1 beg end)
      (message "Aligning inline comments...done"))))

(defun vhdl-align-inline-comment-buffer ()
  "Align inline comments within buffer.  Groups of code lines separated by
empty lines are aligned individually, if `vhdl-align-groups' is non-nil."
  (interactive)
  (vhdl-align-inline-comment-region (point-min) (point-max)))

(defun vhdl-fixup-whitespace-region (beg end &optional no-message)
  "Fixup whitespace in region.  Surround operator symbols by one space,
eliminate multiple spaces (except at beginning of line), eliminate spaces at
end of line, do nothing in comments."
  (interactive "r")
  (unless no-message (message "Fixing up whitespace..."))
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    ;; surround operator symbols by one space
    (goto-char beg)
    (while (re-search-forward "\\([^/:<>=]\\|^\\)\\(--\\|:\\|=\\|<\\|>\\|:=\\|<=\\|>=\\|=>\\)\\([^=>]\\|$\\)"
			      end t)
      (if (equal "--" (match-string 2))
	  (re-search-forward ".*\n" end t)
	(replace-match "\\1 \\2 \\3")))
    ;; have no space before and one space after `,' and ';'
    (goto-char beg)
    (while (re-search-forward "\\(--\\|\\s-*\\([,;]\\)\\)" end t)
      (if (equal "--" (match-string 1))
	  (re-search-forward ".*\n" end t)
	(replace-match "\\2 " nil nil nil 1)))
    ;; eliminate multiple spaces and spaces at end of line
    (goto-char beg)
    (while (or (and (looking-at "--.*\n") (re-search-forward "--.*\n" end t))
	       (and (looking-at "\\s-+$") (re-search-forward "\\s-+$" end t)
		    (progn (replace-match "" nil nil) t))
	       (and (looking-at "\\s-+;") (re-search-forward "\\s-+;" end t)
		    (progn (replace-match ";" nil nil) t))
	       (and (looking-at "^\\s-+") (re-search-forward "^\\s-+" end t))
	       (and (looking-at "\\s-+--") (re-search-forward "\\s-+" end t)
		    (progn (replace-match "  " nil nil) t ))
	       (and (looking-at "\\s-+") (re-search-forward "\\s-+" end t)
		    (progn (replace-match " " nil nil) t ))
	       (re-search-forward "\\S-+" end t))))
  (unless no-message (message "Fixing up whitespace...done")))

(defun vhdl-fixup-whitespace-buffer ()
  "Fixup whitespace in buffer.  Surround operator symbols by one space,
eliminate multiple spaces (except at beginning of line), eliminate spaces at
end of line, do nothing in comments."
  (interactive)
  (vhdl-fixup-whitespace-region (point-min) (point-max)))

(defun vhdl-beautify-region (beg end)
  "Beautify region by applying indentation, whitespace fixup, alignment, and
case fixing to a resion.  Calls functions `vhdl-indent-buffer',
`vhdl-align-noindent-buffer' (variable `vhdl-align-groups' set to non-nil), and
`vhdl-fix-case-buffer'."
  (interactive "r")
  (vhdl-indent-region beg end nil)
  (let ((vhdl-align-groups t))
    (vhdl-align-noindent-region beg end))
  (vhdl-fix-case-region beg end))

(defun vhdl-beautify-buffer ()
  "Beautify buffer by applying indentation, whitespace fixup, alignment, and
case fixing to entire buffer.  Calls `vhdl-beautify-region' for the entire
buffer."
  (interactive)
  (vhdl-beautify-region (point-min) (point-max)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Electrification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst vhdl-template-prompt-syntax "[^ =<>][^<>@.\n]*[^ =<>]"
  "Syntax of prompt inserted by template generators.")

(defvar vhdl-template-invoked-by-hook nil
  "Indicates whether a template has been invoked by a hook or by key or menu.
Used for undoing after template abortion.")

;; correct different behavior of function `unread-command-events' in XEmacs
(defalias 'vhdl-character-to-event
  (if (string-match "XEmacs" emacs-version) 'character-to-event 'identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Enabling/disabling

(defun vhdl-mode-line-update ()
  "Update the modeline string for VHDL major mode."
  (setq mode-name (concat "VHDL"
			  (and (or vhdl-electric-mode vhdl-stutter-mode) "/")
			  (and vhdl-electric-mode "e")
			  (and vhdl-stutter-mode "s")))
  (force-mode-line-update))

(defun vhdl-electric-mode (arg)
  "Toggle VHDL electric mode.
Turn on if ARG positive, turn off if ARG negative, toggle if ARG zero or nil."
  (interactive "P")
  (setq vhdl-electric-mode
	(cond ((or (not arg) (zerop arg)) (not vhdl-electric-mode))
	      ((> arg 0) t) (t nil)))
  (vhdl-mode-line-update))

(defun vhdl-stutter-mode (arg)
  "Toggle VHDL stuttering mode.
Turn on if ARG positive, turn off if ARG negative, toggle if ARG zero or nil."
  (interactive "P")
  (setq vhdl-stutter-mode
	(cond ((or (not arg) (zerop arg)) (not vhdl-stutter-mode))
	      ((> arg 0) t) (t nil)))
  (vhdl-mode-line-update))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuttering

(defun vhdl-electric-dash (count)
  "-- starts a comment, --- draws a horizontal line,
---- starts a display comment"
  (interactive "p")
  (if vhdl-stutter-mode
      (cond
       ((and abbrev-start-location (= abbrev-start-location (point)))
	(setq abbrev-start-location nil)
	(goto-char last-abbrev-location)
	(beginning-of-line nil)
	(vhdl-comment-display))
       ((/= (preceding-char) ?-)	; standard dash (minus)
	(self-insert-command count))
       (t (self-insert-command count)
	  (message "Enter '-' for horiz. line, 'CR' for commenting-out code, else enter comment")
	  (let ((next-input (read-char)))
	    (if (= next-input ?-)	; triple dash
		(progn
		  (vhdl-comment-display-line)
		  (message
		   "Enter '-' for display comment, else continue coding")
		  (let ((next-input (read-char)))
		    (if (= next-input ?-) ; four dashes
			(vhdl-comment-display t)
		      (setq unread-command-events ; pushback the char
			    (list (vhdl-character-to-event next-input))))))
	      (setq unread-command-events ; pushback the char
		    (list (vhdl-character-to-event next-input)))
	      (vhdl-comment-insert)))))
    (self-insert-command count)))

(defun vhdl-electric-open-bracket (count) "'[' --> '(', '([' --> '['"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1))
      (if (= (preceding-char) ?\()
	  (progn (delete-char -1) (insert-char ?\[ 1))
	(insert-char ?\( 1))
    (self-insert-command count)))

(defun vhdl-electric-close-bracket (count) "']' --> ')', ')]' --> ']'"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1))
      (progn
	(if (= (preceding-char) ?\))
	    (progn (delete-char -1) (insert-char ?\] 1))
	  (insert-char ?\) 1))
	(blink-matching-open))
    (self-insert-command count)))

(defun vhdl-electric-quote (count) "'' --> \""
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1))
      (if (= (preceding-char) last-input-char)
	  (progn (delete-backward-char 1) (insert-char ?\" 1))
	(insert-char ?\' 1))
    (self-insert-command count)))

(defun vhdl-electric-semicolon (count) "';;' --> ' : ', ': ;' --> ' := '"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1))
      (cond ((= (preceding-char) last-input-char)
	     (progn (delete-char -1)
		    (when (not (eq (preceding-char) ? )) (insert " "))
		    (insert ": ")
		    (setq this-command 'vhdl-electric-colon)))
	    ((and
	      (eq last-command 'vhdl-electric-colon) (= (preceding-char) ? ))
	     (progn (delete-char -1) (insert "= ")))
	    (t (insert-char ?\; 1)))
    (self-insert-command count)))

(defun vhdl-electric-comma (count) "',,' --> ' <= '"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1))
      (cond ((= (preceding-char) last-input-char)
	     (progn (delete-char -1)
		    (when (not (eq (preceding-char) ? )) (insert " "))
		    (insert "<= ")))
	    (t (insert-char ?\, 1)))
    (self-insert-command count)))

(defun vhdl-electric-period (count) "'..' --> ' => '"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1))
      (cond ((= (preceding-char) last-input-char)
	     (progn (delete-char -1)
		    (when (not (eq (preceding-char) ? )) (insert " "))
		    (insert "=> ")))
	    (t (insert-char ?\. 1)))
    (self-insert-command count)))

(defun vhdl-electric-equal (count) "'==' --> ' == '"
  (interactive "p")
  (if (and vhdl-stutter-mode (= count 1))
      (cond ((= (preceding-char) last-input-char)
	     (progn (delete-char -1)
		    (when (not (eq (preceding-char) ? )) (insert " "))
		    (insert "== ")))
	    (t (insert-char ?\= 1)))
    (self-insert-command count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  VHDL templates

(defun vhdl-template-paired-parens ()
  "Insert a pair of round parentheses, placing point between them."
  (interactive)
  (insert "()")
  (backward-char))

(defun vhdl-template-alias ()
  "Insert alias declaration."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "ALIAS ")
    (when (vhdl-template-field "name" nil t start (point))
      (insert " : ")
      (unless (vhdl-template-field
	       (concat "[type" (and (vhdl-standard-p 'ams) " or nature") "]")
	       nil t)
	(backward-delete-char 3))
      (vhdl-insert-keyword " IS ")
      (vhdl-template-field "name" ";")
      (vhdl-comment-insert-inline))))

(defun vhdl-template-architecture ()
  "Insert architecture."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	arch-name entity-exists string
	(case-fold-search t))
    (vhdl-insert-keyword "ARCHITECTURE ")
    (when (setq arch-name
		(vhdl-template-field "name" nil t start (point)))
      (vhdl-insert-keyword " OF ")
      (save-excursion
	(vhdl-ext-syntax-table
	 (setq entity-exists (re-search-backward
			      "\\<entity \\(\\w+\\) is\\>" nil t))
	 (setq string (match-string 1))))
      (if (and entity-exists (not (equal string "")))
	  (insert string)
	(vhdl-template-field "entity name"))
      (vhdl-insert-keyword " IS")
      (vhdl-template-begin-end
       (unless (vhdl-standard-p '87) "ARCHITECTURE") arch-name margin
       (memq vhdl-insert-empty-lines '(unit all))))))

(defun vhdl-template-array (kind &optional secondary)
  "Insert array type definition."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "ARRAY (")
    (when (or (vhdl-template-field "range" nil (not secondary) start (point))
	      secondary)
      (vhdl-insert-keyword ") OF ")
      (vhdl-template-field (if (eq kind 'type) "type" "nature"))
      (vhdl-insert-keyword ";"))))

(defun vhdl-template-assert ()
  "Insert an assertion statement."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "ASSERT ")
    (when vhdl-conditions-in-parenthesis (insert "("))
    (when (vhdl-template-field "condition (negated)" nil t start (point))
      (when vhdl-conditions-in-parenthesis (insert ")"))
      (setq start (point))
      (vhdl-insert-keyword " REPORT ")
      (unless (vhdl-template-field "string expression" nil nil nil nil t)
	(delete-region start (point)))
      (setq start (point))
      (vhdl-insert-keyword " SEVERITY ")
      (unless (vhdl-template-field "[NOTE | WARNING | ERROR | FAILURE]" nil t)
	(delete-region start (point)))
      (insert ";"))))

(defun vhdl-template-attribute ()
  "Insert an attribute declaration or specification."
  (interactive)
  (if (eq (vhdl-decision-query
	   "attribute" "(d)eclaration or (s)pecification?" t) ?s)
      (vhdl-template-attribute-spec)
    (vhdl-template-attribute-decl)))

(defun vhdl-template-attribute-decl ()
  "Insert an attribute declaration."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "ATTRIBUTE ")
    (when (vhdl-template-field "name" " : " t start (point))
      (vhdl-template-field "type" ";")
      (vhdl-comment-insert-inline))))

(defun vhdl-template-attribute-spec ()
  "Insert an attribute specification."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "ATTRIBUTE ")
    (when (vhdl-template-field "name" nil t start (point))
      (vhdl-insert-keyword " OF ")
      (vhdl-template-field "entity names | OTHERS | ALL" " : ")
      (vhdl-template-field "entity class")
      (vhdl-insert-keyword " IS ")
      (vhdl-template-field "expression" ";"))))

(defun vhdl-template-block ()
  "Insert a block."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	label)
    (vhdl-insert-keyword ": BLOCK ")
    (goto-char start)
    (when (setq label (vhdl-template-field "label" nil t start (+ (point) 8)))
      (forward-word 1)
      (forward-char 1)
      (insert "(")
      (if (vhdl-template-field "[guard expression]" nil t)
	  (insert ")")
	(delete-char -2))
      (unless (vhdl-standard-p '87) (vhdl-insert-keyword " IS"))
      (vhdl-template-begin-end "BLOCK" label margin)
      (vhdl-comment-block))))

(defun vhdl-template-block-configuration ()
  "Insert a block configuration statement."
  (interactive)
  (let ((margin (current-indentation))
	(start (point)))
    (vhdl-insert-keyword "FOR ")
    (when (vhdl-template-field "block name" nil t start (point))
      (vhdl-insert-keyword "\n\n")
      (indent-to margin)
      (vhdl-insert-keyword "END FOR;")
      (end-of-line 0)
      (indent-to (+ margin vhdl-basic-offset)))))

(defun vhdl-template-break ()
  "Insert a break statement."
  (interactive)
  (let (position)
    (vhdl-insert-keyword "BREAK")
    (setq position (point))
    (insert " ")
    (while (or
	    (progn (vhdl-insert-keyword "FOR ")
		   (if (vhdl-template-field "[quantity name]" " USE " t)
		       (progn (vhdl-template-field "quantity name" " => ") t)
		     (kill-word -1) nil))
	    (vhdl-template-field "[quantity name]" " => " t))
      (vhdl-template-field "expression")
      (setq position (point))
      (insert ", "))
    (delete-region position (point))
    (unless (vhdl-sequential-statement-p)
      (vhdl-insert-keyword " ON ")
      (if (vhdl-template-field "[sensitivity list]" nil t)
	  (setq position (point))
	(delete-region position (point))))
    (vhdl-insert-keyword " WHEN ")
    (when vhdl-conditions-in-parenthesis (insert "("))
    (if (vhdl-template-field "[condition]" nil t)
	(when vhdl-conditions-in-parenthesis (insert ")"))
      (delete-region position (point)))
    (insert ";")))

(defun vhdl-template-case (&optional kind)
  "Insert a case statement."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	label)
    (unless kind (setq kind (if (vhdl-sequential-statement-p) 'is 'use)))
    (if (or (not (eq vhdl-optional-labels 'all)) (vhdl-standard-p '87))
	(vhdl-insert-keyword "CASE ")
      (vhdl-insert-keyword ": CASE ")
      (goto-char start)
      (setq label (vhdl-template-field "[label]" nil t))
      (unless label (delete-char 2))
      (forward-word 1)
      (forward-char 1))
    (when (vhdl-template-field "expression" nil t start (point))
      (vhdl-insert-keyword (concat " " (if (eq kind 'is) "IS" "USE") "\n\n"))
      (indent-to margin)
      (vhdl-insert-keyword "END CASE")
      (when label (insert " " label))
      (insert ";")
      (forward-line -1)
      (indent-to (+ margin vhdl-basic-offset))
      (vhdl-insert-keyword "WHEN ")
      (let ((position (point)))
	(insert " => ;\n")
	(indent-to (+ margin vhdl-basic-offset))
	(vhdl-insert-keyword "WHEN OTHERS => null;")
	(goto-char position)))))

(defun vhdl-template-case-is ()
  "Insert a sequential case statement."
  (interactive)
  (vhdl-template-case 'is))

(defun vhdl-template-case-use ()
  "Insert a simultaneous case statement."
  (interactive)
  (vhdl-template-case 'use))

(defun vhdl-template-component ()
  "Insert a component declaration."
  (interactive)
  (vhdl-template-component-decl))

(defun vhdl-template-component-conf ()
  "Insert a component configuration (uses `vhdl-template-configuration-spec'
since these are almost equivalent)."
  (interactive)
  (let ((margin (current-indentation))
	(result (vhdl-template-configuration-spec t)))
    (when result
      (insert "\n")
      (indent-to margin)
      (vhdl-insert-keyword "END FOR;")
      (when (eq result 'no-use)
	(end-of-line -0)))))

(defun vhdl-template-component-decl ()
  "Insert a component declaration."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	name end-column)
    (vhdl-insert-keyword "COMPONENT ")
    (when (setq name (vhdl-template-field "name" nil t start (point)))
      (insert "\n\n")
      (indent-to margin)
      (vhdl-insert-keyword "END COMPONENT")
      (unless (vhdl-standard-p '87) (insert " " name))
      (insert ";")
      (setq end-column (current-column))
      (end-of-line -0)
      (indent-to (+ margin vhdl-basic-offset))
      (vhdl-template-generic-list t t)
      (insert "\n")
      (indent-to (+ margin vhdl-basic-offset))
      (vhdl-template-port-list t)
      (beginning-of-line 2)
      (forward-char end-column))))

(defun vhdl-template-component-inst ()
  "Insert a component instantiation statement."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	unit position)
    (when (vhdl-template-field "instance label" nil t start (point))
      (insert ": ")
      (if (vhdl-standard-p '87)
	  (vhdl-template-field "component name")
	;; direct instantiation
	(setq unit (vhdl-template-field
		    "[COMPONENT | ENTITY | CONFIGURATION]" " " t))
	(setq unit (upcase (or unit "")))
	(cond ((equal unit "ENTITY")
	       (vhdl-template-field "library name" "." nil nil nil nil "work")
	       (vhdl-template-field "entity name" "(")
	       (if (vhdl-template-field "[architecture name]" nil t)
		   (insert ")")
		 (delete-char -1)))
	      ((equal unit "CONFIGURATION")
	       (vhdl-template-field "library name" "." nil nil nil nil "work")
	       (vhdl-template-field "configuration name"))
	      (t (vhdl-template-field "component name"))))
      (insert "\n")
      (indent-to (+ margin vhdl-basic-offset))
      (setq position (point))
      (vhdl-insert-keyword "GENERIC ")
      (when (vhdl-template-map position t t)
	(insert "\n")
	(indent-to (+ margin vhdl-basic-offset)))
      (setq position (point))
      (vhdl-insert-keyword "PORT ")
      (unless (vhdl-template-map position t t)
	(kill-line -0)
	(delete-char -1))
      (insert ";"))))

(defun vhdl-template-conditional-signal-asst ()
  "Insert a conditional signal assignment."
  (interactive)
  (when (vhdl-template-field "target signal")
    (insert " <= ")
;    (if (not (equal (vhdl-template-field "[GUARDED] [TRANSPORT]") ""))
;       (insert " "))
    (let ((margin (current-column))
	  (start (point))
	  position)
      (vhdl-template-field "waveform")
      (setq position (point))
      (vhdl-insert-keyword " WHEN ")
      (when vhdl-conditions-in-parenthesis (insert "("))
      (while (and (vhdl-template-field "[condition]" nil t)
		  (progn
		    (when vhdl-conditions-in-parenthesis (insert ")"))
		    (setq position (point))
		    (vhdl-insert-keyword " ELSE")
		    (insert "\n")
		    (indent-to margin)
		    (vhdl-template-field "[waveform]" nil t)))
	(setq position (point))
	(vhdl-insert-keyword " WHEN ")
	(when vhdl-conditions-in-parenthesis (insert "(")))
      (delete-region position (point))
      (insert ";")
      (when vhdl-auto-align (vhdl-align-noindent-region start (point) 1)))))

(defun vhdl-template-configuration ()
  "Insert a configuration specification if within an architecture,
a block or component configuration if within a configuration declaration,
a configuration declaration if not within a design unit."
  (interactive)
  (let ((case-fold-search t))
    (vhdl-ext-syntax-table
     (cond
      ((and (save-excursion		; architecture body
	      (re-search-backward "^\\(architecture\\|end\\)\\>" nil t))
	    (equal "ARCHITECTURE" (upcase (match-string 1))))
       (vhdl-template-configuration-spec))
      ((and (save-excursion		; configuration declaration
	      (re-search-backward "^\\(configuration\\|end\\)\\>" nil t))
	    (equal "CONFIGURATION" (upcase (match-string 1))))
       (if (eq (vhdl-decision-query
		"configuration" "(b)lock or (c)omponent configuration?" t) ?c)
	   (vhdl-template-component-conf)
	 (vhdl-template-block-configuration)))
      (t (vhdl-template-configuration-decl)))))) ; otherwise

(defun vhdl-template-configuration-spec (&optional optional-use)
  "Insert a configuration specification."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	aspect position)
    (vhdl-insert-keyword "FOR ")
    (when (vhdl-template-field "component names | OTHERS | ALL" " : "
			       t start (point))
      (vhdl-template-field "component type" "\n")
      (indent-to (+ margin vhdl-basic-offset))
      (setq start (point))
      (vhdl-insert-keyword "USE ")
      (if (and optional-use
	       (not (setq aspect (vhdl-template-field
				  "[ENTITY | CONFIGURATION | OPEN]" " " t))))
	  (progn (delete-region start (point)) 'no-use)
	(unless optional-use
	  (setq aspect (vhdl-template-field
			"ENTITY | CONFIGURATION | OPEN" " ")))
	(setq aspect (upcase (or aspect "")))
	(cond ((equal aspect "ENTITY")
	       (vhdl-template-field "library name" "." nil nil nil nil "work")
	       (vhdl-template-field "entity name" "(")
	       (if (vhdl-template-field "[architecture name]" nil t)
		   (insert ")")
		 (delete-char -1))
	       (insert "\n")
	       (indent-to (+ margin (* 2 vhdl-basic-offset)))
	       (setq position (point))
	       (vhdl-insert-keyword "GENERIC ")
	       (when (vhdl-template-map position t t)
		 (insert "\n")
		 (indent-to (+ margin (* 2 vhdl-basic-offset))))
	       (setq position (point))
	       (vhdl-insert-keyword "PORT ")
	       (unless (vhdl-template-map position t t)
		 (kill-line -0)
		 (delete-char -1))
	       (insert ";")
	       t)
	      ((equal aspect "CONFIGURATION")
	       (vhdl-template-field "library name" "." nil nil nil nil "work")
	       (vhdl-template-field "configuration name" ";"))
	      (t (backward-delete-char 1) (insert ";") t))))))


(defun vhdl-template-configuration-decl ()
  "Insert a configuration declaration."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	(case-fold-search t)
	entity-exists string name position)
    (vhdl-insert-keyword "CONFIGURATION ")
    (when (setq name (vhdl-template-field "name" nil t start (point)))
      (vhdl-insert-keyword " OF ")
      (save-excursion
	(vhdl-ext-syntax-table
	 (setq entity-exists (re-search-backward
			      "\\<entity \\(\\w*\\) is\\>" nil t))
	 (setq string (match-string 1))))
      (if (and entity-exists (not (equal string "")))
	  (insert string)
	(vhdl-template-field "entity name"))
      (vhdl-insert-keyword " IS\n")
      (when (memq vhdl-insert-empty-lines '(unit all)) (insert "\n"))
      (indent-to (+ margin vhdl-basic-offset))
      (setq position (point))
      (insert "\n")
      (when (memq vhdl-insert-empty-lines '(unit all)) (insert "\n"))
      (indent-to margin)
      (vhdl-insert-keyword "END ")
      (unless (vhdl-standard-p '87)
	(vhdl-insert-keyword "CONFIGURATION "))
      (insert name ";")
      (goto-char position))))

(defun vhdl-template-constant ()
  "Insert a constant declaration."
  (interactive)
  (let ((start (point))
	(in-arglist (vhdl-in-argument-list-p)))
    (vhdl-insert-keyword "CONSTANT ")
    (when (vhdl-template-field "name" nil t start (point))
      (insert " : ")
      (when in-arglist (vhdl-insert-keyword "IN "))
      (vhdl-template-field "type")
      (if in-arglist
	  (progn (insert ";")
		 (vhdl-comment-insert-inline))
	(let ((position (point)))
	  (insert " := ")
	  (unless (vhdl-template-field "[initialization]" nil t)
	    (delete-region position (point)))
	  (insert ";")
	  (vhdl-comment-insert-inline))))))

(defun vhdl-template-default ()
  "Insert nothing."
  (interactive)
  (insert " ")
  (unexpand-abbrev)
  (backward-word 1)
  (vhdl-case-word 1)
  (forward-char 1))

(defun vhdl-template-default-indent ()
  "Insert nothing and indent."
  (interactive)
  (insert " ")
  (unexpand-abbrev)
  (backward-word 1)
  (vhdl-case-word 1)
  (forward-char 1)
  (vhdl-indent-line))

(defun vhdl-template-disconnect ()
  "Insert a disconnect statement."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "DISCONNECT ")
    (when (vhdl-template-field "signal names | OTHERS | ALL"
			       " : " t start (point))
      (vhdl-template-field "type")
      (vhdl-insert-keyword " AFTER ")
      (vhdl-template-field "time expression" ";"))))

(defun vhdl-template-else ()
  "Insert an else statement."
  (interactive)
  (let ((case-fold-search t)
	margin)
    (vhdl-ext-syntax-table
     (vhdl-insert-keyword "ELSE")
     (if (save-excursion
	   (re-search-backward "\\(\\<when\\>\\|;\\)" nil t)
	   (equal "WHEN" (upcase (match-string 1))))
	 (insert " ")
       (vhdl-indent-line)
       (setq margin (current-indentation))
       (insert "\n")
       (indent-to (+ margin vhdl-basic-offset))))))

(defun vhdl-template-elsif ()
  "Insert an elsif statement."
  (interactive)
  (let ((start (point))
	margin)
    (vhdl-insert-keyword "ELSIF ")
    (when vhdl-conditions-in-parenthesis (insert "("))
    (when (vhdl-template-field "condition" nil t start (point))
      (when vhdl-conditions-in-parenthesis (insert ")"))
      (vhdl-indent-line)
      (setq margin (current-indentation))
      (vhdl-insert-keyword
       (concat " " (if (vhdl-sequential-statement-p) "THEN" "USE") "\n"))
      (indent-to (+ margin vhdl-basic-offset)))))

(defun vhdl-template-entity ()
  "Insert an entity."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	name end-column)
    (vhdl-insert-keyword "ENTITY ")
    (when (setq name (vhdl-template-field "name" nil t start (point)))
      (vhdl-insert-keyword " IS\n\n")
      (indent-to margin)
      (vhdl-insert-keyword "END ")
      (unless (vhdl-standard-p '87) (vhdl-insert-keyword "ENTITY "))
      (insert name ";")
      (setq end-column (current-column))
      (end-of-line -0)
      (indent-to (+ margin vhdl-basic-offset))
      (when (memq vhdl-insert-empty-lines '(unit all)) (insert "\n"))
      (indent-to (+ margin vhdl-basic-offset))
      (when (vhdl-template-generic-list t)
	(when (memq vhdl-insert-empty-lines '(unit all)) (insert "\n")))
      (insert "\n")
      (indent-to (+ margin vhdl-basic-offset))
      (when (vhdl-template-port-list t)
	(when (memq vhdl-insert-empty-lines '(unit all)) (insert "\n")))
      (beginning-of-line 2)
      (forward-char end-column))))

(defun vhdl-template-exit ()
  "Insert an exit statement."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "EXIT ")
    (unless (vhdl-template-field "[loop label]" nil t)
      (delete-char -1))
    (let ((position (point)))
      (vhdl-insert-keyword " WHEN ")
      (when vhdl-conditions-in-parenthesis (insert "("))
      (if (vhdl-template-field "[condition]" nil t)
	  (when vhdl-conditions-in-parenthesis (insert ")"))
	(delete-region position (point))))
    (insert ";")))

(defun vhdl-template-file ()
  "Insert a file declaration."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "FILE ")
    (when (vhdl-template-field "name" nil t start (point))
      (insert " : ")
      (vhdl-template-field "type")
      (unless (vhdl-standard-p '87)
	(vhdl-insert-keyword " OPEN ")
	(unless (vhdl-template-field "[READ_MODE | WRITE_MODE | APPEND_MODE]"
				     nil t)
	  (backward-delete-char 6)))
      (vhdl-insert-keyword " IS ")
      (when (vhdl-standard-p '87)
	(vhdl-template-field "[IN | OUT]" " " t))
      (vhdl-template-field "filename-string" nil nil nil nil t)
      (insert ";")
      (vhdl-comment-insert-inline))))

(defun vhdl-template-for ()
  "Insert a block or component configuration if within a configuration
declaration, a configuration specification if within an architecture
declarative part (and not within a subprogram), and a for-loop otherwise."
  (interactive)
  (let ((case-fold-search t))
    (vhdl-ext-syntax-table
     (cond
      ((and (save-excursion		; configuration declaration
	      (re-search-backward "^\\(configuration\\|end\\)\\>" nil t))
	    (equal "CONFIGURATION" (upcase (match-string 1))))
       (if (eq (vhdl-decision-query
		"for" "(b)lock or (c)omponent configuration?" t) ?c)
	   (vhdl-template-component-conf)
	 (vhdl-template-block-configuration)))
      ((and (save-excursion
	      (re-search-backward	; architecture declarative part
	       "^\\(architecture\\|entity\\|begin\\|end\\)\\>" nil t))
	    (equal "ARCHITECTURE" (upcase (match-string 1)))
	    (not (and (save-excursion	; not subprogram
			(re-search-backward
			 "^\\s-*\\(architecture\\|begin\\|end\\)\\>" nil t))
		      (equal "BEGIN" (upcase (match-string 1)))
		      (save-excursion
			(re-search-backward
			 "^\\s-*\\(function\\|procedure\\)\\>" nil t)))))
       (vhdl-template-configuration-spec))
      ((vhdl-sequential-statement-p)	; sequential statement
       (vhdl-template-for-loop))
      (t (vhdl-template-for-generate)))))) ; concurrent statement

(defun vhdl-template-for-generate ()
  "Insert a for-generate."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	label string position)
    (vhdl-insert-keyword ": FOR ")
    (setq position (point-marker))
    (goto-char start)
    (when (setq label (vhdl-template-field "label" nil t start position))
      (goto-char position)
      (vhdl-template-field "loop variable")
      (vhdl-insert-keyword " IN ")
      (vhdl-template-field "range")
      (vhdl-template-generate-body margin label))))

(defun vhdl-template-for-loop ()
  "Insert a for loop."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	label index)
    (if (not (eq vhdl-optional-labels 'all))
	(vhdl-insert-keyword "FOR ")
      (vhdl-insert-keyword ": FOR ")
      (goto-char start)
      (setq label (vhdl-template-field "[label]" nil t))
      (unless label (delete-char 2))
      (forward-word 1)
      (forward-char 1))
    (when (setq index (vhdl-template-field "loop variable"
					   nil t start (point)))
      (vhdl-insert-keyword " IN ")
      (vhdl-template-field "range")
      (vhdl-insert-keyword " LOOP\n\n")
      (indent-to margin)
      (vhdl-insert-keyword "END LOOP")
      (if label
	  (insert " " label ";")
	(insert ";")
	(when vhdl-self-insert-comments (insert "  -- " index)))
      (forward-line -1)
      (indent-to (+ margin vhdl-basic-offset)))))

(defun vhdl-template-footer ()
  "Insert a VHDL file footer."
  (interactive)
  (unless (equal vhdl-file-footer "")
    (save-excursion
      (goto-char (point-max))
      (insert "\n")
      (vhdl-insert-string-or-file vhdl-file-footer))))

(defun vhdl-template-function (&optional kind)
  "Insert a function declaration or body."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	name)
    (vhdl-insert-keyword "FUNCTION ")
    (when (setq name (vhdl-template-field "name" nil t start (point)))
      (vhdl-template-argument-list t)
      (when vhdl-auto-align (vhdl-align-noindent-region start (point) 1))
      (end-of-line)
      (insert "\n")
      (indent-to (+ margin vhdl-basic-offset))
      (vhdl-insert-keyword "RETURN ")
      (vhdl-template-field "type")
      (if (if kind (eq kind 'body)
	    (eq (vhdl-decision-query nil "(d)eclaration or (b)ody?") ?b))
	  (progn (vhdl-insert-keyword " IS")
		 (vhdl-template-begin-end
		  (unless (vhdl-standard-p '87) "FUNCTION") name margin)
		 (vhdl-comment-block))
	(insert ";")))))

(defun vhdl-template-function-decl ()
  "Insert a function declaration."
  (interactive)
  (vhdl-template-function 'decl))

(defun vhdl-template-function-body ()
  "Insert a function declaration."
  (interactive)
  (vhdl-template-function 'body))

(defun vhdl-template-generate ()
  "Insert a generation scheme."
  (interactive)
  (if (eq (vhdl-decision-query nil "(f)or or (i)f?" t) ?i)
      (vhdl-template-if-generate)
    (vhdl-template-for-generate)))

(defun vhdl-template-generic ()
  "Insert generic declaration, or generic map in instantiation statements."
  (interactive)
  (let ((start (point))
	(case-fold-search t))
    (vhdl-ext-syntax-table
     (cond
      ((and (save-excursion		; entity declaration
	      (re-search-backward "^\\(entity\\|end\\)\\>" nil t))
	    (equal "ENTITY" (upcase (match-string 1))))
       (vhdl-template-generic-list nil))
      ((or (save-excursion
	     (or (beginning-of-line)
		 (looking-at "^\\s-*\\w+\\s-*:\\s-*\\w+")))
	   (equal 'statement-cont (car (car (vhdl-get-syntactic-context)))))
       (vhdl-insert-keyword "GENERIC ")
       (vhdl-template-map start))
      (t (vhdl-template-generic-list nil t))))))

(defun vhdl-template-group ()
  "Insert group or group template declaration."
  (interactive)
  (let ((start (point)))
    (if (eq (vhdl-decision-query
	     "group" "(d)eclaration or (t)emplate declaration?" t) ?t)
	(vhdl-template-group-template)
      (vhdl-template-group-decl))))

(defun vhdl-template-group-decl ()
  "Insert group declaration."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "GROUP ")
    (when (vhdl-template-field "name" " : " t start (point))
      (vhdl-template-field "template name" " (")
      (vhdl-template-field "constituent list" ");")
      (vhdl-comment-insert-inline))))

(defun vhdl-template-group-template ()
  "Insert group template declaration."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "GROUP ")
    (when (vhdl-template-field "template name" nil t start (point))
      (vhdl-insert-keyword " IS (")
      (vhdl-template-field "entity class list" ");")
      (vhdl-comment-insert-inline))))

(defun vhdl-template-header ()
  "Insert a VHDL file header."
  (interactive)
  (unless (equal vhdl-file-header "")
    (let ((case-fold-search t)
	  (project-name (or (nth 0 (aget vhdl-project-alist vhdl-project)) ""))
	  (project-desc (or (nth 2 (aget vhdl-project-alist vhdl-project)) ""))
	  eot)
      (vhdl-ext-syntax-table
       (save-excursion
	 (save-restriction
	   (widen)
	   (goto-char (point-min))
	   (vhdl-insert-string-or-file vhdl-file-header)
	   (setq eot (point))
	   (narrow-to-region (point-min) eot)
	   (goto-char (point-min))
	   (while (search-forward "<projectdesc>" nil t)
	     (replace-match project-desc t t))
	   (goto-char (point-min))
	   (while (search-forward "<filename>" nil t)
	     (replace-match (buffer-name) t t))
	   (goto-char (point-min))
	   (while (search-forward "<author>" nil t)
	     (replace-match "" t t)
	     (insert (user-full-name))
	     (when user-mail-address (insert "  <" user-mail-address ">")))
	   (goto-char (point-min))
	   (while (search-forward "<login>" nil t)
	     (replace-match (user-login-name) t t))
	   (goto-char (point-min))
	   (while (search-forward "<project>" nil t)
	     (replace-match project-name t t))
	   (goto-char (point-min))
	   (while (search-forward "<company>" nil t)
	     (replace-match vhdl-company-name t t))
	   (goto-char (point-min))
	   (while (search-forward "<platform>" nil t)
	     (replace-match vhdl-platform-spec t t))
	   (goto-char (point-min))
	   ;; Replace <RCS> with $, so that RCS for the source is
	   ;; not over-enthusiastic with replacements
	   (while (search-forward "<RCS>" nil t)
	     (replace-match "$" nil t))
	   (goto-char (point-min))
	   (while (search-forward "<date>" nil t)
	     (replace-match "" t t)
	     (vhdl-template-insert-date))
	   (goto-char (point-min))
	   (let (string)
	     (while
		 (re-search-forward "<\\(\\(\\w\\|\\s_\\)*\\) string>" nil t)
	       (setq string (read-string (concat (match-string 1) ": ")))
	       (replace-match string t t)))))
       (goto-char (point-min))
       (when (search-forward "<cursor>" nil t)
	 (replace-match "" t t))
       (when (or (not project-name) (equal project-name ""))
	 (message "You can specify a project title in custom variable `vhdl-project-alist'"))
       (when (or (not project-desc) (equal project-desc ""))
	 (message "You can specify a project description in custom variable `vhdl-project-alist'"))
       (when (equal vhdl-company-name "")
	 (message "You can specify a company name in custom variable `vhdl-company-name'"))
       (when (equal vhdl-platform-spec "")
	 (message "You can specify a platform in custom variable `vhdl-platform-spec'"))))))

(defun vhdl-template-if ()
  "Insert a sequential if statement or an if-generate statement."
  (interactive)
  (if (vhdl-sequential-statement-p)
      (vhdl-template-if-then)
    (if (and (vhdl-standard-p 'ams)
	     (eq (vhdl-decision-query "if" "(g)enerate or (u)se?" t) ?u))
	(vhdl-template-if-use)
      (vhdl-template-if-generate))))

(defun vhdl-template-if-generate ()
  "Insert an if-generate."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	label string position)
    (vhdl-insert-keyword ": IF ")
    (setq position (point-marker))
    (goto-char start)
    (when (setq label (vhdl-template-field "label" nil t start position))
      (goto-char position)
      (when vhdl-conditions-in-parenthesis (insert "("))
      (vhdl-template-field "condition")
      (when vhdl-conditions-in-parenthesis (insert ")"))
      (vhdl-template-generate-body margin label))))

(defun vhdl-template-if-then-use (kind)
  "Insert a sequential if statement."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	label)
    (if (or (not (eq vhdl-optional-labels 'all)) (vhdl-standard-p '87))
	(vhdl-insert-keyword "IF ")
      (vhdl-insert-keyword ": IF ")
      (goto-char start)
      (setq label (vhdl-template-field "[label]" nil t))
      (unless label (delete-char 2))
      (forward-word 1)
      (forward-char 1))
    (when vhdl-conditions-in-parenthesis (insert "("))
    (when (vhdl-template-field "condition" nil t start (point))
      (when vhdl-conditions-in-parenthesis (insert ")"))
      (vhdl-insert-keyword
       (concat " " (if (eq kind 'then) "THEN" "USE") "\n\n"))
      (indent-to margin)
      (vhdl-insert-keyword "END IF")
      (when label (insert " " label))
      (insert ";")
      (forward-line -1)
      (indent-to (+ margin vhdl-basic-offset)))))

(defun vhdl-template-if-then ()
  "Insert a sequential if statement."
  (interactive)
  (vhdl-template-if-then-use 'then))

(defun vhdl-template-if-use ()
  "Insert a simultaneous if statement."
  (interactive)
  (vhdl-template-if-then-use 'use))

(defun vhdl-template-instance ()
  "Insert a component instantiation statement."
  (interactive)
  (vhdl-template-component-inst))

(defun vhdl-template-library ()
  "Insert a library specification."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	name end-pos)
    (vhdl-insert-keyword "LIBRARY ")
    (when (setq name (vhdl-template-field "names" nil t start (point)))
      (insert ";")
      (unless (string-match "," name)
	(setq end-pos (point))
	(insert "\n")
	(indent-to margin)
	(vhdl-insert-keyword "USE ")
	(insert name)
	(vhdl-insert-keyword "..ALL;")
	(backward-char 5)
	(if (vhdl-template-field "package name")
	    (forward-char 5)
	  (delete-region end-pos (+ (point) 5)))))))

(defun vhdl-template-limit ()
  "Insert a limit."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "LIMIT ")
    (when (vhdl-template-field "quantity names | OTHERS | ALL" " : "
			       t start (point))
      (vhdl-template-field "type")
      (vhdl-insert-keyword " WITH ")
      (vhdl-template-field "real expression" ";"))))

(defun vhdl-template-loop ()
  "Insert a loop."
  (interactive)
  (let ((char (vhdl-decision-query nil "(w)hile, (f)or, or (b)are?" t)))
    (cond ((eq char ?w)
	   (vhdl-template-while-loop))
	  ((eq char ?f)
	   (vhdl-template-for-loop))
	  (t (vhdl-template-bare-loop)))))

(defun vhdl-template-bare-loop ()
  "Insert a loop."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	label)
    (if (not (eq vhdl-optional-labels 'all))
	(vhdl-insert-keyword "LOOP ")
      (vhdl-insert-keyword ": LOOP ")
      (goto-char start)
      (setq label (vhdl-template-field "[label]" nil t))
      (unless label (delete-char 2))
      (forward-word 1)
      (delete-char 1))
    (insert "\n\n")
    (indent-to margin)
    (vhdl-insert-keyword "END LOOP")
    (insert (if label (concat " " label ";") ";"))
    (forward-line -1)
    (indent-to (+ margin vhdl-basic-offset))))

(defun vhdl-template-map (&optional start optional secondary)
  "Insert a map specification with association list."
  (interactive)
  (let ((start (or start (point)))
	margin end-pos)
    (vhdl-insert-keyword "MAP (")
    (if (not vhdl-association-list-with-formals)
	(if (vhdl-template-field
	     (concat (and optional "[") "association list" (and optional "]"))
	     ")" (or (not secondary) optional)
	     (and (not secondary) start) (point))
	    t
	  (if (and optional secondary) (delete-region start (point)))
	  nil)
      (if vhdl-argument-list-indent
	  (setq margin (current-column))
	(setq margin (+ (current-indentation) vhdl-basic-offset))
	(insert "\n")
	(indent-to margin))
      (if (vhdl-template-field
	   (concat (and optional "[") "formal" (and optional "]"))
	   " => " (or (not secondary) optional)
	   (and (not secondary) start) (point))
	  (progn
	    (vhdl-template-field "actual" ",")
	    (setq end-pos (point))
	    (insert "\n")
	    (indent-to margin)
	    (while (vhdl-template-field "[formal]" " => " t)
	      (vhdl-template-field "actual" ",")
	      (setq end-pos (point))
	      (insert "\n")
	      (indent-to margin))
	    (delete-region end-pos (point))
	    (backward-delete-char 1)
	    (insert ")")
	    (when vhdl-auto-align (vhdl-align-noindent-region start (point) 1))
	    t)
	(when (and optional secondary) (delete-region start (point)))
	nil))))

(defun vhdl-template-modify (&optional noerror)
  "Actualize modification date."
  (interactive)
  (let ((case-fold-search t))
    (vhdl-ext-syntax-table
     (save-excursion
       (goto-char (point-min))
       (if (re-search-forward vhdl-modify-date-prefix-string nil t)
	   (progn (kill-line)
		  (vhdl-template-insert-date))
	 (unless noerror
	   (error (concat "Modification date prefix string \""
			  vhdl-modify-date-prefix-string "\" not found"))))))))

(defun vhdl-template-modify-noerror ()
  "Call `vhdl-template-modify' with NOERROR non-nil."
  (vhdl-template-modify t))

(defun vhdl-template-nature ()
  "Insert a nature declaration."
  (interactive)
  (let ((start (point))
	name mid-pos end-pos)
    (vhdl-insert-keyword "NATURE ")
    (when (setq name (vhdl-template-field "name" nil t start (point)))
      (vhdl-insert-keyword " IS ")
      (let ((definition
	      (upcase
	       (or (vhdl-template-field
		    "across type | ARRAY | RECORD")
		   ""))))
	(cond ((equal definition "")
	       (insert ";"))
	      ((equal definition "ARRAY")
	       (kill-word -1)
	       (vhdl-template-array 'nature t))
	      ((equal definition "RECORD")
	       (setq mid-pos (point-marker))
	       (kill-word -1)
	       (vhdl-template-record 'nature name t))
	      (t
	       (vhdl-insert-keyword " ACROSS ")
	       (vhdl-template-field "through type")
	       (vhdl-insert-keyword " THROUGH ")
	       (vhdl-template-field "reference name")
	       (vhdl-insert-keyword " REFERENCE;")))
	(when mid-pos
	  (setq end-pos (point-marker))
	  (goto-char mid-pos)
	  (end-of-line))
	(vhdl-comment-insert-inline)
	(when end-pos (goto-char end-pos))))))

(defun vhdl-template-next ()
  "Insert a next statement."
  (interactive)
  (vhdl-insert-keyword "NEXT ")
  (unless (vhdl-template-field "[loop label]" nil t)
    (delete-char -1))
  (let ((position (point)))
    (vhdl-insert-keyword " WHEN ")
    (when vhdl-conditions-in-parenthesis (insert "("))
    (if (vhdl-template-field "[condition]" nil t)
	(when vhdl-conditions-in-parenthesis (insert ")"))
      (delete-region position (point)))
    (insert ";")))

(defun vhdl-template-others ()
  "Insert an others aggregate."
  (interactive)
  (vhdl-insert-keyword "(OTHERS => '')")
  (backward-char 2))

(defun vhdl-template-package (&optional kind)
  "Insert a package specification or body."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	name body position)
    (vhdl-insert-keyword "PACKAGE ")
    (setq body (if kind (eq kind 'body)
		 (eq (vhdl-decision-query nil "(d)eclaration or (b)ody?") ?b)))
    (when body (vhdl-insert-keyword "BODY "))
    (when (setq name (vhdl-template-field "name" nil t start (point)))
      (vhdl-insert-keyword " IS\n")
      (when (memq vhdl-insert-empty-lines '(unit all)) (insert "\n"))
      (indent-to (+ margin vhdl-basic-offset))
      (setq position (point))
      (insert "\n")
      (when (memq vhdl-insert-empty-lines '(unit all)) (insert "\n"))
      (indent-to margin)
      (vhdl-insert-keyword "END ")
      (unless (vhdl-standard-p '87)
	(vhdl-insert-keyword (concat "PACKAGE " (and body "BODY "))))
      (insert (or name "") ";")
      (goto-char position))))

(defun vhdl-template-package-decl ()
  "Insert a package specification."
  (interactive)
  (vhdl-template-package 'decl))

(defun vhdl-template-package-body ()
  "Insert a package body."
  (interactive)
  (vhdl-template-package 'body))

(defun vhdl-template-port ()
  "Insert a port declaration, or port map in instantiation statements."
  (interactive)
  (let ((start (point))
	(case-fold-search t))
    (vhdl-ext-syntax-table
     (cond
      ((and (save-excursion		; entity declaration
	      (re-search-backward "^\\(entity\\|end\\)\\>" nil t))
	    (equal "ENTITY" (upcase (match-string 1))))
       (vhdl-template-port-list nil))
      ((or (save-excursion
	     (or (beginning-of-line)
		 (looking-at "^\\s-*\\w+\\s-*:\\s-*\\w+")))
	   (equal 'statement-cont (car (car (vhdl-get-syntactic-context)))))
       (vhdl-insert-keyword "PORT ")
       (vhdl-template-map start))
      (t (vhdl-template-port-list nil))))))

(defun vhdl-template-procedural ()
  "Insert a procedural."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	(case-fold-search t)
	label)
    (vhdl-insert-keyword "PROCEDURAL ")
    (when (memq vhdl-optional-labels '(process all))
      (goto-char start)
      (insert ": ")
      (goto-char start)
      (setq label (vhdl-template-field "[label]" nil t))
      (unless label (delete-char 2))
      (forward-word 1)
      (forward-char 1))
    (unless (vhdl-standard-p '87) (vhdl-insert-keyword "IS"))
    (vhdl-template-begin-end "PROCEDURAL" label margin)
    (vhdl-comment-block)))

(defun vhdl-template-procedure (&optional kind)
  "Insert a procedure declaration or body."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	name)
    (vhdl-insert-keyword "PROCEDURE ")
    (when (setq name (vhdl-template-field "name" nil t start (point)))
      (vhdl-template-argument-list)
      (if (if kind (eq kind 'body)
	    (eq (vhdl-decision-query nil "(d)eclaration or (b)ody?") ?b))
	  (progn (vhdl-insert-keyword " IS")
		 (when vhdl-auto-align
		   (vhdl-align-noindent-region start (point) 1))
		 (end-of-line)
		 (vhdl-template-begin-end
		  (unless (vhdl-standard-p '87) "PROCEDURE")
		  name margin)
		 (vhdl-comment-block))
	(insert ";")
	(when vhdl-auto-align (vhdl-align-noindent-region start (point) 1))
	(end-of-line)))))

(defun vhdl-template-procedure-decl ()
  "Insert a procedure declaration."
  (interactive)
  (vhdl-template-procedure 'decl))

(defun vhdl-template-procedure-body ()
  "Insert a procedure body."
  (interactive)
  (vhdl-template-procedure 'body))

(defun vhdl-template-process (&optional kind)
  "Insert a process."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	(case-fold-search t)
	label seq input-signals clock reset final-pos)
    (setq seq (if kind (eq kind 'seq)
		(eq (vhdl-decision-query
		     "process" "(c)ombinational or (s)equential?" t) ?s)))
    (vhdl-insert-keyword "PROCESS ")
    (when (memq vhdl-optional-labels '(process all))
      (goto-char start)
      (insert ": ")
      (goto-char start)
      (setq label (vhdl-template-field "[label]" nil t))
      (unless label (delete-char 2))
      (forward-word 1)
      (forward-char 1))
    (insert "(")
    (if (not seq)
	(unless (setq input-signals
		      (vhdl-template-field "[sensitivity list]" ")" t))
	  (setq input-signals "")
	  (delete-char -2))
      (setq clock (or (and (not (equal "" vhdl-clock-name))
			   (progn (insert vhdl-clock-name) vhdl-clock-name))
		      (vhdl-template-field "clock name") "<clock>"))
      (when (eq vhdl-reset-kind 'async)
	(insert ", ")
	(setq reset (or (and (not (equal "" vhdl-reset-name))
			     (progn (insert vhdl-reset-name) vhdl-reset-name))
			(vhdl-template-field "reset name") "<reset>")))
      (insert ")"))
    (unless (vhdl-standard-p '87) (vhdl-insert-keyword " IS"))
    (vhdl-template-begin-end "PROCESS" label margin)
    (when seq (setq reset (vhdl-template-seq-process clock reset)))
    (when vhdl-prompt-for-comments
      (setq final-pos (point-marker))
      (vhdl-ext-syntax-table
       (when (and (re-search-backward "\\<begin\\>" nil t)
		  (re-search-backward "\\<process\\>" nil t))
	 (end-of-line -0)
	 (if (bobp)
	     (progn (insert "\n") (forward-line -1))
	   (insert "\n"))
	 (indent-to margin)
	 (insert "-- purpose: ")
	 (if (not (vhdl-template-field "[description]" nil t))
	     (vhdl-line-kill-entire)
	   (insert "\n")
	   (indent-to margin)
	   (insert "-- type   : ")
	   (insert (if seq "sequential" "combinational") "\n")
	   (indent-to margin)
	   (insert "-- inputs : ")
	   (if (not seq)
	       (insert input-signals)
	     (insert clock ", ")
	     (when reset (insert reset ", "))
	     (unless (vhdl-template-field "[signal names]" nil t)
	       (delete-char -2)))
	   (insert "\n")
	   (indent-to margin)
	   (insert "-- outputs: ")
	   (vhdl-template-field "[signal names]" nil t))))
      (goto-char final-pos))))

(defun vhdl-template-process-comb ()
  "Insert a combinational process."
  (interactive)
  (vhdl-template-process 'comb))

(defun vhdl-template-process-seq ()
  "Insert a sequential process."
  (interactive)
  (vhdl-template-process 'seq))

(defun vhdl-template-quantity ()
  "Insert a quantity declaration."
  (interactive)
  (if (vhdl-in-argument-list-p)
      (let ((start (point)))
	(vhdl-insert-keyword "QUANTITY ")
	(when (vhdl-template-field "names" nil t start (point))
	  (insert " : ")
	  (vhdl-template-field "[IN | OUT]" " " t)
	  (vhdl-template-field "type")
	  (insert ";")
	  (vhdl-comment-insert-inline)))
    (let ((char (vhdl-decision-query
		 "quantity" "(f)ree, (b)ranch, or (s)ource quantity?" t)))
      (cond ((eq char ?f) (vhdl-template-quantity-free))
	    ((eq char ?b) (vhdl-template-quantity-branch))
	    ((eq char ?s) (vhdl-template-quantity-source))
	    (t (vhdl-template-undo (point) (point)))))))

(defun vhdl-template-quantity-free ()
  "Insert a free quantity declaration."
  (interactive)
  (vhdl-insert-keyword "QUANTITY ")
  (vhdl-template-field "names")
  (insert " : ")
  (vhdl-template-field "type")
  (let ((position (point)))
    (insert " := ")
    (unless (vhdl-template-field "[initialization]" nil t)
      (delete-region position (point)))
    (insert ";")
    (vhdl-comment-insert-inline)))

(defun vhdl-template-quantity-branch ()
  "Insert a branch quantity declaration."
  (interactive)
  (let (position)
    (vhdl-insert-keyword "QUANTITY ")
    (when (vhdl-template-field "[across names]" " " t)
      (vhdl-insert-keyword "ACROSS "))
    (when (vhdl-template-field "[through names]" " " t)
      (vhdl-insert-keyword "THROUGH "))
    (vhdl-template-field "plus terminal name")
    (setq position (point))
    (vhdl-insert-keyword " TO ")
    (unless (vhdl-template-field "[minus terminal name]" nil t)
      (delete-region position (point)))
    (insert ";")
    (vhdl-comment-insert-inline)))

(defun vhdl-template-quantity-source ()
  "Insert a source quantity declaration."
  (interactive)
  (vhdl-insert-keyword "QUANTITY ")
  (vhdl-template-field "names")
  (insert " : ")
  (vhdl-template-field "type" " ")
  (if (eq (vhdl-decision-query nil "(s)pectrum or (n)oise?") ?n)
      (progn (vhdl-insert-keyword "NOISE ")
	     (vhdl-template-field "power expression"))
    (vhdl-insert-keyword "SPECTRUM ")
    (vhdl-template-field "magnitude expression" ", ")
    (vhdl-template-field "phase expression"))
  (insert ";")
  (vhdl-comment-insert-inline))

(defun vhdl-template-record (kind &optional name secondary)
  "Insert a record type declaration."
  (interactive)
  (let ((margin (current-column))
	(start (point))
	(first t))
    (vhdl-insert-keyword "RECORD\n")
    (indent-to (+ margin vhdl-basic-offset))
    (when (or (vhdl-template-field "element names"
				   nil (not secondary) start (point))
	      secondary)
      (while (or first (vhdl-template-field "[element names]" nil t))
	(insert " : ")
	(vhdl-template-field (if (eq kind 'type) "type" "nature") ";")
	(vhdl-comment-insert-inline)
	(insert "\n")
	(indent-to (+ margin vhdl-basic-offset))
	(setq first nil))
      (kill-line -0)
      (indent-to margin)
      (vhdl-insert-keyword "END RECORD")
      (unless (vhdl-standard-p '87) (and name (insert " " name)))
      (insert ";")
      (when vhdl-auto-align (vhdl-align-noindent-region start (point) 1)))))

(defun vhdl-template-report ()
  "Insert a report statement."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "REPORT ")
    (if (equal "\"\"" (vhdl-template-field
		       "string expression" nil t start (point) t))
	(backward-delete-char 2)
      (setq start (point))
      (vhdl-insert-keyword " SEVERITY ")
      (unless (vhdl-template-field "[NOTE | WARNING | ERROR | FAILURE]" nil t)
	(delete-region start (point)))
      (insert ";"))))

(defun vhdl-template-return ()
  "Insert a return statement."
  (interactive)
  (vhdl-insert-keyword "RETURN ")
  (unless (vhdl-template-field "[expression]" nil t)
    (delete-char -1))
  (insert ";"))

(defun vhdl-template-selected-signal-asst ()
  "Insert a selected signal assignment."
  (interactive)
  (let ((margin (current-indentation))
	(start (point))
	(choices t))
    (let ((position (point)))
      (vhdl-insert-keyword " SELECT ")
      (goto-char position))
    (vhdl-insert-keyword "WITH ")
    (when (vhdl-template-field "selector expression"
			       nil t start (+ (point) 7))
      (forward-word 1)
      (delete-char 1)
      (insert "\n")
      (indent-to (+ margin vhdl-basic-offset))
      (vhdl-template-field "target signal" " <= ")
;      (vhdl-template-field "[GUARDED] [TRANSPORT]")
      (insert "\n")
      (indent-to (+ margin vhdl-basic-offset))
      (vhdl-template-field "waveform")
      (vhdl-insert-keyword " WHEN ")
      (vhdl-template-field "choices" ",")
      (insert "\n")
      (indent-to (+ margin vhdl-basic-offset))
      (while (and choices (vhdl-template-field "[waveform]" nil t))
	(vhdl-insert-keyword " WHEN ")
	(if (setq choices (vhdl-template-field "[choices]" "," t))
	    (progn (insert "\n") (indent-to (+ margin vhdl-basic-offset)))
	  (vhdl-insert-keyword "OTHERS")))
      (when choices
	(fixup-whitespace)
	(delete-char -2))
      (insert ";")
      (when vhdl-auto-align (vhdl-align-noindent-region start (point) 1)))))

(defun vhdl-template-signal ()
  "Insert a signal declaration."
  (interactive)
  (let ((start (point))
	(in-arglist (vhdl-in-argument-list-p)))
    (vhdl-insert-keyword "SIGNAL ")
    (when (vhdl-template-field "names" nil t start (point))
      (insert " : ")
      (when in-arglist (vhdl-template-field "[IN | OUT | INOUT]" " " t))
      (vhdl-template-field "type")
      (if in-arglist
	  (progn (insert ";")
		 (vhdl-comment-insert-inline))
	(let ((position (point)))
	  (insert " := ")
	  (unless (vhdl-template-field "[initialization]" nil t)
	    (delete-region position (point)))
	  (insert ";")
	  (vhdl-comment-insert-inline))))))

(defun vhdl-template-subnature ()
  "Insert a subnature declaration."
  (interactive)
  (let ((start (point))
	position)
    (vhdl-insert-keyword "SUBNATURE ")
    (when (vhdl-template-field "name" nil t start (point))
      (vhdl-insert-keyword " IS ")
      (vhdl-template-field "nature" " (")
      (if (vhdl-template-field "[index range]" nil t)
	  (insert ")")
	(delete-char -2))
      (setq position (point))
      (vhdl-insert-keyword " TOLERANCE ")
      (if (equal "\"\"" (vhdl-template-field "[string expression]"
					     nil t nil nil t))
	  (delete-region position (point))
	(vhdl-insert-keyword " ACROSS ")
	(vhdl-template-field "string expression" nil nil nil nil t)
	(vhdl-insert-keyword " THROUGH"))
      (insert ";")
      (vhdl-comment-insert-inline))))

(defun vhdl-template-subprogram-body ()
  "Insert a subprogram body."
  (interactive)
  (if (eq (vhdl-decision-query nil "(p)rocedure or (f)unction?" t) ?f)
      (vhdl-template-function-body)
    (vhdl-template-procedure-body)))

(defun vhdl-template-subprogram-decl ()
  "Insert a subprogram declaration."
  (interactive)
  (if (eq (vhdl-decision-query nil "(p)rocedure or (f)unction?" t) ?f)
      (vhdl-template-function-decl)
    (vhdl-template-procedure-decl)))

(defun vhdl-template-subtype ()
  "Insert a subtype declaration."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "SUBTYPE ")
    (when (vhdl-template-field "name" nil t start (point))
      (vhdl-insert-keyword " IS ")
      (vhdl-template-field "type" " ")
      (unless
	  (vhdl-template-field "[RANGE value range | ( index range )]" nil t)
	(delete-char -1))
      (insert ";")
      (vhdl-comment-insert-inline))))

(defun vhdl-template-terminal ()
  "Insert a terminal declaration."
  (interactive)
  (let ((start (point)))
    (vhdl-insert-keyword "TERMINAL ")
    (when (vhdl-template-field "names" nil t start (point))
      (insert " : ")
      (vhdl-template-field "nature")
      (insert ";")
      (vhdl-comment-insert-inline))))

(defun vhdl-template-type ()
  "Insert a type declaration."
  (interactive)
  (let ((start (point))
	name mid-pos end-pos)
    (vhdl-insert-keyword "TYPE ")
    (when (setq name (vhdl-template-field "name" nil t start (point)))
      (vhdl-insert-keyword " IS ")
      (let ((definition
	      (upcase
	       (or (vhdl-template-field
		    "[scalar type | ARRAY | RECORD | ACCESS | FILE]" nil t)
		   ""))))
	(cond ((equal definition "")
	       (backward-delete-char 4)
	       (insert ";"))
	      ((equal definition "ARRAY")
	       (kill-word -1)
	       (vhdl-template-array 'type t))
	      ((equal definition "RECORD")
	       (setq mid-pos (point-marker))
	       (kill-word -1)
	       (vhdl-template-record 'type name t))
	      ((equal definition "ACCESS")
	       (insert " ")
	       (vhdl-template-field "type" ";"))
	      ((equal definition "FILE")
	       (vhdl-insert-keyword " OF ")
	       (vhdl-template-field "type" ";"))
	      (t (insert ";")))
	(when mid-pos
	  (setq end-pos (point-marker))
	  (goto-char mid-pos)
	  (end-of-line))
	(vhdl-comment-insert-inline)
	(when end-pos (goto-char end-pos))))))

(defun vhdl-template-use ()
  "Insert a use clause."
  (interactive)
  (let ((start (point))
	(case-fold-search t))
    (vhdl-ext-syntax-table
     (vhdl-insert-keyword "USE ")
     (when (save-excursion (beginning-of-line) (looking-at "^\\s-*use\\>"))
       (vhdl-insert-keyword "..ALL;")
       (backward-char 6)
       (when (vhdl-template-field "library name" nil t start (+ (point) 6))
	 (forward-char 1)
	 (vhdl-template-field "package name")
	 (forward-char 5))))))

(defun vhdl-template-variable ()
  "Insert a variable declaration."
  (interactive)
  (let ((start (point))
	(case-fold-search t)
	(in-arglist (vhdl-in-argument-list-p)))
    (vhdl-ext-syntax-table
     (if (or (save-excursion
	       (and (re-search-backward
		     "\\<function\\|procedure\\|process\\|procedural\\|end\\>"
		     nil t)
		    (not (progn (backward-word 1) (looking-at "\\<end\\>")))))
	     (save-excursion (backward-word 1) (looking-at "\\<shared\\>")))
	 (vhdl-insert-keyword "VARIABLE ")
       (vhdl-insert-keyword "SHARED VARIABLE ")))
    (when (vhdl-template-field "names" nil t start (point))
      (insert " : ")
      (when in-arglist (vhdl-template-field "[IN | OUT | INOUT]" " " t))
      (vhdl-template-field "type")
      (if in-arglist
	  (progn (insert ";")
		 (vhdl-comment-insert-inline))
	(let ((position (point)))
	  (insert " := ")
	  (unless (vhdl-template-field "[initialization]" nil t)
	    (delete-region position (point)))
	  (insert ";")
	  (vhdl-comment-insert-inline))))))

(defun vhdl-template-wait ()
  "Insert a wait statement."
  (interactive)
  (vhdl-insert-keyword "WAIT ")
  (unless (vhdl-template-field
	   "[ON sensitivity list] [UNTIL condition] [FOR time expression]"
	   nil t)
    (delete-char -1))
  (insert ";"))

(defun vhdl-template-when ()
  "Indent correctly if within a case statement."
  (interactive)
  (let ((position (point))
	(case-fold-search t)
	margin)
    (vhdl-ext-syntax-table
     (if (and (= (current-column) (current-indentation))
	      (re-search-forward "\\<end\\>" nil t)
	      (looking-at "\\s-*\\<case\\>"))
	 (progn
	   (setq margin (current-indentation))
	   (goto-char position)
	   (delete-horizontal-space)
	   (indent-to (+ margin vhdl-basic-offset)))
       (goto-char position)))
    (vhdl-insert-keyword "WHEN ")))

(defun vhdl-template-while-loop ()
  "Insert a while loop."
  (interactive)
  (let* ((margin (current-indentation))
	 (start (point))
	 label)
    (if (not (eq vhdl-optional-labels 'all))
	(vhdl-insert-keyword "WHILE ")
      (vhdl-insert-keyword ": WHILE ")
      (goto-char start)
      (setq label (vhdl-template-field "[label]" nil t))
      (unless label (delete-char 2))
      (forward-word 1)
      (forward-char 1))
    (when vhdl-conditions-in-parenthesis (insert "("))
    (when (vhdl-template-field "condition" nil t start (point))
      (when vhdl-conditions-in-parenthesis (insert ")"))
      (vhdl-insert-keyword " LOOP\n\n")
      (indent-to margin)
      (vhdl-insert-keyword "END LOOP")
      (insert (if label (concat " " label ";") ";"))
      (forward-line -1)
      (indent-to (+ margin vhdl-basic-offset)))))

(defun vhdl-template-with ()
  "Insert a with statement (i.e. selected signal assignment)."
  (interactive)
  (let ((case-fold-search t))
    (vhdl-ext-syntax-table
     (if (save-excursion
	   (re-search-backward "\\(\\<limit\\>\\|;\\)")
	   (equal ";" (match-string 1)))
	 (vhdl-template-selected-signal-asst)
       (vhdl-insert-keyword "WITH ")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special templates

(defun vhdl-template-clocked-wait ()
  "Insert a wait statement for rising/falling clock edge."
  (interactive)
  (let ((start (point))
	clock)
    (vhdl-insert-keyword "WAIT UNTIL ")
    (when (setq clock
		(or (and (not (equal "" vhdl-clock-name))
			 (progn (insert vhdl-clock-name) vhdl-clock-name))
		    (vhdl-template-field "clock name" nil t start (point))))
      (insert "'event")
      (vhdl-insert-keyword " AND ")
      (insert clock)
      (insert
       " = " (if vhdl-clock-rising-edge vhdl-one-string vhdl-zero-string) ";")
      (vhdl-comment-insert-inline
       (concat (if vhdl-clock-rising-edge "rising" "falling")
	       " clock edge")))))

(defun vhdl-template-seq-process (clock reset)
  "Insert a template for the body of a sequential process."
  (let ((margin (current-indentation))
	position)
    (vhdl-insert-keyword "IF ")
    (when (eq vhdl-reset-kind 'async)
      (insert reset " = "
	      (if vhdl-reset-active-high vhdl-one-string vhdl-zero-string))
      (vhdl-insert-keyword " THEN")
      (vhdl-comment-insert-inline
       (concat "asynchronous reset (active "
	       (if vhdl-reset-active-high "high" "low") ")"))
      (insert "\n") (indent-to (+ margin vhdl-basic-offset))
      (setq position (point))
      (insert "\n") (indent-to margin)
      (vhdl-insert-keyword "ELSIF "))
    (if (eq vhdl-clock-edge-condition 'function)
	(insert (if vhdl-clock-rising-edge "rising" "falling")
		"_edge(" clock ")")
      (insert clock "'event")
      (vhdl-insert-keyword " AND ")
      (insert clock " = "
	      (if vhdl-clock-rising-edge vhdl-one-string vhdl-zero-string)))
    (vhdl-insert-keyword " THEN")
    (vhdl-comment-insert-inline
     (concat (if vhdl-clock-rising-edge "rising" "falling") " clock edge"))
    (insert "\n") (indent-to (+ margin vhdl-basic-offset))
    (when (eq vhdl-reset-kind 'sync)
      (vhdl-insert-keyword "IF ")
      (setq reset (or (and (not (equal "" vhdl-reset-name))
			   (progn (insert vhdl-reset-name) vhdl-reset-name))
		      (vhdl-template-field "reset name") "<reset>"))
      (insert " = "
	      (if vhdl-reset-active-high vhdl-one-string vhdl-zero-string))
      (vhdl-insert-keyword " THEN")
      (vhdl-comment-insert-inline
       (concat "synchronous reset (active "
	       (if vhdl-reset-active-high "high" "low") ")"))
      (insert "\n") (indent-to (+ margin (* 2 vhdl-basic-offset)))
      (setq position (point))
      (insert "\n") (indent-to (+ margin vhdl-basic-offset))
      (vhdl-insert-keyword "ELSE")
      (insert "\n") (indent-to (+ margin (* 2 vhdl-basic-offset)))
      (insert "\n") (indent-to (+ margin vhdl-basic-offset))
      (vhdl-insert-keyword "END IF;"))
    (when (eq vhdl-reset-kind 'none)
      (setq position (point)))
    (insert "\n") (indent-to margin)
    (vhdl-insert-keyword "END IF;")
    (goto-char position)
    reset))

(defun vhdl-template-standard-package (library package)
  "Insert specification of a standard package.  Include a library
specification, if not already there."
  (let ((margin (current-indentation))
	(case-fold-search t))
    (save-excursion
      (vhdl-ext-syntax-table
       (and (not (bobp))
	    (re-search-backward
	     (concat "^\\s-*\\(library\\s-+\\(\\(\\w\\|\\s_\\)+,\\s-+\\)*"
		     library "\\|end\\)\\>") nil t))))
    (unless (and (match-string 1) (string-match "library" (match-string 1)))
      (vhdl-insert-keyword "LIBRARY ")
      (insert library ";\n")
      (indent-to margin))
    (vhdl-insert-keyword "USE ")
    (insert library "." package)
    (vhdl-insert-keyword ".ALL;")))

(defun vhdl-template-package-math-complex ()
  "Insert specification of `math_complex' package."
  (interactive)
  (vhdl-template-standard-package "ieee" "math_complex"))

(defun vhdl-template-package-math-real ()
  "Insert specification of `math_real' package."
  (interactive)
  (vhdl-template-standard-package "ieee" "math_real"))

(defun vhdl-template-package-numeric-bit ()
  "Insert specification of `numeric_bit' package."
  (interactive)
  (vhdl-template-standard-package "ieee" "numeric_bit"))

(defun vhdl-template-package-numeric-std ()
  "Insert specification of `numeric_std' package."
  (interactive)
  (vhdl-template-standard-package "ieee" "numeric_std"))

(defun vhdl-template-package-std-logic-1164 ()
  "Insert specification of `std_logic_1164' package."
  (interactive)
  (vhdl-template-standard-package "ieee" "std_logic_1164"))

(defun vhdl-template-package-std-logic-arith ()
  "Insert specification of `std_logic_arith' package."
  (interactive)
  (vhdl-template-standard-package "ieee" "std_logic_arith"))

(defun vhdl-template-package-std-logic-misc ()
  "Insert specification of `std_logic_misc' package."
  (interactive)
  (vhdl-template-standard-package "ieee" "std_logic_misc"))

(defun vhdl-template-package-std-logic-signed ()
  "Insert specification of `std_logic_signed' package."
  (interactive)
  (vhdl-template-standard-package "ieee" "std_logic_signed"))

(defun vhdl-template-package-std-logic-textio ()
  "Insert specification of `std_logic_textio' package."
  (interactive)
  (vhdl-template-standard-package "ieee" "std_logic_textio"))

(defun vhdl-template-package-std-logic-unsigned ()
  "Insert specification of `std_logic_unsigned' package."
  (interactive)
  (vhdl-template-standard-package "ieee" "std_logic_unsigned"))

(defun vhdl-template-package-textio ()
  "Insert specification of `textio' package."
  (interactive)
  (vhdl-template-standard-package "std" "textio"))

(defun vhdl-template-directive (directive)
  "Insert directive."
  (unless (= (current-indentation) (current-column))
    (delete-horizontal-space)
    (insert "  "))
  (insert "-- pragma " directive))

(defun vhdl-template-directive-translate-on ()
  "Insert directive 'translate_on'."
  (interactive)
  (vhdl-template-directive "translate_on"))

(defun vhdl-template-directive-translate-off ()
  "Insert directive 'translate_off'."
  (interactive)
  (vhdl-template-directive "translate_off"))

(defun vhdl-template-directive-synthesis-on ()
  "Insert directive 'synthesis_on'."
  (interactive)
  (vhdl-template-directive "synthesis_on"))

(defun vhdl-template-directive-synthesis-off ()
  "Insert directive 'synthesis_off'."
  (interactive)
  (vhdl-template-directive "synthesis_off"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comment templates and functions

(defun vhdl-comment-indent ()
  "Indent comments."
  (let* ((position (point))
	 (col
	  (progn
	    (forward-line -1)
	    (if (re-search-forward "--" position t)
		(- (current-column) 2)	; existing comment at bol stays there
	      (goto-char position)
	      (skip-chars-backward " \t")
	      (max comment-column	; else indent to comment column
		   (1+ (current-column))))))) ; except leave at least one space
    (goto-char position)
    col))

(defun vhdl-comment-insert ()
  "Start a comment at the end of the line.
If on line with code, indent at least `comment-column'.
If starting after end-comment-column, start a new line."
  (interactive)
  (when (> (current-column) end-comment-column) (newline-and-indent))
  (if (or (looking-at "\\s-*$") ; end of line
	  (and (not unread-command-events) ; called with key binding or menu
	       (not (end-of-line))))
      (let (margin)
	(while (= (preceding-char) ?-) (delete-char -1))
	(setq margin (current-column))
	(delete-horizontal-space)
	(if (bolp)
	    (progn (indent-to margin) (insert "--"))
	  (insert "  ")
	  (indent-to comment-column)
	  (insert "--"))
	(if (not unread-command-events) (insert " ")))
    ;; else code following current point implies commenting out code
    (let (next-input code)
      (while (= (preceding-char) ?-) (delete-char -2))
      (while (= (setq next-input (read-char)) 13) ; CR
	(insert "--") ; or have a space after it?
	(forward-char -2)
	(forward-line 1)
	(message "Enter CR if commenting out a line of code.")
	(setq code t))
      (when (not code)
	(insert "--")) ; hardwire to 1 space or use vhdl-basic-offset?
      (setq unread-command-events
	    (list (vhdl-character-to-event next-input)))))) ; pushback the char

(defun vhdl-comment-display (&optional line-exists)
  "Add 2 comment lines at the current indent, making a display comment."
  (interactive)
  (let ((margin (current-indentation)))
    (when (not line-exists) (vhdl-comment-display-line))
    (insert "\n") (indent-to margin)
    (insert "\n") (indent-to margin)
    (vhdl-comment-display-line)
    (end-of-line -0)
    (insert "-- ")))

(defun vhdl-comment-display-line ()
  "Displays one line of dashes."
  (interactive)
  (while (= (preceding-char) ?-) (delete-char -2))
  (let* ((col (current-column))
	 (len (- end-comment-column col)))
    (insert-char ?- len)))

(defun vhdl-comment-append-inline ()
  "Append empty inline comment to current line."
  (interactive)
  (end-of-line)
  (delete-horizontal-space)
  (insert "  ")
  (indent-to comment-column)
  (insert "-- "))

(defun vhdl-comment-insert-inline (&optional string always-insert)
  "Insert inline comment."
  (when (or (and string (or vhdl-self-insert-comments always-insert))
	    (and (not string) vhdl-prompt-for-comments))
    (let ((position (point)))
      (insert "  ")
      (indent-to comment-column)
      (insert "-- ")
      (if (or (and string (progn (insert string) t))
	      (vhdl-template-field "[comment]" nil t))
	  (when (> (current-column) end-comment-column)
	    (setq position (point-marker))
	    (re-search-backward "-- ")
	    (insert "\n")
	    (indent-to comment-column)
	    (goto-char position))
	(delete-region position (point))))))

(defun vhdl-comment-block ()
  "Insert comment for code block."
  (when vhdl-prompt-for-comments
    (let ((final-pos (point-marker))
	  (case-fold-search t))
      (vhdl-ext-syntax-table
       (when (and (re-search-backward "^\\s-*begin\\>" nil t)
		  (re-search-backward
		   "\\<\\(architecture\\|block\\|function\\|procedure\\|process\\|procedural\\)\\>"
		   nil t))
	 (let (margin)
	   (back-to-indentation)
	   (setq margin (current-column))
	   (end-of-line -0)
	   (if (bobp)
	       (progn (insert "\n") (forward-line -1))
	     (insert "\n"))
	   (indent-to margin)
	   (insert "-- purpose: ")
	   (unless (vhdl-template-field "[description]" nil t)
	     (vhdl-line-kill-entire)))))
      (goto-char final-pos))))

(defun vhdl-comment-uncomment-region (beg end &optional arg)
  "Comment out region if not commented out, uncomment otherwise."
  (interactive "r\nP")
  (save-excursion
    (goto-char (1- end))
    (end-of-line)
    (setq end (point-marker))
    (goto-char beg)
    (beginning-of-line)
    (setq beg (point))
    (if (looking-at comment-start)
	(comment-region beg end -1)
      (comment-region beg end))))

(defun vhdl-comment-uncomment-line (&optional arg)
  "Comment out line if not commented out, uncomment otherwise."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (let ((position (point)))
      (forward-line (or arg 1))
      (vhdl-comment-uncomment-region position (point)))))

(defun vhdl-comment-kill-region (beg end)
  "Kill comments in region."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char beg)
    (beginning-of-line)
    (while (< (point) end)
      (if (looking-at "^\\(\\s-*--.*\n\\)")
	  (progn (delete-region (match-beginning 1) (match-end 1)))
	(beginning-of-line 2)))))

(defun vhdl-comment-kill-inline-region (beg end)
  "Kill inline comments in region."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char beg)
    (beginning-of-line)
    (while (< (point) end)
      (when (looking-at "^.*[^ \t\n-]+\\(\\s-*--.*\\)$")
	(delete-region (match-beginning 1) (match-end 1)))
      (beginning-of-line 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtemplates

(defun vhdl-template-begin-end (construct name margin &optional empty-lines)
  "Insert a begin ... end pair with optional name after the end.
Point is left between them."
  (let (position)
    (insert "\n")
    (when (or empty-lines (eq vhdl-insert-empty-lines 'all)) (insert "\n"))
    (indent-to margin)
    (vhdl-insert-keyword "BEGIN")
    (when (and (or construct name) vhdl-self-insert-comments)
      (insert "  --")
      (when construct (insert " ") (vhdl-insert-keyword construct))
      (when name (insert " " name)))
    (insert "\n")
    (when (or empty-lines (eq vhdl-insert-empty-lines 'all)) (insert "\n"))
    (indent-to (+ margin vhdl-basic-offset))
    (setq position (point))
    (insert "\n")
    (when (or empty-lines (eq vhdl-insert-empty-lines 'all)) (insert "\n"))
    (indent-to margin)
    (vhdl-insert-keyword "END")
    (when construct (insert " ") (vhdl-insert-keyword construct))
    (insert (if name (concat " " name) "") ";")
    (goto-char position)))

(defun vhdl-template-argument-list (&optional is-function)
  "Read from user a procedure or function argument list."
  (insert " (")
  (let ((margin (current-column))
	(start (point))
	(end-pos (point))
	not-empty interface semicolon-pos)
    (when (not vhdl-argument-list-indent)
      (setq margin (+ (current-indentation) vhdl-basic-offset))
      (insert "\n")
      (indent-to margin))
    (setq interface (vhdl-template-field
		     (concat "[CONSTANT | SIGNAL"
			     (unless is-function " | VARIABLE") "]") " " t))
    (while (vhdl-template-field "[names]" nil t)
      (setq not-empty t)
      (insert " : ")
      (when (not is-function)
	(if (and interface (equal (upcase interface) "CONSTANT"))
	    (vhdl-insert-keyword "IN ")
	  (vhdl-template-field "[IN | OUT | INOUT]" " " t)))
      (vhdl-template-field "type")
      (setq semicolon-pos (point))
      (insert ";")
      (vhdl-comment-insert-inline)
      (setq end-pos (point))
      (insert "\n")
      (indent-to margin)
      (setq interface (vhdl-template-field
		       (concat "[CONSTANT | SIGNAL"
			       (unless is-function " | VARIABLE") "]") " " t)))
    (delete-region end-pos (point))
    (when semicolon-pos (goto-char semicolon-pos))
    (if not-empty
	(progn (delete-char 1) (insert ")"))
      (backward-delete-char 2))))

(defun vhdl-template-generic-list (optional &optional no-value)
  "Read from user a generic spec argument list."
  (let (margin
	(start (point)))
    (vhdl-insert-keyword "GENERIC (")
    (setq margin (current-column))
    (when (not vhdl-argument-list-indent)
      (let ((position (point)))
	(back-to-indentation)
	(setq margin (+ (current-column) vhdl-basic-offset))
	(goto-char position)
	(insert "\n")
	(indent-to margin)))
    (let ((vhdl-generics (vhdl-template-field
			  (concat (and optional "[") "name"
				  (and no-value "s") (and optional "]"))
			  nil optional)))
      (if (not vhdl-generics)
	  (if optional
	      (progn (vhdl-line-kill-entire) (end-of-line -0)
		     (when (not vhdl-argument-list-indent)
		       (vhdl-line-kill-entire) (end-of-line -0)))
	    (vhdl-template-undo start (point))
	    nil )
	(insert " : ")
	(let (semicolon-pos end-pos)
	  (while vhdl-generics
	    (vhdl-template-field "type")
	    (if no-value
		(progn (setq semicolon-pos (point))
		       (insert ";"))
	      (insert " := ")
	      (unless (vhdl-template-field "[value]" nil t)
		(delete-char -4))
	      (setq semicolon-pos (point))
	      (insert ";"))
	    (vhdl-comment-insert-inline)
	    (setq end-pos (point))
 	    (insert "\n")
	    (indent-to margin)
	    (setq vhdl-generics (vhdl-template-field
				 (concat "[name" (and no-value "s") "]")
				 " : " t)))
	  (delete-region end-pos (point))
	  (goto-char semicolon-pos)
	  (insert ")")
	  (end-of-line)
	  (when vhdl-auto-align (vhdl-align-noindent-region start (point) 1))
	  t)))))

(defun vhdl-template-port-list (optional)
  "Read from user a port spec argument list."
  (let ((start (point))
	margin vhdl-ports object)
    (vhdl-insert-keyword "PORT (")
    (setq margin (current-column))
    (when (not vhdl-argument-list-indent)
      (let ((position (point)))
	(back-to-indentation)
	(setq margin (+ (current-column) vhdl-basic-offset))
	(goto-char position)
	(insert "\n")
	(indent-to margin)))
    (when (vhdl-standard-p 'ams)
      (setq object (vhdl-template-field "[SIGNAL | TERMINAL | QUANTITY]"
					" " t)))
    (setq vhdl-ports (vhdl-template-field
		      (concat (and optional "[") "names" (and optional "]"))
		      nil optional))
    (if (not vhdl-ports)
	(if optional
	    (progn (vhdl-line-kill-entire) (end-of-line -0)
		   (when (not vhdl-argument-list-indent)
		     (vhdl-line-kill-entire) (end-of-line -0)))
	  (vhdl-template-undo start (point))
	  nil)
      (insert " : ")
      (let (semicolon-pos end-pos)
	(while vhdl-ports
	  (cond ((or (null object) (equal "SIGNAL" (upcase object)))
		 (vhdl-template-field "IN | OUT | INOUT" " "))
		((equal "QUANTITY" (upcase object))
		 (vhdl-template-field "[IN | OUT]" " " t)))
	  (vhdl-template-field
	   (if (and object (equal "TERMINAL" (upcase object)))
		    "nature" "type"))
	  (setq semicolon-pos (point))
	  (insert ";")
	  (vhdl-comment-insert-inline)
	  (setq end-pos (point))
	  (insert "\n")
	  (indent-to margin)
	  (when (vhdl-standard-p 'ams)
	    (setq object (vhdl-template-field "[SIGNAL | TERMINAL | QUANTITY]"
					      " " t)))
	  (setq vhdl-ports (vhdl-template-field "[names]" " : " t)))
	(delete-region end-pos (point))
	(goto-char semicolon-pos)
	(insert ")")
	(end-of-line)
	(when vhdl-auto-align (vhdl-align-noindent-region start end-pos 1))
	t))))

(defun vhdl-template-generate-body (margin label)
  "Insert body for generate template."
  (vhdl-insert-keyword " GENERATE")
  (if (not (vhdl-standard-p '87))
      (vhdl-template-begin-end "GENERATE" label margin)
    (insert "\n\n")
    (indent-to margin)
    (vhdl-insert-keyword "END GENERATE ")
    (insert label ";")
    (end-of-line 0)
    (indent-to (+ margin vhdl-basic-offset))))

(defun vhdl-template-insert-date ()
  "Insert date in appropriate format."
  (interactive)
  (insert
   (cond
    ;; 'american, 'european', 'scientific kept for backward compatibility
    ((eq vhdl-date-format 'american) (format-time-string "%m/%d/%Y" nil))
    ((eq vhdl-date-format 'european) (format-time-string "%d.%m.%Y" nil))
    ((eq vhdl-date-format 'scientific) (format-time-string "%Y/%m/%d" nil))
    (t (format-time-string vhdl-date-format nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help functions

(defun vhdl-electric-space (count)
  "Expand abbreviations and self-insert space(s), do indent-new-comment-line
if in comment and past end-comment-column."
  (interactive "p")
  (cond ((vhdl-in-comment-p)
	 (self-insert-command count)
	 (cond ((>= (current-column) (+ 2 end-comment-column))
		(backward-word 1)
		(indent-new-comment-line)
		(forward-word 1)
		(forward-char 1))
	       ((>= (current-column) end-comment-column)
		(indent-new-comment-line))
	       (t nil)))
	((or (and (>= (preceding-char) ?a) (<= (preceding-char) ?z))
	     (and (>= (preceding-char) ?A) (<= (preceding-char) ?Z)))
	 (vhdl-ext-syntax-table
	  (let ((case-fold-search t))
	    (expand-abbrev)))
	 (self-insert-command count))
	(t (self-insert-command count))))

(defun vhdl-template-field (prompt &optional follow-string optional
				   begin end is-string default)
  "Prompt for string and insert it in buffer with optional FOLLOW-STRING.
If OPTIONAL is nil, the prompt is left if an empty string is inserted.  If
an empty string is inserted, return nil and call `vhdl-template-undo' for
the region between BEGIN and END.  IS-STRING indicates whether a string
with double-quotes is to be inserted.  DEFAULT specifies a default string."
  (let ((position (point))
	string)
    (insert "<" prompt ">")
    (setq string
	  (condition-case ()
	      (read-from-minibuffer (concat prompt ": ")
				    (or (and is-string '("\"\"" . 2)) default)
				    vhdl-minibuffer-local-map)
	    (quit (if (and optional begin end)
		      (progn (beep) "")
		    (keyboard-quit)))))
    (when (or (not (equal string "")) optional)
      (delete-region position (point)))
    (when (and (equal string "") optional begin end)
      (vhdl-template-undo begin end)
      (message "Template aborted"))
    (when (not (equal string ""))
      (insert string)
      (vhdl-fix-case-region-1 position (point) vhdl-upper-case-keywords
			      vhdl-keywords-regexp))
    (when (or (not (equal string "")) (not optional))
      (insert (or follow-string "")))
    (if (equal string "") nil string)))

(defun vhdl-decision-query (string prompt &optional optional)
  "Query a decision from the user."
  (let ((start (point)))
    (when string (vhdl-insert-keyword (concat string " ")))
    (message prompt)
    (let ((char (read-char)))
      (delete-region start (point))
      (if (and optional (eq char ?\r))
	  (progn (insert " ")
		 (unexpand-abbrev)
		 (throw 'abort "Template aborted"))
	char))))

(defun vhdl-insert-keyword (keyword)
  "Insert KEYWORD and adjust case."
  (insert (if vhdl-upper-case-keywords (upcase keyword) (downcase keyword))))

(defun vhdl-case-keyword (keyword)
  "Adjust case of KEYWORD."
  (if vhdl-upper-case-keywords (upcase keyword) (downcase keyword)))

(defun vhdl-case-word (num)
  "Adjust case or following NUM words."
  (if vhdl-upper-case-keywords (upcase-word num) (downcase-word num)))

(defun vhdl-minibuffer-tab (&optional prefix-arg)
  "If preceeding character is part of a word or a paren then hippie-expand,
else if right of non whitespace on line then tab-to-tab-stop,
else indent line in proper way for current major mode (used for word
completion in VHDL minibuffer)."
  (interactive "P")
  (cond ((= (char-syntax (preceding-char)) ?w)
	 (let ((case-fold-search (not vhdl-word-completion-case-sensitive))
	       (case-replace nil))
	   (vhdl-expand-abbrev prefix-arg)))
	((or (= (preceding-char) ?\() (= (preceding-char) ?\)))
	 (let ((case-fold-search (not vhdl-word-completion-case-sensitive))
	       (case-replace nil))
	   (vhdl-expand-paren prefix-arg)))
	((> (current-column) (current-indentation))
 	 (tab-to-tab-stop))
	(t (if (eq indent-line-function 'indent-to-left-margin)
	       (insert-tab prefix-arg)
	     (if prefix-arg
		 (funcall indent-line-function prefix-arg)
	       (funcall indent-line-function))))))

(defun vhdl-template-search-prompt ()
  "Search for left out template prompts and query again."
  (interactive)
  (let ((case-fold-search t))
    (vhdl-ext-syntax-table
     (when (or (re-search-forward
		(concat "<\\(" vhdl-template-prompt-syntax "\\)>") nil t)
	       (re-search-backward
		(concat "<\\(" vhdl-template-prompt-syntax "\\)>") nil t))
       (let ((string (match-string 1)))
	 (replace-match "")
	 (vhdl-template-field string))))))

(defun vhdl-template-undo (begin end)
  "Undo aborted template by deleting region and unexpanding the keyword."
  (cond (vhdl-template-invoked-by-hook
	 (goto-char end)
	 (insert " ")
	 (delete-region begin end)
	 (unexpand-abbrev))
	(t (delete-region begin end))))

(defun vhdl-insert-string-or-file (string)
  "Insert STRING or file contents if STRING is an existing file name."
  (unless (equal string "")
    (cond ((file-exists-p string)
	   (forward-char (cadr (insert-file-contents string))))
	  (t (insert string)))))

(defun vhdl-sequential-statement-p ()
  "Check if point is within sequential statement part."
  (save-excursion
    (let ((case-fold-search t)
	  (start (point)))
      (vhdl-ext-syntax-table
       (set-match-data nil)
       (while (and (re-search-backward "^\\s-*\\(begin\\|end\\(\\s-*\\(case\\|if\\|loop\\)\\)?\\)\\>"
				       nil t)
		   (match-string 2)))
       (and (match-data)
	    (equal "BEGIN" (upcase (match-string 1)))
	    (re-search-backward "^\\s-*\\(\\w+\\s-*:\\s-*\\)?\\(\\w+\\s-+\\)?\\(function\\|procedure\\|process\\|procedural\\|end\\)\\>"
				nil t)
	    (not (equal "END" (upcase (match-string 3)))))))))

(defun vhdl-in-argument-list-p ()
  "Check if within an argument list."
  (save-excursion
    (let ((case-fold-search t))
      (vhdl-ext-syntax-table
       (or (string-match "arglist"
			 (format "%s" (car (car (vhdl-get-syntactic-context)))))
	   (progn (beginning-of-line)
		  (looking-at "^\\s-*\\(generic\\|port\\|\\(\\(impure\\|pure\\)\\s-+\\|\\)function\\|procedure\\)\\>\\s-*\\(\\w+\\s-*\\)?(")
		  ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abbrev hooks

(defun vhdl-hooked-abbrev (func)
  "Do function, if syntax says abbrev is a keyword, invoked by hooked abbrev,
but not if inside a comment or quote)."
  (if (or (vhdl-in-comment-p)
	  (vhdl-in-string-p)
	  (save-excursion
	    (forward-word -1)
	    (and (looking-at "\\<end\\>") (not (looking-at "\\<end;")))))
      (progn
	(insert " ")
	(unexpand-abbrev)
	(delete-char -1))
    (if (not vhdl-electric-mode)
	(progn
	  (insert " ")
	  (unexpand-abbrev)
	  (backward-word 1)
	  (vhdl-case-word 1)
	  (delete-char 1))
      (let ((invoke-char last-command-char)
	    (abbrev-mode -1)
	    (vhdl-template-invoked-by-hook t))
	(let ((caught (catch 'abort
			(funcall func))))
	  (when (stringp caught) (message caught)))
	(when (= invoke-char ?-) (setq abbrev-start-location (point)))
	;; delete CR which is still in event queue
	(if (string-match "XEmacs" emacs-version)
	    (enqueue-eval-event 'delete-char -1)
	  (setq unread-command-events	; push back a delete char
		(list (vhdl-character-to-event ?\177))))))))

(defun vhdl-template-alias-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-alias))
(defun vhdl-template-architecture-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-architecture))
(defun vhdl-template-assert-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-assert))
(defun vhdl-template-attribute-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-attribute))
(defun vhdl-template-block-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-block))
(defun vhdl-template-break-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-break))
(defun vhdl-template-case-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-case))
(defun vhdl-template-component-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-component))
(defun vhdl-template-instance-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-instance))
(defun vhdl-template-conditional-signal-asst-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-conditional-signal-asst))
(defun vhdl-template-configuration-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-configuration))
(defun vhdl-template-constant-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-constant))
(defun vhdl-template-disconnect-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-disconnect))
(defun vhdl-template-display-comment-hook ()
  (vhdl-hooked-abbrev 'vhdl-comment-display))
(defun vhdl-template-else-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-else))
(defun vhdl-template-elsif-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-elsif))
(defun vhdl-template-entity-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-entity))
(defun vhdl-template-exit-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-exit))
(defun vhdl-template-file-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-file))
(defun vhdl-template-for-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-for))
(defun vhdl-template-function-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-function))
(defun vhdl-template-generic-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-generic))
(defun vhdl-template-group-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-group))
(defun vhdl-template-library-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-library))
(defun vhdl-template-limit-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-limit))
(defun vhdl-template-if-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-if))
(defun vhdl-template-bare-loop-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-bare-loop))
(defun vhdl-template-map-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-map))
(defun vhdl-template-nature-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-nature))
(defun vhdl-template-next-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-next))
(defun vhdl-template-package-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-package))
(defun vhdl-template-port-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-port))
(defun vhdl-template-procedural-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-procedural))
(defun vhdl-template-procedure-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-procedure))
(defun vhdl-template-process-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-process))
(defun vhdl-template-quantity-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-quantity))
(defun vhdl-template-report-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-report))
(defun vhdl-template-return-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-return))
(defun vhdl-template-selected-signal-asst-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-selected-signal-asst))
(defun vhdl-template-signal-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-signal))
(defun vhdl-template-subnature-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-subnature))
(defun vhdl-template-subtype-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-subtype))
(defun vhdl-template-terminal-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-terminal))
(defun vhdl-template-type-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-type))
(defun vhdl-template-use-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-use))
(defun vhdl-template-variable-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-variable))
(defun vhdl-template-wait-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-wait))
(defun vhdl-template-when-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-when))
(defun vhdl-template-while-loop-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-while-loop))
(defun vhdl-template-with-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-with))
(defun vhdl-template-and-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-and))
(defun vhdl-template-or-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-or))
(defun vhdl-template-nand-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-nand))
(defun vhdl-template-nor-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-nor))
(defun vhdl-template-xor-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-xor))
(defun vhdl-template-xnor-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-xnor))
(defun vhdl-template-not-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-not))

(defun vhdl-template-default-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-default))
(defun vhdl-template-default-indent-hook ()
  (vhdl-hooked-abbrev 'vhdl-template-default-indent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Template insertion from completion list

(defun vhdl-template-insert-construct (name)
  "Insert the built-in construct template with NAME."
  (interactive
   (list (let ((completion-ignore-case t))
	   (completing-read "Construct name: "
			    vhdl-template-construct-alist nil t))))
  (vhdl-template-insert-fun
   (car (cdr (assoc name vhdl-template-construct-alist)))))

(defun vhdl-template-insert-package (name)
  "Insert the built-in package template with NAME."
  (interactive
   (list (let ((completion-ignore-case t))
	   (completing-read "Package name: "
			    vhdl-template-package-alist nil t))))
  (vhdl-template-insert-fun
   (car (cdr (assoc name vhdl-template-package-alist)))))

(defun vhdl-template-insert-directive (name)
  "Insert the built-in directive template with NAME."
  (interactive
   (list (let ((completion-ignore-case t))
	   (completing-read "Directive name: "
			    vhdl-template-directive-alist nil t))))
  (vhdl-template-insert-fun
   (car (cdr (assoc name vhdl-template-directive-alist)))))

(defun vhdl-template-insert-fun (fun)
  "Call FUN to insert a built-in template."
  (let ((caught (catch 'abort (when fun (funcall fun)))))
    (when (stringp caught) (message caught))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Models
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vhdl-model-insert (model-name)
  "Insert the user model with name MODEL-NAME."
  (interactive
   (let ((completion-ignore-case t))
     (list (completing-read "Model name: " vhdl-model-alist))))
  (vhdl-indent-line)
  (let ((start (point-marker))
	(margin (current-indentation))
	(case-fold-search t)
	model position prompt string end)
    (vhdl-ext-syntax-table
     (when (setq model (assoc model-name vhdl-model-alist))
       ;; insert model
       (beginning-of-line)
       (delete-horizontal-space)
       (goto-char start)
       (vhdl-insert-string-or-file (nth 1 model))
       (setq end (point-marker))
       ;; indent code
       (goto-char start)
       (beginning-of-line)
       (while (< (point) end)
	 (unless (looking-at "^$")
	   (insert-char ?  margin))
	 (beginning-of-line 2))
       (goto-char start)
       ;; insert clock
       (unless (equal "" vhdl-clock-name)
	 (while (re-search-forward "<clock>" end t)
	   (replace-match vhdl-clock-name)))
       (goto-char start)
       ;; insert reset
       (unless (equal "" vhdl-reset-name)
	 (while (re-search-forward "<reset>" end t)
	   (replace-match vhdl-reset-name)))
       (goto-char start)
       ;; query prompts
       (while (re-search-forward
	       (concat "<\\(" vhdl-template-prompt-syntax "\\)>") end t)
	 (unless (equal "cursor" (match-string 1))
	   (setq position (match-beginning 1))
	   (setq prompt (match-string 1))
	   (replace-match "")
	   (setq string (vhdl-template-field prompt nil t))
	   ;; replace occurences of same prompt
	   (while (re-search-forward (concat "<\\(" prompt "\\)>") end t)
	     (replace-match (or string "")))
	   (goto-char position)))
       (goto-char start)
       ;; goto final position
       (if (re-search-forward "<cursor>" end t)
	   (replace-match "")
	 (goto-char end))))))

(defun vhdl-model-defun ()
  "Define help and hook functions for user models."
  (let ((model-alist vhdl-model-alist)
	model-name model-keyword)
    (while model-alist
      ;; define functions for user models that can be invoked from menu and key
      ;; bindings and which themselves call `vhdl-model-insert' with the model
      ;; name as argument
      (setq model-name (nth 0 (car model-alist)))
      (eval `(defun ,(vhdl-function-name "vhdl-model" model-name) ()
	       ,(concat "Insert model for \"" model-name "\".")
	       (interactive)
	       (vhdl-model-insert ,model-name)))
      ;; define hooks for user models that are invoked from keyword abbrevs
      (setq model-keyword (nth 3 (car model-alist)))
      (unless (equal model-keyword "")
	(eval `(defun
		 ,(vhdl-function-name
		   "vhdl-model" model-name "hook") ()
		 (vhdl-hooked-abbrev
		  ',(vhdl-function-name "vhdl-model" model-name)))))
      (setq model-alist (cdr model-alist)))))

(vhdl-model-defun)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Port translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vhdl-port-list nil
  "Variable to hold last PORT map parsed.")
;; structure: (parenthesised expression means list of such entries)
;; ((generic-names) generic-type generic-init generic-comment)
;; ((port-names) port-object port-direct port-type port-comment)

(defun vhdl-parse-string (string &optional optional)
  "Check that the text following point matches the regexp in STRING.
END is the point beyond which matching/searching should not go."
  (if (looking-at string)
      (re-search-forward string nil t)
    (unless optional
      (throw 'parse (format "Syntax error near line %s" (vhdl-current-line))))
    nil))

(defun vhdl-replace-string (regexp-cons string)
  "Replace STRING from car of REGEXP-CONS to cdr of REGEXP-CONS."
  (vhdl-ext-syntax-table
   (if (string-match (car regexp-cons) string)
       (replace-match (cdr regexp-cons) t nil string)
     string)))

(defun vhdl-port-flatten ()
  "Flatten port list so that only one generic/port exists per line."
  (interactive)
  (if (not vhdl-port-list)
      (error "No port read")
    (message "Flattening port...")
    (let ((new-vhdl-port-list (list (car vhdl-port-list)))
	  (old-vhdl-port-list (cdr vhdl-port-list))
	  old-port-list new-port-list old-port new-port names)
      ;; traverse port list and flatten entries
      (while old-vhdl-port-list
	(setq old-port-list (car old-vhdl-port-list))
	(setq new-port-list nil)
	(while old-port-list
	  (setq old-port (car old-port-list))
	  (setq names (car old-port))
	  (while names
	    (setq new-port (cons (list (car names)) (cdr old-port)))
	    (setq new-port-list (append new-port-list (list new-port)))
	    (setq names (cdr names)))
	  (setq old-port-list (cdr old-port-list)))
	(setq old-vhdl-port-list (cdr old-vhdl-port-list))
	(setq new-vhdl-port-list (append new-vhdl-port-list
					 (list new-port-list))))
      (setq vhdl-port-list new-vhdl-port-list)
      (message "Flattening port...done"))))

(defun vhdl-port-copy ()
  "Get generic and port information from an entity or component declaration."
  (interactive)
  (message "Reading port...")
  (save-excursion
    (let ((case-fold-search t)
	  parse-error end-of-list
	  name generics ports
	  object names direct type init comment)
      (vhdl-ext-syntax-table
       (setq
	parse-error
	(catch 'parse
	  ;; check if within entity or component declaration
	  (when (or (not (re-search-backward
			  "^\\s-*\\(component\\|entity\\|end\\)\\>" nil t))
		    (equal "end" (match-string 1)))
	    (throw 'parse "Not within entity or component declaration"))
	  (forward-word 1)
	  (vhdl-parse-string "\\s-*\\(\\w+\\)\\s-*\\(is\\)?\\s-*$")
	  (setq name (match-string 1))
	  (vhdl-forward-syntactic-ws)
	  ;; parse generic clause
	  (when (vhdl-parse-string "generic[ \t\n]*(" t)
	    (vhdl-forward-syntactic-ws)
	    (setq end-of-list (looking-at ")"))
	    (while (not end-of-list)
	      ;; parse names
	      (vhdl-parse-string "\\(\\w+\\)[ \t\n]*")
	      (setq names (list (match-string 1)))
	      (while (vhdl-parse-string ",[ \t\n]*\\(\\w+\\)[ \t\n]*" t)
		(setq names (append names (list (match-string 1)))))
	      ;; parse type
	      (vhdl-parse-string ":[ \t\n]*\\([^():;\n]+\\)")
	      (setq type (match-string 1))
	      (setq comment nil)
	      (while (looking-at "(")
		(setq type
		      (concat type
			      (buffer-substring
			       (point) (progn (forward-sexp) (point)))
			      (and (vhdl-parse-string "\\([^():;\n]*\\)" t)
				   (match-string 1)))))
	      ;; special case: closing parenthesis is on separate line
	      (when (and type (string-match "\\(\\s-*--\\s-*\\)\\(.*\\)" type))
		(setq comment (substring type (match-beginning 2)))
		(setq type (substring type 0 (match-beginning 1))))
	      ;; strip of trailing whitespace
	      (string-match "\\(\\(\\s-*\\S-+\\)+\\)\\s-*" type)
	      (setq type (substring type 0 (match-end 1)))
	      ;; parse initialization expression
	      (setq init nil)
	      (when (vhdl-parse-string ":=[ \t\n]*" t)
		(vhdl-parse-string "\\([^();\n]*\\)")
		(setq init (match-string 1))
		(while (looking-at "(")
		  (setq init
			(concat init
				(buffer-substring
				 (point) (progn (forward-sexp) (point)))
				(and (vhdl-parse-string "\\([^();\n]*\\)" t)
				     (match-string 1))))))
	      ;; special case: closing parenthesis is on separate line
	      (when (and init (string-match "\\(\\s-*--\\s-*\\)\\(.*\\)" init))
		(setq comment (substring init (match-beginning 2)))
		(setq init (substring init 0 (match-beginning 1)))
		(vhdl-forward-syntactic-ws))
	      (skip-chars-forward " \t")
	      ;; parse inline comment, special case: as above, no initial.
	      (unless comment
		(setq comment (and (vhdl-parse-string "--\\s-*\\([^\n]*\\)" t)
				   (match-string 1))))
	      (vhdl-forward-syntactic-ws)
	      (setq end-of-list (vhdl-parse-string ")" t))
	      (vhdl-parse-string ";\\s-*")
	      ;; parse inline comment
	      (unless comment
		(setq comment (and (vhdl-parse-string "--\\s-*\\([^\n]*\\)" t)
				   (match-string 1))))
	      (vhdl-forward-syntactic-ws)
	      ;; save everything in list
	      (setq generics (append generics
				     (list (list names type init comment))))))
	  ;; parse port clause
	  (when (vhdl-parse-string "port[ \t\n]*(" t)
	    (vhdl-forward-syntactic-ws)
	    (setq end-of-list (looking-at ")"))
	    (while (not end-of-list)
	      ;; parse object
	      (setq object
		    (and (vhdl-parse-string
			  "\\(signal\\|quantity\\|terminal\\)[ \t\n]*" t)
			 (match-string 1)))
	      ;; parse names
	      (vhdl-parse-string "\\(\\w+\\)[ \t\n]*")
	      (setq names (list (match-string 1)))
	      (while (vhdl-parse-string ",[ \t\n]*\\(\\w+\\)[ \t\n]*" t)
		(setq names (append names (list (match-string 1)))))
	      ;; parse direction
	      (vhdl-parse-string ":[ \t\n]*")
	      (setq direct
		    (and (vhdl-parse-string "\\(IN\\|OUT\\|INOUT\\)[ \t\n]+" t)
			 (match-string 1)))
	      ;; parse type
	      (vhdl-parse-string "\\([^();\n]+\\)")
	      (setq type (match-string 1))
	      (setq comment nil)
	      (while (looking-at "(")
		(setq type (concat type
				   (buffer-substring
				    (point) (progn (forward-sexp) (point)))
				   (and (vhdl-parse-string "\\([^();\n]*\\)" t)
					(match-string 1)))))
	      ;; special case: closing parenthesis is on separate line
	      (when (string-match "\\(\\s-*--\\s-*\\)\\(.*\\)" type)
		(setq comment (substring type (match-beginning 2)))
		(setq type (substring type 0 (match-beginning 1))))
	      ;; strip of trailing whitespace
	      (string-match "\\(\\(\\s-*\\S-+\\)+\\)\\s-*" type)
	      (setq type (substring type 0 (match-end 1)))
	      (vhdl-forward-syntactic-ws)
	      (setq end-of-list (vhdl-parse-string ")" t))
	      (vhdl-parse-string ";\\s-*")
	      ;; parse inline comment
	      (unless comment
		(setq comment (and (vhdl-parse-string "--\\s-*\\([^\n]*\\)" t)
				   (match-string 1))))
	      (vhdl-forward-syntactic-ws)
	      ;; save everything in list
	      (setq ports
		    (append ports
			    (list (list names object direct type comment))))))
	  nil)))
      ;; finish parsing
      (if parse-error
	  (error parse-error)
	(setq vhdl-port-list (list name generics ports))
	(message "Reading port...done")))))

(defun vhdl-port-paste-generic (&optional no-init)
  "Paste a generic clause."
  (let ((margin (current-indentation))
	list-margin start names generic
	(generics-list (nth 1 vhdl-port-list)))
    ;; paste generic clause
    (when generics-list
      (setq start (point))
      (vhdl-insert-keyword "GENERIC (")
      (unless vhdl-argument-list-indent
	(insert "\n") (indent-to (+ margin vhdl-basic-offset)))
      (setq list-margin (current-column))
      (while generics-list
	;; paste names
	(setq generic (car generics-list))
	(setq names (nth 0 generic))
	(while names
	  (insert (car names))
	  (setq names (cdr names))
	  (when names (insert ", ")))
	;; paste type
	(insert " : " (nth 1 generic))
	;; paste initialization
	(when (and (not no-init) (nth 2 generic))
	  (insert " := " (nth 2 generic)))
	(unless (cdr generics-list) (insert ")"))
	(insert ";")
	;; paste comment
	(when (and vhdl-include-port-comments (nth 3 generic))
	  (vhdl-comment-insert-inline (nth 3 generic) t))
	(setq generics-list (cdr generics-list))
	(when generics-list (insert "\n") (indent-to list-margin)))
      ;; align generic clause
      (when vhdl-auto-align (vhdl-align-noindent-region start (point) 1 t)))))

(defun vhdl-port-paste-port ()
  "Paste a port clause."
  (let ((margin (current-indentation))
	list-margin start names port
	(ports-list (nth 2 vhdl-port-list)))
    ;; paste port clause
    (when ports-list
      (setq start (point))
      (vhdl-insert-keyword "PORT (")
      (unless vhdl-argument-list-indent
	(insert "\n") (indent-to (+ margin vhdl-basic-offset)))
      (setq list-margin (current-column))
      (while ports-list
	(setq port (car ports-list))
	;; paste object
	(when (nth 1 port) (insert (nth 1 port) " "))
	;; paste names
	(setq names (nth 0 port))
	(while names
	  (insert (car names))
	  (setq names (cdr names))
	  (when names (insert ", ")))
	;; paste direction
	(insert " : ")
	(when (nth 2 port) (insert (nth 2 port) " "))
	;; paste type
	(insert (nth 3 port))
	(unless (cdr ports-list) (insert ")"))
	(insert ";")
	;; paste comment
	(when (and vhdl-include-port-comments (nth 4 port))
	  (vhdl-comment-insert-inline (nth 4 port) t))
	(setq ports-list (cdr ports-list))
	(when ports-list (insert "\n") (indent-to list-margin)))
      ;; align port clause
      (when vhdl-auto-align (vhdl-align-noindent-region start (point) 1)))))

(defun vhdl-port-paste-declaration (kind)
  "Paste as an entity or component declaration."
  (vhdl-indent-line)
  (let ((margin (current-indentation))
	(name (nth 0 vhdl-port-list)))
    (vhdl-insert-keyword (if (eq kind 'entity) "ENTITY " "COMPONENT "))
    (insert name)
    (if (eq kind 'entity) (vhdl-insert-keyword " IS"))
      ;; paste generic and port clause
    (when (nth 1 vhdl-port-list)
      (insert "\n")
      (when (and (memq vhdl-insert-empty-lines '(unit all)) (eq kind 'entity))
	(insert "\n"))
      (indent-to (+ margin vhdl-basic-offset))
      (vhdl-port-paste-generic (eq kind 'component)))
    (when (nth 2 vhdl-port-list)
      (insert "\n")
      (when (and (memq vhdl-insert-empty-lines '(unit all))
		 (eq kind 'entity))
	(insert "\n"))
      (indent-to (+ margin vhdl-basic-offset)))
    (vhdl-port-paste-port)
    (insert "\n")
    (when (and (memq vhdl-insert-empty-lines '(unit all)) (eq kind 'entity))
      (insert "\n"))
    (indent-to margin)
    (vhdl-insert-keyword "END")
    (if (eq kind 'entity)
	(progn
	  (unless (vhdl-standard-p '87) (vhdl-insert-keyword " ENTITY"))
	  (insert " " name))
      (vhdl-insert-keyword " COMPONENT")
      (unless (vhdl-standard-p '87) (insert " " name)))
    (insert ";")))

(defun vhdl-port-paste-entity ()
  "Paste as an entity declaration."
  (interactive)
  (if (not vhdl-port-list)
      (error "No port read")
    (message "Pasting port as entity...")
    (vhdl-port-paste-declaration 'entity)
    (message "Pasting port as entity...done")))

(defun vhdl-port-paste-component ()
  "Paste as a component declaration."
  (interactive)
  (if (not vhdl-port-list)
      (error "No port read")
    (message "Pasting port as component...")
    (vhdl-port-paste-declaration 'component)
    (message "Pasting port as component...done")))

(defun vhdl-port-paste-generic-map (&optional secondary no-constants)
  "Paste as a generic map."
  (interactive)
  (unless secondary (vhdl-indent-line))
  (let ((margin (current-indentation))
	list-margin start generic
	(generics-list (nth 1 vhdl-port-list)))
    (when generics-list
      (setq start (point))
      (vhdl-insert-keyword "GENERIC MAP (")
      (if (not vhdl-association-list-with-formals)
	  ;; paste list of actual generics
	  (while generics-list
	    (insert (or (nth 2 (car generics-list)) " "))
	    (setq generics-list (cdr generics-list))
	    (insert (if generics-list ", " ")")))
	(unless vhdl-argument-list-indent
	  (insert "\n") (indent-to (+ margin (* 2 vhdl-basic-offset))))
	(setq list-margin (current-column))
	(while generics-list
	  (setq generic (car generics-list))
	  ;; paste formal and actual generic
	  (insert (car (nth 0 generic)) " => "
		  (if no-constants
		      (car (nth 0 generic))
		    (or (nth 2 generic) "")))
	  (setq generics-list (cdr generics-list))
	  (insert (if generics-list "," ")"))
	  ;; paste comment
	  (when (and vhdl-include-port-comments (nth 3 generic))
	    (vhdl-comment-insert-inline (nth 3 generic) t))
	  (when generics-list (insert "\n") (indent-to list-margin)))
	;; align generic map
	(when vhdl-auto-align
	  (vhdl-align-noindent-region start (point) 1 t))))))

(defun vhdl-port-paste-port-map ()
  "Paste as a port map."
  (let ((margin (current-indentation))
	list-margin start port
	(ports-list (nth 2 vhdl-port-list)))
    (when ports-list
      (setq start (point))
      (vhdl-insert-keyword "PORT MAP (")
      (if (not vhdl-association-list-with-formals)
	  ;; paste list of actual ports
	  (while ports-list
	    (insert (vhdl-replace-string vhdl-actual-port-name
					 (car (nth 0 (car ports-list)))))
	    (setq ports-list (cdr ports-list))
	    (insert (if ports-list ", " ");")))
	(unless vhdl-argument-list-indent
	  (insert "\n") (indent-to (+ margin (* 2 vhdl-basic-offset))))
	(setq list-margin (current-column))
	(while ports-list
	  (setq port (car ports-list))
	  ;; paste formal and actual port
	  (insert (car (nth 0 port)) " => ")
	  (insert (vhdl-replace-string vhdl-actual-port-name
				       (car (nth 0 port))))
	  (setq ports-list (cdr ports-list))
	  (insert (if ports-list "," ");"))
	  ;; paste comment
	  (when (or vhdl-include-direction-comments
		    (and vhdl-include-port-comments (nth 4 port)))
	    (vhdl-comment-insert-inline
	     (concat
	      (if vhdl-include-direction-comments
		  (format "%-4s" (or (concat (nth 2 port) " ") "")) "")
	      (if vhdl-include-port-comments (nth 4 port) "")) t))
	  (when ports-list (insert "\n") (indent-to list-margin)))
	;; align port clause
	(when vhdl-auto-align
	  (vhdl-align-noindent-region start (point) 1))))))

(defun vhdl-port-paste-instance (&optional name)
  "Paste as an instantiation."
  (interactive)
  (if (not vhdl-port-list)
      (error "No port read")
    (let ((orig-vhdl-port-list vhdl-port-list))
      ;; flatten local copy of port list (must be flat for port mapping)
      (vhdl-port-flatten)
      (vhdl-indent-line)
      (let ((margin (current-indentation))
	    list-margin start generic port
	    (generics-list (nth 1 vhdl-port-list))
	    (ports-list (nth 2 vhdl-port-list)))
	;; paste instantiation
	(if name
	    (insert name ": ")
	  (if (equal (cdr vhdl-instance-name) "")
	      (vhdl-template-field "instance name" ": ")
	    (insert (vhdl-replace-string vhdl-instance-name
					 (nth 0 vhdl-port-list)) ": ")))
	(message "Pasting port as instantiation...")
	(if (vhdl-standard-p '87)
	    (insert (nth 0 vhdl-port-list))
	  (vhdl-insert-keyword "ENTITY ")
	  (insert "work." (nth 0 vhdl-port-list)))
	(when (nth 1 vhdl-port-list)
	  (insert "\n") (indent-to (+ margin vhdl-basic-offset))
	  (vhdl-port-paste-generic-map t t))
	(when (nth 2 vhdl-port-list)
	  (insert "\n") (indent-to (+ margin vhdl-basic-offset))
	  (vhdl-port-paste-port-map))
	(message "Pasting port as instantiation...done"))
      (setq vhdl-port-list orig-vhdl-port-list))))

(defun vhdl-port-paste-signals (&optional initialize)
  "Paste ports as internal signals."
  (interactive)
  (if (not vhdl-port-list)
      (error "No port read")
    (message "Pasting port as signals...")
    (vhdl-indent-line)
    (let ((margin (current-indentation))
	  start port names
	  (ports-list (nth 2 vhdl-port-list)))
      (when ports-list
	(setq start (point))
	(while ports-list
	  (setq port (car ports-list))
	  ;; paste object
	  (if (nth 1 port)
	      (insert (nth 1 port) " ")
	    (vhdl-insert-keyword "SIGNAL "))
	  ;; paste actual port signals
	  (setq names (nth 0 port))
	  (while names
	    (insert (vhdl-replace-string vhdl-actual-port-name (car names)))
	    (setq names (cdr names))
	    (when names (insert ", ")))
	  ;; paste type
	  (insert " : " (nth 3 port))
	  ;; paste initialization (inputs only)
	  (when (and initialize (equal "in" (nth 2 port)))
	    (insert
	     " := "
	     (if (string-match "(.+)" (nth 3 port)) "(others => '0')" "'0'")))
	  (insert ";")
	  ;; paste comment
	  (when (and vhdl-include-port-comments (nth 4 port))
	    (vhdl-comment-insert-inline (nth 4 port) t))
	  (setq ports-list (cdr ports-list))
	  (when ports-list (insert "\n") (indent-to margin)))
	;; align signal list
	(when vhdl-auto-align (vhdl-align-noindent-region start (point) 1))))
    (message "Pasting port as signals...done")))

(defun vhdl-port-paste-constants ()
  "Paste generics as constants."
  (interactive)
  (if (not vhdl-port-list)
      (error "No port read")
    (let ((orig-vhdl-port-list vhdl-port-list))
      (message "Pasting port as constants...")
      ;; flatten local copy of port list (must be flat for constant initial.)
      (vhdl-port-flatten)
      (vhdl-indent-line)
      (let ((margin (current-indentation))
	    start generic name
	    (generics-list (nth 1 vhdl-port-list)))
	(when generics-list
	  (setq start (point))
	  (while generics-list
	    (setq generic (car generics-list))
	    (vhdl-insert-keyword "CONSTANT ")
	    ;; paste generic constants
	    (setq name (nth 0 generic))
	    (when name
	      (insert (car name))
	      ;; paste type
	      (insert " : " (nth 1 generic))
	      ;; paste initialization
	      (when (nth 2 generic)
		(insert " := " (nth 2 generic)))
	      (insert ";")
	      ;; paste comment
	      (when (and vhdl-include-port-comments (nth 3 generic))
		(vhdl-comment-insert-inline (nth 3 generic) t))
	      (setq generics-list (cdr generics-list))
	      (when generics-list (insert "\n") (indent-to margin))))
	    ;; align signal list
	  (when vhdl-auto-align
	    (vhdl-align-noindent-region start (point) 1))))
      (message "Pasting port as constants...done")
      (setq vhdl-port-list orig-vhdl-port-list))))

(defun vhdl-port-paste-testbench ()
  "Paste as a bare-bones test bench."
  (interactive)
  (if (not vhdl-port-list)
      (error "No port read")
    (message "Pasting port as test bench...")
    (let ((case-fold-search t)
	  (ent-name (vhdl-replace-string vhdl-testbench-entity-name
					 (nth 0 vhdl-port-list)))
	  (source-buffer (current-buffer))
	  arch-name ent-file-name arch-file-name no-entity position)
      ;; open entity file
      (when (not (eq vhdl-testbench-create-files 'none))
	(string-match "\\.[^.]*\\'" (buffer-file-name (current-buffer)))
	(setq ent-file-name
	      (concat ent-name
		      (substring (buffer-file-name (current-buffer))
				 (match-beginning 0))))
	(when (file-exists-p ent-file-name)
	  (if (y-or-n-p
	       (concat "File `" ent-file-name "' exists; overwrite? "))
	      (progn (delete-file ent-file-name)
		     (when (get-file-buffer ent-file-name)
		       (set-buffer ent-file-name)
                       (set-buffer-modified-p nil)
                       (kill-buffer ent-file-name)))
	    (if (eq vhdl-testbench-create-files 'separate)
		(setq no-entity t)
	      (error "Pasting port as test bench...aborted"))))
	(unless no-entity
	  (set-buffer source-buffer)
	  (find-file ent-file-name)))
      (let ((margin 0))
	(unless (and (eq vhdl-testbench-create-files 'separate) no-entity)
	  ;; paste entity header
	  (unless (equal "" vhdl-testbench-entity-header)
	    (vhdl-insert-string-or-file vhdl-testbench-entity-header))
	  (vhdl-comment-display-line) (insert "\n\n") (indent-to margin)
	  ;; paste std_logic_1164 package
	  (vhdl-insert-keyword "LIBRARY ")
	  (insert "ieee;\n") (indent-to margin)
	  (vhdl-insert-keyword "USE ")
	  (insert "ieee.std_logic_1164.")
	  (vhdl-insert-keyword "ALL;")
	  (insert "\n\n") (indent-to margin) (vhdl-comment-display-line)
	  (insert "\n\n") (indent-to margin)
	  ;; paste entity declaration
	  (vhdl-insert-keyword "ENTITY ")
	  (insert ent-name)
	  (vhdl-insert-keyword " IS")
	  (when (memq vhdl-insert-empty-lines '(unit all)) (insert "\n"))
	  (insert "\n") (indent-to margin)
	  (vhdl-insert-keyword "END ")
	  (unless (vhdl-standard-p '87) (vhdl-insert-keyword "ENTITY "))
	  (insert ent-name ";")
	  (insert "\n\n") (indent-to margin)
	  (vhdl-comment-display-line) (insert "\n"))
	;; get architecture name
	(setq arch-name
	      (if (equal (cdr vhdl-testbench-architecture-name) "")
		  (read-from-minibuffer "architecture name: "
					nil vhdl-minibuffer-local-map)
		(vhdl-replace-string vhdl-testbench-architecture-name
				     (nth 0 vhdl-port-list))))
	;; open architecture file
	(when (eq vhdl-testbench-create-files 'separate)
	  (save-buffer)
	  (string-match "\\.[^.]*\\'" (buffer-file-name (current-buffer)))
	  (setq arch-file-name
		(concat arch-name
			(substring (buffer-file-name (current-buffer))
				   (match-beginning 0))))
	  (when (file-exists-p arch-file-name)
	    (if (y-or-n-p
		 (concat "File `" ent-file-name "' exists; overwrite? "))
		(progn (delete-file arch-file-name)
		       (when (get-file-buffer arch-file-name)
			 (set-buffer (get-file-buffer arch-file-name))
			 (set-buffer-modified-p nil)
			 (kill-buffer arch-file-name)))
	      (error "Pasting port as test bench...aborted")))
	  (set-buffer source-buffer)
	  (find-file arch-file-name)
	  ;; paste architecture header
	  (unless (equal "" vhdl-testbench-architecture-header)
	    (vhdl-insert-string-or-file vhdl-testbench-architecture-header))
	  (vhdl-comment-display-line)
	  (insert "\n"))
	(insert "\n") (indent-to margin)
	;; paste architecture body
	(vhdl-insert-keyword "ARCHITECTURE ")
	(insert arch-name)
	(vhdl-insert-keyword " OF ")
	(insert ent-name)
	(vhdl-insert-keyword " IS")
	(insert "\n\n") (indent-to margin)
	;; paste component declaration
	(when (vhdl-standard-p '87)
	  (vhdl-port-paste-component)
	  (insert "\n\n") (indent-to margin))
	;; paste constants
	(when (nth 1 vhdl-port-list)
	  (vhdl-port-paste-constants)
	  (insert "\n\n") (indent-to margin))
	;; paste internal signals
	(vhdl-port-paste-signals vhdl-testbench-initialize-signals)
	;; paste custom declarations
	(unless (equal "" vhdl-testbench-declarations)
	  (insert "\n\n")
	  (vhdl-insert-string-or-file vhdl-testbench-declarations)
	  (delete-indentation))
	(setq position (point))
	(insert "\n\n") (indent-to margin)
	(vhdl-comment-display-line) (insert "\n")
	(goto-char position)
	(vhdl-template-begin-end
	 (unless (vhdl-standard-p '87) "ARCHITECTURE")
	 arch-name margin t)
	;; paste instantiation
	(vhdl-port-paste-instance
	 (vhdl-replace-string vhdl-testbench-dut-name
			      (nth 0 vhdl-port-list)))
	(insert "\n")
	;; paste custom statements
	(unless (equal "" vhdl-testbench-statements)
	  (insert "\n")
	  (vhdl-insert-string-or-file vhdl-testbench-statements))
	(insert "\n")
	(indent-to (+ margin vhdl-basic-offset))
	(when (not (eq vhdl-testbench-create-files 'none))
	  (save-buffer))
	(message "Pasting port as test bench...done")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hippie expand customization

(defvar vhdl-expand-upper-case nil)

(defun vhdl-try-expand-abbrev (old)
  "Try expanding abbreviations from `vhdl-abbrev-list'."
  (unless old
    (he-init-string (he-dabbrev-beg) (point))
    (setq he-expand-list
	  (let ((abbrev-list vhdl-abbrev-list)
		(sel-abbrev-list '()))
	    (while abbrev-list
	      (when (or (not (stringp (car abbrev-list)))
			(string-match
			 (concat "^" he-search-string) (car abbrev-list)))
		(setq sel-abbrev-list
		      (cons (car abbrev-list) sel-abbrev-list)))
	      (setq abbrev-list (cdr abbrev-list)))
	    (nreverse sel-abbrev-list))))
  (while (and he-expand-list
	      (or (not (stringp (car he-expand-list)))
		  (he-string-member (car he-expand-list) he-tried-table t)))
;		  (equal (car he-expand-list) he-search-string)))
    (unless (stringp (car he-expand-list))
      (setq vhdl-expand-upper-case (car he-expand-list)))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn (when old (he-reset-string))
	     nil)
    (he-substitute-string
     (if vhdl-expand-upper-case
	 (upcase (car he-expand-list))
       (car he-expand-list))
     t)
    (setq he-expand-list (cdr he-expand-list))
    t))

(defun vhdl-he-list-beg ()
  "Also looks at the word before `(' in order to better match parenthesized
expressions (e.g. for index ranges of types and signals)."
  (save-excursion
    (condition-case ()
	(progn (backward-up-list 1)
 	       (skip-syntax-backward "w_")) ; crashes in `viper-mode'
      (error ()))
    (point)))

;; override `he-list-beg' from `hippie-exp'
(unless (and (boundp 'viper-mode) viper-mode)
 (require 'hippie-exp)
 (defalias 'he-list-beg 'vhdl-he-list-beg))

;; function for expanding abbrevs and dabbrevs
(fset 'vhdl-expand-abbrev (make-hippie-expand-function
			   '(try-expand-dabbrev
			     try-expand-dabbrev-all-buffers
			     vhdl-try-expand-abbrev)))

;; function for expanding parenthesis
(fset 'vhdl-expand-paren (make-hippie-expand-function
			  '(try-expand-list
			    try-expand-list-all-buffers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Case fixing

(defun vhdl-fix-case-region-1 (beg end upper-case word-regexp &optional count)
  "Convert all words matching word-regexp in region to lower or upper case,
depending on parameter upper-case."
  (let ((case-fold-search t)
	(case-replace nil)
	(last-update 0))
    (vhdl-ext-syntax-table
     (save-excursion
       (goto-char end)
       (setq end (point-marker))
       (goto-char beg)
       (while (re-search-forward word-regexp end t)
	 (or (vhdl-in-comment-p)
	     (vhdl-in-string-p)
	     (if upper-case
		 (upcase-word -1)
	       (downcase-word -1)))
	 (when (and count vhdl-progress-interval
		    (< vhdl-progress-interval
		       (- (nth 1 (current-time)) last-update)))
	   (message "Fixing case... (%2d%s)"
		    (+ (* count 25) (/ (* 25 (- (point) beg)) (- end beg)))
		    "%")
	   (setq last-update (nth 1 (current-time)))))
       (goto-char end)))
    (and count vhdl-progress-interval (message "Fixing case...done"))))

(defun vhdl-fix-case-region (beg end &optional arg)
  "Convert all VHDL words in region to lower or upper case, depending on
variables vhdl-upper-case-{keywords,types,attributes,enum-values}."
  (interactive "r\nP")
  (vhdl-fix-case-region-1
   beg end vhdl-upper-case-keywords vhdl-keywords-regexp 0)
  (vhdl-fix-case-region-1
   beg end vhdl-upper-case-types vhdl-types-regexp 1)
  (vhdl-fix-case-region-1
   beg end vhdl-upper-case-attributes (concat "'" vhdl-attributes-regexp) 2)
  (vhdl-fix-case-region-1
   beg end vhdl-upper-case-enum-values vhdl-enum-values-regexp 3))

(defun vhdl-fix-case-buffer ()
  "Convert all VHDL words in buffer to lower or upper case, depending on
variables vhdl-upper-case-{keywords,types,attributes,enum-values}."
  (interactive)
  (vhdl-fix-case-region (point-min) (point-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line handling functions

(defun vhdl-current-line ()
  "Return the line number of the line containing point."
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun vhdl-line-kill-entire (&optional arg)
  "Delete entire line."
  (interactive "p")
  (beginning-of-line)
  (kill-line (or arg 1)))

(defun vhdl-line-kill (&optional arg)
  "Kill current line."
  (interactive "p")
  (vhdl-line-kill-entire arg))

(defun vhdl-line-copy (&optional arg)
  "Copy current line."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (let ((position (point)))
      (forward-line (or arg 1))
      (copy-region-as-kill position (point)))))

(defun vhdl-line-yank ()
  "Yank entire line."
  (interactive)
  (beginning-of-line)
  (yank))

(defun vhdl-line-expand (&optional prefix-arg)
  "Hippie-expand current line."
  (interactive "P")
  (let ((case-fold-search t) (case-replace nil)
	(hippie-expand-try-functions-list
	 '(try-expand-line try-expand-line-all-buffers)))
    (hippie-expand prefix-arg)))

(defun vhdl-line-transpose-next (&optional arg)
  "Interchange this line with next line."
  (interactive "p")
  (forward-line 1)
  (transpose-lines (or arg 1))
  (forward-line -1))

(defun vhdl-line-transpose-previous (&optional arg)
  "Interchange this line with previous line."
  (interactive "p")
  (forward-line 1)
  (transpose-lines (- 0 (or arg 0)))
  (forward-line -1))

(defun vhdl-line-open ()
  "Open a new line and indent."
  (interactive)
  (end-of-line -0)
  (newline-and-indent))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vhdl-project-switch (name)
  "Switch to project NAME."
  (setq vhdl-project name)
  (when (and (boundp 'speedbar-frame) (frame-live-p speedbar-frame))
    (speedbar-refresh)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (using `compile.el')

(defun vhdl-compile-init ()
  "Initialize for compilation."
  (unless compilation-error-regexp-alist
    (setq compilation-error-regexp-alist
	  (let ((commands-alist vhdl-compiler-alist)
		regexp-alist sublist)
	    (while commands-alist
	      (setq sublist (nth 5 (car commands-alist)))
	      (unless (equal "" (car sublist))
		(setq regexp-alist
		      (cons (list (nth 0 sublist)
				  (if (= 0 (nth 1 sublist))
				      (if (string-match
					    "XEmacs" emacs-version) 9 nil)
				    (nth 1 sublist))
				  (nth 2 sublist))
			    regexp-alist)))
	      (setq commands-alist (cdr commands-alist)))
	    regexp-alist)))
  (unless compilation-file-regexp-alist
    (setq compilation-file-regexp-alist
	  (let ((commands-alist vhdl-compiler-alist)
		regexp-alist)
	    (while commands-alist
	      (unless (equal "" (car (nth 6 (car commands-alist))))
		(setq regexp-alist
		      (append regexp-alist
			      (list (nth 6 (car commands-alist))))))
	      (setq commands-alist (cdr commands-alist)))
	    regexp-alist))))

(defun vhdl-compile ()
  "Compile current buffer using the VHDL compiler specified in
`vhdl-compiler'."
  (interactive)
  (vhdl-compile-init)
  (let* ((command-elem (assoc vhdl-compiler vhdl-compiler-alist))
	 (command (nth 1 command-elem))
	 (default-directory (expand-file-name (nth 4 command-elem))))
    (when command
      (compile (concat command " " vhdl-compiler-options
		       (unless (string-equal vhdl-compiler-options "") " ")
		       (buffer-file-name))))))

(defun vhdl-make ()
  "Call make command for compilation of all updated source files (requires
`Makefile')."
  (interactive)
  (vhdl-compile-init)
  (let* ((command-elem (assoc vhdl-compiler vhdl-compiler-alist))
	 (command (nth 2 command-elem))
	 (default-directory (expand-file-name (nth 4 command-elem))))
    (if (equal command "")
	(compile "make")
      (compile command))))

(defun vhdl-generate-makefile ()
  "Generate new `Makefile'."
  (interactive)
  (vhdl-compile-init)
  (let* ((command-elem (assoc vhdl-compiler vhdl-compiler-alist))
	 (command (nth 3 command-elem))
	 (default-directory (expand-file-name (nth 4 command-elem))))
    (if (not (equal command ""))
	(compile command)
      (error "No such command specified for `%s'" vhdl-compiler))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hideshow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (using `hideshow.el')

(defun vhdl-forward-unit (&optional count)
  "Find begin and end of VHDL design units (for hideshow)."
  (interactive "p")
  (let ((case-fold-search t))
    (if (< count 0)
	(re-search-backward
	 "^\\(architecture\\|configuration\\|entity\\|package\\)\\>" nil t)
      (re-search-forward "^end\\>" nil t))))

(when (string-match "XEmacs" emacs-version)
  (require 'hideshow))

(unless (assq 'vhdl-mode hs-special-modes-alist)
  (setq hs-special-modes-alist
	(cons
	 '(vhdl-mode
	   "\\(^\\)\\(architecture\\|ARCHITECTURE\\|configuration\\|CONFIGURATION\\|entity\\|ENTITY\\|package\\|PACKAGE\\)\\>"
	   "\\(^\\)\\(end\\|END\\)\\>"
	   "--\\( \\|$\\)"
	   vhdl-forward-unit)
	 hs-special-modes-alist)))

(defun vhdl-hideshow-init ()
  "Initialize `hideshow'."
  (if vhdl-hide-all-init
      (add-hook 'hs-minor-mode-hook 'hs-hide-all)
    (remove-hook 'hs-minor-mode-hook 'hs-hide-all))
  (if vhdl-hideshow-menu
      (hs-minor-mode 1)
    (when (boundp 'hs-minor-mode) (hs-minor-mode 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Font locking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (using `font-lock.el')

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help functions for translate-off region highlighting

(defun vhdl-within-translate-off ()
  "Return point if within translate-off region, else nil."
  (and (save-excursion
	 (re-search-backward
	  "^\\s-*--\\s-*pragma\\s-*translate_\\(on\\|off\\)\\s-*\n" nil t))
       (equal "off" (match-string 1))
       (point)))

(defun vhdl-start-translate-off (limit)
  "Return point before translate-off pragma if before LIMIT, else nil."
  (when (re-search-forward
	 "^\\s-*--\\s-*pragma\\s-*translate_off\\s-*\n" limit t)
    (match-beginning 0)))

(defun vhdl-end-translate-off (limit)
  "Return point after translate-on pragma if before LIMIT, else nil."
  (re-search-forward "^\\s-*--\\s-*pragma\\s-*translate_on\\s-*\n" limit t))

(defun vhdl-match-translate-off (limit)
  "Match a translate-off block, setting match-data and returning t, else nil."
  (when (< (point) limit)
    (let ((start (or (vhdl-within-translate-off)
		     (vhdl-start-translate-off limit)))
	  (case-fold-search t))
      (when start
	(let ((end (or (vhdl-end-translate-off limit) limit)))
	  (set-match-data (list start end))
	  (goto-char end))))))

(defun vhdl-font-lock-match-item (limit)
  "Match, and move over, any declaration item after point. Adapted from
`font-lock-match-c-style-declaration-item-and-skip-to-next'."
  (condition-case nil
      (save-restriction
	(narrow-to-region (point-min) limit)
	;; match item
	(when (looking-at "\\s-*\\(\\w+\\)")
	  (save-match-data
	    (goto-char (match-end 1))
	    ;; move to next item
	    (if (looking-at "\\(\\s-*,\\)")
		(goto-char (match-end 1))
	      (end-of-line) t))))
    (error t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax definitions

(defconst vhdl-font-lock-syntactic-keywords
  '(("\\(\'\\).\\(\'\\)" (1 (7 . ?\')) (2 (7 . ?\'))))
  "Mark single quotes as having string quote syntax in 'c' instances.")

(defvar vhdl-font-lock-keywords nil
  "Regular expressions to highlight in VHDL Mode.")

(defconst vhdl-font-lock-keywords-0
  (list
   ;; highlight template prompts
   (list (concat "\\(<" vhdl-template-prompt-syntax ">\\)")
	 1 'vhdl-font-lock-prompt-face t)

   ;; highlight directives
   '("--\\s-*pragma\\s-+\\(.*\\)$" 1 vhdl-font-lock-directive-face t)
   )
  "For consideration as a value of `vhdl-font-lock-keywords'.
This does highlighting of template prompts and directives (pragmas).")

(defvar vhdl-font-lock-keywords-1 nil
  ;; set in `vhdl-font-lock-init' because dependent on custom variables
  "For consideration as a value of `vhdl-font-lock-keywords'.
This does highlighting of keywords and standard identifiers.")

(defconst vhdl-font-lock-keywords-2
  (list
   ;; highlight names of units, subprograms, and components when declared
   (list
    (concat
     "^\\s-*\\("
     "architecture\\|configuration\\|entity\\|package\\(\\s-+body\\|\\)\\|"
     "\\(\\(impure\\|pure\\)\\s-+\\|\\)function\\|procedure\\|component"
     "\\)\\s-+\\(\\w+\\)")
    5 'font-lock-function-name-face)

   ;; highlight entity names of architectures and configurations
   (list
    "^\\s-*\\(architecture\\|configuration\\)\\s-+\\w+\\s-+of\\s-+\\(\\w+\\)"
    2 'font-lock-function-name-face)

   ;; highlight labels of common constructs
   (list
    (concat
     "^\\s-*\\(\\w+\\)\\s-*:\\(\\s-\\|\n\\)*\\(\\("
     "assert\\|block\\|case\\|component\\|configuration\\|entity\\|exit\\|"
     "for\\|if\\|loop\\|next\\|null\\|postponed\\|process\\|"
     (when (vhdl-standard-p 'ams) "procedural\\|")
     "with\\|while"
     "\\)\\>\\|[^\n]*<=\\)")
    1 'font-lock-function-name-face)

   ;; highlight label and component name of component instantiations
   (list
    (concat
     "^\\s-*\\(\\w+\\)\\s-*:[ \t\n]*\\(component\\s-+\\|\\)\\(\\w+\\)"
     "\\(\\s-\\|\n\\)+\\(generic\\|port\\)\\s-+map\\>")
    '(1 font-lock-function-name-face) '(3 font-lock-function-name-face))

   ;; highlight names and labels at end of constructs
   (list
    (concat
     "^\\s-*end\\s-+\\(\\("
     "architecture\\|block\\|case\\|component\\|configuration\\|entity\\|"
     "for\\|function\\|generate\\|if\\|loop\\|package\\(\\s-+body\\|\\)\\|"
     "procedure\\|\\(postponed\\s-+\\|\\)process\\|"
     (when (vhdl-standard-p 'ams) "procedural\\|")
     "units"
     "\\)\\>\\|\\)\\s-*\\(\\w*\\)")
    5 'font-lock-function-name-face)

   ;; highlight labels in exit and next statements
   (list
    (concat
     "^\\s-*\\(\\w+\\s-*:\\s-*\\)?\\(exit\\|next\\)\\s-+\\(\\w*\\)")
    3 'font-lock-function-name-face)

   ;; highlight entity name in attribute specifications
   (list
    (concat
     "^\\s-*attribute\\s-+\\w+\\s-+of\\s-+\\(\\w+\\(,\\s-*\\w+\\)*\\)\\s-*:")
    1 'font-lock-function-name-face)

   ;; highlight labels in component specifications
   (list
    (concat
     "^\\s-*for\\s-+\\(\\w+\\(,\\s-*\\w+\\)*\\)\\s-*:"
     "\\(\\s-\\|\n\\)*\\(\\w+\\)")
    '(1 font-lock-function-name-face) '(4 font-lock-function-name-face))

   ;; highlight attribute name in attribute declarations/specifications
   (list
    (concat
     "^\\s-*attribute\\s-+\\(\\w+\\)")
    1 'vhdl-font-lock-attribute-face)

   ;; highlight type/nature name in (sub)type/(sub)nature declarations
   (list
    (concat
     "^\\s-*\\(sub\\|\\)\\(nature\\|type\\)\\s-+\\(\\w+\\)")
    3 'font-lock-type-face)

   ;; highlight signal/variable/constant declaration names
   (list "\\(:[^=]\\)"
	 '(vhdl-font-lock-match-item
	   (progn (goto-char (match-beginning 1))
		  (skip-syntax-backward " ")
		  (skip-syntax-backward "w_")
		  (skip-syntax-backward " ")
		  (while (= (preceding-char) ?,)
		    (backward-char 1)
		    (skip-syntax-backward " ")
		    (skip-syntax-backward "w_")
		    (skip-syntax-backward " ")))
;  		  (skip-chars-backward "^-(\n\";")
	   (goto-char (match-end 1)) (1 font-lock-variable-name-face)))

   ;; highlight alias/group declaration names and for-loop/-generate variables
   (list "\\<\\(alias\\|for\\|group\\)\\s-+\\w+\\s-+\\(in\\|is\\)\\>"
	 '(vhdl-font-lock-match-item
	   (progn (goto-char (match-end 1)) (match-beginning 2))
	   nil (1 font-lock-variable-name-face)))
   )
  "For consideration as a value of `vhdl-font-lock-keywords'.
This does context sensitive highlighting of names and labels.")

(defvar vhdl-font-lock-keywords-3 nil
  ;; set in `vhdl-font-lock-init' because dependent on custom variables
  "For consideration as a value of `vhdl-font-lock-keywords'.
This does highlighting of words with special syntax.")

(defvar vhdl-font-lock-keywords-4 nil
  ;; set in `vhdl-font-lock-init' because dependent on custom variables
  "For consideration as a value of `vhdl-font-lock-keywords'.
This does highlighting of additional reserved words.")

(defconst vhdl-font-lock-keywords-5
  ;; background highlight translate-off regions
  '((vhdl-match-translate-off (0 vhdl-font-lock-translate-off-face append)))
  "For consideration as a value of `vhdl-font-lock-keywords'.
This does background highlighting of translate-off regions.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font and color definitions

(defvar vhdl-font-lock-prompt-face         'vhdl-font-lock-prompt-face
  "Face name to use for prompts.")

(defvar vhdl-font-lock-attribute-face      'vhdl-font-lock-attribute-face
  "Face name to use for standardized attributes.")

(defvar vhdl-font-lock-enumvalue-face      'vhdl-font-lock-enumvalue-face
  "Face name to use for standardized enumeration values.")

(defvar vhdl-font-lock-function-face       'vhdl-font-lock-function-face
  "Face name to use for standardized functions and packages.")

(defvar vhdl-font-lock-directive-face      'vhdl-font-lock-directive-face
  "Face name to use for directives.")

(defvar vhdl-font-lock-reserved-words-face 'vhdl-font-lock-reserved-words-face
  "Face name to use for additional reserved words.")

(defvar vhdl-font-lock-translate-off-face  'vhdl-font-lock-translate-off-face
  "Face name to use for translate-off regions.")

;; face names to use for words with special syntax.
(let ((syntax-alist vhdl-special-syntax-alist)
      name)
  (while syntax-alist
    (setq name (vhdl-function-name
		"vhdl-font-lock" (nth 0 (car syntax-alist)) "face"))
    (eval `(defvar ,name ',name
	     ,(concat "Face name to use for "
		      (nth 0 (car syntax-alist)) ".")))
    (setq syntax-alist (cdr syntax-alist))))

(defgroup vhdl-highlight-faces nil
  "Faces for highlighting."
  :group 'vhdl-highlight)

;; add faces used from `font-lock'
(custom-add-to-group
 'vhdl-highlight-faces 'font-lock-comment-face 'custom-face)
(custom-add-to-group
 'vhdl-highlight-faces 'font-lock-string-face 'custom-face)
(custom-add-to-group
 'vhdl-highlight-faces 'font-lock-keyword-face 'custom-face)
(custom-add-to-group
 'vhdl-highlight-faces 'font-lock-type-face 'custom-face)
(custom-add-to-group
 'vhdl-highlight-faces 'font-lock-function-name-face 'custom-face)
(custom-add-to-group
 'vhdl-highlight-faces 'font-lock-variable-name-face 'custom-face)

(defface vhdl-font-lock-prompt-face
  '((((class color) (background light)) (:foreground "Red" :bold t))
    (((class color) (background dark)) (:foreground "Pink" :bold t))
    (t (:inverse-video t)))
  "Font lock mode face used to highlight prompts."
  :group 'vhdl-highlight-faces
  :group 'font-lock-highlighting-faces)

(defface vhdl-font-lock-attribute-face
  '((((class color) (background light)) (:foreground "Orchid"))
    (((class color) (background dark)) (:foreground "LightSteelBlue"))
    (t (:italic t :bold t)))
  "Font lock mode face used to highlight standardized attributes."
  :group 'vhdl-highlight-faces
  :group 'font-lock-highlighting-faces)

(defface vhdl-font-lock-enumvalue-face
  '((((class color) (background light)) (:foreground "Gold4"))
    (((class color) (background dark)) (:foreground "BurlyWood"))
    (t (:italic t :bold t)))
  "Font lock mode face used to highlight standardized enumeration values."
  :group 'vhdl-highlight-faces
  :group 'font-lock-highlighting-faces)

(defface vhdl-font-lock-function-face
  '((((class color) (background light)) (:foreground "Orchid4"))
    (((class color) (background dark)) (:foreground "Orchid1"))
    (t (:italic t :bold t)))
  "Font lock mode face used to highlight standardized functions and packages."
  :group 'vhdl-highlight-faces
  :group 'font-lock-highlighting-faces)

(defface vhdl-font-lock-directive-face
  '((((class color) (background light)) (:foreground "CadetBlue"))
    (((class color) (background dark)) (:foreground "Aquamarine"))
    (t (:italic t :bold t)))
  "Font lock mode face used to highlight directives."
  :group 'vhdl-highlight-faces
  :group 'font-lock-highlighting-faces)

(defface vhdl-font-lock-reserved-words-face
  '((((class color) (background light)) (:foreground "Orange" :bold t))
    (((class color) (background dark)) (:foreground "Yellow" :bold t))
    (t ()))
  "Font lock mode face used to highlight additional reserved words."
  :group 'vhdl-highlight-faces
  :group 'font-lock-highlighting-faces)

(defface vhdl-font-lock-translate-off-face
  '((((class color) (background light)) (:background "LightGray"))
    (((class color) (background dark)) (:background "DimGray"))
    (t ()))
  "Font lock mode face used to background highlight translate-off regions."
  :group 'vhdl-highlight-faces
  :group 'font-lock-highlighting-faces)

;; font lock mode faces used to highlight words with special syntax.
(let ((syntax-alist vhdl-special-syntax-alist))
  (while syntax-alist
    (eval `(defface ,(vhdl-function-name
		      "vhdl-font-lock" (car (car syntax-alist)) "face")
	     '((((class color) (background light))
		(:foreground ,(nth 2 (car syntax-alist))))
	       (((class color) (background dark))
		(:foreground ,(nth 3 (car syntax-alist))))
	       (t ()))
	     ,(concat "Font lock mode face used to highlight "
		      (nth 0 (car syntax-alist)) ".")
	     :group 'vhdl-highlight-faces
	     :group 'font-lock-highlighting-faces))
    (setq syntax-alist (cdr syntax-alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font lock initialization

(defun vhdl-font-lock-init ()
  "Initialize fontification."
  ;; highlight keywords and standardized types, attributes, enumeration
  ;; values, and subprograms
  (setq vhdl-font-lock-keywords-1
	(list
	 (list (concat "'" vhdl-attributes-regexp)
	       1 'vhdl-font-lock-attribute-face)
	 (list vhdl-types-regexp       1 'font-lock-type-face)
	 (list vhdl-functions-regexp   1 'vhdl-font-lock-function-face)
	 (list vhdl-packages-regexp    1 'vhdl-font-lock-function-face)
	 (list vhdl-enum-values-regexp 1 'vhdl-font-lock-enumvalue-face)
	 (list vhdl-keywords-regexp    1 'font-lock-keyword-face)))
  ;; highlight words with special syntax.
  (setq vhdl-font-lock-keywords-3
	(let ((syntax-alist vhdl-special-syntax-alist)
	      keywords)
	  (while syntax-alist
	    (setq keywords
		  (cons
		   (cons (concat "\\<\\(" (nth 1 (car syntax-alist)) "\\)\\>")
			 (vhdl-function-name
			  "vhdl-font-lock" (nth 0 (car syntax-alist)) "face"))
		   keywords))
	    (setq syntax-alist (cdr syntax-alist)))
	  keywords))
  ;; highlight additional reserved words
  (setq vhdl-font-lock-keywords-4
	(list (list vhdl-reserved-words-regexp 1
		    'vhdl-font-lock-reserved-words-face)))
  ;; highlight everything together
  (setq vhdl-font-lock-keywords
	(append
	 vhdl-font-lock-keywords-0
	 (when vhdl-highlight-keywords vhdl-font-lock-keywords-1)
	 (when (or vhdl-highlight-forbidden-words
		   vhdl-highlight-verilog-keywords) vhdl-font-lock-keywords-4)
	 (when vhdl-highlight-special-words vhdl-font-lock-keywords-3)
	 (when vhdl-highlight-names vhdl-font-lock-keywords-2)
	 (when vhdl-highlight-translate-off vhdl-font-lock-keywords-5))))

;; initialize fontification for VHDL Mode
(vhdl-font-lock-init)

(defun vhdl-fontify-buffer ()
  "Re-initialize fontification and fontify buffer."
  (interactive)
  (setq font-lock-defaults
	(list
	 'vhdl-font-lock-keywords nil
	 (not vhdl-highlight-case-sensitive) '((?\_ . "w")) 'beginning-of-line
	 '(font-lock-syntactic-keywords . vhdl-font-lock-syntactic-keywords)))
  (when (fboundp 'font-lock-unset-defaults)
    (font-lock-unset-defaults))		; not implemented in XEmacs
  (font-lock-set-defaults)
  (font-lock-fontify-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization for postscript printing

(defun vhdl-ps-print-settings ()
  "Initialize custom face and page settings for postscript printing."
  ;; define custom face settings
  (unless (or (not vhdl-print-customize-faces)
	      ps-print-color-p)
    (set (make-local-variable 'ps-bold-faces)
	 '(font-lock-keyword-face
	   font-lock-type-face
	   vhdl-font-lock-attribute-face
	   vhdl-font-lock-enumvalue-face
	   vhdl-font-lock-directive-face))
    (set (make-local-variable 'ps-italic-faces)
	 '(font-lock-comment-face
	   font-lock-function-name-face
	   font-lock-type-face
	   vhdl-font-lock-attribute-face
	   vhdl-font-lock-enumvalue-face
	   vhdl-font-lock-directive-face))
    (set (make-local-variable 'ps-underlined-faces)
	 '(font-lock-string-face))
    (setq ps-always-build-face-reference t))
  ;; define page settings, so that a line containing 79 characters (default)
  ;; fits into one column
  (when vhdl-print-two-column
    (set (make-local-variable 'ps-landscape-mode) t)
    (set (make-local-variable 'ps-number-of-columns) 2)
    (set (make-local-variable 'ps-font-size) 7.0)
    (set (make-local-variable 'ps-header-title-font-size) 10.0)
    (set (make-local-variable 'ps-header-font-size) 9.0)
    (set (make-local-variable 'ps-header-offset) 12.0)
    (when (eq ps-paper-type 'letter)
      (set (make-local-variable 'ps-inter-column) 40.0)
      (set (make-local-variable 'ps-left-margin) 40.0)
      (set (make-local-variable 'ps-right-margin) 40.0))))

(defun vhdl-ps-print-init ()
  "Initialize postscript printing."
  (if (string-match "XEmacs" emacs-version)
      (vhdl-ps-print-settings)
    (make-local-variable 'ps-print-hook)
    (add-hook 'ps-print-hook 'vhdl-ps-print-settings)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hierarchy browser (using `speedbar.el')
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allows displaying the hierarchy of all VHDL design units contained in a
;; directory by using the speedbar.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables

(defvar vhdl-entity-alist nil
  "Cache with entities and corresponding architectures and configurations for
each visited directory.")
;; structure: (parenthesised expression means list of such entries)
;; (directory-name
;;   (ent-name ent-file ent-line
;;     (arch-name arch-file arch-line
;;       (inst-name inst-file inst-line inst-ent-name inst-arch-name))
;;     (conf-name conf-file conf-line))

(defvar vhdl-package-alist nil
  "Cache with packages for each visited directory.")
;; structure: (parenthesised expression means list of such entries)
;; (directory-name
;;   (pack-name pack-file pack-line pack-body-file pack-body-line))

(defvar vhdl-ent-inst-alist nil
  "Cache with instantiated entities for each visited directory.")
;; structure: (parenthesised expression means list of such entries)
;; (directory-name (inst-ent-name))

(defvar vhdl-project-entity-alist nil
  "Cache with entities and corresponding architectures and configurations for
each visited project.")
;; same structure as `vhdl-entity-alist'

(defvar vhdl-project-package-alist nil
  "Cache with packages for each visited directory.")
;; same structure as `vhdl-package-alist'

(defvar vhdl-project-ent-inst-list nil
  "Cache with instantiated entities for each visited directory.")
;; same structure as `vhdl-ent-inst-alist'

(defvar vhdl-speedbar-shown-units-alist nil
  "Alist of design units simultaneously open in the current speedbar for each
directory and project.")

(defvar vhdl-speedbar-last-file-name nil
  "Last file for which design units were highlighted.")

(defvar vhdl-file-alist nil
  "Cache with design units in each file.")
;; structure (parenthesised expression means list of such entries)
;; (file-name (ent-list) (arch-list) (conf-list) (pack-list) (inst-list))

;; help function
(defsubst vhdl-speedbar-project-p ()
  "Return non-nil if a project is displayed, i.e. directories or files are
specified."
  (nth 1 (aget vhdl-project-alist vhdl-project)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scan functions

(defun vhdl-scan-file-contents (name &optional num-string)
  "Scan contents of VHDL files in FILE-LIST."
  (string-match "\\(.*/\\)\\(.*\\)" name)
;   (unless (file-directory-p (match-string 1 name))
;     (message "No such directory: \"%s\"" (match-string 1 name)))
  (let* ((is-directory (= (match-beginning 2) (match-end 2)))
	 (file-list
	  (if is-directory
	      (nreverse (vhdl-get-source-files t name))
	    (vhdl-directory-files (match-string 1 name) t
				  (wildcard-to-regexp (match-string 2 name)))))
	 (case-fold-search t)
	 (source-buffer (current-buffer))
	 ent-alist pack-alist ent-inst-list no-files)
    (when (and (not is-directory) (null file-list))
      (message "No such file: \"%s\"" name))
    (save-excursion
      (when file-list
	(setq no-files (length file-list))
	;; do for all files
	(while file-list
	  (message "Scanning %s %s\"%s\"... (%2d%s)"
		   (if is-directory "directory" "files")
		   (or num-string "") name
 		   (/ (* 100 (- no-files (length file-list))) no-files) "%")
	  (let ((file-name (abbreviate-file-name (car file-list)))
		opened arch-name ent-name
		ent-list arch-list conf-list pack-list inst-list)
	    ;; open file
	    (if (find-buffer-visiting file-name)
		(set-buffer (find-buffer-visiting file-name))
	      (set-buffer (find-file-noselect file-name nil t))
	      (setq opened t))
	    (modify-syntax-entry ?_ "w" (syntax-table))
	    ;; scan for entities
	    (goto-char (point-min))
	    (while (re-search-forward "^\\s-*entity\\s-+\\(\\w+\\)" nil t)
	      (let* ((ent-entry (aget ent-alist (match-string 1)))
		     (arch-alist (nth 2 ent-entry))
		     (conf-alist (nth 3 ent-entry)))
		(setq ent-list (cons (match-string 1) ent-list))
		(aput 'ent-alist (match-string 1)
		      (list file-name (vhdl-current-line)
			    arch-alist conf-alist nil))))
	    ;; scan for architectures and instantiations
	    (goto-char (point-min))
	    (while (re-search-forward
		    (concat
		     "^\\s-*\\(architecture\\s-+\\(\\w+\\)\\s-+of\\s-+\\(\\w+\\)\\|"
		     "\\(\\w+\\)\\s-*:\\(\\s-\\|\n\\)*\\(entity\\s-+\\w+\\.\\)?"
		     "\\(\\w+\\)\\(\\s-*(\\(\\w+\\))\\)?\\(\\s-\\|\n\\|--.*\n\\)*"
		     "\\(generic\\|port\\)\\s-+map\\>\\)")
		    nil t)
	      (if (match-string 2)
		  ;; architecture found
		  (let* ((ent-entry (aget ent-alist (match-string 3)))
			 (arch-alist (nth 2 ent-entry))
			 (conf-alist (nth 3 ent-entry)))
		    (setq arch-name (match-string 2))
		    (setq ent-name (match-string 3))
		    (setq arch-list (cons arch-name arch-list))
		    (vhdl-aappend 'arch-alist arch-name
				  (list file-name (vhdl-current-line) nil))
		    (setq ent-entry (list (nth 0 ent-entry) (nth 1 ent-entry)
					  arch-alist conf-alist nil))
		    (aput 'ent-alist ent-name ent-entry))
		;; instantiation found
		(let* ((ent-entry (aget ent-alist ent-name))
		       (arch-alist (nth 2 ent-entry))
		       (arch-entry (aget arch-alist arch-name))
		       (inst-alist (nth 2 arch-entry))
		       (inst-name (match-string 4))
		       (inst-ent-name (match-string 7))
		       (inst-arch-name (match-string 9))
		       (conf-alist (nth 3 ent-entry)))
		  (re-search-backward ":" nil t)
		  (setq inst-list (cons inst-name inst-list))
		  (vhdl-aappend 'inst-alist inst-name
				(list file-name (vhdl-current-line)
				      inst-ent-name inst-arch-name))
		  (setq arch-entry
			(list (nth 0 arch-entry) (nth 1 arch-entry)
			      inst-alist))
		  (vhdl-aappend 'arch-alist arch-name arch-entry)
		  (setq ent-entry (list (nth 0 ent-entry) (nth 1 ent-entry)
					arch-alist conf-alist nil))
		  (aput 'ent-alist ent-name ent-entry)
		  (unless (member inst-ent-name ent-inst-list)
		    (setq ent-inst-list
			  (cons inst-ent-name ent-inst-list))))))
	    ;; scan for configurations
	    (goto-char (point-min))
	    (while (re-search-forward
		    "^\\s-*configuration\\s-+\\(\\w+\\)\\s-+of\\s-+\\(\\w+\\)"
		    nil t)
	      (let* ((ent-entry (aget ent-alist (match-string 2)))
		     (arch-alist (nth 2 ent-entry))
		     (conf-alist (nth 3 ent-entry)))
		(setq conf-list (cons (match-string 1) conf-list))
		(vhdl-aappend 'conf-alist (match-string 1)
			      (list file-name (vhdl-current-line)))
		(setq ent-entry (list (nth 0 ent-entry) (nth 1 ent-entry)
				      arch-alist conf-alist nil))
		(aput 'ent-alist (match-string 2) ent-entry)))
	    ;; scan for packages
	    (goto-char (point-min))
	    (while (re-search-forward
		    "^\\s-*package\\s-+\\(body\\s-+\\)?\\(\\w+\\)" nil t)
	      (let ((pack-entry (aget pack-alist (match-string 2))))
		(setq pack-list (cons (match-string 2) pack-list))
		(aput 'pack-alist (match-string 2)
		      (if (not (match-string 1))
			  (list file-name (vhdl-current-line)
				(nth 2 pack-entry) (nth 3 pack-entry))
			(list (nth 0 pack-entry) (nth 1 pack-entry)
			      file-name (vhdl-current-line))))))
	    (setq file-list (cdr file-list))
	    ;; add design units to variable `vhdl-file-alist'
	    (aput 'vhdl-file-alist file-name
		  (list ent-list arch-list conf-list pack-list inst-list))
	    ;; close file
	    (if opened
		(kill-buffer (current-buffer))
	      (when (not vhdl-underscore-is-part-of-word)
		(modify-syntax-entry ?_ "_" vhdl-mode-syntax-table)))
	    (set-buffer source-buffer)))
	;; sort entities and packages
	(setq ent-alist
	      (sort ent-alist
		    (function (lambda (a b) (string-lessp (car a) (car b))))))
	(setq pack-alist
	      (sort pack-alist
		    (function (lambda (a b) (string-lessp (car a) (car b))))))
	;; put directory contents into cache
	(when ent-alist
	  (aput 'vhdl-entity-alist name ent-alist))
	(when pack-alist
	  (aput 'vhdl-package-alist name pack-alist))
	(when ent-inst-list
	  (aput 'vhdl-ent-inst-alist name (list ent-inst-list)))
	(message "Scanning %s %s\"%s\"...done"
		 (if is-directory "directory" "files") (or num-string "") name)
	t))))

(defun vhdl-scan-project-contents (project &optional rescan)
  "Scan the contents of all VHDL files found in the directories and files
of PROJECT."
  (let ((dir-list-tmp (nth 1 (aget vhdl-project-alist project)))
	dir-list pro-ent-alist pro-pack-alist pro-ent-inst-list
	dir name num-dir act-dir)
    ;; resolve environment variables and path wildcards
    (setq dir-list-tmp (vhdl-resolve-paths dir-list-tmp))
    ;; expand directories
    (while dir-list-tmp
      (setq dir (car dir-list-tmp))
      ;; get subdirectories
      (if (string-match "-r \\(.*/\\)" dir)
	  (setq dir-list (append dir-list (vhdl-get-subdirs
					   (match-string 1 dir))))
	(setq dir-list (append dir-list (list dir))))
      (setq dir-list-tmp (cdr dir-list-tmp)))
    ;; get entities and packages of each directory in DIR-LIST
    (setq num-dir (length dir-list)
	  act-dir 1)
    (while dir-list
      (setq name (abbreviate-file-name (car dir-list)))
      (or (and (not rescan)
	       (or (assoc name vhdl-entity-alist)
		   (assoc name vhdl-package-alist)))
	  (vhdl-scan-file-contents name (format "(%s/%s) " act-dir num-dir)))
      ;; merge entities and corresponding architectures and configurations
      (let ((ent-alist (aget vhdl-entity-alist name)))
	(while ent-alist
	  (let* ((ent-name (car (car ent-alist)))
		 (ent-entry (cdr (car ent-alist)))
		 (pro-ent-entry (aget pro-ent-alist ent-name)))
	    (aput 'pro-ent-alist ent-name
		  (list (or (nth 0 pro-ent-entry) (nth 0 ent-entry))
			(or (nth 1 pro-ent-entry) (nth 1 ent-entry))
			(append (nth 2 pro-ent-entry) (nth 2 ent-entry))
			(append (nth 3 pro-ent-entry) (nth 3 ent-entry)))))
	  (setq ent-alist (cdr ent-alist))))
      ;; merge packages and corresponding package bodies
      (let ((pack-alist (aget vhdl-package-alist name)))
	(while pack-alist
	  (let* ((pack-name (car (car pack-alist)))
		 (pack-entry (cdr (car pack-alist)))
		 (pro-pack-entry (aget pro-pack-alist pack-name)))
	    (aput 'pro-pack-alist pack-name
		  (list (or (nth 0 pro-pack-entry) (nth 0 pack-entry))
			(or (nth 1 pro-pack-entry) (nth 1 pack-entry))
			(or (nth 2 pro-pack-entry) (nth 2 pack-entry))
			(or (nth 3 pro-pack-entry) (nth 3 pack-entry)))))
	  (setq pack-alist (cdr pack-alist))))
      ;; merge list of instantiated entities
      (setq pro-ent-inst-list
	    (append pro-ent-inst-list
		    (copy-alist
		     (car (aget vhdl-ent-inst-alist name)))))
      (setq dir-list (cdr dir-list)
	    act-dir (1+ act-dir)))
    ;; sort lists and put them into the caches
    (when pro-ent-alist
      (aput 'vhdl-project-entity-alist project
	    (sort pro-ent-alist
		  (function (lambda (a b) (string-lessp (car a) (car b)))))))
    (when pro-pack-alist
      (aput 'vhdl-project-package-alist project
	    (sort pro-pack-alist
		  (function (lambda (a b) (string-lessp (car a) (car b)))))))
    (when pro-ent-inst-list
      (aput 'vhdl-project-ent-inst-list project pro-ent-inst-list))))

(defun vhdl-get-hierarchy (ent-name arch-name level indent &optional ent-hier)
  "Get instantiation hierarchy beginning in architecture ARCH-NAME of
entity ENT-NAME."
  (let* ((ent-alist (if (vhdl-speedbar-project-p)
			(aget vhdl-project-entity-alist vhdl-project)
		      (aget vhdl-entity-alist
			    (abbreviate-file-name
			     (file-name-as-directory
			      (speedbar-line-path (1- indent)))))))
	 (ent-entry (aget ent-alist ent-name))
	 (arch-entry (if arch-name (aget (nth 2 ent-entry) arch-name)
		       (cdr (car (last (nth 2 ent-entry))))))
	 (inst-list (nth 2 arch-entry))
	 inst-entry inst-ent-entry inst-arch-entry hier-list)
    (when (= level 0) (message "Extract design hierarchy..."))
    (when (member ent-name ent-hier)
      (error (format "Instantiation loop detected; component \"%s\" instantiates itself"
		     ent-name)))
    (while inst-list
      (setq inst-entry (car inst-list))
      (setq inst-ent-entry (aget ent-alist (nth 3 inst-entry)))
      (setq inst-arch-entry
	    (if (nth 4 inst-entry)
		(cons (nth 4 inst-entry)
		      (aget (nth 2 inst-ent-entry) (nth 4 inst-entry)))
	      (car (last (nth 2 inst-ent-entry)))))
      (setq hier-list
	    (append
	     hier-list
	     (cons (list (nth 0 inst-entry)
			 (cons (nth 1 inst-entry) (nth 2 inst-entry))
			 (nth 3 inst-entry)
			 (cons (nth 0 inst-ent-entry) (nth 1 inst-ent-entry))
			 (nth 0 inst-arch-entry)
			 (cons (nth 1 inst-arch-entry) (nth 2 inst-arch-entry))
			 level)
		   (vhdl-get-hierarchy (nth 3 inst-entry) (nth 4 inst-entry)
				       (1+ level) indent
				       (cons ent-name ent-hier)))))
      (setq inst-list (cdr inst-list)))
    (when (= level 0) (message "Extract design hierarchy...done"))
    hier-list))

(defun vhdl-get-instantiations (ent-name indent)
  "Get all instantiations of entity ENT-NAME."
  (let ((ent-alist (if (vhdl-speedbar-project-p)
		       (aget vhdl-project-entity-alist vhdl-project)
		     (aget vhdl-entity-alist
			   (abbreviate-file-name
			    (file-name-as-directory
			     (speedbar-line-path indent))))))
	arch-alist inst-alist ent-inst-list
	ent-entry arch-entry inst-entry)
    (while ent-alist
      (setq ent-entry (car ent-alist))
      (setq arch-alist (nth 3 ent-entry))
      (while arch-alist
	(setq arch-entry (car arch-alist))
	(setq inst-alist (nth 3 arch-entry))
	(while inst-alist
	  (setq inst-entry (car inst-alist))
	  (when (equal ent-name (nth 3 inst-entry))
	    (setq ent-inst-list
		  (cons (list (nth 0 inst-entry)
			      (cons (nth 1 inst-entry) (nth 2 inst-entry))
			      (nth 0 ent-entry)
			      (cons (nth 1 ent-entry) (nth 2 ent-entry))
			      (nth 0 arch-entry)
			      (cons (nth 1 arch-entry) (nth 2 arch-entry)))
			      ent-inst-list)))
	  (setq inst-alist (cdr inst-alist)))
	(setq arch-alist (cdr arch-alist)))
      (setq ent-alist (cdr ent-alist)))
    (nreverse ent-inst-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add hierarchy browser functionality to speedbar.

(defvar vhdl-speedbar-key-map nil
  "Keymap used when in the VHDL hierarchy browser mode.")

(defvar vhdl-speedbar-menu-items
  '(["Edit Design Unit" speedbar-edit-line t]
    ["Expand Hierarchy" speedbar-expand-line
     (save-excursion (beginning-of-line) (looking-at "[0-9]+: *.\\+. "))]
    ["Contract Hierarchy" speedbar-contract-line
     (save-excursion (beginning-of-line) (looking-at "[0-9]+: *.-. "))]
    ["Rescan Hierarchy" vhdl-speedbar-rescan-hierarchy t]
    "--"
    ["Copy Port" vhdl-speedbar-port-copy
     (save-excursion
       (beginning-of-line) (looking-at "[0-9]+: *\\[[-+?]\\] "))])
  "Additional menu-items to add to speedbar frame.")

(defun vhdl-speedbar-initialize ()
  "Initialize speedbar."
  ;; general settings
;  (set (make-local-variable 'speedbar-tag-hierarchy-method) nil)
  ;; VHDL file extensions (extracted from `auto-mode-alist')
  (let ((mode-alist auto-mode-alist))
    (while mode-alist
      (when (eq (cdr (car mode-alist)) 'vhdl-mode)
	(speedbar-add-supported-extension (car (car mode-alist))))
      (setq mode-alist (cdr mode-alist))))
  ;; hierarchy browser settings
  (when (boundp 'speedbar-mode-functions-list)
    (speedbar-add-mode-functions-list
     '("vhdl hierarchy"
       (speedbar-item-info . vhdl-speedbar-item-info)
       (speedbar-line-path . speedbar-files-line-path)))
    (unless vhdl-speedbar-key-map
      (setq vhdl-speedbar-key-map (speedbar-make-specialized-keymap))
      (define-key vhdl-speedbar-key-map "e" 'speedbar-edit-line)
      (define-key vhdl-speedbar-key-map "\C-m" 'speedbar-edit-line)
      (define-key vhdl-speedbar-key-map "+" 'speedbar-expand-line)
      (define-key vhdl-speedbar-key-map "-" 'speedbar-contract-line)
      (define-key vhdl-speedbar-key-map "s" 'vhdl-speedbar-rescan-hierarchy)
      (define-key vhdl-speedbar-key-map "c" 'vhdl-speedbar-port-copy))
    (define-key speedbar-key-map "h"
      (lambda () (interactive)
	(speedbar-change-initial-expansion-list "vhdl hierarchy")))
    (speedbar-add-expansion-list '("vhdl hierarchy" vhdl-speedbar-menu-items
				   vhdl-speedbar-key-map
				   vhdl-speedbar-display-hierarchy))
    (setq speedbar-stealthy-function-list
	  (cons '("vhdl hierarchy" vhdl-speedbar-update-current-unit)
		speedbar-stealthy-function-list))
    (when vhdl-speedbar-show-hierarchy
      (setq speedbar-initial-expansion-list-name "vhdl hierarchy"))))

(defun vhdl-speedbar (&optional arg)
  "Open/close speedbar."
  (interactive)
  (if (not (fboundp 'speedbar))
      (error "WARNING:  Speedbar is only available in newer Emacs versions")
    (condition-case ()			; due to bug in `speedbar-el' v0.7.2a
	(speedbar-frame-mode arg)
      (error (error "WARNING:  Install included `speedbar.el' patch first")))))

;; initialize speedbar for VHDL Mode
(if (not (boundp 'speedbar-frame))
    (add-hook 'speedbar-load-hook 'vhdl-speedbar-initialize)
  (vhdl-speedbar-initialize)
  (when speedbar-frame (speedbar-refresh)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display functions

;; macros must be defined in the file they are used (copied from `speedbar.el')
(defmacro speedbar-with-writable (&rest forms)
  "Allow the buffer to be writable and evaluate FORMS."
  (list 'let '((inhibit-read-only t))
	(cons 'progn forms)))
(put 'speedbar-with-writable 'lisp-indent-function 0)

(defun vhdl-speedbar-display-hierarchy (directory depth &optional rescan)
  "Display directory and hierarchy information in speedbar."
  (setq directory (abbreviate-file-name (file-name-as-directory directory)))
  (setq speedbar-last-selected-file nil)
  (speedbar-with-writable
   (save-excursion
     (if (vhdl-speedbar-project-p)
	 (progn
	   ;; insert project title
	   (vhdl-speedbar-make-title-line "Project:" 0)
	   (let ((start (point)))
	     (insert "p:")
	     (put-text-property start (point) 'invisible t)
	     (setq start (point))
	     (insert vhdl-project)
	     (put-text-property start (point) 'face 'speedbar-directory-face))
	   (insert-char ?\n 1)
	   ;; scan and insert hierarchy of project
	   (vhdl-speedbar-insert-project-hierarchy vhdl-project
						   speedbar-power-click))
       ;; insert directory path
       (speedbar-directory-buttons directory depth)
       ;; insert subdirectories
       (vhdl-speedbar-insert-dirs (speedbar-file-lists directory) depth)
       ;; scan and insert hierarchy of current directory
       (vhdl-speedbar-insert-dir-hierarchy directory depth
					   speedbar-power-click)
       ;; expand subdirectories
       (when (= depth 0) (vhdl-speedbar-expand-dirs directory))))))

(defun vhdl-speedbar-insert-hierarchy (ent-alist pack-alist
						 ent-inst-list depth)
  "Insert hierarchy of ENT-ALIST and PACK-ALIST."
  (if (not (or ent-alist pack-alist))
      (vhdl-speedbar-make-title-line "No design units!" depth)
    (let (ent-entry pack-entry)
      ;; insert entities
      (when ent-alist (vhdl-speedbar-make-title-line "Entities:" depth))
      (while ent-alist
	(setq ent-entry (car ent-alist))
	(speedbar-make-tag-line
	 'bracket ?+ 'vhdl-speedbar-expand-entity (nth 0 ent-entry)
	 (nth 0 ent-entry) 'vhdl-speedbar-find-file
	 (cons (nth 1 ent-entry) (nth 2 ent-entry))
	 'vhdl-speedbar-entity-face depth)
	(when (not (member (nth 0 ent-entry) ent-inst-list))
	  (end-of-line 0) (insert " (top)") (forward-char 1))
	(setq ent-alist (cdr ent-alist)))
      ;; insert packages
      (when pack-alist (vhdl-speedbar-make-title-line "Packages:" depth))
      (while pack-alist
	(setq pack-entry (car pack-alist))
	(vhdl-speedbar-make-pack-line
	 (nth 0 pack-entry)
	 (cons (nth 1 pack-entry) (nth 2 pack-entry))
	 (cons (nth 3 pack-entry) (nth 4 pack-entry))
	 depth)
	(setq pack-alist (cdr pack-alist))))))

(defun vhdl-speedbar-insert-project-hierarchy (project &optional rescan)
  "Insert hierarchy of project.  Rescan directories if RESCAN is non-nil,
otherwise use cached data of directories."
  (when (or rescan (and (not (assoc project vhdl-project-entity-alist))
			(not (assoc project vhdl-project-package-alist))))
    (vhdl-scan-project-contents project rescan))
    ;; insert design hierarchy in speedbar
  (vhdl-speedbar-insert-hierarchy
   (aget vhdl-project-entity-alist project)
   (aget vhdl-project-package-alist project)
   (aget vhdl-project-ent-inst-list project) 0)
    ;; expand design units
  (vhdl-speedbar-expand-units project))

(defun vhdl-speedbar-insert-dir-hierarchy (directory depth &optional rescan)
  "Insert hierarchy of DIRECTORY.  Rescan directory if RESCAN is non-nil,
otherwise use cached data."
  (when (or rescan (and (not (assoc directory vhdl-entity-alist))
			(not (assoc directory vhdl-package-alist))))
    (vhdl-scan-file-contents directory))
  (vhdl-speedbar-insert-hierarchy
   (aget vhdl-entity-alist directory)
   (aget vhdl-package-alist directory)
   (car (aget vhdl-ent-inst-alist directory))
   depth)
  (vhdl-speedbar-expand-units directory))

(defun vhdl-speedbar-rescan-hierarchy ()
  "Rescan hierarchy for the directory under the cursor or the current project."
  (interactive)
  (cond
   ;; the current project
   ((vhdl-speedbar-project-p)
    (vhdl-scan-project-contents vhdl-project t)
    (speedbar-refresh))
   ;; the top-level directory
   ((save-excursion (beginning-of-line) (looking-at "[^0-9]"))
    (re-search-forward "[0-9]+:" nil t)
    (vhdl-scan-file-contents (abbreviate-file-name (speedbar-line-path)))
    (speedbar-refresh))
   ;; the current directory
   (t (let ((path (speedbar-line-path)))
	(string-match "^\\(.+/\\)" path)
	(vhdl-scan-file-contents (abbreviate-file-name (match-string 1 path)))
	(speedbar-refresh)))))

(defun vhdl-speedbar-expand-dirs (directory)
  "Expand subdirectories in DIRECTORY according to
 `speedbar-shown-directories'."
  ;; (nicked from `speedbar-default-directory-list')
  (let ((sf (cdr (reverse speedbar-shown-directories))))
    (setq speedbar-shown-directories
	  (list (expand-file-name default-directory)))
    (while sf
      (when (speedbar-goto-this-file (car sf))
	(beginning-of-line)
	(when (looking-at "[0-9]+:\\s-*<")
	  (goto-char (match-end 0))
	  (let* ((position (point))
		 (directory (abbreviate-file-name
			     (file-name-as-directory (speedbar-line-file)))))
	    (speedbar-do-function-pointer))))
      (setq sf (cdr sf)))))

(defun vhdl-speedbar-expand-units (directory)
  "Expand design units in DIRECTORY according to
`vhdl-speedbar-shown-units-alist'."
  (let ((ent-alist (aget vhdl-speedbar-shown-units-alist directory)))
    (adelete 'vhdl-speedbar-shown-units-alist directory)
    (while ent-alist			; expand entities
      (vhdl-speedbar-goto-this-unit directory (car (car ent-alist)))
      (beginning-of-line)
      (let ((arch-alist (nth 1 (car ent-alist)))
	    position)
	(when (looking-at "[0-9]+:\\s-*\\[")
	  (goto-char (match-end 0))
	  (setq position (point))
	  (speedbar-do-function-pointer)
	  (while arch-alist		; expand architectures
	    (goto-char position)
	    (when (re-search-forward
		   (concat "[0-9]+:\\s-*\\(\\[\\|{.}\\s-+"
			   (car arch-alist) "\\>\\)") nil t)
	      (beginning-of-line)
	      (when (looking-at "[0-9]+:\\s-*{")
		(goto-char (match-end 0))
		(speedbar-do-function-pointer)))
	    (setq arch-alist (cdr arch-alist))))
	(setq ent-alist (cdr ent-alist))))))

(defun vhdl-speedbar-expand-entity (text token indent)
  "Expand/contract the entity under the cursor."
  (cond
   ((string-match "+" text)		; expand entity
    (let* ((ent-alist (if (vhdl-speedbar-project-p)
			  (aget vhdl-project-entity-alist vhdl-project)
			(aget vhdl-entity-alist
			      (abbreviate-file-name
			       (file-name-as-directory
				(speedbar-line-path indent))))))
	   (arch-alist (nth 2 (aget ent-alist token)))
	   (conf-alist (nth 3 (aget ent-alist token)))
	   (inst-alist (vhdl-get-instantiations token indent))
	   arch-entry conf-entry inst-entry)
      (if (not (or arch-alist conf-alist inst-alist))
	  (speedbar-change-expand-button-char ??)
	(speedbar-change-expand-button-char ?-)
	;; add entity to `vhdl-speedbar-shown-units-alist'
	(let* ((directory (if (vhdl-speedbar-project-p)
			      vhdl-project
			    (abbreviate-file-name
			     (file-name-as-directory (speedbar-line-path)))))
	       (ent-alist (aget vhdl-speedbar-shown-units-alist directory)))
	  (aput 'ent-alist (speedbar-line-text) nil)
	  (aput 'vhdl-speedbar-shown-units-alist directory ent-alist))
	(speedbar-with-writable
	  (save-excursion
	    (end-of-line) (forward-char 1)
	    ;; insert architectures
	    (when arch-alist
	      (vhdl-speedbar-make-title-line "Architectures:" (1+ indent)))
	    (while arch-alist
	      (setq arch-entry (car arch-alist))
	      (speedbar-make-tag-line
	       'curly ?+ 'vhdl-speedbar-expand-architecture
	       (cons token (nth 0 arch-entry))
	       (nth 0 arch-entry) 'vhdl-speedbar-find-file
	       (cons (nth 1 arch-entry) (nth 2 arch-entry))
	       'vhdl-speedbar-architecture-face (1+ indent))
	      (setq arch-alist (cdr arch-alist)))
	    ;; insert configurations
	    (when conf-alist
	      (vhdl-speedbar-make-title-line "Configurations:" (1+ indent)))
	    (while conf-alist
	      (setq conf-entry (car conf-alist))
	      (speedbar-make-tag-line
	       nil nil nil
	       (cons token (nth 0 conf-entry))
	       (nth 0 conf-entry) 'vhdl-speedbar-find-file
	       (cons (nth 1 conf-entry) (nth 2 conf-entry))
	       'vhdl-speedbar-configuration-face (1+ indent))
	      (setq conf-alist (cdr conf-alist)))
	    ;; insert instantiations
	    (when inst-alist
	      (vhdl-speedbar-make-title-line "Instantiations:" (1+ indent)))
	    (while inst-alist
	      (setq inst-entry (car inst-alist))
	      (vhdl-speedbar-make-inst-line
	       (nth 0 inst-entry) (nth 1 inst-entry)
	       (nth 2 inst-entry) (nth 3 inst-entry)
	       (nth 4 inst-entry) (nth 5 inst-entry) (1+ indent) 0)
	      (setq inst-alist (cdr inst-alist)))))
	(setq speedbar-last-selected-file nil)
	(save-excursion (speedbar-stealthy-updates)))))
   ((string-match "-" text)		; contract entity
    (speedbar-change-expand-button-char ?+)
    ;; remove entity from `vhdl-speedbar-shown-units-alist'
    (let* ((directory (if (vhdl-speedbar-project-p)
			  vhdl-project
			(abbreviate-file-name
			 (file-name-as-directory (speedbar-line-path)))))
	   (ent-alist (aget vhdl-speedbar-shown-units-alist directory)))
      (adelete 'ent-alist (speedbar-line-text))
      (if ent-alist
	  (aput 'vhdl-speedbar-shown-units-alist directory ent-alist)
	(adelete 'vhdl-speedbar-shown-units-alist directory)))
    (speedbar-delete-subblock indent))
   (t (error "No architectures, configurations, nor instantiations exist for this entity")))
  (speedbar-center-buffer-smartly))

(defun vhdl-speedbar-expand-architecture (text token indent)
  "Expand/contract the architecture under the cursor."
  (cond
   ((string-match "+" text)		; expand architecture
    (let ((hier-alist (vhdl-get-hierarchy (car token) (cdr token) 0 indent)))
      (if (not hier-alist)
	  (speedbar-change-expand-button-char ??)
	(speedbar-change-expand-button-char ?-)
	;; add architecture to `vhdl-speedbar-shown-units-alist'
	(let* ((path (speedbar-line-path))
	       (dummy (string-match "^\\(.+/\\)\\([^/ ]+\\)" path))
	       (ent-name (match-string 2 path))
	       (directory (if (vhdl-speedbar-project-p)
			      vhdl-project
			    (abbreviate-file-name (match-string 1 path))))
	       (ent-alist (aget vhdl-speedbar-shown-units-alist directory))
	       (arch-alist (nth 0 (aget ent-alist ent-name t))))
	  (aput 'ent-alist ent-name
		(list (cons (speedbar-line-text) arch-alist)))
	  (aput 'vhdl-speedbar-shown-units-alist directory ent-alist))
	(speedbar-with-writable
	 (save-excursion
	   (end-of-line) (forward-char 1)
	   ;; insert instance hierarchy
	   (when hier-alist
	     (vhdl-speedbar-make-title-line "Subcomponents:" (1+ indent)))
	   (while hier-alist
	     (let ((entry (car hier-alist)))
	       (vhdl-speedbar-make-inst-line
		(nth 0 entry) (nth 1 entry)
		(nth 2 entry) (nth 3 entry)
		(nth 4 entry) (nth 5 entry)
		(1+ indent) (nth 6 entry))
	       (setq hier-alist (cdr hier-alist))))))
	(setq speedbar-last-selected-file nil)
	(save-excursion (speedbar-stealthy-updates)))))
   ((string-match "-" text)		; contract architecture
    (speedbar-change-expand-button-char ?+)
    ;; remove architecture from `vhdl-speedbar-shown-units-alist'
    (let* ((path (speedbar-line-path))
	   (dummy (string-match "^\\(.+/\\)\\([^/ ]+\\)" path))
	   (ent-name (match-string 2 path))
	   (directory (if (vhdl-speedbar-project-p)
			  vhdl-project
			(abbreviate-file-name (match-string 1 path))))
	   (ent-alist (aget vhdl-speedbar-shown-units-alist directory))
	   (arch-alist (nth 0 (aget ent-alist ent-name t))))
      (aput 'ent-alist ent-name
	    (list (delete (speedbar-line-text) arch-alist)))
      (aput 'vhdl-speedbar-shown-units-alist directory ent-alist))
    (speedbar-delete-subblock indent))
   (t (error "No component instantiations contained in this architecture")))
  (speedbar-center-buffer-smartly))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display help functions

(defun vhdl-speedbar-update-current-unit (&optional no-position)
  "Highlight all design units that are contained in the current file.
NO-POSITION non-nil means do not re-position cursor."
  (let ((last-frame (selected-frame))
	file-name position)
    ;; get current file name
    (select-frame speedbar-attached-frame)
    (setq file-name (abbreviate-file-name (or (buffer-file-name) "")))
    (unless (equal file-name speedbar-last-selected-file)
      (select-frame speedbar-frame)
      (set-buffer speedbar-buffer)
      (speedbar-with-writable
	(save-excursion
	  ;; unhighlight last units
	  (let* ((file-entry
		  (aget vhdl-file-alist speedbar-last-selected-file)))
	    (vhdl-speedbar-update-units
	     "\\[.\\]" (nth 0 file-entry)
	     speedbar-last-selected-file 'vhdl-speedbar-entity-face)
	    (vhdl-speedbar-update-units
	     "{.}" (nth 1 file-entry)
	     speedbar-last-selected-file 'vhdl-speedbar-architecture-face)
	    (vhdl-speedbar-update-units
	     ">" (nth 2 file-entry)
	     speedbar-last-selected-file 'vhdl-speedbar-configuration-face)
	    (vhdl-speedbar-update-units
	     ">" (nth 3 file-entry)
	     speedbar-last-selected-file 'vhdl-speedbar-package-face)
	    (vhdl-speedbar-update-units
	     ">" (nth 4 file-entry)
	     speedbar-last-selected-file 'vhdl-speedbar-instantiation-face))
	  ;; highlight current units
	  (let* ((file-entry (aget vhdl-file-alist file-name)))
	    (vhdl-speedbar-update-units
	     "\\[.\\]" (nth 0 file-entry)
	     file-name 'vhdl-speedbar-entity-selected-face)
	    (setq position (or position (point-marker)))
	    (vhdl-speedbar-update-units
	     "{.}" (nth 1 file-entry)
	     file-name 'vhdl-speedbar-architecture-selected-face)
	    (setq position (or position (point-marker)))
	    (vhdl-speedbar-update-units
	     ">" (nth 2 file-entry)
	     file-name 'vhdl-speedbar-configuration-selected-face)
	    (setq position (or position (point-marker)))
	    (vhdl-speedbar-update-units
	     ">" (nth 3 file-entry)
	     file-name 'vhdl-speedbar-package-selected-face)
	    (setq position (or position (point-marker)))
	    (vhdl-speedbar-update-units
	     ">" (nth 4 file-entry)
	     file-name 'vhdl-speedbar-instantiation-selected-face))))
	    (setq position (or position (point-marker)))
      ;; move speedbar so the first highlighted unit is visible
      (when (and position (not no-position))
	(goto-char position)
	(speedbar-center-buffer-smartly)
	(speedbar-position-cursor-on-line))
      (setq speedbar-last-selected-file file-name))
    (select-frame last-frame)
    t))

(defun vhdl-speedbar-update-units (text unit-list file-name face)
  "Help function to highlight design units."
  (let (position)
    (while unit-list
      (goto-char (point-min))
      (while (re-search-forward
	      (concat text " \\(" (car unit-list) "\\)\\>") nil t)
	(when (equal file-name (car (get-text-property
				     (match-beginning 1) 'speedbar-token)))
	  (setq position (or position (point-marker)))
	  (put-text-property (match-beginning 1) (match-end 1) 'face face)))
      (setq unit-list (cdr unit-list)))
    (when position (goto-char position))))

(defun vhdl-speedbar-make-inst-line (inst-name inst-file-marker
				    ent-name ent-file-marker
				    arch-name arch-file-marker
				    depth offset)
  "Insert instantiation entry."
  (let ((start (point)))
    (insert (int-to-string depth) ":")
    (put-text-property start (point) 'invisible t)
    (setq start (point))
    (insert-char ?  (+ depth (* offset vhdl-speedbar-hierarchy-indent)))
    (insert "> ")
    (put-text-property start (point) 'invisible nil)
    (setq start (point))
    (insert inst-name)
    (speedbar-make-button
     start (point) 'vhdl-speedbar-instantiation-face 'speedbar-highlight-face
     'vhdl-speedbar-find-file inst-file-marker)
    (setq start (point))
    (insert ": ")
    (put-text-property start (point) 'invisible nil)
    (setq start (point))
    (insert ent-name)
    (speedbar-make-button
     start (point) 'vhdl-speedbar-entity-face 'speedbar-highlight-face
     'vhdl-speedbar-find-file ent-file-marker)
    (setq start (point))
    (when arch-name
      (insert " (")
      (put-text-property start (point) 'invisible nil)
      (setq start (point))
      (insert arch-name)
      (speedbar-make-button
       start (point) 'vhdl-speedbar-architecture-face 'speedbar-highlight-face
       'vhdl-speedbar-find-file arch-file-marker)
      (setq start (point))
      (insert ")"))
    (put-text-property start (point) 'invisible nil)
    (insert-char ?\n 1)
    (put-text-property (1- (point)) (point) 'invisible nil)))

(defun vhdl-speedbar-make-pack-line (pack-name pack-file-marker
					       body-file-marker depth)
  "Insert package entry."
  (let ((start (point)))
    (insert (int-to-string depth) ":")
    (put-text-property start (point) 'invisible t)
    (setq start (point))
    (insert-char ?  depth)
    (insert "> ")
    (put-text-property start (point) 'invisible nil)
    (setq start (point))
    (insert pack-name)
    (speedbar-make-button
     start (point) 'vhdl-speedbar-package-face 'speedbar-highlight-face
     'vhdl-speedbar-find-file pack-file-marker)
    (when (car body-file-marker)
      (setq start (point))
      (insert " (")
      (put-text-property start (point) 'invisible nil)
      (setq start (point))
      (insert "body")
      (speedbar-make-button
       start (point) 'vhdl-speedbar-package-face 'speedbar-highlight-face
       'vhdl-speedbar-find-file body-file-marker)
      (setq start (point))
      (insert ")")
      (put-text-property start (point) 'invisible nil))
    (insert-char ?\n 1)
    (put-text-property (1- (point)) (point) 'invisible nil)))

(defun vhdl-speedbar-make-title-line (text depth)
  "Insert design unit title entry."
  (let ((start (point)))
    (insert (int-to-string depth) ":")
    (put-text-property start (point) 'invisible t)
    (setq start (point))
    (insert-char ?  depth)
    (put-text-property start (point) 'invisible nil)
    (setq start (point))
    (insert text)
    (speedbar-make-button start (point) nil nil nil nil)
    (insert-char ?\n 1)
    (put-text-property start (point) 'invisible nil)))

(defun vhdl-speedbar-insert-dirs (files level)
  "Insert subdirectories."
  (let ((dirs (car files)))
    (while dirs
      (speedbar-make-tag-line 'angle ?+ 'vhdl-speedbar-dired (car dirs)
			      (car dirs) 'speedbar-dir-follow nil
			      'speedbar-directory-face level)
      (setq dirs (cdr dirs)))))

(defun vhdl-speedbar-dired (text token indent)
  "Speedbar click handler for directory expand button in hierarchy mode."
  (cond ((string-match "+" text)	; we have to expand this dir
	 (setq speedbar-shown-directories
	       (cons (expand-file-name
		      (concat (speedbar-line-path indent) token "/"))
		     speedbar-shown-directories))
  	 (speedbar-change-expand-button-char ?-)
	 (speedbar-reset-scanners)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (vhdl-speedbar-insert-dirs
	      (speedbar-file-lists
	       (concat (speedbar-line-path indent) token "/"))
	      (1+ indent))
	     (speedbar-reset-scanners)
	     (vhdl-speedbar-insert-dir-hierarchy
	      (abbreviate-file-name
	       (concat (speedbar-line-path indent) token "/"))
	      (1+ indent) speedbar-power-click)))
	 (setq speedbar-last-selected-file nil)
	 (save-excursion (speedbar-stealthy-updates)))
	((string-match "-" text)	; we have to contract this node
	 (speedbar-reset-scanners)
	 (let ((oldl speedbar-shown-directories)
	       (newl nil)
	       (td (expand-file-name
		    (concat (speedbar-line-path indent) token))))
	   (while oldl
	     (if (not (string-match (concat "^" (regexp-quote td)) (car oldl)))
		 (setq newl (cons (car oldl) newl)))
	     (setq oldl (cdr oldl)))
	   (setq speedbar-shown-directories (nreverse newl)))
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun vhdl-speedbar-item-info ()
  "Derive and display information about this line item."
  (save-excursion
    (beginning-of-line)
    ;; skip invisible number info
    (when (looking-at "[0-9]+:") (goto-char (match-end 0)))
    (when (looking-at "p:")
      (message "Project \"%s\""
	       (nth 0 (aget vhdl-project-alist vhdl-project))))
    (cond
     ;; directory entry
     ((looking-at "\\s-*<[-+?]> ") (speedbar-files-item-info))
     ;; design unit entry
     ((looking-at "\\s-*\\([[{][-+?][]}]\\|>\\) ")
      (goto-char (match-end 0))
      (let ((face (get-text-property (point) 'face)))
	(message
	 "%s \"%s\" in \"%s\""
	 ;; design unit kind
	 (cond ((or (eq face 'vhdl-speedbar-entity-face)
		    (eq face 'vhdl-speedbar-entity-selected-face))
		"Entity")
	       ((or (eq face 'vhdl-speedbar-architecture-face)
		    (eq face 'vhdl-speedbar-architecture-selected-face))
		"Architecture")
	       ((or (eq face 'vhdl-speedbar-configuration-face)
		    (eq face 'vhdl-speedbar-configuration-selected-face))
		"Configuration")
	       ((or (eq face 'vhdl-speedbar-package-face)
		    (eq face 'vhdl-speedbar-package-selected-face))
		"Package")
	       ((or (eq face 'vhdl-speedbar-instantiation-face)
		    (eq face 'vhdl-speedbar-instantiation-selected-face))
		"Instantiation")
	       (t ""))
	 ;; design unit name
	 (buffer-substring-no-properties
	  (point) (progn (looking-at"\\(\\w\\|_\\)+") (match-end 0)))
	 ;; file name
	 (abbreviate-file-name
	  (or (car (get-text-property (point) 'speedbar-token)) "?"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help functions

(defun vhdl-get-subdirs (directory)
  "Recursively get subdirectories of DIRECTORY."
  (let ((dir-list (list (file-name-as-directory directory)))
	subdir-list file-list)
    (setq file-list (vhdl-directory-files directory t "\\w.*"))
    (while file-list
      (when (file-directory-p (car file-list))
	(setq dir-list (append dir-list (vhdl-get-subdirs (car file-list)))))
      (setq file-list (cdr file-list)))
    dir-list))

(defun vhdl-resolve-paths (path-list)
  "Resolve environment variables and path wildcards in PATH-LIST."
  (let (path-list-1 path-list-2 path-list-3 path-beg path-end dir)
    ;; resolve environment variables
    (while path-list
      (setq dir (car path-list))
      (while (string-match "\\(.*\\)${?\\(\\(\\w\\|_\\)+\\)}?\\(.*\\)" dir)
	(setq dir (concat (match-string 1 dir) (getenv (match-string 2 dir))
			  (match-string 4 dir))))
      (setq path-list-1 (cons dir path-list-1))
      (setq path-list (cdr path-list)))
    ;; eliminate non-existent directories
    (while path-list-1
      (setq dir (car path-list-1))
      (string-match "\\(-r \\)?\\(\\([^?*]*/\\)*\\)" dir)
      (if (file-directory-p (match-string 2 dir))
	  (setq path-list-2 (cons dir path-list-2))
	(message "No such directory: \"%s\"" (match-string 2 dir)))
      (setq path-list-1 (cdr path-list-1)))
    ;; resolve path wildcards
    (while path-list-2
      (setq dir (car path-list-2))
      (if (string-match
	   "\\(-r \\)?\\(\\([^?*]*/\\)*\\)\\([^/]*[?*][^/]*\\)\\(/.*\\)" dir)
	  (progn
	    (setq path-beg (match-string 1 dir)
		  path-end (match-string 5 dir))
	    (setq path-list-2
		  (append
		   (mapcar
		    (function
		     (lambda (var) (concat path-beg var path-end)))
		    (let ((all-list (vhdl-directory-files
				     (match-string 2 dir) t
				     (concat "\\<" (wildcard-to-regexp
						    (match-string 4 dir)))))
			  dir-list)
		      (while all-list
			(when (file-directory-p (car all-list))
			  (setq dir-list (cons (car all-list) dir-list)))
			(setq all-list (cdr all-list)))
		      dir-list))
		   (cdr path-list-2))))
	(string-match "\\(-r \\)?\\(.*\\)/.*" dir)
	(when (file-directory-p (match-string 2 dir))
	  (setq path-list-3 (cons dir path-list-3)))
	(setq path-list-2 (cdr path-list-2))))
    path-list-3))

(defun vhdl-aappend (alist-symbol key value)
  "Append a key-value pair to an alist.
Similar to `aput' but moves the key-value pair to the tail of the alist."
  (let ((elem (aelement key value))
	(alist (adelete alist-symbol key)))
    (set alist-symbol (append alist elem))))

(defun vhdl-speedbar-goto-this-unit (directory unit)
  "If UNIT is displayed in DIRECTORY, goto this line and return t, else nil."
  (let ((dest (point)))
    (if (and (if (vhdl-speedbar-project-p)
		 (progn (goto-char (point-min)) t)
	       (speedbar-goto-this-file directory))
	     (re-search-forward (concat "[]}] " unit "\\>") nil t))
	(progn (speedbar-position-cursor-on-line)
	       t)
      (goto-char dest)
      nil)))

(defun vhdl-speedbar-find-file (text token indent)
  "When user clicks on TEXT, load file with name and position in TOKEN."
  (if (not (car token))
      (error "Design unit does not exist")
    (speedbar-find-file-in-frame (car token))
    (goto-line (cdr token))
    (recenter)
    (vhdl-speedbar-update-current-unit t)
    (speedbar-set-timer speedbar-update-speed)
    (speedbar-maybee-jump-to-attached-frame)))

(defun vhdl-speedbar-toggle-hierarchy ()
  "Toggle between hierarchy and file browsing mode."
  (interactive)
  (if (not (boundp 'speedbar-mode-functions-list))
      (error "WARNING:  Install included `speedbar.el' patch first")
    (if (equal speedbar-initial-expansion-list-name "vhdl hierarchy")
	(speedbar-change-initial-expansion-list "files")
      (speedbar-change-initial-expansion-list "vhdl hierarchy"))))

(defun vhdl-speedbar-port-copy ()
  "Copy the port of the entity under the cursor."
  (interactive)
  (beginning-of-line)
  (if (re-search-forward "\\([0-9]\\)+:\\s-*\\[[-+?]\\] \\(\\(\\w\\|\\s_\\)+\\)"
			 (save-excursion (end-of-line) (point)) t)
    (condition-case ()
	(let* ((indent (string-to-number (match-string 1)))
	       (ent-name (match-string 2))
	       (ent-alist (if (vhdl-speedbar-project-p)
			      (aget vhdl-project-entity-alist vhdl-project)
			    (aget vhdl-entity-alist
				  (abbreviate-file-name
				   (file-name-as-directory
				    (speedbar-line-path indent))))))
	       (ent-entry (aget ent-alist ent-name))
	       (file-name (nth 0 ent-entry))
	       opened)
	  ;; open file
	  (if (find-buffer-visiting file-name)
	      (set-buffer (file-name-nondirectory file-name))
	    (set-buffer (find-file-noselect file-name nil t))
	    (modify-syntax-entry ?\- ". 12" (syntax-table))
	    (modify-syntax-entry ?\n ">" (syntax-table))
	    (modify-syntax-entry ?\^M ">" (syntax-table))
	    (setq opened t))
	  ;; scan port
	  (goto-line (nth 1 ent-entry))
	  (end-of-line)
	  (vhdl-port-copy)
	  ;; close file
	  (when opened (kill-buffer (current-buffer))))
      (error (error "Port not scanned successfully")))
    (error "No entity on current line")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fontification

(defface vhdl-speedbar-entity-face
  '((((class color) (background light)) (:foreground "ForestGreen"))
    (((class color) (background dark)) (:foreground "PaleGreen")))
  "Face used for displaying entity names."
  :group 'speedbar-faces)

(defface vhdl-speedbar-architecture-face
  '((((class color) (background light)) (:foreground "Blue"))
    (((class color) (background dark)) (:foreground "LightSkyBlue")))
  "Face used for displaying architecture names."
  :group 'speedbar-faces)

(defface vhdl-speedbar-configuration-face
  '((((class color) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (background dark)) (:foreground "Salmon")))
  "Face used for displaying configuration names."
  :group 'speedbar-faces)

(defface vhdl-speedbar-package-face
  '((((class color) (background light)) (:foreground "Grey50"))
    (((class color) (background dark)) (:foreground "Grey80")))
  "Face used for displaying package names."
  :group 'speedbar-faces)

(defface vhdl-speedbar-instantiation-face
  '((((class color) (background light)) (:foreground "Brown"))
    (((class color) (background dark)) (:foreground "Yellow")))
  "Face used for displaying instantiation names."
  :group 'speedbar-faces)

(defface vhdl-speedbar-entity-selected-face
  '((((class color) (background light)) (:foreground "ForestGreen" :underline t))
    (((class color) (background dark)) (:foreground "PaleGreen" :underline t)))
  "Face used for displaying entity names."
  :group 'speedbar-faces)

(defface vhdl-speedbar-architecture-selected-face
  '((((class color) (background light)) (:foreground "Blue" :underline t))
    (((class color) (background dark)) (:foreground "LightSkyBlue" :underline t)))
  "Face used for displaying architecture names."
  :group 'speedbar-faces)

(defface vhdl-speedbar-configuration-selected-face
  '((((class color) (background light)) (:foreground "DarkGoldenrod" :underline t))
    (((class color) (background dark)) (:foreground "Salmon" :underline t)))
  "Face used for displaying configuration names."
  :group 'speedbar-faces)

(defface vhdl-speedbar-package-selected-face
  '((((class color) (background light)) (:foreground "Grey50" :underline t))
    (((class color) (background dark)) (:foreground "Grey80" :underline t)))
  "Face used for displaying package names."
  :group 'speedbar-faces)

(defface vhdl-speedbar-instantiation-selected-face
  '((((class color) (background light)) (:foreground "Brown" :underline t))
    (((class color) (background dark)) (:foreground "Yellow" :underline t)))
  "Face used for displaying instantiation names."
  :group 'speedbar-faces)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bug reports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (using `reporter.el')

(defconst vhdl-mode-help-address "vhdl-mode@geocities.com"
  "Address for VHDL Mode bug reports.")

(defun vhdl-version ()
  "Echo the current version of VHDL Mode in the minibuffer."
  (interactive)
  (message "Using VHDL Mode version %s" vhdl-version)
  (vhdl-keep-region-active))

;; get reporter-submit-bug-report when byte-compiling
(eval-when-compile
  (require 'reporter))

(defun vhdl-submit-bug-report ()
  "Submit via mail a bug report on VHDL Mode."
  (interactive)
  ;; load in reporter
  (and
   (y-or-n-p "Do you want to submit a report on VHDL Mode? ")
   (require 'reporter)
   (reporter-submit-bug-report
    vhdl-mode-help-address
    (concat "VHDL Mode " vhdl-version)
    (list
     ;; report all important variables
     'vhdl-offsets-alist
     'vhdl-comment-only-line-offset
     'tab-width
     'vhdl-electric-mode
     'vhdl-stutter-mode
     'vhdl-indent-tabs-mode
     'vhdl-project-alist
     'vhdl-project
     'vhdl-compiler-alist
     'vhdl-compiler
     'vhdl-compiler-options
     'vhdl-standard
     'vhdl-basic-offset
     'vhdl-upper-case-keywords
     'vhdl-upper-case-types
     'vhdl-upper-case-attributes
     'vhdl-upper-case-enum-values
     'vhdl-upper-case-constants
     'vhdl-electric-keywords
     'vhdl-optional-labels
     'vhdl-insert-empty-lines
     'vhdl-argument-list-indent
     'vhdl-association-list-with-formals
     'vhdl-conditions-in-parenthesis
     'vhdl-zero-string
     'vhdl-one-string
     'vhdl-file-header
     'vhdl-file-footer
     'vhdl-company-name
     'vhdl-platform-spec
     'vhdl-date-format
     'vhdl-modify-date-prefix-string
     'vhdl-modify-date-on-saving
     'vhdl-reset-kind
     'vhdl-reset-active-high
     'vhdl-clock-rising-edge
     'vhdl-clock-edge-condition
     'vhdl-clock-name
     'vhdl-reset-name
     'vhdl-model-alist
     'vhdl-include-port-comments
     'vhdl-include-direction-comments
     'vhdl-actual-port-name
     'vhdl-instance-name
     'vhdl-testbench-entity-name
     'vhdl-testbench-architecture-name
     'vhdl-testbench-dut-name
     'vhdl-testbench-entity-header
     'vhdl-testbench-architecture-header
     'vhdl-testbench-declarations
     'vhdl-testbench-statements
     'vhdl-testbench-initialize-signals
     'vhdl-testbench-create-files
     'vhdl-self-insert-comments
     'vhdl-prompt-for-comments
     'vhdl-inline-comment-column
     'vhdl-end-comment-column
     'vhdl-auto-align
     'vhdl-align-groups
     'vhdl-highlight-keywords
     'vhdl-highlight-names
     'vhdl-highlight-special-words
     'vhdl-highlight-forbidden-words
     'vhdl-highlight-verilog-keywords
     'vhdl-highlight-translate-off
     'vhdl-highlight-case-sensitive
     'vhdl-special-syntax-alist
     'vhdl-forbidden-words
     'vhdl-forbidden-syntax
     'vhdl-speedbar
     'vhdl-speedbar-show-hierarchy
     'vhdl-speedbar-hierarchy-indent
     'vhdl-index-menu
     'vhdl-source-file-menu
     'vhdl-hideshow-menu
     'vhdl-hide-all-init
     'vhdl-print-two-column
     'vhdl-print-customize-faces
     'vhdl-intelligent-tab
     'vhdl-word-completion-case-sensitive
     'vhdl-word-completion-in-minibuffer
     'vhdl-underscore-is-part-of-word
     'vhdl-mode-hook
     'vhdl-startup-warnings)
    (function
     (lambda ()
       (insert
	(if vhdl-special-indent-hook
	    (concat "\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n"
		    "vhdl-special-indent-hook is set to '"
		    (format "%s" vhdl-special-indent-hook)
		    ".\nPerhaps this is your problem?\n"
		    "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\n")
	  "\n"))))
    nil
    "Dear VHDL Mode maintainers,")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'vhdl-mode)

;;; vhdl-mode.el ends here
