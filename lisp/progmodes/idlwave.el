;;; idlwave.el --- IDL and WAVE CL editing mode for GNU Emacs
;; Copyright (c) 1994-1997 Chris Chase
;; Copyright (c) 1999 Carsten Dominik
;; Copyright (c) 1999 Free Software Foundation

;; Author: Chris Chase <chase@att.com>
;; Maintainer: Carsten Dominik <dominik@strw.leidenuniv.nl>
;; Version: 3.15
;; Date: $Date: 2000/02/04 09:19:36 $
;; Keywords: languages

;; This file is part of the GNU Emacs.

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

;; In distant past, based on pascal.el.  Though bears little
;; resemblance to that now.
;;
;; Incorporates many ideas, such as abbrevs, action routines, and
;; continuation line indenting, from wave.el.
;; wave.el original written by Lubos Pochman, Precision Visuals, Boulder.
;;
;; See the mode description ("C-h m" in idlwave-mode or "C-h f idlwave-mode")
;; for features, key bindings, and info.
;; Also, Info format documentation is available with `M-x idlwave-info'
;;
;;
;; INSTALLATION
;; ============
;;
;; Follow the instructions in the INSTALL file of the distribution.
;; In short, put this file on your load path and add the following
;; lines to your .emacs file:
;;
;; (autoload 'idlwave-mode "idlwave" "IDLWAVE Mode" t)
;; (autoload 'idlwave-shell "idlw-shell" "IDLWAVE Shell" t)
;; (setq auto-mode-alist (cons '("\\.pro\\'" . idlwave-mode) auto-mode-alist))
;;
;;
;; SOURCE
;; ======
;;
;; The newest version of this file is available from the maintainers
;; Webpage.
;;
;;   http://www.strw.leidenuniv.el/~dominik/Tools/idlwave
;;
;; DOCUMENTATION
;; =============
;;
;; IDLWAVE is documented online in info format.
;; A printable version of the documentation is available from the
;; maintainers webpage (see under SOURCE)
;;
;; 
;; ACKNOWLEDGMENTS
;; ===============
;;
;;  Thanks to the following people for their contributions and comments:
;;
;;    Ulrik Dickow <dickow@nbi.dk>
;;    Eric E. Dors <edors@lanl.gov>
;;    Stein Vidar H. Haugan <s.v.h.haugan@astro.uio.no>
;;    David Huenemoerder <dph@space.mit.edu>
;;    Kevin Ivory <Kevin.Ivory@linmpi.mpg.de>
;;    Xuyong Liu <liu@stsci.edu>
;;    Simon Marshall <Simon.Marshall@esrin.esa.it>
;;    Laurent Mugnier <mugnier@onera.fr>
;;    Lubos Pochman <lubos@rsinc.com>
;;    Patrick M. Ryan <pat@jaameri.gsfc.nasa.gov>
;;    Marty Ryba <ryba@ll.mit.edu>
;;    Phil Williams <williams@irc.chmcc.org>
;;    J.D. Smith <jdsmith@astrosun.tn.cornell.edu>
;;    Phil Sterne <sterne@dublin.llnl.gov>
;;
;; CUSTOMIZATION:
;; =============
;;
;; IDLWAVE has customize support - so if you want to learn about the
;;  variables which control the behavior of the mode, use
;; `M-x idlwave-customize'.
;;
;; You can set your own preferred values with Customize, or with Lisp
;; code in .emacs.  For an example of what to put into .emacs, check
;; the TexInfo documentation.
;;
;; KNOWN PROBLEMS:
;; ==============
;;
;;   Moving the point backwards in conjunction with abbrev expansion
;;   does not work as I would like it, but this is a problem with
;;   emacs abbrev expansion done by the self-insert-command.  It ends
;;   up inserting the character that expanded the abbrev after moving
;;   point backward, e.g., "\cl" expanded with a space becomes
;;   "LONG( )" with point before the close paren.  This is solved by
;;   using a temporary function in `post-command-hook' - not pretty, 
;;   but it works.
;;
;;   Tabs and spaces are treated equally as whitespace when filling a
;;   comment paragraph.  To accomplish this, tabs are permanently
;;   replaced by spaces in the text surrounding the paragraph, which
;;   may be an undesirable side-effect.  Replacing tabs with spaces is
;;   limited to comments only and occurs only when a comment
;;   paragraph is filled via `idlwave-fill-paragraph'.
;;
;;   "&" is ignored when parsing statements.
;;   Avoid muti-statement lines (using "&") on block begin and end
;;   lines.  Multi-statement lines can mess up the formatting, for
;;   example, multiple end statements on a line: endif & endif.
;;   Using "&" outside of block begin/end lines should be okay.
;;
;;   It is possible that the parser which decides what to complete has
;;   problems with pointer dereferencing statements.  I don't use
;;   pointers often enough to find out - please report any problems.
;;
;;   Completion and Routine Info do not know about inheritance.  Thus,
;;   Keywords inherited from superclasses are not displayed and cannot
;;   completed.
;;
;;   When forcing completion of method keywords, the initial
;;   query for a method has multiple entries for some methods.  Would
;;   be too difficult to fix this hardly used case.
;;

;;; Code:

(eval-when-compile (require 'cl))

(eval-and-compile
  ;; Kludge to allow `defcustom' for Emacs 19.
  (condition-case () (require 'custom) (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old or no custom-library, hack around it!
    (defmacro defgroup (&rest args) nil)
    (defmacro defcustom (var value doc &rest args) 
      (` (defvar (, var) (, value) (, doc))))))

(defgroup idlwave nil
  "Major mode for editing IDL/WAVE CL .pro files"
  :tag "IDLWAVE"
  :link '(url-link :tag "Home Page" 
		   "http://strw.leidenuniv.nl/~dominik/Tools/idlwave")
  :link '(emacs-commentary-link :tag "Commentary in idlw-shell.el"
				"idlw-shell.el")
  :link '(emacs-commentary-link :tag "Commentary in idlwave.el" "idlwave.el")
  :link '(custom-manual "(idlwave)Top")
  :prefix "idlwave"
  :group 'languages)

;;; Variables for indentation behavior ---------------------------------------

(defgroup idlwave-code-formatting nil
  "Indentation and formatting options for IDLWAVE mode."
  :group 'idlwave)

(defcustom idlwave-main-block-indent 0
  "*Extra indentation for the main block of code.
That is the block between the FUNCTION/PRO statement and the END
statement for that program unit."
  :group 'idlwave-code-formatting
  :type 'integer)

(defcustom idlwave-block-indent 4
  "*Extra indentation applied to block lines.
If you change this, you probably also want to change `idlwave-end-offset'."
  :group 'idlwave-code-formatting
  :type 'integer)

(defcustom idlwave-end-offset -4
  "*Extra indentation applied to block END lines.
A value equal to negative `idlwave-block-indent' will make END lines
line up with the block BEGIN lines."
  :group 'idlwave-code-formatting
  :type 'integer)

(defcustom idlwave-continuation-indent 2
  "*Extra indentation applied to continuation lines.
This extra offset applies to the first of a set of continuation lines.
The following lines receive the same indentation as the first.
Also, the value of this variable applies to continuation lines inside
parenthesis.  When the current line contains an open unmatched ([{,
the next line is indented to that parenthesis plus the value of this variable."
  :group 'idlwave-code-formatting
  :type 'integer)

(defcustom idlwave-hanging-indent t
  "*If set non-nil then comment paragraphs are indented under the
hanging indent given by `idlwave-hang-indent-regexp' match in the first line
of the paragraph."
  :group 'idlwave-code-formatting
  :type 'boolean)

(defcustom idlwave-hang-indent-regexp "- "
  "*Regular expression matching the position of the hanging indent
in the first line of a comment paragraph. The size of the indent
extends to the end of the match for the regular expression."
  :group 'idlwave-code-formatting
  :type 'regexp)

(defcustom idlwave-use-last-hang-indent nil
  "*If non-nil then use last match on line for `idlwave-indent-regexp'."
  :group 'idlwave-code-formatting
  :type 'boolean)

(defcustom idlwave-fill-comment-line-only t
  "*If non-nil then auto fill will only operate on comment lines."
  :group 'idlwave-code-formatting
  :type 'boolean)

(defcustom idlwave-auto-fill-split-string t
  "*If non-nil then auto fill will split strings with the IDL `+' operator.
When the line end falls within a string, string concatenation with the 
'+' operator will be used to distribute a long string over lines.  
If nil and a string is split then a terminal beep and warning are issued.

This variable is ignored when `idlwave-fill-comment-line-only' is
non-nil, since in this case code is not auto-filled."
  :group 'idlwave-code-formatting
  :type 'boolean)

(defcustom idlwave-split-line-string t
  "*If non-nil then `idlwave-split-line' will split strings with `+'.
When the splitting point of a line falls inside a string, split the string
using the `+' string concatenation operator.  If nil and a string is
split then a terminal beep and warning are issued."
  :group 'idlwave-code-formatting
  :type 'boolean)

(defcustom idlwave-no-change-comment ";;;"
  "*The indentation of a comment that starts with this regular
expression will not be changed. Note that the indentation of a comment
at the beginning of a line is never changed."
  :group 'idlwave-code-formatting
  :type 'string)

(defcustom idlwave-begin-line-comment nil
  "*A comment anchored at the beginning of line.
A comment matching this regular expression will not have its
indentation changed.  If nil the default is \"^;\", i.e., any line
beginning with a \";\".  Expressions for comments at the beginning of
the line should begin with \"^\"."
  :group 'idlwave-code-formatting
  :type '(choice (const :tag "Any line beginning with `;'" nil)
		 'regexp))

(defcustom idlwave-code-comment ";;[^;]"
  "*A comment that starts with this regular expression on a line by
itself is indented as if it is a part of IDL code.  As a result if
the comment is not preceded by whitespace it is unchanged."
  :group 'idlwave-code-formatting
  :type 'regexp)

;; Comments not matching any of the above will be indented as a
;; right-margin comment, i.e., to a minimum of `comment-column'.


;;; Routine Info and Completion ---------------------------------------

(defgroup idlwave-routine-info-and-completion nil
  "Routine info and name/keyword completion options for IDLWAVE mode."
  :group 'idlwave)

(defcustom idlwave-scan-all-buffers-for-routine-info t
  "*Non-nil means, scan all buffers for IDL programs when updating info.
`idlwave-update-routine-info' scans buffers of the current Emacs session
for routine definitions.  When this variable is nil, it only parses the
current buffer.  When non-nil, all buffers are searched.
A prefix to \\[idlwave-update-routine-info] toggles the meaning of this
variable for the duration of the command."
  :group 'idlwave-routine-info-and-completion
  :type 'boolean)

(defcustom idlwave-query-shell-for-routine-info t
  "*Non-nil means query the shell for info about compiled routines.
Querying the shell is useful to get information about compiled modules,
and it is turned on by default.  However, when you have a complete library
scan, this is not necessary."
  :group 'idlwave-routine-info-and-completion
  :type 'boolean)

(defcustom idlwave-library-path nil
  "Library path for Windows and MacOS.  Not needed under Unix.
When selecting the directories to scan for IDL library routine info,
IDLWAVE can under UNIX query the shell for the exact search path.
However, under Windows and MacOS, the IDLWAVE shell does not work.  In this
case, this variable specifies the path where IDLWAVE can find library files.
The shell will only be asked when this variable is nil.
The value is a list of directories.  A directory preceeded by a `+' will
be searched recursively.  If you set this variable on a UNIX system, the shell
will not be asked."
  :group 'idlwave-routine-info-and-completion
  :type '(repeat (directory)))

(defcustom idlwave-libinfo-file nil
  "*File for routine information of the IDL library.
When this points to a file, the file will be loaded when IDLWAVE first
accesses routine info (or does completion).
When you scan the library with `idlwave-create-libinfo-file', this file
will be used to store the result."
  :group 'idlwave-routine-info-and-completion
  :type 'file)

(eval-and-compile
  (defconst idlwave-tmp
    '(choice :tag "by applying the function"
      (const upcase)
      (const downcase)
      (const capitalize)
      (const preserve)
      (symbol :tag "Other"))))


(defcustom idlwave-completion-case '((routine . upcase)
				     (keyword . upcase)
				     (class   . preserve)
				     (method  . preserve))
  "Association list setting the case of completed words.

This variable determines the case (UPPER/lower/Capitalized...) of
words inserted into the buffer by completion.  The preferred case can
be specified separately for routine names, keywords, classes and
methods. 
This alist should therefore have entries for `routine' (normal
functions and procedures, i.e. non-methods), `keyword', `class', and
`method'.  Plausible values are

upcase      upcase whole word, like `BOX_CURSOR'
downcase    downcase whole word, like `read_ppm'
capitalize  capitalize each part, like `Widget_Control'
preserve    preserve case as is, like `IDLgrView'

The value can also be any Emacs Lisp function which transforms the
case of characters in a string.

A value of `preserve' means that the case of the completed word is
identical to the way it was written in the definition statement of the
routine.  This was implemented to allow for mixed-case completion, in
particular of object classes and methods.
If a completable word is defined in multiple locations, the meaning of
`preserve' is not unique since the different definitions might be
cased differently.  Therefore IDLWAVE always takes the case of the
*first* definition it encounters during routine info collection and
uses the case derived from it consistently.

Note that a lowercase-only string in the buffer will always be completed in
lower case (but see the variable `idlwave-completion-force-default-case').

After changing this variable, you need to either restart Emacs or press
`C-u C-c C-i' to update the internal lists."
  :group 'idlwave-routine-info-and-completion
  :type `(repeat
	  (cons (symbol :tag "Derive completion case for")
		,idlwave-tmp)))

(defcustom idlwave-completion-force-default-case nil
  "*Non-nil means, completion will always honor `idlwave-completion-case'.
When nil, only the completion of a mixed case or upper case string
will honor the default settings in `idlwave-completion-case', while
the completion of lower case strings will be completed entirely in
lower case."
  :group 'idlwave-routine-info-and-completion
  :type 'boolean)

(defcustom idlwave-complete-empty-string-as-lower-case nil
  "*Non-nil means, the empty string is considered downcase for completion.
The case of what is already in the buffer determines the case of completions.
When this variable is non-nil, the empty string is considered to be downcase.
Completing on the empty string then offers downcase versions of the possible
completions."
  :group 'idlwave-routine-info-and-completion
  :type 'boolean)

(defvar idlwave-default-completion-case-is-down nil
  "Obsolete variable.  See `idlwave-complete-empty-string-as-lower-case' and
`idlwave-completion-case'.")

(defcustom idlwave-buffer-case-takes-precedence nil
  "*Non-nil means, the case of tokens in buffers dominates over system stuff.
To make this possible, we need to re-case everything each time we update
the routine info from the buffers.  This is slow.
The default is to consider the case given in the system and library files
first which makes updating much faster."
  :group 'idlwave-routine-info-and-completion
  :type 'boolean)

(defcustom idlwave-completion-show-classes 1
  "*Number of classes to show when completing object methods and keywords.
When completing methods or keywords for an object with unknown class,
the *Completions* buffer will show the legal classes for each completion
like this:

MyMethod <Class1,Class2,Class3>

The value of this variable may be nil to inhibit display, or an integer to
indicate the maximum number of classes to display.

On XEmacs, a full list of classes will also be placed into a `help-echo'
property on the competion items, so that the list of classes for the current
item is displayed in the echo area.  If the value of this variable is a
negative integer, the `help-echo' property will be suppressed."
  :group 'idlwave-routine-info-and-completion
  :type '(choice (const :tag "Don't show" nil)
		 (integer :tag "Number of classes shown" 1)))

(defcustom idlwave-completion-fontify-classes t
  "*Non-nil means, fontify the classes in completions buffer.
This makes it easier to distinguish the completion items from the extra
class info listed.  See `idlwave-completion-show-classes'."
  :group 'idlwave-routine-info-and-completion
  :type 'boolean)

(defcustom idlwave-query-class '((method-default . nil)
				 (keyword-default . nil))
  "Association list governing specification of object classes for completion.

When IDLWAVE is trying to complete items which belong to the object
oriented part of IDL, it usually cannot determine the class of a given
object from context.  In order to provide the user with a correct list
of methods or keywords, it would have to determine the appropriate
class.  IDLWAVE has two ways to deal with this problem.

1. One possibility is to combine the items of all available
   classes for the purpose of completion.  So when completing a
   method, all methods of all classes are available, and when
   completing a keyword, all keywords allowed for this method in any
   class will be possible completions.  This behavior is very much
   like normal completion and is therefore the default.  It works much 
   better than one might think - only for the INIT, GETPROPERTY and
   SETPROPERTY the keyword lists become uncomfortably long.
   See also `idlwave-completion-show-classes'.

2. The second possibility is to ask the user on each occasion.  To
   make this less interruptive, IDLWAVE can store the class as a text
   property on the object operator `->'.  For a given object in the
   source code, class selection will then be needed only once
   - for example to complete the method.  Keywords to the method can
   then be completed directly, because the class is already known.
   You will have to turn on the storage of the selected class
   explicitly with the variable `idlwave-store-inquired-class'.

This variable allows to configure IDLWAVE's behavior during
completion.  Its value is an alist, which should contain at least two
elements: (method-default . VALUE) and (keyword-default . VALUE),
where VALUE is either t or nil.  These specify if the class should be
determined during method and keyword completion, respectively.

The alist may have additional entries specifying exceptions from the 
keyword completion rule for specific methods, like INIT or
GETPROPERTY.  In order to turn on class specification for the INIT
method, add an entry (\"INIT\" . t).  The method name must be ALL-CAPS."
  :group 'idlwave-routine-info-and-completion
  :type '(list
	  (cons (const method-default)
		(boolean :tag "Determine class when completing METHODS    "))
	  (cons (const keyword-default)
		(boolean :tag "Determine class when completing KEYWORDS   "))
	  (repeat
	   :tag "Exceptions to defaults"
	   :inline t
	   (cons (string  :tag "MODULE" :value "")
		 (boolean :tag "Determine class for this method")))))

(defcustom idlwave-store-inquired-class nil
  "*Non-nil means, store class of a method call as text property on `->'.
IDLWAVE sometimes has to ask the user for the class associated with a
particular object method call.  This happens during the commands
`idlwave-routine-info' and `idlwave-complete', depending upon the
value of the variable `idlwave-query-class'.

When you specify a class, this information can be stored as a text
property on the `->' arrow in the source code, so that during the same 
editing session, IDLWAVE will not have to ask again.  When this
variable is non-nil, IDLWAVE will store and reuse the class information.
The class stored can be checked and removed with `\\[idlwave-routine-info]'
on the arrow.

The default of this variable is nil, since the result of commands then
is more predictable.  However, if you know what you are doing, it can
be nice to turn this on.

An arrow which knows the class will be highlighted with
`idlwave-class-arrow-face'.  The command \\[idlwave-routine-info]
displays (with prefix arg: deletes) the class stored on the arrow
at point."
  :group 'idlwave-routine-info-and-completion
  :type 'boolean)

(defcustom idlwave-class-arrow-face 'bold
  "*Face to highlight object operator arrows `->' which carry a class property.
When IDLWAVE stores a class name as text property on an object arrow
(see variable `idlwave-store-inquired-class', it highlights the arrow
with this font in order to remind the user that this arrow is special."
  :group 'idlwave-routine-info-and-completion
  :type 'symbol)

(defcustom idlwave-resize-routine-help-window t
  "*Non-nil means, resize the Routine-info *Help* window to fit the content."
  :group 'idlwave-routine-info-and-completion
  :type 'boolean)

(defcustom idlwave-keyword-completion-adds-equal t
  "*Non-nil means, completion automatically adds `=' after completed keywords."
  :group 'idlwave-routine-info
  :type 'boolean)

(defcustom idlwave-function-completion-adds-paren t
  "*Non-nil means, completion automatically adds `(' after completed function.
Nil means, don't add anything.
A value of `2' means, also add the closing parenthesis and position cursor
between the two."
  :group 'idlwave-routine-info
  :type '(choice (const :tag "Nothing" nil)
		 (const :tag "(" t)
		 (const :tag "()" 2)))

(defcustom idlwave-completion-restore-window-configuration t
  "*Non-nil means, try to restore the window configuration after completion.
When completion is not unique, Emacs displays a list of completions.
This messes up your window configuration.  With this variable set, IDLWAVE
restores the old configuration after successful completion."
  :group 'idlwave-routine-info-and-completion
  :type 'boolean)

;;; Variables for abbrev and action behavior -----------------------------

(defgroup idlwave-abbrev-and-indent-action nil
  "IDLWAVE performs actions when expanding abbreviations or indenting lines.
The variables in this group govern this."
  :group 'idlwave)

(defcustom idlwave-do-actions nil
  "*Non-nil means performs actions when indenting.
The actions that can be performed are listed in `idlwave-indent-action-table'."
  :group 'idlwave-abbrev-and-indent-action
  :type 'boolean)

(defcustom idlwave-abbrev-start-char "\\"
  "*A single character string used to start abbreviations in abbrev mode.
Possible characters to chose from: ~`\%
or even '?'.  '.' is not a good choice because it can make structure
field names act like abbrevs in certain circumstances.

Changes to this in `idlwave-mode-hook' will have no effect.  Instead a user
must set it directly using `setq' in the .emacs file before idlwave.el
is loaded."
  :group 'idlwave-abbrev-and-indent-action
  :type 'string)

(defcustom idlwave-surround-by-blank nil
  "*Non-nil means, enable `idlwave-surround'.
If non-nil, `=',`<',`>',`&',`,', `->' are surrounded with spaces by
`idlwave-surround'.
See help for `idlwave-indent-action-table' for symbols using `idlwave-surround'.

Also see the default key bindings for keys using `idlwave-surround'.
Keys are bound and made into actions calling `idlwave-surround' with
`idlwave-action-and-binding'.
See help for `idlwave-action-and-binding' for examples.

Also see help for `idlwave-surround'."
  :group 'idlwave-abbrev-and-indent-action
  :type 'boolean)

(defcustom idlwave-pad-keyword t
  "*Non-nil means pad '=' for keywords like assignments.
Whenever `idlwave-surround' is non-nil then this affects how '=' is padded
for keywords.  If non-nil it is padded the same as for assignments.
If nil then spaces are removed."
  :group 'idlwave-abbrev-and-indent-action
  :type 'boolean)

(defcustom idlwave-show-block t
  "*Non-nil means point blinks to block beginning for `idlwave-show-begin'."
  :group 'idlwave-abbrev-and-indent-action
  :type 'boolean)

(defcustom idlwave-expand-generic-end nil
  "*Non-nil means expand generic END to ENDIF/ENDELSE/ENDWHILE etc."
  :group 'idlwave-abbrev-and-indent-action
  :type 'boolean)

(defcustom idlwave-abbrev-move t
  "*Non-nil means the abbrev hook can move point.
Set to nil by `idlwave-expand-region-abbrevs'. To see the abbrev
definitions, use the command `list-abbrevs', for abbrevs that move
point. Moving point is useful, for example, to place point between
parentheses of expanded functions.

See `idlwave-check-abbrev'."
  :group 'idlwave-abbrev-and-indent-action
  :type 'boolean)

(defcustom idlwave-abbrev-change-case nil
  "*Non-nil means all abbrevs will be forced to either upper or lower case.
If the value t, all expanded abbrevs will be upper case.
If the value is 'down then abbrevs will be forced to lower case.
If nil, the case will not change.
If `idlwave-reserved-word-upcase' is non-nil, reserved words will always be
upper case, regardless of this variable."
  :group 'idlwave-abbrev-and-indent-action
  :type 'boolean)

(defcustom idlwave-reserved-word-upcase nil
  "*Non-nil means, reserved words will be made upper case via abbrev expansion.
If nil case of reserved words is controlled by `idlwave-abbrev-change-case'.
Has effect only if in abbrev-mode."
  :group 'idlwave-abbrev-and-indent-action
  :type 'boolean)

;;; Action/Expand Tables.
;;
;; The average user may have difficulty modifying this directly.  It
;; can be modified/set in idlwave-mode-hook, but it is easier to use
;; idlwave-action-and-binding. See help for idlwave-action-and-binding for
;; examples of how to add an action.
;;
;; The action table is used by `idlwave-indent-line' whereas both the
;; action and expand tables are used by `idlwave-indent-and-action'.  In
;; general, the expand table is only used when a line is explicitly
;; indented.  Whereas, in addition to being used when the expand table
;; is used, the action table is used when a line is indirectly
;; indented via line splitting, auto-filling or a new line creation.
;;
;; Example actions:
;;
;;  Capitalize system vars
;;   (idlwave-action-and-binding idlwave-sysvar '(capitalize-word 1) t)
;;
;;  Capitalize procedure name
;;   (idlwave-action-and-binding "\\<\\(pro\\|function\\)\\>[ \t]*\\<"
;;                           '(capitalize-word 1) t)
;;
;;  Capitalize common block name
;;   (idlwave-action-and-binding "\\<common\\>[ \t]+\\<"
;;                           '(capitalize-word 1) t)
;;  Capitalize label
;;   (idlwave-action-and-binding (concat "^[ \t]*" idlwave-label)
;;                           '(capitalize-word -1) t)

(defvar idlwave-indent-action-table nil
  "*Associated array containing action lists of search string (car),
and function as a cdr. This table is used by `idlwave-indent-line'.
See documentation for `idlwave-do-action' for a complete description of
the action lists.

Additions to the table are made with `idlwave-action-and-binding' when a
binding is not requested.
See help on `idlwave-action-and-binding' for examples.")

(defvar idlwave-indent-expand-table nil
  "*Associated array containing action lists of search string (car),
and function as a cdr. The table is used by the
`idlwave-indent-and-action' function. See documentation for
`idlwave-do-action' for a complete description of the action lists.

Additions to the table are made with `idlwave-action-and-binding' when a
binding is requested.
See help on `idlwave-action-and-binding' for examples.")

;;; Documentation header and history keyword ---------------------------------

(defgroup idlwave-documentation nil
  "Options for documenting IDLWAVE files."
  :group 'idlwave)

;; FIXME: make defcustom?
(defvar idlwave-file-header
  (list nil
        ";+
; NAME:
;
;
;
; PURPOSE:
;
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;
;
;
; INPUTS:
;
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;-
")
  "*A list (PATHNAME STRING) specifying the doc-header template to use for
summarizing a file. If PATHNAME is non-nil then this file will be included.
Otherwise STRING is used. If NIL, the file summary will be omitted.
For example you might set PATHNAME to the path for the
lib_template.pro file included in the IDL distribution.")

(defcustom idlwave-timestamp-hook 'idlwave-default-insert-timestamp
  "*The hook function used to update the timestamp of a function."
  :group 'idlwave-documentation
  :type 'function)

(defcustom idlwave-doc-modifications-keyword "HISTORY"
  "*The modifications keyword to use with the log documentation commands.
A ':' is added to the keyword end.
Inserted by doc-header and used to position logs by doc-modification.
If nil it will not be inserted."
  :group 'idlwave-documentation
  :type 'string)

(defcustom idlwave-doclib-start "^;+\\+"
  "*Regexp matching the start of a document library header."
  :group 'idlwave-documentation
  :type 'regexp)

(defcustom idlwave-doclib-end "^;+-"
  "*Regexp matching the end of a document library header."
  :group 'idlwave-documentation
  :type 'regexp)

;;; External Programs -------------------------------------------------------

(defgroup idlwave-external-programs nil
  "Miscellaneous options for IDLWAVE mode."
  :group 'idlwave)

;; WARNING: The following variable has recently been moved from
;; idlw-shell.el to this file.  I hope this does not break
;; anything.

(defcustom idlwave-shell-explicit-file-name "idl"
  "*If non-nil, is the command to run IDL.
Should be an absolute file path or path relative to the current environment
execution search path."
  :group 'idlwave-external-programs
  :type 'string)

;; FIXME: Document a case when is this needed.
(defcustom idlwave-shell-command-line-options nil
  "*A list of command line options for calling the IDL program."
  :type '(repeat (string :value ""))
  :group 'idlwave-external-programs)

(defcustom idlwave-help-application "idlhelp"
  "*The external application providing reference help for programming."
  :group 'idlwave-external-programs
  :type 'string)

;;; Miscellaneous variables -------------------------------------------------

(defgroup idlwave-misc nil
  "Miscellaneous options for IDLWAVE mode."
  :group 'idlwave)

(defcustom idlwave-startup-message t
  "*Non-nil displays a startup message when `idlwave-mode' is first called."
  :group 'idlwave-misc
  :type 'boolean)

(defcustom idlwave-default-font-lock-items 
  '(pros-and-functions batch-files idl-keywords label goto
		       common-blocks class-arrows)
  "Items which should be fontified on the default fontification level 2.
IDLWAVE defines 3 levels of fontification.  Level 1 is very little, level 3
is everything and level 2 is specified by this list.
This variable must be set before IDLWAVE gets loaded.  It is
a list of symbols, the following symbols are allowed.

pros-and-functions   Procedure and Function definitions
batch-files          Batch Files
idl-keywords         IDL Keywords
label                Statement Labels
goto                 Goto Statements
common-blocks        Common Blocks
keyword-parameters   Keyword Parameters in routine definitions and calls
system-variables     System Variables
fixme                FIXME: Warning in comments (on XEmacs only v. 21.0 and up)
class-arrows         Object Arrows with class property"
  :group 'idlwave-misc
  :type '(set
	  :inline t :greedy t
	  (const :tag "Procedure and Function definitions" pros-and-functions)
	  (const :tag "Batch Files"                        batch-files)
	  (const :tag "IDL Keywords (reserved words)"      idl-keywords)
	  (const :tag "Statement Labels"                   label)
	  (const :tag "Goto Statements"                    goto)
	  (const :tag "Common Blocks"                      common-blocks)
	  (const :tag "Keyword Parameters"                 keyword-parameters)
	  (const :tag "System Variables"                   system-variables)
	  (const :tag "FIXME: Warning"                     fixme)
	  (const :tag "Object Arrows with class property " class-arrows)))

(defcustom idlwave-mode-hook nil
  "Normal hook.  Executed when a buffer is put into `idlwave-mode'."
  :group 'idlwave-misc
  :type 'hook)

(defcustom idlwave-load-hook nil
  "Normal hook.  Executed when idlwave.el is loaded."
  :group 'idlwave-misc
  :type 'hook)

;;;
;;; End customization variables section
;;;

;;; Non customization variables

;;; font-lock mode - Additions by Phil Williams, Ulrik Dickow and
;;; Simon Marshall <simon@gnu.ai.mit.edu>
;;; and Carsten Dominik...

(defconst idlwave-font-lock-keywords-1 nil
  "Subdued level highlighting for IDLWAVE mode.")

(defconst idlwave-font-lock-keywords-2 nil
  "Medium level highlighting for IDLWAVE mode.")

(defconst idlwave-font-lock-keywords-3 nil
  "Gaudy level highlighting for IDLWAVE mode.")

(let* ((oldp (or (string-match "Lucid" emacs-version)
		 (not (boundp 'emacs-minor-version))
		 (and (<= emacs-major-version 19) 
		      (<= emacs-minor-version 29))))

       ;; The following are the reserved words in IDL.  Maybe we should
       ;; highlight some more stuff as well?       
       (idl-keywords
	;; To update this regexp, update the list of keywords and 
	;; evaluate the form.
;	(insert 
;	 (concat 
;	  "\"\\\\<"
;	  (regexp-opt 
;	   '("and" "or" "xor" "not"
;	     "eq" "ge" "gt" "le" "lt" "ne" 
;	     "for" "do" "endfor"
;	     "if" "then" "endif" "else" "endelse" 
;	     "case" "of" "endcase"
;	     "begin" "end"
;	     "repeat" "until" "endrep"
;	     "while" "endwhile" 
;	     "goto" "return"
;	     "inherits" "mod"
;	     "on_error" "on_ioerror"))  ; on_error is not officially reserved
;	  "\\\\>\""))
	(concat	"\\<\\("
		"and\\|begin\\|case\\|do\\|e\\(lse\\|nd\\(case\\|else\\|"
		"for\\|if\\|rep\\|while\\)?\\|q\\)\\|for\\|g\\(oto\\|[et]\\)"
		"\\|i\\(f\\|nherits\\)\\|l[et]\\|mod\\|n\\(e\\|ot\\)\\|"
		"o\\(n_ioerror\\|[fr]\\)\\|re\\(peat\\|turn\\)\\|then\\|"
		"until\\|while\\|xor"
		"\\)\\>"))

       ;; Procedure declarations.  Fontify keyword plus procedure name.
       ;; Function  declarations.  Fontify keyword plus function  name.
       (pros-and-functions
	'("\\<\\(function\\|pro\\)\\>[ \t]+\\(\\sw+\\(::\\sw+\\)?\\)"
	  (1 font-lock-keyword-face)
	  (2 font-lock-function-name-face nil t)))

       ;; Common blocks
       (common-blocks
	'("\\<\\(common\\)\\>[ \t]*\\(\\sw+\\)?[ \t]*,?"
	  (1 font-lock-keyword-face)	          ; "common"
	  (2 font-lock-reference-face nil t)      ; block name
	  (font-lock-match-c++-style-declaration-item-and-skip-to-next
	   ;; Start with point after block name and comma
	   (goto-char (match-end 0))  ; needed for XEmacs, could be nil 
	   nil
	   (1 font-lock-variable-name-face)       ; variable names
	   )))

       ;; Batch files
       (batch-files
	'("^[ \t]*\\(@[^ \t\n]+\\)" (1 font-lock-string-face)))

       ;; FIXME warning.
       (fixme
	'("\\<FIXME:" (0 font-lock-warning-face t)))

       ;; Labels
       (label
	'("^[ \t]*\\([a-zA-Z]\\sw*:\\)" (1 font-lock-reference-face)))

       ;; The goto statement and its label
       (goto
	'("\\(goto\\)[ \t]*,[ \t]*\\([a-zA-Z]\\sw*\\)"
	  (1 font-lock-keyword-face)
	  (2 font-lock-reference-face)))

       ;; Named parameters, like /xlog or ,xrange=[]
       ;; This is anchored to the comma preceeding the keyword.
       ;; Treats continuation lines, works only during whole buffer
       ;; fontification.  Slow, use it only in fancy fontification.
       (keyword-parameters
	'("[(,][ \t]*\\(\\$[ \t]*\\(;.*\\)?\\(\n[ \t]*;.*\\)*\n[ \t]*\\)?\\(/[a-zA-Z_]\\sw*\\|[a-zA-Z_]\\sw*[ \t]*=\\)"
	  (4 font-lock-reference-face)))

       ;; System variables start with a bang.
       (system-variables
	'("\\(![a-zA-Z_]+\\(\\.\\sw+\\)?\\)"
	  (1 font-lock-variable-name-face)))

       ;; Special and unusual operators (not used because too noisy)
       (special-operators
	'("[<>#]" (0 font-lock-keyword-face)))

       ;; All operators (not used because too noisy)
       (all-operators
	'("[-*^#+<>/]" (0 font-lock-keyword-face)))
	
       ;; Arrows with text property `idlwave-class'
       (class-arrows
	(list 'idlwave-match-class-arrows 
	      (list 0 (if (featurep 'xemacs) 
			  idlwave-class-arrow-face
			'idlwave-class-arrow-face))))

       )

  ;; The following lines are just a dummy to make the compiler shut up
  ;; about variables bound but not used.
  (setq oldp oldp
	idl-keywords idl-keywords
	pros-and-functions pros-and-functions
	common-blocks common-blocks
	batch-files batch-files
	fixme fixme
	label label
	goto goto
	keyword-parameters keyword-parameters
	system-variables system-variables
	special-operators special-operators
	all-operators all-operators
	class-arrows class-arrows)

  (setq idlwave-font-lock-keywords-1
	(list pros-and-functions
	      batch-files
	      ))

  (setq idlwave-font-lock-keywords-2
	(mapcar 'symbol-value idlwave-default-font-lock-items))

  (setq idlwave-font-lock-keywords-3 
	(list pros-and-functions
	      batch-files
	      idl-keywords
	      label goto
	      common-blocks
	      keyword-parameters
	      system-variables
	      class-arrows
	      ))
  )

(defun idlwave-match-class-arrows (limit)
  ;; Match an object arrow with class property
  (and idlwave-store-inquired-class
       (re-search-forward "->" limit 'limit)
       (get-text-property (match-beginning 0) 'idlwave-class)))

(defvar idlwave-font-lock-keywords idlwave-font-lock-keywords-2
  "Default expressions to highlight in IDLWAVE mode.")

(defvar idlwave-font-lock-defaults
  '((idlwave-font-lock-keywords
     idlwave-font-lock-keywords-1 
     idlwave-font-lock-keywords-2
     idlwave-font-lock-keywords-3)
    nil t 
    ((?$ . "w") (?_ . "w") (?. . "w")) 
    beginning-of-line))

(put 'idlwave-mode 'font-lock-defaults 
     idlwave-font-lock-defaults) ; XEmacs

(defconst idlwave-comment-line-start-skip "^[ \t]*;"
  "Regexp to match the start of a full-line comment.
That is the _beginning_ of a line containing a comment delimiter `;' preceded
only by whitespace.")

(defconst idlwave-begin-block-reg "\\<\\(pro\\|function\\|begin\\|case\\)\\>"
  "Regular expression to find the beginning of a block. The case does
not matter. The search skips matches in comments.")

(defconst idlwave-begin-unit-reg "\\<\\(pro\\|function\\)\\>\\|\\`"
  "Regular expression to find the beginning of a unit. The case does
not matter.")

(defconst idlwave-end-unit-reg "\\<\\(pro\\|function\\)\\>\\|\\'"
  "Regular expression to find the line that indicates the end of unit.
This line is the end of buffer or the start of another unit. The case does
not matter. The search skips matches in comments.")

(defconst idlwave-continue-line-reg "\\<\\$"
  "Regular expression to match a continued line.")

(defconst idlwave-end-block-reg
  "\\<end\\(\\|case\\|else\\|for\\|if\\|rep\\|while\\)\\>"
  "Regular expression to find the end of a block. The case does
not matter. The search skips matches found in comments.")

(defconst idlwave-block-matches
  '(("pro"      . "end")
    ("function" . "end")
    ("case"     . "endcase")
    ("else"     . "endelse")
    ("for"      . "endfor")
    ("then"     . "endif")
    ("repeat"   . "endrep")
    ("while"    . "endwhile"))
  "Matches between statements and the corresponding END variant.
The cars are the reserved words starting a block.  If the block really
begins with BEGIN, the cars are the reserved words before the begin
which can be used to identify the block type.
This is used to check for the correct END type, to close blocks and
to expand generic end statements to their detailed form.")

(defconst idlwave-block-match-regexp
  "\\<\\(else\\|for\\|then\\|repeat\\|while\\)\\>"
"Regular expression matching reserved words which can stand before
blocks starting with a BEGIN statement.  The matches must have associations
`idlwave-block-matches'")

(defconst idlwave-identifier "[a-zA-Z][a-zA-Z0-9$_]*"
  "Regular expression matching an IDL identifier.")

(defconst idlwave-sysvar (concat "!" idlwave-identifier)
  "Regular expression matching IDL system variables.")

(defconst idlwave-variable (concat idlwave-identifier "\\|" idlwave-sysvar)
  "Regular expression matching IDL variable names.")

(defconst idlwave-label (concat idlwave-identifier ":")
  "Regular expression matching IDL labels.")

(defconst idlwave-statement-match
  (list
   ;; "endif else" is the the only possible "end" that can be
   ;; followed by a statement on the same line.
   '(endelse . ("end\\(\\|if\\)\\s +else" "end\\(\\|if\\)\\s +else"))
   ;; all other "end"s can not be followed by a statement.
   (cons 'end (list idlwave-end-block-reg nil))
   '(if . ("if\\>" "then"))
   '(for . ("for\\>" "do"))
   '(begin . ("begin\\>" nil))
   '(pdef . ("pro\\>\\|function\\>" nil))
   '(while . ("while\\>" "do"))
   '(repeat . ("repeat\\>" "repeat"))
   '(goto . ("goto\\>" nil))
   '(case . ("case\\>" nil))
   (cons 'call (list (concat idlwave-identifier "\\(\\s *$\\|\\s *,\\)") nil))
   '(assign . ("[^=>\n]*=" nil)))
  
  "Associated list of statement matching regular expressions.
Each regular expression matches the start of an IDL statement.  The
first element of each association is a symbol giving the statement
type.  The associated value is a list.  The first element of this list
is a regular expression matching the start of an IDL statement for
identifying the statement type.  The second element of this list is a
regular expression for finding a substatement for the type.  The
substatement starts after the end of the found match modulo
whitespace.  If it is nil then the statement has no substatement.  The
list order matters since matching an assignment statement exactly is
not possible without parsing.  Thus assignment statement become just
the leftover unidentified statements containing and equal sign. "  )

(defvar idlwave-fill-function 'auto-fill-function
  "IDL mode auto fill function.")

(defvar idlwave-comment-indent-function 'comment-indent-function
  "IDL mode comment indent function.")

;; Note that this is documented in the v18 manuals as being a string
;; of length one rather than a single character.
;; The code in this file accepts either format for compatibility.
(defvar idlwave-comment-indent-char ?\ 
  "Character to be inserted for IDL comment indentation.
Normally a space.")

(defconst idlwave-continuation-char ?$
  "Character which is inserted as a last character on previous line by
   \\[idlwave-split-line] to begin a continuation line.  Normally $.")

(defconst idlwave-mode-version " 3.15")

(defmacro idlwave-keyword-abbrev (&rest args)
  "Creates a function for abbrev hooks to call `idlwave-check-abbrev' with args."
  (` (quote (lambda ()
              (, (append '(idlwave-check-abbrev) args))))))

;; If I take the time I can replace idlwave-keyword-abbrev with
;; idlwave-code-abbrev and remove the quoted abbrev check from
;; idlwave-check-abbrev.  Then, e.g, (idlwave-keyword-abbrev 0 t) becomes
;; (idlwave-code-abbrev idlwave-check-abbrev 0 t).  In fact I should change
;; the name of idlwave-check-abbrev to something like idlwave-modify-abbrev.

(defmacro idlwave-code-abbrev (&rest args)
  "Creates a function for abbrev hooks that ensures abbrevs are not quoted.
Specifically, if the abbrev is in a comment or string it is unexpanded.
Otherwise ARGS forms a list that is evaluated."
  (` (quote (lambda ()
	      (, (prin1-to-string args))  ;; Puts the code in the doc string
              (if (idlwave-quoted) (progn (unexpand-abbrev) nil)
                (, (append args)))))))

(defvar idlwave-mode-map (make-sparse-keymap)
  "Keymap used in IDL mode.")

(defvar idlwave-mode-syntax-table (make-syntax-table)
  "Syntax table in use in `idlwave-mode' buffers.")

(modify-syntax-entry ?+   "."  idlwave-mode-syntax-table)
(modify-syntax-entry ?-   "."  idlwave-mode-syntax-table)
(modify-syntax-entry ?*   "."  idlwave-mode-syntax-table)
(modify-syntax-entry ?/   "."  idlwave-mode-syntax-table)
(modify-syntax-entry ?^   "."  idlwave-mode-syntax-table)
(modify-syntax-entry ?#   "."  idlwave-mode-syntax-table)
(modify-syntax-entry ?=   "."  idlwave-mode-syntax-table)
(modify-syntax-entry ?%   "."  idlwave-mode-syntax-table)
(modify-syntax-entry ?<   "."  idlwave-mode-syntax-table)
(modify-syntax-entry ?>   "."  idlwave-mode-syntax-table)
(modify-syntax-entry ?\'  "\"" idlwave-mode-syntax-table)
(modify-syntax-entry ?\"  "\"" idlwave-mode-syntax-table)
(modify-syntax-entry ?\\  "."  idlwave-mode-syntax-table)
(modify-syntax-entry ?_   "_"  idlwave-mode-syntax-table)
(modify-syntax-entry ?{   "(}" idlwave-mode-syntax-table)
(modify-syntax-entry ?}   "){" idlwave-mode-syntax-table)
(modify-syntax-entry ?$   "_"  idlwave-mode-syntax-table)
(modify-syntax-entry ?.   "."  idlwave-mode-syntax-table)
(modify-syntax-entry ?\;  "<"  idlwave-mode-syntax-table)
(modify-syntax-entry ?\n  ">"  idlwave-mode-syntax-table)
(modify-syntax-entry ?\f  ">"  idlwave-mode-syntax-table)

(defvar idlwave-find-symbol-syntax-table
  (copy-syntax-table idlwave-mode-syntax-table)
  "Syntax table that treats symbol characters as word characters.")

(modify-syntax-entry ?$   "w"  idlwave-find-symbol-syntax-table)
(modify-syntax-entry ?_   "w"  idlwave-find-symbol-syntax-table)

(defun idlwave-action-and-binding (key cmd &optional select)
  "KEY and CMD are made into a key binding and an indent action.
KEY is a string - same as for the `define-key' function.  CMD is a
function of no arguments or a list to be evaluated.  CMD is bound to
KEY in `idlwave-mode-map' by defining an anonymous function calling
`self-insert-command' followed by CMD.  If KEY contains more than one
character a binding will only be set if SELECT is 'both.

(KEY . CMD\ is also placed in the `idlwave-indent-expand-table',
replacing any previous value for KEY.  If a binding is not set then it
will instead be placed in `idlwave-indent-action-table'.

If the optional argument SELECT is nil then an action and binding are
created.  If SELECT is 'noaction, then a binding is always set and no
action is created.  If SELECT is 'both then an action and binding
will both be created even if KEY contains more than one character.
Otherwise, if SELECT is non-nil then only an action is created.

Some examples:
No spaces before and 1 after a comma
   (idlwave-action-and-binding \",\"  '(idlwave-surround 0 1))
A minimum of 1 space before and after `=' (see `idlwave-expand-equal').
   (idlwave-action-and-binding \"=\"  '(idlwave-expand-equal -1 -1))
Capitalize system variables - action only
   (idlwave-action-and-binding idlwave-sysvar '(capitalize-word 1) t)"
  (if (not (equal select 'noaction))
      ;; Add action
      (let* ((table (if select 'idlwave-indent-action-table
                      'idlwave-indent-expand-table))
             (cell (assoc key (eval table))))
        (if cell
            ;; Replace action command
            (setcdr cell cmd)
          ;; New action
          (set table (append (eval table) (list (cons key cmd)))))))
  ;; Make key binding for action
  (if (or (and (null select) (= (length key) 1))
          (equal select 'noaction)
          (equal select 'both))
      (define-key idlwave-mode-map key
        (append '(lambda ()
                            (interactive)
                            (self-insert-command 1))
                (list (if (listp cmd)
                          cmd
                        (list cmd)))))))

(fset 'idlwave-debug-map (make-sparse-keymap))

(define-key idlwave-mode-map "\C-c "    'idlwave-hard-tab)
;(define-key idlwave-mode-map "\C-c\C- " 'idlwave-hard-tab)
(define-key idlwave-mode-map "'"        'idlwave-show-matching-quote)
(define-key idlwave-mode-map "\""       'idlwave-show-matching-quote)
(define-key idlwave-mode-map "\C-c;"    'idlwave-toggle-comment-region)
(define-key idlwave-mode-map "\C-\M-a"  'idlwave-beginning-of-subprogram)
(define-key idlwave-mode-map "\C-\M-e"  'idlwave-end-of-subprogram)
(define-key idlwave-mode-map "\C-c{"    'idlwave-beginning-of-block)
(define-key idlwave-mode-map "\C-c}"    'idlwave-end-of-block)
(define-key idlwave-mode-map "\C-c]"    'idlwave-close-block)
(define-key idlwave-mode-map "\M-\C-h"  'idlwave-mark-subprogram)
(define-key idlwave-mode-map "\M-\C-n"  'idlwave-forward-block)
(define-key idlwave-mode-map "\M-\C-p"  'idlwave-backward-block)
(define-key idlwave-mode-map "\M-\C-d"  'idlwave-down-block)
(define-key idlwave-mode-map "\M-\C-u"  'idlwave-backward-up-block)
(define-key idlwave-mode-map "\M-\r"    'idlwave-split-line)
(define-key idlwave-mode-map "\M-\C-q"  'idlwave-indent-subprogram)
(define-key idlwave-mode-map "\C-c\C-p" 'idlwave-previous-statement)
(define-key idlwave-mode-map "\C-c\C-n" 'idlwave-next-statement)
;; (define-key idlwave-mode-map "\r"       'idlwave-newline)
;; (define-key idlwave-mode-map "\t"       'idlwave-indent-line)
(define-key idlwave-mode-map "\C-c\C-a" 'idlwave-auto-fill-mode)
(define-key idlwave-mode-map "\M-q"     'idlwave-fill-paragraph)
(define-key idlwave-mode-map "\M-s"     'idlwave-edit-in-idlde)
(define-key idlwave-mode-map "\C-c\C-h" 'idlwave-doc-header)
(define-key idlwave-mode-map "\C-c\C-m" 'idlwave-doc-modification)
(define-key idlwave-mode-map "\C-c\C-c" 'idlwave-case)
(define-key idlwave-mode-map "\C-c\C-d" 'idlwave-debug-map)
(define-key idlwave-mode-map "\C-c\C-d\C-c" 'idlwave-shell-save-and-run)
(define-key idlwave-mode-map "\C-c\C-f" 'idlwave-for)
;;  (define-key idlwave-mode-map "\C-c\C-f" 'idlwave-function)
;;  (define-key idlwave-mode-map "\C-c\C-p" 'idlwave-procedure)
(define-key idlwave-mode-map "\C-c\C-r" 'idlwave-repeat)
(define-key idlwave-mode-map "\C-c\C-w" 'idlwave-while)
(define-key idlwave-mode-map "\C-c\C-s" 'idlwave-shell)
(define-key idlwave-mode-map "\C-c\C-l" 'idlwave-shell-recenter-shell-window)
(autoload 'idlwave-shell "idlw-shell"
  "Run an inferior IDL, with I/O through buffer `(idlwave-shell-buffer)'." t)
(autoload 'idlwave-shell-send-command "idlw-shell")
(autoload 'idlwave-shell-recenter-shell-window "idlw-shell"
  "Run `idlwave-shell' and switch back to current window" t)
(autoload 'idlwave-shell-save-and-run "idlw-shell"
  "Save and run buffer under the shell." t)
(define-key idlwave-mode-map "\C-c\C-v"   'idlwave-find-module)
(define-key idlwave-mode-map "\C-c?"      'idlwave-routine-info)
(define-key idlwave-mode-map "\M-?"       'idlwave-routine-info-from-idlhelp)
(define-key idlwave-mode-map [(meta tab)] 'idlwave-complete)
(define-key idlwave-mode-map "\C-c\C-i"   'idlwave-update-routine-info)
(define-key idlwave-mode-map "\C-c="      'idlwave-resolve)

;; Set action and key bindings.
;; See description of the function `idlwave-action-and-binding'.
;; Automatically add spaces for the following characters
(idlwave-action-and-binding "&"  '(idlwave-surround -1 -1))
(idlwave-action-and-binding "<"  '(idlwave-surround -1 -1))
(idlwave-action-and-binding ">"  '(idlwave-surround -1 -1 '(?-)))
(idlwave-action-and-binding "->" '(idlwave-surround -1 -1 nil 2))
(idlwave-action-and-binding ","  '(idlwave-surround 0 -1))
;; Automatically add spaces to equal sign if not keyword
(idlwave-action-and-binding "="  '(idlwave-expand-equal -1 -1))

;;;
;;; Abbrev Section
;;;
;;; When expanding abbrevs and the abbrev hook moves backward, an extra
;;; space is inserted (this is the space typed by the user to expanded
;;; the abbrev).
;;;

(condition-case nil
    (modify-syntax-entry (string-to-char idlwave-abbrev-start-char) 
			 "w" idlwave-mode-syntax-table)
  (error nil))

(defvar idlwave-mode-abbrev-table nil
  "Abbreviation table used for IDLWAVE mode")
(define-abbrev-table 'idlwave-mode-abbrev-table ())
(let ((abbrevs-changed nil)          ;; mask the current value to avoid save
      (tb idlwave-mode-abbrev-table)
      (c idlwave-abbrev-start-char))
  ;;
  ;; Templates
  ;;
  (define-abbrev tb (concat c "c")   "" (idlwave-code-abbrev idlwave-case))
  (define-abbrev tb (concat c "f")   "" (idlwave-code-abbrev idlwave-for))
  (define-abbrev tb (concat c "fu")  "" (idlwave-code-abbrev idlwave-function))
  (define-abbrev tb (concat c "pr")  "" (idlwave-code-abbrev idlwave-procedure))
  (define-abbrev tb (concat c "r")   "" (idlwave-code-abbrev idlwave-repeat))
  (define-abbrev tb (concat c "w")   "" (idlwave-code-abbrev idlwave-while))
  (define-abbrev tb (concat c "i")   "" (idlwave-code-abbrev idlwave-if))
  (define-abbrev tb (concat c "elif") "" (idlwave-code-abbrev idlwave-elif))
  ;;
  ;; Keywords, system functions, conversion routines
  ;;
  (define-abbrev tb (concat c "b")  "begin"        (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb (concat c "co") "common"       (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb (concat c "cb") "byte()"       (idlwave-keyword-abbrev 1))
  (define-abbrev tb (concat c "cx") "fix()"        (idlwave-keyword-abbrev 1))
  (define-abbrev tb (concat c "cl") "long()"       (idlwave-keyword-abbrev 1))
  (define-abbrev tb (concat c "cf") "float()"      (idlwave-keyword-abbrev 1))
  (define-abbrev tb (concat c "cs") "string()"     (idlwave-keyword-abbrev 1))
  (define-abbrev tb (concat c "cc") "complex()"    (idlwave-keyword-abbrev 1))
  (define-abbrev tb (concat c "cd") "double()"     (idlwave-keyword-abbrev 1))
  (define-abbrev tb (concat c "e")  "else"         (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb (concat c "ec") "endcase"      'idlwave-show-begin)
  (define-abbrev tb (concat c "ee") "endelse"      'idlwave-show-begin)
  (define-abbrev tb (concat c "ef") "endfor"       'idlwave-show-begin)
  (define-abbrev tb (concat c "ei") "endif else if" 'idlwave-show-begin)
  (define-abbrev tb (concat c "el") "endif else"   'idlwave-show-begin)
  (define-abbrev tb (concat c "en") "endif"        'idlwave-show-begin)
  (define-abbrev tb (concat c "er") "endrep"       'idlwave-show-begin)
  (define-abbrev tb (concat c "ew") "endwhile"     'idlwave-show-begin)
  (define-abbrev tb (concat c "g")  "goto,"        (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb (concat c "h")  "help,"        (idlwave-keyword-abbrev 0))
  (define-abbrev tb (concat c "k")  "keyword_set()" (idlwave-keyword-abbrev 1))
  (define-abbrev tb (concat c "n")  "n_elements()" (idlwave-keyword-abbrev 1))
  (define-abbrev tb (concat c "on") "on_error,"    (idlwave-keyword-abbrev 0))
  (define-abbrev tb (concat c "oi") "on_ioerror,"  (idlwave-keyword-abbrev 0 1))
  (define-abbrev tb (concat c "ow") "openw,"       (idlwave-keyword-abbrev 0))
  (define-abbrev tb (concat c "or") "openr,"       (idlwave-keyword-abbrev 0))
  (define-abbrev tb (concat c "ou") "openu,"       (idlwave-keyword-abbrev 0))
  (define-abbrev tb (concat c "p")  "print,"       (idlwave-keyword-abbrev 0))
  (define-abbrev tb (concat c "pt") "plot,"        (idlwave-keyword-abbrev 0))
  (define-abbrev tb (concat c "re") "read,"        (idlwave-keyword-abbrev 0))
  (define-abbrev tb (concat c "rf") "readf,"       (idlwave-keyword-abbrev 0))
  (define-abbrev tb (concat c "ru") "readu,"       (idlwave-keyword-abbrev 0))
  (define-abbrev tb (concat c "rt") "return"       (idlwave-keyword-abbrev 0))
  (define-abbrev tb (concat c "sc") "strcompress()" (idlwave-keyword-abbrev 1))
  (define-abbrev tb (concat c "sn") "strlen()"     (idlwave-keyword-abbrev 1))
  (define-abbrev tb (concat c "sl") "strlowcase()" (idlwave-keyword-abbrev 1))
  (define-abbrev tb (concat c "su") "strupcase()"  (idlwave-keyword-abbrev 1))
  (define-abbrev tb (concat c "sm") "strmid()"     (idlwave-keyword-abbrev 1))
  (define-abbrev tb (concat c "sp") "strpos()"     (idlwave-keyword-abbrev 1))
  (define-abbrev tb (concat c "st") "strput()"     (idlwave-keyword-abbrev 1))
  (define-abbrev tb (concat c "sr") "strtrim()"    (idlwave-keyword-abbrev 1))
  (define-abbrev tb (concat c "t")  "then"         (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb (concat c "u")  "until"        (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb (concat c "wu") "writeu,"      (idlwave-keyword-abbrev 0))
  (define-abbrev tb (concat c "ine") "if n_elements() eq 0 then"
    (idlwave-keyword-abbrev 11))
  (define-abbrev tb (concat c "inn") "if n_elements() ne 0 then"
    (idlwave-keyword-abbrev 11))
  (define-abbrev tb (concat c "np") "n_params()"   (idlwave-keyword-abbrev 0))
  (define-abbrev tb (concat c "s")  "size()"       (idlwave-keyword-abbrev 1))
  (define-abbrev tb (concat c "wi") "widget_info()" (idlwave-keyword-abbrev 1))
  (define-abbrev tb (concat c "wc") "widget_control," (idlwave-keyword-abbrev 0))
  
  ;; This section is reserved words only. (From IDL user manual)
  ;;
  (define-abbrev tb "and"        "and"        (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "begin"      "begin"      (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "case"       "case"       (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "common"     "common"     (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "do"         "do"         (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "else"       "else"       (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "end"        "end"        'idlwave-show-begin-check)
  (define-abbrev tb "endcase"    "endcase"    'idlwave-show-begin-check)
  (define-abbrev tb "endelse"    "endelse"    'idlwave-show-begin-check)
  (define-abbrev tb "endfor"     "endfor"     'idlwave-show-begin-check)
  (define-abbrev tb "endif"      "endif"      'idlwave-show-begin-check)
  (define-abbrev tb "endrep"     "endrep"     'idlwave-show-begin-check)
  (define-abbrev tb "endwhi"     "endwhi"     'idlwave-show-begin-check)
  (define-abbrev tb "endwhile"   "endwhile"   'idlwave-show-begin-check)
  (define-abbrev tb "eq"         "eq"         (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "for"        "for"        (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "function"   "function"   (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "ge"         "ge"         (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "goto"       "goto"       (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "gt"         "gt"         (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "if"         "if"         (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "le"         "le"         (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "lt"         "lt"         (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "mod"        "mod"        (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "ne"         "ne"         (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "not"        "not"        (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "of"         "of"         (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "on_ioerror" "on_ioerror" (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "or"         "or"         (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "pro"        "pro"        (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "repeat"     "repeat"     (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "then"       "then"       (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "until"      "until"      (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "while"      "while"      (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "xor"        "xor"        (idlwave-keyword-abbrev 0 t)))

(defvar imenu-create-index-function)
(defvar extract-index-name-function)
(defvar prev-index-position-function)
(defvar imenu-extract-index-name-function)
(defvar imenu-prev-index-position-function)
;; defined later - so just make the compiler shut up
(defvar idlwave-mode-menu)  
(defvar idlwave-mode-debug-menu)

;;;###autoload
(defun idlwave-mode ()
  "Major mode for editing IDL and WAVE CL .pro files.

The main features of this mode are

1. Indentation and Formatting
   --------------------------
   Like other Emacs programming modes, C-j inserts a newline and indents.
   TAB is used for explicit indentation of the current line.

   To start a continuation line, use \\[idlwave-split-line].  This function can also
   be used in the middle of a line to split the line at that point.
   When used inside a long constant string, the string is split at
   that point with the `+' concatenation operator.

   Comments are indented as follows:

   `;;;' Indentation remains unchanged.
   `;;'  Indent like the surrounding code
   `;'   Indent to a minimum column.

   The indentation of comments starting in column 0 is never changed.

   Use \\[idlwave-fill-paragraph] to refill a paragraph inside a comment.  The indentation
   of the second line of the paragraph relative to the first will be
   retained.  Use \\[idlwave-auto-fill-mode] to toggle auto-fill mode for these comments.
   When the variable `idlwave-fill-comment-line-only' is nil, code
   can also be auto-filled and auto-indented (not recommended).

   To convert pre-existing IDL code to your formatting style, mark the
   entire buffer with \\[mark-whole-buffer] and execute \\[idlwave-expand-region-abbrevs].
   Then mark the entire buffer again followed by \\[indent-region] (`indent-region').

2. Routine Info
   ------------
   IDLWAVE displays information about the calling sequence and the accepted
   keyword parameters of a procedure or function with \\[idlwave-routine-info].
   \\[idlwave-find-module] jumps to the source file of a module.
   These commands know about system routines, all routines in idlwave-mode
   buffers and (when the idlwave-shell is active) about all modules
   currently compiled under this shell.  Use \\[idlwave-update-routine-info] to update this
   information, which is also used for completion (see next item).

3. Completion
   ----------
   \\[idlwave-complete] completes the names of procedures, functions and
   keyword parameters.  It is context sensitive and figures out what
   is expected at point (procedure/function/keyword).  Lower case
   strings are completed in lower case, other strings in mixed or
   upper case.

4. Code Templates and Abbreviations
   --------------------------------
   Many Abbreviations are predefined to expand to code fragments and templates.
   The abbreviations start generally with a `\\`.  Some examples

   \\pr        PROCEDURE template
   \\fu        FUNCTION template
   \\c         CASE statement template
   \\f         FOR loop template
   \\r         REPEAT Loop template
   \\w         WHILE loop template
   \\i         IF statement template
   \\elif      IF-ELSE statement template
   \\b         BEGIN
   
   For a full list, use \\[idlwave-list-abbrevs].  Some templates also have
   direct keybindings - see the list of keybindings below.

   \\[idlwave-doc-header] inserts a documentation header at the beginning of the
   current program unit (pro, function or main).  Change log entries
   can be added to the current program unit with \\[idlwave-doc-modification].

5. Automatic Case Conversion
   -------------------------
   The case of reserved words and some abbrevs is controlled by
   `idlwave-reserved-word-upcase' and `idlwave-abbrev-change-case'.

6. Automatic END completion
   ------------------------
   If the variable `idlwave-expand-generic-end' is non-nil, each END typed
   will be converted to the specific version, like ENDIF, ENDFOR, etc.

7. Hooks
   -----
   Loading idlwave.el runs `idlwave-load-hook'.
   Turning on `idlwave-mode' runs `idlwave-mode-hook'.

8. Documentation and Customization
   -------------------------------
   Info documentation for this package is available.  Use \\[idlwave-info]
   to display (complain to your sysadmin if that does not work).
   For Postscript and HTML versions of the documentation, check IDLWAVE's
   homepage at `http://www.strw.leidenuniv.nl/~dominik/Tools/idlwave'.
   IDLWAVE has customize support - see the group `idlwave'.

9. Keybindings
   -----------
   Here is a list of all keybindings of this mode.
   If some of the key bindings below show with ??, use \\[describe-key]
   followed by the key sequence to see what the key sequence does.

\\{idlwave-mode-map}"

  (interactive)
  (kill-all-local-variables)
  
  (if idlwave-startup-message
      (message "Emacs IDLWAVE mode version %s." idlwave-mode-version))
  (setq idlwave-startup-message nil)
  
  (setq local-abbrev-table idlwave-mode-abbrev-table)
  (set-syntax-table idlwave-mode-syntax-table)
  
  (set (make-local-variable 'indent-line-function) 'idlwave-indent-and-action)
  
  (make-local-variable idlwave-comment-indent-function)
  (set idlwave-comment-indent-function 'idlwave-comment-hook)
  
  (set (make-local-variable 'comment-start-skip) ";+[ \t]*")
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'abbrev-all-caps) t)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'completion-ignore-case) t)
  
  (use-local-map idlwave-mode-map)

  (when (featurep 'easymenu)
    (easy-menu-add idlwave-mode-menu idlwave-mode-map)
    (easy-menu-add idlwave-mode-debug-menu idlwave-mode-map))

  (setq mode-name "IDLWAVE")
  (setq major-mode 'idlwave-mode)
  (setq abbrev-mode t)
  
  (set (make-local-variable idlwave-fill-function) 'idlwave-auto-fill)
  (setq comment-end "")
  (set (make-local-variable 'comment-multi-line) nil)
  (set (make-local-variable 'paragraph-separate) "[ \t\f]*$\\|[ \t]*;+[ \t]*$")
  (set (make-local-variable 'paragraph-start) "[ \t\f]\\|[ \t]*;+[ \t]")
  (set (make-local-variable 'paragraph-ignore-fill-prefix) nil)
  (set (make-local-variable 'parse-sexp-ignore-comments) nil)
  
  ;; Set tag table list to use IDLTAGS as file name.
  (if (boundp 'tag-table-alist)
      (add-to-list 'tag-table-alist '("\\.pro$" . "IDLTAGS")))
  
  ;; Font-lock additions - originally Phil Williams, then Ulrik Dickow
  ;; Following line is for Emacs - XEmacs uses the corresponding porperty
  ;; on the `idlwave-mode' symbol.
  (set (make-local-variable 'font-lock-defaults) idlwave-font-lock-defaults)

  ;; Imenu setup
  (set (make-local-variable 'imenu-create-index-function)
       'imenu-default-create-index-function)
  (set (make-local-variable 'imenu-extract-index-name-function)
       'idlwave-unit-name)
  (set (make-local-variable 'imenu-prev-index-position-function)
       'idlwave-prev-index-position)

  ;; Make a local post-command-hook and add our hook to it
  (make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook 'idlwave-command-hook nil t)

  ;; Run the mode hook
  (run-hooks 'idlwave-mode-hook))

;;
;;  Done with start up and initialization code.
;;  The remaining routines are the code formatting functions.
;;

(defun idlwave-push-mark (&rest rest)
  "Push mark for compatibility with Emacs 18/19."
  (if (fboundp 'iconify-frame)
      (apply 'push-mark rest)
    (push-mark)))

(defun idlwave-hard-tab ()
  "Inserts TAB in buffer in current position."
  (interactive)
  (insert "\t"))

;;; This stuff is experimental

(defvar idlwave-command-hook nil
  "If non-nil, a list that can be evaluated using `eval'.
It is evaluated in the lisp function `idlwave-command-hook' which is
placed in `post-command-hook'.")

(defun idlwave-command-hook ()
  "Command run after every command.
Evaluates a non-nil value of the *variable* `idlwave-command-hook' and
sets the variable to zero afterwards."
  (and idlwave-command-hook
       (listp idlwave-command-hook)
       (condition-case nil
	   (eval idlwave-command-hook)
	 (error nil)))
  (setq idlwave-command-hook nil))

;;; End experiment

;; It would be better to use expand.el for better abbrev handling and
;; versatility.

(defun idlwave-check-abbrev (arg &optional reserved)
  "Reverses abbrev expansion if in comment or string.
Argument ARG is the number of characters to move point
backward if `idlwave-abbrev-move' is non-nil.
If optional argument RESERVED is non-nil then the expansion
consists of reserved words, which will be capitalized if
`idlwave-reserved-word-upcase' is non-nil.
Otherwise, the abbrev will be capitalized if `idlwave-abbrev-change-case'
is non-nil, unless its value is \`down in which case the abbrev will be
made into all lowercase.
Returns non-nil if abbrev is left expanded."
  (if (idlwave-quoted)
      (progn (unexpand-abbrev)
             nil)
    (if (and reserved idlwave-reserved-word-upcase)
        (upcase-region last-abbrev-location (point))
      (cond
       ((equal idlwave-abbrev-change-case 'down)
        (downcase-region last-abbrev-location (point)))
       (idlwave-abbrev-change-case
        (upcase-region last-abbrev-location (point)))))
    (if (and idlwave-abbrev-move (> arg 0))
        (if (boundp 'post-command-hook)
            (setq idlwave-command-hook (list 'backward-char (1+ arg)))
          (backward-char arg)))
    t))

(defun idlwave-in-comment ()
  "Returns t if point is inside a comment, nil otherwise."
  (save-excursion
    (let ((here (point)))
      (and (idlwave-goto-comment) (> here (point))))))

(defun idlwave-goto-comment ()
  "Move to start of comment delimiter on current line.
Moves to end of line if there is no comment delimiter.
Ignores comment delimiters in strings.
Returns point if comment found and nil otherwise."
  (let ((eos (progn (end-of-line) (point)))
        (data (match-data))
        found)
    ;; Look for first comment delimiter not in a string
    (beginning-of-line)
    (setq found (search-forward comment-start eos 'lim))
    (while (and found (idlwave-in-quote))
      (setq found (search-forward comment-start eos 'lim)))
    (store-match-data data)
    (and found (not (idlwave-in-quote))
         (progn
           (backward-char 1)
           (point)))))

(defun idlwave-show-matching-quote ()
  "Insert quote and show matching quote if this is end of a string."
  (interactive)
  (let ((bq (idlwave-in-quote))
        (inq last-command-char))
    (if (and bq (not (idlwave-in-comment)))
        (let ((delim (char-after bq)))
          (insert inq)
          (if (eq inq delim)
              (save-excursion
                (goto-char bq)
                (sit-for 1))))
      ;; Not the end of a string
      (insert inq))))

(defun idlwave-show-begin-check ()
  "Ensure that the previous word was a token before `idlwave-show-begin'.
An END token must be preceded by whitespace."
  (if
      (save-excursion
        (backward-word 1)
        (backward-char 1)
        (looking-at "[ \t\n\f]"))
      (idlwave-show-begin)))

(defun idlwave-show-begin ()
  "Finds the start of current block and blinks to it for a second.
Also checks if the correct end statement has been used."
  ;; All end statements are reserved words
  (let* ((pos (point))
	 end end1)
    (when (and (idlwave-check-abbrev 0 t)
	       idlwave-show-block)
      (save-excursion
	;; Move inside current block
	(setq end (buffer-substring 
		   (save-excursion (skip-chars-backward "a-zA-Z")
				   (point))
		   (point)))
	(idlwave-beginning-of-statement)
	(idlwave-block-jump-out -1 'nomark)
	(when (setq end1 (cdr (idlwave-block-master)))
	  (cond
	   ((null end1)) ; no-opeartion
	   ((string= (downcase end) (downcase end1))
	    (sit-for 1))
	   ((string= (downcase end) "end")
	    ;; A generic end
	    (if idlwave-expand-generic-end
		(save-excursion
		  (goto-char pos)
		  (backward-char 3)
		  (insert (if (string= end "END") (upcase end1) end1))
		  (delete-char 3)))
	    (sit-for 1))
	   (t
	    (beep)
	    (message "Warning: Shouldn't this be \"%s\" instead of \"%s\"?" 
		     end1 end)
	    (sit-for 1))))))))

(defun idlwave-block-master ()
  (let ((case-fold-search t))
    (save-excursion
      (cond
       ((looking-at "pro\\|case\\|function\\>")
	(assoc (downcase (match-string 0)) idlwave-block-matches))
       ((looking-at "begin\\>")
	(let ((limit (save-excursion 
		       (idlwave-beginning-of-statement) 
		       (point))))
	  (cond
	   ((re-search-backward idlwave-block-match-regexp limit t)
	    (assoc (downcase (match-string 1))
		   idlwave-block-matches))
	   ;;((re-search-backward ":[ \t]*\\=" limit t)
	   ;; ;; seems to be a case thing
	   ;; '("begin" . "end"))
	   (t
	    ;; Just a nromal block
	    '("begin" . "end")))))
       (t nil)))))

(defun idlwave-close-block ()
  "Terminate the current block with the correct END statement."
  (interactive)

  ;; Start new line if we are not in a new line
  (unless (save-excursion
	    (skip-chars-backward " \t")
	    (bolp))
    (let ((idlwave-show-block nil))
      (newline-and-indent)))

  ;; Check which end is needed and insert it.
  (let ((case-fold-search t) end)
    (save-excursion
      (idlwave-beginning-of-statement)
      (idlwave-block-jump-out -1 'nomark)
      (if (setq end (idlwave-block-master))
	  (setq end (cdr end))
	(error "Cannot close block")))
    (insert end)
    (idlwave-newline)))

(defun idlwave-surround (&optional before after escape-chars length)
  "Surround the LENGTH characters before point with blanks.
LENGTH defaults to 1.
Optional arguments BEFORE and AFTER affect the behavior before and
after the characters (see also description of `idlwave-make-space'):

nil            do nothing
0              force no spaces
integer > 0    force exactly n spaces
integer < 0    at least |n| spaces

The function does nothing if any of the following conditions is true:
- `idlwave-surround-by-blank' is nil
- the character before point is inside a string or comment
- the char preceeding the string to be surrounded is a member of ESCAPE-CHARS.
  This hack is used to avoid padding of `>' when it is part of
  the '->' operator.  In this case, ESCAPE-CHARS would be '(?-)."

  (setq length (or length 1))   ; establish a default for LENGTH

  (when (and idlwave-surround-by-blank
	     (not (idlwave-quoted))
	     (not (memq (char-after (- (point) (1+ length))) escape-chars)))
    (backward-char length)
    (save-restriction
      (let ((here (point)))
	(skip-chars-backward " \t")
	(if (bolp)
	    ;; avoid clobbering indent
	    (progn
	      (move-to-column (idlwave-calculate-indent))
	      (if (<= (point) here)
		  (narrow-to-region (point) here))
	      (goto-char here)))
	(idlwave-make-space before))
      (skip-chars-forward " \t"))
    (forward-char length)
    (idlwave-make-space after)
    ;; Check to see if the line should auto wrap
    (if (and (equal (char-after (1- (point))) ? )
	     (> (current-column) fill-column))
	(funcall auto-fill-function))))

(defun idlwave-make-space (n)
  "Make space at point.
The space affected is all the spaces and tabs around point.
If n is non-nil then point is left abs(n) spaces from the beginning of
the contiguous space.
The amount of space at point is determined by N.
If the value of N is:
nil   - do nothing.
> 0   - exactly N spaces.
< 0   - a minimum of -N spaces, i.e., do not change if there are
        already -N spaces.
0     - no spaces (i.e. remove any existing space)."
  (if (integerp n)
      (let
          ((start-col (progn (skip-chars-backward " \t") (current-column)))
           (left (point))
           (end-col (progn (skip-chars-forward " \t") (current-column))))
        (delete-horizontal-space)
        (cond
         ((> n 0)
          (idlwave-indent-to (+ start-col n))
          (goto-char (+ left n)))
         ((< n 0)
          (idlwave-indent-to end-col (- n))
          (goto-char (- left n)))
         ;; n = 0, done
         ))))

(defun idlwave-newline ()
  "Inserts a newline and indents the current and previous line."
  (interactive)
  ;;
  ;; Handle unterminated single and double quotes
  ;; If not in a comment and in a string then insertion of a newline
  ;; will mean unbalanced quotes.
  ;;
  (if (and (not (idlwave-in-comment)) (idlwave-in-quote))
      (progn (beep)
             (message "Warning: unbalanced quotes?")))
  (newline)
  ;;
  ;; The current line is being split, the cursor should be at the
  ;; beginning of the new line skipping the leading indentation.
  ;;
  ;; The reason we insert the new line before indenting is that the
  ;; indenting could be confused by keywords (e.g. END) on the line
  ;; after the split point.  This prevents us from just using
  ;; `indent-for-tab-command' followed by `newline-and-indent'.
  ;;
  (beginning-of-line 0)
  (idlwave-indent-line)
  (forward-line)
  (idlwave-indent-line))

;;
;;  Use global variable 'comment-column' to set parallel comment
;;
;; Modeled on lisp.el
;; Emacs Lisp and IDL (Wave CL) have identical comment syntax
(defun idlwave-comment-hook ()
  "Compute indent for the beginning of the IDL comment delimiter."
  (if (or (looking-at idlwave-no-change-comment)
          (if idlwave-begin-line-comment
              (looking-at idlwave-begin-line-comment)
	    (looking-at "^;")))
      (current-column)
    (if (looking-at idlwave-code-comment)
        (if (save-excursion (skip-chars-backward " \t") (bolp))
            ;; On line by itself, indent as code
            (let ((tem (idlwave-calculate-indent)))
              (if (listp tem) (car tem) tem))
          ;; after code - do not change
          (current-column))
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
           comment-column))))

(defun idlwave-split-line ()
  "Continue line by breaking line at point and indent the lines.
For a code line insert continuation marker. If the line is a line comment
then the new line will contain a comment with the same indentation.
Splits strings with the IDL operator `+' if `idlwave-split-line-string' is
non-nil."
  (interactive)
  (let (beg)
    (if (not (idlwave-in-comment))
        ;; For code line add continuation.
        ;; Check if splitting a string.
        (progn
          (if (setq beg (idlwave-in-quote))
              (if idlwave-split-line-string
                  ;; Split the string.
                  (progn (insert (setq beg (char-after beg)) " + "
                                 idlwave-continuation-char beg)
                         (backward-char 1))
                ;; Do not split the string.
                (beep)
                (message "Warning: continuation inside string!!")
                (insert " " idlwave-continuation-char))
            ;; Not splitting a string.
            (insert " " idlwave-continuation-char))
          (newline-and-indent))
      (indent-new-comment-line))
    ;; Indent previous line
    (setq beg (- (point-max) (point)))
    (forward-line -1)
    (idlwave-indent-line)
    (goto-char (- (point-max) beg))
    ;; Reindent new line
    (idlwave-indent-line)))

(defun idlwave-beginning-of-subprogram ()
  "Moves point to the beginning of the current program unit."
  (interactive)
  (idlwave-find-key idlwave-begin-unit-reg -1))

(defun idlwave-end-of-subprogram ()
  "Moves point to the start of the next program unit."
  (interactive)
  (idlwave-end-of-statement)
  (idlwave-find-key idlwave-end-unit-reg 1))

(defun idlwave-mark-statement ()
  "Mark current IDL statement."
  (interactive)
  (idlwave-end-of-statement)
  (let ((end (point)))
    (idlwave-beginning-of-statement)
    (idlwave-push-mark end nil t)))

(defun idlwave-mark-block ()
  "Mark containing block."
  (interactive)
  (idlwave-end-of-statement)
  (idlwave-backward-up-block -1)
  (idlwave-end-of-statement)
  (let ((end (point)))
    (idlwave-backward-block)
    (idlwave-beginning-of-statement)
    (idlwave-push-mark end nil t)))


(defun idlwave-mark-subprogram ()
  "Put mark at beginning of program, point at end.
The marks are pushed."
  (interactive)
  (idlwave-end-of-statement)
  (idlwave-beginning-of-subprogram)
  (let ((beg (point)))
    (idlwave-forward-block)
    (idlwave-push-mark beg nil t))
  (exchange-point-and-mark))

(defun idlwave-backward-up-block (&optional arg)
  "Move to beginning of enclosing block if prefix ARG >= 0.
If prefix ARG < 0 then move forward to enclosing block end."
  (interactive "p")
  (idlwave-block-jump-out (- arg) 'nomark))

(defun idlwave-beginning-of-block ()
  "Go to the beginning of the current block."
  (interactive)
  (idlwave-block-jump-out -1 'nomark)
  (forward-word 1))

(defun idlwave-end-of-block ()
  "Go to the beginning of the current block."
  (interactive)
  (idlwave-block-jump-out 1 'nomark)
  (backward-word 1))

(defun idlwave-forward-block ()
  "Move across next nested block."
  (interactive)
  (if (idlwave-down-block 1)
      (idlwave-block-jump-out 1 'nomark)))

(defun idlwave-backward-block ()
  "Move backward across previous nested block."
  (interactive)
  (if (idlwave-down-block -1)
      (idlwave-block-jump-out -1 'nomark)))

(defun idlwave-down-block (&optional arg)
  "Go down a block.
With ARG: ARG >= 0 go forwards, ARG < 0 go backwards.
Returns non-nil if successfull."
  (interactive "p")
  (let (status)
    (if (< arg 0)
        ;; Backward
        (let ((eos (save-excursion
                     (idlwave-block-jump-out -1 'nomark)
                     (point))))
          (if (setq status (idlwave-find-key 
			    idlwave-end-block-reg -1 'nomark eos))
              (idlwave-beginning-of-statement)
            (message "No nested block before beginning of containing block.")))
      ;; Forward
      (let ((eos (save-excursion
                   (idlwave-block-jump-out 1 'nomark)
                   (point))))
        (if (setq status (idlwave-find-key 
			  idlwave-begin-block-reg 1 'nomark eos))
            (idlwave-end-of-statement)
          (message "No nested block before end of containing block."))))
    status))

(defun idlwave-mark-doclib ()
  "Put point at beginning of doc library header, mark at end.
The marks are pushed."
  (interactive)
  (let (beg
        (here (point)))
    (goto-char (point-max))
    (if (re-search-backward idlwave-doclib-start nil t)
        (progn 
	  (setq beg (progn (beginning-of-line) (point)))
	  (if (re-search-forward idlwave-doclib-end nil t)
	      (progn
		(forward-line 1)
		(idlwave-push-mark beg nil t)
		(message "Could not find end of doc library header.")))
	  (message "Could not find doc library header start.")
	  (goto-char here)))))

(defvar idlwave-shell-prompt-pattern)
(defun idlwave-beginning-of-statement ()
  "Move to beginning of the current statement.
Skips back past statement continuations.
Point is placed at the beginning of the line whether or not this is an
actual statement."
  (interactive)
  (cond
   ((eq major-mode 'idlwave-shell-mode)
    (if (re-search-backward idlwave-shell-prompt-pattern nil t)
	(goto-char (match-end 0))))
   (t	
    (if (save-excursion (forward-line -1) (idlwave-is-continuation-line))
	(idlwave-previous-statement)
      (beginning-of-line)))))

(defun idlwave-previous-statement ()
  "Moves point to beginning of the previous statement.
Returns t if the current line before moving is the beginning of
the first non-comment statement in the file, and nil otherwise."
  (interactive)
  (let (first-statement)
    (if (not (= (forward-line -1) 0))
        ;; first line in file
        t
      ;; skip blank lines, label lines, include lines and line comments
      (while (and
              ;; The current statement is the first statement until we
              ;; reach another statement.
              (setq first-statement
                    (or
                     (looking-at idlwave-comment-line-start-skip)
                     (looking-at "[ \t]*$")
                     (looking-at (concat "[ \t]*" idlwave-label "[ \t]*$"))
                     (looking-at "^@")))
              (= (forward-line -1) 0)))
      ;; skip continuation lines
      (while (and
              (save-excursion
                (forward-line -1)
                (idlwave-is-continuation-line))
              (= (forward-line -1) 0)))
      first-statement)))

;; FIXME: end-of-statement does not work correctly when comment lines
;; are inside the statement.  It does work correctly for line-end
;; comments, though.
(defun idlwave-end-of-statement ()
  "Moves point to the end of the current IDL statement.
If not in a statement just moves to end of line. Returns position."
  (interactive)
  (while (and (idlwave-is-continuation-line)
              (= (forward-line 1) 0)))
  (end-of-line)
  (point))

(defun idlwave-next-statement ()
  "Moves point to beginning of the next IDL statement.
 Returns t if that statement is the last
 non-comment IDL statement in the file, and nil otherwise."
  (interactive)
  (let (last-statement)
    (idlwave-end-of-statement)
    ;; skip blank lines, label lines, include lines and line comments
    (while (and (= (forward-line 1) 0)
                ;; The current statement is the last statement until
                ;; we reach a new statement.
                (setq last-statement
                      (or
                       (looking-at idlwave-comment-line-start-skip)
                       (looking-at "[ \t]*$")
                       (looking-at (concat "[ \t]*" idlwave-label "[ \t]*$"))
                       (looking-at "^@")))))
    last-statement))

(defun idlwave-skip-label ()
  "Skip label or case statement element.
Returns position after label.
If there is no label point is not moved and nil is returned."
  ;; Just look for the first non quoted colon and check to see if it
  ;; is inside a sexp.  If is not in a sexp it must be part of a label
  ;; or case statement element.
  (let ((start (point))
        (end (idlwave-find-key ":" 1 'nomark
			       (save-excursion
				 (idlwave-end-of-statement) (point)))))
    (if (and end
             (= (nth 0 (parse-partial-sexp start end)) 0))
        (progn
          (forward-char)
          (point))
      (goto-char start)
      nil)))

(defun idlwave-start-of-substatement (&optional pre)
  "Move to start of next IDL substatement after point.
Uses the type of the current IDL statement to determine if the next
statement is on a new line or is a subpart of the current statement.
Returns point at start of substatement modulo whitespace.
If optional argument is non-nil move to beginning of current
substatement. "
  (let ((orig (point))
        (eos (idlwave-end-of-statement))
        (ifnest 0)
        st nst last)
    (idlwave-beginning-of-statement)
    (idlwave-skip-label)
    (setq last (point))
    ;; Continue looking for substatements until we are past orig
    (while (and (<= (point) orig) (not (eobp)))
      (setq last (point))
      (setq nst (nth 1 (cdr (setq st (car (idlwave-statement-type))))))
      (if (equal (car st) 'if) (setq ifnest (1+ ifnest)))
      (cond ((and nst
                  (idlwave-find-key nst 1 'nomark eos))
             (goto-char (match-end 0)))
            ((and (> ifnest 0) (idlwave-find-key "\\<else\\>" 1 'nomark eos))
             (setq ifnest (1- ifnest))
             (goto-char (match-end 0)))
            (t (setq ifnest 0)
               (idlwave-next-statement))))
    (if pre (goto-char last))
    (point)))

(defun idlwave-statement-type ()
  "Return the type of the current IDL statement.
Uses `idlwave-statement-match' to return a cons of (type . point) with
point the ending position where the type was determined. Type is the
association from `idlwave-statement-match', i.e. the cons cell from the
list not just the type symbol. Returns nil if not an identifiable
statement."
  (save-excursion
    ;; Skip whitespace within a statement which is spaces, tabs, continuations
    (while (looking-at "[ \t]*\\<\\$")
      (forward-line 1))
    (skip-chars-forward " \t")
    (let ((st idlwave-statement-match)
          (case-fold-search t))
      (while (and (not (looking-at (nth 0 (cdr (car st)))))
                  (setq st (cdr st))))
      (if st
          (append st (match-end 0))))))

(defun idlwave-expand-equal (&optional before after)
  "Pad '=' with spaces.
Two cases: Assignment statement, and keyword assignment.
The case is determined using `idlwave-start-of-substatement' and
`idlwave-statement-type'.
The equal sign will be surrounded by BEFORE and AFTER blanks.
If `idlwave-pad-keyword' is non-nil then keyword
assignment is treated just like assignment statements.  Otherwise,
spaces are removed for keyword assignment.
Limits in for loops are treated as keyword assignment.
See `idlwave-surround'. "
  ;; Even though idlwave-surround checks `idlwave-surround-by-blank' this
  ;; check saves the time of finding the statement type.
  (if idlwave-surround-by-blank
      (let ((st (save-excursion
                  (idlwave-start-of-substatement t)
                  (idlwave-statement-type))))
        (if (or
             (and (equal (car (car st)) 'assign)
                  (equal (cdr st) (point)))
             idlwave-pad-keyword)
            ;; An assignment statement
            (idlwave-surround before after)
          (idlwave-surround 0 0)))))

(defun idlwave-indent-and-action ()
  "Call `idlwave-indent-line' and do expand actions."
  (interactive)
  (idlwave-indent-line t)
  )

(defun idlwave-indent-line (&optional expand)
  "Indents current IDL line as code or as a comment.
The actions in `idlwave-indent-action-table' are performed.
If the optional argument EXPAND is non-nil then the actions in
`idlwave-indent-expand-table' are performed."
  (interactive)
  ;; Move point out of left margin.
  (if (save-excursion
        (skip-chars-backward " \t")
        (bolp))
      (skip-chars-forward " \t"))
  (let ((mloc (point-marker)))
    (save-excursion
      (beginning-of-line)
      (if (looking-at idlwave-comment-line-start-skip)
          ;; Indentation for a line comment
          (progn
            (skip-chars-forward " \t")
            (idlwave-indent-left-margin (idlwave-comment-hook)))
        ;;
        ;; Code Line
        ;;
        ;; Before indenting, run action routines.
        ;;
        (if (and expand idlwave-do-actions)
            (mapcar 'idlwave-do-action idlwave-indent-expand-table))
        ;;
        (if idlwave-do-actions
            (mapcar 'idlwave-do-action idlwave-indent-action-table))
        ;;
        ;; No longer expand abbrevs on the line.  The user can do this
        ;; manually using expand-region-abbrevs.
        ;;
        ;; Indent for code line
        ;;
        (beginning-of-line)
        (if (or
             ;; a label line
             (looking-at (concat "^" idlwave-label "[ \t]*$"))
             ;; a batch command
             (looking-at "^[ \t]*@"))
            ;; leave flush left
            nil
          ;; indent the line
          (idlwave-indent-left-margin (idlwave-calculate-indent)))
        ;; Adjust parallel comment
        (end-of-line)
        (if (idlwave-in-comment)
            (indent-for-comment))))
    (goto-char mloc)
    ;; Get rid of marker
    (set-marker mloc nil)
    ))

(defun idlwave-do-action (action)
  "Perform an action repeatedly on a line.
ACTION is a list (REG . FUNC).  REG is a regular expression.  FUNC is
either a function name to be called with `funcall' or a list to be
evaluated with `eval'.  The action performed by FUNC should leave point
after the match for REG - otherwise an infinite loop may be entered."
  (let ((action-key (car action))
        (action-routine (cdr action)))
    (beginning-of-line)
    (while (idlwave-look-at action-key)
      (if (listp action-routine)
          (eval action-routine)
        (funcall action-routine)))))

(defun idlwave-indent-to (col &optional min)
  "Indent from point with spaces until column COL.
Inserts space before markers at point."
  (if (not min) (setq min 0))
  (insert-before-markers
   (make-string (max min (- col (current-column))) ? )))

(defun idlwave-indent-left-margin (col)
  "Indent the current line to column COL.
Indents such that first non-whitespace character is at column COL
Inserts spaces before markers at point."
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (idlwave-indent-to col)))

(defun idlwave-indent-subprogram ()
  "Indents program unit which contains point."
  (interactive)
  (save-excursion
    (idlwave-end-of-statement)
    (idlwave-beginning-of-subprogram)
    (let ((beg (point)))
      (idlwave-forward-block)
      (message "Indenting subprogram...")
      (indent-region beg (point) nil))
    (message "Indenting subprogram...done.")))

(defun idlwave-calculate-indent ()
  "Return appropriate indentation for current line as IDL code."
  (save-excursion
    (beginning-of-line)
    (cond
     ;; Check for beginning of unit - main (beginning of buffer), pro, or
     ;; function
     ((idlwave-look-at idlwave-begin-unit-reg)
      0)
     ;; Check for continuation line
     ((save-excursion
        (and (= (forward-line -1) 0)
             (idlwave-is-continuation-line)))
      (idlwave-calculate-cont-indent))
     ;; calculate indent based on previous and current statements
     (t (let ((the-indent
               ;; calculate indent based on previous statement
               (save-excursion
                 (cond
                  ((idlwave-previous-statement)
                   0)
                  ;; Main block
                  ((idlwave-look-at idlwave-begin-unit-reg t)
                   (+ (idlwave-current-statement-indent)
		      idlwave-main-block-indent))
                  ;; Begin block
                  ((idlwave-look-at idlwave-begin-block-reg t)
                   (+ (idlwave-current-statement-indent)
		      idlwave-block-indent))
                  ((idlwave-look-at idlwave-end-block-reg t)
                   (- (idlwave-current-statement-indent)
		      idlwave-end-offset
		      idlwave-block-indent))
                  ((idlwave-current-statement-indent))))))
          ;; adjust the indentation based on the current statement
          (cond
           ;; End block
           ((idlwave-look-at idlwave-end-block-reg t)
            (+ the-indent idlwave-end-offset))
           (the-indent)))))))

;;
;; Parenthesses balacing/indent
;;

(defun idlwave-calculate-cont-indent ()
  "Calculates the IDL continuation indent column from the previous statement.
Note that here previous statement means the beginning of the current
statement if this statement is a continuation of the previous line.
Intervening comments or comments within the previous statement can
screw things up if the comments contain parentheses characters."
  (save-excursion
    (let* (open
           (case-fold-search t)
           (end-reg (progn (beginning-of-line) (point)))
           (close-exp (progn (skip-chars-forward " \t") (looking-at "\\s)")))
           (beg-reg (progn (idlwave-previous-statement) (point))))
      ;;
      ;; If PRO or FUNCTION declaration indent after name, and first comma.
      ;;
      (if (idlwave-look-at "\\<\\(pro\\|function\\)\\>")
          (progn
            (forward-sexp 1)
            (if (looking-at "[ \t]*,[ \t]*")
                (goto-char (match-end 0)))
            (current-column))
        ;;
        ;; Not a PRO or FUNCTION
        ;;
        ;; Look for innermost unmatched open paren
        ;;
        (if (setq open (car (cdr (parse-partial-sexp beg-reg end-reg))))
            ;; Found innermost open paren.
            (progn
              (goto-char open)
	      ;; Line up with next word unless this is a closing paren.
              (cond
               ;; This is a closed paren - line up under open paren.
               (close-exp
                (current-column))
               ;; Empty - just add regular indent. Take into account
               ;; the forward-char
               ((progn
                  ;; Skip paren
                  (forward-char 1)
                  (looking-at "[ \t$]*$"))
                (+ (current-column) idlwave-continuation-indent -1))
               ;; Line up with first word
               ((progn
                  (skip-chars-forward " \t")
                  (current-column)))))
          ;; No unmatched open paren. Just a simple continuation.
          (goto-char beg-reg)
          (+ (idlwave-current-indent)
             ;; Make adjustments based on current line
             (cond
              ;; Else statement
              ((progn
                 (goto-char end-reg)
                 (skip-chars-forward " \t")
                 (looking-at "else"))
               0)
              ;; Ordinary continuation
              (idlwave-continuation-indent))))))))

(defun idlwave-find-key (key-reg &optional dir nomark limit)
  "Move in direction of the optional second argument DIR to the
next keyword not contained in a comment or string and occurring before
optional fourth argument LIMIT. DIR defaults to forward direction.  If
DIR is negative the search is backwards, otherwise, it is
forward. LIMIT defaults to the beginning or end of the buffer
according to the direction of the search. The keyword is given by the
regular expression argument KEY-REG.  The search is case insensitive.
Returns position if successful and nil otherwise.  If found
`push-mark' is executed unless the optional third argument NOMARK is
non-nil. If found, the point is left at the keyword beginning."
  (or dir (setq dir 0))
  (or limit (setq limit (cond ((>= dir 0) (point-max)) ((point-min)))))
  (let (found
        (old-syntax-table (syntax-table))
        (case-fold-search t))
    (unwind-protect
	(save-excursion
	  (set-syntax-table idlwave-find-symbol-syntax-table)
	  (if (>= dir 0)
	      (while (and (setq found (and
				       (re-search-forward key-reg limit t)
				       (match-beginning 0)))
			  (idlwave-quoted)
			  (not (eobp))))
	    (while (and (setq found (and
				     (re-search-backward key-reg limit t)
				     (match-beginning 0)))
			(idlwave-quoted)
			(not (bobp))))))
      (set-syntax-table old-syntax-table))
    (if found (progn
                (if (not nomark) (push-mark))
                (goto-char found)))))

(defun idlwave-block-jump-out (&optional dir nomark)
  "When optional argument DIR is non-negative, move forward to end of
current block using the `idlwave-begin-block-reg' and `idlwave-end-block-reg'
regular expressions. When DIR is negative, move backwards to block beginning.
Recursively calls itself to skip over nested blocks. DIR defaults to
forward. Calls `push-mark' unless the optional argument NOMARK is
non-nil. Movement is limited by the start of program units because of
possibility of unbalanced blocks."
  (interactive "P")
  (or dir (setq dir 0))
  (let* ((here (point))
         (case-fold-search t)
         (limit (if (>= dir 0) (point-max) (point-min)))
         (block-limit (if (>= dir 0) 
			  idlwave-begin-block-reg
			idlwave-end-block-reg))
         found
         (block-reg (concat idlwave-begin-block-reg "\\|"
			    idlwave-end-block-reg))
         (unit-limit (or (save-excursion
			   (if (< dir 0)
			       (idlwave-find-key
				idlwave-begin-unit-reg dir t limit)
			     (end-of-line)
			     (idlwave-find-key 
			      idlwave-end-unit-reg dir t limit)))
			 limit)))
    (if (>= dir 0) (end-of-line)) ;Make sure we are in current block
    (if (setq found (idlwave-find-key  block-reg dir t unit-limit))
        (while (and found (looking-at block-limit))
          (if (>= dir 0) (forward-word 1))
          (idlwave-block-jump-out dir t)
          (setq found (idlwave-find-key block-reg dir t unit-limit))))
    (if (not nomark) (push-mark here))
    (if (not found) (goto-char unit-limit)
      (if (>= dir 0) (forward-word 1)))))

(defun idlwave-current-statement-indent ()
  "Return indentation of the current statement.
If in a statement, moves to beginning of statement before finding indent."
  (idlwave-beginning-of-statement)
  (idlwave-current-indent))

(defun idlwave-current-indent ()
  "Return the column of the indentation of the current line.
Skips any whitespace. Returns 0 if the end-of-line follows the whitespace."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    ;; if we are at the end of blank line return 0
    (cond ((eolp) 0)
          ((current-column)))))

(defun idlwave-is-continuation-line ()
  "Tests if current line is continuation line."
  (save-excursion
    (idlwave-look-at "\\<\\$")))

(defun idlwave-is-comment-line ()
  (save-excursion
    (beginning-of-line 1)
    (looking-at "[ \t]*;")))

(defun idlwave-look-at (regexp &optional cont beg)
  "Searches current line from current point for the regular expression
REGEXP. If optional argument CONT is non-nil, searches to the end of
the current statement. If optional arg BEG is non-nil, search starts
from the beginning of the current statement. Ignores matches that end
in a comment or inside a string expression. Returns point if
successful, nil otherwise.  This function produces unexpected results
if REGEXP contains quotes or a comment delimiter. The search is case
insensitive.  If successful leaves point after the match, otherwise,
does not move point."
  (let ((here (point))
        (old-syntax-table (syntax-table))
        (case-fold-search t)
        eos
        found)
    (unwind-protect
	(progn
	  (set-syntax-table idlwave-find-symbol-syntax-table)
	  (setq eos
		(if cont
		    (save-excursion (idlwave-end-of-statement) (point))
		  (save-excursion (end-of-line) (point))))
	  (if beg (idlwave-beginning-of-statement))
	  (while (and (setq found (re-search-forward regexp eos t))
		      (idlwave-quoted))))
      (set-syntax-table old-syntax-table))
    (if (not found) (goto-char here))
    found))

(defun idlwave-fill-paragraph (&optional nohang)
  "Fills paragraphs in comments.
A paragraph is made up of all contiguous lines having the same comment
leader (the leading whitespace before the comment delimiter and the
comment delimiter).  In addition, paragraphs are separated by blank
line comments. The indentation is given by the hanging indent of the
first line, otherwise by the minimum indentation of the lines after
the first line. The indentation of the first line does not change.
Does not effect code lines. Does not fill comments on the same line
with code.  The hanging indent is given by the end of the first match
matching `idlwave-hang-indent-regexp' on the paragraph's first line . If the
optional argument NOHANG is non-nil then the hanging indent is
ignored."
  (interactive "P")
  ;; check if this is a line comment
  (if (save-excursion
        (beginning-of-line)
        (skip-chars-forward " \t")
        (looking-at comment-start))
      (let
          ((indent 999)
           pre here diff fill-prefix-reg bcl first-indent
           hang start end)
        ;; Change tabs to spaces in the surrounding paragraph.
        ;; The surrounding paragraph will be the largest containing block of
        ;; contiguous line comments. Thus, we may be changing tabs in
        ;; a much larger area than is needed, but this is the easiest
        ;; brute force way to do it.
        ;;
        ;; This has the undesirable side effect of replacing the tabs
        ;; permanently without the user's request or knowledge.
        (save-excursion
          (backward-paragraph)
          (setq start (point)))
        (save-excursion
          (forward-paragraph)
          (setq end (point)))
        (untabify start end)
        ;;
        (setq here (point))
        (beginning-of-line)
        (setq bcl (point))
        (re-search-forward
         (concat "^[ \t]*" comment-start "+")
         (save-excursion (end-of-line) (point))
         t)
        ;; Get the comment leader on the line and its length
        (setq pre (current-column))
        ;; the comment leader is the indentation plus exactly the
        ;; number of consecutive ";".
        (setq fill-prefix-reg
              (concat
               (setq fill-prefix
                     (regexp-quote
                      (buffer-substring (save-excursion
                                          (beginning-of-line) (point))
                                        (point))))
               "[^;]"))
	
        ;; Mark the beginning and end of the paragraph
        (goto-char bcl)
        (while (and (looking-at fill-prefix-reg)
                    (not (looking-at paragraph-separate))
                    (not (bobp)))
          (forward-line -1))
        ;; Move to first line of paragraph
        (if (/= (point) bcl)
            (forward-line 1))
        (setq start (point))
        (goto-char bcl)
        (while (and (looking-at fill-prefix-reg)
                    (not (looking-at paragraph-separate))
                    (not (eobp)))
          (forward-line 1))
        (beginning-of-line)
        (if (or (not (looking-at fill-prefix-reg))
                (looking-at paragraph-separate))
            (forward-line -1))
        (end-of-line)
        ;; if at end of buffer add a newline (need this because
        ;; fill-region needs END to be at the beginning of line after
        ;; the paragraph or it will add a line).
        (if (eobp)
            (progn (insert ?\n) (backward-char 1)))
        ;; Set END to the beginning of line after the paragraph
        ;; END is calculated as distance from end of buffer
        (setq end (- (point-max) (point) 1))
        ;;
        ;; Calculate the indentation for the paragraph.
        ;;
        ;; In the following while statements, after one iteration
        ;; point will be at the beginning of a line in which case
        ;; the while will not be executed for the
        ;; the first paragraph line and thus will not affect the
        ;; indentation.
        ;;
        ;; First check to see if indentation is based on hanging indent.
        (if (and (not nohang) idlwave-hanging-indent
                 (setq hang
                       (save-excursion
                         (goto-char start)
                         (idlwave-calc-hanging-indent))))
            ;; Adjust lines of paragraph by inserting spaces so that
            ;; each line's indent is at least as great as the hanging
            ;; indent. This is needed for fill-paragraph to work with
            ;; a fill-prefix.
            (progn
              (setq indent hang)
              (beginning-of-line)
              (while (> (point) start)
                (re-search-forward comment-start-skip
                                   (save-excursion (end-of-line) (point))
                                   t)
                (if (> (setq diff (- indent (current-column))) 0)
                    (progn
                      (if (>= here (point))
                          ;; adjust the original location for the
                          ;; inserted text.
                          (setq here (+ here diff)))
                      (insert (make-string diff ? ))))
                (forward-line -1))
              )
	  
          ;; No hang. Instead find minimum indentation of paragraph
          ;; after first line.
          ;; For the following while statement, since START is at the
          ;; beginning of line and END is at the the end of line
          ;; point is greater than START at least once (which would
          ;; be the case for a single line paragraph).
          (while (> (point) start)
            (beginning-of-line)
            (setq indent
                  (min indent
                       (progn
                         (re-search-forward
                          comment-start-skip
                          (save-excursion (end-of-line) (point))
                          t)
                         (current-column))))
            (forward-line -1))
          )
        (setq fill-prefix (concat fill-prefix
                                  (make-string (- indent pre)
                                               ? )))
        ;; first-line indent
        (setq first-indent
              (max
               (progn
                 (re-search-forward
                  comment-start-skip
                  (save-excursion (end-of-line) (point))
                  t)
                 (current-column))
               indent))
	
        ;; try to keep point at its original place
        (goto-char here)

        ;; In place of the more modern fill-region-as-paragraph, a hack
        ;; to keep whitespace untouched on the first line within the
        ;; indent length and to preserve any indent on the first line
        ;; (first indent).
        (save-excursion
          (setq diff
                (buffer-substring start (+ start first-indent -1)))
          (subst-char-in-region start (+ start first-indent -1) ?  ?~ nil)
          (fill-region-as-paragraph
           start
           (- (point-max) end)
           (current-justification)
           nil)
          (delete-region start (+ start first-indent -1))
          (goto-char start)
          (insert diff))
        ;; When we want the point at the beginning of the comment
        ;; body fill-region will put it at the beginning of the line.
        (if (bolp) (skip-chars-forward (concat " \t" comment-start)))
        (setq fill-prefix nil))))

(defun idlwave-calc-hanging-indent ()
  "Calculate the position of the hanging indent for the comment
paragraph.  The hanging indent position is given by the first match
with the `idlwave-hang-indent-regexp'.  If `idlwave-use-last-hang-indent' is
non-nil then use last occurrence matching `idlwave-hang-indent-regexp' on
the line.
If not found returns nil."
  (if idlwave-use-last-hang-indent
      (save-excursion
        (end-of-line)
        (if (re-search-backward
             idlwave-hang-indent-regexp
             (save-excursion (beginning-of-line) (point))
             t)
            (+ (current-column) (length idlwave-hang-indent-regexp))))
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward
           idlwave-hang-indent-regexp
           (save-excursion (end-of-line) (point))
           t)
          (current-column)))))

(defun idlwave-auto-fill ()
  "Called to break lines in auto fill mode.
Only fills comment lines if `idlwave-fill-comment-line-only' is non-nil.
Places a continuation character at the end of the line
if not in a comment.  Splits strings with IDL concatenation operator
`+' if `idlwave-auto-fill-split-string is non-nil."
  (if (<= (current-column) fill-column)
      nil                             ; do not to fill
    (if (or (not idlwave-fill-comment-line-only)
	    (save-excursion
	      ;; Check for comment line
	      (beginning-of-line)
	      (looking-at idlwave-comment-line-start-skip)))
	(let (beg)
	  (idlwave-indent-line)
	  ;; Prevent actions do-auto-fill which calls indent-line-function.
	  (let (idlwave-do-actions
		(paragraph-start ".")
		(paragraph-separate "."))
	    (do-auto-fill))
	  (save-excursion
	    (end-of-line 0)
	    ;; Indent the split line
	    (idlwave-indent-line)
	    )
	  (if (save-excursion
		(beginning-of-line)
		(looking-at idlwave-comment-line-start-skip))
	      ;; A continued line comment
	      ;; We treat continued line comments as part of a comment
	      ;; paragraph. So we check for a hanging indent.
	      (if idlwave-hanging-indent
		  (let ((here (- (point-max) (point)))
			(indent
			 (save-excursion
			   (forward-line -1)
			   (idlwave-calc-hanging-indent))))
		    (if indent
			(progn
			  ;; Remove whitespace between comment delimiter and
			  ;; text, insert spaces for appropriate indentation.
			  (beginning-of-line)
			  (re-search-forward
			   comment-start-skip
			   (save-excursion (end-of-line) (point)) t)
			  (delete-horizontal-space)
			  (idlwave-indent-to indent)
			  (goto-char (- (point-max) here)))
		      )))
	    ;; Split code or comment?
	    (if (save-excursion
		  (end-of-line 0)
		  (idlwave-in-comment))
		;; Splitting a non-line comment.
		;; Insert the comment delimiter from split line
		(progn
		  (save-excursion
		    (beginning-of-line)
		    (skip-chars-forward " \t")
		    ;; Insert blank to keep off beginning of line
		    (insert " "
			    (save-excursion
			      (forward-line -1)
			      (buffer-substring (idlwave-goto-comment)
						(progn
						  (skip-chars-forward "; ")
						  (point))))))
		  (idlwave-indent-line))
	      ;; Split code line - add continuation character
	      (save-excursion
		(end-of-line 0)
		;; Check to see if we split a string
		(if (and (setq beg (idlwave-in-quote))
			 idlwave-auto-fill-split-string)
		    ;; Split the string and concatenate.
		    ;; The first extra space is for the space
		    ;; the line was split. That space was removed.
		    (insert " " (char-after beg) " +"))
		(insert " $"))
	      (if beg
		  (if idlwave-auto-fill-split-string
		      ;; Make the second part of continued string
		      (save-excursion
			(beginning-of-line)
			(skip-chars-forward " \t")
			(insert (char-after beg)))
		    ;; Warning
		    (beep)
		    (message "Warning: continuation inside a string.")))
	      ;; Although do-auto-fill (via indent-new-comment-line) calls
	      ;; idlwave-indent-line for the new line, re-indent again
	      ;; because of the addition of the continuation character.
	      (idlwave-indent-line))
	    )))))

(defun idlwave-auto-fill-mode (arg)
  "Toggle auto-fill mode for IDL mode.
With arg, turn auto-fill mode on if arg is positive.
In auto-fill mode, inserting a space at a column beyond `fill-column'
automatically breaks the line at a previous space."
  (interactive "P")
  (prog1 (set idlwave-fill-function
              (if (if (null arg)
                      (not (symbol-value idlwave-fill-function))
                    (> (prefix-numeric-value arg) 0))
                  'idlwave-auto-fill
                nil))
    ;; update mode-line
    (set-buffer-modified-p (buffer-modified-p))))

(defun idlwave-doc-header (&optional nomark )
  "Insert a documentation header at the beginning of the unit.
Inserts the value of the variable idlwave-file-header. Sets mark before
moving to do insertion unless the optional prefix argument NOMARK
is non-nil."
  (interactive "P")
  (or nomark (push-mark))
  ;; make sure we catch the current line if it begins the unit
  (end-of-line)
  (idlwave-beginning-of-subprogram)
  (beginning-of-line)
  ;; skip function or procedure line
  (if (idlwave-look-at "\\<\\(pro\\|function\\)\\>")
      (progn
        (idlwave-end-of-statement)
        (if (> (forward-line 1) 0) (insert "\n"))))
  (if idlwave-file-header
      (cond ((car idlwave-file-header)
             (insert-file (car idlwave-file-header)))
            ((stringp (car (cdr idlwave-file-header)))
             (insert (car (cdr idlwave-file-header)))))))


(defun idlwave-default-insert-timestamp ()
  "Default timestamp insertion function"
  (insert (current-time-string))
  (insert ", " (user-full-name))
  (insert " <" (user-login-name) "@" (system-name) ">")
  ;; Remove extra spaces from line
  (idlwave-fill-paragraph)
  ;; Insert a blank line comment to separate from the date entry -
  ;; will keep the entry from flowing onto date line if re-filled.
  (insert "\n;\n;\t\t"))

(defun idlwave-doc-modification ()
  "Insert a brief modification log at the beginning of the current program.
Looks for an occurrence of the value of user variable
`idlwave-doc-modifications-keyword' if non-nil. Inserts time and user name
and places the point for the user to add a log. Before moving, saves
location on mark ring so that the user can return to previous point."
  (interactive)
  (push-mark)
  ;; make sure we catch the current line if it begins the unit
  (end-of-line)
  (idlwave-beginning-of-subprogram)
  (let ((pro (idlwave-look-at "\\<\\(function\\|pro\\)\\>"))
        (case-fold-search nil))
    (if (re-search-forward
         (concat idlwave-doc-modifications-keyword ":")
         ;; set search limit at next unit beginning
         (save-excursion (idlwave-end-of-subprogram) (point))
         t)
        (end-of-line)
      ;; keyword not present, insert keyword
      (if pro (idlwave-next-statement))  ; skip past pro or function statement
      (beginning-of-line)
      (insert "\n" comment-start "\n")
      (forward-line -2)
      (insert comment-start " " idlwave-doc-modifications-keyword ":")))
  (idlwave-newline)
  (beginning-of-line)
  (insert ";\n;\t")
  (run-hooks 'idlwave-timestamp-hook))

;;; CJC 3/16/93
;;; Interface to expand-region-abbrevs which did not work when the
;;; abbrev hook associated with an abbrev moves point backwards
;;; after abbrev expansion, e.g., as with the abbrev '.n'.
;;; The original would enter an infinite loop in attempting to expand
;;; .n (it would continually expand and unexpand the abbrev without expanding
;;; because the point would keep going back to the beginning of the
;;; abbrev instead of to the end of the abbrev). We now keep the
;;; abbrev hook from moving backwards.
;;;
(defun idlwave-expand-region-abbrevs (start end)
  "Expand each abbrev occurrence in the region.
Calling from a program, arguments are START END."
  (interactive "r")
  (save-excursion
    (goto-char (min start end))
    (let ((idlwave-show-block nil)          ;Do not blink
          (idlwave-abbrev-move nil))        ;Do not move
      (expand-region-abbrevs start end 'noquery))))

(defun idlwave-quoted ()
  "Returns t if point is in a comment or quoted string.
nil otherwise."
  (or (idlwave-in-comment) (idlwave-in-quote)))

(defun idlwave-in-quote ()
  "Returns location of the opening quote
if point is in a IDL string constant, nil otherwise.
Ignores comment delimiters on the current line.
Properly handles nested quotation marks and octal
constants - a double quote followed by an octal digit."
;;; Treat an octal inside an apostrophe to be a normal string. Treat a
;;; double quote followed by an octal digit to be an octal constant
;;; rather than a string. Therefore, there is no terminating double
;;; quote.
  (save-excursion
    ;; Because single and double quotes can quote each other we must
    ;; search for the string start from the beginning of line.
    (let* ((start (point))
           (eol (progn (end-of-line) (point)))
           (bq (progn (beginning-of-line) (point)))
           (endq (point))
           (data (match-data))
           delim
           found)
      (while  (< endq start)
	;; Find string start
	;; Don't find an octal constant beginning with a double quote
	(if (re-search-forward "\"[^0-7]\\|'\\|\"$" eol 'lim)
	    ;; Find the string end.
	    ;; In IDL, two consecutive delimiters after the start of a
	    ;; string act as an
	    ;; escape for the delimiter in the string.
	    ;; Two consecutive delimiters alone (i.e., not after the
	    ;; start of a string) is the the null string.
	    (progn
	      ;; Move to position after quote
	      (goto-char (1+ (match-beginning 0)))
	      (setq bq (1- (point)))
	      ;; Get the string delimiter
	      (setq delim (char-to-string (preceding-char)))
	      ;; Check for null string
	      (if (looking-at delim)
		  (progn (setq endq (point)) (forward-char 1))
		;; Look for next unpaired delimiter
		(setq found (search-forward delim eol 'lim))
		(while (looking-at delim)
		  (forward-char 1)
		  (setq found (search-forward delim eol 'lim)))
		(if found
		    (setq endq (- (point) 1))
		  (setq endq (point)))
		))
	  (progn (setq bq (point)) (setq endq (point)))))
      (store-match-data data)
      ;; return string beginning position or nil
      (if (> start bq) bq))))

;; Statement templates

;; Replace these with a general template function, something like
;; expand.el (I think there was also something with a name similar to
;; dmacro.el)

(defun idlwave-template (s1 s2 &optional prompt noindent)
  "Build a template with optional prompt expression.

Opens a line if point is not followed by a newline modulo intervening
whitespace.  S1 and S2 are strings.  S1 is inserted at point followed
by S2.  Point is inserted between S1 and S2.  The case of S1 and S2 is
adjusted according to `idlwave-abbrev-change-case'.  If optional argument
PROMPT is a string then it is displayed as a message in the
minibuffer.  The PROMPT serves as a reminder to the user of an
expression to enter.

The lines containing S1 and S2 are reindented using `indent-region'
unless the optional second argument NOINDENT is non-nil."
  (cond ((eq idlwave-abbrev-change-case 'down)
	 (setq s1 (downcase s1) s2 (downcase s2)))
	(idlwave-abbrev-change-case
	 (setq s1 (upcase s1) s2 (upcase s2))))
  (let ((beg (save-excursion (beginning-of-line) (point)))
        end)
    (if (not (looking-at "\\s-*\n"))
        (open-line 1))
    (insert s1)
    (save-excursion
      (insert s2)
      (setq end (point)))
    (if (not noindent)
        (indent-region beg end nil))
    (if (stringp prompt)
        (message prompt))))

(defun idlwave-rw-case (string)
  "Make STRING have the case required by `idlwave-reserved-word-upcase'."
  (if idlwave-reserved-word-upcase
      (upcase string)
    string))

(defun idlwave-elif ()
  "Build skeleton IDL if-else block."
  (interactive)
  (idlwave-template
   (idlwave-rw-case "if")
   (idlwave-rw-case " then begin\n\nendif else begin\n\nendelse")
   "Condition expression"))

(defun idlwave-case ()
  "Build skeleton IDL case statement."
  (interactive)
  (idlwave-template 
   (idlwave-rw-case "case")
   (idlwave-rw-case " of\n\nendcase")
   "Selector expression"))

(defun idlwave-for ()
  "Build skeleton for loop statment."
  (interactive)
  (idlwave-template 
   (idlwave-rw-case "for")
   (idlwave-rw-case " do begin\n\nendfor")
   "Loop expression"))

(defun idlwave-if ()
  "Build skeleton for loop statment."
  (interactive)
  (idlwave-template
   (idlwave-rw-case "if")
   (idlwave-rw-case " then begin\n\nendif")
   "Scalar logical expression"))

(defun idlwave-procedure ()
  (interactive)
  (idlwave-template 
   (idlwave-rw-case "pro")
   (idlwave-rw-case "\n\nreturn\nend")
   "Procedure name"))

(defun idlwave-function ()
  (interactive)
  (idlwave-template 
   (idlwave-rw-case "function")
   (idlwave-rw-case "\n\nreturn\nend")
   "Function name"))

(defun idlwave-repeat ()
  (interactive)
  (idlwave-template
   (idlwave-rw-case "repeat begin\n\nendrep until")
   (idlwave-rw-case "")
   "Exit condition"))

(defun idlwave-while ()
  (interactive)
  (idlwave-template 
   (idlwave-rw-case "while")
   (idlwave-rw-case " do begin\n\nendwhile")
   "Entry condition"))

(defun idlwave-split-string (string &optional pattern)
  "Return a list of substrings of STRING which are separated by PATTERN.
If PATTERN is omitted, it defaults to \"[ \\f\\t\\n\\r\\v]+\"."
  (or pattern
      (setq pattern "[ \f\t\n\r\v]+"))
  (let (parts (start 0))
    (while (string-match pattern string start)
      (setq parts (cons (substring string start (match-beginning 0)) parts)
	    start (match-end 0)))
    (nreverse (cons (substring string start) parts))))

(defun idlwave-replace-string (string replace_string replace_with)
  (let* ((start 0)
	 (last (length string))
	 (ret_string "")
	 end)
    (while (setq end (string-match replace_string string start))
      (setq ret_string
	    (concat ret_string (substring string start end) replace_with))
      (setq start (match-end 0)))
    (setq ret_string (concat ret_string (substring string start last)))))

(defun idlwave-get-buffer-visiting (file)
  ;; Return the buffer currently visiting FILE
  (cond
   ((boundp 'find-file-compare-truenames) ; XEmacs
    (let ((find-file-compare-truenames t))
      (get-file-buffer file)))
   ((fboundp 'find-buffer-visiting)       ; Emacs
    (find-buffer-visiting file))
   (t (error "This should not happen (idlwave-get-buffer-visiting)"))))

(defun idlwave-find-file-noselect (file)
  ;; Return a buffer visiting file.
  (or (idlwave-get-buffer-visiting file)
      (find-file-noselect file)))

(defvar idlwave-scanned-lib-directories)
(defun idlwave-find-lib-file-noselet (file)
  ;; Find FILE on the scanned lib path and return a buffer visiting it
  (let* ((dirs idlwave-scanned-lib-directories)
	 dir efile)
    (catch 'exit
      (while (setq dir (pop dirs))
	(if (file-regular-p
	     (setq efile (expand-file-name file dir)))
	    (throw 'exit (idlwave-find-file-noselect efile)))))))

(defun idlwave-make-tags ()
  "Creates the IDL tags file IDLTAGS in the current directory from
the list of directories specified in the minibuffer. Directories may be
for example: . /usr/local/rsi/idl/lib. All the subdirectories of the
specified top directories are searched if the directory name is prefixed
by @. Specify @ directories with care, it may take a long, long time if
you specify /."
  (interactive)
  (let (directory directories cmd append status numdirs dir getsubdirs
		  buffer save_buffer files numfiles item errbuf)
    
    ;;
    ;; Read list of directories
    (setq directory (read-string "Tag Directories: " "."))
    (setq directories (idlwave-split-string directory "[ \t]+"))
    ;;
    ;; Set etags command, vars
    (setq cmd "etags --output=IDLTAGS --language=none --regex='/[
\\t]*[pP][Rr][Oo][ \\t]+\\([^ \\t,]+\\)/' --regex='/[
\\t]*[Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn][ \\t]+\\([^ \\t,]+\\)/' ")
    (setq append " ")
    (setq status 0)
    ;;
    ;; For each directory
    (setq numdirs 0)
    (setq dir (nth numdirs directories))
    (while (and dir)
      ;;
      ;; Find the subdirectories
      (if (string-match "^[@]\\(.+\\)$" dir)
	  (setq getsubdirs t) (setq getsubdirs nil))
      (if (and getsubdirs) (setq dir (substring dir 1 (length dir))))
      (setq dir (expand-file-name dir))
      (if (file-directory-p dir)
	  (progn
	    (if (and getsubdirs)
		(progn
		  (setq buffer (get-buffer-create "*idltags*"))
		  (call-process "sh" nil buffer nil "-c"
				(concat "find " dir " -type d -print"))
		  (setq save_buffer (current-buffer))
		  (set-buffer buffer)
		  (setq files (idlwave-split-string
			       (idlwave-replace-string
				(buffer-substring 1 (point-max))
				"\n" "/*.pro ")
			       "[ \t]+"))
		  (set-buffer save_buffer)
		  (kill-buffer buffer))
	      (setq files (list (concat dir "/*.pro"))))
	    ;;
	    ;; For each subdirectory
	    (setq numfiles 0)
	    (setq item (nth numfiles files))
	    (while (and item)
	      ;;
	      ;; Call etags
	      (if (not (string-match "^[ \\t]*$" item))
		  (progn
		    (message (concat "Tagging " item "..."))
		    (setq errbuf (get-buffer-create "*idltags-error*"))
		    (setq status (+ status
				    (call-process "sh" nil errbuf nil "-c"
						  (concat cmd append item))))
		    ;;
		    ;; Append additional tags
		    (setq append " --append ")
		    (setq numfiles (1+ numfiles))
		    (setq item (nth numfiles files)))
		(progn
		  (setq numfiles (1+ numfiles))
		  (setq item (nth numfiles files))
		  )))
	    
	    (setq numdirs (1+ numdirs))
	    (setq dir (nth numdirs directories)))
	(progn
	  (setq numdirs (1+ numdirs))
	  (setq dir (nth numdirs directories)))))
    
    (setq errbuf (get-buffer-create "*idltags-error*"))
    (if (= status 0)
	(kill-buffer errbuf))
    (message "")
    ))

(defun idlwave-toggle-comment-region (beg end &optional n)
  "Comment the lines in the region if the first non-blank line is
commented, and conversely, uncomment region. If optional prefix arg
N is non-nil, then for N positive, add N comment delimiters or for N
negative, remove N comment delimiters.
Uses `comment-region' which does not place comment delimiters on
blank lines."
  (interactive "r\nP")
  (if n
      (comment-region beg end (prefix-numeric-value n))
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      ;; skip blank lines
      (skip-chars-forward " \t\n")
      (if (looking-at (concat "[ \t]*\\(" comment-start "+\\)"))
          (comment-region beg end
                          (- (length (buffer-substring
                                      (match-beginning 1)
                                      (match-end 1)))))
        (comment-region beg end)))))


;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------
;; ----------------------------------------------------------------------------
;;
;; Completion and Routine Info
;;

;; String "intern" functions

;; For the completion and routine info function, we want to normalize
;; the case of procedure names etc.  We do this by "interning" these
;; string is a hand-crafted way.  Hashes are used to map the downcase
;; version of the strings to the cased versions.  Since these cased
;; versions are really lisp objects, we can use `eq' to search, which
;; is a large performance boost.
;; All new strings need to be "sinterned".  We do this as early as
;; possible after getting these strings from completion or buffer
;; substrings.  So most of the code can simply assume to deal with
;; "sinterned" strings.  The only exception is that the functions
;; which scan whole buffers for routine information do not intern the
;; grabbed strings.  This is only done afterwards.  Therefore in these 
;; functions it is *not* save to assume the strings can be compared
;; with `eq' and be fed into the routine assq functions.

;; Here we define the hashing functions.

;; The variables which hold the hashes.
(defvar idlwave-sint-routines '(nil))
(defvar idlwave-sint-keywords '(nil))
(defvar idlwave-sint-methods  '(nil))
(defvar idlwave-sint-classes  '(nil))
(defvar idlwave-sint-files    '(nil))

(defun idlwave-reset-sintern (&optional what)
  "Reset all sintern hashes."
  ;; Make sure the hash functions are accessible.
  (if (or (not (fboundp 'gethash))
	  (not (fboundp 'puthash)))
      (progn 
	(require 'cl)
	(or (fboundp 'puthash)
	    (defalias 'puthash 'cl-puthash))))
  (let ((entries '((idlwave-sint-routines 1000 10)
		   (idlwave-sint-keywords 1000 10)
		   (idlwave-sint-methods   100 10)
		   (idlwave-sint-classes    10 10))))

    ;; Make sure these are lists
    (loop for entry in entries
      for var = (car entry)
      do (if (not (consp (symbol-value var))) (set var (list nil))))

    (when (or (eq what t) (eq what 'syslib)
	      (null (cdr idlwave-sint-routines)))
      ;; Reset the system & library hash
      (loop for entry in entries
	for var = (car entry) for size = (nth 1 entry)
	do (setcdr (symbol-value var) 
		   (make-hash-table ':size size ':test 'equal)))
      (setq idlwave-sint-files nil))

    (when (or (eq what t) (eq what 'bufsh)
	      (null (car idlwave-sint-routines)))
      ;; Reset the buffer & shell hash
      (loop for entry in entries
	for var = (car entry) for size = (nth 1 entry)
	do (setcar (symbol-value var) 
		   (make-hash-table ':size size ':test 'equal))))))

(defun idlwave-sintern-routine-or-method (name &optional class set)
  (if class
      (idlwave-sintern-method name set)
    (idlwave-sintern-routine name set)))

(defun idlwave-sintern (stype &rest args)
  (apply (intern (concat "idlwave-sintern-" (symbol-name stype))) args))

;;(defmacro idlwave-sintern (type var)
;;  `(cond ((not (stringp name)) name)
;;	 ((gethash (downcase name) (cdr ,var)))
;;	 ((gethash (downcase name) (car ,var)))
;;	 (set (idlwave-sintern-set name ,type ,var set))
;;	 (name)))

(defun idlwave-sintern-routine (name &optional set)
  (cond ((not (stringp name)) name)
	((gethash (downcase name) (cdr idlwave-sint-routines)))
	((gethash (downcase name) (car idlwave-sint-routines)))
	(set (idlwave-sintern-set name 'routine idlwave-sint-routines set))
	(name)))
(defun idlwave-sintern-keyword (name &optional set)
  (cond ((not (stringp name)) name)
	((gethash (downcase name) (cdr idlwave-sint-keywords)))
	((gethash (downcase name) (car idlwave-sint-keywords)))
	(set (idlwave-sintern-set name 'keyword idlwave-sint-keywords set))
	(name)))
(defun idlwave-sintern-method (name &optional set)
  (cond ((not (stringp name)) name)
	((gethash (downcase name) (cdr idlwave-sint-methods)))
	((gethash (downcase name) (car idlwave-sint-methods)))
	(set (idlwave-sintern-set name 'method idlwave-sint-methods set))
	(name)))
(defun idlwave-sintern-class (name &optional set)
  (cond ((not (stringp name)) name)
	((gethash (downcase name) (cdr idlwave-sint-classes)))
	((gethash (downcase name) (car idlwave-sint-classes)))
	(set (idlwave-sintern-set name 'class idlwave-sint-classes set))
	(name)))

(defun idlwave-sintern-file (name &optional set)
  (car (or (member name idlwave-sint-files)
	   (setq idlwave-sint-files (cons name idlwave-sint-files)))))

(defun idlwave-sintern-set (name type tables set)
  (let* ((func (or (cdr (assq type idlwave-completion-case))
		   'identity))
	 (iname (funcall (if (eq func 'preserve) 'identity func) name))
	 (table (if (eq set 'sys) (cdr tables) (car tables))))
    (puthash (downcase name) iname table)
    iname))

(defun idlwave-sintern-rinfo-list (list &optional set)
  "Sintern all strings in the rinfo LIST.  With optional parameter SET:
also set new patterns.  Probably this will always have to be t."
  (let (entry name type class kwds res source call olh new)
    (while list
      (setq entry (car list)
	    list (cdr list)
	    name (car entry)
	    type (nth 1 entry)
	    class (nth 2 entry)
	    source (nth 3 entry)
	    call (nth 4 entry)
	    kwds (nth 5 entry)
	    olh (nth 6 entry))
      (setq kwds (mapcar (lambda (x)
			   (list (idlwave-sintern-keyword (car x) set)))
			 kwds))
      (if class
	  (progn
	    (if (symbolp class) (setq class (symbol-name class)))
	    (setq class (idlwave-sintern-class class set))
	    (setq name (idlwave-sintern-method name set)))
	(setq name (idlwave-sintern-routine name set)))
      (if (stringp (cdr source))
	  (setcdr source (idlwave-sintern-file (cdr source) t)))
      (setq new (if olh
		    (list name type class source call kwds olh)
		  (list name type class source call kwds)))
      (setq res (cons new res)))
    (nreverse res)))

;;---------------------------------------------------------------------------


;; The variables which hold the information
(defvar idlwave-builtin-routines nil
  "Holds the routine-info obtained by scanning buffers.")
(defvar idlwave-buffer-routines nil
  "Holds the routine-info obtained by scanning buffers.")
(defvar idlwave-compiled-routines nil
  "Holds the procedure routine-info obtained by asking the shell.")
(defvar idlwave-library-routines nil
  "Holds the procedure routine-info from the library scan.")
(defvar idlwave-scanned-lib-directories nil
  "The directories scanned to get libinfo.")
(defvar idlwave-routines nil
  "Holds the combinded procedure routine-info.")
(defvar idlwave-class-alist nil
  "Holds the class names known to IDLWAVE.")
(defvar idlwave-class-history nil
  "The history of classes selected with the minibuffer.")
(defvar idlwave-force-class-query nil)
(defvar idlwave-before-completion-wconf nil
  "The window configuration just before the completion buffer was displayed.")

;;
;; The code to get routine info from different sources.

(defvar idlwave-builtin-routines)
(defun idlwave-routines ()
  "Provide a list of IDL routines.
This routine loads the builtin routines on the first call.  Later it
only returns the value of the variable."
  (or idlwave-routines
      (progn
	(idlwave-update-routine-info)
	;; return the current value
	idlwave-routines)))

(defun idlwave-update-routine-info (&optional arg)
  "Update the internal routine-info lists.
These lists are used by `idlwave-routine-info' (\\[idlwave-routine-info])
and by `idlwave-complete' (\\[idlwave-complete]) to provide information
about individual routines.

The information can come from 4 sources:
1. IDL programs in the current editing session
2. Compiled modules in an IDL shell running as Emacs subprocess
3. A list which covers the IDL system routines.
4. A list which covers the prescanned library files.

Scans all IDLWAVE-mode buffers of the current editing session (see
`idlwave-scan-all-buffers-for-routine-info').
When an IDL shell is running, this command also queries the IDL program
for currently compiled routines.

With prefix ARG, also reload the system and library lists.
With two prefix ARG's, also rescans the library tree."
  (interactive "P")
  (if (equal arg '(16))
      (idlwave-create-libinfo-file t)
    (let* ((reload (or arg
		       idlwave-buffer-case-takes-precedence
		       (null idlwave-builtin-routines))))
      
      (setq idlwave-buffer-routines nil
	    idlwave-compiled-routines nil)
      ;; Reset the appropriate hashes
      (idlwave-reset-sintern (cond (reload t)
				   ((null idlwave-builtin-routines) t)
				   (t 'bufsh)))
      
      (if idlwave-buffer-case-takes-precedence
	  ;; We can safely scan the buffer stuff first
	  (progn
	    (idlwave-update-buffer-routine-info)
	    (and reload (idlwave-load-system-rinfo)))
	;; We first do the system info, and then the buffers
	(and reload (idlwave-load-system-rinfo))
	(idlwave-update-buffer-routine-info))

      ;; Let's see if there is a shell
      (let* ((shell-is-running (and (fboundp 'idlwave-shell-is-running)
				    (idlwave-shell-is-running)))
	     (ask-shell (and shell-is-running
			     idlwave-query-shell-for-routine-info)))
      
	(if (or (not ask-shell)
		(not (interactive-p)))
	    ;; 1. If we are not going to ask the shell, we need to do the
	    ;;    concatenation now.
	    ;; 2. When this function is called non-interactively, it means
	    ;;    that someone needs routine info *now*.  The shell update
	    ;;    causes the concatenation *delayed*, so not in time for
	    ;;    the current command.  Therefore, we do a concatenation
	    ;;    now, even though the shell might do it again.
	    (idlwave-concatenate-rinfo-lists))
      
	(when ask-shell
	  ;; Ask the shell about the routines it knows.
	  (message "Querying the shell")
	  (idlwave-shell-update-routine-info))))))

(defun idlwave-load-system-rinfo ()
  ;; Load and case-treat the system and lib info files.
  (load "idlw-rinfo" t)
  (message "Normalizing idlwave-builtin-routines...")
  (setq idlwave-builtin-routines
	(idlwave-sintern-rinfo-list idlwave-builtin-routines 'sys))
  (message "Normalizing idlwave-builtin-routines...done")
  (setq idlwave-routines idlwave-builtin-routines)
  (when (and (stringp idlwave-libinfo-file)
	     (file-regular-p idlwave-libinfo-file))
    (condition-case nil
	(progn
	  (load-file idlwave-libinfo-file)
	  (message "Normalizing idlwave-library-routines...")
	  (setq idlwave-library-routines (idlwave-sintern-rinfo-list
					  idlwave-library-routines 'sys))
	  (message "Normalizing idlwave-library-routines...done"))
      (error nil))))

(defun idlwave-update-buffer-routine-info ()
  (let (res)
    (if idlwave-scan-all-buffers-for-routine-info
	(progn
	  ;; Scan all buffers, current buffer last
	  (message "Scanning all buffers...")
	  (setq res (idlwave-get-routine-info-from-buffers 
		     (reverse (buffer-list)))))
      ;; Just scan this buffer
      (if (eq major-mode 'idlwave-mode)
	  (progn
	    (message "Scanning current buffer...")
	    (setq res (idlwave-get-routine-info-from-buffers
		       (list (current-buffer)))))))
    ;; Put the result into the correct variable
    (setq idlwave-buffer-routines 
	  (idlwave-sintern-rinfo-list res t))))

(defun idlwave-concatenate-rinfo-lists ()
  "Put the different sources for routine information together."
  ;; The sequence here is important because earlier definitions shadow 
  ;; later ones.  We assume that if things in the buffers are newer
  ;; then in the shell of the system, it is meant to be different.
  ;; FIXME: should the builtin stuff be before the library?
  ;;        This is how IDL searches, the user may also have
  ;;        functions overloading system stuff, and then the lib
  ;;        should be first.  Difficult to find a general solution.
  ;; FIXME: can't we use nconc here in some way, to save memory?
  ;;        This is possible for buffer abd shell stuff, but these are 
  ;;        small anyway, and so it is not so critical.
  (setq idlwave-routines (append idlwave-buffer-routines
				 idlwave-compiled-routines
				 idlwave-library-routines
				 idlwave-builtin-routines))
  (setq idlwave-class-alist nil)
  (let (class)
    (loop for x in idlwave-routines do
      (when (and (setq class (nth 2 x))
		 (not (assq class idlwave-class-alist)))
	(push (list class) idlwave-class-alist))))
  ;; Give a message with information about the number of routines we have.
  (message 
   "Routine info updated:  buffer(%d)  compiled(%d)  library(%d)  system(%d)"
   (length idlwave-buffer-routines)
   (length idlwave-compiled-routines)
   (length idlwave-library-routines)
   (length idlwave-builtin-routines)))

;;----- Scanning buffers -------------------

(defun idlwave-get-routine-info-from-buffers (buffers)
  "Call `idlwave-get-buffer-routine-info' on idlwave-mode buffers in BUFFERS."
  (let (buf routine-lists res)
    (save-excursion
      (while (setq buf (pop buffers))
	(set-buffer buf)
	(if (eq major-mode 'idlwave-mode)
	    ;; yes, this buffer has the right mode.
	    (progn (setq res (condition-case nil
				 (idlwave-get-buffer-routine-info)
			       (error nil)))
		   (push res routine-lists)))))
    ;; Concatenate the individual lists and return the result
    (apply 'nconc routine-lists)))

(defun idlwave-get-buffer-routine-info ()
  "Scan the current buffer for routine info.  Return (PRO-LIST FUNC-LIST)."
  (let* ((case-fold-search t)
	 routine-list string entry)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward 
		"^[ \t]*\\<\\(pro\\|function\\)\\>" nil t)
	  (setq string (buffer-substring 
			(match-beginning 0)
			(progn 
			  (idlwave-end-of-statement)
			  (point))))
	  (setq entry (idlwave-parse-definition string))
	  (push entry routine-list))))
    routine-list))

(defun idlwave-parse-definition (string)
  "Parse a module definition."
  (let ((case-fold-search t)
	start name args type keywords class)
    ;; Remove comments
    (while (string-match ";.*" string)
      (setq string (replace-match "" t t string)))
    ;; Remove the continuation line stuff
    (while (string-match "\\([^a-zA-Z0-9$_]\\)\\$[ \t]*\n" string)
      (setq string (replace-match "\\1 " t nil string)))
    ;; Match the name and type.
    (when (string-match
	   "\\<\\(pro\\|function\\)\\>\\s-+\\(\\([a-zA-Z0-9$_]+\\)::\\)?\\([a-zA-Z0-9$_]+\\)" string)
      (setq start (match-end 0))
      (setq type (downcase (match-string 1 string)))
      (if (match-beginning 3)
	  (setq class (match-string 3 string)))
      (setq name (match-string 4 string)))
    ;; Match normal args and keyword args
    (while (string-match
	    ",\\s-*\\([a-zA-Z][a-zA-Z0-9$_]*\\|_extra\\)\\s-*\\(=\\)?"
	    string start)
      (setq start (match-end 0))
      (if (match-beginning 2)
	  (push (match-string 1 string) keywords)
	(push (match-string 1 string) args)))
    ;; Normalize and sort.
    (setq args (nreverse args))
    (setq keywords (sort keywords (lambda (a b) 
				    (string< (downcase a) (downcase b)))))
    ;; Make and return the entry
    ;; We don't know which argument are optional, so this information
    ;; will not be contained in the calling sequence.
    (list name
	  (if (equal type "pro") 'pro 'fun)
	  class
	  (cond ((not (boundp 'idlwave-scanning-lib))
		 (cons 'buffer (buffer-file-name)))
		((string= (downcase 
			   (file-name-sans-extension
			    (file-name-nondirectory (buffer-file-name))))
			  (downcase name))
		 (list 'lib))
		(t (cons 'lib (file-name-nondirectory (buffer-file-name)))))
	  (concat 
	   (if (string= type "function") "Result = " "")
	   (if class "Obj ->[%s::]" "")
	   "%s"
	   (if args
	       (concat
		(if (string= type "function") "(" ", ")
		(mapconcat 'identity args ", ")
		(if (string= type "function") ")" ""))))
	  (if keywords
	      (mapcar 'list keywords)
	    nil))))

;;----- Scanning the library -------------------

(defun idlwave-create-libinfo-file (&optional arg)
  "Scan all files on selected dirs of IDL search path for routine information.
A widget checklist will allow you to choose the directories.
Write the result as a file `idlwave-libinfo-file'.  When this file exists,
will be automatically loaded to give routine information about library
routines.
With ARG, just rescan the same directories as last time - so no widget
will pop up."
  (interactive "P")
  ;; Make sure the file is loaded if it exists.
  (if (and (stringp idlwave-libinfo-file)
	   (file-regular-p idlwave-libinfo-file))
      (condition-case nil
	  (load-file idlwave-libinfo-file)
	(error nil)))
  ;; Make sure the file name makes sense
  (unless (and (stringp idlwave-libinfo-file)
	       (file-accessible-directory-p
		(file-name-directory idlwave-libinfo-file))
	       (not (string= "" (file-name-nondirectory 
				 idlwave-libinfo-file))))
    (error "`idlwave-libinfo-file' does not point to file in accessible directory."))
  
  (cond
   ((and arg idlwave-scanned-lib-directories)
    ;; Rescan the known directories
    (idlwave-scan-lib-files idlwave-scanned-lib-directories))
   (idlwave-library-path
    ;; Get the directories from that variable
    (idlwave-display-libinfo-widget
     (idlwave-expand-path idlwave-library-path)
     idlwave-scanned-lib-directories))
   (t
    ;; Ask the shell for the path and run the widget
    (message "Asking the shell for IDL path...")
    (idlwave-shell-send-command 
     "__pa=expand_path(!path,/array)&for i=0,n_elements(__pa)-1 do print,'PATH:',__pa[i]"
     '(idlwave-libinfo-command-hook nil)
     'hide))))

(defun idlwave-libinfo-command-hook (&optional arg)
  ;; Command hook used by `idlwave-create-libinfo-file'.
  (if arg
      ;; Scan immediately
      (idlwave-scan-lib-files idlwave-scanned-lib-directories)
    ;; Display the widget
    (idlwave-display-libinfo-widget (idlwave-shell-path-filter)
				    idlwave-scanned-lib-directories)))

(defvar idlwave-shell-command-output)
(defun idlwave-shell-path-filter ()
  ;; Convert the output of the path query into a list of directories
  (let ((path-string idlwave-shell-command-output)
	(case-fold-search t)
	(start 0)
	dirs)
    (while (string-match "^PATH:[ \t]*\\(.*\\)\n" path-string start)
      (push (match-string 1 path-string) dirs)
      (setq start (match-end 0)))
    (nreverse dirs)))

(defconst idlwave-libinfo-widget-help-string 
  "This is the front-end to the creation of IDLWAVE library routine info.
Please select below the directories on IDL's search path from which you
would like to extract routine information, which will be stored in the file

             %s

If this is not the correct file, first set variable `idlwave-libinfo-file'.
Then call this command again.
After selecting the directories, choose [Scan & Save] to scan the library
directories and save the routine info.
\n")

(defvar idlwave-widget)
(defvar widget-keymap)
(defun idlwave-display-libinfo-widget (dirs selected-dirs)
  "Create the widget to select IDL search path directories for scanning."
  (interactive)
  (require 'widget)
  (require 'wid-edit)
  (unless dirs
      (error "Don't know IDL's search path"))

  ;; Allow only those directories to be selected which are in the path.
  (setq selected-dirs (delq nil (mapcar (lambda (x)
					  (if (member x dirs) x nil))
					selected-dirs)))
  (kill-buffer (get-buffer-create "*IDLWAVE Widget*"))
  (switch-to-buffer (get-buffer-create "*IDLWAVE Widget*"))
  (kill-all-local-variables)
  (make-local-variable 'idlwave-widget)
  (widget-insert (format idlwave-libinfo-widget-help-string
			 idlwave-libinfo-file))
  
  (widget-create 'push-button
		 :notify 'idlwave-widget-scan-lib-files
		 :help-echo "testing"
		 "Scan & Save")
  (widget-insert "  ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (kill-buffer (current-buffer)))
		 "Quit")
  (widget-insert "  ")
  (widget-create 'push-button
		 :notify 'idlwave-delete-libinfo-file
		 "Delete File")
  (widget-insert "  ")
  (widget-create 'push-button
		 :notify '(lambda (&rest ignore)
			    (idlwave-display-libinfo-widget
			     (widget-get idlwave-widget :path-dirs)
			     (widget-get idlwave-widget :path-dirs)))
		 "Select All")
  (widget-insert "  ")
  (widget-create 'push-button
		 :notify '(lambda (&rest ignore)
			    (idlwave-display-libinfo-widget
			     (widget-get idlwave-widget :path-dirs)
			     nil))
		 "Deselect All")
  (widget-insert "\n\n")

  (widget-insert "Select Directories\n")
  
  (setq idlwave-widget
	(apply 'widget-create
	       'checklist
	       :value selected-dirs
	       :greedy t
	       :tag "List of directories"
	       (mapcar (lambda (x) (list 'item x)) dirs)))
  (widget-put idlwave-widget :path-dirs dirs)
  (widget-insert "\n")
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min))
  (delete-other-windows))
  
(defun idlwave-delete-libinfo-file (&rest ignore)
  (if (yes-or-no-p
       (format "Delete file %s " idlwave-libinfo-file))
      (progn
	(delete-file idlwave-libinfo-file)
	(message "%s has been deleted" idlwave-libinfo-file))))

(defun idlwave-widget-scan-lib-files (&rest ignore)
  ;; Call `idlwave-scan-lib-files' with data taken from the widget.
  (let* ((widget idlwave-widget)
	 (selected-dirs (widget-value widget)))
    (idlwave-scan-lib-files selected-dirs)))

(defvar font-lock-mode)
(defun idlwave-scan-lib-files (selected-dirs)
  ;; Scan the files in SELECTED-DIRS and store the info in a file
  (let* ((idlwave-scanning-lib t)
	 (idlwave-completion-case nil)
	 dirs dir files file)
    (setq idlwave-library-routines nil)
    (setq idlwave-scanned-lib-directories selected-dirs)
    (save-excursion
      (set-buffer (get-buffer-create "*idlwave-scan.pro*"))
      (idlwave-mode)
      (setq dirs (reverse selected-dirs))
      (while (setq dir (pop dirs))
	(when (file-directory-p dir)
	  (setq files (directory-files dir 'full "\\.[pP][rR][oO]\\'"))
	  (while (setq file (pop files))
	    (when (file-regular-p file)
	      (if (not (file-readable-p file))
		  (message "Skipping %s (no read permission)" file)
		(message "Scanning %s..." file)
		(erase-buffer)
		(insert-file-contents file 'visit)
		(setq idlwave-library-routines
		      (append (idlwave-get-routine-info-from-buffers
			       (list (current-buffer)))
			      idlwave-library-routines)))
	      )))))
    (kill-buffer "*idlwave-scan.pro*")
    (kill-buffer (get-buffer-create "*IDLWAVE Widget*"))
    (let ((font-lock-maximum-size 0))
      (find-file idlwave-libinfo-file))
    (if (and (boundp 'font-lock-mode)
	     font-lock-mode)
	(font-lock-mode 0))
    (erase-buffer)
    (insert ";; IDLWAVE libinfo file\n")
    (insert (format ";; Created %s\n\n" (current-time-string)))

    ;; Define the variable which contains a list of all scanned directories
    (insert "\n(setq idlwave-scanned-lib-directories\n    '(")
    (mapcar (lambda (x)
	      (insert (format "\n      \"%s\"" x)))
	    selected-dirs)
    (insert "))\n")
    ;; Define the routine info list
    (insert "\n(setq idlwave-library-routines\n    '(")
    (mapcar (lambda (x)
	      (insert "\n    ")
	      (insert (with-output-to-string (prin1 x))))
	    idlwave-library-routines)
    (insert (format "))\n\n;;; %s ends here\n"
		    (file-name-nondirectory idlwave-libinfo-file)))
    (goto-char (point-min))
    ;; Save the buffer
    (save-buffer 0)
    (kill-buffer (current-buffer)))
  (message "Info for %d routines saved in %s"
	   (length idlwave-library-routines)
	   idlwave-libinfo-file)
  (sit-for 2)
  (idlwave-update-routine-info t))

(defun idlwave-expand-path (path &optional default-dir)
  ;; Expand parts of path starting with '+' recursively into directory list.
  ;; Relative recursive path elements are expanded relative to DEFAULT-DIR.
  (message "Expanding path...")
  (let (path1 dir recursive)
    (while (setq dir (pop path))
      (if (setq recursive (string= (substring dir 0 1) "+"))
	  (setq dir (substring dir 1)))
      (if (and recursive
	       (not (file-name-absolute-p dir)))
	  (setq dir (expand-file-name dir default-dir)))
      (if recursive
	  ;; Expand recursively
	  (setq path1 (append (idlwave-recursive-directory-list dir) path1))
	;; Keep unchanged
	(push dir path1)))
    (message "Expanding path...done")
    (nreverse path1)))

(defun idlwave-recursive-directory-list (dir)
  ;; Return a list of all directories below DIR, including DIR itself
  (let ((path (list dir)) path1 file files)
    (while (setq dir (pop path))
      (when (file-directory-p dir)
	(setq files (nreverse (directory-files dir t "[^.]")))
	(while (setq file (pop files))
	  (if (file-directory-p file) 
	      (push (file-name-as-directory file) path)))
	(push dir path1)))
    path1))

;;----- Asking the shell -------------------

;; First, here is the idl program which can be used to query IDL for
;; defined routines. 
(defconst idlwave-routine-info.pro
  "
function idlwave_make_info_entry,name,func=func,separator=sep
  ;; See if it's an object method
  func    = keyword_set(func) 
  methsep = strpos(name,'::')
  meth    = methsep ne -1
  
  ;; Get routine info
  pars   = routine_info(name,/parameters,functions=func)
  source = routine_info(name,/source,functions=func)
  nargs  = pars.num_args
  nkw    = pars.num_kw_args
  if nargs gt 0 then args = pars.args
  if nkw   gt 0 then kwargs = pars.kw_args
  
  ;; Trim the class, and make the name
  if meth then begin 
      class = strmid(name,0,methsep)
      name  = strmid(name,methsep+2,strlen(name)-1)
      if nargs gt 0 then begin 
          ;; remove the self argument
          wh = where(args ne 'SELF',nargs)
          if nargs gt 0 then args = args(wh)
      endif
  endif else begin
      ;; No class, just a normal routine.
      class = \"\"
  endelse
   
  ;; Calling sequence
  cs = \"\"
  if func then cs = 'Result = '
  if meth then cs = cs + 'Obj -> [' + '%s' + '::]'
  cs = cs + '%s'
  if func then cs = cs + '(' else if nargs gt 0 then cs = cs + ', '
  if nargs gt 0 then begin
      for j=0,nargs-1 do begin
          cs = cs + args(j)
          if j lt nargs-1 then cs = cs + ', '
      endfor
  end
  if func then cs = cs + ')'
  ;; Keyword arguments
  kwstring = ''
  if nkw gt 0 then begin
      for j=0,nkw-1 do begin
          kwstring = kwstring + ' ' + kwargs(j)
      endfor
  endif
  
  ret=(['IDLWAVE-PRO','IDLWAVE-FUN', $
        'IDLWAVE-PRO','IDLWAVE-FUN'])(func+2*meth)
  
  return,ret + ': ' + name + sep + class + sep + source(0).path  $
    + sep + cs + sep + kwstring
end

pro idlwave_routine_info
  sep = '<@>'
  print,'>>>BEGIN OF IDLWAVE ROUTINE INFO (\"' + sep + '\" IS THE SEPARATOR)'
  all = routine_info()
  for i=0,n_elements(all)-1 do $
    print,idlwave_make_info_entry(all(i),separator=sep)
  all = routine_info(/functions)
  for i=0,n_elements(all)-1 do $
    print,idlwave_make_info_entry(all(i),/func,separator=sep)
  print,'>>>END OF IDLWAVE ROUTINE INFO'
end
" 
  "The idl program to get the routine info stuff.
The output of this program is parsed by `idlwave-shell-routine-info-filter'.")

(defun idlwave-shell-routine-info-filter ()
  "Function which parses the special output from idlwave_routine_info.pro."
  (let ((text idlwave-shell-command-output)
	(start 0)
	sep sep-re file type spec specs name cs key keys class)
    ;; Initialize variables
    (setq idlwave-compiled-routines nil)
    ;; Cut out the correct part of the output.
    (if (string-match
	 "^>>>BEGIN OF IDLWAVE ROUTINE INFO (\"\\(.+\\)\" IS THE SEPARATOR.*"
	 text)
	(setq sep (match-string 1 text)
	      sep-re (concat (regexp-quote sep) " *")
	      text (substring text (match-end 0)))
      (error "Routine Info error: No match for BEGIN line"))
    (if (string-match "^>>>END OF IDLWAVE ROUTINE INFO.*" text)
	(setq text (substring text 0 (match-beginning 0)))
      (error "Routine Info error: No match for END line"))
    ;; Match the output lines
    (while (string-match "^IDLWAVE-\\(PRO\\|FUN\\): \\(.*\\)" text start)
      (setq start (match-end 0))
      (setq type (match-string 1 text)
	    spec (match-string 2 text)
	    specs (idlwave-split-string spec sep-re)
	    name (nth 0 specs)
	    class (if (equal (nth 1 specs) "") nil (nth 1 specs))
	    file (nth 2 specs)
	    cs (nth 3 specs)
	    key (nth 4 specs)
	    keys (if (and (stringp key)
			  (not (string-match "\\` *\\'" key)))
		     (mapcar 'list 
			     (delete "" (idlwave-split-string key " +")))))
      (setq name (idlwave-sintern-routine-or-method name class t)
	    class (idlwave-sintern-class class t)
	    keys (mapcar (lambda (x) 
			   (list (idlwave-sintern-keyword (car x) t))) keys))
      ;; Make sure we use the same string object for the same file
      (setq file (idlwave-sintern-file file t))
      ;; FIXME: What should I do with routines from the temp file???
      ;;        Maybe just leave it in - there is a chance that the
      ;;        routine is still in there.
      ;;      (if (equal file idlwave-shell-temp-pro-file)
      ;;	  (setq file nil))

      ;; In the following ignore routines already defined in buffers,
      ;; assuming that if the buffer stuff differs, it is a "new"
      ;; version. 
      ;; We could do the same for the library to avoid duplicates -
      ;; but I think frequently a user might have several versions of
      ;; the same function in different programs, and in this case the 
      ;; compiled one will be the best guess of all version.
      ;; Therefore, we leave duplicates of library routines in.

      (cond ((string= name "$MAIN$"))    ; ignore this one
	    ((and (string= type "PRO")
		  ;; FIXME: is it OK to make the buffer routines dominate?
		  (not (idlwave-rinfo-assq name 'pro class 
					   idlwave-buffer-routines))
		  ;; FIXME: is it OK to make the library routines dominate?
		  ;;(not (idlwave-rinfo-assq name 'pro class 
		  ;;			   idlwave-library-routines))
		  )
	     (push (list name 'pro class (cons 'compiled file) cs keys)
		   idlwave-compiled-routines))
	    ((and (string= type "FUN")
		  ;; FIXME: is it OK to make the buffer routines dominate?
		  (not (idlwave-rinfo-assq name 'fun class 
					   idlwave-buffer-routines))
		  ;; FIXME: is it OK to make the library routines dominate?
		  ;; (not (idlwave-rinfo-assq name 'fun class 
		  ;;			   idlwave-library-routines))
		  )
	     (push (list name 'fun class (cons 'compiled file) cs keys)
		   idlwave-compiled-routines)))))
  ;; Reverse the definitions so that they are alphabetically sorted.
  (setq idlwave-compiled-routines
	(nreverse idlwave-compiled-routines)))

(defvar idlwave-shell-temp-pro-file)
(defun idlwave-shell-update-routine-info ()
  "Query the shell for routine_info of compiled modules and update the lists."
  ;; Save and compile the procedure
  (save-excursion
    (set-buffer (idlwave-find-file-noselect
		 idlwave-shell-temp-pro-file))
    (erase-buffer)
    (insert idlwave-routine-info.pro)
    (save-buffer 0))
  (idlwave-shell-send-command (concat ".run " idlwave-shell-temp-pro-file)
			      nil 'hide)

  ;; Execute the procedure and analyze the output
  (idlwave-shell-send-command "idlwave_routine_info"
			      '(progn
				 (idlwave-shell-routine-info-filter)
				 (idlwave-concatenate-rinfo-lists))
			      'hide))

;; ---------------------------------------------------------------------------
;;
;; Completion and displaying routine calling sequences

(defun idlwave-complete (&optional arg module class)
  "Complete a function, procedure or keyword name at point.
This function is smart and figures out what can be legally completed
at this point.
- At the beginning of a statement it completes procedure names.
- In the middle of a statement it completes function names.
- after a `(' or `,' in the argument list of a function or procedure,
  it completes a keyword of the relevant function or procedure.
- In the first arg of `OBJ_NEW', it completes a class name.

When several completions are possible, a list will be displayed in the 
*Completions* buffer.  If this list is too long to fit into the
window, scrolling can be achieved by repeatedly pressing \\[idlwave-complete].

The function also knows about object methods.  When it needs a class
name, the action depends upon `idlwave-query-class', which see.  You
can force IDLWAVE to ask you for a class name with a \\[universal-argument] prefix
argument to this command.

See also the variables `idlwave-keyword-completion-adds-equal' and
`idlwave-function-completion-adds-paren'.

The optional ARG can be used to specify the completion type in order
to override IDLWAVE's idea of what should be completed at point.
Possible values are:

0  <=>  query for the completion type
1  <=>  'procedure
2  <=>  'procedure-keyword
3  <=>  'function
4  <=>  'function-keyword
5  <=>  'procedure-method
6  <=>  'procedure-method-keyword
7  <=>  'function-method
8  <=>  'function-method-keyword
9  <=>  'class

For Lisp programmers only:
When we force a keyword, optional argument MODULE can contain the module name.
When we force a method or a method keyword, CLASS can specify the class."
  (interactive "P")
  (idlwave-routines)
  (let* ((where-list
	  (if (and arg
		   (or (integerp arg)
		       (symbolp arg)))
	      (idlwave-make-force-complete-where-list arg module class)
	    (idlwave-where)))
	 (what (nth 2 where-list))
	 (idlwave-force-class-query (equal arg '(4))))

    (if (and module (string-match "::" module))
	(setq class (substring module 0 (match-beginning 0))
	      module (substring module (match-end 0))))

    (cond

     ((and (null arg)
	   (eq (car-safe last-command) 'idlwave-display-completion-list)
	   (get-buffer-window "*Completions*"))
      (setq this-command last-command)
      (idlwave-scroll-completions))

     ((null what)
      (error "Nothing to complete here"))

     ((eq what 'class)
      (idlwave-complete-class))

     ((eq what 'procedure)
      ;; Complete a procedure name
      (let* ((class-selector (idlwave-determine-class (nth 3 where-list) 'pro))
	     (isa (concat "procedure" (if class-selector "-method" "")))
	     (type-selector 'pro))
	(idlwave-complete-in-buffer
	 'procedure (if class-selector 'method 'routine)
	 (idlwave-routines) 'idlwave-selector
	 (format "Select a %s name%s"
		 isa
		 (if class-selector
		     (format " (class is %s)" class-selector)
		   ""))
	 isa
	 'idlwave-attach-method-classes)))

     ((eq what 'function)
      ;; Complete a function name
      (let* ((class-selector (idlwave-determine-class (nth 3 where-list) 'fun))
	     (isa (concat "function" (if class-selector "-method" "")))
	     (type-selector 'fun))
	(idlwave-complete-in-buffer
	 'function (if class-selector 'method 'routine)
	 (idlwave-routines) 'idlwave-selector
	 (format "Select a %s name%s"
		 isa
		 (if class-selector
		     (format " (class is %s)" class-selector)
		   ""))
	 isa
	 'idlwave-attach-method-classes)))

     ((eq what 'procedure-keyword)
      ;; Complete a procedure keyword
      (let* ((where (nth 3 where-list))
	     (name  (car where))
	     (method-selector name)
	     (type-selector 'pro)
	     (class (idlwave-determine-class where 'pro))
	     (class-selector class)
	     (isa (format "procedure%s-keyword" (if class "-method" "")))
	     (entry (idlwave-rinfo-assq
		     name 'pro class (idlwave-routines)))
	     (list (nth 5 entry)))
	(unless (or entry (eq class t))
	  (error "Nothing known about procedure %s"
		 (idlwave-make-full-name class name)))
	(setq list (idlwave-fix-keywords name 'pro class list))
	(unless list (error (format "No keywords available for procedure %s"
				    (idlwave-make-full-name class name))))
	(idlwave-complete-in-buffer
	 'keyword 'keyword list nil
	 (format "Select keyword for procedure %s%s"
		 (idlwave-make-full-name class name)
		 (if (member '("_EXTRA") list) " (note _EXTRA)" ""))
	 isa
	 'idlwave-attach-keyword-classes)))

     ((eq what 'function-keyword)
      ;; Complete a function keyword
      (let* ((where (nth 3 where-list))
	     (name  (car where))
	     (method-selector name)
	     (type-selector 'fun)
	     (class (idlwave-determine-class where 'fun))
	     (class-selector class)
	     (isa (format "function%s-keyword" (if class "-method" "")))
	     (entry (idlwave-rinfo-assq
		     name 'fun class (idlwave-routines)))
	     (list (nth 5 entry)))
	(unless (or entry (eq class t))
	  (error "Nothing known about function %s"
		 (idlwave-make-full-name class name)))
	(setq list (idlwave-fix-keywords name 'fun class list))
	(unless list (error (format "No keywords available for function %s"
				    (idlwave-make-full-name class name))))
	(idlwave-complete-in-buffer
	 'keyword 'keyword list nil
	 (format "Select keyword for function %s%s"
		 (idlwave-make-full-name class name)
		 (if (member '("_EXTRA") list) " (note _EXTRA)" ""))
	 isa
	 'idlwave-attach-keyword-classes)))
		 
     (t (error "This should not happen (idlwave-complete)")))))

(defun idlwave-make-force-complete-where-list (what &optional module class)
  ;; Return an artificial WHERE specification to force the completion
  ;; routine to complete a specific item independent of context.
  ;; WHAT is the prefix arg of `idlwave-complete', see there for details.
  ;; MODULE and CLASS can be used to specify the routine name and class.
  ;; The class name will also be found in MODULE if that is like "class::mod".
  (let* ((what-list '(("procedure") ("procedure-keyword")
		      ("function") ("function-keyword")
		      ("procedure-method") ("procedure-method-keyword")
		      ("function-method") ("function-method-keyword")
		      ("class")))
	 (module (idlwave-sintern-routine-or-method module class))
	 (class (idlwave-sintern-class class))
	 (what (cond 
		((equal what 0)
		 (setq what
		       (intern (completing-read 
				"Complete what? " what-list nil t))))
		((integerp what)
		 (setq what (intern (car (nth (1- what) what-list)))))
		((and what
		      (symbolp what)
		      (assoc (symbol-name what) what-list))
		 what)
		(t (error "Illegal WHAT"))))
	 (nil-list '(nil nil nil nil))
	 (class-list (list nil nil (or class t) nil)))

    (cond

     ((eq what 'procedure)
      (list nil-list nil-list 'procedure nil-list nil))

     ((eq what 'procedure-keyword)
      (let* ((class-selector nil)
	     (type-selector 'pro)
	     (pro (or module
		      (idlwave-completing-read 
		       "Procedure: " (idlwave-routines) 'idlwave-selector))))
	(setq pro (idlwave-sintern-routine pro))
	(list nil-list nil-list 'procedure-keyword
	      (list pro nil nil nil) nil)))

     ((eq what 'function)
      (list nil-list nil-list 'function nil-list nil))

     ((eq what 'function-keyword)
      (let* ((class-selector nil)
	     (type-selector 'fun)
	     (func (or module
		       (idlwave-completing-read 
			"Function: " (idlwave-routines) 'idlwave-selector))))
	(setq func (idlwave-sintern-routine func))
	(list nil-list nil-list 'function-keyword
	      (list func nil nil nil) nil)))

     ((eq what 'procedure-method)
      (list nil-list nil-list 'procedure class-list nil))

     ((eq what 'procedure-method-keyword)
      (let* ((class (idlwave-determine-class class-list 'pro))
	     (class-selector class)
	     (type-selector 'pro)
	     (pro (or module
		      (idlwave-completing-read
		       (format "Procedure in %s class: " class-selector)
		       (idlwave-routines) 'idlwave-selector))))
	(setq pro (idlwave-sintern-method pro))
	(list nil-list nil-list 'procedure-keyword
	      (list pro nil class nil) nil)))

     ((eq what 'function-method)
      (list nil-list nil-list 'function class-list nil))

     ((eq what 'function-method-keyword)
      (let* ((class (idlwave-determine-class class-list 'fun))
	     (class-selector class)
	     (type-selector 'fun)
	     (func (or module
		       (idlwave-completing-read
			(format "Function in %s class: " class-selector)
			(idlwave-routines) 'idlwave-selector))))
	(setq func (idlwave-sintern-method func))
	(list nil-list nil-list 'function-keyword
	      (list func nil class nil) nil)))

     ((eq what 'class)
      (list nil-list nil-list 'class nil-list nil))
     
     (t (error "Illegal value for WHAT")))))

(defun idlwave-completing-read (&rest args)
  ;; Completing read, case insensitive
  (let ((old-value (default-value 'completion-ignore-case)))
    (unwind-protect
	(progn
	  (setq-default completion-ignore-case t)
	  (apply 'completing-read args))
      (setq-default completion-ignore-case old-value))))

(defun idlwave-make-full-name (class name)
  ;; Make a fully qualified module name including the class name
  (concat (if class (format "%s::" class) "") name))

(defun idlwave-rinfo-assq (name type class list)
  ;; Works like assq, but also checks type and class
  (catch 'exit
    (let (match)
      (while (setq match (assq name list))
	(and (or (eq type t)
		 (eq (nth 1 match) type))
	     (eq (nth 2 match) class)
	     (throw 'exit match))
	(setq list (cdr (memq match list)))))))

(defun idlwave-all-assq (key list)
  "Return a list of all associations of Key in LIST."
  (let (rtn elt)
    (while (setq elt (assq key list))
      (push elt rtn)
      (setq list (cdr (memq elt list))))
    (nreverse rtn)))

(defun idlwave-all-method-classes (method &optional type)
  "Return all classes which have a method METHOD.  TYPE is 'fun or 'pro.
When TYPE is not specified, both procedures and functions will be considered."
  (if (null method)
      (mapcar 'car idlwave-class-alist)
    (let (rtn)
      (mapcar (lambda (x)
		(and (nth 2 x)
		     (or (not type)
			 (eq type (nth 1 x)))
		     (push (nth 2 x) rtn)))
	      (idlwave-all-assq method (idlwave-routines)))
      (idlwave-uniquify rtn))))

(defun idlwave-all-method-keyword-classes (method keyword &optional type)
  "Return all classes which have a method METHOD with keyword KEYWORD.
TYPE is 'fun or 'pro.
When TYPE is not specified, both procedures and functions will be considered."
  (if (or (null method)
	  (null keyword))
      nil
    (let (rtn)
      (mapcar (lambda (x)
		(and (nth 2 x)
		     (or (not type)
			 (eq type (nth 1 x)))
		     (assoc keyword (nth 5 x))
		     (push (nth 2 x) rtn)))
	      (idlwave-all-assq method (idlwave-routines)))
      (idlwave-uniquify rtn))))

(defun idlwave-determine-class (info type)
  ;; Determine the class of a routine call.  INFO is the structure returned
  ;; `idlwave-what-function' or `idlwave-what-procedure'.
  ;; The third element in this structure is the class.  When nil, we return nil.
  ;; When t, try to get the class from text properties at the arrow,
  ;; otherwise prompt the user for a class name.  Also stores the selected
  ;; class as a text property at the arrow.
  ;; TYPE is 'fun or 'pro.
  (let* ((class (nth 2 info))
	 (apos (nth 3 info))
	 (nassoc (assoc (if (stringp (car info))
			    (upcase (car info))
			  (car info))
			idlwave-query-class))
	 (dassoc (assq (if (car info) 'keyword-default 'method-default)
		       idlwave-query-class))
	 (query (cond (nassoc (cdr nassoc))
		      (dassoc (cdr dassoc))
		      (t t)))
	 (arrow (and apos (string= (buffer-substring apos (+ 2 apos)) "->")))
	 (force-query idlwave-force-class-query)
	 store class-alist)
    (cond
     ((null class) nil)
     ((eq t class)
      ;; There is an object which would like to know its class
      (if (and arrow (get-text-property apos 'idlwave-class)
	       idlwave-store-inquired-class
	       (not force-query))
	  (setq class (get-text-property apos 'idlwave-class)
		class (idlwave-sintern-class class)))
      (when (and (eq class t)
		 (or force-query query))
	(setq class-alist 
	      (mapcar 'list (idlwave-all-method-classes (car info) type)))
	(setq class
	      (idlwave-sintern-class
	       (cond
		((and (= (length class-alist) 0) (not force-query))
		 (error "No classes available with method %s" (car info)))
		((and (= (length class-alist) 1) (not force-query))
		 (car (car class-alist)))
		(t 
		 (setq store idlwave-store-inquired-class)
		 (idlwave-completing-read 
		  (format "Class%s: " (if (stringp (car info))
					  (format " for %s method %s"
						  type (car info))
					""))
		  class-alist nil nil nil 'idlwave-class-history))))))
      (when (and class (not (eq t class)))
	;; We have a real class here
	(when (and store arrow)
	  (put-text-property apos (+ apos 2) 'idlwave-class class)
	  (put-text-property apos (+ apos 2) 'face idlwave-class-arrow-face))
	(setf (nth 2 info) class))
      ;; Return the class
      class)
     ;; Default as fallback
     (t class))))

(defvar type-selector)
(defvar class-selector)
(defvar method-selector)
(defun idlwave-selector (a)
  (and (eq (nth 1 a) type-selector)
       (or (and (nth 2 a) (eq class-selector t))
	   (eq (nth 2 a) class-selector))))

(defun idlwave-where ()
  "Find out where we are.
The return value is a list with the following stuff:
(PRO-LIST FUNC-LIST COMPLETE-WHAT CW-LIST LAST-CHAR)

PRO-LIST       (PRO POINT CLASS ARROW)
FUNC-LIST      (FUNC POINT CLASS ARROW)
COMPLETE-WHAT  a symbol indicating what kind of completion makes sense here
CW-LIST        Like PRO-LIST, for what can be copmpleted here.
LAST-CHAR      last relevant character before point (non-white non-comment,
               not part of current identifier or leading slash).

In the lists, we have these meanings:
PRO:    Procedure name
FUNC:   Function name
POINT:  Where is this
CLASS:  What class has the routine (nil=no, t=is method, but class unknown)
ARROW:  Where is the arrow?"
  (idlwave-routines)
  (let* ((bos (save-excursion (idlwave-beginning-of-statement) (point)))
 	 (func-entry (idlwave-what-function bos))
         (func (car func-entry))
         (func-class (nth 1 func-entry))
         (func-arrow (nth 2 func-entry))
	 (func-point (or (nth 3 func-entry) 0))
	 (func-level (or (nth 4 func-entry) 0))
	 (pro-entry (idlwave-what-procedure bos))
	 (pro (car pro-entry))
         (pro-class (nth 1 pro-entry))
         (pro-arrow (nth 2 pro-entry))
	 (pro-point (or (nth 3 pro-entry) 0))
	 (last-char (idlwave-last-valid-char))
         (case-fold-search t)
	 cw cw-mod cw-arrow cw-class cw-point)
    (if (< func-point pro-point) (setq func nil))
    (cond
     ((string-match 
       "\\`[ \t]*\\([a-zA-Z][a-zA-Z0-9$_]*\\)?\\'" 
       (buffer-substring (if (> pro-point 0) pro-point bos) (point)))
      (setq cw 'procedure cw-class pro-class cw-point pro-point
	    cw-arrow pro-arrow))
     ((string-match "\\`[ \t]*\\(pro\\|function\\)\\>"
		    (buffer-substring bos (point)))
      nil)
     ((string-match "OBJ_NEW([ \t]*'\\([a-zA-Z][a-zA-Z0-9$_]*\\)?\\'"
		    (buffer-substring bos (point)))
      (setq cw 'class))                    
     ((and func 
	   (> func-point pro-point)
	   (= func-level 1)
	   (memq last-char '(?\( ?,)))
      (setq cw 'function-keyword cw-mod func cw-point func-point
	    cw-class func-class cw-arrow func-arrow))
     ((and pro (eq last-char ?,))
      (setq cw 'procedure-keyword cw-mod pro cw-point pro-point
	    cw-class pro-class cw-arrow pro-arrow))
;     ((member last-char '(?\' ?\) ?\] ?!))
;      ;; after these chars, a function makes no sense
;      ;; FIXME: I am sure there can be more in this list
;      ;; FIXME: Do we want to do this at all?
;      nil)
     ;; Everywhere else we try a function.
     (t
      (setq cw 'function)
      (save-excursion
	(if (re-search-backward "->[ \t]*\\(\\([$a-zA-Z0-9_]+\\)::\\)?[$a-zA-Z0-9_]*\\=" bos t)
	    (setq cw-arrow (match-beginning 0)
		  cw-class (if (match-end 2)
                               (idlwave-sintern-class (match-string 2))
                              t))))))
    (list (list pro pro-point pro-class pro-arrow)
          (list func func-point func-class func-arrow)
          cw
	  (list cw-mod cw-point cw-class cw-arrow)
	  last-char)))

(defun idlwave-this-word (&optional class)
  ;; Grab the word around point.  CLASS is for the `skip-chars=...' functions
  (setq class (or class "a-zA-Z0-9$_"))
  (save-excursion
    (buffer-substring-no-properties
     (progn (skip-chars-backward class) (point))
     (progn (skip-chars-forward  class) (point)))))

(defvar idlwave-find-symbol-syntax-table)
(defun idlwave-what-function (&optional bound)
  ;; Find out if point is within the argument list of a function.
  ;; The return value is ("function-name" (point) level).
  ;; Level is 1 on the to level parenthesis, higher further down.

  ;; If the optional BOUND is an integer, bound backwards directed
  ;;    searches to this point.

  (catch 'exit
    (let (pos 
	  func-point
	  (old-syntax (syntax-table))
	  (cnt 0)
	  func arrow-start class)
      (unwind-protect
	  (save-restriction
	    (save-excursion
	      (set-syntax-table idlwave-find-symbol-syntax-table)
	      (narrow-to-region (max 1 (or bound 0)) (point-max))
	      ;; move back out of the current parenthesis
	      (while (condition-case nil
			 (progn (up-list -1) t)
		       (error nil))
		(setq pos (point))
		(incf cnt)
		(when (and (= (following-char) ?\()
			   (re-search-backward 
			    "\\(::\\|\\<\\)\\([a-zA-Z][a-zA-Z0-9$_]*\\)\\="
					       bound t))
		  (setq func (match-string 2)
			func-point (goto-char (match-beginning 2))
			pos func-point)
		  (if (re-search-backward 
		       "->[ \t]*\\(\\([a-zA-Z][a-zA-Z0-9$_]*\\)::\\)?\\=" bound t)
		      (setq arrow-start (match-beginning 0)
			    class (or (match-string 2) t)))
		  (throw 
		   'exit 
		   (list
		    (idlwave-sintern-routine-or-method func class)
		    (idlwave-sintern-class class)
		    arrow-start func-point cnt)))
		(goto-char pos))
	      (throw 'exit nil)))
	(set-syntax-table old-syntax)))))

(defun idlwave-what-procedure (&optional bound)
  ;; Find out if point is within the argument list of a procedure.
  ;; The return value is ("procedure-name" class arrow-pos (point)).

  ;; If the optional BOUND is an integer, bound backwards directed
  ;;    searches to this point.
  (let ((pos (point)) pro-point
	pro class arrow-start string)
    (save-excursion 
      (idlwave-beginning-of-statement)
      (setq string (buffer-substring (point) pos))
      (if (string-match 
	   "\\`[ \t]*\\([a-zA-Z][a-zA-Z0-9$_]*\\)[ \t]*\\(,\\|\\'\\)" string)
	  (setq pro (match-string 1 string)
		pro-point (+ (point) (match-beginning 1)))
	(if (and (idlwave-skip-object)
		 (setq string (buffer-substring (point) pos))
		 (string-match 
		  "\\`[ \t]*\\(->\\)[ \t]*\\(\\([a-zA-Z][a-zA-Z0-9$_]*\\)::\\)?\\([a-zA-Z][a-zA-Z0-9$_]*\\)?[ \t]*\\(,\\|\\'\\)" string))
	    (setq pro (if (match-beginning 4)
			  (match-string 4 string))
		  pro-point (if (match-beginning 4)
			        (+ (point) (match-beginning 4))
			pos)
		  arrow-start (+ (point) (match-beginning 1))
		  class (or (match-string 3 string) t)))))
    (list (idlwave-sintern-routine-or-method pro class)
	  (idlwave-sintern-class class)
	  arrow-start
	  pro-point)))

(defun idlwave-skip-object ()
  ;; If there is an object at point, move over it and return t.
  (let ((pos (point)))
    (if (catch 'exit
	  (save-excursion
	    (skip-chars-forward " 	")  ; white space
	    (skip-chars-forward "*")        ; de-reference
	    (cond
	     ((looking-at idlwave-identifier)
	      (goto-char (match-end 0)))
	     ((eq (following-char) ?\()
	      nil)
	     (t (throw 'exit nil)))
	    (catch 'endwhile
	      (while t
		(cond ((eq (following-char) ?.)
		       (forward-char 1)
		       (if (not (looking-at idlwave-identifier))
			   (throw 'exit nil))
		       (goto-char (match-end 0)))
		      ((memq (following-char) '(?\( ?\[))
		       (condition-case nil
			   (forward-list 1)
			 (error (throw 'exit nil))))
		      (t (throw 'endwhile t)))))
	    (if (looking-at "[ \t]*->")
		(throw 'exit (setq pos (match-beginning 0)))
	      (throw 'exit nil))))
	(goto-char pos)
      nil)))
  

(defun idlwave-last-valid-char ()
  "Return the last character before point which is not white or a comment
and also not part of the current identifier.  Since we do this in
order to identify places where keywords are, we consider the initial
`/' of a keyword as part of the identifier.
This function is not general, can only be used for completion stuff."
  (catch 'exit
    (save-excursion
      ;; skip the current identifier
      (skip-chars-backward "a-zA-Z0-9_$")
      ;; also skip a leading slash which might be belong to the keyword
      (if (eq (preceding-char) ?/)
	  (backward-char 1))
      ;; FIXME: does not check if this is a valid identifier
      (while t
	(skip-chars-backward " \t")
	(cond
	 ((memq (preceding-char) '(?\; ?\$)) (throw 'exit nil))
	 ((eq (preceding-char) ?\n)
	  (beginning-of-line 0)
	  (if (looking-at "\\([^;]\\)*\\$[ \t]*\\(;.*\\)?\n")
	      ;; continuation line
	      (goto-char (match-end 1))
	    (throw 'exit nil)))
	 (t (throw 'exit (preceding-char))))))))

(defvar idlwave-complete-after-success-form nil
  "A form to evaluate after successful completion.")
(defvar idlwave-complete-after-success-form-force nil
  "A form to evaluate after completion selection in *Completions* buffer.")
(defconst idlwave-completion-mark (make-marker)
  "A mark pointing to the beginning of the completion string.")

(defun idlwave-complete-in-buffer (type stype list selector prompt isa
					&optional prepare-display-function)
  "Perform TYPE completion of word before point against LIST.
SELECTOR is the PREDICATE argument for the completion function.
Show PROMPT in echo area.  TYPE is one of 'function, 'procedure or 'keyword."
  (let* ((completion-ignore-case t)
	 beg (end (point)) slash part spart completion all-completions
	 dpart dcompletion)

    (unless list
      (error (concat prompt ": No completions available")))

    ;; What is already in the buffer?
    (save-excursion
      (skip-chars-backward "a-zA-Z0-9_$")
      (setq slash (eq (preceding-char) ?/)
	    beg (point)
	    idlwave-complete-after-success-form
	    (list 'idlwave-after-successful-completion
		  (list 'quote type) slash beg)
	    idlwave-complete-after-success-form-force
	    (list 'idlwave-after-successful-completion
		  (list 'quote type) slash (list 'quote 'force))))

    ;; Try a completion
    (setq part (buffer-substring beg end)
	  dpart (downcase part)
	  spart (idlwave-sintern stype part)
	  completion (try-completion part list selector)
	  dcompletion (if (stringp completion) (downcase completion)))
    (cond
     ((null completion)
      ;; nothing available.
      (error "Can't find %s completion for \"%s\"" isa part))
     ((and (not (equal dpart dcompletion))
	   (not (eq t completion)))
      ;; We can add something
      (delete-region beg end)
      (if (and (string= part dpart)
	       (or (not (string= part ""))
		   idlwave-complete-empty-string-as-lower-case)
	       (not idlwave-completion-force-default-case))
	  (insert dcompletion)
	(insert completion))
      (if (eq t (try-completion completion list selector))
	  ;; Now this is a unique match
	  (idlwave-after-successful-completion type slash beg))
      t)
     ((or (eq completion t)
	  (and (equal dpart dcompletion)
	       (= 1 (length (setq all-completions
				  (idlwave-uniquify
				   (all-completions part list selector)))))))
      ;; This is already complete
      (idlwave-after-successful-completion type slash beg)
      (message "%s is already the complete %s" part isa)
      nil)
     (t
      ;; We cannot add something - offer a list.
      (message "Making completion list...")
      (let* ((list all-completions)
	     (complete (memq spart all-completions))
	     (completion-highlight-first-word-only t) ; XEmacs
	     (completion-fixup-function               ; Emacs
	      (lambda () (and (eq (preceding-char) ?>)
			      (re-search-backward " <" beg t)))))
	(setq list (sort list (lambda (a b)
				(string< (downcase a) (downcase b)))))
	(if prepare-display-function
	    (setq list (funcall prepare-display-function list)))
	(if (and (string= part dpart)
		 (or (not (string= part ""))
		     idlwave-complete-empty-string-as-lower-case)
		 (not idlwave-completion-force-default-case))
	    (setq list (mapcar (lambda (x)
				 (if (listp x) 
				     (setcar x (downcase (car x)))
				   (setq x (downcase x)))
				 x)
			       list)))
	(idlwave-display-completion-list list prompt beg complete))
      t))))

(defun idlwave-complete-class ()
  "Complete a class at point."
  (interactive)
  ;; Call `idlwave-routines' to make sure the class list will be available
  (idlwave-routines)
  ;; Now do the completion
  (idlwave-complete-in-buffer 'class 'class idlwave-class-alist nil 
			      "Select a class" "class"))


(defun idlwave-attach-classes (list is-kwd show-classes)
  ;; attach the proper class list to a LIST of completion items.
  ;; IS-KWD, when non-nil, shows its keywords - otherwise its methods
  ;; SHOW-CLASSES is the value of `idlwave-completion-show-classes'.
  (catch 'exit
    (if (or (null show-classes)         ; don't wnat to see classes
	    (null class-selector)       ; not a method call
	    (stringp class-selector))   ; the class is already known
	;; In these cases, we do not have to do anything
	(throw 'exit list))
    
    ;; The property and dots stuff currently only make sense with XEmacs
    ;; because Emacs drops text properties when filling the *Completions*
    ;; buffer.
    (let* ((do-prop (and (featurep 'xemacs) (>= show-classes 0)))
	   (do-buf (not (= show-classes 0)))
	   (do-dots (featurep 'xemacs))
	   (max (abs show-classes))
	   (lmax (if do-dots (apply 'max (mapcar 'length list))))
	  classes nclasses class-info space)
      (mapcar 
       (lambda (x)
	 ;; get the classes
	 (setq classes
	       (if is-kwd 
		   (idlwave-all-method-keyword-classes
		    method-selector x type-selector)
		 (idlwave-all-method-classes x type-selector)))
	 (setq nclasses (length classes))
	 ;; Make the separator between item and class-info
	 (if do-dots
	     (setq space (concat " " (make-string (- lmax (length x)) ?.)))
	   (setq space " "))
	 (if  do-buf
	     ;; We do want info in the buffer
	     (if (<= nclasses max)
		 (setq class-info (concat
				   space
				   "<" (mapconcat 'identity classes ",") ">"))
	       (setq class-info (format "%s<%d classes>" space nclasses)))
	   (setq class-info nil))
	 (when do-prop
	   ;; We do want properties
	   (setq x (copy-sequence x))
	   (put-text-property 0 (length x)
			      'help-echo (mapconcat 'identity classes " ")
			      x))
	 (if class-info
	     (list x class-info)
	   x))
       list))))

(defun idlwave-attach-method-classes (list)
  ;; Call idlwave-attach-classes with method parameters
  (idlwave-attach-classes list nil idlwave-completion-show-classes))
(defun idlwave-attach-keyword-classes (list)
  ;; Call idlwave-attach-classes with keyword parameters
  (idlwave-attach-classes list t idlwave-completion-show-classes))

;;----------------------------------------------------------------------
;;----------------------------------------------------------------------
;;----------------------------------------------------------------------
;;----------------------------------------------------------------------
;;----------------------------------------------------------------------

(defun idlwave-scroll-completions (&optional message)
  "Scroll the completion window on this frame."
  (let ((cwin (get-buffer-window "*Completions*" 'visible))
	(win (selected-window)))
    (unwind-protect
	(progn
	  (select-window cwin)
	  (condition-case nil
	      (scroll-up)
	    (error (if (and (listp last-command)
			    (nth 2 last-command))
		       (progn
			 (select-window win)
			 (eval idlwave-complete-after-success-form))
		     (set-window-start cwin (point-min)))))
	  (and message (message message)))
      (select-window win))))

(defun idlwave-display-completion-list (list &optional message beg complete)
  "Display the completions in LIST in the completions buffer and echo MESSAGE."
  (unless (and (get-buffer-window "*Completions*")
	       (idlwave-local-value 'idlwave-completion-p "*Completions*"))
    (move-marker idlwave-completion-mark beg)
    (setq idlwave-before-completion-wconf (current-window-configuration)))

  (if (featurep 'xemacs)
      (idlwave-display-completion-list-xemacs list)
    (idlwave-display-completion-list-emacs list))

  ;; Store a special value in `this-command'.  When `idlwave-complete'
  ;; finds this in `last-command', it will scroll the *Completions* buffer.
  (setq this-command (list 'idlwave-display-completion-list message complete))

  ;; Mark the completions buffer as created by cib
  (idlwave-set-local 'idlwave-completion-p t "*Completions*")

  ;; Fontify the classes
  (if (and idlwave-completion-fontify-classes
           (consp (car list)))
      (idlwave-completion-fontify-classes))

  ;; Display the message
  (message (or message "Making completion list...done")))

(defun idlwave-choose (function &rest args)
  "Call FUNCTION as a completion chooser and pass ARGS to it."
  (let ((completion-ignore-case t))	    ; install correct value
    (apply function args))
  (eval idlwave-complete-after-success-form-force))

(defun idlwave-restore-wconf-after-completion ()
  "Restore the old (before completion) window configuration."
  (and idlwave-completion-restore-window-configuration
       idlwave-before-completion-wconf
       (set-window-configuration idlwave-before-completion-wconf)))

(defun idlwave-set-local (var value &optional buffer)
  "Set the buffer-local value of VAR in BUFFER to VALUE."
  (save-excursion
    (set-buffer (or buffer (current-buffer)))
    (set (make-local-variable var) value)))

(defun idlwave-local-value (var &optional buffer)
  "Return the value of VAR in BUFFER, but only if VAR is local to BUFFER."
  (save-excursion
    (set-buffer (or buffer (current-buffer)))
    (and (local-variable-p var (current-buffer))
	 (symbol-value var))))

;; In XEmacs, we can use :activate-callback directly

(defun idlwave-display-completion-list-xemacs (list)
  (with-output-to-temp-buffer "*Completions*"
    (display-completion-list list :activate-callback 
			     'idlwave-default-choose-completion)))

(defun idlwave-default-choose-completion (&rest args)
  "Execute `default-choose-completion' and then restore the win-conf."
  (apply 'idlwave-choose 'default-choose-completion args))

;; In Emacs we have to replace the keymap in the *Completions* buffer
;; in order to install our wrappers.

(defvar idlwave-completion-map nil
  "Keymap for completion-list-mode with idlwave-complete.")

(defun idlwave-display-completion-list-emacs (list)
  "Display completion list and install the choose wrappers."
  (with-output-to-temp-buffer "*Completions*"
    (display-completion-list list))
  (save-excursion
    (set-buffer "*Completions*")
    (use-local-map
     (or idlwave-completion-map
	 (setq idlwave-completion-map
	       (idlwave-make-modified-completion-map (current-local-map)))))))
  
(defun idlwave-make-modified-completion-map (old-map)
  "Replace `choose-completion' and `mouse-choose-completion' in OLD-MAP."
  (let ((new-map (copy-keymap old-map)))
    (substitute-key-definition 
     'choose-completion 'idlwave-choose-completion new-map)
    (substitute-key-definition
     'mouse-choose-completion 'idlwave-mouse-choose-completion new-map)
    new-map))

(defun idlwave-choose-completion (&rest args)
  "Choose the completion that point is in or next to."
  (interactive)
  (apply 'idlwave-choose 'choose-completion args))

(defun idlwave-mouse-choose-completion (&rest args)
  "Click on an alternative in the `*Completions*' buffer to choose it."
  (interactive "e")
  (apply 'idlwave-choose 'mouse-choose-completion args))

;;----------------------------------------------------------------------
;;----------------------------------------------------------------------

(defun idlwave-completion-fontify-classes ()
  "Goto the *Completions* buffer and fontify the class info."
  (when (featurep 'font-lock)
    (save-excursion
      (set-buffer "*Completions*")
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "\\.*<[^>]+>" nil t)
	  (put-text-property (match-beginning 0) (match-end 0)
			     'face 'font-lock-string-face))))))

(defun idlwave-uniquify (list)
  (let (nlist)
    (loop for x in list do
      (add-to-list 'nlist x))
    nlist))

(defun idlwave-after-successful-completion (type slash &optional verify)
  "Add `=' or `(' after successful completion of keyword and function.
Restore the pre-completion window configuration if possible."
  (cond
   ((eq type 'procedure)
    nil)
   ((eq type 'function)
    (cond
     ((equal idlwave-function-completion-adds-paren nil) nil)
     ((or (equal idlwave-function-completion-adds-paren t)
	  (equal idlwave-function-completion-adds-paren 1))
      (insert "("))
     ((equal idlwave-function-completion-adds-paren 2)
      (insert "()")
      (backward-char 1))
     (t nil)))
   ((eq type 'keyword)
    (if (and idlwave-keyword-completion-adds-equal
	     (not slash))
	(progn (insert "=") t)
      nil)))

  ;; Restore the pre-completion window configuration if this is safe.
  
  (if (or (eq verify 'force)                                    ; force 
	  (and 
	   (get-buffer-window "*Completions*")                  ; visible
	   (idlwave-local-value 'idlwave-completion-p 
				"*Completions*")                ; cib-buffer
	   (eq (marker-buffer idlwave-completion-mark)
	       (current-buffer))                                ; buffer OK
	   (equal (marker-position idlwave-completion-mark)
		  verify)))                                     ; pos OK
      (idlwave-restore-wconf-after-completion))
  (move-marker idlwave-completion-mark nil)
  (setq idlwave-before-completion-wconf nil))

(defun idlwave-routine-info-from-idlhelp (&optional arg)
  "Make IDLHELP display the online documentation about the routine at point.
Sends the command `? MODULE' to the IDLWAVE-Shell.  Shell must be running,
it does not autostart for this task."
  (interactive "P")
  (idlwave-routine-info arg 'external))

(defun idlwave-routine-info (&optional arg external)
  "Display a routines calling sequence and list of keywords.
When point is on the name a function or procedure, or in the argument
list of a function or procedure, this command displays a help buffer
with the information.  When called with prefix arg, enforce class
query.

When point is on an object operator `->', display the class stored in
this arrow, if any (see `idlwave-store-inquired-class').  With a
prefix arg, the class property is cleared out."

  (interactive "P")
  (idlwave-routines)
  (if (string-match "->" (buffer-substring
			  (max (point-min) (1- (point)))
			  (min (+ 2 (point)) (point-max))))
      ;; Cursor is on an arrow
      (if (get-text-property (point) 'idlwave-class)
	  ;; arrow has class property
	  (if arg
	      ;; Remove property
	      (save-excursion
		(backward-char 1)
		(when (looking-at ".?\\(->\\)")
		  (remove-text-properties (match-beginning 1) (match-end 1)
					  '(idlwave-class nil face nil))
		  (message "Class property removed from arrow")))
	    ;; Echo class property
	    (message "Arrow has text property identifying object to be class %s"
		     (get-text-property (point) 'idlwave-class)))
	;; No property found
	(message "Arrow has no class text property"))

    ;; Not on an arrow...
    (let* ((idlwave-query-class nil)
	   (idlwave-force-class-query (equal arg '(4)))
	   (module (idlwave-what-module)))
      (cond ((car module)
	     (if external
		 (apply 'idlwave-search-online-help module)
	       (apply 'idlwave-display-calling-sequence module)))
	    (t
	     (error "Don't know which calling sequence to show."))))))

(defun idlwave-search-online-help (name &optional type class olh)
  "Tell IDL to lookup CLASS::NAME with type TYPE in the online help.
If TYPE and CLASS are both nil, just look up NAME in the default help file."
  ;; If only the IDLHELP application was better designed, so that
  ;; we could make it open the right thing right away.  As things are,
  ;; we need to pipe the stuff through the help search engine, and we
  ;; cannot enter a space.
  (let* (extra book full string cmd)

    ;; Try to find a clue for the right help book
    (if (and type (not olh))
	(setq olh (or (nth 6 (idlwave-rinfo-assq
			      name type class idlwave-builtin-routines))
		      (nth 6 (idlwave-rinfo-assq 
			      name type class idlwave-routines)))))

    ;; Sometimes the book is given as a symbol - make it a string
    (if (and olh (symbolp olh)) (setq olh (symbol-name olh)))
    (setq book (or olh "idl"))   ; We need a default
    ;; Add the FULL_PATH keyword if appropriate
    (if (and (file-name-absolute-p book)
	     (file-exists-p book))
	(setq full ",/FULL_PATH")
      (setq full ""))

    ;; We would like to add "Method" or so, but stupid IDL online help
    ;; command treats a space as a separator and interprets the next thing as
    ;; the book name.
    ;; (setq extra (cond ((eq type 'kwd) " keyword")
    ;;		      (class          " method")
    ;;		      ((eq type 'pro) " procedure")
    ;;		      ((eq type 'fun) " function")
    ;;		      (t "")))
    (setq extra "")

    ;; Methods are subitems of classes, the separator is a single `:'
    (if (and name class (not (eq type 'kwd)))
	(setq name (concat class ":" name)))
    ;; FIXME:  We used to use book, but in idl5.3, all help is in idl.hlp
    (setq string (concat name extra)
	  cmd (format "ONLINE_HELP,'%s',BOOK='%s'%s" string "idl" full))
;	  cmd (format "ONLINE_HELP,'%s',BOOK='%s'%s" string book full))
    (message "Sending to IDL: %s" cmd) (sit-for 2)
    (idlwave-shell-send-command cmd)))

(defun idlwave-resolve (&optional arg)
  "Call RESOLVE on the module name at point.
Like `idlwave-routine-info', this looks for a routine call at point.
After confirmation in the minibuffer, it will use the shell to issue
a RESOLVE call for this routine, to attempt to make it defined and its
routine info available for IDLWAVE.  If the routine is a method call,
both `class__method' and `class__define' will be tried.
With ARG, enforce query for the class of object methods."
  (interactive "P")
  (let* ((idlwave-query-class nil)
	 (idlwave-force-class-query (equal arg '(4)))
	 (module (idlwave-what-module))
	 (name (idlwave-make-full-name (nth 2 module) (car module)))
	 (type (if (eq (nth 1 module) 'pro) "pro" "function"))
	 (resolve (read-string "Resolve: " (format "%s %s" type name)))
	 (kwd "")
	 class)
    (if (string-match "\\(pro\\|function\\)[ \t]+\\(\\(.*\\)::\\)?\\(.*\\)"
		      resolve)
	(setq type (match-string 1 resolve)
	      class (if (match-beginning 2) 
			(match-string 3 resolve)
		      nil)
	      name (match-string 4 resolve)))
    (if (string= (downcase type) "function")
	(setq kwd ",/is_function"))

    (cond
     ((null class)
      (idlwave-shell-send-command 
       (format "resolve_routine,'%s'%s" (downcase name) kwd)
       'idlwave-update-routine-info
       nil t))
     (t
      (idlwave-shell-send-command 
       (format "resolve_routine,'%s__define'%s" (downcase class) kwd)
       (list 'idlwave-shell-send-command 
	     (format "resolve_routine,'%s__%s'%s" 
		     (downcase class) (downcase name) kwd)
	     '(idlwave-update-routine-info)
	     nil t))))))

(defun idlwave-find-module (&optional arg)
  "Find the source code of an IDL module.
Works for modules for which IDLWAVE has routine info available.
The function offers as default the module name `idlwave-routine-info' would
use.  With ARG force class query for object methods."
  (interactive "P")
  (let* ((idlwave-query-class nil)
	 (idlwave-force-class-query (equal arg '(4)))
	 (module (idlwave-what-module))
	 (default (concat (idlwave-make-full-name (nth 2 module) (car module))
			  (if (eq (nth 1 module) 'pro) "<p>" "<f>")))
	 (list 
	  (delq nil
		(mapcar (lambda (x) 
			  (if (eq 'system (car-safe (nth 3 x)))
			      ;; Take out system routines with no source.
			      nil
			    (cons
			     (concat (idlwave-make-full-name (nth 2 x) (car x))
				     (if (eq (nth 1 x) 'pro) "<p>" "<f>"))
			     (cdr x))))
			(idlwave-routines))))
	 (name (idlwave-completing-read
		(format "Module (Default %s): " 
			(if default default "none"))
		list))
	 type class)
    (if (string-match "\\`\\s-*\\'" name)
	;; Nothing, use the default.
	(setq name default))
    (if (string-match "<[fp]>" name)
	(setq type (substring name -2 -1)
	      name (substring name 0 -3)))
    (if (string-match "\\(.*\\)::\\(.*\\)" name)
	(setq class (match-string 1 name)
	      name (match-string 2 name)))
    (setq name (idlwave-sintern-routine-or-method name class)
	  class (idlwave-sintern-class class)
	  type (cond ((equal type "f") 'fun)
		     ((equal type "p") 'pro)
		     (t t)))
    (idlwave-do-find-module name type class)))

(defun idlwave-do-find-module (name type class)
  (let ((name1 (idlwave-make-full-name class name))
	source buf1 entry
	(buf (current-buffer))
	(pos (point)))
    (setq entry (idlwave-rinfo-assq name type class (idlwave-routines))
	  source (nth 3 entry))
    (cond
     ((or (null name) (equal name ""))
      (error "Abort"))
     ((null entry)
      (error "Nothing known about a module %s" name1))
     ((eq (car source) 'system)
      (error "Source code for system routine %s is not available." 
	     name1))
     ((equal (cdr source) "")
      (error "Source code for routine %s is not available."
	     name1))
     ((memq (car source) '(buffer lib compiled))
      (setq buf1 
	    (if (eq (car source) 'lib)
		(idlwave-find-lib-file-noselet 
		 (or (cdr source)
		     (format "%s.pro" (downcase name))))
	      (idlwave-find-file-noselect (cdr source))))
      (pop-to-buffer buf1)
      (goto-char 1)
      (let ((case-fold-search t))
	(if (re-search-forward
	     (concat "^[ \t]*\\<"
		     (cond ((equal type "f") "function")
			   ((equal type "p") "pro")
			   (t "\\(pro\\|function\\)"))
		     "\\>[ \t]+" 
		     (regexp-quote (downcase name1))
		     "[^a-zA-Z0-9_$]")
	     nil t)
	    (goto-char (match-beginning 0))
	  (pop-to-buffer buf)
	  (goto-char pos)
	  (error "Could not find routine %s" name1)))))))

(defun idlwave-what-module ()
  "Return a default module for stuff near point.
Used by `idlwave-routine-info' and `idlwave-find-module'."
  (idlwave-routines)
  (let* ((where (idlwave-where))
	 (cw (nth 2 where))
	 (pro (car (nth 0 where)))
	 (func (car (nth 1 where)))
	 (this-word (idlwave-this-word "a-zA-Z0-9$_"))
	 (next-char (save-excursion (skip-chars-forward "a-zA-Z0-9$_")
				    (following-char)))
	 )
    (cond
     ((and (eq cw 'procedure)
	   (not (equal this-word "")))
      (setq this-word (idlwave-sintern-routine-or-method 
		       this-word (nth 2 (nth 3 where))))
      (list this-word 'pro
	    (idlwave-determine-class 
	     (cons this-word (cdr (nth 3 where)))
	     'pro)))
     ((and (eq cw 'function) 
	   (not (equal this-word ""))
	   (eq next-char ?\())              ; exclude arrays, vars.
      (setq this-word (idlwave-sintern-routine-or-method 
		       this-word (nth 2 (nth 3 where))))
      (list this-word 'fun
	    (idlwave-determine-class
	     (cons this-word (cdr (nth 3 where)))
	     'fun)))
     (func
      (list func 'fun (idlwave-determine-class (nth 1 where) 'fun)))
     (pro
      (list pro 'pro (idlwave-determine-class (nth 0 where) 'pro)))
     (t nil))))

(defun idlwave-fix-keywords (name type class keywords)
  ;; This fixes the list of keywords.
  (let ((case-fold-search t)
	name1 type1)

    ;; If this is the OBJ_NEW function, try to figure out the class and use
    ;; the keywords from the corresponding INIT method.
    (if (and (equal name "OBJ_NEW")
	     (eq major-mode 'idlwave-mode))
	(let* ((bos (save-excursion (idlwave-beginning-of-statement) (point)))
	       (string (buffer-substring bos (point)))
	       (case-fold-search t)
	       class)
	  (and (string-match "obj_new([^'\"]*['\"]\\([a-zA-Z0-9_]+\\)"
			     string)
	       (setq class (idlwave-sintern-class (match-string 1 string)))
	       (setq keywords 
		     (append keywords 
			     (nth 5 (idlwave-rinfo-assq
				     (idlwave-sintern-method "INIT")
				     'fun
				     class
				     (idlwave-routines))))))))

    ;; If the class is `t', combine all keywords of all methods NAME
    (when (eq class t)
      (loop for x in (idlwave-routines) do
	(and (nth 2 x)                         ; non-nil class
	     (or (and (eq (nth 1 x) type)      ; default type
		      (eq (car x) name))       ; default name
		 (and (eq (nth 1 x) type1)     ; backup type
		      (eq (car x) name1)))     ; backup name
	     (mapcar (lambda (k) (add-to-list 'keywords k))
		     (nth 5 x))))
      (setq keywords (idlwave-uniquify keywords)))
    ;; Return the final list
    keywords))

(defvar idlwave-rinfo-map (make-sparse-keymap))
(define-key idlwave-rinfo-map 
  (if (featurep 'xemacs) [button2] [mouse-2])
  'idlwave-mouse-active-rinfo)
(define-key idlwave-rinfo-map 
  (if (featurep 'xemacs) [button3] [mouse-3])
  'idlwave-mouse-active-rinfo-right)
(defvar idlwave-popup-source)

(defun idlwave-display-calling-sequence (name type class)
  ;; Display the calling sequence of module NAME, type TYPE in class CLASS.
  (let* ((entry (idlwave-rinfo-assq
		 name type class (idlwave-routines)))
	 (name (or (car entry) name))
	 (class (or (nth 2 entry) class))
	 (source (nth 3 entry))
	 ;;(system (eq (car source) 'system))
	 (calling-seq (nth 4 entry))
	 (keywords (nth 5 entry))
	 (olh (nth 6 entry))
	 (help-echo3  "                                     Button3: IDL Online Help")
	 (help-echo23 "Button2: Pop to source and back.     Button3: IDL Online Help")
	 (col 0)
	 (data (list name type class (current-buffer) olh))
	 (km-prop (if (featurep 'xemacs) 'keymap 'local-map))
	 beg props win)
    (setq keywords (idlwave-fix-keywords name type class keywords))
    (cond
     ((null entry)
      (error "No %s %s known" type name))
     ((or (null name) (equal name ""))
      (error "No function or procedure call at point."))
     ((null calling-seq)
      (error "Calling sequence of %s %s is not available" type name))
     (t
      (save-excursion
	(set-buffer (get-buffer-create "*Help*"))
	(setq buffer-read-only nil)
	(erase-buffer)
	(set (make-local-variable 'idlwave-popup-source) nil)
	(setq props (list 'mouse-face 'highlight
			  km-prop idlwave-rinfo-map
			  'help-echo help-echo23
			  'data (cons 'usage data)))
	(insert "Usage:    ")
	(setq beg (point))
	(insert (if class
		    (format calling-seq class name)
		  (format calling-seq name))
		"\n")
	(add-text-properties beg (point) props)

	(insert "Keywords:")
	(if (null keywords)
	    (insert " No keywords accepted.")
	  (setq col 9)
	  (mapcar
	   (lambda (x)
	     (if (>= (+ col 1 (length (car x))) 
		     (window-width))
		 (progn
		   (insert "\n         ")
		   (setq col 9)))
	     (insert " ")
	     (setq beg (point)
		   props (list 'mouse-face 'highlight
			       km-prop idlwave-rinfo-map
			       'data (cons 'keyword data)
			       'help-echo help-echo3
			       'keyword (car x)))
	     (insert (car x))
	     (add-text-properties beg (point) props)
	     (setq col (+ col 1 (length (car x)))))
	   keywords))
	(insert "\n")

	(insert "Origin:   ")
	(setq beg (point)
	      props (list 'mouse-face 'highlight
			  km-prop idlwave-rinfo-map
			  'help-echo help-echo23
			  'data (cons 'origin data)))
	(cond
	 ((eq (car source) 'system)
	  (insert "system routine"))
	 ((equal source '(lib))
	  (insert (format "library file %s.pro" (downcase name))))
	 ((eq (car source) 'lib)
	  (insert "library file ")
	  (insert (cdr source)))
	 ((eq (car source) 'buffer)
	  (insert "buffer visiting ")
	  (insert (abbreviate-file-name (cdr source))))
	 ((eq (car source) 'compiled)
	  (insert "compiled from ")
	  (insert (cdr source))))
	(add-text-properties beg (point) props)
	(setq buffer-read-only t))
      (display-buffer "*Help*")
      (if (and (setq win (get-buffer-window "*Help*"))
	       idlwave-resize-routine-help-window)
	  (progn
	    (let ((ww (selected-window)))
	      (unwind-protect
		  (progn
		    (select-window win)
		    (enlarge-window (- (/ (frame-height) 2) 
				       (window-height)))
		    (shrink-window-if-larger-than-buffer))
		(select-window ww)))))))))

(defun idlwave-mouse-active-rinfo-right (ev)
  (interactive "e")
  (idlwave-mouse-active-rinfo ev 'right))

(defun idlwave-mouse-active-rinfo (ev &optional right)
  (interactive "e")
  (mouse-set-point ev)
  (let (data id name type class buf keyword olh bufwin)
    (setq data (get-text-property (point) 'data)
	  keyword (get-text-property (point) 'keyword)
	  id (car data)
	  name (nth 1 data)
	  type (nth 2 data)
	  class (nth 3 data)
	  buf (nth 4 data)
	  olh (nth 5 data)
	  bufwin (get-buffer-window buf t))
    (cond ((or (eq id 'usage) (eq id 'origin))
	   (if right
	       (idlwave-search-online-help name type class)
	     (setq idlwave-popup-source (not idlwave-popup-source))
	     (if idlwave-popup-source
		 (condition-case err
		     (idlwave-do-find-module name type class)
		   (error
		    (setq idlwave-popup-source nil)
		    (if (window-live-p bufwin) (select-window bufwin))
		    (error (nth 1 err))))
	       (if bufwin
		   (select-window bufwin)
		 (pop-to-buffer buf)))))
	  ((eq id 'keyword)
	   (if right
	       (idlwave-search-online-help keyword 'kwd class olh)
	     (error "Button2 not active for keywords"))))))

;; ----------------------------------------------------------------------------
;;
;; Additions for use with imenu.el and func-menu.el
;; (pop-up a list of IDL units in the current file).
;;

(defun idlwave-prev-index-position ()
  "Search for the previous procedure or function.
Return nil if not found.  For use with imenu.el."
  (save-match-data
    (cond
     ((idlwave-find-key "\\<\\(pro\\|function\\)\\>" -1 'nomark))
     ;;   ((idlwave-find-key idlwave-begin-unit-reg 1 'nomark)
     (t nil))))

(defun idlwave-unit-name ()
  "Return the unit name.
Assumes that point is at the beginning of the unit as found by
`idlwave-prev-index-position'."
  (forward-sexp 2)
  (forward-sexp -1)
  (let ((begin (point)))
    (re-search-forward "[a-zA-Z][a-zA-Z0-9$_]+\\(::[a-zA-Z][a-zA-Z0-9$_]+\\)?")
    (if (fboundp 'buffer-substring-no-properties)
        (buffer-substring-no-properties begin (point))
      (buffer-substring begin (point)))))

(defun idlwave-function-menu ()
  "Use `imenu' or `function-menu' to jump to a procedure or function."
  (interactive)
  (if (string-match "XEmacs" emacs-version)
      (progn
	(require 'func-menu)
	(function-menu))
    (require 'imenu)
    (imenu (imenu-choose-buffer-index))))

;; Here we kack func-menu.el in order to support this new mode.
;; The latest versions of func-menu.el already have this stuff in, so
;; we hack only if it is not already there.
(when (fboundp 'eval-after-load)
  (eval-after-load "func-menu"
    '(progn
       (or (assq 'idlwave-mode fume-function-name-regexp-alist)
	   (not (boundp 'fume-function-name-regexp-idl))      ; avoid problems
	   (setq fume-function-name-regexp-alist
		 (cons '(idlwave-mode . fume-function-name-regexp-idl)
		       fume-function-name-regexp-alist)))
       (or (assq 'idlwave-mode fume-find-function-name-method-alist)
	   (not (fboundp 'fume-find-next-idl-function-name))  ; avoid problems
	   (setq fume-find-function-name-method-alist
		 (cons '(idlwave-mode . fume-find-next-idl-function-name)
		       fume-find-function-name-method-alist))))))

(defun idlwave-edit-in-idlde ()
  "Edit the current file in IDL Development environment."
  (interactive)
  (start-process "idldeclient" nil
		 idlwave-shell-explicit-file-name "-c" "-e"
                 (buffer-file-name) "&"))
                
(defun idlwave-launch-idlhelp ()
  "Start the IDLhelp application."
  (interactive)
  (start-process "idlhelp" nil idlwave-help-application))
 
;; Menus - using easymenu.el
(defvar idlwave-mode-menu-def
  `("IDLWAVE"
    ["PRO/FUNC menu" idlwave-function-menu t]
    ("Motion"
     ["Subprogram Start" idlwave-beginning-of-subprogram t]
     ["Subprogram End" idlwave-end-of-subprogram t]
     ["Block Start" idlwave-beginning-of-block t]
     ["Block End" idlwave-end-of-block t]
     ["Up Block" idlwave-backward-up-block t]
     ["Down Block" idlwave-down-block t]
     ["Skip Block Backward" idlwave-backward-block t]
     ["Skip Block Forward" idlwave-forward-block t])
    ("Mark"
     ["Subprogram" idlwave-mark-subprogram t]
     ["Block" idlwave-mark-block t]
     ["Header" idlwave-mark-doclib t])
    ("Format"
     ["Indent Subprogram" idlwave-indent-subprogram t]
     ["(Un)Comment Region" idlwave-toggle-comment-region "C-c ;"]
     ["Continue/Split line" idlwave-split-line t]
     "--"
     ["Toggle Auto Fill" idlwave-auto-fill-mode :style toggle
      :selected (symbol-value idlwave-fill-function)])
    ("Templates"
     ["Procedure" idlwave-procedure t]
     ["Function" idlwave-function t]
     ["Doc Header" idlwave-doc-header t]
     ["Log" idlwave-doc-modification t]
     "--"
     ["Case" idlwave-case t]
     ["For" idlwave-for t]
     ["Repeat" idlwave-repeat t]
     ["While" idlwave-while t]
     "--"
     ["Close Block" idlwave-close-block t])
    ("Completion / RInfo"
     ["Complete" idlwave-complete t]
     ("Complete Special"
      ["1 Procedure Name" (idlwave-complete 'procedure) t]
      ["2 Procedure Keyword" (idlwave-complete 'procedure-keyword) t]
      "--"
      ["3 Function Name" (idlwave-complete 'function) t]
      ["4 Function Keyword" (idlwave-complete 'function-keyword) t]
      "--"
      ["5 Procedure Method Name" (idlwave-complete 'procedure-method) t]
      ["6 Procedure Method Keyword" (idlwave-complete 'procedure-method-keyword) t]
      "--"
      ["7 Function Method Name" (idlwave-complete 'function-method) t]
      ["8 Function Method Keyword" (idlwave-complete 'function-method-keyword) t]
      "--"
      ["9 Class Name"  idlwave-complete-class t])
     "--"
     ["Show Routine Info" idlwave-routine-info t]
     ["Show Routine Doc with IDLHELP" idlwave-routine-info-from-idlhelp t]
     "--"
     ["Find Routine Source" idlwave-find-module t]
     "--"
     ["Update Routine Info" idlwave-update-routine-info t]
     "--"
     "IDL Library Routine Info"
     ["Select Library Directories" idlwave-create-libinfo-file t]
     ["Scan Directories" (idlwave-update-routine-info '(16)) idlwave-scanned-lib-directories])
     "--"
    ("External"
     ["Generate IDL tags" idlwave-make-tags t]
     ["Start IDL shell" idlwave-shell t]
     ["Edit file in IDLDE" idlwave-edit-in-idlde t]
     ["Launch IDL Help" idlwave-launch-idlhelp t])
    "--"
    ("Customize"
     ["Browse IDLWAVE Group" idlwave-customize t]
     "--"
     ["Build Full Customize Menu" idlwave-create-customize-menu 
      (fboundp 'customize-menu-create)])
    ("Documentation"
     ["Describe Mode" describe-mode t]
     ["Abbreviation List" idlwave-list-abbrevs t]
     "--"
     ["Commentary in idlwave.el" idlwave-show-commentary t]
     ["Commentary in idlw-shell.el" idlwave-shell-show-commentary t]
     "--"
     ["Info" idlwave-info t]
     "--"
     ["Launch IDL Help" idlwave-launch-idlhelp t])))

(defvar idlwave-mode-debug-menu-def
  '("Debug"
    ["Start IDL shell" idlwave-shell t]
    ["Save and .RUN buffer" idlwave-shell-save-and-run
     (and (boundp 'idlwave-shell-automatic-start) 
	  idlwave-shell-automatic-start)]))

(if (or (featurep 'easymenu) (load "easymenu" t))
    (progn
      (easy-menu-define idlwave-mode-menu idlwave-mode-map 
			"IDL and WAVE CL editing menu" 
			idlwave-mode-menu-def)
      (easy-menu-define idlwave-mode-debug-menu idlwave-mode-map 
			"IDL and WAVE CL editing menu" 
			idlwave-mode-debug-menu-def)))

(defun idlwave-customize ()
  "Call the customize function with idlwave as argument."
  (interactive)
  ;; Try to load the code for the shell, so that we can customize it 
  ;; as well.
  (or (featurep 'idlw-shell)
      (load "idlw-shell" t))
  (customize-browse 'idlwave))

(defun idlwave-create-customize-menu ()
  "Create a full customization menu for IDLWAVE, insert it into the menu."
  (interactive)
  (if (fboundp 'customize-menu-create)
      (progn
	;; Try to load the code for the shell, so that we can customize it 
	;; as well.
	(or (featurep 'idlw-shell)
	    (load "idlw-shell" t))
	(easy-menu-change 
	 '("IDLWAVE") "Customize"
	 `(["Browse IDLWAVE group" idlwave-customize t]
	   "--"
	   ,(customize-menu-create 'idlwave)
	   ["Set" Custom-set t]
	   ["Save" Custom-save t]
	   ["Reset to Current" Custom-reset-current t]
	   ["Reset to Saved" Custom-reset-saved t]
	   ["Reset to Standard Settings" Custom-reset-standard t]))
	(message "\"IDLWAVE\"-menu now contains full customization menu"))
    (error "Cannot expand menu (outdated version of cus-edit.el)")))

(defun idlwave-show-commentary ()
  "Use the finder to view the file documentation from `idlwave.el'."
  (interactive)
  (require 'finder)
  (finder-commentary "idlwave.el"))

(defun idlwave-shell-show-commentary ()
  "Use the finder to view the file documentation from `idlw-shell.el'."
  (interactive)
  (require 'finder)
  (finder-commentary "idlw-shell.el"))

(defun idlwave-info ()
  "Read documentation for IDLWAVE in the info system."
  (interactive)
  (require 'info)
  (Info-goto-node "(idlwave)"))

(defun idlwave-list-abbrevs (arg)
  "Show the code abbreviations define in IDLWAVE mode.
This lists all abbrevs where the replacement text differs from the input text.
These are the ones the users want to learn to speed up their writing.

The function does *not* list abbrevs which replace a word with itself
to call a hook.  These hooks are used to change the case of words or
to blink the matching `begin', and the user does not need to know them.

With arg, list all abbrevs with the corresponding hook.

This function was written since `list-abbrevs' looks terrible for IDLWAVE mode."

  (interactive "P")
  (let ((table (symbol-value 'idlwave-mode-abbrev-table))
	abbrevs
	str rpl func fmt (len-str 0) (len-rpl 0))
    (mapatoms 
     (lambda (sym)
       (if (symbol-value sym)
	   (progn
	     (setq str (symbol-name sym)
		   rpl (symbol-value sym)
		   func (symbol-function sym))
	     (if arg
		 (setq func (prin1-to-string func))
	       (if (and (listp func) (stringp (nth 2 func)))
		   (setq rpl (concat "EVAL: " (nth 2 func))
			 func "")
		 (setq func "")))
	     (if (or arg (not (string= rpl str)))
		 (progn
		   (setq len-str (max len-str (length str)))
		   (setq len-rpl (max len-rpl (length rpl)))
		   (setq abbrevs (cons (list str rpl func) abbrevs)))))))
     table)
    ;; sort the list
    (setq abbrevs (sort abbrevs (lambda (a b) (string< (car a) (car b)))))
    ;; Make the format
    (setq fmt (format "%%-%ds   %%-%ds   %%s\n" len-str len-rpl))
    (with-output-to-temp-buffer "*Help*"
      (if arg
	  (progn
	    (princ "Abbreviations and Actions in IDLWAVE-Mode\n") 
	    (princ "=========================================\n\n")
	    (princ (format fmt "KEY" "REPLACE" "HOOK"))
	    (princ (format fmt "---" "-------" "----")))
	(princ "Code Abbreviations and Templates in IDLWAVE-Mode\n")
	(princ "================================================\n\n")
	(princ (format fmt "KEY" "ACTION" ""))
	(princ (format fmt "---" "------" "")))
      (mapcar
       (lambda (list)
	 (setq str (car list)
	       rpl (nth 1 list)
	       func (nth 2 list))
	 (princ (format fmt str rpl func)))
       abbrevs)))
  ;; Make sure each abbreviation uses only one display line
  (save-excursion
    (set-buffer "*Help*")
    (setq truncate-lines t)))

(run-hooks 'idlwave-load-hook)

(provide 'idlwave)

;;; idlwave.el ends here



