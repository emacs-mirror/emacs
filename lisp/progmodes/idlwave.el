;;; idlwave.el --- IDL and WAVE CL editing mode for GNU Emacs
;; Copyright (c) 1999, 2000 Free Software Foundation

;; Author: Chris Chase <chase@att.com>
;; Maintainer: John-David Smith <jdsmith@astro.cornell.edu>
;; Version: 4.7
;; Date: $Date: 2001/12/01 13:14:33 $
;; Keywords: languages

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

(defgroup idlwave-routine-info nil
  "Routine Info options for IDLWAVE mode."
  :group 'idlwave)

(defcustom idlwave-scan-all-buffers-for-routine-info t
  "*Non-nil means, scan buffers for IDL programs when updating info.
The scanning is done by the command `idlwave-update-routine-info'.
The following values are allowed:

nil       Don't scan any buffers.
t         Scan all idlwave-mode buffers in the current editing session.
current   Scan only the current buffer, but no other buffers."
  :group 'idlwave-routine-info
  :type '(choice
	  (const :tag "No buffer" nil)
	  (const :tag "All buffers" t)
	  (const :tag "Current buffer only" 'current)))

(defcustom idlwave-query-shell-for-routine-info t
  "*Non-nil means query the shell for info about compiled routines.
Querying the shell is useful to get information about compiled modules,
and it is turned on by default.  However, when you have a complete library
scan, this is not necessary."
  :group 'idlwave-routine-info
  :type 'boolean)

(defcustom idlwave-auto-routine-info-updates
  '(find-file save-buffer kill-buffer compile-buffer)
  "*Controls under what circumstances routine info is updated automatically.
Possible values:
nil       Never
t         All available
(...)     A list of circumstances. Allowed members are:
           find-file       Add info for new IDLWAVE buffers.
           save-buffer     Update buffer info when buffer is saved
           kill-buffer     Remove buffer info when buffer gets killed
           compile-buffer  Update shell info after `idlwave-shell-save-and...'"
  :group 'idlwave-routine-info
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "As often as possible" t)
	  (set :tag "Checklist" :greedy t
	       (const :tag "When visiting a file" find-file)
	       (const :tag "When saving a buffer" save-buffer)
	       (const :tag "After a buffer was killed" kill-buffer)
	       (const :tag "After a buffer was compiled successfully, update shell info" compile-buffer))))
	       
(defcustom idlwave-rinfo-max-source-lines 5
  "*Maximum number of source files displayed in the Routine Info window.
When an integer, it is the maximum number of source files displayed.
t means to show all source files."
  :group 'idlwave-routine-info
  :type 'integer)

(defcustom idlwave-library-path nil
  "Library path for Windows and MacOS.  Not needed under Unix.
When selecting the directories to scan for IDL library routine info,
IDLWAVE can under UNIX query the shell for the exact search path.
However, under Windows and MacOS, the IDLWAVE shell does not work.  In this
case, this variable specifies the path where IDLWAVE can find library files.
The shell will only be asked when this variable is nil.
The value is a list of directories.  A directory preceeded by a `+' will
be searched recursively.  If you set this variable on a UNIX system, the shell
will not be asked.
See also `idlwave-system-directory'."
  :group 'idlwave-routine-info
  :type '(repeat (directory)))

(defcustom idlwave-system-directory ""
  "The IDL system directory for Windows and MacOS.  Not needed under UNIX.
Set this to the value of the `!DIR' system variable in IDL.  IDLWAVE uses
this to find out which of the library routines belong to the official system
library.  All files inside the `lib' subdirectory are considered system
library files - so don't install private stuff in this directory.
On UNIX systems, IDLWAVE queries the shell for the value of `!DIR'.
See also `idlwave-library-path'."
  :group 'idlwave-routine-info
  :type 'directory)

(defcustom idlwave-libinfo-file "~/.idlcat.el"
  "*File for routine information of the IDL library.
When this points to a file, the file will be loaded when IDLWAVE first
accesses routine info (or does completion).
When you scan the library with `idlwave-create-libinfo-file', this file
will be used to store the result."
  :group 'idlwave-routine-info
  :type 'file)

(defcustom idlwave-special-lib-alist nil
  "Alist of regular expressions matching special library directories.
When listing routine source locations, IDLWAVE gives a short hint where
the file defining the routine is located.  By default it lists `SystemLib' 
for routines in the system library `!DIR/lib' and `Library' for anything
else.  This variable can define additional types.  The car of each entry
is a regular expression matching the file name (they normally will match
on the path).  The cdr is the string to be used as identifier.  Max 10
chars are allowed."
  :group 'idlwave-routine-info
  :type '(repeat
	  (cons regexp string)))

(defgroup idlwave-online-help nil
  "Online Help options for IDLWAVE mode."
  :group 'idlwave)

(defcustom idlwave-help-directory ""
  "The directory where idlw-help.txt and idlw-help.el are stored."
  :group 'idlwave-online-help
  :type 'file)

(defcustom idlwave-help-use-dedicated-frame t
  "*Non-nil means, use a separate frame for Online Help if possible."
  :group 'idlwave-online-help
  :type 'boolean)

(defcustom idlwave-help-frame-parameters
  '((height . 20) (unsplittable . t))
  "The frame parameters for the special Online Help frame.
See also `idlwave-help-use-dedicated-frame'.
If you do not set the frame width here, the value specified in
`idlw-help.el' will be used."
  :group 'idlwave-online-help
  :type '(repeat
	  (cons symbol sexp)))

(defcustom idlwave-max-popup-menu-items 20
  "Maximum number of items per pane in popup menus.
Currently only used for class selection during completion help."
  :group 'idlwave-online-help
  :type 'integer)

(defcustom idlwave-extra-help-function 'idlwave-help-with-source
  "The function to call for online help if the normal help fails.
Online help works only for system routines which are described in the
IDL manuals.  A function may be specified to access help from other sources.

The function must accept four arguments: NAME, TYPE, CLASS, KEYWORD.
The Help buffer is current when this function is called, and the help
text should be loaded into this buffer.  If help is found, the function
should return the buffer position which should be used as `window-start'
in the help window.  Also, the variable `idlwave-help-mode-line-indicator'
should be set to a useful string, which will be displayed in the mode line
of the help window.  If should also set the variable `idlwave-min-frame-width'
to a positive integer.  IDLWAVE will ensure that the help frame is at
least that many columns wide.
Failure to find help should be indicated by throwing an error.

When this variable is non-nil, IDLWAVE will allow the mouse-3 help click
for every routine and keyword, even though the item may not be highlighted
in blue (indicating the availability of system documentation).

The default value for this function is `idlwave-help-with-source' which
loads the routine source file into the help buffer.  If you try to write
a different function which accesses a special help file or so, it is
probably a good idea to still call this function as a fallback."
  :group 'idlwave-online-help
  :type 'symbol)

(defcustom idlwave-help-fontify-source-code nil
  "*Non-nil means, fontify source code displayed as help like normal code."
  :group 'idlwave-online-help
  :type 'boolean)

(defcustom idlwave-help-source-try-header t
  "*Non-nil means, try to find help in routine header when displaying source.
Routines which are not documented in the system manual use their source as
help text.  When this variable is non-nil, we try to find a description of
the help item in the first routine doclib header above the routine definition.
If the variable is nil, or if we cannot find/parse the header, the routine
definition is displayed instead."
  :group 'idlwave-online-help
  :type 'boolean)

(defface idlwave-help-link-face
  '((((class color)) (:foreground "Blue"))
    (t (:bold t)))
  "Face for highlighting links into IDLWAVE online help."
  :group 'idlwave-online-help)

(defcustom idlwave-help-activate-links-agressively t
  "*Non-nil means, make all possible links in help active.
This just activates all words which are also a help topic - some links may
be misleading."
  :group 'idlwave-online-help
  :type 'boolean)
  

(defgroup idlwave-completion nil
  "Completion options for IDLWAVE mode."
  :prefix "idlwave"
  :group 'idlwave)

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
  :group 'idlwave-completion
  :type `(repeat
	  (cons (symbol :tag "Derive completion case for")
		,idlwave-tmp)))

(defcustom idlwave-completion-force-default-case nil
  "*Non-nil means, completion will always honor `idlwave-completion-case'.
When nil, only the completion of a mixed case or upper case string
will honor the default settings in `idlwave-completion-case', while
the completion of lower case strings will be completed entirely in
lower case."
  :group 'idlwave-completion
  :type 'boolean)

(defcustom idlwave-complete-empty-string-as-lower-case nil
  "*Non-nil means, the empty string is considered downcase for completion.
The case of what is already in the buffer determines the case of completions.
When this variable is non-nil, the empty string is considered to be downcase.
Completing on the empty string then offers downcase versions of the possible
completions."
  :group 'idlwave-completion
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
  :group 'idlwave-completion
  :type 'boolean)

(defcustom idlwave-highlight-help-links-in-completion t
  "*Non-nil means, highlight completions for which system help is available.
Help can then be accessed with mouse-3.
This option is only effective when the online help system is installed."
  :group 'idlwave-completion
  :type 'boolean)

(defcustom idlwave-support-inheritance t
  "Non-nil means, treat inheritance with completion, online help etc.
When nil,  IDLWAVE only knows about the native methods and tags of a class,
not about inherited ones."
  :group 'idlwave-routine-info
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
  :group 'idlwave-completion
  :type '(choice (const :tag "Don't show" nil)
		 (integer :tag "Number of classes shown" 1)))

(defcustom idlwave-completion-fontify-classes t
  "*Non-nil means, fontify the classes in completions buffer.
This makes it easier to distinguish the completion items from the extra
class info listed.  See `idlwave-completion-show-classes'."
  :group 'idlwave-completion
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
  :group 'idlwave-completion
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
  :group 'idlwave-completion
  :type 'boolean)

(defcustom idlwave-class-arrow-face 'bold
  "*Face to highlight object operator arrows `->' which carry a class property.
When IDLWAVE stores a class name as text property on an object arrow
(see variable `idlwave-store-inquired-class', it highlights the arrow
with this font in order to remind the user that this arrow is special."
  :group 'idlwave-completion
  :type 'symbol)

(defcustom idlwave-resize-routine-help-window t
  "*Non-nil means, resize the Routine-info *Help* window to fit the content."
  :group 'idlwave-completion
  :type 'boolean)

(defcustom idlwave-keyword-completion-adds-equal t
  "*Non-nil means, completion automatically adds `=' after completed keywords."
  :group 'idlwave-completion
  :type 'boolean)

(defcustom idlwave-function-completion-adds-paren t
  "*Non-nil means, completion automatically adds `(' after completed function.
Nil means, don't add anything.
A value of `2' means, also add the closing parenthesis and position cursor
between the two."
  :group 'idlwave-completion
  :type '(choice (const :tag "Nothing" nil)
		 (const :tag "(" t)
		 (const :tag "()" 2)))

(defcustom idlwave-completion-restore-window-configuration t
  "*Non-nil means, try to restore the window configuration after completion.
When completion is not unique, Emacs displays a list of completions.
This messes up your window configuration.  With this variable set, IDLWAVE
restores the old configuration after successful completion."
  :group 'idlwave-completion
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
for keywords.  If t, it is padded the same as for assignments.
If nil then spaces are removed.  With any other value, spaces are left
unchanged."
  :group 'idlwave-abbrev-and-indent-action
  :type '(choice
	  (const :tag "Pad like assignments" t)
	  (const :tag "Remove space near `='" nil)
	  (const :tag "Keep space near `='" 'keep)))

(defcustom idlwave-show-block t
  "*Non-nil means point blinks to block beginning for `idlwave-show-begin'."
  :group 'idlwave-abbrev-and-indent-action
  :type 'boolean)

(defcustom idlwave-expand-generic-end nil
  "*Non-nil means expand generic END to ENDIF/ENDELSE/ENDWHILE etc."
  :group 'idlwave-abbrev-and-indent-action
  :type 'boolean)

(defcustom idlwave-reindent-end t
  "*Non-nil means re-indent line after END was typed."
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
  "Path locations of external commands used by IDLWAVE."
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

;;; Some Shell variables which must be defined here.-----------------------

(defcustom idlwave-shell-debug-modifiers '()
  "List of modifiers to be used for the debugging commands.
Will be used to bind debugging commands in the shell buffer and in all
source buffers.  These are additional convenience bindings, the debugging
commands are always available with the `C-c C-d' prefix.
If you set this to '(control shift), this means setting a breakpoint will
be on `C-S-b', compiling a source file on `C-S-c' etc.  Possible modifiers
are `control', `meta', `super', `hyper', `alt', and `shift'."
  :group 'idlwave-shell-general-setup
  :type '(set :tag "Specify modifiers"
	       (const control)
	       (const meta)
	       (const super)
	       (const hyper)
	       (const alt)
	       (const shift)))

(defcustom idlwave-shell-automatic-start nil
  "*If non-nil attempt invoke idlwave-shell if not already running.
This is checked when an attempt to send a command to an
IDL process is made."
  :group 'idlwave-shell-general-setup
  :type 'boolean)

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
	  (const :tag "Tags in Structure Definition"       structtag)
	  (const :tag "Structure Name"                     structname)
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

(defvar idlwave-experimental nil
  "Non-nil means turn on a few experimental features.
This variable is only for the maintainer, to test difficult stuff,
while still distributing stable releases.
As a user, you should not set this to t.")

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
;	 (prin1-to-string
;	  (concat 
;	   "\\<\\("
;	   (regexp-opt 
;	    '("and" "or" "xor" "not"
;	      "eq" "ge" "gt" "le" "lt" "ne" 
;	      "for" "do" "endfor"
;	      "if" "then" "endif" "else" "endelse" 
;	      "case" "of" "endcase"
;	      "switch" "break" "continue" "endswitch"
;	      "begin" "end"
;	      "repeat" "until" "endrep"
;	      "while" "endwhile" 
;	      "goto" "return"
;	      "inherits" "mod"
;	      "compile_opt" "forward_function"
;	      "on_error" "on_ioerror"))  ; on_error is not officially reserved
;	   "\\)\\>")))

	"\\<\\(and\\|b\\(egin\\|reak\\)\\|c\\(ase\\|o\\(mpile_opt\\|ntinue\\)\\)\\|do\\|e\\(lse\\|nd\\(case\\|else\\|for\\|if\\|rep\\|switch\\|while\\)?\\|q\\)\\|for\\(ward_function\\)?\\|g\\(oto\\|[et]\\)\\|i\\(f\\|nherits\\)\\|l[et]\\|mod\\|n\\(e\\|ot\\)\\|o\\(n_\\(error\\|ioerror\\)\\|[fr]\\)\\|re\\(peat\\|turn\\)\\|switch\\|then\\|until\\|while\\|xor\\)\\>")

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

       ;; Tags in structure definitions.  Note that this definition actually
       ;; collides with labels, so we have to use the same face.
       (structtag
	'("\\<\\([a-zA-Z][a-zA-Z0-9_]*:\\)[^:]" (1 font-lock-reference-face)))

       ;; Structure names
       (structname
	'("\\({\\|\\<inherits\\s-\\)\\s-*\\([a-zA-Z][a-zA-Z0-9_]*\\)[},\t \n]"
	  (2 font-lock-function-name-face)))

       ;; Named parameters, like /xlog or ,xrange=[]
       ;; This is anchored to the comma preceeding the keyword.
       ;; Treats continuation lines, works only during whole buffer
       ;; fontification.  Slow, use it only in fancy fontification.
       (keyword-parameters
	'("\\(,\\|[a-zA-Z0-9_](\\)[ \t]*\\(\\$[ \t]*\\(;.*\\)?\\(\n[ \t]*;.*\\)*\n[ \t]*\\)?\\(/[a-zA-Z_]\\sw*\\|[a-zA-Z_]\\sw*[ \t]*=\\)"
	  (5 font-lock-reference-face)))

       ;; System variables start with a bang.
       (system-variables
	'("\\(![a-zA-Z_0-9]+\\(\\.\\sw+\\)?\\)"
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
	structtag structtag
	structname structname
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
	      structtag
	      structname
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

(defconst idlwave-begin-block-reg 
  "\\<\\(pro\\|function\\|begin\\|case\\|switch\\)\\>"
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
  "\\<end\\(\\|case\\|switch\\|else\\|for\\|if\\|rep\\|while\\)\\>"
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
    ("switch"   . "endswitch")
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
   ;; "endif else" is the only possible "end" that can be
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
   '(switch . ("switch\\>" nil))
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
the leftover unidentified statements containing an equal sign."  )

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

(defconst idlwave-mode-version " 4.7")

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
              (if (idlwave-quoted)
		  (progn (unexpand-abbrev) nil)
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

(defmacro idlwave-with-special-syntax (&rest body)
  "Execute BODY with a different systax table."
  `(let ((saved-syntax (syntax-table)))
     (unwind-protect
	 (progn
	   (set-syntax-table idlwave-find-symbol-syntax-table)
	   ,@body)
       (set-syntax-table saved-syntax))))

(defvar idlwave-print-symbol-syntax-table
  (copy-syntax-table idlwave-mode-syntax-table)
  "Syntax table that treats symbol characters as word characters.")

(modify-syntax-entry ?$   "w"  idlwave-find-symbol-syntax-table)
(modify-syntax-entry ?_   "w"  idlwave-find-symbol-syntax-table)
(modify-syntax-entry ?!   "w"  idlwave-find-symbol-syntax-table)
(modify-syntax-entry ?.   "w"  idlwave-find-symbol-syntax-table)

(defmacro idlwave-with-special-syntax1 (&rest body)
  "Execute BODY with a different systax table."
  `(let ((saved-syntax (syntax-table)))
     (unwind-protect
	 (progn
	   (set-syntax-table idlwave-find-symbol-syntax-table)
	   ,@body)
       (set-syntax-table saved-syntax))))

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
(define-key idlwave-mode-map [(control tab)] 'idlwave-hard-tab)
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
(define-key idlwave-mode-map "\C-c\C-d\C-b" 'idlwave-shell-break-here)
(when (and (boundp 'idlwave-shell-debug-modifiers)
	 (listp idlwave-shell-debug-modifiers)
	 (not (equal idlwave-shell-debug-modifiers '())))
  ;; Bind the debug commands also with the special modifiers.
  (let ((shift (memq 'shift idlwave-shell-debug-modifiers))
	(mods-noshift (delq 'shift 
			    (copy-sequence idlwave-shell-debug-modifiers))))
    (define-key idlwave-mode-map 
      (vector (append mods-noshift (list (if shift ?C ?c))))
      'idlwave-shell-save-and-run)
    (define-key idlwave-mode-map 
      (vector (append mods-noshift (list (if shift ?B ?b))))
      'idlwave-shell-break-here)))
(define-key idlwave-mode-map "\C-c\C-f" 'idlwave-for)
;;  (define-key idlwave-mode-map "\C-c\C-f" 'idlwave-function)
;;  (define-key idlwave-mode-map "\C-c\C-p" 'idlwave-procedure)
(define-key idlwave-mode-map "\C-c\C-r" 'idlwave-repeat)
(define-key idlwave-mode-map "\C-c\C-w" 'idlwave-while)
(define-key idlwave-mode-map "\C-c\C-k" 'idlwave-kill-autoloaded-buffers)
(define-key idlwave-mode-map "\C-c\C-s" 'idlwave-shell)
(define-key idlwave-mode-map "\C-c\C-l" 'idlwave-shell-recenter-shell-window)
(define-key idlwave-mode-map "\C-c\C-b" 'idlwave-list-buffer-load-path-shadows)
(autoload 'idlwave-shell "idlw-shell"
  "Run an inferior IDL, with I/O through buffer `(idlwave-shell-buffer)'." t)
(autoload 'idlwave-shell-send-command "idlw-shell")
(autoload 'idlwave-shell-recenter-shell-window "idlw-shell"
  "Run `idlwave-shell' and switch back to current window" t)
(autoload 'idlwave-shell-save-and-run "idlw-shell"
  "Save and run buffer under the shell." t)
(autoload 'idlwave-shell-break-here "idlw-shell"
  "Set breakpoint in current line." t)
(define-key idlwave-mode-map "\C-c\C-v"   'idlwave-find-module)
(define-key idlwave-mode-map "\C-c?"      'idlwave-routine-info)
(define-key idlwave-mode-map "\M-?"       'idlwave-context-help)
(define-key idlwave-mode-map [(meta tab)] 'idlwave-complete)
(define-key idlwave-mode-map "\C-c\C-i"   'idlwave-update-routine-info)
(define-key idlwave-mode-map "\C-c="      'idlwave-resolve)
(define-key idlwave-mode-map 
  (if (featurep 'xemacs) [(shift button3)] [(shift mouse-3)])
  'idlwave-mouse-context-help)

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
  (define-abbrev tb (concat c "sw")  "" (idlwave-code-abbrev idlwave-switch))
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
  (define-abbrev tb (concat c "es") "endswitch"    'idlwave-show-begin)
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
  (define-abbrev tb "break"      "break"      (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "case"       "case"       (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "common"     "common"     (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "continue"   "continue"   (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "do"         "do"         (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "else"       "else"       (idlwave-keyword-abbrev 0 t))
  (define-abbrev tb "end"        "end"        'idlwave-show-begin-check)
  (define-abbrev tb "endcase"    "endcase"    'idlwave-show-begin-check)
  (define-abbrev tb "endelse"    "endelse"    'idlwave-show-begin-check)
  (define-abbrev tb "endfor"     "endfor"     'idlwave-show-begin-check)
  (define-abbrev tb "endif"      "endif"      'idlwave-show-begin-check)
  (define-abbrev tb "endrep"     "endrep"     'idlwave-show-begin-check)
  (define-abbrev tb "endswitch"  "endswitch"  'idlwave-show-begin-check)
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
  (define-abbrev tb "switch"     "switch"     (idlwave-keyword-abbrev 0 t))
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
   information, which is also used for completion (see item 4).

3. Online IDL Help
   ---------------
   \\[idlwave-context-help] displays the IDL documentation relevant
   for the system variable, keyword, or routine at point.  A single key
   stroke gets you directly to the right place in the docs.  Two additional
   files (an ASCII version of the IDL documentation and a topics file) must
   be installed for this - check the IDLWAVE webpage for these files.

4. Completion
   ----------
   \\[idlwave-complete] completes the names of procedures, functions
   class names and keyword parameters.  It is context sensitive and
   figures out what is expected at point (procedure/function/keyword).
   Lower case strings are completed in lower case, other strings in
   mixed or upper case.

5. Code Templates and Abbreviations
   --------------------------------
   Many Abbreviations are predefined to expand to code fragments and templates.
   The abbreviations start generally with a `\\`.  Some examples

   \\pr        PROCEDURE template
   \\fu        FUNCTION template
   \\c         CASE statement template
   \\sw        SWITCH statement template
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

6. Automatic Case Conversion
   -------------------------
   The case of reserved words and some abbrevs is controlled by
   `idlwave-reserved-word-upcase' and `idlwave-abbrev-change-case'.

7. Automatic END completion
   ------------------------
   If the variable `idlwave-expand-generic-end' is non-nil, each END typed
   will be converted to the specific version, like ENDIF, ENDFOR, etc.

8. Hooks
   -----
   Loading idlwave.el runs `idlwave-load-hook'.
   Turning on `idlwave-mode' runs `idlwave-mode-hook'.

9. Documentation and Customization
   -------------------------------
   Info documentation for this package is available.  Use \\[idlwave-info]
   to display (complain to your sysadmin if that does not work).
   For Postscript and HTML versions of the documentation, check IDLWAVE's
   homepage at `http://www.strw.leidenuniv.nl/~dominik/Tools/idlwave'.
   IDLWAVE has customize support - see the group `idlwave'.

10.Keybindings
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
  (add-hook 'post-command-hook 'idlwave-command-hook nil 'local)

  ;; Make local hooks for buffer updates
  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'idlwave-kill-buffer-update nil 'local)
  (make-local-hook 'after-save-hook)
  (add-hook 'after-save-hook 'idlwave-save-buffer-update nil 'local)
  (add-hook 'after-save-hook 'idlwave-revoke-license-to-kill nil 'local)

  ;; Update the routine info with info about current buffer?
  (idlwave-new-buffer-update)

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
  ;; Re-indent end line
  (if idlwave-reindent-end
      (idlwave-indent-line))
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
       ((looking-at "pro\\|case\\|switch\\|function\\>")
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
  (insert "end")
  (idlwave-show-begin))

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
    (if (and (equal (char-after (1- (point))) ?\ )
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
  ;; Expand abbreviation, just like normal RET would.
  (and abbrev-mode (expand-abbrev))
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
	    (if (not (member (char-before) '(?\  ?\t)))
		(insert " "))
            (insert idlwave-continuation-char))
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


(defun idlwave-current-routine ()
  "Return (NAME TYPE CLASS) of current routine."
  (idlwave-routines)
  (save-excursion
    (idlwave-beginning-of-subprogram)
    (if (looking-at "[ \t]*\\<\\(pro\\|function\\)\\>\\s-+\\(\\([a-zA-Z0-9$_]+\\)::\\)?\\([a-zA-Z0-9$_]+\\)")
	(let* ((type (if (string= (downcase (match-string 1)) "pro")
			 'pro 'function))
	       (class (idlwave-sintern-class (match-string 3)))
	       (name (idlwave-sintern-routine-or-method (match-string 4) class)))
	  (list name type class)))))

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

(defun idlwave-end-of-statement ()
  "Moves point to the end of the current IDL statement.
If not in a statement just moves to end of line.  Returns position."
  (interactive)
  (while (and (idlwave-is-continuation-line)
              (= (forward-line 1) 0))
    (while (and (idlwave-is-comment-or-empty-line)
		(= (forward-line 1) 0))))
  (end-of-line)
  (point))

(defun idlwave-end-of-statement0 ()
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

(defun idlwave-skip-label-or-case ()
  "Skip label or case statement element.
Returns position after label.
If there is no label point is not moved and nil is returned."
  ;; Case expressions and labels are terminated by a colon.
  ;; So we find the first colon in the line and make sure
  ;; - no `?' is before it (might be a ? b : c)
  ;; - it is not in a comment
  ;; - not in a string constant
  ;; - not in parenthesis (like a[0:3])
  ;; As many in this mode, this function is heuristic and not an exact
  ;; parser. 
  (let ((start (point))
        (end (idlwave-find-key ":" 1 'nomark
			       (save-excursion
				 (idlwave-end-of-statement) (point)))))
    (if (and end
             (= (nth 0 (parse-partial-sexp start end)) 0)
	     (not (string-match "\\?" (buffer-substring start end))))
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
substatement."
  (let ((orig (point))
        (eos (idlwave-end-of-statement))
        (ifnest 0)
        st nst last)
    (idlwave-beginning-of-statement)
    (idlwave-skip-label-or-case)
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
    ;; If a continuation line starts here, move to next line
    (if (looking-at "[ \t]*\\$\\([ \t]*\\(;\\|$\\)\\)")
	(beginning-of-line 2))
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
If `idlwave-pad-keyword' is t then keyword assignment is treated just
like assignment statements.  When nil, spaces are removed for keyword
assignment.  Any other value keeps the current space around the `='.
Limits in for loops are treated as keyword assignment.
See `idlwave-surround'. "
  ;; Even though idlwave-surround checks `idlwave-surround-by-blank' this
  ;; check saves the time of finding the statement type.
  (if idlwave-surround-by-blank
      (let ((st (save-excursion
                  (idlwave-start-of-substatement t)
                  (idlwave-statement-type))))

        (cond ((or (and (equal (car (car st)) 'assign)
			(equal (cdr st) (point)))
		   (eq t idlwave-pad-keyword))
	       ;; An assignment statement or keywor and we need padding
	       (idlwave-surround before after))
	      ((null idlwave-pad-keyword)
	       ;; Spaces should be removed at a keyword
	       (idlwave-surround 0 0))
	      (t)))))	       

(defun idlwave-indent-and-action ()
  "Call `idlwave-indent-line' and do expand actions."
  (interactive)
  (save-excursion
    (if	(and idlwave-expand-generic-end 
	     (re-search-backward "\\<\\(end\\)\\s-*\\=" 
				 (max 0 (- (point) 10)) t)
	     (looking-at "\\(end\\)\\([ \n\t]\\|\\'\\)"))
	(progn (goto-char (match-end 1))
	       (idlwave-show-begin))))
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
   (make-string (max min (- col (current-column))) ?\ )))

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

(defun idlwave-find-key (key-re &optional dir nomark limit)
  "Move to next match of the regular expression KEY-RE.
Matches inside comments or string constants will be ignored.
If DIR is negative, the search will be backwards.
At a successful match, the mark is pushed unless NOMARK is non-nil.
Searches are limited to LIMIT.
Searches are case-insensitive and use a special syntax table which
treats `$' and `_' as word characters.
Return value is the beginning of the match or (in case of failure) nil."
  (setq dir (or dir 0))
  (let ((case-fold-search t)
	(search-func (if (> dir 0) 're-search-forward 're-search-backward))
	found)
    (idlwave-with-special-syntax
     (save-excursion
       (catch 'exit
	 (while (funcall search-func key-re limit t)
	   (if (not (idlwave-quoted))
	       (throw 'exit (setq found (match-beginning 0))))))))
    (if found
	(progn
	  (if (not nomark) (push-mark))
	  (goto-char found)
	  found)
      nil)))

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
  "Tests if the current line is a comment line."
  (save-excursion
    (beginning-of-line 1)
    (looking-at "[ \t]*;")))

(defun idlwave-is-comment-or-empty-line ()
  "Tests if the current line is a comment line."
  (save-excursion
    (beginning-of-line 1)
    (looking-at "[ \t]*[;\n]")))

(defun idlwave-look-at (regexp &optional cont beg)
  "Searches current line from current point for REGEXP.
If optional argument CONT is non-nil, searches to the end of
the current statement.
If optional arg BEG is non-nil, search starts from the beginning of the
current statement.
Ignores matches that end in a comment or inside a string expression.
Returns point if successful, nil otherwise.
This function produces unexpected results if REGEXP contains quotes or
a comment delimiter. The search is case insensitive.
If successful leaves point after the match, otherwise, does not move point."
  (let ((here (point))
        (case-fold-search t)
        (eos (save-excursion
	       (if cont (idlwave-end-of-statement) (end-of-line))
	       (point)))
        found)
    (idlwave-with-special-syntax
     (if beg (idlwave-beginning-of-statement))
     (while (and (setq found (re-search-forward regexp eos t))
		 (idlwave-quoted))))
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
                      (insert (make-string diff ?\ ))))
                (forward-line -1))
              )
	  
          ;; No hang. Instead find minimum indentation of paragraph
          ;; after first line.
          ;; For the following while statement, since START is at the
          ;; beginning of line and END is at the end of line
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
                                               ?\ )))
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
          (subst-char-in-region start (+ start first-indent -1) ?\  ?~ nil)
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
  (insert "\n;\n;\t\t"))t

(defun idlwave-doc-modification ()
  "Insert a brief modification log at the beginning of the current program.
Looks for an occurrence of the value of user variable
`idlwave-doc-modifications-keyword' if non-nil. Inserts time and user name
and places the point for the user to add a log. Before moving, saves
location on mark ring so that the user can return to previous point."
  (interactive)
  (push-mark)
  (let* (beg end)
    (if (and (or (re-search-backward idlwave-doclib-start nil t)
		 (progn
		   (goto-char (point-min))
		   (re-search-forward idlwave-doclib-start nil t)))
	     (setq beg (match-beginning 0))
	     (re-search-forward idlwave-doclib-end nil t)
	     (setq end (match-end 0)))
	(progn
	  (goto-char beg)
	  (if (re-search-forward 
	       (concat idlwave-doc-modifications-keyword ":")
	       end t)
	      (end-of-line)
	    (goto-char end)
	    (end-of-line -1)
	    (insert "\n" comment-start "\n")
	    (insert comment-start " " idlwave-doc-modifications-keyword ":"))
	  (insert "\n;\n;\t")
	  (run-hooks 'idlwave-timestamp-hook))
      (error "No valid DOCLIB header"))))

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
	    ;; start of a string) is the null string.
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
  (if (eq major-mode 'idlwave-shell-mode)
      ;; This is a gross hack to avoit template abbrev expansion
      ;; in the shell.  FIXME: This is a dirty hack.
      (if (and (eq this-command 'self-insert-command)
	       (equal last-abbrev-location (point)))
	  (insert last-abbrev-text)
	(error "No templates in idlwave-shell"))
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
	  (message prompt)))))
  
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

(defun idlwave-switch ()
  "Build skeleton IDL switch statement."
  (interactive)
  (idlwave-template 
   (idlwave-rw-case "switch")
   (idlwave-rw-case " of\n\nendswitch")
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

(defvar idlwave-outlawed-buffers nil
  "List of buffer pulled up by idlwave for special reasons.
Buffers in this list may be killed by `idlwave-kill-autoloaded-buffers'.")

(defun idlwave-find-file-noselect (file &optional why)
  ;; Return a buffer visiting file.
  (or (idlwave-get-buffer-visiting file)
      (let ((buf (find-file-noselect file)))
	(if why (add-to-list 'idlwave-outlawed-buffers (cons buf why)))
	buf)))

(defun idlwave-kill-autoloaded-buffers ()
  "Cleanup by killing buffers created automatically by IDLWAVE.
Function prompts for a letter to identify the buffers to kill.
Possible letters are:

f    Buffers created by the command \\[idlwave-find-module] or mouse
     clicks in the routine info window.
s    Buffers created by the IDLWAVE Shell to display where execution
     stopped or an error was found.
a    Both of the above.

Buffer containing unsaved changes require confirmation before they are killed."
  (interactive)
  (if (null idlwave-outlawed-buffers)
      (error "No IDLWAVE-created buffers available")
    (princ (format "Kill IDLWAVE-created buffers: [f]ind source(%d), [s]hell display(%d), [a]ll ? "
		   (idlwave-count-outlawed-buffers 'find)
		   (idlwave-count-outlawed-buffers 'shell)))
    (let ((c (read-char)))
      (cond
       ((member c '(?f ?\C-f))
	(idlwave-do-kill-autoloaded-buffers 'find))
       ((member c '(?s ?\C-s))
	(idlwave-do-kill-autoloaded-buffers 'shell))
       ((member c '(?a ?\C-a))
	(idlwave-do-kill-autoloaded-buffers t))
       (t (error "Abort"))))))

(defun idlwave-count-outlawed-buffers (tag)
  "How many outlawed buffers have tag TAG?"
  (length (delq nil
		(mapcar 
		 (lambda (x) (eq (cdr x) tag)) 
		 idlwave-outlawed-buffers))))

(defun idlwave-do-kill-autoloaded-buffers (&rest reasons)
  "Kill all buffers pulled up by IDLWAVE matching REASONS."
  (let* ((list (copy-sequence idlwave-outlawed-buffers))
	 (cnt 0)
	 entry)
    (while (setq entry (pop list))
      (if (buffer-live-p (car entry))
	  (and (or (memq t reasons)
		   (memq (cdr entry) reasons))
	       (kill-buffer (car entry))
	       (incf cnt)
	       (setq idlwave-outlawed-buffers 
		     (delq entry idlwave-outlawed-buffers)))
	(setq idlwave-outlawed-buffers 
	      (delq entry idlwave-outlawed-buffers))))
    (message "%d buffer%s killed" cnt (if (= cnt 1) "" "s"))))

(defun idlwave-revoke-license-to-kill ()
  "Remove BUFFER from the buffers which may be killed.
Killing would be done by `idlwave-do-kill-autoloaded-buffers'.
Intended for `after-save-hook'."
  (let* ((buf (current-buffer))
	 (entry (assq buf idlwave-outlawed-buffers)))
    ;; Revoke license
    (if entry
	(setq idlwave-outlawed-buffers 
	      (delq entry idlwave-outlawed-buffers)))
    ;; Remove this function from the hook.
    (remove-hook 'after-save-hook 'idlwave-revoke-license-to-kill 'local)))

(defvar idlwave-path-alist)
(defun idlwave-locate-lib-file (file)
  ;; Find FILE on the scanned lib path and return a buffer visiting it
  (let* ((dirs idlwave-path-alist)
	 dir efile)
    (catch 'exit
      (while (setq dir (car (pop dirs)))
	(if (file-regular-p
	     (setq efile (expand-file-name file dir)))
	    (throw 'exit efile))))))
(defun idlwave-expand-lib-file-name (file)
  ;; Find FILE on the scanned lib path and return a buffer visiting it
  (cond 
   ((null file) nil)
   ((string-match "\\`\\({\\([0-9]+\\)}/\\)\\(.*\\)" file)
    (expand-file-name (match-string 3 file)
		      (car (nth (1- (string-to-int (match-string 2 file)))
				idlwave-path-alist))))
   ((file-name-absolute-p file) file)
   (t (idlwave-locate-lib-file file))))

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

;; Creating new sintern tables

(defun idlwave-new-sintern-type (tag)
  "Define a variable and a function to sintern the new type TAG.
This defines the function `idlwave-sintern-TAG' and the variable
`idlwave-sint-TAGs'."
  (let* ((name (symbol-name tag))
	 (names (concat name "s"))
	 (var (intern (concat "idlwave-sint-" names)))
	 (func (intern (concat "idlwave-sintern-" name))))
    (set var nil) ; initial value of the association list
    (fset func    ; set the function
	  `(lambda (name &optional set)
	     (cond ((not (stringp name)) name)
		   ((cdr (assoc (downcase name) ,var)))
		   (set
		    (setq ,var (cons (cons (downcase name) name) ,var))
		    name)
		   (name))))))

(defun idlwave-reset-sintern-type (tag)
  "Reset the sintern variable associated with TAG."
  (set (intern (concat "idlwave-sint-" (symbol-name tag) "s")) nil))

;;---------------------------------------------------------------------------


;; The variables which hold the information
(defvar idlwave-system-routines nil
  "Holds the routine-info obtained by scanning buffers.")
(defvar idlwave-buffer-routines nil
  "Holds the routine-info obtained by scanning buffers.")
(defvar idlwave-compiled-routines nil
  "Holds the routine-info obtained by asking the shell.")
(defvar idlwave-unresolved-routines nil
  "Holds the unresolved routine-info obtained by asking the shell.")
(defvar idlwave-library-routines nil
  "Holds the procedure routine-info from the library scan.")
(defvar idlwave-path-alist nil
  "Alist with !PATH directories and a flag if the dir has been scanned.")
(defvar idlwave-true-path-alist nil
  "Like `idlwave-path-alist', but with true filenames.")
(defvar idlwave-routines nil
  "Holds the combinded procedure routine-info.")
(defvar idlwave-class-alist nil
  "Holds the class names known to IDLWAVE.")
(defvar idlwave-class-history nil
  "The history of classes selected with the minibuffer.")
(defvar idlwave-force-class-query nil)
(defvar idlwave-before-completion-wconf nil
  "The window configuration just before the completion buffer was displayed.")
(defvar idlwave-last-system-routine-info-cons-cell nil
  "The last cons cell in the system routine info.")

;;
;; The code to get routine info from different sources.

(defvar idlwave-system-routines)
(defun idlwave-routines ()
  "Provide a list of IDL routines.
This routine loads the builtin routines on the first call.  Later it
only returns the value of the variable."
  (or idlwave-routines
      (progn
	(idlwave-update-routine-info)
	;; return the current value
	idlwave-routines)))

(defvar idlwave-update-rinfo-hook nil
  "List of functions which should run after a global rinfo update.
Does not run after automatic updates of buffer or the shell.")

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
		       (null idlwave-system-routines))))
      
      (setq idlwave-buffer-routines nil
	    idlwave-compiled-routines nil
	    idlwave-unresolved-routines nil)
      ;; Reset the appropriate hashes
      (idlwave-reset-sintern (cond (reload t)
				   ((null idlwave-system-routines) t)
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
	    (idlwave-concatenate-rinfo-lists nil t))
      
	(when ask-shell
	  ;; Ask the shell about the routines it knows.
	  (message "Querying the shell")
	  (idlwave-shell-update-routine-info nil t))))))

(defun idlwave-load-system-rinfo ()
  ;; Load and case-treat the system and lib info files.
  (load "idlw-rinfo" t)
  (message "Normalizing idlwave-system-routines...")
  (setq idlwave-system-routines
	(idlwave-sintern-rinfo-list idlwave-system-routines 'sys))
  (message "Normalizing idlwave-system-routines...done")
  (setq idlwave-routines (copy-sequence idlwave-system-routines))
  (setq idlwave-last-system-routine-info-cons-cell
	(nthcdr (1- (length idlwave-routines)) idlwave-routines))
  (when (and (stringp idlwave-libinfo-file)
	     (file-regular-p idlwave-libinfo-file))
    (condition-case nil
	(progn
	  (load-file idlwave-libinfo-file)
	  (setq idlwave-true-path-alist nil)
	  (message "Normalizing idlwave-library-routines...")
	  (setq idlwave-library-routines (idlwave-sintern-rinfo-list
					  idlwave-library-routines 'sys))
	  (message "Normalizing idlwave-library-routines...done"))
      (error nil)))
  (run-hooks 'idlwave-after-load-rinfo-hook))


(defun idlwave-update-buffer-routine-info ()
  (let (res)
    (cond 
     ((eq idlwave-scan-all-buffers-for-routine-info t)
      ;; Scan all buffers, current buffer last
      (message "Scanning all buffers...")
      (setq res (idlwave-get-routine-info-from-buffers 
		 (reverse (buffer-list)))))
     ((null idlwave-scan-all-buffers-for-routine-info)
      ;; Don't scan any buffers
      (setq res nil))
     (t
      ;; Just scan this buffer
      (if (eq major-mode 'idlwave-mode)
	  (progn
	    (message "Scanning current buffer...")
	    (setq res (idlwave-get-routine-info-from-buffers
		       (list (current-buffer))))))))
    ;; Put the result into the correct variable
    (setq idlwave-buffer-routines 
	  (idlwave-sintern-rinfo-list res t))))

(defun idlwave-concatenate-rinfo-lists (&optional quiet run-hook)
  "Put the different sources for routine information together."
  ;; The sequence here is important because earlier definitions shadow 
  ;; later ones.  We assume that if things in the buffers are newer
  ;; then in the shell of the system, it is meant to be different.

  (setcdr idlwave-last-system-routine-info-cons-cell
	  (append idlwave-buffer-routines
		  idlwave-compiled-routines
		  idlwave-library-routines))
  (setq idlwave-class-alist nil)

  ;; Give a message with information about the number of routines we have.
  (unless quiet
    (message 
     "Routine info updated:  buffer(%d)  compiled(%d)  catalog(%d)  system(%d)"
     (length idlwave-buffer-routines)
     (length idlwave-compiled-routines)
     (length idlwave-library-routines)
     (length idlwave-system-routines)))
  (if run-hook
      (run-hooks 'idlwave-update-rinfo-hook)))

(defun idlwave-class-alist ()
  "Return the class alist - make it if necessary."
  (or idlwave-class-alist
      (let (class)
	(loop for x in idlwave-routines do
	  (when (and (setq class (nth 2 x))
		     (not (assq class idlwave-class-alist)))
	    (push (list class) idlwave-class-alist)))
	idlwave-class-alist)))      

;; Three functions for the hooks
(defun idlwave-save-buffer-update ()
  (idlwave-update-current-buffer-info 'save-buffer))
(defun idlwave-kill-buffer-update ()
  (idlwave-update-current-buffer-info 'kill-buffer))
(defun idlwave-new-buffer-update ()
  (idlwave-update-current-buffer-info 'find-file))

(defun idlwave-update-current-buffer-info (why)
  "Undate idlwave-routines for current buffer.  Can run from after-save-hook."
  (when (and (eq major-mode 'idlwave-mode)
	     (or (eq t idlwave-auto-routine-info-updates)
		 (memq why idlwave-auto-routine-info-updates))
	     idlwave-scan-all-buffers-for-routine-info
	     idlwave-routines)
    (condition-case nil
	(let (routines)
	  (idlwave-replace-buffer-routine-info
	   (buffer-file-name)
	   (if (eq why 'kill-buffer)
	       nil
	     (setq routines
		   (idlwave-sintern-rinfo-list
		    (idlwave-get-routine-info-from-buffers
		     (list (current-buffer))) 'set))))
	  (idlwave-concatenate-rinfo-lists 'quiet)
	  routines)
      (error nil))))

(defun idlwave-replace-buffer-routine-info (file new)
  "Cut the part from FILE out of `idlwave-buffer-routines' and add NEW."
  (let ((list idlwave-buffer-routines) 
	found)
    (while list
      ;; The following test uses eq to make sure it works correctly
      ;; when two buffers visit the same file.  Then the file names
      ;; will be equal, but not eq.
      (if (eq (cdr (nth 3 (car list))) file)
	  (progn
	    (setcar list nil)
	    (setq found t))
	(if found
	    ;; End of that section reached. Jump. 
	    (setq list nil)))
      (setq list (cdr list)))
    (setq idlwave-buffer-routines
	  (append new (delq nil idlwave-buffer-routines)))))

;;----- Scanning buffers -------------------

(defun idlwave-get-routine-info-from-buffers (buffers)
  "Call `idlwave-get-buffer-routine-info' on idlwave-mode buffers in BUFFERS."
  (let (buf routine-lists res)
    (save-excursion
      (while (setq buf (pop buffers))
	(set-buffer buf)
	(if (and (eq major-mode 'idlwave-mode)
		 buffer-file-name)
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
		"^[ \t]*\\(pro\\|function\\)[ \t]" nil t)
	  (setq string (buffer-substring 
			(match-beginning 0)
			(progn 
			  (idlwave-end-of-statement)
			  (point))))
	  (setq entry (idlwave-parse-definition string))
	  (push entry routine-list))))
    routine-list))

(defvar idlwave-scanning-lib-dir)
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
    (while (string-match "\n" string)
      (setq string (replace-match " " t nil string)))
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
	    ",\\s-*\\([a-zA-Z][a-zA-Z0-9$_]*\\|\\(_ref\\)?_extra\\)\\s-*\\(=\\)?"
	    string start)
      (setq start (match-end 0))
      (if (match-beginning 3)
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
;		((string= (downcase 
;			   (file-name-sans-extension
;			    (file-name-nondirectory (buffer-file-name))))
;			  (downcase name))
;		 (list 'lib))
;		(t (cons 'lib (file-name-nondirectory (buffer-file-name))))
		(t (cons 'lib (concat idlwave-scanning-lib-dir
				      (file-name-nondirectory (buffer-file-name))))))
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

(defvar idlwave-sys-dir nil
  "Internal variable.")

(defun idlwave-sys-dir ()
  "Return the syslib directory, or a dummy that never matches."
  (or idlwave-sys-dir
      "@@@@@@@@"))

(defvar idlwave-shell-path-query)
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
	       (> (length idlwave-libinfo-file) 0)
	       (file-accessible-directory-p
		(file-name-directory idlwave-libinfo-file))
	       (not (string= "" (file-name-nondirectory 
				 idlwave-libinfo-file))))
    (error "`idlwave-libinfo-file' does not point to file in accessible directory"))
  
  (cond
   ((and arg idlwave-path-alist
	 (consp (car idlwave-path-alist))
	 idlwave-sys-dir)
    ;; Rescan the known directories
    (idlwave-scan-lib-files
     idlwave-sys-dir
      idlwave-path-alist))
   (idlwave-library-path
    ;; Get the directories from that variable
    (idlwave-display-libinfo-widget
     idlwave-system-directory
     (idlwave-expand-path idlwave-library-path)
     (delq nil (mapcar (lambda (x) (if (consp x) (if (cdr x) (car x) nil) x))
		       idlwave-path-alist))))
   (t
    ;; Ask the shell for the path and run the widget
    (message "Asking the shell for IDL path...")
    (require 'idlw-shell)
    (idlwave-shell-send-command idlwave-shell-path-query
				'(idlwave-libinfo-command-hook nil)
				'hide))))

(defun idlwave-libinfo-command-hook (&optional arg)
  ;; Command hook used by `idlwave-create-libinfo-file'.
  (if arg
      ;; Scan immediately
      (idlwave-scan-lib-files
       idlwave-sys-dir
       idlwave-path-alist)
    ;; Display the widget
    (let* ((rpl (idlwave-shell-path-filter))
	   (sysdir (car rpl))
	   (dirs (cdr rpl)))
      (idlwave-display-libinfo-widget 
       sysdir dirs
       (delq nil (mapcar (lambda (x) (if (cdr x) (car x) nil))
			 idlwave-path-alist))))))

(defconst idlwave-libinfo-widget-help-string 
  "This is the front-end to the creation of IDLWAVE library catalog.
Please select below the directories on IDL's search path from which you
would like to extract routine information, which will be stored in the file

             %s

If this is not the correct file, first set variable `idlwave-libinfo-file'.
Then call this command again.

For writing code, you need to include the directories which contain the
routines you use.  If IDLWAVE should be able to analyse routine shadowing
it is best to scan all directories.

After selecting the directories, choose [Scan & Save] to scan the library
directories and save the routine info.
\n")

(defvar idlwave-widget)
(defvar widget-keymap)
(defun idlwave-display-libinfo-widget (sysdir dirs selected-dirs)
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
			     (widget-get idlwave-widget :sysdir)
			     (widget-get idlwave-widget :path-dirs)
			     (widget-get idlwave-widget :path-dirs)))
		 "Select All")
  (widget-insert "  ")
  (widget-create 'push-button
		 :notify '(lambda (&rest ignore)
			    (idlwave-display-libinfo-widget
			     (widget-get idlwave-widget :sysdir)
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
  (widget-put idlwave-widget :sysdir sysdir)
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
	 (selected-dirs (widget-value widget))
	 (sysdir (widget-get widget :sysdir))
	 (path-dirs (widget-get widget :path-dirs))
	 (path-dir-alist
	  (mapcar (lambda (x) (cons x (if (member x selected-dirs) t nil)))
		  path-dirs)))
    (idlwave-scan-lib-files sysdir path-dir-alist)))

(defvar font-lock-mode)
(defun idlwave-scan-lib-files (sysdir path-alist)
  ;; Scan the files in PATH-ALIST and store the info in a file
  (let* ((idlwave-scanning-lib t)
	 (idlwave-scanning-lib-dir "")
	 (dircnt (1+ (length path-alist)))
	 (idlwave-completion-case nil)
	 dirs-alist dir files file)
    (setq idlwave-library-routines nil)
    (setq idlwave-path-alist path-alist)
    (setq idlwave-true-path-alist nil)
    (setq idlwave-sys-dir sysdir)
    (save-excursion
      (set-buffer (get-buffer-create "*idlwave-scan.pro*"))
      (idlwave-mode)
      (setq dirs-alist (reverse path-alist))
      (while (setq dir (pop dirs-alist))
	(decf dircnt)
	(when (cdr dir)
	  ;; Has the flag of scanned directories
	  (setq dir (car dir))
	  (setq idlwave-scanning-lib-dir (format "{%d}/" dircnt))
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
		))))))
    ;; Sorting is not necessary since we sort each time before a routine
    ;; is used.  So we don't do it here - the catalog file looks nicer
    ;; when it is unsorted.
    ;;(message "Sorting...")
    ;;(setq idlwave-library-routines 
    ;;(sort idlwave-library-routines 'idlwave-routine-entry-compare))
    ;;(message "Sorting...done")
    (message "Creating libinfo file...")
    (kill-buffer "*idlwave-scan.pro*")
    (kill-buffer (get-buffer-create "*IDLWAVE Widget*"))
    (let ((font-lock-maximum-size 0)
	  (auto-mode-alist nil))
      (find-file idlwave-libinfo-file))
    (if (and (boundp 'font-lock-mode)
	     font-lock-mode)
	(font-lock-mode 0))
    (erase-buffer)
    (insert ";; IDLWAVE libinfo file\n")
    (insert (format ";; Created %s\n\n" (current-time-string)))

    ;; Define the variable which knows the value of "!DIR"
    (insert (format "\n(setq idlwave-sys-dir \"%s\")\n"
		    idlwave-sys-dir))

    ;; Define the variable which contains a list of all scanned directories
    (insert "\n(setq idlwave-path-alist\n    '(")
    (mapcar (lambda (x)
	      (insert (format "\n      (\"%s\" . %s)" (car x) (cdr x))))
	    path-alist)
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
  (message "Creating libinfo file...done")
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
;; START OF IDLWAVE SUPPORT ROUTINES
pro idlwave_print_info_entry,name,func=func,separator=sep
  ;; See if it's an object method
  if name eq '' then return
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
  
  ret=(['IDLWAVE-PRO','IDLWAVE-FUN'])(func)
  
  print,ret + ': ' + name + sep + class + sep + source(0).path  $
    + sep + cs + sep + kwstring
end

pro idlwave_routine_info
  sep = '<@>'
  print,'>>>BEGIN OF IDLWAVE ROUTINE INFO (\"' + sep + '\" IS THE SEPARATOR)'
  all = routine_info()
  for i=0,n_elements(all)-1 do $
    idlwave_print_info_entry,all(i),separator=sep
  all = routine_info(/functions)
  for i=0,n_elements(all)-1 do $
    idlwave_print_info_entry,all(i),/func,separator=sep
  print,'>>>END OF IDLWAVE ROUTINE INFO'
end

pro idlwave_get_sysvars
  forward_function strjoin,strtrim,strsplit
  catch,error_status
  if error_status ne 0 then begin
      print, 'Cannot get info about system variables'
  endif else begin
      help,/brief,output=s,/system_variables  ; ? unsafe use of OUTPUT=
      s = strtrim(strjoin(s,' ',/single),2)   ; make one line
      v = strsplit(s,' +',/regex,/extract)    ; get variables
      for i=0,n_elements(v)-1 do begin
          t = ['']                            ; get tag list
          a=execute('if n_tags('+v[i]+') gt 0 then t=tag_names('+v[i]+')')
          print, 'IDLWAVE-SYSVAR: '+v[i]+' '+strjoin(t,' ',/single)
      endfor
  endelse
end

pro idlwave_get_class_tags, class
  res = execute('tags=tag_names({'+class+'})')
  if res then print,'IDLWAVE-CLASS-TAGS: '+class+string(format='(1000(\" \",A))',tags)
end
;; END OF IDLWAVE SUPPORT ROUTINES
" 
  "The idl programs to get info from the shell.")

(defvar idlwave-idlwave_routine_info-compiled nil
  "Remembers if the routine info procedure is already compiled.")

(defvar idlwave-shell-temp-pro-file)
(defvar idlwave-shell-temp-rinfo-save-file)
(defun idlwave-shell-update-routine-info (&optional quiet run-hooks)
  "Query the shell for routine_info of compiled modules and update the lists."
  ;; Save and compile the procedure.  The compiled procedure is then
  ;; saved into an IDL SAVE file, to allow for fast RESTORE.
  ;; We need to RESTORE the procedure each time we use it, since
  ;; the user may have killed or redefined it.  In particluar,
  ;; .RESET_SESSION will kill all user procedures.
  (unless (and idlwave-idlwave_routine_info-compiled
	       (file-readable-p idlwave-shell-temp-rinfo-save-file))
    (save-excursion
      (set-buffer (idlwave-find-file-noselect
		   idlwave-shell-temp-pro-file))
      (erase-buffer)
      (insert idlwave-routine-info.pro)
      (save-buffer 0))
    (idlwave-shell-send-command 
     (concat ".run " idlwave-shell-temp-pro-file)
     nil 'hide)
    (idlwave-shell-send-command
     (format "save,'idlwave_routine_info','idlwave_print_info_entry',FILE='%s',/ROUTINES" 
	     idlwave-shell-temp-rinfo-save-file)
     nil 'hide))

  ;; Restore and execute the procedure, analyze the output
  (idlwave-shell-send-command
   (format "RESTORE, '%s' & idlwave_routine_info"
	   idlwave-shell-temp-rinfo-save-file)
   `(progn
      (idlwave-shell-routine-info-filter)
      (idlwave-concatenate-rinfo-lists ,quiet ,run-hooks))
   'hide))

;; ---------------------------------------------------------------------------
;;
;; Completion and displaying routine calling sequences

(defvar idlwave-completion-help-info nil)
(defvar idlwave-current-obj_new-class nil)
(defvar idlwave-complete-special nil)

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

     ;; Check for any special completion functions
     ((and idlwave-complete-special
	   (idlwave-complete-special)))

     ((and (idlwave-in-quote)
	   (not (eq what 'class)))
      (idlwave-complete-filename))

     ((null what)
      (error "Nothing to complete here"))

     ((eq what 'class)
      (setq idlwave-completion-help-info '(class))
      (idlwave-complete-class))

     ((eq what 'procedure)
      ;; Complete a procedure name
      (let* ((class-selector (idlwave-determine-class (nth 3 where-list) 'pro))
	     (super-classes (idlwave-all-class-inherits class-selector))
	     (isa (concat "procedure" (if class-selector "-method" "")))
	     (type-selector 'pro))
	(setq idlwave-completion-help-info 
	      (list 'routine nil type-selector class-selector nil super-classes))
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
	     (super-classes (idlwave-all-class-inherits class-selector))
	     (isa (concat "function" (if class-selector "-method" "")))
	     (type-selector 'fun))
	(setq idlwave-completion-help-info 
	      (list 'routine nil type-selector class-selector nil super-classes))
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
	     (super-classes (idlwave-all-class-inherits class-selector))
	     (isa (format "procedure%s-keyword" (if class "-method" "")))
	     (entry (idlwave-best-rinfo-assq
		     name 'pro class (idlwave-routines)))
	     (list (nth 5 entry)))
	(unless (or entry (eq class t))
	  (error "Nothing known about procedure %s"
		 (idlwave-make-full-name class name)))
	(setq list (idlwave-fix-keywords name 'pro class list))
	(unless list (error (format "No keywords available for procedure %s"
				    (idlwave-make-full-name class name))))
	(setq idlwave-completion-help-info 
	      (list 'keyword name type-selector class-selector nil super-classes))
	(idlwave-complete-in-buffer
	 'keyword 'keyword list nil
	 (format "Select keyword for procedure %s%s"
		 (idlwave-make-full-name class name)
		 (if (or (member '("_EXTRA") list)
			 (member '("_REF_EXTRA") list))			 
		     " (note _EXTRA)" ""))
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
	     (super-classes (idlwave-all-class-inherits class-selector))
	     (isa (format "function%s-keyword" (if class "-method" "")))
	     (entry (idlwave-best-rinfo-assq
		     name 'fun class (idlwave-routines)))
	     (list (nth 5 entry))
	     msg-name)
	(unless (or entry (eq class t))
	  (error "Nothing known about function %s"
		 (idlwave-make-full-name class name)))
	(setq list (idlwave-fix-keywords name 'fun class list))
	;; OBJ_NEW: Messages mention the proper Init method
	(setq msg-name (if (and (null class)
				(string= (upcase name) "OBJ_NEW"))
			   (concat idlwave-current-obj_new-class
				   "::Init (via OBJ_NEW)")
			 (idlwave-make-full-name class name)))
	(unless list (error (format "No keywords available for function %s"
				    msg-name)))
	(setq idlwave-completion-help-info 
	      (list 'keyword name type-selector class-selector nil super-classes))
	(idlwave-complete-in-buffer
	 'keyword 'keyword list nil
	 (format "Select keyword for function %s%s" msg-name
		 (if (or (member '("_EXTRA") list)
			 (member '("_REF_EXTRA") list))			 
		     " (note _EXTRA)" ""))
	 isa
	 'idlwave-attach-keyword-classes)))

     (t (error "This should not happen (idlwave-complete)")))))

(defvar idlwave-complete-special nil
  "List of special completion functions.
These functions are called for each completion.  Each function must check
if its own special completion context is present.  If yes, it should
use `idlwave-complete-in-buffer' to do some completion and return `t'.
If such a function returns `t', *no further* attempts to complete
other contexts will be done.  If the function returns `nil', other completions
will be tried.")
(defun idlwave-complete-special ()
  (let ((functions idlwave-complete-special)
	fun)
    (catch 'exit
      (while (setq fun (pop functions))
	(if (funcall fun)
	    (throw 'exit t)))
      nil)))

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
	     (super-classes nil)
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
	     (super-classes nil)
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
	     (super-classes (idlwave-all-class-inherits class-selector))
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
	     (super-classes (idlwave-all-class-inherits class-selector))
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

(defvar idlwave-shell-default-directory)
(defun idlwave-complete-filename ()
  "Use the comint stuff to complete a file name."
  (require 'comint)
  (let* ((comint-file-name-chars "~/A-Za-z0-9+@:_.$#%={}\\-")
	 (comint-completion-addsuffix nil)
	 (default-directory
	   (if (and (boundp 'idlwave-shell-default-directory)
		    (stringp idlwave-shell-default-directory)
		    (file-directory-p idlwave-shell-default-directory))
	       idlwave-shell-default-directory
	     default-directory)))	   
    (comint-dynamic-complete-filename)))

(defun idlwave-make-full-name (class name)
  ;; Make a fully qualified module name including the class name
  (concat (if class (format "%s::" class) "") name))

(defun idlwave-rinfo-assoc (name type class list)
  "Like `idlwave-rinfo-assq', but sintern strings first."
  (idlwave-rinfo-assq 
   (idlwave-sintern-routine-or-method name class)
   type (idlwave-sintern-class class) list))

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

(defun idlwave-rinfo-assq-any-class (name type class list)
  (let* ((classes (cons class (idlwave-all-class-inherits class)))
	 class rtn)
    (while classes
      (if (setq rtn (idlwave-rinfo-assq name type (pop classes) list))
	  (setq classes nil)))
    rtn))

(defun idlwave-best-rinfo-assq (name type class list)
  "Like `idlwave-rinfo-assq', but get all twins and sort, then return first."
  (let ((twins (idlwave-routine-twins
		(idlwave-rinfo-assq-any-class name type class list)
		list))
	syslibp)
    (when (> (length twins) 1)
      (setq twins (sort twins 'idlwave-routine-entry-compare-twins))
      (if (and (eq 'system (car (nth 3 (car twins))))
	       (setq syslibp (idlwave-any-syslib (cdr twins)))
	       (not (equal 1 syslibp)))
	  ;; Its a syslib, so we need to remove the system entry
	  (setq twins (cdr twins))))
    (car twins)))

(defun idlwave-best-rinfo-assoc (name type class list)
  "Like `idlwave-best-rinfo-assq', but sintern strings first."
  (idlwave-best-rinfo-assq
   (idlwave-sintern-routine-or-method name class)
   type (idlwave-sintern-class class) list))

(defun idlwave-any-syslib (entries)
  "Does the entry list ENTRIES contain a syslib entry?
If yes, return the index (>=1)."
  (let (file (cnt 0))
    (catch 'exit
      (while entries
	(incf cnt)
	(setq file (cdr (nth 3 (car entries))))
	(if (and file
		 (idlwave-syslib-p
		  (idlwave-expand-lib-file-name file)))
	    (throw 'exit cnt)
	  (setq entries (cdr entries))))
      nil)))

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
      (mapcar 'car (idlwave-class-alist))
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

(defun idlwave-members-only (list club)
  "Return list of all elements in LIST which are also in CLUB."
  (let (rtn)
    (while list
      (if (member (car list) club)
	  (setq rtn (cons (car list) rtn)))
      (setq list (cdr list)))
    (nreverse rtn)))

(defun idlwave-nonmembers-only (list club)
  "Return list of all elements in LIST which are not in CLUB."
  (let (rtn)
    (while list
      (if (member (car list) club)
	  nil
	(setq rtn (cons (car list) rtn)))
      (setq list (cdr list)))
    (nreverse rtn)))

(defun idlwave-determine-class (info type)
  ;; Determine the class of a routine call.  INFO is the structure returned
  ;; `idlwave-what-function' or `idlwave-what-procedure'.
  ;; The third element in this structure is the class.  When nil, we return nil.
  ;; When t, try to get the class from text properties at the arrow.  When
  ;; the object is "self", we use the class of the current routine.
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
	 (is-self 
	  (and arrow
	       (save-excursion (goto-char apos)
			       (forward-word -1)
			       (let ((case-fold-search t))
				 (looking-at "self\\>")))))
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
      (when (and (eq t class)
		 is-self)
	(setq class (or (nth 2 (idlwave-current-routine)) class)))
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
(defvar super-classes)
(defun idlwave-selector (a)
  (and (eq (nth 1 a) type-selector)
       (or (and (nth 2 a) (eq class-selector t))
	   (eq (nth 2 a) class-selector)
	   (memq (nth 2 a) super-classes)
	   )))

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
  (let* (;(bos (save-excursion (idlwave-beginning-of-statement) (point)))
         (bos (save-excursion (idlwave-start-of-substatement 'pre) (point)))
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
     ((string-match "\\`[ \t]*\\(pro\\|function\\)[ \t]+[a-zA-Z0-9_]*\\'"
                    (buffer-substring bos (point)))
      (setq cw 'class))
     ((string-match 
       "\\`[ \t]*\\([a-zA-Z][a-zA-Z0-9$_]*\\)?\\'" 
       (buffer-substring (if (> pro-point 0) pro-point bos) (point)))
      (setq cw 'procedure cw-class pro-class cw-point pro-point
	    cw-arrow pro-arrow))
     ((string-match "\\`[ \t]*\\(pro\\|function\\)\\>"
		    (buffer-substring bos (point)))
      nil)
     ((string-match "OBJ_NEW([ \t]*['\"]\\([a-zA-Z0-9$_]*\\)?\\'"
		    (buffer-substring bos (point)))
      (setq cw 'class))                    
     ((string-match "\\<inherits\\s-+\\([a-zA-Z0-9$_]*\\)?\\'"
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

(defun idlwave-what-function (&optional bound)
  ;; Find out if point is within the argument list of a function.
  ;; The return value is ("function-name" (point) level).
  ;; Level is 1 on the to level parenthesis, higher further down.

  ;; If the optional BOUND is an integer, bound backwards directed
  ;;    searches to this point.

  (catch 'exit
    (let (pos 
	  func-point
	  (cnt 0)
	  func arrow-start class)
      (idlwave-with-special-syntax
       (save-restriction
	 (save-excursion
	   (narrow-to-region (max 1 (or bound 0)) (point-max))
	   ;; move back out of the current parenthesis
	   (while (condition-case nil
		      (progn (up-list -1) t)
		    (error nil))
	     (setq pos (point))
	     (incf cnt)
	     (when (and (= (following-char) ?\()
			(re-search-backward 
			 "\\(::\\|\\<\\)\\([a-zA-Z][a-zA-Z0-9$_]*\\)[ \t]*\\="
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
	   (throw 'exit nil)))))))

(defun idlwave-what-procedure (&optional bound)
  ;; Find out if point is within the argument list of a procedure.
  ;; The return value is ("procedure-name" class arrow-pos (point)).

  ;; If the optional BOUND is an integer, bound backwards directed
  ;;    searches to this point.
  (let ((pos (point)) pro-point
	pro class arrow-start string)
    (save-excursion 
      ;;(idlwave-beginning-of-statement)
      (idlwave-start-of-substatement 'pre)
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
	     ;; "complete" means, this is already a valid completion
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
  ;; Check for the special case of completing empty string after pro/function
  (if (let ((case-fold-search t))
	(save-excursion
	  (and
	   (re-search-backward "\\<\\(pro\\|function\\)[ \t]+\\="
			       (- (point) 15) t)
	   (goto-char (point-min))
	   (re-search-forward 
	    "^[ \t]*\\(pro\\|function\\)[ \t]+\\([a-zA-Z0-9_]+::\\)" nil t))))
      ;; Yank the full class specification
      (insert (match-string 2))
    ;; Do the completion
    (idlwave-complete-in-buffer 'class 'class (idlwave-class-alist) nil 
				"Select a class" "class")))

(defun idlwave-attach-classes (list is-kwd show-classes)
  ;; Attach the proper class list to a LIST of completion items.
  ;; IS-KWD, when non-nil, shows its keywords - otherwise its methods
  ;; SHOW-CLASSES is the value of `idlwave-completion-show-classes'.
  (catch 'exit
    (if (or (null show-classes)           ; don't want to see classes
	    (null class-selector)         ; not a method call
	    (and (stringp class-selector) ; the class is already known
		 (not super-classes)))    ; no possibilities for inheritance
	;; In these cases, we do not have to do anything
	(throw 'exit list))
    
    (let* ((do-prop (and (>= show-classes 0)
			 (>= emacs-major-version 21)))
	   (do-buf (not (= show-classes 0)))
	   ; (do-dots (featurep 'xemacs))
	   (do-dots t)
	   (inherit (if super-classes
			(cons class-selector super-classes)))
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
	 (if inherit
	     (setq classes 
		   (delq nil
			 (mapcar (lambda (x) (if (memq x inherit) x nil))
				 classes))))
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

(defvar idlwave-completion-setup-hook nil)

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
      (idlwave-display-completion-list-xemacs 
       list)
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

  ;; Run the hook
  (run-hooks 'idlwave-completion-setup-hook)

  ;; Display the message
  (message (or message "Making completion list...done")))

(defun idlwave-choose (function &rest args)
  "Call FUNCTION as a completion chooser and pass ARGS to it."
  (let ((completion-ignore-case t))	    ; install correct value
    (apply function args))
  (if (and (eq major-mode 'idlwave-shell-mode)
	   (boundp 'font-lock-mode)
	   (not font-lock-mode))
      ;; Remove the fontification of the word before point
      (let ((beg (save-excursion
		   (skip-chars-backward "a-zA-Z0-9_")
		   (point))))
	(remove-text-properties beg (point) '(face nil))))
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

;; In XEmacs, we can use :activate-callback directly to advice the
;; choose functions.  We use the private keymap only for the online
;; help feature.

(defvar idlwave-completion-map nil
  "Keymap for completion-list-mode with idlwave-complete.")

(defun idlwave-display-completion-list-xemacs (list &rest cl-args)
  (with-output-to-temp-buffer "*Completions*"
    (apply 'display-completion-list list
	   ':activate-callback 'idlwave-default-choose-completion
	   cl-args))
  (save-excursion
    (set-buffer "*Completions*")
    (use-local-map
     (or idlwave-completion-map
	 (setq idlwave-completion-map
	       (idlwave-make-modified-completion-map-xemacs
		(current-local-map)))))))

(defun idlwave-default-choose-completion (&rest args)
  "Execute `default-choose-completion' and then restore the win-conf."
  (apply 'idlwave-choose 'default-choose-completion args))

(defun idlwave-make-modified-completion-map-xemacs (old-map)
  "Replace `choose-completion' and `mouse-choose-completion' in OLD-MAP."
  (let ((new-map (copy-keymap old-map)))
    (define-key new-map [button3up] 'idlwave-mouse-completion-help)
    (define-key new-map [button3] (lambda ()
				    (interactive)
				    (setq this-command last-command)))
    new-map))

;; In Emacs we also to replace choose keybindings in the completion
;; map in order to install our wrappers.

(defun idlwave-display-completion-list-emacs (list)
  "Display completion list and install the choose wrappers."
  (with-output-to-temp-buffer "*Completions*"
    (display-completion-list list))
  (save-excursion
    (set-buffer "*Completions*")
    (use-local-map
     (or idlwave-completion-map
	 (setq idlwave-completion-map
	       (idlwave-make-modified-completion-map-emacs
		(current-local-map)))))))

(defun idlwave-make-modified-completion-map-emacs (old-map)
  "Replace `choose-completion' and `mouse-choose-completion' in OLD-MAP."
  (let ((new-map (copy-keymap old-map)))
    (substitute-key-definition 
     'choose-completion 'idlwave-choose-completion new-map)
    (substitute-key-definition
     'mouse-choose-completion 'idlwave-mouse-choose-completion new-map)
    (define-key new-map [mouse-3] 'idlwave-mouse-completion-help)
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

;;; ------------------------------------------------------------------------
;;; Sturucture parsing code, and code to manage class info

;;
;; - Go again over the documentation how to write a completion
;;   plugin.  It is in self.el, but currently still very bad.
;;   This could be in a separate file in the distribution, or 
;;   in an appendix for the manual.  

(defun idlwave-struct-tags ()
  "Return a list of all tags in the structure defined at point.
Point is expected just before the opening `{' of the struct definition."
  (save-excursion
    (let* ((borders (idlwave-struct-borders))
	   (beg (car borders))
	   (end (cdr borders))
	   tags)
      (goto-char beg)
      (while (re-search-forward "[{,][ \t]*\\(\\$.*\n[ \t]*\\)?\\([a-zA-Z][a-zA-Z0-9_]*\\)[ \t]*:" end t)
	;; Check if we are still on the top level of the structure.
	(if (and (condition-case nil (progn (up-list -1) t) (error nil))
		 (= (point) beg))
	    (push (match-string 2) tags))
	(goto-char (match-end 0)))
      (nreverse tags))))

(defun idlwave-struct-inherits ()
  "Return a list of all `inherits' names in the struct at point.
Point is expected just before the opening `{' of the struct definition."
  (save-excursion
    (let* ((borders (idlwave-struct-borders))
	   (beg (car borders))
	   (end (cdr borders))
	   (case-fold-search t)
	   names)
      (goto-char beg)
      (while (re-search-forward "[{,][ \t]*\\(\\$.*\n[ \t]*\\)?inherits[ \t]*\\(\\$.*\n[ \t]*\\)?\\([a-zA-Z][a-zA-Z0-9_]*\\)" end t)
	;; Check if we are still on the top level of the structure.
	(if (and (condition-case nil (progn (up-list -1) t) (error nil))
		 (= (point) beg))
	    (push (match-string 3) names))
	(goto-char (match-end 0)))
      (nreverse names))))


(defun idlwave-struct-borders ()
  "Return the borders of the {...} after point as a cons cell."
  (let (beg)
    (save-excursion
      (skip-chars-forward "^{")
      (setq beg (point))
      (condition-case nil (forward-list 1)
	(error (goto-char beg)))
      (cons beg (point)))))

(defun idlwave-find-structure-definition (&optional var name bound)
  "Search forward for a structure definition.
If VAR is non-nil, search for a structure assigned to variable VAR.
If NAME is non-nil, search for a named structure NAME.
If BOUND is an integer, limit the search.
If BOUND is the symbol `all', we search first back and then forward
through the entire file."
  (let* ((ws "[ \t]*\\(\\$.*\n[ \t]*\\)?")
	 (case-fold-search t)
	 (lim (if (integerp bound) bound nil))
	 (re (concat
	      (if var
		  (concat "\\<" (regexp-quote (downcase var)) "\\>" ws)
		"\\(\\)")
	      "=" ws "\\({\\)"
	      (if name (concat ws "\\<" (downcase name) "[^a-zA-Z0-9_$]") ""))))
    (if (or (and (eq bound 'all)
		 (re-search-backward re nil t))
	    (re-search-forward re lim t))
	(goto-char (match-beginning 3)))))

(defvar idlwave-class-info nil)
(defvar idlwave-system-class-info nil)
(add-hook 'idlwave-update-rinfo-hook
	  (lambda () (setq idlwave-class-info nil)))
(add-hook 'idlwave-after-load-rinfo-hook
	  (lambda () (setq idlwave-class-info nil)))

(defun idlwave-class-info (class)
  (let (list entry)
    (unless idlwave-class-info
      ;; Info is nil, put in the system stuff.
      (setq idlwave-class-info idlwave-system-class-info)
      (setq list idlwave-class-info)
      (while (setq entry (pop list))
	(idlwave-sintern-class-info entry)))
    (setq class (idlwave-sintern-class class))
    (setq entry (assq class idlwave-class-info))
    (unless entry
      (setq entry (idlwave-find-class-info class))
      (when entry
	;; Sintern and cache the info
	(idlwave-sintern-class-info entry)
	(push entry idlwave-class-info)))
    entry))

(defun idlwave-sintern-class-info (entry)
  "Sintern the class names in a class-info entry."
  (let ((taglist (assq 'tags entry))
	(inherits (assq 'inherits entry)))
    (setcar entry (idlwave-sintern-class (car entry) 'set))
    (if inherits
	(setcdr inherits (mapcar (lambda (x) (idlwave-sintern-class x 'set))
				 (cdr inherits))))))

(defun idlwave-find-class-info (class)
  "Find the __define procedure for a class structure and return info entry."
  (let* ((pro (concat (downcase class) "__define"))
	 (class (idlwave-sintern-class class))
	 (idlwave-auto-routine-info-updates nil)
	 (file (cdr (nth 3 (idlwave-rinfo-assoc pro 'pro nil 
						(idlwave-routines)))))
	 buf)
    (if (or (not file)
	    (not (file-regular-p 
		  (setq file (idlwave-expand-lib-file-name file)))))
	nil ; Cannot get info
      (save-excursion
	(if (setq buf (idlwave-get-buffer-visiting file))
	    (set-buffer buf)
	  (set-buffer (get-buffer-create " *IDLWAVE-tmp*"))
	  (erase-buffer)
	  (unless (eq major-mode 'idlwave-mode)
	    (idlwave-mode))
	  (insert-file-contents file))
	(save-excursion
	  (goto-char 1)
	  (setq case-fold-search t)
	  (when (and (re-search-forward
		      (concat "^[ \t]*pro[ \t]+" pro "\\>") nil t)
		     ;; FIXME: should we limit to end of pro here?
		     (idlwave-find-structure-definition nil class))
	    (list class
		  (cons 'tags (idlwave-struct-tags))
		  (cons 'inherits (idlwave-struct-inherits)))))))))

(defun idlwave-class-tags (class)
  "Return the native tags in CLASS."
  (cdr (assq 'tags (idlwave-class-info class))))
(defun idlwave-class-inherits (class)
  "Return the direct superclasses of CLASS."
  (cdr (assq 'inherits (idlwave-class-info class))))

(defun idlwave-all-class-tags (class)
  "Return a list of native and inherited tags in CLASS."
  (apply 'append (mapcar 'idlwave-class-tags
			 (cons class (idlwave-all-class-inherits class)))))

(defun idlwave-all-class-inherits (class)
  "Return a list of all superclasses of CLASS (recursively expanded).
The list is cashed in `idlwave-class-info' for faster access."
  (cond
   ((not idlwave-support-inheritance) nil)
   ((eq class nil) nil)
   ((eq class t) nil)
   (t
    (let ((info (idlwave-class-info class))
	  entry)
      (if (setq entry (assq 'all-inherits info))
	  (cdr entry)
	(let ((inherits (idlwave-class-inherits class))
	      rtn all-inherits cl)
	  (while inherits
	    (setq cl (pop inherits)
		  rtn (cons cl rtn)
		  inherits (append inherits (idlwave-class-inherits cl))))
	  (setq all-inherits (nreverse rtn))
	  (nconc info (list (cons 'all-inherits all-inherits)))
	  all-inherits))))))


;;==========================================================================
;;
;; Completing class structure tags.  This is a completion plugin.
;; The necessary taglist is constructed dynamically

(defvar idlwave-current-tags-class nil)
(defvar idlwave-current-class-tags nil)
(defvar idlwave-current-native-class-tags nil)
(defvar idlwave-sint-classtags nil)
(idlwave-new-sintern-type 'classtag)
(add-to-list 'idlwave-complete-special 'idlwave-complete-class-structure-tag)
(add-hook 'idlwave-update-rinfo-hook 'idlwave-classtag-reset)

(defun idlwave-complete-class-structure-tag ()
  "Complete a structure tag on a `self' argument in an object method."
  (interactive)
  (let ((pos (point))
	(case-fold-search t))
    (if (save-excursion
	  ;; Check if the context is right
	  (skip-chars-backward "[a-zA-Z0-9._$]")
	  (and (< (point) (- pos 4))
	       (looking-at "self\\.")))
	(let* ((class (nth 2 (idlwave-current-routine))))
	  ;; Check if we are in a class routine
	  (unless class
	    (error "Not in a method procedure or function"))
	  ;; Check if we need to update the "current" class
	  (if (not (equal class idlwave-current-tags-class))
	      (idlwave-prepare-class-tag-completion class))
	  (setq idlwave-completion-help-info nil)
	  (let  ((idlwave-cpl-bold idlwave-current-native-class-tags))
	    (idlwave-complete-in-buffer
	     'classtag 'classtag 
	     idlwave-current-class-tags nil
	     (format "Select a tag of class %s" class)
	     "class tag"))
	  t) ; return t to skip other completions
      nil)))

(defun idlwave-classtag-reset ()
  (setq idlwave-current-tags-class nil))

(defun idlwave-prepare-class-tag-completion (class)
  "Find and parse the necessary class definitions for class structure tags."
  (setq idlwave-sint-classtags nil)
  (setq idlwave-current-tags-class class)
  (setq idlwave-current-class-tags
	(mapcar (lambda (x)
		  (list (idlwave-sintern-classtag x 'set)))
		(idlwave-all-class-tags class)))
  (setq idlwave-current-native-class-tags
	(mapcar 'downcase (idlwave-class-tags class))))

;===========================================================================
;;
;; Completing system variables and their structure fields
;; This is also a plugin.  It is a bit bigger since we support loading
;; current system variables from the shell and highlighting in the
;; completions buffer. 

(defvar idlwave-sint-sysvars nil)
(defvar idlwave-sint-sysvartags nil)
(idlwave-new-sintern-type 'sysvar)
(idlwave-new-sintern-type 'sysvartag)
(add-to-list 'idlwave-complete-special 'idlwave-complete-sysvar-or-tag)
(add-hook 'idlwave-update-rinfo-hook 'idlwave-sysvars-reset)
(add-hook 'idlwave-after-load-rinfo-hook 'idlwave-remember-builtin-sysvars)
(add-hook 'idlwave-after-load-rinfo-hook 'idlwave-sintern-sysvar-alist)

(defvar idlwave-system-variables-alist nil
  "Alist of system variables and the associated structure tags.
Gets set in `idlw-rinfo.el'.")
(defvar idlwave-builtin-system-variables nil)

(defun idlwave-complete-sysvar-or-tag ()
  "Complete a system variable."
  (interactive)
  (let ((pos (point))
	(case-fold-search t))
    (cond ((save-excursion
	     ;; Check if the context is right for system variable
	     (skip-chars-backward "[a-zA-Z0-9_$]")
	     (equal (char-before) ?!))
	   (setq idlwave-completion-help-info '(idlwave-complete-sysvar-help))
	   (idlwave-complete-in-buffer 'sysvar 'sysvar 
				       idlwave-system-variables-alist nil
				       "Select a system variable"
				       "system variable")
	   t)  ; return t to skip other completions
	  ((save-excursion
	     ;; Check if the context is right for sysvar tag
	     (skip-chars-backward "[a-zA-Z0-9_$.]")
	     (and (equal (char-before) ?!)
		  (looking-at "\\([a-zA-Z][a-zA-Z0-9_$]*\\)\\.")
		  (<= (match-end 0) pos)))
	   ;; Complete a system variable tag
	   (let* ((var (idlwave-sintern-sysvar (match-string 1)))
		  (entry (assq var idlwave-system-variables-alist))
		  (tags (cdr entry)))
	     (or entry (error "!%s is not know to be a system variable" var))
	     (or tags (error "System variable !%s is not a structure" var))
	     (setq idlwave-completion-help-info
		   (list 'idlwave-complete-sysvar-help var))
	     (idlwave-complete-in-buffer 'sysvartag 'sysvartag 
					 tags nil
					 "Select a system variable tag"
					 "system variable tag")
	     t)) ; return t to skip other completions
	  (t nil))))

(defvar name) 
(defvar kwd)
(defun idlwave-complete-sysvar-help (mode word)
  (cond
   ((eq mode 'test)
    (or (and (eq nil (nth 1 idlwave-completion-help-info))
	     (member (downcase word) idlwave-builtin-system-variables))
	(and (stringp (nth 1 idlwave-completion-help-info))
	     (member (downcase (nth 1 idlwave-completion-help-info))
		     idlwave-builtin-system-variables))))
   ((eq mode 'set)
    (setq name "system variables"
	  kwd (concat "!"
		      (if (stringp (nth 1 idlwave-completion-help-info))
			  (nth 1 idlwave-completion-help-info)
			word))))
   (t (error "This should not happen"))))
			  

(defun idlwave-sysvars-reset ()
  (if (and (fboundp 'idlwave-shell-is-running)
	   (idlwave-shell-is-running))
      (idlwave-shell-send-command "idlwave_get_sysvars"
				  'idlwave-process-sysvars 'hide)))

(defun idlwave-process-sysvars ()
  (idlwave-shell-filter-sysvars)
  (setq idlwave-sint-sysvars nil
	idlwave-sint-sysvartags nil)
  (idlwave-sintern-sysvar-alist))

(defun idlwave-remember-builtin-sysvars ()
  (setq idlwave-builtin-system-variables
	(mapcar 'downcase 
		(mapcar 'car idlwave-system-variables-alist))))

(defun idlwave-sintern-sysvar-alist ()
  (let ((list idlwave-system-variables-alist) entry)
    (while (setq entry (pop list))
      (setcar entry (idlwave-sintern-sysvar (car entry) 'set))
      (setcdr entry (mapcar (lambda (x) 
			      (list (idlwave-sintern-sysvartag (car x) 'set)))
			    (cdr entry))))))

(defvar idlwave-shell-command-output)
(defun idlwave-shell-filter-sysvars ()
  "Get the system variables and structure tags."
  (let ((text idlwave-shell-command-output)
	(start 0)
	(old idlwave-system-variables-alist)
	var tags type name class)
    (setq idlwave-system-variables-alist nil)
    (while (string-match "^IDLWAVE-SYSVAR: !\\([a-zA-Z0-9_$]+\\)\\( \\(.*\\)\\)?"
			 text start)
      (setq start (match-end 0)
	    var (match-string 1 text)
	    tags (if (match-end 3) (idlwave-split-string (match-string 3 text))))
      (setq idlwave-system-variables-alist
	    (cons (cons var (mapcar 'list tags))
		  idlwave-system-variables-alist)))
    ;; Keep the old value if query was not successful
    (setq idlwave-system-variables-alist
	  (or idlwave-system-variables-alist old))))

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

(defun idlwave-mouse-context-help (ev &optional arg)
  "Call `idlwave-context-help' on the clicked location."
  (interactive "eP")
  (mouse-set-point ev)
  (idlwave-context-help arg))

(defvar idlwave-last-context-help-pos nil)
(defun idlwave-context-help (&optional arg)
  "Display IDL Online Help on context.
If point is on a keyword, help for that keyword will be shown.
If point is on a routine name or in the argument list of a routine,
help for that routine will be displayed.
Works for system routines and keywords only."
  (interactive "P")
  (idlwave-require-online-help)
  (idlwave-do-context-help arg))

(defun idlwave-mouse-completion-help (ev)
  "Display online help about the completion at point."
  (interactive "eP")
  (idlwave-require-online-help)
  ;; Restore last-command for next command, to make scrolling of completions
  ;; work.
  (setq this-command last-command)
  (idlwave-do-mouse-completion-help ev))
  

(defvar idlwave-help-is-loaded nil
  "Is online help avaiable?")
;; The following variables will be defined by `idlw-help.el'.
(defvar idlwave-help-frame-width nil)
(defvar idlwave-help-file nil)
(defvar idlwave-help-topics nil)

(defun idlwave-help-directory ()
  "Return the help directory, or nil if that is not known."
  (or (and (stringp idlwave-help-directory)
	   (> (length idlwave-help-directory) 0)
	   idlwave-help-directory)
      (getenv "IDLWAVE_HELP_DIRECTORY")))

(defun idlwave-require-online-help ()
  (if idlwave-help-is-loaded
      t  ;; everything is OK.
    (let* ((dir (or (idlwave-help-directory)
		    (error "Online Help is not installed (idlwave-help-directory is unknown)")))
	   (lfile1 (expand-file-name "idlw-help.elc" dir))
	   (lfile2 (expand-file-name "idlw-help.el" dir))
	   (hfile (expand-file-name "idlw-help.txt" dir)))
      (if (or (and (file-regular-p lfile1) (load-file lfile1))
	      (and (file-regular-p lfile2) (load-file lfile2)))
	  (progn 
	    (if (and idlwave-help-frame-parameters
		     (not (assoc 'width idlwave-help-frame-parameters)))
		(push (cons 'width idlwave-help-frame-width)
		      idlwave-help-frame-parameters))
	    (or idlwave-help-topics
		(error "File `%s' in help dir `%s' does not define `idlwave-help-topics'" 
			 "idlw-help.el" dir)))
	(error "No such file `%s' in help dir `%s'" "idlw-help.el" dir))
      (if (file-regular-p hfile)
	  (setq idlwave-help-is-loaded t
		idlwave-help-file hfile)
	(error "No such file `%s' in dir `%s'" "idlw-help.txt" dir)))))

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
      (if (car module)
	  (apply 'idlwave-display-calling-sequence
		 (idlwave-fix-module-if-obj_new module))
	(error "Don't know which calling sequence to show")))))

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
	 (module (idlwave-fix-module-if-obj_new (idlwave-what-module)))
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

(defun idlwave-do-find-module (name type class &optional force-source)
  (let ((name1 (idlwave-make-full-name class name))
	source buf1 entry
	(buf (current-buffer))
	(pos (point))
	name2)
    (setq entry (idlwave-best-rinfo-assq name type class (idlwave-routines))
	  source (or force-source (nth 3 entry))
	  name2 (if (nth 2 entry)
		    (idlwave-make-full-name (nth 2 entry) name)
		  name1))		  
    (cond
     ((or (null name) (equal name ""))
      (error "Abort"))
     ((null entry)
      (error "Nothing known about a module %s" name2))
     ((eq (car source) 'system)
      (error "Source code for system routine %s is not available" 
	     name2))
     ((equal (cdr source) "")
      (error "Source code for routine %s is not available"
	     name2))
     ((memq (car source) '(buffer lib compiled))
      (setq buf1 
	    (if (eq (car source) 'lib)
		(idlwave-find-file-noselect
		 (idlwave-expand-lib-file-name
		  (or (cdr source)
		      (format "%s.pro" (downcase name)))) 'find)
	      (idlwave-find-file-noselect (cdr source) 'find)))
      (pop-to-buffer buf1 t)
      (goto-char (point-max))
      (let ((case-fold-search t))
	(if (re-search-backward
	     (concat "^[ \t]*\\<"
		     (cond ((equal type "f") "function")
			   ((equal type "p") "pro")
			   (t "\\(pro\\|function\\)"))
		     "\\>[ \t]+" 
		     (regexp-quote (downcase name2))
		     "[^a-zA-Z0-9_$]")
	     nil t)
	    (goto-char (match-beginning 0))
	  (pop-to-buffer buf)
	  (goto-char pos)
	  (error "Could not find routine %s" name2)))))))

(defun idlwave-what-module ()
  "Return a default module for stuff near point.
Used by `idlwave-routine-info' and `idlwave-find-module'."
  (idlwave-routines)
  (if (let ((case-fold-search t))
	(save-excursion
	  (idlwave-beginning-of-statement)
	  (looking-at "[ \t]*\\(pro\\|function\\)[ \t]+\\(\\([a-zA-Z0-9_$]+\\)::\\)?\\([a-zA-Z0-9$_]+\\)\\([, \t\n]\\|$\\)")))
      ;; This is a function or procedure definition statement
      ;; We return the defined routine as module.
      (list
       (idlwave-sintern-routine-or-method (match-string 4)
					  (match-string 2))
       (if (equal (downcase (match-string 1)) "pro") 'pro 'fun)
       (idlwave-sintern-class (match-string 3)))

    ;; Not a definition statement - analyze precise positon.
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
	     (or (eq next-char ?\()	; exclude arrays, vars.
		 (looking-at "[a-zA-Z0-9_]*[ \t]*(")))
	(setq this-word (idlwave-sintern-routine-or-method 
			 this-word (nth 2 (nth 3 where))))
	(list this-word 'fun
	      (idlwave-determine-class
	       (cons this-word (cdr (nth 3 where)))
	       'fun)))
       ((and (memq cw '(function-keyword procedure-keyword))
	     (not (equal this-word ""))
	     (eq next-char ?\())	; A function!
	(setq this-word (idlwave-sintern-routine this-word))
	(list this-word 'fun nil))
       (func
	(list func 'fun (idlwave-determine-class (nth 1 where) 'fun)))
       (pro
	(list pro 'pro (idlwave-determine-class (nth 0 where) 'pro)))
       (t nil)))))

(defun idlwave-what-module-find-class ()
  "Call idlwave-what-module and find the inherited class if necessary."
  (let* ((module (idlwave-what-module))
	 (class (nth 2 module))
	 classes)
    (if (and (= (length module) 3)
	     (stringp class))
	(list (car module)
	      (nth 1 module)
	      (apply 'idlwave-find-inherited-class module))
      module)))

(defun idlwave-find-inherited-class (name type class)
  "Find the class which defines TYPE NAME and is CLASS or inherited by CLASS."
  (let ((entry (idlwave-best-rinfo-assoc name type class (idlwave-routines))))
    (if entry
	(nth 2 entry)
      class)))

(defun idlwave-fix-module-if-obj_new (module)
  "Check if MODULE points to obj_new.  If yes, and if the cursor is in the
keyword region, change to the appropriate Init method."
  (let* ((name (car module))
	 (pos (point))
	 (case-fold-search t)
	 string)
    (if (and (stringp name)
	     (equal (downcase name) "obj_new")
	     (save-excursion
	       (idlwave-beginning-of-statement)
	       (setq string (buffer-substring (point) pos))
	       (string-match "obj_new([^'\"]*['\"]\\([a-zA-Z0-9_]+\\)"
			     string)))
	(let ((name "Init")
	      (class (match-string 1 string)))
	  (setq module (list (idlwave-sintern-method "Init")
			     'fun
			     (idlwave-sintern-class class)))))
    module))


(defun idlwave-fix-keywords (name type class keywords)
  ;; This fixes the list of keywords.
  (let ((case-fold-search t)
	name1 type1)

    ;; If this is the OBJ_NEW function, try to figure out the class and use
    ;; the keywords from the corresponding INIT method.
    (if (and (equal name "OBJ_NEW")
	     (or (eq major-mode 'idlwave-mode)
		 (eq major-mode 'idlwave-shell-mode)))
	(let* ((bos (save-excursion (idlwave-beginning-of-statement) (point)))
	       (string (buffer-substring bos (point)))
	       (case-fold-search t)
	       class)
	  (and (string-match "obj_new([^'\"]*['\"]\\([a-zA-Z0-9_]+\\)"
			     string)
	       (setq class (idlwave-sintern-class (match-string 1 string)))
	       (setq idlwave-current-obj_new-class class)
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

    ;; If we have inheritance, add all keywords from superclasses
    ;; :-(  Taken out because JD says it does not work this way.
;    (when (and (stringp class)
;	       (or (assq (idlwave-sintern-keyword "_extra") keywords)
;		   (assq (idlwave-sintern-keyword "_ref_extra") keywords))
;	       (boundp 'super-classes))
;      (loop for x in (idlwave-routines) do
;	(and (nth 2 x)                           ; non-nil class
;	     (or (eq (nth 2 x) class)            ; the right class
;		 (memq (nth 2 x) super-classes)) ; an inherited class
;	     (or (and (eq (nth 1 x) type)        ; default type
;		      (eq (car x) name))         ; default name
;		 (and (eq (nth 1 x) type1)       ; backup type
;		      (eq (car x) name1)))       ; backup name
;	     (mapcar (lambda (k) (add-to-list 'keywords k))
;		     (nth 5 x))))
;      (setq keywords (idlwave-uniquify keywords)))
    
    ;; Return the final list
    keywords))

(defun idlwave-expand-keyword (keyword module)
  "Expand KEYWORD to one of the legal keyword parameters of MODULE.
KEYWORD may be an exact match or an abbreviation of a keyword.
If the match is exact, KEYWORD itself is returned, even if there may be other
keywords of which KEYWORD is an abbreviation.  This is necessary because some
system routines have keywords which are prefixes of other keywords.
If KEYWORD is an abbreviation of several keywords, a list of all possible
completions is returned.
If the abbreviation was unique, the correct keyword is returned.
If it cannot be a keyword, the function return nil.
If we do not know about MODULE, just return KEYWORD literally."
  (let* ((name (car module))
	 (type (nth 1 module))
	 (class (nth 2 module))
	 (kwd (idlwave-sintern-keyword keyword))
	 (entry (idlwave-best-rinfo-assoc name type class (idlwave-routines)))
	 (kwd-alist (nth 5 entry))
	 (extra (or (assq (idlwave-sintern-keyword "_EXTRA") kwd-alist)
		    (assq (idlwave-sintern-keyword "_REF_EXTRA") kwd-alist)))
	 (completion-ignore-case t)
	 candidates)
    (cond ((assq kwd kwd-alist) 
	   kwd)
	  ((setq candidates (all-completions kwd kwd-alist))
	   (if (= (length candidates) 1)
	       (car candidates)
	     candidates))
	  ((and entry extra)
	   ;; Inheritance may cause this keyword to be correct 
	   keyword)
	  (entry
	   ;; We do know the function, which does not have the keyword.
	   nil)
	  (t
	   ;; We do not know the function, so this just might be a correct
	   ;; keyword - return it as it is.
	   keyword))))

(defvar idlwave-rinfo-mouse-map (make-sparse-keymap))
(defvar idlwave-rinfo-map (make-sparse-keymap))
(define-key idlwave-rinfo-mouse-map 
  (if (featurep 'xemacs) [button2] [mouse-2])
  'idlwave-mouse-active-rinfo)
(define-key idlwave-rinfo-mouse-map 
  (if (featurep 'xemacs) [(shift button2)] [(shift mouse-2)])
  'idlwave-mouse-active-rinfo-shift)
(define-key idlwave-rinfo-mouse-map 
  (if (featurep 'xemacs) [button3] [mouse-3])
  'idlwave-mouse-active-rinfo-right)
(define-key idlwave-rinfo-mouse-map " " 'idlwave-active-rinfo-space)
(define-key idlwave-rinfo-map "q" 'idlwave-quit-help)
(define-key idlwave-rinfo-mouse-map "q" 'idlwave-quit-help)
(defvar idlwave-popup-source nil)
(defvar idlwave-rinfo-marker (make-marker))

(defun idlwave-quit-help ()
  (interactive)
  (let ((ri-window (get-buffer-window "*Help*"))
	(olh-window (get-buffer-window "*IDLWAVE Help*")))
    (when (and olh-window
	       (fboundp 'idlwave-help-quit))
      (select-window olh-window)
      (idlwave-help-quit))
    (when (window-live-p ri-window)
      (delete-window ri-window))))

(defun idlwave-display-calling-sequence (name type class
					      &optional initial-class)
  ;; Display the calling sequence of module NAME, type TYPE in class CLASS.
  (let* ((initial-class (or initial-class class))
	 (entry (or (idlwave-best-rinfo-assq name type class
					     (idlwave-routines))
		    (idlwave-rinfo-assq name type class 
					idlwave-unresolved-routines)))
	 (name (or (car entry) name))
	 (class (or (nth 2 entry) class))
	 (superclasses (idlwave-all-class-inherits initial-class))
	 (twins (idlwave-routine-twins entry))
	 (dtwins (idlwave-study-twins twins))
	 (all dtwins)
	 (system (idlwave-rinfo-assq
		  name type class idlwave-system-routines))
	 (have-sysdoc (and system (idlwave-help-directory)))
	 ;; (source (nth 3 entry))
	 (have-olh (and (or system idlwave-extra-help-function)
			(idlwave-help-directory)))
	 (calling-seq (nth 4 entry))
	 (keywords (nth 5 entry))
	 (olh (nth 6 entry))
	 (help-echo-kwd
	  (if have-olh
	      "Button2: Insert KEYWORD (SHIFT=`/KEYWORD')    Button3: Online Help "
	    "Button2: Insert KEYWORD (SHIFT=`/KEYWORD')."))
	 (help-echo-use
	  (if have-olh
	      "Button2/3: Online Help"
	    nil))
	 (help-echo-src
	  (if (idlwave-help-directory)
	      "Button2: Pop to source and back.             Button3: Source in Help window."
	    "Button2: Pop to source and back."))
	 (help-echo-class
	  "Button2: Display info about same method in superclass")
	 (col 0)
	 (data (list name type class (current-buffer) olh initial-class))
	 (km-prop (if (featurep 'xemacs) 'keymap 'local-map))
	 (face 'idlwave-help-link-face)
	 beg props win cnt total)
    (setq keywords (idlwave-fix-keywords name type class keywords))
    (cond
     ((null entry)
      (error "No %s %s known %s" type name
	     (if initial-class (concat "in class " initial-class) "")))
     ((or (null name) (equal name ""))
      (error "No function or procedure call at point"))
     ((null calling-seq)
      (error "Calling sequence of %s %s is not available" type name))
     (t
      (save-excursion
	(move-marker idlwave-rinfo-marker (point))
	(set-buffer (get-buffer-create "*Help*"))
	(use-local-map idlwave-rinfo-map)
	(setq buffer-read-only nil)
	(erase-buffer)
	(set (make-local-variable 'idlwave-popup-source) nil)
	(set (make-local-variable 'idlwave-current-obj_new-class)
				  idlwave-current-obj_new-class)
	(when superclasses
	  (setq props (list 'mouse-face 'highlight
			    km-prop idlwave-rinfo-mouse-map
			    'help-echo help-echo-class
			    'data (cons 'class data)))
	  (let ((classes (cons initial-class superclasses)) c)
	    (insert "Classes: ")
	    (while (setq c (pop classes))
	      (insert " ")
	      (setq beg (point))
	      (insert c)
	      (if (equal (downcase c) (downcase class))
		  (add-text-properties beg (point) (list 'face 'bold))
		(if (idlwave-rinfo-assq name type c (idlwave-routines))
		    (add-text-properties beg (point) props))))
	    (insert "\n")))
	(setq props (if have-olh
			(list 'mouse-face 'highlight
			      km-prop idlwave-rinfo-mouse-map
			      'help-echo help-echo-use
			      'data (cons 'usage data))))
	(if have-sysdoc (setq props (append (list 'face face) props)))
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
			       km-prop idlwave-rinfo-mouse-map
			       'data (cons 'keyword data)
			       'help-echo help-echo-kwd
			       'keyword (car x)))
	     (if have-sysdoc (setq props (append (list 'face face) props)))
	     (insert (car x))
	     (add-text-properties beg (point) props)
	     (setq col (+ col 1 (length (car x)))))
	   keywords))
	
	(setq cnt 1 total (length all))
	(while (setq entry (pop all))
	  (setq props (list 'mouse-face 'highlight
			    km-prop idlwave-rinfo-mouse-map
			    'help-echo help-echo-src
			    'source (cons (car (nth 2 entry)) (nth 1 entry))
			    'data (cons 'source data)))
	  (idlwave-insert-source-location
	   (format "\n%-8s  %s" 
		   (if (equal cnt 1)
		       (if (> total 1) "Sources:" "Source:")
		     "")
		   (if (> total 1) "- " ""))
	   entry props)
	  (incf cnt)
	  (when (and all (> cnt idlwave-rinfo-max-source-lines))
	    ;; No more source lines, please
	    (insert (format 
		     "\n          Source information truncated to %d entries."
		     idlwave-rinfo-max-source-lines))
	    (setq all nil)))

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

(defun idlwave-insert-source-location (prefix entry &optional file-props)
  "Insert a source location into the routine info buffer.
Start line with PREFIX.
If a file name is inserted, add FILE-PROPS to it."

  (let* ((key (car entry))
	 (file (nth 1 entry))
	 (types (nth 2 entry))
	 (shell-flag (member 'compiled types))
	 (buffer-flag (member 'buffer types))
	 (lib-flag (member 'lib types))
	 (ndupl (or (and buffer-flag (idlwave-count-eq 'buffer types))
		    (and lib-flag (idlwave-count-eq 'lib types))
		    1))
	 (doflags t)
	 beg special)

    (insert prefix)

    (cond
     ((eq key 'system)
      (setq doflags nil)
                                (insert "System    "))
     ((eq key 'builtin)
      (setq doflags nil)
                                (insert "Builtin   "))
     ((and (not file) shell-flag)
                                (insert "Unresolved"))
     ((null file)               (insert "ERROR"))
     ((setq special (idlwave-special-lib-test file))
      (insert (format "%-10s" special)))
     ((idlwave-syslib-p file)
      (if (string-match "obsolete" (file-name-directory file))
	                        (insert "Obsolete  ")
                                (insert "SystemLib ")))
     ((idlwave-lib-p file)      (insert "Library   "))
     (t                         (insert "Other     ")))

    (when doflags
      (insert (concat
	       "  ["
	       (if lib-flag "C" "-")
	       (if shell-flag "S" "-")
	       (if buffer-flag "B" "-")
	       "] ")))
    (when (> ndupl 1) 
      (setq beg (point))
      (insert (format "(%dx) " ndupl))
      (add-text-properties beg (point) (list 'face 'bold)))
    (when (and file (not (equal file "")))
      (setq beg (point))
      (insert (apply 'abbreviate-file-name
		     (if (featurep 'xemacs) (list file t) (list file))))
      (if file-props
	  (add-text-properties beg (point) file-props)))))

(defun idlwave-special-lib-test (file)
  "Check the path of FILE against the regexps which define special libs.
Return the name of the special lib if there is a match."
  (let ((alist idlwave-special-lib-alist)
	entry rtn)
    (cond
     ((stringp file)
      (while (setq entry (pop alist))
	(if (string-match (car entry) file)
	    (setq rtn (cdr entry)
		  alist nil)))
      rtn)
     (t nil))))
  
(defun idlwave-mouse-active-rinfo-right (ev)
  (interactive "e")
  (idlwave-mouse-active-rinfo ev 'right))

(defun idlwave-mouse-active-rinfo-shift (ev)
  (interactive "e")
  (idlwave-mouse-active-rinfo ev nil 'shift))

(defun idlwave-active-rinfo-space ()
  (interactive)
  (idlwave-mouse-active-rinfo nil 'right))

(defun idlwave-mouse-active-rinfo (ev &optional right shift)
  "Does the mouse actions in the routine info buffer.
Optional args RIGHT and SHIFT indicate, if mouse-3 was used, and if SHIFT
was pressed."
  (interactive "e")
  (if ev (mouse-set-point ev))
  (let (data id name type class buf keyword bufwin source word initial-class)
    (setq data (get-text-property (point) 'data)
	  source (get-text-property (point) 'source)
	  keyword (get-text-property (point) 'keyword)
	  id (car data)
	  name (nth 1 data) type (nth 2 data) class (nth 3 data)
	  buf (nth 4 data)
	  initial-class (nth 6 data)
	  word (idlwave-this-word)
	  bufwin (get-buffer-window buf t))
    (cond ((eq id 'class)
	   (if (window-live-p bufwin) (select-window bufwin))
	   (idlwave-display-calling-sequence 
	    (idlwave-sintern-method name)
	    type (idlwave-sintern-class word) 
	    initial-class))
	  ((eq id 'usage)
	   (idlwave-require-online-help)
	   (idlwave-online-help nil name type class))
	  ((eq id 'source)
	   (if (and right (idlwave-help-directory))
	       (let ((idlwave-extra-help-function 'idlwave-help-with-source)
		     (idlwave-help-source-try-header nil)
		     ;; Fake idlwave-routines, to make help find the right entry
		     (idlwave-routines
		      (list (list (nth 1 data) (nth 2 data) (nth 3 data) source ""))))
		 (idlwave-require-online-help)
		 (idlwave-help-get-special-help name type class nil))
	     (setq idlwave-popup-source (not idlwave-popup-source))
	     (if idlwave-popup-source
		 (condition-case err
		     (idlwave-do-find-module name type class source)
		   (error
		    (setq idlwave-popup-source nil)
		    (if (window-live-p bufwin) (select-window bufwin))
		    (error (nth 1 err))))
	       (if bufwin
		   (select-window bufwin)
		 (pop-to-buffer buf))
	       (goto-char (marker-position idlwave-rinfo-marker)))))
	  ((eq id 'keyword)
	   (if right
	       (progn
		 (idlwave-require-online-help)
		 (idlwave-online-help nil name type class keyword))
	     (idlwave-rinfo-insert-keyword keyword buf shift))))))

(defun idlwave-rinfo-insert-keyword (keyword buffer &optional shift)
  "Insert KEYWORD in BUFFER.  Make sure buffer is displayed in a window."
  (let ((bwin (get-buffer-window buffer)))
    (if idlwave-complete-empty-string-as-lower-case
	(setq keyword (downcase keyword)))
    (if bwin
	(select-window bwin)
      (pop-to-buffer buffer)
      (setq bwin (get-buffer-window buffer)))
    (if (eq (preceding-char) ?/)
	(insert keyword)
      (unless (save-excursion 
		(re-search-backward
		 "[(,][ \t]*\\(\\$[ \t]*\\(;.*\\)?\n\\)?[ \t]*\\=" 
		 (min (- (point) 100) (point-min)) t))
	(insert ", "))
      (if shift (insert "/"))
      (insert keyword)
      (if (and (not shift)
	       idlwave-keyword-completion-adds-equal)
	  (insert "=")))))

(defun idlwave-list-buffer-load-path-shadows (&optional arg)
  "List the load path shadows of all routines defined in current buffer."
  (interactive "P")
  (idlwave-routines)
  (if (eq major-mode 'idlwave-mode)
      (idlwave-list-load-path-shadows
       nil (idlwave-update-current-buffer-info 'save-buffer)
       "in current buffer")
    (error "Current buffer is not in idlwave-mode")))

(defun idlwave-list-shell-load-path-shadows (&optional arg)
  "List the load path shadows of all routines compiled under the shell.
This is very useful for checking an IDL application.  Just compile the
application, do RESOLVE_ALL, and `C-c C-i' to compile all referenced
routines and update IDLWAVE internal info.  Then check for shadowing
with this command."
  (interactive "P")
  (cond
   ((or (not (fboundp 'idlwave-shell-is-running))
	(not (idlwave-shell-is-running)))
    (error "Shell is not running"))
   ((null idlwave-compiled-routines)
    (error "No compiled routines.  Maybe you need to update with `C-c C-i'"))
   (t
    (idlwave-list-load-path-shadows nil idlwave-compiled-routines
				    "in the shell"))))

(defun idlwave-list-all-load-path-shadows (&optional arg)
  "List the load path shadows of all routines known to IDLWAVE."
  (interactive "P")
  (idlwave-list-load-path-shadows nil nil "globally"))

(defun idlwave-list-load-path-shadows (arg &optional special-routines loc)
  "List the routines which are defined multiple times.
Search the information IDLWAVE has about IDL routines for multiple
definitions.
When SPECIAL-ROUTINES in non-nil, only look for shadows of these routines.

When IDL hits a routine call which is not defined, it will search on
the load path in order to find a definition.  The output of this
command can be used to detect possible name clashes during this process."
  (idlwave-routines)  ; Make sure everything is loaded.
  (unless idlwave-library-routines
    (or (y-or-n-p 
	 "You don't have a library catalog.  Continue anyway? ")
	(error "Abort")))
  (let* ((routines (append idlwave-system-routines
			   idlwave-compiled-routines
			   idlwave-library-routines
			   idlwave-buffer-routines
			   nil))
	 (km-prop (if (featurep 'xemacs) 'keymap 'local-map))
	 (keymap (make-sparse-keymap))
	 (props (list 'mouse-face 'highlight
		      km-prop keymap
		      'help-echo "Mouse2: Find source"))      
	 (nroutines (length (or special-routines routines)))
	 (step (/ nroutines 99))
	 (n 0)
	 (next-perc 1)
	 (cnt 0)
	 (idlwave-sort-prefer-buffer-info nil)
	 routine twins dtwins twin done props1 lroutines)

    (if special-routines
	;; Just looking for shadows of a few special routines
	(setq lroutines routines
	      routines special-routines))

    (message "Sorting routines...")
    (setq routines (sort routines
			 (lambda (a b)
			   (string< (downcase (idlwave-make-full-name
					       (nth 2 a) (car a)))
				    (downcase (idlwave-make-full-name
					       (nth 2 b) (car b)))))))
    (message "Sorting routines...done")

    (define-key keymap (if (featurep 'xemacs) [(button2)] [(mouse-2)])
      (lambda (ev) 
	(interactive "e")
	(mouse-set-point ev)
	(apply 'idlwave-do-find-module
	       (get-text-property (point) 'find-args))))
    (define-key keymap [(return)]
      (lambda () 
	(interactive)
	(apply 'idlwave-do-find-module
	       (get-text-property (point) 'find-args))))
    (message "Compiling list...( 0%%)")
    (save-excursion
      (set-buffer (get-buffer-create "*Shadows*"))
      (setq buffer-read-only nil)
      (erase-buffer)
      (while (setq routine (pop routines))
	(setq n (1+ n))
	(if (= (* next-perc step) n)
	    (progn
	      (message "Compiling list...(%2d%%)" next-perc)
	      (setq next-perc (1+ next-perc))))
	;; Get a list of all twins
	(setq twins (idlwave-routine-twins routine (or lroutines routines)))
	(if (memq routine done)
	    (setq dtwins nil)
	  (setq dtwins (idlwave-study-twins twins)))
	;; Mark all twins as delt with
	(setq done (append twins done))
	(when (or (> (length dtwins) 1)
		  (> (idlwave-count-eq 'lib (nth 2 (car dtwins))) 1)
		  (> (idlwave-count-eq 'buffer (nth 2 (car dtwins))) 1))
	  (incf cnt)
	  (insert (format "\n%s%s"
			  (idlwave-make-full-name (nth 2 routine) (car routine))
			  (if (eq (nth 1 routine) 'fun) "()" "")))
	  (while (setq twin (pop dtwins))
	    (setq props1 (append (list 'find-args
				       (list (nth 0 routine) 
					     (nth 1 routine) 
					     (nth 2 routine)
					     (cons 'lib (nth 1 twin))))
				 props))
	    (idlwave-insert-source-location "\n   - " twin props1))))
      (goto-char (point-min))
      (setq buffer-read-only t))
    (setq loc (or loc ""))
    (if (> cnt 0)
	(progn
	  (display-buffer (get-buffer "*Shadows*"))
	  (message "%d case%s of shadowing found %s"
		   cnt (if (= cnt 1) "" "s") loc))
      (message "No shadowing conflicts found %s" loc))))

(defun idlwave-print-source (routine)
  (let* ((source (nth 3 routine))
	 (stype (car source))
	 (sfile (cdr source)))
    (if (and (eq stype 'lib) sfile)
	(progn
	  (setq sfile (idlwave-expand-lib-file-name sfile))
	  (if (idlwave-syslib-p sfile) (setq stype 'syslib))))
    (if (and (eq stype 'compiled)
	     (or (not (stringp sfile))
		 (not (string-match "\\S-" sfile))))
	(setq stype 'unresolved))
    (princ (format "      %-10s %s\n" 
		   stype
		   (if sfile sfile "No source code available")))))

(defun idlwave-routine-twins (entry &optional list)
  "Return all twin entries of ENTRY in LIST.
LIST defaults to `idlwave-routines'.
Twin entries are those which have the same name, type, and class.
ENTRY will also be returned, as the first item of this list."
  (let* ((name (car entry))
	 (type (nth 1 entry))
	 (class (nth 2 entry))
	 (candidates (idlwave-all-assq name (or list (idlwave-routines))))
	 twins candidate)
    (while (setq candidate (pop candidates))
      (if (and (not (eq candidate entry))
	       (eq type (nth 1 candidate))
	       (eq class (nth 2 candidate)))
	  (push candidate twins)))
    (if (setq candidate (idlwave-rinfo-assq name type class 
					    idlwave-unresolved-routines))
	(push candidate twins))
    (cons entry (nreverse twins))))

(defun idlwave-study-twins (entries)
  "Return dangerous twins of first entry in TWINS.
Dangerous twins are routines with same name, but in different files
on the load path.
If a file is in the system library and has an entry in the
`idlwave-system-routines' list, we omit the latter because many IDL 
routines are implemented as library routines."
  (let* ((entry (car entries))
	 (name (car entry))      ; 
	 (type (nth 1 entry))    ; Must be bound for
	 (class (nth 2 entry))   ;  idlwave-routine-twin-compare
	 (cnt 0)
	 source type file thefile alist syslibp key)
    (while (setq entry (pop entries))
      (incf cnt)
      (setq source (nth 3 entry)
	    type (car source)
	    file (cdr source))
      (if (eq type 'lib)
	  (setq file (idlwave-expand-lib-file-name file)))
      ;; Make KEY to index entry properly
      (setq key (cond ((eq type 'system) type)
		      (file (file-truename file))
		      (t 'unresolved)))
      (if (and file 
	       (not syslibp)
	       (idlwave-syslib-p file))
	  ;; We do have an entry in the system library
	  (setq syslibp t))
      
      (setq thefile (or thefile file))
      (if (setq entry (assoc key alist))
	  (push type (nth 2 entry))
	(push (list key file (list type)) alist)))
    
    (setq alist (nreverse alist))
    
    (when syslibp
      ;; File is system *library* - remove any system entry
      (setq alist (delq (assoc 'system alist) alist)))
    
    (when (and (idlwave-syslib-scanned-p)
	       (setq entry (assoc 'system alist)))
      (setcar entry 'builtin))
    (sort alist 'idlwave-routine-twin-compare)))

(defvar name)
(defvar type)
(defvar class)
(defvar idlwave-sort-prefer-buffer-info t
  "Internal variable used to influence `idlwave-routine-twin-compare'.")

(defmacro idlwave-xor (a b)
  `(and (or ,a ,b)
	(not (and ,a ,b))))

(defun idlwave-routine-entry-compare (a b)
  "Compare two routine info entries for sortiung.  This is the general case.
It first compates class, names, and type.  If it turns out that A and B
are twins (same name, class, and type),  calls another routine which
compares twins on the basis of their file names and path locations."
  (let ((name (car a)) (type (nth 1 a)) (class (nth 2 a)))
    (cond
     ((not (equal (idlwave-downcase-safe class)
		  (idlwave-downcase-safe (nth 2 b))))
      ;; Class decides
      (cond ((null (nth 2 b)) nil)
	    ((null class) t)
	    (t (string< (downcase class) (downcase (nth 2 b))))))
     ((not (equal (downcase name) (downcase (car b))))
      ;; Name decides
      (string< (downcase name) (downcase (car b))))
     ((not (eq type (nth 1 b)))
      ;; Type decides
      (< (if (eq type 'fun) 1 0) (if (eq (nth 1 b) 'fun) 1 0)))
     (t	
      ;; A and B are twins - so the decision is more complicated.
      ;; Call twin-compare with the proper arguments.
      (idlwave-routine-entry-compare-twins a b)))))

(defun idlwave-routine-entry-compare-twins (a b)
  "Compare two routine entries, under the assumption that they are twins.
This basically calles `idlwave-routine-twin-compare' with the correct args."
  (let ((name (car a)) (type (nth 1 a)) (class (nth 2 a)) ; needed outside
	(atype (car (nth 3 a)))
	(btype (car (nth 3 b)))
	(afile (cdr (nth 3 a)))
	(bfile (cdr (nth 3 b))))
    (if (eq atype 'lib)
	(setq afile (idlwave-expand-lib-file-name afile)))
    (if (eq btype 'lib)
	(setq bfile (idlwave-expand-lib-file-name bfile)))
    (idlwave-routine-twin-compare
     (if (stringp afile)
	 (list (file-truename afile) afile (list atype))
       (list atype afile (list atype)))
     (if (stringp bfile)
	 (list (file-truename bfile) bfile (list btype))
       (list btype bfile (list btype))))
    ))

(defun idlwave-routine-twin-compare (a b)
  "Compare two routine twin entries for sorting.
In here, A and B are not normal routine info entries, but special
lists (KEY FILENAME (TYPES...)).
This expects NAME TYPE CLASS to be bound to the right values."
  (let* (;; Dis-assemble entries
	 (akey (car a))	     (bkey (car b))
	 (afile (nth 1 a))   (bfile (nth 1 b))
	 (atypes (nth 2 a))  (btypes (nth 2 b))
	 ;; System routines?
	 (asysp (memq akey '(builtin system)))
	 (bsysp (memq bkey '(builtin system)))
	 ;; Compiled routines?
	 (acompp (memq 'compiled atypes))
	 (bcompp (memq 'compiled btypes))
	 ;; Unresolved?
	 (aunresp (or (eq akey 'unresolved)
		      (and acompp (not afile))))
	 (bunresp (or (eq bkey 'unresolved)
		      (and bcompp (not bfile))))
	 ;; Buffer info available?
	 (abufp (memq 'buffer atypes))
	 (bbufp (memq 'buffer btypes))
	 ;; On search path?
	 (tpath-alist (idlwave-true-path-alist))
	 (apathp (assoc akey tpath-alist))
	 (bpathp (assoc bkey tpath-alist))
	 ;; How early on search path?  High number means early since we
	 ;; measure the tail of the path list
	 (anpath (length (memq apathp tpath-alist)))
	 (bnpath (length (memq bpathp tpath-alist)))
	 ;; Look at file names
	 (aname (if (stringp afile) (downcase (file-name-nondirectory afile)) ""))
	 (bname (if (stringp bfile) (downcase (file-name-nondirectory bfile)) ""))
	 (fname-re (if class (format "\\`%s__\\(%s\\|define\\)\\.pro\\'"
				     (regexp-quote (downcase class))
				     (regexp-quote (downcase name)))
		     (format "\\`%s\\.pro" (regexp-quote (downcase name)))))
	 ;; Is file name derived from the routine name?
	 ;; Method file or class definition file?
	 (anamep (string-match fname-re aname))
	 (adefp (and class anamep (string= "define" (match-string 1 aname))))
	 (bnamep (string-match fname-re bname))
	 (bdefp (and class bnamep (string= "define" (match-string 1 bname)))))

    ;; Now: follow JD's ideas about sorting.  Looks really simple now,
    ;; doesn't it?  The difficult stuff is hidden above...
    (cond
     ((idlwave-xor asysp  bsysp)       asysp)	; System entries first
     ((idlwave-xor aunresp bunresp)    bunresp) ; Unresolved last
     ((and idlwave-sort-prefer-buffer-info
	   (idlwave-xor abufp bbufp))  abufp)	; Buffers before non-buffers
     ((idlwave-xor acompp bcompp)      acompp)	; Compiled entries
     ((idlwave-xor apathp bpathp)      apathp)	; Library before non-library
     ((idlwave-xor anamep bnamep)      anamep)	; Correct file names first
     ((and class anamep bnamep                  ; both file names match ->
	   (idlwave-xor adefp bdefp))  bdefp)	; __define after __method
     ((> anpath bnpath)                t)	; Who is first on path?
     (t                                nil))))	; Default

(defun idlwave-downcase-safe (string)
  "Donwcase if string, else return unchanged."
  (if (stringp string)
      (downcase string)
    string))

(defun idlwave-count-eq (elt list)
  "How often is ELT in LIST?"
  (length (delq nil (mapcar (lambda (x) (eq x elt)) list))))

(defun idlwave-syslib-p (file)
  "Non-nil of FILE is in the system library."
  (let* ((true-syslib (file-name-as-directory
		       (file-truename
			(expand-file-name "lib" (idlwave-sys-dir)))))
	 (true-file (file-truename file)))
    (string-match (concat "^" (regexp-quote true-syslib)) true-file)))

(defun idlwave-lib-p (file)
  "Non-nil if file is in the library"
  (let ((true-dir (file-name-directory (file-truename file))))
    (assoc true-dir (idlwave-true-path-alist))))

(defun idlwave-true-path-alist ()
  "Return `idlwave-path-alist' alist with true-names.
Info is cached, but relies on the functons setting `idlwave-path-alist'
to reset the variable `idlwave-true-path-alist' to nil."
  (or idlwave-true-path-alist
      (setq idlwave-true-path-alist
	    (mapcar (lambda(x) (cons
				(file-name-as-directory
				 (file-truename
				  (directory-file-name
				   (car x))))
				(cdr x)))
		    idlwave-path-alist))))

(defun idlwave-syslib-scanned-p ()
  "Non-nil if the system lib file !DIR/lib has been scanned."
  (let* ((true-syslib (file-name-as-directory
		       (file-truename
			(expand-file-name "lib" (idlwave-sys-dir))))))
    (cdr (assoc true-syslib (idlwave-true-path-alist)))))

;; ----------------------------------------------------------------------------
;;
;; Online Help display


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
    ("Completion"
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
      ["9 Class Name"  idlwave-complete-class t]))
    ("Routine Info"
     ["Show Routine Info" idlwave-routine-info t]
     ["Online Context Help" idlwave-context-help (idlwave-help-directory)]
     "--"
     ["Find Routine Source" idlwave-find-module t]
     ["Resolve Routine" idlwave-resolve (featurep 'idlw-shell)]
     "--"
     ["Update Routine Info" idlwave-update-routine-info t]
     "--"
     "IDL Library Catalog"
     ["Select Catalog Directories" idlwave-create-libinfo-file t]
     ["Scan Directories" (idlwave-update-routine-info '(16))
      idlwave-path-alist]
     "--"
     "Routine Shadows"
     ["Check Current Buffer" idlwave-list-buffer-load-path-shadows t]
     ["Check Compiled Routines" idlwave-list-shell-load-path-shadows t]
     ["Check Everything" idlwave-list-all-load-path-shadows t])
    ("Misc"
     ["Kill auto-created buffers" idlwave-kill-autoloaded-buffers t]
     "--"
     ["Insert TAB character" idlwave-hard-tab t])
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

;; Try to load online help, but catch any errors.
(condition-case nil
    (idlwave-require-online-help)
  (error nil))

;; Run the hook
(run-hooks 'idlwave-load-hook)

(provide 'idlwave)

;;; idlwave.el ends here
