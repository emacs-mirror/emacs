;;; loaddefs.el --- define autoloads from other files

;; Copyright (C) 1985,86,87,92,93,94,95,96,97,98 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; **********************************************************************
;;; You should never need to write autoloads by hand and put them here.
;;;
;;; It is no longer necessary.  Instead use autoload.el to maintain them
;;; for you.  Just insert ";;;###autoload" before defuns or defmacros you
;;; want to be autoloaded, or other forms you want copied into loaddefs.el
;;; (defvars, key definitions, etc.).  For example,
;;;	;;;###autoload
;;;	(defun foobar () ....)
;;;	;;;###autoload (define-key global-map "f" 'foobar)
;;;	;;;###autoload
;;;	(defvar foobar-var nil "\
;;;	This is foobar-var's doc-string.")
;;;
;;; Then do M-x update-file-autoloads on the file to update loaddefs.el.
;;;
;;; You can also use M-x update-autoloads-from-directory to update the
;;; autoloads in loaddefs.el for all .el files in the directory specified.
;;; **********************************************************************

;;; Generated autoloads follow (made by autoload.el).

;;; To sort them, execute the following after narrowing
;;; to a region starting just after the following formfeed (control-l)
;;; and ending just after the last formfeed in the file.

;;;(sort-regexp-fields nil "\n*.*\n.*from \\(.*\\)[^]*" "\\1"
;;;		    (point-min) (point-max))

;;;### (autoloads (ada-make-filename-from-adaname ada-mode) "ada-mode"
;;;;;;  "progmodes/ada-mode.el" (14049 32049))
;;; Generated autoloads from progmodes/ada-mode.el

(autoload (quote ada-mode) "ada-mode" "\
Ada mode is the major mode for editing Ada code.

Bindings are as follows: (Note: 'LFD' is control-j.)

 Indent line                                          '\\[ada-tab]'
 Indent line, insert newline and indent the new line. '\\[newline-and-indent]'

 Re-format the parameter-list point is in             '\\[ada-format-paramlist]'
 Indent all lines in region                           '\\[ada-indent-region]'
 Call external pretty printer program                 '\\[ada-call-pretty-printer]'

 Adjust case of identifiers and keywords in region    '\\[ada-adjust-case-region]'
 Adjust case of identifiers and keywords in buffer    '\\[ada-adjust-case-buffer]'

 Call EXTERNAL pretty printer (if you have one)       '\\[ada-call-pretty-printer]'

 Fill comment paragraph                               '\\[ada-fill-comment-paragraph]'
 Fill comment paragraph and justify each line         '\\[ada-fill-comment-paragraph-justify]'
 Fill comment paragraph, justify and append postfix   '\\[ada-fill-comment-paragraph-postfix]'

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
 If you use this function in a spec and no body is available, it gets created
 with body stubs.

If you use ada-xref.el:
 Goto declaration:          '\\[ada-point-and-xref]' on the identifier
                         or '\\[ada-goto-declaration]' with point on the identifier
 Complete identifier:       '\\[ada-complete-identifier]'
 Execute Gnatf:             '\\[ada-gnatf-current]'" t nil)

(autoload (quote ada-make-filename-from-adaname) "ada-mode" "\
Determine the filename of a package/procedure from its own Ada name." t nil)

;;;***

;;;### (autoloads (add-log-current-defun change-log-mode add-change-log-entry-other-window
;;;;;;  add-change-log-entry find-change-log prompt-for-change-log-name
;;;;;;  add-log-mailing-address add-log-full-name) "add-log" "add-log.el"
;;;;;;  (14214 15653))
;;; Generated autoloads from add-log.el

(defvar add-log-full-name nil "\
*Full name of user, for inclusion in ChangeLog daily headers.
This defaults to the value returned by the `user-full-name' function.")

(defvar add-log-mailing-address nil "\
*Electronic mail address of user, for inclusion in ChangeLog daily headers.
This defaults to the value of `user-mail-address'.")

(autoload (quote prompt-for-change-log-name) "add-log" "\
Prompt for a change log name." nil nil)

(autoload (quote find-change-log) "add-log" "\
Find a change log file for \\[add-change-log-entry] and return the name.

Optional arg FILE-NAME specifies the file to use.
If FILE-NAME is nil, use the value of `change-log-default-name'.
If 'change-log-default-name' is nil, behave as though it were 'ChangeLog'
\(or whatever we use on this operating system).

If 'change-log-default-name' contains a leading directory component, then
simply find it in the current directory.  Otherwise, search in the current
directory and its successive parents for a file so named.

Once a file is found, `change-log-default-name' is set locally in the
current buffer to the complete file name." nil nil)

(autoload (quote add-change-log-entry) "add-log" "\
Find change log file and add an entry for today.
Optional arg WHOAMI (interactive prefix) non-nil means prompt for user
name and site.

Second arg is FILE-NAME of change log.  If nil, uses `change-log-default-name'.
Third arg OTHER-WINDOW non-nil means visit in other window.
Fourth arg NEW-ENTRY non-nil means always create a new entry at the front;
never append to an existing entry.  Option `add-log-keep-changes-together'
otherwise affects whether a new entry is created.

Today's date is calculated according to `change-log-time-zone-rule' if
non-nil, otherwise in local time." t nil)

(autoload (quote add-change-log-entry-other-window) "add-log" "\
Find change log file in other window and add an entry for today.
Optional arg WHOAMI (interactive prefix) non-nil means prompt for user
name and site.
Second optional arg FILE-NAME is file name of change log.
If nil, use `change-log-default-name'.

Affected by the same options as `add-change-log-entry'." t nil)
 (define-key ctl-x-4-map "a" 'add-change-log-entry-other-window)

(autoload (quote change-log-mode) "add-log" "\
Major mode for editing change logs; like Indented Text Mode.
Prevents numeric backups and sets `left-margin' to 8 and `fill-column' to 74.
New log entries are usually made with \\[add-change-log-entry] or \\[add-change-log-entry-other-window].
Each entry behaves as a paragraph, and the entries for one day as a page.
Runs `change-log-mode-hook'." t nil)

(defvar add-log-lisp-like-modes (quote (emacs-lisp-mode lisp-mode scheme-mode dsssl-mode lisp-interaction-mode)) "\
*Modes that look like Lisp to `add-log-current-defun'.")

(defvar add-log-c-like-modes (quote (c-mode c++-mode c++-c-mode objc-mode)) "\
*Modes that look like C to `add-log-current-defun'.")

(defvar add-log-tex-like-modes (quote (TeX-mode plain-TeX-mode LaTeX-mode plain-tex-mode latex-mode)) "\
*Modes that look like TeX to `add-log-current-defun'.")

(autoload (quote add-log-current-defun) "add-log" "\
Return name of function definition point is in, or nil.

Understands C, Lisp, LaTeX (\"functions\" are chapters, sections, ...),
Texinfo (@node titles), Perl, and Fortran.

Other modes are handled by a heuristic that looks in the 10K before
point for uppercase headings starting in the first column or
identifiers followed by `:' or `=', see variable
`add-log-current-defun-header-regexp'.

Has a preference of looking backwards." nil nil)

;;;***

;;;### (autoloads (defadvice ad-add-advice ad-default-compilation-action
;;;;;;  ad-redefinition-action) "advice" "emacs-lisp/advice.el" (14192
;;;;;;  4647))
;;; Generated autoloads from emacs-lisp/advice.el

(defvar ad-redefinition-action (quote warn) "\
*Defines what to do with redefinitions during Advice de/activation.
Redefinition occurs if a previously activated function that already has an
original definition associated with it gets redefined and then de/activated.
In such a case we can either accept the current definition as the new
original definition, discard the current definition and replace it with the
old original, or keep it and raise an error.  The values `accept', `discard',
`error' or `warn' govern what will be done.  `warn' is just like `accept' but
it additionally prints a warning message.  All other values will be
interpreted as `error'.")

(defvar ad-default-compilation-action (quote maybe) "\
*Defines whether to compile advised definitions during activation.
A value of `always' will result in unconditional compilation, `never' will
always avoid compilation, `maybe' will compile if the byte-compiler is already
loaded, and `like-original' will compile if the original definition of the
advised function is compiled or a built-in function. Every other value will
be interpreted as `maybe'. This variable will only be considered if the 
COMPILE argument of `ad-activate' was supplied as nil.")

(autoload (quote ad-add-advice) "advice" "\
Adds a piece of ADVICE to FUNCTION's list of advices in CLASS.
If FUNCTION already has one or more pieces of advice of the specified
CLASS then POSITION determines where the new piece will go.  The value
of POSITION can either be `first', `last' or a number where 0 corresponds
to `first'.  Numbers outside the range will be mapped to the closest
extreme position.  If there was already a piece of ADVICE with the same
name, then the position argument will be ignored and the old advice
will be overwritten with the new one.
    If the FUNCTION was not advised already, then its advice info will be 
initialized.  Redefining a piece of advice whose name is part of the cache-id
will clear the cache." nil nil)

(autoload (quote defadvice) "advice" "\
Defines a piece of advice for FUNCTION (a symbol).
The syntax of `defadvice' is as follows:

  (defadvice FUNCTION (CLASS NAME [POSITION] [ARGLIST] FLAG...)
    [DOCSTRING] [INTERACTIVE-FORM]
    BODY... )

FUNCTION ::= Name of the function to be advised.
CLASS ::= `before' | `around' | `after' | `activation' | `deactivation'.
NAME ::= Non-nil symbol that names this piece of advice.
POSITION ::= `first' | `last' | NUMBER. Optional, defaults to `first',
    see also `ad-add-advice'.
ARGLIST ::= An optional argument list to be used for the advised function
    instead of the argument list of the original.  The first one found in
    before/around/after-advices will be used.
FLAG ::= `protect'|`disable'|`activate'|`compile'|`preactivate'|`freeze'.
    All flags can be specified with unambiguous initial substrings.
DOCSTRING ::= Optional documentation for this piece of advice.
INTERACTIVE-FORM ::= Optional interactive form to be used for the advised
    function.  The first one found in before/around/after-advices will be used.
BODY ::= Any s-expression.

Semantics of the various flags:
`protect': The piece of advice will be protected against non-local exits in
any code that precedes it.  If any around-advice of a function is protected
then automatically all around-advices will be protected (the complete onion).

`activate': All advice of FUNCTION will be activated immediately if
FUNCTION has been properly defined prior to this application of `defadvice'.

`compile': In conjunction with `activate' specifies that the resulting
advised function should be compiled.

`disable': The defined advice will be disabled, hence, it will not be used 
during activation until somebody enables it.

`preactivate': Preactivates the advised FUNCTION at macro-expansion/compile
time.  This generates a compiled advised definition according to the current
advice state that will be used during activation if appropriate.  Only use
this if the `defadvice' gets actually compiled.

`freeze': Expands the `defadvice' into a redefining `defun/defmacro' according
to this particular single advice.  No other advice information will be saved.
Frozen advices cannot be undone, they behave like a hard redefinition of
the advised function.  `freeze' implies `activate' and `preactivate'.  The
documentation of the advised function can be dumped onto the `DOC' file
during preloading.

Look at the file `advice.el' for comprehensive documentation." nil (quote macro))

;;;***

;;;### (autoloads (ange-ftp-hook-function ange-ftp-reread-dir) "ange-ftp"
;;;;;;  "ange-ftp.el" (14203 50169))
;;; Generated autoloads from ange-ftp.el
 (defalias 'ange-ftp-re-read-dir 'ange-ftp-reread-dir)

(autoload (quote ange-ftp-reread-dir) "ange-ftp" "\
Reread remote directory DIR to update the directory cache.
The implementation of remote ftp file names caches directory contents
for speed.  Therefore, when new remote files are created, Emacs
may not know they exist.  You can use this command to reread a specific
directory, so that Emacs will know its current contents." t nil)

(autoload (quote ange-ftp-hook-function) "ange-ftp" nil nil nil)

(or (assoc "^/[^/:]*[^/:.]:" file-name-handler-alist) (setq file-name-handler-alist (cons (quote ("^/[^/:]*[^/:.]:" . ange-ftp-hook-function)) file-name-handler-alist)))

(or (assoc "^/[^/:]*\\'" file-name-handler-alist) (setq file-name-handler-alist (cons (quote ("^/[^/:]*\\'" . ange-ftp-completion-hook-function)) file-name-handler-alist)))

;;;***

;;;### (autoloads (appt-make-list appt-delete appt-add appt-display-diary
;;;;;;  appt-display-duration appt-msg-window appt-display-mode-line
;;;;;;  appt-visible appt-audible appt-message-warning-time appt-issue-message)
;;;;;;  "appt" "calendar/appt.el" (14063 14962))
;;; Generated autoloads from calendar/appt.el

(defvar appt-issue-message t "\
*Non-nil means check for appointments in the diary buffer.
To be detected, the diary entry must have the time
as the first thing on a line.")

(defvar appt-message-warning-time 12 "\
*Time in minutes before an appointment that the warning begins.")

(defvar appt-audible t "\
*Non-nil means beep to indicate appointment.")

(defvar appt-visible t "\
*Non-nil means display appointment message in echo area.")

(defvar appt-display-mode-line t "\
*Non-nil means display minutes to appointment and time on the mode line.")

(defvar appt-msg-window t "\
*Non-nil means display appointment message in another window.")

(defvar appt-display-duration 10 "\
*The number of seconds an appointment message is displayed.")

(defvar appt-display-diary t "\
*Non-nil means to display the next days diary on the screen. 
This will occur at midnight when the appointment list is updated.")

(autoload (quote appt-add) "appt" "\
Add an appointment for the day at TIME and issue MESSAGE.
The time should be in either 24 hour format or am/pm format." t nil)

(autoload (quote appt-delete) "appt" "\
Delete an appointment from the list of appointments." t nil)

(autoload (quote appt-make-list) "appt" nil nil nil)

;;;***

;;;### (autoloads (apropos-documentation apropos-value apropos apropos-command
;;;;;;  apropos-variable) "apropos" "apropos.el" (13940 33135))
;;; Generated autoloads from apropos.el

(autoload (quote apropos-variable) "apropos" "\
Show user variables that match REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also show
normal variables." t nil)

(fset (quote command-apropos) (quote apropos-command))

(autoload (quote apropos-command) "apropos" "\
Show commands (interactively callable functions) that match REGEXP.
With optional prefix ARG, or if `apropos-do-all' is non-nil, also show
noninteractive functions.

If VAR-PREDICATE is non-nil, show only variables, and only those that
satisfy the predicate VAR-PREDICATE." t nil)

(autoload (quote apropos) "apropos" "\
Show all bound symbols whose names match REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also show unbound
symbols and key bindings, which is a little more time-consuming.
Returns list of symbols and documentation found." t nil)

(autoload (quote apropos-value) "apropos" "\
Show all symbols whose value's printed image matches REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also looks
at the function and at the names and values of properties.
Returns list of symbols and values found." t nil)

(autoload (quote apropos-documentation) "apropos" "\
Show symbols whose documentation contain matches for REGEXP.
With optional prefix ARG or if `apropos-do-all' is non-nil, also use
documentation that is not stored in the documentation file and show key
bindings.
Returns list of symbols and documentation found." t nil)

;;;***

;;;### (autoloads (archive-mode) "arc-mode" "arc-mode.el" (14033
;;;;;;  32154))
;;; Generated autoloads from arc-mode.el

(autoload (quote archive-mode) "arc-mode" "\
Major mode for viewing an archive file in a dired-like way.
You can move around using the usual cursor motion commands.
Letters no longer insert themselves.
Type `e' to pull a file out of the archive and into its own buffer;
or click mouse-2 on the file's line in the archive mode buffer.

If you edit a sub-file of this archive (as with the `e' command) and
save it, the contents of that buffer will be saved back into the
archive.

\\{archive-mode-map}" nil nil)

;;;***

;;;### (autoloads (array-mode) "array" "array.el" (13623 48741))
;;; Generated autoloads from array.el

(autoload (quote array-mode) "array" "\
Major mode for editing arrays.

  Array mode is a specialized mode for editing arrays.  An array is
considered to be a two-dimensional set of strings.  The strings are
NOT recognized as integers or real numbers.

  The array MUST reside at the top of the buffer.  

  TABs are not respected, and may be converted into spaces at any time.
Setting the variable 'respect-tabs to non-nil will prevent TAB conversion,
but will cause many functions to give errors if they encounter one.

  Upon entering array mode, you will be prompted for the values of
several variables.  Others will be calculated based on the values you
supply.  These variables are all local the the buffer.  Other buffer
in array mode may have different values assigned to the variables.
The variables are:

Variables you assign:
     max-row:          The number of rows in the array.
     max-column:       The number of columns in the array. 
     columns-per-line: The number of columns in the array per line of buffer.
     field-width:      The width of each field, in characters.
     rows-numbered:    A logical variable describing whether to ignore
                       row numbers in the buffer.

Variables which are calculated:
     line-length:      The number of characters in a buffer line.
     lines-per-row:    The number of buffer lines used to display each row.

  The following commands are available (an asterisk indicates it may
take a numeric prefix argument):

    *  	\\<array-mode-map>\\[array-forward-column]	  Move forward one column.
    *  	\\[array-backward-column]	  Move backward one column.
    *  	\\[array-next-row]	  Move down one row.
    *  	\\[array-previous-row]	  Move up one row.

    *   \\[array-copy-forward]	  Copy the current field into the column to the right.
    *   \\[array-copy-backward]	  Copy the current field into the column to the left.
    *   \\[array-copy-down]	  Copy the current field into the row below.
    *   \\[array-copy-up]	  Copy the current field into the row above.

    *   \\[array-copy-column-forward]   Copy the current column into the column to the right.
    *   \\[array-copy-column-backward]   Copy the current column into the column to the left.
    *   \\[array-copy-row-down]   Copy the current row into the row below.
    *   \\[array-copy-row-up]   Copy the current row into the row above.

        \\[array-fill-rectangle]   Copy the field at mark into every cell with row and column
                  between that of point and mark.

	\\[array-what-position]	  Display the current array row and column.
	\\[array-goto-cell]	  Go to a particular array cell.

	\\[array-make-template]	  Make a template for a new array.
	\\[array-reconfigure-rows]	  Reconfigure the array.
        \\[array-expand-rows]   Expand the array (remove row numbers and
                  newlines inside rows)

        \\[array-display-local-variables]   Display the current values of local variables.

Entering array mode calls the function `array-mode-hook'." t nil)

;;;***

;;;### (autoloads (asm-mode) "asm-mode" "progmodes/asm-mode.el" (13833
;;;;;;  34413))
;;; Generated autoloads from progmodes/asm-mode.el

(autoload (quote asm-mode) "asm-mode" "\
Major mode for editing typical assembler code.
Features a private abbrev table and the following bindings:

\\[asm-colon]	outdent a preceding label, tab to next tab stop.
\\[tab-to-tab-stop]	tab to next tab stop.
\\[asm-newline]	newline, then tab to next tab stop.
\\[asm-comment]	smart placement of assembler comments.

The character used for making comments is set by the variable
`asm-comment-char' (which defaults to `?\\;').

Alternatively, you may set this variable in `asm-mode-set-comment-hook',
which is called near the beginning of mode initialization.

Turning on Asm mode runs the hook `asm-mode-hook' at the end of initialization.

Special commands:
\\{asm-mode-map}
" t nil)

;;;***

;;;### (autoloads (auto-show-mode auto-show-mode) "auto-show" "auto-show.el"
;;;;;;  (14201 235))
;;; Generated autoloads from auto-show.el

(defvar auto-show-mode nil "\
Non-nil means do automatic horizontal scrolling, when lines are truncated.

This variable is automatically local in each buffer where it is set.

Setting this variable directly does not take effect;
use either \\[customize] or the function `auto-show-mode'.")

(custom-add-to-group (quote auto-show) (quote auto-show-mode) (quote custom-variable))

(custom-add-load (quote auto-show-mode) (quote auto-show))

(autoload (quote auto-show-mode) "auto-show" "\
Turn automatic horizontal scroll mode on or off.
With arg, turn auto scrolling on if arg is positive, off otherwise.
This mode is enabled or disabled for each buffer individually.
It takes effect only when `truncate-lines' is non-nil." t nil)

;;;***

;;;### (autoloads (auto-insert-mode define-auto-insert auto-insert)
;;;;;;  "autoinsert" "autoinsert.el" (14118 1735))
;;; Generated autoloads from autoinsert.el

(autoload (quote auto-insert) "autoinsert" "\
Insert default contents into a new file if `auto-insert' is non-nil.
Matches the visited file name against the elements of `auto-insert-alist'." t nil)

(autoload (quote define-auto-insert) "autoinsert" "\
Associate CONDITION with (additional) ACTION in `auto-insert-alist'.
Optional AFTER means to insert action after all existing actions for CONDITION,
or if CONDITION had no actions, after all other CONDITIONs." nil nil)

(autoload (quote auto-insert-mode) "autoinsert" "\
Toggle auto-insert mode.
With prefix ARG, turn auto-insert mode on if and only if ARG is positive.
Returns the new status of auto-insert mode (non-nil means on).

When auto-insert mode is enabled, when new files are created you can
insert a template for the file depending on the mode of the buffer." t nil)

;;;***

;;;### (autoloads (batch-update-autoloads update-autoloads-from-directories
;;;;;;  update-file-autoloads) "autoload" "emacs-lisp/autoload.el"
;;;;;;  (14198 40228))
;;; Generated autoloads from emacs-lisp/autoload.el

(autoload (quote update-file-autoloads) "autoload" "\
Update the autoloads for FILE in `generated-autoload-file'
\(which FILE might bind in its local variables)." t nil)

(autoload (quote update-autoloads-from-directories) "autoload" "\
Update loaddefs.el with all the current autoloads from DIRS, and no old ones.
This uses `update-file-autoloads' (which see) do its work." t nil)

(autoload (quote batch-update-autoloads) "autoload" "\
Update loaddefs.el autoloads in batch mode.
Calls `update-autoloads-from-directories' on the command line arguments." nil nil)

;;;***

;;;### (autoloads (global-auto-revert-mode turn-on-auto-revert-mode
;;;;;;  auto-revert-mode) "autorevert" "autorevert.el" (13691 6971))
;;; Generated autoloads from autorevert.el

(autoload (quote auto-revert-mode) "autorevert" "\
Toggle reverting buffer when file on disk changes.

With arg, turn Auto Revert mode on if and only if arg is positive.
This is a minor mode that affects only the current buffer.
Use `global-auto-revert-mode' to automatically revert all buffers." t nil)

(autoload (quote turn-on-auto-revert-mode) "autorevert" "\
Turn on Auto-Revert Mode.

This function is designed to be added to hooks, for example:
  (add-hook 'c-mode-hook 'turn-on-auto-revert-mode)" nil nil)

(autoload (quote global-auto-revert-mode) "autorevert" "\
Revert any buffer when file on disk change.

With arg, turn Auto Revert mode on globally if and only if arg is positive.
This is a minor mode that affects all buffers.
Use `auto-revert-mode' to revert a particular buffer." t nil)

;;;***

;;;### (autoloads (mouse-avoidance-mode) "avoid" "avoid.el" (14118
;;;;;;  1851))
;;; Generated autoloads from avoid.el

(autoload (quote mouse-avoidance-mode) "avoid" "\
Set cursor avoidance mode to MODE.
MODE should be one of the symbols `banish', `exile', `jump', `animate',
`cat-and-mouse', `proteus', or `none'.

If MODE is nil, toggle mouse avoidance between `none` and `banish'
modes.  Positive numbers and symbols other than the above are treated
as equivalent to `banish'; negative numbers and `-' are equivalent to `none'.

Effects of the different modes: 
 * banish: Move the mouse to the upper-right corner on any keypress.
 * exile: Move the mouse to the corner only if the cursor gets too close,
     and allow it to return once the cursor is out of the way.
 * jump: If the cursor gets too close to the mouse, displace the mouse
     a random distance & direction.
 * animate: As `jump', but shows steps along the way for illusion of motion.
 * cat-and-mouse: Same as `animate'.
 * proteus: As `animate', but changes the shape of the mouse pointer too.

Whenever the mouse is moved, the frame is also raised.

\(see `mouse-avoidance-threshold' for definition of \"too close\",
and `mouse-avoidance-nudge-dist' and `mouse-avoidance-nudge-var' for
definition of \"random distance\".)" t nil)

;;;***

;;;### (autoloads (awk-mode) "awk-mode" "progmodes/awk-mode.el" (13549
;;;;;;  38922))
;;; Generated autoloads from progmodes/awk-mode.el

(autoload (quote awk-mode) "awk-mode" "\
Major mode for editing AWK code.
This is much like C mode except for the syntax of comments.  It uses
the same keymap as C mode and has the same variables for customizing
indentation.  It has its own abbrev table and its own syntax table.

Turning on AWK mode calls the value of the variable `awk-mode-hook'
with no args, if that value is non-nil." t nil)

;;;***

;;;### (autoloads (backquote) "backquote" "emacs-lisp/backquote.el"
;;;;;;  (13607 51085))
;;; Generated autoloads from emacs-lisp/backquote.el

(autoload (quote backquote) "backquote" "\
Argument STRUCTURE describes a template to build.

The whole structure acts as if it were quoted except for certain
places where expressions are evaluated and inserted or spliced in.

For example:

b              => (ba bb bc)		; assume b has this value
`(a b c)       => (a b c)		; backquote acts like quote
`(a ,b c)      => (a (ba bb bc) c)	; insert the value of b
`(a ,@b c)     => (a ba bb bc c)	; splice in the value of b

Vectors work just like lists.  Nested backquotes are permitted." nil (quote macro))

(defalias (quote \`) (symbol-function (quote backquote)))

;;;***

;;;### (autoloads (display-battery battery) "battery" "battery.el"
;;;;;;  (13599 8260))
;;; Generated autoloads from battery.el

(autoload (quote battery) "battery" "\
Display battery status information in the echo area.
The text beeing displayed in the echo area is controlled by the variables
`battery-echo-area-format' and `battery-status-function'." t nil)

(autoload (quote display-battery) "battery" "\
Display battery status information in the mode line.
The text beeing displayed in the mode line is controlled by the variables
`battery-mode-line-format' and `battery-status-function'.
The mode line will be updated automatically every `battery-update-interval'
seconds." t nil)

;;;***

;;;### (autoloads (bibtex-mode) "bibtex" "textmodes/bibtex.el" (14198
;;;;;;  46296))
;;; Generated autoloads from textmodes/bibtex.el

(autoload (quote bibtex-mode) "bibtex" "\
Major mode for editing BibTeX files.

To submit a problem report, enter \\[bibtex-submit-bug-report] from a
BibTeX mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducable test case and send the
message.


General information on working with BibTeX mode:

You should use commands as \\[bibtex-Book] to get a template for a
specific entry. You should then fill in all desired fields using
\\[bibtex-next-field] to jump from field to field. After having filled
in all desired fields in the entry, you should clean the new entry
with command \\[bibtex-clean-entry].

Some features of BibTeX mode are available only by setting variable
bibtex-maintain-sorted-entries to t. However, then BibTeX mode will
work with buffer containing only valid (syntactical correct) entries
and with entries being sorted. This is usually the case, if you have
created a buffer completely with BibTeX mode and finished every new
entry with \\[bibtex-clean-entry].

For third party BibTeX buffers, please call the function
`bibtex-convert-alien' to fully take advantage of all features of
BibTeX mode.


Special information:

A command such as \\[bibtex-Book] will outline the fields for a BibTeX book entry.

The optional fields start with the string OPT, and are thus ignored by BibTeX.
Alternatives from which only one is required start with the string ALT.
The OPT or ALT string may be removed from a field with \\[bibtex-remove-OPT-or-ALT].
\\[bibtex-make-field] inserts a new field after the current one.
\\[bibtex-kill-field] kills the current field entirely.
\\[bibtex-yank] will yank the last recently killed field after the
current field.
\\[bibtex-remove-delimiters] removes the double-quotes or braces around the text of the current field.
 \\[bibtex-empty-field] replaces the text of the current field with the default \"\" or {}.

The command \\[bibtex-clean-entry] cleans the current entry, i.e. it removes OPT/ALT
from all non-empty optional or alternative fields, checks that no required
fields are empty, and does some formatting dependent on the value of
bibtex-entry-format.
Note: some functions in BibTeX mode depend on entries being in a special
format (all fields beginning on separate lines), so it is usually a bad
idea to remove `realign' from bibtex-entry-format.

Use \\[bibtex-find-text] to position the cursor at the end of the current field.
Use \\[bibtex-next-field] to move to end of the next field.

The following may be of interest as well:

  Functions:
    bibtex-entry
    bibtex-kill-entry
    bibtex-yank-pop
    bibtex-pop-previous
    bibtex-pop-next
    bibtex-complete-string
    bibtex-complete-key
    bibtex-print-help-message
    bibtex-generate-autokey
    bibtex-beginning-of-entry
    bibtex-end-of-entry
    bibtex-reposition-window
    bibtex-mark-entry
    bibtex-ispell-abstract
    bibtex-ispell-entry
    bibtex-narrow-to-entry
    bibtex-hide-entry-bodies
    bibtex-sort-buffer
    bibtex-validate
    bibtex-count
    bibtex-fill-entry
    bibtex-reformat
    bibtex-convert-alien

  Variables:
    bibtex-field-delimiters
    bibtex-include-OPTcrossref
    bibtex-include-OPTkey
    bibtex-user-optional-fields
    bibtex-entry-format
    bibtex-sort-ignore-string-entries
    bibtex-maintain-sorted-entries
    bibtex-entry-field-alist
    bibtex-predefined-strings
    bibtex-string-files

---------------------------------------------------------
Entry to BibTeX mode calls the value of `bibtex-mode-hook' if that value is
non-nil.

\\{bibtex-mode-map}" t nil)

;;;***

;;;### (autoloads (blackbox) "blackbox" "play/blackbox.el" (13229
;;;;;;  27943))
;;; Generated autoloads from play/blackbox.el

(autoload (quote blackbox) "blackbox" "\
Play blackbox.  Optional prefix argument is the number of balls;
the default is 4.

What is blackbox?

Blackbox is a game of hide and seek played on an 8 by 8 grid (the
Blackbox).  Your opponent (Emacs, in this case) has hidden several
balls (usually 4) within this box.  By shooting rays into the box and
observing where they emerge it is possible to deduce the positions of
the hidden balls.  The fewer rays you use to find the balls, the lower
your score.

Overview of play:

\\<blackbox-mode-map>To play blackbox, type \\[blackbox].  An optional prefix argument
specifies the number of balls to be hidden in the box; the default is
four.

The cursor can be moved around the box with the standard cursor
movement keys.

To shoot a ray, move the cursor to the edge of the box and press SPC.
The result will be determined and the playfield updated.

You may place or remove balls in the box by moving the cursor into the
box and pressing \\[bb-romp].

When you think the configuration of balls you have placed is correct,
press \\[bb-done].  You will be informed whether you are correct or
not, and be given your score.  Your score is the number of letters and
numbers around the outside of the box plus five for each incorrectly
placed ball.  If you placed any balls incorrectly, they will be
indicated with `x', and their actual positions indicated with `o'.

Details:

There are three possible outcomes for each ray you send into the box:

	Detour: the ray is deflected and emerges somewhere other than
		where you sent it in.  On the playfield, detours are
		denoted by matching pairs of numbers -- one where the
		ray went in, and the other where it came out.

	Reflection: the ray is reflected and emerges in the same place
		it was sent in.  On the playfield, reflections are
		denoted by the letter `R'.

	Hit:	the ray strikes a ball directly and is absorbed.  It does
		not emerge from the box.  On the playfield, hits are
		denoted by the letter `H'.

The rules for how balls deflect rays are simple and are best shown by
example.

As a ray approaches a ball it is deflected ninety degrees.  Rays can
be deflected multiple times.  In the diagrams below, the dashes
represent empty box locations and the letter `O' represents a ball.
The entrance and exit points of each ray are marked with numbers as
described under \"Detour\" above.  Note that the entrance and exit
points are always interchangeable.  `*' denotes the path taken by the
ray.

Note carefully the relative positions of the ball and the ninety
degree deflection it causes.

    1                                            
  - * - - - - - -         - - - - - - - -         - - - - - - - -       
  - * - - - - - -         - - - - - - - -         - - - - - - - -       
1 * * - - - - - -         - - - - - - - -         - O - - - - O -       
  - - O - - - - -         - - O - - - - -         - - * * * * - -
  - - - - - - - -         - - - * * * * * 2     3 * * * - - * - -
  - - - - - - - -         - - - * - - - -         - - - O - * - -      
  - - - - - - - -         - - - * - - - -         - - - - * * - -       
  - - - - - - - -         - - - * - - - -         - - - - * - O -       
                                2                         3

As mentioned above, a reflection occurs when a ray emerges from the same point
it was sent in.  This can happen in several ways:

                                                                           
  - - - - - - - -         - - - - - - - -          - - - - - - - -
  - - - - O - - -         - - O - O - - -          - - - - - - - -
R * * * * - - - -         - - - * - - - -          O - - - - - - -
  - - - - O - - -         - - - * - - - -        R - - - - - - - -
  - - - - - - - -         - - - * - - - -          - - - - - - - -
  - - - - - - - -         - - - * - - - -          - - - - - - - -
  - - - - - - - -       R * * * * - - - -          - - - - - - - -
  - - - - - - - -         - - - - O - - -          - - - - - - - -

In the first example, the ray is deflected downwards by the upper
ball, then left by the lower ball, and finally retraces its path to
its point of origin.  The second example is similar.  The third
example is a bit anomalous but can be rationalized by realizing the
ray never gets a chance to get into the box.  Alternatively, the ray
can be thought of as being deflected downwards and immediately
emerging from the box.

A hit occurs when a ray runs straight into a ball:

  - - - - - - - -         - - - - - - - -          - - - - - - - -
  - - - - - - - -         - - - - - - - -          - - - - O - - -
  - - - - - - - -         - - - - O - - -        H * * * * - - - -
  - - - - - - - -       H * * * * O - - -          - - - * - - - -
  - - - - - - - -         - - - - O - - -          - - - O - - - -
H * * * O - - - -         - - - - - - - -          - - - - - - - -
  - - - - - - - -         - - - - - - - -          - - - - - - - -
  - - - - - - - -         - - - - - - - -          - - - - - - - -

Be sure to compare the second example of a hit with the first example of
a reflection." t nil)

;;;***

;;;### (autoloads (bookmark-menu-delete bookmark-menu-rename bookmark-menu-locate
;;;;;;  bookmark-menu-jump bookmark-menu-insert bookmark-bmenu-list
;;;;;;  bookmark-load bookmark-save bookmark-write bookmark-delete
;;;;;;  bookmark-insert bookmark-rename bookmark-insert-location
;;;;;;  bookmark-relocate bookmark-jump bookmark-set) "bookmark"
;;;;;;  "bookmark.el" (14143 29124))
;;; Generated autoloads from bookmark.el
 (define-key ctl-x-map "rb" 'bookmark-jump)
 (define-key ctl-x-map "rm" 'bookmark-set)
 (define-key ctl-x-map "rl" 'bookmark-bmenu-list)

(defvar bookmark-map nil "\
Keymap containing bindings to bookmark functions.
It is not bound to any key by default: to bind it
so that you have a bookmark prefix, just use `global-set-key' and bind a
key of your choice to `bookmark-map'.  All interactive bookmark
functions have a binding in this keymap.")

(define-prefix-command (quote bookmark-map))

(define-key bookmark-map "x" (quote bookmark-set))

(define-key bookmark-map "m" (quote bookmark-set))

(define-key bookmark-map "j" (quote bookmark-jump))

(define-key bookmark-map "g" (quote bookmark-jump))

(define-key bookmark-map "i" (quote bookmark-insert))

(define-key bookmark-map "e" (quote edit-bookmarks))

(define-key bookmark-map "f" (quote bookmark-insert-location))

(define-key bookmark-map "r" (quote bookmark-rename))

(define-key bookmark-map "d" (quote bookmark-delete))

(define-key bookmark-map "l" (quote bookmark-load))

(define-key bookmark-map "w" (quote bookmark-write))

(define-key bookmark-map "s" (quote bookmark-save))

(add-hook (quote kill-emacs-hook) (function (lambda nil (and (featurep (quote bookmark)) bookmark-alist (bookmark-time-to-save-p t) (bookmark-save)))))

(autoload (quote bookmark-set) "bookmark" "\
Set a bookmark named NAME inside a file.
If name is nil, then the user will be prompted.
With prefix arg, will not overwrite a bookmark that has the same name
as NAME if such a bookmark already exists, but instead will \"push\"
the new bookmark onto the bookmark alist.  Thus the most recently set
bookmark with name NAME would be the one in effect at any given time,
but the others are still there, should you decide to delete the most
recent one.

To yank words from the text of the buffer and use them as part of the
bookmark name, type C-w while setting a bookmark.  Successive C-w's
yank successive words.

Typing C-u inserts the name of the last bookmark used in the buffer
\(as an aid in using a single bookmark name to track your progress
through a large file).  If no bookmark was used, then C-u inserts the
name of the file being visited.

Use \\[bookmark-delete] to remove bookmarks (you give it a name,
and it removes only the first instance of a bookmark with that name from
the list of bookmarks.)" t nil)

(autoload (quote bookmark-jump) "bookmark" "\
Jump to bookmark BOOKMARK (a point in some file).  
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this.

If the file pointed to by BOOKMARK no longer exists, you will be asked
if you wish to give the bookmark a new location, and bookmark-jump
will then jump to the new location, as well as recording it in place
of the old one in the permanent bookmark record." t nil)

(autoload (quote bookmark-relocate) "bookmark" "\
Relocate BOOKMARK to another file (reading file name with minibuffer).
This makes an already existing bookmark point to that file, instead of
the one it used to point at.  Useful when a file has been renamed
after a bookmark was set in it." t nil)

(autoload (quote bookmark-insert-location) "bookmark" "\
Insert the name of the file associated with BOOKMARK.
Optional second arg NO-HISTORY means don't record this in the
minibuffer history list `bookmark-history'." t nil)

(defalias (quote bookmark-locate) (quote bookmark-insert-location))

(autoload (quote bookmark-rename) "bookmark" "\
Change the name of OLD bookmark to NEW name.
If called from keyboard, prompt for OLD and NEW.  If called from
menubar, select OLD from a menu and prompt for NEW.

If called from Lisp, prompt for NEW if only OLD was passed as an
argument.  If called with two strings, then no prompting is done.  You
must pass at least OLD when calling from Lisp.

While you are entering the new name, consecutive C-w's insert
consecutive words from the text of the buffer into the new bookmark
name." t nil)

(autoload (quote bookmark-insert) "bookmark" "\
Insert the text of the file pointed to by bookmark BOOKMARK.  
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this." t nil)

(autoload (quote bookmark-delete) "bookmark" "\
Delete BOOKMARK from the bookmark list.  
Removes only the first instance of a bookmark with that name.  If
there are one or more other bookmarks with the same name, they will
not be deleted.  Defaults to the \"current\" bookmark (that is, the
one most recently used in this file, if any).
Optional second arg BATCH means don't update the bookmark list buffer,
probably because we were called from there." t nil)

(autoload (quote bookmark-write) "bookmark" "\
Write bookmarks to a file (reading the file name with the minibuffer).
Don't use this in Lisp programs; use `bookmark-save' instead." t nil)

(autoload (quote bookmark-save) "bookmark" "\
Save currently defined bookmarks.
Saves by default in the file defined by the variable
`bookmark-default-file'.  With a prefix arg, save it in file FILE
\(second argument).

If you are calling this from Lisp, the two arguments are PREFIX-ARG
and FILE, and if you just want it to write to the default file, then
pass no arguments.  Or pass in nil and FILE, and it will save in FILE
instead.  If you pass in one argument, and it is non-nil, then the
user will be interactively queried for a file to save in.

When you want to load in the bookmarks from a file, use
`bookmark-load', \\[bookmark-load].  That function will prompt you
for a file, defaulting to the file defined by variable
`bookmark-default-file'." t nil)

(autoload (quote bookmark-load) "bookmark" "\
Load bookmarks from FILE (which must be in bookmark format).
Appends loaded bookmarks to the front of the list of bookmarks.  If
optional second argument OVERWRITE is non-nil, existing bookmarks are
destroyed.  Optional third arg NO-MSG means don't display any messages
while loading.

If you load a file that doesn't contain a proper bookmark alist, you
will corrupt Emacs's bookmark list.  Generally, you should only load
in files that were created with the bookmark functions in the first
place.  Your own personal bookmark file, `~/.emacs.bmk', is
maintained automatically by Emacs; you shouldn't need to load it
explicitly.

If you load a file containing bookmarks with the same names as
bookmarks already present in your Emacs, the new bookmarks will get
unique numeric suffixes \"<2>\", \"<3>\", ... following the same
method buffers use to resolve name collisions." t nil)

(autoload (quote bookmark-bmenu-list) "bookmark" "\
Display a list of existing bookmarks.
The list is displayed in a buffer named `*Bookmark List*'.
The leftmost column displays a D if the bookmark is flagged for
deletion, or > if it is flagged for displaying." t nil)

(defalias (quote list-bookmarks) (quote bookmark-bmenu-list))

(defalias (quote edit-bookmarks) (quote bookmark-bmenu-list))

(autoload (quote bookmark-menu-insert) "bookmark" "\
Insert the text of the file pointed to by bookmark BOOKMARK.  
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this.

Warning: this function only takes an EVENT as argument.  Use the
corresponding bookmark function from Lisp (the one without the
\"-menu-\" in its name)." t nil)

(autoload (quote bookmark-menu-jump) "bookmark" "\
Jump to bookmark BOOKMARK (a point in some file).  
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this.

Warning: this function only takes an EVENT as argument.  Use the
corresponding bookmark function from Lisp (the one without the
\"-menu-\" in its name)." t nil)

(autoload (quote bookmark-menu-locate) "bookmark" "\
Insert the name of the file associated with BOOKMARK. 
\(This is not the same as the contents of that file).

Warning: this function only takes an EVENT as argument.  Use the
corresponding bookmark function from Lisp (the one without the
\"-menu-\" in its name)." t nil)

(autoload (quote bookmark-menu-rename) "bookmark" "\
Change the name of OLD-BOOKMARK to NEWNAME.  
If called from keyboard, prompts for OLD-BOOKMARK and NEWNAME.
If called from menubar, OLD-BOOKMARK is selected from a menu, and
prompts for NEWNAME. 
If called from Lisp, prompts for NEWNAME if only OLD-BOOKMARK was
passed as an argument.  If called with two strings, then no prompting
is done.  You must pass at least OLD-BOOKMARK when calling from Lisp.

While you are entering the new name, consecutive C-w's insert
consecutive words from the text of the buffer into the new bookmark
name.

Warning: this function only takes an EVENT as argument.  Use the
corresponding bookmark function from Lisp (the one without the
\"-menu-\" in its name)." t nil)

(autoload (quote bookmark-menu-delete) "bookmark" "\
Delete the bookmark named NAME from the bookmark list.  
Removes only the first instance of a bookmark with that name.  If
there are one or more other bookmarks with the same name, they will
not be deleted.  Defaults to the \"current\" bookmark (that is, the
one most recently used in this file, if any).

Warning: this function only takes an EVENT as argument.  Use the
corresponding bookmark function from Lisp (the one without the
\"-menu-\" in its name)." t nil)

(defvar menu-bar-bookmark-map (make-sparse-keymap "Bookmark functions"))

(defalias (quote menu-bar-bookmark-map) (symbol-value (quote menu-bar-bookmark-map)))

(define-key menu-bar-bookmark-map [load] (quote ("Load a Bookmark File..." . bookmark-load)))

(define-key menu-bar-bookmark-map [write] (quote ("Save Bookmarks As..." . bookmark-write)))

(define-key menu-bar-bookmark-map [save] (quote ("Save Bookmarks" . bookmark-save)))

(define-key menu-bar-bookmark-map [edit] (quote ("Edit Bookmark List" . bookmark-bmenu-list)))

(define-key menu-bar-bookmark-map [delete] (quote ("Delete Bookmark" . bookmark-menu-delete)))

(define-key menu-bar-bookmark-map [rename] (quote ("Rename Bookmark" . bookmark-menu-rename)))

(define-key menu-bar-bookmark-map [locate] (quote ("Insert Location" . bookmark-menu-locate)))

(define-key menu-bar-bookmark-map [insert] (quote ("Insert Contents" . bookmark-menu-insert)))

(define-key menu-bar-bookmark-map [set] (quote ("Set Bookmark" . bookmark-set)))

(define-key menu-bar-bookmark-map [jump] (quote ("Jump to Bookmark" . bookmark-menu-jump)))

;;;***

;;;### (autoloads (browse-url-generic browse-url-mail browse-url-mmm
;;;;;;  browse-url-lynx-emacs browse-url-lynx-xterm browse-url-w3-gnudoit
;;;;;;  browse-url-w3 browse-url-iximosaic browse-url-cci browse-url-grail
;;;;;;  browse-url-mosaic browse-url-netscape browse-url-at-mouse
;;;;;;  browse-url-at-point browse-url browse-url-of-region browse-url-of-dired-file
;;;;;;  browse-url-of-buffer browse-url-of-file browse-url-generic-program
;;;;;;  browse-url-save-file browse-url-netscape-display browse-url-new-window-p
;;;;;;  browse-url-browser-function) "browse-url" "browse-url.el"
;;;;;;  (14124 22133))
;;; Generated autoloads from browse-url.el

(defvar browse-url-browser-function (if (eq system-type (quote windows-nt)) (quote browse-url-default-windows-browser) (quote browse-url-netscape)) "\
*Function to display the current buffer in a WWW browser.
This is used by the `browse-url-at-point', `browse-url-at-mouse', and
`browse-url-of-file' commands.

If the value is not a function it should be a list of pairs
\(REGEXP . FUNCTION).  In this case the function called will be the one
associated with the first REGEXP which matches the current URL.  The
function is passed the URL and any other args of `browse-url'.  The last
regexp should probably be \".\" to specify a default browser.")

(defvar browse-url-new-window-p nil "\
*If non-nil, always open a new browser window with appropriate browsers.
Passing an interactive argument to \\[browse-url], or specific browser
commands reverses the effect of this variable.  Requires Netscape version
1.1N or later or XMosaic version 2.5 or later if using those browsers.")

(defvar browse-url-netscape-display nil "\
*The X display for running Netscape, if not same as Emacs'.")

(defvar browse-url-save-file nil "\
*If non-nil, save the buffer before displaying its file.
Used by the `browse-url-of-file' command.")

(defvar browse-url-generic-program nil "\
*The name of the browser program used by `browse-url-generic'.")

(autoload (quote browse-url-of-file) "browse-url" "\
Ask a WWW browser to display FILE.
Display the current buffer's file if FILE is nil or if called
interactively.  Turn the filename into a URL with function
`browse-url-file-url'.  Pass the URL to a browser using the
`browse-url' function then run `browse-url-of-file-hook'." t nil)

(autoload (quote browse-url-of-buffer) "browse-url" "\
Ask a WWW browser to display BUFFER.
Display the current buffer if BUFFER is nil.  Display only the
currently visible part of BUFFER (from a temporary file) if buffer is
narrowed." t nil)

(autoload (quote browse-url-of-dired-file) "browse-url" "\
In Dired, ask a WWW browser to display the file named on this line." t nil)

(autoload (quote browse-url-of-region) "browse-url" "\
Ask a WWW browser to display the current region." t nil)

(autoload (quote browse-url) "browse-url" "\
Ask a WWW browser to load URL.
Prompts for a URL, defaulting to the URL at or before point.  Variable
`browse-url-browser-function' says which browser to use." t nil)

(autoload (quote browse-url-at-point) "browse-url" "\
Ask a WWW browser to load the URL at or before point.
Doesn't let you edit the URL like `browse-url'.  Variable
`browse-url-browser-function' says which browser to use." t nil)

(autoload (quote browse-url-at-mouse) "browse-url" "\
Ask a WWW browser to load a URL clicked with the mouse.
The URL is the one around or before the position of the mouse click
but point is not changed.  Doesn't let you edit the URL like
`browse-url'.  Variable `browse-url-browser-function' says which browser
to use." t nil)

(autoload (quote browse-url-netscape) "browse-url" "\
Ask the Netscape WWW browser to load URL.

Default to the URL around or before point.  The strings in variable
`browse-url-netscape-arguments' are also passed to Netscape.

When called interactively, if variable `browse-url-new-window-p' is
non-nil, load the document in a new Netscape window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-p'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-p'." t nil)

(autoload (quote browse-url-mosaic) "browse-url" "\
Ask the XMosaic WWW browser to load URL.

Default to the URL around or before point.  The strings in variable
`browse-url-mosaic-arguments' are also passed to Mosaic and the
program is invoked according to the variable
`browse-url-mosaic-program'.

When called interactively, if variable `browse-url-new-window-p' is
non-nil, load the document in a new Mosaic window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-p'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-p'." t nil)

(defvar browse-url-grail (concat (or (getenv "GRAILDIR") "~/.grail") "/user/rcgrail.py") "\
Location of Grail remote control client script `rcgrail.py'.
Typically found in $GRAILDIR/rcgrail.py, or ~/.grail/user/rcgrail.py.")

(autoload (quote browse-url-grail) "browse-url" "\
Ask the Grail WWW browser to load URL.
Default to the URL around or before point.  Runs the program in the
variable `browse-url-grail'." t nil)

(autoload (quote browse-url-cci) "browse-url" "\
Ask the XMosaic WWW browser to load URL.
Default to the URL around or before point.

This function only works for XMosaic version 2.5 or later.  You must
select `CCI' from XMosaic's File menu, set the CCI Port Address to the
value of variable `browse-url-CCI-port', and enable `Accept requests'.

When called interactively, if variable `browse-url-new-window-p' is
non-nil, load the document in a new browser window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-p'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-p'." t nil)

(autoload (quote browse-url-iximosaic) "browse-url" "\
Ask the IXIMosaic WWW browser to load URL.
Default to the URL around or before point." t nil)

(autoload (quote browse-url-w3) "browse-url" "\
Ask the w3 WWW browser to load URL.
Default to the URL around or before point.

When called interactively, if variable `browse-url-new-window-p' is
non-nil, load the document in a new window.  A non-nil interactive
prefix argument reverses the effect of `browse-url-new-window-p'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-p'." t nil)

(autoload (quote browse-url-w3-gnudoit) "browse-url" "\
Ask another Emacs running gnuserv to load the URL using the W3 browser.
The `browse-url-gnudoit-program' program is used with options given by
`browse-url-gnudoit-args'.  Default to the URL around or before point." t nil)

(autoload (quote browse-url-lynx-xterm) "browse-url" "\
Ask the Lynx WWW browser to load URL.
Default to the URL around or before point.  A new Lynx process is run
in an Xterm window using the Xterm program named by `browse-url-xterm-program'
with possible additional arguments `browse-url-xterm-args'." t nil)

(autoload (quote browse-url-lynx-emacs) "browse-url" "\
Ask the Lynx WWW browser to load URL.
Default to the URL around or before point.  With a prefix argument, run
a new Lynx process in a new buffer.

When called interactively, if variable `browse-url-new-window-p' is
non-nil, load the document in a new lynx in a new term window,
otherwise use any existing one.  A non-nil interactive prefix argument
reverses the effect of `browse-url-new-window-p'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-p'." t nil)

(autoload (quote browse-url-mmm) "browse-url" "\
Ask the MMM WWW browser to load URL.
Default to the URL around or before point." t nil)

(autoload (quote browse-url-mail) "browse-url" "\
Open a new mail message buffer within Emacs.
Default to using the mailto: URL around or before point as the
recipient's address.  Supplying a non-nil interactive prefix argument
will cause the mail to be composed in another window rather than the
current one.

When called interactively, if variable `browse-url-new-window-p' is
non-nil use `compose-mail-other-window', otherwise `compose-mail'.  A
non-nil interactive prefix argument reverses the effect of
`browse-url-new-window-p'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-p'." t nil)

(autoload (quote browse-url-generic) "browse-url" "\
Ask the WWW browser defined by `browse-url-generic-program' to load URL.
Default to the URL around or before point.  A fresh copy of the
browser is started up in a new process with possible additional arguments
`browse-url-generic-args'.  This is appropriate for browsers which
don't offer a form of remote control." t nil)

;;;***

;;;### (autoloads (snarf-bruces bruce) "bruce" "play/bruce.el" (13607
;;;;;;  42463))
;;; Generated autoloads from play/bruce.el

(autoload (quote bruce) "bruce" "\
Adds that special touch of class to your outgoing mail." t nil)

(autoload (quote snarf-bruces) "bruce" "\
Return a vector containing the lines from `bruce-phrases-file'." nil nil)

;;;***

;;;### (autoloads (batch-byte-recompile-directory batch-byte-compile
;;;;;;  display-call-tree byte-compile compile-defun byte-compile-file
;;;;;;  byte-recompile-directory byte-force-recompile) "bytecomp"
;;;;;;  "emacs-lisp/bytecomp.el" (14090 25925))
;;; Generated autoloads from emacs-lisp/bytecomp.el

(autoload (quote byte-force-recompile) "bytecomp" "\
Recompile every `.el' file in DIRECTORY that already has a `.elc' file.
Files in subdirectories of DIRECTORY are processed also." t nil)

(autoload (quote byte-recompile-directory) "bytecomp" "\
Recompile every `.el' file in DIRECTORY that needs recompilation.
This is if a `.elc' file exists but is older than the `.el' file.
Files in subdirectories of DIRECTORY are processed also.

If the `.elc' file does not exist, normally the `.el' file is *not* compiled.
But a prefix argument (optional second arg) means ask user,
for each such `.el' file, whether to compile it.  Prefix argument 0 means
don't ask and compile the file anyway.

A nonzero prefix argument also means ask about each subdirectory.

If the third argument FORCE is non-nil,
recompile every `.el' file that already has a `.elc' file." t nil)

(autoload (quote byte-compile-file) "bytecomp" "\
Compile a file of Lisp code named FILENAME into a file of byte code.
The output file's name is made by appending `c' to the end of FILENAME.
With prefix arg (noninteractively: 2nd arg), load the file after compiling.
The value is t if there were no errors, nil if errors." t nil)

(autoload (quote compile-defun) "bytecomp" "\
Compile and evaluate the current top-level form.
Print the result in the minibuffer.
With argument, insert value in current buffer after the form." t nil)

(autoload (quote byte-compile) "bytecomp" "\
If FORM is a symbol, byte-compile its function definition.
If FORM is a lambda or a macro, byte-compile it as a function." nil nil)

(autoload (quote display-call-tree) "bytecomp" "\
Display a call graph of a specified file.
This lists which functions have been called, what functions called
them, and what functions they call.  The list includes all functions
whose definitions have been compiled in this Emacs session, as well as
all functions called by those functions.

The call graph does not include macros, inline functions, or
primitives that the byte-code interpreter knows about directly (eq,
cons, etc.).

The call tree also lists those functions which are not known to be called
\(that is, to which no calls have been compiled), and which cannot be
invoked interactively." t nil)

(autoload (quote batch-byte-compile) "bytecomp" "\
Run `byte-compile-file' on the files remaining on the command line.
Use this from the command line, with `-batch';
it won't work in an interactive Emacs.
Each file is processed even if an error occurred previously.
For example, invoke \"emacs -batch -f batch-byte-compile $emacs/ ~/*.el\"" nil nil)

(autoload (quote batch-byte-recompile-directory) "bytecomp" "\
Runs `byte-recompile-directory' on the dirs remaining on the command line.
Must be used only with `-batch', and kills Emacs on completion.
For example, invoke `emacs -batch -f batch-byte-recompile-directory .'." nil nil)

;;;***

;;;### (autoloads nil "cal-dst" "calendar/cal-dst.el" (12984 38822))
;;; Generated autoloads from calendar/cal-dst.el

(put (quote calendar-daylight-savings-starts) (quote risky-local-variable) t)

(put (quote calendar-daylight-savings-ends) (quote risky-local-variable) t)

;;;***

;;;### (autoloads (list-yahrzeit-dates) "cal-hebrew" "calendar/cal-hebrew.el"
;;;;;;  (13997 6380))
;;; Generated autoloads from calendar/cal-hebrew.el

(autoload (quote list-yahrzeit-dates) "cal-hebrew" "\
List Yahrzeit dates for *Gregorian* DEATH-DATE from START-YEAR to END-YEAR.
When called interactively from the calendar window, the date of death is taken
from the cursor position." t nil)

;;;***

;;;### (autoloads (calendar solar-holidays islamic-holidays christian-holidays
;;;;;;  hebrew-holidays other-holidays local-holidays oriental-holidays
;;;;;;  general-holidays holidays-in-diary-buffer diary-list-include-blanks
;;;;;;  nongregorian-diary-marking-hook mark-diary-entries-hook nongregorian-diary-listing-hook
;;;;;;  diary-display-hook diary-hook list-diary-entries-hook print-diary-entries-hook
;;;;;;  american-calendar-display-form european-calendar-display-form
;;;;;;  european-date-diary-pattern american-date-diary-pattern european-calendar-style
;;;;;;  abbreviated-calendar-year sexp-diary-entry-symbol diary-include-string
;;;;;;  islamic-diary-entry-symbol hebrew-diary-entry-symbol diary-nonmarking-symbol
;;;;;;  diary-file today-invisible-calendar-hook today-visible-calendar-hook
;;;;;;  initial-calendar-window-hook calendar-load-hook all-islamic-calendar-holidays
;;;;;;  all-christian-calendar-holidays all-hebrew-calendar-holidays
;;;;;;  mark-holidays-in-calendar view-calendar-holidays-initially
;;;;;;  mark-diary-entries-in-calendar number-of-diary-entries view-diary-entries-initially
;;;;;;  calendar-offset calendar-week-start-day) "calendar" "calendar/calendar.el"
;;;;;;  (14198 38416))
;;; Generated autoloads from calendar/calendar.el

(defvar calendar-week-start-day 0 "\
*The day of the week on which a week in the calendar begins.
0 means Sunday (default), 1 means Monday, and so on.")

(defvar calendar-offset 0 "\
*The offset of the principal month from the center of the calendar window.
0 means the principal month is in the center (default), -1 means on the left,
+1 means on the right.  Larger (or smaller) values push the principal month off
the screen.")

(defvar view-diary-entries-initially nil "\
*Non-nil means display current date's diary entries on entry.
The diary is displayed in another window when the calendar is first displayed,
if the current date is visible.  The number of days of diary entries displayed
is governed by the variable `number-of-diary-entries'.")

(defvar number-of-diary-entries 1 "\
*Specifies how many days of diary entries are to be displayed initially.
This variable affects the diary display when the command M-x diary is used,
or if the value of the variable `view-diary-entries-initially' is t.  For
example, if the default value 1 is used, then only the current day's diary
entries will be displayed.  If the value 2 is used, then both the current
day's and the next day's entries will be displayed.

The value can also be a vector such as [0 2 2 2 2 4 1]; this value
says to display no diary entries on Sunday, the display the entries
for the current date and the day after on Monday through Thursday,
display Friday through Monday's entries on Friday, and display only
Saturday's entries on Saturday.

This variable does not affect the diary display with the `d' command
from the calendar; in that case, the prefix argument controls the
number of days of diary entries displayed.")

(defvar mark-diary-entries-in-calendar nil "\
*Non-nil means mark dates with diary entries, in the calendar window.
The marking symbol is specified by the variable `diary-entry-marker'.")

(defvar view-calendar-holidays-initially nil "\
*Non-nil means display holidays for current three month period on entry.
The holidays are displayed in another window when the calendar is first
displayed.")

(defvar mark-holidays-in-calendar nil "\
*Non-nil means mark dates of holidays in the calendar window.
The marking symbol is specified by the variable `calendar-holiday-marker'.")

(defvar all-hebrew-calendar-holidays nil "\
*If nil, show only major holidays from the Hebrew calendar.
This means only those Jewish holidays that appear on secular calendars.

If t, show all the holidays that would appear in a complete Hebrew calendar.")

(defvar all-christian-calendar-holidays nil "\
*If nil, show only major holidays from the Christian calendar.
This means only those Christian holidays that appear on secular calendars.

If t, show all the holidays that would appear in a complete Christian
calendar.")

(defvar all-islamic-calendar-holidays nil "\
*If nil, show only major holidays from the Islamic calendar.
This means only those Islamic holidays that appear on secular calendars.

If t, show all the holidays that would appear in a complete Islamic
calendar.")

(defvar calendar-load-hook nil "\
*List of functions to be called after the calendar is first loaded.
This is the place to add key bindings to `calendar-mode-map'.")

(defvar initial-calendar-window-hook nil "\
*List of functions to be called when the calendar window is first opened.
The functions invoked are called after the calendar window is opened, but
once opened is never called again.  Leaving the calendar with the `q' command
and reentering it will cause these functions to be called again.")

(defvar today-visible-calendar-hook nil "\
*List of functions called whenever the current date is visible.
This can be used, for example, to replace today's date with asterisks; a
function `calendar-star-date' is included for this purpose:
    (setq today-visible-calendar-hook 'calendar-star-date)
It can also be used to mark the current date with `calendar-today-marker';
a function is also provided for this:
    (setq today-visible-calendar-hook 'calendar-mark-today)

The corresponding variable `today-invisible-calendar-hook' is the list of
functions called when the calendar function was called when the current
date is not visible in the window.

Other than the use of the provided functions, the changing of any
characters in the calendar buffer by the hooks may cause the failure of the
functions that move by days and weeks.")

(defvar today-invisible-calendar-hook nil "\
*List of functions called whenever the current date is not visible.

The corresponding variable `today-visible-calendar-hook' is the list of
functions called when the calendar function was called when the current
date is visible in the window.

Other than the use of the provided functions, the changing of any
characters in the calendar buffer by the hooks may cause the failure of the
functions that move by days and weeks.")

(defvar diary-file "~/diary" "\
*Name of the file in which one's personal diary of dates is kept.

The file's entries are lines in any of the forms

            MONTH/DAY
            MONTH/DAY/YEAR
            MONTHNAME DAY
            MONTHNAME DAY, YEAR
            DAYNAME

at the beginning of the line; the remainder of the line is the diary entry
string for that date.  MONTH and DAY are one or two digit numbers, YEAR is
a number and may be written in full or abbreviated to the final two digits.
If the date does not contain a year, it is generic and applies to any year.
DAYNAME entries apply to any date on which is on that day of the week.
MONTHNAME and DAYNAME can be spelled in full, abbreviated to three
characters (with or without a period), capitalized or not.  Any of DAY,
MONTH, or MONTHNAME, YEAR can be `*' which matches any day, month, or year,
respectively.

The European style (in which the day precedes the month) can be used
instead, if you execute `european-calendar' when in the calendar, or set
`european-calendar-style' to t in your .emacs file.  The European forms are

            DAY/MONTH
            DAY/MONTH/YEAR
            DAY MONTHNAME
            DAY MONTHNAME YEAR
            DAYNAME

To revert to the default American style from the European style, execute
`american-calendar' in the calendar.

A diary entry can be preceded by the character
`diary-nonmarking-symbol' (ordinarily `&') to make that entry
nonmarking--that is, it will not be marked on dates in the calendar
window but will appear in a diary window.

Multiline diary entries are made by indenting lines after the first with
either a TAB or one or more spaces.

Lines not in one the above formats are ignored.  Here are some sample diary
entries (in the default American style):

     12/22/1988 Twentieth wedding anniversary!!
     &1/1. Happy New Year!
     10/22 Ruth's birthday.
     21: Payday
     Tuesday--weekly meeting with grad students at 10am
              Supowit, Shen, Bitner, and Kapoor to attend.
     1/13/89 Friday the thirteenth!!
     &thu 4pm squash game with Lloyd.
     mar 16 Dad's birthday
     April 15, 1989 Income tax due.
     &* 15 time cards due.

If the first line of a diary entry consists only of the date or day name with
no trailing blanks or punctuation, then that line is not displayed in the
diary window; only the continuation lines is shown.  For example, the
single diary entry

     02/11/1989
      Bill Blattner visits Princeton today
      2pm Cognitive Studies Committee meeting
      2:30-5:30 Lizzie at Lawrenceville for `Group Initiative'
      4:00pm Jamie Tappenden
      7:30pm Dinner at George and Ed's for Alan Ryan
      7:30-10:00pm dance at Stewart Country Day School

will appear in the diary window without the date line at the beginning.  This
facility allows the diary window to look neater, but can cause confusion if
used with more than one day's entries displayed.

Diary entries can be based on Lisp sexps.  For example, the diary entry

      %%(diary-block 11 1 1990 11 10 1990) Vacation

causes the diary entry \"Vacation\" to appear from November 1 through November
10, 1990.  Other functions available are `diary-float', `diary-anniversary',
`diary-cyclic', `diary-day-of-year', `diary-iso-date', `diary-french-date',
`diary-hebrew-date', `diary-islamic-date', `diary-mayan-date',
`diary-chinese-date', `diary-coptic-date', `diary-ethiopic-date',
`diary-persian-date', `diary-yahrzeit', `diary-sunrise-sunset',
`diary-phases-of-moon', `diary-parasha', `diary-omer', `diary-rosh-hodesh',
and `diary-sabbath-candles'.  See the documentation for the function
`list-sexp-diary-entries' for more details.

Diary entries based on the Hebrew and/or the Islamic calendar are also
possible, but because these are somewhat slow, they are ignored
unless you set the `nongregorian-diary-listing-hook' and the
`nongregorian-diary-marking-hook' appropriately.  See the documentation
for these functions for details.

Diary files can contain directives to include the contents of other files; for
details, see the documentation for the variable `list-diary-entries-hook'.")

(defvar diary-nonmarking-symbol "&" "\
*Symbol indicating that a diary entry is not to be marked in the calendar.")

(defvar hebrew-diary-entry-symbol "H" "\
*Symbol indicating a diary entry according to the Hebrew calendar.")

(defvar islamic-diary-entry-symbol "I" "\
*Symbol indicating a diary entry according to the Islamic calendar.")

(defvar diary-include-string "#include" "\
*The string indicating inclusion of another file of diary entries.
See the documentation for the function `include-other-diary-files'.")

(defvar sexp-diary-entry-symbol "%%" "\
*The string used to indicate a sexp diary entry in diary-file.
See the documentation for the function `list-sexp-diary-entries'.")

(defvar abbreviated-calendar-year t "\
*Interpret a two-digit year DD in a diary entry as either 19DD or 20DD.
For the Gregorian calendar; similarly for the Hebrew and Islamic calendars.
If this variable is nil, years must be written in full.")

(defvar european-calendar-style nil "\
*Use the European style of dates in the diary and in any displays.
If this variable is t, a date 1/2/1990 would be interpreted as February 1,
1990.  The accepted European date styles are

            DAY/MONTH
            DAY/MONTH/YEAR
            DAY MONTHNAME
            DAY MONTHNAME YEAR
            DAYNAME

Names can be capitalized or not, written in full, or abbreviated to three
characters with or without a period.")

(defvar american-date-diary-pattern (quote ((month "/" day "[^/0-9]") (month "/" day "/" year "[^0-9]") (monthname " *" day "[^,0-9]") (monthname " *" day ", *" year "[^0-9]") (dayname "\\W"))) "\
*List of pseudo-patterns describing the American patterns of date used.
See the documentation of `diary-date-forms' for an explanation.")

(defvar european-date-diary-pattern (quote ((day "/" month "[^/0-9]") (day "/" month "/" year "[^0-9]") (backup day " *" monthname "\\W+\\<[^*0-9]") (day " *" monthname " *" year "[^0-9]") (dayname "\\W"))) "\
*List of pseudo-patterns describing the European patterns of date used.
See the documentation of `diary-date-forms' for an explanation.")

(defvar european-calendar-display-form (quote ((if dayname (concat dayname ", ")) day " " monthname " " year)) "\
*Pseudo-pattern governing the way a date appears in the European style.
See the documentation of calendar-date-display-form for an explanation.")

(defvar american-calendar-display-form (quote ((if dayname (concat dayname ", ")) monthname " " day ", " year)) "\
*Pseudo-pattern governing the way a date appears in the American style.
See the documentation of `calendar-date-display-form' for an explanation.")

(defvar print-diary-entries-hook (quote lpr-buffer) "\
*List of functions called after a temporary diary buffer is prepared.
The buffer shows only the diary entries currently visible in the diary
buffer.  The default just does the printing.  Other uses might include, for
example, rearranging the lines into order by day and time, saving the buffer
instead of deleting it, or changing the function used to do the printing.")

(defvar list-diary-entries-hook nil "\
*List of functions called after diary file is culled for relevant entries.
It is to be used for diary entries that are not found in the diary file.

A function `include-other-diary-files' is provided for use as the value of
this hook.  This function enables you to use shared diary files together
with your own.  The files included are specified in the diary file by lines
of the form

        #include \"filename\"

This is recursive; that is, #include directives in files thus included are
obeyed.  You can change the \"#include\" to some other string by changing
the variable `diary-include-string'.  When you use `include-other-diary-files'
as part of the list-diary-entries-hook, you will probably also want to use the
function `mark-included-diary-files' as part of `mark-diary-entries-hook'.

For example, you could use

     (setq list-diary-entries-hook
       '(include-other-diary-files sort-diary-entries))
     (setq diary-display-hook 'fancy-diary-display)

in your `.emacs' file to cause the fancy diary buffer to be displayed with
diary entries from various included files, each day's entries sorted into
lexicographic order.")

(defvar diary-hook nil "\
*List of functions called after the display of the diary.
Can be used for appointment notification.")

(defvar diary-display-hook nil "\
*List of functions that handle the display of the diary.
If nil (the default), `simple-diary-display' is used.  Use `ignore' for no
diary display.

Ordinarily, this just displays the diary buffer (with holidays indicated in
the mode line), if there are any relevant entries.  At the time these
functions are called, the variable `diary-entries-list' is a list, in order
by date, of all relevant diary entries in the form of ((MONTH DAY YEAR)
STRING), where string is the diary entry for the given date.  This can be
used, for example, a different buffer for display (perhaps combined with
holidays), or produce hard copy output.

A function `fancy-diary-display' is provided as an alternative
choice for this hook; this function prepares a special noneditable diary
buffer with the relevant diary entries that has neat day-by-day arrangement
with headings.  The fancy diary buffer will show the holidays unless the
variable `holidays-in-diary-buffer' is set to nil.  Ordinarily, the fancy
diary buffer will not show days for which there are no diary entries, even
if that day is a holiday; if you want such days to be shown in the fancy
diary buffer, set the variable `diary-list-include-blanks' to t.")

(defvar nongregorian-diary-listing-hook nil "\
*List of functions called for listing diary file and included files.
As the files are processed for diary entries, these functions are used to cull
relevant entries.  You can use either or both of `list-hebrew-diary-entries'
and `list-islamic-diary-entries'.  The documentation for these functions
describes the style of such diary entries.")

(defvar mark-diary-entries-hook nil "\
*List of functions called after marking diary entries in the calendar.

A function `mark-included-diary-files' is also provided for use as the
mark-diary-entries-hook; it enables you to use shared diary files together
with your own.  The files included are specified in the diary file by lines
of the form
        #include \"filename\"
This is recursive; that is, #include directives in files thus included are
obeyed.  You can change the \"#include\" to some other string by changing the
variable `diary-include-string'.  When you use `mark-included-diary-files' as
part of the mark-diary-entries-hook, you will probably also want to use the
function `include-other-diary-files' as part of `list-diary-entries-hook'.")

(defvar nongregorian-diary-marking-hook nil "\
*List of functions called for marking diary file and included files.
As the files are processed for diary entries, these functions are used to cull
relevant entries.  You can use either or both of `mark-hebrew-diary-entries'
and `mark-islamic-diary-entries'.  The documentation for these functions
describes the style of such diary entries.")

(defvar diary-list-include-blanks nil "\
*If nil, do not include days with no diary entry in the list of diary entries.
Such days will then not be shown in the fancy diary buffer, even if they
are holidays.")

(defvar holidays-in-diary-buffer t "\
*Non-nil means include holidays in the diary display.
The holidays appear in the mode line of the diary buffer, or in the
fancy diary buffer next to the date.  This slows down the diary functions
somewhat; setting it to nil makes the diary display faster.")

(put (quote general-holidays) (quote risky-local-variable) t)

(defvar general-holidays (quote ((holiday-fixed 1 1 "New Year's Day") (holiday-float 1 1 3 "Martin Luther King Day") (holiday-fixed 2 2 "Groundhog Day") (holiday-fixed 2 14 "Valentine's Day") (holiday-float 2 1 3 "President's Day") (holiday-fixed 3 17 "St. Patrick's Day") (holiday-fixed 4 1 "April Fools' Day") (holiday-float 5 0 2 "Mother's Day") (holiday-float 5 1 -1 "Memorial Day") (holiday-fixed 6 14 "Flag Day") (holiday-float 6 0 3 "Father's Day") (holiday-fixed 7 4 "Independence Day") (holiday-float 9 1 1 "Labor Day") (holiday-float 10 1 2 "Columbus Day") (holiday-fixed 10 31 "Halloween") (holiday-fixed 11 11 "Veteran's Day") (holiday-float 11 4 4 "Thanksgiving"))) "\
*General holidays.  Default value is for the United States.
See the documentation for `calendar-holidays' for details.")

(put (quote oriental-holidays) (quote risky-local-variable) t)

(defvar oriental-holidays (quote ((if (fboundp (quote atan)) (holiday-chinese-new-year)))) "\
*Oriental holidays.
See the documentation for `calendar-holidays' for details.")

(put (quote local-holidays) (quote risky-local-variable) t)

(defvar local-holidays nil "\
*Local holidays.
See the documentation for `calendar-holidays' for details.")

(put (quote other-holidays) (quote risky-local-variable) t)

(defvar other-holidays nil "\
*User defined holidays.
See the documentation for `calendar-holidays' for details.")

(put (quote hebrew-holidays-1) (quote risky-local-variable) t)

(defvar hebrew-holidays-1 (quote ((holiday-rosh-hashanah-etc) (if all-hebrew-calendar-holidays (holiday-julian 11 (let* ((m displayed-month) (y displayed-year) (year)) (increment-calendar-month m y -1) (let ((year (extract-calendar-year (calendar-julian-from-absolute (calendar-absolute-from-gregorian (list m 1 y)))))) (if (zerop (% (1+ year) 4)) 22 21))) "\"Tal Umatar\" (evening)")))))

(put (quote hebrew-holidays-2) (quote risky-local-variable) t)

(defvar hebrew-holidays-2 (quote ((if all-hebrew-calendar-holidays (holiday-hanukkah) (holiday-hebrew 9 25 "Hanukkah")) (if all-hebrew-calendar-holidays (holiday-hebrew 10 (let ((h-year (extract-calendar-year (calendar-hebrew-from-absolute (calendar-absolute-from-gregorian (list displayed-month 28 displayed-year)))))) (if (= (% (calendar-absolute-from-hebrew (list 10 10 h-year)) 7) 6) 11 10)) "Tzom Teveth")) (if all-hebrew-calendar-holidays (holiday-hebrew 11 15 "Tu B'Shevat")))))

(put (quote hebrew-holidays-3) (quote risky-local-variable) t)

(defvar hebrew-holidays-3 (quote ((if all-hebrew-calendar-holidays (holiday-hebrew 11 (let ((m displayed-month) (y displayed-year)) (increment-calendar-month m y 1) (let* ((h-year (extract-calendar-year (calendar-hebrew-from-absolute (calendar-absolute-from-gregorian (list m (calendar-last-day-of-month m y) y))))) (s-s (calendar-hebrew-from-absolute (if (= (% (calendar-absolute-from-hebrew (list 7 1 h-year)) 7) 6) (calendar-dayname-on-or-before 6 (calendar-absolute-from-hebrew (list 11 17 h-year))) (calendar-dayname-on-or-before 6 (calendar-absolute-from-hebrew (list 11 16 h-year)))))) (day (extract-calendar-day s-s))) day)) "Shabbat Shirah")))))

(put (quote hebrew-holidays-4) (quote risky-local-variable) t)

(defvar hebrew-holidays-4 (quote ((holiday-passover-etc) (if (and all-hebrew-calendar-holidays (let* ((m displayed-month) (y displayed-year) (year)) (increment-calendar-month m y -1) (let ((year (extract-calendar-year (calendar-julian-from-absolute (calendar-absolute-from-gregorian (list m 1 y)))))) (= 21 (% year 28))))) (holiday-julian 3 26 "Kiddush HaHamah")) (if all-hebrew-calendar-holidays (holiday-tisha-b-av-etc)))))

(put (quote hebrew-holidays) (quote risky-local-variable) t)

(defvar hebrew-holidays (append hebrew-holidays-1 hebrew-holidays-2 hebrew-holidays-3 hebrew-holidays-4) "\
*Jewish holidays.
See the documentation for `calendar-holidays' for details.")

(put (quote christian-holidays) (quote risky-local-variable) t)

(defvar christian-holidays (quote ((if all-christian-calendar-holidays (holiday-fixed 1 6 "Epiphany")) (holiday-easter-etc) (if all-christian-calendar-holidays (holiday-greek-orthodox-easter)) (if all-christian-calendar-holidays (holiday-fixed 8 15 "Assumption")) (if all-christian-calendar-holidays (holiday-advent)) (holiday-fixed 12 25 "Christmas") (if all-christian-calendar-holidays (holiday-julian 12 25 "Eastern Orthodox Christmas")))) "\
*Christian holidays.
See the documentation for `calendar-holidays' for details.")

(put (quote islamic-holidays) (quote risky-local-variable) t)

(defvar islamic-holidays (quote ((holiday-islamic 1 1 (format "Islamic New Year %d" (let ((m displayed-month) (y displayed-year)) (increment-calendar-month m y 1) (extract-calendar-year (calendar-islamic-from-absolute (calendar-absolute-from-gregorian (list m (calendar-last-day-of-month m y) y))))))) (if all-islamic-calendar-holidays (holiday-islamic 1 10 "Ashura")) (if all-islamic-calendar-holidays (holiday-islamic 3 12 "Mulad-al-Nabi")) (if all-islamic-calendar-holidays (holiday-islamic 7 26 "Shab-e-Mi'raj")) (if all-islamic-calendar-holidays (holiday-islamic 8 15 "Shab-e-Bara't")) (holiday-islamic 9 1 "Ramadan Begins") (if all-islamic-calendar-holidays (holiday-islamic 9 27 "Shab-e Qadr")) (if all-islamic-calendar-holidays (holiday-islamic 10 1 "Id-al-Fitr")) (if all-islamic-calendar-holidays (holiday-islamic 12 10 "Id-al-Adha")))) "\
*Islamic holidays.
See the documentation for `calendar-holidays' for details.")

(put (quote solar-holidays) (quote risky-local-variable) t)

(defvar solar-holidays (quote ((if (fboundp (quote atan)) (solar-equinoxes-solstices)) (if (progn (require (quote cal-dst)) t) (funcall (quote holiday-sexp) calendar-daylight-savings-starts (quote (format "Daylight Savings Time Begins %s" (if (fboundp (quote atan)) (solar-time-string (/ calendar-daylight-savings-starts-time (float 60)) calendar-standard-time-zone-name) ""))))) (funcall (quote holiday-sexp) calendar-daylight-savings-ends (quote (format "Daylight Savings Time Ends %s" (if (fboundp (quote atan)) (solar-time-string (/ calendar-daylight-savings-ends-time (float 60)) calendar-daylight-time-zone-name) "")))))) "\
*Sun-related holidays.
See the documentation for `calendar-holidays' for details.")

(put (quote calendar-holidays) (quote risky-local-variable) t)

(defvar calendar-setup nil "\
The frame set up of the calendar.
The choices are `one-frame' (calendar and diary together in one separate,
dedicated frame), `two-frames' (calendar and diary in separate, dedicated
frames), `calendar-only' (calendar in a separate, dedicated frame); with
any other value the current frame is used.")

(autoload (quote calendar) "calendar" "\
Choose between the one frame, two frame, or basic calendar displays.
The original function `calendar' has been renamed `calendar-basic-setup'." t nil)

;;;***

;;;### (autoloads nil "cc-langs" "progmodes/cc-langs.el" (13932 14133))
;;; Generated autoloads from progmodes/cc-langs.el

(defvar c-mode-syntax-table nil "\
Syntax table used in c-mode buffers.")

(defvar c++-mode-syntax-table nil "\
Syntax table used in c++-mode buffers.")

(defvar objc-mode-syntax-table nil "\
Syntax table used in objc-mode buffers.")

(defvar java-mode-syntax-table nil "\
Syntax table used in java-mode buffers.")

(defvar idl-mode-syntax-table nil "\
Syntax table used in idl-mode buffers.")

(defvar pike-mode-syntax-table nil "\
Syntax table used in pike-mode buffers.")

;;;***

;;;### (autoloads (pike-mode idl-mode java-mode objc-mode c++-mode
;;;;;;  c-mode c-initialize-cc-mode) "cc-mode" "progmodes/cc-mode.el"
;;;;;;  (13932 14133))
;;; Generated autoloads from progmodes/cc-mode.el

(autoload (quote c-initialize-cc-mode) "cc-mode" nil nil nil)

(autoload (quote c-mode) "cc-mode" "\
Major mode for editing K&R and ANSI C code.
To submit a problem report, enter `\\[c-submit-bug-report]' from a
c-mode buffer.  This automatically sets up a mail buffer with version
information already added.  You just need to add a description of the
problem, including a reproducible test case and send the message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `c-mode-hook' is run with no args, if that value is
bound and has a non-nil value.  Also the hook `c-mode-common-hook' is
run first.

Key bindings:
\\{c-mode-map}" t nil)

(autoload (quote c++-mode) "cc-mode" "\
Major mode for editing C++ code.
To submit a problem report, enter `\\[c-submit-bug-report]' from a
c++-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case, and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `c++-mode-hook' is run with no args, if that
variable is bound and has a non-nil value.  Also the hook
`c-mode-common-hook' is run first.

Key bindings:
\\{c++-mode-map}" t nil)

(autoload (quote objc-mode) "cc-mode" "\
Major mode for editing Objective C code.
To submit a problem report, enter `\\[c-submit-bug-report]' from an
objc-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case, and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `objc-mode-hook' is run with no args, if that value
is bound and has a non-nil value.  Also the hook `c-mode-common-hook'
is run first.

Key bindings:
\\{objc-mode-map}" t nil)

(autoload (quote java-mode) "cc-mode" "\
Major mode for editing Java code.
To submit a problem report, enter `\\[c-submit-bug-report]' from a
java-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `java-mode-hook' is run with no args, if that value
is bound and has a non-nil value.  Also the common hook
`c-mode-common-hook' is run first.  Note that this mode automatically
sets the \"java\" style before calling any hooks so be careful if you
set styles in `c-mode-common-hook'.

Key bindings:
\\{java-mode-map}" t nil)

(autoload (quote idl-mode) "cc-mode" "\
Major mode for editing CORBA's IDL code.
To submit a problem report, enter `\\[c-submit-bug-report]' from an
idl-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case, and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `idl-mode-hook' is run with no args, if that
variable is bound and has a non-nil value.  Also the hook
`c-mode-common-hook' is run first.

Key bindings:
\\{idl-mode-map}" t nil)

(autoload (quote pike-mode) "cc-mode" "\
Major mode for editing Pike code.
To submit a problem report, enter `\\[c-submit-bug-report]' from an
idl-mode buffer.  This automatically sets up a mail buffer with
version information already added.  You just need to add a description
of the problem, including a reproducible test case, and send the
message.

To see what version of CC Mode you are running, enter `\\[c-version]'.

The hook variable `pike-mode-hook' is run with no args, if that value
is bound and has a non-nil value.  Also the common hook
`c-mode-common-hook' is run first.

Key bindings:
\\{pike-mode-map}" t nil)

;;;***

;;;### (autoloads (c-set-offset c-add-style c-set-style) "cc-styles"
;;;;;;  "progmodes/cc-styles.el" (13932 14133))
;;; Generated autoloads from progmodes/cc-styles.el

(autoload (quote c-set-style) "cc-styles" "\
Set CC Mode variables to use one of several different indentation styles.
STYLENAME is a string representing the desired style from the list of
styles described in the variable `c-style-alist'.  See that variable
for details of setting up styles.

The variable `c-indentation-style' always contains the buffer's current
style name." t nil)

(autoload (quote c-add-style) "cc-styles" "\
Adds a style to `c-style-alist', or updates an existing one.
STYLE is a string identifying the style to add or update.  DESCRIP is
an association list describing the style and must be of the form:

  ([BASESTYLE] (VARIABLE . VALUE) [(VARIABLE . VALUE) ...])

See the variable `c-style-alist' for the semantics of BASESTYLE,
VARIABLE and VALUE.  This function also sets the current style to
STYLE using `c-set-style' if the optional SET-P flag is non-nil." t nil)

(autoload (quote c-set-offset) "cc-styles" "\
Change the value of a syntactic element symbol in `c-offsets-alist'.
SYMBOL is the syntactic element symbol to change and OFFSET is the new
offset for that syntactic element.  Optional ADD says to add SYMBOL to
`c-offsets-alist' if it doesn't already appear there." t nil)

;;;***

;;;### (autoloads nil "cc-vars" "progmodes/cc-vars.el" (13932 14133))
;;; Generated autoloads from progmodes/cc-vars.el

(defconst c-emacs-features (let ((infodock-p (boundp (quote infodock-version))) (comments (let ((table (copy-syntax-table)) entry) (modify-syntax-entry 97 ". 12345678" table) (cond ((arrayp table) (setq entry (aref table 97)) (if (consp entry) (setq entry (car entry)))) ((fboundp (quote get-char-table)) (setq entry (get-char-table 97 table))) ((and (fboundp (quote char-table-p)) (char-table-p table)) (setq entry (car (char-table-range table [97])))) (t (error "CC Mode is incompatible with this version of Emacs"))) (if (= (logand (lsh entry -16) 255) 255) (quote 8-bit) (quote 1-bit))))) (if infodock-p (list comments (quote infodock)) (list comments))) "\
A list of features extant in the Emacs you are using.
There are many flavors of Emacs out there, each with different
features supporting those needed by CC Mode.  Here's the current
supported list, along with the values for this variable:

 XEmacs 19:                  (8-bit)
 XEmacs 20:                  (8-bit)
 Emacs 19:                   (1-bit)

Infodock (based on XEmacs) has an additional symbol on this list:
`infodock'.")

;;;***

;;;### (autoloads (ccl-execute-with-args check-ccl-program define-ccl-program
;;;;;;  declare-ccl-program ccl-dump ccl-compile) "ccl" "international/ccl.el"
;;;;;;  (14236 17288))
;;; Generated autoloads from international/ccl.el

(autoload (quote ccl-compile) "ccl" "\
Return a compiled code of CCL-PROGRAM as a vector of integer." nil nil)

(autoload (quote ccl-dump) "ccl" "\
Disassemble compiled CCL-CODE." nil nil)

(autoload (quote declare-ccl-program) "ccl" "\
Declare NAME as a name of CCL program.

This macro exists for backward compatibility.  In the old version of
Emacs, to compile a CCL program which calls another CCL program not
yet defined, it must be declared as a CCL program in advance.  But,
now CCL program names are resolved not at compile time but before
execution.

Optional arg VECTOR is a compiled CCL code of the CCL program." nil (quote macro))

(autoload (quote define-ccl-program) "ccl" "\
Set NAME the compiled code of CCL-PROGRAM.
CCL-PROGRAM is `eval'ed before being handed to the CCL compiler `ccl-compile'.
The compiled code is a vector of integers." nil (quote macro))

(autoload (quote check-ccl-program) "ccl" "\
Check validity of CCL-PROGRAM.
If CCL-PROGRAM is a symbol denoting a CCL program, return
CCL-PROGRAM, else return nil.
If CCL-PROGRAM is a vector and optional arg NAME (symbol) is supplied,
register CCL-PROGRAM by name NAME, and return NAME." nil (quote macro))

(autoload (quote ccl-execute-with-args) "ccl" "\
Execute CCL-PROGRAM with registers initialized by the remaining args.
The return value is a vector of resulting CCL registers." nil nil)

;;;***

;;;### (autoloads (checkdoc-minor-mode checkdoc-ispell-defun checkdoc-ispell-comments
;;;;;;  checkdoc-ispell-continue checkdoc-ispell-start checkdoc-ispell-message-text
;;;;;;  checkdoc-ispell-message-interactive checkdoc-ispell-interactive
;;;;;;  checkdoc-ispell-current-buffer checkdoc-ispell checkdoc-defun
;;;;;;  checkdoc-eval-defun checkdoc-message-text checkdoc-rogue-spaces
;;;;;;  checkdoc-continue checkdoc-start checkdoc-current-buffer
;;;;;;  checkdoc-eval-current-buffer checkdoc-message-interactive
;;;;;;  checkdoc-interactive checkdoc) "checkdoc" "emacs-lisp/checkdoc.el"
;;;;;;  (13847 27399))
;;; Generated autoloads from emacs-lisp/checkdoc.el

(autoload (quote checkdoc) "checkdoc" "\
Interactivly check the entire buffer for style errors.
The current status of the ckeck will be displayed in a buffer which
the users will view as each check is completed." t nil)

(autoload (quote checkdoc-interactive) "checkdoc" "\
Interactively check the current buffer for doc string errors.
Prefix argument START-HERE will start the checking from the current
point, otherwise the check starts at the beginning of the current
buffer.  Allows navigation forward and backwards through document
errors.  Does not check for comment or space warnings.
Optional argument SHOWSTATUS indicates that we should update the
checkdoc status window instead of the usual behavior." t nil)

(autoload (quote checkdoc-message-interactive) "checkdoc" "\
Interactively check the current buffer for message string errors.
Prefix argument START-HERE will start the checking from the current
point, otherwise the check starts at the beginning of the current
buffer.  Allows navigation forward and backwards through document
errors.  Does not check for comment or space warnings.
Optional argument SHOWSTATUS indicates that we should update the
checkdoc status window instead of the usual behavior." t nil)

(autoload (quote checkdoc-eval-current-buffer) "checkdoc" "\
Evaluate and check documentation for the current buffer.
Evaluation is done first because good documentation for something that
doesn't work is just not useful.  Comments, doc strings, and rogue
spacing are all verified." t nil)

(autoload (quote checkdoc-current-buffer) "checkdoc" "\
Check current buffer for document, comment, error style, and rogue spaces.
With a prefix argument (in Lisp, the argument TAKE-NOTES),
store all errors found in a warnings buffer,
otherwise stop after the first error." t nil)

(autoload (quote checkdoc-start) "checkdoc" "\
Start scanning the current buffer for documentation string style errors.
Only documentation strings are checked.
Use `checkdoc-continue' to continue checking if an error cannot be fixed.
Prefix argument TAKE-NOTES means to collect all the warning messages into
a separate buffer." t nil)

(autoload (quote checkdoc-continue) "checkdoc" "\
Find the next doc string in the current buffer which has a style error.
Prefix argument TAKE-NOTES means to continue through the whole buffer and
save warnings in a separate buffer.  Second optional argument START-POINT
is the starting location.  If this is nil, `point-min' is used instead." t nil)

(autoload (quote checkdoc-rogue-spaces) "checkdoc" "\
Find extra spaces at the end of lines in the current file.
Prefix argument TAKE-NOTES non-nil means to save warnings in a
separate buffer.  Otherwise print a message.  This returns the error
if there is one.
Optional argument INTERACT permits more interactive fixing." t nil)

(autoload (quote checkdoc-message-text) "checkdoc" "\
Scan the buffer for occurrences of the error function, and verify text.
Optional argument TAKE-NOTES causes all errors to be logged." t nil)

(autoload (quote checkdoc-eval-defun) "checkdoc" "\
Evaluate the current form with `eval-defun' and check its documentation.
Evaluation is done first so the form will be read before the
documentation is checked.  If there is a documentation error, then the display
of what was evaluated will be overwritten by the diagnostic message." t nil)

(autoload (quote checkdoc-defun) "checkdoc" "\
Examine the doc string of the function or variable under point.
Call `error' if the doc string has problems.  If NO-ERROR is
non-nil, then do not call error, but call `message' instead.
If the doc string passes the test, then check the function for rogue white
space at the end of each line." t nil)

(autoload (quote checkdoc-ispell) "checkdoc" "\
Check the style and spelling of everything interactively.
Calls `checkdoc' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc'" t nil)

(autoload (quote checkdoc-ispell-current-buffer) "checkdoc" "\
Check the style and spelling of the current buffer.
Calls `checkdoc-current-buffer' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-current-buffer'" t nil)

(autoload (quote checkdoc-ispell-interactive) "checkdoc" "\
Check the style and spelling of the current buffer interactively.
Calls `checkdoc-interactive' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-interactive'" t nil)

(autoload (quote checkdoc-ispell-message-interactive) "checkdoc" "\
Check the style and spelling of message text interactively.
Calls `checkdoc-message-interactive' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-message-interactive'" t nil)

(autoload (quote checkdoc-ispell-message-text) "checkdoc" "\
Check the style and spelling of message text interactively.
Calls `checkdoc-message-text' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-message-text'" t nil)

(autoload (quote checkdoc-ispell-start) "checkdoc" "\
Check the style and spelling of the current buffer.
Calls `checkdoc-start' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-start'" t nil)

(autoload (quote checkdoc-ispell-continue) "checkdoc" "\
Check the style and spelling of the current buffer after point.
Calls `checkdoc-continue' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-continue'" t nil)

(autoload (quote checkdoc-ispell-comments) "checkdoc" "\
Check the style and spelling of the current buffer's comments.
Calls `checkdoc-comments' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-comments'" t nil)

(autoload (quote checkdoc-ispell-defun) "checkdoc" "\
Check the style and spelling of the current defun with Ispell.
Calls `checkdoc-defun' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-defun'" t nil)

(autoload (quote checkdoc-minor-mode) "checkdoc" "\
Toggle Checkdoc minor mode, a mode for checking Lisp doc strings.
With prefix ARG, turn Checkdoc minor mode on iff ARG is positive.

In Checkdoc minor mode, the usual bindings for `eval-defun' which is
bound to \\<checkdoc-minor-keymap> \\[checkdoc-eval-defun] and `checkdoc-eval-current-buffer' are overridden to include
checking of documentation strings.

\\{checkdoc-minor-keymap}" t nil)

;;;***

;;;### (autoloads (encode-hz-buffer encode-hz-region decode-hz-buffer
;;;;;;  decode-hz-region setup-chinese-cns-environment setup-chinese-big5-environment
;;;;;;  setup-chinese-gb-environment) "china-util" "language/china-util.el"
;;;;;;  (13774 36670))
;;; Generated autoloads from language/china-util.el

(autoload (quote setup-chinese-gb-environment) "china-util" "\
Setup multilingual environment (MULE) for Chinese GB2312 users." t nil)

(autoload (quote setup-chinese-big5-environment) "china-util" "\
Setup multilingual environment (MULE) for Chinese Big5 users." t nil)

(autoload (quote setup-chinese-cns-environment) "china-util" "\
Setup multilingual environment (MULE) for Chinese CNS11643 family users." t nil)

(autoload (quote decode-hz-region) "china-util" "\
Decode HZ/ZW encoded text in the current region.
Return the length of resulting text." t nil)

(autoload (quote decode-hz-buffer) "china-util" "\
Decode HZ/ZW encoded text in the current buffer." t nil)

(autoload (quote encode-hz-region) "china-util" "\
Encode the text in the current region to HZ.
Return the length of resulting text." t nil)

(autoload (quote encode-hz-buffer) "china-util" "\
Encode the text in the current buffer to HZ." t nil)

;;;***

;;;### (autoloads (command-history-mode list-command-history repeat-matching-complex-command)
;;;;;;  "chistory" "chistory.el" (13569 33539))
;;; Generated autoloads from chistory.el

(autoload (quote repeat-matching-complex-command) "chistory" "\
Edit and re-evaluate complex command with name matching PATTERN.
Matching occurrences are displayed, most recent first, until you select
a form for evaluation.  If PATTERN is empty (or nil), every form in the
command history is offered.  The form is placed in the minibuffer for
editing and the result is evaluated." t nil)

(autoload (quote list-command-history) "chistory" "\
List history of commands typed to minibuffer.
The number of commands listed is controlled by `list-command-history-max'.
Calls value of `list-command-history-filter' (if non-nil) on each history
element to judge if that element should be excluded from the list.

The buffer is left in Command History mode." t nil)

(autoload (quote command-history-mode) "chistory" "\
Major mode for examining commands from `command-history'.
The number of commands listed is controlled by `list-command-history-max'.
The command history is filtered by `list-command-history-filter' if non-nil.
Use \\<command-history-map>\\[command-history-repeat] to repeat the command on the current line.

Otherwise much like Emacs-Lisp Mode except that there is no self-insertion
and digits provide prefix arguments.  Tab does not indent.
\\{command-history-map}
Calls the value of `command-history-hook' if that is non-nil.
The Command History listing is recomputed each time this mode is invoked." t nil)

;;;***

;;;### (autoloads nil "cl" "emacs-lisp/cl.el" (14160 15534))
;;; Generated autoloads from emacs-lisp/cl.el

(defvar custom-print-functions nil "\
This is a list of functions that format user objects for printing.
Each function is called in turn with three arguments: the object, the
stream, and the print level (currently ignored).  If it is able to
print the object it returns true; otherwise it returns nil and the
printer proceeds to the next function on the list.

This variable is not used at present, but it is defined in hopes that
a future Emacs interpreter will be able to use it.")

;;;***

;;;### (autoloads (common-lisp-indent-function) "cl-indent" "emacs-lisp/cl-indent.el"
;;;;;;  (13903 50959))
;;; Generated autoloads from emacs-lisp/cl-indent.el

(autoload (quote common-lisp-indent-function) "cl-indent" nil nil nil)

;;;***

;;;### (autoloads (c-macro-expand) "cmacexp" "progmodes/cmacexp.el"
;;;;;;  (13661 53315))
;;; Generated autoloads from progmodes/cmacexp.el

(autoload (quote c-macro-expand) "cmacexp" "\
Expand C macros in the region, using the C preprocessor.
Normally display output in temp buffer, but
prefix arg means replace the region with it.

`c-macro-preprocessor' specifies the preprocessor to use.
Prompt for arguments to the preprocessor (e.g. `-DDEBUG -I ./include')
if the user option `c-macro-prompt-flag' is non-nil.

Noninteractive args are START, END, SUBST.
For use inside Lisp programs, see also `c-macro-expansion'." t nil)

;;;***

;;;### (autoloads (run-scheme) "cmuscheme" "cmuscheme.el" (13569
;;;;;;  33926))
;;; Generated autoloads from cmuscheme.el

(autoload (quote run-scheme) "cmuscheme" "\
Run an inferior Scheme process, input and output via buffer *scheme*.
If there is a process already running in `*scheme*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `scheme-program-name').  Runs the hooks `inferior-scheme-mode-hook'
\(after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)" t nil)
 (add-hook 'same-window-buffer-names "*scheme*")

;;;***

;;;### (autoloads (codepage-setup cp-supported-codepages cp-offset-for-codepage
;;;;;;  cp-language-for-codepage cp-charset-for-codepage cp-make-coding-systems-for-codepage)
;;;;;;  "codepage" "international/codepage.el" (14124 7952))
;;; Generated autoloads from international/codepage.el

(autoload (quote cp-make-coding-systems-for-codepage) "codepage" "\
Create a coding system to convert IBM CODEPAGE into charset ISO-NAME
whose first character is at offset OFFSET from the beginning of 8-bit
ASCII table.

The created coding system has the usual 3 subsidiary systems: for Unix-,
DOS- and Mac-style EOL conversion.  However, unlike built-in coding
systems, the Mac-style EOL conversion is currently not supported by the
decoder and encoder created by this function." nil nil)

(autoload (quote cp-charset-for-codepage) "codepage" "\
Return the charset for which there is a translation table to DOS CODEPAGE.
CODEPAGE must be the name of a DOS codepage, a string." nil nil)

(autoload (quote cp-language-for-codepage) "codepage" "\
Return the name of the MULE language environment for CODEPAGE.
CODEPAGE must be the name of a DOS codepage, a string." nil nil)

(autoload (quote cp-offset-for-codepage) "codepage" "\
Return the offset to be used in setting up coding systems for CODEPAGE.
CODEPAGE must be the name of a DOS codepage, a string." nil nil)

(autoload (quote cp-supported-codepages) "codepage" "\
Return an alist of supported codepages.

Each association in the alist has the form (NNN . CHARSET), where NNN is the
codepage number, and CHARSET is the MULE charset which is the closest match
for the character set supported by that codepage.

A codepage NNN is supported if a variable called `cpNNN-decode-table' exists,
is a vector, and has a charset property." nil nil)

(autoload (quote codepage-setup) "codepage" "\
Create a coding system cpCODEPAGE to support the IBM codepage CODEPAGE.

These coding systems are meant for encoding and decoding 8-bit non-ASCII
characters used by the IBM codepages, typically in conjunction with files
read/written by MS-DOS software, or for display on the MS-DOS terminal." t nil)

;;;***

;;;### (autoloads (comint-run make-comint) "comint" "comint.el" (14198
;;;;;;  39428))
;;; Generated autoloads from comint.el

(autoload (quote make-comint) "comint" "\
Make a comint process NAME in a buffer, running PROGRAM.
The name of the buffer is made by surrounding NAME with `*'s.
PROGRAM should be either a string denoting an executable program to create
via `start-process', or a cons pair of the form (HOST . SERVICE) denoting a TCP
connection to be opened via `open-network-stream'.  If there is already a
running process in that buffer, it is not restarted.  Optional third arg
STARTFILE is the name of a file to send the contents of to the process.

If PROGRAM is a string, any more args are arguments to PROGRAM." nil nil)

(autoload (quote comint-run) "comint" "\
Run PROGRAM in a comint buffer and switch to it.
The buffer name is made by surrounding the file name of PROGRAM with `*'s.
The file name is used to make a symbol name, such as `comint-sh-hook', and any
hooks on this symbol are run in the buffer.
See `make-comint' and `comint-exec'." t nil)

;;;***

;;;### (autoloads (compare-windows) "compare-w" "compare-w.el" (14220
;;;;;;  17934))
;;; Generated autoloads from compare-w.el

(autoload (quote compare-windows) "compare-w" "\
Compare text in current window with text in next window.
Compares the text starting at point in each window,
moving over text in each one as far as they match.

This command pushes the mark in each window
at the prior location of point in that window.
If both windows display the same buffer,
the mark is pushed twice in that buffer:
first in the other window, then in the selected window.

A prefix arg means ignore changes in whitespace.
The variable `compare-windows-whitespace' controls how whitespace is skipped.
If `compare-ignore-case' is non-nil, changes in case are also ignored." t nil)

;;;***

;;;### (autoloads (next-error compilation-minor-mode compilation-shell-minor-mode
;;;;;;  compilation-mode grep-find grep compile compilation-search-path
;;;;;;  compilation-ask-about-save compilation-window-height compilation-mode-hook)
;;;;;;  "compile" "progmodes/compile.el" (14198 39593))
;;; Generated autoloads from progmodes/compile.el

(defvar compilation-mode-hook nil "\
*List of hook functions run by `compilation-mode' (see `run-hooks').")

(defvar compilation-window-height nil "\
*Number of lines in a compilation window.  If nil, use Emacs default.")

(defvar compilation-process-setup-function nil "\
*Function to call to customize the compilation process.
This functions is called immediately before the compilation process is
started.  It can be used to set any variables or functions that are used
while processing the output of the compilation process.")

(defvar compilation-buffer-name-function nil "\
Function to compute the name of a compilation buffer.
The function receives one argument, the name of the major mode of the
compilation buffer.  It should return a string.
nil means compute the name with `(concat \"*\" (downcase major-mode) \"*\")'.")

(defvar compilation-finish-function nil "\
Function to call when a compilation process finishes.
It is called with two arguments: the compilation buffer, and a string
describing how the process finished.")

(defvar compilation-finish-functions nil "\
Functions to call when a compilation process finishes.
Each function is called with two arguments: the compilation buffer,
and a string describing how the process finished.")

(defvar compilation-ask-about-save t "\
*If not nil, M-x compile asks which buffers to save before compiling.
Otherwise, it saves all modified buffers without asking.")

(defvar compilation-search-path (quote (nil)) "\
*List of directories to search for source files named in error messages.
Elements should be directory names, not file names of directories.
nil as an element means to try the default directory.")

(autoload (quote compile) "compile" "\
Compile the program including the current buffer.  Default: run `make'.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer `*compilation*'.

You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it.

Interactively, prompts for the command if `compilation-read-command' is
non-nil; otherwise uses `compile-command'.  With prefix arg, always prompts.

To run more than one compilation at once, start one and rename the
`*compilation*' buffer to some other name with \\[rename-buffer].
Then start the next one.

The name used for the buffer is actually whatever is returned by
the function in `compilation-buffer-name-function', so you can set that
to a function that generates a unique name." t nil)

(autoload (quote grep) "compile" "\
Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use \\[next-error] (M-x next-error),
or \\<compilation-minor-mode-map>\\[compile-goto-error] in the grep output buffer, to go to the lines
where grep found matches.

This command uses a special history list for its arguments, so you can
easily repeat a grep command.

A prefix argument says to default the argument based upon the current
tag the cursor is over, substituting it into the last grep command
in the grep command history (or into `grep-command'
if that history list is empty)." t nil)

(autoload (quote grep-find) "compile" "\
Run grep via find, with user-specified args, and collect output in a buffer.
While find runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to.

This command uses a special history list for its arguments, so you can
easily repeat a find command." t nil)

(autoload (quote compilation-mode) "compile" "\
Major mode for compilation log buffers.
\\<compilation-mode-map>To visit the source for a line-numbered error,
move point to the error message line and type \\[compile-goto-error].
To kill the compilation, type \\[kill-compilation].

Runs `compilation-mode-hook' with `run-hooks' (which see)." t nil)

(autoload (quote compilation-shell-minor-mode) "compile" "\
Toggle compilation shell minor mode.
With arg, turn compilation mode on if and only if arg is positive.
See `compilation-mode'.
Turning the mode on runs the normal hook `compilation-shell-minor-mode-hook'." t nil)

(autoload (quote compilation-minor-mode) "compile" "\
Toggle compilation minor mode.
With arg, turn compilation mode on if and only if arg is positive.
See `compilation-mode'.
Turning the mode on runs the normal hook `compilation-minor-mode-hook'." t nil)

(autoload (quote next-error) "compile" "\
Visit next compilation error message and corresponding source code.

If all the error messages parsed so far have been processed already,
the message buffer is checked for new ones.

A prefix arg specifies how many error messages to move;
negative means move back to previous error messages.
Just C-u as a prefix means reparse the error message buffer
and start at the first error.

\\[next-error] normally uses the most recently started compilation or
grep buffer.  However, it can operate on any buffer with output from
the \\[compile] and \\[grep] commands, or, more generally, on any
buffer in Compilation mode or with Compilation Minor mode enabled.  To
specify use of a particular buffer for error messages, type
\\[next-error] in that buffer.

Once \\[next-error] has chosen the buffer for error messages,
it stays with that buffer until you use it in some other buffer which
uses Compilation mode or Compilation Minor mode.

See variables `compilation-parse-errors-function' and
`compilation-error-regexp-alist' for customization ideas." t nil)
 (define-key ctl-x-map "`" 'next-error)

;;;***

;;;### (autoloads (partial-completion-mode) "complete" "complete.el"
;;;;;;  (14115 62109))
;;; Generated autoloads from complete.el

(autoload (quote partial-completion-mode) "complete" "\
Toggle Partial Completion mode.
With prefix ARG, turn Partial Completion mode on if ARG is positive.

When Partial Completion mode is enabled, TAB (or M-TAB if `PC-meta-flag' is
nil) is enhanced so that if some string is divided into words and each word is
delimited by a character in `PC-word-delimiters', partial words are completed
as much as possible.

For example, M-x p-c-m expands to M-x partial-completion-mode since no other
command begins with that sequence of characters, and
\\[find-file] f_b.c TAB might complete to foo_bar.c if that file existed and no
other file in that directory begin with that sequence of characters.

Unless `PC-disable-includes' is non-nil, the \"<...>\" sequence is interpreted
specially in \\[find-file].  For example,
\\[find-file] <sys/time.h> RET finds the file /usr/include/sys/time.h.
See also the variable `PC-include-file-path'." t nil)

;;;***

;;;### (autoloads (dynamic-completion-mode) "completion" "completion.el"
;;;;;;  (13876 49580))
;;; Generated autoloads from completion.el

(autoload (quote dynamic-completion-mode) "completion" "\
Enable dynamic word-completion." t nil)

;;;***

;;;### (autoloads (shuffle-vector cookie-snarf cookie-insert cookie)
;;;;;;  "cookie1" "play/cookie1.el" (13538 26297))
;;; Generated autoloads from play/cookie1.el

(autoload (quote cookie) "cookie1" "\
Return a random phrase from PHRASE-FILE.  When the phrase file
is read in, display STARTMSG at beginning of load, ENDMSG at end." nil nil)

(autoload (quote cookie-insert) "cookie1" "\
Insert random phrases from PHRASE-FILE; COUNT of them.  When the phrase file
is read in, display STARTMSG at beginning of load, ENDMSG at end." nil nil)

(autoload (quote cookie-snarf) "cookie1" "\
Reads in the PHRASE-FILE, returns it as a vector of strings.
Emit STARTMSG and ENDMSG before and after.  Caches the result; second
and subsequent calls on the same file won't go to disk." nil nil)

(autoload (quote shuffle-vector) "cookie1" "\
Randomly permute the elements of VECTOR (all permutations equally likely)" nil nil)

;;;***

;;;### (autoloads (copyright copyright-update) "copyright" "emacs-lisp/copyright.el"
;;;;;;  (13940 33273))
;;; Generated autoloads from emacs-lisp/copyright.el

(autoload (quote copyright-update) "copyright" "\
Update the copyright notice at the beginning of the buffer to indicate
the current year.  If optional prefix ARG is given replace the years in the
notice rather than adding the current year after them.  If necessary and
`copyright-current-gpl-version' is set, the copying permissions following the
copyright, if any, are updated as well." t nil)

(autoload (quote copyright) "copyright" "\
Insert a copyright by $ORGANIZATION notice at cursor." t nil)

;;;***

;;;### (autoloads (cperl-mode) "cperl-mode" "progmodes/cperl-mode.el"
;;;;;;  (14177 62783))
;;; Generated autoloads from progmodes/cperl-mode.el

(autoload (quote cperl-mode) "cperl-mode" "\
Major mode for editing Perl code.
Expression and list commands understand all C brackets.
Tab indents for Perl code.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.

Various characters in Perl almost always come in pairs: {}, (), [],
sometimes <>.  When the user types the first, she gets the second as
well, with optional special formatting done on {}.  (Disabled by
default.)  You can always quote (with \\[quoted-insert]) the left
\"paren\" to avoid the expansion.  The processing of < is special,
since most the time you mean \"less\".  Cperl mode tries to guess
whether you want to type pair <>, and inserts is if it
appropriate.  You can set `cperl-electric-parens-string' to the string that
contains the parenths from the above list you want to be electrical.
Electricity of parenths is controlled by `cperl-electric-parens'.
You may also set `cperl-electric-parens-mark' to have electric parens
look for active mark and \"embrace\" a region if possible.'

CPerl mode provides expansion of the Perl control constructs:

   if, else, elsif, unless, while, until, continue, do, 
   for, foreach, formy and foreachmy.

and POD directives (Disabled by default, see `cperl-electric-keywords'.)

The user types the keyword immediately followed by a space, which
causes the construct to be expanded, and the point is positioned where
she is most likely to want to be.  eg. when the user types a space
following \"if\" the following appears in the buffer: if () { or if ()
} { } and the cursor is between the parentheses.  The user can then
type some boolean expression within the parens.  Having done that,
typing \\[cperl-linefeed] places you - appropriately indented - on a
new line between the braces (if you typed \\[cperl-linefeed] in a POD
directive line, then appropriate number of new lines is inserted).  

If CPerl decides that you want to insert \"English\" style construct like

            bite if angry;

it will not do any expansion.  See also help on variable
`cperl-extra-newline-before-brace'.  (Note that one can switch the
help message on expansion by setting `cperl-message-electric-keyword'
to nil.)

\\[cperl-linefeed] is a convenience replacement for typing carriage
return.  It places you in the next line with proper indentation, or if
you type it inside the inline block of control construct, like

            foreach (@lines) {print; print}

and you are on a boundary of a statement inside braces, it will
transform the construct into a multiline and will place you into an
appropriately indented blank line.  If you need a usual 
`newline-and-indent' behaviour, it is on \\[newline-and-indent], 
see documentation on `cperl-electric-linefeed'.

Use \\[cperl-invert-if-unless] to change a construction of the form

	    if (A) { B }

into

            B if A;

\\{cperl-mode-map}

Setting the variable `cperl-font-lock' to t switches on font-lock-mode
\(even with older Emacsen), `cperl-electric-lbrace-space' to t switches
on electric space between $ and {, `cperl-electric-parens-string' is
the string that contains parentheses that should be electric in CPerl
\(see also `cperl-electric-parens-mark' and `cperl-electric-parens'),
setting `cperl-electric-keywords' enables electric expansion of
control structures in CPerl.  `cperl-electric-linefeed' governs which
one of two linefeed behavior is preferable.  You can enable all these
options simultaneously (recommended mode of use) by setting
`cperl-hairy' to t.  In this case you can switch separate options off
by setting them to `null'.  Note that one may undo the extra
whitespace inserted by semis and braces in `auto-newline'-mode by
consequent \\[cperl-electric-backspace].

If your site has perl5 documentation in info format, you can use commands
\\[cperl-info-on-current-command] and \\[cperl-info-on-command] to access it.
These keys run commands `cperl-info-on-current-command' and
`cperl-info-on-command', which one is which is controlled by variable
`cperl-info-on-command-no-prompt' and `cperl-clobber-lisp-bindings' 
\(in turn affected by `cperl-hairy').

Even if you have no info-format documentation, short one-liner-style
help is available on \\[cperl-get-help], and one can run perldoc or
man via menu.

It is possible to show this help automatically after some idle time.
This is regulated by variable `cperl-lazy-help-time'.  Default with
`cperl-hairy' (if the value of `cperl-lazy-help-time' is nil) is 5
secs idle time .  It is also possible to switch this on/off from the
menu, or via \\[cperl-toggle-autohelp].  Requires `run-with-idle-timer'.

Use \\[cperl-lineup] to vertically lineup some construction - put the
beginning of the region at the start of construction, and make region
span the needed amount of lines.

Variables `cperl-pod-here-scan', `cperl-pod-here-fontify',
`cperl-pod-face', `cperl-pod-head-face' control processing of pod and
here-docs sections.  With capable Emaxen results of scan are used
for indentation too, otherwise they are used for highlighting only.

Variables controlling indentation style:
 `cperl-tab-always-indent'
    Non-nil means TAB in CPerl mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 `cperl-indent-left-aligned-comments'
    Non-nil means that the comment starting in leftmost column should indent.
 `cperl-auto-newline'
    Non-nil means automatically newline before and after braces,
    and after colons and semicolons, inserted in Perl code.  The following
    \\[cperl-electric-backspace] will remove the inserted whitespace.
    Insertion after colons requires both this variable and 
    `cperl-auto-newline-after-colon' set. 
 `cperl-auto-newline-after-colon'
    Non-nil means automatically newline even after colons.
    Subject to `cperl-auto-newline' setting.
 `cperl-indent-level'
    Indentation of Perl statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 `cperl-continued-statement-offset'
    Extra indentation given to a substatement, such as the
    then-clause of an if, or body of a while, or just a statement continuation.
 `cperl-continued-brace-offset'
    Extra indentation given to a brace that starts a substatement.
    This is in addition to `cperl-continued-statement-offset'.
 `cperl-brace-offset'
    Extra indentation for line if it starts with an open brace.
 `cperl-brace-imaginary-offset'
    An open brace following other text is treated as if it the line started
    this far to the right of the actual line indentation.
 `cperl-label-offset'
    Extra indentation for line that is a label.
 `cperl-min-label-indent'
    Minimal indentation for line that is a label.

Settings for K&R and BSD indentation styles are
  `cperl-indent-level'                5    8
  `cperl-continued-statement-offset'  5    8
  `cperl-brace-offset'               -5   -8
  `cperl-label-offset'               -5   -8

CPerl knows several indentation styles, and may bulk set the
corresponding variables.  Use \\[cperl-set-style] to do this.  Use
\\[cperl-set-style-back] to restore the memorized preexisting values
\(both available from menu).

If `cperl-indent-level' is 0, the statement after opening brace in
column 0 is indented on 
`cperl-brace-offset'+`cperl-continued-statement-offset'.

Turning on CPerl mode calls the hooks in the variable `cperl-mode-hook'
with no args.

DO NOT FORGET to read micro-docs (available from `Perl' menu)
or as help on variables `cperl-tips', `cperl-problems',
`cperl-non-problems', `cperl-praise', `cperl-speed'." t nil)

;;;***

;;;### (autoloads (cpp-parse-edit cpp-highlight-buffer) "cpp" "progmodes/cpp.el"
;;;;;;  (13826 9909))
;;; Generated autoloads from progmodes/cpp.el

(autoload (quote cpp-highlight-buffer) "cpp" "\
Highlight C code according to preprocessor conditionals.
This command pops up a buffer which you should edit to specify
what kind of highlighting to use, and the criteria for highlighting.
A prefix arg suppresses display of that buffer." t nil)

(autoload (quote cpp-parse-edit) "cpp" "\
Edit display information for cpp conditionals." t nil)

;;;***

;;;### (autoloads (crisp-mode crisp-mode) "crisp" "emulation/crisp.el"
;;;;;;  (14179 6457))
;;; Generated autoloads from emulation/crisp.el

(defvar crisp-mode nil "\
Track status of CRiSP emulation mode.
A value of nil means CRiSP mode is not enabled.  A value of t
indicates CRiSP mode is enabled.

Setting this variable directly does not take effect;
use either M-x customize or the function `crisp-mode'.")

(custom-add-to-group (quote crisp) (quote crisp-mode) (quote custom-variable))

(custom-add-load (quote crisp-mode) (quote crisp))

(autoload (quote crisp-mode) "crisp" "\
Toggle CRiSP emulation minor mode.
With ARG, turn CRiSP mode on if ARG is positive, off otherwise." t nil)

;;;***

;;;### (autoloads (customize-menu-create custom-menu-create custom-save-all
;;;;;;  customize-save-customized custom-file customize-browse custom-buffer-create-other-window
;;;;;;  custom-buffer-create customize-apropos-groups customize-apropos-faces
;;;;;;  customize-apropos-options customize-apropos customize-saved
;;;;;;  customize-customized customize-face-other-window customize-face
;;;;;;  customize-option-other-window customize-changed-options customize-option
;;;;;;  customize-group-other-window customize-group customize customize-save-variable
;;;;;;  customize-set-variable customize-set-value) "cus-edit" "cus-edit.el"
;;;;;;  (14092 56283))
;;; Generated autoloads from cus-edit.el
 (add-hook 'same-window-regexps "\\`\\*Customiz.*\\*\\'")

(autoload (quote customize-set-value) "cus-edit" "\
Set VARIABLE to VALUE.  VALUE is a Lisp object.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read the value.

If VARIABLE has a `custom-type' property, it must be a widget and the
`:prompt-value' property of that widget will be used for reading the value." t nil)

(autoload (quote customize-set-variable) "cus-edit" "\
Set the default for VARIABLE to VALUE.  VALUE is a Lisp object.

If VARIABLE has a `custom-set' property, that is used for setting
VARIABLE, otherwise `set-default' is used.

The `customized-value' property of the VARIABLE will be set to a list
with a quoted VALUE as its sole list member.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read the value.

If VARIABLE has a `custom-type' property, it must be a widget and the
`:prompt-value' property of that widget will be used for reading the value. " t nil)

(autoload (quote customize-save-variable) "cus-edit" "\
Set the default for VARIABLE to VALUE, and save it for future sessions.
If VARIABLE has a `custom-set' property, that is used for setting
VARIABLE, otherwise `set-default' is used.

The `customized-value' property of the VARIABLE will be set to a list
with a quoted VALUE as its sole list member.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read the value.

If VARIABLE has a `custom-type' property, it must be a widget and the
`:prompt-value' property of that widget will be used for reading the value. " t nil)

(autoload (quote customize) "cus-edit" "\
Select a customization buffer which you can use to set user options.
User options are structured into \"groups\".
Initially the top-level group `Emacs' and its immediate subgroups
are shown; the contents of those subgroups are initially hidden." t nil)

(autoload (quote customize-group) "cus-edit" "\
Customize GROUP, which must be a customization group." t nil)

(autoload (quote customize-group-other-window) "cus-edit" "\
Customize GROUP, which must be a customization group." t nil)

(defalias (quote customize-variable) (quote customize-option))

(autoload (quote customize-option) "cus-edit" "\
Customize SYMBOL, which must be a user option variable." t nil)

(autoload (quote customize-changed-options) "cus-edit" "\
Customize all user option variables changed in Emacs itself.
This includes new user option variables and faces, and new
customization groups, as well as older options and faces whose default
values have changed since the previous major Emacs release.

With argument SINCE-VERSION (a string), customize all user option
variables that were added (or their meanings were changed) since that
version." t nil)

(defalias (quote customize-variable-other-window) (quote customize-option-other-window))

(autoload (quote customize-option-other-window) "cus-edit" "\
Customize SYMBOL, which must be a user option variable.
Show the buffer in another window, but don't select it." t nil)

(autoload (quote customize-face) "cus-edit" "\
Customize SYMBOL, which should be a face name or nil.
If SYMBOL is nil, customize all faces." t nil)

(autoload (quote customize-face-other-window) "cus-edit" "\
Show customization buffer for FACE in other window." t nil)

(autoload (quote customize-customized) "cus-edit" "\
Customize all user options set since the last save in this session." t nil)

(autoload (quote customize-saved) "cus-edit" "\
Customize all already saved user options." t nil)

(autoload (quote customize-apropos) "cus-edit" "\
Customize all user options matching REGEXP.
If ALL is `options', include only options.
If ALL is `faces', include only faces.
If ALL is `groups', include only groups.
If ALL is t (interactively, with prefix arg), include options which are not
user-settable, as well as faces and groups." t nil)

(autoload (quote customize-apropos-options) "cus-edit" "\
Customize all user options matching REGEXP.
With prefix arg, include options which are not user-settable." t nil)

(autoload (quote customize-apropos-faces) "cus-edit" "\
Customize all user faces matching REGEXP." t nil)

(autoload (quote customize-apropos-groups) "cus-edit" "\
Customize all user groups matching REGEXP." t nil)

(autoload (quote custom-buffer-create) "cus-edit" "\
Create a buffer containing OPTIONS.
Optional NAME is the name of the buffer.
OPTIONS should be an alist of the form ((SYMBOL WIDGET)...), where
SYMBOL is a customization option, and WIDGET is a widget for editing
that option." nil nil)

(autoload (quote custom-buffer-create-other-window) "cus-edit" "\
Create a buffer containing OPTIONS.
Optional NAME is the name of the buffer.
OPTIONS should be an alist of the form ((SYMBOL WIDGET)...), where
SYMBOL is a customization option, and WIDGET is a widget for editing
that option." nil nil)

(autoload (quote customize-browse) "cus-edit" "\
Create a tree browser for the customize hierarchy." t nil)

(defvar custom-file nil "\
File used for storing customization information.
The default is nil, which means to use your init file
as specified by `user-init-file'.  If you specify some other file,
you need to explicitly load that file for the settings to take effect.")

(autoload (quote customize-save-customized) "cus-edit" "\
Save all user options which have been set in this session." t nil)

(autoload (quote custom-save-all) "cus-edit" "\
Save all customizations in `custom-file'." nil nil)

(autoload (quote custom-menu-create) "cus-edit" "\
Create menu for customization group SYMBOL.
The menu is in a format applicable to `easy-menu-define'." nil nil)

(autoload (quote customize-menu-create) "cus-edit" "\
Return a customize menu for customization group SYMBOL.
If optional NAME is given, use that as the name of the menu. 
Otherwise the menu will be named `Customize'.
The format is suitable for use with `easy-menu-define'." nil nil)

;;;***

;;;### (autoloads (custom-set-faces custom-declare-face) "cus-face"
;;;;;;  "cus-face.el" (14228 23896))
;;; Generated autoloads from cus-face.el

(autoload (quote custom-declare-face) "cus-face" "\
Like `defface', but FACE is evaluated as a normal argument." nil nil)

(autoload (quote custom-set-faces) "cus-face" "\
Initialize faces according to user preferences.
The arguments should be a list where each entry has the form:

  (FACE SPEC [NOW])

SPEC is stored as the saved value for FACE.
If NOW is present and non-nil, FACE is created now, according to SPEC.

See `defface' for the format of SPEC." nil nil)

;;;***

;;;### (autoloads (standard-display-cyrillic-translit cyrillic-encode-alternativnyj-char
;;;;;;  cyrillic-encode-koi8-r-char setup-cyrillic-alternativnyj-environment
;;;;;;  setup-cyrillic-koi8-environment setup-cyrillic-iso-environment)
;;;;;;  "cyril-util" "language/cyril-util.el" (13774 36671))
;;; Generated autoloads from language/cyril-util.el

(autoload (quote setup-cyrillic-iso-environment) "cyril-util" "\
Setup multilingual environment (MULE) for Cyrillic ISO-8859-5 users." t nil)

(autoload (quote setup-cyrillic-koi8-environment) "cyril-util" "\
Setup multilingual environment (MULE) for Cyrillic KOI8 users." t nil)

(autoload (quote setup-cyrillic-alternativnyj-environment) "cyril-util" "\
Setup multilingual environment (MULE) for Cyrillic ALTERNATIVNYJ users." t nil)

(autoload (quote cyrillic-encode-koi8-r-char) "cyril-util" "\
Return KOI8-R external character code of CHAR if appropriate." nil nil)

(autoload (quote cyrillic-encode-alternativnyj-char) "cyril-util" "\
Return ALTERNATIVNYJ external character code of CHAR if appropriate." nil nil)

(autoload (quote standard-display-cyrillic-translit) "cyril-util" "\
Display a cyrillic buffer using a transliteration.
For readability, the table is slightly
different from the one used for the input method `cyrillic-translit'.

The argument is a string which specifies which language you are using;
that affects the choice of transliterations slightly.
Possible values are listed in 'cyrillic-language-alist'.
If the argument is t, we use the default cyrillic transliteration.
If the argument is nil, we return the display table to its standard state." t nil)

;;;***

;;;### (autoloads (dabbrev-expand dabbrev-completion) "dabbrev" "dabbrev.el"
;;;;;;  (13833 28318))
;;; Generated autoloads from dabbrev.el

(define-key esc-map "/" (quote dabbrev-expand))

(define-key esc-map [67108911] (quote dabbrev-completion))

(autoload (quote dabbrev-completion) "dabbrev" "\
Completion on current word.
Like \\[dabbrev-expand] but finds all expansions in the current buffer
and presents suggestions for completion.

With a prefix argument, it searches all buffers accepted by the
function pointed out by `dabbrev-friend-buffer-function' to find the
completions.

If the prefix argument is 16 (which comes from C-u C-u),
then it searches *all* buffers.

With no prefix argument, it reuses an old completion list
if there is a suitable one already." t nil)

(autoload (quote dabbrev-expand) "dabbrev" "\
Expand previous word \"dynamically\".

Expands to the most recent, preceding word for which this is a prefix.
If no suitable preceding word is found, words following point are
considered.  If still no suitable word is found, then look in the
buffers accepted by the function pointed out by variable
`dabbrev-friend-buffer-function'.

A positive prefix argument, N, says to take the Nth backward *distinct*
possibility.  A negative argument says search forward.

If the cursor has not moved from the end of the previous expansion and
no argument is given, replace the previously-made expansion
with the next possible expansion not yet tried.

The variable `dabbrev-backward-only' may be used to limit the
direction of search to backward if set non-nil.

See also `dabbrev-abbrev-char-regexp' and \\[dabbrev-completion]." t nil)

;;;***

;;;### (autoloads (dcl-mode) "dcl-mode" "progmodes/dcl-mode.el" (13706
;;;;;;  39004))
;;; Generated autoloads from progmodes/dcl-mode.el

(autoload (quote dcl-mode) "dcl-mode" "\
Major mode for editing DCL-files.

This mode indents command lines in blocks.  (A block is commands between
THEN-ELSE-ENDIF and between lines matching dcl-block-begin-regexp and
dcl-block-end-regexp.)

Labels are indented to a fixed position unless they begin or end a block.
Whole-line comments (matching dcl-comment-line-regexp) are not indented. 
Data lines are not indented.

Key bindings:

\\{dcl-mode-map}
Commands not usually bound to keys:

\\[dcl-save-nondefault-options]		Save changed options
\\[dcl-save-all-options]		Save all options
\\[dcl-save-option]			Save any option
\\[dcl-save-mode]			Save buffer mode

Variables controlling indentation style and extra features:

 dcl-basic-offset
    Extra indentation within blocks.

 dcl-continuation-offset
    Extra indentation for continued lines.

 dcl-margin-offset
    Indentation for the first command line in a file or SUBROUTINE.

 dcl-margin-label-offset
    Indentation for a label.

 dcl-comment-line-regexp
    Lines matching this regexp will not be indented. 

 dcl-block-begin-regexp
 dcl-block-end-regexp
    Regexps that match command lines that begin and end, respectively,
    a block of commmand lines that will be given extra indentation.
    Command lines between THEN-ELSE-ENDIF are always indented; these variables
    make it possible to define other places to indent.
    Set to nil to disable this feature.

 dcl-calc-command-indent-function
    Can be set to a function that customizes indentation for command lines.
    Two such functions are included in the package:
	dcl-calc-command-indent-multiple
	dcl-calc-command-indent-hang

 dcl-calc-cont-indent-function
    Can be set to a function that customizes indentation for continued lines.
    One such function is included in the package:
	dcl-calc-cont-indent-relative    (set by default)

 dcl-tab-always-indent
    If t, pressing TAB always indents the current line.
    If nil, pressing TAB indents the current line if point is at the left 
    margin.

 dcl-electric-characters 
    Non-nil causes lines to be indented at once when a label, ELSE or ENDIF is
    typed.

 dcl-electric-reindent-regexps
    Use this variable and function dcl-electric-character to customize
    which words trigger electric indentation.

 dcl-tempo-comma
 dcl-tempo-left-paren
 dcl-tempo-right-paren
    These variables control the look of expanded templates.

 dcl-imenu-generic-expression
    Default value for imenu-generic-expression.  The default includes
    SUBROUTINE labels in the main listing and sub-listings for
    other labels, CALL, GOTO and GOSUB statements. 

 dcl-imenu-label-labels
 dcl-imenu-label-goto
 dcl-imenu-label-gosub
 dcl-imenu-label-call
    Change the text that is used as sub-listing labels in imenu.

Loading this package calls the value of the variable
`dcl-mode-load-hook' with no args, if that value is non-nil. 
Turning on DCL mode calls the value of the variable `dcl-mode-hook' 
with no args, if that value is non-nil.


The following example uses the default values for all variables:

$! This is a comment line that is not indented (it matches 
$! dcl-comment-line-regexp)
$! Next follows the first command line.  It is indented dcl-margin-offset.
$       i = 1
$       ! Other comments are indented like command lines.
$       ! A margin label indented dcl-margin-label-offset:
$ label: 
$       if i.eq.1
$       then
$           ! Lines between THEN-ELSE and ELSE-ENDIF are 
$           ! indented dcl-basic-offset
$           loop1: ! This matches dcl-block-begin-regexp...
$               ! ...so this line is indented dcl-basic-offset
$               text = \"This \" + - ! is a continued line 
                       \"lined up with the command line\"
$               type sys$input
Data lines are not indented at all. 
$           endloop1: ! This matches dcl-block-end-regexp
$       endif
$
" t nil)

;;;***

;;;### (autoloads (cancel-debug-on-entry debug-on-entry debug) "debug"
;;;;;;  "emacs-lisp/debug.el" (14144 59544))
;;; Generated autoloads from emacs-lisp/debug.el

(setq debugger (quote debug))

(autoload (quote debug) "debug" "\
Enter debugger.  To return, type \\<debugger-mode-map>`\\[debugger-continue]'.
Arguments are mainly for use when this is called from the internals
of the evaluator.

You may call with no args, or you may pass nil as the first arg and
any other args you like.  In that case, the list of args after the
first will be printed into the backtrace buffer." t nil)

(autoload (quote debug-on-entry) "debug" "\
Request FUNCTION to invoke debugger each time it is called.
If you tell the debugger to continue, FUNCTION's execution proceeds.
This works by modifying the definition of FUNCTION,
which must be written in Lisp, not predefined.
Use \\[cancel-debug-on-entry] to cancel the effect of this command.
Redefining FUNCTION also cancels it." t nil)

(autoload (quote cancel-debug-on-entry) "debug" "\
Undo effect of \\[debug-on-entry] on FUNCTION.
If argument is nil or an empty string, cancel for all functions." t nil)

;;;***

;;;### (autoloads (decipher-mode decipher) "decipher" "play/decipher.el"
;;;;;;  (13875 47217))
;;; Generated autoloads from play/decipher.el

(autoload (quote decipher) "decipher" "\
Format a buffer of ciphertext for cryptanalysis and enter Decipher mode." t nil)

(autoload (quote decipher-mode) "decipher" "\
Major mode for decrypting monoalphabetic substitution ciphers.
Lower-case letters enter plaintext.
Upper-case letters are commands.

The buffer is made read-only so that normal Emacs commands cannot
modify it.

The most useful commands are:
\\<decipher-mode-map>
\\[decipher-digram-list]  Display a list of all digrams & their frequency
\\[decipher-frequency-count]  Display the frequency of each ciphertext letter
\\[decipher-adjacency-list]  Show adjacency list for current letter (lists letters appearing next to it)
\\[decipher-make-checkpoint]  Save the current cipher alphabet (checkpoint)
\\[decipher-restore-checkpoint]  Restore a saved cipher alphabet (checkpoint)" t nil)

;;;***

;;;### (autoloads (delete-selection-mode delete-selection-mode) "delsel"
;;;;;;  "delsel.el" (14118 1897))
;;; Generated autoloads from delsel.el

(defalias (quote pending-delete-mode) (quote delete-selection-mode))

(autoload (quote delete-selection-mode) "delsel" "\
Toggle Delete Selection mode.
With prefix ARG, turn Delete Selection mode on if and only if ARG is
positive.

When Delete Selection mode is enabled, Transient Mark mode is also
enabled and typed text replaces the selection if the selection is
active.  Otherwise, typed text is just inserted at point regardless of
any selection." t nil)

(defvar delete-selection-mode nil "\
Toggle Delete Selection mode.
See command `delete-selection-mode'.
Setting this variable directly does not take effect;
use either \\[customize] or the function `delete-selection-mode'.")

(custom-add-to-group (quote editing-basics) (quote delete-selection-mode) (quote custom-variable))

(custom-add-load (quote delete-selection-mode) (quote delsel))

;;;***

;;;### (autoloads (derived-mode-init-mode-variables define-derived-mode)
;;;;;;  "derived" "derived.el" (13721 58283))
;;; Generated autoloads from derived.el

(autoload (quote define-derived-mode) "derived" "\
Create a new mode as a variant of an existing mode.

The arguments to this command are as follow:

CHILD:     the name of the command for the derived mode.
PARENT:    the name of the command for the parent mode (ie. text-mode).
NAME:      a string which will appear in the status line (ie. \"Hypertext\")
DOCSTRING: an optional documentation string--if you do not supply one,
           the function will attempt to invent something useful.
BODY:      forms to execute just before running the
           hooks for the new mode.

Here is how you could define LaTeX-Thesis mode as a variant of LaTeX mode:

  (define-derived-mode LaTeX-thesis-mode LaTeX-mode \"LaTeX-Thesis\")

You could then make new key bindings for `LaTeX-thesis-mode-map'
without changing regular LaTeX mode.  In this example, BODY is empty,
and DOCSTRING is generated by default.

On a more complicated level, the following command uses sgml-mode as
the parent, and then sets the variable `case-fold-search' to nil:

  (define-derived-mode article-mode sgml-mode \"Article\"
    \"Major mode for editing technical articles.\"
    (setq case-fold-search nil))

Note that if the documentation string had been left out, it would have
been generated automatically, with a reference to the keymap." nil (quote macro))

(autoload (quote derived-mode-init-mode-variables) "derived" "\
Initialise variables for a new mode. 
Right now, if they don't already exist, set up a blank keymap, an
empty syntax table, and an empty abbrev table -- these will be merged
the first time the mode is used." nil nil)

;;;***

;;;### (autoloads (desktop-load-default desktop-read) "desktop" "desktop.el"
;;;;;;  (13897 58559))
;;; Generated autoloads from desktop.el

(autoload (quote desktop-read) "desktop" "\
Read the Desktop file and the files it specifies.
This is a no-op when Emacs is running in batch mode." t nil)

(autoload (quote desktop-load-default) "desktop" "\
Load the `default' start-up library manually.
Also inhibit further loading of it.  Call this from your `.emacs' file
to provide correct modes for autoloaded files." nil nil)

;;;***

;;;### (autoloads (devanagari-decode-itrans-region devanagari-encode-itrans-region
;;;;;;  in-is13194-devanagari-pre-write-conversion devanagari-decompose-to-is13194-region
;;;;;;  in-is13194-devanagari-post-read-conversion devanagari-compose-from-is13194-region
;;;;;;  devanagari-compose-region devanagari-compose-string devanagari-decompose-region
;;;;;;  devanagari-decompose-string char-to-glyph-devanagari indian-to-devanagari-string
;;;;;;  devanagari-to-indian-region indian-to-devanagari-region devanagari-to-indian
;;;;;;  indian-to-devanagari setup-devanagari-environment) "devan-util"
;;;;;;  "language/devan-util.el" (13941 57965))
;;; Generated autoloads from language/devan-util.el

(autoload (quote setup-devanagari-environment) "devan-util" "\
Setup multilingual environment (MULE) for languages using Devanagari." t nil)

(autoload (quote indian-to-devanagari) "devan-util" "\
Convert IS 13194 characters to Devanagari basic characters." nil nil)

(autoload (quote devanagari-to-indian) "devan-util" "\
Convert Devanagari basic characters to IS 13194 characters." nil nil)

(autoload (quote indian-to-devanagari-region) "devan-util" "\
Convert IS 13194 characters in region to Devanagari basic characters." t nil)

(autoload (quote devanagari-to-indian-region) "devan-util" "\
Convert Devanagari basic characters in region to Indian characters." t nil)

(autoload (quote indian-to-devanagari-string) "devan-util" "\
Convert Indian String to Devanagari Basic Character String." nil nil)

(autoload (quote char-to-glyph-devanagari) "devan-util" "\
Convert Devanagari characters in the string to Devanagari glyphs.  
Ligatures and special rules are processed." nil nil)

(autoload (quote devanagari-decompose-string) "devan-util" "\
Decompose Devanagari glyph string STR to basic Devanagari character string." nil nil)

(autoload (quote devanagari-decompose-region) "devan-util" nil t nil)

(autoload (quote devanagari-compose-string) "devan-util" nil nil nil)

(autoload (quote devanagari-compose-region) "devan-util" nil t nil)

(autoload (quote devanagari-compose-from-is13194-region) "devan-util" "\
Compose IS 13194 characters in the region to Devanagari characters." t nil)

(autoload (quote in-is13194-devanagari-post-read-conversion) "devan-util" nil nil nil)

(autoload (quote devanagari-decompose-to-is13194-region) "devan-util" "\
Decompose Devanagari characters in the region to IS 13194 characters." t nil)

(autoload (quote in-is13194-devanagari-pre-write-conversion) "devan-util" nil nil nil)

(autoload (quote devanagari-encode-itrans-region) "devan-util" nil t nil)

(autoload (quote devanagari-decode-itrans-region) "devan-util" nil t nil)

;;;***

;;;### (autoloads (diary-mail-entries diary) "diary-lib" "calendar/diary-lib.el"
;;;;;;  (14151 2344))
;;; Generated autoloads from calendar/diary-lib.el

(autoload (quote diary) "diary-lib" "\
Generate the diary window for ARG days starting with the current date.
If no argument is provided, the number of days of diary entries is governed
by the variable `number-of-diary-entries'.  This function is suitable for
execution in a `.emacs' file." t nil)

(autoload (quote diary-mail-entries) "diary-lib" "\
Send a mail message showing diary entries for next NDAYS days.
If no prefix argument is given, NDAYS is set to `diary-mail-days'.

You can call `diary-mail-entries' every night using an at/cron job.
For example, this script will run the program at 2am daily.  Since
`emacs -batch' does not load your `.emacs' file, you must ensure that
all relevant variables are set, as done here.

#!/bin/sh
# diary-rem.sh -- repeatedly run the Emacs diary-reminder
emacs -batch \\
-eval \"(setq diary-mail-days 3 \\
             european-calendar-style t \\
             diary-mail-addr \\\"user@host.name\\\" )\" \\
-l diary-lib -f diary-mail-entries 
at -f diary-rem.sh 0200 tomorrow

You may have to tweak the syntax of the `at' command to suit your
system.  Alternatively, you can specify a cron entry:
0 1 * * * diary-rem.sh
to run it every morning at 1am." t nil)

;;;***

;;;### (autoloads (diff-backup diff diff-command diff-switches) "diff"
;;;;;;  "diff.el" (13332 40193))
;;; Generated autoloads from diff.el

(defvar diff-switches "-c" "\
*A string or list of strings specifying switches to be be passed to diff.")

(defvar diff-command "diff" "\
*The command to use to run diff.")

(autoload (quote diff) "diff" "\
Find and display the differences between OLD and NEW files.
Interactively the current buffer's file name is the default for NEW
and a backup file for NEW is the default for OLD.
With prefix arg, prompt for diff switches." t nil)

(autoload (quote diff-backup) "diff" "\
Diff this file with its backup file or vice versa.
Uses the latest backup, if there are several numerical backups.
If this file is a backup, diff it with its original.
The backup file is the first file given to `diff'." t nil)

;;;***

;;;### (autoloads (dired-noselect dired-other-frame dired-other-window
;;;;;;  dired dired-copy-preserve-time dired-dwim-target dired-keep-marker-symlink
;;;;;;  dired-keep-marker-hardlink dired-keep-marker-copy dired-keep-marker-rename
;;;;;;  dired-trivial-filenames dired-ls-F-marks-symlinks dired-listing-switches)
;;;;;;  "dired" "dired.el" (14052 36795))
;;; Generated autoloads from dired.el

(defvar dired-listing-switches "-al" "\
*Switches passed to `ls' for dired.  MUST contain the `l' option.
May contain all other options that don't contradict `-l';
may contain even `F', `b', `i' and `s'.  See also the variable
`dired-ls-F-marks-symlinks' concerning the `F' switch.")

(defvar dired-chown-program (if (memq system-type (quote (hpux dgux usg-unix-v irix linux gnu/linux))) "chown" (if (file-exists-p "/usr/sbin/chown") "/usr/sbin/chown" "/etc/chown")) "\
Name of chown command (usually `chown' or `/etc/chown').")

(defvar dired-ls-F-marks-symlinks nil "\
*Informs dired about how `ls -lF' marks symbolic links.
Set this to t if `ls' (or whatever program is specified by
`insert-directory-program') with `-lF' marks the symbolic link
itself with a trailing @ (usually the case under Ultrix).

Example: if `ln -s foo bar; ls -F bar' gives `bar -> foo', set it to
nil (the default), if it gives `bar@ -> foo', set it to t.

Dired checks if there is really a @ appended.  Thus, if you have a
marking `ls' program on one host and a non-marking on another host, and
don't care about symbolic links which really end in a @, you can
always set this variable to t.")

(defvar dired-trivial-filenames "^\\.\\.?$\\|^#" "\
*Regexp of files to skip when finding first file of a directory.
A value of nil means move to the subdir line.
A value of t means move to first file.")

(defvar dired-keep-marker-rename t "\
*Controls marking of renamed files.
If t, files keep their previous marks when they are renamed.
If a character, renamed files (whether previously marked or not)
are afterward marked with that character.")

(defvar dired-keep-marker-copy 67 "\
*Controls marking of copied files.
If t, copied files are marked if and as the corresponding original files were.
If a character, copied files are unconditionally marked with that character.")

(defvar dired-keep-marker-hardlink 72 "\
*Controls marking of newly made hard links.
If t, they are marked if and as the files linked to were marked.
If a character, new links are unconditionally marked with that character.")

(defvar dired-keep-marker-symlink 89 "\
*Controls marking of newly made symbolic links.
If t, they are marked if and as the files linked to were marked.
If a character, new links are unconditionally marked with that character.")

(defvar dired-dwim-target nil "\
*If non-nil, dired tries to guess a default target directory.
This means: if there is a dired buffer displayed in the next window,
use its current subdir, instead of the current subdir of this dired buffer.

The target is used in the prompt for file copy, rename etc.")

(defvar dired-copy-preserve-time t "\
*If non-nil, Dired preserves the last-modified time in a file copy.
\(This works on only some systems.)")
 (define-key ctl-x-map "d" 'dired)

(autoload (quote dired) "dired" "\
\"Edit\" directory DIRNAME--delete, rename, print, etc. some files in it.
Optional second argument SWITCHES specifies the `ls' options used.
\(Interactively, use a prefix argument to be able to specify SWITCHES.)
Dired displays a list of files in DIRNAME (which may also have
shell wildcards appended to select certain files).  If DIRNAME is a cons,
its first element is taken as the directory name and the rest as an explicit
list of files to make directory entries for.
\\<dired-mode-map>You can move around in it with the usual commands.
You can flag files for deletion with \\[dired-flag-file-deletion] and then
delete them by typing \\[dired-do-flagged-delete].
Type \\[describe-mode] after entering dired for more info.

If DIRNAME is already in a dired buffer, that buffer is used without refresh." t nil)
 (define-key ctl-x-4-map "d" 'dired-other-window)

(autoload (quote dired-other-window) "dired" "\
\"Edit\" directory DIRNAME.  Like `dired' but selects in another window." t nil)
 (define-key ctl-x-5-map "d" 'dired-other-frame)

(autoload (quote dired-other-frame) "dired" "\
\"Edit\" directory DIRNAME.  Like `dired' but makes a new frame." t nil)

(autoload (quote dired-noselect) "dired" "\
Like `dired' but returns the dired buffer as value, does not select it." nil nil)

;;;***

;;;### (autoloads (dired-do-query-replace dired-do-search dired-hide-all
;;;;;;  dired-hide-subdir dired-tree-down dired-tree-up dired-kill-subdir
;;;;;;  dired-mark-subdir-files dired-goto-subdir dired-prev-subdir
;;;;;;  dired-maybe-insert-subdir dired-downcase dired-upcase dired-do-symlink-regexp
;;;;;;  dired-do-hardlink-regexp dired-do-copy-regexp dired-do-rename-regexp
;;;;;;  dired-do-rename dired-do-hardlink dired-do-symlink dired-do-copy
;;;;;;  dired-create-directory dired-rename-file dired-copy-file
;;;;;;  dired-relist-file dired-remove-file dired-add-file dired-do-redisplay
;;;;;;  dired-do-load dired-do-byte-compile dired-do-compress dired-compress-file
;;;;;;  dired-do-kill-lines dired-do-shell-command dired-do-print
;;;;;;  dired-do-chown dired-do-chgrp dired-do-chmod dired-backup-diff
;;;;;;  dired-diff) "dired-aux" "dired-aux.el" (14157 4475))
;;; Generated autoloads from dired-aux.el

(autoload (quote dired-diff) "dired-aux" "\
Compare file at point with file FILE using `diff'.
FILE defaults to the file at the mark.
The prompted-for file is the first file given to `diff'.
With prefix arg, prompt for second argument SWITCHES,
 which is options for `diff'." t nil)

(autoload (quote dired-backup-diff) "dired-aux" "\
Diff this file with its backup file or vice versa.
Uses the latest backup, if there are several numerical backups.
If this file is a backup, diff it with its original.
The backup file is the first file given to `diff'.
With prefix arg, prompt for argument SWITCHES which is options for `diff'." t nil)

(autoload (quote dired-do-chmod) "dired-aux" "\
Change the mode of the marked (or next ARG) files.
This calls chmod, thus symbolic modes like `g+w' are allowed." t nil)

(autoload (quote dired-do-chgrp) "dired-aux" "\
Change the group of the marked (or next ARG) files." t nil)

(autoload (quote dired-do-chown) "dired-aux" "\
Change the owner of the marked (or next ARG) files." t nil)

(autoload (quote dired-do-print) "dired-aux" "\
Print the marked (or next ARG) files.
Uses the shell command coming from variables `lpr-command' and
`lpr-switches' as default." t nil)

(autoload (quote dired-do-shell-command) "dired-aux" "\
Run a shell command COMMAND on the marked files.
If no files are marked or a specific numeric prefix arg is given,
the next ARG files are used.  Just \\[universal-argument] means the current file.
The prompt mentions the file(s) or the marker, as appropriate.

If there is output, it goes to a separate buffer.

Normally the command is run on each file individually.
However, if there is a `*' in the command then it is run
just once with the entire file list substituted there.

No automatic redisplay of dired buffers is attempted, as there's no
telling what files the command may have changed.  Type
\\[dired-do-redisplay] to redisplay the marked files.

The shell command has the top level directory as working directory, so
output files usually are created there instead of in a subdir." t nil)

(autoload (quote dired-do-kill-lines) "dired-aux" "\
Kill all marked lines (not the files).
With a prefix argument, kill that many lines starting with the current line.
\(A negative argument kills lines before the current line.)
To kill an entire subdirectory, go to its directory header line
and use this command with a prefix argument (the value does not matter)." t nil)

(autoload (quote dired-compress-file) "dired-aux" nil nil nil)

(autoload (quote dired-do-compress) "dired-aux" "\
Compress or uncompress marked (or next ARG) files." t nil)

(autoload (quote dired-do-byte-compile) "dired-aux" "\
Byte compile marked (or next ARG) Emacs Lisp files." t nil)

(autoload (quote dired-do-load) "dired-aux" "\
Load the marked (or next ARG) Emacs Lisp files." t nil)

(autoload (quote dired-do-redisplay) "dired-aux" "\
Redisplay all marked (or next ARG) files.
If on a subdir line, redisplay that subdirectory.  In that case,
a prefix arg lets you edit the `ls' switches used for the new listing." t nil)

(autoload (quote dired-add-file) "dired-aux" nil nil nil)

(autoload (quote dired-remove-file) "dired-aux" nil nil nil)

(autoload (quote dired-relist-file) "dired-aux" nil nil nil)

(autoload (quote dired-copy-file) "dired-aux" nil nil nil)

(autoload (quote dired-rename-file) "dired-aux" nil nil nil)

(autoload (quote dired-create-directory) "dired-aux" "\
Create a directory called DIRECTORY." t nil)

(autoload (quote dired-do-copy) "dired-aux" "\
Copy all marked (or next ARG) files, or copy the current file.
This normally preserves the last-modified date when copying.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory,
and new copies of these files are made in that directory
with the same names that the files currently have." t nil)

(autoload (quote dired-do-symlink) "dired-aux" "\
Make symbolic links to current file or all marked (or next ARG) files.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory
and new symbolic links are made in that directory
with the same names that the files currently have." t nil)

(autoload (quote dired-do-hardlink) "dired-aux" "\
Add names (hard links) current file or all marked (or next ARG) files.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory
and new hard links are made in that directory
with the same names that the files currently have." t nil)

(autoload (quote dired-do-rename) "dired-aux" "\
Rename current file or all marked (or next ARG) files.
When renaming just the current file, you specify the new name.
When renaming multiple or marked files, you specify a directory." t nil)

(autoload (quote dired-do-rename-regexp) "dired-aux" "\
Rename marked files containing REGEXP to NEWNAME.
As each match is found, the user must type a character saying
  what to do with it.  For directions, type \\[help-command] at that time.
NEWNAME may contain \\=\\<n> or \\& as in `query-replace-regexp'.
REGEXP defaults to the last regexp used.

With a zero prefix arg, renaming by regexp affects the absolute file name.
Normally, only the non-directory part of the file name is used and changed." t nil)

(autoload (quote dired-do-copy-regexp) "dired-aux" "\
Copy all marked files containing REGEXP to NEWNAME.
See function `dired-do-rename-regexp' for more info." t nil)

(autoload (quote dired-do-hardlink-regexp) "dired-aux" "\
Hardlink all marked files containing REGEXP to NEWNAME.
See function `dired-do-rename-regexp' for more info." t nil)

(autoload (quote dired-do-symlink-regexp) "dired-aux" "\
Symlink all marked files containing REGEXP to NEWNAME.
See function `dired-do-rename-regexp' for more info." t nil)

(autoload (quote dired-upcase) "dired-aux" "\
Rename all marked (or next ARG) files to upper case." t nil)

(autoload (quote dired-downcase) "dired-aux" "\
Rename all marked (or next ARG) files to lower case." t nil)

(autoload (quote dired-maybe-insert-subdir) "dired-aux" "\
Insert this subdirectory into the same dired buffer.
If it is already present, just move to it (type \\[dired-do-redisplay] to refresh),
  else inserts it at its natural place (as `ls -lR' would have done).
With a prefix arg, you may edit the ls switches used for this listing.
  You can add `R' to the switches to expand the whole tree starting at
  this subdirectory.
This function takes some pains to conform to `ls -lR' output." t nil)

(autoload (quote dired-prev-subdir) "dired-aux" "\
Go to previous subdirectory, regardless of level.
When called interactively and not on a subdir line, go to this subdir's line." t nil)

(autoload (quote dired-goto-subdir) "dired-aux" "\
Go to end of header line of DIR in this dired buffer.
Return value of point on success, otherwise return nil.
The next char is either \\n, or \\r if DIR is hidden." t nil)

(autoload (quote dired-mark-subdir-files) "dired-aux" "\
Mark all files except `.' and `..' in current subdirectory.
If the Dired buffer shows multiple directories, this command
marks the files listed in the subdirectory that point is in." t nil)

(autoload (quote dired-kill-subdir) "dired-aux" "\
Remove all lines of current subdirectory.
Lower levels are unaffected." t nil)

(autoload (quote dired-tree-up) "dired-aux" "\
Go up ARG levels in the dired tree." t nil)

(autoload (quote dired-tree-down) "dired-aux" "\
Go down in the dired tree." t nil)

(autoload (quote dired-hide-subdir) "dired-aux" "\
Hide or unhide the current subdirectory and move to next directory.
Optional prefix arg is a repeat factor.
Use \\[dired-hide-all] to (un)hide all directories." t nil)

(autoload (quote dired-hide-all) "dired-aux" "\
Hide all subdirectories, leaving only their header lines.
If there is already something hidden, make everything visible again.
Use \\[dired-hide-subdir] to (un)hide a particular subdirectory." t nil)

(autoload (quote dired-do-search) "dired-aux" "\
Search through all marked files for a match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue]." t nil)

(autoload (quote dired-do-query-replace) "dired-aux" "\
Do `query-replace-regexp' of FROM with TO, on all marked files.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit] or ESC), you can resume the query replace
with the command \\[tags-loop-continue]." t nil)

;;;***

;;;### (autoloads (dired-jump) "dired-x" "dired-x.el" (13796 29070))
;;; Generated autoloads from dired-x.el

(autoload (quote dired-jump) "dired-x" "\
Jump to dired buffer corresponding to current buffer.
If in a file, dired the current directory and move to file's line.
If in dired already, pop up a level and goto old directory's line.
In case the proper dired file line cannot be found, refresh the dired
buffer and try again." t nil)

;;;***

;;;### (autoloads (dirtrack) "dirtrack" "dirtrack.el" (14032 29853))
;;; Generated autoloads from dirtrack.el

(autoload (quote dirtrack) "dirtrack" "\
Determine the current directory by scanning the process output for a prompt.
The prompt to look for is the first item in `dirtrack-list'.

You can toggle directory tracking by using the function `dirtrack-toggle'.

If directory tracking does not seem to be working, you can use the
function `dirtrack-debug-toggle' to turn on debugging output.

You can enable directory tracking by adding this function to 
`comint-output-filter-functions'.
" nil nil)

;;;***

;;;### (autoloads (disassemble) "disass" "emacs-lisp/disass.el" (13776
;;;;;;  9510))
;;; Generated autoloads from emacs-lisp/disass.el

(autoload (quote disassemble) "disass" "\
Print disassembled code for OBJECT in (optional) BUFFER.
OBJECT can be a symbol defined as a function, or a function itself
\(a lambda expression or a compiled-function object).
If OBJECT is not already compiled, we compile it, but do not
redefine OBJECT if it is a symbol." t nil)

;;;***

;;;### (autoloads (standard-display-european create-glyph standard-display-underline
;;;;;;  standard-display-graphic standard-display-g1 standard-display-ascii
;;;;;;  standard-display-default standard-display-8bit describe-current-display-table
;;;;;;  describe-display-table set-display-table-slot display-table-slot
;;;;;;  make-display-table) "disp-table" "disp-table.el" (14168 7661))
;;; Generated autoloads from disp-table.el

(autoload (quote make-display-table) "disp-table" "\
Return a new, empty display table." nil nil)

(autoload (quote display-table-slot) "disp-table" "\
Return the value of the extra slot in DISPLAY-TABLE named SLOT.
SLOT may be a number from 0 to 5 inclusive, or a slot name (symbol).
Valid symbols are `truncation', `wrap', `escape', `control',
`selective-display', and `vertical-border'." nil nil)

(autoload (quote set-display-table-slot) "disp-table" "\
Set the value of the extra slot in DISPLAY-TABLE named SLOT to VALUE.
SLOT may be a number from 0 to 5 inclusive, or a name (symbol).
Valid symbols are `truncation', `wrap', `escape', `control',
`selective-display', and `vertical-border'." nil nil)

(autoload (quote describe-display-table) "disp-table" "\
Describe the display table DT in a help buffer." nil nil)

(autoload (quote describe-current-display-table) "disp-table" "\
Describe the display table in use in the selected window and buffer." t nil)

(autoload (quote standard-display-8bit) "disp-table" "\
Display characters in the range L to H literally." nil nil)

(autoload (quote standard-display-default) "disp-table" "\
Display characters in the range L to H using the default notation." nil nil)

(autoload (quote standard-display-ascii) "disp-table" "\
Display character C using printable string S." nil nil)

(autoload (quote standard-display-g1) "disp-table" "\
Display character C as character SC in the g1 character set.
This function assumes that your terminal uses the SO/SI characters;
it is meaningless for an X frame." nil nil)

(autoload (quote standard-display-graphic) "disp-table" "\
Display character C as character GC in graphics character set.
This function assumes VT100-compatible escapes; it is meaningless for an
X frame." nil nil)

(autoload (quote standard-display-underline) "disp-table" "\
Display character C as character UC plus underlining." nil nil)

(autoload (quote create-glyph) "disp-table" nil nil nil)

(autoload (quote standard-display-european) "disp-table" "\
Semi-obsolete way to toggle display of ISO 8859 European characters.

This function is semi-obsolete; if you want to do your editing with
unibyte characters, it is better to `set-language-environment' coupled
with either the `--unibyte' option or the EMACS_UNIBYTE environment
variable, or else customize `enable-multibyte-characters'.

With prefix argument, this command enables European character display
if arg is positive, disables it otherwise.  Otherwise, it toggles
European character display.

When this mode is enabled, characters in the range of 160 to 255
display not as octal escapes, but as accented characters.  Codes 146
and 160 display as apostrophe and space, even though they are not the
ASCII codes for apostrophe and space.

Enabling European character display with this command noninteractively
from Lisp code also selects Latin-1 as the language environment, and
selects unibyte mode for all Emacs buffers (both existing buffers and
those created subsequently).  This provides increased compatibility
for users who call this function in `.emacs'." t nil)

;;;***

;;;### (autoloads (dissociated-press) "dissociate" "play/dissociate.el"
;;;;;;  (13229 28169))
;;; Generated autoloads from play/dissociate.el

(autoload (quote dissociated-press) "dissociate" "\
Dissociate the text of the current buffer.
Output goes in buffer named *Dissociation*,
which is redisplayed each time text is added to it.
Every so often the user must say whether to continue.
If ARG is positive, require ARG chars of continuity.
If ARG is negative, require -ARG words of continuity.
Default is 2." t nil)

;;;***

;;;### (autoloads (doctor) "doctor" "play/doctor.el" (13556 41287))
;;; Generated autoloads from play/doctor.el

(autoload (quote doctor) "doctor" "\
Switch to *doctor* buffer and start giving psychotherapy." t nil)

;;;***

;;;### (autoloads (double-mode) "double" "double.el" (13777 61323))
;;; Generated autoloads from double.el

(autoload (quote double-mode) "double" "\
Toggle Double mode.
With prefix arg, turn Double mode on iff arg is positive.

When Double mode is on, some keys will insert different strings
when pressed twice.  See variable `double-map' for details." t nil)

;;;***

;;;### (autoloads (dunnet) "dunnet" "play/dunnet.el" (13607 44565))
;;; Generated autoloads from play/dunnet.el

(autoload (quote dunnet) "dunnet" "\
Switch to *dungeon* buffer and start game." t nil)

;;;***

;;;### (autoloads (gnus-earcon-display) "earcon" "gnus/earcon.el"
;;;;;;  (14030 49072))
;;; Generated autoloads from gnus/earcon.el

(autoload (quote gnus-earcon-display) "earcon" "\
Play sounds in message buffers." t nil)

;;;***

;;;### (autoloads (easy-mmode-define-minor-mode) "easy-mmode" "emacs-lisp/easy-mmode.el"
;;;;;;  (14213 21772))
;;; Generated autoloads from emacs-lisp/easy-mmode.el

(autoload (quote easy-mmode-define-minor-mode) "easy-mmode" "\
Define a new minor mode MODE.
This function defines the associated control variable, keymap,
toggle command, and hooks (see `easy-mmode-define-toggle').

DOC is the documentation for the mode toggle command.
Optional LIGHTER is displayed in the mode-bar when the mode is on.
Optional KEYMAP is the default (defvar) keymap bound to the mode keymap.
If it is a list, it is passed to `easy-mmode-define-keymap'
in order to build a valid keymap.
 
\(defmacro easy-mmode-define-minor-mode
  (MODE DOC &optional INIT-VALUE LIGHTER KEYMAP)...)" nil (quote macro))

;;;***

;;;### (autoloads (easy-menu-change easy-menu-create-menu easy-menu-do-define
;;;;;;  easy-menu-define) "easymenu" "emacs-lisp/easymenu.el" (14228
;;;;;;  23892))
;;; Generated autoloads from emacs-lisp/easymenu.el

(autoload (quote easy-menu-define) "easymenu" "\
Define a menu bar submenu in maps MAPS, according to MENU.
The menu keymap is stored in symbol SYMBOL, both as its value
and as its function definition.   DOC is used as the doc string for SYMBOL.

The first element of MENU must be a string.  It is the menu bar item name.
It may be followed by the following keyword argument pairs

   :filter FUNCTION

FUNCTION is a function with one argument, the menu.  It returns the actual
menu displayed.

   :visible INCLUDE

INCLUDE is an expression; this menu is only visible if this
expression has a non-nil value.  `:include' is an alias for `:visible'.

   :active ENABLE

ENABLE is an expression; the menu is enabled for selection
whenever this expression's value is non-nil.

The rest of the elements in MENU, are menu items.

A menu item is usually a vector of three elements:  [NAME CALLBACK ENABLE]

NAME is a string--the menu item name.

CALLBACK is a command to run when the item is chosen,
or a list to evaluate when the item is chosen.

ENABLE is an expression; the item is enabled for selection
whenever this expression's value is non-nil.

Alternatively, a menu item may have the form: 

   [ NAME CALLBACK [ KEYWORD ARG ] ... ]

Where KEYWORD is one of the symbols defined below.

   :keys KEYS

KEYS is a string; a complex keyboard equivalent to this menu item.
This is normally not needed because keyboard equivalents are usually
computed automatically.
KEYS is expanded with `substitute-command-keys' before it is used.

   :key-sequence KEYS

KEYS is nil a string or a vector; nil or a keyboard equivalent to this
menu item.
This is a hint that will considerably speed up Emacs first display of
a menu.  Use `:key-sequence nil' when you know that this menu item has no
keyboard equivalent.

   :active ENABLE

ENABLE is an expression; the item is enabled for selection
whenever this expression's value is non-nil.

   :included INCLUDE

INCLUDE is an expression; this item is only visible if this
expression has a non-nil value.

   :suffix NAME

NAME is a string; the name of an argument to CALLBACK.

   :style STYLE
   
STYLE is a symbol describing the type of menu item.  The following are
defined:  

toggle: A checkbox.
        Prepend the name with `(*) ' or `( ) ' depending on if selected or not.
radio: A radio button.
       Prepend the name with `[X] ' or `[ ] ' depending on if selected or not.
button: Surround the name with `[' and `]'. Use this for an item in the
        menu bar itself.
anything else means an ordinary menu item.

   :selected SELECTED

SELECTED is an expression; the checkbox or radio button is selected
whenever this expression's value is non-nil.

A menu item can be a string.  Then that string appears in the menu as
unselectable text.  A string consisting solely of hyphens is displayed
as a solid horizontal line.

A menu item can be a list with the same format as MENU.  This is a submenu." nil (quote macro))

(autoload (quote easy-menu-do-define) "easymenu" nil nil nil)

(autoload (quote easy-menu-create-menu) "easymenu" "\
Create a menu called MENU-NAME with items described in MENU-ITEMS.
MENU-NAME is a string, the name of the menu.  MENU-ITEMS is a list of items
possibly preceded by keyword pairs as described in `easy-menu-define'." nil nil)

(autoload (quote easy-menu-change) "easymenu" "\
Change menu found at PATH as item NAME to contain ITEMS.
PATH is a list of strings for locating the menu containing NAME in the
menu bar.  ITEMS is a list of menu items, as in `easy-menu-define'.
These items entirely replace the previous items in that map.
If NAME is not present in the menu located by PATH, then add item NAME to
that menu. If the optional argument BEFORE is present add NAME in menu
just before BEFORE, otherwise add at end of menu.

Either call this from `menu-bar-update-hook' or use a menu filter,
to implement dynamic menus." nil nil)

;;;***

;;;### (autoloads (electric-buffer-list) "ebuff-menu" "ebuff-menu.el"
;;;;;;  (13778 5246))
;;; Generated autoloads from ebuff-menu.el

(autoload (quote electric-buffer-list) "ebuff-menu" "\
Pops up a buffer describing the set of Emacs buffers.
Vaguely like ITS lunar select buffer; combining typeoutoid buffer
listing with menuoid buffer selection.

If the very next character typed is a space then the buffer list
window disappears.  Otherwise, one may move around in the buffer list
window, marking buffers to be selected, saved or deleted.

To exit and select a new buffer, type a space when the cursor is on
the appropriate line of the buffer-list window.  Other commands are
much like those of buffer-menu-mode.

Calls value of `electric-buffer-menu-mode-hook' on entry if non-nil.

\\{electric-buffer-menu-mode-map}" t nil)

;;;***

;;;### (autoloads (Electric-command-history-redo-expression) "echistory"
;;;;;;  "echistory.el" (13229 28451))
;;; Generated autoloads from echistory.el

(autoload (quote Electric-command-history-redo-expression) "echistory" "\
Edit current history line in minibuffer and execute result.
With prefix arg NOCONFIRM, execute current line as-is without editing." t nil)

;;;***

;;;### (autoloads (edebug-eval-top-level-form def-edebug-spec edebug-all-forms
;;;;;;  edebug-all-defs) "edebug" "emacs-lisp/edebug.el" (13988 42888))
;;; Generated autoloads from emacs-lisp/edebug.el

(defvar edebug-all-defs nil "\
*If non-nil, evaluation of any defining forms will instrument for Edebug.
This applies to `eval-defun', `eval-region', `eval-buffer', and
`eval-current-buffer'.  `eval-region' is also called by
`eval-last-sexp', and `eval-print-last-sexp'.

You can use the command `edebug-all-defs' to toggle the value of this
variable.  You may wish to make it local to each buffer with
\(make-local-variable 'edebug-all-defs) in your
`emacs-lisp-mode-hook'.")

(defvar edebug-all-forms nil "\
*Non-nil evaluation of all forms will instrument for Edebug.
This doesn't apply to loading or evaluations in the minibuffer.
Use the command `edebug-all-forms' to toggle the value of this option.")

(autoload (quote def-edebug-spec) "edebug" "\
Set the edebug-form-spec property of SYMBOL according to SPEC.
Both SYMBOL and SPEC are unevaluated. The SPEC can be 0, t, a symbol
\(naming a function), or a list." nil (quote macro))

(defalias (quote edebug-defun) (quote edebug-eval-top-level-form))

(autoload (quote edebug-eval-top-level-form) "edebug" "\
Evaluate a top level form, such as a defun or defmacro.
This is like `eval-defun', but the code is always instrumented for Edebug.
Print its name in the minibuffer and leave point where it is,
or if an error occurs, leave point after it with mark at the original point." t nil)

;;;***

;;;### (autoloads (ediff-documentation ediff-version ediff-revision
;;;;;;  ediff-patch-buffer ediff-patch-file run-ediff-from-cvs-buffer
;;;;;;  ediff-merge-revisions-with-ancestor ediff-merge-revisions
;;;;;;  ediff-merge-buffers-with-ancestor ediff-merge-buffers ediff-merge-files-with-ancestor
;;;;;;  ediff-merge-files ediff-regions-linewise ediff-regions-wordwise
;;;;;;  ediff-windows-linewise ediff-windows-wordwise ediff-merge-directory-revisions-with-ancestor
;;;;;;  ediff-merge-directory-revisions ediff-merge-directories-with-ancestor
;;;;;;  ediff-merge-directories ediff-directories3 ediff-directory-revisions
;;;;;;  ediff-directories ediff-buffers3 ediff-buffers ediff-files3
;;;;;;  ediff-files) "ediff" "ediff.el" (13680 6249))
;;; Generated autoloads from ediff.el

(autoload (quote ediff-files) "ediff" "\
Run Ediff on a pair of files, FILE-A and FILE-B." t nil)

(autoload (quote ediff-files3) "ediff" "\
Run Ediff on three files, FILE-A, FILE-B, and FILE-C." t nil)

(defalias (quote ediff3) (quote ediff-files3))

(defalias (quote ediff) (quote ediff-files))

(autoload (quote ediff-buffers) "ediff" "\
Run Ediff on a pair of buffers, BUFFER-A and BUFFER-B." t nil)

(defalias (quote ebuffers) (quote ediff-buffers))

(autoload (quote ediff-buffers3) "ediff" "\
Run Ediff on three buffers, BUFFER-A, BUFFER-B, and BUFFER-C." t nil)

(defalias (quote ebuffers3) (quote ediff-buffers3))

(autoload (quote ediff-directories) "ediff" "\
Run Ediff on a pair of directories, DIR1 and DIR2, comparing files that have
the same name in both. The third argument, REGEXP, is a regular expression that
can be used to filter out certain file names." t nil)

(defalias (quote edirs) (quote ediff-directories))

(autoload (quote ediff-directory-revisions) "ediff" "\
Run Ediff on a directory, DIR1, comparing its files with their revisions.
The second argument, REGEXP, is a regular expression that filters the file
names. Only the files that are under revision control are taken into account." t nil)

(defalias (quote edir-revisions) (quote ediff-directory-revisions))

(autoload (quote ediff-directories3) "ediff" "\
Run Ediff on three directories, DIR1, DIR2, and DIR3, comparing files that
have the same name in all three. The last argument, REGEXP, is a regular
expression that can be used to filter out certain file names." t nil)

(defalias (quote edirs3) (quote ediff-directories3))

(autoload (quote ediff-merge-directories) "ediff" "\
Run Ediff on a pair of directories, DIR1 and DIR2, merging files that have
the same name in both. The third argument, REGEXP, is a regular expression that
can be used to filter out certain file names." t nil)

(defalias (quote edirs-merge) (quote ediff-merge-directories))

(autoload (quote ediff-merge-directories-with-ancestor) "ediff" "\
Merge files in directories DIR1 and DIR2 using files in ANCESTOR-DIR as ancestors.
Ediff merges files that have identical names in DIR1, DIR2. If a pair of files
in DIR1 and DIR2 doesn't have an ancestor in ANCESTOR-DIR, Ediff will merge
without ancestor. The fourth argument, REGEXP, is a regular expression that
can be used to filter out certain file names." t nil)

(autoload (quote ediff-merge-directory-revisions) "ediff" "\
Run Ediff on a directory, DIR1, merging its files with their revisions.
The second argument, REGEXP, is a regular expression that filters the file
names. Only the files that are under revision control are taken into account." t nil)

(defalias (quote edir-merge-revisions) (quote ediff-merge-directory-revisions))

(autoload (quote ediff-merge-directory-revisions-with-ancestor) "ediff" "\
Run Ediff on a directory, DIR1, merging its files with their revisions and ancestors.
The second argument, REGEXP, is a regular expression that filters the file
names. Only the files that are under revision control are taken into account." t nil)

(defalias (quote edir-merge-revisions-with-ancestor) (quote ediff-merge-directory-revisions-with-ancestor))

(defalias (quote edirs-merge-with-ancestor) (quote ediff-merge-directories-with-ancestor))

(autoload (quote ediff-windows-wordwise) "ediff" "\
Compare WIND-A and WIND-B, which are selected by clicking, wordwise.
With prefix argument, DUMB-MODE, or on a non-windowing display, works as
follows:
If WIND-A is nil, use selected window.
If WIND-B is nil, use window next to WIND-A." t nil)

(autoload (quote ediff-windows-linewise) "ediff" "\
Compare WIND-A and WIND-B, which are selected by clicking, linewise.
With prefix argument, DUMB-MODE, or on a non-windowing display, works as
follows:
If WIND-A is nil, use selected window.
If WIND-B is nil, use window next to WIND-A." t nil)

(autoload (quote ediff-regions-wordwise) "ediff" "\
Run Ediff on a pair of regions in two different buffers.
Regions (i.e., point and mark) are assumed to be set in advance.
This function is effective only for relatively small regions, up to 200
lines. For large regions, use `ediff-regions-linewise'." t nil)

(autoload (quote ediff-regions-linewise) "ediff" "\
Run Ediff on a pair of regions in two different buffers.
Regions (i.e., point and mark) are assumed to be set in advance.
Each region is enlarged to contain full lines.
This function is effective for large regions, over 100-200
lines. For small regions, use `ediff-regions-wordwise'." t nil)

(defalias (quote ediff-merge) (quote ediff-merge-files))

(autoload (quote ediff-merge-files) "ediff" "\
Merge two files without ancestor." t nil)

(autoload (quote ediff-merge-files-with-ancestor) "ediff" "\
Merge two files with ancestor." t nil)

(defalias (quote ediff-merge-with-ancestor) (quote ediff-merge-files-with-ancestor))

(autoload (quote ediff-merge-buffers) "ediff" "\
Merge buffers without ancestor." t nil)

(autoload (quote ediff-merge-buffers-with-ancestor) "ediff" "\
Merge buffers with ancestor." t nil)

(autoload (quote ediff-merge-revisions) "ediff" "\
Run Ediff by merging two revisions of a file.
The file is the optional FILE argument or the file visited by the current
buffer." t nil)

(autoload (quote ediff-merge-revisions-with-ancestor) "ediff" "\
Run Ediff by merging two revisions of a file with a common ancestor.
The file is the the optional FILE argument or the file visited by the current
buffer." t nil)

(autoload (quote run-ediff-from-cvs-buffer) "ediff" "\
Run Ediff-merge on appropriate revisions of the selected file.
First run after `M-x cvs-update'. Then place the cursor on a line describing a
file and then run `run-ediff-from-cvs-buffer'." t nil)

(autoload (quote ediff-patch-file) "ediff" "\
Run Ediff by patching SOURCE-FILENAME." t nil)

(autoload (quote ediff-patch-buffer) "ediff" "\
Run Ediff by patching BUFFER-NAME." t nil)

(defalias (quote epatch) (quote ediff-patch-file))

(defalias (quote epatch-buffer) (quote ediff-patch-buffer))

(autoload (quote ediff-revision) "ediff" "\
Run Ediff by comparing versions of a file.
The file is an optional FILE argument or the file visited by the current
buffer. Use `vc.el' or `rcs.el' depending on `ediff-version-control-package'." t nil)

(defalias (quote erevision) (quote ediff-revision))

(autoload (quote ediff-version) "ediff" "\
Return string describing the version of Ediff.
When called interactively, displays the version." t nil)

(autoload (quote ediff-documentation) "ediff" "\
Display Ediff's manual.
With optional NODE, goes to that node." t nil)

;;;***

;;;### (autoloads (ediff-show-registry) "ediff-mult" "ediff-mult.el"
;;;;;;  (13680 6243))
;;; Generated autoloads from ediff-mult.el

(autoload (quote ediff-show-registry) "ediff-mult" "\
Display Ediff's registry." t nil)

(defalias (quote eregistry) (quote ediff-show-registry))

;;;***

;;;### (autoloads (ediff-toggle-use-toolbar ediff-toggle-multiframe)
;;;;;;  "ediff-util" "ediff-util.el" (13896 8693))
;;; Generated autoloads from ediff-util.el

(autoload (quote ediff-toggle-multiframe) "ediff-util" "\
Switch from multiframe display to single-frame display and back.
To change the default, set the variable `ediff-window-setup-function',
which see." t nil)

(autoload (quote ediff-toggle-use-toolbar) "ediff-util" "\
Enable or disable Ediff toolbar.
Works only in versions of Emacs that support toolbars.
To change the default, set the variable `ediff-use-toolbar-p', which see." t nil)

;;;***

;;;### (autoloads (format-kbd-macro read-kbd-macro edit-named-kbd-macro
;;;;;;  edit-last-kbd-macro edit-kbd-macro) "edmacro" "edmacro.el"
;;;;;;  (13957 59578))
;;; Generated autoloads from edmacro.el
 (define-key ctl-x-map "\C-k" 'edit-kbd-macro)

(defvar edmacro-eight-bits nil "\
*Non-nil if edit-kbd-macro should leave 8-bit characters intact.
Default nil means to write characters above \\177 in octal notation.")

(autoload (quote edit-kbd-macro) "edmacro" "\
Edit a keyboard macro.
At the prompt, type any key sequence which is bound to a keyboard macro.
Or, type `C-x e' or RET to edit the last keyboard macro, `C-h l' to edit
the last 100 keystrokes as a keyboard macro, or `M-x' to edit a macro by
its command name.
With a prefix argument, format the macro in a more concise way." t nil)

(autoload (quote edit-last-kbd-macro) "edmacro" "\
Edit the most recently defined keyboard macro." t nil)

(autoload (quote edit-named-kbd-macro) "edmacro" "\
Edit a keyboard macro which has been given a name by `name-last-kbd-macro'." t nil)

(autoload (quote read-kbd-macro) "edmacro" "\
Read the region as a keyboard macro definition.
The region is interpreted as spelled-out keystrokes, e.g., \"M-x abc RET\".
See documentation for `edmacro-mode' for details.
Leading/trailing \"C-x (\" and \"C-x )\" in the text are allowed and ignored.
The resulting macro is installed as the \"current\" keyboard macro.

In Lisp, may also be called with a single STRING argument in which case
the result is returned rather than being installed as the current macro.
The result will be a string if possible, otherwise an event vector.
Second argument NEED-VECTOR means to return an event vector always." t nil)

(autoload (quote format-kbd-macro) "edmacro" "\
Return the keyboard macro MACRO as a human-readable string.
This string is suitable for passing to `read-kbd-macro'.
Second argument VERBOSE means to put one command per line with comments.
If VERBOSE is `1', put everything on one line.  If VERBOSE is omitted
or nil, use a compact 80-column format." nil nil)

;;;***

;;;### (autoloads (edt-emulation-on) "edt" "emulation/edt.el" (13271
;;;;;;  33643))
;;; Generated autoloads from emulation/edt.el

(autoload (quote edt-emulation-on) "edt" "\
Turn on EDT Emulation." t nil)

;;;***

;;;### (autoloads (electric-helpify with-electric-help) "ehelp" "ehelp.el"
;;;;;;  (13116 19630))
;;; Generated autoloads from ehelp.el

(autoload (quote with-electric-help) "ehelp" "\
Pop up an \"electric\" help buffer.
The arguments are THUNK &optional BUFFER NOERASE MINHEIGHT.
THUNK is a function of no arguments which is called to initialize the
contents of BUFFER.  BUFFER defaults to `*Help*'.  BUFFER will be
erased before THUNK is called unless NOERASE is non-nil.  THUNK will
be called while BUFFER is current and with `standard-output' bound to
the buffer specified by BUFFER.

If THUNK returns nil, we display BUFFER starting at the top, and
shrink the window to fit.  If THUNK returns non-nil, we don't do those things.

After THUNK has been called, this function \"electrically\" pops up a window
in which BUFFER is displayed and allows the user to scroll through that buffer
in electric-help-mode. The window's height will be at least MINHEIGHT if
this value is non-nil.

If THUNK returns nil, we display BUFFER starting at the top, and
shrink the window to fit.  If THUNK returns non-nil, we don't do those
things.

When the user exits (with `electric-help-exit', or otherwise) the help
buffer's window disappears (i.e., we use `save-window-excursion')
BUFFER is put into `default-major-mode' (or `fundamental-mode') when we exit." nil nil)

(autoload (quote electric-helpify) "ehelp" nil nil nil)

;;;***

;;;### (autoloads (turn-on-eldoc-mode eldoc-mode eldoc-mode) "eldoc"
;;;;;;  "emacs-lisp/eldoc.el" (13881 40287))
;;; Generated autoloads from emacs-lisp/eldoc.el

(defvar eldoc-mode nil "\
*If non-nil, show the defined parameters for the elisp function near point.

For the emacs lisp function at the beginning of the sexp which point is
within, show the defined parameters for the function in the echo area.
This information is extracted directly from the function or macro if it is
in pure lisp.  If the emacs function is a subr, the parameters are obtained
from the documentation string if possible.

If point is over a documented variable, print that variable's docstring
instead.

This variable is buffer-local.")

(autoload (quote eldoc-mode) "eldoc" "\
*Enable or disable eldoc mode.
See documentation for the variable of the same name for more details.

If called interactively with no prefix argument, toggle current condition
of the mode.
If called with a positive or negative prefix argument, enable or disable
the mode, respectively." t nil)

(autoload (quote turn-on-eldoc-mode) "eldoc" "\
Unequivocally turn on eldoc-mode (see variable documentation)." t nil)

;;;***

;;;### (autoloads (elint-initialize) "elint" "emacs-lisp/elint.el"
;;;;;;  (13363 2815))
;;; Generated autoloads from emacs-lisp/elint.el

(autoload (quote elint-initialize) "elint" "\
Initialize elint." t nil)

;;;***

;;;### (autoloads (elp-submit-bug-report elp-results elp-instrument-package
;;;;;;  elp-instrument-list elp-restore-function elp-instrument-function)
;;;;;;  "elp" "emacs-lisp/elp.el" (13578 6112))
;;; Generated autoloads from emacs-lisp/elp.el

(autoload (quote elp-instrument-function) "elp" "\
Instrument FUNSYM for profiling.
FUNSYM must be a symbol of a defined function." t nil)

(autoload (quote elp-restore-function) "elp" "\
Restore an instrumented function to its original definition.
Argument FUNSYM is the symbol of a defined function." t nil)

(autoload (quote elp-instrument-list) "elp" "\
Instrument for profiling, all functions in `elp-function-list'.
Use optional LIST if provided instead." t nil)

(autoload (quote elp-instrument-package) "elp" "\
Instrument for profiling, all functions which start with PREFIX.
For example, to instrument all ELP functions, do the following:

    \\[elp-instrument-package] RET elp- RET" t nil)

(autoload (quote elp-results) "elp" "\
Display current profiling results.
If `elp-reset-after-results' is non-nil, then current profiling
information for all instrumented functions are reset after results are
displayed." t nil)

(autoload (quote elp-submit-bug-report) "elp" "\
Submit via mail, a bug report on elp." t nil)

;;;***

;;;### (autoloads (report-emacs-bug) "emacsbug" "mail/emacsbug.el"
;;;;;;  (13649 21490))
;;; Generated autoloads from mail/emacsbug.el

(autoload (quote report-emacs-bug) "emacsbug" "\
Report a bug in GNU Emacs.
Prompts for bug subject.  Leaves you in a mail buffer." t nil)

;;;***

;;;### (autoloads (emerge-merge-directories emerge-revisions-with-ancestor
;;;;;;  emerge-revisions emerge-files-with-ancestor-remote emerge-files-remote
;;;;;;  emerge-files-with-ancestor-command emerge-files-command emerge-buffers-with-ancestor
;;;;;;  emerge-buffers emerge-files-with-ancestor emerge-files) "emerge"
;;;;;;  "emerge.el" (13661 53123))
;;; Generated autoloads from emerge.el

(defvar menu-bar-emerge-menu (make-sparse-keymap "Emerge"))

(fset (quote menu-bar-emerge-menu) (symbol-value (quote menu-bar-emerge-menu)))

(define-key menu-bar-emerge-menu [emerge-merge-directories] (quote ("Merge Directories..." . emerge-merge-directories)))

(define-key menu-bar-emerge-menu [emerge-revisions-with-ancestor] (quote ("Revisions with Ancestor..." . emerge-revisions-with-ancestor)))

(define-key menu-bar-emerge-menu [emerge-revisions] (quote ("Revisions..." . emerge-revisions)))

(define-key menu-bar-emerge-menu [emerge-files-with-ancestor] (quote ("Files with Ancestor..." . emerge-files-with-ancestor)))

(define-key menu-bar-emerge-menu [emerge-files] (quote ("Files..." . emerge-files)))

(define-key menu-bar-emerge-menu [emerge-buffers-with-ancestor] (quote ("Buffers with Ancestor..." . emerge-buffers-with-ancestor)))

(define-key menu-bar-emerge-menu [emerge-buffers] (quote ("Buffers..." . emerge-buffers)))

(autoload (quote emerge-files) "emerge" "\
Run Emerge on two files." t nil)

(autoload (quote emerge-files-with-ancestor) "emerge" "\
Run Emerge on two files, giving another file as the ancestor." t nil)

(autoload (quote emerge-buffers) "emerge" "\
Run Emerge on two buffers." t nil)

(autoload (quote emerge-buffers-with-ancestor) "emerge" "\
Run Emerge on two buffers, giving another buffer as the ancestor." t nil)

(autoload (quote emerge-files-command) "emerge" nil nil nil)

(autoload (quote emerge-files-with-ancestor-command) "emerge" nil nil nil)

(autoload (quote emerge-files-remote) "emerge" nil nil nil)

(autoload (quote emerge-files-with-ancestor-remote) "emerge" nil nil nil)

(autoload (quote emerge-revisions) "emerge" "\
Emerge two RCS revisions of a file." t nil)

(autoload (quote emerge-revisions-with-ancestor) "emerge" "\
Emerge two RCS revisions of a file, with another revision as ancestor." t nil)

(autoload (quote emerge-merge-directories) "emerge" nil t nil)

;;;***

;;;### (autoloads (encoded-kbd-mode) "encoded-kb" "international/encoded-kb.el"
;;;;;;  (14035 16830))
;;; Generated autoloads from international/encoded-kb.el

(autoload (quote encoded-kbd-mode) "encoded-kb" "\
Toggle Encoded-kbd minor mode.
With arg, turn Encoded-kbd mode on if and only if arg is positive.

You should not turn this mode on manually, instead use the command
\\[set-keyboard-coding-system] which turns on or off this mode
automatically.

In Encoded-kbd mode, a text sent from keyboard is accepted
as a multilingual text encoded in a coding system set by
\\[set-keyboard-coding-system]." nil nil)

;;;***

;;;### (autoloads (enriched-decode enriched-encode enriched-mode)
;;;;;;  "enriched" "enriched.el" (14228 23900))
;;; Generated autoloads from enriched.el

(autoload (quote enriched-mode) "enriched" "\
Minor mode for editing text/enriched files.
These are files with embedded formatting information in the MIME standard
text/enriched format.
Turning the mode on runs `enriched-mode-hook'.

More information about Enriched mode is available in the file 
etc/enriched.doc  in the Emacs distribution directory.

Commands:

\\<enriched-mode-map>\\{enriched-mode-map}" t nil)

(autoload (quote enriched-encode) "enriched" nil nil nil)

(autoload (quote enriched-decode) "enriched" nil nil nil)

;;;***

;;;### (autoloads (setenv) "env" "env.el" (13582 12080))
;;; Generated autoloads from env.el

(autoload (quote setenv) "env" "\
Set the value of the environment variable named VARIABLE to VALUE.
VARIABLE should be a string.  VALUE is optional; if not provided or is
`nil', the environment variable VARIABLE will be removed.  

Interactively, a prefix argument means to unset the variable.
Interactively, the current value (if any) of the variable
appears at the front of the history list when you type in the new value.

This function works by modifying `process-environment'." t nil)

;;;***

;;;### (autoloads (complete-tag select-tags-table tags-apropos list-tags
;;;;;;  tags-query-replace tags-search tags-loop-continue next-file
;;;;;;  pop-tag-mark find-tag-regexp find-tag-other-frame find-tag-other-window
;;;;;;  find-tag find-tag-noselect tags-table-files visit-tags-table
;;;;;;  find-tag-default-function find-tag-hook tags-add-tables tags-table-list)
;;;;;;  "etags" "progmodes/etags.el" (13878 24272))
;;; Generated autoloads from progmodes/etags.el

(defvar tags-file-name nil "\
*File name of tags table.
To switch to a new tags table, setting this variable is sufficient.
If you set this variable, do not also set `tags-table-list'.
Use the `etags' program to make a tags table file.")
 (put 'tags-file-name 'variable-interactive "fVisit tags table: ")

(defvar tags-table-list nil "\
*List of file names of tags tables to search.
An element that is a directory means the file \"TAGS\" in that directory.
To switch to a new list of tags tables, setting this variable is sufficient.
If you set this variable, do not also set `tags-file-name'.
Use the `etags' program to make a tags table file.")

(defvar tags-add-tables (quote ask-user) "\
*Control whether to add a new tags table to the current list.
t means do; nil means don't (always start a new list).
Any other value means ask the user whether to add a new tags table
to the current list (as opposed to starting a new list).")

(defvar find-tag-hook nil "\
*Hook to be run by \\[find-tag] after finding a tag.  See `run-hooks'.
The value in the buffer in which \\[find-tag] is done is used,
not the value in the buffer \\[find-tag] goes to.")

(defvar find-tag-default-function nil "\
*A function of no arguments used by \\[find-tag] to pick a default tag.
If nil, and the symbol that is the value of `major-mode'
has a `find-tag-default-function' property (see `put'), that is used.
Otherwise, `find-tag-default' is used.")

(autoload (quote visit-tags-table) "etags" "\
Tell tags commands to use tags table file FILE.
FILE should be the name of a file created with the `etags' program.
A directory name is ok too; it means file TAGS in that directory.

Normally \\[visit-tags-table] sets the global value of `tags-file-name'.
With a prefix arg, set the buffer-local value instead.
When you find a tag with \\[find-tag], the buffer it finds the tag
in is given a local value of this variable which is the name of the tags
file the tag was in." t nil)

(autoload (quote tags-table-files) "etags" "\
Return a list of files in the current tags table.
Assumes the tags table is the current buffer.  The file names are returned
as they appeared in the `etags' command that created the table, usually
without directory names." nil nil)

(autoload (quote find-tag-noselect) "etags" "\
Find tag (in current tags table) whose name contains TAGNAME.
Returns the buffer containing the tag's definition and moves its point there,
but does not select the buffer.
The default for TAGNAME is the expression in the buffer near point.

If second arg NEXT-P is t (interactively, with prefix arg), search for
another tag that matches the last tagname or regexp used.  When there are
multiple matches for a tag, more exact matches are found first.  If NEXT-P
is the atom `-' (interactively, with prefix arg that is a negative number
or just \\[negative-argument]), pop back to the previous tag gone to.

If third arg REGEXP-P is non-nil, treat TAGNAME as a regexp.

A marker representing the point when this command is onvoked is pushed
onto a ring and may be popped back to with \\[pop-tag-mark].
Contrast this with the ring of marks gone to by the command.

See documentation of variable `tags-file-name'." t nil)

(autoload (quote find-tag) "etags" "\
Find tag (in current tags table) whose name contains TAGNAME.
Select the buffer containing the tag's definition, and move point there.
The default for TAGNAME is the expression in the buffer around or before point.

If second arg NEXT-P is t (interactively, with prefix arg), search for
another tag that matches the last tagname or regexp used.  When there are
multiple matches for a tag, more exact matches are found first.  If NEXT-P
is the atom `-' (interactively, with prefix arg that is a negative number
or just \\[negative-argument]), pop back to the previous tag gone to.

If third arg REGEXP-P is non-nil, treat TAGNAME as a regexp.

A marker representing the point when this command is onvoked is pushed
onto a ring and may be popped back to with \\[pop-tag-mark].
Contrast this with the ring of marks gone to by the command.

See documentation of variable `tags-file-name'." t nil)
 (define-key esc-map "." 'find-tag)

(autoload (quote find-tag-other-window) "etags" "\
Find tag (in current tags table) whose name contains TAGNAME.
Select the buffer containing the tag's definition in another window, and
move point there.  The default for TAGNAME is the expression in the buffer
around or before point.

If second arg NEXT-P is t (interactively, with prefix arg), search for
another tag that matches the last tagname or regexp used.  When there are
multiple matches for a tag, more exact matches are found first.  If NEXT-P
is negative (interactively, with prefix arg that is a negative number or
just \\[negative-argument]), pop back to the previous tag gone to.

If third arg REGEXP-P is non-nil, treat TAGNAME as a regexp.

A marker representing the point when this command is onvoked is pushed
onto a ring and may be popped back to with \\[pop-tag-mark].
Contrast this with the ring of marks gone to by the command.

See documentation of variable `tags-file-name'." t nil)
 (define-key ctl-x-4-map "." 'find-tag-other-window)

(autoload (quote find-tag-other-frame) "etags" "\
Find tag (in current tags table) whose name contains TAGNAME.
Select the buffer containing the tag's definition in another frame, and
move point there.  The default for TAGNAME is the expression in the buffer
around or before point.

If second arg NEXT-P is t (interactively, with prefix arg), search for
another tag that matches the last tagname or regexp used.  When there are
multiple matches for a tag, more exact matches are found first.  If NEXT-P
is negative (interactively, with prefix arg that is a negative number or
just \\[negative-argument]), pop back to the previous tag gone to.

If third arg REGEXP-P is non-nil, treat TAGNAME as a regexp.

A marker representing the point when this command is onvoked is pushed
onto a ring and may be popped back to with \\[pop-tag-mark].
Contrast this with the ring of marks gone to by the command.

See documentation of variable `tags-file-name'." t nil)
 (define-key ctl-x-5-map "." 'find-tag-other-frame)

(autoload (quote find-tag-regexp) "etags" "\
Find tag (in current tags table) whose name matches REGEXP.
Select the buffer containing the tag's definition and move point there.

If second arg NEXT-P is t (interactively, with prefix arg), search for
another tag that matches the last tagname or regexp used.  When there are
multiple matches for a tag, more exact matches are found first.  If NEXT-P
is negative (interactively, with prefix arg that is a negative number or
just \\[negative-argument]), pop back to the previous tag gone to.

If third arg OTHER-WINDOW is non-nil, select the buffer in another window.

A marker representing the point when this command is onvoked is pushed
onto a ring and may be popped back to with \\[pop-tag-mark].
Contrast this with the ring of marks gone to by the command.

See documentation of variable `tags-file-name'." t nil)
 (define-key esc-map [?\C-.] 'find-tag-regexp)
 (define-key esc-map "*" 'pop-tag-mark)

(autoload (quote pop-tag-mark) "etags" "\
Pop back to where \\[find-tag] was last invoked.

This is distinct from invoking \\[find-tag] with a negative argument
since that pops a stack of markers at which tags were found, not from
where they were found." t nil)

(autoload (quote next-file) "etags" "\
Select next file among files in current tags table.

A first argument of t (prefix arg, if interactive) initializes to the
beginning of the list of files in the tags table.  If the argument is
neither nil nor t, it is evalled to initialize the list of files.

Non-nil second argument NOVISIT means use a temporary buffer
 to save time and avoid uninteresting warnings.

Value is nil if the file was already visited;
if the file was newly read in, the value is the filename." t nil)

(autoload (quote tags-loop-continue) "etags" "\
Continue last \\[tags-search] or \\[tags-query-replace] command.
Used noninteractively with non-nil argument to begin such a command (the
argument is passed to `next-file', which see).

Two variables control the processing we do on each file: the value of
`tags-loop-scan' is a form to be executed on each file to see if it is
interesting (it returns non-nil if so) and `tags-loop-operate' is a form to
evaluate to operate on an interesting file.  If the latter evaluates to
nil, we exit; otherwise we scan the next file." t nil)
 (define-key esc-map "," 'tags-loop-continue)

(autoload (quote tags-search) "etags" "\
Search through all files listed in tags table for match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue].

See documentation of variable `tags-file-name'." t nil)

(autoload (quote tags-query-replace) "etags" "\
Query-replace-regexp FROM with TO through all files listed in tags table.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit] or ESC), you can resume the query-replace
with the command \\[tags-loop-continue].

See documentation of variable `tags-file-name'." t nil)

(autoload (quote list-tags) "etags" "\
Display list of tags in file FILE.
This searches only the first table in the list, and no included tables.
FILE should be as it appeared in the `etags' command, usually without a
directory specification." t nil)

(autoload (quote tags-apropos) "etags" "\
Display list of all tags in tags table REGEXP matches." t nil)

(autoload (quote select-tags-table) "etags" "\
Select a tags table file from a menu of those you have already used.
The list of tags tables to select from is stored in `tags-table-set-list';
see the doc of that variable if you want to add names to the list." t nil)

(autoload (quote complete-tag) "etags" "\
Perform tags completion on the text around point.
Completes to the set of names listed in the current tags table.
The string to complete is chosen in the same way as the default
for \\[find-tag] (which see)." t nil)

;;;***

;;;### (autoloads (ethio-write-file ethio-find-file ethio-java-to-fidel-buffer
;;;;;;  ethio-fidel-to-java-buffer ethio-tex-to-fidel-buffer ethio-fidel-to-tex-buffer
;;;;;;  ethio-input-special-character ethio-replace-space ethio-modify-vowel
;;;;;;  ethio-fidel-to-sera-marker ethio-fidel-to-sera-mail ethio-fidel-to-sera-mail-or-marker
;;;;;;  ethio-fidel-to-sera-buffer ethio-fidel-to-sera-region ethio-sera-to-fidel-marker
;;;;;;  ethio-sera-to-fidel-mail ethio-sera-to-fidel-mail-or-marker
;;;;;;  ethio-sera-to-fidel-buffer ethio-sera-to-fidel-region setup-ethiopic-environment-internal
;;;;;;  setup-ethiopic-environment) "ethio-util" "language/ethio-util.el"
;;;;;;  (14180 44051))
;;; Generated autoloads from language/ethio-util.el

(autoload (quote setup-ethiopic-environment) "ethio-util" "\
Setup multilingual environment for Ethiopic." nil nil)

(autoload (quote setup-ethiopic-environment-internal) "ethio-util" nil nil nil)

(autoload (quote ethio-sera-to-fidel-region) "ethio-util" "\
Convert the characters in region from SERA to FIDEL.
The variable `ethio-primary-language' specifies the primary language
and `ethio-secondary-language' specifies the secondary.

If the 3rd parameter SECONDARY is given and non-nil, assume the region
begins begins with the secondary language; otherwise with the primary
language.

If the 4th parameter FORCE is given and non-nil, perform conversion
even if the buffer is read-only.

See also the descriptions of the variables
`ethio-use-colon-for-colon' and
`ethio-use-three-dot-question'." t nil)

(autoload (quote ethio-sera-to-fidel-buffer) "ethio-util" "\
Convert the current buffer from SERA to FIDEL.

The variable `ethio-primary-language' specifies the primary
language and `ethio-secondary-language' specifies the secondary.

If the 1st optional parameter SECONDARY is non-nil, assume the buffer
begins with the secondary language; otherwise with the primary
language.

If the 2nd optional parametr FORCE is non-nil, perform conversion even if the
buffer is read-only.

See also the descriptions of the variables
`ethio-use-colon-for-colon' and
`ethio-use-three-dot-question'." t nil)

(autoload (quote ethio-sera-to-fidel-mail-or-marker) "ethio-util" "\
Execute ethio-sera-to-fidel-mail or ethio-sera-to-fidel-marker depending on the current major mode.
If in rmail-mode or in mail-mode, execute the former; otherwise latter." t nil)

(autoload (quote ethio-sera-to-fidel-mail) "ethio-util" "\
Convert SERA to FIDEL to read/write mail and news.

If the buffer contains the markers \"<sera>\" and \"</sera>\",
convert the segments between them into FIDEL.

If invoked interactively and there is no marker, convert the subject field
and the body into FIDEL using `ethio-sera-to-fidel-region'." t nil)

(autoload (quote ethio-sera-to-fidel-marker) "ethio-util" "\
Convert the regions surrounded by \"<sera>\" and \"</sera>\" from SERA to FIDEL.
Assume that each region begins with `ethio-primary-language'.
The markers \"<sera>\" and \"</sera>\" themselves are not deleted." t nil)

(autoload (quote ethio-fidel-to-sera-region) "ethio-util" "\
Replace all the FIDEL characters in the region to the SERA format.
The variable `ethio-primary-language' specifies the primary
language and `ethio-secondary-language' specifies the secondary.

If the 3dr parameter SECONDARY is given and non-nil, try to convert
the region so that it begins in the secondary language; otherwise with
the primary language.

If the 4th parameter FORCE is given and non-nil, convert even if the
buffer is read-only.

See also the descriptions of the variables
`ethio-use-colon-for-colon', `ethio-use-three-dot-question',
`ethio-quote-vowel-always' and `ethio-numeric-reduction'." t nil)

(autoload (quote ethio-fidel-to-sera-buffer) "ethio-util" "\
Replace all the FIDEL characters in the current buffer to the SERA format.
The variable `ethio-primary-language' specifies the primary
language and `ethio-secondary-language' specifies the secondary.

If the 1st optional parameter SECONDARY is non-nil, try to convert the
region so that it begins in the secondary language; otherwise with the
primary language.

If the 2nd optional parameter FORCE is non-nil, convert even if the
buffer is read-only.

See also the descriptions of the variables
`ethio-use-colon-for-colon', `ethio-use-three-dot-question',
`ethio-quote-vowel-always' and `ethio-numeric-reduction'." t nil)

(autoload (quote ethio-fidel-to-sera-mail-or-marker) "ethio-util" "\
Execute ethio-fidel-to-sera-mail or ethio-fidel-to-sera-marker depending on the current major mode.
If in rmail-mode or in mail-mode, execute the former; otherwise latter." t nil)

(autoload (quote ethio-fidel-to-sera-mail) "ethio-util" "\
Convert FIDEL to SERA to read/write mail and news.

If the body contains at least one Ethiopic character,
 1) insert the string \"<sera>\" at the beginning of the body,
 2) insert \"</sera>\" at the end of the body, and
 3) convert the body into SERA.

The very same procedure applies to the subject field, too." t nil)

(autoload (quote ethio-fidel-to-sera-marker) "ethio-util" "\
Convert the regions surrounded by \"<sera>\" and \"</sera>\" from FIDEL to SERA.
The markers \"<sera>\" and \"</sera>\" themselves are not deleted." t nil)

(autoload (quote ethio-modify-vowel) "ethio-util" "\
Modify the vowel of the FIDEL that is under the cursor." t nil)

(autoload (quote ethio-replace-space) "ethio-util" "\
Replace ASCII spaces with Ethiopic word separators in the region.

In the specified region, replace word separators surrounded by two
Ethiopic characters, depending on the first parameter CH, which should
be 1, 2, or 3.

If CH = 1, word separator will be replaced with an ASCII space.
If CH = 2, with two ASCII spaces.
If CH = 3, with the Ethiopic colon-like word separator.

The second and third parameters BEGIN and END specify the region." t nil)

(autoload (quote ethio-input-special-character) "ethio-util" "\
Allow the user to input special characters." t nil)

(autoload (quote ethio-fidel-to-tex-buffer) "ethio-util" "\
Convert each fidel characters in the current buffer into a fidel-tex command.
Each command is always surrounded by braces." t nil)

(autoload (quote ethio-tex-to-fidel-buffer) "ethio-util" "\
Convert fidel-tex commands in the current buffer into fidel chars." t nil)

(autoload (quote ethio-fidel-to-java-buffer) "ethio-util" "\
Convert Ethiopic characters into the Java escape sequences.

Each escape sequence is of the form uXXXX, where XXXX is the
character's codepoint (in hex) in Unicode.

If `ethio-java-save-lowercase' is non-nil, use [0-9a-f].
Otherwise, [0-9A-F]." nil nil)

(autoload (quote ethio-java-to-fidel-buffer) "ethio-util" "\
Convert the Java escape sequences into corresponding Ethiopic characters." nil nil)

(autoload (quote ethio-find-file) "ethio-util" "\
Transcribe file content into Ethiopic dependig on filename suffix." nil nil)

(autoload (quote ethio-write-file) "ethio-util" "\
Transcribe Ethiopic characters in ASCII depending on the file extension." nil nil)

;;;***

;;;### (autoloads (executable-self-display executable-set-magic)
;;;;;;  "executable" "progmodes/executable.el" (13940 33475))
;;; Generated autoloads from progmodes/executable.el

(autoload (quote executable-set-magic) "executable" "\
Set this buffer's interpreter to INTERPRETER with optional ARGUMENT.
The variables `executable-magicless-file-regexp', `executable-prefix',
`executable-insert', `executable-query' and `executable-chmod' control
when and how magic numbers are inserted or replaced and scripts made
executable." t nil)

(autoload (quote executable-self-display) "executable" "\
Turn a text file into a self-displaying Un*x command.
The magic number of such a command displays all lines but itself." t nil)

;;;***

;;;### (autoloads (expand-jump-to-next-slot expand-jump-to-previous-slot
;;;;;;  expand-add-abbrevs) "expand" "expand.el" (14001 49584))
;;; Generated autoloads from expand.el

(autoload (quote expand-add-abbrevs) "expand" "\
Add a list of abbrev to abbrev table TABLE.
ABBREVS is a list of abbrev definitions; each abbrev description entry
has the form (ABBREV EXPANSION ARG).

ABBREV is the abbreviation to replace.

EXPANSION is the replacement string or a function which will make the
expansion.  For example you, could use the DMacros or skeleton packages
to generate such functions.

ARG is an optional argument which can be a number or a list of
numbers.  If ARG is a number, point is placed ARG chars from the
beginning of the expanded text.

If ARG is a list of numbers, point is placed according to the first
member of the list, but you can visit the other specified positions
cyclicaly with the functions `expand-jump-to-previous-slot' and
`expand-jump-to-next-slot'.

If ARG is omitted, point is placed at the end of the expanded text." nil nil)

(autoload (quote expand-jump-to-previous-slot) "expand" "\
Move the cursor to the previous slot in the last abbrev expansion.
This is used only in conjunction with `expand-add-abbrevs'." t nil)

(autoload (quote expand-jump-to-next-slot) "expand" "\
Move the cursor to the next slot in the last abbrev expansion.
This is used only in conjunction with `expand-add-abbrevs'." t nil)
 (define-key ctl-x-map "ap" 'expand-jump-to-previous-slot)
 (define-key ctl-x-map "an" 'expand-jump-to-next-slot)

;;;***

;;;### (autoloads (f90-mode) "f90" "progmodes/f90.el" (13970 7858))
;;; Generated autoloads from progmodes/f90.el

(autoload (quote f90-mode) "f90" "\
Major mode for editing Fortran 90 code in free format.

\\[f90-indent-new-line] corrects current indentation and creates new indented line.
\\[f90-indent-line] indents the current line correctly. 
\\[f90-indent-subprogram] indents the current subprogram. 

Type `? or `\\[help-command] to display a list of built-in abbrevs for F90 keywords.

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
 f90-indented-comment-re
    Regexp determining the type of comment to be intended like code.
    (default \"!\")
 f90-directive-comment-re
    Regexp of comment-like directive like \"!HPF\\\\$\", not to be indented.
    (default \"!hpf\\\\$\")
 f90-break-delimiters
    Regexp holding list of delimiters at which lines may be broken.
    (default \"[-+*/><=,% \\t]\")
 f90-break-before-delimiters
    Non-nil causes `f90-do-auto-fill' to break lines before delimiters.
    (default t)
 f90-beginning-ampersand 
    Automatic insertion of & at beginning of continuation lines. (default t)
 f90-smart-end 
    From an END statement, check and fill the end using matching block start.
    Allowed values are 'blink, 'no-blink, and nil, which determine
    whether to blink the matching beginning.) (default 'blink)
 f90-auto-keyword-case
    Automatic change of case of keywords. (default nil)
    The possibilities are 'downcase-word, 'upcase-word, 'capitalize-word.
 f90-leave-line-no
    Do not left-justify line numbers. (default nil)
 f90-startup-message
    Set to nil to inhibit message first time F90 mode is used. (default t)
 f90-keywords-re
    List of keywords used for highlighting/upcase-keywords etc.

Turning on F90 mode calls the value of the variable `f90-mode-hook'
with no args, if that value is non-nil." t nil)

;;;***

;;;### (autoloads (list-colors-display facemenu-read-color list-text-properties-at
;;;;;;  facemenu-remove-special facemenu-remove-all facemenu-remove-face-props
;;;;;;  facemenu-set-read-only facemenu-set-intangible facemenu-set-invisible
;;;;;;  facemenu-set-face-from-menu facemenu-set-background facemenu-set-foreground
;;;;;;  facemenu-set-face) "facemenu" "facemenu.el" (13453 47399))
;;; Generated autoloads from facemenu.el
 (define-key global-map "\M-g" 'facemenu-keymap)
 (autoload 'facemenu-keymap "facemenu" "Keymap for face-changing commands." t 'keymap)

(defvar facemenu-face-menu (let ((map (make-sparse-keymap "Face"))) (define-key map "o" (cons "Other..." (quote facemenu-set-face))) map) "\
Menu keymap for faces.")

(defalias (quote facemenu-face-menu) facemenu-face-menu)

(defvar facemenu-foreground-menu (let ((map (make-sparse-keymap "Foreground Color"))) (define-key map "o" (cons "Other..." (quote facemenu-set-foreground))) map) "\
Menu keymap for foreground colors.")

(defalias (quote facemenu-foreground-menu) facemenu-foreground-menu)

(defvar facemenu-background-menu (let ((map (make-sparse-keymap "Background Color"))) (define-key map "o" (cons "Other..." (quote facemenu-set-background))) map) "\
Menu keymap for background colors")

(defalias (quote facemenu-background-menu) facemenu-background-menu)

(defvar facemenu-special-menu (let ((map (make-sparse-keymap "Special"))) (define-key map [115] (cons "Remove Special" (quote facemenu-remove-special))) (define-key map [116] (cons "Intangible" (quote facemenu-set-intangible))) (define-key map [118] (cons "Invisible" (quote facemenu-set-invisible))) (define-key map [114] (cons "Read-Only" (quote facemenu-set-read-only))) map) "\
Menu keymap for non-face text-properties.")

(defalias (quote facemenu-special-menu) facemenu-special-menu)

(defvar facemenu-justification-menu (let ((map (make-sparse-keymap "Justification"))) (define-key map [99] (cons "Center" (quote set-justification-center))) (define-key map [98] (cons "Full" (quote set-justification-full))) (define-key map [114] (cons "Right" (quote set-justification-right))) (define-key map [108] (cons "Left" (quote set-justification-left))) (define-key map [117] (cons "Unfilled" (quote set-justification-none))) map) "\
Submenu for text justification commands.")

(defalias (quote facemenu-justification-menu) facemenu-justification-menu)

(defvar facemenu-indentation-menu (let ((map (make-sparse-keymap "Indentation"))) (define-key map [decrease-right-margin] (cons "Indent Right Less" (quote decrease-right-margin))) (define-key map [increase-right-margin] (cons "Indent Right More" (quote increase-right-margin))) (define-key map [decrease-left-margin] (cons "Indent Less" (quote decrease-left-margin))) (define-key map [increase-left-margin] (cons "Indent More" (quote increase-left-margin))) map) "\
Submenu for indentation commands.")

(defalias (quote facemenu-indentation-menu) facemenu-indentation-menu)

(defvar facemenu-menu nil "\
Facemenu top-level menu keymap.")

(setq facemenu-menu (make-sparse-keymap "Text Properties"))

(let ((map facemenu-menu)) (define-key map [dc] (cons "Display Colors" (quote list-colors-display))) (define-key map [df] (cons "Display Faces" (quote list-faces-display))) (define-key map [dp] (cons "List Properties" (quote list-text-properties-at))) (define-key map [ra] (cons "Remove Text Properties" (quote facemenu-remove-all))) (define-key map [rm] (cons "Remove Face Properties" (quote facemenu-remove-face-props))) (define-key map [s1] (list "-----------------")))

(let ((map facemenu-menu)) (define-key map [in] (cons "Indentation" (quote facemenu-indentation-menu))) (define-key map [ju] (cons "Justification" (quote facemenu-justification-menu))) (define-key map [s2] (list "-----------------")) (define-key map [sp] (cons "Special Properties" (quote facemenu-special-menu))) (define-key map [bg] (cons "Background Color" (quote facemenu-background-menu))) (define-key map [fg] (cons "Foreground Color" (quote facemenu-foreground-menu))) (define-key map [fc] (cons "Face" (quote facemenu-face-menu))))

(defalias (quote facemenu-menu) facemenu-menu)

(autoload (quote facemenu-set-face) "facemenu" "\
Add FACE to the region or next character typed.
It will be added to the top of the face list; any faces lower on the list that
will not show through at all will be removed.

Interactively, the face to be used is read with the minibuffer.

If the region is active and there is no prefix argument,
this command sets the region to the requested face.

Otherwise, this command specifies the face for the next character
inserted.  Moving point or switching buffers before
typing a character to insert cancels the specification." t nil)

(autoload (quote facemenu-set-foreground) "facemenu" "\
Set the foreground color of the region or next character typed.
The color is prompted for.  A face named `fg:color' is used (or created).
If the region is active, it will be set to the requested face.  If
it is inactive (even if mark-even-if-inactive is set) the next
character that is typed (via `self-insert-command') will be set to
the selected face.  Moving point or switching buffers before
typing a character cancels the request." t nil)

(autoload (quote facemenu-set-background) "facemenu" "\
Set the background color of the region or next character typed.
The color is prompted for.  A face named `bg:color' is used (or created).
If the region is active, it will be set to the requested face.  If
it is inactive (even if mark-even-if-inactive is set) the next
character that is typed (via `self-insert-command') will be set to
the selected face.  Moving point or switching buffers before
typing a character cancels the request." t nil)

(autoload (quote facemenu-set-face-from-menu) "facemenu" "\
Set the face of the region or next character typed.
This function is designed to be called from a menu; the face to use
is the menu item's name.

If the region is active and there is no prefix argument,
this command sets the region to the requested face.

Otherwise, this command specifies the face for the next character
inserted.  Moving point or switching buffers before
typing a character to insert cancels the specification." t nil)

(autoload (quote facemenu-set-invisible) "facemenu" "\
Make the region invisible.
This sets the `invisible' text property; it can be undone with
`facemenu-remove-special'." t nil)

(autoload (quote facemenu-set-intangible) "facemenu" "\
Make the region intangible: disallow moving into it.
This sets the `intangible' text property; it can be undone with
`facemenu-remove-special'." t nil)

(autoload (quote facemenu-set-read-only) "facemenu" "\
Make the region unmodifiable.
This sets the `read-only' text property; it can be undone with
`facemenu-remove-special'." t nil)

(autoload (quote facemenu-remove-face-props) "facemenu" "\
Remove `face' and `mouse-face' text properties." t nil)

(autoload (quote facemenu-remove-all) "facemenu" "\
Remove all text properties from the region." t nil)

(autoload (quote facemenu-remove-special) "facemenu" "\
Remove all the \"special\" text properties from the region.
These special properties include `invisible', `intangible' and `read-only'." t nil)

(autoload (quote list-text-properties-at) "facemenu" "\
Pop up a buffer listing text-properties at LOCATION." t nil)

(autoload (quote facemenu-read-color) "facemenu" "\
Read a color using the minibuffer." nil nil)

(autoload (quote list-colors-display) "facemenu" "\
Display names of defined colors, and show what they look like.
If the optional argument LIST is non-nil, it should be a list of
colors to display.  Otherwise, this command computes a list
of colors that the current display can handle." t nil)

;;;***

;;;### (autoloads (turn-on-fast-lock fast-lock-mode) "fast-lock"
;;;;;;  "fast-lock.el" (14139 58050))
;;; Generated autoloads from fast-lock.el

(autoload (quote fast-lock-mode) "fast-lock" "\
Toggle Fast Lock mode.
With arg, turn Fast Lock mode on if and only if arg is positive and the buffer
is associated with a file.  Enable it automatically in your `~/.emacs' by:

 (setq font-lock-support-mode 'fast-lock-mode)

If Fast Lock mode is enabled, and the current buffer does not contain any text
properties, any associated Font Lock cache is used if its timestamp matches the
buffer's file, and its `font-lock-keywords' match those that you are using.

Font Lock caches may be saved:
- When you save the file's buffer.
- When you kill an unmodified file's buffer.
- When you exit Emacs, for all unmodified or saved buffers.
Depending on the value of `fast-lock-save-events'.
See also the commands `fast-lock-read-cache' and `fast-lock-save-cache'.

Use \\[font-lock-fontify-buffer] to fontify the buffer if the cache is bad.

Various methods of control are provided for the Font Lock cache.  In general,
see variable `fast-lock-cache-directories' and function `fast-lock-cache-name'.
For saving, see variables `fast-lock-minimum-size', `fast-lock-save-events',
`fast-lock-save-others' and `fast-lock-save-faces'." t nil)

(autoload (quote turn-on-fast-lock) "fast-lock" "\
Unconditionally turn on Fast Lock mode." nil nil)

(when (fboundp (quote add-minor-mode)) (defvar fast-lock-mode nil) (add-minor-mode (quote fast-lock-mode) nil))

;;;***

;;;### (autoloads (feedmail-queue-reminder feedmail-run-the-queue
;;;;;;  feedmail-run-the-queue-global-prompt feedmail-run-the-queue-no-prompts)
;;;;;;  "feedmail" "mail/feedmail.el" (13855 26165))
;;; Generated autoloads from mail/feedmail.el

(autoload (quote feedmail-run-the-queue-no-prompts) "feedmail" "\
Like feedmail-run-the-queue, but suppress confirmation prompts." t nil)

(autoload (quote feedmail-run-the-queue-global-prompt) "feedmail" "\
Like feedmail-run-the-queue, but with a global confirmation prompt.
This is generally most useful if run non-interactively, since you can
bail out with an appropriate answer to the global confirmation prompt." t nil)

(autoload (quote feedmail-run-the-queue) "feedmail" "\
Visit each message in the feedmail queue directory and send it out.
Return value is a list of three things: number of messages sent, number of
messages skipped, and number of non-message things in the queue (commonly
backup file names and the like)." t nil)

(autoload (quote feedmail-queue-reminder) "feedmail" "\
Perform some kind of reminder activity about queued and draft messages.
Called with an optional symbol argument which says what kind of event
is triggering the reminder activity.  The default is 'on-demand, which
is what you typically would use if you were putting this in your emacs start-up
or mail hook code.  Other recognized values for WHAT-EVENT (these are passed
internally by feedmail):

   after-immediate      (a message has just been sent in immediate mode)
   after-queue          (a message has just been queued)
   after-draft          (a message has just been placed in the draft directory)
   after-run            (the queue has just been run, possibly sending messages)

WHAT-EVENT is used as a key into the table feedmail-queue-reminder-alist.  If
the associated value is a function, it is called without arguments and is expected
to perform the reminder activity.  You can supply your own reminder functions
by redefining feedmail-queue-reminder-alist.  If you don't want any reminders,
you can set feedmail-queue-reminder-alist to nil." t nil)

;;;***

;;;### (autoloads (dired-at-point ffap-at-mouse ffap-menu ffap find-file-at-point
;;;;;;  ffap-next) "ffap" "ffap.el" (14021 36699))
;;; Generated autoloads from ffap.el

(autoload (quote ffap-next) "ffap" "\
Search buffer for next file or URL, and run ffap.
Optional argument BACK says to search backwards.
Optional argument WRAP says to try wrapping around if necessary.
Interactively: use a single prefix to search backwards,
double prefix to wrap forward, triple to wrap backwards.
Actual search is done by `ffap-next-guess'." t nil)

(autoload (quote find-file-at-point) "ffap" "\
Find FILENAME, guessing a default from text around point.
If `ffap-url-regexp' is not nil, the FILENAME may also be an URL.
With a prefix, this command behaves exactly like `ffap-file-finder'.
If `ffap-require-prefix' is set, the prefix meaning is reversed.
See also the variables `ffap-dired-wildcards', `ffap-newfile-prompt',
and the functions `ffap-file-at-point' and `ffap-url-at-point'.

See <ftp://ftp.mathcs.emory.edu/pub/mic/emacs/> for latest version." t nil)

(autoload (quote ffap) "ffap" "\
A short alias for the find-file-at-point command." nil nil)

(autoload (quote ffap-menu) "ffap" "\
Put up a menu of files and urls mentioned in this buffer.
Then set mark, jump to choice, and try to fetch it.  The menu is
cached in `ffap-menu-alist', and rebuilt by `ffap-menu-rescan'.
The optional RESCAN argument (a prefix, interactively) forces
a rebuild.  Searches with `ffap-menu-regexp'." t nil)

(autoload (quote ffap-at-mouse) "ffap" "\
Find file or url guessed from text around mouse click.
Interactively, calls `ffap-at-mouse-fallback' if no guess is found.
Return value:
  * if a guess string is found, return it (after finding it)
  * if the fallback is called, return whatever it returns
  * otherwise, nil" t nil)

(autoload (quote dired-at-point) "ffap" "\
Start Dired, defaulting to file at point.  See `ffap'." t nil)

;;;***

;;;### (autoloads (file-cache-minibuffer-complete) "filecache" "filecache.el"
;;;;;;  (14027 1470))
;;; Generated autoloads from filecache.el

(autoload (quote file-cache-minibuffer-complete) "filecache" "\
Complete a filename in the minibuffer using a preloaded cache.
Filecache does two kinds of substitution: it completes on names in
the cache, and, once it has found a unique name, it cycles through
the directories that the name is available in.  With a prefix argument, 
the name is considered already unique; only the second substitution 
\(directories) is done." t nil)
 (define-key minibuffer-local-completion-map [C-tab] 'file-cache-minibuffer-complete)
 (define-key minibuffer-local-map [C-tab] 'file-cache-minibuffer-complete)
 (define-key minibuffer-local-must-match-map [C-tab] 'file-cache-minibuffer-complete)

;;;***

;;;### (autoloads (find-grep-dired find-name-dired find-dired find-grep-options
;;;;;;  find-ls-option) "find-dired" "find-dired.el" (13717 27320))
;;; Generated autoloads from find-dired.el

(defvar find-ls-option (if (eq system-type (quote berkeley-unix)) (quote ("-ls" . "-gilsb")) (quote ("-exec ls -ld {} \\;" . "-ld"))) "\
*Description of the option to `find' to produce an `ls -l'-type listing.
This is a cons of two strings (FIND-OPTION . LS-SWITCHES).  FIND-OPTION
gives the option (or options) to `find' that produce the desired output.
LS-SWITCHES is a list of `ls' switches to tell dired how to parse the output.")

(defvar find-grep-options (if (or (eq system-type (quote berkeley-unix)) (string-match "solaris2" system-configuration) (string-match "irix" system-configuration)) "-s" "-q") "\
*Option to grep to be as silent as possible.
On Berkeley systems, this is `-s'; on Posix, and with GNU grep, `-q' does it.
On other systems, the closest you can come is to use `-l'.")

(autoload (quote find-dired) "find-dired" "\
Run `find' and go into Dired mode on a buffer of the output.
The command run (after changing into DIR) is

    find . \\( ARGS \\) -ls

except that the variable `find-ls-option' specifies what to use
as the final argument." t nil)

(autoload (quote find-name-dired) "find-dired" "\
Search DIR recursively for files matching the globbing pattern PATTERN,
and run dired on those files.
PATTERN is a shell wildcard (not an Emacs regexp) and need not be quoted.
The command run (after changing into DIR) is

    find . -name 'PATTERN' -ls" t nil)

(autoload (quote find-grep-dired) "find-dired" "\
Find files in DIR containing a regexp ARG and start Dired on output.
The command run (after changing into DIR) is

    find . -exec grep -s ARG {} \\; -ls

Thus ARG can also contain additional grep options." t nil)

;;;***

;;;### (autoloads (ff-mouse-find-other-file-other-window ff-mouse-find-other-file
;;;;;;  ff-find-other-file ff-get-other-file) "find-file" "find-file.el"
;;;;;;  (13937 22881))
;;; Generated autoloads from find-file.el

(autoload (quote ff-get-other-file) "find-file" "\
Find the header or source file corresponding to this file.
See also the documentation for `ff-find-other-file;.

If optional IN-OTHER-WINDOW is non-nil, find the file in another window." t nil)

(autoload (quote ff-find-other-file) "find-file" "\
Find the header or source file corresponding to this file.
Being on a `#include' line pulls in that file.

If optional IN-OTHER-WINDOW is non-nil, find the file in the other window.
If optional IGNORE-INCLUDE is non-nil, ignore being on `#include' lines.

Variables of interest include:

 - ff-case-fold-search
   Non-nil means ignore cases in matches (see case-fold-search).
   If you have extensions in different cases, you will want this to be nil.

 - ff-always-in-other-window 
   If non-nil, always open the other file in another window, unless an
   argument is given to ff-find-other-file.

 - ff-ignore-include 
   If non-nil, ignores #include lines.

 - ff-always-try-to-create 
   If non-nil, always attempt to create the other file if it was not found.

 - ff-quiet-mode 
   If non-nil, traces which directories are being searched.

 - ff-special-constructs 
   A list of regular expressions specifying how to recognise special 
   constructs such as include files etc, and an associated method for 
   extracting the filename from that construct.

 - ff-other-file-alist
   Alist of extensions to find given the current file's extension.

 - ff-search-directories 
   List of directories searched through with each extension specified in
   ff-other-file-alist that matches this file's extension.

 - ff-pre-find-hooks 
   List of functions to be called before the search for the file starts.

 - ff-pre-load-hooks 
   List of functions to be called before the other file is loaded.

 - ff-post-load-hooks
   List of functions to be called after the other file is loaded.

 - ff-not-found-hooks
   List of functions to be called if the other file could not be found.

 - ff-file-created-hooks
   List of functions to be called if the other file has been created." t nil)

(autoload (quote ff-mouse-find-other-file) "find-file" "\
Visit the file you click on." t nil)

(autoload (quote ff-mouse-find-other-file-other-window) "find-file" "\
Visit the file you click on." t nil)

;;;***

;;;### (autoloads (find-function-setup-keys find-variable-at-point
;;;;;;  find-function-at-point find-function-on-key find-variable-other-frame
;;;;;;  find-variable-other-window find-variable find-variable-noselect
;;;;;;  find-function-other-frame find-function-other-window find-function
;;;;;;  find-function-noselect) "find-func" "emacs-lisp/find-func.el"
;;;;;;  (13980 37653))
;;; Generated autoloads from emacs-lisp/find-func.el

(autoload (quote find-function-noselect) "find-func" "\
Return a pair (BUFFER . POINT) pointing to the definition of FUNCTION.

Finds the Emacs Lisp library containing the definition of FUNCTION
in a buffer and the point of the definition.  The buffer is
not selected.

If the file where FUNCTION is defined is not known, then it is
searched for in `find-function-source-path' if non nil, otherwise
in `load-path'." nil nil)

(autoload (quote find-function) "find-func" "\
Find the definition of the FUNCTION near point.

Finds the Emacs Lisp library containing the definition of the function
near point (selected by `function-at-point') in a buffer and
places point before the definition.  Point is saved in the buffer if
it is one of the current buffers.

The library where FUNCTION is defined is searched for in
`find-function-source-path', if non nil, otherwise in `load-path'.
See also `find-function-recenter-line' and `find-function-after-hook'." t nil)

(autoload (quote find-function-other-window) "find-func" "\
Find, in another window, the definition of FUNCTION near point.

See `find-function' for more details." t nil)

(autoload (quote find-function-other-frame) "find-func" "\
Find, in ananother frame, the definition of FUNCTION near point.

See `find-function' for more details." t nil)

(autoload (quote find-variable-noselect) "find-func" "\
Return a pair `(buffer . point)' pointing to the definition of SYMBOL.

Finds the Emacs Lisp library containing the definition of SYMBOL
in a buffer and the point of the definition.  The buffer is
not selected.

The library where VARIABLE is defined is searched for in
`find-function-source-path', if non nil, otherwise in `load-path'." nil nil)

(autoload (quote find-variable) "find-func" "\
Find the definition of the VARIABLE near point.

Finds the Emacs Lisp library containing the definition of the variable
near point (selected by `variable-at-point') in a buffer and
places point before the definition.  Point is saved in the buffer if
it is one of the current buffers.

The library where VARIABLE is defined is searched for in
`find-function-source-path', if non nil, otherwise in `load-path'.
See also `find-function-recenter-line' and `find-function-after-hook'." t nil)

(autoload (quote find-variable-other-window) "find-func" "\
Find, in another window, the definition of VARIABLE near point.

See `find-variable' for more details." t nil)

(autoload (quote find-variable-other-frame) "find-func" "\
Find, in annother frame, the definition of VARIABLE near point.

See `find-variable' for more details." t nil)

(autoload (quote find-function-on-key) "find-func" "\
Find the function that KEY invokes.  KEY is a string.
Point is saved if FUNCTION is in the current buffer." t nil)

(autoload (quote find-function-at-point) "find-func" "\
Find directly the function at point in the other window." t nil)

(autoload (quote find-variable-at-point) "find-func" "\
Find directly the function at point in the other window." t nil)

(autoload (quote find-function-setup-keys) "find-func" "\
Define some key bindings for the find-function family of functions." nil nil)

;;;***

;;;### (autoloads (enable-flow-control-on enable-flow-control) "flow-ctrl"
;;;;;;  "flow-ctrl.el" (12550 53108))
;;; Generated autoloads from flow-ctrl.el

(autoload (quote enable-flow-control) "flow-ctrl" "\
Toggle flow control handling.
When handling is enabled, user can type C-s as C-\\, and C-q as C-^.
With arg, enable flow control mode if arg is positive, otherwise disable." t nil)

(autoload (quote enable-flow-control-on) "flow-ctrl" "\
Enable flow control if using one of a specified set of terminal types.
Use `(enable-flow-control-on \"vt100\" \"h19\")' to enable flow control
on VT-100 and H19 terminals.  When flow control is enabled,
you must type C-\\ to get the effect of a C-s, and type C-^
to get the effect of a C-q." nil nil)

;;;***

;;;### (autoloads (flyspell-mode-off flyspell-mode) "flyspell" "textmodes/flyspell.el"
;;;;;;  (14218 3459))
;;; Generated autoloads from textmodes/flyspell.el

(autoload (quote flyspell-mode) "flyspell" "\
Minor mode performing on-the-fly spelling checking.
Ispell is automatically spawned on background for each entered words.
The default flyspell behavior is to highlight incorrect words.
With no argument, this command toggles Flyspell mode.
With a prefix argument ARG, turn Flyspell minor mode on iff ARG is positive.
  
Bindings:
\\[ispell-word]: correct words (using Ispell).
\\[flyspell-auto-correct-word]: automatically correct word.
\\[flyspell-correct-word] (or mouse-2): popup correct words.

Hooks:
flyspell-mode-hook is run after flyspell is entered.

Remark:
`flyspell-mode' uses `ispell-mode'.  Thus all Ispell options are
valid.  For instance, a personal dictionary can be used by
invoking `ispell-change-dictionary'.

Consider using the `ispell-parser' to check your text.  For instance
consider adding:
\(add-hook 'tex-mode-hook (function (lambda () (setq ispell-parser 'tex))))
in your .emacs file.

flyspell-region checks all words inside a region.

flyspell-buffer checks the whole buffer." t nil)

(autoload (quote flyspell-mode-off) "flyspell" "\
Turn Flyspell mode off." nil nil)

;;;***

;;;### (autoloads (follow-delete-other-windows-and-split follow-mode
;;;;;;  turn-off-follow-mode turn-on-follow-mode) "follow" "follow.el"
;;;;;;  (13674 33097))
;;; Generated autoloads from follow.el

(autoload (quote turn-on-follow-mode) "follow" "\
Turn on Follow mode. Please see the function `follow-mode'." t nil)

(autoload (quote turn-off-follow-mode) "follow" "\
Turn off Follow mode. Please see the function `follow-mode'." t nil)

(autoload (quote follow-mode) "follow" "\
Minor mode that combines windows into one tall virtual window.

The feeling of a \"virtual window\" has been accomplished by the use
of two major techniques:

* The windows always displays adjacent sections of the buffer.
  This means that whenever one window is moved, all the
  others will follow.  (Hence the name Follow Mode.)

* Should the point (cursor) end up outside a window, another
  window displaying that point is selected, if possible.  This
  makes it possible to walk between windows using normal cursor
  movement commands.

Follow mode comes to its prime when used on a large screen and two
side-by-side window are used. The user can, with the help of Follow
mode, use two full-height windows as though they would have been
one. Imagine yourself editing a large function, or section of text,
and being able to use 144 lines instead of the normal 72... (your
mileage may vary).

To split one large window into two side-by-side windows, the commands
`\\[split-window-horizontally]' or `M-x follow-delete-other-windows-and-split' can be used.

Only windows displayed in the same frame follow each-other.

If the variable `follow-intercept-processes' is non-nil, Follow mode
will listen to the output of processes and redisplay accordingly.
\(This is the default.)

When Follow mode is switched on, the hook `follow-mode-hook'
is called.  When turned off, `follow-mode-off-hook' is called.

Keys specific to Follow mode:
\\{follow-mode-map}" t nil)

(autoload (quote follow-delete-other-windows-and-split) "follow" "\
Create two side by side windows and enter Follow Mode.

Execute this command to display as much as possible of the text
in the selected window.  All other windows, in the current
frame, are deleted and the selected window is split in two
side-by-side windows. Follow Mode is activated, hence the
two windows always will display two successive pages.
\(If one window is moved, the other one will follow.)

If ARG is positive, the leftmost window is selected.  If it negative,
the rightmost is selected.  If ARG is nil, the leftmost window is
selected if the original window is the first one in the frame.

To bind this command to a hotkey, place the following line
in your `~/.emacs' file, replacing [f7] by your favourite key:
    (global-set-key [f7] 'follow-delete-other-windows-and-split)" t nil)

;;;***

;;;### (autoloads (font-lock-fontify-buffer global-font-lock-mode
;;;;;;  font-lock-add-keywords turn-on-font-lock font-lock-mode)
;;;;;;  "font-lock" "font-lock.el" (14230 12346))
;;; Generated autoloads from font-lock.el

(defvar font-lock-mode-hook nil "\
Function or functions to run on entry to Font Lock mode.")

(autoload (quote font-lock-mode) "font-lock" "\
Toggle Font Lock mode.
With arg, turn Font Lock mode on if and only if arg is positive.

When Font Lock mode is enabled, text is fontified as you type it:

 - Comments are displayed in `font-lock-comment-face';
 - Strings are displayed in `font-lock-string-face';
 - Certain other expressions are displayed in other faces according to the
   value of the variable `font-lock-keywords'.

You can enable Font Lock mode in any major mode automatically by turning on in
the major mode's hook.  For example, put in your ~/.emacs:

 (add-hook 'c-mode-hook 'turn-on-font-lock)

Alternatively, you can use Global Font Lock mode to automagically turn on Font
Lock mode in buffers whose major mode supports it and whose major mode is one
of `font-lock-global-modes'.  For example, put in your ~/.emacs:

 (global-font-lock-mode t)

There are a number of support modes that may be used to speed up Font Lock mode
in various ways, specified via the variable `font-lock-support-mode'.  Where
major modes support different levels of fontification, you can use the variable
`font-lock-maximum-decoration' to specify which level you generally prefer.
When you turn Font Lock mode on/off the buffer is fontified/defontified, though
fontification occurs only if the buffer is less than `font-lock-maximum-size'.

For example, to specify that Font Lock mode use use Lazy Lock mode as a support
mode and use maximum levels of fontification, put in your ~/.emacs:

 (setq font-lock-support-mode 'lazy-lock-mode)
 (setq font-lock-maximum-decoration t)

To add your own highlighting for some major mode, and modify the highlighting
selected automatically via the variable `font-lock-maximum-decoration', you can
use `font-lock-add-keywords'.

To fontify a buffer, without turning on Font Lock mode and regardless of buffer
size, you can use \\[font-lock-fontify-buffer].

To fontify a block (the function or paragraph containing point, or a number of
lines around point), perhaps because modification on the current line caused
syntactic change on other lines, you can use \\[font-lock-fontify-block].

See the variable `font-lock-defaults-alist' for the Font Lock mode default
settings.  You can set your own default settings for some mode, by setting a
buffer local value for `font-lock-defaults', via its mode hook." t nil)

(autoload (quote turn-on-font-lock) "font-lock" "\
Turn on Font Lock mode conditionally.
Turn on only if the terminal can display it." nil nil)

(autoload (quote font-lock-add-keywords) "font-lock" "\
Add highlighting KEYWORDS for MAJOR-MODE.
MAJOR-MODE should be a symbol, the major mode command name, such as `c-mode'
or nil.  If nil, highlighting keywords are added for the current buffer.
KEYWORDS should be a list; see the variable `font-lock-keywords'.
By default they are added at the beginning of the current highlighting list.
If optional argument APPEND is `set', they are used to replace the current
highlighting list.  If APPEND is any other non-nil value, they are added at the
end of the current highlighting list.

For example:

 (font-lock-add-keywords 'c-mode
  '((\"\\\\\\=<\\\\(FIXME\\\\):\" 1 font-lock-warning-face prepend)
    (\"\\\\\\=<\\\\(and\\\\|or\\\\|not\\\\)\\\\\\=>\" . font-lock-keyword-face)))

adds two fontification patterns for C mode, to fontify `FIXME:' words, even in
comments, and to fontify `and', `or' and `not' words as keywords.

Note that some modes have specialised support for additional patterns, e.g.,
see the variables `c-font-lock-extra-types', `c++-font-lock-extra-types',
`objc-font-lock-extra-types' and `java-font-lock-extra-types'." nil nil)

(autoload (quote global-font-lock-mode) "font-lock" "\
Toggle Global Font Lock mode.
With prefix ARG, turn Global Font Lock mode on if and only if ARG is positive.
Displays a message saying whether the mode is on or off if MESSAGE is non-nil.
Returns the new status of Global Font Lock mode (non-nil means on).

When Global Font Lock mode is enabled, Font Lock mode is automagically
turned on in a buffer if its major mode is one of `font-lock-global-modes'." t nil)

(autoload (quote font-lock-fontify-buffer) "font-lock" "\
Fontify the current buffer the way `font-lock-mode' would." t nil)

;;;***

;;;### (autoloads (create-fontset-from-fontset-spec) "fontset" "international/fontset.el"
;;;;;;  (14094 46514))
;;; Generated autoloads from international/fontset.el

(autoload (quote create-fontset-from-fontset-spec) "fontset" "\
Create a fontset from fontset specification string FONTSET-SPEC.
FONTSET-SPEC is a string of the format:
	FONTSET-NAME,CHARSET-NAME0:FONT-NAME0,CHARSET-NAME1:FONT-NAME1, ...
Any number of SPACE, TAB, and NEWLINE can be put before and after commas.

Optional 2nd argument STYLE-VARIANT is a list of font styles
\(e.g. bold, italic) or the symbol t to specify all available styles.
If this argument is specified, fontsets which differs from
FONTSET-NAME in styles are also created.  An element of STYLE-VARIANT
may be cons of style and a font name.  In this case, the style variant
fontset uses the font for ASCII character set.

If this function attempts to create already existing fontset, error is
signaled unless the optional 3rd argument NOERROR is non-nil.

It returns a name of the created fontset." nil nil)

;;;***

;;;### (autoloads (forms-find-file-other-window forms-find-file forms-mode)
;;;;;;  "forms" "forms.el" (14162 18837))
;;; Generated autoloads from forms.el

(autoload (quote forms-mode) "forms" "\
Major mode to visit files in a field-structured manner using a form.

Commands:                        Equivalent keys in read-only mode:
 TAB            forms-next-field          TAB
 C-c TAB        forms-next-field          
 C-c <          forms-first-record         <
 C-c >          forms-last-record          >
 C-c ?          describe-mode              ?
 C-c C-k        forms-delete-record
 C-c C-q        forms-toggle-read-only     q
 C-c C-o        forms-insert-record
 C-c C-l        forms-jump-record          l
 C-c C-n        forms-next-record          n
 C-c C-p        forms-prev-record          p
 C-c C-r        forms-search-reverse       r
 C-c C-s        forms-search-forward       s
 C-c C-x        forms-exit                 x
" t nil)

(autoload (quote forms-find-file) "forms" "\
Visit a file in Forms mode." t nil)

(autoload (quote forms-find-file-other-window) "forms" "\
Visit a file in Forms mode in other window." t nil)

;;;***

;;;### (autoloads (fortran-mode fortran-tab-mode-default) "fortran"
;;;;;;  "progmodes/fortran.el" (14164 461))
;;; Generated autoloads from progmodes/fortran.el

(defvar fortran-tab-mode-default nil "\
*Default tabbing/carriage control style for empty files in Fortran mode.
A value of t specifies tab-digit style of continuation control.
A value of nil specifies that continuation lines are marked
with a character in column 6.")

(autoload (quote fortran-mode) "fortran" "\
Major mode for editing Fortran code.
\\[fortran-indent-line] indents the current Fortran line correctly.
DO statements must not share a common CONTINUE.

Type ;? or ;\\[help-command] to display a list of built-in abbrevs for
Fortran keywords.

Key definitions:
\\{fortran-mode-map}

Variables controlling indentation style and extra features:

 `comment-start'
    Normally nil in Fortran mode.  If you want to use comments
    starting with `!', set this to the string \"!\".
 `fortran-do-indent'
    Extra indentation within do blocks.  (default 3)
 `fortran-if-indent'
    Extra indentation within if blocks.  (default 3)
 `fortran-structure-indent'
    Extra indentation within structure, union, map and interface blocks.
    (default 3)
 `fortran-continuation-indent'
    Extra indentation applied to continuation statements.  (default 5)
 `fortran-comment-line-extra-indent'
    Amount of extra indentation for text within full-line comments.  (default 0)
 `fortran-comment-indent-style'
    nil    means don't change indentation of text in full-line comments,
    fixed  means indent that text at `fortran-comment-line-extra-indent' beyond
           the value of `fortran-minimum-statement-indent-fixed' (for fixed
           format continuation style) or `fortran-minimum-statement-indent-tab'
           (for TAB format continuation style).
    relative  means indent at `fortran-comment-line-extra-indent' beyond the
 	      indentation for a line of code.
    (default 'fixed)
 `fortran-comment-indent-char'
    Single-character string to be inserted instead of space for
    full-line comment indentation.  (default \" \")
 `fortran-minimum-statement-indent-fixed'
    Minimum indentation for Fortran statements in fixed format mode.  (def.6)
 `fortran-minimum-statement-indent-tab'
    Minimum indentation for Fortran statements in TAB format mode.  (default 9)
 `fortran-line-number-indent'
    Maximum indentation for line numbers.  A line number will get
    less than this much indentation if necessary to avoid reaching
    column 5.  (default 1)
 `fortran-check-all-num-for-matching-do'
    Non-nil causes all numbered lines to be treated as possible \"continue\"
    statements.  (default nil)
 `fortran-blink-matching-if'
    Non-nil causes \\[fortran-indent-line] on an ENDIF statement to blink on
    matching IF.  Also, from an ENDDO statement, blink on matching DO [WHILE]
    statement.  (default nil)
 `fortran-continuation-string'
    Single-character string to be inserted in column 5 of a continuation
    line.  (default \"$\")
 `fortran-comment-region'
    String inserted by \\[fortran-comment-region] at start of each line in
    region.  (default \"c$$$\")
 `fortran-electric-line-number'
    Non-nil causes line number digits to be moved to the correct column
    as typed.  (default t)
 `fortran-break-before-delimiters'
    Non-nil causes `fortran-fill' to break lines before delimiters.
    (default t)

Turning on Fortran mode calls the value of the variable `fortran-mode-hook'
with no args, if that value is non-nil." t nil)

;;;***

;;;### (autoloads (generic-mode define-generic-mode) "generic" "generic.el"
;;;;;;  (13973 2889))
;;; Generated autoloads from generic.el

(autoload (quote define-generic-mode) "generic" "\
Create a new generic mode with NAME.

Args: (NAME COMMENT-LIST KEYWORD-LIST FONT-LOCK-LIST AUTO-MODE-LIST
            FUNCTION-LIST &optional DESCRIPTION)

NAME should be a symbol; its string representation is used as the function
name. If DESCRIPTION is provided, it is used as the docstring for the new
function.

COMMENT-LIST is a list, whose entries are either a single character,
a one or two character string or a cons pair. If the entry is a character
or a one-character string, it is added to the mode's syntax table with
comment-start syntax. If the entry is a cons pair, the elements of the
pair are considered to be comment-start and comment-end respectively.
Note that Emacs has limitations regarding comment characters.

KEYWORD-LIST is a list of keywords to highlight with `font-lock-keyword-face'.
Each keyword should be a string.

FONT-LOCK-LIST is a list of additional expressions to highlight. Each entry
in the list should have the same form as an entry in `font-lock-defaults-alist'

AUTO-MODE-LIST is a list of regular expressions to add to auto-mode-alist.
These regexps are added to auto-mode-alist as soon as `define-generic-mode'
is called; any old regexps with the same name are removed.

FUNCTION-LIST is a list of functions to call to do some additional setup.

See the file generic-x.el for some examples of `define-generic-mode'." nil nil)

(autoload (quote generic-mode) "generic" "\
Basic comment and font-lock functionality for `generic' files.
\(Files which are too small to warrant their own mode, but have
comment characters, keywords, and the like.)

To define a generic-mode, use the function `define-generic-mode'.
Some generic modes are defined in `generic-x.el'." t nil)

;;;***

;;;### (autoloads (gnus gnus-other-frame gnus-slave gnus-no-server
;;;;;;  gnus-slave-no-server) "gnus" "gnus/gnus.el" (14030 49469))
;;; Generated autoloads from gnus/gnus.el

(autoload (quote gnus-slave-no-server) "gnus" "\
Read network news as a slave, without connecting to local server." t nil)

(autoload (quote gnus-no-server) "gnus" "\
Read network news.
If ARG is a positive number, Gnus will use that as the
startup level.	If ARG is nil, Gnus will be started at level 2.
If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use.
As opposed to `gnus', this command will not connect to the local server." t nil)

(autoload (quote gnus-slave) "gnus" "\
Read news as a slave." t nil)

(autoload (quote gnus-other-frame) "gnus" "\
Pop up a frame to read news." t nil)

(autoload (quote gnus) "gnus" "\
Read network news.
If ARG is non-nil and a positive number, Gnus will use that as the
startup level.	If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use." t nil)

;;;***

;;;### (autoloads (gnus-agent-batch gnus-agent-batch-fetch gnus-agentize
;;;;;;  gnus-plugged gnus-unplugged) "gnus-agent" "gnus/gnus-agent.el"
;;;;;;  (14029 49119))
;;; Generated autoloads from gnus/gnus-agent.el

(autoload (quote gnus-unplugged) "gnus-agent" "\
Start Gnus unplugged." t nil)

(autoload (quote gnus-plugged) "gnus-agent" "\
Start Gnus plugged." t nil)

(autoload (quote gnus-agentize) "gnus-agent" "\
Allow Gnus to be an offline newsreader.
The normal usage of this command is to put the following as the
last form in your `.gnus.el' file:

\(gnus-agentize)

This will modify the `gnus-before-startup-hook', `gnus-post-method',
and `message-send-mail-function' variables, and install the Gnus
agent minor mode in all Gnus buffers." t nil)

(autoload (quote gnus-agent-batch-fetch) "gnus-agent" "\
Start Gnus and fetch session." t nil)

(autoload (quote gnus-agent-batch) "gnus-agent" nil t nil)

;;;***

;;;### (autoloads (gnus-audio-play) "gnus-audio" "gnus/gnus-audio.el"
;;;;;;  (14030 49345))
;;; Generated autoloads from gnus/gnus-audio.el

(autoload (quote gnus-audio-play) "gnus-audio" "\
Play a sound through the speaker." t nil)

;;;***

;;;### (autoloads (gnus-cache-generate-nov-databases gnus-cache-generate-active
;;;;;;  gnus-jog-cache) "gnus-cache" "gnus/gnus-cache.el" (14030
;;;;;;  49350))
;;; Generated autoloads from gnus/gnus-cache.el

(autoload (quote gnus-jog-cache) "gnus-cache" "\
Go through all groups and put the articles into the cache.

Usage:
$ emacs -batch -l ~/.emacs -l gnus -f gnus-jog-cache" t nil)

(autoload (quote gnus-cache-generate-active) "gnus-cache" "\
Generate the cache active file." t nil)

(autoload (quote gnus-cache-generate-nov-databases) "gnus-cache" "\
Generate NOV files recursively starting in DIR." t nil)

;;;***

;;;### (autoloads (gnus-fetch-group-other-frame gnus-fetch-group)
;;;;;;  "gnus-group" "gnus/gnus-group.el" (14177 56450))
;;; Generated autoloads from gnus/gnus-group.el

(autoload (quote gnus-fetch-group) "gnus-group" "\
Start Gnus if necessary and enter GROUP.
Returns whether the fetching was successful or not." t nil)

(autoload (quote gnus-fetch-group-other-frame) "gnus-group" "\
Pop up a frame and enter GROUP." t nil)

;;;***

;;;### (autoloads (gnus-batch-score) "gnus-kill" "gnus/gnus-kill.el"
;;;;;;  (14030 49385))
;;; Generated autoloads from gnus/gnus-kill.el

(defalias (quote gnus-batch-kill) (quote gnus-batch-score))

(autoload (quote gnus-batch-score) "gnus-kill" "\
Run batched scoring.
Usage: emacs -batch -l ~/.emacs -l gnus -f gnus-batch-score" t nil)

;;;***

;;;### (autoloads (gnus-change-server) "gnus-move" "gnus/gnus-move.el"
;;;;;;  (14030 49391))
;;; Generated autoloads from gnus/gnus-move.el

(autoload (quote gnus-change-server) "gnus-move" "\
Move from FROM-SERVER to TO-SERVER.
Update the .newsrc.eld file to reflect the change of nntp server." t nil)

;;;***

;;;### (autoloads (gnus-mule-initialize gnus-mule-add-group) "gnus-mule"
;;;;;;  "gnus/gnus-mule.el" (14092 5285))
;;; Generated autoloads from gnus/gnus-mule.el

(autoload (quote gnus-mule-add-group) "gnus-mule" "\
Specify that articles of news group NAME are encoded in CODING-SYSTEM.
All news groups deeper than NAME are also the target.
If CODING-SYSTEM is a cons, the car and cdr part are regarded as
coding-system for reading and writing respectively." nil nil)

(autoload (quote gnus-mule-initialize) "gnus-mule" "\
Do several settings for GNUS to enable automatic code conversion." nil nil)

;;;***

;;;### (autoloads (gnus-batch-brew-soup) "gnus-soup" "gnus/gnus-soup.el"
;;;;;;  (14030 49414))
;;; Generated autoloads from gnus/gnus-soup.el

(autoload (quote gnus-batch-brew-soup) "gnus-soup" "\
Brew a SOUP packet from groups mention on the command line.
Will use the remaining command line arguments as regular expressions
for matching on group names.

For instance, if you want to brew on all the nnml groups, as well as
groups with \"emacs\" in the name, you could say something like:

$ emacs -batch -f gnus-batch-brew-soup ^nnml \".*emacs.*\"

Note -- this function hasn't been implemented yet." t nil)

;;;***

;;;### (autoloads (gnus-update-format) "gnus-spec" "gnus/gnus-spec.el"
;;;;;;  (14030 49416))
;;; Generated autoloads from gnus/gnus-spec.el

(autoload (quote gnus-update-format) "gnus-spec" "\
Update the format specification near point." t nil)

;;;***

;;;### (autoloads (gnus-declare-backend gnus-unload) "gnus-start"
;;;;;;  "gnus/gnus-start.el" (14030 49423))
;;; Generated autoloads from gnus/gnus-start.el

(autoload (quote gnus-unload) "gnus-start" "\
Unload all Gnus features." t nil)

(autoload (quote gnus-declare-backend) "gnus-start" "\
Declare backend NAME with ABILITIES as a Gnus backend." nil nil)

;;;***

;;;### (autoloads (gnus-add-configuration) "gnus-win" "gnus/gnus-win.el"
;;;;;;  (14030 49464))
;;; Generated autoloads from gnus/gnus-win.el

(autoload (quote gnus-add-configuration) "gnus-win" "\
Add the window configuration CONF to `gnus-buffer-configuration'." nil nil)

;;;***

;;;### (autoloads (gomoku) "gomoku" "play/gomoku.el" (13940 33305))
;;; Generated autoloads from play/gomoku.el

(autoload (quote gomoku) "gomoku" "\
Start a Gomoku game between you and Emacs.
If a game is in progress, this command allow you to resume it.
If optional arguments N and M are given, an N by M board is used.
If prefix arg is given for N, M is prompted for.

You and Emacs play in turn by marking a free square.  You mark it with X
and Emacs marks it with O. The winner is the first to get five contiguous
marks horizontally, vertically or in diagonal.

You play by moving the cursor over the square you choose and hitting
\\<gomoku-mode-map>\\[gomoku-human-plays].
Use \\[describe-mode] for more info." t nil)

;;;***

;;;### (autoloads (goto-address goto-address-at-point goto-address-at-mouse)
;;;;;;  "goto-addr" "goto-addr.el" (13884 38368))
;;; Generated autoloads from goto-addr.el

(autoload (quote goto-address-at-mouse) "goto-addr" "\
Send to the e-mail address or load the URL clicked with the mouse.
Send mail to address at position of mouse click.  See documentation for
`goto-address-find-address-at-point'.  If no address is found
there, then load the URL at or before the position of the mouse click." t nil)

(autoload (quote goto-address-at-point) "goto-addr" "\
Send to the e-mail address or load the URL at point.
Send mail to address at point.  See documentation for
`goto-address-find-address-at-point'.  If no address is found
there, then load the URL at or before point." t nil)

(autoload (quote goto-address) "goto-addr" "\
Sets up goto-address functionality in the current buffer.
Allows user to use mouse/keyboard command to click to go to a URL
or to send e-mail.
By default, goto-address binds to mouse-2 and C-c RET.

Also fontifies the buffer appropriately (see `goto-address-fontify-p' and
`goto-address-highlight-p' for more information)." t nil)

;;;***

;;;### (autoloads (gs-load-image) "gs" "gs.el" (14228 23893))
;;; Generated autoloads from gs.el

(autoload (quote gs-load-image) "gs" "\
Load a PS image for display on FRAME.
SPEC is an image specification, IMG-HEIGHT and IMG-WIDTH are width
and height of the image in pixels.  WINDOW-AND-PIXMAP-ID is a string of
the form \"WINDOW-ID PIXMAP-ID\".  Value is non-nil if successful." nil nil)

;;;***

;;;### (autoloads (jdb pdb perldb xdb dbx sdb gdb) "gud" "gud.el"
;;;;;;  (14130 46613))
;;; Generated autoloads from gud.el

(autoload (quote gdb) "gud" "\
Run gdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)

(autoload (quote sdb) "gud" "\
Run sdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)

(autoload (quote dbx) "gud" "\
Run dbx on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)

(autoload (quote xdb) "gud" "\
Run xdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger.

You can set the variable 'gud-xdb-directories' to a list of program source
directories if your program contains sources from more than one directory." t nil)

(autoload (quote perldb) "gud" "\
Run perldb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)

(autoload (quote pdb) "gud" "\
Run pdb on program FILE in buffer `*gud-FILE*'.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)

(autoload (quote jdb) "gud" "\
Run jdb with command line COMMAND-LINE in a buffer.  The buffer is named
\"*gud*\" if no initial class is given or \"*gud-<initial-class-basename>*\"
if there is.  If the \"-classpath\" switch is given, omit all whitespace
between it and it's value." t nil)
 (add-hook 'same-window-regexps "\\*gud-.*\\*\\(\\|<[0-9]+>\\)")

;;;***

;;;### (autoloads (handwrite) "handwrite" "play/handwrite.el" (14033
;;;;;;  23498))
;;; Generated autoloads from play/handwrite.el

(autoload (quote handwrite) "handwrite" "\
Turns the buffer into a \"handwritten\" document.
The functions `handwrite-10pt', `handwrite-11pt', `handwrite-12pt'
and `handwrite-13pt' set up for various sizes of output.

Variables: handwrite-linespace     (default 12)
           handwrite-fontsize      (default 11)
           handwrite-numlines      (default 60)
           handwrite-pagenumbering (default nil)" t nil)

;;;***

;;;### (autoloads (hanoi) "hanoi" "play/hanoi.el" (12579 52103))
;;; Generated autoloads from play/hanoi.el

(autoload (quote hanoi) "hanoi" "\
Towers of Hanoi diversion.  Argument is number of rings." t nil)

;;;***

;;;### (autoloads nil "help-macro" "help-macro.el" (13838 43658))
;;; Generated autoloads from help-macro.el

(defvar three-step-help nil "\
*Non-nil means give more info about Help command in three steps.
The three steps are simple prompt, prompt with all options,
and window listing and describing the options.
A value of nil means skip the middle step, so that
\\[help-command] \\[help-command] gives the window that lists the options.")

;;;***

;;;### (autoloads (Helper-help Helper-describe-bindings) "helper"
;;;;;;  "emacs-lisp/helper.el" (12536 45574))
;;; Generated autoloads from emacs-lisp/helper.el

(autoload (quote Helper-describe-bindings) "helper" "\
Describe local key bindings of current mode." t nil)

(autoload (quote Helper-help) "helper" "\
Provide help for current mode." t nil)

;;;***

;;;### (autoloads (hexlify-buffer hexl-find-file hexl-mode) "hexl"
;;;;;;  "hexl.el" (14124 7361))
;;; Generated autoloads from hexl.el

(autoload (quote hexl-mode) "hexl" "\
\\<hexl-mode-map>
A major mode for editing binary files in hex dump format.

This function automatically converts a buffer into the hexl format
using the function `hexlify-buffer'.

Each line in the buffer has an \"address\" (displayed in hexadecimal)
representing the offset into the file that the characters on this line
are at and 16 characters from the file (displayed as hexadecimal
values grouped every 16 bits) and as their ASCII values.

If any of the characters (displayed as ASCII characters) are
unprintable (control or meta characters) they will be replaced as
periods.

If `hexl-mode' is invoked with an argument the buffer is assumed to be
in hexl format.

A sample format:

  HEX ADDR: 0001 0203 0405 0607 0809 0a0b 0c0d 0e0f     ASCII-TEXT
  --------  ---- ---- ---- ---- ---- ---- ---- ----  ----------------
  00000000: 5468 6973 2069 7320 6865 786c 2d6d 6f64  This is hexl-mod
  00000010: 652e 2020 4561 6368 206c 696e 6520 7265  e.  Each line re
  00000020: 7072 6573 656e 7473 2031 3620 6279 7465  presents 16 byte
  00000030: 7320 6173 2068 6578 6164 6563 696d 616c  s as hexadecimal
  00000040: 2041 5343 4949 0a61 6e64 2070 7269 6e74   ASCII.and print
  00000050: 6162 6c65 2041 5343 4949 2063 6861 7261  able ASCII chara
  00000060: 6374 6572 732e 2020 416e 7920 636f 6e74  cters.  Any cont
  00000070: 726f 6c20 6f72 206e 6f6e 2d41 5343 4949  rol or non-ASCII
  00000080: 2063 6861 7261 6374 6572 730a 6172 6520   characters.are 
  00000090: 6469 7370 6c61 7965 6420 6173 2070 6572  displayed as per
  000000a0: 696f 6473 2069 6e20 7468 6520 7072 696e  iods in the prin
  000000b0: 7461 626c 6520 6368 6172 6163 7465 7220  table character 
  000000c0: 7265 6769 6f6e 2e0a                      region..

Movement is as simple as movement in a normal emacs text buffer.  Most
cursor movement bindings are the same (ie. Use \\[hexl-backward-char], \\[hexl-forward-char], \\[hexl-next-line], and \\[hexl-previous-line]
to move the cursor left, right, down, and up).

Advanced cursor movement commands (ala \\[hexl-beginning-of-line], \\[hexl-end-of-line], \\[hexl-beginning-of-buffer], and \\[hexl-end-of-buffer]) are
also supported.

There are several ways to change text in hexl mode:

ASCII characters (character between space (0x20) and tilde (0x7E)) are
bound to self-insert so you can simply type the character and it will
insert itself (actually overstrike) into the buffer.

\\[hexl-quoted-insert] followed by another keystroke allows you to insert the key even if
it isn't bound to self-insert.  An octal number can be supplied in place
of another key to insert the octal number's ASCII representation.

\\[hexl-insert-hex-char] will insert a given hexadecimal value (if it is between 0 and 0xFF)
into the buffer at the current point.

\\[hexl-insert-octal-char] will insert a given octal value (if it is between 0 and 0377)
into the buffer at the current point.

\\[hexl-insert-decimal-char] will insert a given decimal value (if it is between 0 and 255)
into the buffer at the current point.

\\[hexl-mode-exit] will exit hexl-mode.

Note: saving the file with any of the usual Emacs commands
will actually convert it back to binary format while saving.

You can use \\[hexl-find-file] to visit a file in hexl-mode.

\\[describe-bindings] for advanced commands." t nil)

(autoload (quote hexl-find-file) "hexl" "\
Edit file FILENAME in hexl-mode.
Switch to a buffer visiting file FILENAME, creating one in none exists." t nil)

(autoload (quote hexlify-buffer) "hexl" "\
Convert a binary buffer to hexl format.
This discards the buffer's undo information." t nil)

;;;***

;;;### (autoloads (hide-ifdef-lines hide-ifdef-read-only hide-ifdef-initially
;;;;;;  hide-ifdef-mode) "hideif" "progmodes/hideif.el" (13859 9225))
;;; Generated autoloads from progmodes/hideif.el

(autoload (quote hide-ifdef-mode) "hideif" "\
Toggle Hide-Ifdef mode.  This is a minor mode, albeit a large one.
With ARG, turn Hide-Ifdef mode on if arg is positive, off otherwise.
In Hide-Ifdef mode, code within #ifdef constructs that the C preprocessor
would eliminate may be hidden from view.  Several variables affect
how the hiding is done:

hide-ifdef-env
	An association list of defined and undefined symbols for the
	current buffer.  Initially, the global value of `hide-ifdef-env'
	is used.

hide-ifdef-define-alist
	An association list of defined symbol lists.  
        Use `hide-ifdef-set-define-alist' to save the current `hide-ifdef-env'
        and `hide-ifdef-use-define-alist' to set the current `hide-ifdef-env'
        from one of the lists in `hide-ifdef-define-alist'.

hide-ifdef-lines
	Set to non-nil to not show #if, #ifdef, #ifndef, #else, and
	#endif lines when hiding.

hide-ifdef-initially
	Indicates whether `hide-ifdefs' should be called when Hide-Ifdef mode
	is activated.

hide-ifdef-read-only
	Set to non-nil if you want to make buffers read only while hiding.
	After `show-ifdefs', read-only status is restored to previous value.

\\{hide-ifdef-mode-map}" t nil)

(defvar hide-ifdef-initially nil "\
*Non-nil means call `hide-ifdefs' when Hide-Ifdef mode is first activated.")

(defvar hide-ifdef-read-only nil "\
*Set to non-nil if you want buffer to be read-only while hiding text.")

(defvar hide-ifdef-lines nil "\
*Non-nil means hide the #ifX, #else, and #endif lines.")

;;;***

;;;### (autoloads (hs-minor-mode hs-mouse-toggle-hiding hs-hide-all
;;;;;;  hs-show-hidden-short-form hs-hide-comments-when-hiding-all)
;;;;;;  "hideshow" "progmodes/hideshow.el" (13845 6001))
;;; Generated autoloads from progmodes/hideshow.el

(defvar hs-hide-comments-when-hiding-all t "\
Hide the comments too when you do an `hs-hide-all'.")

(defvar hs-show-hidden-short-form t "\
Leave only the first line visible in a hidden block.
If non-nil only the first line is visible when a block is in the
hidden state, else both the first line and the last line are shown.
A nil value disables `hs-adjust-block-beginning', which see.

An example of how this works: (in C mode)
original:

  /* My function main
     some more stuff about main
  */
  int
  main(void)
  {
    int x=0;
    return 0;
  }


hidden and `hs-show-hidden-short-form' is nil
  /* My function main...
  */
  int
  main(void)
  {...
  }

hidden and `hs-show-hidden-short-form' is t
  /* My function main...
  int
  main(void)...

For the last case you have to be on the line containing the
ellipsis when you do `hs-show-block'.")

(defvar hs-special-modes-alist (quote ((c-mode "{" "}" nil nil hs-c-like-adjust-block-beginning) (c++-mode "{" "}" "/[*/]" nil hs-c-like-adjust-block-beginning) (java-mode "\\(\\(\\([ 	]*\\(\\(abstract\\|final\\|native\\|p\\(r\\(ivate\\|otected\\)\\|ublic\\)\\|s\\(tatic\\|ynchronized\\)\\)[ 	\n]+\\)*[.a-zA-Z0-9_:]+[ 	\n]*\\(\\[[ 	\n]*\\][ 	\n]*\\)?\\([a-zA-Z0-9_:]+[ 	\n]*\\)([^)]*)\\([ \n	]+throws[ 	\n][^{]+\\)?\\)\\|\\([ 	]*static[^{]*\\)\\)[ 	\n]*{\\)" "}" "/[*/]" java-hs-forward-sexp hs-c-like-adjust-block-beginning))) "\
*Alist for initializing the hideshow variables for different modes.
It has the form
  (MODE START END COMMENT-START FORWARD-SEXP-FUNC ADJUST-BEG-FUNC).
If present, hideshow will use these values as regexps for start, end
and comment-start, respectively.  Since Algol-ish languages do not have
single-character block delimiters, the function `forward-sexp' used
by hideshow doesn't work.  In this case, if a similar function is
available, you can register it and have hideshow use it instead of
`forward-sexp'.  See the documentation for `hs-adjust-block-beginning'
to see what is the use of ADJUST-BEG-FUNC.

If any of those is left nil, hideshow will try to guess some values
using function `hs-grok-mode-type'.

Note that the regexps should not contain leading or trailing whitespace.")

(autoload (quote hs-hide-all) "hideshow" "\
Hide all top-level blocks, displaying only first and last lines.
Move point to the beginning of the line, and it run the normal hook
`hs-hide-hook'.  See documentation for `run-hooks'.
If `hs-hide-comments-when-hiding-all' is t, also hide the comments." t nil)

(autoload (quote hs-mouse-toggle-hiding) "hideshow" "\
Toggle hiding/showing of a block.
Should be bound to a mouse key." t nil)

(autoload (quote hs-minor-mode) "hideshow" "\
Toggle hideshow minor mode.
With ARG, turn hideshow minor mode on if ARG is positive, off otherwise.
When hideshow minor mode is on, the menu bar is augmented with hideshow
commands and the hideshow commands are enabled.
The value '(hs . t) is added to `buffer-invisibility-spec'.
Last, the normal hook `hs-minor-mode-hook' is run; see the doc
for `run-hooks'.

The main commands are: `hs-hide-all', `hs-show-all', `hs-hide-block',
`hs-show-block', `hs-hide-level' and `hs-show-region'.
Also see the documentation for the variable `hs-show-hidden-short-form'.

Turning hideshow minor mode off reverts the menu bar and the
variables to default values and disables the hideshow commands.

Key bindings:
\\{hs-minor-mode-map}" t nil)

;;;***

;;;### (autoloads (global-highlight-changes highlight-compare-with-file
;;;;;;  highlight-changes-rotate-faces highlight-changes-previous-change
;;;;;;  highlight-changes-next-change highlight-changes-mode highlight-changes-remove-highlight)
;;;;;;  "hilit-chg" "hilit-chg.el" (14082 18582))
;;; Generated autoloads from hilit-chg.el

(autoload (quote highlight-changes-remove-highlight) "hilit-chg" "\
Remove the change face from the region.  
This allows you to manually remove highlighting from uninteresting changes." t nil)

(autoload (quote highlight-changes-mode) "hilit-chg" "\
Toggle (or initially set) Highlight Changes mode.

Without an argument,  
  if Highlight Changes mode is not enabled, then enable it (to either active
  or passive as determined by variable highlight-changes-initial-state);
  otherwise, toggle between active and passive states.

With an argument,
  if just C-u  or  a positive argument,  set state to active;
  with a zero argument,  set state to passive;
  with a negative argument,  disable Highlight Changes mode completely.

Active state -  means changes are shown in a distinctive face.
Passive state - means changes are kept and new ones recorded but are
		not displayed in a different face.

Functions:
\\[highlight-changes-next-change] - move point to beginning of next change
\\[highlight-changes-previous-change] - move to beginning of previous change 
\\[highlight-compare-with-file] - mark text as changed by comparing this
	buffer with the contents of a file
\\[highlight-changes-remove-highlight] - remove the change face from the region
\\[highlight-changes-rotate-faces] - rotate different \"ages\" of changes through 
	various faces.


Hook variables:
highlight-changes-enable-hook - when Highlight Changes mode enabled.
highlight-changes-toggle-hook - when entering active or passive state
highlight-changes-disable-hook - when turning off Highlight Changes mode.
" t nil)

(autoload (quote highlight-changes-next-change) "hilit-chg" "\
Move to the beginning of the next change, if in Highlight Changes mode." t nil)

(autoload (quote highlight-changes-previous-change) "hilit-chg" "\
Move to the beginning of the previous change, if in Highlight Changes mode." t nil)

(autoload (quote highlight-changes-rotate-faces) "hilit-chg" "\
Rotate the faces used by Highlight Changes mode.

Current changes will be display in the face described by the first element
of highlight-changes-face-list, those (older) changes will be shown in the
face described by the second element, and so on.  Very old changes remain
shown in the last face in the list.

You can automatically rotate colours when the buffer is saved
by adding this to local-write-file-hooks,  by evaling (in the
buffer to be saved):
  (add-hook 'local-write-file-hooks 'highlight-changes-rotate-faces)
" t nil)

(autoload (quote highlight-compare-with-file) "hilit-chg" "\
Compare this buffer with a file, and highlight differences.

The current buffer must be an unmodified buffer visiting a file,
and not in read-only mode.

If the backup filename exists, it is used as the default
when called interactively.

If a buffer is visiting the file being compared against,  it also will
have its differences highlighted.   Otherwise, the file is read in
temporarily but the buffer is deleted.

If a buffer is read-only,  differences will be highlighted but no property
changes made,  so \\[highlight-changes-next-change] and
\\[highlight-changes-previous-change] will not work." t nil)

(autoload (quote global-highlight-changes) "hilit-chg" "\
Turn on or off global Highlight Changes mode.

When called interactively:
- if no prefix, toggle global Highlight Changes mode on or off
- if called with a positive prefix (or just C-u) turn it on in active mode
- if called with a zero prefix  turn it on in passive mode
- if called with a negative prefix turn it off

When called from a program:
- if ARG is nil or omitted, turn it off
- if ARG is 'active,  turn it on in active mode
- if ARG is 'passive, turn it on in passive mode
- otherwise just turn it on 

When global Highlight Changes mode is enabled, Highlight Changes mode is turned
on for future \"suitable\" buffers (and for \"suitable\" existing buffers if
variable `highlight-changes-global-changes-existing-buffers' is non-nil).
\"Suitablity\" is determined by variable `highlight-changes-global-modes'." t nil)

;;;***

;;;### (autoloads (make-hippie-expand-function hippie-expand hippie-expand-only-buffers
;;;;;;  hippie-expand-ignore-buffers hippie-expand-max-buffers hippie-expand-no-restriction
;;;;;;  hippie-expand-dabbrev-as-symbol hippie-expand-dabbrev-skip-space
;;;;;;  hippie-expand-verbose) "hippie-exp" "hippie-exp.el" (13674
;;;;;;  32879))
;;; Generated autoloads from hippie-exp.el

(defvar hippie-expand-try-functions-list (quote (try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)) "\
The list of expansion functions tried in order by `hippie-expand'.
To change the behavior of `hippie-expand', remove, change the order of,
or insert functions in this list.")

(defvar hippie-expand-verbose t "\
*Non-nil makes `hippie-expand' output which function it is trying.")

(defvar hippie-expand-dabbrev-skip-space nil "\
*Non-nil means tolerate trailing spaces in the abbreviation to expand.")

(defvar hippie-expand-dabbrev-as-symbol t "\
*Non-nil means expand as symbols, i.e. syntax `_' is considered a letter.")

(defvar hippie-expand-no-restriction t "\
*Non-nil means that narrowed buffers are widened during search.")

(defvar hippie-expand-max-buffers nil "\
*The maximum number of buffers (apart from the current) searched.
If nil, all buffers are searched.")

(defvar hippie-expand-ignore-buffers (quote ("^ \\*.*\\*$" dired-mode)) "\
*A list specifying which buffers not to search (if not current).
Can contain both regexps matching buffer names (as strings) and major modes
\(as atoms)")

(defvar hippie-expand-only-buffers nil "\
*A list specifying the only buffers to search (in addition to current).
Can contain both regexps matching buffer names (as strings) and major modes
\(as atoms). If non-NIL, this variable overrides the variable
`hippie-expand-ignore-buffers'.")

(autoload (quote hippie-expand) "hippie-exp" "\
Try to expand text before point, using multiple methods.
The expansion functions in `hippie-expand-try-functions-list' are
tried in order, until a possible expansion is found.  Repeated
application of `hippie-expand' inserts successively possible
expansions.  
With a positive numeric argument, jumps directly to the ARG next
function in this list.  With a negative argument or just \\[universal-argument], 
undoes the expansion." t nil)

(autoload (quote make-hippie-expand-function) "hippie-exp" "\
Construct a function similar to `hippie-expand'.
Make it use the expansion functions in TRY-LIST.  An optional second
argument VERBOSE non-nil makes the function verbose." nil (quote macro))

;;;***

;;;### (autoloads (list-holidays) "holidays" "calendar/holidays.el"
;;;;;;  (13462 53924))
;;; Generated autoloads from calendar/holidays.el

(autoload (quote list-holidays) "holidays" "\
Display holidays for years Y1 to Y2 (inclusive).

The optional list of holidays L defaults to `calendar-holidays'.  See the
documentation for that variable for a description of holiday lists.

The optional LABEL is used to label the buffer created." t nil)

;;;***

;;;### (autoloads (hscroll-global-mode hscroll-mode turn-on-hscroll)
;;;;;;  "hscroll" "hscroll.el" (14210 21345))
;;; Generated autoloads from hscroll.el

(autoload (quote turn-on-hscroll) "hscroll" "\
Unconditionally turn on Hscroll mode in the current buffer." nil nil)

(autoload (quote hscroll-mode) "hscroll" "\
Toggle HScroll mode in the current buffer.
With ARG, turn HScroll mode on if ARG is positive, off otherwise.
In HScroll mode, truncated lines will automatically scroll left or
right when point gets near either edge of the window.
  See also \\[hscroll-global-mode]." t nil)

(autoload (quote hscroll-global-mode) "hscroll" "\
Toggle HScroll mode in all buffers (excepting minibuffers).
With ARG, turn HScroll mode on if ARG is positive, off otherwise.
If a buffer ever has HScroll mode set locally (via \\[hscroll-mode]), 
it will forever use the local value (i.e., \\[hscroll-global-mode] 
will have no effect on it).
  See also \\[hscroll-mode]." t nil)

;;;***

;;;### (autoloads (icomplete-minibuffer-setup icomplete-mode) "icomplete"
;;;;;;  "icomplete.el" (14118 2220))
;;; Generated autoloads from icomplete.el

(autoload (quote icomplete-mode) "icomplete" "\
Activate incremental minibuffer completion for this Emacs session.
Deactivates with negative universal argument." t nil)

(autoload (quote icomplete-minibuffer-setup) "icomplete" "\
Run in minibuffer on activation to establish incremental completion.
Usually run by inclusion in `minibuffer-setup-hook'." nil nil)

;;;***

;;;### (autoloads (icon-mode) "icon" "progmodes/icon.el" (13549 38921))
;;; Generated autoloads from progmodes/icon.el

(autoload (quote icon-mode) "icon" "\
Major mode for editing Icon code.
Expression and list commands understand all Icon brackets.
Tab indents for Icon code.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
\\{icon-mode-map}
Variables controlling indentation style:
 icon-tab-always-indent
    Non-nil means TAB in Icon mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 icon-auto-newline
    Non-nil means automatically newline before and after braces
    inserted in Icon code.
 icon-indent-level
    Indentation of Icon statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 icon-continued-statement-offset
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 icon-continued-brace-offset
    Extra indentation given to a brace that starts a substatement.
    This is in addition to `icon-continued-statement-offset'.
 icon-brace-offset
    Extra indentation for line if it starts with an open brace.
 icon-brace-imaginary-offset
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.

Turning on Icon mode calls the value of the variable `icon-mode-hook'
with no args, if that value is non-nil." t nil)

;;;***

;;;### (autoloads (ielm) "ielm" "ielm.el" (13638 46794))
;;; Generated autoloads from ielm.el
 (add-hook 'same-window-buffer-names "*ielm*")

(autoload (quote ielm) "ielm" "\
Interactively evaluate Emacs Lisp expressions.
Switches to the buffer `*ielm*', or creates it if it does not exist." t nil)

;;;***

;;;### (autoloads (defimage remove-images insert-image put-image
;;;;;;  create-image image-type-available-p image-type-from-file-header)
;;;;;;  "image" "image.el" (14228 23898))
;;; Generated autoloads from image.el

(autoload (quote image-type-from-file-header) "image" "\
Determine the type of image file FILE from its first few bytes.
Value is a symbol specifying the image type, or nil if type cannot
be determined." nil nil)

(autoload (quote image-type-available-p) "image" "\
Value is non-nil if image type TYPE is available.
Image types are symbols like `xbm' or `jpeg'." nil nil)

(autoload (quote create-image) "image" "\
Create an image which will be loaded from FILE.
Optional TYPE is a symbol describing the image type.  If TYPE is omitted
or nil, try to determine the image file type from its first few bytes.
If that doesn't work, use FILE's extension.as image type.
Optional PROPS are additional image attributes to assign to the image,
like, e.g. `:heuristic-mask t'.
Value is the image created, or nil if images of type TYPE are not supported." nil nil)

(autoload (quote put-image) "image" "\
Put image IMAGE in front of POS in BUFFER.
IMAGE must be an image created with `create-image' or `defimage'.
POS may be an integer or marker.
BUFFER nil or omitted means use the current buffer.
AREA is where to display the image.  AREA nil or omitted means
display it in the text area, a value of `left-margin' means
display it in the left marginal area, a value of `right-margin'
means display it in the right marginal area.
IMAGE is displayed by putting an overlay into BUFFER with a
`before-string' that has a `display' property whose value is the
image." nil nil)

(autoload (quote insert-image) "image" "\
Insert IMAGE into current buffer at point.
AREA is where to display the image.  AREA nil or omitted means
display it in the text area, a value of `left-margin' means
display it in the left marginal area, a value of `right-margin'
means display it in the right marginal area.
IMAGE is displayed by inserting an \"x\" into the current buffer
having a `display' property whose value is the image." nil nil)

(autoload (quote remove-images) "image" "\
Remove images between START and END in BUFFER.
Remove only images that were put in BUFFER with calls to `put-image'.
BUFFER nil or omitted means use the current buffer." nil nil)

(autoload (quote defimage) "image" "\
Define SYMBOL as an image.

SPECS is a list of image specifications.  DOC is an optional
documentation string.

Each image specification in SPECS is a property list.  The contents of
a specification are image type dependent.  All specifications must at
least contain the properties `:type TYPE' and `:file FILE', where TYPE
is a symbol specifying the image type, e.g. `xbm', and FILE is the
file to load the image from.  The first image specification whose TYPE
is supported, and FILE exists, is used to define SYMBOL.

Example:

   (defimage test-image ((:type xpm :file \"~/test1.xpm\")
                         (:type xbm :file \"~/test1.xbm\")))" nil (quote macro))

;;;***

;;;### (autoloads (imenu imenu-add-menubar-index imenu-add-to-menubar
;;;;;;  imenu-sort-function) "imenu" "imenu.el" (14090 26268))
;;; Generated autoloads from imenu.el

(defvar imenu-sort-function nil "\
*The function to use for sorting the index mouse-menu.

Affects only the mouse index menu.

Set this to nil if you don't want any sorting (faster).
The items in the menu are then presented in the order they were found
in the buffer.

Set it to `imenu--sort-by-name' if you want alphabetic sorting.

The function should take two arguments and return t if the first
element should come before the second.  The arguments are cons cells;
\(NAME . POSITION).  Look at `imenu--sort-by-name' for an example.")

(defvar imenu-generic-expression nil "\
The regex pattern to use for creating a buffer index.

If non-nil this pattern is passed to `imenu--generic-function'
to create a buffer index.

The value should be an alist with elements that look like this:
 (MENU-TITLE REGEXP INDEX)
or like this:
 (MENU-TITLE REGEXP INDEX FUNCTION ARGUMENTS...)
with zero or more ARGUMENTS.  The former format creates a simple element in
the index alist when it matches; the latter creates a special element
of the form  (NAME FUNCTION POSITION-MARKER ARGUMENTS...)
with FUNCTION and ARGUMENTS beiong copied from `imenu-generic-expression'.

MENU-TITLE is a string used as the title for the submenu or nil if the
entries are not nested.

REGEXP is a regexp that should match a construct in the buffer that is
to be displayed in the menu; i.e., function or variable definitions,
etc.  It contains a substring which is the name to appear in the
menu.  See the info section on Regexps for more information.

INDEX points to the substring in REGEXP that contains the name (of the
function, variable or type) that is to appear in the menu.

The variable is buffer-local.

The variable `imenu-case-fold-search' determines whether or not the
regexp matches are case sensitive. and `imenu-syntax-alist' can be
used to alter the syntax table for the search.

For example, see the value of `lisp-imenu-generic-expression' used by
`lisp-mode' and `emacs-lisp-mode' with `imenu-syntax-alist' set
locally to give the characters which normally have \"punctuation\"
syntax \"word\" syntax during matching.")

(make-variable-buffer-local (quote imenu-generic-expression))

(defvar imenu-create-index-function (quote imenu-default-create-index-function) "\
The function to use for creating a buffer index.

It should be a function that takes no arguments and returns an index
of the current buffer as an alist.

Simple elements in the alist look like (INDEX-NAME . INDEX-POSITION).
Special elements look like (INDEX-NAME INDEX-POSITION FUNCTION ARGUMENTS...).
A nested sub-alist element looks like (INDEX-NAME SUB-ALIST).
The function `imenu--subalist-p' tests an element and returns t
if it is a sub-alist.

This function is called within a `save-excursion'.

The variable is buffer-local.")

(make-variable-buffer-local (quote imenu-create-index-function))

(defvar imenu-prev-index-position-function (quote beginning-of-defun) "\
Function for finding the next index position.

If `imenu-create-index-function' is set to
`imenu-default-create-index-function', then you must set this variable
to a function that will find the next index, looking backwards in the
file.

The function should leave point at the place to be connected to the
index and it should return nil when it doesn't find another index.

This variable is local in all buffers.")

(make-variable-buffer-local (quote imenu-prev-index-position-function))

(defvar imenu-extract-index-name-function nil "\
Function for extracting the index item name, given a position.

This function is called after `imenu-prev-index-position-function'
finds a position for an index item, with point at that position.
It should return the name for that index item.

This variable is local in all buffers.")

(make-variable-buffer-local (quote imenu-extract-index-name-function))

(defvar imenu-default-goto-function (quote imenu-default-goto-function) "\
The default function called when selecting an Imenu item.
The function in this variable is called when selecting a normal index-item.")

(make-variable-buffer-local (quote imenu-default-goto-function))

(make-variable-buffer-local (quote imenu-case-fold-search))

(autoload (quote imenu-add-to-menubar) "imenu" "\
Add an `imenu' entry to the menu bar for the current buffer.
NAME is a string used to name the menu bar item.
See the command `imenu' for more information." t nil)

(autoload (quote imenu-add-menubar-index) "imenu" "\
Add an Imenu \"Index\" entry on the menu bar for the current buffer.

A trivial interface to `imenu-add-to-menubar' suitable for use in a hook." t nil)

(autoload (quote imenu) "imenu" "\
Jump to a place in the buffer chosen using a buffer menu or mouse menu.
INDEX-ITEM specifies the position.  See `imenu-choose-buffer-index'
for more information." t nil)

;;;***

;;;### (autoloads (inferior-lisp) "inf-lisp" "inf-lisp.el" (13898
;;;;;;  16181))
;;; Generated autoloads from inf-lisp.el

(defvar inferior-lisp-filter-regexp "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'" "\
*What not to save on inferior Lisp's input history.
Input matching this regexp is not saved on the input history in Inferior Lisp
mode.  Default is whitespace followed by 0 or 1 single-letter colon-keyword 
\(as in :a, :c, etc.)")

(defvar inferior-lisp-program "lisp" "\
*Program name for invoking an inferior Lisp with for Inferior Lisp mode.")

(defvar inferior-lisp-load-command "(load \"%s\")\n" "\
*Format-string for building a Lisp expression to load a file.
This format string should use `%s' to substitute a file name
and should result in a Lisp expression that will command the inferior Lisp
to load that file.  The default works acceptably on most Lisps.
The string \"(progn (load \\\"%s\\\" :verbose nil :print t) (values))\\n\"
produces cosmetically superior output for this application,
but it works only in Common Lisp.")

(defvar inferior-lisp-prompt "^[^> \n]*>+:? *" "\
Regexp to recognise prompts in the Inferior Lisp mode.
Defaults to \"^[^> \\n]*>+:? *\", which works pretty good for Lucid, kcl,
and franz.  This variable is used to initialize `comint-prompt-regexp' in the 
Inferior Lisp buffer.

More precise choices:
Lucid Common Lisp: \"^\\\\(>\\\\|\\\\(->\\\\)+\\\\) *\"
franz: \"^\\\\(->\\\\|<[0-9]*>:\\\\) *\"
kcl: \"^>+ *\"

This is a fine thing to set in your .emacs file.")

(defvar inferior-lisp-mode-hook (quote nil) "\
*Hook for customising Inferior Lisp mode.")

(autoload (quote inferior-lisp) "inf-lisp" "\
Run an inferior Lisp process, input and output via buffer `*inferior-lisp*'.
If there is a process already running in `*inferior-lisp*', just switch
to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-lisp-program').  Runs the hooks from
`inferior-lisp-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)" t nil)
 (add-hook 'same-window-buffer-names "*inferior-lisp*")

(defalias (quote run-lisp) (quote inferior-lisp))

;;;***

;;;### (autoloads (Info-speedbar-browser Info-goto-emacs-key-command-node
;;;;;;  Info-goto-emacs-command-node info-standalone info info-other-window)
;;;;;;  "info" "info.el" (14115 64939))
;;; Generated autoloads from info.el

(autoload (quote info-other-window) "info" "\
Like `info' but show the Info buffer in another window." t nil)
 (add-hook 'same-window-buffer-names "*info*")

(autoload (quote info) "info" "\
Enter Info, the documentation browser.
Optional argument FILE specifies the file to examine;
the default is the top-level directory of Info.

In interactive use, a prefix argument directs this command
to read a file name from the minibuffer.

The search path for Info files is in the variable `Info-directory-list'.
The top-level Info directory is made by combining all the files named `dir' 
in all the directories in that path." t nil)

(autoload (quote info-standalone) "info" "\
Run Emacs as a standalone Info reader.
Usage:  emacs -f info-standalone [filename]
In standalone mode, \\<Info-mode-map>\\[Info-exit] exits Emacs itself." nil nil)

(autoload (quote Info-goto-emacs-command-node) "info" "\
Go to the Info node in the Emacs manual for command COMMAND.
The command is found by looking up in Emacs manual's Command Index
or in another manual found via COMMAND's `info-file' property or
the variable `Info-file-list-for-emacs'." t nil)

(autoload (quote Info-goto-emacs-key-command-node) "info" "\
Go to the Info node in the Emacs manual the command bound to KEY, a string.
Interactively, if the binding is execute-extended-command, a command is read.
The command is found by looking up in Emacs manual's Command Index
or in another manual found via COMMAND's `info-file' property or
the variable `Info-file-list-for-emacs'." t nil)

(autoload (quote Info-speedbar-browser) "info" "\
Initialize speedbar to display an info node browser.
This will add a speedbar major display mode." t nil)

;;;***

;;;### (autoloads (info-complete-file info-complete-symbol info-lookup-file
;;;;;;  info-lookup-symbol info-lookup-reset) "info-look" "info-look.el"
;;;;;;  (14177 55621))
;;; Generated autoloads from info-look.el

(autoload (quote info-lookup-reset) "info-look" "\
Throw away all cached data.
This command is useful if the user wants to start at the beginning without
quitting Emacs, for example, after some Info documents were updated on the
system." t nil)

(autoload (quote info-lookup-symbol) "info-look" "\
Display the definition of SYMBOL, as found in the relevant manual.
When this command is called interactively, it reads SYMBOL from the minibuffer.
In the minibuffer, use M-n to yank the default argument value
into the minibuffer so you can edit it.
The default symbol is the one found at point." t nil)

(autoload (quote info-lookup-file) "info-look" "\
Display the documentation of a file.
When this command is called interactively, it reads FILE from the minibuffer.
In the minibuffer, use M-n to yank the default file name
into the minibuffer so you can edit it.
The default file name is the one found at point." t nil)

(autoload (quote info-complete-symbol) "info-look" "\
Perform completion on symbol preceding point." t nil)

(autoload (quote info-complete-file) "info-look" "\
Perform completion on file preceding point." t nil)

;;;***

;;;### (autoloads (batch-info-validate Info-validate Info-split Info-tagify)
;;;;;;  "informat" "informat.el" (13728 5098))
;;; Generated autoloads from informat.el

(autoload (quote Info-tagify) "informat" "\
Create or update Info file tag table in current buffer or in a region." t nil)

(autoload (quote Info-split) "informat" "\
Split an info file into an indirect file plus bounded-size subfiles.
Each subfile will be up to 50,000 characters plus one node.

To use this command, first visit a large Info file that has a tag
table.  The buffer is modified into a (small) indirect info file which
should be saved in place of the original visited file.

The subfiles are written in the same directory the original file is
in, with names generated by appending `-' and a number to the original
file name.  The indirect file still functions as an Info file, but it
contains just the tag table and a directory of subfiles." t nil)

(autoload (quote Info-validate) "informat" "\
Check current buffer for validity as an Info file.
Check that every node pointer points to an existing node." t nil)

(autoload (quote batch-info-validate) "informat" "\
Runs `Info-validate' on the files remaining on the command line.
Must be used only with -batch, and kills Emacs on completion.
Each file will be processed even if an error occurred previously.
For example, invoke \"emacs -batch -f batch-info-validate $info/ ~/*.info\"" nil nil)

;;;***

;;;### (autoloads (isearch-process-search-multibyte-characters isearch-toggle-input-method
;;;;;;  isearch-toggle-specified-input-method) "isearch-x" "international/isearch-x.el"
;;;;;;  (13770 34350))
;;; Generated autoloads from international/isearch-x.el

(autoload (quote isearch-toggle-specified-input-method) "isearch-x" "\
Select an input method and turn it on in interactive search." t nil)

(autoload (quote isearch-toggle-input-method) "isearch-x" "\
Toggle input method in interactive search." t nil)

(autoload (quote isearch-process-search-multibyte-characters) "isearch-x" nil nil nil)

;;;***

;;;### (autoloads (iso-accents-mode) "iso-acc" "international/iso-acc.el"
;;;;;;  (14127 16153))
;;; Generated autoloads from international/iso-acc.el

(autoload (quote iso-accents-mode) "iso-acc" "\
Toggle ISO Accents mode, in which accents modify the following letter.
This permits easy insertion of accented characters according to ISO-8859-1.
When Iso-accents mode is enabled, accent character keys
\(`, ', \", ^, / and ~) do not self-insert; instead, they modify the following
letter key so that it inserts an ISO accented letter.

You can customize ISO Accents mode to a particular language
with the command `iso-accents-customize'.

Special combinations: ~c gives a c with cedilla,
~d gives an Icelandic eth (d with dash).
~t gives an Icelandic thorn.
\"s gives German sharp s.
/a gives a with ring.
/e gives an a-e ligature.
~< and ~> give guillemots.
~! gives an inverted exclamation mark.
~? gives an inverted question mark.

With an argument, a positive argument enables ISO Accents mode, 
and a negative argument disables it." t nil)

;;;***

;;;### (autoloads (iso-cvt-define-menu iso-cvt-write-only iso-cvt-read-only
;;;;;;  iso-iso2duden iso-iso2gtex iso-gtex2iso iso-tex2iso iso-iso2tex
;;;;;;  iso-german iso-spanish) "iso-cvt" "international/iso-cvt.el"
;;;;;;  (13768 42797))
;;; Generated autoloads from international/iso-cvt.el

(autoload (quote iso-spanish) "iso-cvt" "\
Translate net conventions for Spanish to ISO 8859-1.
The region between FROM and TO is translated using the table TRANS-TAB.
Optional arg BUFFER is ignored (so that the function can can be used in
`format-alist')." t nil)

(autoload (quote iso-german) "iso-cvt" "\
Translate net conventions for German to ISO 8859-1.
The region between FROM and TO is translated using the table TRANS-TAB.
Optional arg BUFFER is ignored (so that the function can can be used in
`format-alist')." t nil)

(autoload (quote iso-iso2tex) "iso-cvt" "\
Translate ISO 8859-1 characters to TeX sequences.
The region between FROM and TO is translated using the table TRANS-TAB.
Optional arg BUFFER is ignored (so that the function can can be used in
`format-alist')." t nil)

(autoload (quote iso-tex2iso) "iso-cvt" "\
Translate TeX sequences to ISO 8859-1 characters.
The region between FROM and TO is translated using the table TRANS-TAB.
Optional arg BUFFER is ignored (so that the function can can be used in
`format-alist')." t nil)

(autoload (quote iso-gtex2iso) "iso-cvt" "\
Translate German TeX sequences to ISO 8859-1 characters.
The region between FROM and TO is translated using the table TRANS-TAB.
Optional arg BUFFER is ignored (so that the function can can be used in
`format-alist')." t nil)

(autoload (quote iso-iso2gtex) "iso-cvt" "\
Translate ISO 8859-1 characters to German TeX sequences.
The region between FROM and TO is translated using the table TRANS-TAB.
Optional arg BUFFER is ignored (so that the function can can be used in
`format-alist')." t nil)

(autoload (quote iso-iso2duden) "iso-cvt" "\
Translate ISO 8859-1 characters to German TeX sequences.
The region between FROM and TO is translated using the table TRANS-TAB.
Optional arg BUFFER is ignored (so that the function can can be used in
`format-alist')." t nil)

(autoload (quote iso-cvt-read-only) "iso-cvt" "\
Warn that format is read-only." t nil)

(autoload (quote iso-cvt-write-only) "iso-cvt" "\
Warn that format is write-only." t nil)

(autoload (quote iso-cvt-define-menu) "iso-cvt" "\
Add submenus to the Files menu, to convert to and from various formats." t nil)

;;;***

;;;### (autoloads nil "iso-transl" "international/iso-transl.el"
;;;;;;  (14164 4446))
;;; Generated autoloads from international/iso-transl.el
 (or key-translation-map (setq key-translation-map (make-sparse-keymap)))
 (define-key key-translation-map "\C-x8" 'iso-transl-ctl-x-8-map)
 (autoload 'iso-transl-ctl-x-8-map "iso-transl" "Keymap for C-x 8 prefix." t 'keymap)

;;;***

;;;### (autoloads (ispell-message ispell-minor-mode ispell-complete-word-interior-frag
;;;;;;  ispell-complete-word ispell-continue ispell-buffer ispell-comments-and-strings
;;;;;;  ispell-region ispell-change-dictionary ispell-kill-ispell
;;;;;;  ispell-help ispell-word ispell-dictionary-alist ispell-local-dictionary-alist
;;;;;;  ispell-personal-dictionary) "ispell" "ispell.el" (14218 3684))
;;; Generated autoloads from ispell.el

(defvar ispell-personal-dictionary nil "\
*File name of your personal spelling dictionary, or nil.
If nil, the default personal dictionary, \"~/.ispell_DICTNAME\" is used,
where DICTNAME is the name of your default dictionary.")

(defvar ispell-local-dictionary-alist nil "\
*Contains local or customized dictionary definitions.
See `ispell-dictionary-alist'.")

(setq ispell-dictionary-alist-1 (quote ((nil "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil iso-8859-1) ("american" "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil iso-8859-1) ("british" "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B" "-d" "british") nil iso-8859-1) ("castellano" "[A-Z\301\311\315\321\323\332\334a-z\341\351\355\361\363\372\374]" "[^A-Z\301\311\315\321\323\332\334a-z\341\351\355\361\363\372\374]" "[---]" nil ("-B" "-d" "castellano") "~tex" iso-8859-1) ("castellano8" "[A-Z\301\311\315\321\323\332\334a-z\341\351\355\361\363\372\374]" "[^A-Z\301\311\315\321\323\332\334a-z\341\351\355\361\363\372\374]" "[---]" nil ("-B" "-d" "castellano") "~latin1" iso-8859-1))))

(setq ispell-dictionary-alist-2 (quote (("czech" "[A-Za-z\301\311\314\315\323\332\331\335\256\251\310\330\317\253\322\341\351\354\355\363\372\371\375\276\271\350\370\357\273\362]" "[^A-Za-z\301\311\314\315\323\332\331\335\256\251\310\330\317\253\322\341\351\354\355\363\372\371\375\276\271\350\370\357\273\362]" "" nil ("-B" "-d" "czech") nil iso-8859-2) ("dansk" "[A-Z\306\330\305a-z\346\370\345]" "[^A-Z\306\330\305a-z\346\370\345]" "[']" nil ("-C") nil iso-8859-1) ("deutsch" "[a-zA-Z\"]" "[^a-zA-Z\"]" "[']" t ("-C") "~tex" iso-8859-1) ("deutsch8" "[a-zA-Z\304\326\334\344\366\337\374]" "[^a-zA-Z\304\326\334\344\366\337\374]" "[']" t ("-C" "-d" "deutsch") "~latin1" iso-8859-1) ("english" "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil iso-8859-1))))

(setq ispell-dictionary-alist-3 (quote (("esperanto" "[A-Za-z\246\254\266\274\306\330\335\336\346\370\375\376]" "[^A-Za-z\246\254\266\274\306\330\335\336\346\370\375\376]" "[-']" t ("-C") "~latin3" iso-8859-1) ("esperanto-tex" "[A-Za-z^\\]" "[^A-Za-z^\\]" "[-'`\"]" t ("-C" "-d" "esperanto") "~tex" iso-8859-1) ("francais7" "[A-Za-z]" "[^A-Za-z]" "[`'^---]" t nil nil iso-8859-1) ("francais" "[A-Za-z\300\302\306\307\310\311\312\313\316\317\324\331\333\334\340\342\347\350\351\352\353\356\357\364\371\373\374]" "[^A-Za-z\300\302\306\307\310\311\312\313\316\317\324\331\333\334\340\342\347\350\351\352\353\356\357\364\371\373\374]" "[---']" t nil "~list" iso-8859-1))))

(setq ispell-dictionary-alist-4 (quote (("francais-tex" "[A-Za-z\300\302\306\307\310\311\312\313\316\317\324\331\333\334\340\342\347\350\351\352\353\356\357\364\371\373\374\\]" "[^A-Za-z\300\302\306\307\310\311\312\313\316\317\324\331\333\334\340\342\347\350\351\352\353\356\357\364\371\373\374\\]" "[---'^`\"]" t nil "~tex" iso-8859-1) ("nederlands" "[A-Za-z\300-\305\307\310-\317\322-\326\331-\334\340-\345\347\350-\357\361\362-\366\371-\374]" "[^A-Za-z\300-\305\307\310-\317\322-\326\331-\334\340-\345\347\350-\357\361\362-\366\371-\374]" "[']" t ("-C") nil iso-8859-1) ("nederlands8" "[A-Za-z\300-\305\307\310-\317\322-\326\331-\334\340-\345\347\350-\357\361\362-\366\371-\374]" "[^A-Za-z\300-\305\307\310-\317\322-\326\331-\334\340-\345\347\350-\357\361\362-\366\371-\374]" "[']" t ("-C") nil iso-8859-1))))

(setq ispell-dictionary-alist-5 (quote (("norsk" "[A-Za-z\305\306\307\310\311\322\324\330\345\346\347\350\351\362\364\370]" "[^A-Za-z\305\306\307\310\311\322\324\330\345\346\347\350\351\362\364\370]" "[\"]" nil ("-d" "norsk") "~list" iso-8859-1) ("norsk7-tex" "[A-Za-z{}\\'^`]" "[^A-Za-z{}\\'^`]" "[\"]" nil ("-d" "norsk") "~plaintex" iso-8859-1) ("polish" "[A-Za-z\241\243\246\254\257\261\263\266\274\277\306\312\321\323\346\352\361\363]" "[^A-Za-z\241\243\246\254\257\261\263\266\274\277\306\312\321\323\346\352\361\363]" "" nil ("-d" "polish") nil iso-8859-2))))

(setq ispell-dictionary-alist-6 (quote (("russian" "[\341\342\367\347\344\345\263\366\372\351\352\353\354\355\356\357\360\362\363\364\365\346\350\343\376\373\375\370\371\377\374\340\361\301\302\327\307\304\305\243\326\332\311\312\313\314\315\316\317\320\322\323\324\325\306\310\303\336\333\335\330\331\337\334\300\321]" "[^\341\342\367\347\344\345\263\366\372\351\352\353\354\355\356\357\360\362\363\364\365\346\350\343\376\373\375\370\371\377\374\340\361\301\302\327\307\304\305\243\326\332\311\312\313\314\315\316\317\320\322\323\324\325\306\310\303\336\333\335\330\331\337\334\300\321]" "" nil ("-C" "-d" "russian") nil koi8-r) ("svenska" "[A-Za-z\345\344\366\351\340\374\350\346\370\347\305\304\326\311\300\334\310\306\330\307]" "[^A-Za-z\345\344\366\351\340\374\350\346\370\347\305\304\326\311\300\334\310\306\330\307]" "[']" nil ("-C") "~list" iso-8859-1))))

(defvar ispell-dictionary-alist (append ispell-local-dictionary-alist ispell-dictionary-alist-1 ispell-dictionary-alist-2 ispell-dictionary-alist-3 ispell-dictionary-alist-4 ispell-dictionary-alist-5 ispell-dictionary-alist-6) "\
An alist of dictionaries and their associated parameters.

Each element of this list is also a list:

\(DICTIONARY-NAME CASECHARS NOT-CASECHARS OTHERCHARS MANY-OTHERCHARS-P
        ISPELL-ARGS EXTENDED-CHARACTER-MODE CHARACTER-SET)

DICTIONARY-NAME is a possible string value of variable `ispell-dictionary',
nil means the default dictionary.

CASECHARS is a regular expression of valid characters that comprise a
word.

NOT-CASECHARS is the opposite regexp of CASECHARS.

OTHERCHARS is a regexp of characters in the NOT-CASECHARS set but which can be
used to construct words in some special way.  If OTHERCHARS characters follow
and precede characters from CASECHARS, they are parsed as part of a word,
otherwise they become word-breaks.  As an example in English, assume the
regular expression \"[']\" for OTHERCHARS.  Then \"they're\" and
\"Steven's\" are parsed as single words including the \"'\" character, but
\"Stevens'\" does not include the quote character as part of the word.
If you want OTHERCHARS to be empty, use the empty string.
Hint: regexp syntax requires the hyphen to be declared first here.

MANY-OTHERCHARS-P is non-nil when multiple OTHERCHARS are allowed in a word.
Otherwise only a single OTHERCHARS character is allowed to be part of any
single word.

ISPELL-ARGS is a list of additional arguments passed to the ispell
subprocess.

EXTENDED-CHARACTER-MODE should be used when dictionaries are used which
have been configured in an Ispell affix file.  (For example, umlauts
can be encoded as \\\"a, a\\\", \"a, ...)  Defaults are ~tex and ~nroff
in English.  This has the same effect as the command-line `-T' option.
The buffer Major Mode controls Ispell's parsing in tex or nroff mode,
but the dictionary can control the extended character mode.
Both defaults can be overruled in a buffer-local fashion. See
`ispell-parsing-keyword' for details on this.

CHARACTER-SET used for languages with multibyte characters.

Note that the CASECHARS and OTHERCHARS slots of the alist should
contain the same character set as casechars and otherchars in the
LANGUAGE.aff file (e.g., english.aff).")

(defvar ispell-menu-map nil "\
Key map for ispell menu.")

(defvar ispell-menu-xemacs nil "\
Spelling menu for XEmacs.
If nil when package is loaded, a standard menu will be set,
and added as a submenu of the \"Edit\" menu.")

(defconst ispell-menu-map-needed (and (not ispell-menu-map) (not (string-match "18\\.[0-9]+\\.[0-9]+" emacs-version)) (not (string-match "Lucid\\|XEmacs" emacs-version))))

(if ispell-menu-map-needed (let ((dicts (reverse (cons (cons "default" nil) ispell-dictionary-alist))) name) (setq ispell-menu-map (make-sparse-keymap "Spell")) (while dicts (setq name (car (car dicts)) dicts (cdr dicts)) (if (stringp name) (define-key ispell-menu-map (vector (intern name)) (cons (concat "Select " (capitalize name)) (list (quote lambda) nil (quote (interactive)) (list (quote ispell-change-dictionary) name))))))))

(if ispell-menu-map-needed (progn (define-key ispell-menu-map [ispell-change-dictionary] (quote ("Change Dictionary" . ispell-change-dictionary))) (define-key ispell-menu-map [ispell-kill-ispell] (quote ("Kill Process" . ispell-kill-ispell))) (define-key ispell-menu-map [ispell-pdict-save] (quote ("Save Dictionary" lambda nil (interactive) (ispell-pdict-save t t)))) (define-key ispell-menu-map [ispell-complete-word] (quote ("Complete Word" . ispell-complete-word))) (define-key ispell-menu-map [ispell-complete-word-interior-frag] (quote ("Complete Word Frag" . ispell-complete-word-interior-frag)))))

(if ispell-menu-map-needed (progn (define-key ispell-menu-map [ispell-continue] (quote ("Continue Check" . ispell-continue))) (define-key ispell-menu-map [ispell-word] (quote ("Check Word" . ispell-word))) (define-key ispell-menu-map [ispell-comments-and-strings] (quote ("Check Comments" . ispell-comments-and-strings))) (define-key ispell-menu-map [ispell-region] (quote ("Check Region" . ispell-region))) (define-key ispell-menu-map [ispell-buffer] (quote ("Check Buffer" . ispell-buffer)))))

(if ispell-menu-map-needed (progn (define-key ispell-menu-map [ispell-message] (quote ("Check Message" . ispell-message))) (define-key ispell-menu-map [ispell-help] (quote ("Help" lambda nil (interactive) (describe-function (quote ispell-help))))) (put (quote ispell-region) (quote menu-enable) (quote mark-active)) (fset (quote ispell-menu-map) (symbol-value (quote ispell-menu-map)))))

(defvar ispell-skip-region-alist (quote ((ispell-words-keyword forward-line) (ispell-dictionary-keyword forward-line) (ispell-pdict-keyword forward-line) (ispell-parsing-keyword forward-line) ("^---*BEGIN PGP [A-Z ]*--*" . "^---*END PGP [A-Z ]*--*") ("^---* \\(Start of \\)?[Ff]orwarded [Mm]essage" . "^---* End of [Ff]orwarded [Mm]essage") ("\\(/\\|\\(\\(\\w\\|-\\)+[.:@]\\)\\)\\(\\w\\|-\\)*\\([.:/@]+\\(\\w\\|-\\|~\\)+\\)+"))) "\
Alist expressing beginning and end of regions not to spell check.
The alist key must be a regular expression.
Valid forms include:
  (KEY) - just skip the key.
  (KEY . REGEXP) - skip to the end of REGEXP.  REGEXP may be string or symbol.
  (KEY REGEXP) - skip to end of REGEXP.  REGEXP must be a string.
  (KEY FUNCTION ARGS) - FUNCTION called with ARGS returns end of region.")

(defvar ispell-tex-skip-alists (quote ((("\\\\addcontentsline" ispell-tex-arg-end 2) ("\\\\add\\(tocontents\\|vspace\\)" ispell-tex-arg-end) ("\\\\\\([aA]lph\\|arabic\\)" ispell-tex-arg-end) ("\\\\bibliographystyle" ispell-tex-arg-end) ("\\\\makebox" ispell-tex-arg-end 0) ("\\\\document\\(class\\|style\\)" . "\\\\begin[ 	\n]*{[ 	\n]*document[ 	\n]*}")) (("\\(figure\\|table\\)\\*?" ispell-tex-arg-end 0) ("list" ispell-tex-arg-end 2) ("program" . "\\\\end[ 	\n]*{[ 	\n]*program[ 	\n]*}") ("verbatim\\*?" . "\\\\end[ 	\n]*{[ 	\n]*verbatim\\*?[ 	\n]*}")))) "\
*Lists of regions to be skipped in TeX mode.
First list is used raw.
Second list has key placed inside \\begin{}.

Delete or add any regions you want to be automatically selected
for skipping in latex mode.")

(define-key esc-map "$" (quote ispell-word))

(autoload (quote ispell-word) "ispell" "\
Check spelling of word under or before the cursor.
If the word is not found in dictionary, display possible corrections
in a window allowing you to choose one.

If optional argument FOLLOWING is non-nil or if `ispell-following-word'
is non-nil when called interactively, then the following word
\(rather than preceding) is checked when the cursor is not over a word.
When the optional argument QUIETLY is non-nil or `ispell-quietly' is non-nil
when called interactively, non-corrective messages are suppressed.

With a prefix argument (or if CONTINUE is non-nil),
resume interrupted spell-checking of a buffer or region.

Word syntax described by `ispell-dictionary-alist' (which see).

This will check or reload the dictionary.  Use \\[ispell-change-dictionary]
or \\[ispell-region] to update the Ispell process." t nil)

(autoload (quote ispell-help) "ispell" "\
Display a list of the options available when a misspelling is encountered.

Selections are:

DIGIT: Replace the word with a digit offered in the *Choices* buffer.
SPC:   Accept word this time.
`i':   Accept word and insert into private dictionary.
`a':   Accept word for this session.
`A':   Accept word and place in `buffer-local dictionary'.
`r':   Replace word with typed-in value.  Rechecked.
`R':   Replace word with typed-in value. Query-replaced in buffer. Rechecked.
`?':   Show these commands.
`x':   Exit spelling buffer.  Move cursor to original point.
`X':   Exit spelling buffer.  Leaves cursor at the current point, and permits
        the aborted check to be completed later.
`q':   Quit spelling session (Kills ispell process).
`l':   Look up typed-in replacement in alternate dictionary.  Wildcards okay.
`u':   Like `i', but the word is lower-cased first.
`m':   Place typed-in value in personal dictionary, then recheck current word.
`C-l':  redraws screen
`C-r':  recursive edit
`C-z':  suspend emacs or iconify frame" nil nil)

(autoload (quote ispell-kill-ispell) "ispell" "\
Kill current Ispell process (so that you may start a fresh one).
With NO-ERROR, just return non-nil if there was no Ispell running." t nil)

(autoload (quote ispell-change-dictionary) "ispell" "\
Change `ispell-dictionary' (q.v.) to DICT and kill old Ispell process.
A new one will be started as soon as necessary.

By just answering RET you can find out what the current dictionary is.

With prefix argument, set the default directory." t nil)

(autoload (quote ispell-region) "ispell" "\
Interactively check a region for spelling errors.
Return non-nil if spell session completed normally." t nil)

(autoload (quote ispell-comments-and-strings) "ispell" "\
Check comments and strings in the current buffer for spelling errors." t nil)

(autoload (quote ispell-buffer) "ispell" "\
Check the current buffer for spelling errors interactively." t nil)

(autoload (quote ispell-continue) "ispell" "\
Continue a halted spelling session beginning with the current word." t nil)

(autoload (quote ispell-complete-word) "ispell" "\
Try to complete the word before or under point (see `lookup-words')
If optional INTERIOR-FRAG is non-nil then the word may be a character
sequence inside of a word.

Standard ispell choices are then available." t nil)

(autoload (quote ispell-complete-word-interior-frag) "ispell" "\
Completes word matching character sequence inside a word." t nil)

(autoload (quote ispell-minor-mode) "ispell" "\
Toggle Ispell minor mode.
With prefix arg, turn Ispell minor mode on iff arg is positive.
 
In Ispell minor mode, pressing SPC or RET
warns you if the previous word is incorrectly spelled.

All the buffer-local variables and dictionaries are ignored -- to read
them into the running ispell process, type \\[ispell-word] SPC." t nil)

(autoload (quote ispell-message) "ispell" "\
Check the spelling of a mail message or news post.
Don't check spelling of message headers except the Subject field.
Don't check included messages.

To abort spell checking of a message region and send the message anyway,
use the `x' command.  (Any subsequent regions will be checked.)
The `X' command aborts the message send so that you can edit the buffer.

To spell-check whenever a message is sent, include the appropriate lines
in your .emacs file:
   (add-hook 'message-send-hook 'ispell-message)  ;; GNUS 5
   (add-hook 'news-inews-hook 'ispell-message)    ;; GNUS 4
   (add-hook 'mail-send-hook  'ispell-message)
   (add-hook 'mh-before-send-letter-hook 'ispell-message)

You can bind this to the key C-c i in GNUS or mail by adding to
`news-reply-mode-hook' or `mail-mode-hook' the following lambda expression:
   (function (lambda () (local-set-key \"\\C-ci\" 'ispell-message)))" t nil)

;;;***

;;;### (autoloads (iswitchb-buffer-other-frame iswitchb-display-buffer
;;;;;;  iswitchb-buffer-other-window iswitchb-buffer iswitchb-default-keybindings
;;;;;;  iswitchb-read-buffer) "iswitchb" "iswitchb.el" (14169 3596))
;;; Generated autoloads from iswitchb.el

(autoload (quote iswitchb-read-buffer) "iswitchb" "\
Replacement for the built-in `read-buffer'.
Return the name of a buffer selected.  
PROMPT is the prompt to give to the user.  DEFAULT if given is the default
buffer to be selected, which will go to the front of the list.
If REQUIRE-MATCH is non-nil, an existing-buffer must be selected." nil nil)

(autoload (quote iswitchb-default-keybindings) "iswitchb" "\
Set up default keybindings for `iswitchb-buffer'.
Call this function to override the normal bindings.  This function also
adds a hook to the minibuffer." t nil)

(autoload (quote iswitchb-buffer) "iswitchb" "\
Switch to another buffer.

The buffer name is selected interactively by typing a substring.  The
buffer is displayed according to `iswitchb-default-method' -- the
default is to show it in the same window, unless it is already visible
in another frame.
For details of keybindings, do `\\[describe-function] iswitchb'." t nil)

(autoload (quote iswitchb-buffer-other-window) "iswitchb" "\
Switch to another buffer and show it in another window.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] iswitchb'." t nil)

(autoload (quote iswitchb-display-buffer) "iswitchb" "\
Display a buffer in another window but don't select it.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] iswitchb'." t nil)

(autoload (quote iswitchb-buffer-other-frame) "iswitchb" "\
Switch to another buffer and show it in another frame.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] iswitchb'." t nil)

;;;***

;;;### (autoloads (read-hiragana-string japanese-zenkaku-region japanese-hankaku-region
;;;;;;  japanese-hiragana-region japanese-katakana-region japanese-zenkaku
;;;;;;  japanese-hankaku japanese-hiragana japanese-katakana setup-japanese-environment-internal
;;;;;;  setup-japanese-environment) "japan-util" "language/japan-util.el"
;;;;;;  (13903 50605))
;;; Generated autoloads from language/japan-util.el

(autoload (quote setup-japanese-environment) "japan-util" "\
Setup multilingual environment (MULE) for Japanese." t nil)

(autoload (quote setup-japanese-environment-internal) "japan-util" nil nil nil)

(autoload (quote japanese-katakana) "japan-util" "\
Convert argument to Katakana and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.
Optional argument HANKAKU t means to convert to `hankaku' Katakana
 (`japanese-jisx0201-kana'), in which case return value
 may be a string even if OBJ is a character if two Katakanas are
 necessary to represent OBJ." nil nil)

(autoload (quote japanese-hiragana) "japan-util" "\
Convert argument to Hiragana and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy." nil nil)

(autoload (quote japanese-hankaku) "japan-util" "\
Convert argument to `hankaku' and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.
Optional argument ASCII-ONLY non-nil means to return only ASCII character." nil nil)

(autoload (quote japanese-zenkaku) "japan-util" "\
Convert argument to `zenkaku' and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy." nil nil)

(autoload (quote japanese-katakana-region) "japan-util" "\
Convert Japanese `hiragana' chars in the region to `katakana' chars.
Optional argument HANKAKU t means to convert to `hankaku katakana' character
of which charset is `japanese-jisx0201-kana'." t nil)

(autoload (quote japanese-hiragana-region) "japan-util" "\
Convert Japanese `katakana' chars in the region to `hiragana' chars." t nil)

(autoload (quote japanese-hankaku-region) "japan-util" "\
Convert Japanese `zenkaku' chars in the region to `hankaku' chars.
`Zenkaku' chars belong to `japanese-jisx0208'
`Hankaku' chars belong to `ascii' or `japanese-jisx0201-kana'.
Optional argument ASCII-ONLY non-nil means to convert only to ASCII char." t nil)

(autoload (quote japanese-zenkaku-region) "japan-util" "\
Convert hankaku' chars in the region to Japanese `zenkaku' chars.
`Zenkaku' chars belong to `japanese-jisx0208'
`Hankaku' chars belong to `ascii' or `japanese-jisx0201-kana'.
Optional argument KATAKANA-ONLY non-nil means to convert only KATAKANA char." t nil)

(autoload (quote read-hiragana-string) "japan-util" "\
Read a Hiragana string from the minibuffer, prompting with string PROMPT.
If non-nil, second arg INITIAL-INPUT is a string to insert before reading." nil nil)

;;;***

;;;### (autoloads (turn-on-jit-lock jit-lock-mode) "jit-lock" "jit-lock.el"
;;;;;;  (14228 23899))
;;; Generated autoloads from jit-lock.el

(autoload (quote jit-lock-mode) "jit-lock" "\
Toggle Just-in-time Lock mode.
With arg, turn Just-in-time Lock mode on if and only if arg is positive.
Enable it automatically by customizing group `font-lock'.

When Just-in-time Lock mode is enabled, fontification is different in the
following ways:

- Demand-driven buffer fontification triggered by Emacs C code.
  This means initial fontification of the whole buffer does not occur.
  Instead, fontification occurs when necessary, such as when scrolling
  through the buffer would otherwise reveal unfontified areas.  This is
  useful if buffer fontification is too slow for large buffers.

- Stealthy buffer fontification if `jit-lock-stealth-time' is non-nil.
  This means remaining unfontified areas of buffers are fontified if Emacs has
  been idle for `jit-lock-stealth-time' seconds, while Emacs remains idle.
  This is useful if any buffer has any deferred fontification.

- Deferred context fontification if `jit-lock-defer-contextually' is
  non-nil.  This means fontification updates the buffer corresponding to
  true syntactic context, after `jit-lock-stealth-time' seconds of Emacs
  idle time, while Emacs remains idle.  Otherwise, fontification occurs
  on modified lines only, and subsequent lines can remain fontified
  corresponding to previous syntactic contexts.  This is useful where
  strings or comments span lines.

Stealth fontification only occurs while the system remains unloaded.
If the system load rises above `jit-lock-stealth-load' percent, stealth
fontification is suspended.  Stealth fontification intensity is controlled via
the variable `jit-lock-stealth-nice' and `jit-lock-stealth-lines'." t nil)

(autoload (quote turn-on-jit-lock) "jit-lock" "\
Unconditionally turn on Just-in-time Lock mode." nil nil)

;;;***

;;;### (autoloads nil "jka-compr" "jka-compr.el" (14097 45967))
;;; Generated autoloads from jka-compr.el
(defun auto-compression-mode (&optional arg)
 "\
Toggle automatic file compression and uncompression.
With prefix argument ARG, turn auto compression on if positive, else off.
Returns the new status of auto compression (non-nil means on)."
 (interactive "P")
 (if (not (fboundp 'jka-compr-installed-p))
     (progn
       (require 'jka-compr)
       ;; That turned the mode on, so make it initially off.
       (toggle-auto-compression)))
 (toggle-auto-compression arg t))

;;;***

;;;### (autoloads (kinsoku) "kinsoku" "international/kinsoku.el"
;;;;;;  (13866 35246))
;;; Generated autoloads from international/kinsoku.el

(autoload (quote kinsoku) "kinsoku" "\
Go to a line breaking position near point by doing `kinsoku' processing.
LINEBEG is a buffer position we can't break a line before.

`Kinsoku' processing is to prohibit specific characters to be placed
at beginning of line or at end of line.  Characters not to be placed
at beginning and end of line have character category `>' and `<'
respectively.  This restriction is dissolved by making a line longer or
shorter.

`Kinsoku' is a Japanese word which originally means ordering to stay
in one place, and is used for the text processing described above in
the context of text formatting." nil nil)

;;;***

;;;### (autoloads (kkc-region) "kkc" "international/kkc.el" (13810
;;;;;;  39847))
;;; Generated autoloads from international/kkc.el

(autoload (quote kkc-region) "kkc" "\
Convert Kana string in the current region to Kanji-Kana mixed string.
Users can select a desirable conversion interactively.
When called from a program, expects two arguments,
positions FROM and TO (integers or markers) specifying the target region.
When it returns, the point is at the tail of the selected conversion,
and the return value is the length of the conversion." t nil)

;;;***

;;;### (autoloads (setup-korean-environment-internal setup-korean-environment)
;;;;;;  "korea-util" "language/korea-util.el" (14135 45477))
;;; Generated autoloads from language/korea-util.el

(defvar default-korean-keyboard "" "\
*The kind of Korean keyboard for Korean input method.
\"\" for 2, \"3\" for 3.")

(autoload (quote setup-korean-environment) "korea-util" "\
Setup multilingual environment (MULE) for Korean." t nil)

(autoload (quote setup-korean-environment-internal) "korea-util" nil nil nil)

;;;***

;;;### (autoloads (lm lm-test-run) "landmark" "play/landmark.el"
;;;;;;  (13850 39344))
;;; Generated autoloads from play/landmark.el

(defalias (quote landmark-repeat) (quote lm-test-run))

(autoload (quote lm-test-run) "landmark" "\
Run 100 Lm games, each time saving the weights from the previous game." t nil)

(defalias (quote landmark) (quote lm))

(autoload (quote lm) "landmark" "\
Start or resume an Lm game.
If a game is in progress, this command allows you to resume it.
Here is the relation between prefix args and game options:

prefix arg | robot is auto-started | weights are saved from last game
---------------------------------------------------------------------
none / 1   | yes                   | no
       2   | yes                   | yes
       3   | no                    | yes
       4   | no                    | no

You start by moving to a square and typing \\[lm-start-robot],
if you did not use a prefix arg to ask for automatic start.
Use \\[describe-mode] for more info." t nil)

;;;***

;;;### (autoloads (setup-lao-environment) "lao-util" "language/lao-util.el"
;;;;;;  (13774 36678))
;;; Generated autoloads from language/lao-util.el

(autoload (quote setup-lao-environment) "lao-util" "\
Setup multilingual environment (MULE) for Lao." t nil)

;;;***

;;;### (autoloads (turn-on-lazy-lock lazy-lock-mode) "lazy-lock"
;;;;;;  "lazy-lock.el" (13971 14325))
;;; Generated autoloads from lazy-lock.el

(autoload (quote lazy-lock-mode) "lazy-lock" "\
Toggle Lazy Lock mode.
With arg, turn Lazy Lock mode on if and only if arg is positive.  Enable it
automatically in your `~/.emacs' by:

 (setq font-lock-support-mode 'lazy-lock-mode)

When Lazy Lock mode is enabled, fontification can be lazy in a number of ways:

- Demand-driven buffer fontification if `lazy-lock-minimum-size' is non-nil.
  This means initial fontification does not occur if the buffer is greater than
  `lazy-lock-minimum-size' characters in length.  Instead, fontification occurs
  when necessary, such as when scrolling through the buffer would otherwise
  reveal unfontified areas.  This is useful if buffer fontification is too slow
  for large buffers.

- Deferred scroll fontification if `lazy-lock-defer-on-scrolling' is non-nil.
  This means demand-driven fontification does not occur as you scroll.
  Instead, fontification is deferred until after `lazy-lock-defer-time' seconds
  of Emacs idle time, while Emacs remains idle.  This is useful if
  fontification is too slow to keep up with scrolling.

- Deferred on-the-fly fontification if `lazy-lock-defer-on-the-fly' is non-nil.
  This means on-the-fly fontification does not occur as you type.  Instead,
  fontification is deferred until after `lazy-lock-defer-time' seconds of Emacs
  idle time, while Emacs remains idle.  This is useful if fontification is too
  slow to keep up with your typing.

- Deferred context fontification if `lazy-lock-defer-contextually' is non-nil.
  This means fontification updates the buffer corresponding to true syntactic
  context, after `lazy-lock-defer-time' seconds of Emacs idle time, while Emacs
  remains idle.  Otherwise, fontification occurs on modified lines only, and
  subsequent lines can remain fontified corresponding to previous syntactic
  contexts.  This is useful where strings or comments span lines.

- Stealthy buffer fontification if `lazy-lock-stealth-time' is non-nil.
  This means remaining unfontified areas of buffers are fontified if Emacs has
  been idle for `lazy-lock-stealth-time' seconds, while Emacs remains idle.
  This is useful if any buffer has any deferred fontification.

Basic Font Lock mode on-the-fly fontification behaviour fontifies modified
lines only.  Thus, if `lazy-lock-defer-contextually' is non-nil, Lazy Lock mode
on-the-fly fontification may fontify differently, albeit correctly.  In any
event, to refontify some lines you can use \\[font-lock-fontify-block].

Stealth fontification only occurs while the system remains unloaded.
If the system load rises above `lazy-lock-stealth-load' percent, stealth
fontification is suspended.  Stealth fontification intensity is controlled via
the variable `lazy-lock-stealth-nice' and `lazy-lock-stealth-lines', and
verbosity is controlled via the variable `lazy-lock-stealth-verbose'." t nil)

(autoload (quote turn-on-lazy-lock) "lazy-lock" "\
Unconditionally turn on Lazy Lock mode." nil nil)

;;;***

;;;### (autoloads (ledit-from-lisp-mode ledit-mode) "ledit" "ledit.el"
;;;;;;  (13229 28763))
;;; Generated autoloads from ledit.el

(defconst ledit-save-files t "\
*Non-nil means Ledit should save files before transferring to Lisp.")

(defconst ledit-go-to-lisp-string "%?lisp" "\
*Shell commands to execute to resume Lisp job.")

(defconst ledit-go-to-liszt-string "%?liszt" "\
*Shell commands to execute to resume Lisp compiler job.")

(autoload (quote ledit-mode) "ledit" "\
\\<ledit-mode-map>Major mode for editing text and stuffing it to a Lisp job.
Like Lisp mode, plus these special commands:
  \\[ledit-save-defun]	-- record defun at or after point
	   for later transmission to Lisp job.
  \\[ledit-save-region] -- record region for later transmission to Lisp job.
  \\[ledit-go-to-lisp] -- transfer to Lisp job and transmit saved text.
  \\[ledit-go-to-liszt] -- transfer to Liszt (Lisp compiler) job
	   and transmit saved text.
\\{ledit-mode-map}
To make Lisp mode automatically change to Ledit mode,
do (setq lisp-mode-hook 'ledit-from-lisp-mode)" t nil)

(autoload (quote ledit-from-lisp-mode) "ledit" nil nil nil)

;;;***

;;;### (autoloads (life) "life" "play/life.el" (13578 2952))
;;; Generated autoloads from play/life.el

(autoload (quote life) "life" "\
Run Conway's Life simulation.
The starting pattern is randomly selected.  Prefix arg (optional first
arg non-nil from a program) is the number of seconds to sleep between
generations (this defaults to 1)." t nil)

;;;***

;;;### (autoloads (unload-feature) "loadhist" "loadhist.el" (13935
;;;;;;  16173))
;;; Generated autoloads from loadhist.el

(autoload (quote unload-feature) "loadhist" "\
Unload the library that provided FEATURE, restoring all its autoloads.
If the feature is required by any other loaded code, and optional FORCE
is nil, raise an error." t nil)

;;;***

;;;### (autoloads (locate-with-filter locate) "locate" "locate.el"
;;;;;;  (13685 31779))
;;; Generated autoloads from locate.el

(autoload (quote locate) "locate" "\
Run the program `locate', putting results in `*Locate*' buffer." t nil)

(autoload (quote locate-with-filter) "locate" "\
Run the locate command with a filter." t nil)

;;;***

;;;### (autoloads (print-region lpr-region print-buffer lpr-buffer
;;;;;;  lpr-command lpr-switches printer-name) "lpr" "lpr.el" (14149
;;;;;;  56563))
;;; Generated autoloads from lpr.el

(defvar printer-name (if (memq system-type (quote (ms-dos windows-nt))) "PRN") "\
*The name of a local printer to which data is sent for printing.
\(Note that PostScript files are sent to `ps-printer-name', which see.)

On Unix-like systems, a string value should be a name understood by
lpr's -P option; otherwise the value should be nil.

On MS-DOS and MS-Windows systems, a string value is taken as the name of
a printer device or port, provided `lpr-command' is set to \"\".
Typical non-default settings would be \"LPT1\" to \"LPT3\" for parallel
printers, or \"COM1\" to \"COM4\" or \"AUX\" for serial printers, or
\"//hostname/printer\" for a shared network printer.  You can also set
it to the name of a file, in which case the output gets appended to that
file.  If you want to discard the printed output, set this to \"NUL\".")

(defvar lpr-switches nil "\
*List of strings to pass as extra options for the printer program.
It is recommended to set `printer-name' instead of including an explicit
switch on this list.
See `lpr-command'.")

(defvar lpr-command (cond ((memq system-type (quote (ms-dos windows-nt))) "") ((memq system-type (quote (usg-unix-v dgux hpux irix))) "lp") (t "lpr")) "\
*Name of program for printing a file.

On MS-DOS and MS-Windows systems, if the value is an empty string then
Emacs will write directly to the printer port named by `printer-name'.
The programs `print' and `nprint' (the standard print programs on
Windows NT and Novell Netware respectively) are handled specially, using
`printer-name' as the destination for output; any other program is
treated like `lpr' except that an explicit filename is given as the last
argument.")

(autoload (quote lpr-buffer) "lpr" "\
Print buffer contents as with Unix command `lpr'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr." t nil)

(autoload (quote print-buffer) "lpr" "\
Print buffer contents as with Unix command `lpr -p'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr." t nil)

(autoload (quote lpr-region) "lpr" "\
Print region contents as with Unix command `lpr'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr." t nil)

(autoload (quote print-region) "lpr" "\
Print region contents as with Unix command `lpr -p'.
`lpr-switches' is a list of extra switches (strings) to pass to lpr." t nil)

;;;***

;;;### (autoloads nil "ls-lisp" "ls-lisp.el" (14184 170))
;;; Generated autoloads from ls-lisp.el

(defvar ls-lisp-support-shell-wildcards t "\
*Non-nil means file patterns are treated as shell wildcards.
nil means they are treated as Emacs regexps (for backward compatibility).
This variable is checked by \\[insert-directory] only when `ls-lisp.el'
package is used.")

;;;***

;;;### (autoloads (phases-of-moon) "lunar" "calendar/lunar.el" (13462
;;;;;;  53924))
;;; Generated autoloads from calendar/lunar.el

(autoload (quote phases-of-moon) "lunar" "\
Display the quarters of the moon for last month, this month, and next month.
If called with an optional prefix argument, prompts for month and year.

This function is suitable for execution in a .emacs file." t nil)

;;;***

;;;### (autoloads (m4-mode) "m4-mode" "progmodes/m4-mode.el" (13962
;;;;;;  30563))
;;; Generated autoloads from progmodes/m4-mode.el

(autoload (quote m4-mode) "m4-mode" "\
A major mode to edit m4 macro files.
\\{m4-mode-map}
" t nil)

;;;***

;;;### (autoloads (apply-macro-to-region-lines kbd-macro-query insert-kbd-macro
;;;;;;  name-last-kbd-macro) "macros" "macros.el" (13229 28843))
;;; Generated autoloads from macros.el

(autoload (quote name-last-kbd-macro) "macros" "\
Assign a name to the last keyboard macro defined.
Argument SYMBOL is the name to define.
The symbol's function definition becomes the keyboard macro string.
Such a \"function\" cannot be called from Lisp, but it is a valid editor command." t nil)

(autoload (quote insert-kbd-macro) "macros" "\
Insert in buffer the definition of kbd macro NAME, as Lisp code.
Optional second arg KEYS means also record the keys it is on
\(this is the prefix argument, when calling interactively).

This Lisp code will, when executed, define the kbd macro with the same
definition it has now.  If you say to record the keys, the Lisp code
will also rebind those keys to the macro.  Only global key bindings
are recorded since executing this Lisp code always makes global
bindings.

To save a kbd macro, visit a file of Lisp code such as your `~/.emacs',
use this command, and then save the file." t nil)

(autoload (quote kbd-macro-query) "macros" "\
Query user during kbd macro execution.
  With prefix argument, enters recursive edit, reading keyboard
commands even within a kbd macro.  You can give different commands
each time the macro executes.
  Without prefix argument, asks whether to continue running the macro.
Your options are: \\<query-replace-map>
\\[act]	Finish this iteration normally and continue with the next.
\\[skip]	Skip the rest of this iteration, and start the next.
\\[exit]	Stop the macro entirely right now.
\\[recenter]	Redisplay the screen, then ask again.
\\[edit]	Enter recursive edit; ask again when you exit from that." t nil)

(autoload (quote apply-macro-to-region-lines) "macros" "\
For each complete line between point and mark, move to the beginning
of the line, and run the last keyboard macro.

When called from lisp, this function takes two arguments TOP and
BOTTOM, describing the current region.  TOP must be before BOTTOM.
The optional third argument MACRO specifies a keyboard macro to
execute.

This is useful for quoting or unquoting included text, adding and
removing comments, or producing tables where the entries are regular.

For example, in Usenet articles, sections of text quoted from another
author are indented, or have each line start with `>'.  To quote a
section of text, define a keyboard macro which inserts `>', put point
and mark at opposite ends of the quoted section, and use
`\\[apply-macro-to-region-lines]' to mark the entire section.

Suppose you wanted to build a keyword table in C where each entry
looked like this:

    { \"foo\", foo_data, foo_function }, 
    { \"bar\", bar_data, bar_function },
    { \"baz\", baz_data, baz_function },

You could enter the names in this format:

    foo
    bar
    baz

and write a macro to massage a word into a table entry:

    \\C-x (
       \\M-d { \"\\C-y\", \\C-y_data, \\C-y_function },
    \\C-x )

and then select the region of un-tablified names and use
`\\[apply-macro-to-region-lines]' to build the table from the names.
" t nil)
 (define-key ctl-x-map "q" 'kbd-macro-query)

;;;***

;;;### (autoloads (what-domain mail-extract-address-components) "mail-extr"
;;;;;;  "mail/mail-extr.el" (13616 25279))
;;; Generated autoloads from mail/mail-extr.el

(autoload (quote mail-extract-address-components) "mail-extr" "\
Given an RFC-822 address ADDRESS, extract full name and canonical address.
Returns a list of the form (FULL-NAME CANONICAL-ADDRESS).
If no name can be extracted, FULL-NAME will be nil.

If the optional argument ALL is non-nil, then ADDRESS can contain zero
or more recipients, separated by commas, and we return a list of
the form ((FULL-NAME CANONICAL-ADDRESS) ...) with one element for
each recipient.  If ALL is nil, then if ADDRESS contains more than
one recipients, all but the first is ignored.

ADDRESS may be a string or a buffer.  If it is a buffer, the visible 
 (narrowed) portion of the buffer will be interpreted as the address.
 (This feature exists so that the clever caller might be able to avoid
 consing a string.)" nil nil)

(autoload (quote what-domain) "mail-extr" "\
Convert mail domain DOMAIN to the country it corresponds to." t nil)

;;;***

;;;### (autoloads (mail-hist-put-headers-into-history mail-hist-keep-history
;;;;;;  mail-hist-enable mail-hist-define-keys) "mail-hist" "mail/mail-hist.el"
;;;;;;  (14075 51559))
;;; Generated autoloads from mail/mail-hist.el

(autoload (quote mail-hist-define-keys) "mail-hist" "\
Define keys for accessing mail header history.  For use in hooks." nil nil)

(autoload (quote mail-hist-enable) "mail-hist" nil nil nil)

(defvar mail-hist-keep-history t "\
*Non-nil means keep a history for headers and text of outgoing mail.")

(autoload (quote mail-hist-put-headers-into-history) "mail-hist" "\
Put headers and contents of this message into mail header history. 
Each header has its own independent history, as does the body of the
message.

This function normally would be called when the message is sent." nil nil)

;;;***

;;;### (autoloads (mail-fetch-field mail-file-babyl-p mail-use-rfc822)
;;;;;;  "mail-utils" "mail/mail-utils.el" (13772 47253))
;;; Generated autoloads from mail/mail-utils.el

(defvar mail-use-rfc822 nil "\
*If non-nil, use a full, hairy RFC822 parser on mail addresses.
Otherwise, (the default) use a smaller, somewhat faster, and
often correct parser.")

(autoload (quote mail-file-babyl-p) "mail-utils" nil nil nil)

(autoload (quote mail-fetch-field) "mail-utils" "\
Return the value of the header field whose type is FIELD-NAME.
The buffer is expected to be narrowed to just the header of the message.
If second arg LAST is non-nil, use the last field of type FIELD-NAME.
If third arg ALL is non-nil, concatenate all such fields with commas between.
If 4th arg LIST is non-nil, return a list of all such fields." nil nil)

;;;***

;;;### (autoloads (define-mail-abbrev build-mail-abbrevs mail-abbrevs-setup)
;;;;;;  "mailabbrev" "mail/mailabbrev.el" (13640 6043))
;;; Generated autoloads from mail/mailabbrev.el

(autoload (quote mail-abbrevs-setup) "mailabbrev" "\
Initialize use of the `mailabbrev' package." nil nil)

(autoload (quote build-mail-abbrevs) "mailabbrev" "\
Read mail aliases from personal mail alias file and set `mail-abbrevs'.
By default this is the file specified by `mail-personal-alias-file'." nil nil)

(autoload (quote define-mail-abbrev) "mailabbrev" "\
Define NAME as a mail alias abbrev that translates to DEFINITION.
If DEFINITION contains multiple addresses, separate them with commas." t nil)

;;;***

;;;### (autoloads (mail-complete define-mail-alias expand-mail-aliases
;;;;;;  mail-complete-style) "mailalias" "mail/mailalias.el" (13996
;;;;;;  15767))
;;; Generated autoloads from mail/mailalias.el

(defvar mail-complete-style (quote angles) "\
*Specifies how \\[mail-complete] formats the full name when it completes.
If `nil', they contain just the return address like:
	king@grassland.com
If `parens', they look like:
	king@grassland.com (Elvis Parsley)
If `angles', they look like:
	Elvis Parsley <king@grassland.com>")

(autoload (quote expand-mail-aliases) "mailalias" "\
Expand all mail aliases in suitable header fields found between BEG and END.
If interactive, expand in header fields.
Suitable header fields are `To', `From', `CC' and `BCC', `Reply-to', and
their `Resent-' variants.

Optional second arg EXCLUDE may be a regular expression defining text to be
removed from alias expansions." t nil)

(autoload (quote define-mail-alias) "mailalias" "\
Define NAME as a mail alias that translates to DEFINITION.
This means that sending a message to NAME will actually send to DEFINITION.

Normally, the addresses in DEFINITION must be separated by commas.
If FROM-MAILRC-FILE is non-nil, then addresses in DEFINITION 
can be separated by spaces; an address can contain spaces
if it is quoted with double-quotes." t nil)

(autoload (quote mail-complete) "mailalias" "\
Perform completion on header field or word preceding point.
Completable headers are according to `mail-complete-alist'.  If none matches
current header, calls `mail-complete-function' and passes prefix arg if any." t nil)

;;;***

;;;### (autoloads (makefile-mode) "make-mode" "progmodes/make-mode.el"
;;;;;;  (14013 4126))
;;; Generated autoloads from progmodes/make-mode.el

(autoload (quote makefile-mode) "make-mode" "\
Major mode for editing Makefiles.
This function ends by invoking the function(s) `makefile-mode-hook'.

\\{makefile-mode-map}

In the browser, use the following keys:

\\{makefile-browser-map}

Makefile mode can be configured by modifying the following variables:

makefile-browser-buffer-name:
    Name of the macro- and target browser buffer.

makefile-target-colon:
    The string that gets appended to all target names
    inserted by `makefile-insert-target'.
    \":\" or \"::\" are quite common values.

makefile-macro-assign:
   The string that gets appended to all macro names
   inserted by `makefile-insert-macro'.
   The normal value should be \" = \", since this is what
   standard make expects. However, newer makes such as dmake
   allow a larger variety of different macro assignments, so you
   might prefer to use \" += \" or \" := \" .

makefile-tab-after-target-colon:
   If you want a TAB (instead of a space) to be appended after the
   target colon, then set this to a non-nil value.

makefile-browser-leftmost-column:
   Number of blanks to the left of the browser selection mark.

makefile-browser-cursor-column:
   Column in which the cursor is positioned when it moves
   up or down in the browser.

makefile-browser-selected-mark:
   String used to mark selected entries in the browser.

makefile-browser-unselected-mark:
   String used to mark unselected entries in the browser.

makefile-browser-auto-advance-after-selection-p:
   If this variable is set to a non-nil value the cursor
   will automagically advance to the next line after an item
   has been selected in the browser.

makefile-pickup-everything-picks-up-filenames-p:
   If this variable is set to a non-nil value then
   `makefile-pickup-everything' also picks up filenames as targets
   (i.e. it calls `makefile-pickup-filenames-as-targets'), otherwise
   filenames are omitted.

makefile-cleanup-continuations-p:
   If this variable is set to a non-nil value then makefile-mode
   will assure that no line in the file ends with a backslash
   (the continuation character) followed by any whitespace.
   This is done by silently removing the trailing whitespace, leaving
   the backslash itself intact.
   IMPORTANT: Please note that enabling this option causes makefile-mode
   to MODIFY A FILE WITHOUT YOUR CONFIRMATION when \"it seems necessary\".

makefile-browser-hook:
   A function or list of functions to be called just before the
   browser is entered. This is executed in the makefile buffer.

makefile-special-targets-list:
   List of special targets. You will be offered to complete
   on one of those in the minibuffer whenever you enter a `.'.
   at the beginning of a line in Makefile mode." t nil)

;;;***

;;;### (autoloads (make-command-summary) "makesum" "makesum.el" (13229
;;;;;;  28914))
;;; Generated autoloads from makesum.el

(autoload (quote make-command-summary) "makesum" "\
Make a summary of current key bindings in the buffer *Summary*.
Previous contents of that buffer are killed first." t nil)

;;;***

;;;### (autoloads (man-follow man) "man" "man.el" (14203 50437))
;;; Generated autoloads from man.el

(defalias (quote manual-entry) (quote man))

(autoload (quote man) "man" "\
Get a Un*x manual page and put it in a buffer.
This command is the top-level command in the man package.  It runs a Un*x
command to retrieve and clean a manpage in the background and places the
results in a Man mode (manpage browsing) buffer.  See variable
`Man-notify-method' for what happens when the buffer is ready.
If a buffer already exists for this man page, it will display immediately." t nil)

(autoload (quote man-follow) "man" "\
Get a Un*x manual page of the item under point and put it in a buffer." t nil)

;;;***

;;;### (autoloads (unbold-region bold-region message-news-other-frame
;;;;;;  message-news-other-window message-mail-other-frame message-mail-other-window
;;;;;;  message-bounce message-resend message-forward message-recover
;;;;;;  message-supersede message-cancel-news message-followup message-wide-reply
;;;;;;  message-reply message-news message-mail message-mode message-signature-file
;;;;;;  message-signature message-indent-citation-function message-cite-function
;;;;;;  message-yank-prefix message-citation-line-function message-send-mail-function
;;;;;;  message-user-organization-file message-signature-separator
;;;;;;  message-from-style) "message" "gnus/message.el" (14030 49476))
;;; Generated autoloads from gnus/message.el

(defvar message-from-style (quote default) "\
*Specifies how \"From\" headers look.

If `nil', they contain just the return address like:
	king@grassland.com
If `parens', they look like:
	king@grassland.com (Elvis Parsley)
If `angles', they look like:
	Elvis Parsley <king@grassland.com>

Otherwise, most addresses look like `angles', but they look like
`parens' if `angles' would need quoting and `parens' would not.")

(defvar message-signature-separator "^-- *$" "\
Regexp matching the signature separator.")

(defvar message-user-organization-file "/usr/lib/news/organization" "\
*Local news organization file.")

(defvar message-send-mail-function (quote message-send-mail-with-sendmail) "\
Function to call to send the current buffer as mail.
The headers should be delimited by a line whose contents match the
variable `mail-header-separator'.

Legal values include `message-send-mail-with-sendmail' (the default),
`message-send-mail-with-mh', `message-send-mail-with-qmail' and
`smtpmail-send-it'.")

(defvar message-citation-line-function (quote message-insert-citation-line) "\
*Function called to insert the \"Whomever writes:\" line.")

(defvar message-yank-prefix "> " "\
*Prefix inserted on the lines of yanked messages.
nil means use indentation.")

(defvar message-cite-function (quote message-cite-original) "\
*Function for citing an original message.
Predefined functions include `message-cite-original' and
`message-cite-original-without-signature'.
Note that `message-cite-original' uses `mail-citation-hook' if that is non-nil.")

(defvar message-indent-citation-function (quote message-indent-citation) "\
*Function for modifying a citation just inserted in the mail buffer.
This can also be a list of functions.  Each function can find the
citation between (point) and (mark t).  And each function should leave
point and mark around the citation text as modified.")

(defvar message-signature t "\
*String to be inserted at the end of the message buffer.
If t, the `message-signature-file' file will be inserted instead.
If a function, the result from the function will be used instead.
If a form, the result from the form will be used instead.")

(defvar message-signature-file "~/.signature" "\
*File containing the text inserted at end of message buffer.")

(condition-case nil (define-mail-user-agent (quote message-user-agent) (quote message-mail) (quote message-send-and-exit) (quote message-kill-buffer) (quote message-send-hook)) (error nil))

(autoload (quote message-mode) "message" "\
Major mode for editing mail and news to be sent.
Like Text Mode but with these additional commands:
C-c C-s  message-send (send the message)    C-c C-c  message-send-and-exit
C-c C-f  move to a header field (and create it if there isn't):
	 C-c C-f C-t  move to To	C-c C-f C-s  move to Subject
	 C-c C-f C-c  move to Cc	C-c C-f C-b  move to Bcc
	 C-c C-f C-w  move to Fcc	C-c C-f C-r  move to Reply-To
	 C-c C-f C-u  move to Summary	C-c C-f C-n  move to Newsgroups
	 C-c C-f C-k  move to Keywords	C-c C-f C-d  move to Distribution
	 C-c C-f C-f  move to Followup-To
C-c C-t  message-insert-to (add a To header to a news followup)
C-c C-n  message-insert-newsgroups (add a Newsgroup header to a news reply)
C-c C-b  message-goto-body (move to beginning of message text).
C-c C-i  message-goto-signature (move to the beginning of the signature).
C-c C-w  message-insert-signature (insert `message-signature-file' file).
C-c C-y  message-yank-original (insert current message, if any).
C-c C-q  message-fill-yanked-message (fill what was yanked).
C-c C-e  message-elide-region (elide the text between point and mark).
C-c C-z  message-kill-to-signature (kill the text up to the signature).
C-c C-r  message-caesar-buffer-body (rot13 the message body)." t nil)

(autoload (quote message-mail) "message" "\
Start editing a mail message to be sent.
OTHER-HEADERS is an alist of header/value pairs." t nil)

(autoload (quote message-news) "message" "\
Start editing a news article to be sent." t nil)

(autoload (quote message-reply) "message" "\
Start editing a reply to the article in the current buffer." t nil)

(autoload (quote message-wide-reply) "message" "\
Make a \"wide\" reply to the message in the current buffer." t nil)

(autoload (quote message-followup) "message" "\
Follow up to the message in the current buffer.
If TO-NEWSGROUPS, use that as the new Newsgroups line." t nil)

(autoload (quote message-cancel-news) "message" "\
Cancel an article you posted." t nil)

(autoload (quote message-supersede) "message" "\
Start composing a message to supersede the current message.
This is done simply by taking the old article and adding a Supersedes
header line with the old Message-ID." t nil)

(autoload (quote message-recover) "message" "\
Reread contents of current buffer from its last auto-save file." t nil)

(autoload (quote message-forward) "message" "\
Forward the current message via mail.
Optional NEWS will use news to forward instead of mail." t nil)

(autoload (quote message-resend) "message" "\
Resend the current article to ADDRESS." t nil)

(autoload (quote message-bounce) "message" "\
Re-mail the current message.
This only makes sense if the current message is a bounce message than
contains some mail you have written which has been bounced back to
you." t nil)

(autoload (quote message-mail-other-window) "message" "\
Like `message-mail' command, but display mail buffer in another window." t nil)

(autoload (quote message-mail-other-frame) "message" "\
Like `message-mail' command, but display mail buffer in another frame." t nil)

(autoload (quote message-news-other-window) "message" "\
Start editing a news article to be sent." t nil)

(autoload (quote message-news-other-frame) "message" "\
Start editing a news article to be sent." t nil)

(autoload (quote bold-region) "message" "\
Bold all nonblank characters in the region.
Works by overstriking characters.
Called from program, takes two arguments START and END
which specify the range to operate on." t nil)

(autoload (quote unbold-region) "message" "\
Remove all boldness (overstruck characters) in the region.
Called from program, takes two arguments START and END
which specify the range to operate on." t nil)

;;;***

;;;### (autoloads (metapost-mode metafont-mode) "meta-mode" "progmodes/meta-mode.el"
;;;;;;  (13549 38921))
;;; Generated autoloads from progmodes/meta-mode.el

(autoload (quote metafont-mode) "meta-mode" "\
Major mode for editing Metafont sources.
Special commands:
\\{meta-mode-map}

Turning on Metafont mode calls the value of the variables
`meta-common-mode-hook' and `metafont-mode-hook'." t nil)

(autoload (quote metapost-mode) "meta-mode" "\
Major mode for editing MetaPost sources.
Special commands:
\\{meta-mode-map}

Turning on MetaPost mode calls the value of the variable
`meta-common-mode-hook' and `metafont-mode-hook'." t nil)

;;;***

;;;### (autoloads (metamail-region metamail-buffer metamail-interpret-body
;;;;;;  metamail-interpret-header) "metamail" "mail/metamail.el"
;;;;;;  (13977 61527))
;;; Generated autoloads from mail/metamail.el

(autoload (quote metamail-interpret-header) "metamail" "\
Interpret a header part of a MIME message in current buffer.
Its body part is not interpreted at all." t nil)

(autoload (quote metamail-interpret-body) "metamail" "\
Interpret a body part of a MIME message in current buffer.
Optional argument VIEWMODE specifies the value of the
EMACS_VIEW_MODE environment variable (defaulted to 1).
Optional argument NODISPLAY non-nil means buffer is not
redisplayed as output is inserted.
Its header part is not interpreted at all." t nil)

(autoload (quote metamail-buffer) "metamail" "\
Process current buffer through `metamail'.
Optional argument VIEWMODE specifies the value of the
EMACS_VIEW_MODE environment variable (defaulted to 1).
Optional argument BUFFER specifies a buffer to be filled (nil
means current).
Optional argument NODISPLAY non-nil means buffer is not
redisplayed as output is inserted." t nil)

(autoload (quote metamail-region) "metamail" "\
Process current region through 'metamail'.
Optional argument VIEWMODE specifies the value of the
EMACS_VIEW_MODE environment variable (defaulted to 1).
Optional argument BUFFER specifies a buffer to be filled (nil
means current).
Optional argument NODISPLAY non-nil means buffer is not
redisplayed as output is inserted." t nil)

;;;***

;;;### (autoloads (mh-letter-mode mh-smail-other-window mh-smail-batch
;;;;;;  mh-smail) "mh-comp" "mail/mh-comp.el" (14042 3200))
;;; Generated autoloads from mail/mh-comp.el

(autoload (quote mh-smail) "mh-comp" "\
Compose and send mail with the MH mail system.
This function is an entry point to mh-e, the Emacs front end
to the MH mail system.

See documentation of `\\[mh-send]' for more details on composing mail." t nil)

(autoload (quote mh-smail-batch) "mh-comp" "\
Set up a mail composition draft with the MH mail system.
This function is an entry point to mh-e, the Emacs front end
to the MH mail system.  This function does not prompt the user
for any header fields, and thus is suitable for use by programs
that want to create a mail buffer.
Users should use `\\[mh-smail]' to compose mail." nil nil)

(autoload (quote mh-smail-other-window) "mh-comp" "\
Compose and send mail in other window with the MH mail system.
This function is an entry point to mh-e, the Emacs front end
to the MH mail system.

See documentation of `\\[mh-send]' for more details on composing mail." t nil)

(autoload (quote mh-letter-mode) "mh-comp" "\
Mode for composing letters in mh-e.\\<mh-letter-mode-map>
When you have finished composing, type \\[mh-send-letter] to send the message
using the MH mail handling system.
See the documentation for \\[mh-edit-mhn] for information on composing MIME
messages.

\\{mh-letter-mode-map}

Variables controlling this mode (defaults in parentheses):

 mh-delete-yanked-msg-window (nil)
    If non-nil, \\[mh-yank-cur-msg] will delete any windows displaying
    the yanked message.

 mh-yank-from-start-of-msg (t)
    If non-nil, \\[mh-yank-cur-msg] will include the entire message.
    If `body', just yank the body (no header).
    If nil, only the portion of the message following the point will be yanked.
    If there is a region, this variable is ignored.

 mh-ins-buf-prefix (\"> \")
    String to insert before each non-blank line of a message as it is
    inserted in a draft letter.

 mh-signature-file-name (\"~/.signature\")
    File to be inserted into message by \\[mh-insert-signature].

This command runs the normal hooks `text-mode-hook' and `mh-letter-mode-hook'." t nil)

;;;***

;;;### (autoloads (mh-version mh-rmail) "mh-e" "mail/mh-e.el" (13644
;;;;;;  7194))
;;; Generated autoloads from mail/mh-e.el

(autoload (quote mh-rmail) "mh-e" "\
Inc(orporate) new mail with MH, or, with arg, scan an MH mail folder.
This function is an entry point to mh-e, the Emacs front end
to the MH mail system." t nil)

(autoload (quote mh-version) "mh-e" "\
Display version information about mh-e and the MH mail handling system." t nil)

;;;***

;;;### (autoloads nil "mh-mime" "mail/mh-mime.el" (13833 28041))
;;; Generated autoloads from mail/mh-mime.el

(defvar mh-mime-content-types (quote (("text/plain") ("text/richtext") ("multipart/mixed") ("multipart/alternative") ("multipart/digest") ("multipart/parallel") ("message/rfc822") ("message/partial") ("message/external-body") ("application/octet-stream") ("application/postscript") ("image/jpeg") ("image/gif") ("audio/basic") ("video/mpeg"))) "\
Legal MIME content types.  See documentation for \\[mh-edit-mhn].")

;;;***

;;;### (autoloads nil "mh-utils" "mail/mh-utils.el" (14091 60586))
;;; Generated autoloads from mail/mh-utils.el

(put (quote mh-progs) (quote risky-local-variable) t)

(put (quote mh-lib) (quote risky-local-variable) t)

(put (quote mh-lib-progs) (quote risky-local-variable) t)

(put (quote mh-nmh-p) (quote risky-local-variable) t)

;;;***

;;;### (autoloads (midnight-delay-set clean-buffer-list) "midnight"
;;;;;;  "midnight.el" (14035 9946))
;;; Generated autoloads from midnight.el

(autoload (quote clean-buffer-list) "midnight" "\
Kill old buffers that have not been displayed recently.
The relevant variables are `clean-buffer-list-delay-general',
`clean-buffer-list-delay-special', `clean-buffer-list-kill-buffer-names',
`clean-buffer-list-kill-never-buffer-names',
`clean-buffer-list-kill-regexps' and
`clean-buffer-list-kill-never-regexps'.
While processing buffers, this procedure displays messages containing
the current date/time, buffer name, how many seconds ago it was
displayed (can be nil if the buffer was never displayed) and its
lifetime, i.e., its \"age\" when it will be purged." t nil)

(autoload (quote midnight-delay-set) "midnight" "\
Modify `midnight-timer' according to `midnight-delay'.
Sets the first argument SYMB (which must be symbol `midnight-delay')
to its second argument TM." nil nil)

;;;***

;;;### (autoloads (convert-mocklisp-buffer) "mlconvert" "emulation/mlconvert.el"
;;;;;;  (12536 45574))
;;; Generated autoloads from emulation/mlconvert.el

(autoload (quote convert-mocklisp-buffer) "mlconvert" "\
Convert buffer of Mocklisp code to real Lisp that GNU Emacs can run." t nil)

;;;***

;;;### (autoloads (modula-2-mode) "modula2" "progmodes/modula2.el"
;;;;;;  (13552 32650))
;;; Generated autoloads from progmodes/modula2.el

(autoload (quote modula-2-mode) "modula2" "\
This is a mode intended to support program development in Modula-2.
All control constructs of Modula-2 can be reached by typing C-c
followed by the first character of the construct.
\\<m2-mode-map>
  \\[m2-begin] begin         \\[m2-case] case
  \\[m2-definition] definition    \\[m2-else] else
  \\[m2-for] for           \\[m2-header] header
  \\[m2-if] if            \\[m2-module] module
  \\[m2-loop] loop          \\[m2-or] or
  \\[m2-procedure] procedure     Control-c Control-w with
  \\[m2-record] record        \\[m2-stdio] stdio
  \\[m2-type] type          \\[m2-until] until
  \\[m2-var] var           \\[m2-while] while
  \\[m2-export] export        \\[m2-import] import
  \\[m2-begin-comment] begin-comment \\[m2-end-comment] end-comment
  \\[suspend-emacs] suspend Emacs     \\[m2-toggle] toggle
  \\[m2-compile] compile           \\[m2-next-error] next-error
  \\[m2-link] link

   `m2-indent' controls the number of spaces for each indentation.
   `m2-compile-command' holds the command to compile a Modula-2 program.
   `m2-link-command' holds the command to link a Modula-2 program." t nil)

;;;***

;;;### (autoloads (mouse-sel-mode) "mouse-sel" "mouse-sel.el" (14118
;;;;;;  2269))
;;; Generated autoloads from mouse-sel.el

(autoload (quote mouse-sel-mode) "mouse-sel" "\
Toggle Mouse Sel mode.
With prefix ARG, turn Mouse Sel mode on if and only if ARG is positive.
Returns the new status of Mouse Sel mode (non-nil means on).

When Mouse Sel mode is enabled, mouse selection is enhanced in various ways:

- Clicking mouse-1 starts (cancels) selection, dragging extends it.

- Clicking or dragging mouse-3 extends the selection as well.

- Double-clicking on word constituents selects words.
Double-clicking on symbol constituents selects symbols.
Double-clicking on quotes or parentheses selects sexps.
Double-clicking on whitespace selects whitespace.
Triple-clicking selects lines.
Quad-clicking selects paragraphs.

- Selecting sets the region & X primary selection, but does NOT affect
the kill-ring.  Because the mouse handlers set the primary selection
directly, mouse-sel sets the variables interprogram-cut-function
and interprogram-paste-function to nil.

- Clicking mouse-2 inserts the contents of the primary selection at
the mouse position (or point, if mouse-yank-at-point is non-nil).

- Pressing mouse-2 while selecting or extending copies selection
to the kill ring.  Pressing mouse-1 or mouse-3 kills it.

- Double-clicking mouse-3 also kills selection.

- M-mouse-1, M-mouse-2 & M-mouse-3 work similarly to mouse-1, mouse-2
& mouse-3, but operate on the X secondary selection rather than the
primary selection and region." t nil)

;;;***

;;;### (autoloads (mpuz) "mpuz" "play/mpuz.el" (14184 34398))
;;; Generated autoloads from play/mpuz.el

(autoload (quote mpuz) "mpuz" "\
Multiplication puzzle with GNU Emacs." t nil)

;;;***

;;;### (autoloads (msb-mode msb-mode) "msb" "msb.el" (14162 41143))
;;; Generated autoloads from msb.el

(defvar msb-mode nil "\
Toggle msb-mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `msb-mode'.")

(custom-add-to-group (quote msb) (quote msb-mode) (quote custom-variable))

(custom-add-load (quote msb-mode) (quote msb))

(autoload (quote msb-mode) "msb" "\
Toggle Msb mode.
With arg, turn Msb mode on if and only if arg is positive.
This mode overrides the binding(s) of `mouse-buffer-menu' to provide a
different buffer menu using the function `msb'." t nil)

;;;***

;;;### (autoloads (dump-codings dump-charsets mule-diag list-input-methods
;;;;;;  list-fontsets describe-fontset describe-font list-coding-systems
;;;;;;  describe-current-coding-system describe-current-coding-system-briefly
;;;;;;  describe-coding-system list-character-sets) "mule-diag" "international/mule-diag.el"
;;;;;;  (14184 58760))
;;; Generated autoloads from international/mule-diag.el

(autoload (quote list-character-sets) "mule-diag" "\
Display a list of all character sets.

The ID column contains a charset identification number for internal Emacs use.
The B column contains a number of bytes occupied in a buffer
  by any character in this character set.
The W column contains a number of columns occupied on the screen
  by any character in this character set.

With prefix arg, the output format gets more cryptic,
but still shows the full information." t nil)

(autoload (quote describe-coding-system) "mule-diag" "\
Display information about CODING-SYSTEM." t nil)

(autoload (quote describe-current-coding-system-briefly) "mule-diag" "\
Display coding systems currently used in a brief format in echo area.

The format is \"F[..],K[..],T[..],P>[..],P<[..], default F[..],P<[..],P<[..]\",
where mnemonics of the following coding systems come in this order
at the place of `..':
  `buffer-file-coding-system` (of the current buffer)
  eol-type of buffer-file-coding-system (of the current buffer)
  Value returned by `keyboard-coding-system'
  eol-type of (keyboard-coding-system)
  Value returned by `terminal-coding-system.
  eol-type of (terminal-coding-system)
  `process-coding-system' for read (of the current buffer, if any)
  eol-type of process-coding-system for read (of the current buffer, if any)
  `process-coding-system' for write (of the current buffer, if any)
  eol-type of process-coding-system for write (of the current buffer, if any)
  `default-buffer-file-coding-system'
  eol-type of default-buffer-file-coding-system
  `default-process-coding-system' for read
  eol-type of default-process-coding-system for read
  `default-process-coding-system' for write
  eol-type of default-process-coding-system" t nil)

(autoload (quote describe-current-coding-system) "mule-diag" "\
Display coding systems currently used, in detail." t nil)

(autoload (quote list-coding-systems) "mule-diag" "\
Display a list of all coding systems.
This shows the mnemonic letter, name, and description of each coding system.

With prefix arg, the output format gets more cryptic,
but still contains full information about each coding system." t nil)

(autoload (quote describe-font) "mule-diag" "\
Display information about fonts which partially match FONTNAME." t nil)

(autoload (quote describe-fontset) "mule-diag" "\
Display information of FONTSET.
This shows the name, size, and style of FONTSET, and the list of fonts
contained in FONTSET.

The column WDxHT contains width and height (pixels) of each fontset
\(i.e. those of ASCII font in the fontset).  The letter `-' in this
column means that the corresponding fontset is not yet used in any
frame.

The O column for each font contains one of the following letters:
 o -- font already opened
 - -- font not yet opened
 x -- font can't be opened
 ? -- no font specified

The Charset column for each font contains a name of character set
displayed (for this fontset) using that font." t nil)

(autoload (quote list-fontsets) "mule-diag" "\
Display a list of all fontsets.
This shows the name, size, and style of each fontset.
With prefix arg, it also list the fonts contained in each fontset;
see the function `describe-fontset' for the format of the list." t nil)

(autoload (quote list-input-methods) "mule-diag" "\
Display information about all input methods." t nil)

(autoload (quote mule-diag) "mule-diag" "\
Display diagnosis of the multilingual environment (Mule).

This shows various information related to the current multilingual
environment, including lists of input methods, coding systems,
character sets, and fontsets (if Emacs is running under a window
system which uses fontsets)." t nil)

(autoload (quote dump-charsets) "mule-diag" "\
Dump information about all charsets into the file `CHARSETS'.
The file is saved in the directory `data-directory'." nil nil)

(autoload (quote dump-codings) "mule-diag" "\
Dump information about all coding systems into the file `CODINGS'.
The file is saved in the directory `data-directory'." nil nil)

;;;***

;;;### (autoloads (decompose-composite-char compose-chars compose-chars-component
;;;;;;  decompose-string decompose-region compose-region detect-coding-with-language-environment
;;;;;;  detect-coding-with-priority coding-system-equal coding-system-translation-table-for-encode
;;;;;;  coding-system-translation-table-for-decode coding-system-pre-write-conversion
;;;;;;  coding-system-post-read-conversion coding-system-eol-type-mnemonic
;;;;;;  lookup-nested-alist set-nested-alist truncate-string-to-width
;;;;;;  store-substring string-to-sequence) "mule-util" "international/mule-util.el"
;;;;;;  (14184 58760))
;;; Generated autoloads from international/mule-util.el

(autoload (quote string-to-sequence) "mule-util" "\
Convert STRING to a sequence of TYPE which contains characters in STRING.
TYPE should be `list' or `vector'." nil nil)

(defsubst string-to-list (string) "Return a list of characters in STRING." (string-to-sequence string (quote list)))

(defsubst string-to-vector (string) "Return a vector of characters in STRING." (string-to-sequence string (quote vector)))

(autoload (quote store-substring) "mule-util" "\
Embed OBJ (string or character) at index IDX of STRING." nil nil)

(autoload (quote truncate-string-to-width) "mule-util" "\
Truncate string STR to end at column END-COLUMN.
The optional 2nd arg START-COLUMN, if non-nil, specifies
the starting column; that means to return the characters occupying
columns START-COLUMN ... END-COLUMN of STR.

The optional 3rd arg PADDING, if non-nil, specifies a padding character
to add at the end of the result if STR doesn't reach column END-COLUMN,
or if END-COLUMN comes in the middle of a character in STR.
PADDING is also added at the beginning of the result
if column START-COLUMN appears in the middle of a character in STR.

If PADDING is nil, no padding is added in these cases, so
the resulting string may be narrower than END-COLUMN." nil nil)

(defalias (quote truncate-string) (quote truncate-string-to-width))

(defsubst nested-alist-p (obj) "Return t if OBJ is a nested alist.\n\nNested alist is a list of the form (ENTRY . BRANCHES), where ENTRY is\nany Lisp object, and BRANCHES is a list of cons cells of the form\n(KEY-ELEMENT . NESTED-ALIST).\n\nYou can use a nested alist to store any Lisp object (ENTRY) for a key\nsequence KEYSEQ, where KEYSEQ is a sequence of KEY-ELEMENT.  KEYSEQ\ncan be a string, a vector, or a list." (and obj (listp obj) (listp (cdr obj))))

(autoload (quote set-nested-alist) "mule-util" "\
Set ENTRY for KEYSEQ in a nested alist ALIST.
Optional 4th arg LEN non-nil means the first LEN elements in KEYSEQ
 is considered.
Optional argument BRANCHES if non-nil is branches for a keyseq
longer than KEYSEQ.
See the documentation of `nested-alist-p' for more detail." nil nil)

(autoload (quote lookup-nested-alist) "mule-util" "\
Look up key sequence KEYSEQ in nested alist ALIST.  Return the definition.
Optional 1st argument LEN specifies the length of KEYSEQ.
Optional 2nd argument START specifies index of the starting key.
The returned value is normally a nested alist of which
car part is the entry for KEYSEQ.
If ALIST is not deep enough for KEYSEQ, return number which is
 how many key elements at the front of KEYSEQ it takes
 to reach a leaf in ALIST.
Optional 3rd argument NIL-FOR-TOO-LONG non-nil means return nil
 even if ALIST is not deep enough." nil nil)

(autoload (quote coding-system-eol-type-mnemonic) "mule-util" "\
Return the string indicating end-of-line format of CODING-SYSTEM." nil nil)

(autoload (quote coding-system-post-read-conversion) "mule-util" "\
Return the value of CODING-SYSTEM's post-read-conversion property." nil nil)

(autoload (quote coding-system-pre-write-conversion) "mule-util" "\
Return the value of CODING-SYSTEM's pre-write-conversion property." nil nil)

(autoload (quote coding-system-translation-table-for-decode) "mule-util" "\
Return the value of CODING-SYSTEM's translation-table-for-decode property." nil nil)

(autoload (quote coding-system-translation-table-for-encode) "mule-util" "\
Return the value of CODING-SYSTEM's translation-table-for-encode property." nil nil)

(autoload (quote coding-system-equal) "mule-util" "\
Return t if and only if CODING-SYSTEM-1 and CODING-SYSTEM-2 are identical.
Two coding systems are identical if two symbols are equal
or one is an alias of the other." nil nil)

(autoload (quote detect-coding-with-priority) "mule-util" "\
Detect a coding system of the text between FROM and TO with PRIORITY-LIST.
PRIORITY-LIST is an alist of coding categories vs the corresponding
coding systems ordered by priority." nil (quote macro))

(autoload (quote detect-coding-with-language-environment) "mule-util" "\
Detect a coding system of the text between FROM and TO with LANG-ENV.
The detection takes into account the coding system priorities for the
language environment LANG-ENV." nil nil)

(autoload (quote compose-region) "mule-util" "\
Compose all characters in the current region into one composite character.
When called from a program, expects two arguments,
positions (integers or markers) specifying the region." t nil)

(autoload (quote decompose-region) "mule-util" "\
Decompose all composite characters in the current region.
Composite characters are broken up into individual components.
When called from a program, expects two arguments,
positions (integers or markers) specifying the region." t nil)

(autoload (quote decompose-string) "mule-util" "\
Decompose all composite characters in STRING." nil nil)

(defconst reference-point-alist (quote ((tl . 0) (tc . 1) (tr . 2) (ml . 3) (mc . 4) (mr . 5) (bl . 6) (bc . 7) (br . 8) (top-left . 0) (top-center . 1) (top-right . 2) (mid-left . 3) (mid-center . 4) (mid-right . 5) (bottom-left . 6) (bottom-center . 7) (bottom-right . 8) (0 . 0) (1 . 1) (2 . 2) (3 . 3) (4 . 4) (5 . 5) (6 . 6) (7 . 7) (8 . 8))) "\
Alist of reference point symbols vs reference point codes.
A reference point symbol is to be used to specify a composition rule
while making a composite character by the function `compose-chars'
\(which see).

Meanings of reference point codes are as follows:

    0----1----2 <-- ascent	0:tl or top-left
    |         |			1:tc or top-center
    |         |			2:tr or top-right
    |         |			3:ml or mid-left
    |    4 <--+---- center	4:mc or mid-center
    |         |			5:mr or mid-right
--- 3         5 <-- baseline	6:bl or bottom-left
    |         |			7:bc or bottom-center
    6----7----8 <-- descent	8:br or bottom-right

Reference point symbols are to be used to specify composition rule of
the form (GLOBAL-REF-POINT . NEW-REF-POINT), where GLOBAL-REF-POINT
is a reference point in the overall glyphs already composed, and
NEW-REF-POINT is a reference point in the new glyph to be added.

For instance, if GLOBAL-REF-POINT is 8 and NEW-REF-POINT is 1, the
overall glyph is updated as follows:

    +-------+--+ <--- new ascent
    |       |  |
    | global|  |
    | glyph |  |
--- |       |  | <--- baseline (doesn't change)
    +----+--+--+
    |    | new |
    |    |glyph|
    +----+-----+ <--- new descent
")

(autoload (quote compose-chars-component) "mule-util" nil nil nil)

(autoload (quote compose-chars) "mule-util" "\
Return one char string composed from the arguments.
For relative composition, each argument should be a non-composition character
or a relative-composition character.
For rule-based composition, Nth (where N is odd) argument should be
a non-composition character or a rule-based-composition character,
and Mth (where M is even) argument should be a composition rule.
A composition rule has the form (GLOBAL-REF-POINT . NEW-REF-POINT).
See the documentation of `reference-point-alist' for more detail." nil nil)

(autoload (quote decompose-composite-char) "mule-util" "\
Convert composite character CHAR to a sequence of the components.
Optional 1st arg TYPE specifies the type of sequence returned.
It should be `string' (default), `list', or `vector'.
Optional 2nd arg WITH-COMPOSITION-RULE non-nil means the returned
sequence contains embedded composition rules if any.  In this case, the
order of elements in the sequence is the same as arguments for
`compose-chars' to create CHAR.
If TYPE is omitted or is `string', composition rules are omitted
even if WITH-COMPOSITION-RULE is t." nil nil)

;;;***

;;;### (autoloads (network-connection network-connection-to-service
;;;;;;  whois-reverse-lookup whois finger ftp nslookup nslookup-host
;;;;;;  route arp netstat ipconfig ping traceroute) "net-utils" "net-utils.el"
;;;;;;  (13870 8854))
;;; Generated autoloads from net-utils.el

(autoload (quote traceroute) "net-utils" "\
Run traceroute program for TARGET." t nil)

(autoload (quote ping) "net-utils" "\
Ping HOST.
If your system's ping continues until interrupted, you can try setting 
`ping-program-options'." t nil)

(autoload (quote ipconfig) "net-utils" "\
Run ipconfig program." t nil)

(defalias (quote ifconfig) (quote ipconfig))

(autoload (quote netstat) "net-utils" "\
Run netstat program." t nil)

(autoload (quote arp) "net-utils" "\
Run the arp program." t nil)

(autoload (quote route) "net-utils" "\
Run the route program." t nil)

(autoload (quote nslookup-host) "net-utils" "\
Lookup the DNS information for HOST." t nil)

(autoload (quote nslookup) "net-utils" "\
Run nslookup program." t nil)

(autoload (quote ftp) "net-utils" "\
Run ftp program." t nil)

(autoload (quote finger) "net-utils" "\
Finger USER on HOST." t nil)

(autoload (quote whois) "net-utils" "\
Send SEARCH-STRING to server defined by the `whois-server-name' variable.
If `whois-guess-server' is non-nil, then try to deduce the correct server
from SEARCH-STRING.  With argument, prompt for whois server." t nil)

(autoload (quote whois-reverse-lookup) "net-utils" nil t nil)

(autoload (quote network-connection-to-service) "net-utils" "\
Open a network connection to SERVICE on HOST." t nil)

(autoload (quote network-connection) "net-utils" "\
Open a network connection to HOST on PORT." t nil)

;;;***

;;;### (autoloads (nndoc-add-type) "nndoc" "gnus/nndoc.el" (14030
;;;;;;  49490))
;;; Generated autoloads from gnus/nndoc.el

(autoload (quote nndoc-add-type) "nndoc" "\
Add document DEFINITION to the list of nndoc document definitions.
If POSITION is nil or `last', the definition will be added
as the last checked definition, if t or `first', add as the
first definition, and if any other symbol, add after that
symbol in the alist." nil nil)

;;;***

;;;### (autoloads (nnfolder-generate-active-file) "nnfolder" "gnus/nnfolder.el"
;;;;;;  (14030 49496))
;;; Generated autoloads from gnus/nnfolder.el

(autoload (quote nnfolder-generate-active-file) "nnfolder" "\
Look for mbox folders in the nnfolder directory and make them into groups." t nil)

;;;***

;;;### (autoloads (nnkiboze-generate-groups) "nnkiboze" "gnus/nnkiboze.el"
;;;;;;  (14030 49502))
;;; Generated autoloads from gnus/nnkiboze.el

(autoload (quote nnkiboze-generate-groups) "nnkiboze" "\
\"Usage: emacs -batch -l nnkiboze -f nnkiboze-generate-groups\".
Finds out what articles are to be part of the nnkiboze groups." t nil)

;;;***

;;;### (autoloads (nnml-generate-nov-databases) "nnml" "gnus/nnml.el"
;;;;;;  (14030 49514))
;;; Generated autoloads from gnus/nnml.el

(autoload (quote nnml-generate-nov-databases) "nnml" "\
Generate NOV databases in all nnml directories." t nil)

;;;***

;;;### (autoloads (nnsoup-revert-variables nnsoup-set-variables nnsoup-pack-replies)
;;;;;;  "nnsoup" "gnus/nnsoup.el" (14030 49518))
;;; Generated autoloads from gnus/nnsoup.el

(autoload (quote nnsoup-pack-replies) "nnsoup" "\
Make an outbound package of SOUP replies." t nil)

(autoload (quote nnsoup-set-variables) "nnsoup" "\
Use the SOUP methods for posting news and mailing mail." t nil)

(autoload (quote nnsoup-revert-variables) "nnsoup" "\
Revert posting and mailing methods to the standard Emacs methods." t nil)

;;;***

;;;### (autoloads (disable-command enable-command disabled-command-hook)
;;;;;;  "novice" "novice.el" (13229 29108))
;;; Generated autoloads from novice.el

(defvar disabled-command-hook (quote disabled-command-hook) "\
Function to call to handle disabled commands.
If nil, the feature is disabled, i.e., all commands work normally.")

(autoload (quote disabled-command-hook) "novice" nil nil nil)

(autoload (quote enable-command) "novice" "\
Allow COMMAND to be executed without special confirmation from now on.
The user's .emacs file is altered so that this will apply
to future sessions." t nil)

(autoload (quote disable-command) "novice" "\
Require special confirmation to execute COMMAND from now on.
The user's .emacs file is altered so that this will apply
to future sessions." t nil)

;;;***

;;;### (autoloads (nroff-mode) "nroff-mode" "textmodes/nroff-mode.el"
;;;;;;  (13611 44372))
;;; Generated autoloads from textmodes/nroff-mode.el

(autoload (quote nroff-mode) "nroff-mode" "\
Major mode for editing text intended for nroff to format.
\\{nroff-mode-map}
Turning on Nroff mode runs `text-mode-hook', then `nroff-mode-hook'.
Also, try `nroff-electric-mode', for automatically inserting
closing requests for requests that are used in matched pairs." t nil)

;;;***

;;;### (autoloads (octave-help) "octave-hlp" "progmodes/octave-hlp.el"
;;;;;;  (13145 50262))
;;; Generated autoloads from progmodes/octave-hlp.el

(autoload (quote octave-help) "octave-hlp" "\
Get help on Octave symbols from the Octave info files.
Look up KEY in the function, operator and variable indices of the files
specified by `octave-help-files'.
If KEY is not a string, prompt for it with completion." t nil)

;;;***

;;;### (autoloads (inferior-octave) "octave-inf" "progmodes/octave-inf.el"
;;;;;;  (14211 48115))
;;; Generated autoloads from progmodes/octave-inf.el

(autoload (quote inferior-octave) "octave-inf" "\
Run an inferior Octave process, I/O via `inferior-octave-buffer'.
This buffer is put in Inferior Octave mode.  See `inferior-octave-mode'.

Unless ARG is non-nil, switches to this buffer.

The elements of the list `inferior-octave-startup-args' are sent as
command line arguments to the inferior Octave process on startup.

Additional commands to be executed on startup can be provided either in
the file specified by `inferior-octave-startup-file' or by the default
startup file, `~/.emacs-octave'." t nil)

(defalias (quote run-octave) (quote inferior-octave))

;;;***

;;;### (autoloads (octave-mode) "octave-mod" "progmodes/octave-mod.el"
;;;;;;  (13935 15541))
;;; Generated autoloads from progmodes/octave-mod.el

(autoload (quote octave-mode) "octave-mod" "\
Major mode for editing Octave code.

This mode makes it easier to write Octave code by helping with
indentation, doing some of the typing for you (with Abbrev mode) and by
showing keywords, comments, strings, etc. in different faces (with
Font Lock mode on terminals that support it).

Octave itself is a high-level language, primarily intended for numerical
computations.  It provides a convenient command line interface for
solving linear and nonlinear problems numerically.  Function definitions
can also be stored in files, and it can be used in a batch mode (which
is why you need this mode!).

The latest released version of Octave is always available via anonymous
ftp from bevo.che.wisc.edu in the directory `/pub/octave'.  Complete
source and binaries for several popular systems are available.

Type \\[list-abbrevs] to display the built-in abbrevs for Octave keywords.

Keybindings
===========

\\{octave-mode-map}

Variables you can use to customize Octave mode
==============================================

octave-auto-indent
  Non-nil means indent current line after a semicolon or space.
  Default is nil.

octave-auto-newline
  Non-nil means auto-insert a newline and indent after a semicolon.
  Default is nil.

octave-blink-matching-block
  Non-nil means show matching begin of block when inserting a space,
  newline or semicolon after an else or end keyword.  Default is t.

octave-block-offset
  Extra indentation applied to statements in block structures.
  Default is 2.

octave-continuation-offset
  Extra indentation applied to Octave continuation lines.
  Default is 4.

octave-continuation-string
  String used for Octave continuation lines.
  Default is a backslash.

octave-mode-startup-message
  Nil means do not display the Octave mode startup message.
  Default is t.

octave-send-echo-input
  Non-nil means always display `inferior-octave-buffer' after sending a
  command to the inferior Octave process.

octave-send-line-auto-forward
  Non-nil means always go to the next unsent line of Octave code after
  sending a line to the inferior Octave process.

octave-send-echo-input
  Non-nil means echo input sent to the inferior Octave process.

Turning on Octave mode runs the hook `octave-mode-hook'.

To begin using this mode for all `.m' files that you edit, add the
following lines to your `.emacs' file:

  (autoload 'octave-mode \"octave-mod\" nil t)
  (setq auto-mode-alist
        (cons '(\"\\\\.m$\" . octave-mode) auto-mode-alist))

To automatically turn on the abbrev, auto-fill and font-lock features,
add the following lines to your `.emacs' file as well:

  (add-hook 'octave-mode-hook
	    (lambda ()
	      (abbrev-mode 1)
	      (auto-fill-mode 1)
	      (if (eq window-system 'x)
		  (font-lock-mode 1))))

To submit a problem report, enter \\[octave-submit-bug-report] from an Octave mode buffer.
This automatically sets up a mail buffer with version information
already added.  You just need to add a description of the problem,
including a reproducible test case and send the message." t nil)

;;;***

;;;### (autoloads (edit-options list-options) "options" "options.el"
;;;;;;  (14045 29891))
;;; Generated autoloads from options.el

(autoload (quote list-options) "options" "\
Display a list of Emacs user options, with values and documentation." t nil)

(autoload (quote edit-options) "options" "\
Edit a list of Emacs user option values.
Selects a buffer containing such a list,
in which there are commands to set the option values.
Type \\[describe-mode] in that buffer for a list of commands.

The Custom feature is intended to make this obsolete." t nil)

;;;***

;;;### (autoloads (outline-minor-mode outline-mode) "outline" "textmodes/outline.el"
;;;;;;  (14115 62973))
;;; Generated autoloads from textmodes/outline.el

(autoload (quote outline-mode) "outline" "\
Set major mode for editing outlines with selective display.
Headings are lines which start with asterisks: one for major headings,
two for subheadings, etc.  Lines not starting with asterisks are body lines.

Body text or subheadings under a heading can be made temporarily
invisible, or visible again.  Invisible lines are attached to the end
of the heading, so they move with it, if the line is killed and yanked
back.  A heading with text hidden under it is marked with an ellipsis (...).

Commands:\\<outline-mode-map>
\\[outline-next-visible-heading]   outline-next-visible-heading      move by visible headings
\\[outline-previous-visible-heading]   outline-previous-visible-heading
\\[outline-forward-same-level]   outline-forward-same-level        similar but skip subheadings
\\[outline-backward-same-level]   outline-backward-same-level
\\[outline-up-heading]   outline-up-heading		    move from subheading to heading

\\[hide-body]	make all text invisible (not headings).
\\[show-all]	make everything in buffer visible.

The remaining commands are used when point is on a heading line.
They apply to some of the body or subheadings of that heading.
\\[hide-subtree]   hide-subtree	make body and subheadings invisible.
\\[show-subtree]   show-subtree	make body and subheadings visible.
\\[show-children]   show-children	make direct subheadings visible.
		 No effect on body, or subheadings 2 or more levels down.
		 With arg N, affects subheadings N levels down.
\\[hide-entry]	   make immediately following body invisible.
\\[show-entry]	   make it visible.
\\[hide-leaves]	   make body under heading and under its subheadings invisible.
		     The subheadings remain visible.
\\[show-branches]  make all subheadings at all levels visible.

The variable `outline-regexp' can be changed to control what is a heading.
A line is a heading if `outline-regexp' matches something at the
beginning of the line.  The longer the match, the deeper the level.

Turning on outline mode calls the value of `text-mode-hook' and then of
`outline-mode-hook', if they are non-nil." t nil)

(autoload (quote outline-minor-mode) "outline" "\
Toggle Outline minor mode.
With arg, turn Outline minor mode on if arg is positive, off otherwise.
See the command `outline-mode' for more information on this mode." t nil)

;;;***

;;;### (autoloads (show-paren-mode) "paren" "paren.el" (14118 2930))
;;; Generated autoloads from paren.el

(autoload (quote show-paren-mode) "paren" "\
Toggle Show Paren mode.
With prefix ARG, turn Show Paren mode on if and only if ARG is positive.
Returns the new status of Show Paren mode (non-nil means on).

When Show Paren mode is enabled, any matching parenthesis is highlighted
in `show-paren-style' after `show-paren-delay' seconds of Emacs idle time." t nil)

;;;***

;;;### (autoloads (pascal-mode) "pascal" "progmodes/pascal.el" (14218
;;;;;;  4426))
;;; Generated autoloads from progmodes/pascal.el

(autoload (quote pascal-mode) "pascal" "\
Major mode for editing Pascal code. \\<pascal-mode-map>
TAB indents for Pascal code.  Delete converts tabs to spaces as it moves back.

\\[pascal-complete-word] completes the word around current point with respect to position in code
\\[pascal-show-completions] shows all possible completions at this point.

Other useful functions are:

\\[pascal-mark-defun]	- Mark function.
\\[pascal-insert-block]	- insert begin ... end;
\\[pascal-star-comment]	- insert (* ... *)
\\[pascal-comment-area]	- Put marked area in a comment, fixing nested comments.
\\[pascal-uncomment-area]	- Uncomment an area commented with \\[pascal-comment-area].
\\[pascal-beg-of-defun]	- Move to beginning of current function.
\\[pascal-end-of-defun]	- Move to end of current function.
\\[pascal-goto-defun]	- Goto function prompted for in the minibuffer.
\\[pascal-outline]	- Enter pascal-outline-mode (see also pascal-outline).

Variables controlling indentation/edit style:

 pascal-indent-level (default 3)
    Indentation of Pascal statements with respect to containing block.
 pascal-case-indent (default 2)
    Indentation for case statements.
 pascal-auto-newline (default nil)
    Non-nil means automatically newline after semicolons and the punctuation
    mark after an end.
 pascal-indent-nested-functions (default t)
    Non-nil means nested functions are indented.
 pascal-tab-always-indent (default t)
    Non-nil means TAB in Pascal mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 pascal-auto-endcomments (default t)
    Non-nil means a comment { ... } is set after the ends which ends cases and
    functions. The name of the function or case will be set between the braces.
 pascal-auto-lineup (default t)
    List of contexts where auto lineup of :'s or ='s should be done.

See also the user variables pascal-type-keywords, pascal-start-keywords and
pascal-separator-keywords.

Turning on Pascal mode calls the value of the variable pascal-mode-hook with
no args, if that value is non-nil." t nil)

;;;***

;;;### (autoloads (pc-bindings-mode) "pc-mode" "emulation/pc-mode.el"
;;;;;;  (13229 29211))
;;; Generated autoloads from emulation/pc-mode.el

(autoload (quote pc-bindings-mode) "pc-mode" "\
Set up certain key bindings for PC compatibility.
The keys affected are:
Delete (and its variants) delete forward instead of backward.
C-Backspace kills backward a word (as C-Delete normally would).
M-Backspace does undo.
Home and End move to beginning and end of line
C-Home and C-End move to beginning and end of buffer.
C-Escape does list-buffers." t nil)

;;;***

;;;### (autoloads (pc-selection-mode pc-selection-mode) "pc-select"
;;;;;;  "emulation/pc-select.el" (13674 34130))
;;; Generated autoloads from emulation/pc-select.el

(autoload (quote pc-selection-mode) "pc-select" "\
Change mark behaviour to emulate Motif, MAC or MS-Windows cut and paste style.

This mode enables Delete Selection mode and Transient Mark mode.

The arrow keys (and others) are bound to new functions
which modify the status of the mark.

The ordinary arrow keys disable the mark.
The shift-arrow keys move, leaving the mark behind.

C-LEFT and C-RIGHT move back or forward one word, disabling the mark.
S-C-LEFT and S-C-RIGHT move back or forward one word, leaving the mark behind.

M-LEFT and M-RIGHT move back or forward one word or sexp, disabling the mark.
S-M-LEFT and S-M-RIGHT move back or forward one word or sexp, leaving the mark
behind. To control wether these keys move word-wise or sexp-wise set the
variable pc-select-meta-moves-sexps after loading pc-select.el but before
turning pc-selection-mode on.

C-DOWN and C-UP move back or forward a paragraph, disabling the mark.
S-C-DOWN and S-C-UP move back or forward a paragraph, leaving the mark behind.

HOME moves to beginning of line, disabling the mark.
S-HOME moves to beginning of line, leaving the mark behind.
With Ctrl or Meta, these keys move to beginning of buffer instead.

END moves to end of line, disabling the mark.
S-END moves to end of line, leaving the mark behind.
With Ctrl or Meta, these keys move to end of buffer instead.

PRIOR or PAGE-UP scrolls and disables the mark.
S-PRIOR or S-PAGE-UP scrolls and leaves the mark behind.

S-DELETE kills the region (`kill-region').
S-INSERT yanks text from the kill ring (`yank').
C-INSERT copies the region into the kill ring (`copy-region-as-kill').

In addition, certain other PC bindings are imitated (to avoid this, set
the variable pc-select-selection-keys-only to t after loading pc-select.el
but before calling pc-selection-mode):

  F6           other-window
  DELETE       delete-char
  C-DELETE     kill-line
  M-DELETE     kill-word
  C-M-DELETE   kill-sexp
  C-BACKSPACE  backward-kill-word
  M-BACKSPACE  undo" t nil)

(defvar pc-selection-mode nil "\
Toggle PC Selection mode.
Change mark behaviour to emulate Motif, MAC or MS-Windows cut and paste style,
and cursor movement commands.
This mode enables Delete Selection mode and Transient Mark mode.
You must modify via \\[customize] for this variable to have an effect.")

(custom-add-to-group (quote pc-select) (quote pc-selection-mode) (quote custom-variable))

(custom-add-load (quote pc-selection-mode) (quote pc-select))

;;;***

;;;### (autoloads (perl-mode) "perl-mode" "progmodes/perl-mode.el"
;;;;;;  (13639 60556))
;;; Generated autoloads from progmodes/perl-mode.el

(autoload (quote perl-mode) "perl-mode" "\
Major mode for editing Perl code.
Expression and list commands understand all Perl brackets.
Tab indents for Perl code.
Comments are delimited with # ... \\n.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
\\{perl-mode-map}
Variables controlling indentation style:
 perl-tab-always-indent
    Non-nil means TAB in Perl mode should always indent the current line,
    regardless of where in the line point is when the TAB command is used.
 perl-tab-to-comment
    Non-nil means that for lines which don't need indenting, TAB will
    either delete an empty comment, indent an existing comment, move 
    to end-of-line, or if at end-of-line already, create a new comment.
 perl-nochange
    Lines starting with this regular expression are not auto-indented.
 perl-indent-level
    Indentation of Perl statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 perl-continued-statement-offset
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 perl-continued-brace-offset
    Extra indentation given to a brace that starts a substatement.
    This is in addition to `perl-continued-statement-offset'.
 perl-brace-offset
    Extra indentation for line if it starts with an open brace.
 perl-brace-imaginary-offset
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.
 perl-label-offset
    Extra indentation for line that is a label.

Various indentation styles:       K&R  BSD  BLK  GNU  LW
  perl-indent-level                5    8    0    2    4
  perl-continued-statement-offset  5    8    4    2    4
  perl-continued-brace-offset      0    0    0    0   -4
  perl-brace-offset               -5   -8    0    0    0
  perl-brace-imaginary-offset      0    0    4    0    0
  perl-label-offset               -5   -8   -2   -2   -2

Turning on Perl mode runs the normal hook `perl-mode-hook'." t nil)

;;;***

;;;### (autoloads (ph-query-form ph-expand-inline ph-get-phone ph-get-email)
;;;;;;  "ph" "ph.el" (13623 48498))
;;; Generated autoloads from ph.el

(autoload (quote ph-get-email) "ph" "\
Get the email field of NAME from the PH/QI directory server." t nil)

(autoload (quote ph-get-phone) "ph" "\
Get the phone field of NAME from the PH/QI directory server." t nil)

(autoload (quote ph-expand-inline) "ph" "\
Query the PH server, and expand the query string before point.
The query string consists of the buffer substring from the point back to
the preceding comma, colon or beginning of line.  If it contains more than
one word, the variable `ph-inline-query-format-list' controls to map these
onto CCSO database field names.
After querying the server for the given string, the expansion specified by 
`ph-inline-expansion-format' is inserted in the buffer at point.
If REPLACE is t, then this expansion replaces the name in the buffer.
If `ph-expanding-overwrites-query' is t, that inverts the meaning of REPLACE." t nil)

(autoload (quote ph-query-form) "ph" "\
Display a form to query the CCSO PH/QI nameserver.
If given a non-nil argument the function first queries the server 
for the existing fields and displays a corresponding form." t nil)

;;;***

;;;### (autoloads (picture-mode) "picture" "textmodes/picture.el"
;;;;;;  (13906 30600))
;;; Generated autoloads from textmodes/picture.el

(autoload (quote picture-mode) "picture" "\
Switch to Picture mode, in which a quarter-plane screen model is used.
Printing characters replace instead of inserting themselves with motion
afterwards settable by these commands:
  C-c <	  Move left after insertion.
  C-c >	  Move right after insertion.
  C-c ^	  Move up after insertion.
  C-c .	  Move down after insertion.
  C-c `	  Move northwest (nw) after insertion.
  C-c '	  Move northeast (ne) after insertion.
  C-c /	  Move southwest (sw) after insertion.
  C-c \\   Move southeast (se) after insertion.
  C-u C-c `  Move westnorthwest (wnw) after insertion.
  C-u C-c '  Move eastnortheast (ene) after insertion.
  C-u C-c /  Move westsouthwest (wsw) after insertion.
  C-u C-c \\  Move eastsoutheast (ese) after insertion.
The current direction is displayed in the mode line.  The initial
direction is right.  Whitespace is inserted and tabs are changed to
spaces when required by movement.  You can move around in the buffer
with these commands:
  \\[picture-move-down]	  Move vertically to SAME column in previous line.
  \\[picture-move-up]	  Move vertically to SAME column in next line.
  \\[picture-end-of-line]	  Move to column following last non-whitespace character.
  \\[picture-forward-column]	  Move right inserting spaces if required.
  \\[picture-backward-column]	  Move left changing tabs to spaces if required.
  C-c C-f Move in direction of current picture motion.
  C-c C-b Move in opposite direction of current picture motion.
  Return  Move to beginning of next line.
You can edit tabular text with these commands:
  M-Tab	  Move to column beneath (or at) next interesting character.
	    `Indents' relative to a previous line.
  Tab	  Move to next stop in tab stop list.
  C-c Tab Set tab stops according to context of this line.
	    With ARG resets tab stops to default (global) value.
	    See also documentation of variable	picture-tab-chars
	    which defines \"interesting character\".  You can manually
	    change the tab stop list with command \\[edit-tab-stops].
You can manipulate text with these commands:
  C-d	  Clear (replace) ARG columns after point without moving.
  C-c C-d Delete char at point - the command normally assigned to C-d.
  \\[picture-backward-clear-column]  Clear (replace) ARG columns before point, moving back over them.
  \\[picture-clear-line]	  Clear ARG lines, advancing over them.	 The cleared
	    text is saved in the kill ring.
  \\[picture-open-line]	  Open blank line(s) beneath current line.
You can manipulate rectangles with these commands:
  C-c C-k Clear (or kill) a rectangle and save it.
  C-c C-w Like C-c C-k except rectangle is saved in named register.
  C-c C-y Overlay (or insert) currently saved rectangle at point.
  C-c C-x Like C-c C-y except rectangle is taken from named register.
  C-c C-r Draw a rectangular box around mark and point.
  \\[copy-rectangle-to-register]   Copies a rectangle to a register.
  \\[advertised-undo]   Can undo effects of rectangle overlay commands
	    commands if invoked soon enough.
You can return to the previous mode with:
  C-c C-c Which also strips trailing whitespace from every line.
	    Stripping is suppressed by supplying an argument.

Entry to this mode calls the value of  picture-mode-hook  if non-nil.

Note that Picture mode commands will work outside of Picture mode, but
they are not defaultly assigned to keys." t nil)

(defalias (quote edit-picture) (quote picture-mode))

;;;***

;;;### (autoloads (pp-eval-last-sexp pp-eval-expression pp) "pp"
;;;;;;  "emacs-lisp/pp.el" (13819 15913))
;;; Generated autoloads from emacs-lisp/pp.el

(autoload (quote pp) "pp" "\
Output the pretty-printed representation of OBJECT, any Lisp object.
Quoting characters are printed as needed to make output that `read'
can handle, whenever this is possible.
Output stream is STREAM, or value of `standard-output' (which see)." nil nil)

(autoload (quote pp-eval-expression) "pp" "\
Evaluate EXPRESSION and pretty-print value into a new display buffer.
If the pretty-printed value fits on one line, the message line is used
instead.  The value is also consed onto the front of the list
in the variable `values'." t nil)

(autoload (quote pp-eval-last-sexp) "pp" "\
Run `pp-eval-expression' on sexp before point (which see).
With argument, pretty-print output into current buffer.
Ignores leading comment characters." t nil)

;;;***

;;;### (autoloads (run-prolog prolog-mode) "prolog" "progmodes/prolog.el"
;;;;;;  (13446 12642))
;;; Generated autoloads from progmodes/prolog.el

(autoload (quote prolog-mode) "prolog" "\
Major mode for editing Prolog code for Prologs.
Blank lines and `%%...' separate paragraphs.  `%'s start comments.
Commands:
\\{prolog-mode-map}
Entry to this mode calls the value of `prolog-mode-hook'
if that value is non-nil." t nil)

(autoload (quote run-prolog) "prolog" "\
Run an inferior Prolog process, input and output via buffer *prolog*." t nil)

;;;***

;;;### (autoloads nil "ps-bdf" "ps-bdf.el" (14053 43936))
;;; Generated autoloads from ps-bdf.el

(defvar bdf-directory-list (if (eq system-type (quote ms-dos)) (list (expand-file-name "fonts/bdf" installation-directory)) (quote ("/usr/local/share/emacs/fonts/bdf"))) "\
*List of directories to search for `BDF' font files.
The default value is '(\"/usr/local/share/emacs/fonts/bdf\").")

;;;***

;;;### (autoloads (ps-mule-begin-page ps-mule-begin-job ps-mule-initialize
;;;;;;  ps-mule-plot-string ps-mule-set-ascii-font ps-mule-prepare-ascii-font
;;;;;;  ps-multibyte-buffer) "ps-mule" "ps-mule.el" (14138 30128))
;;; Generated autoloads from ps-mule.el

(defvar ps-multibyte-buffer nil "\
*Specifies the multi-byte buffer handling.

Valid values are:

  nil                     This is the value to use the default settings which
			  is by default for printing buffer with only ASCII
			  and Latin characters.   The default setting can be
			  changed by setting the variable
			  `ps-mule-font-info-database-default' differently.
			  The initial value of this variable is
			  `ps-mule-font-info-database-latin' (which see).

  `non-latin-printer'     This is the value to use when you have a Japanese
			  or Korean PostScript printer and want to print
			  buffer with ASCII, Latin-1, Japanese (JISX0208 and
			  JISX0201-Kana) and Korean characters.  At present,
			  it was not tested the Korean characters printing.
			  If you have a korean PostScript printer, please,
			  test it.

  `bdf-font'              This is the value to use when you want to print
			  buffer with BDF fonts.  BDF fonts include both latin
			  and non-latin fonts.  BDF (Bitmap Distribution
			  Format) is a format used for distributing X's font
			  source file.  BDF fonts are included in
			  `intlfonts-1.1' which is a collection of X11 fonts
			  for all characters supported by Emacs.  In order to
			  use this value, be sure to have installed
			  `intlfonts-1.1' and set the variable
			  `bdf-directory-list' appropriately (see ps-bdf.el for
			  documentation of this variable).

  `bdf-font-except-latin' This is like `bdf-font' except that it is used
			  PostScript default fonts to print ASCII and Latin-1
			  characters.  This is convenient when you want or
			  need to use both latin and non-latin characters on
			  the same buffer.  See `ps-font-family',
			  `ps-header-font-family' and `ps-font-info-database'.

Any other value is treated as nil.")

(autoload (quote ps-mule-prepare-ascii-font) "ps-mule" "\
Setup special ASCII font for STRING.
STRING should contain only ASCII characters." nil nil)

(autoload (quote ps-mule-set-ascii-font) "ps-mule" nil nil nil)

(autoload (quote ps-mule-plot-string) "ps-mule" "\
Generate PostScript code for ploting characters in the region FROM and TO.

It is assumed that all characters in this region belong to the same charset.

Optional argument BG-COLOR specifies background color.

Returns the value:

	(ENDPOS . RUN-WIDTH)

Where ENDPOS is the end position of the sequence and RUN-WIDTH is the width of
the sequence." nil nil)

(autoload (quote ps-mule-initialize) "ps-mule" "\
Initialize global data for printing multi-byte characters." nil nil)

(autoload (quote ps-mule-begin-job) "ps-mule" "\
Start printing job for multi-byte chars between FROM and TO.
This checks if all multi-byte characters in the region are printable or not." nil nil)

(autoload (quote ps-mule-begin-page) "ps-mule" nil nil nil)

;;;***

;;;### (autoloads (ps-extend-face ps-extend-face-list ps-setup ps-nb-pages-region
;;;;;;  ps-nb-pages-buffer ps-line-lengths ps-despool ps-spool-region-with-faces
;;;;;;  ps-spool-region ps-spool-buffer-with-faces ps-spool-buffer
;;;;;;  ps-print-region-with-faces ps-print-region ps-print-buffer-with-faces
;;;;;;  ps-print-buffer ps-paper-type) "ps-print" "ps-print.el" (14182
;;;;;;  2572))
;;; Generated autoloads from ps-print.el

(defvar ps-paper-type (quote letter) "\
*Specifies the size of paper to format for.
Should be one of the paper types defined in `ps-page-dimensions-database', for
example `letter', `legal' or `a4'.")

(autoload (quote ps-print-buffer) "ps-print" "\
Generate and print a PostScript image of the buffer.

Interactively, when you use a prefix argument (C-u), the command
prompts the user for a file name, and saves the PostScript image
in that file instead of sending it to the printer.

Noninteractively, the argument FILENAME is treated as follows: if it
is nil, send the image to the printer.  If FILENAME is a string, save
the PostScript image in a file with that name." t nil)

(autoload (quote ps-print-buffer-with-faces) "ps-print" "\
Generate and print a PostScript image of the buffer.
Like `ps-print-buffer', but includes font, color, and underline
information in the generated image.  This command works only if you
are using a window system, so it has a way to determine color values." t nil)

(autoload (quote ps-print-region) "ps-print" "\
Generate and print a PostScript image of the region.
Like `ps-print-buffer', but prints just the current region." t nil)

(autoload (quote ps-print-region-with-faces) "ps-print" "\
Generate and print a PostScript image of the region.
Like `ps-print-region', but includes font, color, and underline
information in the generated image.  This command works only if you
are using a window system, so it has a way to determine color values." t nil)

(autoload (quote ps-spool-buffer) "ps-print" "\
Generate and spool a PostScript image of the buffer.
Like `ps-print-buffer' except that the PostScript image is saved in a
local buffer to be sent to the printer later.

Use the command `ps-despool' to send the spooled images to the printer." t nil)

(autoload (quote ps-spool-buffer-with-faces) "ps-print" "\
Generate and spool a PostScript image of the buffer.
Like `ps-spool-buffer', but includes font, color, and underline
information in the generated image.  This command works only if you
are using a window system, so it has a way to determine color values.

Use the command `ps-despool' to send the spooled images to the printer." t nil)

(autoload (quote ps-spool-region) "ps-print" "\
Generate a PostScript image of the region and spool locally.
Like `ps-spool-buffer', but spools just the current region.

Use the command `ps-despool' to send the spooled images to the printer." t nil)

(autoload (quote ps-spool-region-with-faces) "ps-print" "\
Generate a PostScript image of the region and spool locally.
Like `ps-spool-region', but includes font, color, and underline
information in the generated image.  This command works only if you
are using a window system, so it has a way to determine color values.

Use the command `ps-despool' to send the spooled images to the printer." t nil)

(autoload (quote ps-despool) "ps-print" "\
Send the spooled PostScript to the printer.

Interactively, when you use a prefix argument (C-u), the command
prompts the user for a file name, and saves the spooled PostScript
image in that file instead of sending it to the printer.

Noninteractively, the argument FILENAME is treated as follows: if it
is nil, send the image to the printer.  If FILENAME is a string, save
the PostScript image in a file with that name." t nil)

(autoload (quote ps-line-lengths) "ps-print" "\
Display the correspondence between a line length and a font size,
using the current ps-print setup.
Try: pr -t file | awk '{printf \"%3d %s
\", length($0), $0}' | sort -r | head" t nil)

(autoload (quote ps-nb-pages-buffer) "ps-print" "\
Display number of pages to print this buffer, for various font heights.
The table depends on the current ps-print setup." t nil)

(autoload (quote ps-nb-pages-region) "ps-print" "\
Display number of pages to print the region, for various font heights.
The table depends on the current ps-print setup." t nil)

(autoload (quote ps-setup) "ps-print" "\
Return the current PostScript-generation setup." nil nil)

(autoload (quote ps-extend-face-list) "ps-print" "\
Extend face in `ps-print-face-extension-alist'.

If optional MERGE-P is non-nil, extensions in FACE-EXTENSION-LIST are merged
with face extension in `ps-print-face-extension-alist'; otherwise, overrides.

The elements in FACE-EXTENSION-LIST is like those for `ps-extend-face'.

See `ps-extend-face' for documentation." nil nil)

(autoload (quote ps-extend-face) "ps-print" "\
Extend face in `ps-print-face-extension-alist'.

If optional MERGE-P is non-nil, extensions in FACE-EXTENSION list are merged
with face extensions in `ps-print-face-extension-alist'; otherwise, overrides.

The elements of FACE-EXTENSION list have the form:

   (FACE-NAME FOREGROUND BACKGROUND EXTENSION...)

FACE-NAME is a face name symbol.

FOREGROUND and BACKGROUND may be nil or a string that denotes the
foreground and background colors respectively.

EXTENSION is one of the following symbols:
   bold      - use bold font.
   italic    - use italic font.
   underline - put a line under text.
   strikeout - like underline, but the line is in middle of text.
   overline  - like underline, but the line is over the text.
   shadow    - text will have a shadow.
   box       - text will be surrounded by a box.
   outline   - print characters as hollow outlines.

If EXTENSION is any other symbol, it is ignored." nil nil)

;;;***

;;;### (autoloads (quail-update-leim-list-file quail-defrule-internal
;;;;;;  quail-defrule quail-install-map quail-define-rules quail-set-keyboard-layout
;;;;;;  quail-define-package quail-use-package) "quail" "international/quail.el"
;;;;;;  (14171 42620))
;;; Generated autoloads from international/quail.el

(autoload (quote quail-use-package) "quail" "\
Start using Quail package PACKAGE-NAME.
The remaining arguments are libraries to be loaded before using the package." nil nil)

(autoload (quote quail-define-package) "quail" "\
Define NAME as a new Quail package for input LANGUAGE.
TITLE is a string to be displayed at mode-line to indicate this package.
Optional arguments are GUIDANCE, DOCSTRING, TRANSLATION-KEYS,
 FORGET-LAST-SELECTION, DETERMINISTIC, KBD-TRANSLATE, SHOW-LAYOUT,
 CREATE-DECODE-MAP, MAXIMUM-SHORTEST, OVERLAY-PLIST,
 UPDATE-TRANSLATION-FUNCTION, CONVERSION-KEYS and SIMPLE.

GUIDANCE specifies how a guidance string is shown in echo area.
If it is t, list of all possible translations for the current key is shown
 with the currently selected translation being highlighted.
If it is an alist, the element has the form (CHAR . STRING).  Each character
 in the current key is searched in the list and the corresponding string is
 shown.
If it is nil, the current key is shown.

DOCSTRING is the documentation string of this package.

TRANSLATION-KEYS specifies additional key bindings used while translation
region is active.  It is an alist of single key character vs. corresponding
command to be called.

FORGET-LAST-SELECTION non-nil means a selected translation is not kept
for the future to translate the same key.  If this flag is nil, a
translation selected for a key is remembered so that it can be the
first candidate when the same key is entered later.

DETERMINISTIC non-nil means the first candidate of translation is
selected automatically without allowing users to select another
translation for a key.  In this case, unselected translations are of
no use for an interactive use of Quail but can be used by some other
programs.  If this flag is non-nil, FORGET-LAST-SELECTION is also set
to t.

KBD-TRANSLATE non-nil means input characters are translated from a
user's keyboard layout to the standard keyboard layout.  See the
documentation of `quail-keyboard-layout' and
`quail-keyboard-layout-standard' for more detail.

SHOW-LAYOUT non-nil means the `quail-help' command should show
the user's keyboard layout visually with translated characters.
If KBD-TRANSLATE is set, it is desirable to set also this flag unless
this package defines no translations for single character keys.

CREATE-DECODE-MAP non-nil means decode map is also created.  A decode
map is an alist of translations and corresponding original keys.
Although this map is not used by Quail itself, it can be used by some
other programs.  For instance, Vietnamese supporting needs this map to
convert Vietnamese text to VIQR format which uses only ASCII
characters to represent Vietnamese characters.

MAXIMUM-SHORTEST non-nil means break key sequence to get maximum
length of the shortest sequence.  When we don't have a translation of
key \"..ABCD\" but have translations of \"..AB\" and \"CD..\", break
the key at \"..AB\" and start translation of \"CD..\".  Hangul
packages, for instance, use this facility.  If this flag is nil, we
break the key just at \"..ABC\" and start translation of \"D..\".

OVERLAY-PLIST if non-nil is a property list put on an overlay which
covers Quail translation region.

UPDATE-TRANSLATION-FUNCTION if non-nil is a function to call to update
the current translation region according to a new translation data.  By
default, a translated text or a user's key sequence (if no translation
for it) is inserted.

CONVERSION-KEYS specifies additional key bindings used while
conversion region is active.  It is an alist of single key character
vs. corresponding command to be called.

If SIMPLE is non-nil, then we do not alter the meanings of
commands such as C-f, C-b, C-n, C-p and TAB; they are treated as
non-Quail commands." nil nil)

(autoload (quote quail-set-keyboard-layout) "quail" "\
Set the current keyboard layout to the same as keyboard KBD-TYPE.

Since some Quail packages depends on a physical layout of keys (not
characters generated by them), those are created by assuming the
standard layout defined in `quail-keyboard-layout-standard'.  This
function tells Quail system the layout of your keyboard so that what
you type is correctly handled." t nil)

(autoload (quote quail-define-rules) "quail" "\
Define translation rules of the current Quail package.
Each argument is a list of KEY and TRANSLATION.
KEY is a string meaning a sequence of keystrokes to be translated.
TRANSLATION is a character, a string, a vector, a Quail map, or a function.
If it is a character, it is the sole translation of KEY.
If it is a string, each character is a candidate for the translation.
If it is a vector, each element (string or character) is a candidate
  for the translation.
In these cases, a key specific Quail map is generated and assigned to KEY.

If TRANSLATION is a Quail map or a function symbol which returns a Quail map,
 it is used to handle KEY." nil (quote macro))

(autoload (quote quail-install-map) "quail" "\
Install the Quail map MAP in the current Quail package.
The installed map can be referred by the function `quail-map'." nil nil)

(autoload (quote quail-defrule) "quail" "\
Add one translation rule, KEY to TRANSLATION, in the current Quail package.
KEY is a string meaning a sequence of keystrokes to be translated.
TRANSLATION is a character, a string, a vector, a Quail map,
 a function, or a cons.
It it is a character, it is the sole translation of KEY.
If it is a string, each character is a candidate for the translation.
If it is a vector, each element (string or character) is a candidate
 for the translation.
If it is a cons, the car is one of the above and the cdr is a function
 to call when translating KEY (the return value is assigned to the
 variable `quail-current-data').  If the cdr part is not a function,
 the value itself is assigned to `quail-current-data'.
In these cases, a key specific Quail map is generated and assigned to KEY.

If TRANSLATION is a Quail map or a function symbol which returns a Quail map,
 it is used to handle KEY.

Optional 3rd argument NAME, if specified, says which Quail package
to define this translation rule in.  The default is to define it in the
current Quail package.

Optional 4th argument APPEND, if non-nil, appends TRANSLATION
to the current translations for KEY instead of replacing them." nil nil)

(autoload (quote quail-defrule-internal) "quail" "\
Define KEY as TRANS in a Quail map MAP." nil nil)

(autoload (quote quail-update-leim-list-file) "quail" "\
Update entries for Quail packages in `LEIM' list file in directory DIRNAME.
DIRNAME is a directory containing Emacs input methods;
normally, it should specify the `leim' subdirectory
of the Emacs source tree.

It searches for Quail packages under `quail' subdirectory of DIRNAME,
and update the file \"leim-list.el\" in DIRNAME.

When called from a program, the remaining arguments are additional
directory names to search for Quail packages under `quail' subdirectory
of each directory." t nil)

;;;***

;;;### (autoloads (quickurl-list quickurl-list-mode quickurl-edit-urls
;;;;;;  quickurl-browse-url-ask quickurl-browse-url quickurl-add-url
;;;;;;  quickurl-ask quickurl) "quickurl" "../../../../gd/gnu/emacs/lisp/quickurl.el"
;;;;;;  (14286 57107))
;;; Generated autoloads from ../../../../gd/gnu/emacs/lisp/quickurl.el

(defconst quickurl-reread-hook-postfix "\n;; Local Variables:\n;; eval: (progn (require 'quickurl) (add-hook 'local-write-file-hooks (lambda () (quickurl-read) nil)))\n;; End:\n" "\
Example `quickurl-postfix' text that adds a local variable to the
`quickurl-url-file' so that if you edit it by hand it will ensure that
`quickurl-urls' is updated with the new URL list.

To make use of this do something like:

  (setq quickurl-postfix quickurl-reread-hook-postfix)

in your ~/.emacs (after loading/requiring quickurl).")

(autoload (quote quickurl) "quickurl" "Insert an URL based on LOOKUP.\n\nIf not supplied LOOKUP is taken to be the word at point in the current\nbuffer, this default action can be modifed via\n`quickurl-grab-lookup-function'." t nil)

(autoload (quote quickurl-ask) "quickurl" "\
Insert an URL, with `completing-read' prompt, based on LOOKUP." t nil)

(autoload (quote quickurl-add-url) "quickurl" "\
Allow the user to interactively add a new URL associated with WORD.

See `quickurl-grab-url' for details on how the default word/url combination
is decided." t nil)

(autoload (quote quickurl-browse-url) "quickurl" "Browse the URL associated with LOOKUP.\n\nIf not supplied LOOKUP is taken to be the word at point in the\ncurrent buffer, this default action can be modifed via\n`quickurl-grab-lookup-function'." t nil)

(autoload (quote quickurl-browse-url-ask) "quickurl" "\
Browse the URL, with `completing-read' prompt, associated with LOOKUP." t nil)

(autoload (quote quickurl-edit-urls) "quickurl" "\
Pull `quickurl-url-file' into a buffer for hand editing." t nil)

(autoload (quote quickurl-list-mode) "quickurl" "\
A mode for browsing the quickurl URL list.

The key bindings for `quickurl-list-mode' are:

\\{quickurl-list-mode-map}" t nil)

(autoload (quote quickurl-list) "quickurl" "\
Display `quickurl-list' as a formatted list using `quickurl-list-mode'." t nil)

;;;***

;;;### (autoloads (remote-compile) "rcompile" "rcompile.el" (13149
;;;;;;  16506))
;;; Generated autoloads from rcompile.el

(autoload (quote remote-compile) "rcompile" "\
Compile the the current buffer's directory on HOST.  Log in as USER.
See \\[compile]." t nil)

;;;***

;;;### (autoloads (clear-rectangle string-rectangle delete-whitespace-rectangle
;;;;;;  open-rectangle insert-rectangle yank-rectangle kill-rectangle
;;;;;;  extract-rectangle delete-extract-rectangle delete-rectangle
;;;;;;  move-to-column-force) "rect" "rect.el" (13926 23562))
;;; Generated autoloads from rect.el

(autoload (quote move-to-column-force) "rect" "\
Move point to column COLUMN rigidly in the current line.
If COLUMN is within a multi-column character, replace it by
spaces and tab." nil nil)

(autoload (quote delete-rectangle) "rect" "\
Delete (don't save) text in rectangle with point and mark as corners.
The same range of columns is deleted in each line starting with the line
where the region begins and ending with the line where the region ends." t nil)

(autoload (quote delete-extract-rectangle) "rect" "\
Delete contents of rectangle and return it as a list of strings.
Arguments START and END are the corners of the rectangle.
The value is list of strings, one for each line of the rectangle." nil nil)

(autoload (quote extract-rectangle) "rect" "\
Return contents of rectangle with corners at START and END.
Value is list of strings, one for each line of the rectangle." nil nil)

(autoload (quote kill-rectangle) "rect" "\
Delete rectangle with corners at point and mark; save as last killed one.
Calling from program, supply two args START and END, buffer positions.
But in programs you might prefer to use `delete-extract-rectangle'." t nil)

(autoload (quote yank-rectangle) "rect" "\
Yank the last killed rectangle with upper left corner at point." t nil)

(autoload (quote insert-rectangle) "rect" "\
Insert text of RECTANGLE with upper left corner at point.
RECTANGLE's first line is inserted at point, its second
line is inserted at a point vertically under point, etc.
RECTANGLE should be a list of strings.
After this command, the mark is at the upper left corner
and point is at the lower right corner." nil nil)

(autoload (quote open-rectangle) "rect" "\
Blank out rectangle with corners at point and mark, shifting text right.
The text previously in the region is not overwritten by the blanks,
but instead winds up to the right of the rectangle." t nil)
 (defalias 'close-rectangle 'delete-whitespace-rectangle) ;; Old name

(autoload (quote delete-whitespace-rectangle) "rect" "\
Delete all whitespace following a specified column in each line.
The left edge of the rectangle specifies the position in each line
at which whitespace deletion should begin.  On each line in the
rectangle, all continuous whitespace starting at that column is deleted." t nil)

(autoload (quote string-rectangle) "rect" "\
Replace rectangle contents with STRING on each line.
The length of STRING need not be the same as the rectangle width.

Called from a program, takes three args; START, END and STRING." t nil)

(autoload (quote clear-rectangle) "rect" "\
Blank out rectangle with corners at point and mark.
The text previously in the region is overwritten by the blanks.
When called from a program, requires two args which specify the corners." t nil)

;;;***

;;;### (autoloads (reftex-citation reftex-mode turn-on-reftex) "reftex"
;;;;;;  "textmodes/reftex.el" (14198 46022))
;;; Generated autoloads from textmodes/reftex.el

(autoload (quote turn-on-reftex) "reftex" "\
Turn on RefTeX mode." nil nil)

(autoload (quote reftex-mode) "reftex" "\
Minor mode with distinct support for \\label, \\ref and \\cite in LaTeX.

Labels can be created with `\\[reftex-label]' and referenced with `\\[reftex-reference]'.
When referencing, you get a menu with all labels of a given type and
context of the label definition.  The selected label is inserted as a
\\ref macro.

Citations can be made with `\\[reftex-citation]' which will use a regular expression
to pull out a *formatted* list of articles from your BibTeX
database.  The selected citation is inserted as a \\cite macro.

A Table of Contents of the entire (multifile) document with browsing
capabilities is available with `\\[reftex-toc]'.

Most command have help available on the fly.  This help is accessed by
pressing `?' to any prompt mentioning this feature.

Extensive documentation about RefTeX is available in Info format.
You can view this information with `\\[reftex-info]'.

\\{reftex-mode-map}
Under X, these and other functions will also be available as `Ref' menu
on the menu bar.

------------------------------------------------------------------------------" t nil)

(autoload (quote reftex-citation) "reftex" "\
Make a citation using BibTeX database files.
After prompting for a regular expression, scans the buffers with
bibtex entries (taken from the \\bibliography command) and offers the
matching entries for selection.  The selected entry is formated according
to `reftex-cite-format' and inserted into the buffer.

If NO-INSERT is non-nil, nothing is inserted, only the selected key returned.

When called with one or two `C-u' prefixes, first rescans the document.
When called with a numeric prefix, make that many citations.  When
called with point inside the braces of a `cite' command, it will
add another key, ignoring the value of `reftex-cite-format'.

The regular expression uses an expanded syntax: && is interpreted as `and'.
Thus, `aaaa&&bbb' matches entries which contain both `aaaa' and `bbb'.
While entering the regexp, completion on knows citation keys is possible.
`=' is a good regular expression to match all entries in all files." t nil)

;;;***

;;;### (autoloads (regexp-opt-depth regexp-opt) "regexp-opt" "emacs-lisp/regexp-opt.el"
;;;;;;  (14168 7138))
;;; Generated autoloads from emacs-lisp/regexp-opt.el

(autoload (quote regexp-opt) "regexp-opt" "\
Return a regexp to match a string in STRINGS.
Each string should be unique in STRINGS and should not contain any regexps,
quoted or not.  If optional PAREN is non-nil, ensure that the returned regexp
is enclosed by at least one regexp grouping construct.
The returned regexp is typically more efficient than the equivalent regexp:

 (let ((open-paren (if PAREN \"\\\\(\" \"\")) (close-paren (if PAREN \"\\\\)\" \"\")))
   (concat open-paren (mapconcat 'regexp-quote STRINGS \"\\\\|\") close-paren))

but typically contains more regexp grouping constructs.
Use `regexp-opt-depth' to count them." nil nil)

(autoload (quote regexp-opt-depth) "regexp-opt" "\
Return the depth of REGEXP.
This means the number of regexp grouping constructs (parenthesised expressions)
in REGEXP." nil nil)

;;;***

;;;### (autoloads (repeat) "repeat" "repeat.el" (14081 4553))
;;; Generated autoloads from repeat.el

(autoload (quote repeat) "repeat" "\
Repeat most recently executed command.
With prefix arg, apply new prefix arg to that command; otherwise, use
the prefix arg that was used before (if any).
This command is like the `.' command in the vi editor.

If this command is invoked by a multi-character key sequence, it can then
be repeated by repeating the final character of that sequence.  This behavior
can be modified by the global variable `repeat-on-final-keystroke'." t nil)

;;;***

;;;### (autoloads (reporter-submit-bug-report) "reporter" "mail/reporter.el"
;;;;;;  (13651 32636))
;;; Generated autoloads from mail/reporter.el

(autoload (quote reporter-submit-bug-report) "reporter" nil nil nil)

;;;***

;;;### (autoloads (reposition-window) "reposition" "reposition.el"
;;;;;;  (13229 29313))
;;; Generated autoloads from reposition.el

(autoload (quote reposition-window) "reposition" "\
Make the current definition and/or comment visible.
Further invocations move it to the top of the window or toggle the
visibility of comments that precede it.
  Point is left unchanged unless prefix ARG is supplied.
  If the definition is fully onscreen, it is moved to the top of the
window.  If it is partly offscreen, the window is scrolled to get the
definition (or as much as will fit) onscreen, unless point is in a comment
which is also partly offscreen, in which case the scrolling attempts to get
as much of the comment onscreen as possible.
  Initially `reposition-window' attempts to make both the definition and
preceding comments visible.  Further invocations toggle the visibility of
the comment lines.
  If ARG is non-nil, point may move in order to make the whole defun
visible (if only part could otherwise be made so), to make the defun line
visible (if point is in code and it could not be made so, or if only
comments, including the first comment line, are visible), or to make the
first comment line visible (if point is in a comment)." t nil)
 (define-key esc-map "\C-l" 'reposition-window)

;;;***

;;;### (autoloads (resume-suspend-hook) "resume" "resume.el" (12679
;;;;;;  50658))
;;; Generated autoloads from resume.el

(autoload (quote resume-suspend-hook) "resume" "\
Clear out the file used for transmitting args when Emacs resumes." nil nil)

;;;***

;;;### (autoloads (make-ring ring-p) "ring" "emacs-lisp/ring.el"
;;;;;;  (13107 22379))
;;; Generated autoloads from emacs-lisp/ring.el

(autoload (quote ring-p) "ring" "\
Returns t if X is a ring; nil otherwise." nil nil)

(autoload (quote make-ring) "ring" "\
Make a ring that can contain SIZE elements." nil nil)

;;;***

;;;### (autoloads (rlogin) "rlogin" "rlogin.el" (13845 29546))
;;; Generated autoloads from rlogin.el
 (add-hook 'same-window-regexps "^\\*rlogin-.*\\*\\(\\|<[0-9]+>\\)")

(autoload (quote rlogin) "rlogin" "\
Open a network login connection via `rlogin' with args INPUT-ARGS.
INPUT-ARGS should start with a host name; it may also contain
other arguments for `rlogin'.

Input is sent line-at-a-time to the remote connection.

Communication with the remote host is recorded in a buffer `*rlogin-HOST*'
\(or `*rlogin-USER@HOST*' if the remote username differs).
If a prefix argument is given and the buffer `*rlogin-HOST*' already exists,
a new buffer with a different connection will be made.

When called from a program, if the optional second argument BUFFER is
a string or buffer, it specifies the buffer to use.

The variable `rlogin-program' contains the name of the actual program to
run.  It can be a relative or absolute path.

The variable `rlogin-explicit-args' is a list of arguments to give to
the rlogin when starting.  They are added after any arguments given in
INPUT-ARGS.

If the default value of `rlogin-directory-tracking-mode' is t, then the
default directory in that buffer is set to a remote (FTP) file name to
access your home directory on the remote machine.  Occasionally this causes
an error, if you cannot access the home directory on that machine.  This
error is harmless as long as you don't try to use that default directory.

If `rlogin-directory-tracking-mode' is neither t nor nil, then the default
directory is initially set up to your (local) home directory.
This is useful if the remote machine and your local machine
share the same files via NFS.  This is the default.

If you wish to change directory tracking styles during a session, use the
function `rlogin-directory-tracking-mode' rather than simply setting the
variable." t nil)

;;;***

;;;### (autoloads (rmail-set-pop-password rmail-input rmail-mode
;;;;;;  rmail rmail-enable-mime rmail-secondary-file-regexp rmail-secondary-file-directory
;;;;;;  rmail-mail-new-frame rmail-primary-inbox-list rmail-delete-after-output
;;;;;;  rmail-highlight-face rmail-highlighted-headers rmail-retry-ignored-headers
;;;;;;  rmail-displayed-headers rmail-ignored-headers rmail-dont-reply-to-names)
;;;;;;  "rmail" "mail/rmail.el" (14211 33252))
;;; Generated autoloads from mail/rmail.el

(defvar rmail-dont-reply-to-names nil "\
*A regexp specifying names to prune of reply to messages.
A value of nil means exclude your own login name as an address
plus whatever is specified by `rmail-default-dont-reply-to-names'.")

(defvar rmail-default-dont-reply-to-names "info-" "\
A regular expression specifying part of the value of the default value of
the variable `rmail-dont-reply-to-names', for when the user does not set
`rmail-dont-reply-to-names' explicitly.  (The other part of the default
value is the user's name.)
It is useful to set this variable in the site customization file.")

(defvar rmail-ignored-headers "^via:\\|^mail-from:\\|^origin:\\|^references:\\|^status:\\|^received:\\|^x400-originator:\\|^x400-recipients:\\|^x400-received:\\|^x400-mts-identifier:\\|^x400-content-type:\\|^\\(resent-\\|\\)message-id:\\|^summary-line:\\|^resent-date:\\|^nntp-posting-host:\\|^path:\\|^x-char.*:\\|^x-face:\\|^x-mailer:\\|^delivered-to:\\|^lines:\\|^mime-version:\\|^content-transfer-encoding:\\|^x-coding-system:\\|^return-path:\\|^errors-to:\\|^return-receipt-to:\\|^x-attribution:\\|^x-disclaimer:" "\
*Regexp to match header fields that Rmail should normally hide.
This variable is used for reformatting the message header,
which normally happens once for each message,
when you view the message for the first time in Rmail.
To make a change in this variable take effect
for a message that you have already viewed,
go to that message and type \\[rmail-toggle-header] twice.")

(defvar rmail-displayed-headers nil "\
*Regexp to match Header fields that Rmail should display.
If nil, display all header fields except those matched by
`rmail-ignored-headers'.")

(defvar rmail-retry-ignored-headers nil "\
*Headers that should be stripped when retrying a failed message.")

(defvar rmail-highlighted-headers "^From:\\|^Subject:" "\
*Regexp to match Header fields that Rmail should normally highlight.
A value of nil means don't highlight.
See also `rmail-highlight-face'.")

(defvar rmail-highlight-face nil "\
*Face used by Rmail for highlighting headers.")

(defvar rmail-delete-after-output nil "\
*Non-nil means automatically delete a message that is copied to a file.")

(defvar rmail-primary-inbox-list nil "\
*List of files which are inboxes for user's primary mail file `~/RMAIL'.
`nil' means the default, which is (\"/usr/spool/mail/$USER\")
\(the name varies depending on the operating system,
and the value of the environment variable MAIL overrides it).")

(defvar rmail-mail-new-frame nil "\
*Non-nil means Rmail makes a new frame for composing outgoing mail.")

(defvar rmail-secondary-file-directory "~/" "\
*Directory for additional secondary Rmail files.")

(defvar rmail-secondary-file-regexp "\\.xmail$" "\
*Regexp for which files are secondary Rmail files.")

(defvar rmail-mode-hook nil "\
List of functions to call when Rmail is invoked.")

(defvar rmail-get-new-mail-hook nil "\
List of functions to call when Rmail has retrieved new mail.")

(defvar rmail-show-message-hook nil "\
List of functions to call when Rmail displays a message.")

(defvar rmail-delete-message-hook nil "\
List of functions to call when Rmail deletes a message.
When the hooks are called, the message has been marked deleted but is
still the current message in the Rmail buffer.")

(defvar rmail-file-coding-system nil "\
Coding system used in RMAIL file.

This is set to nil by default.")

(defvar rmail-enable-mime nil "\
*If non-nil, RMAIL uses MIME feature.
If the value is t, RMAIL automatically shows MIME decoded message.
If the value is neither t nor nil, RMAIL does not show MIME decoded message
until a user explicitly requires it.")

(defvar rmail-show-mime-function nil "\
Function to show MIME decoded message of RMAIL file.")

(defvar rmail-mime-feature (quote rmail-mime) "\
Feature to require to load MIME support in Rmail.
When starting Rmail, if `rmail-enable-mime' is non-nil,
this feature is required with `require'.")

(defvar rmail-decode-mime-charset t "\
*Non-nil means a message is decoded by MIME's charset specification.
If this variable is nil, or the message has not MIME specification,
the message is decoded as normal way.

If the variable `rmail-enable-mime' is non-nil, this variables is
ignored, and all the decoding work is done by a feature specified by
the variable `rmail-mime-feature'.")

(defvar rmail-mime-charset-pattern "^content-type:[ ]*text/plain;[ 	\n]*charset=\"?\\([^ 	\n\"]+\\)\"?" "\
Regexp to match MIME-charset specification in a header of message.
The first parenthesized expression should match the MIME-charset name.")

(autoload (quote rmail) "rmail" "\
Read and edit incoming mail.
Moves messages into file named by `rmail-file-name' (a babyl format file)
 and edits that file in RMAIL Mode.
Type \\[describe-mode] once editing that file, for a list of RMAIL commands.

May be called with file name as argument; then performs rmail editing on
that file, but does not copy any new mail into the file.
Interactively, if you supply a prefix argument, then you
have a chance to specify a file name with the minibuffer.

If `rmail-display-summary' is non-nil, make a summary for this RMAIL file." t nil)

(autoload (quote rmail-mode) "rmail" "\
Rmail Mode is used by \\<rmail-mode-map>\\[rmail] for editing Rmail files.
All normal editing commands are turned off.
Instead, these commands are available:

\\[rmail-beginning-of-message]	Move point to front of this message (same as \\[beginning-of-buffer]).
\\[scroll-up]	Scroll to next screen of this message.
\\[scroll-down]	Scroll to previous screen of this message.
\\[rmail-next-undeleted-message]	Move to Next non-deleted message.
\\[rmail-previous-undeleted-message]	Move to Previous non-deleted message.
\\[rmail-next-message]	Move to Next message whether deleted or not.
\\[rmail-previous-message]	Move to Previous message whether deleted or not.
\\[rmail-first-message]	Move to the first message in Rmail file.
\\[rmail-last-message]	Move to the last message in Rmail file.
\\[rmail-show-message]	Jump to message specified by numeric position in file.
\\[rmail-search]	Search for string and show message it is found in.
\\[rmail-delete-forward]	Delete this message, move to next nondeleted.
\\[rmail-delete-backward]	Delete this message, move to previous nondeleted.
\\[rmail-undelete-previous-message]	Undelete message.  Tries current message, then earlier messages
	till a deleted message is found.
\\[rmail-edit-current-message]	Edit the current message.  \\[rmail-cease-edit] to return to Rmail.
\\[rmail-expunge]	Expunge deleted messages.
\\[rmail-expunge-and-save]	Expunge and save the file.
\\[rmail-quit]       Quit Rmail: expunge, save, then switch to another buffer.
\\[save-buffer] Save without expunging.
\\[rmail-get-new-mail]	Move new mail from system spool directory into this file.
\\[rmail-mail]	Mail a message (same as \\[mail-other-window]).
\\[rmail-continue]	Continue composing outgoing message started before.
\\[rmail-reply]	Reply to this message.  Like \\[rmail-mail] but initializes some fields.
\\[rmail-retry-failure]	Send this message again.  Used on a mailer failure message.
\\[rmail-forward]	Forward this message to another user.
\\[rmail-output-to-rmail-file]       Output this message to an Rmail file (append it).
\\[rmail-output]	Output this message to a Unix-format mail file (append it).
\\[rmail-output-body-to-file]	Save message body to a file.  Default filename comes from Subject line.
\\[rmail-input]	Input Rmail file.  Run Rmail on that file.
\\[rmail-add-label]	Add label to message.  It will be displayed in the mode line.
\\[rmail-kill-label]	Kill label.  Remove a label from current message.
\\[rmail-next-labeled-message]   Move to Next message with specified label
          (label defaults to last one specified).
          Standard labels: filed, unseen, answered, forwarded, deleted.
          Any other label is present only if you add it with \\[rmail-add-label].
\\[rmail-previous-labeled-message]   Move to Previous message with specified label
\\[rmail-summary]	Show headers buffer, with a one line summary of each message.
\\[rmail-summary-by-labels]	Summarize only messages with particular label(s).
\\[rmail-summary-by-recipients]   Summarize only messages with particular recipient(s).
\\[rmail-summary-by-regexp]   Summarize only messages with particular regexp(s).
\\[rmail-summary-by-topic]   Summarize only messages with subject line regexp(s).
\\[rmail-toggle-header]	Toggle display of complete header." t nil)

(autoload (quote rmail-input) "rmail" "\
Run Rmail on file FILENAME." t nil)

(autoload (quote rmail-set-pop-password) "rmail" "\
Set PASSWORD to be used for retrieving mail from a POP server." t nil)

;;;***

;;;### (autoloads (rmail-edit-current-message) "rmailedit" "mail/rmailedit.el"
;;;;;;  (13738 33868))
;;; Generated autoloads from mail/rmailedit.el

(autoload (quote rmail-edit-current-message) "rmailedit" "\
Edit the contents of this message." t nil)

;;;***

;;;### (autoloads (rmail-next-labeled-message rmail-previous-labeled-message
;;;;;;  rmail-read-label rmail-kill-label rmail-add-label) "rmailkwd"
;;;;;;  "mail/rmailkwd.el" (12875 8065))
;;; Generated autoloads from mail/rmailkwd.el

(autoload (quote rmail-add-label) "rmailkwd" "\
Add LABEL to labels associated with current RMAIL message.
Completion is performed over known labels when reading." t nil)

(autoload (quote rmail-kill-label) "rmailkwd" "\
Remove LABEL from labels associated with current RMAIL message.
Completion is performed over known labels when reading." t nil)

(autoload (quote rmail-read-label) "rmailkwd" nil nil nil)

(autoload (quote rmail-previous-labeled-message) "rmailkwd" "\
Show previous message with one of the labels LABELS.
LABELS should be a comma-separated list of label names.
If LABELS is empty, the last set of labels specified is used.
With prefix argument N moves backward N messages with these labels." t nil)

(autoload (quote rmail-next-labeled-message) "rmailkwd" "\
Show next message with one of the labels LABELS.
LABELS should be a comma-separated list of label names.
If LABELS is empty, the last set of labels specified is used.
With prefix argument N moves forward N messages with these labels." t nil)

;;;***

;;;### (autoloads (set-rmail-inbox-list) "rmailmsc" "mail/rmailmsc.el"
;;;;;;  (13772 51036))
;;; Generated autoloads from mail/rmailmsc.el

(autoload (quote set-rmail-inbox-list) "rmailmsc" "\
Set the inbox list of the current RMAIL file to FILE-NAME.
You can specify one file name, or several names separated by commas.
If FILE-NAME is empty, remove any existing inbox list." t nil)

;;;***

;;;### (autoloads (rmail-output-body-to-file rmail-output rmail-fields-not-to-output
;;;;;;  rmail-output-to-rmail-file rmail-output-file-alist) "rmailout"
;;;;;;  "mail/rmailout.el" (14179 6366))
;;; Generated autoloads from mail/rmailout.el

(defvar rmail-output-file-alist nil "\
*Alist matching regexps to suggested output Rmail files.
This is a list of elements of the form (REGEXP . NAME-EXP).
The suggestion is taken if REGEXP matches anywhere in the message buffer.
NAME-EXP may be a string constant giving the file name to use,
or more generally it may be any kind of expression that returns
a file name as a string.")

(autoload (quote rmail-output-to-rmail-file) "rmailout" "\
Append the current message to an Rmail file named FILE-NAME.
If the file does not exist, ask if it should be created.
If file is being visited, the message is appended to the Emacs
buffer visiting that file.
If the file exists and is not an Rmail file, the message is
appended in inbox format, the same way `rmail-output' does it.

The default file name comes from `rmail-default-rmail-file',
which is updated to the name you use in this command.

A prefix argument N says to output N consecutive messages
starting with the current one.  Deleted messages are skipped and don't count." t nil)

(defvar rmail-fields-not-to-output nil "\
*Regexp describing fields to exclude when outputting a message to a file.")

(autoload (quote rmail-output) "rmailout" "\
Append this message to system-inbox-format mail file named FILE-NAME.
A prefix argument N says to output N consecutive messages
starting with the current one.  Deleted messages are skipped and don't count.
When called from lisp code, N may be omitted.

If the pruned message header is shown on the current message, then
messages will be appended with pruned headers; otherwise, messages
will be appended with their original headers.

The default file name comes from `rmail-default-file',
which is updated to the name you use in this command.

The optional third argument NOATTRIBUTE, if non-nil, says not
to set the `filed' attribute, and not to display a message.

The optional fourth argument FROM-GNUS is set when called from GNUS." t nil)

(autoload (quote rmail-output-body-to-file) "rmailout" "\
Write this message body to the file FILE-NAME.
FILE-NAME defaults, interactively, from the Subject field of the message." t nil)

;;;***

;;;### (autoloads (rmail-sort-by-keywords rmail-sort-by-lines rmail-sort-by-correspondent
;;;;;;  rmail-sort-by-recipient rmail-sort-by-author rmail-sort-by-subject
;;;;;;  rmail-sort-by-date) "rmailsort" "mail/rmailsort.el" (13051
;;;;;;  48729))
;;; Generated autoloads from mail/rmailsort.el

(autoload (quote rmail-sort-by-date) "rmailsort" "\
Sort messages of current Rmail file by date.
If prefix argument REVERSE is non-nil, sort them in reverse order." t nil)

(autoload (quote rmail-sort-by-subject) "rmailsort" "\
Sort messages of current Rmail file by subject.
If prefix argument REVERSE is non-nil, sort them in reverse order." t nil)

(autoload (quote rmail-sort-by-author) "rmailsort" "\
Sort messages of current Rmail file by author.
If prefix argument REVERSE is non-nil, sort them in reverse order." t nil)

(autoload (quote rmail-sort-by-recipient) "rmailsort" "\
Sort messages of current Rmail file by recipient.
If prefix argument REVERSE is non-nil, sort them in reverse order." t nil)

(autoload (quote rmail-sort-by-correspondent) "rmailsort" "\
Sort messages of current Rmail file by other correspondent.
If prefix argument REVERSE is non-nil, sort them in reverse order." t nil)

(autoload (quote rmail-sort-by-lines) "rmailsort" "\
Sort messages of current Rmail file by number of lines.
If prefix argument REVERSE is non-nil, sort them in reverse order." t nil)

(autoload (quote rmail-sort-by-keywords) "rmailsort" "\
Sort messages of current Rmail file by labels.
If prefix argument REVERSE is non-nil, sort them in reverse order.
KEYWORDS is a comma-separated list of labels." t nil)

;;;***

;;;### (autoloads (rmail-summary-line-decoder rmail-summary-by-senders
;;;;;;  rmail-summary-by-topic rmail-summary-by-regexp rmail-summary-by-recipients
;;;;;;  rmail-summary-by-labels rmail-summary rmail-summary-line-count-flag
;;;;;;  rmail-summary-scroll-between-messages) "rmailsum" "mail/rmailsum.el"
;;;;;;  (14181 58887))
;;; Generated autoloads from mail/rmailsum.el

(defvar rmail-summary-scroll-between-messages t "\
*Non-nil means Rmail summary scroll commands move between messages.")

(defvar rmail-summary-line-count-flag t "\
*Non-nil if Rmail summary should show the number of lines in each message.")

(autoload (quote rmail-summary) "rmailsum" "\
Display a summary of all messages, one line per message." t nil)

(autoload (quote rmail-summary-by-labels) "rmailsum" "\
Display a summary of all messages with one or more LABELS.
LABELS should be a string containing the desired labels, separated by commas." t nil)

(autoload (quote rmail-summary-by-recipients) "rmailsum" "\
Display a summary of all messages with the given RECIPIENTS.
Normally checks the To, From and Cc fields of headers;
but if PRIMARY-ONLY is non-nil (prefix arg given),
 only look in the To and From fields.
RECIPIENTS is a string of regexps separated by commas." t nil)

(autoload (quote rmail-summary-by-regexp) "rmailsum" "\
Display a summary of all messages according to regexp REGEXP.
If the regular expression is found in the header of the message
\(including in the date and other lines, as well as the subject line),
Emacs will list the header line in the RMAIL-summary." t nil)

(autoload (quote rmail-summary-by-topic) "rmailsum" "\
Display a summary of all messages with the given SUBJECT.
Normally checks the Subject field of headers;
but if WHOLE-MESSAGE is non-nil (prefix arg given), 
 look in the whole message.
SUBJECT is a string of regexps separated by commas." t nil)

(autoload (quote rmail-summary-by-senders) "rmailsum" "\
Display a summary of all messages with the given SENDERS.
SENDERS is a string of names separated by commas." t nil)

(defvar rmail-summary-line-decoder (function identity) "\
*Function to decode summary-line.

By default, `identity' is set.")

;;;***

;;;### (autoloads (news-post-news) "rnewspost" "mail/rnewspost.el"
;;;;;;  (13668 48323))
;;; Generated autoloads from mail/rnewspost.el

(autoload (quote news-post-news) "rnewspost" "\
Begin editing a new USENET news article to be posted.
Type \\[describe-mode] once editing the article to get a list of commands.
If NOQUERY is non-nil, we do not query before doing the work." t nil)

;;;***

;;;### (autoloads (toggle-rot13-mode rot13-other-window) "rot13"
;;;;;;  "rot13.el" (13253 16866))
;;; Generated autoloads from rot13.el

(autoload (quote rot13-other-window) "rot13" "\
Display current buffer in rot 13 in another window.
To terminate the rot13 display, delete that window." t nil)

(autoload (quote toggle-rot13-mode) "rot13" "\
Toggle the use of rot 13 encoding for the current window." t nil)

;;;***

;;;### (autoloads (resize-minibuffer-mode resize-minibuffer-frame-exactly
;;;;;;  resize-minibuffer-frame-max-height resize-minibuffer-frame
;;;;;;  resize-minibuffer-window-exactly resize-minibuffer-window-max-height
;;;;;;  resize-minibuffer-mode) "rsz-mini" "rsz-mini.el" (14118 2329))
;;; Generated autoloads from rsz-mini.el

(defvar resize-minibuffer-mode nil "\
Toggle resizing the minibuffer so its entire contents are visible.
Setting this variable directly does not take effect;
use either \\[customize] or the function `resize-minibuffer-mode'.")

(custom-add-to-group (quote resize-minibuffer) (quote resize-minibuffer-mode) (quote custom-variable))

(custom-add-load (quote resize-minibuffer-mode) (quote rsz-mini))

(defvar resize-minibuffer-window-max-height nil "\
*Maximum size the minibuffer window is allowed to become.
If less than 1 or not a number, the limit is the height of the frame in
which the active minibuffer window resides.")

(defvar resize-minibuffer-window-exactly t "\
*Allow making minibuffer exactly the size to display all its contents.
If `nil', the minibuffer window can temporarily increase in size but
never get smaller while it is active.  Any other value allows exact
resizing.")

(defvar resize-minibuffer-frame nil "\
*Allow changing the frame height of minibuffer frames.
If non-`nil' and the active minibuffer is the sole window in its frame,
allow changing the frame height.")

(defvar resize-minibuffer-frame-max-height nil "\
*Maximum size the minibuffer frame is allowed to become.
If less than 1 or not a number, there is no limit.")

(defvar resize-minibuffer-frame-exactly t "\
*Allow making minibuffer frame exactly the size to display all its contents.
If `nil', the minibuffer frame can temporarily increase in size but
never get smaller while it is active.  Any other value allows exact
resizing.")

(autoload (quote resize-minibuffer-mode) "rsz-mini" "\
Toggle resize-minibuffer mode.
With argument, enable resize-minibuffer mode if and only if argument
is positive.

When this minor mode is enabled, the minibuffer is dynamically resized to
contain the entire region of text put in it as you type.

The variable `resize-minibuffer-mode' is set to t or nil depending on
whether this mode is active or not.

The maximum height to which the minibuffer can grow is controlled by the
variable `resize-minibuffer-window-max-height'.

The variable `resize-minibuffer-window-exactly' determines whether the
minibuffer window should ever be shrunk to make it no larger than needed to
display its contents.

When using a window system, it is possible for a minibuffer to be the sole
window in a frame.  Since that window is already its maximum size, the only
way to make more text visible at once is to increase the size of the frame.
The variable `resize-minibuffer-frame' controls whether this should be
done.  The variables `resize-minibuffer-frame-max-height' and
`resize-minibuffer-frame-exactly' are analogous to their window
counterparts." t nil)

;;;***

;;;### (autoloads (dsssl-mode scheme-mode) "scheme" "progmodes/scheme.el"
;;;;;;  (13730 29898))
;;; Generated autoloads from progmodes/scheme.el

(autoload (quote scheme-mode) "scheme" "\
Major mode for editing Scheme code.
Editing commands are similar to those of lisp-mode.

In addition, if an inferior Scheme process is running, some additional
commands will be defined, for evaluating expressions and controlling
the interpreter, and the state of the process will be displayed in the
modeline of all Scheme buffers.  The names of commands that interact
with the Scheme process start with \"xscheme-\".  For more information
see the documentation for xscheme-interaction-mode.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{scheme-mode-map}
Entry to this mode calls the value of scheme-mode-hook
if that value is non-nil." t nil)

(autoload (quote dsssl-mode) "scheme" "\
Major mode for editing DSSSL code.
Editing commands are similar to those of lisp-mode.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{scheme-mode-map}
Entering this mode runs the hooks `scheme-mode-hook' and then
`dsssl-mode-hook' and inserts the value of `dsssl-sgml-declaration' if
that variable's value is a string." t nil)

;;;***

;;;### (autoloads (gnus-score-mode) "score-mode" "gnus/score-mode.el"
;;;;;;  (14030 49534))
;;; Generated autoloads from gnus/score-mode.el

(autoload (quote gnus-score-mode) "score-mode" "\
Mode for editing Gnus score files.
This mode is an extended emacs-lisp mode.

\\{gnus-score-mode-map}" t nil)

;;;***

;;;### (autoloads (scribe-mode) "scribe" "textmodes/scribe.el" (13552
;;;;;;  24630))
;;; Generated autoloads from textmodes/scribe.el

(autoload (quote scribe-mode) "scribe" "\
Major mode for editing files of Scribe (a text formatter) source.
Scribe-mode is similar text-mode, with a few extra commands added.
\\{scribe-mode-map}

Interesting variables:

scribe-fancy-paragraphs
  Non-nil makes Scribe mode use a different style of paragraph separation.

scribe-electric-quote
  Non-nil makes insert of double quote use `` or '' depending on context.

scribe-electric-parenthesis
  Non-nil makes an open-parenthesis char (one of `([<{')
  automatically insert its close if typed after an @Command form." t nil)

;;;***

;;;### (autoloads (mail-other-frame mail-other-window mail mail-mode
;;;;;;  mail-signature mail-personal-alias-file mail-alias-file mail-default-reply-to
;;;;;;  mail-archive-file-name mail-header-separator mail-yank-ignored-headers
;;;;;;  mail-interactive mail-self-blind mail-from-style) "sendmail"
;;;;;;  "mail/sendmail.el" (14115 60575))
;;; Generated autoloads from mail/sendmail.el

(defvar mail-from-style (quote angles) "\
*Specifies how \"From:\" fields look.

If `nil', they contain just the return address like:
	king@grassland.com
If `parens', they look like:
	king@grassland.com (Elvis Parsley)
If `angles', they look like:
	Elvis Parsley <king@grassland.com>
If `system-default', Rmail allows the system to insert its default From field.")

(defvar mail-self-blind nil "\
*Non-nil means insert BCC to self in messages to be sent.
This is done when the message is initialized,
so you can remove or alter the BCC field to override the default.")

(defvar mail-interactive nil "\
*Non-nil means when sending a message wait for and display errors.
nil means let mailer mail back a message to report errors.")

(defvar mail-yank-ignored-headers "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^remailed\\|^received:\\|^message-id:\\|^summary-line:\\|^to:\\|^subject:\\|^in-reply-to:\\|^return-path:" "\
*Delete these headers from old message when it's inserted in a reply.")

(defvar send-mail-function (quote sendmail-send-it) "\
Function to call to send the current buffer as mail.
The headers should be delimited by a line which is
not a valid RFC822 header or continuation line.")

(defvar mail-header-separator "--text follows this line--" "\
*Line used to separate headers from text in messages being composed.")

(defvar mail-archive-file-name nil "\
*Name of file to write all outgoing messages in, or nil for none.
This can be an inbox file or an Rmail file.")

(defvar mail-default-reply-to nil "\
*Address to insert as default Reply-to field of outgoing messages.
If nil, it will be initialized from the REPLYTO environment variable
when you first send mail.")

(defvar mail-alias-file nil "\
*If non-nil, the name of a file to use instead of `/usr/lib/aliases'.
This file defines aliases to be expanded by the mailer; this is a different
feature from that of defining aliases in `.mailrc' to be expanded in Emacs.
This variable has no effect unless your system uses sendmail as its mailer.")

(defvar mail-personal-alias-file "~/.mailrc" "\
*If non-nil, the name of the user's personal mail alias file.
This file typically should be in same format as the `.mailrc' file used by
the `Mail' or `mailx' program.
This file need not actually exist.")

(defvar mail-signature nil "\
*Text inserted at end of mail buffer when a message is initialized.
If t, it means to insert the contents of the file `mail-signature-file'.
If a string, that string is inserted.
 (To make a proper signature, the string should begin with \\n\\n-- \\n,
  which is the standard way to delimit a signature in a message.)
Otherwise, it should be an expression; it is evaluated
and should insert whatever you want to insert.")

(autoload (quote mail-mode) "sendmail" "\
Major mode for editing mail to be sent.
Like Text Mode but with these additional commands:
\\[mail-send]  mail-send (send the message)    \\[mail-send-and-exit]  mail-send-and-exit
Here are commands that move to a header field (and create it if there isn't):
	 \\[mail-to]  move to To:	\\[mail-subject]  move to Subject:
	 \\[mail-cc]  move to CC:	\\[mail-bcc]  move to BCC:
	 \\[mail-fcc]  move to FCC:
\\[mail-text]  mail-text (move to beginning of message text).
\\[mail-signature]  mail-signature (insert `mail-signature-file' file).
\\[mail-yank-original]  mail-yank-original (insert current message, in Rmail).
\\[mail-fill-yanked-message]  mail-fill-yanked-message (fill what was yanked).
\\[mail-sent-via]  mail-sent-via (add a Sent-via field for each To or CC)." t nil)

(defvar sendmail-coding-system nil "\
*Coding system for encoding the outgoing mail.
This has higher priority than `default-buffer-file-coding-system'
and `default-sendmail-coding-system',
but lower priority than the local value of `buffer-file-coding-system'.
See also the function `select-sendmail-coding-system'.")

(defvar default-sendmail-coding-system (quote iso-latin-1) "\
Default coding system for encoding the outgoing mail.
This variable is used only when `sendmail-coding-system' is nil.

This variable is set/changed by the command set-language-environment.
User should not set this variable manually,
instead use sendmail-coding-system to get a constant encoding
of outgoing mails regardless of the current language environment.
See also the function `select-sendmail-coding-system'.")
 (add-hook 'same-window-buffer-names "*mail*")

(autoload (quote mail) "sendmail" "\
Edit a message to be sent.  Prefix arg means resume editing (don't erase).
When this function returns, the buffer `*mail*' is selected.
The value is t if the message was newly initialized; otherwise, nil.

Optionally, the signature file `mail-signature-file' can be inserted at the
end; see the variable `mail-signature'.

\\<mail-mode-map>
While editing message, type \\[mail-send-and-exit] to send the message and exit.

Various special commands starting with C-c are available in sendmail mode
to move to message header fields:
\\{mail-mode-map}

If `mail-self-blind' is non-nil, a BCC to yourself is inserted
when the message is initialized.

If `mail-default-reply-to' is non-nil, it should be an address (a string);
a Reply-to: field with that address is inserted.

If `mail-archive-file-name' is non-nil, an FCC field with that file name
is inserted.

The normal hook `mail-setup-hook' is run after the message is
initialized.  It can add more default fields to the message.

When calling from a program, the first argument if non-nil says
not to erase the existing contents of the `*mail*' buffer.

The second through fifth arguments,
 TO, SUBJECT, IN-REPLY-TO and CC, specify if non-nil
 the initial contents of those header fields.
 These arguments should not have final newlines.
The sixth argument REPLYBUFFER is a buffer which contains an
 original message being replied to, or else an action
 of the form (FUNCTION . ARGS) which says how to insert the original.
 Or it can be nil, if not replying to anything.
The seventh argument ACTIONS is a list of actions to take
 if/when the message is sent.  Each action looks like (FUNCTION . ARGS);
 when the message is sent, we apply FUNCTION to ARGS.
 This is how Rmail arranges to mark messages `answered'." t nil)

(autoload (quote mail-other-window) "sendmail" "\
Like `mail' command, but display mail buffer in another window." t nil)

(autoload (quote mail-other-frame) "sendmail" "\
Like `mail' command, but display mail buffer in another frame." t nil)

;;;***

;;;### (autoloads (server-start) "server" "server.el" (14057 44873))
;;; Generated autoloads from server.el

(autoload (quote server-start) "server" "\
Allow this Emacs process to be a server for client processes.
This starts a server communications subprocess through which
client \"editors\" can send your editing commands to this Emacs job.
To use the server, set up the program `emacsclient' in the
Emacs distribution as your standard \"editor\".

Prefix arg means just kill any existing server communications subprocess." t nil)

;;;***

;;;### (autoloads (html-mode sgml-mode) "sgml-mode" "textmodes/sgml-mode.el"
;;;;;;  (14053 30123))
;;; Generated autoloads from textmodes/sgml-mode.el

(autoload (quote sgml-mode) "sgml-mode" "\
Major mode for editing SGML documents.
Makes > match <.  Makes / blink matching /.
Keys <, &, SPC within <>, \" and ' can be electric depending on
`sgml-quick-keys'.

An argument of N to a tag-inserting command means to wrap it around
the next N words.  In Transient Mark mode, when the mark is active,
N defaults to -1, which means to wrap it around the current region.

If you like upcased tags, put (setq sgml-transformation 'upcase) in
your `.emacs' file.

Use \\[sgml-validate] to validate your document with an SGML parser.

Do \\[describe-variable] sgml- SPC to see available variables.
Do \\[describe-key] on the following bindings to discover what they do.
\\{sgml-mode-map}" t nil)

(autoload (quote html-mode) "sgml-mode" "\
Major mode based on SGML mode for editing HTML documents.
This allows inserting skeleton constructs used in hypertext documents with
completion.  See below for an introduction to HTML.  Use
\\[browse-url-of-buffer] to see how this comes out.  See also `sgml-mode' on
which this is based.

Do \\[describe-variable] html- SPC and \\[describe-variable] sgml- SPC to see available variables.

To write fairly well formatted pages you only need to know few things.  Most
browsers have a function to read the source code of the page being seen, so
you can imitate various tricks.  Here's a very short HTML primer which you
can also view with a browser to see what happens:

<title>A Title Describing Contents</title> should be on every page.  Pages can
have <h1>Very Major Headlines</h1> through <h6>Very Minor Headlines</h6>
<hr> Parts can be separated with horizontal rules.

<p>Paragraphs only need an opening tag.  Line breaks and multiple spaces are
ignored unless the text is <pre>preformatted.</pre>  Text can be marked as
<b>bold</b>, <i>italic</i> or <u>underlined</u> using the normal  M-g  or
Edit/Text Properties/Face commands.

Pages can have <a name=\"SOMENAME\">named points</a> and can link other points
to them with <a href=\"#SOMENAME\">see also somename</a>.  In the same way <a
href=\"URL\">see also URL</a> where URL is a filename relative to current
directory, or absolute as in `http://www.cs.indiana.edu/elisp/w3/docs.html'.

Images in many formats can be inlined with <img src=\"URL\">.

If you mainly create your own documents, `sgml-specials' might be
interesting.  But note that some HTML 2 browsers can't handle `&apos;'.
To work around that, do:
   (eval-after-load \"sgml-mode\" '(aset sgml-char-names ?' nil))

\\{html-mode-map}" t nil)

;;;***

;;;### (autoloads (sh-mode) "sh-script" "progmodes/sh-script.el"
;;;;;;  (14202 47537))
;;; Generated autoloads from progmodes/sh-script.el

(put (quote sh-mode) (quote mode-class) (quote special))

(autoload (quote sh-mode) "sh-script" "\
Major mode for editing shell scripts.
This mode works for many shells, since they all have roughly the same syntax,
as far as commands, arguments, variables, pipes, comments etc. are concerned.
Unless the file's magic number indicates the shell, your usual shell is
assumed.  Since filenames rarely give a clue, they are not further analyzed.

This mode adapts to the variations between shells (see `sh-set-shell') by
means of an inheritance based feature lookup (see `sh-feature').  This
mechanism applies to all variables (including skeletons) that pertain to
shell-specific features.

The default style of this mode is that of Rosenblatt's Korn shell book.
The syntax of the statements varies with the shell being used.  The
following commands are available, based on the current shell's syntax:

\\[sh-case]	 case statement
\\[sh-for]	 for loop
\\[sh-function]	 function definition
\\[sh-if]	 if statement
\\[sh-indexed-loop]	 indexed loop from 1 to n
\\[sh-while-getopts]	 while getopts loop
\\[sh-repeat]	 repeat loop
\\[sh-select]	 select loop
\\[sh-until]	 until loop
\\[sh-while]	 while loop

\\[backward-delete-char-untabify]	 Delete backward one position, even if it was a tab.
\\[sh-newline-and-indent]	 Delete unquoted space and indent new line same as this one.
\\[sh-end-of-command]	 Go to end of successive commands.
\\[sh-beginning-of-command]	 Go to beginning of successive commands.
\\[sh-set-shell]	 Set this buffer's shell, and maybe its magic number.
\\[sh-execute-region]	 Have optional header and region be executed in a subshell.

\\[sh-maybe-here-document]	 Without prefix, following an unquoted < inserts here document.
{, (, [, ', \", `
	Unless quoted with \\, insert the pairs {}, (), [], or '', \"\", ``.

If you generally program a shell different from your login shell you can
set `sh-shell-file' accordingly.  If your shell's file name doesn't correctly
indicate what shell it is use `sh-alias-alist' to translate.

If your shell gives error messages with line numbers, you can use \\[executable-interpret]
with your script for an edit-interpret-debug cycle." t nil)

(defalias (quote shell-script-mode) (quote sh-mode))

;;;***

;;;### (autoloads (list-load-path-shadows) "shadow" "emacs-lisp/shadow.el"
;;;;;;  (13667 34738))
;;; Generated autoloads from emacs-lisp/shadow.el

(autoload (quote list-load-path-shadows) "shadow" "\
Display a list of Emacs Lisp files that shadow other files.

This function lists potential load-path problems.  Directories in the
`load-path' variable are searched, in order, for Emacs Lisp
files.  When a previously encountered file name is found again, a
message is displayed indicating that the later file is \"hidden\" by
the earlier.

For example, suppose `load-path' is set to

\(\"/usr/gnu/emacs/site-lisp\" \"/usr/gnu/emacs/share/emacs/19.30/lisp\")

and that each of these directories contains a file called XXX.el.  Then
XXX.el in the site-lisp directory is referred to by all of:
\(require 'XXX), (autoload .... \"XXX\"), (load-library \"XXX\") etc.

The first XXX.el file prevents emacs from seeing the second (unless
the second is loaded explicitly via load-file).

When not intended, such shadowings can be the source of subtle
problems.  For example, the above situation may have arisen because the
XXX package was not distributed with versions of emacs prior to
19.30.  An emacs maintainer downloaded XXX from elsewhere and installed
it.  Later, XXX was updated and included in the emacs distribution.
Unless the emacs maintainer checks for this, the new version of XXX
will be hidden behind the old (which may no longer work with the new
emacs version).

This function performs these checks and flags all possible
shadowings.  Because a .el file may exist without a corresponding .elc
\(or vice-versa), these suffixes are essentially ignored.  A file
XXX.elc in an early directory (that does not contain XXX.el) is
considered to shadow a later file XXX.el, and vice-versa.

When run interactively, the shadowings (if any) are displayed in a
buffer called `*Shadows*'.  Shadowings are located by calling the
\(non-interactive) companion function, `find-emacs-lisp-shadows'." t nil)

;;;***

;;;### (autoloads (shell shell-prompt-pattern) "shell" "shell.el"
;;;;;;  (14143 46028))
;;; Generated autoloads from shell.el

(defvar shell-prompt-pattern "^[^#$%>\n]*[#$%>] *" "\
Regexp to match prompts in the inferior shell.
Defaults to \"^[^#$%>\\n]*[#$%>] *\", which works pretty well.
This variable is used to initialise `comint-prompt-regexp' in the 
shell buffer.

The pattern should probably not match more than one line.  If it does,
Shell mode may become confused trying to distinguish prompt from input
on lines which don't start with a prompt.

This is a fine thing to set in your `.emacs' file.")

(autoload (quote shell) "shell" "\
Run an inferior shell, with I/O through buffer *shell*.
If buffer exists but shell process is not running, make new shell.
If buffer exists and shell process is running, just switch to buffer `*shell*'.
Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL.
If a file `~/.emacs_SHELLNAME' exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

To specify a coding system for converting non-ASCII characters
in the input and output to the shell, use \\[universal-coding-system-argument]
before \\[shell].  You can also specify this with \\[set-buffer-process-coding-system]
in the shell buffer, after you start the shell.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)" t nil)
 (add-hook 'same-window-buffer-names "*shell*")

;;;***

;;;### (autoloads (simula-mode) "simula" "progmodes/simula.el" (13578
;;;;;;  3004))
;;; Generated autoloads from progmodes/simula.el

(autoload (quote simula-mode) "simula" "\
Major mode for editing SIMULA code.
\\{simula-mode-map}
Variables controlling indentation style:
 simula-tab-always-indent
    Non-nil means TAB in SIMULA mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 simula-indent-level
    Indentation of SIMULA statements with respect to containing block.
 simula-substatement-offset
    Extra indentation after DO, THEN, ELSE, WHEN and OTHERWISE.
 simula-continued-statement-offset 3
    Extra indentation for lines not starting a statement or substatement,
    e.g. a nested FOR-loop.  If value is a list, each line in a multiple-
    line continued statement will have the car of the list extra indentation
    with respect to the previous line of the statement.
 simula-label-offset -4711
    Offset of SIMULA label lines relative to usual indentation.
 simula-if-indent '(0 . 0)
    Extra indentation of THEN and ELSE with respect to the starting IF.
    Value is a cons cell, the car is extra THEN indentation and the cdr
    extra ELSE indentation.  IF after ELSE is indented as the starting IF.
 simula-inspect-indent '(0 . 0)
    Extra indentation of WHEN and OTHERWISE with respect to the
    corresponding INSPECT.  Value is a cons cell, the car is
    extra WHEN indentation and the cdr extra OTHERWISE indentation.
 simula-electric-indent nil
    If this variable is non-nil, `simula-indent-line'
    will check the previous line to see if it has to be reindented.
 simula-abbrev-keyword 'upcase
    Determine how SIMULA keywords will be expanded.  Value is one of
    the symbols `upcase', `downcase', `capitalize', (as in) `abbrev-table',
    or nil if they should not be changed.
 simula-abbrev-stdproc 'abbrev-table
    Determine how standard SIMULA procedure and class names will be
    expanded.  Value is one of the symbols `upcase', `downcase', `capitalize',
    (as in) `abbrev-table', or nil if they should not be changed.

Turning on SIMULA mode calls the value of the variable simula-mode-hook
with no arguments, if that value is non-nil

Warning: simula-mode-hook should not read in an abbrev file without calling
the function simula-install-standard-abbrevs afterwards, preferably not
at all." t nil)

;;;***

;;;### (autoloads (skeleton-pair-insert-maybe skeleton-insert skeleton-proxy
;;;;;;  skeleton-proxy-new define-skeleton) "skeleton" "skeleton.el"
;;;;;;  (13940 33237))
;;; Generated autoloads from skeleton.el

(defvar skeleton-filter (quote identity) "\
Function for transforming a skeleton proxy's aliases' variable value.")

(autoload (quote define-skeleton) "skeleton" "\
Define a user-configurable COMMAND that enters a statement skeleton.
DOCUMENTATION is that of the command, while the variable of the same name,
which contains the skeleton, has a documentation to that effect.
INTERACTOR and ELEMENT ... are as defined under `skeleton-insert'." nil (quote macro))

(autoload (quote skeleton-proxy-new) "skeleton" "\
Insert skeleton defined by variable of same name (see `skeleton-insert').
Prefix ARG allows wrapping around words or regions (see `skeleton-insert').
If no ARG was given, but the region is visible, ARG defaults to -1 depending
on `skeleton-autowrap'.  An ARG of  M-0  will prevent this just for once.
This command can also be an abbrev expansion (3rd and 4th columns in
\\[edit-abbrevs]  buffer: \"\"  command-name).

When called as a function, optional first argument STR may also be a string
which will be the value of `str' whereas the skeleton's interactor is then
ignored." t nil)

(autoload (quote skeleton-proxy) "skeleton" "\
Insert skeleton defined by variable of same name (see `skeleton-insert').
Prefix ARG allows wrapping around words or regions (see `skeleton-insert').
If no ARG was given, but the region is visible, ARG defaults to -1 depending
on `skeleton-autowrap'.  An ARG of  M-0  will prevent this just for once.
This command can also be an abbrev expansion (3rd and 4th columns in
\\[edit-abbrevs]  buffer: \"\"  command-name).

When called as a function, optional first argument STR may also be a string
which will be the value of `str' whereas the skeleton's interactor is then
ignored." t nil)

(autoload (quote skeleton-insert) "skeleton" "\
Insert the complex statement skeleton SKELETON describes very concisely.

With optional second argument REGIONS, wrap first interesting point
\(`_') in skeleton around next REGIONS words, if REGIONS is positive.
If REGIONS is negative, wrap REGIONS preceding interregions into first
REGIONS interesting positions (successive `_'s) in skeleton.

An interregion is the stretch of text between two contiguous marked
points.  If you marked A B C [] (where [] is the cursor) in
alphabetical order, the 3 interregions are simply the last 3 regions.
But if you marked B A [] C, the interregions are B-A, A-[], []-C.

The optional third argument STR, if specified, is the value for the
variable `str' within the skeleton.  When this is non-nil, the
interactor gets ignored, and this should be a valid skeleton element.

SKELETON is made up as (INTERACTOR ELEMENT ...).  INTERACTOR may be nil if
not needed, a prompt-string or an expression for complex read functions.

If ELEMENT is a string or a character it gets inserted (see also
`skeleton-transformation').  Other possibilities are:

	\\n	go to next line and indent according to mode
	_	interesting point, interregion here, point after termination
	>	indent line (or interregion if > _) according to major mode
	@	add position to `skeleton-positions'
	&	do next ELEMENT if previous moved point
	|	do next ELEMENT if previous didn't move point
	-num	delete num preceding characters (see `skeleton-untabify')
	resume:	skipped, continue here if quit is signaled
	nil	skipped

Further elements can be defined via `skeleton-further-elements'.  ELEMENT may
itself be a SKELETON with an INTERACTOR.  The user is prompted repeatedly for
different inputs.  The SKELETON is processed as often as the user enters a
non-empty string.  \\[keyboard-quit] terminates skeleton insertion, but
continues after `resume:' and positions at `_' if any.  If INTERACTOR in such
a subskeleton is a prompt-string which contains a \".. %s ..\" it is
formatted with `skeleton-subprompt'.  Such an INTERACTOR may also be a list of
strings with the subskeleton being repeated once for each string.

Quoted Lisp expressions are evaluated for their side-effects.
Other Lisp expressions are evaluated and the value treated as above.
Note that expressions may not return `t' since this implies an
endless loop.  Modes can define other symbols by locally setting them
to any valid skeleton element.  The following local variables are
available:

	str	first time: read a string according to INTERACTOR
		then: insert previously read string once more
	help	help-form during interaction with the user or `nil'
	input	initial input (string or cons with index) while reading str
	v1, v2	local variables for memorizing anything you want

When done with skeleton, but before going back to `_'-point call
`skeleton-end-hook' if that is non-`nil'." nil nil)

(autoload (quote skeleton-pair-insert-maybe) "skeleton" "\
Insert the character you type ARG times.

With no ARG, if `skeleton-pair' is non-nil, pairing can occur.  If the region
is visible the pair is wrapped around it depending on `skeleton-autowrap'.
Else, if `skeleton-pair-on-word' is non-nil or we are not before or inside a
word, and if `skeleton-pair-filter' returns nil, pairing is performed.

If a match is found in `skeleton-pair-alist', that is inserted, else
the defaults are used.  These are (), [], {}, <> and `' for the
symmetrical ones, and the same character twice for the others." t nil)

;;;***

;;;### (autoloads (smtpmail-send-it) "smtpmail" "mail/smtpmail.el"
;;;;;;  (14186 47077))
;;; Generated autoloads from mail/smtpmail.el

(autoload (quote smtpmail-send-it) "smtpmail" nil nil nil)

;;;***

;;;### (autoloads (snake) "snake" "play/snake.el" (13700 16190))
;;; Generated autoloads from play/snake.el

(autoload (quote snake) "snake" "\
Play the Snake game.
Move the snake around without colliding with its tail or with the border.

Eating dots causes the snake to get longer.

snake-mode keybindings:
   \\<snake-mode-map>
\\[snake-start-game]	Starts a new game of Snake
\\[snake-end-game]	Terminates the current game
\\[snake-pause-game]	Pauses (or resumes) the current game
\\[snake-move-left]	Makes the snake move left
\\[snake-move-right]	Makes the snake move right
\\[snake-move-up]	Makes the snake move up
\\[snake-move-down]	Makes the snake move down

" t nil)

;;;***

;;;### (autoloads (snmpv2-mode snmp-mode) "snmp-mode" "snmp-mode.el"
;;;;;;  (14082 18353))
;;; Generated autoloads from snmp-mode.el

(autoload (quote snmp-mode) "snmp-mode" "\
Major mode for editing SNMP MIBs.
Expression and list commands understand all C brackets.
Tab indents for C code.
Comments start with -- and end with newline or another --.
Delete converts tabs to spaces as it moves back.
\\{snmp-mode-map}
Turning on snmp-mode runs the hooks in `snmp-common-mode-hook', then
`snmp-mode-hook'." t nil)

(autoload (quote snmpv2-mode) "snmp-mode" "\
Major mode for editing SNMPv2 MIBs.
Expression and list commands understand all C brackets.
Tab indents for C code.
Comments start with -- and end with newline or another --.
Delete converts tabs to spaces as it moves back.
\\{snmp-mode-map}
Turning on snmp-mode runs the hooks in `snmp-common-mode-hook',
then `snmpv2-mode-hook'." t nil)

;;;***

;;;### (autoloads (solar-equinoxes-solstices sunrise-sunset calendar-location-name
;;;;;;  calendar-longitude calendar-latitude calendar-time-display-form)
;;;;;;  "solar" "calendar/solar.el" (13462 53924))
;;; Generated autoloads from calendar/solar.el

(defvar calendar-time-display-form (quote (12-hours ":" minutes am-pm (if time-zone " (") time-zone (if time-zone ")"))) "\
*The pseudo-pattern that governs the way a time of day is formatted.

A pseudo-pattern is a list of expressions that can involve the keywords
`12-hours', `24-hours', and `minutes',  all numbers in string form,
and `am-pm' and `time-zone',  both alphabetic strings.

For example, the form

  '(24-hours \":\" minutes
    (if time-zone \" (\") time-zone (if time-zone \")\"))

would give military-style times like `21:07 (UTC)'.")

(defvar calendar-latitude nil "\
*Latitude of `calendar-location-name' in degrees.

The value can be either a decimal fraction (one place of accuracy is
sufficient), + north, - south, such as 40.7 for New York City, or the value
can be a vector [degrees minutes north/south] such as [40 50 north] for New
York City.

This variable should be set in `site-start'.el.")

(defvar calendar-longitude nil "\
*Longitude of `calendar-location-name' in degrees.

The value can be either a decimal fraction (one place of accuracy is
sufficient), + east, - west, such as -73.9 for New York City, or the value
can be a vector [degrees minutes east/west] such as [73 55 west] for New
York City.

This variable should be set in `site-start'.el.")

(defvar calendar-location-name (quote (let ((float-output-format "%.1f")) (format "%s%s, %s%s" (if (numberp calendar-latitude) (abs calendar-latitude) (+ (aref calendar-latitude 0) (/ (aref calendar-latitude 1) 60.0))) (if (numberp calendar-latitude) (if (> calendar-latitude 0) "N" "S") (if (equal (aref calendar-latitude 2) (quote north)) "N" "S")) (if (numberp calendar-longitude) (abs calendar-longitude) (+ (aref calendar-longitude 0) (/ (aref calendar-longitude 1) 60.0))) (if (numberp calendar-longitude) (if (> calendar-longitude 0) "E" "W") (if (equal (aref calendar-longitude 2) (quote east)) "E" "W"))))) "\
*Expression evaluating to name of `calendar-longitude', `calendar-latitude'.
For example, \"New York City\".  Default value is just the latitude, longitude
pair.

This variable should be set in `site-start'.el.")

(autoload (quote sunrise-sunset) "solar" "\
Local time of sunrise and sunset for today.  Accurate to a few seconds.
If called with an optional prefix argument, prompt for date.

If called with an optional double prefix argument, prompt for longitude,
latitude, time zone, and date, and always use standard time.

This function is suitable for execution in a .emacs file." t nil)

(autoload (quote solar-equinoxes-solstices) "solar" "\
*local* date and time of equinoxes and solstices, if visible in the calendar window.
Requires floating point." nil nil)

;;;***

;;;### (autoloads (solitaire) "solitaire" "play/solitaire.el" (13672
;;;;;;  19835))
;;; Generated autoloads from play/solitaire.el

(autoload (quote solitaire) "solitaire" "\
Play Solitaire.

To play Solitaire, type \\[solitaire].
\\<solitaire-mode-map>
Move around the board using the cursor keys.
Move stones using \\[solitaire-move] followed by a direction key.
Undo moves using \\[solitaire-undo].
Check for possible moves using \\[solitaire-do-check].
\(The variable `solitaire-auto-eval' controls whether to automatically
check after each move or undo)

What is Solitaire?

I don't know who invented this game, but it seems to be rather old and
its origin seems to be northern Africa.  Here's how to play:
Initially, the board will look similar to this:

	Le Solitaire             
	============             
	
		o   o   o        
	
		o   o   o        
	
	o   o   o   o   o   o   o
	
	o   o   o   .   o   o   o
	
	o   o   o   o   o   o   o
	
		o   o   o        
	
		o   o   o        

Let's call the o's stones and the .'s holes.  One stone fits into one
hole.  As you can see, all holes but one are occupied by stones.  The
aim of the game is to get rid of all but one stone, leaving that last
one in the middle of the board if you're cool.

A stone can be moved if there is another stone next to it, and a hole
after that one.  Thus there must be three fields in a row, either
horizontally or vertically, up, down, left or right, which look like
this:  o  o  .

Then the first stone is moved to the hole, jumping over the second,
which therefore is taken away.  The above thus `evaluates' to:  .  .  o

That's all.  Here's the board after two moves:

		o   o   o        
	
		.   o   o        
	
	o   o   .   o   o   o   o
	
	o   .   o   o   o   o   o
	
	o   o   o   o   o   o   o
	
		o   o   o        
	
		o   o   o

Pick your favourite shortcuts:

\\{solitaire-mode-map}" t nil)

;;;***

;;;### (autoloads (reverse-region sort-columns sort-regexp-fields
;;;;;;  sort-fields sort-numeric-fields sort-pages sort-paragraphs
;;;;;;  sort-lines sort-subr) "sort" "sort.el" (13304 43432))
;;; Generated autoloads from sort.el

(autoload (quote sort-subr) "sort" "\
General text sorting routine to divide buffer into records and sort them.
Arguments are REVERSE NEXTRECFUN ENDRECFUN &optional STARTKEYFUN ENDKEYFUN.

We divide the accessible portion of the buffer into disjoint pieces
called sort records.  A portion of each sort record (perhaps all of
it) is designated as the sort key.  The records are rearranged in the
buffer in order by their sort keys.  The records may or may not be
contiguous.

Usually the records are rearranged in order of ascending sort key.
If REVERSE is non-nil, they are rearranged in order of descending sort key.
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order.

The next four arguments are functions to be called to move point
across a sort record.  They will be called many times from within sort-subr.

NEXTRECFUN is called with point at the end of the previous record.
It moves point to the start of the next record.
It should move point to the end of the buffer if there are no more records.
The first record is assumed to start at the position of point when sort-subr
is called.

ENDRECFUN is called with point within the record.
It should move point to the end of the record.

STARTKEYFUN moves from the start of the record to the start of the key.
It may return either a non-nil value to be used as the key, or
else the key is the substring between the values of point after
STARTKEYFUN and ENDKEYFUN are called.  If STARTKEYFUN is nil, the key
starts at the beginning of the record.

ENDKEYFUN moves from the start of the sort key to the end of the sort key.
ENDKEYFUN may be nil if STARTKEYFUN returns a value or if it would be the
same as ENDRECFUN." nil nil)

(autoload (quote sort-lines) "sort" "\
Sort lines in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort).
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order." t nil)

(autoload (quote sort-paragraphs) "sort" "\
Sort paragraphs in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort).
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order." t nil)

(autoload (quote sort-pages) "sort" "\
Sort pages in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort).
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order." t nil)

(autoload (quote sort-numeric-fields) "sort" "\
Sort lines in region numerically by the ARGth field of each line.
Fields are separated by whitespace and numbered from 1 up.
Specified field must contain a number in each line of the region.
With a negative arg, sorts by the ARGth field counted from the right.
Called from a program, there are three arguments:
FIELD, BEG and END.  BEG and END specify region to sort." t nil)

(autoload (quote sort-fields) "sort" "\
Sort lines in region lexicographically by the ARGth field of each line.
Fields are separated by whitespace and numbered from 1 up.
With a negative arg, sorts by the ARGth field counted from the right.
Called from a program, there are three arguments:
FIELD, BEG and END.  BEG and END specify region to sort.
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order." t nil)

(autoload (quote sort-regexp-fields) "sort" "\
Sort the region lexicographically as specified by RECORD-REGEXP and KEY.
RECORD-REGEXP specifies the textual units which should be sorted.
  For example, to sort lines RECORD-REGEXP would be \"^.*$\"
KEY specifies the part of each record (ie each match for RECORD-REGEXP)
  is to be used for sorting.
  If it is \"\\\\digit\" then the digit'th \"\\\\(...\\\\)\" match field from
  RECORD-REGEXP is used.
  If it is \"\\\\&\" then the whole record is used.
  Otherwise, it is a regular-expression for which to search within the record.
If a match for KEY is not found within a record then that record is ignored.

With a negative prefix arg sorts in reverse order.

The variable `sort-fold-case' determines whether alphabetic case affects
the sort order.

For example: to sort lines in the region by the first word on each line
 starting with the letter \"f\",
 RECORD-REGEXP would be \"^.*$\" and KEY would be \"\\\\=\\<f\\\\w*\\\\>\"" t nil)

(autoload (quote sort-columns) "sort" "\
Sort lines in region alphabetically by a certain range of columns.
For the purpose of this command, the region includes
the entire line that point is in and the entire line the mark is in.
The column positions of point and mark bound the range of columns to sort on.
A prefix argument means sort into reverse order.
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order.

Note that `sort-columns' rejects text that contains tabs,
because tabs could be split across the specified columns
and it doesn't know how to handle that.  Also, when possible,
it uses the `sort' utility program, which doesn't understand tabs.
Use \\[untabify] to convert tabs to spaces before sorting." t nil)

(autoload (quote reverse-region) "sort" "\
Reverse the order of lines in a region.
From a program takes two point or marker arguments, BEG and END." t nil)

;;;***

;;;### (autoloads (speedbar-get-focus speedbar-frame-mode) "speedbar"
;;;;;;  "speedbar.el" (14168 6532))
;;; Generated autoloads from speedbar.el

(defalias (quote speedbar) (quote speedbar-frame-mode))

(autoload (quote speedbar-frame-mode) "speedbar" "\
Enable or disable speedbar.  Positive ARG means turn on, negative turn off.
nil means toggle.  Once the speedbar frame is activated, a buffer in
`speedbar-mode' will be displayed.  Currently, only one speedbar is
supported at a time.
`speedbar-before-popup-hook' is called before popping up the speedbar frame.
`speedbar-before-delete-hook' is called before the frame is deleted." t nil)

(autoload (quote speedbar-get-focus) "speedbar" "\
Change frame focus to or from the speedbar frame.
If the selected frame is not speedbar, then speedbar frame is
selected.  If the speedbar frame is active, then select the attached frame." t nil)

;;;***

;;;### (autoloads (spell-string spell-region spell-word spell-buffer)
;;;;;;  "spell" "textmodes/spell.el" (13553 46126))
;;; Generated autoloads from textmodes/spell.el

(put (quote spell-filter) (quote risky-local-variable) t)

(autoload (quote spell-buffer) "spell" "\
Check spelling of every word in the buffer.
For each incorrect word, you are asked for the correct spelling
and then put into a query-replace to fix some or all occurrences.
If you do not want to change a word, just give the same word
as its \"correct\" spelling; then the query replace is skipped." t nil)

(autoload (quote spell-word) "spell" "\
Check spelling of word at or before point.
If it is not correct, ask user for the correct spelling
and `query-replace' the entire buffer to substitute it." t nil)

(autoload (quote spell-region) "spell" "\
Like `spell-buffer' but applies only to region.
Used in a program, applies from START to END.
DESCRIPTION is an optional string naming the unit being checked:
for example, \"word\"." t nil)

(autoload (quote spell-string) "spell" "\
Check spelling of string supplied as argument." t nil)

;;;***

;;;### (autoloads (snarf-spooks spook) "spook" "play/spook.el" (13607
;;;;;;  43407))
;;; Generated autoloads from play/spook.el

(autoload (quote spook) "spook" "\
Adds that special touch of class to your outgoing mail." t nil)

(autoload (quote snarf-spooks) "spook" "\
Return a vector containing the lines from `spook-phrases-file'." nil nil)

;;;***

;;;### (autoloads (sql-postgres sql-mode sql-help) "sql" "progmodes/sql.el"
;;;;;;  (14184 35036))
;;; Generated autoloads from progmodes/sql.el

(autoload (quote sql-help) "sql" "\
Shows short help for the SQL modes.

Use an entry function to open an interactive SQL buffer.  This buffer is
usually named `*SQL*'.  The name of the major mode is SQLi.

Use the following commands to start a specific SQL interpreter:

    PostGres: \\[sql-postgres]

Other non-free SQL implementations are also supported:

    MySQL: \\[sql-mysql]
    Solid: \\[sql-solid]
    Oracle: \\[sql-oracle]
    Informix: \\[sql-informix]
    Sybase: \\[sql-sybase]
    Ingres: \\[sql-ingres]
    Microsoft: \\[sql-ms]

But we urge you to choose a free implementation instead of these.

Once you have the SQLi buffer, you can enter SQL statements in the
buffer.  The output generated is appended to the buffer and a new prompt
is generated.  See the In/Out menu in the SQLi buffer for some functions
that help you navigate through the buffer, the input history, etc.

Put a line with a call to autoload into your `~/.emacs' file for each
entry function you want to use regularly:

\(autoload 'sql-postgres \"sql\" \"Interactive SQL mode.\" t)

If you have a really complex SQL statement or if you are writing a
procedure, you can do this in a separate buffer.  Put the new buffer in
`sql-mode' by calling \\[sql-mode].  The name of this buffer can be
anything.  The name of the major mode is SQL.

In this SQL buffer (SQL mode), you can send the region or the entire
buffer to the interactive SQL buffer (SQLi mode).  The results are
appended to the SQLi buffer without disturbing your SQL buffer." t nil)

(autoload (quote sql-mode) "sql" "\
Major mode to edit SQL.

You can send SQL statements to the SQLi buffer using
\\[sql-send-region].  Such a buffer must exist before you can do this.
See `sql-help' on how to create SQLi buffers.

\\{sql-mode-map} 
Customization: Entry to this mode runs the `sql-mode-hook'.

When you put a buffer in SQL mode, the buffer stores the last SQLi
buffer created as its destination in the variable `sql-buffer'.  This
will be the buffer \\[sql-send-region] sends the region to.  If this
SQLi buffer is killed, \\[sql-send-region] is no longer able to
determine where the strings should be sent to.  You can set the
value of `sql-buffer' using \\[sql-set-sqli-buffer].

For information on how to create multiple SQLi buffers, see
`sql-interactive-mode'." t nil)

(autoload (quote sql-postgres) "sql" "\
Run psql by Postgres as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-postgres-program'.  Login uses
the variables `sql-database' and `sql-server' as default, if set.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-postgres].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.  If your output lines end with ^M, 
your might try undecided-dos as a coding system.  If this doesn't help,
Try to set `comint-output-filter-functions' like this:

\(setq comint-output-filter-functions (append comint-output-filter-functions
					     '(comint-strip-ctrl-m)))

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)" t nil)

;;;***

;;;### (autoloads (strokes-mode strokes-load-user-strokes strokes-help
;;;;;;  strokes-describe-stroke strokes-do-complex-stroke strokes-do-stroke
;;;;;;  strokes-read-complex-stroke strokes-read-stroke strokes-global-set-stroke)
;;;;;;  "strokes" "strokes.el" (13337 50329))
;;; Generated autoloads from strokes.el

(defvar strokes-mode nil "\
Non-nil when `strokes' is globally enabled")

(autoload (quote strokes-global-set-stroke) "strokes" "\
Interactively give STROKE the global binding as COMMAND.
Operated just like `global-set-key', except for strokes.
COMMAND is a symbol naming an interactively-callable function.  STROKE
is a list of sampled positions on the stroke grid as described in the
documentation for the `strokes-define-stroke' function." t nil)

(defalias (quote global-set-stroke) (quote strokes-global-set-stroke))

(autoload (quote strokes-read-stroke) "strokes" "\
Read a simple stroke (interactively) and return the stroke.
Optional PROMPT in minibuffer displays before and during stroke reading.
This function will display the stroke interactively as it is being
entered in the strokes buffer if the variable
`strokes-use-strokes-buffer' is non-nil.
Optional EVENT is acceptable as the starting event of the stroke" nil nil)

(autoload (quote strokes-read-complex-stroke) "strokes" "\
Read a complex stroke (interactively) and return the stroke.
Optional PROMPT in minibuffer displays before and during stroke reading.
Note that a complex stroke allows the user to pen-up and pen-down.  This
is implemented by allowing the user to paint with button1 or button2 and
then complete the stroke with button3.
Optional EVENT is acceptable as the starting event of the stroke" nil nil)

(autoload (quote strokes-do-stroke) "strokes" "\
Read a simple stroke from the user and then exectute its comand.
This must be bound to a mouse event." t nil)

(autoload (quote strokes-do-complex-stroke) "strokes" "\
Read a complex stroke from the user and then exectute its command.
This must be bound to a mouse event." t nil)

(autoload (quote strokes-describe-stroke) "strokes" "\
Displays the command which STROKE maps to, reading STROKE interactively." t nil)

(defalias (quote describe-stroke) (quote strokes-describe-stroke))

(autoload (quote strokes-help) "strokes" "\
Get instructional help on using the the `strokes' package." t nil)

(autoload (quote strokes-load-user-strokes) "strokes" "\
Load user-defined strokes from file named by `strokes-file'." t nil)

(defalias (quote load-user-strokes) (quote strokes-load-user-strokes))

(autoload (quote strokes-mode) "strokes" "\
Toggle strokes being enabled.
With ARG, turn strokes on if and only if ARG is positive or true.
Note that `strokes-mode' is a global mode.  Think of it as a minor
mode in all buffers when activated.
By default, strokes are invoked with mouse button-2.  You can define
new strokes with

> M-x global-set-stroke

To use strokes for pictographic editing, such as Chinese/Japanese, use
Sh-button-2, which draws strokes and inserts them.  Encode/decode your
strokes with

> M-x strokes-encode-buffer
> M-x strokes-decode-buffer" t nil)

;;;***

;;;### (autoloads (sc-cite-original) "supercite" "mail/supercite.el"
;;;;;;  (14192 4731))
;;; Generated autoloads from mail/supercite.el

(autoload (quote sc-cite-original) "supercite" "\
Workhorse citing function which performs the initial citation.
This is callable from the various mail and news readers' reply
function according to the agreed upon standard.  See `\\[sc-describe]'
for more details.  `sc-cite-original' does not do any yanking of the
original message but it does require a few things:

     1) The reply buffer is the current buffer.

     2) The original message has been yanked and inserted into the
        reply buffer.

     3) Verbose mail headers from the original message have been
        inserted into the reply buffer directly before the text of the
        original message.

     4) Point is at the beginning of the verbose headers.

     5) Mark is at the end of the body of text to be cited.

For Emacs 19's, the region need not be active (and typically isn't
when this function is called.  Also, the hook `sc-pre-hook' is run
before, and `sc-post-hook' is run after the guts of this function." nil nil)

;;;***

;;;### (autoloads (tabify untabify) "tabify" "tabify.el" (13227 8735))
;;; Generated autoloads from tabify.el

(autoload (quote untabify) "tabify" "\
Convert all tabs in region to multiple spaces, preserving columns.
Called non-interactively, the region is specified by arguments
START and END, rather than by the position of point and mark.
The variable `tab-width' controls the spacing of tab stops." t nil)

(autoload (quote tabify) "tabify" "\
Convert multiple spaces in region to tabs when possible.
A group of spaces is partially replaced by tabs
when this can be done without changing the column they end at.
Called non-interactively, the region is specified by arguments
START and END, rather than by the position of point and mark.
The variable `tab-width' controls the spacing of tab stops." t nil)

;;;***

;;;### (autoloads (talk-connect) "talk" "talk.el" (13229 29627))
;;; Generated autoloads from talk.el

(autoload (quote talk-connect) "talk" "\
Connect to display DISPLAY for the Emacs talk group." t nil)

;;;***

;;;### (autoloads (tar-mode) "tar-mode" "tar-mode.el" (14189 16949))
;;; Generated autoloads from tar-mode.el

(autoload (quote tar-mode) "tar-mode" "\
Major mode for viewing a tar file as a dired-like listing of its contents.
You can move around using the usual cursor motion commands. 
Letters no longer insert themselves.
Type `e' to pull a file out of the tar file and into its own buffer;
or click mouse-2 on the file's line in the Tar mode buffer.
Type `c' to copy an entry from the tar file into another file on disk.

If you edit a sub-file of this archive (as with the `e' command) and 
save it with Control-x Control-s, the contents of that buffer will be 
saved back into the tar-file buffer; in this way you can edit a file 
inside of a tar archive without extracting it and re-archiving it.

See also: variables `tar-update-datestamp' and `tar-anal-blocksize'.
\\{tar-mode-map}" nil nil)

;;;***

;;;### (autoloads (tcl-mode) "tcl-mode" "progmodes/tcl-mode.el" (13878
;;;;;;  37688))
;;; Generated autoloads from progmodes/tcl-mode.el

(autoload (quote tcl-mode) "tcl-mode" "\
Major mode for editing tcl scripts.
The following keys are bound:
\\{tcl-mode-map}
" t nil)

;;;***

;;;### (autoloads (rsh telnet) "telnet" "telnet.el" (13858 52046))
;;; Generated autoloads from telnet.el
 (add-hook 'same-window-regexps "\\*telnet-.*\\*\\(\\|<[0-9]+>\\)")

(autoload (quote telnet) "telnet" "\
Open a network login connection to host named HOST (a string).
Communication with HOST is recorded in a buffer `*PROGRAM-HOST*'
where PROGRAM is the telnet program being used.  This program
is controlled by the contents of the global variable `telnet-host-properties',
falling back on the value of the global variable `telnet-program'.
Normally input is edited in Emacs and sent a line at a time." t nil)
 (add-hook 'same-window-regexps "\\*rsh-[^-]*\\*\\(\\|<[0-9]*>\\)")

(autoload (quote rsh) "telnet" "\
Open a network login connection to host named HOST (a string).
Communication with HOST is recorded in a buffer `*rsh-HOST*'.
Normally input is edited in Emacs and sent a line at a time." t nil)

;;;***

;;;### (autoloads (ansi-term term make-term) "term" "term.el" (14213
;;;;;;  23781))
;;; Generated autoloads from term.el

(autoload (quote make-term) "term" "\
Make a term process NAME in a buffer, running PROGRAM.
The name of the buffer is made by surrounding NAME with `*'s.
If there is already a running process in that buffer, it is not restarted.
Optional third arg STARTFILE is the name of a file to send the contents of to
the process.  Any more args are arguments to PROGRAM." nil nil)

(autoload (quote term) "term" "\
Start a terminal-emulator in a new buffer." t nil)

(autoload (quote ansi-term) "term" "\
Start a terminal-emulator in a new buffer." t nil)

;;;***

;;;### (autoloads (terminal-emulator) "terminal" "terminal.el" (14007
;;;;;;  19919))
;;; Generated autoloads from terminal.el

(autoload (quote terminal-emulator) "terminal" "\
Under a display-terminal emulator in BUFFER, run PROGRAM on arguments ARGS.
ARGS is a list of argument-strings.  Remaining arguments are WIDTH and HEIGHT.
BUFFER's contents are made an image of the display generated by that program,
and any input typed when BUFFER is the current Emacs buffer is sent to that
program as keyboard input.

Interactively, BUFFER defaults to \"*terminal*\" and PROGRAM and ARGS
are parsed from an input-string using your usual shell.
WIDTH and HEIGHT are determined from the size of the current window
-- WIDTH will be one less than the window's width, HEIGHT will be its height.

To switch buffers and leave the emulator, or to give commands
to the emulator itself (as opposed to the program running under it),
type Control-^.  The following character is an emulator command.
Type Control-^ twice to send it to the subprogram.
This escape character may be changed using the variable `terminal-escape-char'.

`Meta' characters may not currently be sent through the terminal emulator.

Here is a list of some of the variables which control the behaviour
of the emulator -- see their documentation for more information:
terminal-escape-char, terminal-scrolling, terminal-more-processing,
terminal-redisplay-interval.

This function calls the value of terminal-mode-hook if that exists
and is non-nil after the terminal buffer has been set up and the
subprocess started." t nil)

;;;***

;;;### (autoloads (tetris) "tetris" "play/tetris.el" (13700 15872))
;;; Generated autoloads from play/tetris.el

(autoload (quote tetris) "tetris" "\
Play the Tetris game.
Shapes drop from the top of the screen, and the user has to move and
rotate the shape to fit in with those at the bottom of the screen so
as to form complete rows.

tetris-mode keybindings:
   \\<tetris-mode-map>
\\[tetris-start-game]	Starts a new game of Tetris
\\[tetris-end-game]	Terminates the current game
\\[tetris-pause-game]	Pauses (or resumes) the current game
\\[tetris-move-left]	Moves the shape one square to the left
\\[tetris-move-right]	Moves the shape one square to the right
\\[tetris-rotate-prev]	Rotates the shape clockwise
\\[tetris-rotate-next]	Rotates the shape anticlockwise
\\[tetris-move-bottom]	Drops the shape to the bottom of the playing area

" t nil)

;;;***

;;;### (autoloads (tex-start-shell slitex-mode latex-mode plain-tex-mode
;;;;;;  tex-mode tex-close-quote tex-open-quote tex-default-mode
;;;;;;  tex-show-queue-command tex-dvi-view-command tex-alt-dvi-print-command
;;;;;;  tex-dvi-print-command tex-bibtex-command latex-block-names
;;;;;;  tex-start-options-string slitex-run-command latex-run-command
;;;;;;  tex-run-command tex-offer-save tex-main-file tex-first-line-header-regexp
;;;;;;  tex-directory tex-shell-file-name) "tex-mode" "textmodes/tex-mode.el"
;;;;;;  (14079 17189))
;;; Generated autoloads from textmodes/tex-mode.el

(defvar tex-shell-file-name nil "\
*If non-nil, the shell file name to run in the subshell used to run TeX.")

(defvar tex-directory "." "\
*Directory in which temporary files are written.
You can make this `/tmp' if your TEXINPUTS has no relative directories in it
and you don't try to apply \\[tex-region] or \\[tex-buffer] when there are
`\\input' commands with relative directories.")

(defvar tex-first-line-header-regexp nil "\
Regexp for matching a first line which `tex-region' should include.
If this is non-nil, it should be a regular expression string;
if it matches the first line of the file,
`tex-region' always includes the first line in the TeX run.")

(defvar tex-main-file nil "\
*The main TeX source file which includes this buffer's file.
The command `tex-file' runs TeX on the file specified by `tex-main-file'
if the variable is non-nil.")

(defvar tex-offer-save t "\
*If non-nil, ask about saving modified buffers before \\[tex-file] is run.")

(defvar tex-run-command "tex" "\
*Command used to run TeX subjob.
TeX Mode sets `tex-command' to this string.
See the documentation of that variable.")

(defvar latex-run-command "latex" "\
*Command used to run LaTeX subjob.
LaTeX Mode sets `tex-command' to this string.
See the documentation of that variable.")

(defvar slitex-run-command "slitex" "\
*Command used to run SliTeX subjob.
SliTeX Mode sets `tex-command' to this string.
See the documentation of that variable.")

(defvar tex-start-options-string "\\nonstopmode\\input" "\
*TeX options to use when running TeX.
These precede the input file name. If nil, TeX runs without option.
See the documentation of `tex-command'.")

(defvar latex-block-names nil "\
*User defined LaTeX block names.
Combined with `standard-latex-block-names' for minibuffer completion.")

(defvar tex-bibtex-command "bibtex" "\
*Command used by `tex-bibtex-file' to gather bibliographic data.
If this string contains an asterisk (`*'), that is replaced by the file name;
otherwise, the file name, preceded by blank, is added at the end.")

(defvar tex-dvi-print-command "lpr -d" "\
*Command used by \\[tex-print] to print a .dvi file.
If this string contains an asterisk (`*'), that is replaced by the file name;
otherwise, the file name, preceded by blank, is added at the end.")

(defvar tex-alt-dvi-print-command "lpr -d" "\
*Command used by \\[tex-print] with a prefix arg to print a .dvi file.
If this string contains an asterisk (`*'), that is replaced by the file name;
otherwise, the file name, preceded by blank, is added at the end.

If two printers are not enough of a choice, you can set the variable
`tex-alt-dvi-print-command' to an expression that asks what you want;
for example,

    (setq tex-alt-dvi-print-command
         '(format \"lpr -P%s\" (read-string \"Use printer: \")))

would tell \\[tex-print] with a prefix argument to ask you which printer to
use.")

(defvar tex-dvi-view-command nil "\
*Command used by \\[tex-view] to display a `.dvi' file.
If this string contains an asterisk (`*'), that is replaced by the file name;
otherwise, the file name, preceded by blank, is added at the end.

This can be set conditionally so that the previewer used is suitable for the
window system being used.  For example,

    (setq tex-dvi-view-command
          (if (eq window-system 'x) \"xdvi\" \"dvi2tty * | cat -s\"))

would tell \\[tex-view] to use xdvi under X windows and to use dvi2tty
otherwise.")

(defvar tex-show-queue-command "lpq" "\
*Command used by \\[tex-show-print-queue] to show the print queue.
Should show the queue(s) that \\[tex-print] puts jobs on.")

(defvar tex-default-mode (quote plain-tex-mode) "\
*Mode to enter for a new file that might be either TeX or LaTeX.
This variable is used when it can't be determined whether the file
is plain TeX or LaTeX or what because the file contains no commands.
Normally set to either `plain-tex-mode' or `latex-mode'.")

(defvar tex-open-quote "``" "\
*String inserted by typing \\[tex-insert-quote] to open a quotation.")

(defvar tex-close-quote "''" "\
*String inserted by typing \\[tex-insert-quote] to close a quotation.")

(autoload (quote tex-mode) "tex-mode" "\
Major mode for editing files of input for TeX, LaTeX, or SliTeX.
Tries to determine (by looking at the beginning of the file) whether
this file is for plain TeX, LaTeX, or SliTeX and calls `plain-tex-mode',
`latex-mode', or `slitex-mode', respectively.  If it cannot be determined,
such as if there are no commands in the file, the value of `tex-default-mode'
says which mode to use." t nil)

(defalias (quote TeX-mode) (quote tex-mode))

(defalias (quote plain-TeX-mode) (quote plain-tex-mode))

(defalias (quote LaTeX-mode) (quote latex-mode))

(autoload (quote plain-tex-mode) "tex-mode" "\
Major mode for editing files of input for plain TeX.
Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.

Use \\[tex-region] to run TeX on the current region, plus a \"header\"
copied from the top of the file (containing macro definitions, etc.),
running TeX under a special subshell.  \\[tex-buffer] does the whole buffer.
\\[tex-file] saves the buffer and then processes the file.
\\[tex-print] prints the .dvi file made by any of these.
\\[tex-view] previews the .dvi file made by any of these.
\\[tex-bibtex-file] runs bibtex on the file of the current buffer.

Use \\[tex-validate-buffer] to check buffer for paragraphs containing
mismatched $'s or braces.

Special commands:
\\{tex-mode-map}

Mode variables:
tex-run-command
	Command string used by \\[tex-region] or \\[tex-buffer].
tex-directory
	Directory in which to create temporary files for TeX jobs
	run by \\[tex-region] or \\[tex-buffer].
tex-dvi-print-command
	Command string used by \\[tex-print] to print a .dvi file.
tex-alt-dvi-print-command
	Alternative command string used by \\[tex-print] (when given a prefix
	argument) to print a .dvi file.
tex-dvi-view-command
	Command string used by \\[tex-view] to preview a .dvi file.
tex-show-queue-command
	Command string used by \\[tex-show-print-queue] to show the print
	queue that \\[tex-print] put your job on.

Entering Plain-tex mode runs the hook `text-mode-hook', then the hook
`tex-mode-hook', and finally the hook `plain-tex-mode-hook'.  When the
special subshell is initiated, the hook `tex-shell-hook' is run." t nil)

(autoload (quote latex-mode) "tex-mode" "\
Major mode for editing files of input for LaTeX.
Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.

Use \\[tex-region] to run LaTeX on the current region, plus the preamble
copied from the top of the file (containing \\documentstyle, etc.),
running LaTeX under a special subshell.  \\[tex-buffer] does the whole buffer.
\\[tex-file] saves the buffer and then processes the file.
\\[tex-print] prints the .dvi file made by any of these.
\\[tex-view] previews the .dvi file made by any of these.
\\[tex-bibtex-file] runs bibtex on the file of the current buffer.

Use \\[tex-validate-buffer] to check buffer for paragraphs containing
mismatched $'s or braces.

Special commands:
\\{tex-mode-map}

Mode variables:
latex-run-command
	Command string used by \\[tex-region] or \\[tex-buffer].
tex-directory
	Directory in which to create temporary files for LaTeX jobs
	run by \\[tex-region] or \\[tex-buffer].
tex-dvi-print-command
	Command string used by \\[tex-print] to print a .dvi file.
tex-alt-dvi-print-command
	Alternative command string used by \\[tex-print] (when given a prefix
	argument) to print a .dvi file.
tex-dvi-view-command
	Command string used by \\[tex-view] to preview a .dvi file.
tex-show-queue-command
	Command string used by \\[tex-show-print-queue] to show the print
	queue that \\[tex-print] put your job on.

Entering Latex mode runs the hook `text-mode-hook', then
`tex-mode-hook', and finally `latex-mode-hook'.  When the special
subshell is initiated, `tex-shell-hook' is run." t nil)

(autoload (quote slitex-mode) "tex-mode" "\
Major mode for editing files of input for SliTeX.
Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.

Use \\[tex-region] to run SliTeX on the current region, plus the preamble
copied from the top of the file (containing \\documentstyle, etc.),
running SliTeX under a special subshell.  \\[tex-buffer] does the whole buffer.
\\[tex-file] saves the buffer and then processes the file.
\\[tex-print] prints the .dvi file made by any of these.
\\[tex-view] previews the .dvi file made by any of these.
\\[tex-bibtex-file] runs bibtex on the file of the current buffer.

Use \\[tex-validate-buffer] to check buffer for paragraphs containing
mismatched $'s or braces.

Special commands:
\\{tex-mode-map}

Mode variables:
slitex-run-command
	Command string used by \\[tex-region] or \\[tex-buffer].
tex-directory
	Directory in which to create temporary files for SliTeX jobs
	run by \\[tex-region] or \\[tex-buffer].
tex-dvi-print-command
	Command string used by \\[tex-print] to print a .dvi file.
tex-alt-dvi-print-command
	Alternative command string used by \\[tex-print] (when given a prefix
	argument) to print a .dvi file.
tex-dvi-view-command
	Command string used by \\[tex-view] to preview a .dvi file.
tex-show-queue-command
	Command string used by \\[tex-show-print-queue] to show the print
	queue that \\[tex-print] put your job on.

Entering SliTeX mode runs the hook `text-mode-hook', then the hook
`tex-mode-hook', then the hook `latex-mode-hook', and finally the hook
`slitex-mode-hook'.  When the special subshell is initiated, the hook
`tex-shell-hook' is run." t nil)

(autoload (quote tex-start-shell) "tex-mode" nil nil nil)

;;;***

;;;### (autoloads (texi2info texinfo-format-region texinfo-format-buffer)
;;;;;;  "texinfmt" "textmodes/texinfmt.el" (14160 15906))
;;; Generated autoloads from textmodes/texinfmt.el

(autoload (quote texinfo-format-buffer) "texinfmt" "\
Process the current buffer as texinfo code, into an Info file.
The Info file output is generated in a buffer visiting the Info file
name specified in the @setfilename command.

Non-nil argument (prefix, if interactive) means don't make tag table
and don't split the file if large.  You can use Info-tagify and
Info-split to do these manually." t nil)

(autoload (quote texinfo-format-region) "texinfmt" "\
Convert the current region of the Texinfo file to Info format.
This lets you see what that part of the file will look like in Info.
The command is bound to \\[texinfo-format-region].  The text that is
converted to Info is stored in a temporary buffer." t nil)

(autoload (quote texi2info) "texinfmt" "\
Convert the current buffer (written in Texinfo code) into an Info file.
The Info file output is generated in a buffer visiting the Info file
names specified in the @setfilename command.

This function automatically updates all node pointers and menus, and
creates a master menu.  This work is done on a temporary buffer that
is automatically removed when the Info file is created.  The original
Texinfo source buffer is not changed.

Non-nil argument (prefix, if interactive) means don't split the file
if large.  You can use Info-split to do this manually." t nil)

;;;***

;;;### (autoloads (texinfo-mode) "texinfo" "textmodes/texinfo.el"
;;;;;;  (14015 33043))
;;; Generated autoloads from textmodes/texinfo.el

(autoload (quote texinfo-mode) "texinfo" "\
Major mode for editing Texinfo files.

  It has these extra commands:
\\{texinfo-mode-map}

  These are files that are used as input for TeX to make printed manuals
and also to be turned into Info files with \\[makeinfo-buffer] or
the `makeinfo' program.  These files must be written in a very restricted and
modified version of TeX input format.

  Editing commands are like text-mode except that the syntax table is
set up so expression commands skip Texinfo bracket groups.  To see
what the Info version of a region of the Texinfo file will look like,
use \\[makeinfo-region], which runs `makeinfo' on the current region.

  You can show the structure of a Texinfo file with \\[texinfo-show-structure].
This command shows the structure of a Texinfo file by listing the
lines with the @-sign commands for @chapter, @section, and the like.
These lines are displayed in another window called the *Occur* window.
In that window, you can position the cursor over one of the lines and
use \\[occur-mode-goto-occurrence], to jump to the corresponding spot
in the Texinfo file.

  In addition, Texinfo mode provides commands that insert various
frequently used @-sign commands into the buffer.  You can use these
commands to save keystrokes.  And you can insert balanced braces with
\\[texinfo-insert-braces] and later use the command \\[up-list] to
move forward past the closing brace.

Also, Texinfo mode provides functions for automatically creating or
updating menus and node pointers.  These functions

  * insert the `Next', `Previous' and `Up' pointers of a node,
  * insert or update the menu for a section, and
  * create a master menu for a Texinfo source file.

Here are the functions:

    texinfo-update-node                \\[texinfo-update-node]
    texinfo-every-node-update          \\[texinfo-every-node-update]
    texinfo-sequential-node-update

    texinfo-make-menu                  \\[texinfo-make-menu]
    texinfo-all-menus-update           \\[texinfo-all-menus-update]
    texinfo-master-menu

    texinfo-indent-menu-description (column &optional region-p)

The `texinfo-column-for-description' variable specifies the column to
which menu descriptions are indented.

Passed an argument (a prefix argument, if interactive), the
`texinfo-update-node' and `texinfo-make-menu' functions do their jobs
in the region.

To use the updating commands, you must structure your Texinfo file
hierarchically, such that each `@node' line, with the exception of the
Top node, is accompanied by some kind of section line, such as an
`@chapter' or `@section' line.

If the file has a `top' node, it must be called `top' or `Top' and
be the first node in the file.

Entering Texinfo mode calls the value of text-mode-hook, and then the
value of texinfo-mode-hook." t nil)

;;;***

;;;### (autoloads (texinfo-sequential-node-update texinfo-every-node-update
;;;;;;  texinfo-update-node) "texnfo-upd" "textmodes/texnfo-upd.el"
;;;;;;  (14021 43245))
;;; Generated autoloads from textmodes/texnfo-upd.el

(autoload (quote texinfo-update-node) "texnfo-upd" "\
Without any prefix argument, update the node in which point is located.
Interactively, a prefix argument means to operate on the region.

The functions for creating or updating nodes and menus, and their
keybindings, are:

    texinfo-update-node (&optional beginning end)    \\[texinfo-update-node]
    texinfo-every-node-update ()                \\[texinfo-every-node-update]
    texinfo-sequential-node-update (&optional region-p)

    texinfo-make-menu (&optional region-p)      \\[texinfo-make-menu]
    texinfo-all-menus-update ()                 \\[texinfo-all-menus-update]
    texinfo-master-menu ()

    texinfo-indent-menu-description (column &optional region-p)

The `texinfo-column-for-description' variable specifies the column to
which menu descriptions are indented. Its default value is 32." t nil)

(autoload (quote texinfo-every-node-update) "texnfo-upd" "\
Update every node in a Texinfo file." t nil)

(autoload (quote texinfo-sequential-node-update) "texnfo-upd" "\
Update one node (or many) in a Texinfo file with sequential pointers.

This function causes the `Next' or `Previous' pointer to point to the
immediately preceding or following node, even if it is at a higher or
lower hierarchical level in the document.  Continually pressing `n' or
`p' takes you straight through the file.

Without any prefix argument, update the node in which point is located.
Non-nil argument (prefix, if interactive) means update the nodes in the
marked region.

This command makes it awkward to navigate among sections and
subsections; it should be used only for those documents that are meant
to be read like a novel rather than a reference, and for which the
Info `g*' command is inadequate." t nil)

;;;***

;;;### (autoloads (thai-pre-write-conversion thai-post-read-conversion
;;;;;;  thai-compose-buffer thai-compose-string thai-compose-region
;;;;;;  setup-thai-environment) "thai-util" "language/thai-util.el"
;;;;;;  (13969 36778))
;;; Generated autoloads from language/thai-util.el

(autoload (quote setup-thai-environment) "thai-util" "\
Setup multilingual environment (MULE) for Thai." t nil)

(autoload (quote thai-compose-region) "thai-util" "\
Compose Thai characters in the region.
When called from a program, expects two arguments,
positions (integers or markers) specifying the region." t nil)

(autoload (quote thai-compose-string) "thai-util" "\
Compose Thai characters in STRING and return the resulting string." nil nil)

(autoload (quote thai-compose-buffer) "thai-util" "\
Compose Thai characters in the current buffer." t nil)

(autoload (quote thai-post-read-conversion) "thai-util" nil nil nil)

(autoload (quote thai-pre-write-conversion) "thai-util" nil nil nil)

;;;***

;;;### (autoloads (thing-at-point bounds-of-thing-at-point forward-thing)
;;;;;;  "thingatpt" "thingatpt.el" (13916 24948))
;;; Generated autoloads from thingatpt.el

(autoload (quote forward-thing) "thingatpt" "\
Move forward to the end of the next THING." nil nil)

(autoload (quote bounds-of-thing-at-point) "thingatpt" "\
Determine the start and end buffer locations for the THING at point.
THING is a symbol which specifies the kind of syntactic entity you want.
Possibilities include `symbol', `list', `sexp', `defun', `filename', `url',
`word', `sentence', `whitespace', `line', `page' and others.

See the file `thingatpt.el' for documentation on how to define
a symbol as a valid THING.

The value is a cons cell (START . END) giving the start and end positions
of the textual entity that was found." nil nil)

(autoload (quote thing-at-point) "thingatpt" "\
Return the THING at point.
THING is a symbol which specifies the kind of syntactic entity you want.
Possibilities include `symbol', `list', `sexp', `defun', `filename', `url',
`word', `sentence', `whitespace', `line', `page' and others.

See the file `thingatpt.el' for documentation on how to define
a symbol as a valid THING." nil nil)

;;;***

;;;### (autoloads (tibetan-pre-write-conversion tibetan-post-read-conversion
;;;;;;  tibetan-compose-buffer tibetan-decompose-buffer tibetan-compose-region
;;;;;;  tibetan-decompose-region tibetan-composition tibetan-compose-string
;;;;;;  tibetan-vertical-stacking tibetan-complete-char-examin tibetan-composable-examin
;;;;;;  tibetan-char-examin tibetan-transcription-to-tibetan tibetan-tibetan-to-transcription
;;;;;;  setup-tibetan-environment) "tibet-util" "language/tibet-util.el"
;;;;;;  (13994 41097))
;;; Generated autoloads from language/tibet-util.el

(autoload (quote setup-tibetan-environment) "tibet-util" nil t nil)

(autoload (quote tibetan-tibetan-to-transcription) "tibet-util" "\
Return a transcription string of Tibetan character CH" nil nil)

(autoload (quote tibetan-transcription-to-tibetan) "tibet-util" "\
Translate Roman transcription into a sequence of Tibetan components." nil nil)

(autoload (quote tibetan-char-examin) "tibet-util" "\
Check if char CH is Tibetan character.
Returns non-nil if CH is Tibetan. Otherwise, returns nil." nil nil)

(autoload (quote tibetan-composable-examin) "tibet-util" "\
Check if Tibetan char CH is composable.
Returns t if CH is a composable char (i.e. neither punctuation nor digit)." nil nil)

(autoload (quote tibetan-complete-char-examin) "tibet-util" "\
Check if composite char CH contains one or more vowel/vowel modifiers.
Returns non-nil, if CH contains vowel/vowel modifiers." nil nil)

(autoload (quote tibetan-vertical-stacking) "tibet-util" "\
Return a vertically stacked composite char consisting of FIRST and SECOND.
If UPWARD is non-nil, then SECOND is put above FIRST." nil nil)

(autoload (quote tibetan-compose-string) "tibet-util" "\
Compose a sequence of Tibetan character components into a composite character.
Returns a string containing a composite character." nil nil)

(autoload (quote tibetan-composition) "tibet-util" "\
Interface to quail input method.
Takes two arguments: char PC and string KEY, where PC is the preceding
character to be composed with current input KEY.
Returns a string which is the result of composition." nil nil)

(autoload (quote tibetan-decompose-region) "tibet-util" "\
Decompose Tibetan characters in the region BEG END into their components.
Components are: base and subjoined consonants, vowel signs, vowel modifiers.
One column punctuations are converted to their 2 column equivalents." t nil)

(autoload (quote tibetan-compose-region) "tibet-util" "\
Make composite chars from Tibetan character components in the region BEG END.
Two column punctuations are converted to their 1 column equivalents." t nil)

(autoload (quote tibetan-decompose-buffer) "tibet-util" "\
Decomposes Tibetan characters in the buffer into their components.
See also docstring of the function tibetan-decompose-region." t nil)

(autoload (quote tibetan-compose-buffer) "tibet-util" "\
Composes Tibetan character components in the buffer.
See also docstring of the function tibetan-compose-region." t nil)

(autoload (quote tibetan-post-read-conversion) "tibet-util" nil nil nil)

(autoload (quote tibetan-pre-write-conversion) "tibet-util" nil nil nil)

;;;***

;;;### (autoloads (display-time-mode display-time display-time-day-and-date)
;;;;;;  "time" "time.el" (14127 12135))
;;; Generated autoloads from time.el

(defvar display-time-day-and-date nil "\
*Non-nil means \\[display-time] should display day and date as well as time.")

(autoload (quote display-time) "time" "\
Enable display of time, load level, and mail flag in mode lines.
This display updates automatically every minute.
If `display-time-day-and-date' is non-nil, the current day and date
are displayed as well.
This runs the normal hook `display-time-hook' after each update." t nil)

(autoload (quote display-time-mode) "time" "\
Toggle display of time, load level, and mail flag in mode lines.
With a numeric arg, enable this display if arg is positive.

When this display is enabled, it updates automatically every minute.
If `display-time-day-and-date' is non-nil, the current day and date
are displayed as well.
This runs the normal hook `display-time-hook' after each update." t nil)

;;;***

;;;### (autoloads (time-stamp-toggle-active time-stamp) "time-stamp"
;;;;;;  "time-stamp.el" (14011 51994))
;;; Generated autoloads from time-stamp.el

(autoload (quote time-stamp) "time-stamp" "\
Update the time stamp string in the buffer.
A template in a file can be automatically updated with a new time stamp
every time you save the file.  Add this line to your .emacs file:
    (add-hook 'write-file-hooks 'time-stamp)
Normally the template must appear in the first 8 lines of a file and
look like one of the following:
      Time-stamp: <>
      Time-stamp: \" \"
The time stamp is written between the brackets or quotes:
      Time-stamp: <1998-02-18 10:20:51 gildea>
The time stamp is updated only if the variable `time-stamp-active' is non-nil.
The format of the time stamp is set by the variable `time-stamp-format'.
The variables `time-stamp-line-limit', `time-stamp-start',
and `time-stamp-end' control finding the template." t nil)

(autoload (quote time-stamp-toggle-active) "time-stamp" "\
Toggle `time-stamp-active', setting whether \\[time-stamp] updates a buffer.
With arg, turn time stamping on if and only if arg is positive." t nil)

;;;***

;;;### (autoloads (with-timeout run-with-idle-timer add-timeout run-with-timer
;;;;;;  run-at-time cancel-function-timers cancel-timer) "timer"
;;;;;;  "timer.el" (13316 52718))
;;; Generated autoloads from timer.el

(defalias (quote disable-timeout) (quote cancel-timer))

(autoload (quote cancel-timer) "timer" "\
Remove TIMER from the list of active timers." nil nil)

(autoload (quote cancel-function-timers) "timer" "\
Cancel all timers scheduled by `run-at-time' which would run FUNCTION." t nil)

(autoload (quote run-at-time) "timer" "\
Perform an action at time TIME.
Repeat the action every REPEAT seconds, if REPEAT is non-nil.
TIME should be a string like \"11:23pm\", nil meaning now, a number of seconds
from now, a value from `current-time', or t (with non-nil REPEAT)
meaning the next integral multiple of REPEAT.
REPEAT may be an integer or floating point number.
The action is to call FUNCTION with arguments ARGS.

This function returns a timer object which you can use in `cancel-timer'." t nil)

(autoload (quote run-with-timer) "timer" "\
Perform an action after a delay of SECS seconds.
Repeat the action every REPEAT seconds, if REPEAT is non-nil.
SECS and REPEAT may be integers or floating point numbers.
The action is to call FUNCTION with arguments ARGS.

This function returns a timer object which you can use in `cancel-timer'." t nil)

(autoload (quote add-timeout) "timer" "\
Add a timer to run SECS seconds from now, to call FUNCTION on OBJECT.
If REPEAT is non-nil, repeat the timer every REPEAT seconds.
This function is for compatibility; see also `run-with-timer'." nil nil)

(autoload (quote run-with-idle-timer) "timer" "\
Perform an action the next time Emacs is idle for SECS seconds.
If REPEAT is non-nil, do this each time Emacs is idle for SECS seconds.
SECS may be an integer or a floating point number.
The action is to call FUNCTION with arguments ARGS.

This function returns a timer object which you can use in `cancel-timer'." t nil)
 (put 'with-timeout 'lisp-indent-function 1)

(autoload (quote with-timeout) "timer" "\
Run BODY, but if it doesn't finish in SECONDS seconds, give up.
If we give up, we run the TIMEOUT-FORMS and return the value of the last one.
The call should look like:
 (with-timeout (SECONDS TIMEOUT-FORMS...) BODY...)
The timeout is checked whenever Emacs waits for some kind of external
event (such as keyboard input, input from subprocesses, or a certain time);
if the program loops without waiting in any way, the timeout will not
be detected." nil (quote macro))

;;;***

;;;### (autoloads (batch-titdic-convert titdic-convert) "titdic-cnv"
;;;;;;  "international/titdic-cnv.el" (13618 46611))
;;; Generated autoloads from international/titdic-cnv.el

(autoload (quote titdic-convert) "titdic-cnv" "\
Convert a TIT dictionary of FILENAME into a Quail package.
Optional argument DIRNAME if specified is the directory name under which
the generated Quail package is saved." t nil)

(autoload (quote batch-titdic-convert) "titdic-cnv" "\
Run `titdic-convert' on the files remaining on the command line.
Use this from the command line, with `-batch';
it won't work in an interactive Emacs.
For example, invoke \"emacs -batch -f batch-titdic-convert XXX.tit\" to
 generate Quail package file \"xxx.el\" from TIT dictionary file \"XXX.tit\".
To get complete usage, invoke \"emacs -batch -f batch-titdic-convert -h\"." nil nil)

;;;***

;;;### (autoloads (tmm-prompt tmm-menubar-mouse tmm-menubar) "tmm"
;;;;;;  "tmm.el" (13700 6160))
;;; Generated autoloads from tmm.el
 (define-key global-map "\M-`" 'tmm-menubar)
 (define-key global-map [f10] 'tmm-menubar)
 (define-key global-map [menu-bar mouse-1] 'tmm-menubar-mouse)

(autoload (quote tmm-menubar) "tmm" "\
Text-mode emulation of looking and choosing from a menubar.
See the documentation for `tmm-prompt'.
X-POSITION, if non-nil, specifies a horizontal position within the menu bar;
we make that menu bar item (the one at that position) the default choice." t nil)

(autoload (quote tmm-menubar-mouse) "tmm" "\
Text-mode emulation of looking and choosing from a menubar.
This command is used when you click the mouse in the menubar
on a console which has no window system but does have a mouse.
See the documentation for `tmm-prompt'." t nil)

(autoload (quote tmm-prompt) "tmm" "\
Text-mode emulation of calling the bindings in keymap.
Creates a text-mode menu of possible choices.  You can access the elements
in the menu in two ways:
   *)  via history mechanism from minibuffer;
   *)  Or via completion-buffer that is automatically shown.
The last alternative is currently a hack, you cannot use mouse reliably.

MENU is like the MENU argument to `x-popup-menu': either a
keymap or an alist of alists.
DEFAULT-ITEM, if non-nil, specifies an initial default choice.
Its value should be an event that has a binding in MENU." nil nil)

;;;***

;;;### (autoloads (tooltip-mode) "tooltip" "tooltip.el" (14228 23896))
;;; Generated autoloads from tooltip.el

(autoload (quote tooltip-mode) "tooltip" "\
Mode for tooltip display.
With ARG, turn tooltip mode on if and only if ARG is positive." t nil)

;;;***

;;;### (autoloads (tpu-edt-on) "tpu-edt" "emulation/tpu-edt.el" (13697
;;;;;;  41987))
;;; Generated autoloads from emulation/tpu-edt.el

(fset (quote tpu-edt-mode) (quote tpu-edt-on))

(fset (quote tpu-edt) (quote tpu-edt-on))

(autoload (quote tpu-edt-on) "tpu-edt" "\
Turn on TPU/edt emulation." t nil)

;;;***

;;;### (autoloads (tpu-set-cursor-bound tpu-set-cursor-free tpu-set-scroll-margins)
;;;;;;  "tpu-extras" "emulation/tpu-extras.el" (13623 36813))
;;; Generated autoloads from emulation/tpu-extras.el

(autoload (quote tpu-set-scroll-margins) "tpu-extras" "\
Set scroll margins." t nil)

(autoload (quote tpu-set-cursor-free) "tpu-extras" "\
Allow the cursor to move freely about the screen." t nil)

(autoload (quote tpu-set-cursor-bound) "tpu-extras" "\
Constrain the cursor to the flow of the text." t nil)

;;;***

;;;### (autoloads (tq-create) "tq" "emacs-lisp/tq.el" (13509 21722))
;;; Generated autoloads from emacs-lisp/tq.el

(autoload (quote tq-create) "tq" "\
Create and return a transaction queue communicating with PROCESS.
PROCESS should be a subprocess capable of sending and receiving
streams of bytes.  It may be a local process, or it may be connected
to a tcp server on another machine." nil nil)

;;;***

;;;### (autoloads (trace-function-background trace-function trace-buffer)
;;;;;;  "trace" "emacs-lisp/trace.el" (13607 52218))
;;; Generated autoloads from emacs-lisp/trace.el

(defvar trace-buffer "*trace-output*" "\
*Trace output will by default go to that buffer.")

(autoload (quote trace-function) "trace" "\
Traces FUNCTION with trace output going to BUFFER.
For every call of FUNCTION Lisp-style trace messages that display argument
and return values will be inserted into BUFFER. This function generates the
trace advice for FUNCTION and activates it together with any other advice
there might be!! The trace BUFFER will popup whenever FUNCTION is called.
Do not use this to trace functions that switch buffers or do any other
display oriented stuff, use `trace-function-background' instead." t nil)

(autoload (quote trace-function-background) "trace" "\
Traces FUNCTION with trace output going quietly to BUFFER.
For every call of FUNCTION Lisp-style trace messages that display argument
and return values will be inserted into BUFFER. This function generates the
trace advice for FUNCTION and activates it together with any other advice
there might be!! Trace output will quietly go to BUFFER without changing
the window or buffer configuration at all." t nil)

;;;***

;;;### (autoloads (2C-split 2C-associate-buffer 2C-two-columns) "two-column"
;;;;;;  "textmodes/two-column.el" (13940 33653))
;;; Generated autoloads from textmodes/two-column.el
 (autoload '2C-command "two-column" () t 'keymap)
 (global-set-key "\C-x6" '2C-command)
 (global-set-key [f2] '2C-command)

(autoload (quote 2C-two-columns) "two-column" "\
Split current window vertically for two-column editing.
When called the first time, associates a buffer with the current
buffer in two-column minor mode (see  \\[describe-mode] ).
Runs `2C-other-buffer-hook' in the new buffer.
When called again, restores the screen layout with the current buffer
first and the associated buffer to its right." t nil)

(autoload (quote 2C-associate-buffer) "two-column" "\
Associate another buffer with this one in two-column minor mode.
Can also be used to associate a just previously visited file, by
accepting the proposed default buffer.

\(See  \\[describe-mode] .)" t nil)

(autoload (quote 2C-split) "two-column" "\
Split a two-column text at point, into two buffers in two-column minor mode.
Point becomes the local value of `2C-window-width'.  Only lines that
have the ARG same preceding characters at that column get split.  The
ARG preceding characters without any leading whitespace become the local
value for `2C-separator'.  This way lines that continue across both
columns remain untouched in the first buffer.

This function can be used with a prototype line, to set up things.  You
write the first line of each column and then split that line.  E.g.:

First column's text    sSs  Second column's text
		       \\___/\\
			/    \\
   5 character Separator      You type  M-5 \\[2C-split]  with the point here.

\(See  \\[describe-mode] .)" t nil)

;;;***

;;;### (autoloads (type-break-guesstimate-keystroke-threshold type-break-statistics
;;;;;;  type-break type-break-mode type-break-keystroke-threshold
;;;;;;  type-break-good-rest-interval type-break-interval type-break-mode)
;;;;;;  "type-break" "type-break.el" (14118 2396))
;;; Generated autoloads from type-break.el

(defvar type-break-mode nil "\
Toggle typing break mode.
See the docstring for the `type-break-mode' command for more information.
Setting this variable directly does not take effect;
use either \\[customize] or the function `type-break-mode'.")

(custom-add-to-group (quote type-break) (quote type-break-mode) (quote custom-variable))

(custom-add-load (quote type-break-mode) (quote type-break))

(defvar type-break-interval (* 60 60) "\
*Number of seconds between scheduled typing breaks.")

(defvar type-break-good-rest-interval (/ type-break-interval 6) "\
*Number of seconds of idle time considered to be an adequate typing rest.

When this variable is non-`nil', emacs checks the idle time between
keystrokes.  If this idle time is long enough to be considered a \"good\"
rest from typing, then the next typing break is simply rescheduled for later.

If a break is interrupted before this much time elapses, the user will be
asked whether or not really to interrupt the break.")

(defvar type-break-keystroke-threshold (let* ((wpm 35) (avg-word-length 5) (upper (* wpm avg-word-length (/ type-break-interval 60))) (lower (/ upper 5))) (cons lower upper)) "\
*Upper and lower bound on number of keystrokes for considering typing break.
This structure is a pair of numbers (MIN . MAX).

The first number is the minimum number of keystrokes that must have been
entered since the last typing break before considering another one, even if
the scheduled time has elapsed; the break is simply rescheduled until later
if the minimum threshold hasn't been reached.  If this first value is nil,
then there is no minimum threshold; as soon as the scheduled time has
elapsed, the user will always be queried.

The second number is the maximum number of keystrokes that can be entered
before a typing break is requested immediately, pre-empting the originally
scheduled break.  If this second value is nil, then no pre-emptive breaks
will occur; only scheduled ones will.

Keys with bucky bits (shift, control, meta, etc) are counted as only one
keystroke even though they really require multiple keys to generate them.

The command `type-break-guesstimate-keystroke-threshold' can be used to
guess a reasonably good pair of values for this variable.")

(autoload (quote type-break-mode) "type-break" "\
Enable or disable typing-break mode.
This is a minor mode, but it is global to all buffers by default.

When this mode is enabled, the user is encouraged to take typing breaks at
appropriate intervals; either after a specified amount of time or when the
user has exceeded a keystroke threshold.  When the time arrives, the user
is asked to take a break.  If the user refuses at that time, emacs will ask
again in a short period of time.  The idea is to give the user enough time
to find a good breaking point in his or her work, but be sufficiently
annoying to discourage putting typing breaks off indefinitely.

A negative prefix argument disables this mode.
No argument or any non-negative argument enables it.

The user may enable or disable this mode by setting the variable of the
same name, though setting it in that way doesn't reschedule a break or
reset the keystroke counter.

If the mode was previously disabled and is enabled as a consequence of
calling this function, it schedules a break with `type-break-schedule' to
make sure one occurs (the user can call that command to reschedule the
break at any time).  It also initializes the keystroke counter.

The variable `type-break-interval' specifies the number of seconds to
schedule between regular typing breaks.  This variable doesn't directly
affect the time schedule; it simply provides a default for the
`type-break-schedule' command.

If set, the variable `type-break-good-rest-interval' specifies the minimum
amount of time which is considered a reasonable typing break.  Whenever
that time has elapsed, typing breaks are automatically rescheduled for
later even if emacs didn't prompt you to take one first.  Also, if a break
is ended before this much time has elapsed, the user will be asked whether
or not to continue.

The variable `type-break-keystroke-threshold' is used to determine the
thresholds at which typing breaks should be considered.  You can use
the command `type-break-guesstimate-keystroke-threshold' to try to
approximate good values for this.

There are several variables that affect how or when warning messages about
imminent typing breaks are displayed.  They include:

        `type-break-mode-line-message-mode'
        `type-break-time-warning-intervals'
        `type-break-keystroke-warning-intervals'
        `type-break-warning-repeat'
        `type-break-warning-countdown-string'
        `type-break-warning-countdown-string-type'

There are several variables that affect if, how, and when queries to begin
a typing break occur.  They include:

        `type-break-query-mode'
        `type-break-query-function'
        `type-break-query-interval'

Finally, the command `type-break-statistics' prints interesting things." t nil)

(autoload (quote type-break) "type-break" "\
Take a typing break.

During the break, a demo selected from the functions listed in
`type-break-demo-functions' is run.

After the typing break is finished, the next break is scheduled
as per the function `type-break-schedule'." t nil)

(autoload (quote type-break-statistics) "type-break" "\
Print statistics about typing breaks in a temporary buffer.
This includes the last time a typing break was taken, when the next one is
scheduled, the keystroke thresholds and the current keystroke count, etc." t nil)

(autoload (quote type-break-guesstimate-keystroke-threshold) "type-break" "\
Guess values for the minimum/maximum keystroke threshold for typing breaks.

If called interactively, the user is prompted for their guess as to how
many words per minute they usually type.  This value should not be your
maximum WPM, but your average.  Of course, this is harder to gauge since it
can vary considerably depending on what you are doing.  For example, one
tends to type less when debugging a program as opposed to writing
documentation.  (Perhaps a separate program should be written to estimate
average typing speed.)

From that, this command sets the values in `type-break-keystroke-threshold'
based on a fairly simple algorithm involving assumptions about the average
length of words (5).  For the minimum threshold, it uses about a fifth of
the computed maximum threshold.

When called from lisp programs, the optional args WORDLEN and FRAC can be
used to override the default assumption about average word length and the
fraction of the maximum threshold to which to set the minimum threshold.
FRAC should be the inverse of the fractional value; for example, a value of
2 would mean to use one half, a value of 4 would mean to use one quarter, etc." t nil)

;;;***

;;;### (autoloads (ununderline-region underline-region) "underline"
;;;;;;  "textmodes/underline.el" (13229 29692))
;;; Generated autoloads from textmodes/underline.el

(autoload (quote underline-region) "underline" "\
Underline all nonblank characters in the region.
Works by overstriking underscores.
Called from program, takes two arguments START and END
which specify the range to operate on." t nil)

(autoload (quote ununderline-region) "underline" "\
Remove all underlining (overstruck underscores) in the region.
Called from program, takes two arguments START and END
which specify the range to operate on." t nil)

;;;***

;;;### (autoloads (unforward-rmail-message undigestify-rmail-message)
;;;;;;  "undigest" "mail/undigest.el" (13475 35425))
;;; Generated autoloads from mail/undigest.el

(autoload (quote undigestify-rmail-message) "undigest" "\
Break up a digest message into its constituent messages.
Leaves original message, deleted, before the undigestified messages." t nil)

(autoload (quote unforward-rmail-message) "undigest" "\
Extract a forwarded message from the containing message.
This puts the forwarded message into a separate rmail message
following the containing message." t nil)

;;;***

;;;### (autoloads (unrmail batch-unrmail) "unrmail" "mail/unrmail.el"
;;;;;;  (13229 29737))
;;; Generated autoloads from mail/unrmail.el

(autoload (quote batch-unrmail) "unrmail" "\
Convert Rmail files to system inbox format.
Specify the input Rmail file names as command line arguments.
For each Rmail file, the corresponding output file name
is made by adding `.mail' at the end.
For example, invoke `emacs -batch -f batch-unrmail RMAIL'." nil nil)

(autoload (quote unrmail) "unrmail" "\
Convert Rmail file FILE to system inbox format file TO-FILE." t nil)

;;;***

;;;### (autoloads (ask-user-about-supersession-threat ask-user-about-lock)
;;;;;;  "userlock" "userlock.el" (13323 27503))
;;; Generated autoloads from userlock.el

(autoload (quote ask-user-about-lock) "userlock" "\
Ask user what to do when he wants to edit FILE but it is locked by OPPONENT.
This function has a choice of three things to do:
  do (signal 'buffer-file-locked (list FILE OPPONENT))
    to refrain from editing the file
  return t (grab the lock on the file)
  return nil (edit the file even though it is locked).
You can redefine this function to choose among those three alternatives
in any way you like." nil nil)

(autoload (quote ask-user-about-supersession-threat) "userlock" "\
Ask a user who is about to modify an obsolete buffer what to do.
This function has two choices: it can return, in which case the modification
of the buffer will proceed, or it can (signal 'file-supersession (file)),
in which case the proposed buffer modification will not be made.

You can rewrite this to use any criterion you like to choose which one to do.
The buffer in question is current when this function is called." nil nil)

;;;***

;;;### (autoloads (vc-annotate vc-update-change-log vc-rename-file
;;;;;;  vc-cancel-version vc-revert-buffer vc-print-log vc-retrieve-snapshot
;;;;;;  vc-create-snapshot vc-directory vc-resolve-conflicts vc-merge
;;;;;;  vc-insert-headers vc-version-other-window vc-diff vc-register
;;;;;;  vc-next-action edit-vc-file with-vc-file vc-annotate-mode-hook
;;;;;;  vc-before-checkin-hook vc-checkin-hook) "vc" "vc.el" (14201
;;;;;;  50318))
;;; Generated autoloads from vc.el

(defvar vc-checkin-hook nil "\
*Normal hook (list of functions) run after a checkin is done.
See `run-hooks'.")

(defvar vc-before-checkin-hook nil "\
*Normal hook (list of functions) run before a file gets checked in.  
See `run-hooks'.")

(defvar vc-annotate-mode-hook nil "\
*Hooks to run when VC-Annotate mode is turned on.")

(autoload (quote with-vc-file) "vc" "\
Execute BODY, checking out a writable copy of FILE first if necessary.
After BODY has been executed, check-in FILE with COMMENT (a string).  
FILE is passed through `expand-file-name'; BODY executed within 
`save-excursion'.  If FILE is not under version control, or locked by 
somebody else, signal error." nil (quote macro))

(autoload (quote edit-vc-file) "vc" "\
Edit FILE under version control, executing BODY.  Checkin with COMMENT.
This macro uses `with-vc-file', passing args to it.
However, before executing BODY, find FILE, and after BODY, save buffer." nil (quote macro))

(autoload (quote vc-next-action) "vc" "\
Do the next logical checkin or checkout operation on the current file.
   If you call this from within a VC dired buffer with no files marked,
it will operate on the file in the current line.
   If you call this from within a VC dired buffer, and one or more
files are marked, it will accept a log message and then operate on
each one.  The log message will be used as a comment for any register
or checkin operations, but ignored when doing checkouts.  Attempted
lock steals will raise an error.
   A prefix argument lets you specify the version number to use.

For RCS and SCCS files:
   If the file is not already registered, this registers it for version
control.
   If the file is registered and not locked by anyone, this checks out
a writable and locked file ready for editing.
   If the file is checked out and locked by the calling user, this
first checks to see if the file has changed since checkout.  If not,
it performs a revert.
   If the file has been changed, this pops up a buffer for entry
of a log message; when the message has been entered, it checks in the
resulting changes along with the log message as change commentary.  If
the variable `vc-keep-workfiles' is non-nil (which is its default), a
read-only copy of the changed file is left in place afterwards.
   If the file is registered and locked by someone else, you are given
the option to steal the lock.

For CVS files:
   If the file is not already registered, this registers it for version
control.  This does a \"cvs add\", but no \"cvs commit\".
   If the file is added but not committed, it is committed.
   If your working file is changed, but the repository file is
unchanged, this pops up a buffer for entry of a log message; when the
message has been entered, it checks in the resulting changes along
with the logmessage as change commentary.  A writable file is retained.
   If the repository file is changed, you are asked if you want to
merge in the changes into your working copy." t nil)

(autoload (quote vc-register) "vc" "\
Register the current file into your version-control system." t nil)

(autoload (quote vc-diff) "vc" "\
Display diffs between file versions.
Normally this compares the current file and buffer with the most recent 
checked in version of that file.  This uses no arguments.
With a prefix argument, it reads the file name to use
and two version designators specifying which versions to compare." t nil)

(autoload (quote vc-version-other-window) "vc" "\
Visit version REV of the current buffer in another window.
If the current buffer is named `F', the version is named `F.~REV~'.
If `F.~REV~' already exists, it is used instead of being re-created." t nil)

(autoload (quote vc-insert-headers) "vc" "\
Insert headers in a file for use with your version-control system.
Headers desired are inserted at point, and are pulled from
the variable `vc-header-alist'." t nil)

(autoload (quote vc-merge) "vc" nil t nil)

(autoload (quote vc-resolve-conflicts) "vc" "\
Invoke ediff to resolve conflicts in the current buffer.
The conflicts must be marked with rcsmerge conflict markers." t nil)

(autoload (quote vc-directory) "vc" nil t nil)

(autoload (quote vc-create-snapshot) "vc" "\
Make a snapshot called NAME.
The snapshot is made from all registered files at or below the current
directory.  For each file, the version level of its latest
version becomes part of the named configuration." t nil)

(autoload (quote vc-retrieve-snapshot) "vc" "\
Retrieve the snapshot called NAME, or latest versions if NAME is empty.
When retrieving a snapshot, there must not be any locked files at or below
the current directory.  If none are locked, all registered files are 
checked out (unlocked) at their version levels in the snapshot NAME.
If NAME is the empty string, all registered files that are not currently 
locked are updated to the latest versions." t nil)

(autoload (quote vc-print-log) "vc" "\
List the change log of the current buffer in a window." t nil)

(autoload (quote vc-revert-buffer) "vc" "\
Revert the current buffer's file back to the version it was based on.
This asks for confirmation if the buffer contents are not identical
to that version.  Note that for RCS and CVS, this function does not 
automatically pick up newer changes found in the master file; 
use C-u \\[vc-next-action] RET to do so." t nil)

(autoload (quote vc-cancel-version) "vc" "\
Get rid of most recently checked in version of this file.
A prefix argument means do not revert the buffer afterwards." t nil)

(autoload (quote vc-rename-file) "vc" "\
Rename file OLD to NEW, and rename its master file likewise." t nil)

(autoload (quote vc-update-change-log) "vc" "\
Find change log file and add entries from recent RCS/CVS logs.
Normally, find log entries for all registered files in the default
directory using `rcs2log', which finds CVS logs preferentially.
The mark is left at the end of the text prepended to the change log.

With prefix arg of C-u, only find log entries for the current buffer's file.

With any numeric prefix arg, find log entries for all currently visited
files that are under version control.  This puts all the entries in the
log for the default directory, which may not be appropriate.

From a program, any arguments are assumed to be filenames and are
passed to the `rcs2log' script after massaging to be relative to the
default directory." t nil)

(autoload (quote vc-annotate) "vc" "\
Display the result of the CVS `annotate' command using colors.
New lines are displayed in red, old in blue.
A prefix argument specifies a factor for stretching the time scale.

`vc-annotate-menu-elements' customizes the menu elements of the
mode-specific menu. `vc-annotate-color-map' and
`vc-annotate-very-old-color' defines the mapping of time to
colors. `vc-annotate-background' specifies the background color." t nil)

;;;***

;;;### (autoloads (vhdl-mode) "vhdl-mode" "progmodes/vhdl-mode.el"
;;;;;;  (14177 57984))
;;; Generated autoloads from progmodes/vhdl-mode.el

(autoload (quote vhdl-mode) "vhdl-mode" "\
Major mode for editing VHDL code.

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
  (see CUSTOMIZATION).

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
  (`\\[vhdl-indent-region]') or buffer (menu).  Argument and port lists are indented normally
  (nil) or relative to the opening parenthesis (non-nil) according to variable
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
  (menu, `\\[vhdl-make]').  This only works if an appropriate Makefile exists.
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
  (if `ps-print-color-p' is non-nil) is possible using the standard Emacs
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
    (setq auto-mode-alist (cons '(\"\\\\.xxx\\\\'\" . vhdl-mode) auto-mode-alist))

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

\\{vhdl-mode-map}" t nil)

;;;***

;;;### (autoloads (vi-mode) "vi" "emulation/vi.el" (13229 29770))
;;; Generated autoloads from emulation/vi.el

(autoload (quote vi-mode) "vi" "\
Major mode that acts like the `vi' editor.
The purpose of this mode is to provide you the combined power of vi (namely,
the \"cross product\" effect of commands and repeat last changes) and Emacs.

This command redefines nearly all keys to look like vi commands.
It records the previous major mode, and any vi command for input
\(`i', `a', `s', etc.) switches back to that mode.
Thus, ordinary Emacs (in whatever major mode you had been using)
is \"input\" mode as far as vi is concerned.

To get back into vi from \"input\" mode, you must issue this command again.
Therefore, it is recommended that you assign it to a key.

Major differences between this mode and real vi :

* Limitations and unsupported features
  - Search patterns with line offset (e.g. /pat/+3 or /pat/z.) are
    not supported.
  - Ex commands are not implemented; try ':' to get some hints.
  - No line undo (i.e. the 'U' command), but multi-undo is a standard feature.

* Modifications
  - The stopping positions for some point motion commands (word boundary,
    pattern search) are slightly different from standard 'vi'.
    Also, no automatic wrap around at end of buffer for pattern searching.
  - Since changes are done in two steps (deletion then insertion), you need
    to undo twice to completely undo a change command.  But this is not needed
    for undoing a repeated change command.
  - No need to set/unset 'magic', to search for a string with regular expr
    in it just put a prefix arg for the search commands.  Replace cmds too.
  - ^R is bound to incremental backward search, so use ^L to redraw screen.

* Extensions
  - Some standard (or modified) Emacs commands were integrated, such as
    incremental search, query replace, transpose objects, and keyboard macros.
  - In command state, ^X links to the 'ctl-x-map', and ESC can be linked to
    esc-map or set undefined.  These can give you the full power of Emacs.
  - See vi-com-map for those keys that are extensions to standard vi, e.g.
    `vi-name-last-change-or-macro', `vi-verify-spelling', `vi-locate-def',
    `vi-mark-region', and 'vi-quote-words'.  Some of them are quite handy.
  - Use \\[vi-switch-mode] to switch among different modes quickly.
  
Syntax table and abbrevs while in vi mode remain as they were in Emacs." t nil)

;;;***

;;;### (autoloads (viqr-pre-write-conversion viqr-post-read-conversion
;;;;;;  viet-encode-viqr-buffer viet-encode-viqr-region viet-decode-viqr-buffer
;;;;;;  viet-decode-viqr-region setup-vietnamese-environment viet-encode-viscii-char)
;;;;;;  "viet-util" "language/viet-util.el" (13876 11221))
;;; Generated autoloads from language/viet-util.el

(autoload (quote viet-encode-viscii-char) "viet-util" "\
Return VISCII character code of CHAR if appropriate." nil nil)

(autoload (quote setup-vietnamese-environment) "viet-util" "\
Setup multilingual environment (MULE) for Vietnamese VISCII users." t nil)

(autoload (quote viet-decode-viqr-region) "viet-util" "\
Convert `VIQR' mnemonics of the current region to Vietnamese characaters.
When called from a program, expects two arguments,
positions (integers or markers) specifying the stretch of the region." t nil)

(autoload (quote viet-decode-viqr-buffer) "viet-util" "\
Convert `VIQR' mnemonics of the current buffer to Vietnamese characaters." t nil)

(autoload (quote viet-encode-viqr-region) "viet-util" "\
Convert Vietnamese characaters of the current region to `VIQR' mnemonics.
When called from a program, expects two arguments,
positions (integers or markers) specifying the stretch of the region." t nil)

(autoload (quote viet-encode-viqr-buffer) "viet-util" "\
Convert Vietnamese characaters of the current buffer to `VIQR' mnemonics." t nil)

(autoload (quote viqr-post-read-conversion) "viet-util" nil nil nil)

(autoload (quote viqr-pre-write-conversion) "viet-util" nil nil nil)

;;;***

;;;### (autoloads (View-exit-and-edit view-mode-enter view-mode view-buffer-other-frame
;;;;;;  view-buffer-other-window view-buffer view-file-other-frame
;;;;;;  view-file-other-window view-file) "view" "view.el" (13968
;;;;;;  41260))
;;; Generated autoloads from view.el

(defvar view-mode nil "\
Non-nil if View mode is enabled.
Don't change this variable directly, you must change it by one of the
functions that enable or disable view mode.")

(make-variable-buffer-local (quote view-mode))

(autoload (quote view-file) "view" "\
View FILE in View mode, returning to previous buffer when done.
Emacs commands editing the buffer contents are not available; instead,
a special set of commands (mostly letters and punctuation)
are defined for moving around in the buffer.
Space scrolls forward, Delete scrolls backward.
For list of all View commands, type H or h while viewing.

This command runs the normal hook `view-mode-hook'." t nil)

(autoload (quote view-file-other-window) "view" "\
View FILE in View mode in another window.
Return that window to its previous buffer when done.
Emacs commands editing the buffer contents are not available; instead,
a special set of commands (mostly letters and punctuation)
are defined for moving around in the buffer.
Space scrolls forward, Delete scrolls backward.
For list of all View commands, type H or h while viewing.

This command runs the normal hook `view-mode-hook'." t nil)

(autoload (quote view-file-other-frame) "view" "\
View FILE in View mode in another frame.
Maybe delete other frame and/or return to previous buffer when done.
Emacs commands editing the buffer contents are not available; instead,
a special set of commands (mostly letters and punctuation)
are defined for moving around in the buffer.
Space scrolls forward, Delete scrolls backward.
For list of all View commands, type H or h while viewing.

This command runs the normal hook `view-mode-hook'." t nil)

(autoload (quote view-buffer) "view" "\
View BUFFER in View mode, returning to previous buffer when done.
Emacs commands editing the buffer contents are not available; instead,
a special set of commands (mostly letters and punctuation)
are defined for moving around in the buffer.
Space scrolls forward, Delete scrolls backward.
For list of all View commands, type H or h while viewing.

This command runs the normal hook `view-mode-hook'.

Optional argument EXIT-ACTION is either nil or a function with buffer as
argument.  This function is called when finished viewing buffer.
Use this argument instead of explicitly setting `view-exit-action'." t nil)

(autoload (quote view-buffer-other-window) "view" "\
View BUFFER in View mode in another window.
Return to previous buffer when done, unless optional NOT-RETURN is non-nil.
Emacs commands editing the buffer contents are not available; instead,
a special set of commands (mostly letters and punctuation)
are defined for moving around in the buffer.
Space scrolls forward, Delete scrolls backward.
For list of all View commands, type H or h while viewing.

This command runs the normal hook `view-mode-hook'.

Optional argument EXIT-ACTION is either nil or a function with buffer as
argument.  This function is called when finished viewing buffer.
Use this argument instead of explicitly setting `view-exit-action'." t nil)

(autoload (quote view-buffer-other-frame) "view" "\
View BUFFER in View mode in another frame.
Return to previous buffer when done, unless optional NOT-RETURN is non-nil.
Emacs commands editing the buffer contents are not available; instead,
a special set of commands (mostly letters and punctuation)
are defined for moving around in the buffer.
Space scrolls forward, Delete scrolls backward.
For list of all View commands, type H or h while viewing.

This command runs the normal hook `view-mode-hook'.

Optional argument EXIT-ACTION is either nil or a function with buffer as
argument.  This function is called when finished viewing buffer.
Use this argument instead of explicitly setting `view-exit-action'." t nil)

(autoload (quote view-mode) "view" "\
Toggle View mode, a minor mode for viewing text but not editing it.
With arg, turn View mode on iff arg is positive.

Emacs commands that do not change the buffer contents are available as usual.
Kill commands insert text in kill buffers but do not delete.  Other commands
\(among them most letters and punctuation) beep and tell that the buffer is
read-only.
\\<view-mode-map>
The following additional commands are provided.  Most commands take prefix
arguments.  Page commands default to \"page size\" lines which is almost a whole
window full, or number of lines set by \\[View-scroll-page-forward-set-page-size] or \\[View-scroll-page-backward-set-page-size].  Half page commands default to
and set \"half page size\" lines which initially is half a window full.  Search
commands default to a repeat count of one.

H, h, ?	 This message.
Digits	provide prefix arguments.
\\[negative-argument]	negative prefix argument.
\\[beginning-of-buffer]	move to the beginning of buffer.
>	move to the end of buffer.
\\[View-scroll-to-buffer-end]	scroll so that buffer end is at last line of window.
SPC	scroll forward prefix (default \"page size\") lines.
DEL	scroll backward prefix (default \"page size\") lines.
\\[View-scroll-page-forward-set-page-size]	like  \\[View-scroll-page-forward]  except prefix sets \"page size\".
\\[View-scroll-page-backward-set-page-size]	like  \\[View-scroll-page-backward]  except prefix sets \"page size\".
\\[View-scroll-half-page-forward]	scroll forward (and if prefix set) \"half page size\" lines.
\\[View-scroll-half-page-backward]	scroll backward (and if prefix set) \"half page size\" lines.
RET, LFD  scroll forward prefix (default one) line(s).
y	scroll backward prefix (default one) line(s).
\\[View-revert-buffer-scroll-page-forward]	revert-buffer if necessary and scroll forward.
	  Use this to view a changing file.
\\[what-line]	prints the current line number.
\\[View-goto-percent]	goes prefix argument (default 100) percent into buffer.
\\[View-goto-line]	goes to line given by prefix argument (default first line).
.	set the mark.
x	exchanges point and mark.
\\[View-back-to-mark]	return to mark and pops mark ring.
	  Mark ring is pushed at start of every successful search and when
	  jump to line occurs.  The mark is set on jump to buffer start or end.
\\[point-to-register]	save current position in character register.
'	go to position saved in character register.
s	do forward incremental search.
r	do reverse incremental search.
\\[View-search-regexp-forward]	searches forward for regular expression, starting after current page.
	  ! and @ have a special meaning at the beginning of the regexp.
	  ! means search for a line with no match for regexp.  @ means start
	  search at beginning (end for backward search) of buffer.
\\	searches backward for regular expression, starting before current page.
\\[View-search-last-regexp-forward]	searches forward for last regular expression.
p	searches backward for last regular expression.
\\[View-quit]	quit View mode, trying to restore window and buffer to previous state.
	  \\[View-quit] is the normal way to leave view mode.
\\[View-exit]	exit View mode but stay in current buffer.  Use this if you started
	  viewing a buffer (file) and find out you want to edit it.
\\[View-exit-and-edit]	exit View mode and make the current buffer editable.
\\[View-quit-all]	quit View mode, trying to restore windows and buffer to previous state.
\\[View-leave]	quit View mode and maybe switch buffers, but don't kill this buffer.
\\[View-kill-and-leave]	quit View mode, kill current buffer and go back to other buffer.

The effect of \\[View-leave] , \\[View-quit] and \\[View-kill-and-leave] depends on how view-mode was entered.  If it was
entered by view-file, view-file-other-window or view-file-other-frame (\\[view-file],
\\[view-file-other-window], \\[view-file-other-frame] or the dired mode v command), then \\[View-quit] will try to kill the
current buffer.  If view-mode was entered from another buffer as is done by
View-buffer, View-buffer-other-window, View-buffer-other frame, View-file,
View-file-other-window or View-file-other-frame then \\[view-leave] , \\[view-quit] and \\[view-kill-and-leave] will return
to that buffer.

Entry to view-mode runs the normal hook `view-mode-hook'." t nil)

(autoload (quote view-mode-enter) "view" "\
Enter View mode and set up exit from view mode depending on optional arguments.
If RETURN-TO is non-nil it is added as an element to the buffer local alist
`view-return-to-alist'.
Save EXIT-ACTION in buffer local variable `view-exit-action'.
It should be either nil or a function that takes a buffer as argument.
This function will be called by `view-mode-exit'.

RETURN-TO is either nil, meaning do nothing when exiting view mode, or
it has the format (WINDOW OLD-WINDOW . OLD-BUF-INFO).
WINDOW is a window used for viewing.
OLD-WINDOW is nil or the window to select after viewing.
OLD-BUF-INFO tells what to do with WINDOW when exiting.  It is one of:
1) nil       Do nothing.
2) t         Delete WINDOW or, if it is the only window, its frame.
3) (OLD-BUFF START POINT)  Display buffer OLD-BUFF with displayed text
                           starting at START and point at POINT in WINDOW.
4) quit-window   Do quit-window in WINDOW.

For list of all View commands, type H or h while viewing.

This function runs the normal hook `view-mode-hook'." nil nil)

(autoload (quote View-exit-and-edit) "view" "\
Exit View mode and make the current buffer editable." t nil)

;;;***

;;;### (autoloads (vip-mode) "vip" "emulation/vip.el" (13650 13220))
;;; Generated autoloads from emulation/vip.el

(autoload (quote vip-mode) "vip" "\
Turn on VIP emulation of VI." t nil)

;;;***

;;;### (autoloads (viper-mode toggle-viper-mode) "viper" "emulation/viper.el"
;;;;;;  (13691 17826))
;;; Generated autoloads from emulation/viper.el

(autoload (quote toggle-viper-mode) "viper" "\
Toggle Viper on/off.
If Viper is enabled, turn it off. Otherwise, turn it on." t nil)

(autoload (quote viper-mode) "viper" "\
Turn on Viper emulation of Vi." t nil)

;;;***

;;;### (autoloads (webjump) "webjump" "webjump.el" (13883 38120))
;;; Generated autoloads from webjump.el

(autoload (quote webjump) "webjump" "\
Jumps to a Web site from a programmable hotlist.

See the documentation for the `webjump-sites' variable for how to customize the
hotlist.

Please submit bug reports and other feedback to the author, Neil W. Van Dyke
<nwv@acm.org>." t nil)

;;;***

;;;### (autoloads (which-func-mode which-func-mode-global) "which-func"
;;;;;;  "which-func.el" (14118 2519))
;;; Generated autoloads from which-func.el

(defvar which-func-mode-global nil "\
*Toggle `which-func-mode' globally.
Setting this variable directly does not take effect;
use either \\[customize] or the function `which-func-mode'.")

(custom-add-to-group (quote which-func) (quote which-func-mode-global) (quote custom-variable))

(custom-add-load (quote which-func-mode-global) (quote which-func))

(defalias (quote which-function-mode) (quote which-func-mode))

(autoload (quote which-func-mode) "which-func" "\
Toggle Which Function mode, globally.
When Which Function mode is enabled, the current function name is
continuously displayed in the mode line, in certain major modes.

With prefix arg, turn Which Function mode on iff arg is positive,
and off otherwise." t nil)

;;;***

;;;### (autoloads (widget-minor-mode widget-browse-other-window widget-browse
;;;;;;  widget-browse-at) "wid-browse" "wid-browse.el" (13218 28295))
;;; Generated autoloads from wid-browse.el

(autoload (quote widget-browse-at) "wid-browse" "\
Browse the widget under point." t nil)

(autoload (quote widget-browse) "wid-browse" "\
Create a widget browser for WIDGET." t nil)

(autoload (quote widget-browse-other-window) "wid-browse" "\
Show widget browser for WIDGET in other window." t nil)

(autoload (quote widget-minor-mode) "wid-browse" "\
Togle minor mode for traversing widgets.
With arg, turn widget mode on if and only if arg is positive." t nil)

;;;***

;;;### (autoloads (widget-delete widget-create widget-prompt-value)
;;;;;;  "wid-edit" "wid-edit.el" (14081 4940))
;;; Generated autoloads from wid-edit.el

(autoload (quote widget-prompt-value) "wid-edit" "\
Prompt for a value matching WIDGET, using PROMPT.
The current value is assumed to be VALUE, unless UNBOUND is non-nil." nil nil)

(autoload (quote widget-create) "wid-edit" "\
Create widget of TYPE.  
The optional ARGS are additional keyword arguments." nil nil)

(autoload (quote widget-delete) "wid-edit" "\
Delete WIDGET." nil nil)

;;;***

;;;### (autoloads (winner-mode winner-mode) "winner" "winner.el"
;;;;;;  (14118 1446))
;;; Generated autoloads from winner.el

(defvar winner-mode nil "\
Toggle winner-mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `winner-mode'.")

(custom-add-to-group (quote winner) (quote winner-mode) (quote custom-variable))

(custom-add-load (quote winner-mode) (quote winner))

(autoload (quote winner-mode) "winner" "\
Toggle Winner mode.
With arg, turn Winner mode on if and only if arg is positive." t nil)

;;;***

;;;### (autoloads (wordstar-mode) "ws-mode" "emulation/ws-mode.el"
;;;;;;  (13415 51533))
;;; Generated autoloads from emulation/ws-mode.el

(autoload (quote wordstar-mode) "ws-mode" "\
Major mode with WordStar-like key bindings.

BUGS:
 - Help menus with WordStar commands (C-j just calls help-for-help)
   are not implemented
 - Options for search and replace
 - Show markers (C-k h) is somewhat strange
 - Search and replace (C-q a) is only available in forward direction

No key bindings beginning with ESC are installed, they will work
Emacs-like.

The key bindings are:

  C-a		backward-word
  C-b		fill-paragraph
  C-c		scroll-up-line
  C-d		forward-char
  C-e		previous-line
  C-f		forward-word
  C-g		delete-char
  C-h		backward-char
  C-i		indent-for-tab-command
  C-j		help-for-help
  C-k		ordstar-C-k-map
  C-l		ws-repeat-search
  C-n		open-line
  C-p		quoted-insert
  C-r		scroll-down-line
  C-s		backward-char
  C-t		kill-word
  C-u		keyboard-quit
  C-v		overwrite-mode
  C-w		scroll-down
  C-x		next-line
  C-y		kill-complete-line
  C-z		scroll-up

  C-k 0		ws-set-marker-0
  C-k 1		ws-set-marker-1
  C-k 2		ws-set-marker-2
  C-k 3		ws-set-marker-3
  C-k 4		ws-set-marker-4
  C-k 5		ws-set-marker-5
  C-k 6		ws-set-marker-6
  C-k 7		ws-set-marker-7
  C-k 8		ws-set-marker-8
  C-k 9		ws-set-marker-9
  C-k b		ws-begin-block
  C-k c		ws-copy-block
  C-k d		save-buffers-kill-emacs
  C-k f		find-file
  C-k h		ws-show-markers
  C-k i		ws-indent-block
  C-k k		ws-end-block
  C-k p		ws-print-block
  C-k q		kill-emacs
  C-k r		insert-file
  C-k s		save-some-buffers
  C-k t		ws-mark-word
  C-k u		ws-exdent-block
  C-k C-u	keyboard-quit
  C-k v		ws-move-block
  C-k w		ws-write-block
  C-k x		kill-emacs
  C-k y		ws-delete-block

  C-o c		wordstar-center-line
  C-o b		switch-to-buffer
  C-o j		justify-current-line
  C-o k		kill-buffer
  C-o l		list-buffers
  C-o m		auto-fill-mode
  C-o r		set-fill-column
  C-o C-u	keyboard-quit
  C-o wd	delete-other-windows
  C-o wh	split-window-horizontally
  C-o wo	other-window
  C-o wv	split-window-vertically

  C-q 0		ws-find-marker-0
  C-q 1		ws-find-marker-1
  C-q 2		ws-find-marker-2
  C-q 3		ws-find-marker-3
  C-q 4		ws-find-marker-4
  C-q 5		ws-find-marker-5
  C-q 6		ws-find-marker-6
  C-q 7		ws-find-marker-7
  C-q 8		ws-find-marker-8
  C-q 9		ws-find-marker-9
  C-q a		ws-query-replace
  C-q b		ws-to-block-begin
  C-q c		end-of-buffer
  C-q d		end-of-line
  C-q f		ws-search
  C-q k		ws-to-block-end
  C-q l		ws-undo
  C-q p		ws-last-cursorp
  C-q r		beginning-of-buffer
  C-q C-u	keyboard-quit
  C-q w		ws-last-error
  C-q y		ws-kill-eol
  C-q DEL	ws-kill-bol
" t nil)

;;;***

;;;### (autoloads (xterm-mouse-mode) "xt-mouse" "xt-mouse.el" (13929
;;;;;;  31262))
;;; Generated autoloads from xt-mouse.el

(autoload (quote xterm-mouse-mode) "xt-mouse" "\
Toggle XTerm mouse mode.
With prefix arg, turn XTerm mouse mode on iff arg is positive.

Turn it on to use emacs mouse commands, and off to use xterm mouse commands." t nil)

;;;***

;;;### (autoloads (psychoanalyze-pinhead apropos-zippy insert-zippyism
;;;;;;  yow) "yow" "play/yow.el" (13607 43523))
;;; Generated autoloads from play/yow.el

(autoload (quote yow) "yow" "\
Return or display a random Zippy quotation.  With prefix arg, insert it." t nil)

(autoload (quote insert-zippyism) "yow" "\
Prompt with completion for a known Zippy quotation, and insert it at point." t nil)

(autoload (quote apropos-zippy) "yow" "\
Return a list of all Zippy quotes matching REGEXP.
If called interactively, display a list of matches." t nil)

(autoload (quote psychoanalyze-pinhead) "yow" "\
Zippy goes to the analyst." t nil)

;;;***

;;;### (autoloads (zone-mode zone-mode-update-serial-hook) "zone-mode"
;;;;;;  "zone-mode.el" (13674 19972))
;;; Generated autoloads from zone-mode.el

(autoload (quote zone-mode-update-serial-hook) "zone-mode" "\
Update the serial number in a zone if the file was modified" t nil)

(autoload (quote zone-mode) "zone-mode" "A mode for editing DNS zone files.\n\nZone-mode does two things:\n\n	- automatically update the serial number for a zone\n		when saving the file\n\n	- fontification" t nil)

;;;***

;;; Don't make backup versions of this file - most of it is generated
;;; automatically by autoload.el, and what isn't changes rarely.
;;; Local Variables:
;;; version-control: never
;;; no-byte-compile: t
;;; no-update-autoloads: t
;;; End:
;;; loaddefs.el ends here
