;;; makefile.el --- makefile editing commands for Emacs

;; Copyright (C) 1992, 1994 Free Software Foundation, Inc.

;; Author: Thomas Neumann <tom@smart.bo.open.de>
;;	Eric S. Raymond <esr@snark.thyrsus.com>
;; Adapted-By: ESR
;; Keywords: unix, tools

;; RMS:
;; This needs work.
;; Also, the doc strings need fixing: the first line doesn't stand alone,
;; and other usage is not high quality.  Symbol names don't have `...'.

;; So, for the meantime, this is not the default mode for makefiles.

;; $Id: makefile.el,v 1.20 1995/05/01 17:16:40 rms Exp kwzh $

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; A major mode for editing makefiles.  The mode knows about Makefile
;; syntax and defines M-n and M-p to move to next and previous productions.
;;
;; The keys $, =, : and . are electric; they try to help you fill in a
;; macro reference, macro definition, ordinary target name, or special
;; target name, respectively.  Such names are completed using a list of
;; targets and macro names parsed out of the makefile.  This list is
;; automatically updated, if necessary, whenever you invoke one of
;; these commands.  You can force it to be updated with C-c C-p.
;;
;; The command C-c C-f adds certain filenames in the current directory
;; as targets.  You can filter out filenames by setting the variable
;; makefile-ignored-files-in-pickup-regex.
;;
;; The command C-c C-u grinds for a bit, then pops up a report buffer
;; showing which target names are up-to-date with respect to their
;; prerequisites, which targets are out-of-date, and which have no
;; prerequisites.
;;
;; The command C-c C-b pops up a browser window listing all target and
;; macro names.  You can mark or unmark items wit C-c SPC, and insert
;; all marked items back in the Makefile with C-c TAB.
;;
;; The command C-c TAB in the makefile buffer inserts a GNU make builtin.
;; You will be prompted for the builtin's args.
;;
;; There are numerous other customization variables.

;;
;; To Do:
;;
;; * makefile-backslash-region should be given better behavior.
;; * Consider binding C-c C-c to comment-region (like cc-mode).
;; * Eliminate electric stuff entirely.
;; * It might be nice to highlight targets differently depending on
;;   whether they are up-to-date or not.  Not sure how this would
;;   interact with font-lock.
;; * Would be nice to edit the commands in ksh-mode and have
;;   indentation and slashification done automatically.  Hard.
;; * Consider removing browser mode.  It seems useless.
;; * ":" should notice when a new target is made and add it to the
;;   list (or at least set makefile-need-target-pickup).
;; * Make browser into a major mode.
;; * Clean up macro insertion stuff.  It is a mess.
;; * Browser entry and exit is weird.  Normalize.
;; * Browser needs to be rewritten.  Right now it is kind of a crock.
;;   Should at least:
;;    * Act more like dired/buffer menu/whatever.
;;    * Highlight as mouse traverses.
;;    * B2 inserts.
;; * Update documentation above.
;; * Update texinfo manual.
;; * Update files.el.



;;; Code:

(provide 'makefile)

;; Sadly we need this for a macro.
(eval-when-compile
  (require 'imenu))

;;; ------------------------------------------------------------
;;; Configurable stuff
;;; ------------------------------------------------------------

(defvar makefile-browser-buffer-name "*Macros and Targets*"
  "Name of the macro- and target browser buffer.")

(defvar makefile-target-colon ":"
  "String to append to all target names inserted by `makefile-insert-target'.
\":\" or \"::\" are common values.")

(defvar makefile-macro-assign " = "
  "String to append to all macro names inserted by `makefile-insert-macro'.
The normal value should be \" = \", since this is what
standard make expects. However, newer makes such as dmake
allow a larger variety of different macro assignments, so you
might prefer to use \" += \" or \" := \" .")

(defvar makefile-electric-keys nil
  "If non-nil, install electric keybindings.
Default is nil.")

(defvar makefile-use-curly-braces-for-macros-p nil
  "Controls the style of generated macro references.
t (actually non-nil) means macro references should use curly braces,
like `${this}'.
nil means use parentheses, like `$(this)'.")

(defvar makefile-tab-after-target-colon t
  "If non-nil, insert a TAB after a target colon.
Otherwise, a space is inserted.
The default is t.")

(defvar makefile-browser-leftmost-column 10
  "Number of blanks to the left of the browser selection mark.")

(defvar makefile-browser-cursor-column 10
  "Column in which the cursor is positioned when it moves
up or down in the browser.")

(defvar makefile-backslash-column 48
  "*Column in which `makefile-backslash-region' inserts backslashes.")

(defvar makefile-browser-selected-mark "+  "
  "String used to mark selected entries in the browser.")

(defvar makefile-browser-unselected-mark "   "
  "String used to mark unselected entries in the browser.")

(defvar makefile-browser-auto-advance-after-selection-p t
  "If non-nil, cursor will move after item is selected in browser.")

(defvar makefile-pickup-everything-picks-up-filenames-p nil
  "If non-nil, `makefile-pickup-everything' picks up filenames as targets.
\(i.e. it calls `makefile-find-filenames-as-targets').
Otherwise filenames are omitted.")

(defvar makefile-cleanup-continuations-p t
  "If non-nil, automatically clean up continuation lines when saving.
A line is cleaned up by removing all whitespace following a trailing
backslash.  This is done silently.
IMPORTANT: Please note that enabling this option causes makefile-mode
to MODIFY A FILE WITHOUT YOUR CONFIRMATION when \'it seems necessary\'.")

(defvar makefile-browser-hook '())

;;
;; Special targets for DMake, Sun's make ...
;; 
(defvar makefile-special-targets-list
  '(("DEFAULT")      ("DONE")        ("ERROR")        ("EXPORT")
    ("FAILED")       ("GROUPEPILOG") ("GROUPPROLOG")  ("IGNORE")
    ("IMPORT")       ("INCLUDE")     ("INCLUDEDIRS")  ("INIT")
    ("KEEP_STATE")   ("MAKEFILES")   ("MAKE_VERSION") ("NO_PARALLEL")
    ("PARALLEL")     ("PHONY")       ("PRECIOUS")     ("REMOVE")
    ("SCCS_GET")     ("SILENT")      ("SOURCE")       ("SUFFIXES")
    ("WAIT")         ("c.o")         ("C.o")          ("m.o")
    ("el.elc")       ("y.c")         ("s.o"))
  "List of special targets.
You will be offered to complete on one of those in the minibuffer whenever
you enter a \".\" at the beginning of a line in makefile-mode.")

(defvar makefile-runtime-macros-list
  '(("@") ("&") (">") ("<") ("*") ("^") ("?") ("%") ("$"))
  "List of macros that are resolved by make at runtime.
If you insert a macro reference using makefile-insert-macro-ref, the name
of the macro is checked against this list. If it can be found its name will
not be enclosed in { } or ( ).")

;; Note that the first big subexpression is used by font lock.  Note
;; that if you change this regexp you must fix the imenu index
;; function defined at the end of the file.
(defconst makefile-dependency-regex
  "^\\([^ \n\t#:]+\\([ \t]+[^ \t\n#:]+\\)*\\)[ \t]*:\\([ \t]*$\\|\\([^=\n].*$\\)\\)" 
  "Regex used to find dependency lines in a makefile.")

;; Note that the first subexpression is used by font lock.  Note that
;; if you change this regexp you must fix the imenu index function
;; defined at the end of the file.
(defconst makefile-macroassign-regex
  "^\\([^ \n\t][^:#= \t\n]*\\)[ \t]*[*:+]?:?="
  "Regex used to find macro assignment lines in a makefile.")

(defconst makefile-ignored-files-in-pickup-regex
  "\\(^\\..*\\)\\|\\(.*~$\\)\\|\\(.*,v$\\)\\|\\(\\.[chy]\\)"
  "Regex for filenames that will NOT be included in the target list.")

;; TABs are important in Makefiles.  So highlight them in font-lock.
;; Thanks to Job Ganzevoort <Job.Ganzevoort@cwi.nl> for this.
(defvar makefile-tab-face
  (if (fboundp 'make-face)
      (progn
	(make-face 'makefile-tab-face)
	(or (face-differs-from-default-p 'makefile-tab-face)
	    (set-face-background 'makefile-tab-face "hotpink"))
	'makefile-tab-face)
    '())
  "Face to use for highlighting leading tabs in font-lock-mode.")

(defconst makefile-font-lock-keywords
  (list
   ;; Do macro assignments.  These get the "variable-name" face rather
   ;; arbitrarily.
   (list makefile-macroassign-regex 1 'font-lock-variable-name-face)
   ;;
   ;; Variable references even in targets/strings/comments:
   '("\\$[({]\\([a-zA-Z0-9_]+\\)[})]" 1 font-lock-reference-face t)
   ;;
   ;; Do dependencies.  These get the function name face.
   (list makefile-dependency-regex 1 'font-lock-function-name-face)

   ;; Highlight leading tab.  Maybe highlighting all leading TABs
   ;; would be nice?  I don't know.
   '("^\t" . makefile-tab-face)))

;;; ------------------------------------------------------------
;;; The following configurable variables are used in the
;;; up-to-date overview .
;;; The standard configuration assumes that your `make' program
;;; can be run in question/query mode using the `-q' option, this
;;; means that the command
;;;
;;;    make -q foo
;;;
;;; should return an exit status of zero if the target `foo' is
;;; up to date and a nonzero exit status otherwise.
;;; Many makes can do this although the docs/manpages do not mention
;;; it. Try it with your favourite one.  GNU make, System V make, and
;;; Dennis Vadura's DMake have no problems.
;;; Set the variable `makefile-brave-make' to the name of the
;;; make utility that does this on your system.
;;; To understand what this is all about see the function definition
;;; of `makefile-query-by-make-minus-q' .
;;; ------------------------------------------------------------

(defvar makefile-brave-make "make"
  "A make that can handle the \'-q\' option.")

(defvar makefile-query-one-target-method 'makefile-query-by-make-minus-q
  "A function symbol [one that can be used as the first argument to
funcall] that provides a function that must conform to the following
interface:

* As its first argument, it must accept the name of the target to
  be checked, as a string.

* As its second argument, it may accept the name of a makefile
  as a string. Depending on what you're going to do you may
  not need this.

* It must return the integer value 0 (zero) if the given target
  should be considered up-to-date in the context of the given
  makefile, any nonzero integer value otherwise.")

(defvar makefile-up-to-date-buffer-name "*Makefile Up-to-date overview*"
  "Name of the Up-to-date overview buffer.")

;;; --- end of up-to-date-overview configuration ------------------

(defvar makefile-mode-map nil
  "The keymap that is used in makefile-mode.")
(if makefile-mode-map
    ()
  (setq makefile-mode-map (make-sparse-keymap))
  ;; set up the keymap
  (define-key makefile-mode-map "\C-c:" 'makefile-insert-target-ref)
  (if makefile-electric-keys
      (progn
	(define-key makefile-mode-map "$" 'makefile-insert-macro-ref)
	(define-key makefile-mode-map ":" 'makefile-electric-colon)
	(define-key makefile-mode-map "=" 'makefile-electric-equal)
	(define-key makefile-mode-map "." 'makefile-electric-dot)))
  (define-key makefile-mode-map "\C-c\C-f" 'makefile-pickup-filenames-as-targets)
  (define-key makefile-mode-map "\C-c\C-b" 'makefile-switch-to-browser)
  (define-key makefile-mode-map "\C-c\C-p" 'makefile-pickup-everything)
  (define-key makefile-mode-map "\C-c\C-u" 'makefile-create-up-to-date-overview)
  (define-key makefile-mode-map "\C-c\C-i" 'makefile-insert-gmake-function)
  (define-key makefile-mode-map "\C-c\C-\\" 'makefile-backslash-region)
  (define-key makefile-mode-map "\M-p"     'makefile-previous-dependency)
  (define-key makefile-mode-map "\M-n"     'makefile-next-dependency)
  (define-key makefile-mode-map "\e\t"     'makefile-complete)

  ;; Make menus.
  (define-key makefile-mode-map [menu-bar makefile-mode]
    (cons "Makefile" (make-sparse-keymap "Makefile")))

  (define-key makefile-mode-map [menu-bar makefile-mode browse]
    '("Pop up Makefile Browser" . makefile-switch-to-browser))
  (define-key makefile-mode-map [menu-bar makefile-mode complete]
    '("Complete Target or Macro" . makefile-complete))
  (define-key makefile-mode-map [menu-bar makefile-mode pickup]
    '("Find Targets and Macros" . makefile-pickup-everything))

  (define-key makefile-mode-map [menu-bar makefile-mode prev]
    '("Move to Previous Dependency" . makefile-previous-dependency))
  (define-key makefile-mode-map [menu-bar makefile-mode next]
    '("Move to Next Dependency" . makefile-next-dependency)))

(defvar makefile-browser-map nil
  "The keymap that is used in the macro- and target browser.")
(if makefile-browser-map
    ()
  (setq makefile-browser-map (make-sparse-keymap))
  (define-key makefile-browser-map "n"    'makefile-browser-next-line)
  (define-key makefile-browser-map "\C-n" 'makefile-browser-next-line)    
  (define-key makefile-browser-map "p"    'makefile-browser-previous-line)
  (define-key makefile-browser-map "\C-p" 'makefile-browser-previous-line)
  (define-key makefile-browser-map " "    'makefile-browser-toggle)
  (define-key makefile-browser-map "i"    'makefile-browser-insert-selection)
  (define-key makefile-browser-map "I"    'makefile-browser-insert-selection-and-quit)  
  (define-key makefile-browser-map "\C-c\C-m" 'makefile-browser-insert-continuation)
  (define-key makefile-browser-map "q"    'makefile-browser-quit)
  ;; disable horizontal movement
  (define-key makefile-browser-map "\C-b" 'undefined)
  (define-key makefile-browser-map "\C-f" 'undefined))  


(defvar makefile-mode-syntax-table nil)
(if makefile-mode-syntax-table
    ()
  (setq makefile-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\( "()    " makefile-mode-syntax-table)
  (modify-syntax-entry ?\) ")(    " makefile-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]    " makefile-mode-syntax-table)
  (modify-syntax-entry ?\] "([    " makefile-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}    " makefile-mode-syntax-table)  
  (modify-syntax-entry ?\} "){    " makefile-mode-syntax-table)
  (modify-syntax-entry ?\' "\"     " makefile-mode-syntax-table)
  (modify-syntax-entry ?\` "\"     " makefile-mode-syntax-table)
  (modify-syntax-entry ?#  "<     " makefile-mode-syntax-table)
  (modify-syntax-entry ?\n ">     " makefile-mode-syntax-table))


;;; ------------------------------------------------------------
;;; Internal variables.
;;; You don't need to configure below this line.
;;; ------------------------------------------------------------

(defvar makefile-target-table nil
  "Table of all target names known for this buffer.")

(defvar makefile-macro-table nil
  "Table of all macro names known for this buffer.")

(defvar makefile-browser-client
  "A buffer in makefile-mode that is currently using the browser.")

(defvar makefile-browser-selection-vector nil)
(defvar makefile-has-prereqs nil)
(defvar makefile-need-target-pickup t)
(defvar makefile-need-macro-pickup t)

(defvar makefile-mode-hook '())

;; Each element looks like '("GNU MAKE FUNCTION" "ARG" "ARG" ... )
;; Each "ARG" is used as a prompt for a required argument.
(defconst makefile-gnumake-functions-alist
  '(
    ;; Text functions
    ("subst" "From" "To" "In")
    ("patsubst" "Pattern" "Replacement" "In")
    ("strip" "Text")
    ("findstring" "Find what" "In")
    ("filter" "Pattern" "Text")
    ("filter-out" "Pattern" "Text")
    ("sort" "List")
    ;; Filename functions
    ("dir" "Names")
    ("notdir" "Names")
    ("suffix" "Names")
    ("basename" "Names")
    ("addsuffix" "Suffix" "Names")
    ("join" "List 1" "List 2")
    ("word" "Index" "Text")
    ("words" "Text")
    ("firstword" "Text")
    ("wildcard" "Pattern")
    ;; Misc functions
    ("foreach" "Variable" "List" "Text")
    ("origin" "Variable")
    ("shell" "Command")))


;;; ------------------------------------------------------------
;;; The mode function itself.
;;; ------------------------------------------------------------

;;;###autoload
(defun makefile-mode ()
  "Major mode for editing Makefiles.
Calling this function invokes the function(s) \"makefile-mode-hook\" before
doing anything else.

\\{makefile-mode-map}

In the browser, use the following keys:

\\{makefile-browser-map}

makefile-mode can be configured by modifying the following
variables:

makefile-browser-buffer-name:
    Name of the macro- and target browser buffer.

makefile-target-colon:
    The string that gets appended to all target names
    inserted by makefile-insert-target.
    \":\" or \"::\" are quite common values.

makefile-macro-assign:
   The string that gets appended to all macro names
   inserted by makefile-insert-macro.
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
   makefile-pickup-everything also picks up filenames as targets
   (i.e. it calls makefile-find-filenames-as-targets), otherwise
   filenames are omitted.

makefile-cleanup-continuations-p:
   If this variable is set to a non-nil value then makefile-mode
   will assure that no line in the file ends with a backslash
   (the continuation character) followed by any whitespace.
   This is done by silently removing the trailing whitespace, leaving
   the backslash itself intact.
   IMPORTANT: Please note that enabling this option causes makefile-mode
   to MODIFY A FILE WITHOUT YOUR CONFIRMATION when \'it seems necessary\'.

makefile-browser-hook:
   A function or list of functions to be called just before the
   browser is entered. This is executed in the makefile buffer.

makefile-special-targets-list:
   List of special targets. You will be offered to complete
   on one of those in the minibuffer whenever you enter a \".\"
   at the beginning of a line in makefile-mode."

  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'local-write-file-hooks)
  (setq local-write-file-hooks
	'(makefile-cleanup-continuations makefile-warn-suspicious-lines))
  (make-local-variable 'makefile-target-table)
  (make-local-variable 'makefile-macro-table)
  (make-local-variable 'makefile-has-prereqs)
  (make-local-variable 'makefile-need-target-pickup)
  (make-local-variable 'makefile-need-macro-pickup)

  ;; Font lock.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(makefile-font-lock-keywords))

  ;; Add-log.
  (make-local-variable 'add-log-current-defun-function)
  (setq add-log-current-defun-function 'makefile-add-log-defun)

  ;; Imenu.
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'makefile-menu-index-function)

  ;; Dabbrev.
  (make-local-variable 'dabbrev-abbrev-skip-leading-regexp)
  (setq dabbrev-abbrev-skip-leading-regexp "\\$")

  ;; Comment stuff.
  (make-local-variable 'comment-start)
  (setq comment-start "#")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#+[ \t]*")

  ;; become the current major mode
  (setq major-mode 'makefile-mode)
  (setq mode-name "Makefile")

  ;; Activate keymap and syntax table.
  (use-local-map makefile-mode-map)
  (set-syntax-table makefile-mode-syntax-table)

  ;; Real TABs are important in makefiles
  (setq indent-tabs-mode t)
  (run-hooks 'makefile-mode-hook))



;;; Motion code.

(defun makefile-next-dependency ()
  "Move (point) to the beginning of the next dependency line below (point)."
  (interactive)
  (let ((here (point)))
    (end-of-line)
    (if (re-search-forward makefile-dependency-regex (point-max) t)
	(progn (beginning-of-line) t)	; indicate success
      (goto-char here) nil)))

(defun makefile-previous-dependency ()
  "Move (point) to the beginning of the next dependency line above (point)."
  (interactive)
  (let ((here (point)))
    (beginning-of-line)
    (if (re-search-backward makefile-dependency-regex (point-min) t)
	(progn (beginning-of-line) t)	; indicate success
      (goto-char here) nil)))



;;; Electric keys.  Blech.

(defun makefile-electric-dot (arg)
  "Prompt for the name of a special target to insert.
Only does electric insertion at beginning of line.
Anywhere else just self-inserts."
  (interactive "p")
  (if (bolp)
      (makefile-insert-special-target)
    (self-insert-command arg)))

(defun makefile-insert-special-target ()
  "Propmt for and insert a special target name.
Uses `makefile-special-targets' list."
  (interactive)
  (makefile-pickup-targets)
  (let ((special-target
	 (completing-read "Special target: "
			  makefile-special-targets-list nil nil nil)))
    (if (zerop (length special-target))
	()
      (insert "." special-target ":")
      (makefile-forward-after-target-colon))))

(defun makefile-electric-equal (arg)
  "Prompt for name of a macro to insert.
Only does prompting if point is at beginning of line.
Anywhere else just self-inserts."
  (interactive "p")
  (makefile-pickup-macros)
  (if (bolp)
      (call-interactively 'makefile-insert-macro)
    (self-insert-command arg)))

(defun makefile-insert-macro (macro-name)
  "Prepare definition of a new macro."
  (interactive "sMacro Name: ")
  (makefile-pickup-macros)
  (if (not (zerop (length macro-name)))
      (progn
	(beginning-of-line)
	(insert macro-name makefile-macro-assign)
	(setq makefile-need-macro-pickup t)
	(makefile-remember-macro macro-name))))

(defun makefile-insert-macro-ref (macro-name)
  "Complete on a list of known macros, then insert complete ref at (point)."
  (interactive
   (list
    (progn
      (makefile-pickup-macros)
      (completing-read "Refer to macro: " makefile-macro-table nil nil nil))))
  (makefile-do-macro-insertion macro-name))

(defun makefile-insert-target (target-name)
  "Prepare definition of a new target (dependency line)."
  (interactive "sTarget: ")
  (if (not (zerop (length target-name)))
      (progn
	(beginning-of-line)
	(insert target-name makefile-target-colon)
	(makefile-forward-after-target-colon)
	(end-of-line)
	(setq makefile-need-target-pickup t)
	(makefile-remember-target target-name))))

(defun makefile-insert-target-ref (target-name)
  "Complete on a list of known targets, then insert target-ref at (point) ."
  (interactive
   (list
    (progn
     (makefile-pickup-targets)
     (completing-read "Refer to target: " makefile-target-table nil nil nil))))
   (if (not (zerop (length target-name)))
       (insert target-name " ")))

(defun makefile-electric-colon (arg)
  "Prompt for name of new target.
Prompting only happens at beginning of line.
Anywhere else just self-inserts."
  (interactive "p")
  (if (bolp)
      (call-interactively 'makefile-insert-target)
    (self-insert-command arg)))



;;; ------------------------------------------------------------
;;; Extracting targets and macros from an existing makefile
;;; ------------------------------------------------------------

(defun makefile-pickup-targets ()
  "Notice names of all target definitions in Makefile."
  (interactive)
  (if (not makefile-need-target-pickup)
      nil
    (setq makefile-need-target-pickup nil)
    (setq makefile-target-table nil)
    (setq makefile-has-prereqs nil)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward makefile-dependency-regex (point-max) t)
	(makefile-add-this-line-targets)))
    (message "Read targets OK.")))

(defun makefile-add-this-line-targets ()
  (save-excursion
    (beginning-of-line)
    (let ((done-with-line nil)
	  (line-number (1+ (count-lines (point-min) (point)))))
      (while (not done-with-line)
	(skip-chars-forward " \t")
	(if (not (setq done-with-line (or (eolp)
					  (char-equal (char-after (point)) ?:))))
	    (progn
	      (let* ((start-of-target-name (point))
		     (target-name
		      (progn
			(skip-chars-forward "^ \t:#")
			(buffer-substring start-of-target-name (point))))
		     (has-prereqs
		      (not (looking-at ":[ \t]*$"))))
		(if (makefile-remember-target target-name has-prereqs)
		    (message "Picked up target \"%s\" from line %d"
			     target-name line-number)))))))))

(defun makefile-pickup-macros ()
  "Notice names of all macro definitions in Makefile."
  (interactive)
  (if (not makefile-need-macro-pickup)
      nil
    (setq makefile-need-macro-pickup nil)
    (setq makefile-macro-table nil)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward makefile-macroassign-regex (point-max) t)
	(makefile-add-this-line-macro)
	(forward-line 1)))
    (message "Read macros OK.")))

(defun makefile-add-this-line-macro ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (not (eolp))
	(let* ((start-of-macro-name (point))
	       (line-number (1+ (count-lines (point-min) (point))))
	       (macro-name (progn
			     (skip-chars-forward "^ \t:#=*")
			     (buffer-substring start-of-macro-name (point)))))
	  (if (makefile-remember-macro macro-name)
	      (message "Picked up macro \"%s\" from line %d"
		       macro-name line-number))))))

(defun makefile-pickup-everything (arg)
  "Notice names of all macros and targets in Makefile.
Prefix arg means force pickups to be redone."
  (interactive "P")
  (if arg
      (progn
	(setq makefile-need-target-pickup t)
	(setq makefile-need-macro-pickup t)))
  (makefile-pickup-macros)
  (makefile-pickup-targets)
  (if makefile-pickup-everything-picks-up-filenames-p
      (makefile-pickup-filenames-as-targets)))

(defun makefile-pickup-filenames-as-targets ()
  "Scan the current directory for filenames to use as targets.
Checks each filename against `makefile-ignored-files-in-pickup-regex'
and adds all qualifying names to the list of known targets."
  (interactive)
  (let* ((dir (file-name-directory (buffer-file-name)))
	 (raw-filename-list (if dir
				(file-name-all-completions "" dir)
			      (file-name-all-completions "" ""))))
    (mapcar '(lambda (name)
	       (if (and (not (file-directory-p name))
			(not (string-match makefile-ignored-files-in-pickup-regex
					   name)))
		   (if (makefile-remember-target name)
		       (message "Picked up file \"%s\" as target" name))))
	    raw-filename-list)))



;;; Completion.

(defun makefile-complete ()
  "Perform completion on Makefile construct preceding point.
Can complete variable and target names.
The context determines which are considered."
  (interactive)
  (let* ((beg (save-excursion
		(skip-chars-backward "^$(){}:#= \t\n")
		(point)))
	 (try (buffer-substring beg (point)))
	 (do-macros nil)
	 (paren nil))

    (save-excursion
      (goto-char beg)
      (let ((pc (preceding-char)))
	(cond
	 ;; Beginning of line means anything.
	 ((bolp)
	  ())

	 ;; Preceding "$" means macros only.
	 ((= pc ?$)
	  (setq do-macros t))

	 ;; Preceding "$(" or "${" means macros only.
	 ((and (or (= pc ?{)
		   (= pc ?\())
	       (progn
		 (setq paren pc)
		 (backward-char)
		 (and (not (bolp))
		      (= (preceding-char) ?$))))
	  (setq do-macros t)))))

    ;; Try completion.
    (let* ((table (append (if do-macros
			      '()
			    makefile-target-table)
			  makefile-macro-table))
	   (completion (try-completion try table)))
      (cond
       ;; Exact match, so insert closing paren or colon.
       ((eq completion t)
	(insert (if do-macros
		    (if (eq paren ?{)
			?}
		      ?\))
		  (if (save-excursion
			(goto-char beg)
			(bolp))
		      ":"
		    " "))))

       ;; No match.
       ((null completion)
	(message "Can't find completion for \"%s\"" try)
	(ding))

       ;; Partial completion.
       ((not (string= try completion))
	;; FIXME it would be nice to supply the closing paren if an
	;; exact, unambiguous match were found.  That is not possible
	;; right now.  Ditto closing ":" for targets.
	(delete-region beg (point))

	;; DO-MACROS means doing macros only.  If not that, then check
	;; to see if this completion is a macro.  Special insertion
	;; must be done for macros.
	(if (or do-macros
		(assoc completion makefile-macro-table))
	    (let ((makefile-use-curly-braces-for-macros-p
		   (or (eq paren ?{)
		       makefile-use-curly-braces-for-macros-p)))
	      (delete-backward-char 2)
	      (makefile-do-macro-insertion completion)
	      (delete-backward-char 1))

	  ;; Just insert targets.
	  (insert completion)))

       ;; Can't complete any more, so make completion list.  FIXME
       ;; this doesn't do the right thing when the completion is
       ;; actually inserted.  I don't think there is an easy way to do
       ;; that.
       (t
	(message "Making completion list...")
	(let ((list (all-completions try table)))
	  (with-output-to-temp-buffer "*Completions*"
	    (display-completion-list list)))
	(message "Making completion list...done"))))))



;; Backslashification.  Stolen from cc-mode.el.

(defun makefile-backslashify-current-line (doit)
  (end-of-line)
  (if doit
      (if (not (save-excursion
		 (forward-char -1)
		 (eq (char-after (point)) ?\\ )))
	  (progn
	    (if (>= (current-column) makefile-backslash-column)
		(insert " \\")
	      (while (<= (current-column) makefile-backslash-column)
		(insert "\t")
		(end-of-line))
	      (delete-char -1)
	      (while (< (current-column) makefile-backslash-column)
		(insert " ")
		(end-of-line))
	      (insert "\\"))))
    (if (not (bolp))
	(progn
	  (forward-char -1)
	  (if (eq (char-after (point)) ?\\ )
	      (let ((saved (save-excursion
			    (end-of-line)
			    (point))))
		(skip-chars-backward " \t")
		(delete-region (point) saved)))))))

(defun makefile-backslash-region (beg end arg)
  "Insert backslashes at end of every line in region.
Useful for defining multi-line rules.
If called with a prefix argument, trailing backslahes are removed."
  (interactive "r\nP")
  (save-excursion
    (let ((do-lastline-p (progn (goto-char end) (not (bolp)))))
      (save-restriction
	(narrow-to-region beg end)
	(goto-char (point-min))
	(while (not (save-excursion
		      (forward-line 1)
		      (eobp)))
	  (makefile-backslashify-current-line (null arg))
	  (forward-line 1)))
      (and do-lastline-p
	   (progn (goto-char end)
		  (makefile-backslashify-current-line (null arg)))))))



;;; ------------------------------------------------------------
;;; Browser mode.
;;; ------------------------------------------------------------

(defun makefile-browser-format-target-line (target selected)
  (format
   (concat (make-string makefile-browser-leftmost-column ?\ )
	   (if selected
	       makefile-browser-selected-mark
	     makefile-browser-unselected-mark)
	   "%s%s")
   target makefile-target-colon))

(defun makefile-browser-format-macro-line (macro selected)
  (format
   (concat (make-string makefile-browser-leftmost-column ?\ )
	   (if selected
	       makefile-browser-selected-mark
	     makefile-browser-unselected-mark)
	   (makefile-format-macro-ref macro))))

(defun makefile-browser-fill (targets macros)
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (erase-buffer)
    (mapconcat
     (function
      (lambda (item) (insert (makefile-browser-format-target-line (car item) nil) "\n")))
     targets
     "")
    (mapconcat
     (function
      (lambda (item) (insert (makefile-browser-format-macro-line (car item) nil) "\n")))
     macros
     "")
    (sort-lines nil (point-min) (point-max))
    (goto-char (1- (point-max)))
    (delete-char 1)			; remove unnecessary newline at eob
    (goto-char (point-min))
    (forward-char makefile-browser-cursor-column)))

;;;
;;; Moving up and down in the browser
;;;

(defun makefile-browser-next-line ()
  "Move the browser selection cursor to the next line."
  (interactive)
  (if (not (makefile-last-line-p))
      (progn
	(forward-line 1)
	(forward-char makefile-browser-cursor-column))))

(defun makefile-browser-previous-line ()
  "Move the browser selection cursor to the previous line."
  (interactive)
  (if (not (makefile-first-line-p))
      (progn
	(forward-line -1)
	(forward-char makefile-browser-cursor-column))))

;;;
;;; Quitting the browser (returns to client buffer)
;;;

(defun makefile-browser-quit ()
  "Leave the browser and return to the makefile buffer."
  (interactive)
  (let ((my-client makefile-browser-client))
    (setq makefile-browser-client nil)	; we quitted, so NO client!
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer))
    (pop-to-buffer my-client)))

;;;
;;; Toggle state of a browser item
;;;

(defun makefile-browser-toggle ()
  "Toggle the selection state of the browser item at the cursor position."
  (interactive)
  (let ((this-line (count-lines (point-min) (point))))
    (setq this-line (max 1 this-line))
    (makefile-browser-toggle-state-for-line this-line)
    (goto-line this-line)
    (let ((inhibit-read-only t))
      (beginning-of-line)
      (if (makefile-browser-on-macro-line-p)
	  (let ((macro-name (makefile-browser-this-line-macro-name)))
	    (kill-line)
	    (insert
	     (makefile-browser-format-macro-line
		macro-name
		(makefile-browser-get-state-for-line this-line))))
	(let ((target-name (makefile-browser-this-line-target-name)))
	  (kill-line)
	  (insert
	   (makefile-browser-format-target-line
	      target-name
	      (makefile-browser-get-state-for-line this-line))))))
    (beginning-of-line)
    (forward-char makefile-browser-cursor-column)
    (if makefile-browser-auto-advance-after-selection-p
	(makefile-browser-next-line))))

;;;
;;; Making insertions into the client buffer
;;;

(defun makefile-browser-insert-continuation ()
  "Insert a makefile continuation.
In the makefile buffer, go to (end-of-line), insert a \'\\\'
character, insert a new blank line, go to that line and indent by one TAB.
This is most useful in the process of creating continued lines when copying
large dependencies from the browser to the client buffer.
\(point) advances accordingly in the client buffer."
  (interactive)
  (save-excursion
    (set-buffer makefile-browser-client)
    (end-of-line)
    (insert "\\\n\t")))

(defun makefile-browser-insert-selection ()
  "Insert all selected targets and/or macros in the makefile buffer.
Insertion takes place at (point)."
  (interactive)
  (save-excursion
    (goto-line 1)
    (let ((current-line 1))
      (while (not (eobp))
	(if (makefile-browser-get-state-for-line current-line)
	    (makefile-browser-send-this-line-item))
	(forward-line 1)
	(setq current-line (1+ current-line))))))

(defun makefile-browser-insert-selection-and-quit ()
  (interactive)
  (makefile-browser-insert-selection)
  (makefile-browser-quit))

(defun makefile-browser-send-this-line-item ()
  (if (makefile-browser-on-macro-line-p)
      (save-excursion
	(let ((macro-name (makefile-browser-this-line-macro-name)))
	  (set-buffer makefile-browser-client)
	  (insert (makefile-format-macro-ref macro-name) " ")))
    (save-excursion
      (let ((target-name (makefile-browser-this-line-target-name)))
	(set-buffer makefile-browser-client)
	(insert target-name " ")))))

(defun makefile-browser-start-interaction ()
  (use-local-map makefile-browser-map)
  (setq buffer-read-only t))

(defun makefile-browse (targets macros)
  (interactive)
  (if (zerop (+ (length targets) (length macros)))
      (progn
	(beep)
	(message "No macros or targets to browse! Consider running 'makefile-pickup-everything\'"))
    (let ((browser-buffer (get-buffer-create makefile-browser-buffer-name)))
	(pop-to-buffer browser-buffer)
	(make-variable-buffer-local 'makefile-browser-selection-vector)
	(makefile-browser-fill targets macros)
	(shrink-window-if-larger-than-buffer)
	(setq makefile-browser-selection-vector
	      (make-vector (+ (length targets) (length macros)) nil))
	(makefile-browser-start-interaction))))

(defun makefile-switch-to-browser ()
  (interactive)
  (run-hooks 'makefile-browser-hook)
  (setq makefile-browser-client (current-buffer))
  (makefile-pickup-targets)
  (makefile-pickup-macros)
  (makefile-browse makefile-target-table makefile-macro-table))



;;; ------------------------------------------------------------
;;; Up-to-date overview buffer
;;; ------------------------------------------------------------

(defun makefile-create-up-to-date-overview ()
  "Create a buffer containing an overview of the state of all known targets.
Known targets are targets that are explicitly defined in that makefile;
in other words, all targets that appear on the left hand side of a
dependency in the makefile."
  (interactive)
  (if (y-or-n-p "Are you sure that the makefile being edited is consistent? ")
      ;;
      ;; The rest of this function operates on a temporary makefile, created by
      ;; writing the current contents of the makefile buffer.
      ;;
      (let ((saved-target-table makefile-target-table)
	    (this-buffer (current-buffer))
	    (makefile-up-to-date-buffer
	     (get-buffer-create makefile-up-to-date-buffer-name))
	    (filename (makefile-save-temporary))
	    ;;
	    ;; Forget the target table because it may contain picked-up filenames
	    ;; that are not really targets in the current makefile.
	    ;; We don't want to query these, so get a new target-table with just the
	    ;; targets that can be found in the makefile buffer.
	    ;; The 'old' target table will be restored later.
	    ;;
	    (real-targets (progn
			    (makefile-pickup-targets)
			    makefile-target-table))
	    (prereqs makefile-has-prereqs)
	    )

	(set-buffer makefile-up-to-date-buffer)
	(setq buffer-read-only nil)
	(erase-buffer)
	(makefile-query-targets filename real-targets prereqs)
	(if (zerop (buffer-size))		; if it did not get us anything
	    (progn
	      (kill-buffer (current-buffer))
	      (message "No overview created!")))
	(set-buffer this-buffer)
	(setq makefile-target-table saved-target-table)
	(if (get-buffer makefile-up-to-date-buffer-name)
	    (progn
	      (pop-to-buffer (get-buffer makefile-up-to-date-buffer-name))
	      (shrink-window-if-larger-than-buffer)
	      (sort-lines nil (point-min) (point-max))
	      (setq buffer-read-only t))))))

(defun makefile-save-temporary ()
  "Create a temporary file from the current makefile buffer."
  (let ((filename (makefile-generate-temporary-filename)))
    (write-region (point-min) (point-max) filename nil 0)
    filename))				; return the filename

(defun makefile-generate-temporary-filename ()
  "Create a filename suitable for use in `makefile-save-temporary'.
Be careful to allow brain-dead file systems (DOS, SYSV ...) to cope
with the generated name!"
  (let ((my-name (user-login-name))
	(my-uid (int-to-string (user-uid))))
    (concat "mktmp"
	  (if (> (length my-name) 3)
	      (substring my-name 0 3)
	    my-name)
	  "."
	  (if (> (length my-uid) 3)
	      (substring my-uid 0 3)
	    my-uid))))

(defun makefile-query-targets (filename target-table prereq-list)
  "Fill the up-to-date-overview-buffer.
Checks each target in TARGET-TABLE using `makefile-query-one-target-method'
and generates the overview, one line per target name."
  (insert
   (mapconcat
    (function (lambda (item)
		(let* ((target-name (car item))
		       (no-prereqs (not (member target-name prereq-list)))
		       (needs-rebuild (or no-prereqs 
					  (funcall
					   makefile-query-one-target-method
					   target-name
					   filename))))
		  (format "\t%s%s"
			  target-name
			  (cond (no-prereqs "  .. has no prerequisites")
				(needs-rebuild "  .. NEEDS REBUILD")
				(t "  .. is up to date"))))
		))
    target-table "\n"))
  (goto-char (point-min))
  (delete-file filename))		; remove the tmpfile

(defun makefile-query-by-make-minus-q (target &optional filename)
  (not (zerop
	(call-process makefile-brave-make nil nil nil
		      "-f" filename "-q" target))))



;;; ------------------------------------------------------------
;;; Continuation cleanup
;;; ------------------------------------------------------------

(defun makefile-cleanup-continuations ()
  (if (eq major-mode 'makefile-mode)
      (if (and makefile-cleanup-continuations-p
	       (not buffer-read-only))
	  (save-excursion
	    (goto-char (point-min))
	    (while (re-search-forward "\\\\[ \t]+$" (point-max) t)
	      (replace-match "\\" t t))))))


;;; ------------------------------------------------------------
;;; Warn of suspicious lines
;;; ------------------------------------------------------------

(defun makefile-warn-suspicious-lines ()
  (let ((dont-save nil))
    (if (eq major-mode 'makefile-mode)
	(let ((suspicious
	       (save-excursion
		 (goto-char (point-min))
		 (re-search-forward
		  "\\(^[\t]+$\\)\\|\\(^[ ]+[\t]\\)" (point-max) t))))
	  (if suspicious
	      (let ((line-nr (count-lines (point-min) suspicious)))
		(setq dont-save
		      (not (y-or-n-p
			    (format "Suspicious line %d. Save anyway "
				    line-nr))))))))
    dont-save))
	  


;;; ------------------------------------------------------------
;;; GNU make function support
;;; ------------------------------------------------------------

(defun makefile-insert-gmake-function ()
  "Insert a GNU make function call.
Asks for the name of the function to use (with completion).
Then prompts for all required parameters."
  (interactive)
  (let* ((gm-function-name (completing-read
			     "Function: "
			     makefile-gnumake-functions-alist
			     nil t nil))
	 (gm-function-prompts
	  (cdr (assoc gm-function-name makefile-gnumake-functions-alist))))
    (if (not (zerop (length gm-function-name)))
	(insert (makefile-format-macro-ref
		 (concat gm-function-name " "
			 (makefile-prompt-for-gmake-funargs
			    gm-function-name gm-function-prompts)))
		" "))))

(defun makefile-prompt-for-gmake-funargs (function-name prompt-list)
  (mapconcat
   (function (lambda (one-prompt)
	       (read-string (format "[%s] %s: " function-name one-prompt)
			    nil)))
   prompt-list
   ","))



;;; ------------------------------------------------------------
;;; Utility functions
;;; ------------------------------------------------------------

(defun makefile-do-macro-insertion (macro-name)
  "Insert a macro reference."
  (if (not (zerop (length macro-name)))
      (if (assoc macro-name makefile-runtime-macros-list)
	  (insert "$" macro-name)
	(insert (makefile-format-macro-ref macro-name)))))

(defun makefile-remember-target (target-name &optional has-prereqs)
  "Remember a given target if it is not already remembered for this buffer."
  (if (not (zerop (length target-name)))
      (progn
      (if (not (assoc target-name makefile-target-table))
	  (setq makefile-target-table
		(cons (list target-name) makefile-target-table)))
      (if has-prereqs
	  (setq makefile-has-prereqs
		(cons target-name makefile-has-prereqs))))))

(defun makefile-remember-macro (macro-name)
  "Remember a given macro if it is not already remembered for this buffer."
  (if (not (zerop (length macro-name)))
      (if (not (assoc macro-name makefile-macro-table))
	  (setq makefile-macro-table
		(cons (list macro-name) makefile-macro-table)))))

(defun makefile-forward-after-target-colon ()
  "Move point forward after inserting the terminating colon of a target.
This acts according to the value of `makefile-tab-after-target-colon'."
  (if makefile-tab-after-target-colon
      (insert "\t")
    (insert " ")))

(defun makefile-browser-on-macro-line-p ()
  "Determine if point is on a macro line in the browser."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "\\$[{(]" (makefile-end-of-line-point) t)))

(defun makefile-browser-this-line-target-name ()
  "Extract the target name from a line in the browser."
  (save-excursion
    (end-of-line)
    (skip-chars-backward "^ \t")
    (buffer-substring (point) (1- (makefile-end-of-line-point)))))

(defun makefile-browser-this-line-macro-name ()
  "Extract the macro name from a line in the browser."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "\\$[{(]" (makefile-end-of-line-point) t)
    (let ((macro-start (point)))
      (skip-chars-forward "^})")
      (buffer-substring macro-start (point)))))

(defun makefile-format-macro-ref (macro-name)
  "Format a macro reference.
Uses `makefile-use-curly-braces-for-macros-p'."
  (if (or (char-equal ?\( (string-to-char macro-name))
	  (char-equal ?\{ (string-to-char macro-name)))
      (format "$%s" macro-name)
    (if makefile-use-curly-braces-for-macros-p
	(format "${%s}" macro-name)
      (format "$(%s)" macro-name))))

(defun makefile-browser-get-state-for-line (n)
  (aref makefile-browser-selection-vector (1- n)))

(defun makefile-browser-set-state-for-line (n to-state)
  (aset makefile-browser-selection-vector (1- n) to-state))

(defun makefile-browser-toggle-state-for-line (n)
  (makefile-browser-set-state-for-line n (not (makefile-browser-get-state-for-line n))))

(defun makefile-beginning-of-line-point ()
  (save-excursion
    (beginning-of-line)
    (point)))

(defun makefile-end-of-line-point ()
  (save-excursion
    (end-of-line)
    (point)))

(defun makefile-last-line-p ()
  (= (makefile-end-of-line-point) (point-max)))

(defun makefile-first-line-p ()
  (= (makefile-beginning-of-line-point) (point-min)))



;;; Support for other packages, like add-log and imenu.

(defun makefile-add-log-defun ()
  ;; "Return name of target or macro point is in, or nil."
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at makefile-macroassign-regex)
      (buffer-substring (match-beginning 1)
			(match-end 1)))
     ((progn
	(forward-char)
	(re-search-backward makefile-dependency-regex nil t))
      (buffer-substring (match-beginning 1)
			(match-end 1)))
     (t nil))))

;; FIXME it might be nice to have them separated by macro vs target.
(defun makefile-menu-index-function ()
  ;; "Generate alist of indices for imenu."
  (let (alist
	stupid
	(re (concat makefile-dependency-regex
		    "\\|"
		    makefile-macroassign-regex)))
    (imenu-progress-message stupid 0)
    (goto-char (point-min))
    (while (re-search-forward re nil t)
      (imenu-progress-message stupid)
      (let ((n (if (match-beginning 1) 1 5)))
	(setq alist (cons
		     (cons (buffer-substring (match-beginning n)
					     (match-end n))
			   (match-beginning n))
		     alist))))
    (imenu-progress-message stupid 100)
    (nreverse alist)))

;;; makefile.el ends here
