;;; makefile.el --- makefile editing commands for Emacs

;; Copyright (C) 1992 Free Software Foundation, Inc.

;; Author: Thomas Neumann <tom@smart.bo.open.de>
;; Adapted-By: ESR
;; Keywords: unix, tools

;; $Id: makefile.el,v 1.3 1992/07/22 02:13:37 eric Exp eric $

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

;; A major mode for editing Makefiles.  Hairy.  Needs more documentation.
;; If you get familiar with it, please write some and send it to the GNU
;; Emacs maintainers.

;;; Code:

(provide 'makefile)

;;; ------------------------------------------------------------
;;; Configureable stuff
;;; ------------------------------------------------------------

(defvar makefile-mode-name "makefile"
  "The \"pretty name\" of makefile-mode, as it
appears in the modeline.")

(defvar makefile-browser-buffer-name "*Macros and Targets*"
  "Name of the macro- and target browser buffer.")

(defvar makefile-target-colon ":"
  "The string that gets appended to all target names
inserted by makefile-insert-target.
\":\" or \"::\" are quite common values.")

(defvar makefile-macro-assign " = "
  "The string that gets appended to all macro names
inserted by makefile-insert-macro.
The normal value should be \" = \", since this is what
standard make expects. However, newer makes such as dmake
allow a larger variety of different macro assignments, so you
might prefer to use \" += \" or \" := \" .")

(defvar makefile-use-curly-braces-for-macros-p nil
  "Set this variable to a non-nil value if you prefer curly braces
in macro-references, so it looks like ${this}. A value of nil
will cause makefile-mode to use parantheses, making macro references
look like $(this) .")

(defvar makefile-tab-after-target-colon t
  "If you want a TAB (instead of a space) to be appended after the
target colon, then set this to a non-nil value.")

(defvar makefile-browser-leftmost-column 10
  "Number of blanks to the left of the browser selection mark.")

(defvar makefile-browser-cursor-column 10
  "Column in which the cursor is positioned when it moves
up or down in the browser.")

(defvar makefile-browser-selected-mark "+  "
  "String used to mark selected entries in the browser.")

(defvar makefile-browser-unselected-mark "   "
  "String used to mark unselected entries in the browser.")

(defvar makefile-browser-auto-advance-after-selection-p t
  "If this variable is set to a non-nil value the cursor
will automagically advance to the next line after an item
has been selected in the browser.")

(defvar makefile-find-file-autopickup-p t
  "If this variable is set to a non-nil value then finding a file in
a makefile-mode buffer will cause an automatic initial pickup of
all macros and targets from the found file.")

(defvar makefile-pickup-everything-picks-up-filenames-p nil
  "If this variable is set to a non-nil value then
makefile-pickup-everything also picks up filenames as targets
(i.e. it calls makefile-find-filenames-as-targets), otherwise
filenames are omitted.")

(defvar makefile-cleanup-continuations-p t
  "If this variable is set to a non-nil value then makefile-mode
will assure that no line in the file ends with a backslash
(the continuation character) followed by any whitespace.
This is done by silently removing the trailing whitespace, leaving
the backslash itself intact.
IMPORTANT: Please note that enabling this option causes makefile-mode
to MODIFY A FILE WITHOUT YOUR CONFIRMATION when \'it seems necessary\'.")

(defvar makefile-browser-hook '()
  "A function or list of functions to be called just before the
browser is entered. This is executed in the makefile buffer, so
you can for example run a makefile-pickup-everything automatically.")

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
  "List of special targets. You will be offered to complete
on one of those in the minibuffer whenever you enter a \".\"
at the beginning of a line in makefile-mode.")

(defvar makefile-runtime-macros-list
  '(("@") ("&") (">") ("<") ("*") ("^") ("?") ("%"))
  "List of macros that are resolved by make at runtime.
If you insert a macro reference using makefile-insert-macro-ref, the name
of the macro is checked against this list. If it can be found its name will
not be enclosed in { } or ( ).")

(defconst makefile-dependency-regex
  "^[^ \t#:]+\\([ \t]+[^ \t#:]+\\)*[ \t]*:\\($\\|\\([^=].*$\\)\\)"
  "Regex used to find dependency lines in a makefile.")

(defconst makefile-macroassign-regex
  "^[^ \t][^:#=]*[\\*:\\+]?:?=.*$"
  "Regex used to find macro assignment lines in a makefile.")

(defconst makefile-ignored-files-in-pickup-regex
  "\\(^\\..*\\)\\|\\(.*~$\\)\\|\\(.*,v$\\)"
  "Regex for filenames that will NOT be included in the target list.")

;;; ------------------------------------------------------------
;;; The following configurable variables are used in the
;;; up-to-date overview .
;;; The standard configuration assumes that your `make' programm
;;; can be run in question/query mode using the `-q' option, this
;;; means that the command
;;;
;;;    make -q foo
;;;
;;; should return an exit status of zero if the target `foo' is
;;; up to date and a nonzero exit status otherwise.
;;; Many makes can do this although the docs/manpages do not mention
;;; it. Try it with your favourite one. GNU make and Dennis Vaduras
;;; DMake have no problems.
;;; Set the variable `makefile-brave-make' to the name of the
;;; make utility that does this on your system.
;;; To understand what this is all about see the function defintion
;;; of `makefile-query-by-make-minus-q' .
;;; ------------------------------------------------------------

(defvar makefile-brave-make "gmake"
  "A make that can handle the \'-q\' option.")

(defvar makefile-query-one-target-method 'makefile-query-by-make-minus-q
  "A function symbol [one that can be used as the first argument to
funcall] that provides a function that must conform to the following
interface:

* As its first argument, it must accept the name of the target to
  be checked, as a string.

* As its second argument, it may accept the name of a makefile
  as a string. Depending on what you\'re going to do you may
  not need this.

* It must return the integer value 0 (zero) if the given target
  should be considered up-to-date in the context of the given
  makefile, any nonzero integer value otherwise.")

(defvar makefile-up-to-date-buffer-name "*Makefile Up-to-date overview*"
  "Name of the Up-to-date overview buffer.")

(defvar makefile-target-needs-rebuild-mark "  .. NEEDS REBUILD"
  "A string that is appended to the target name in the up-to-date
overview if that target is considered to require a rebuild.")

(defvar makefile-target-up-to-date-mark    "  .. is up to date"
  "A string that is appenden to the target name in the up-to-date
overview if that target is considered up-to-date.")

;;; --- end of up-to-date-overview configuration ------------------


(defvar makefile-mode-map nil
  "The keymap that is used in makefile-mode.")
(if makefile-mode-map
    ()
  (setq makefile-mode-map (make-sparse-keymap))
  ;; set up the keymap
  (define-key makefile-mode-map "$"        'makefile-insert-macro-ref)
  (define-key makefile-mode-map "\C-c:"    'makefile-insert-target-ref)
  (define-key makefile-mode-map ":"        'makefile-electric-colon)
  (define-key makefile-mode-map "="        'makefile-electric-equal)
  (define-key makefile-mode-map "."        'makefile-electric-dot)
  (define-key makefile-mode-map "\C-c\C-t" 'makefile-pickup-targets)
  (define-key makefile-mode-map "\C-c\C-m" 'makefile-pickup-macros)
  (define-key makefile-mode-map "\C-c\C-f" 'makefile-pickup-filenames-as-targets)
  (define-key makefile-mode-map "\C-c\C-0" 'makefile-forget-everything)
  (define-key makefile-mode-map "\C-c0"    'makefile-forget-everything)  
  (define-key makefile-mode-map "\C-c\C-b" 'makefile-switch-to-browser)
  (define-key makefile-mode-map "\C-c\C-p" 'makefile-pickup-everything)
  (define-key makefile-mode-map "\C-c\C-u" 'makefile-create-up-to-date-overview)
  (define-key makefile-mode-map "\C-c\C-i" 'makefile-insert-gmake-function)
  (define-key makefile-mode-map "\M-p"     'makefile-previous-dependency)
  (define-key makefile-mode-map "\M-n"     'makefile-next-dependency))  

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


(defvar makefile-mode-syntax-table nil
  "The syntax-table used in makefile mode.")
(if makefile-mode-syntax-table
    ()
  (setq makefile-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\( "()    " makefile-mode-syntax-table)
  (modify-syntax-entry ?\) ")(    " makefile-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]    " makefile-mode-syntax-table)
  (modify-syntax-entry ?\] "([    " makefile-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}    " makefile-mode-syntax-table)  
  (modify-syntax-entry ?\} "){    " makefile-mode-syntax-table)
  (modify-syntax-entry ?#  "<     " makefile-mode-syntax-table)
  (modify-syntax-entry ?\n ">     " makefile-mode-syntax-table))
  
  
;;; ------------------------------------------------------------
;;; Internal variables.
;;; You don't need to configure below this line.
;;; ------------------------------------------------------------

(defvar makefile-target-table nil
  "Table of all targets that have been inserted in
this Makefile buffer using makefile-insert-target or picked up
using makefile-pickup-targets.")

(defvar makefile-macro-table nil
  "Table of all macros that have been iserted in
this Makefile buffer using makefile-insert-macro or picked up
using makefile-pickup-macros.")

(defvar makefile-browser-client
  "A buffer in makefile-mode that is currently using the browser.")

(defvar makefile-browser-selection-vector nil)

(defvar makefile-mode-hook '())

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
    ("shell" "Command"))
  "A list of GNU make 3.62 function names associated with
the prompts for each function.
This is used in the function makefile-insert-gmake-function .")


;;; ------------------------------------------------------------
;;; The mode function itself.
;;; ------------------------------------------------------------

(defun makefile-mode ()
  "Major mode for editing Makefiles.
Calling this function invokes the function(s) \"makefile-mode-hook\" before
doing anything else.

\\{makefile-mode-map}

In the browser, use the following keys:

\\{makefile-browser-map}

makefile-mode can be configured by modifying the following
variables:

makefile-mode-name:
    The \"pretty name\" of makefile-mode, as it
    appears in the modeline.

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

makefile-find-file-autopickup-p:
   If this variable is set to a non-nil value then finding a file in
   a makefile-mode buffer will cause an automatic initial pickup of
   all macros and targets from the found file.

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
   browser is entered. This is executed in the makefile buffer, so
   you can for example run a makefile-pickup-everything automatically.

makefile-special-targets-list:
   List of special targets. You will be offered to complete
   on one of those in the minibuffer whenever you enter a \".\"
   at the beginning of a line in makefile-mode."
  (interactive)
  (kill-all-local-variables)
  (if (not (memq 'makefile-find-file-autopickup find-file-hooks))
      (setq find-file-hooks
	    (append find-file-hooks (list 'makefile-find-file-autopickup))))
  (if (not (memq 'makefile-cleanup-continuations write-file-hooks))
      (setq write-file-hooks
	    (append write-file-hooks (list 'makefile-cleanup-continuations))))
  (make-variable-buffer-local 'makefile-target-table)
  (make-variable-buffer-local 'makefile-macro-table)
  (makefile-forget-all-macros)
  (makefile-forget-all-targets)
  (setq comment-start "#")
  (setq comment-end "")
  (setq comment-start-skip "#[ \t]*")
  ;; become the current major mode
  (setq major-mode 'makefile-mode)
  (setq mode-name makefile-mode-name)
  ;; activate keymap
  (use-local-map makefile-mode-map)
  (set-syntax-table makefile-mode-syntax-table)
  (run-hooks 'makefile-mode-hook))  


(defun makefile-find-file-autopickup ()
  (if (eq major-mode 'makefile-mode)
      (if makefile-find-file-autopickup-p
	  (makefile-pickup-everything))))

(defun makefile-next-dependency ()
  "Move (point) to the beginning of the next dependency line
below the current position of (point)."
  (interactive)
  (let ((here (point)))
    (end-of-line)
    (if (re-search-forward makefile-dependency-regex (point-max) t)
	(progn (beginning-of-line) t)	; indicate success
      (goto-char here) nil)))
      
(defun makefile-previous-dependency ()
  "Move (point) to the beginning of the next dependency line
above the current position of (point)."
  (interactive)
  (let ((here (point)))
    (beginning-of-line)
    (if (re-search-backward makefile-dependency-regex (point-min) t)
	(progn (beginning-of-line) t)	; indicate success
      (goto-char here) nil)))


(defun makefile-electric-dot ()
  "At (bol), offer completion on makefile-special-targets-list.
Anywhere else just insert a dot."
  (interactive)
  (if (bolp)
      (makefile-insert-special-target)
    (insert ".")))


(defun makefile-insert-special-target ()
  "Offer completion on makefile-special-targets-list and insert
the result at (point)."
  (interactive)
  (let
      ((special-target
       (completing-read "Special target: "
			makefile-special-targets-list nil nil nil)))
    (if (zerop (length special-target))
	()
      (insert (format ".%s:" special-target))
      (makefile-forward-after-target-colon))))


(defun makefile-electric-equal ()
  "At (bol) do makefile-insert-macro. Anywhere else just
self-insert."
  (interactive)
  (if (bolp)
      (call-interactively 'makefile-insert-macro)
    (insert "=")))

(defun makefile-insert-macro (macro-name)
  "Prepare definition of a new macro."
  (interactive "sMacro Name: ")
  (if (not (zerop (length macro-name)))
      (progn
	(beginning-of-line)
	(insert (format "%s%s" macro-name makefile-macro-assign))
	(makefile-remember-macro macro-name))))


(defun makefile-insert-macro-ref (macro-name)
  "Offer completion on a list of known macros, then
insert complete macro-ref at (point) ."
  (interactive
   (list
    (completing-read "Refer to macro: " makefile-macro-table nil nil nil)))
   (if (not (zerop (length macro-name)))
       (if (assoc macro-name makefile-runtime-macros-list)
	   (insert (format "$%s " macro-name))
	 (insert (makefile-format-macro-ref macro-name) " "))))


(defun makefile-insert-target (target-name)
  "Prepare definition of a new target (dependency line)."
  (interactive "sTarget: ")
  (if (not (zerop (length target-name)))
      (progn
	(beginning-of-line)
	(insert (format "%s%s" target-name makefile-target-colon))
	(makefile-forward-after-target-colon)
	(end-of-line)
	(makefile-remember-target target-name))))


(defun makefile-insert-target-ref (target-name)
  "Offer completion on a list of known targets, then
insert complete target-ref at (point) ."
  (interactive
   (list
    (completing-read "Refer to target: " makefile-target-table nil nil nil)))
   (if (not (zerop (length target-name)))
       (progn
	 (insert (format "%s " target-name)))))

(defun makefile-electric-colon ()
  "At (bol) defines a new target, anywhere else just self-insert ."
  (interactive)
  (if (bolp)
      (call-interactively 'makefile-insert-target)
    (insert ":")))


;;; ------------------------------------------------------------
;;; Extracting targets and macros from an existing makefile
;;; ------------------------------------------------------------

(defun makefile-pickup-targets ()
  "Scan a buffer that contains a makefile for target definitions (dependencies)
and add them to the list of known targets."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward makefile-dependency-regex (point-max) t)
      (makefile-add-this-line-targets))))
;      (forward-line 1))))

(defun makefile-add-this-line-targets ()
  (save-excursion
    (beginning-of-line)
    (let ((done-with-line nil))
      (while (not done-with-line)
	(skip-chars-forward " \t")
	(if (not (setq done-with-line (or (eolp)
					  (char-equal (char-after (point)) ?:))))
	    (progn
	      (let* ((start-of-target-name (point))
		     (target-name
		      (progn
			(skip-chars-forward "^ \t:#")
			(buffer-substring start-of-target-name (point)))))
		(if (makefile-remember-target target-name)
		    (message "Picked up target \"%s\"" target-name)))))))))


(defun makefile-pickup-macros ()
  "Scan a buffer that contains a makefile for macro definitions
and add them to the list of known macros."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward makefile-macroassign-regex (point-max) t)
      (makefile-add-this-line-macro)
      (forward-line 1))))

(defun makefile-add-this-line-macro ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (not (eolp))
	(let* ((start-of-macro-name (point))
	       (macro-name (progn
			     (skip-chars-forward "^ \t:#=*")
			     (buffer-substring start-of-macro-name (point)))))
	  (if (makefile-remember-macro macro-name)
	      (message "Picked up macro \"%s\"" macro-name))))))


(defun makefile-pickup-everything ()
  "Calls makefile-pickup-targets and makefile-pickup-macros.
See their documentation for what they do."
  (interactive)
  (makefile-pickup-macros)
  (makefile-pickup-targets)
  (if makefile-pickup-everything-picks-up-filenames-p
      (makefile-pickup-filenames-as-targets)))


(defun makefile-pickup-filenames-as-targets ()
  "Scan the current directory for filenames, check each filename
against makefile-ignored-files-in-pickup-regex and add all qualifying
names to the list of known targets."
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

;;; ------------------------------------------------------------
;;; The browser window
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
  (setq buffer-read-only nil)
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
  (forward-char makefile-browser-cursor-column)
  (setq buffer-read-only t))
  

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
  "Leave the makefile-browser-buffer and return to the buffer
from that it has been entered."
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
    (setq buffer-read-only nil)
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
	    (makefile-browser-get-state-for-line this-line)))))
    (setq buffer-read-only t)
    (beginning-of-line)
    (forward-char makefile-browser-cursor-column)
    (if makefile-browser-auto-advance-after-selection-p
	(makefile-browser-next-line))))

;;;
;;; Making insertions into the client buffer
;;;

(defun makefile-browser-insert-continuation ()
  "In the browser\'s client buffer, go to (end-of-line), insert a \'\\\'
character, insert a new blank line, go to that line and indent by one TAB.
This is most useful in the process of creating continued lines when 'sending' large
dependencies from the browser to the client buffer.
(point) advances accordingly in the client buffer."
  (interactive)
  (save-excursion
    (set-buffer makefile-browser-client)
    (end-of-line)
    (insert "\\\n\t")))

(defun makefile-browser-insert-selection ()
  "Insert all browser-selected targets and/or macros in the browser\'s
client buffer.
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
	(setq makefile-browser-selection-vector
	      (make-vector (+ (length targets) (length macros)) nil))
	(makefile-browser-start-interaction))))
  

(defun makefile-switch-to-browser ()
  (interactive)
  (run-hooks 'makefile-browser-hook)
  (setq makefile-browser-client (current-buffer))
  (makefile-browse makefile-target-table makefile-macro-table))


;;; ------------------------------------------------------------
;;; Up-to-date overview buffer
;;; ------------------------------------------------------------

(defun makefile-create-up-to-date-overview ()
  "Create a buffer containing an overview of the state of all
known targets from the makefile that is currently being edited.
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
			    (makefile-forget-all-targets)
			    (makefile-pickup-targets)
			    makefile-target-table)))

	(set-buffer makefile-up-to-date-buffer)
	(setq buffer-read-only nil)
	(erase-buffer)
	(makefile-query-targets filename real-targets)
	(if (zerop (buffer-size))		; if it did not get us anything
	    (progn
	      (kill-buffer (current-buffer))
	      (message "No overview created!")))
	(set-buffer this-buffer)
	(setq makefile-target-table saved-target-table)
	(if (get-buffer makefile-up-to-date-buffer-name)
	    (progn
	      (pop-to-buffer (get-buffer makefile-up-to-date-buffer-name))
	      (sort-lines nil (point-min) (point-max))
	      (setq buffer-read-only t))))))
      
  

(defun makefile-save-temporary ()
  "Create a temporary file from the current makefile buffer."
  (let ((filename (makefile-generate-temporary-filename)))
    (write-region (point-min) (point-max) filename nil 0)
    filename))				; return the filename

(defun makefile-generate-temporary-filename ()
  "Create a filename suitable for use in makefile-save-temporary.
Be careful to allow brain-dead file systems (DOS, SYSV ...) to cope
with the generated name !"
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

(defun makefile-query-targets (filename target-table)
  "This function fills the up-to-date-overview-buffer.
It checks each target in target-table using makefile-query-one-target-method
and generates the overview, one line per target name."
  (insert
   (mapconcat '(lambda (item)
		 (let ((target-name (car item)))
		   (makefile-format-up-to-date-buffer-entry
		    (funcall makefile-query-one-target-method
			     target-name filename) target-name)))
	      target-table "\n"))
  (goto-char (point-min))
  (delete-file filename))		; remove the tmpfile

(defun makefile-query-by-make-minus-q (target &optional filename)
  (not (zerop (call-process makefile-brave-make nil nil nil "-f" filename "-q" target))))

(defun makefile-format-up-to-date-buffer-entry (needs-rebuild target)
  (format "\t%s%s"
	  target
	  (if needs-rebuild
	      makefile-target-needs-rebuild-mark
	    makefile-target-up-to-date-mark)))


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
;;; GNU make function support
;;; ------------------------------------------------------------

(defun makefile-insert-gmake-function ()
  "This function is intended to help you using the numerous
macro-like \'function calls\' of GNU make.
It will ask you for the name of the function you wish to
use (with completion), then, after you selected the function,
it will prompt you for all required parameters.
This function \'knows\' about the required parameters of every
GNU make function and will use meaningfull prompts for the
various args, making it much easier to take advantage of this
powerfull GNU make feature."
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
	       (read-string (format "[%s] %s: " function-name one-prompt) nil)))
   prompt-list
   ","))
    


;;; ------------------------------------------------------------
;;; Utility functions
;;; ------------------------------------------------------------

(defun makefile-forget-all-targets ()
  "Clear the target-table for this buffer."
  (interactive)
  (setq makefile-target-table '()))

(defun makefile-forget-all-macros ()
  "Clear the macro-table for this buffer."
  (interactive)
  (setq makefile-macro-table '()))


(defun makefile-forget-everything ()
  "Clear the macro-table AND the target-table for this buffer."
  (interactive)
  (if (y-or-n-p "Really forget all macro- and target information ? ")
      (progn
	(makefile-forget-all-targets)
	(makefile-forget-all-macros)
	(if (get-buffer makefile-browser-buffer-name)
	    (kill-buffer makefile-browser-buffer-name))
	(message "Cleared macro- and target tables."))))

(defun makefile-remember-target (target-name)
  "Remember a given target if it is not already remembered for this buffer."
  (if (not (zerop (length target-name)))
      (if (not (assoc target-name makefile-target-table))
	  (setq makefile-target-table
		(cons (list target-name) makefile-target-table)))))

(defun makefile-remember-macro (macro-name)
  "Remember a given macro if it is not already remembered for this buffer."
  (if (not (zerop (length macro-name)))
      (if (not (assoc macro-name makefile-macro-table))
	  (setq makefile-macro-table
		(cons (list macro-name) makefile-macro-table)))))

(defun makefile-forward-after-target-colon ()
"Move point forward after the terminating colon
of a target has been inserted.
This accts according to the value of makefile-tab-after-target-colon ."
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
  "Format a macro reference according to the value of the
configuration variable makefile-use-curly-braces-for-macros-p ."
  (if makefile-use-curly-braces-for-macros-p
      (format "${%s}" macro-name)
    (format "$(%s)" macro-name)))

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

;; makefile.el ends here
