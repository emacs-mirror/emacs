;;; pcomplete.el --- programmable completion

;; Copyright (C) 1999, 2000, 2001, 2002 Free Sofware Foundation

;; Author: John Wiegley <johnw@gnu.org>
;; Keywords: processes abbrev

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

;; This module provides a programmable completion facility using
;; "completion functions".  Each completion function is responsible
;; for producing a list of possible completions relevant to the current
;; argument position.
;;
;; To use pcomplete with shell-mode, for example, you will need the
;; following in your .emacs file:
;;
;;   (load "pcmpl-auto")
;;   (add-hook 'shell-mode-hook 'pcomplete-shell-setup)
;;
;; Most of the code below simply provides support mechanisms for
;; writing completion functions.  Completion functions themselves are
;; very easy to write.  They have few requirements beyond those of
;; regular Lisp functions.
;;
;; Consider the following example, which will complete against
;; filenames for the first two arguments, and directories for all
;; remaining arguments:
;;
;;   (defun pcomplete/my-command ()
;;     (pcomplete-here (pcomplete-entries))
;;     (pcomplete-here (pcomplete-entries))
;;     (while (pcomplete-here (pcomplete-dirs))))
;;
;; Here are the requirements for completion functions:
;;
;; @ They must be called "pcomplete/MAJOR-MODE/NAME", or
;;   "pcomplete/NAME".  This is how they are looked up, using the NAME
;;   specified in the command argument (the argument in first
;;   position).
;;
;; @ They must be callable with no arguments.
;;
;; @ Their return value is ignored.  If they actually return normally,
;;   it means no completions were available.
;;
;; @ In order to provide completions, they must throw the tag
;;   `pcomplete-completions'.  The value must be the list of possible
;;   completions for the final argument.
;;
;; @ To simplify completion function logic, the tag `pcompleted' may
;;   be thrown with a value of nil in order to abort the function.  It
;;   means that there were no completions available.
;;
;; When a completion function is called, the variable `pcomplete-args'
;; is in scope, and contains all of the arguments specified on the
;; command line.  The variable `pcomplete-last' is the index of the
;; last argument in that list.
;;
;; The variable `pcomplete-index' is used by the completion code to
;; know which argument the completion function is currently examining.
;; It always begins at 1, meaning the first argument after the command
;; name.
;;
;; To facilitate writing completion logic, a special macro,
;; `pcomplete-here', has been provided which does several things:
;;
;;  1. It will throw `pcompleted' (with a value of nil) whenever
;;     `pcomplete-index' exceeds `pcomplete-last'.
;;
;;  2. It will increment `pcomplete-index' if the final argument has
;;     not been reached yet.
;;
;;  3. It will evaluate the form passed to it, and throw the result
;;     using the `pcomplete-completions' tag, if it is called when
;;     `pcomplete-index' is pointing to the final argument.
;;
;; Sometimes a completion function will want to vary the possible
;; completions for an argument based on the previous one.  To
;; facilitate tests like this, the function `pcomplete-test' and
;; `pcomplete-match' are provided.  Called with one argument, they
;; test the value of the previous command argument.  Otherwise, a
;; relative index may be given as an optional second argument, where 0
;; refers to the current argument, 1 the previous, 2 the one before
;; that, etc.  The symbols `first' and `last' specify absolute
;; offsets.
;;
;; Here is an example which will only complete against directories for
;; the second argument if the first argument is also a directory:
;;
;;   (defun pcomplete/example ()
;;      (pcomplete-here (pcomplete-entries))
;;      (if (pcomplete-test 'file-directory-p)
;;          (pcomplete-here (pcomplete-dirs))
;;        (pcomplete-here (pcomplete-entries))))
;;
;; For generating completion lists based on directory contents, see
;; the functions `pcomplete-entries', `pcomplete-dirs',
;; `pcomplete-executables' and `pcomplete-all-entries'.
;;
;; Consult the documentation for `pcomplete-here' for information
;; about its other arguments.

;;; Code:

(provide 'pcomplete)

(defgroup pcomplete nil
  "Programmable completion."
  :version "21.1"
  :group 'processes)

;;; User Variables:

(defcustom pcomplete-file-ignore nil
  "*A regexp of filenames to be disregarded during file completion."
  :type '(choice regexp (const :tag "None" nil))
  :group 'pcomplete)

(defcustom pcomplete-dir-ignore nil
  "*A regexp of names to be disregarded during directory completion."
  :type '(choice regexp (const :tag "None" nil))
  :group 'pcomplete)

(defcustom pcomplete-ignore-case (memq system-type '(ms-dos windows-nt))
  "*If non-nil, ignore case when doing filename completion."
  :type 'boolean
  :group 'pcomplete)

(defcustom pcomplete-autolist nil
  "*If non-nil, automatically list possibilities on partial completion.
This mirrors the optional behavior of tcsh."
  :type 'boolean
  :group 'pcomplete)

(defcustom pcomplete-suffix-list (list directory-sep-char ?:)
  "*A list of characters which constitute a proper suffix."
  :type '(repeat character)
  :group 'pcomplete)

(defcustom pcomplete-recexact nil
  "*If non-nil, use shortest completion if characters cannot be added.
This mirrors the optional behavior of tcsh.

A non-nil value is useful if `pcomplete-autolist' is non-nil too."
  :type 'boolean
  :group 'pcomplete)

(defcustom pcomplete-arg-quote-list nil
  "*List of characters to quote when completing an argument."
  :type '(choice (repeat character)
		 (const :tag "Don't quote" nil))
  :group 'pcomplete)

(defcustom pcomplete-quote-arg-hook nil
  "*A hook which is run to quote a character within a filename.
Each function is passed both the filename to be quoted, and the index
to be considered.  If the function wishes to provide an alternate
quoted form, it need only return the replacement string.  If no
function provides a replacement, quoting shall proceed as normal,
using a backslash to quote any character which is a member of
`pcomplete-arg-quote-list'."
  :type 'hook
  :group 'pcomplete)

(defcustom pcomplete-man-function 'man
  "*A function to that will be called to display a manual page.
It will be passed the name of the command to document."
  :type 'function
  :group 'pcomplete)

(defcustom pcomplete-compare-entry-function 'string-lessp
  "*This function is used to order file entries for completion.
The behavior of most all shells is to sort alphabetically."
  :type '(radio (function-item string-lessp)
		(function-item file-newer-than-file-p)
		(function :tag "Other"))
  :group 'pcomplete)

(defcustom pcomplete-help nil
  "*A string or function (or nil) used for context-sensitive help.
If a string, it should name an Info node that will be jumped to.
If non-nil, it must a sexp that will be evaluated, and whose
result will be shown in the minibuffer.
If nil, the function `pcomplete-man-function' will be called with the
current command argument."
  :type '(choice string sexp (const :tag "Use man page" nil))
  :group 'pcomplete)

(defcustom pcomplete-expand-before-complete nil
  "*If non-nil, expand the current argument before completing it.
This means that typing something such as '$HOME/bi' followed by
\\[pcomplete-argument] will cause the variable reference to be
resolved first, and the resultant value that will be completed against
to be inserted in the buffer.  Note that exactly what gets expanded
and how is entirely up to the behavior of the
`pcomplete-parse-arguments-function'."
  :type 'boolean
  :group 'pcomplete)

(defcustom pcomplete-parse-arguments-function
  'pcomplete-parse-buffer-arguments
  "*A function to call to parse the current line's arguments.
It should be called with no parameters, and with point at the position
of the argument that is to be completed.

It must either return nil, or a cons cell of the form:

  ((ARG...) (BEG-POS...))

The two lists must be identical in length.  The first gives the final
value of each command line argument (which need not match the textual
representation of that argument), and BEG-POS gives the beginning
position of each argument, as it is seen by the user.  The establishes
a relationship between the fully resolved value of the argument, and
the textual representation of the argument."
  :type 'function
  :group 'pcomplete)

(defcustom pcomplete-cycle-completions t
  "*If non-nil, hitting the TAB key cycles through the completion list.
Typical Emacs behavior is to complete as much as possible, then pause
waiting for further input.  Then if TAB is hit again, show a list of
possible completions.  When `pcomplete-cycle-completions' is non-nil,
it acts more like zsh or 4nt, showing the first maximal match first,
followed by any further matches on each subsequent pressing of the TAB
key.  \\[pcomplete-list] is the key to press if the user wants to see
the list of possible completions."
  :type 'boolean
  :group 'pcomplete)

(defcustom pcomplete-cycle-cutoff-length 5
  "*If the number of completions is greater than this, don't cycle.
This variable is a compromise between the traditional Emacs style of
completion, and the \"cycling\" style.  Basically, if there are more
than this number of completions possible, don't automatically pick the
first one and then expect the user to press TAB to cycle through them.
Typically, when there are a large number of completion possibilities,
the user wants to see them in a list buffer so that they can know what
options are available.  But if the list is small, it means the user
has already entered enough input to disambiguate most of the
possibilities, and therefore they are probably most interested in
cycling through the candidates.  Set this value to nil if you want
cycling to always be enabled."
  :type '(choice integer (const :tag "Always cycle" nil))
  :group 'pcomplete)

(defcustom pcomplete-restore-window-delay 1
  "*The number of seconds to wait before restoring completion windows.
Once the completion window has been displayed, if the user then goes
on to type something else, that completion window will be removed from
the display (actually, the original window configuration before it was
displayed will be restored), after this many seconds of idle time.  If
set to nil, completion windows will be left on second until the user
removes them manually.  If set to 0, they will disappear immediately
after the user enters a key other than TAB."
  :type '(choice integer (const :tag "Never restore" nil))
  :group 'pcomplete)

(defcustom pcomplete-try-first-hook nil
  "*A list of functions which are called before completing an argument.
This can be used, for example, for completing things which might apply
to all arguments, such as variable names after a $."
  :type 'hook
  :group 'pcomplete)

(defcustom pcomplete-command-completion-function
  (function
   (lambda ()
     (pcomplete-here (pcomplete-executables))))
  "*Function called for completing the initial command argument."
  :type 'function
  :group 'pcomplete)

(defcustom pcomplete-command-name-function 'pcomplete-command-name
  "*Function called for determining the current command name."
  :type 'function
  :group 'pcomplete)

(defcustom pcomplete-default-completion-function
  (function
   (lambda ()
     (while (pcomplete-here (pcomplete-entries)))))
  "*Function called when no completion rule can be found.
This function is used to generate completions for every argument."
  :type 'function
  :group 'pcomplete)

(defcustom pcomplete-use-paring t
  "*If t, pare alternatives that have already been used.
If nil, you will always see the completion set of possible options, no
matter which of those options have already been used in previous
command arguments."
  :type 'boolean
  :group 'pcomplete)

;;; Internal Variables:

;; for cycling completion support
(defvar pcomplete-current-completions nil)
(defvar pcomplete-last-completion-length)
(defvar pcomplete-last-completion-stub)
(defvar pcomplete-last-completion-raw)
(defvar pcomplete-last-window-config nil)
(defvar pcomplete-window-restore-timer nil)

(make-variable-buffer-local 'pcomplete-current-completions)
(make-variable-buffer-local 'pcomplete-last-completion-length)
(make-variable-buffer-local 'pcomplete-last-completion-stub)
(make-variable-buffer-local 'pcomplete-last-completion-raw)
(make-variable-buffer-local 'pcomplete-last-window-config)
(make-variable-buffer-local 'pcomplete-window-restore-timer)

;; used for altering pcomplete's behavior.  These global variables
;; should always be nil.
(defvar pcomplete-show-help nil)
(defvar pcomplete-show-list nil)
(defvar pcomplete-expand-only-p nil)

;;; User Functions:

;;;###autoload
(defun pcomplete ()
  "Support extensible programmable completion.
To use this function, just bind the TAB key to it, or add it to your
completion functions list (it should occur fairly early in the list)."
  (interactive)
  (if (and (interactive-p)
	   pcomplete-cycle-completions
	   pcomplete-current-completions
	   (memq last-command '(pcomplete
				pcomplete-expand-and-complete
				pcomplete-reverse)))
      (progn
	(delete-backward-char pcomplete-last-completion-length)
	(if (eq this-command 'pcomplete-reverse)
	    (progn
	      (setq pcomplete-current-completions
		    (cons (car (last pcomplete-current-completions))
			  pcomplete-current-completions))
	      (setcdr (last pcomplete-current-completions 2) nil))
	  (nconc pcomplete-current-completions
		 (list (car pcomplete-current-completions)))
	  (setq pcomplete-current-completions
		(cdr pcomplete-current-completions)))
	(pcomplete-insert-entry pcomplete-last-completion-stub
				(car pcomplete-current-completions)
				nil pcomplete-last-completion-raw))
    (setq pcomplete-current-completions nil
	  pcomplete-last-completion-raw nil)
    (catch 'pcompleted
      (let* ((pcomplete-stub)
	     pcomplete-seen pcomplete-norm-func
	     pcomplete-args pcomplete-last pcomplete-index
	     (pcomplete-autolist pcomplete-autolist)
	     (pcomplete-suffix-list pcomplete-suffix-list)
	     (completions (pcomplete-completions))
	     (result (pcomplete-do-complete pcomplete-stub completions)))
	(and result
	     (not (eq (car result) 'listed))
	     (cdr result)
	     (pcomplete-insert-entry pcomplete-stub (cdr result)
				     (memq (car result)
					   '(sole shortest))
				     pcomplete-last-completion-raw))))))

;;;###autoload
(defun pcomplete-reverse ()
  "If cycling completion is in use, cycle backwards."
  (interactive)
  (call-interactively 'pcomplete))

;;;###autoload
(defun pcomplete-expand-and-complete ()
  "Expand the textual value of the current argument.
This will modify the current buffer."
  (interactive)
  (let ((pcomplete-expand-before-complete t))
    (pcomplete)))

;;;###autoload
(defun pcomplete-continue ()
  "Complete without reference to any cycling completions."
  (interactive)
  (setq pcomplete-current-completions nil
	pcomplete-last-completion-raw nil)
  (call-interactively 'pcomplete))

;;;###autoload
(defun pcomplete-expand ()
  "Expand the textual value of the current argument.
This will modify the current buffer."
  (interactive)
  (let ((pcomplete-expand-before-complete t)
	(pcomplete-expand-only-p t))
    (pcomplete)
    (when (and pcomplete-current-completions
	       (> (length pcomplete-current-completions) 0))
      (delete-backward-char pcomplete-last-completion-length)
      (while pcomplete-current-completions
	(unless (pcomplete-insert-entry
		 "" (car pcomplete-current-completions) t
		 pcomplete-last-completion-raw)
	  (insert-and-inherit " "))
	(setq pcomplete-current-completions
	      (cdr pcomplete-current-completions))))))

;;;###autoload
(defun pcomplete-help ()
  "Display any help information relative to the current argument."
  (interactive)
  (let ((pcomplete-show-help t))
    (pcomplete)))

;;;###autoload
(defun pcomplete-list ()
  "Show the list of possible completions for the current argument."
  (interactive)
  (when (and pcomplete-cycle-completions
	     pcomplete-current-completions
	     (eq last-command 'pcomplete-argument))
    (delete-backward-char pcomplete-last-completion-length)
    (setq pcomplete-current-completions nil
	  pcomplete-last-completion-raw nil))
  (let ((pcomplete-show-list t))
    (pcomplete)))

;;; Internal Functions:

;; argument handling

;; for the sake of the bye-compiler, when compiling other files that
;; contain completion functions
(defvar pcomplete-args nil)
(defvar pcomplete-begins nil)
(defvar pcomplete-last nil)
(defvar pcomplete-index nil)
(defvar pcomplete-stub nil)
(defvar pcomplete-seen nil)
(defvar pcomplete-norm-func nil)

(defun pcomplete-arg (&optional index offset)
  "Return the textual content of the INDEXth argument.
INDEX is based from the current processing position.  If INDEX is
positive, values returned are closer to the command argument; if
negative, they are closer to the last argument.  If the INDEX is
outside of the argument list, nil is returned.  The default value for
INDEX is 0, meaning the current argument being examined.

The special indices `first' and `last' may be used to access those
parts of the list.

The OFFSET argument is added to/taken away from the index that will be
used.  This is really only useful with `first' and `last', for
accessing absolute argument positions."
  (setq index
	(if (eq index 'first)
	    0
	  (if (eq index 'last)
	      pcomplete-last
	    (- pcomplete-index (or index 0)))))
  (if offset
      (setq index (+ index offset)))
  (nth index pcomplete-args))

(defun pcomplete-begin (&optional index offset)
  "Return the beginning position of the INDEXth argument.
See the documentation for `pcomplete-arg'."
  (setq index
	(if (eq index 'first)
	    0
	  (if (eq index 'last)
	      pcomplete-last
	    (- pcomplete-index (or index 0)))))
  (if offset
      (setq index (+ index offset)))
  (nth index pcomplete-begins))

(defsubst pcomplete-actual-arg (&optional index offset)
  "Return the actual text representation of the last argument.
This different from `pcomplete-arg', which returns the textual value
that the last argument evaluated to.  This function returns what the
user actually typed in."
  (buffer-substring (pcomplete-begin index offset) (point)))

(defsubst pcomplete-next-arg ()
  "Move the various pointers to the next argument."
  (setq pcomplete-index (1+ pcomplete-index)
	pcomplete-stub (pcomplete-arg))
  (if (> pcomplete-index pcomplete-last)
      (progn
	(message "No completions")
	(throw 'pcompleted nil))))

(defun pcomplete-command-name ()
  "Return the command name of the first argument."
  (file-name-nondirectory (pcomplete-arg 'first)))

(defun pcomplete-match (regexp &optional index offset start)
  "Like `string-match', but on the current completion argument."
  (let ((arg (pcomplete-arg (or index 1) offset)))
    (if arg
	(string-match regexp arg start)
      (throw 'pcompleted nil))))

(defun pcomplete-match-string (which &optional index offset)
  "Like `string-match', but on the current completion argument."
  (let ((arg (pcomplete-arg (or index 1) offset)))
    (if arg
	(match-string which arg)
      (throw 'pcompleted nil))))

(defalias 'pcomplete-match-beginning 'match-beginning)
(defalias 'pcomplete-match-end 'match-end)

(defsubst pcomplete--test (pred arg)
  "Perform a programmable completion predicate match."
  (and pred
       (cond ((eq pred t) t)
	     ((functionp pred)
	      (funcall pred arg))
	     ((stringp pred)
	      (string-match (concat "^" pred "$") arg)))
       pred))

(defun pcomplete-test (predicates &optional index offset)
  "Predicates to test the current programmable argument with."
  (let ((arg (pcomplete-arg (or index 1) offset)))
    (unless (null predicates)
      (if (not (listp predicates))
	  (pcomplete--test predicates arg)
	(let ((pred predicates)
	      found)
	  (while (and pred (not found))
	    (setq found (pcomplete--test (car pred) arg)
		  pred (cdr pred)))
	  found)))))

(defun pcomplete-parse-buffer-arguments ()
  "Parse whitespace separated arguments in the current region."
  (let ((begin (point-min))
	(end (point-max))
	begins args)
    (save-excursion
      (goto-char begin)
      (while (< (point) end)
	(skip-chars-forward " \t\n")
	(setq begins (cons (point) begins))
	(skip-chars-forward "^ \t\n")
	(setq args (cons (buffer-substring-no-properties
			  (car begins) (point))
			 args)))
      (cons (reverse args) (reverse begins)))))

;;;###autoload
(defun pcomplete-comint-setup (completef-sym)
  "Setup a comint buffer to use pcomplete.
COMPLETEF-SYM should be the symbol where the
dynamic-complete-functions are kept.  For comint mode itself, this is
`comint-dynamic-complete-functions'."
  (set (make-local-variable 'pcomplete-parse-arguments-function)
       'pcomplete-parse-comint-arguments)
  (make-local-variable completef-sym)
  (let ((elem (memq 'comint-dynamic-complete-filename
		    (symbol-value completef-sym))))
    (if elem
	(setcar elem 'pcomplete)
      (nconc (symbol-value completef-sym)
	     (list 'pcomplete)))))

;;;###autoload
(defun pcomplete-shell-setup ()
  "Setup shell-mode to use pcomplete."
  (pcomplete-comint-setup 'shell-dynamic-complete-functions))

(defun pcomplete-parse-comint-arguments ()
  "Parse whitespace separated arguments in the current region."
  (let ((begin (save-excursion (comint-bol nil) (point)))
	(end (point))
	begins args)
    (save-excursion
      (goto-char begin)
      (while (< (point) end)
	(skip-chars-forward " \t\n")
	(setq begins (cons (point) begins))
	(let ((skip t))
	  (while skip
	    (skip-chars-forward "^ \t\n")
	    (if (eq (char-before) ?\\)
		(skip-chars-forward " \t\n")
	      (setq skip nil))))
	(setq args (cons (buffer-substring-no-properties
			  (car begins) (point))
			 args)))
      (cons (reverse args) (reverse begins)))))

(defun pcomplete-parse-arguments (&optional expand-p)
  "Parse the command line arguments.  Most completions need this info."
  (let ((results (funcall pcomplete-parse-arguments-function)))
    (when results
      (setq pcomplete-args (or (car results) (list ""))
	    pcomplete-begins (or (cdr results) (list (point)))
	    pcomplete-last (1- (length pcomplete-args))
	    pcomplete-index 0
	    pcomplete-stub (pcomplete-arg 'last))
      (let ((begin (pcomplete-begin 'last)))
	(if (and pcomplete-cycle-completions
		 (listp pcomplete-stub)
		 (not pcomplete-expand-only-p))
	    (let* ((completions pcomplete-stub)
		   (common-stub (car completions))
		   (c completions)
		   (len (length common-stub)))
	      (while (and c (> len 0))
		(while (and (> len 0)
			    (not (string=
				  (substring common-stub 0 len)
				  (substring (car c) 0
					     (min (length (car c))
						  len)))))
		  (setq len (1- len)))
		(setq c (cdr c)))
	      (setq pcomplete-stub (substring common-stub 0 len)
		    pcomplete-autolist t)
	      (when (and begin (not pcomplete-show-list))
		(delete-region begin (point))
		(pcomplete-insert-entry "" pcomplete-stub))
	      (throw 'pcomplete-completions completions))
	  (when expand-p
	    (if (stringp pcomplete-stub)
		(when begin
		  (delete-region begin (point))
		  (insert-and-inherit pcomplete-stub))
	      (if (and (listp pcomplete-stub)
		       pcomplete-expand-only-p)
		  ;; this is for the benefit of `pcomplete-expand'
		  (setq pcomplete-last-completion-length (- (point) begin)
			pcomplete-current-completions pcomplete-stub)
		(error "Cannot expand argument"))))
	  (if pcomplete-expand-only-p
	      (throw 'pcompleted t)
	    pcomplete-args))))))

(defun pcomplete-quote-argument (filename)
  "Return FILENAME with magic characters quoted.
Magic characters are those in `pcomplete-arg-quote-list'."
  (if (null pcomplete-arg-quote-list)
      filename
    (let ((len (length filename))
	  (index 0)
	  (result "")
	  replacement char)
      (while (< index len)
	(setq replacement (run-hook-with-args-until-success
			   'pcomplete-quote-arg-hook filename index))
	(cond
	 (replacement
	  (setq result (concat result replacement)))
	 ((and (setq char (aref filename index))
	       (memq char pcomplete-arg-quote-list))
	  (setq result (concat result "\\" (char-to-string char))))
	 (t
	  (setq result (concat result (char-to-string char)))))
	(setq index (1+ index)))
      result)))

;; file-system completion lists

(defsubst pcomplete-dirs-or-entries (&optional regexp predicate)
  "Return either directories, or qualified entries."
  (append (let ((pcomplete-stub pcomplete-stub))
	    (pcomplete-entries
	     regexp (or predicate
			(function
			 (lambda (path)
			   (not (file-directory-p path)))))))
	  (pcomplete-entries nil 'file-directory-p)))

(defun pcomplete-entries (&optional regexp predicate)
  "Complete against a list of directory candidates.
This function always uses the last argument as the basis for
completion.
If REGEXP is non-nil, it is a regular expression used to refine the
match (files not matching the REGEXP will be excluded).
If PREDICATE is non-nil, it will also be used to refine the match
\(files for which the PREDICATE returns nil will be excluded).
If PATH is non-nil, it will be used for completion instead of
consulting the last argument."
  (let* ((name (substitute-env-vars pcomplete-stub))
	 (default-directory (expand-file-name
			     (or (file-name-directory name)
				 default-directory)))
	 above-cutoff)
    (setq name (file-name-nondirectory name)
	  pcomplete-stub name)
    (let ((completions
	   (file-name-all-completions name default-directory)))
      (if regexp
	  (setq completions
		(pcomplete-pare-list
		 completions nil
		 (function
		  (lambda (file)
		    (not (string-match regexp file)))))))
      (if predicate
	  (setq completions
		(pcomplete-pare-list
		 completions nil
		 (function
		  (lambda (file)
		    (not (funcall predicate file)))))))
      (if (or pcomplete-file-ignore pcomplete-dir-ignore)
	  (setq completions
		(pcomplete-pare-list
		 completions nil
		 (function
		  (lambda (file)
		    (if (eq (aref file (1- (length file)))
			    directory-sep-char)
			(and pcomplete-dir-ignore
			     (string-match pcomplete-dir-ignore file))
		      (and pcomplete-file-ignore
			   (string-match pcomplete-file-ignore file))))))))
      (setq above-cutoff (> (length completions)
			    pcomplete-cycle-cutoff-length))
      (sort completions
	    (function
	     (lambda (l r)
	       ;; for the purposes of comparison, remove the
	       ;; trailing slash from directory names.
	       ;; Otherwise, "foo.old/" will come before "foo/",
	       ;; since . is earlier in the ASCII alphabet than
	       ;; /
	       (let ((left (if (eq (aref l (1- (length l)))
				   directory-sep-char)
			       (substring l 0 (1- (length l)))
			     l))
		     (right (if (eq (aref r (1- (length r)))
				    directory-sep-char)
				(substring r 0 (1- (length r)))
			      r)))
		 (if above-cutoff
		     (string-lessp left right)
		   (funcall pcomplete-compare-entry-function
			    left right)))))))))

(defsubst pcomplete-all-entries (&optional regexp predicate)
  "Like `pcomplete-entries', but doesn't ignore any entries."
  (let (pcomplete-file-ignore
	pcomplete-dir-ignore)
    (pcomplete-entries regexp predicate)))

(defsubst pcomplete-dirs (&optional regexp)
  "Complete amongst a list of directories."
  (pcomplete-entries regexp 'file-directory-p))

(defsubst pcomplete-executables (&optional regexp)
  "Complete amongst a list of directories and executables."
  (pcomplete-entries regexp 'file-executable-p))

;; generation of completion lists

(defun pcomplete-find-completion-function (command)
  "Find the completion function to call for the given COMMAND."
  (let ((sym (intern-soft
	      (concat "pcomplete/" (symbol-name major-mode) "/" command))))
    (unless sym
      (setq sym (intern-soft (concat "pcomplete/" command))))
    (and sym (fboundp sym) sym)))

(defun pcomplete-completions ()
  "Return a list of completions for the current argument position."
  (catch 'pcomplete-completions
    (when (pcomplete-parse-arguments pcomplete-expand-before-complete)
      (if (= pcomplete-index pcomplete-last)
	  (funcall pcomplete-command-completion-function)
	(let ((sym (or (pcomplete-find-completion-function
			(funcall pcomplete-command-name-function))
		       pcomplete-default-completion-function)))
	  (ignore
	   (pcomplete-next-arg)
	   (funcall sym)))))))

(defun pcomplete-opt (options &optional prefix no-ganging args-follow)
  "Complete a set of OPTIONS, each beginning with PREFIX (?- by default).
PREFIX may be t, in which case no PREFIX character is necessary.
If REQUIRED is non-nil, the options must be present.
If NO-GANGING is non-nil, each option is separate.  -xy is not allowed.
If ARGS-FOLLOW is non-nil, then options which arguments which take may
have the argument appear after a ganged set of options.  This is how
tar behaves, for example."
  (if (and (= pcomplete-index pcomplete-last)
	   (string= (pcomplete-arg) "-"))
      (let ((len (length options))
	    (index 0)
	    char choices)
	(while (< index len)
	  (setq char (aref options index))
	  (if (eq char ?\()
	      (let ((result (read-from-string options index)))
		(setq index (cdr result)))
	    (unless (memq char '(?/ ?* ?? ?.))
	      (setq choices (cons (char-to-string char) choices)))
	    (setq index (1+ index))))
	(throw 'pcomplete-completions
	       (mapcar
		(function
		 (lambda (opt)
		   (concat "-" opt)))
		(pcomplete-uniqify-list choices))))
    (let ((arg (pcomplete-arg)))
      (when (and (> (length arg) 1)
		 (stringp arg)
		 (eq (aref arg 0) (or prefix ?-)))
	(pcomplete-next-arg)
	(let ((char (aref arg 1))
	      (len (length options))
	      (index 0)
	      opt-char arg-char result)
	  (while (< (1+ index) len)
	    (setq opt-char (aref options index)
		  arg-char (aref options (1+ index)))
	    (if (eq arg-char ?\()
		(setq result
		      (read-from-string options (1+ index))
		      index (cdr result)
		      result (car result))
	      (setq result nil))
	    (when (and (eq char opt-char)
		       (memq arg-char '(?\( ?/ ?* ?? ?.)))
	      (if (< pcomplete-index pcomplete-last)
		  (pcomplete-next-arg)
		(throw 'pcomplete-completions
		       (cond ((eq arg-char ?/) (pcomplete-dirs))
			     ((eq arg-char ?*) (pcomplete-executables))
			     ((eq arg-char ??) nil)
			     ((eq arg-char ?.) (pcomplete-entries))
			     ((eq arg-char ?\() (eval result))))))
	    (setq index (1+ index))))))))

(defun pcomplete--here (&optional form stub paring form-only)
  "Complete aganst the current argument, if at the end.
See the documentation for `pcomplete-here'."
  (if (< pcomplete-index pcomplete-last)
      (progn
	(if (eq paring 0)
	    (setq pcomplete-seen nil)
	  (unless (eq paring t)
	    (let ((arg (pcomplete-arg)))
	      (unless (not (stringp arg))
		(setq pcomplete-seen
		      (cons (if paring
				(funcall paring arg)
			      (file-truename arg))
			    pcomplete-seen))))))
	(pcomplete-next-arg)
	t)
    (when pcomplete-show-help
      (pcomplete--help)
      (throw 'pcompleted t))
    (if stub
	(setq pcomplete-stub stub))
    (if (or (eq paring t) (eq paring 0))
	(setq pcomplete-seen nil)
      (setq pcomplete-norm-func (or paring 'file-truename)))
    (unless form-only
      (run-hooks 'pcomplete-try-first-hook))
    (throw 'pcomplete-completions (eval form))))

(defmacro pcomplete-here (&optional form stub paring form-only)
  "Complete aganst the current argument, if at the end.
If completion is to be done here, evaluate FORM to generate the list
of strings which will be used for completion purposes.  If STUB is a
string, use it as the completion stub instead of the default (which is
the entire text of the current argument).

For an example of when you might want to use STUB: if the current
argument text is 'long-path-name/', you don't want the completions
list display to be cluttered by 'long-path-name/' appearing at the
beginning of every alternative.  Not only does this make things less
intelligle, but it is also inefficient.  Yet, if the completion list
does not begin with this string for every entry, the current argument
won't complete correctly.

The solution is to specify a relative stub.  It allows you to
substitute a different argument from the current argument, almost
always for the sake of efficiency.

If PARING is nil, this argument will be pared against previous
arguments using the function `file-truename' to normalize them.
PARING may be a function, in which case that function is for
normalization.  If PARING is the value t, the argument dealt with by
this call will not participate in argument paring.  If it the integer
0, all previous arguments that have been seen will be cleared.

If FORM-ONLY is non-nil, only the result of FORM will be used to
generate the completions list.  This means that the hook
`pcomplete-try-first-hook' will not be run."
  `(pcomplete--here (quote ,form) ,stub ,paring ,form-only))

(defmacro pcomplete-here* (&optional form stub form-only)
  "An alternate form which does not participate in argument paring."
  `(pcomplete-here ,form ,stub t ,form-only))

;; display support

(defun pcomplete-restore-windows ()
  "If the only window change was due to Completions, restore things."
  (if pcomplete-last-window-config
      (let* ((cbuf (get-buffer "*Completions*"))
	     (cwin (and cbuf (get-buffer-window cbuf))))
	(when (and cwin (window-live-p cwin))
	  (bury-buffer cbuf)
	  (set-window-configuration pcomplete-last-window-config))))
  (setq pcomplete-last-window-config nil
	pcomplete-window-restore-timer nil))

;; Abstractions so that the code below will work for both Emacs 20 and
;; XEmacs 21

(unless (fboundp 'event-matches-key-specifier-p)
  (defalias 'event-matches-key-specifier-p 'eq))

(unless (fboundp 'read-event)
  (defsubst read-event (&optional prompt)
    (aref (read-key-sequence prompt) 0)))

(unless (fboundp 'event-basic-type)
  (defalias 'event-basic-type 'event-key))

(defun pcomplete-show-completions (completions)
  "List in help buffer sorted COMPLETIONS.
Typing SPC flushes the help buffer."
  (let* ((curbuf (current-buffer)))
    (when pcomplete-window-restore-timer
      (cancel-timer pcomplete-window-restore-timer)
      (setq pcomplete-window-restore-timer nil))
    (unless pcomplete-last-window-config
      (setq pcomplete-last-window-config (current-window-configuration)))
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list completions))
    (message "Hit space to flush")
    (let (event)
      (prog1
	  (catch 'done
	    (while (with-current-buffer (get-buffer "*Completions*")
		     (setq event (read-event)))
	      (cond
	       ((event-matches-key-specifier-p event ? )
		(set-window-configuration pcomplete-last-window-config)
		(setq pcomplete-last-window-config nil)
		(throw 'done nil))
	       ((event-matches-key-specifier-p event 'tab)
		(save-selected-window
		  (select-window (get-buffer-window "*Completions*"))
		  (if (pos-visible-in-window-p (point-max))
		      (goto-char (point-min))
		    (scroll-up)))
		(message ""))
	       (t
		(setq unread-command-events (list event))
		(throw 'done nil)))))
	(if (and pcomplete-last-window-config
		 pcomplete-restore-window-delay)
	    (setq pcomplete-window-restore-timer
		  (run-with-timer pcomplete-restore-window-delay nil
				  'pcomplete-restore-windows)))))))

;; insert completion at point

(defun pcomplete-insert-entry (stub entry &optional addsuffix raw-p)
  "Insert a completion entry at point.
Returns non-nil if a space was appended at the end."
  (let ((here (point)))
    (if (not pcomplete-ignore-case)
	(insert-and-inherit (if raw-p
				(substring entry (length stub))
			      (pcomplete-quote-argument
			       (substring entry (length stub)))))
      ;; the stub is not quoted at this time, so to determine the
      ;; length of what should be in the buffer, we must quote it
      (delete-backward-char (length (pcomplete-quote-argument stub)))
      ;; if there is already a backslash present to handle the first
      ;; character, don't bother quoting it
      (when (eq (char-before) ?\\)
	(insert-and-inherit (substring entry 0 1))
	(setq entry (substring entry 1)))
      (insert-and-inherit (if raw-p
			      entry
			    (pcomplete-quote-argument entry))))
    (let (space-added)
      (when (and (not (memq (char-before) pcomplete-suffix-list))
		 addsuffix)
	(insert-and-inherit " ")
	(setq space-added t))
      (setq pcomplete-last-completion-length (- (point) here)
	    pcomplete-last-completion-stub stub)
      space-added)))

;; selection of completions

(defun pcomplete-do-complete (stub completions)
  "Dynamically complete at point using STUB and COMPLETIONS.
This is basically just a wrapper for `pcomplete-stub' which does some
extra checking, and munging of the COMPLETIONS list."
  (unless (stringp stub)
    (message "Cannot complete argument")
    (throw 'pcompleted nil))
  (if (null completions)
      (ignore
       (if (and stub (> (length stub) 0))
	   (message "No completions of %s" stub)
	 (message "No completions")))
    ;; pare it down, if applicable
    (if (and pcomplete-use-paring pcomplete-seen)
	(let* ((arg (pcomplete-arg))
	       (prefix
		(file-name-as-directory
		 (funcall pcomplete-norm-func
			  (substring arg 0 (- (length arg)
					      (length pcomplete-stub)))))))
	  (setq pcomplete-seen
		(mapcar 'directory-file-name pcomplete-seen))
	  (let ((p pcomplete-seen))
	    (while p
	      (add-to-list 'pcomplete-seen
			   (funcall pcomplete-norm-func (car p)))
	      (setq p (cdr p))))
	  (setq completions
		(mapcar
		 (function
		  (lambda (elem)
		    (file-relative-name elem prefix)))
		 (pcomplete-pare-list
		  (mapcar
		   (function
		    (lambda (elem)
		      (expand-file-name elem prefix)))
		   completions)
		  pcomplete-seen
		  (function
		   (lambda (elem)
		     (member (directory-file-name
			      (funcall pcomplete-norm-func elem))
			     pcomplete-seen))))))))
    ;; OK, we've got a list of completions.
    (if pcomplete-show-list
	(pcomplete-show-completions completions)
      (pcomplete-stub stub completions))))

(defun pcomplete-stub (stub candidates &optional cycle-p)
  "Dynamically complete STUB from CANDIDATES list.
This function inserts completion characters at point by completing
STUB from the strings in CANDIDATES.  A completions listing may be
shown in a help buffer if completion is ambiguous.

Returns nil if no completion was inserted.
Returns `sole' if completed with the only completion match.
Returns `shortest' if completed with the shortest of the matches.
Returns `partial' if completed as far as possible with the matches.
Returns `listed' if a completion listing was shown.

See also `pcomplete-filename'."
  (let* ((completion-ignore-case pcomplete-ignore-case)
	 (candidates (mapcar 'list candidates))
	 (completions (all-completions stub candidates)))
    (let (result entry)
      (cond
       ((null completions)
	(if (and stub (> (length stub) 0))
	    (message "No completions of %s" stub)
	  (message "No completions")))
       ((= 1 (length completions))
	(setq entry (car completions))
	(if (string-equal entry stub)
	    (message "Sole completion"))
	(setq result 'sole))
       ((and pcomplete-cycle-completions
	     (or cycle-p
		 (not pcomplete-cycle-cutoff-length)
		 (<= (length completions)
		     pcomplete-cycle-cutoff-length)))
	(setq entry (car completions)
	      pcomplete-current-completions completions))
       (t ; There's no unique completion; use longest substring
	(setq entry (try-completion stub candidates))
	(cond ((and pcomplete-recexact
		    (string-equal stub entry)
		    (member entry completions))
	       ;; It's not unique, but user wants shortest match.
	       (message "Completed shortest")
	       (setq result 'shortest))
	      ((or pcomplete-autolist
		   (string-equal stub entry))
	       ;; It's not unique, list possible completions.
	       (pcomplete-show-completions completions)
	       (setq result 'listed))
	      (t
	       (message "Partially completed")
	       (setq result 'partial)))))
      (cons result entry))))

;; context sensitive help

(defun pcomplete--help ()
  "Produce context-sensitive help for the current argument.
If specific documentation can't be given, be generic.
INFODOC specifies the Info node to goto.  DOCUMENTATION is a sexp
which will produce documentation for the argument (it is responsible
for displaying in its own buffer)."
  (if (and pcomplete-help
	   (or (and (stringp pcomplete-help)
		    (fboundp 'Info-goto-node))
	       (listp pcomplete-help)))
      (if (listp pcomplete-help)
	  (message (eval pcomplete-help))
	(save-window-excursion (info))
	(switch-to-buffer-other-window "*info*")
	(funcall (symbol-function 'Info-goto-node) pcomplete-help))
    (if pcomplete-man-function
	(let ((cmd (funcall pcomplete-command-name-function)))
	  (if (and cmd (> (length cmd) 0))
	      (funcall pcomplete-man-function cmd)))
      (message "No context-sensitive help available"))))

;; general utilities

(defsubst pcomplete-time-less-p (t1 t2)
  "Say whether time T1 is less than time T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

(defun pcomplete-pare-list (l r &optional pred)
  "Destructively remove from list L all elements matching any in list R.
Test is done using `equal'.
If PRED is non-nil, it is a function used for further removal.
Returns the resultant list."
  (while (and l (or (and r (member (car l) r))
		    (and pred
			 (funcall pred (car l)))))
    (setq l (cdr l)))
  (let ((m l))
    (while m
      (while (and (cdr m)
		  (or (and r (member (cadr m) r))
		      (and pred
			   (funcall pred (cadr m)))))
	(setcdr m (cddr m)))
      (setq m (cdr m))))
  l)

(defun pcomplete-uniqify-list (l)
  "Sort and remove multiples in L."
  (setq l (sort l 'string-lessp))
  (let ((m l))
    (while m
      (while (and (cdr m)
		  (string= (car m)
			   (cadr m)))
	(setcdr m (cddr m)))
      (setq m (cdr m))))
  l)

(defun pcomplete-process-result (cmd &rest args)
  "Call CMD using `call-process' and return the simplest result."
  (with-temp-buffer
    (apply 'call-process cmd nil t nil args)
    (skip-chars-backward "\n")
    (buffer-substring (point-min) (point))))

;; create a set of aliases which allow completion functions to be not
;; quite so verbose

;; jww (1999-10-20): are these a good idea?
; (defalias 'pc-here 'pcomplete-here)
; (defalias 'pc-test 'pcomplete-test)
; (defalias 'pc-opt 'pcomplete-opt)
; (defalias 'pc-match 'pcomplete-match)
; (defalias 'pc-match-string 'pcomplete-match-string)
; (defalias 'pc-match-beginning 'pcomplete-match-beginning)
; (defalias 'pc-match-end 'pcomplete-match-end)

;;; pcomplete.el ends here
