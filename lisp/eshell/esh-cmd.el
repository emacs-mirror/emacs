;;; esh-cmd.el --- command invocation  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2026 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;;_* Invoking external commands
;;
;; External commands cause processes to be created, by loading
;; external executables into memory.  This is what most normal shells
;; do, most of the time.  For more information, see [External commands].
;;
;;;_* Invoking Lisp functions
;;
;; A Lisp function can be invoked using Lisp syntax, or command shell
;; syntax.  For example, to run `dired' to edit the current directory:
;;
;;   /tmp $ (dired ".")
;;
;; Or:
;;
;;   /tmp $ dired .
;;
;; The latter form is preferable, but the former is more precise,
;; since it involves no translations.  See [Argument parsing], to
;; learn more about how arguments are transformed before passing them
;; to commands.
;;
;; Ordinarily, if 'dired' were also available as an external command,
;; the external version would be called in preference to any Lisp
;; function of the same name.  To change this behavior so that Lisp
;; functions always take precedence, set
;; `eshell-prefer-lisp-functions' to t.

;;;_* Alias functions
;;
;; Whenever a command is specified using a simple name, such as 'ls',
;; Eshell will first look for a Lisp function of the name `eshell/ls'.
;; If it exists, it will be called in preference to any other command
;; which might have matched the name 'ls' (such as command aliases,
;; external commands, Lisp functions of that name, etc).
;;
;; This is the most flexible mechanism for creating new commands,
;; since it does not pollute the global namespace, yet allows you to
;; use all of Lisp's facilities to define that piece of functionality.
;; Most of Eshell's "builtin" commands are defined as alias functions.
;;
;;;_* Lisp arguments
;;
;; It is possible to invoke a Lisp form as an argument.  This can be
;; done either by specifying the form as you might in Lisp, or by
;; using the '$' character to introduce a value-interpolation:
;;
;;   echo (+ 1 2)
;;
;; Or
;;
;;   echo $(+ 1 2)
;;
;; The two forms are equivalent.  The second is required only if the
;; form being interpolated is within a string, or is a subexpression
;; of a larger argument:
;;
;;   echo x$(+ 1 2) "String $(+ 1 2)"
;;
;; To pass a Lisp symbol as an argument, use the alternate quoting
;; syntax, since the single quote character is far too overused in
;; shell syntax:
;;
;;   echo #'lisp-symbol
;;
;; Backquote can also be used:
;;
;;   echo `(list ,lisp-symbol)
;;
;; Lisp arguments are identified using the following regexp:

;;;_* Command hooks
;;
;; There are several hooks involved with command execution, which can
;; be used either to change or augment Eshell's behavior.


;;; Code:

(require 'esh-util)
(require 'esh-arg)
(require 'esh-proc)
(require 'esh-module)
(require 'esh-ext)

(require 'eldoc)
(require 'generator)
(require 'pcomplete)

(eval-when-compile
  (require 'cl-lib))

(defgroup eshell-cmd nil
  "Executing an Eshell command is as simple as typing it in and \
pressing \\<eshell-mode-map>\\[eshell-send-input].
There are several different kinds of commands, however."
  :tag "Command invocation"
  ;; :link '(info-link "(eshell)Command invocation")
  :group 'eshell)

(defcustom eshell-prefer-lisp-functions nil
  "If non-nil, prefer Lisp functions to external commands."
  :type 'boolean)

(defcustom eshell-lisp-regexp "\\([(`]\\|#'\\)"
  "A regexp which, if matched at beginning of an argument, means Lisp.
Such arguments will be passed to `read', and then evaluated."
  :type 'regexp)

(defcustom eshell-lisp-form-nil-is-failure t
  "If non-nil, Lisp forms like (COMMAND ARGS) treat a nil result as failure."
  :type 'boolean)

(defcustom eshell-pre-command-hook nil
  "A hook run before each interactive command is invoked."
  :type 'hook)

(defcustom eshell-post-command-hook nil
  "A hook run after each interactive command is invoked."
  :type 'hook)

(defcustom eshell-prepare-command-hook nil
  "A set of functions called to prepare a named command.
The command name and its argument are in `eshell-last-command-name'
and `eshell-last-arguments'.  The functions on this hook can change
the value of these symbols if necessary.

To prevent a command from executing at all, set
`eshell-last-command-name' to nil."
  :type 'hook)

(defcustom eshell-named-command-hook nil
  "A set of functions called before a named command is invoked.
Each function will be passed the command name and arguments that were
passed to `eshell-named-command'.

If any of the functions returns a non-nil value, the named command
will not be invoked, and that value will be returned from
`eshell-named-command'.

In order to substitute an alternate command form for execution, the
hook function should throw it using the tag `eshell-replace-command'.
For example:

  (add-hook \\='eshell-named-command-hook #\\='subst-with-cd)
  (defun subst-with-cd (command args)
    (throw \\='eshell-replace-command
	   (eshell-parse-command \"cd\" args)))

Although useless, the above code will cause any non-glob, non-Lisp
command (i.e., `ls' as opposed to `*ls' or `(ls)') to be replaced by a
call to `cd' using the arguments that were passed to the function.

When adding a function to this hook, you should also set the property
`eshell-which-function' for the function.  This property should hold a
function that takes a single COMMAND argument and returns a string
describing where Eshell will find the function."
  :type 'hook)

(defcustom eshell-pre-rewrite-command-hook
  '(eshell-no-command-conversion)
  "A hook run before command rewriting begins.
The terms of the command to be rewritten is passed as arguments, and
may be modified in place.  Any return value is ignored."
  :type 'hook)

(defcustom eshell-rewrite-command-hook
  '(eshell-rewrite-for-command
    eshell-rewrite-while-command
    eshell-rewrite-if-command
    eshell-rewrite-sexp-command
    eshell-rewrite-initial-subcommand
    eshell-rewrite-named-command)
  "A set of functions used to rewrite the command argument.
Once parsing of a command line is completed, the next step is to
rewrite the initial argument into something runnable.

A module may wish to associate special behavior with certain argument
syntaxes at the beginning of a command line.  They are welcome to do
so by adding a function to this hook.  The first function to return a
substitute command form is the one used.  Each function is passed the
command's full argument list, which is a list of sexps (typically
forms or strings)."
  :type 'hook)

(defvar eshell-post-rewrite-command-function #'identity
  "Function run after command rewriting is finished.
Takes the (rewritten) command, modifies it as it sees fit and returns
the new result to use instead.")
(defvar eshell-post-rewrite-command-hook nil
  "A hook run after command rewriting is finished.
Each function is passed the symbol containing the rewritten command,
which may be modified directly.  Any return value is ignored.")
(make-obsolete-variable 'eshell-post-rewrite-command-hook
                        'eshell-post-rewrite-command-function "24.4")

(defcustom eshell-complex-commands '("ls")
  "A list of commands names or functions, that determine complexity.
That is, if a command is defined by a function named eshell/NAME,
and NAME is part of this list, it is invoked as a complex command.
Complex commands are always correct, but run much slower.  If a
command works fine without being part of this list, then it doesn't
need to be.

If an entry is a function, it will be called with the name, and should
return non-nil if the command is complex."
  :type '(repeat :tag "Commands"
		 (choice (string :tag "Name")
			 (function :tag "Predicate"))))

;;; User Variables:

(defcustom eshell-cmd-load-hook nil
  "A hook that gets run when `eshell-cmd' is loaded."
  :version "24.1"		       ; removed eshell-cmd-initialize
  :type 'hook)

(defcustom eshell-subcommand-bindings
  '((eshell-in-subcommand-p t)
    (eshell-in-pipeline-p nil)
    (default-directory default-directory))
  "A list of `let' bindings for subcommand environments."
  :version "29.1"		       ; removed `process-environment'
  :type 'sexp
  :risky t)

;;; Internal Variables:

;; These variables have been merged into `eshell-foreground-command'.
;; Outside of this file, the most-common use for them is to check
;; whether they're nil.
(define-obsolete-variable-alias 'eshell-last-async-procs
  'eshell-foreground-command "30.1")
(define-obsolete-variable-alias 'eshell-current-command
  'eshell-foreground-command "30.1")

(defvar eshell-foreground-command nil
  "The currently-running foreground command, if any.
This is a list of the form (FORM PROCESSES).  FORM is the Eshell
command form.  PROCESSES is a list of processes that deferred the
command.")
(defvar eshell-background-commands nil
  "A list of currently-running deferred commands.
Each element is of the form (FORM PROCESSES), as with
`eshell-foreground-command' (which see).")

(defvar eshell-command-name nil)
(defvar eshell-command-arguments nil)
(defvar eshell-in-pipeline-p nil
  "Internal Eshell variable, non-nil inside a pipeline.
Has the value `first', `last' for the first/last commands in the pipeline,
otherwise t.")
(defvar eshell-in-subcommand-p nil)

(defvar eshell-last-arguments nil)
(defvar eshell-last-command-name nil)
(defvar-local eshell-last-command-status 0
  "The exit code from the last command.  0 if successful.")
(defvar-local eshell-last-command-result nil
  "The result of the last command.  Not related to success.")

(defvar eshell-deferrable-commands '(eshell-deferrable)
  "A list of functions which might return a deferrable process.
If they return a process object (or list thereof), execution of the
calling Eshell command will wait for completion (in the background)
before finishing the command.")

(defvar eshell-allow-commands t
  "If non-nil, allow evaluating command forms (including Lisp forms).
If you want to forbid command forms, you can let-bind this to a
non-nil value before calling `eshell-do-eval'.  Then, any command
forms will signal `eshell-commands-forbidden'.  This is useful
if, for example, you want to evaluate simple expressions like
variable expansions, but not fully-evaluate the command.  See
also `eshell-complete-parse-arguments'.")

(define-error 'eshell-commands-forbidden "Commands forbidden")

;;; Functions:

(defsubst eshell-interactive-process-p ()
  "Return non-nil if there is a currently running command process."
  (declare (obsolete 'eshell-foreground-command "30.1"))
  eshell-foreground-command)

(defsubst eshell-head-process ()
  "Return the currently running process at the head of any pipeline.
This only returns external (non-Lisp) processes."
  (caadr eshell-foreground-command))

(defsubst eshell-tail-process ()
  "Return the currently running process at the tail of any pipeline.
This only returns external (non-Lisp) processes."
  (car (last (cadr eshell-foreground-command))))

(define-obsolete-function-alias 'eshell-interactive-process
  'eshell-tail-process "29.1")

(defun eshell-cmd-initialize ()     ;Called from `eshell-mode' via intern-soft!
  "Initialize the Eshell command processing module."
  (setq-local eshell-foreground-command nil)
  (setq-local eshell-background-commands nil)
  (setq-local eshell-command-name nil)
  (setq-local eshell-command-arguments nil)
  (setq-local eshell-last-arguments nil)
  (setq-local eshell-last-command-name nil)

  (add-hook 'eshell-kill-hook #'eshell-resume-command nil t)
  (add-hook 'eshell-parse-argument-hook
	    #'eshell-parse-subcommand-argument nil t)
  (add-hook 'eshell-parse-argument-hook
	    #'eshell-parse-lisp-argument nil t)

  (when (eshell-using-module 'eshell-cmpl)
    (add-hook 'pcomplete-try-first-hook
	      #'eshell-complete-lisp-symbols nil t)))

(defun eshell-complete-lisp-symbols ()
  "If there is a Lisp symbol, complete it."
  (let ((arg (pcomplete-actual-arg)))
    (when (string-match (concat "\\`" eshell-lisp-regexp) arg)
      (setq pcomplete-stub (substring arg (match-end 0))
	    pcomplete-last-completion-raw t)
      (throw 'pcomplete-completions
	     (all-completions pcomplete-stub obarray 'boundp)))))

;; Current command management

(defun eshell-add-command (form &optional background)
  "Add a command FORM to our list of known commands and return the new entry.
If non-nil, BACKGROUND indicates that this is a command running
in the background.  The result is a command entry in the
form (BACKGROUND FORM PROCESSES), where PROCESSES is initially
nil."
  (cons (when background 'background)
        (if background
            (car (push (list form nil) eshell-background-commands))
          (cl-assert (null eshell-foreground-command))
          (setq eshell-foreground-command (list form nil)))))

(defun eshell-remove-command (command)
  "Remove COMMAND from our list of known commands.
COMMAND should be a list of the form (BACKGROUND FORM PROCESSES),
as returned by `eshell-add-command' (which see)."
  (let ((background (car command))
        (entry (cdr command)))
    (if background
        (setq eshell-background-commands
              (delq entry eshell-background-commands))
      (cl-assert (eq eshell-foreground-command entry))
      (setq eshell-foreground-command nil))))

(defun eshell-commands-for-process (process)
  "Return all commands associated with a PROCESS.
Each element will have the form (BACKGROUND FORM PROCESSES), as
returned by `eshell-add-command' (which see).

Usually, there should only be one element in this list, but it's
theoretically possible to have more than one associated command
for a given process."
  (nconc (when (memq process (cadr eshell-foreground-command))
           (list (cons nil eshell-foreground-command)))
         (seq-keep (lambda (cmd)
                     (when (memq process (cadr cmd))
                       (cons 'background cmd)))
                   eshell-background-commands)))

;; Command parsing

(defun eshell-parse-command (command &optional args toplevel)
  "Parse the COMMAND, adding ARGS if given.
COMMAND can be a string, a cons cell (START . END) demarcating a
buffer region, or (:file . FILENAME) to parse the contents of
FILENAME.

TOPLEVEL, if non-nil, means that the outermost command (the
user's input command) is being parsed, and that pre and post
command hooks should be run before and after the command."
  (pcase-let*
    ((terms
      (append
       (if (eshell--region-p command)
           (eshell-parse-arguments (car command) (cdr command))
         (eshell-with-temp-command command
           (goto-char (point-max))
           (eshell-parse-arguments (point-min) (point-max))))
       args))
     ;; Split up our commands in reverse order.
     (`(,sub-chains . ,sep-terms)
      (eshell-split-commands terms "[&;]" t t))
     ;; The last command (first in our reversed list) is implicitly
     ;; terminated by ";".
     (sep-terms (cons ";" sep-terms))
     (commands
      (nreverse
       (mapcan
        (lambda (cmd)
          (let ((sep (pop sep-terms)))
            (if (null cmd)
                (when (equal sep "&")
                  (error "Empty command before `&'"))
              (setq cmd (eshell-parse-pipeline cmd))
              (unless eshell-in-pipeline-p
                (setq cmd `(eshell-do-command ,cmd)))
              ;; Copy I/O handles so each full statement can manipulate
              ;; them if they like.
              (setq cmd `(eshell-with-copied-handles ,cmd))
              (when (equal sep "&")
                (setq cmd `(eshell-do-subjob ,cmd)))
              (list cmd))))
        sub-chains))))
    (if toplevel
	`(eshell-commands (progn
                            (run-hooks 'eshell-pre-command-hook)
                            (unwind-protect
                                (progn ,@commands)
                              (run-hooks 'eshell-post-command-hook))))
      (macroexp-progn commands))))

(defun eshell-debug-show-parsed-args (terms)
  "Display parsed arguments in the debug buffer."
  (ignore (eshell-debug-command 'form
            "parsed arguments\n\n%s" (eshell-stringify terms))))

(defun eshell-no-command-conversion (terms)
  "Don't convert the command argument."
  (ignore
   (if (and (listp (car terms))
	    (eq (caar terms) 'eshell-convert))
       (setcar terms (cadr (car terms))))))

(defun eshell-subcommand-arg-values (terms)
  "Convert subcommand arguments {x} to ${x}, in order to take their values."
  (declare (obsolete nil "31.1"))
  (setq terms (cdr terms))		; skip command argument
  (while terms
    (if (and (listp (car terms))
	     (eq (caar terms) 'eshell-as-subcommand))
	(setcar terms `(eshell-convert
                        (eshell-command-to-value ,(car terms)))))
    (setq terms (cdr terms))))

(defun eshell-rewrite-sexp-command (terms)
  "Rewrite a sexp in initial position, such as `(+ 1 2)'."
  ;; this occurs when a Lisp expression is in first position
  (when (and (listp (car terms))
             (eq (caar terms) 'eshell-lisp-command))
    (car terms)))

(defun eshell-rewrite-initial-subcommand (terms)
  "Rewrite a subcommand in initial position, such as `{+ 1 2}'."
  (if (and (listp (car terms))
	   (eq (caar terms) 'eshell-as-subcommand))
      (car terms)))

(defun eshell-rewrite-named-command (terms)
  "If no other rewriting rule transforms TERMS, assume a named command."
  (when terms
    (setq terms (cons (car terms)
                      ;; Convert arguments to take their values.
                      (mapcar #'eshell-term-as-value (cdr terms))))
    (let ((sym (if eshell-in-pipeline-p
		   'eshell-named-command*
	         'eshell-named-command))
          (grouped-terms (eshell-prepare-splice terms)))
      (cond
       (grouped-terms
        `(let ((new-terms (append ,@grouped-terms)))
           (,sym (car new-terms) (cdr new-terms))))
       ;; If no terms are spliced, use a simpler command form.
       ((cdr terms)
        (list sym (car terms) `(list ,@(cdr terms))))
       (t
        (list sym (car terms)))))))

(defvar eshell--command-body)
(defvar eshell--test-body)

(defsubst eshell-invokify-arg (arg &optional share-output silent)
  "Change ARG so it can be invoked from a structured command.

SHARE-OUTPUT, if non-nil, means this invocation should share the
current output stream, which is separately redirectable.  SILENT
means the user and/or any redirections shouldn't see any output
from this command.  If both SHARE-OUTPUT and SILENT are non-nil,
the second is ignored."
  (declare (obsolete nil "31.1"))
  ;; something that begins with `eshell-convert' means that it
  ;; intends to return a Lisp value.  We want to get past this,
  ;; but if it's not _actually_ a value interpolation -- in which
  ;; we leave it alone.  In fact, the only time we muck with it
  ;; is in the case of a {subcommand} that has been turned into
  ;; the interpolation, ${subcommand}, by the parser because it
  ;; didn't know better.
  (if (and (listp arg)
	   (eq (car arg) 'eshell-convert)
	   (eq (car (cadr arg)) 'eshell-command-to-value))
      (if share-output
	  (cadr (cadr arg))
	`(eshell-commands ,(cadr (cadr arg)) ,silent))
    arg))

(defvar eshell--local-vars nil
  "List of locally bound vars that should take precedence over env-vars.")

(iter-defun eshell-for-iterate (&rest args)
  "Iterate over the elements of each sequence in ARGS.
If ARGS is not a sequence, treat it as a list of one element."
  (dolist (arg args)
    (when (eshell--range-string-p arg)
      (setq arg (eshell--string-to-range arg)))
    (cond
     ((eshell-range-p arg)
      (let ((i (eshell-range-begin arg))
            (end (eshell-range-end arg)))
        ;; NOTE: We could support unbounded ranges here, but those
        ;; aren't very easy to use in Eshell yet.  (We'd need something
        ;; like the "break" statement for "for" loops.)
        (cl-assert (and i end))
        (while (< i end)
          (iter-yield i)
          (incf i))))
     ((stringp arg)
      (iter-yield arg))
     ((listp arg)
      (dolist (i arg) (iter-yield i)))
     ((arrayp arg)
      (dotimes (i (length arg)) (iter-yield (aref arg i))))
     (t
      (iter-yield arg)))))

(defun eshell-rewrite-for-command (terms)
  "Rewrite a `for' command into its equivalent Eshell command form.
Because the implementation of `for' relies upon conditional evaluation
of its argument (i.e., use of a Lisp special form), it must be
implemented via rewriting, rather than as a function."
  (if (and (equal (car terms) "for")
	   (equal (nth 2 terms) "in"))
      (let ((iter-symbol (intern (nth 1 terms)))
            (body (car (last terms))))
	(setcdr (last terms 2) nil)
        `(let ((eshell--local-vars (cons ',iter-symbol eshell--local-vars)))
           (iter-do (,iter-symbol (eshell-for-iterate
                                   ,@(mapcar #'eshell-term-as-value
                                             (nthcdr 3 terms))))
             ,body)))))

(defun eshell-structure-basic-command (func names keyword test &rest body)
  "With TERMS, KEYWORD, and two NAMES, structure a basic command.
The first of NAMES should be the positive form, and the second the
negative.  It's not likely that users should ever need to call this
function."
  (declare (obsolete nil "31.1"))
  (unless test
    (error "Missing test for `%s' command" keyword))

  ;; If the test form is a subcommand, wrap it in `eshell-commands' to
  ;; silence the output.
  (when (memq (car test) '(eshell-as-subcommand eshell-lisp-command))
    (setq test `(eshell-commands ,test t)))

  ;; If the test form begins with `eshell-convert' or
  ;; `eshell-escape-arg', it means something data-wise will be
  ;; returned, and we should let that determine the truth of the
  ;; statement.
  (unless (memq (car test) '(eshell-convert eshell-escape-arg))
    (setq test
	  `(progn (eshell-deferrable ,test)
                  (eshell-exit-success-p))))

  ;; should we reverse the sense of the test?  This depends
  ;; on the `names' parameter.  If it's the symbol nil, yes.
  ;; Otherwise, it can be a pair of strings; if the keyword
  ;; we're using matches the second member of that pair (a
  ;; list), we should reverse it.
  (if (or (eq names nil)
	  (and (listp names)
	       (string= keyword (cadr names))))
      (setq test `(not ,test)))

  ;; Finally, create the form that represents this structured command.
  `(,func ,test ,@body))

(defun eshell-silence-test-command (terms)
  "If TERMS is a subcommand, wrap it in `eshell-commands' to silence output."
  (if (memq (car-safe terms) '(eshell-as-subcommand eshell-lisp-command))
      `(eshell-command-success (eshell-commands ,terms t))
    terms))

(defun eshell-rewrite-while-command (terms)
  "Rewrite a `while' command into its equivalent Eshell command form.
Because the implementation of `while' relies upon conditional
evaluation of its argument (i.e., use of a Lisp special form), it
must be implemented via rewriting, rather than as a function."
  (when (and (stringp (car terms))
             (member (car terms) '("while" "until")))
    (unless (cadr terms)
      (error "Missing test for `while' command"))
    (let ((condition (eshell-silence-test-command (cadr terms))))
      (unless (string= (car terms) "while")
        (setq condition `(not ,condition)))
      `(while ,condition
         ,(caddr terms)))))

(defun eshell-rewrite-if-command (terms)
  "Rewrite an `if' command into its equivalent Eshell command form.
Because the implementation of `if' relies upon conditional
evaluation of its argument (i.e., use of a Lisp special form), it
must be implemented via rewriting, rather than as a function."
  (when (and (stringp (car terms))
             (member (car terms) '("if" "unless")))
    (unless (cadr terms)
      (error "Missing test for `while' command"))
    (let ((condition (eshell-silence-test-command (cadr terms)))
          (then (caddr terms))
          (else (if (equal (nth 3 terms) "else")
                    ;; If there's an "else" keyword, allow chaining
                    ;; together multiple "if" forms...
                    (or (eshell-rewrite-if-command (nthcdr 4 terms))
                        (nth 4 terms))
                  ;; ... otherwise, only allow a single "else" block
                  ;; (without the keyword) as before for compatibility.
                  (nth 3 terms))))
      (unless (string= (car terms) "if")
        (setq condition `(not ,condition)))
      `(if ,condition ,then ,else))))

(defun eshell-set-exit-info (status &optional result)
  "Set the exit status and result for the last command.
STATUS is the process exit code (zero, if the command completed
successfully).  RESULT is the value of the last command."
  (when status
    (setq eshell-last-command-status status))
  (setq eshell-last-command-result result))

(defun eshell-exit-success-p ()
  "Return non-nil if the last command was successful.
This means an exit code of 0."
  (= eshell-last-command-status 0))

(defvar eshell--cmd)

(defun eshell-parse-pipeline (terms)
  "Parse a pipeline from TERMS, return the appropriate Lisp forms."
  (pcase-let*
      ((`(,bigpieces . ,sep-terms)
        (eshell-split-commands terms "\\(&&\\|||\\)" nil t))
       (results) (final))
    (dolist (subterms bigpieces)
      (let* ((pieces (eshell-split-commands subterms "|"))
             (p pieces))
        (while p
          (let ((cmd (car p)))
            (run-hook-with-args 'eshell-pre-rewrite-command-hook cmd)
            (setq cmd (run-hook-with-args-until-success
                       'eshell-rewrite-command-hook cmd))
            (let ((eshell--cmd cmd))
              (run-hook-with-args 'eshell-post-rewrite-command-hook
                                  'eshell--cmd)
              (setq cmd eshell--cmd))
            (setcar p (funcall eshell-post-rewrite-command-function cmd)))
          (setq p (cdr p)))
        (push (if (<= (length pieces) 1)
                  (car pieces)
                (cl-assert (not eshell-in-pipeline-p))
                `(eshell-execute-pipeline (quote ,pieces)))
              results)))
    ;; `results' might be empty; this happens in the case of
    ;; multi-line input
    (setq final (car results)
          results (cdr results)
          sep-terms (nreverse sep-terms))
    (while results
      (cl-assert (car sep-terms))
      (setq final `(,(if (string= (pop sep-terms) "&&") 'and 'or)
                    (eshell-command-success
                     (eshell-deferrable ,(pop results)))
                    ,final)))
    final))

(defun eshell-parse-subcommand-argument ()
  "Parse a subcommand argument of the form `{command}'."
  (if (and (not eshell-current-argument)
	   (not eshell-current-quoted)
	   (eq (char-after) ?\{)
	   (or (= (point-max) (1+ (point)))
	       (not (eq (char-after (1+ (point))) ?\}))))
      (let ((end (eshell-find-delimiter ?\{ ?\})))
        (unless end
          (throw 'eshell-incomplete "{"))
        (when (eshell-arg-delimiter (1+ end))
          (prog1
              `(eshell-as-subcommand
                ,(eshell-parse-command (cons (1+ (point)) end)))
            (goto-char (1+ end)))))))

(defun eshell-parse-lisp-argument ()
  "Parse a Lisp expression which is specified as an argument."
  (if (and (not eshell-current-argument)
	   (not eshell-current-quoted)
	   (looking-at eshell-lisp-regexp))
      (let* ((here (point))
	     (obj
	      (condition-case nil
		  (read (current-buffer))
		(end-of-file
                 (throw 'eshell-incomplete "(")))))
	(if (eshell-arg-delimiter)
	    `(eshell-lisp-command (quote ,obj))
	  (ignore (goto-char here))))))

(defun eshell-split-commands (terms separator &optional
                                    reversed return-seps)
  "Split TERMS using SEPARATOR.
If REVERSED is non-nil, the list of separated term groups will be
returned in reverse order.

If RETURN-SEPS is nil, return just the separated terms as a list;
otherwise, return both the separated terms and their separators
as a pair of lists."
  (let (sub-chains sub-terms sep-terms)
    (dolist (term terms)
      (if (and (eq (car-safe term) 'eshell-operator)
               (string-match (concat "^" separator "$")
                             (nth 1 term)))
          (progn
            (push (nth 1 term) sep-terms)
            (push (nreverse sub-terms) sub-chains)
            (setq sub-terms nil))
        (push term sub-terms)))
    (when terms
      (push (nreverse sub-terms) sub-chains))
    (unless reversed
      (setq sub-chains (nreverse sub-chains)
            sep-terms (nreverse sep-terms)))
    (if return-seps
        (cons sub-chains sep-terms)
      sub-chains)))

(defun eshell-separate-commands (terms separator &optional
				       reversed last-terms-sym)
  "Separate TERMS using SEPARATOR.
If REVERSED is non-nil, the list of separated term groups will be
returned in reverse order.  If LAST-TERMS-SYM is a symbol, its value
will be set to a list of all the separator operators found (or (nil)
if none)."
  (declare (obsolete eshell-split-commands "30.1"))
  (let ((split-terms (eshell-split-commands terms separator reversed
                                            last-terms-sym)))
    (if last-terms-sym
        (progn
          (set last-terms-sym (cdr split-terms))
          (car split-terms))
      split-terms)))

;;_* Command evaluation macros
;;
;; The structure of the following macros is very important to
;; `eshell-do-eval' [Iterative evaluation]:
;;
;; @ Don't use special forms that conditionally evaluate their
;;   arguments, such as `let*', unless Eshell explicitly supports them.
;;   Eshell supports the following special forms: `and', `catch',
;;   `condition-case', `if', `let', `or', `prog1', `progn', `quote',
;;   `setq', `unwind-protect', and `while'.
;;
;; @ The two "special" variables are `eshell-current-handles' and
;;   `eshell-current-subjob-p'.  Bind them locally with a `let' if you
;;   need to change them.  Change them directly only if your intention
;;   is to change the calling environment.
;;
;; These rules likewise apply to any other code that generates forms
;; that `eshell-do-eval' will evaluated, such as command rewriting
;; hooks (see `eshell-rewrite-command-hook' and friends).

(defmacro eshell-with-handles (handle-args &rest body)
  "Create a new set of I/O handles and evaluate BODY.
HANDLE-ARGS is a list of arguments to pass to `eshell-create-handles'.
After evaluating BODY, automatically release the handles, allowing them
to close."
  (declare (indent 1))
  `(let ((eshell-current-handles (eshell-create-handles ,@handle-args)))
     (unwind-protect
         ,(if (length= body 1) (car body) `(progn ,@body))
       (eshell-close-handles))))

(defmacro eshell-with-copied-handles (&rest body)
  "Copy the current I/O handles and evaluate BODY.
After evaluating BODY, automatically release the handles, allowing them
to close."
  (declare (indent 0))
  `(let ((eshell-current-handles
          (eshell-duplicate-handles eshell-current-handles)))
     (unwind-protect
         ,(if (length= body 1) (car body) `(progn ,@body))
       (eshell-close-handles))))

(defmacro eshell-do-subjob (object)
  "Evaluate a command OBJECT as a subjob.
We indicate that the process was run in the background by
returning it as (:eshell-background . PROCESSES)."
  `(let ((eshell-current-subjob-p t)
         ;; Print subjob messages.  This could have been cleared
         ;; (e.g. by `eshell-source-file', which see).
         (eshell-subjob-messages t))
     (eshell-resume-eval (eshell-add-command ',object 'background))))

(defmacro eshell-commands (object &optional silent)
  "Place a valid set of handles, and context, around command OBJECT."
  `(let (eshell-current-subjob-p)
     (eshell-with-handles (,(not silent) 'append)
       ,object)))

(defmacro eshell-command-success (command)
  "Return non-nil if COMMAND exits successfully."
  `(progn ,command (eshell-exit-success-p)))

(defvar eshell-this-command-hook nil)

(defmacro eshell-do-command (object)
  "Trap any errors that occur, so they are not entirely fatal.
Also, the variable `eshell-this-command-hook' is available for the
duration of OBJECT's evaluation.  Note that functions should be added
to this hook using `nconc', and *not* `add-hook'.

Someday, when Scheme will become the dominant Emacs language, all of
this grossness will be made to disappear by using `call/cc'..."
  `(eshell-condition-case err
       (let ((eshell-this-command-hook '(ignore)))
         (unwind-protect
             (eshell-deferrable ,object)
           (mapc #'funcall eshell-this-command-hook)))
     (error
      (eshell-errorn (error-message-string err))
      (eshell-set-exit-info 1))))

(define-obsolete-function-alias 'eshell-trap-errors #'eshell-do-command "31.1")

(defalias 'eshell-deferrable 'identity
  "A wrapper to mark a particular form as potentially deferrable.
If the wrapped form returns a process (or list thereof), Eshell will
wait for completion in the background for the process(es) to complete.")

(define-obsolete-function-alias 'eshell-copy-handles
  #'eshell-with-copied-handles "30.1")

(defmacro eshell-protect (object)
  "Protect I/O handles, so they aren't get closed after eval'ing OBJECT."
  (declare (obsolete nil "31.1"))
  `(progn
     (eshell-protect-handles eshell-current-handles)
     ,object))

(defmacro eshell-do-pipelines (pipeline &optional notfirst)
  "Execute the commands in PIPELINE, connecting each to one another.
Returns a list of the processes in the pipeline.

This macro calls itself recursively, with NOTFIRST non-nil."
  (when (setq pipeline (cadr pipeline))
    `(eshell-with-copied-handles
      (let ((next-procs
             ,(when (cdr pipeline)
                `(eshell-do-pipelines (quote ,(cdr pipeline)) t)))
            ;; First and last elements in a pipeline may need special
            ;; treatment (currently only `eshell-ls-files' uses
            ;; `last').  Affects `process-connection-type' in
            ;; `eshell-gather-process-output'.
            (eshell-in-pipeline-p
             ,(cond ((not notfirst) (quote 'first))
                    ((cdr pipeline) t)
                    (t (quote 'last)))))
        ,(when (cdr pipeline)
           `(eshell-set-output-handle ,eshell-output-handle
                                      'append (car next-procs)))
        (let ((proc ,(car pipeline)))
          (cons proc next-procs))))))

(defmacro eshell-do-pipelines-synchronously (pipeline)
  "Execute the commands in PIPELINE in sequence synchronously.
This collects the output of each command in turn, passing it as
input to the next one in the pipeline.  Returns the result of the
first command invocation in the pipeline (usually t or nil).

This is used on systems where async subprocesses are not
supported."
  (when (setq pipeline (cadr pipeline))
    `(prog1
         (eshell-with-copied-handles
          (progn
            ,(when (cdr pipeline)
               `(let ((output-marker ,(point-marker)))
                  (eshell-set-output-handle ,eshell-output-handle
                                            'append output-marker)))
            (let (;; XXX: `eshell-in-pipeline-p' has a different
                  ;; meaning for synchronous processes: it's non-nil
                  ;; only when piping *to* a process.
                  (eshell-in-pipeline-p ,(and (cdr pipeline) t)))
              ,(car pipeline))))
       ,(when (cdr pipeline)
          `(eshell-do-pipelines-synchronously (quote ,(cdr pipeline)))))))

(define-obsolete-function-alias 'eshell-process-identity #'identity "31.1")

(defmacro eshell-execute-pipeline (pipeline)
  "Execute the commands in PIPELINE, connecting each to one another."
  (if eshell-supports-asynchronous-processes
      `(remove nil (eshell-do-pipelines ,pipeline))
    `(eshell-do-pipelines-synchronously ,pipeline)))

(defmacro eshell-as-subcommand (command)
  "Execute COMMAND as a subcommand.
A subcommand creates a local environment so that any changes to
the environment don't propagate outside of the subcommand's
scope.  This lets you use commands like `cd' within a subcommand
without changing the current directory of the main Eshell
buffer."
  `(let ,eshell-subcommand-bindings
     ,command))

(defmacro eshell-do-command-to-value (object)
  "Run a subcommand prepared by `eshell-command-to-value'.
This avoids the need to use `let*'."
  (declare (obsolete nil "30.1"))
  `(let ((eshell-current-handles
	  (eshell-create-handles value 'overwrite)))
     (progn
       ,object
       (symbol-value value))))

(defmacro eshell-command-to-value (command)
  "Run an Eshell COMMAND synchronously, returning its output."
  (let ((value (make-symbol "eshell-temp")))
    `(eshell-with-handles (',value 'overwrite)
       (let ((eshell-in-pipeline-p nil))
         ,command
         ,value))))

(defun eshell-term-as-value (term)
  "Convert an Eshell TERM to take its value."
  (cond
   ((eq (car-safe term) 'eshell-as-subcommand) ; {x} -> ${x}
    `(eshell-convert (eshell-command-to-value ,term)))
   ((eq (car-safe term) 'eshell-lisp-command)  ; (x) -> $(x)
    `(eshell-command-to-value ,term))
   (t term)))

;;;_* Iterative evaluation
;;
;; Eshell runs all of its external commands asynchronously, so that
;; Emacs is not blocked while the operation is being performed.
;; However, this introduces certain synchronization difficulties,
;; since the Lisp code, once it returns, will not "go back" to finish
;; executing the commands which haven't yet been started.
;;
;; What Eshell does to work around this problem (basically, the lack
;; of threads in Lisp), is that it evaluates the command sequence
;; iteratively.  Whenever an asynchronous process is begun, evaluation
;; terminates and control is given back to Emacs.  When that process
;; finishes, it will resume the evaluation using the remainder of the
;; command tree.

(iter-defun eshell--find-subcommands (haystack)
  "Recursively search for subcommand forms in HAYSTACK.
This yields the SUBCOMMANDs when found in forms like
\"(eshell-as-subcommand SUBCOMMAND)\"."
  (dolist (elem haystack)
    (cond
     ((eq (car-safe elem) 'eshell-as-subcommand)
      (iter-yield (cadr elem)))
     ((listp elem)
      (iter-yield-from (eshell--find-subcommands elem))))))

(defun eshell--invoke-command-directly-p (command)
  "Determine whether the given COMMAND can be invoked directly.
COMMAND should be a non-top-level Eshell command in parsed form.

A command can be invoked directly if all of the following are true:

* The command is of the form
  (eshell-with-copied-handles
   (eshell-do-command (eshell-named-command NAME [ARGS]))).

* NAME is a string referring to an alias function and isn't a
  complex command (see `eshell-complex-commands').

* Any subcommands in ARGS can also be invoked directly."
  (pcase command
    (`(eshell-with-copied-handles
       (eshell-do-command (eshell-named-command ,name . ,args)))
     (and name (stringp name)
	  (not (member name eshell-complex-commands))
	  (catch 'simple
	    (dolist (pred eshell-complex-commands t)
	      (when (and (functionp pred)
		         (funcall pred name))
	        (throw 'simple nil))))
	  (eshell-find-alias-function name)
          (catch 'indirect-subcommand
            (iter-do (subcommand (eshell--find-subcommands (car args)))
              (unless (eshell--invoke-command-directly-p subcommand)
                (throw 'indirect-subcommand nil)))
            t)))))

(defun eshell-invoke-directly-p (command)
  "Determine whether the given COMMAND can be invoked directly.
COMMAND should be a top-level Eshell command in parsed form, as
produced by `eshell-parse-command'."
  (pcase command
    (`(eshell-commands (progn ,_ (unwind-protect (progn ,base) . ,_)))
     (eshell--invoke-command-directly-p base))))

(define-obsolete-function-alias 'eshell-invoke-directly
  'eshell-invoke-directly-p "30.1")

(defun eshell-eval-argument (argument)
  "Evaluate a single Eshell ARGUMENT and return the result."
  (let* ((form (eshell-with-temp-command argument
                 (eshell-parse-argument)))
         (result (eshell-do-eval form t)))
    (cl-assert (eq (car result) 'quote))
    (cadr result)))

(defun eshell-eval-command (command &optional input)
  "Evaluate the given COMMAND iteratively.
Return the process (or head and tail processes) created by
COMMAND, if any.  If COMMAND is a background command, return the
process(es) in a cons cell like:

  (:eshell-background . PROCESSES)"
  (if eshell-foreground-command
      (progn
        ;; We can just stick the new command at the end of the current
        ;; one, and everything will happen as it should.
        (setcdr (last (cdar eshell-foreground-command))
                (list `(let ((here (and (eobp) (point))))
                         ,(and input
                               `(insert-and-inherit ,(concat input "\n")))
                         (if here
                             (eshell-update-markers here))
                         (eshell-do-eval ',command))))
        (eshell-debug-command 'form
          "enqueued command form for %S\n\n%s"
          (or input "<no string>")
          (eshell-stringify (car eshell-foreground-command))))
    (eshell-debug-command-start input)
    (let* (result
           (delim (catch 'eshell-incomplete
                    (ignore (setq result (eshell-resume-eval
                                          (eshell-add-command command)))))))
      (when delim
        (error "Unmatched delimiter: %S" delim))
      result)))

(defun eshell-resume-command (proc status)
  "Resume the current command when a pipeline ends.
PROC is the process that invoked this from its sentinel, and
STATUS is its status."
  (when proc
    ;; Iterate over all the commands associated with this process.  Each
    ;; element is a list of the form (BACKGROUND FORM PROCESSES) (see
    ;; `eshell-add-command').
    (dolist (command (eshell-commands-for-process proc))
      (unless (seq-some #'eshell-process-active-p (nth 2 command))
        (setf (nth 2 command) nil) ; Clear processes from command.
        (if (and ;; Check STATUS to determine whether we want to resume or
                 ;; abort the command.
                 (stringp status)
                 (not (string= "stopped" status))
                 (not (string-match eshell-reset-signals status)))
            (eshell-resume-eval command)
          (eshell-remove-command command)
          ;; Check if the command we just aborted is marked as a
          ;; background command.  If not, we need to reset the prompt so
          ;; the user can enter another command.
          (unless (car command)
            (declare-function eshell-reset "esh-mode" (&optional no-hooks))
            (eshell-reset)))))))

(defun eshell-resume-eval (command)
  "Destructively evaluate a COMMAND which may need to be deferred.
COMMAND is a command entry of the form (BACKGROUND FORM
PROCESSES) (see `eshell-add-command').

Return the result of COMMAND's FORM if it wasn't deferred.  If
BACKGROUND is non-nil and Eshell defers COMMAND, return a list of
the form (:eshell-background . PROCESSES)."
  (eshell-condition-case err
      (let (retval procs)
        (unwind-protect
            (progn
              (setq procs
                    (catch 'eshell-defer
                      (ignore (setq retval (eshell-do-eval (cadr command))))))
              (cond
               (retval (cadr retval))
               ((car command) (cons :eshell-background procs))))
          (if procs
              (setf (nth 2 command) procs)
            ;; If we didn't defer this command, clear it out.  This
            ;; applies both when the command has finished normally,
            ;; and when a signal or thrown value causes us to unwind.
            (eshell-remove-command command))))
    (error
     (error (error-message-string err)))))

(defmacro eshell-manipulate (form tag &rest body)
  "Manipulate a command FORM with BODY, using TAG as a debug identifier."
  (declare (indent 2))
  (let ((tag-symbol (make-symbol "tag")))
    `(if (not (memq 'form eshell-debug-command))
         (progn ,@body)
       (let ((,tag-symbol ,tag))
         (eshell-always-debug-command 'form
           "%s\n\n%s" ,tag-symbol (eshell-stringify ,form))
         (unwind-protect
             (progn ,@body)
           (eshell-always-debug-command 'form
             "done %s\n\n%s" ,tag-symbol (eshell-stringify ,form)))))))

(defun eshell-do-eval (form &optional synchronous-p)
  "Evaluate FORM, simplifying it as we go.
Unless SYNCHRONOUS-P is non-nil, throws `eshell-defer' if it needs to
be finished later after the completion of an asynchronous subprocess.

As this function evaluates FORM, it will gradually replace
subforms with the (quoted) result of evaluating them.  For
example, a function call is replaced with the result of the call.
This allows us to resume evaluation of FORM after something
inside throws `eshell-defer' simply by calling this function
again.  Any forms preceding one that throw `eshell-defer' will
have been replaced by constants."
  (cond
   ((not (listp form))
    (list 'quote (eval form)))
   ((memq (car form) '(quote function))
    form)
   (t
    ;; skip past the call to `eshell-do-eval'
    (when (eq (car form) 'eshell-do-eval)
      (setq form (cadr (cadr form))))
    ;; expand any macros directly into the form.  This is done so that
    ;; we can modify any `let' forms to evaluate only once.
    (if (macrop (car form))
        (let ((exp (copy-tree (macroexpand form))))
          (eshell-manipulate form
              (format-message "expanding macro `%s'" (symbol-name (car form)))
	    (setcar form (car exp))
	    (setcdr form (cdr exp)))))
    (let ((args (cdr form)))
      (cond
       ((eq (car form) 'while)
        ;; Wrap the `while' form with let-bindings for the command and
        ;; test bodies.  This helps us resume evaluation midway
        ;; through the loop.
        (let ((new-form (copy-tree `(let ((eshell--command-body nil)
                                          (eshell--test-body nil))
                                      (eshell--wrapped-while ,@args)))))
          (eshell-manipulate form "modifying while form"
            (setcar form (car new-form))
            (setcdr form (cdr new-form)))
          (eshell-do-eval form synchronous-p)))
       ((eq (car form) 'eshell--wrapped-while)
        (when eshell--command-body
          (cl-assert (not synchronous-p))
          (eshell-do-eval eshell--command-body)
          (setq eshell--command-body nil
                eshell--test-body nil))
        ;; `copy-tree' is needed here so that the test argument
        ;; doesn't get modified and thus always yield the same result.
        (unless eshell--test-body
          (setq eshell--test-body (copy-tree (car args))))
        (while (cadr (eshell-do-eval eshell--test-body synchronous-p))
          (setq eshell--command-body
                (if (cddr args)
                    `(progn ,@(copy-tree (cdr args)))
                  (copy-tree (cadr args))))
          (eshell-do-eval eshell--command-body synchronous-p)
          (setq eshell--command-body nil
                eshell--test-body (copy-tree (car args)))))
       ((eq (car form) 'if)
        (eshell-manipulate form "evaluating if condition"
          ;; Evaluate the condition and replace our `if' form with
          ;; THEN or ELSE as appropriate.
          (let ((new-form
                 (cond
                  ((cadr (eshell-do-eval (car args) synchronous-p))
                   (cadr args))            ; COND is non-nil
                  ((cdddr args)
                   `(progn ,@(cddr args))) ; Multiple ELSE forms
                  (t
                   (caddr args)))))        ; Zero or one ELSE forms
            (unless (consp new-form)
              (setq new-form `(progn ,new-form)))
            (setcar form (car new-form))
            (setcdr form (cdr new-form))))
        (eshell-do-eval form synchronous-p))
       ((memq (car form) '(and or))
        (eshell-manipulate form (format-message "evaluating %s form" (car form))
          (let* ((result (eshell-do-eval (car args) synchronous-p))
                 (value (cadr result)))
            (if (or (null (cdr args))
                    (if (eq (car form) 'or) value (not value)))
                ;; If this is the last sub-form or we short-circuited,
                ;; just return the result.
                result
              ;; Otherwise, remove this sub-form and re-evaluate.
              (setcdr form (cdr args))
              (eshell-do-eval form synchronous-p)))))
       ((eq (car form) 'setcar)
	(setcar (cdr args) (eshell-do-eval (cadr args) synchronous-p))
	(eval form))
       ((eq (car form) 'setcdr)
	(setcar (cdr args) (eshell-do-eval (cadr args) synchronous-p))
	(eval form))
       ((eq (car form) 'let)
        (unless (eq (car-safe (cadr args)) 'eshell-do-eval)
          (eshell-manipulate form "evaluating let args"
            (dolist (letarg (car args))
              (when (and (listp letarg)
                         (not (eq (cadr letarg) 'quote)))
                (setcdr letarg
                        (list (eshell-do-eval
                               (cadr letarg) synchronous-p)))))))
        (cl-progv
            (mapcar (lambda (binding)
                      (if (consp binding) (car binding) binding))
                    (car args))
            ;; These expressions should all be constants now.
            (mapcar (lambda (binding)
                      (when (consp binding) (eval (cadr binding))))
                    (car args))
          (let (deferred result)
            ;; Evaluate the `let' body, catching `eshell-defer' so we
            ;; can handle it below.
            (setq deferred
                  (catch 'eshell-defer
                    (ignore (setq result (eshell-do-eval
                                          (macroexp-progn (cdr args))
                                          synchronous-p)))))
            ;; If something threw `eshell-defer', we need to update
            ;; the let-bindings' values so that those values are
            ;; correct when we resume evaluation of this form.
            (when deferred
              (eshell-manipulate form "rebinding let args after `eshell-defer'"
                (let ((bindings (car args)))
                  (while bindings
                    (let ((binding (if (consp (car bindings))
                                       (caar bindings)
                                     (car bindings))))
                      (setcar bindings
                              (list binding
                                    (list 'quote (symbol-value binding)))))
                    (pop bindings))))
              (throw 'eshell-defer deferred))
            ;; If we get here, there was no `eshell-defer' thrown, so
            ;; just return the `let' body's result.
            result)))
       ((memq (car form) '(catch condition-case))
        ;; `catch' and `condition-case' have to be handled specially,
        ;; because we only want to call `eshell-do-eval' on their
        ;; second forms.
	;;
	;; NOTE: This requires obedience by all forms which this
	;; function might encounter, that they do not contain
	;; other special forms.
        (setq args (cdr args))
	(unless (eq (caar args) 'eshell-do-eval)
          (eshell-manipulate form "handling special form"
	    (setcar args `(eshell-do-eval ',(car args) ,synchronous-p))))
	(eval form))
       ((eq (car form) 'unwind-protect)
        ;; `unwind-protect' has to be handled specially, because we
        ;; only want to call `eshell-do-eval' on its first form, and
        ;; we need to ensure we let `eshell-defer' through without
        ;; evaluating the unwind forms.
        (let (deferred)
          (unwind-protect
              (eshell-manipulate form "handling `unwind-protect' body form"
                (setq deferred
                      (catch 'eshell-defer
                        (ignore
                         (setcar args (eshell-do-eval
                                       (car args) synchronous-p)))))
                (car args))
            (if deferred
                (throw 'eshell-defer deferred)
              (eshell-manipulate form "handling `unwind-protect' unwind forms"
                (pop args)
                (while args
                  (setcar args (eshell-do-eval (car args) synchronous-p))
                  (pop args)))))))
       ((eq (car form) 'setq)
	(if (cddr args) (error "Unsupported form (setq X1 E1 X2 E2..)"))
        (eshell-manipulate form "evaluating arguments to setq"
          (setcar (cdr args) (eshell-do-eval (cadr args) synchronous-p)))
	(list 'quote (eval form)))
       (t
	(if (and args (not (memq (car form) '(run-hooks))))
            (eshell-manipulate form
		(format-message "evaluating arguments to `%s'"
				(car form))
	      (while args
		(setcar args (eshell-do-eval (car args) synchronous-p))
		(setq args (cdr args)))))
	(cond
	 ((eq (car form) 'progn)
	  (car (last (cdr form))))
	 ((eq (car form) 'prog1)
	  (cadr form))
	 (t
	  ;; If a command desire to replace its execution form with
	  ;; another command form, all it needs to do is throw the new
	  ;; form using the exception tag `eshell-replace-command'.
	  ;; For example, let's say that the form currently being
	  ;; eval'd is:
	  ;;
	  ;;   (eshell-named-command "hello")
	  ;;
	  ;; Now, let's assume the 'hello' command is an Eshell alias,
	  ;; the definition of which yields the command:
	  ;;
	  ;;   (eshell-named-command "echo" (list "Hello" "world"))
	  ;;
	  ;; What the alias code would like to do is simply substitute
	  ;; the alias form for the original form.  To accomplish
	  ;; this, all it needs to do is to throw the substitution
	  ;; form with the `eshell-replace-command' tag, and the form
	  ;; will be replaced within the current command, and
	  ;; execution will then resume (iteratively) as before.
	  ;; Thus, aliases can even contain references to asynchronous
	  ;; sub-commands, and things will still work out as they
	  ;; should.
	  (let* (result
                 (new-form
                  (catch 'eshell-replace-command
                    (ignore
                     (setq result (eval form))))))
	    (if new-form
		(progn
                  (eshell-manipulate form "substituting replacement form"
		    (setcar form (car new-form))
		    (setcdr form (cdr new-form)))
		  (eshell-do-eval form synchronous-p))
              (if-let* (((memq (car form) eshell-deferrable-commands))
                        (procs (eshell-make-process-list result)))
                  (if synchronous-p
		      (funcall #'eshell-wait-for-processes procs)
		    (eshell-manipulate form "inserting ignore form"
		      (setcar form 'ignore)
		      (setcdr form nil))
                    (when (seq-some #'eshell-process-active-p procs)
                      (throw 'eshell-defer procs)))
                (list 'quote result))))))))))))

;; command invocation

(declare-function help-fns-function-description-header "help-fns")

(defun eshell/which (command &rest names)
  "Identify the COMMAND, and where it is located."
  (let (not-found)
    (dolist (name (cons command names))
      (condition-case error
          (eshell-printn
           (catch 'found
             (run-hook-wrapped
              'eshell-named-command-hook
              (lambda (hook)
                (when-let* (((symbolp hook))
                            (which-func (get hook 'eshell-which-function))
                            (result (funcall which-func command)))
                  (throw 'found result))))
             (eshell-plain-command--which name)))
        (error (eshell-error (format "which: %s\n" (cadr error)))
               (setq not-found t))))
    (when not-found (eshell-set-exit-info 1))))

(put 'eshell/which 'eshell-no-numeric-conversions t)

(defun eshell-named-command (command &optional args)
  "Insert output from a plain COMMAND, using ARGS.
COMMAND may result in an alias being executed, or a plain command."
  (unless eshell-allow-commands
    (signal 'eshell-commands-forbidden '(named)))
  ;; Strip off any leading nil values.  This can only happen if a
  ;; variable evaluates to nil, such as "$var x", where `var' is nil.
  ;; In that case, the command name becomes `x', for compatibility
  ;; with most regular shells (the difference is that they do an
  ;; interpolation pass before the argument parsing pass, but Eshell
  ;; does both at the same time).
  (while (and (not command) args)
    (setq command (pop args)))
  (setq eshell-last-arguments args
        eshell-last-command-name (eshell-stringify command))
  (run-hook-with-args 'eshell-prepare-command-hook)
  (cl-assert (stringp eshell-last-command-name))
  (when eshell-last-command-name
    (or (run-hook-with-args-until-success
         'eshell-named-command-hook eshell-last-command-name
         eshell-last-arguments)
        (eshell-plain-command eshell-last-command-name
                              eshell-last-arguments))))

(define-obsolete-function-alias 'eshell-named-command* #'eshell-named-command
  "31.1")

(defun eshell-find-alias-function (name)
  "Check whether a function called `eshell/NAME' exists."
  (let* ((sym (intern-soft (concat "eshell/" name)))
	 (file (symbol-file sym 'defun)))
    ;; If the function exists, but is defined in an eshell module
    ;; that's not currently enabled, don't report it as found.
    (if (and file
	     (setq file (file-name-base file))
	     (string-match "\\`\\(em\\|esh\\)-\\([[:alnum:]]+\\)\\'" file))
	(let ((module-sym
	       (intern (concat "eshell-" (match-string 2 file)))))
	  (if (and (functionp sym)
		   (or (null module-sym)
		       (eshell-using-module module-sym)
		       (memq module-sym (eshell-subgroups 'eshell))))
	      sym))
      ;; Otherwise, if it's bound, return it.
      (if (functionp sym)
	  sym))))

(defun eshell--find-plain-lisp-command (command)
  "Look for `eshell/COMMAND' and return it when COMMAND should use it."
  (let* ((esym (eshell-find-alias-function command))
         (sym (or esym (intern-soft command))))
    (when (and sym (fboundp sym)
               (or esym eshell-prefer-lisp-functions
                   (not (eshell-search-path command))))
      sym)))

(defun eshell-plain-command--which (command)
  (if-let* ((sym (eshell--find-plain-lisp-command command)))
      (or (with-output-to-string
            (require 'help-fns)
            (princ (format "%s is " sym))
            (help-fns-function-description-header sym))
          command)
    (eshell-external-command--which command)))

(defun eshell-plain-command (command args)
  "Insert output from a plain COMMAND, using ARGS.
COMMAND may result in either a Lisp function being executed by name,
or an external command."
  (if-let* ((sym (eshell--find-plain-lisp-command command)))
      (eshell-lisp-command sym args)
    (eshell-external-command command args)))

(defun eshell-exec-lisp (printer errprint func-or-form args form-p)
  "Execute a Lisp FUNC-OR-FORM, maybe passing ARGS.
PRINTER and ERRPRINT are functions to use for printing regular
messages and errors, respectively.  FORM-P should be non-nil if
FUNC-OR-FORM represent a Lisp form; ARGS will be ignored in that
case."
  (eshell-condition-case err
      (let ((result
             (save-current-buffer
               (if form-p
                   (eval func-or-form)
                 (apply func-or-form args)))))
        (and result (funcall printer result))
        result)
    (eshell-pipe-broken
     ;; If FUNC-OR-FORM tried and failed to write some output to a
     ;; process, it will raise an `eshell-pipe-broken' signal (this is
     ;; analogous to SIGPIPE on POSIX systems).  In this case, set the
     ;; command status to some non-zero value to indicate an error; to
     ;; match GNU/Linux, we use 141, which the numeric value of
     ;; SIGPIPE on GNU/Linux (13) with the high bit (2^7) set.
     (when (memq eshell-in-pipeline-p '(nil last))
       (eshell-set-exit-info 141))
     nil)
    (error
     (when (memq eshell-in-pipeline-p '(nil last))
       (eshell-set-exit-info 1))
     (let ((msg (error-message-string err)))
       (unless form-p
         (let ((prog-name (string-trim-left (symbol-name func-or-form)
                                            "eshell/")))
           (if (eq (car err) 'wrong-number-of-arguments)
               (setq msg (format "%s usage: %s" prog-name
                                 (elisp-get-fnsym-args-string func-or-form)))
             (setq msg (format "%s: %s" prog-name msg)))))
       (funcall errprint msg))
     nil)))

(defsubst eshell-apply* (printer errprint func args)
  "Call FUNC, with ARGS, trapping errors and return them as output.
PRINTER and ERRPRINT are functions to use for printing regular
messages and errors, respectively."
  (eshell-exec-lisp printer errprint func args nil))

(defsubst eshell-funcall* (printer errprint func &rest args)
  "Call FUNC, with ARGS, trapping errors and return them as output.
PRINTER and ERRPRINT are functions to use for printing regular
messages and errors, respectively."
  (eshell-apply* printer errprint func args))

(defsubst eshell-eval* (printer errprint form)
  "Evaluate FORM, trapping errors and returning them.
PRINTER and ERRPRINT are functions to use for printing regular
messages and errors, respectively."
  (eshell-exec-lisp printer errprint form nil t))

(defsubst eshell-apply (func args)
  "Call FUNC, with ARGS, trapping errors and return them as output.
Print the result using `eshell-print'; if an error occurs, print
it via `eshell-error'."
  (eshell-apply* #'eshell-print #'eshell-error func args))

(defsubst eshell-funcall (func &rest args)
  "Call FUNC, with ARGS, trapping errors and return them as output.
Print the result using `eshell-print'; if an error occurs, print
it via `eshell-error'."
  (eshell-apply func args))

(defsubst eshell-eval (form)
  "Evaluate FORM, trapping errors and returning them.
Print the result using `eshell-print'; if an error occurs, print
it via `eshell-error'."
  (eshell-eval* #'eshell-print #'eshell-error form))

(defsubst eshell-applyn (func args)
  "Call FUNC, with ARGS, trapping errors and return them as output.
Print the result using `eshell-printn'; if an error occurs, print it
via `eshell-errorn'."
  (eshell-apply* #'eshell-printn #'eshell-errorn func args))

(defsubst eshell-funcalln (func &rest args)
  "Call FUNC, with ARGS, trapping errors and return them as output.
Print the result using `eshell-printn'; if an error occurs, print it
via `eshell-errorn'."
  (eshell-applyn func args))

(defsubst eshell-evaln (form)
  "Evaluate FORM, trapping errors and returning them.
Print the result using `eshell-printn'; if an error occurs, print it
via `eshell-errorn'."
  (eshell-eval* #'eshell-printn #'eshell-errorn form))

(defun eshell/funcall (func &rest args)
  "Eshell built-in command for `funcall' (which see).
This simply calls FUNC with the specified ARGS.  FUNC may be a symbol or
a string naming a Lisp function."
  (when (stringp func)
    (setq func (intern func)))
  (apply func args))

(defvar eshell-last-output-end)         ;Defined in esh-mode.el.

(defun eshell-lisp-command (object &optional args)
  "Insert Lisp OBJECT, using ARGS if a function."
  (unless eshell-allow-commands
    (signal 'eshell-commands-forbidden '(lisp)))
  (catch 'eshell-external               ; deferred to an external command
    (when (memq eshell-in-pipeline-p '(nil last))
      (eshell-set-exit-info 0))
    (setq eshell-last-arguments args)
    (let* ((eshell-ensure-newline-p t)
           (command-form-p (functionp object))
           result)
      (if command-form-p
          (let ((numeric (not (get object 'eshell-no-numeric-conversions)))
                (fname-args (get object 'eshell-filename-arguments)))
            (when (or numeric fname-args)
              (while args
                (let ((arg (car args)))
                  (cond
                   ((and numeric (eshell--numeric-string-p arg))
                    ;; If any of the arguments are flagged as numbers
                    ;; waiting for conversion, convert them now.
                    (setcar args (string-to-number arg)))
                   ((and fname-args (stringp arg)
                         (string-equal arg "~"))
                    ;; If any of the arguments match "~", prepend "./"
                    ;; to treat it as a regular file name.
                    (setcar args (concat "./" arg)))))
                (setq args (cdr args))))
            (setq eshell-last-command-name
                  (concat "#<function " (symbol-name object) ">")))
        (setq eshell-last-command-name "#<Lisp object>"))
      (setq result (eshell-exec-lisp
                    #'eshell-print-maybe-n #'eshell-error-maybe-n
                    object eshell-last-arguments (not command-form-p)))
      (when (memq eshell-in-pipeline-p '(nil last))
        (eshell-set-exit-info
         ;; If `eshell-lisp-form-nil-is-failure' is non-nil, Lisp forms
         ;; that succeeded but have a nil result should have an exit
         ;; status of 2.
         (when (and eshell-lisp-form-nil-is-failure
                    (not command-form-p)
                    (= eshell-last-command-status 0)
                    (not result))
           2)
         result))
      nil)))

(define-obsolete-function-alias 'eshell-lisp-command* #'eshell-lisp-command
  "31.1")

(provide 'esh-cmd)

;;; esh-cmd.el ends here
