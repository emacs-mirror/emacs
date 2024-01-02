;;; esh-var.el --- handling of variables  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2024 Free Software Foundation, Inc.

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

;; These are the possible variable interpolation syntaxes.  Also keep
;; in mind that if an argument looks like a number, it will be
;; converted to a number.  This is not significant when invoking
;; external commands, but it's important when calling Lisp functions.
;;
;;   $VARIABLE
;;
;; Interval the value of an environment variable, or a Lisp variable
;;
;;   $ALSO-VAR
;;
;; "-" is a valid part of a variable name.
;;
;;   $\"MYVAR\"-TOO
;;   $'MYVAR'-TOO
;;
;; Only "MYVAR" is part of the variable name in this case.
;;
;;   $(lisp)
;;
;; Returns result of Lisp evaluation.  Note: Used alone like this, it
;; is identical to just saying (lisp); but with the variable expansion
;; form, the result may be interpolated a larger string, such as
;; '$(lisp)/other'.
;;
;;   ${command}
;;
;; Returns the value of an eshell subcommand.  See the note above
;; regarding Lisp evaluations.
;;
;;   $<command>
;;
;; Evaluates an eshell subcommand, redirecting the output to a
;; temporary file, and returning the file name.
;;
;;   $EXPR[10]
;;
;; Return the 10th element of $EXPR, which can be any dollar
;; expression.  If $EXPR's value is a string, it will be split in
;; order to make it a list.  The splitting will occur at whitespace.
;;
;;   $EXPR[10 20]
;;
;; As above, but instead of returning a single element, it now returns a
;; list of two elements.
;;
;;   $EXPR[: 10]
;;
;; Like $EXPR[10], except that splitting occurs at the colon now.
;;
;;   $EXPR["\\\\" 10]
;;
;; Separate on backslash characters.  Actually, the first argument --
;; if it doesn't have the form of a number -- can be any regular
;; expression.  So to split on numbers, use '$EXPR["[0-9]+" 10 20]'.
;;
;;   $EXPR[hello]
;;
;; Calls `assoc' on $EXPR with 'hello', expecting it to be an alist.
;;
;;   $#EXPR
;;
;; Returns the length of the value of $EXPR.  This could also be
;; done using the `length' Lisp function.
;;
;; There are also a few special variables defined by Eshell.  '$$' is
;; the value of the last command (t or nil, in the case of an external
;; command).  This makes it possible to chain results:
;;
;;   /tmp $ echo /var/spool/mail/johnw
;;   /var/spool/mail/johnw
;;   /tmp $ dirname $$
;;   /var/spool/mail/
;;   /tmp $ cd $$
;;   /var/spool/mail $
;;
;; '$_' refers to the last argument of the last command.  And $?
;; contains the exit code of the last command (0 or 1 for Lisp
;; functions, based on successful completion).

;;; Code:

(require 'esh-util)
(require 'esh-cmd)
(require 'esh-opt)
(require 'esh-module)
(require 'esh-arg)
(require 'esh-io)

(require 'pcomplete)
(require 'ring)

(defvar-local eshell-inside-emacs (format "%s,eshell" emacs-version)
  "Value for the `INSIDE_EMACS' environment variable.")

(defgroup eshell-var nil
  "Variable interpolation is introduced whenever the `$' character
appears unquoted in any argument (except when that argument is
surrounded by single quotes).  It may be used to interpolate a
variable value, a subcommand, or even the result of a Lisp form."
  :tag "Variable handling"
  :group 'eshell)

;;; User Variables:

(defcustom eshell-var-load-hook nil
  "A list of functions to call when loading `eshell-var'."
  :version "24.1"			; removed eshell-var-initialize
  :type 'hook)

(defcustom eshell-prefer-lisp-variables nil
  "If non-nil, prefer Lisp variables to environment variables."
  :type 'boolean)

(defcustom eshell-complete-export-definition t
  "If non-nil, completing names for `export' shows current definition."
  :type 'boolean)

(defcustom eshell-modify-global-environment nil
  "If non-nil, using `export' changes Emacs's global environment."
  :type 'boolean)

(defcustom eshell-variable-name-regexp "[A-Za-z0-9_-]+"
  "A regexp identifying what constitutes a variable name reference.
Note that this only applies for `$NAME'.  If the syntax `$<NAME>' is
used, then NAME can contain any character, including angle brackets,
if they are quoted with a backslash."
  :type 'regexp)

(defcustom eshell-variable-aliases-list
  `(;; for eshell.el
    ("COLUMNS" ,(lambda () (window-body-width nil 'remap)) t t)
    ("LINES" ,(lambda () (window-body-height nil 'remap)) t t)
    ("INSIDE_EMACS" eshell-inside-emacs t)

    ;; for esh-ext.el
    ("PATH" (,(lambda () (string-join (eshell-get-path t) (path-separator)))
             . ,(lambda (_ value)
                  (eshell-set-path value)
                  value))
     t t)

    ;; for esh-cmd.el
    ("_" ,(lambda (indices quoted)
	    (if (not indices)
	        (car (last eshell-last-arguments))
	      (eshell-apply-indices eshell-last-arguments
				    indices quoted))))
    ("?" (eshell-last-command-status . nil))
    ("$" (eshell-last-command-result . nil))

    ;; for em-alias.el and em-script.el
    ("0" eshell-command-name)
    ("1" ,(lambda () (nth 0 eshell-command-arguments)) nil t)
    ("2" ,(lambda () (nth 1 eshell-command-arguments)) nil t)
    ("3" ,(lambda () (nth 2 eshell-command-arguments)) nil t)
    ("4" ,(lambda () (nth 3 eshell-command-arguments)) nil t)
    ("5" ,(lambda () (nth 4 eshell-command-arguments)) nil t)
    ("6" ,(lambda () (nth 5 eshell-command-arguments)) nil t)
    ("7" ,(lambda () (nth 6 eshell-command-arguments)) nil t)
    ("8" ,(lambda () (nth 7 eshell-command-arguments)) nil t)
    ("9" ,(lambda () (nth 8 eshell-command-arguments)) nil t)
    ("*" (eshell-command-arguments . nil)))
  "This list provides aliasing for variable references.
Each member is of the following form:

  (NAME VALUE [COPY-TO-ENVIRONMENT] [SIMPLE-FUNCTION])

NAME defines the name of the variable, VALUE is a Lisp value used to
compute the string value that will be returned when the variable is
accessed via the syntax `$NAME'.

If VALUE is a cons (GET . SET), then variable references to NAME
will use GET to get the value, and SET to set it.  GET and SET
can be one of the forms described below.  If SET is nil, the
variable is read-only.

If VALUE is a function, its behavior depends on the value of
SIMPLE-FUNCTION.  If SIMPLE-FUNCTION is nil, call VALUE with two
arguments: the list of the indices that were used in the reference,
and either t or nil depending on whether or not the variable was
quoted with double quotes.  For example, if `NAME' were aliased
to a function, a reference of `$NAME[10][20]' would result in that
function being called with the arguments `((\"10\") (\"20\"))' and
nil.  If SIMPLE-FUNCTION is non-nil, call the function with no
arguments and then pass its return value to `eshell-apply-indices'.

When VALUE is a function, it's read-only by default.  To make it
writable, use the (GET . SET) form described above.  If SET is a
function, it takes two arguments: a list of indices (currently
always nil, but reserved for future enhancement), and the new
value to set.

If VALUE is a string, get/set the value for the variable with
that name in the current environment.  When getting the value, if
no variable with that name exists in the environment, but if a
symbol with that same name exists and has a value bound to it,
return that symbol's value instead.  You can prefer symbol values
over environment values by setting the value of
`eshell-prefer-lisp-variables' to t.

If VALUE is a symbol, get/set the value bound to it.

If VALUE has any other type, signal an error.

Additionally, if COPY-TO-ENVIRONMENT is non-nil, the alias should be
copied (a.k.a. \"exported\") to the environment of created subprocesses."
  :version "29.1"
  :type '(repeat (list string sexp
		       (choice (const :tag "Copy to environment" t)
                               (const :tag "Use only in Eshell" nil))
                       (choice (const :tag "Call without argument" t)
                               (const :tag "Call with 2 arguments" nil))))
  :risky t)

(defvar-keymap eshell-var-mode-map
  "C-c M-v" #'eshell-insert-envvar)

;;; Functions:

(define-minor-mode eshell-var-mode
  "Minor mode for the esh-var module.

\\{eshell-var-mode-map}"
  :keymap eshell-var-mode-map)

(defun eshell-var-initialize ()     ;Called from `eshell-mode' via intern-soft!
  "Initialize the variable handle code."
  ;; Break the association with our parent's environment.  Otherwise,
  ;; changing a variable will affect all of Emacs.
  (unless eshell-modify-global-environment
    (setq-local process-environment (eshell-copy-environment)))
  (setq-local eshell-subcommand-bindings
              (append
               '((process-environment (eshell-copy-environment))
                 (eshell-variable-aliases-list eshell-variable-aliases-list)
                 (eshell-path-env-list eshell-path-env-list))
               eshell-subcommand-bindings))

  (setq-local eshell-special-chars-inside-quoting
       (append eshell-special-chars-inside-quoting '(?$)))
  (setq-local eshell-special-chars-outside-quoting
       (append eshell-special-chars-outside-quoting '(?$)))

  (add-hook 'eshell-parse-argument-hook #'eshell-interpolate-variable t t)

  (add-hook 'eshell-prepare-command-hook
	    #'eshell-handle-local-variables nil t)

  (when (eshell-using-module 'eshell-cmpl)
    (add-hook 'pcomplete-try-first-hook
	      #'eshell-complete-variable-reference nil t)
    (add-hook 'pcomplete-try-first-hook
	      #'eshell-complete-variable-assignment nil t)))

(defun eshell-handle-local-variables ()
  "Allow for the syntax `VAR=val <command> <args>'."
  ;; strip off any null commands, which can only happen if a variable
  ;; evaluates to nil, such as "$var x", where `var' is nil.  The
  ;; command name in that case becomes `x', for compatibility with
  ;; most regular shells (the difference is that they do an
  ;; interpolation pass before the argument parsing pass, but Eshell
  ;; does both at the same time).
  (while (and (not eshell-last-command-name)
	      eshell-last-arguments)
    (setq eshell-last-command-name (car eshell-last-arguments)
	  eshell-last-arguments (cdr eshell-last-arguments)))
  (let ((setvar "\\`\\([A-Za-z_][A-Za-z0-9_]*\\)=\\(.*\\)\\'")
	(command (eshell-stringify eshell-last-command-name))
	(args eshell-last-arguments))
    ;; local variable settings (such as 'CFLAGS=-O2 make') are handled
    ;; by making the whole command into a subcommand, and calling
    ;; setenv immediately before the command is invoked.  This means
    ;; that 'BLAH=x cd blah' won't work exactly as expected, but that
    ;; is by no means a typical use of local environment variables.
    (if (and command (string-match setvar command))
	(throw
	 'eshell-replace-command
	 (list
	  'eshell-as-subcommand
	  (append
	   (list 'progn)
	   (let ((l (list t)))
	     (while (string-match setvar command)
	       (nconc
		l (list
                   (list 'eshell-set-variable
                         (match-string 1 command)
                         (match-string 2 command))))
	       (setq command (eshell-stringify (car args))
		     args (cdr args)))
	     (cdr l))
	   (list (list 'eshell-named-command
		       command (list 'quote args)))))))))

(defun eshell-interpolate-variable ()
  "Parse a variable interpolation.
This function is explicit for adding to `eshell-parse-argument-hook'."
  (when (and (eq (char-after) ?$)
	     (/= (1+ (point)) (point-max)))
    (forward-char)
    (list 'eshell-escape-arg
	  (eshell-parse-variable))))

(defun eshell/define (var-alias definition)
  "Define a VAR-ALIAS using DEFINITION."
  ;; FIXME: This function doesn't work (it produces variable aliases
  ;; in a form not recognized by other parts of the code), and likely
  ;; hasn't worked since before its introduction into Emacs.  It
  ;; should either be removed or fixed up.
  (declare (obsolete nil "29.1"))
  (if (not definition)
      (setq eshell-variable-aliases-list
	    (delq (assoc var-alias eshell-variable-aliases-list)
		  eshell-variable-aliases-list))
    (let ((def (assoc var-alias eshell-variable-aliases-list))
	  (alias-def
	   (list var-alias
		 (list 'quote (if (= (length definition) 1)
				  (car definition)
				definition)))))
      (if def
	  (setq eshell-variable-aliases-list
		(delq (assoc var-alias eshell-variable-aliases-list)
		      eshell-variable-aliases-list)))
      (setq eshell-variable-aliases-list
	    (cons alias-def
		  eshell-variable-aliases-list))))
  nil)

(defun eshell/export (&rest sets)
  "This alias allows the `export' command to act as bash users expect."
  (dolist (set sets)
    (when (and (stringp set)
               (string-match "^\\([^=]+\\)=\\(.*\\)" set))
      (eshell-set-variable (match-string 1 set)
                           (match-string 2 set)))))

(defun pcomplete/eshell-mode/export ()
  "Completion function for Eshell's `export'."
  (while (pcomplete-here
	  (if eshell-complete-export-definition
	      process-environment
	    (eshell-envvar-names)))))

(defun eshell/unset (&rest args)
  "Unset one or more variables.
This is equivalent to calling `eshell/set' for all of ARGS with
the values of nil for each."
  (dolist (arg args)
    (eshell-set-variable arg nil)))

(defun pcomplete/eshell-mode/unset ()
  "Completion function for Eshell's `unset'."
  (while (pcomplete-here (eshell-envvar-names))))

(defun eshell/set (&rest args)
  "Allow command-ish use of `set'."
  (let (last-value)
    (while args
      (setq last-value (eshell-set-variable (car args) (cadr args))
            args (cddr args)))
    last-value))

(defun pcomplete/eshell-mode/set ()
  "Completion function for Eshell's `set'."
  (while (pcomplete-here (eshell-envvar-names))))

(defun eshell/setq (&rest args)
  "Allow command-ish use of `setq'."
  (let (last-value)
    (while args
      (let ((sym (intern (car args)))
	    (val (cadr args)))
	(setq last-value (set sym val)
	      args (cddr args))))
    last-value))

(defun pcomplete/eshell-mode/setq ()
  "Completion function for Eshell's `setq'."
  (while (and (pcomplete-here (all-completions pcomplete-stub
					       obarray #'boundp))
	      (pcomplete-here))))

;; FIXME the real "env" command does more than this, it runs a program
;; in a modified environment.
(defun eshell/env (&rest args)
  "Implementation of `env' in Lisp."
  (eshell-init-print-buffer)
  (eshell-eval-using-options
   "env" args
   '((?h "help" nil nil "show this usage screen")
     :external "env"
     :usage "<no arguments>")
   (dolist (setting (sort (eshell-environment-variables) 'string-lessp))
     (eshell-buffered-print setting "\n"))
   (eshell-flush)))

(defun eshell-insert-envvar (envvar-name)
  "Insert ENVVAR-NAME into the current buffer at point."
  (interactive
   (list (read-envvar-name "Name of environment variable: " t)))
  (insert-and-inherit "$" envvar-name))

(defun eshell-envvar-names (&optional environment)
  "Return a list of currently visible environment variable names."
  (mapcar (lambda (x)
            (substring x 0 (string-search "=" x)))
	  (or environment process-environment)))

(defun eshell-environment-variables ()
  "Return a `process-environment', fully updated.
This involves setting any variable aliases which affect the
environment, as specified in `eshell-variable-aliases-list'."
  (let ((process-environment (eshell-copy-environment)))
    (dolist (var-alias eshell-variable-aliases-list)
      (if (nth 2 var-alias)
	  (setenv (car var-alias)
		  (eshell-stringify
		   (or (eshell-get-variable (car var-alias)) "")))))
    process-environment))

(defun eshell-parse-variable ()
  "Parse the next variable reference at point.
The variable name could refer to either an environment variable, or a
Lisp variable.  The priority order depends on the setting of
`eshell-prefer-lisp-variables'.

Its purpose is to call `eshell-parse-variable-ref', and then to
process any indices that come after the variable reference."
  (let* ((get-len (when (eq (char-after) ?#)
		    (forward-char) t))
	 value indices)
    (setq value (eshell-parse-variable-ref get-len)
	  indices (and (not (eobp))
		       (eq (char-after) ?\[)
		       (eshell-parse-indices))
          ;; This is an expression that will be evaluated by `eshell-do-eval',
          ;; which only support let-binding of dynamically-scoped vars
	  value `(let ((indices (eshell-eval-indices ',indices))) ,value))
    (when get-len
      (setq value `(length ,value)))
    (when eshell-current-quoted
      (setq value `(eshell-stringify ,value)))
    value))

(defun eshell-parse-variable-ref (&optional modifier-p)
  "Eval a variable reference.
Returns a Lisp form which, if evaluated, will return the value of the
variable.

If MODIFIER-P is non-nil, the value of the variable will be
modified by some function.  If MODIFIER-P is nil, the value will be
used as-is; this allows optimization of some kinds of variable
references.

Possible variable references are:

  NAME          an environment or Lisp variable value
  \"LONG-NAME\"   disambiguates the length of the name
  `LONG-NAME'   as above
  {COMMAND}     result of command is variable's value
  (LISP-FORM)   result of Lisp form is variable's value
  <COMMAND>     write the output of command to a temporary file;
                result is the file name"
  (cond
   ((eq (char-after) ?{)
    (let ((end (eshell-find-delimiter ?\{ ?\})))
      (if (not end)
          (throw 'eshell-incomplete ?\{)
        (forward-char)
        (prog1
            `(eshell-apply-indices
              (eshell-convert
               (eshell-command-to-value
                (eshell-as-subcommand
                 ,(let ((subcmd (or (eshell-unescape-inner-double-quote end)
                                    (cons (point) end)))
                        (eshell-current-quoted nil))
                    (eshell-parse-command subcmd))))
               ;; If this is a simple double-quoted form like
               ;; "${COMMAND}" (i.e. no indices after the subcommand
               ;; and no `#' modifier before), ensure we convert to a
               ;; single string.  This avoids unnecessary work
               ;; (e.g. splitting the output by lines) when it would
               ;; just be joined back together afterwards.
               ,(when (and (not modifier-p) eshell-current-quoted)
                  '(not indices)))
              indices ,eshell-current-quoted)
          (goto-char (1+ end))))))
   ((eq (char-after) ?\<)
    (let ((end (eshell-find-delimiter ?\< ?\>)))
      (if (not end)
          (throw 'eshell-incomplete ?\<)
        (let* ((temp (make-temp-file temporary-file-directory))
               (cmd (concat (buffer-substring (1+ (point)) end)
                            " > " temp)))
          (prog1
              `(let ((eshell-current-handles
                      (eshell-create-handles ,temp 'overwrite)))
                 (progn
                   (eshell-as-subcommand
                    ,(let ((eshell-current-quoted nil))
                       (eshell-parse-command cmd)))
                   (ignore
                    (nconc eshell-this-command-hook
                           ;; Quote this lambda; it will be evaluated
                           ;; by `eshell-do-eval', which requires very
                           ;; particular forms in order to work
                           ;; properly.  See bug#54190.
                           (list (function
                                  (lambda ()
                                    (delete-file ,temp)
                                    (when-let ((buffer (get-file-buffer ,temp)))
                                      (kill-buffer buffer)))))))
                   (eshell-apply-indices ,temp indices ,eshell-current-quoted)))
            (goto-char (1+ end)))))))
   ((eq (char-after) ?\()
    (condition-case nil
        `(eshell-apply-indices
          (eshell-command-to-value
           (eshell-lisp-command
            ',(read (or (eshell-unescape-inner-double-quote (point-max))
                        (current-buffer)))))
          indices ,eshell-current-quoted)
      (end-of-file
       (throw 'eshell-incomplete ?\())))
   ((looking-at (rx-to-string
                 `(or "'" ,(if eshell-current-quoted "\\\"" "\""))))
    (eshell-with-temp-command
        (or (eshell-unescape-inner-double-quote (point-max))
            (cons (point) (point-max)))
      (let ((name (if (eq (char-after) ?\')
                      (eshell-parse-literal-quote)
                    (eshell-parse-double-quote))))
        (when name
          `(eshell-get-variable ,(eval name) indices ,eshell-current-quoted)))))
   ((assoc (char-to-string (char-after))
           eshell-variable-aliases-list)
    (forward-char)
    `(eshell-get-variable ,(char-to-string (char-before)) indices
                          ,eshell-current-quoted))
   ((looking-at eshell-variable-name-regexp)
    (prog1
        `(eshell-get-variable ,(match-string 0) indices ,eshell-current-quoted)
      (goto-char (match-end 0))))
   (t
    (error "Invalid variable reference"))))

(defvar eshell-glob-function)

(defun eshell-parse-indices ()
  "Parse and return a list of index-lists.

For example, \"[0 1][2]\" becomes:
  ((\"0\" \"1\") (\"2\")."
  (let (indices)
    (while (eq (char-after) ?\[)
      (let ((end (eshell-find-delimiter ?\[ ?\])))
	(if (not end)
	    (throw 'eshell-incomplete ?\[)
	  (forward-char)
          (eshell-with-temp-command (or (eshell-unescape-inner-double-quote end)
                                        (cons (point) end))
	    (let (eshell-glob-function (eshell-current-quoted nil))
	      (setq indices (cons (eshell-parse-arguments
                                   (point-min) (point-max))
				  indices))))
	  (goto-char (1+ end)))))
    (nreverse indices)))

(defun eshell-eval-indices (indices)
  "Evaluate INDICES, a list of index-lists generated by `eshell-parse-indices'."
  (mapcar (lambda (i) (mapcar #'eval i)) indices))

(defun eshell-get-variable (name &optional indices quoted)
  "Get the value for the variable NAME.
INDICES is a list of index-lists (see `eshell-parse-indices').
If QUOTED is non-nil, this was invoked inside double-quotes."
  (if-let ((alias (assoc name eshell-variable-aliases-list)))
      (let ((target (nth 1 alias)))
        (when (and (not (functionp target))
                   (consp target))
          (setq target (car target)))
        (cond
         ((functionp target)
          (if (nth 3 alias)
              (eshell-apply-indices (funcall target) indices quoted)
            (let ((max-arity (cdr (func-arity target))))
              (if (or (eq max-arity 'many) (>= max-arity 2))
                  (funcall target indices quoted)
                (display-warning
                 '(eshell variable-alias)
                 (concat "Function for `eshell-variable-aliases-list' "
                         "entry should accept two arguments: INDICES "
                         "and QUOTED.'"))
                (funcall target indices)))))
         ((symbolp target)
          (eshell-apply-indices (symbol-value target) indices quoted))
         (t
          (eshell-get-variable target indices quoted))))
    (unless (stringp name)
      (error "Unknown variable `%s'" (eshell-stringify name)))
    (eshell-apply-indices
     (let ((sym (intern-soft name)))
       (if (and sym (boundp sym)
		(or eshell-prefer-lisp-variables
		    (memq sym eshell--local-vars) ; bug#15372
		    (not (getenv name))))
	   (symbol-value sym)
	 (getenv name)))
     indices quoted)))

(defun eshell-set-variable (name value)
  "Set the variable named NAME to VALUE.
NAME can be a string (in which case it refers to an environment
variable or variable alias) or a symbol (in which case it refers
to a Lisp variable)."
  (if-let ((alias (assoc name eshell-variable-aliases-list)))
      (let ((target (nth 1 alias)))
        (cond
         ((functionp target)
          (setq target nil))
         ((consp target)
          (setq target (cdr target))))
        (cond
         ((functionp target)
          (funcall target nil value))
         ((null target)
          (unless eshell-in-subcommand-p
            (error "Variable `%s' is not settable" (eshell-stringify name)))
          (push `(,name ,(lambda () value) t t)
                eshell-variable-aliases-list)
          value)
         ;; Since getting a variable alias with a string target and
         ;; `eshell-prefer-lisp-variables' non-nil gets the
         ;; corresponding Lisp variable, make sure setting does the
         ;; same.
         ((and eshell-prefer-lisp-variables
               (stringp target))
          (eshell-set-variable (intern target) value))
         (t
          (eshell-set-variable target value))))
    (cond
     ((stringp name)
      (setenv name value))
     ((symbolp name)
      (set name value))
     (t
      (error "Unknown variable `%s'" (eshell-stringify name))))))

(defun eshell-apply-indices (value indices &optional quoted)
  "Apply to VALUE all of the given INDICES, returning the sub-result.
The format of INDICES is:

  ((INT-OR-NAME-OR-OTHER INT-OR-NAME INT-OR-NAME ...)
   ...)

Each member of INDICES represents a level of nesting.  If the first
member of a sublist is not an integer or name, and the value it's
referencing is a string, that will be used as the regexp with which
is to divide the string into sub-parts.  The default is whitespace.
Otherwise, each INT-OR-NAME refers to an element of the list value.
Integers imply a direct index, and names, an associate lookup using
`assoc'.

If QUOTED is non-nil, this was invoked inside double-quotes.
This affects the behavior of splitting strings: without quoting,
the split values are converted to numbers via
`eshell-convert-to-number' if possible; with quoting, they're
left as strings.

For example, to retrieve the second element of a user's record in
'/etc/passwd', the variable reference would look like:

  ${grep johnw /etc/passwd}[: 2]"
  (while indices
    (let ((refs (car indices)))
      (when (stringp value)
	(let (separator (index (caar indices)))
          (when (and (stringp index)
                     (not (get-text-property 0 'number index)))
            (setq separator index
                  refs (cdr refs)))
	  (setq value (split-string value separator))
          (unless quoted
            (setq value (mapcar #'eshell-convert-to-number value)))))
      (cond
       ((< (length refs) 0)
	(error "Invalid array variable index: %s"
	       (eshell-stringify refs)))
       ((= (length refs) 1)
	(setq value (eshell-index-value value (car refs))))
       (t
	(let ((new-value (list t)))
	  (while refs
	    (nconc new-value
		   (list (eshell-index-value value
					     (car refs))))
	    (setq refs (cdr refs)))
	  (setq value (cdr new-value))))))
    (setq indices (cdr indices)))
  value)

(defun eshell-index-value (value index)
  "Reference VALUE using the given INDEX."
  (when (and (stringp index) (get-text-property 0 'number index))
    (setq index (string-to-number index)))
  (if (integerp index)
      (cond
       ((ring-p value)
        (if (> index (ring-length value))
            (error "Index exceeds length of ring")
          (ring-ref value index)))
       ((listp value)
        (if (> index (length value))
            (error "Index exceeds length of list")
          (nth index value)))
       ((vectorp value)
        (if (> index (length value))
            (error "Index exceeds length of vector")
          (aref value index)))
       (t
        (error "Invalid data type for indexing")))
    ;; INDEX is some non-integer value, so treat VALUE as an alist.
    (cdr (assoc index value))))

;;;_* Variable name completion

(defun eshell-complete-variable-reference ()
  "If there is a variable reference, complete it."
  (let ((arg (pcomplete-actual-arg)) index)
    (when (setq index
		(string-match
		 (concat "\\$\\(" eshell-variable-name-regexp
			 "\\)?\\'") arg))
      (setq pcomplete-stub (substring arg (1+ index)))
      (throw 'pcomplete-completions (eshell-variables-list)))))

(defun eshell-variables-list ()
  "Generate list of applicable variables."
  (let ((argname pcomplete-stub)
	completions)
    (dolist (alias eshell-variable-aliases-list)
      (if (string-match (concat "^" argname) (car alias))
	  (setq completions (cons (car alias) completions))))
    (sort
     (append
      (mapcar
       (lambda (varname)
         (let ((value (eshell-get-variable varname)))
           (if (and value
                    (stringp value)
                    (file-directory-p value))
               (concat varname "/")
             varname)))
       (eshell-envvar-names (eshell-environment-variables)))
      (all-completions argname obarray 'boundp)
      completions)
     'string-lessp)))

(defun eshell-complete-variable-assignment ()
  "If there is a variable assignment, allow completion of entries."
  (let ((arg (pcomplete-actual-arg)) pos)
    (when (string-match (concat "\\`" eshell-variable-name-regexp "=") arg)
      (setq pos (match-end 0))
      (if (string-match "\\(:\\)[^:]*\\'" arg)
	  (setq pos (match-end 1)))
      (setq pcomplete-stub (substring arg pos))
      (throw 'pcomplete-completions (pcomplete-entries)))))

(provide 'esh-var)
;;; esh-var.el ends here
