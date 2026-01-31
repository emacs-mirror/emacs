;;; subr.el --- basic lisp subroutines for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 1985-2026 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: internal
;; Package: emacs

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

;;; Code:

;; declare-function's args use &rest, not &optional, for compatibility
;; with byte-compile-macroexpand-declare-function.

(defmacro declare-function (_fn _file &rest _args)
  "Tell the byte-compiler that function FN is defined, in FILE.
The FILE argument is not used by the byte-compiler, but by the
`check-declare' package, which checks that FILE contains a
definition for FN.  (FILE can be nil, and that disables this
check.)

FILE can be either a Lisp file (in which case the \".el\"
extension is optional), or a C file.  C files are expanded
relative to the Emacs \"src/\" directory.  Lisp files are
searched for using `locate-library', and if that fails they are
expanded relative to the location of the file containing the
declaration.  A FILE with an \"ext:\" prefix is an external file.
`check-declare' will check such files if they are found, and skip
them without error if they are not.

Optional ARGLIST specifies FN's arguments, in the same form as
in `defun' (including the parentheses); or it is t to not specify
FN's arguments.  An omitted ARGLIST defaults to t, not nil: a nil
ARGLIST specifies an empty argument list, and an explicit t
ARGLIST is a placeholder that allows supplying a later arg.

Optional FILEONLY non-nil means that `check-declare' will check
only that FILE exists, not that it defines FN.  This is intended
for function definitions that `check-declare' does not recognize,
e.g., `defstruct'.

Note that for the purposes of `check-declare', this statement
must be the first non-whitespace on a line.

For more information, see Info node `(elisp)Declaring Functions'."
  (declare (advertised-calling-convention
	    (fn file &optional arglist fileonly) nil))
  ;; Does nothing - `byte-compile-macroexpand-declare-function' does
  ;; the work.
  nil)


;;;; Basic Lisp macros.

(defalias 'not #'null)
(defalias 'sxhash #'sxhash-equal)

(defmacro noreturn (form)
  "Evaluate FORM, expecting it not to return.
If FORM does return, signal an error."
  (declare (debug t))
  `(prog1 ,form
     (error "Form marked with `noreturn' did return")))

(defmacro 1value (form)
  "Evaluate FORM, expecting a constant return value.
If FORM returns differing values when running under Testcover,
Testcover will raise an error."
  (declare (debug t))
  form)

(defmacro def-edebug-spec (symbol spec)
  "Set the Edebug SPEC to use for sexps which have SYMBOL as head.
Both SYMBOL and SPEC are unevaluated.  The SPEC can be:
0 (instrument no arguments); t (instrument all arguments);
a symbol (naming a function with an Edebug specification); or a list.
The elements of the list describe the argument types; see
Info node `(elisp)Specification List' for details."
  (declare (indent 1))
  `(put (quote ,symbol) 'edebug-form-spec (quote ,spec)))

(defun def-edebug-elem-spec (name spec)
  "Define a new Edebug spec element NAME as shorthand for SPEC.
The SPEC has to be a list."
  (declare (indent 1))
  (when (string-match "\\`[&:]" (symbol-name name))
    ;; & and : have special meaning in spec element names.
    (error "Edebug spec name cannot start with '&' or ':'"))
  (unless (consp spec)
    (error "Edebug spec has to be a list: %S" spec))
  (put name 'edebug-elem-spec spec))


(defmacro lambda (&rest cdr)
  "Return an anonymous function.
Under dynamic binding, a call of the form (lambda ARGS DOCSTRING
INTERACTIVE BODY) is self-quoting; the result of evaluating the
lambda expression is the expression itself.  Under lexical
binding, the result is a closure.  Regardless, the result is a
function, i.e., it may be stored as the function value of a
symbol, passed to `funcall' or `mapcar', etc.

ARGS should take the same form as an argument list for a `defun'.
DOCSTRING is an optional documentation string.
 If present, it should describe how to call the function.
 But documentation strings are usually not useful in nameless functions.
INTERACTIVE should be a call to the function `interactive', which see.
It may also be omitted.
BODY should be a list of Lisp expressions.

\(fn ARGS [DOCSTRING] [INTERACTIVE] BODY)"
  (declare (doc-string 2) (indent defun)
           (debug (&define lambda-list lambda-doc
                           [&optional ("interactive" interactive)]
                           def-body)))
  ;; Note that this definition should not use backquotes; subr.el should not
  ;; depend on backquote.el.
  (list 'function (cons 'lambda cdr)))

(defmacro prog2 (form1 form2 &rest body)
  "Eval FORM1, FORM2 and BODY sequentially; return value from FORM2.
The value of FORM2 is saved during the evaluation of the
remaining args, whose values are discarded."
  (declare (indent 2) (debug t))
  `(progn ,form1 (prog1 ,form2 ,@body)))

(defmacro setq-default (&rest args)
  "Set the default value of variable VAR to VALUE.
VAR, the variable name, is literal (not evaluated);
VALUE is an expression: it is evaluated and its value returned.
The default value of a variable is seen in buffers
that do not have their own values for the variable.

More generally, you can use multiple variables and values, as in
  (setq-default VAR VALUE VAR VALUE...)
This sets each VAR's default value to the corresponding VALUE.
The VALUE for the Nth VAR can refer to the new default values
of previous VARs.

\(fn [VAR VALUE]...)"
  (declare (debug setq))
  (let ((exps nil))
    (while args
      (push `(set-default ',(pop args) ,(pop args)) exps))
    `(progn . ,(nreverse exps))))

(defmacro setq-local (&rest pairs)
  "Make each VARIABLE local to current buffer and set it to corresponding VALUE.

The arguments are variable/value pairs.  For each VARIABLE in a pair,
make VARIABLE buffer-local in the current buffer and assign to it the
corresponding VALUE of the pair.  The VARIABLEs are literal symbols
and should not be quoted.

The VALUE of the Nth pair is not computed until after the VARIABLE
of the (N-1)th pair is set; thus, each VALUE can use the new VALUEs
of VARIABLEs set by earlier pairs.

The return value of the `setq-local' form is the VALUE of the last
pair.

In some corner cases you may need to resort to
`set-buffer-local-toplevel-value' instead, which see.

\(fn [VARIABLE VALUE]...)"
  (declare (debug setq))
  (unless (evenp (length pairs))
    (error "PAIRS must have an even number of variable/value members"))
  (let ((expr nil))
    (while pairs
      (unless (symbolp (car pairs))
        (error "Attempting to set a non-symbol: %s" (car pairs)))
      ;; Can't use backquote here, it's too early in the bootstrap.
      (setq expr
            (cons
             (list 'setq (car pairs)
                   (list 'prog1
                    (car (cdr pairs))
                    (list 'make-local-variable (list 'quote (car pairs)))))
             expr))
      (setq pairs (cdr (cdr pairs))))
    (macroexp-progn (nreverse expr))))

(defmacro defvar-local (symbol &rest args)
  "Define SYMBOL as a buffer-local variable with default value VALUE.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set.
\n(fn SYMBOL &optional VALUE DOCSTRING)"
  (declare (debug defvar) (doc-string 3) (indent defun))
  ;; Can't use backquote here, it's too early in the bootstrap.
  (let ((value (car-safe args))
        (docstring (car-safe (cdr-safe args))))
    (list 'progn
          (if (zerop (length args))
              (list 'defvar symbol)
            (list 'defvar symbol value docstring))
          (list 'make-variable-buffer-local (list 'quote symbol)))))

(defun buffer-local-boundp (symbol buffer)
  "Return non-nil if SYMBOL is bound in BUFFER.
Also see `local-variable-p'."
  (declare (side-effect-free t))
  (condition-case nil
      (buffer-local-value symbol buffer)
    (:success t)
    (void-variable nil)))

(defmacro buffer-local-set-state (&rest pairs)
  "Like `setq-local', but allow restoring the previous state of locals later.
This macro returns an object that can be passed to `buffer-local-restore-state'
in order to restore the state of the local variables set via this macro.

\(fn [VARIABLE VALUE]...)"
  (declare (debug setq))
  (unless (evenp (length pairs))
    (error "PAIRS must have an even number of variable/value members"))
  (let ((vars nil)
        (tmp pairs))
    (while tmp (push (car tmp) vars) (setq tmp (cddr tmp)))
    (setq vars (nreverse vars))
    `(prog1
         (buffer-local-set-state--get ',vars)
       (setq-local ,@pairs))))

(defun buffer-local-set-state--get (vars)
  (let ((states nil))
    (dolist (var vars)
      (push (list var
                  (and (boundp var)
                       (local-variable-p var))
                  (and (boundp var)
                       (symbol-value var)))
            states))
    (nreverse states)))

(defun buffer-local-restore-state (states)
  "Restore values of buffer-local variables recorded in STATES.
STATES should be an object returned by `buffer-local-set-state'."
  (pcase-dolist (`(,variable ,local ,value) states)
    (if local
        (set variable value)
      (kill-local-variable variable))))

(defmacro push (newelt place)
  "Add NEWELT to the list stored in the generalized variable PLACE.

This is morally equivalent to (setf PLACE (cons NEWELT PLACE)),
except that PLACE is evaluated only once (after NEWELT).

For more information about generalized variables, see Info node
`(elisp) Generalized Variables'."
  (declare (debug (form gv-place)))
  (if (symbolp place)
      ;; Important special case, to avoid triggering GV too early in
      ;; the bootstrap.
      (list 'setq place
            (list 'cons newelt place))
    (require 'macroexp)
    (macroexp-let2 macroexp-copyable-p x newelt
      (gv-letplace (getter setter) place
        (funcall setter `(cons ,x ,getter))))))

(defmacro pop (place)
  "Return the first element of PLACE's value, and remove it from the list.

PLACE must be a generalized variable whose value is a list.
If the value is nil, `pop' returns nil but does not actually
change the list.

For more information about generalized variables, see Info node
`(elisp) Generalized Variables'."
  (declare (debug (gv-place)))
  ;; We use `car-safe' here instead of `car' because the behavior is the same
  ;; (if it's not a cons cell, the `cdr' would have signaled an error already),
  ;; but `car-safe' is total, so the byte-compiler can safely remove it if the
  ;; result is not used.
  `(car-safe
    ,(if (symbolp place)
         ;; So we can use `pop' in the bootstrap before `gv' can be used.
         (list 'prog1 place (list 'setq place (list 'cdr place)))
       (gv-letplace (getter setter) place
         (macroexp-let2 macroexp-copyable-p x getter
           `(prog1 ,x ,(funcall setter `(cdr ,x))))))))

;; Note: `static-if' can be copied into a package to enable it to be
;; used in Emacsen older than Emacs 30.1.  If the package is used in
;; very old Emacsen or XEmacs (in which `eval' takes exactly one
;; argument) the copy will need amending.
(defmacro static-if (condition then-form &rest else-forms)
  "A conditional compilation macro.
Evaluate CONDITION at macro-expansion time.  If it is non-nil,
expand the macro to THEN-FORM.  Otherwise expand it to ELSE-FORMS
enclosed in a `progn' form.  ELSE-FORMS may be empty."
  (declare (indent 2)
           (debug (sexp sexp &rest sexp)))
  (if (eval condition lexical-binding)
      then-form
    (cons 'progn else-forms)))

(defmacro when (cond &rest body)
  "If COND yields non-nil, do BODY, else return nil.
When COND yields non-nil, eval BODY forms sequentially and return
value of last one, or nil if there are none."
  (declare (indent 1) (debug t))
  (if body
      (list 'if cond (cons 'progn body))
    (macroexp-warn-and-return (format-message "`when' with empty body")
                              (list 'progn cond nil) '(empty-body when) t)))

(defmacro static-when (condition &rest body)
  "A conditional compilation macro.
Evaluate CONDITION at macro-expansion time.  If it is non-nil,
expand the macro to evaluate all BODY forms sequentially and return
the value of the last one, or nil if there are none."
  (declare (indent 1) (debug t))
  (if body
      (if (eval condition lexical-binding)
          (cons 'progn body)
        nil)
    (macroexp-warn-and-return (format-message "`static-when' with empty body")
                              nil '(empty-body static-when) t
                              condition)))

(defmacro unless (cond &rest body)
  "If COND yields nil, do BODY, else return nil.
When COND yields nil, eval BODY forms sequentially and return
value of last one, or nil if there are none."
  (declare (indent 1) (debug t))
  (if body
      (cons 'if (cons cond (cons nil body)))
    (macroexp-warn-and-return (format-message "`unless' with empty body")
                              (list 'progn cond nil) '(empty-body unless) t)))

(defmacro static-unless (condition &rest body)
  "A conditional compilation macro.
Evaluate CONDITION at macro-expansion time.  If it is nil,
expand the macro to evaluate all BODY forms sequentially and return
the value of the last one, or nil if there are none."
  (declare (indent 1) (debug t))
  (if body
      (if (eval condition lexical-binding)
          nil
        (cons 'progn body))
    (macroexp-warn-and-return (format-message "`static-unless' with empty body")
                              (list 'progn nil nil) '(empty-body static-unless) t)))

(defsubst subr-primitive-p (object)
  "Return t if OBJECT is a built-in primitive written in C.
Such objects can be functions or special forms."
  (declare (side-effect-free error-free))
  (and (subrp object)
       (not (native-comp-function-p object))))

(defsubst primitive-function-p (object)
  "Return t if OBJECT is a built-in primitive function.
This excludes special forms, since they are not functions."
  (declare (side-effect-free error-free))
  (and (subrp object)
       (not (or (native-comp-function-p object)
                (eq (cdr (subr-arity object)) 'unevalled)))))

(defsubst xor (cond1 cond2)
  "Return the boolean exclusive-or of COND1 and COND2.
If only one of the arguments is non-nil, return it; otherwise
return nil."
  (declare (pure t) (side-effect-free error-free))
  (cond ((not cond1) cond2)
        ((not cond2) cond1)))

(defmacro dolist (spec &rest body)
  "Loop over a list.
Evaluate BODY with VAR bound to each car from LIST, in turn.
Then evaluate RESULT to get return value, default nil.

\(fn (VAR LIST [RESULT]) BODY...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  (unless (consp spec)
    (signal 'wrong-type-argument (list 'consp spec)))
  (unless (<= 2 (length spec) 3)
    (signal 'wrong-number-of-arguments (list '(2 . 3) (length spec))))
  (let ((tail (make-symbol "tail")))
    `(let ((,tail ,(nth 1 spec)))
       (while ,tail
         (let ((,(car spec) (car ,tail)))
           ,@body
           (setq ,tail (cdr ,tail))))
       ,@(cdr (cdr spec)))))

(defmacro dotimes (spec &rest body)
  "Loop a certain number of times.
Evaluate BODY with VAR bound to successive integers running from 0,
inclusive, to COUNT, exclusive.

Finally RESULT is evaluated to get the return value (nil if
RESULT is omitted).  Using RESULT is deprecated, and may result
in compilation warnings about unused variables.

\(fn (VAR COUNT [RESULT]) BODY...)"
  (declare (indent 1) (debug dolist))
  (let ((var (nth 0 spec))
        (end (nth 1 spec))
        (upper-bound (make-symbol "upper-bound"))
        (counter (make-symbol "counter")))
    `(let ((,upper-bound ,end)
           (,counter 0))
       (while (< ,counter ,upper-bound)
         (let ((,var ,counter))
           ,@body)
         (setq ,counter (1+ ,counter)))
       ,@(if (cddr spec)
             ;; FIXME: This let often leads to "unused var" warnings.
             `((let ((,var ,counter)) ,@(cddr spec)))))))

(defmacro declare (&rest specs)
  "Do not evaluate any arguments, and return nil.
If a `declare' form appears as the first form in the body of a
`defun' or `defmacro' form, SPECS specifies various additional
information about the function or macro; these go into effect
during the evaluation of the `defun' or `defmacro' form.

The possible values of SPECS are specified by
`defun-declarations-alist' and `macro-declarations-alist'.

For more information, see info node `(elisp)Declare Form'."
  ;; `declare' is handled directly by `defun/defmacro' rather than here.
  ;; If we get here, it's because there's a `declare' somewhere not attached
  ;; to a `defun/defmacro', i.e. a `declare' which doesn't do what it's
  ;; intended to do.
  (let ((form `(declare . ,specs)))  ;; FIXME: WIBNI we had &whole?
    (macroexp-warn-and-return
     (format-message "Stray `declare' form: %S" form)
     ;; Make a "unique" harmless form to circumvent
     ;; the cache in `macroexp-warn-and-return'.
     `(progn ',form nil) nil 'compile-only)))

(defmacro ignore-errors (&rest body)
  "Execute BODY; if an error occurs, return nil.
Otherwise, return result of last form in BODY.
See also `with-demoted-errors' that does something similar
without silencing all errors."
  (declare (debug t) (indent 0))
  `(condition-case nil (progn ,@body) (error nil)))

(defmacro ignore-error (condition &rest body)
  "Execute BODY; if the error CONDITION occurs, return nil.
Otherwise, return result of last form in BODY.

CONDITION can also be a list of error conditions.
The CONDITION argument is not evaluated.  Do not quote it."
  (declare (debug t) (indent 1))
  (cond
   ((and (eq (car-safe condition) 'quote)
         (cdr condition) (null (cddr condition)))
    (macroexp-warn-and-return
     (format-message
      "`ignore-error' condition argument should not be quoted: %S"
      condition)
     `(condition-case nil (progn ,@body) (,(cadr condition) nil))
     nil t condition))
   (body
    `(condition-case nil (progn ,@body) (,condition nil)))
   (t
    (macroexp-warn-and-return (format-message "`ignore-error' with empty body")
                              nil '(empty-body ignore-error) t condition))))


;;;; Basic Lisp functions.

(defvar gensym-counter 0
  "Number used to construct the name of the next symbol created by `gensym'.")

(defun gensym (&optional prefix)
  "Return a new uninterned symbol.
The name is made by appending `gensym-counter' to PREFIX.
PREFIX is a string, and defaults to \"g\"."
  (declare (important-return-value t))
  (let ((num (prog1 gensym-counter
               (setq gensym-counter (1+ gensym-counter)))))
    (make-symbol (format "%s%d" (or prefix "g") num))))

(defun ignore (&rest _arguments)
  "Ignore ARGUMENTS, do nothing, and return nil.
This function accepts any number of arguments in ARGUMENTS.
Also see `always'."
  ;; Not declared `side-effect-free' because we don't want calls to it
  ;; elided; see `byte-compile-ignore'.
  (declare (ftype (function (&rest t) null))
           (pure t) (completion ignore))
  (interactive)
  nil)

(defun always (&rest _arguments)
  "Ignore ARGUMENTS, do nothing, and return t.
This function accepts any number of arguments in ARGUMENTS.
Also see `ignore'."
  (declare (pure t) (side-effect-free error-free))
  t)

(defun error (string &rest args)
  "Signal an error, making a message by passing ARGS to `format-message'.
Errors cause entry to the debugger when `debug-on-error' is non-nil.
This can be overridden by `debug-ignored-errors'.

When `noninteractive' is non-nil (in particular, in batch mode), an
unhandled error calls `kill-emacs', which terminates the Emacs
session with a non-zero exit code.

To signal with MESSAGE without interpreting format characters
like `%', `\\=`' and `\\='', use (error \"%s\" MESSAGE).
In Emacs, the convention is that error messages start with a capital
letter but *do not* end with a period.  Please follow this convention
for the sake of consistency.

To alter the look of the displayed error messages, you can use
the `command-error-function' variable."
  (declare (ftype (function (string &rest t) nil)))
  (signal 'error (list (apply #'format-message string args))))

(defun user-error (format &rest args)
  "Signal a user error, making a message by passing ARGS to `format-message'.
This is like `error' except that a user error (or \"pilot error\") comes
from an incorrect manipulation by the user, not from an actual problem.
In contrast with other errors, user errors normally do not cause
entry to the debugger, even when `debug-on-error' is non-nil.
This can be overridden by `debug-ignored-errors'.

To signal with MESSAGE without interpreting format characters
like `%', `\\=`' and `\\='', use (user-error \"%s\" MESSAGE).
In Emacs, the convention is that error messages start with a capital
letter but *do not* end with a period.  Please follow this convention
for the sake of consistency.

To alter the look of the displayed error messages, you can use
the `command-error-function' variable."
  (signal 'user-error (list (apply #'format-message format args))))

(defun define-error (name message &optional parent)
  "Define NAME as a new error signal.
MESSAGE is a string that will be output to the echo area if such an error
is signaled without being caught by a `condition-case'.
PARENT is either a signal or a list of signals from which it inherits.
Defaults to `error'."
  (unless parent (setq parent 'error))
  (let ((conditions
         (if (consp parent)
             (apply #'append
                    (mapcar (lambda (parent)
                              (cons parent
                                    (or (get parent 'error-conditions)
                                        (error "Unknown signal `%s'" parent))))
                            parent))
           (cons parent (get parent 'error-conditions)))))
    (put name 'error-conditions
         (delete-dups (copy-sequence (cons name conditions))))
    (when message (put name 'error-message message))))

;; We put this here instead of in frame.el so that it's defined even on
;; systems where frame.el isn't loaded.
(defun frame-configuration-p (object)
  "Return non-nil if OBJECT seems to be a frame configuration.
Any list whose car is `frame-configuration' is assumed to be a frame
configuration."
  (declare (pure t) (side-effect-free error-free))
  (and (consp object)
       (eq (car object) 'frame-configuration)))

(defun apply-partially (fun &rest args)
  "Return a function that is a partial application of FUN to ARGS.
ARGS is a list of the first N arguments to pass to FUN.
The result is a new function which does the same as FUN, except that
the first N arguments are fixed at the values with which this function
was called.

In almost all cases, you want to use a regular anonymous function
defined with `lambda' instead.  It will be faster, because it does not
have the overhead of calling `apply' and `append', which this function
has to do internally."
  (declare (side-effect-free error-free))
  (lambda (&rest args2)
    (apply fun (append args args2))))

(defun zerop (number)
  "Return t if NUMBER is zero."
  ;; Used to be in C, but it's pointless since (= 0 n) is faster anyway because
  ;; = has a byte-code.
  (declare (ftype (function (number) boolean))
           (pure t) (side-effect-free t)
           (compiler-macro (lambda (_) `(= 0 ,number))))
  (= 0 number))

(defun plusp (number)
  "Return t if NUMBER is positive."
  (declare (ftype (function (number) boolean))
           (side-effect-free t)
           (compiler-macro (lambda (_) `(> ,number 0))))
  (> number 0))

(defun minusp (number)
  "Return t if NUMBER is negative."
  (declare (ftype (function (number) boolean))
           (side-effect-free t)
           (compiler-macro (lambda (_) `(< ,number 0))))
  (< number 0))

(defun oddp (integer)
  "Return t if INTEGER is odd."
  (declare (ftype (function (integer) boolean))
           (pure t) (side-effect-free t)
           (compiler-macro (lambda (_) `(not (eq (% ,integer 2) 0)))))
  (not (eq (% integer 2) 0)))

(defun evenp (integer)
  "Return t if INTEGER is even."
  (declare (ftype (function (integer) boolean))
           (pure t) (side-effect-free t)
           (compiler-macro (lambda (_) `(eq (% ,integer 2) 0))))
  (eq (% integer 2) 0))

(defun fixnump (object)
  "Return t if OBJECT is a fixnum."
  (declare (ftype (function (t) boolean))
           (side-effect-free error-free))
  (and (integerp object)
       (<= most-negative-fixnum object most-positive-fixnum)))

(defun bignump (object)
  "Return t if OBJECT is a bignum."
  (declare (ftype (function (t) boolean))
           (side-effect-free error-free))
  (and (integerp object) (not (fixnump object))))

(defun lsh (value count)
  "Return VALUE with its bits shifted left by COUNT.
If COUNT is negative, shifting is actually to the right.
In this case, if VALUE is a negative fixnum treat it as unsigned,
i.e., subtract 2 * `most-negative-fixnum' from VALUE before shifting it.

Most uses of this function turn out to be mistakes.  We recommend using
`ash' instead, unless COUNT could ever be negative, in which case your
program should only use this function if it specifically requires the
special handling of negative COUNT."
  (declare (ftype (function (integer integer) integer))
           (compiler-macro
            (lambda (form)
              (macroexp-warn-and-return
               (format-message "avoid `lsh'; use `ash' instead")
               form '(suspicious lsh) t form)))
           (side-effect-free t))
  (when (and (minusp value) (minusp count))
    (when (< value most-negative-fixnum)
      (signal 'args-out-of-range (list value count)))
    (setq value (logand (ash value -1) most-positive-fixnum))
    (setq count (1+ count)))
  (ash value count))


;;;; List functions.

(defun internal--compiler-macro-cXXr (form x)
  (let* ((head (car form))
         (n (symbol-name head))
         (i (- (length n) 2)))
    (if (not (string-match "c[ad]+r\\'" n))
        (if (and (fboundp head) (symbolp (symbol-function head)))
            (internal--compiler-macro-cXXr
             (cons (symbol-function head) (cdr form)) x)
          (error "Compiler macro for cXXr applied to non-cXXr form"))
      (while (> i (match-beginning 0))
        (setq x (list (if (eq (aref n i) ?a) 'car 'cdr) x))
        (setq i (1- i)))
      x)))

(defun caar (x)
  "Return the car of the car of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (car (car x)))

(defun cadr (x)
  "Return the car of the cdr of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (car (cdr x)))

(defun cdar (x)
  "Return the cdr of the car of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (cdr (car x)))

(defun cddr (x)
  "Return the cdr of the cdr of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (cdr (cdr x)))

(defun caaar (x)
  "Return the `car' of the `car' of the `car' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (car (car (car x))))

(defun caadr (x)
  "Return the `car' of the `car' of the `cdr' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (car (car (cdr x))))

(defun cadar (x)
  "Return the `car' of the `cdr' of the `car' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (car (cdr (car x))))

(defun caddr (x)
  "Return the `car' of the `cdr' of the `cdr' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (car (cdr (cdr x))))

(defun cdaar (x)
  "Return the `cdr' of the `car' of the `car' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (cdr (car (car x))))

(defun cdadr (x)
  "Return the `cdr' of the `car' of the `cdr' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (cdr (car (cdr x))))

(defun cddar (x)
  "Return the `cdr' of the `cdr' of the `car' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (cdr (cdr (car x))))

(defun cdddr (x)
  "Return the `cdr' of the `cdr' of the `cdr' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (cdr (cdr (cdr x))))

(defun caaaar (x)
  "Return the `car' of the `car' of the `car' of the `car' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (car (car (car (car x)))))

(defun caaadr (x)
  "Return the `car' of the `car' of the `car' of the `cdr' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (car (car (car (cdr x)))))

(defun caadar (x)
  "Return the `car' of the `car' of the `cdr' of the `car' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (car (car (cdr (car x)))))

(defun caaddr (x)
  "Return the `car' of the `car' of the `cdr' of the `cdr' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (car (car (cdr (cdr x)))))

(defun cadaar (x)
  "Return the `car' of the `cdr' of the `car' of the `car' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (car (cdr (car (car x)))))

(defun cadadr (x)
  "Return the `car' of the `cdr' of the `car' of the `cdr' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (car (cdr (car (cdr x)))))

(defun caddar (x)
  "Return the `car' of the `cdr' of the `cdr' of the `car' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (car (cdr (cdr (car x)))))

(defun cadddr (x)
  "Return the `car' of the `cdr' of the `cdr' of the `cdr' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (car (cdr (cdr (cdr x)))))

(defun cdaaar (x)
  "Return the `cdr' of the `car' of the `car' of the `car' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (cdr (car (car (car x)))))

(defun cdaadr (x)
  "Return the `cdr' of the `car' of the `car' of the `cdr' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (cdr (car (car (cdr x)))))

(defun cdadar (x)
  "Return the `cdr' of the `car' of the `cdr' of the `car' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (cdr (car (cdr (car x)))))

(defun cdaddr (x)
  "Return the `cdr' of the `car' of the `cdr' of the `cdr' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (cdr (car (cdr (cdr x)))))

(defun cddaar (x)
  "Return the `cdr' of the `cdr' of the `car' of the `car' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (cdr (cdr (car (car x)))))

(defun cddadr (x)
  "Return the `cdr' of the `cdr' of the `car' of the `cdr' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (cdr (cdr (car (cdr x)))))

(defun cdddar (x)
  "Return the `cdr' of the `cdr' of the `cdr' of the `car' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (cdr (cdr (cdr (car x)))))

(defun cddddr (x)
  "Return the `cdr' of the `cdr' of the `cdr' of the `cdr' of X."
  (declare (side-effect-free t)
           (compiler-macro internal--compiler-macro-cXXr))
  (cdr (cdr (cdr (cdr x)))))

(defun last (list &optional n)
  "Return the last link of LIST.  Its car is the last element.
If LIST is nil, return nil.
If N is non-nil, return the Nth-to-last link of LIST.
If N is bigger than the length of LIST, return LIST."
  (declare (ftype (function (list &optional integer) list))
           (pure t) (side-effect-free t))    ; pure up to mutation
  (if n
      (and (>= n 0)
           (let ((m (safe-length list)))
             (if (< n m) (nthcdr (- m n) list) list)))
    (and list
         (nthcdr (1- (safe-length list)) list))))

(defun butlast (list &optional n)
  "Return a copy of LIST with the last N elements removed.
If N is omitted or nil, return a copy of LIST without its last element.
If N is zero or negative, return LIST."
  (declare (side-effect-free t))
  (unless n
    (setq n 1))
  (if (<= n 0)
      list
    (take (- (length list) n) list)))

(defun nbutlast (list &optional n)
  "Modify LIST to remove the last N elements.
If N is omitted or nil, remove the last element."
  (let ((m (length list)))
    (or n (setq n 1))
    (and (< n m)
	 (progn
	   (if (plusp n) (setcdr (nthcdr (- (1- m) n) list) nil))
	   list))))

(defun delete-dups (list)
  "Destructively remove `equal' duplicates from LIST.
Store the result in LIST and return it.  LIST must be a proper list.
Of several `equal' occurrences of an element in LIST, the first
one is kept.  See `seq-uniq' for non-destructive operation."
  (let ((l (length list)))
    (if (> l 100)
        (let ((hash (make-hash-table :test #'equal :size l))
              (tail list) retail)
          (puthash (car list) t hash)
          (while (setq retail (cdr tail))
            (let ((elt (car retail)))
              (if (gethash elt hash)
                  (setcdr tail (cdr retail))
                (puthash elt t hash)
                (setq tail retail)))))
      (let ((tail list))
        (while tail
          (setcdr tail (delete (car tail) (cdr tail)))
          (setq tail (cdr tail))))))
  list)

;; See https://lists.gnu.org/r/emacs-devel/2013-05/msg00204.html
(defun delete-consecutive-dups (list &optional circular)
  "Destructively remove `equal' consecutive duplicates from LIST.
First and last elements are considered consecutive if CIRCULAR is
non-nil.
Of several consecutive `equal' occurrences, the one earliest in
the list is kept."
  (let ((tail list) last)
    (while (cdr tail)
      (if (equal (car tail) (cadr tail))
	  (setcdr tail (cddr tail))
	(setq last tail
	      tail (cdr tail))))
    (if (and circular
	     last
	     (equal (car tail) (car list)))
	(setcdr last nil)))
  list)

(defun number-sequence (from &optional to inc)
  "Return a sequence of numbers from FROM to TO (both inclusive) as a list.
INC is the increment used between numbers in the sequence and defaults to 1.
So, the Nth element of the list is (+ FROM (* N INC)) where N counts from
zero.  TO is included only if there is an N for which TO = FROM + N * INC.
If TO is nil or numerically equal to FROM, return (FROM).
If INC is positive and TO is less than FROM, or INC is negative
and TO is larger than FROM, return nil.
If INC is zero and TO is neither nil nor numerically equal to
FROM, signal an error.

This function is primarily designed for integer arguments.
Nevertheless, FROM, TO and INC can be integer or float.  However,
floating point arithmetic is inexact.  For instance, depending on
the machine, it may quite well happen that
\(number-sequence 0.4 0.6 0.2) returns the one element list (0.4),
whereas (number-sequence 0.4 0.8 0.2) returns a list with three
elements.  Thus, if some of the arguments are floats and one wants
to make sure that TO is included, one may have to explicitly write
TO as (+ FROM (* N INC)) or use a variable whose value was
computed with this exact expression.  Alternatively, you can,
of course, also replace TO with a slightly larger value
\(or a slightly more negative value if INC is negative)."
  (declare (side-effect-free t))
  (if (or (not to) (= from to))
      (list from)
    (or inc (setq inc 1))
    (when (zerop inc) (error "The increment can not be zero"))
    (let (seq (n 0) (next from))
      (if (plusp inc)
          (while (<= next to)
            (setq seq (cons next seq)
                  n (1+ n)
                  next (+ from (* n inc))))
        (while (>= next to)
          (setq seq (cons next seq)
                n (1+ n)
                next (+ from (* n inc)))))
      (nreverse seq))))

(defun copy-tree (tree &optional vectors-and-records)
  "Make a copy of TREE.
If TREE is a cons cell, this recursively copies both its car and its cdr.
Contrast to `copy-sequence', which copies only along the cdrs.
With the second argument VECTORS-AND-RECORDS non-nil, this
traverses and copies vectors and records as well as conses."
  (declare (side-effect-free error-free))
  (if (consp tree)
      (let (result)
	(while (consp tree)
	  (let ((newcar (car tree)))
	    (if (or (consp (car tree))
                    (and vectors-and-records
                         (or (vectorp (car tree)) (recordp (car tree)))))
		(setq newcar (copy-tree (car tree) vectors-and-records)))
	    (push newcar result))
	  (setq tree (cdr tree)))
	(nconc (nreverse result)
               (if (and vectors-and-records (or (vectorp tree) (recordp tree)))
                   (copy-tree tree vectors-and-records)
                 tree)))
    (if (and vectors-and-records (or (vectorp tree) (recordp tree)))
	(let ((i (length (setq tree (copy-sequence tree)))))
	  (while (>= (setq i (1- i)) 0)
	    (aset tree i (copy-tree (aref tree i) vectors-and-records)))
	  tree)
      tree)))


;;;; Various list-search functions.

(defun assoc-default (key alist &optional test default)
  "Find object KEY in a pseudo-alist ALIST.
ALIST is a list of conses or objects.  Each element
 (or the element's car, if it is a cons) is compared with KEY by
 calling TEST, with two arguments: (i) the element or its car,
 and (ii) KEY.
If that is non-nil, the element matches; then `assoc-default'
 returns the element's cdr, if it is a cons, or DEFAULT if the
 element is not a cons.

If no element matches, the value is nil.
If TEST is omitted or nil, `equal' is used."
  (declare (important-return-value t))
  (let (found (tail alist) value)
    (while (and tail (not found))
      (let ((elt (car tail)))
	(when (funcall (or test #'equal) (if (consp elt) (car elt) elt) key)
	  (setq found t value (if (consp elt) (cdr elt) default))))
      (setq tail (cdr tail)))
    value))

(defun member-ignore-case (elt list)
  "Like `member', but ignore differences in case and text representation.
ELT must be a string.  Upper-case and lower-case letters are treated as equal.
Unibyte strings are converted to multibyte for comparison.
Non-strings in LIST are ignored."
  (declare (side-effect-free t))
  (while (and list
	      (not (and (stringp (car list))
			(string-equal-ignore-case elt (car list)))))
    (setq list (cdr list)))
  list)

(defun assoc-delete-all (key alist &optional test)
  "Delete from ALIST all elements whose car is KEY.
Compare keys with TEST.  Defaults to `equal'.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (declare (important-return-value t))
  (unless test (setq test #'equal))
  (while (and (consp (car alist))
	      (funcall test (caar alist) key))
    (setq alist (cdr alist)))
  (let ((tail alist) tail-cdr)
    (while (setq tail-cdr (cdr tail))
      (if (and (consp (car tail-cdr))
	       (funcall test (caar tail-cdr) key))
	  (setcdr tail (cdr tail-cdr))
	(setq tail tail-cdr))))
  alist)

(defun assq-delete-all (key alist)
  "Delete from ALIST all elements whose car is `eq' to KEY.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (declare (important-return-value t))
  (assoc-delete-all key alist #'eq))

(defun rassq-delete-all (value alist)
  "Delete from ALIST all elements whose cdr is `eq' to VALUE.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (declare (important-return-value t))
  (while (and (consp (car alist))
	      (eq (cdr (car alist)) value))
    (setq alist (cdr alist)))
  (let ((tail alist) tail-cdr)
    (while (setq tail-cdr (cdr tail))
      (if (and (consp (car tail-cdr))
	       (eq (cdr (car tail-cdr)) value))
	  (setcdr tail (cdr tail-cdr))
	(setq tail tail-cdr))))
  alist)

(defun alist-get (key alist &optional default remove testfn)
  "Find the first element of ALIST whose `car' equals KEY and return its `cdr'.
If KEY is not found in ALIST, return DEFAULT.
Equality with KEY is tested by TESTFN, defaulting to `eq'.

You can use `alist-get' in \"place expressions\"; i.e., as a
generalized variable.  Doing this will modify an existing
association (more precisely, the first one if multiple exist), or
add a new element to the beginning of ALIST, destructively
modifying the list stored in ALIST.

Example:

   (setq foo \\='((a . 0)))
   (setf (alist-get \\='a foo) 1
         (alist-get \\='b foo) 2)

   foo => ((b . 2) (a . 1))


When using it to set a value, optional argument REMOVE non-nil
means to remove KEY from ALIST if the new value is `eql' to
DEFAULT (more precisely the first found association will be
deleted from the alist).

Example:

  (setq foo \\='((a . 1) (b . 2)))
  (setf (alist-get \\='b foo nil \\='remove) nil)

  foo => ((a . 1))"
  (declare (important-return-value t))
  (ignore remove) ;;Silence byte-compiler.
  (let ((x (if (not testfn)
               (assq key alist)
             (assoc key alist testfn))))
    (if x (cdr x) default)))

(defun remove (elt seq)
  "Return a copy of SEQ with all occurrences of ELT removed.
SEQ must be a list, vector, or string.  The comparison is done with `equal'.
Contrary to `delete', this does not use side-effects, and the argument
SEQ is not modified."
  (declare (side-effect-free t))
  (delete elt (if (nlistp seq)
                  ;; If SEQ isn't a list, there's no need to copy SEQ because
                  ;; `delete' will return a new object.
                  seq
                (copy-sequence seq))))

(defun remq (elt list)
  "Return LIST with all occurrences of ELT removed.
The comparison is done with `eq'.  Contrary to `delq', this does not use
side-effects, and the argument LIST is not modified."
  (declare (side-effect-free t))
  (while (and (eq elt (car list)) (setq list (cdr list))))
  (if (memq elt list)
      (delq elt (copy-sequence list))
    list))

(defun internal--effect-free-fun-arg-p (x)
  (or (symbolp x) (closurep x) (memq (car-safe x) '(function quote))))

(defun take-while (pred list)
  "Return the longest prefix of LIST whose elements satisfy PRED."
  (declare (compiler-macro
            (lambda (_form)
              (let* ((tail (make-symbol "tail"))
                     (pred (macroexpand-all pred macroexpand-all-environment))
                     (f (and (not (internal--effect-free-fun-arg-p pred))
                             (make-symbol "f")))
                     (r (make-symbol "r")))
                `(let (,@(and f `((,f ,pred)))
                       (,tail ,list)
                       (,r nil))
                   (while (and ,tail (funcall ,(or f pred) (car ,tail)))
                     (push (car ,tail) ,r)
                     (setq ,tail (cdr ,tail)))
                   (nreverse ,r))))))
  (let ((r nil))
    (while (and list (funcall pred (car list)))
      (push (car list) r)
      (setq list (cdr list)))
    (nreverse r)))

(defun drop-while (pred list)
  "Skip initial elements of LIST satisfying PRED and return the rest."
  (declare (compiler-macro
            (lambda (_form)
              (let* ((tail (make-symbol "tail"))
                     (pred (macroexpand-all pred macroexpand-all-environment))
                     (f (and (not (internal--effect-free-fun-arg-p pred))
                             (make-symbol "f"))))
                `(let (,@(and f `((,f ,pred)))
                       (,tail ,list))
                   (while (and ,tail (funcall ,(or f pred) (car ,tail)))
                     (setq ,tail (cdr ,tail)))
                   ,tail)))))
  (while (and list (funcall pred (car list)))
    (setq list (cdr list)))
  list)

(defun all (pred list)
  "Non-nil if PRED is true for all elements in LIST."
  (declare (compiler-macro (lambda (_) `(not (drop-while ,pred ,list)))))
  (not (drop-while pred list)))

(defun any (pred list)
  "Non-nil if PRED is true for at least one element in LIST.
Returns the LIST suffix starting at the first element that satisfies PRED,
or nil if none does."
  (declare (compiler-macro
            (lambda (_)
              `(drop-while (lambda (x) (not (funcall ,pred x))) ,list))))
  (drop-while (lambda (x) (not (funcall pred x))) list))

;;;; Keymap support.

(defun kbd (keys)
  "Convert KEYS to the internal Emacs key representation.
KEYS should be a string in the format returned by commands such
as \\[describe-key] (`describe-key').

This is the same format used for saving keyboard macros (see
`edmacro-mode').

Here's some example key sequences:

    \"f\"
    \"C-c C-c\"
    \"H-<left>\"
    \"M-RET\"
    \"C-M-<return>\"

For an approximate inverse of this, see `key-description'."
  (declare (pure t) (side-effect-free t))
  (let ((res (key-parse keys)))
    ;; For historical reasons, parse "C-x ( C-d C-x )" as "C-d", since
    ;; `kbd' used to be a wrapper around `read-kbd-macro'.
    (when (and (>= (length res) 4)
               (eq (aref res 0) ?\C-x)
               (eq (aref res 1) ?\()
               (eq (aref res (- (length res) 2)) ?\C-x)
               (eq (aref res (- (length res) 1)) ?\)))
      (setq res (apply #'vector (let ((lres (append res nil)))
                                  ;; Remove the first and last two elements.
                                  (setq lres (cddr lres))
                                  (setq lres (nreverse lres))
                                  (setq lres (cddr lres))
                                  (nreverse lres)))))

    (if (not (memq nil (mapcar (lambda (ch)
                                 (and (numberp ch)
                                      (<= 0 ch 127)))
                               res)))
        ;; Return a string.
        (concat (mapcar #'identity res))
      ;; Return a vector.
      res)))

(defun undefined ()
  "Beep to tell the user this binding is undefined."
  (declare (completion ignore))
  (interactive)
  (ding)
  (if defining-kbd-macro
      (error "%s is undefined" (key-description (this-single-command-keys)))
    (message "%s is undefined" (key-description (this-single-command-keys))))
  (force-mode-line-update)
  ;; If this is a down-mouse event, don't reset prefix-arg;
  ;; pass it to the command run by the up event.
  (setq prefix-arg
        (when (memq 'down (event-modifiers last-command-event))
          current-prefix-arg)))

;; Prevent the \{...} documentation construct
;; from mentioning keys that run this command.
(put 'undefined 'suppress-keymap t)

(defun suppress-keymap (map &optional nodigits)
  "Make MAP override all normally self-inserting keys to be undefined.
Normally, as an exception, digits and minus-sign are set to make prefix args,
but optional second arg NODIGITS non-nil treats them like other chars."
  (define-key map [remap self-insert-command] #'undefined)
  (or nodigits
      (let (loop)
	(define-key map "-" #'negative-argument)
	;; Make plain numbers do numeric args.
	(setq loop ?0)
	(while (<= loop ?9)
	  (define-key map (char-to-string loop) #'digit-argument)
	  (setq loop (1+ loop))))))

(defun make-composed-keymap (maps &optional parent)
  "Construct a new keymap composed of MAPS and inheriting from PARENT.
When looking up a key in the returned map, the key is looked in each
keymap of MAPS in turn until a binding is found.
If no binding is found in MAPS, the lookup continues in PARENT, if non-nil.
As always with keymap inheritance, a nil binding in MAPS overrides
any corresponding binding in PARENT, but it does not override corresponding
bindings in other keymaps of MAPS.
MAPS can be a list of keymaps or a single keymap.
PARENT if non-nil should be a keymap."
  (declare (side-effect-free t))
  `(keymap
    ,@(if (keymapp maps) (list maps) maps)
    ,@parent))

(defun define-key-after (keymap key definition &optional after)
  "Add binding in KEYMAP for KEY => DEFINITION, right after AFTER's binding.
This is a legacy function; see `keymap-set-after' for the
recommended function to use instead.

This is like `define-key' except that the binding for KEY is placed
just after the binding for the event AFTER, instead of at the beginning
of the map.  Note that AFTER must be an event type (like KEY), NOT a command
\(like DEFINITION).

If AFTER is t or omitted, the new binding goes at the end of the keymap.
AFTER should be a single event type--a symbol or a character, not a sequence.

Bindings are always added before any inherited map.

The order of bindings in a keymap matters only when it is used as
a menu, so this function is not useful for non-menu keymaps."
  (declare (indent defun))
  (unless after (setq after t))
  (or (keymapp keymap)
      (signal 'wrong-type-argument (list 'keymapp keymap)))
  (setq key
	(if (<= (length key) 1) (aref key 0)
	  (setq keymap (lookup-key keymap
				   (apply #'vector
					  (butlast (mapcar #'identity key)))))
	  (aref key (1- (length key)))))
  (let ((tail keymap) done inserted)
    (while (and (not done) tail)
      ;; Delete any earlier bindings for the same key.
      (if (eq (car-safe (car (cdr tail))) key)
	  (setcdr tail (cdr (cdr tail))))
      ;; If we hit an included map, go down that one.
      (if (keymapp (car tail)) (setq tail (car tail)))
      ;; When we reach AFTER's binding, insert the new binding after.
      ;; If we reach an inherited keymap, insert just before that.
      ;; If we reach the end of this keymap, insert at the end.
      (if (or (and (eq (car-safe (car tail)) after)
		   (not (eq after t)))
	      (eq (car (cdr tail)) 'keymap)
	      (null (cdr tail)))
	  (progn
	    ;; Stop the scan only if we find a parent keymap.
	    ;; Keep going past the inserted element
	    ;; so we can delete any duplications that come later.
	    (if (eq (car (cdr tail)) 'keymap)
		(setq done t))
	    ;; Don't insert more than once.
	    (or inserted
		(setcdr tail (cons (cons key definition) (cdr tail))))
	    (setq inserted t)))
      (setq tail (cdr tail)))))

(defun define-prefix-command (command &optional mapvar name)
  "Define COMMAND as a prefix command.  COMMAND should be a symbol.
A new sparse keymap is stored as COMMAND's function definition and its
value.
This prepares COMMAND for use as a prefix key's binding.
If a second optional argument MAPVAR is given, it should be a symbol.
The map is then stored as MAPVAR's value instead of as COMMAND's
value; but COMMAND is still defined as a function.
The third optional argument NAME, if given, supplies a menu name
string for the map.  This is required to use the keymap as a menu.
This function returns COMMAND."
  (let ((map (make-sparse-keymap name)))
    (fset command map)
    (set (or mapvar command) map)
    command))

(defun map-keymap-sorted (function keymap)
  "Implement `map-keymap' with sorting.
Don't call this function; it is for internal use only."
  (let (list)
    (map-keymap (lambda (a b) (push (cons a b) list))
                keymap)
    (setq list (sort list
                     (lambda (a b)
                       (setq a (car a) b (car b))
                       (if (integerp a)
                           (if (integerp b) (< a b)
                             t)
                         (if (integerp b) t
                           ;; string< also accepts symbols.
                           (string< a b))))))
    (dolist (p list)
      (funcall function (car p) (cdr p)))))

(defun keymap--menu-item-binding (val)
  "Return the binding part of a menu-item."
  (cond
   ((not (consp val)) val)              ;Not a menu-item.
   ((eq 'menu-item (car val))
    (let* ((binding (nth 2 val))
           (plist (nthcdr 3 val))
           (filter (plist-get plist :filter)))
      (if filter (funcall filter binding)
        binding)))
   ((and (consp (cdr val)) (stringp (cadr val)))
    (cddr val))
   ((stringp (car val))
    (cdr val))
   (t val)))                            ;Not a menu-item either.

(defun keymap--menu-item-with-binding (item binding)
  "Build a menu-item like ITEM but with its binding changed to BINDING."
  (cond
   ((not (consp item)) binding)		;Not a menu-item.
   ((eq 'menu-item (car item))
    (setq item (copy-sequence item))
    (let ((tail (nthcdr 2 item)))
      (setcar tail binding)
      ;; Remove any potential filter.
      (if (plist-get (cdr tail) :filter)
          (setcdr tail (plist-put (cdr tail) :filter nil))))
    item)
   ((and (consp (cdr item)) (stringp (cadr item)))
    (cons (car item) (cons (cadr item) binding)))
   (t (cons (car item) binding))))

(defun keymap--merge-bindings (val1 val2)
  "Merge bindings VAL1 and VAL2."
  (let ((map1 (keymap--menu-item-binding val1))
        (map2 (keymap--menu-item-binding val2)))
    (if (not (and (keymapp map1) (keymapp map2)))
        ;; There's nothing to merge: val1 takes precedence.
        val1
      (let ((map (list 'keymap map1 map2))
            (item (if (keymapp val1) (if (keymapp val2) nil val2) val1)))
        (keymap--menu-item-with-binding item map)))))

(defun keymap-canonicalize (map)
  "Return a simpler equivalent keymap.
This resolves inheritance and redefinitions.  The returned keymap
should behave identically to a copy of KEYMAP w.r.t `lookup-key'
and use in active keymaps and menus.
Subkeymaps may be modified but are not canonicalized."
  (declare (important-return-value t))
  ;; FIXME: Problem with the difference between a nil binding
  ;; that hides a binding in an inherited map and a nil binding that's ignored
  ;; to let some further binding visible.  Currently a nil binding hides all.
  ;; FIXME: we may want to carefully (re)order elements in case they're
  ;; menu-entries.
  (let ((bindings ())
        (ranges ())
	(prompt (keymap-prompt map)))
    (while (keymapp map)
      (setq map (map-keymap ;; -internal
                 (lambda (key item)
                   (if (consp key)
                       (if (= (car key) (1- (cdr key)))
                           ;; If we have a two-character range, then
                           ;; treat it as two separate characters
                           ;; (because this makes `describe-bindings'
                           ;; look better and shouldn't affect
                           ;; anything else).
                           (progn
                             (push (cons (car key) item) bindings)
                             (push (cons (cdr key) item) bindings))
                         ;; Treat char-ranges specially.
                         (push (cons key item) ranges))
                     (push (cons key item) bindings)))
                 map)))
    ;; Create the new map.
    (setq map (funcall (if ranges #'make-keymap #'make-sparse-keymap) prompt))
    (dolist (binding ranges)
      ;; Treat char-ranges specially.  FIXME: need to merge as well.
      (define-key map (vector (car binding)) (cdr binding)))
    ;; Process the bindings starting from the end.
    (dolist (binding (prog1 bindings (setq bindings ())))
      (let* ((key (car binding))
             (oldbind (assq key bindings)))
        (push (if (not oldbind)
                  ;; The normal case: no duplicate bindings.
                  binding
                ;; This is the second binding for this key.
                (setq bindings (delq oldbind bindings))
                (cons key (keymap--merge-bindings (cdr binding)
                                                  (cdr oldbind))))
              bindings)))
    (nconc map bindings)))

(put 'keyboard-translate-table 'char-table-extra-slots 0)

(defun keyboard-translate (from to)
  "Translate character FROM to TO on the current terminal.
This is a legacy function; see `key-translate' for the
recommended function to use instead.

This function creates a `keyboard-translate-table' if necessary
and then modifies one entry in it."
  (or (char-table-p keyboard-translate-table)
      (setq keyboard-translate-table
	    (make-char-table 'keyboard-translate-table nil)))
  (aset keyboard-translate-table from to))

;;;; Key binding commands.

(defun global-set-key (key command)
  "Give KEY a global binding as COMMAND.
This is a legacy function; see `keymap-global-set' for the
recommended function to use instead.

COMMAND is the command definition to use; usually it is
a symbol naming an interactively-callable function.
KEY is a key sequence; noninteractively, it is a string or vector
of characters or event types, and non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be included if you use a vector.

Note that if KEY has a local binding in the current buffer,
that local binding will continue to shadow any global binding
that you make with this function."
  (interactive
   (let* ((menu-prompting nil)
          (key (read-key-sequence "Set key globally: " nil t)))
     (list key
           (read-command (format "Set key %s to command: "
                                 (key-description key))))))
  (or (vectorp key) (stringp key)
      (signal 'wrong-type-argument (list 'arrayp key)))
  (define-key (current-global-map) key command))

(defun local-set-key (key command)
  "Give KEY a local binding as COMMAND.
This is a legacy function; see `keymap-local-set' for the
recommended function to use instead.

COMMAND is the command definition to use; usually it is
a symbol naming an interactively-callable function.
KEY is a key sequence; noninteractively, it is a string or vector
of characters or event types, and non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be included if you use a vector.

The binding goes in the current buffer's local map, which in most
cases is shared with all other buffers in the same major mode."
  (interactive "KSet key locally: \nCSet key %s locally to command: ")
  (let ((map (current-local-map)))
    (or map
	(use-local-map (setq map (make-sparse-keymap))))
    (or (vectorp key) (stringp key)
	(signal 'wrong-type-argument (list 'arrayp key)))
    (define-key map key command)))

(defun global-unset-key (key)
  "Remove global binding of KEY.
This is a legacy function; see `keymap-global-unset' for the
recommended function to use instead.

KEY is a string or vector representing a sequence of keystrokes."
  (interactive "kUnset key globally: ")
  (global-set-key key nil))

(defun local-unset-key (key)
  "Remove local binding of KEY.
This is a legacy function; see `keymap-local-unset' for the
recommended function to use instead.

KEY is a string or vector representing a sequence of keystrokes."
  (interactive "kUnset key locally: ")
  (if (current-local-map)
      (local-set-key key nil))
  nil)

(defun local-key-binding (keys &optional accept-default)
  "Return the binding for command KEYS in current local keymap only.
This is a legacy function; see `keymap-local-lookup' for the
recommended function to use instead.

KEYS is a string or vector, a sequence of keystrokes.
The binding is probably a symbol with a function definition.

If optional argument ACCEPT-DEFAULT is non-nil, recognize default
bindings; see the description of `lookup-key' for more details
about this."
  (let ((map (current-local-map)))
    (when map (lookup-key map keys accept-default))))

(defun global-key-binding (keys &optional accept-default)
  "Return the binding for command KEYS in current global keymap only.
This is a legacy function; see `keymap-global-lookup' for the
recommended function to use instead.

KEYS is a string or vector, a sequence of keystrokes.
The binding is probably a symbol with a function definition.
This function's return values are the same as those of `lookup-key'
\(which see).

If optional argument ACCEPT-DEFAULT is non-nil, recognize default
bindings; see the description of `lookup-key' for more details
about this."
  (lookup-key (current-global-map) keys accept-default))


;;;; substitute-key-definition and its subroutines.

(defvar key-substitution-in-progress nil
  "Used internally by `substitute-key-definition'.")

(defun substitute-key-definition (olddef newdef keymap &optional oldmap prefix)
  "Replace OLDDEF with NEWDEF for any keys in KEYMAP now defined as OLDDEF.
This is a legacy function; see `keymap-substitute' for the
recommended function to use instead.

In other words, OLDDEF is replaced with NEWDEF wherever it appears.
Alternatively, if optional fourth argument OLDMAP is specified, we redefine
in KEYMAP as NEWDEF those keys that are defined as OLDDEF in OLDMAP.

If you don't specify OLDMAP, you can usually get the same results
in a cleaner way with command remapping, like this:
  (define-key KEYMAP [remap OLDDEF] NEWDEF)
\n(fn OLDDEF NEWDEF KEYMAP &optional OLDMAP)"
  ;; Don't document PREFIX in the doc string because we don't want to
  ;; advertise it.  It's meant for recursive calls only.  Here's its
  ;; meaning

  ;; If optional argument PREFIX is specified, it should be a key
  ;; prefix, a string.  Redefined bindings will then be bound to the
  ;; original key, with PREFIX added at the front.
  (or prefix (setq prefix ""))
  (let* ((scan (or oldmap keymap))
	 (prefix1 (vconcat prefix [nil]))
	 (key-substitution-in-progress
	  (cons scan key-substitution-in-progress)))
    ;; Scan OLDMAP, finding each char or event-symbol that
    ;; has any definition, and act on it with hack-key.
    (map-keymap
     (lambda (char defn)
       (aset prefix1 (length prefix) char)
       (substitute-key-definition-key defn olddef newdef prefix1 keymap))
     scan)))

(defun substitute-key-definition-key (defn olddef newdef prefix keymap)
  (let (inner-def skipped menu-item)
    ;; Find the actual command name within the binding.
    (if (eq (car-safe defn) 'menu-item)
	(setq menu-item defn defn (nth 2 defn))
      ;; Skip past menu-prompt.
      (while (stringp (car-safe defn))
	(push (pop defn) skipped))
      ;; Skip past cached key-equivalence data for menu items.
      (if (consp (car-safe defn))
	  (setq defn (cdr defn))))
    (if (or (eq defn olddef)
	    ;; Compare with equal if definition is a key sequence.
	    ;; That is useful for operating on function-key-map.
	    (and (or (stringp defn) (vectorp defn))
		 (equal defn olddef)))
	(define-key keymap prefix
	  (if menu-item
	      (let ((copy (copy-sequence menu-item)))
		(setcar (nthcdr 2 copy) newdef)
		copy)
	    (nconc (nreverse skipped) newdef)))
      ;; Look past a symbol that names a keymap.
      (setq inner-def
	    (or (indirect-function defn) defn))
      ;; For nested keymaps, we use `inner-def' rather than `defn' so as to
      ;; avoid autoloading a keymap.  This is mostly done to preserve the
      ;; original non-autoloading behavior of pre-map-keymap times.
      (if (and (keymapp inner-def)
	       ;; Avoid recursively scanning
	       ;; where KEYMAP does not have a submap.
	       (let ((elt (lookup-key keymap prefix)))
		 (or (null elt) (natnump elt) (keymapp elt)))
	       ;; Avoid recursively rescanning keymap being scanned.
	       (not (memq inner-def key-substitution-in-progress)))
	  ;; If this one isn't being scanned already, scan it now.
	  (substitute-key-definition olddef newdef keymap inner-def prefix)))))


;;;; The global keymap tree.

(defvar esc-map
  (let ((map (make-keymap)))
    (define-key map "u" #'upcase-word)
    (define-key map "l" #'downcase-word)
    (define-key map "c" #'capitalize-word)
    (define-key map "x" #'execute-extended-command)
    (define-key map "X" #'execute-extended-command-for-buffer)
    map)
  "Default keymap for ESC (meta) commands.
The normal global definition of the character ESC indirects to this keymap.")
(fset 'ESC-prefix esc-map)
(make-obsolete 'ESC-prefix 'esc-map "28.1")

(defvar ctl-x-4-map (make-sparse-keymap)
  "Keymap for subcommands of \\`C-x 4'.")
(defalias 'ctl-x-4-prefix ctl-x-4-map)

(defvar ctl-x-5-map (make-sparse-keymap)
  "Keymap for frame commands.")
(defalias 'ctl-x-5-prefix ctl-x-5-map)

(defvar tab-prefix-map (make-sparse-keymap)
  "Keymap for tab-bar related commands.")

(defvar ctl-x-map
  (let ((map (make-keymap)))
    (define-key map "4" 'ctl-x-4-prefix)
    (define-key map "5" 'ctl-x-5-prefix)
    (define-key map "t" tab-prefix-map)

    (define-key map "b" #'switch-to-buffer)
    (define-key map "k" #'kill-buffer)
    (define-key map "\C-u" #'upcase-region)   (put 'upcase-region   'disabled t)
    (define-key map "\C-l" #'downcase-region) (put 'downcase-region 'disabled t)
    (define-key map "<" #'scroll-left)
    (define-key map ">" #'scroll-right)
    map)
  "Default keymap for \\`C-x' commands.
The normal global definition of the character \\`C-x' indirects to this
keymap.")
(fset 'Control-X-prefix ctl-x-map)
(make-obsolete 'Control-X-prefix 'ctl-x-map "28.1")

(defvar global-map
  (let ((map (make-keymap)))
    (define-key map "\C-[" 'ESC-prefix)
    (define-key map "\C-x" 'Control-X-prefix)

    (define-key map "\C-i" #'self-insert-command)
    (let* ((vec1 (make-vector 1 nil))
           (f (lambda (from to)
                (while (< from to)
                  (aset vec1 0 from)
                  (define-key map vec1 #'self-insert-command)
                  (setq from (1+ from))))))
      (funcall f #o040 #o0177)
      (when (eq system-type 'ms-dos)      ;FIXME: Why?
        (funcall f #o0200 #o0240))
      (funcall f #o0240 #o0400))

    (define-key map "\C-a" #'beginning-of-line)
    (define-key map "\C-b" #'backward-char)
    (define-key map "\C-e" #'end-of-line)
    (define-key map "\C-f" #'forward-char)

    (define-key map "\C-z"     #'suspend-emacs) ;FIXME: Re-bound later!
    (define-key map "\C-x\C-z" #'suspend-emacs) ;FIXME: Re-bound later!

    (define-key map "\C-v"    #'scroll-up-command)
    (define-key map "\M-v"    #'scroll-down-command)
    (define-key map "\M-\C-v" #'scroll-other-window)

    (define-key map "\M-\C-c" #'exit-recursive-edit)
    (define-key map "\C-]"    #'abort-recursive-edit)
    map)
  "Default global keymap mapping Emacs keyboard input into commands.
The value is a keymap that is usually (but not necessarily) Emacs's
global map.

See also `current-global-map'.")
(use-global-map global-map)


;;;; Event manipulation functions.

(defconst listify-key-sequence-1 (logior 128 ?\M-\C-@))

(defun listify-key-sequence (key)
  "Convert a key sequence to a list of events."
  (declare (side-effect-free t))
  (if (vectorp key)
      (append key nil)
    (mapcar (lambda (c)
              (if (> c 127)
                  (logxor c listify-key-sequence-1)
                c))
	    key)))

(defun eventp (object)
  "Return non-nil if OBJECT is an input event or event object."
  (declare (ftype (function (t) boolean))
           (pure t) (side-effect-free error-free))
  (or (integerp object)
      (and (if (consp object)
               (setq object (car object))
             object)
           (symbolp object)
           (not (keywordp object)))))

(defun event-modifiers (event)
  "Return a list of symbols representing the modifier keys in event EVENT.
The elements of the list may include `meta', `control',
`shift', `hyper', `super', `alt', `click', `double', `triple', `drag',
and `down'.
EVENT may be an event or an event type.  If EVENT is a symbol
that has never been used in an event that has been read as input
in the current Emacs session, then this function may fail to include
the `click' modifier."
  (declare (side-effect-free t))
  (unless (stringp event)
    (let ((type event))
      (if (listp type)
	  (setq type (car type)))
      (if (symbolp type)
          ;; Don't read event-symbol-elements directly since we're not
          ;; sure the symbol has already been parsed.
	  (cdr (internal-event-symbol-parse-modifiers type))
        (let ((list nil)
	      (char (logand type (lognot (logior ?\M-\0 ?\C-\0 ?\S-\0
					         ?\H-\0 ?\s-\0 ?\A-\0)))))
	  (if (not (zerop (logand type ?\M-\0)))
	      (push 'meta list))
	  (if (or (not (zerop (logand type ?\C-\0)))
		  (< char 32))
	      (push 'control list))
	  (if (or (not (zerop (logand type ?\S-\0)))
		  (/= char (downcase char)))
	      (push 'shift list))
	  (or (zerop (logand type ?\H-\0))
	      (push 'hyper list))
	  (or (zerop (logand type ?\s-\0))
	      (push 'super list))
	  (or (zerop (logand type ?\A-\0))
	      (push 'alt list))
	  list)))))

(defun event-basic-type (event)
  "Return the basic type of the given event (all modifiers removed).
The value is a printing character (not upper case) or a symbol.
EVENT may be an event or an event type.  If EVENT is a symbol
that has never been used in an event that has been read as input
in the current Emacs session, then this function may return nil."
  (declare (side-effect-free t))
  (unless (stringp event)
    (if (consp event)
        (setq event (car event)))
    (if (symbolp event)
        (car (get event 'event-symbol-elements))
      (let* ((base (logand event (1- ?\A-\0)))
	     (uncontrolled (if (< base 32) (logior base 64) base)))
        ;; There are some numbers that are invalid characters and
        ;; cause `downcase' to get an error.
        (condition-case ()
	    (downcase uncontrolled)
	  (error uncontrolled))))))

(defsubst mouse-movement-p (object)
  "Return non-nil if OBJECT is a mouse movement event."
  (declare (ftype (function (t) boolean))
           (side-effect-free error-free))
  (eq (car-safe object) 'mouse-movement))

(defun mouse-event-p (object)
  "Return non-nil if OBJECT is a mouse click event."
  (declare (side-effect-free t))
  ;; is this really correct? maybe remove mouse-movement?
  (memq (event-basic-type object) '(mouse-1 mouse-2 mouse-3 mouse-movement)))

(defun event--posn-at-point ()
  ;; Use `window-point' for the case when the current buffer
  ;; is temporarily switched to some other buffer (bug#50256)
  (let* ((pos (window-point))
         (posn (posn-at-point pos (if (minibufferp (current-buffer))
                                      (minibuffer-window)))))
    (cond ((null posn) ;; `pos' is "out of sight".
           (setq posn (list (selected-window) pos '(0 . 0) 0)))
          ;; If `pos' is inside a chunk of text hidden by an `invisible'
          ;; or `display' property, `posn-at-point' returns the position
          ;; that *is* visible, whereas `event--posn-at-point' is used
          ;; when we have a keyboard event, whose position is `point' even
          ;; if that position is invisible.
          ((> (length posn) 5)
           (setf (nth 5 posn) pos)))
    posn))

(defun event-start (event)
  "Return the starting position of EVENT.
EVENT should be a mouse click, drag, touch screen, or key press
event.  If EVENT is nil, the value of `posn-at-point' is used
instead.

The following accessor functions are used to access the elements
of the position:

`posn-window': The window of the event end, or its frame if the
event end point belongs to no window.
`posn-area': A symbol identifying the area the event occurred in,
or nil if the event occurred in the text area.
`posn-point': The buffer position of the event.
`posn-x-y': The pixel-based coordinates of the event.
`posn-col-row': The estimated column and row corresponding to the
position of the event.
`posn-actual-col-row': The actual column and row corresponding to the
position of the event.
`posn-string': The string object of the event, which is either
nil or (STRING . POSITION)'.
`posn-image': The image object of the event, if any.
`posn-object': The image or string object of the event, if any.
`posn-timestamp': The time the event occurred, in milliseconds.

For more information, see Info node `(elisp)Click Events'."
  (declare (side-effect-free t))
  (if (and (consp event)
           (or (eq (car event) 'touchscreen-begin)
               (eq (car event) 'touchscreen-end)))
      ;; Touch screen begin and end events save their information in a
      ;; different format, where the mouse position list is the cdr of
      ;; (nth 1 event).
      (cdadr event)
    (or (and (consp event)
             ;; Ignore touchscreen update events.  They store the posn
             ;; in a different format, and can have multiple posns.
             (not (eq (car event) 'touchscreen-update))
             (nth 1 event))
        (event--posn-at-point))))

(defun event-end (event)
  "Return the ending position of EVENT.
EVENT should be a click, drag, touch screen, or key press event.

See `event-start' for a description of the value returned."
  (declare (side-effect-free t))
  (if (and (consp event)
           (or (eq (car event) 'touchscreen-begin)
               (eq (car event) 'touchscreen-end)))
      (cdadr event)
    (or (and (consp event)
             (not (eq (car event) 'touchscreen-update))
             (nth (if (consp (nth 2 event)) 2 1) event))
        (event--posn-at-point))))

(defsubst event-click-count (event)
  "Return the multi-click count of EVENT, a click or drag event.
The return value is a positive integer."
  (declare (side-effect-free t))
  (if (and (consp event) (integerp (nth 2 event))) (nth 2 event) 1))

(defsubst event-line-count (event)
  "Return the line count of EVENT, a mousewheel event.
The return value is a positive integer."
  (declare (side-effect-free t))
  (if (and (consp event) (integerp (nth 3 event))) (nth 3 event) 1))

;;;; Extracting fields of the positions in an event.

(defun posnp (obj)
  "Return non-nil if OBJ appears to be a valid `posn' object specifying a window.
A `posn' object is returned from functions such as `event-start'.
If OBJ is a valid `posn' object, but specifies a frame rather
than a window, return nil."
  (declare (side-effect-free error-free))
  ;; FIXME: Correct the behavior of this function so that all valid
  ;; `posn' objects are recognized, after updating other code that
  ;; depends on its present behavior.
  (and (windowp (car-safe obj))
       (atom (car-safe (setq obj (cdr obj))))                ;AREA-OR-POS.
       (integerp (car-safe (car-safe (setq obj (cdr obj))))) ;XOFFSET.
       (integerp (car-safe (cdr obj)))))                     ;TIMESTAMP.

(defsubst posn-window (position)
  "Return the window in POSITION.
If POSITION is outside the frame where the event was initiated,
return that frame instead.  POSITION should be a list of the form
returned by the `event-start' and `event-end' functions."
  (declare (side-effect-free t))
  (nth 0 position))

(defsubst posn-area (position)
  "Return the window area recorded in POSITION, or nil for the text area.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (declare (side-effect-free t))
  (let ((area (if (consp (nth 1 position))
		  (car (nth 1 position))
		(nth 1 position))))
    (and (symbolp area) area)))

(defun posn-point (position)
  "Return the buffer location in POSITION.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions.
Returns nil if POSITION does not correspond to any buffer location (e.g.
a click on a scroll bar)."
  (declare (side-effect-free t))
  (or (nth 5 position)
      (let ((pt (nth 1 position)))
        (or (car-safe pt)
            ;; Apparently this can also be `vertical-scroll-bar' (bug#13979).
            (if (integerp pt) pt)))))

(defun posn-set-point (position)
  "Move point to POSITION.
Select the corresponding window as well."
  (if (framep (posn-window position))
      (progn
        (unless (windowp (frame-selected-window (posn-window position)))
          (error "Position not in text area of window"))
        (select-window (frame-selected-window (posn-window position))))
    (unless (windowp (posn-window position))
      (error "Position not in text area of window"))
    (select-window (posn-window position)))
  (if (numberp (posn-point position))
      (goto-char (posn-point position))))

(defsubst posn-x-y (position)
  "Return the x and y coordinates in POSITION.
The return value has the form (X . Y), where X and Y are given in
pixels.  POSITION should be a list of the form returned by
`event-start' and `event-end'."
  (declare (side-effect-free t))
  (nth 2 position))

(declare-function scroll-bar-scale "scroll-bar" (num-denom whole))

(defun posn-col-row (position &optional use-window)
  "Return the nominal column and row in POSITION, measured in characters.
The column and row values are approximations calculated from the x
and y coordinates in POSITION and the frame's default character width
and default line height, including spacing.

If USE-WINDOW is non-nil, use the typical width of a character in
the window indicated by POSITION instead of the frame.  (This
makes a difference is a window has a zoom level.)

For a scroll-bar event, the result column is 0, and the row
corresponds to the vertical position of the click in the scroll bar.

POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (declare (side-effect-free t))
  (let* ((pair            (posn-x-y position))
         (frame-or-window (posn-window position))
         (frame           (if (framep frame-or-window)
                              frame-or-window
                            (window-frame frame-or-window)))
         (window          (when (windowp frame-or-window) frame-or-window))
         (area            (posn-area position)))
    (cond
     ((null frame-or-window)
      '(0 . 0))
     ((eq area 'vertical-scroll-bar)
      (cons 0 (scroll-bar-scale pair (1- (window-height window)))))
     ((eq area 'horizontal-scroll-bar)
      (cons (scroll-bar-scale pair (window-width window)) 0))
     (t
      (if use-window
          (cons (/ (car pair) (window-font-width window))
                (/ (cdr pair) (window-font-height window)))
        ;; FIXME: This should take line-spacing properties on
        ;; newlines into account.
        (let* ((spacing (when (display-graphic-p frame)
                          (or (with-current-buffer
                                  (window-buffer (frame-selected-window frame))
                                (total-line-spacing))
                              (total-line-spacing
                               (frame-parameter frame 'line-spacing))))))
	  (cond ((floatp spacing)
	         (setq spacing (truncate (* spacing
					    (frame-char-height frame)))))
	        ((null spacing)
	         (setq spacing 0)))
	  (cons (/ (car pair) (frame-char-width frame))
	        (/ (cdr pair) (+ (frame-char-height frame) spacing)))))))))

(defun posn-actual-col-row (position)
  "Return the window row number in POSITION and character number in that row.

Return nil if POSITION does not contain the actual position; in that case
`posn-col-row' can be used to get approximate values.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions.

This function does not account for the width on display, like the
number of visual columns taken by a TAB or image.  If you need
the coordinates of POSITION in character units, you should use
`posn-col-row', not this function."
  (declare (side-effect-free t))
  (nth 6 position))

(defsubst posn-timestamp (position)
  "Return the timestamp of POSITION.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (declare (side-effect-free t))
  (nth 3 position))

(defun posn-string (position)
  "Return the string object of POSITION.
Value is a cons (STRING . STRING-POS), or nil if not a string.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (declare (side-effect-free t))
  (let ((x (nth 4 position)))
    ;; Apparently this can also be `handle' or `below-handle' (bug#13979).
    (when (consp x) x)))

(defsubst posn-image (position)
  "Return the image object of POSITION.
Value is a list (image ...), or nil if not an image.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (declare (side-effect-free t))
  (nth 7 position))

(defsubst posn-object (position)
  "Return the object (image or string) of POSITION.
Value is a list (image ...) for an image object, a cons cell
\(STRING . STRING-POS) for a string object, and nil for a buffer position.
POSITION should be a list of the form returned by the `event-start'
and `event-end' functions."
  (declare (side-effect-free t))
  (or (posn-image position) (posn-string position)))

(defsubst posn-object-x-y (position)
  "Return the x and y coordinates relative to the glyph of object of POSITION.
The return value has the form (DX . DY), where DX and DY are
given in pixels, and they are relative to the top-left corner of
the clicked glyph of object at POSITION.  POSITION should be a
list of the form returned by `event-start' and `event-end'."
  (declare (side-effect-free t))
  (nth 8 position))

(defsubst posn-object-width-height (position)
  "Return the pixel width and height of the object of POSITION.
The return value has the form (WIDTH . HEIGHT).  POSITION should
be a list of the form returned by `event-start' and `event-end'."
  (declare (side-effect-free t))
  (nth 9 position))

(defun values--store-value (value)
  "Store VALUE in the obsolete `values' variable."
  (with-suppressed-warnings ((obsolete values))
    (push value values))
  value)


;;;; Obsolescent names for functions.

(make-obsolete 'invocation-directory "use the variable of the same name."
               "27.1")
(make-obsolete 'invocation-name "use the variable of the same name." "27.1")

;; We used to declare string-to-unibyte obsolete, but it is a valid
;; way of getting a unibyte string that can be indexed by bytes, when
;; the original string has raw bytes in their internal multibyte
;; representation.  This can be useful when one needs to examine
;; individual bytes at known offsets from the string beginning.
;; (make-obsolete 'string-to-unibyte   "use `encode-coding-string'." "26.1")
;; string-to-multibyte is also sometimes useful (and there's no good
;; general replacement for it), so it's also been revived in Emacs 27.1.
;; (make-obsolete 'string-to-multibyte "use `decode-coding-string'." "26.1")
;; bug#23850
(make-obsolete 'string-as-unibyte   "use `encode-coding-string'." "26.1")
(make-obsolete 'string-make-unibyte   "use `encode-coding-string'." "26.1")
(make-obsolete 'string-as-multibyte "use `decode-coding-string'." "26.1")
(make-obsolete 'string-make-multibyte "use `decode-coding-string'." "26.1")

(defun log10 (x)
  "Return (log X 10), the log base 10 of X."
  (declare (ftype (function (number) float))
           (side-effect-free t) (obsolete log "24.4"))
  (log x 10))

(set-advertised-calling-convention 'indirect-function '(object) "25.1")
(set-advertised-calling-convention 'redirect-frame-focus '(frame focus-frame) "24.3")
(set-advertised-calling-convention 'libxml-parse-xml-region '(&optional start end base-url) "27.1")
(set-advertised-calling-convention 'libxml-parse-html-region '(&optional start end base-url) "27.1")
(set-advertised-calling-convention 'sleep-for '(seconds) "30.1")
(set-advertised-calling-convention 'time-convert '(time form) "29.1")

;;;; Obsolescence declarations for variables, and aliases.
(make-obsolete-variable
 'inhibit-point-motion-hooks
 "use `cursor-intangible-mode' or `cursor-sensor-mode' instead"
 ;; It's been announced as obsolete in NEWS and in the docstring since Emacs-25,
 ;; but it's only been marked for compilation warnings since Emacs-29.
 "25.1")
(make-obsolete-variable 'operating-system-release nil "28.1")
(make-obsolete-variable 'inhibit-changing-match-data 'save-match-data "29.1")

(make-obsolete 'run-window-configuration-change-hook nil "27.1")

(make-obsolete-variable 'command-debug-status
                        "expect it to be removed in a future version." "25.2")

;; This was introduced in 21.4 for pre-unicode unification.  That
;; usage was rendered obsolete in 23.1, which uses Unicode internally.
;; Other uses are possible, so this variable is not _really_ obsolete,
;; but Stefan insists to mark it so.
(make-obsolete-variable 'translation-table-for-input nil "23.1")

(make-obsolete-variable 'x-gtk-use-window-move nil "26.1")

(defvaralias 'messages-buffer-max-lines 'message-log-max)
(define-obsolete-variable-alias 'inhibit-nul-byte-detection
  'inhibit-null-byte-detection "28.1")
(make-obsolete-variable 'load-dangerous-libraries
                        "no longer used." "27.1")

(define-obsolete-function-alias 'compare-window-configurations
  #'window-configuration-equal-p "29.1")

;; We can't actually make `values' obsolete, because that will result
;; in warnings when using `values' in let-bindings.
;;(make-obsolete-variable 'values "no longer used" "28.1")

(defvar max-specpdl-size 2500
  "Former limit on specbindings, now without effect.
This variable used to limit the size of the specpdl stack which,
among other things, holds dynamic variable bindings and `unwind-protect'
activations.  To prevent runaway recursion, use `max-lisp-eval-depth'
instead; it will indirectly limit the specpdl stack size as well.")
(make-obsolete-variable 'max-specpdl-size nil "29.1")

(make-obsolete-variable 'comp-enable-subr-trampolines
                        'native-comp-enable-subr-trampolines
                        "29.1")

(defvaralias 'comp-enable-subr-trampolines 'native-comp-enable-subr-trampolines)

(make-obsolete-variable 'native-comp-deferred-compilation
                        'native-comp-jit-compilation
                        "29.1")

(defvaralias 'native-comp-deferred-compilation 'native-comp-jit-compilation)

(define-obsolete-function-alias 'fetch-bytecode #'ignore "30.1")

(define-obsolete-function-alias 'purecopy #'identity "31.1")

(make-obsolete-variable 'pure-bytes-used "no longer used." "31.1")


;;;; Alternate names for functions - these are not being phased out.

(defalias 'drop #'nthcdr)
(defalias 'send-string #'process-send-string)
(defalias 'send-region #'process-send-region)
(defalias 'string= #'string-equal)
(defalias 'string< #'string-lessp)
(defalias 'string> #'string-greaterp)
(defalias 'move-marker #'set-marker)
(defalias 'rplaca #'setcar)
(defalias 'rplacd #'setcdr)
(defalias 'beep #'ding) ;preserve lingual purity
(defalias 'indent-to-column #'indent-to)
(defalias 'backward-delete-char #'delete-backward-char)
(defalias 'search-forward-regexp (symbol-function 're-search-forward))
(defalias 'search-backward-regexp (symbol-function 're-search-backward))
(defalias 'int-to-string #'number-to-string)
(defalias 'store-match-data #'set-match-data)
(defalias 'chmod #'set-file-modes)
(defalias 'mkdir #'make-directory)
(defalias 'wholenump #'natnump)

;; These were the XEmacs names, now obsolete:
(defalias 'point-at-eol #'line-end-position)
(make-obsolete 'point-at-eol "use `line-end-position' or `pos-eol' instead." "29.1")
(defalias 'point-at-bol #'line-beginning-position)
(make-obsolete 'point-at-bol "use `line-beginning-position' or `pos-bol' instead." "29.1")
(define-obsolete-function-alias 'user-original-login-name #'user-login-name "28.1")

;; These are in obsolete/autoload.el, but are commonly used by
;; third-party scripts that assume that they exist without requiring
;; autoload.  These should be removed when obsolete/autoload.el is
;; removed.
(autoload 'make-directory-autoloads "autoload" nil t)
(autoload 'update-directory-autoloads "autoload" nil t)


;;;; Hook manipulation functions.

(defun add-hook (hook function &optional depth local)
  ;; Note: the -100..100 depth range is arbitrary and was chosen to match the
  ;; range used in add-function.
  "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.

HOOK should be a symbol.  If HOOK is void, or if HOOK's value is a
single function, it is changed to a list of functions.

The place where the function is added depends on the DEPTH
parameter.  DEPTH defaults to 0.  By convention, it should be
a number between -100 and 100 where 100 means that the function
should be at the very end of the list, whereas -100 means that
the function should always come first.
Since nothing is \"always\" true, don't use 100 nor -100.
When two functions have the same depth, the new one gets added after the
old one if depth is strictly positive and before otherwise.

For backward compatibility reasons, a symbol other than nil is
interpreted as a DEPTH of 90.

The optional fourth argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its global value.
This makes the hook buffer-local, and it makes t a member of the
buffer-local value.  That acts as a flag to run the hook
functions of the global value as well as in the local value.

FUNCTION may be any valid function, but it's recommended to use a
function symbol and not a lambda form.  Using a symbol will
ensure that the function is not re-added if the function is
edited, and using lambda forms may also have a negative
performance impact when running `add-hook' and `remove-hook'."
  (or (boundp hook) (set hook nil))
  (or (default-boundp hook) (set-default hook nil))
  (unless (numberp depth) (setq depth (if depth 90 0)))
  (if local (unless (local-variable-if-set-p hook)
	      (set (make-local-variable hook) (list t)))
    ;; Detect the case where make-local-variable was used on a hook
    ;; and do what we used to do.
    (when (and (local-variable-if-set-p hook)
               (not (and (consp (symbol-value hook))
                         (memq t (symbol-value hook)))))
      (setq local t)))
  (let ((hook-value (if local (symbol-value hook) (default-value hook))))
    ;; If the hook value is a single function, turn it into a list.
    (when (or (not (listp hook-value)) (functionp hook-value))
      (setq hook-value (list hook-value)))
    ;; Do the actual addition if necessary
    (unless (member function hook-value)
      (let ((depth-sym (get hook 'hook--depth-alist)))
        ;; While the `member' test above has to use `equal' for historical
        ;; reasons, `equal' is a performance problem on large/cyclic functions,
        ;; so we index `hook--depth-alist' with `eql'.  (bug#46326)
        (unless (zerop depth)
          (unless depth-sym
            (setq depth-sym (make-symbol "depth-alist"))
            (set depth-sym nil)
            (setf (get hook 'hook--depth-alist) depth-sym))
          (if local (make-local-variable depth-sym))
          (setf (alist-get function
                           (if local (symbol-value depth-sym)
                             (default-value depth-sym))
                           0)
                depth))
        (setq hook-value
	      (if (< 0 depth)
		  (append hook-value (list function))
		(cons function hook-value)))
        (when depth-sym
          (let ((depth-alist (if local (symbol-value depth-sym)
                               (default-value depth-sym))))
            (when depth-alist
              (setq hook-value
                    (sort (if (< 0 depth) hook-value (copy-sequence hook-value))
                          (lambda (f1 f2)
                            (< (alist-get f1 depth-alist 0 nil #'eq)
                               (alist-get f2 depth-alist 0 nil #'eq))))))))))
    ;; Set the actual variable
    (if local
	(progn
	  ;; If HOOK isn't a permanent local,
	  ;; but FUNCTION wants to survive a change of modes,
	  ;; mark HOOK as partially permanent.
	  (and (symbolp function)
	       (get function 'permanent-local-hook)
	       (not (get hook 'permanent-local))
	       (put hook 'permanent-local 'permanent-local-hook))
	  (set hook hook-value))
      (set-default hook hook-value))))

(defun remove-hook (hook function &optional local)
  "Remove FUNCTION from HOOK's functions.
HOOK should be a symbol, and FUNCTION may be any valid function.
Do nothing if HOOK does not currently contain FUNCTION.
Compare functions with `equal`, which means that it can be
slow if FUNCTION is not a symbol.  See `add-hook'.

The optional third argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value.

Interactively, prompt for the various arguments (skipping local
unless HOOK has both local and global functions).  If multiple
functions have the same representation under `princ', the first
one will be removed."
  (interactive
   (let* ((default (and (symbolp (variable-at-point))
                        (symbol-name (variable-at-point))))
          (hook (intern (completing-read
                         (format-prompt "Hook variable" default)
                         obarray #'boundp t nil nil default)))
          (local
           (and
            (local-variable-p hook)
            (symbol-value hook)
            ;; No need to prompt if there's nothing global
            (or (not (default-value hook))
                (y-or-n-p (format "%s has a buffer-local binding, use that? "
                                  hook)))))
          (fn-alist (mapcar
                     (lambda (x) (cons (with-output-to-string (prin1 x)) x))
                     (if local (symbol-value hook) (default-value hook))))
          (function (alist-get (completing-read
                                (format "%s hook to remove: "
                                        (if local "Buffer-local" "Global"))
                                fn-alist
                                nil t nil 'set-variable-value-history)
                               fn-alist nil nil #'string=)))
     (list hook function local)))
  (or (boundp hook) (set hook nil))
  (or (default-boundp hook) (set-default hook nil))
  ;; Do nothing if LOCAL is t but this hook has no local binding.
  (unless (and local (not (local-variable-p hook)))
    ;; Detect the case where make-local-variable was used on a hook
    ;; and do what we used to do.
    (when (and (local-variable-p hook)
	       (not (and (consp (symbol-value hook))
			 (memq t (symbol-value hook)))))
      (setq local t))
    (let ((hook-value (if local (symbol-value hook) (default-value hook)))
          (old-fun nil))
      ;; Remove the function, for both the list and the non-list cases.
      (if (or (not (listp hook-value)) (eq (car hook-value) 'lambda))
	  (when (equal hook-value function)
	    (setq old-fun hook-value)
	    (setq hook-value nil))
	(when (setq old-fun (car (member function hook-value)))
	  (setq hook-value (remq old-fun hook-value))))
      (when old-fun
        ;; Remove auxiliary depth info to avoid leaks (bug#46414)
        ;; and to avoid the list growing too long.
        (let* ((depth-sym (get hook 'hook--depth-alist))
               (depth-alist (if depth-sym (if local (symbol-value depth-sym)
                                            (default-value depth-sym))))
               (di (assq old-fun depth-alist)))
          (when di
            (setf (if local (symbol-value depth-sym)
                    (default-value depth-sym))
                  (remq di depth-alist)))))
      ;; If the function is on the global hook, we need to shadow it locally
      ;;(when (and local (member function (default-value hook))
      ;;	       (not (member (cons 'not function) hook-value)))
      ;;  (push (cons 'not function) hook-value))
      ;; Set the actual variable
      (if (not local)
	  (set-default hook hook-value)
	(if (equal hook-value '(t))
	    (kill-local-variable hook)
	  (set hook hook-value))))))

(defmacro letrec (binders &rest body)
  "Bind variables according to BINDERS then eval BODY.
The value of the last form in BODY is returned.
Each element of BINDERS is a list (SYMBOL VALUEFORM) that binds
SYMBOL to the value of VALUEFORM.

The main difference between this macro and `let'/`let*' is that
all symbols are bound before any of the VALUEFORMs are evalled."
  ;; Useful only in lexical-binding mode.
  ;; As a special-form, we could implement it more efficiently (and cleanly,
  ;; making the vars actually unbound during evaluation of the binders).
  (declare (debug let) (indent 1))
  ;; Use plain `let*' for the non-recursive definitions.
  ;; This only handles the case where the first few definitions are not
  ;; recursive.  Nothing as fancy as an SCC analysis.
  (let ((seqbinds nil))
    ;; Our args haven't yet been macro-expanded, so `macroexp--fgrep'
    ;; may fail to see references that will be introduced later by
    ;; macroexpansion.  We could call `macroexpand-all' to avoid that,
    ;; but in order to avoid that, we instead check to see if the binders
    ;; appear in the macroexp environment, since that's how references can be
    ;; introduced later on.
    (unless (macroexp--fgrep binders macroexpand-all-environment)
      (while (and binders
                  (null (macroexp--fgrep binders (nth 1 (car binders)))))
        (push (pop binders) seqbinds)))
    (let ((nbody (if (null binders)
                     (macroexp-progn body)
                   `(let ,(mapcar #'car binders)
                      ,@(mapcan (lambda (binder)
                                  (and (cdr binder) (list `(setq ,@binder))))
                                binders)
                      ,@body))))
      (cond
       ;; All bindings are recursive.
       ((null seqbinds) nbody)
       ;; Special case for trivial uses.
       ((and (symbolp nbody) (null (cdr seqbinds)) (eq nbody (caar seqbinds)))
        (nth 1 (car seqbinds)))
       ;; General case.
       (t `(let* ,(nreverse seqbinds) ,nbody))))))

(defmacro dlet (binders &rest body)
  "Like `let' but using dynamic scoping."
  (declare (indent 1) (debug let))
  ;; (defvar FOO) only affects the current scope, but in order for
  ;; this not to affect code after the main `let' we need to create a new scope,
  ;; which is what the surrounding `let' is for.
  ;; FIXME: (let () ...) currently doesn't actually create a new scope,
  ;; which is why we use (let (_) ...).
  `(let (_)
     ,@(mapcar (lambda (binder)
                 `(defvar ,(if (consp binder) (car binder) binder)))
               binders)
     (let ,binders ,@body)))


(defmacro with-wrapper-hook (hook args &rest body)
  "Run BODY, using wrapper functions from HOOK with additional ARGS.
HOOK is an abnormal hook.  Each hook function in HOOK \"wraps\"
around the preceding ones, like a set of nested `around' advices.

Each hook function should accept an argument list consisting of a
function FUN, followed by the additional arguments in ARGS.

The first hook function in HOOK is passed a FUN that, if it is called
with arguments ARGS, performs BODY (i.e., the default operation).
The FUN passed to each successive hook function is defined based
on the preceding hook functions; if called with arguments ARGS,
it does what the `with-wrapper-hook' call would do if the
preceding hook functions were the only ones present in HOOK.

Each hook function may call its FUN argument as many times as it wishes,
including never.  In that case, such a hook function acts to replace
the default definition altogether, and any preceding hook functions.
Of course, a subsequent hook function may do the same thing.

Each hook function definition is used to construct the FUN passed
to the next hook function, if any.  The last (or \"outermost\")
FUN is then called once."
  (declare (indent 2) (debug (form sexp body))
           (obsolete "use a <foo>-function variable modified by `add-function'."
                     "24.4"))
  `(subr--with-wrapper-hook-no-warnings ,hook ,args ,@body))

(defmacro subr--with-wrapper-hook-no-warnings (hook args &rest body)
  "Like (with-wrapper-hook HOOK ARGS BODY), but without warnings."
  (declare (debug (form sexp def-body)))
  ;; We need those two gensyms because CL's lexical scoping is not available
  ;; for function arguments :-(
  (let ((funs (make-symbol "funs"))
        (global (make-symbol "global"))
        (argssym (make-symbol "args"))
        (runrestofhook (make-symbol "runrestofhook")))
    ;; Since the hook is a wrapper, the loop has to be done via
    ;; recursion: a given hook function will call its parameter in order to
    ;; continue looping.
    `(letrec ((,runrestofhook
               (lambda (,funs ,global ,argssym)
                 ;; `funs' holds the functions left on the hook and `global'
                 ;; holds the functions left on the global part of the hook
                 ;; (in case the hook is local).
                 (if (consp ,funs)
                     (if (eq t (car ,funs))
                         (funcall ,runrestofhook
                                  (append ,global (cdr ,funs)) nil ,argssym)
                       (apply (car ,funs)
                              (apply-partially
                               (lambda (,funs ,global &rest ,argssym)
                                 (funcall ,runrestofhook ,funs ,global ,argssym))
                               (cdr ,funs) ,global)
                              ,argssym))
                   ;; Once there are no more functions on the hook, run
                   ;; the original body.
                   (apply (lambda ,args ,@body) ,argssym)))))
       (funcall ,runrestofhook ,hook
                ;; The global part of the hook, if any.
                ,(if (symbolp hook)
                     `(if (local-variable-p ',hook)
                          (default-value ',hook)))
                (list ,@args)))))

(defun add-to-list (list-var element &optional append compare-fn)
  "Add ELEMENT to the value of LIST-VAR if it isn't there yet.
The test for presence of ELEMENT is done with `equal', or with
COMPARE-FN if that's non-nil.
If ELEMENT is added, it is added at the beginning of the list,
unless the optional argument APPEND is non-nil, in which case
ELEMENT is added at the end.
LIST-VAR should not refer to a lexical variable.

The return value is the new value of LIST-VAR.

This is meant to be used for adding elements to configuration
variables, such as adding a directory to a path variable
like `load-path', but please do not abuse it to construct
arbitrary lists in Elisp code, where using `push' or `cl-pushnew'
will get you more efficient code.

If you want to use `add-to-list' on a variable that is not
defined until a certain package is loaded, you should put the
call to `add-to-list' into a hook function that will be run only
after loading the package.  `eval-after-load' provides one way to
do this.  In some cases other hooks, such as major mode hooks,
can do the job."
  (declare
   (compiler-macro
    (lambda (exp)
      ;; FIXME: Something like this could be used for `set' as well.
      (if (or (not (eq 'quote (car-safe list-var)))
              (special-variable-p (cadr list-var))
              (not (macroexp-const-p append)))
          exp
        (let* ((sym (cadr list-var))
               (append (eval append lexical-binding))
               (msg (format-message
                     "`add-to-list' can't use lexical var `%s'; use `push' or `cl-pushnew'"
                     sym))
               ;; Big ugly hack, so we output a warning only during
               ;; byte-compilation, and so we can use
               ;; byte-compile-not-lexical-var-p to silence the warning
               ;; when a defvar has been seen but not yet executed.
               (warnfun (lambda ()
                          ;; FIXME: We should also emit a warning for let-bound
                          ;; variables with dynamic binding.
                          (when (assq sym byte-compile--lexical-environment)
                            (byte-compile-report-error msg :fill))))
               (code
                (macroexp-let2 macroexp-copyable-p x element
                  `(if ,(if compare-fn
                            (progn
                              (require 'cl-lib)
                              `(cl-member ,x ,sym :test ,compare-fn))
                          ;; For bootstrapping reasons, don't rely on
                          ;; cl--compiler-macro-member for the base case.
                          `(member ,x ,sym))
                       ,sym
                     ,(if append
                          `(setq ,sym (append ,sym (list ,x)))
                        `(push ,x ,sym))))))
          (if (not (macroexp-compiling-p))
              code
            `(progn
               (macroexp--funcall-if-compiled ',warnfun)
               ,code)))))))
  (if (cond
       ((null compare-fn)
	(member element (symbol-value list-var)))
       ((eq compare-fn #'eq)
	(memq element (symbol-value list-var)))
       ((eq compare-fn #'eql)
	(memql element (symbol-value list-var)))
       (t
	(let ((lst (symbol-value list-var)))
	  (while (and lst
		      (not (funcall compare-fn element (car lst))))
	    (setq lst (cdr lst)))
          lst)))
      (symbol-value list-var)
    (set list-var
	 (if append
	     (append (symbol-value list-var) (list element))
	   (cons element (symbol-value list-var))))))


(defun add-to-ordered-list (list-var element &optional order)
  "Add ELEMENT to the value of LIST-VAR if it isn't there yet.
The test for presence of ELEMENT is done with `eq'.

The value of LIST-VAR is kept ordered based on the ORDER
parameter.

If the third optional argument ORDER is a number (integer or
float), set the element's list order to the given value.  If
ORDER is nil or omitted, do not change the numeric order of
ELEMENT.  If ORDER has any other value, remove the numeric order
of ELEMENT if it has one.

The list order for each element is stored in LIST-VAR's
`list-order' property.
LIST-VAR cannot refer to a lexical variable.

The return value is the new value of LIST-VAR."
  (let ((ordering (get list-var 'list-order)))
    (unless ordering
      (put list-var 'list-order
           (setq ordering (make-hash-table :weakness 'key :test 'eq))))
    (when order
      (puthash element (and (numberp order) order) ordering))
    (unless (memq element (symbol-value list-var))
      (set list-var (cons element (symbol-value list-var))))
    (set list-var (sort (symbol-value list-var)
			(lambda (a b)
			  (let ((oa (gethash a ordering))
				(ob (gethash b ordering)))
			    (if (and oa ob)
				(< oa ob)
			      oa)))))))

(defun add-to-history (history-var newelt &optional maxelt keep-all)
  "Add NEWELT to the history list stored in the variable HISTORY-VAR.
Return the new history list.
If MAXELT is non-nil, it specifies the maximum length of the history.
Otherwise, the maximum history length is the value of the `history-length'
property on symbol HISTORY-VAR, if set, or the value of the `history-length'
variable.  The possible values of maximum length have the same meaning as
the values of `history-length'.
Remove duplicates of NEWELT if `history-delete-duplicates' is non-nil.
If optional fourth arg KEEP-ALL is non-nil, add NEWELT to history even
if it is empty or duplicates the most recent entry in the history.
HISTORY-VAR cannot refer to a lexical variable."
  (unless maxelt
    (setq maxelt (or (get history-var 'history-length)
		     history-length)))
  (let ((history (symbol-value history-var))
	tail)
    (when (and (listp history)
	       (or keep-all
		   (not (stringp newelt))
		   (plusp (length newelt)))
	       (or keep-all
		   (not (equal (car history) newelt))))
      (if history-delete-duplicates
	  (setq history (delete newelt history)))
      (setq history (cons newelt history))
      (when (integerp maxelt)
        (if (>= 0 maxelt)
	    (setq history nil)
	  (setq tail (nthcdr (1- maxelt) history))
	  (when (consp tail)
            (setcdr tail nil))))
      (set history-var history))))


;;;; Mode hooks.

(defvar delay-mode-hooks nil
  "If non-nil, `run-mode-hooks' should delay running the hooks.")
(defvar-local delayed-mode-hooks nil
  "List of delayed mode hooks waiting to be run.")
(put 'delay-mode-hooks 'permanent-local t)

(defvar-local delayed-after-hook-functions nil
  "List of delayed :after-hook forms waiting to be run.
These forms come from `define-derived-mode'.")

(defvar change-major-mode-after-body-hook nil
  "Normal hook run in major mode functions, before the mode hooks.")

(defvar after-change-major-mode-hook nil
  "Normal hook run at the very end of major mode functions.")

(defun run-mode-hooks (&rest hooks)
  "Run mode hooks `delayed-mode-hooks' and HOOKS, or delay HOOKS.
Call `hack-local-variables' to set up file local and directory local
variables.

If the variable `delay-mode-hooks' is non-nil, does not do anything,
just adds the HOOKS to the list `delayed-mode-hooks'.
Otherwise, runs hooks in the sequence: `change-major-mode-after-body-hook',
`delayed-mode-hooks' (in reverse order), HOOKS, then runs
`hack-local-variables' (if the buffer is visiting a file),
runs the hook `after-change-major-mode-hook', and finally
evaluates the functions in `delayed-after-hook-functions' (see
`define-derived-mode').

Major mode functions should use this instead of `run-hooks' when
running their FOO-mode-hook."
  (if delay-mode-hooks
      ;; Delaying case.
      (dolist (hook hooks)
	(push hook delayed-mode-hooks))
    ;; Normal case, just run the hook as before plus any delayed hooks.
    (setq hooks (nconc (nreverse delayed-mode-hooks) hooks))
    (and (bound-and-true-p syntax-propertize-function)
         (not (local-variable-p 'parse-sexp-lookup-properties))
         ;; `syntax-propertize' sets `parse-sexp-lookup-properties' for us, but
         ;; in order for the sexp primitives to automatically call
         ;; `syntax-propertize' we need `parse-sexp-lookup-properties' to be
         ;; set first.
         (setq-local parse-sexp-lookup-properties t))
    (setq delayed-mode-hooks nil)
    (apply #'run-hooks (cons 'change-major-mode-after-body-hook hooks))
    (if (buffer-file-name)
        (with-demoted-errors "File local-variables error: %s"
          (hack-local-variables 'no-mode)))
    (run-hooks 'after-change-major-mode-hook)
    (dolist (fun (prog1 (nreverse delayed-after-hook-functions)
                    (setq delayed-after-hook-functions nil)))
      (funcall fun))))

(defmacro delay-mode-hooks (&rest body)
  "Execute BODY, but delay any `run-mode-hooks'.
These hooks will be executed by the first following call to
`run-mode-hooks' that occurs outside any `delay-mode-hooks' form.
Affects only hooks run in the current buffer."
  (declare (debug t) (indent 0))
  `(progn
     (make-local-variable 'delay-mode-hooks)
     (let ((delay-mode-hooks t))
       ,@body)))

;;; `if-let*' and friends.
;;
;; We considered adding a `cond-let*' in late 2025:
;; <https://lists.gnu.org/archive/html/emacs-devel/2025-09/msg00058.html>.
;; We decided to add the `bind-and*' clause type to `cond*' instead.
;; At first it seems simple to extend `if-let*'/`when-let*'/`and-let*'
;; to `cond', but the extension is not unambiguous: there are multiple
;; useful, incompatible ways to do it.
;; In particular, it quickly becomes clear that one wants clauses that
;; only establish bindings for proceeding clauses, instead of exiting
;; the `cond-let*'.  But then
;; - Should these bindings be just like in `let*', or like in
;;   `if-let*'?  In other words, should it be that if a binding
;;   evaluates to nil we skip the remaining bindings (bind them all to
;;   nil)?  Both ways of doing it seem useful.
;; - The parentheses quickly pile up.  How can we avoid the programmer
;;   having to count parentheses?  Some propose using square brackets
;;   (i.e., vectors) for the binding-only clauses, but Emacs Lisp is a
;;   traditional Lisp which uses exclusively parentheses for control
;;   constructs.  Therefore, introducing square brackets here would be
;;   jarring to read.  Another option would be to use symbols at the
;;   beginning of clauses, like `cond*' does.
;; Whichever way one goes, the resulting macro ends up complicated,
;; with a substantial learning burden.  Adding `bind-and*' clauses to
;; `cond*' gives us the desired functionality, and does not make
;; `cond*' much more complicated.  In other words, `cond*' is already
;; complicated, and one complicated `cond'-extending macro is better
;; than two.  --spwhitton

(defun internal--build-binding (binding prev-var)
  "Check and build a single BINDING with PREV-VAR."
  (setq binding
        (cond
         ((symbolp binding)
          (list binding binding))
         ((null (cdr binding))
          (list (make-symbol "s") (car binding)))
         ((eq '_ (car binding))
          (list (make-symbol "s") (cadr binding)))
         (t binding)))
  (when (> (length binding) 2)
    (signal 'error
            (cons "`let' bindings can have only one value-form" binding)))
  (let ((var (car binding)))
    `(,var (and ,prev-var ,(cadr binding)))))

(defun internal--build-bindings (bindings)
  "Check and build conditional value forms for BINDINGS."
  (let ((prev-var t))
    (mapcar (lambda (binding)
              (let ((binding (internal--build-binding binding prev-var)))
                (setq prev-var (car binding))
                binding))
            bindings)))

;; FIXME: Once Emacs 29 is ancient history we can consider a
;; byte-compiler warning.  This is because Emacs 29 and older will warn
;; about unused variables with (_ VALUEFORM).
(defmacro if-let* (varlist then &rest else)
  "Bind variables according to VARLIST and evaluate THEN or ELSE.
Evaluate each binding in turn, as in `let*', stopping if a
binding value is nil.  If all are non-nil return the value of
THEN, otherwise the value of the last form in ELSE, or nil if
there are none.

Each element of VARLIST is a list (SYMBOL VALUEFORM) that binds SYMBOL
to the value of VALUEFORM.  If only the test result is of interest, use
`_' as SYMBOL, i.e. (_ VALUEFORM), in which case VALUEFORM is evaluated
and checked for nil but the result is not bound.
An element of VARLIST can also be of the form SYMBOL, in which case the
binding of SYMBOL is checked for nil, only.

An older form for entries of VARLIST is also supported, where SYMBOL is
omitted, i.e. (VALUEFORM).  This means the same as (_ VALUEFORM).
This form is not recommended because many Lisp programmers find it
significantly less readable.  A future release of Emacs may introduce a
byte-compiler warning for uses of (VALUEFORM) in VARLIST."
  (declare (indent 2)
           (debug ((&rest [&or symbolp (symbolp form) (form)])
                   body)))
  (if varlist
      `(let* ,(setq varlist (internal--build-bindings varlist))
         (if ,(caar (last varlist))
             ,then
           ,@else))
    `(let* () ,then)))

(defmacro when-let* (varlist &rest body)
  "Bind variables according to VARLIST and conditionally evaluate BODY.
Evaluate each binding in turn, stopping if a binding value is nil.
If all are non-nil, evaluate the forms in BODY
and return the value of the last form.

The variable list VARLIST is the same as in `if-let*'.

See also `and-let*'."
  (declare (indent 1) (debug if-let*))
  (let ((res (list 'if-let* varlist (macroexp-progn body))))
    (if body res
      (macroexp-warn-and-return "Empty body" res 'empty-body))))

(defmacro and-let* (varlist &rest body)
  "Bind variables according to VARLIST and conditionally evaluate BODY.
Evaluate each binding in turn, stopping if a binding value is nil.
If all bindings are non-nil, evaluate the forms in BODY
and return the value of the last form, or else the last binding value
if BODY is empty.

Like `when-let*', except for the handling of an empty BODY.

Some Lisp programmers follow the convention that `and' and `and-let*'
are for forms evaluated for return value, and `when' and `when-let*' are
for forms evaluated for side-effect with returned values ignored."
  ;; ^ Document this convention here because it explains why we have
  ;;   both `when-let*' and `and-let*' (in addition to the additional
  ;;   feature of `and-let*' when BODY is empty).
  (declare (indent 1) (debug if-let*))
  (let (res)
    (if varlist
        `(let* ,(setq varlist (internal--build-bindings varlist))
           (when ,(setq res (caar (last varlist)))
             ,@(or body `(,res))))
      `(let* () ,@(or body '(t))))))

(defmacro if-let (spec then &rest else)
  "Bind variables according to SPEC and evaluate THEN or ELSE.
This is like `if-let*' except, as a special case, interpret a SPEC of
the form \(SYMBOL SOMETHING) like \((SYMBOL SOMETHING)).  This exists
for backward compatibility with an old syntax that accepted only one
binding."
  (declare (indent 2)
           (debug ([&or (symbolp form)  ; must be first, Bug#48489
                        (&rest [&or symbolp (symbolp form) (form)])]
                   body))
           (obsolete if-let* "31.1"))
  (when (and (<= (length spec) 2)
             (not (listp (car spec))))
    ;; Adjust the single binding case
    (setq spec (list spec)))
  (list 'if-let* spec then (macroexp-progn else)))

(defmacro when-let (spec &rest body)
  "Bind variables according to SPEC and conditionally evaluate BODY.
Evaluate each binding in turn, stopping if a binding value is nil.
If all are non-nil, evaluate the forms in BODY
and return the value of the last form.

The variable list SPEC is the same as in `if-let'."
  (declare (indent 1) (debug if-let)
           (obsolete "use `when-let*' or `and-let*' instead." "31.1"))
  ;; Previously we expanded to `if-let', and then required a
  ;; `with-suppressed-warnings' to avoid doubling up the obsoletion
  ;; warnings.  But that triggers a bytecompiler bug; see bug#74530.
  ;; So for now we reimplement `if-let' here.
  (when (and (<= (length spec) 2)
             (not (listp (car spec))))
    (setq spec (list spec)))
  (list 'if-let* spec (macroexp-progn body)))

(defmacro while-let (spec &rest body)
  "Bind variables according to SPEC and conditionally evaluate BODY.
Evaluate each binding in turn, stopping if a binding value is nil.
If all bindings are non-nil, eval BODY and repeat.

The variable list SPEC is the same as in `if-let*'."
  (declare (indent 1) (debug if-let))
  (let ((done (gensym "done")))
    `(catch ',done
       (while t
         ;; This is `if-let*', not `if-let', deliberately, despite the
         ;; name of this macro.  See bug#60758.
         (if-let* ,spec
             (progn
               ,@body)
           (throw ',done nil))))))

;; PUBLIC: find if the current mode derives from another.

(defun merge-ordered-lists (lists &optional error-function)
  "Merge LISTS in a consistent order.
LISTS is a list of lists of elements.
Merge them into a single list containing the same elements (removing
duplicates), obeying their relative positions in each list.
The order of the (sub)lists determines the final order in those cases where
the order within the sublists does not impose a unique choice.
Equality of elements is tested with `eql'.

If a consistent order does not exist, call ERROR-FUNCTION with
a remaining list of lists that we do not know how to merge.
It should return the candidate to use to continue the merge, which
has to be the head of one of the lists.
By default we choose the head of the first list."
  ;; Algorithm inspired from
  ;; [C3](https://en.wikipedia.org/wiki/C3_linearization)
  (let ((result '()))
    (setq lists (remq nil lists)) ;Don't mutate the original `lists' argument.
    (while (cdr lists)
      ;; Try to find the next element of the result.  This is achieved
      ;; by considering the first element of each input list and accepting
      ;; a candidate if it is consistent with the rest of the input lists.
      (let* ((find-next
	      (lambda (lists)
	        (let ((next nil)
	              (tail lists))
	          (while tail
	            (let ((candidate (caar tail))
	                  (other-lists lists))
	              ;; Ensure CANDIDATE is not in any position but the first
	              ;; in any of the element lists of LISTS.
	              (while other-lists
	                (if (not (memql candidate (cdr (car other-lists))))
	                    (setq other-lists (cdr other-lists))
	                  (setq candidate nil)
	                  (setq other-lists nil)))
	              (if (not candidate)
	                  (setq tail (cdr tail))
	                (setq next candidate)
	                (setq tail nil))))
	          next)))
	     (next (funcall find-next lists)))
	(unless next ;; The graph is inconsistent.
	  (let ((tail lists))
            ;; Try and reduce the "remaining-list" such that its `caar`
            ;; participates in the inconsistency (is part of an actual cycle).
	    (while (and (cdr tail) (null (funcall find-next (cdr tail))))
	      (setq tail (cdr tail)))
	    (setq next (funcall (or error-function
	                            (lambda (remaining-lists)
                                      (message "Inconsistent hierarchy: %S"
                                               remaining-lists)
                                      (caar remaining-lists)))
	                        tail))
	    (unless (assoc next lists #'eql)
	      (error "Invalid candidate returned by error-function: %S" next))
	    ;; Break the cycle, while keeping other dependencies.
            (dolist (list lists) (setcdr list (remq next (cdr list))))))
	;; The graph is consistent so far, add NEXT to result and
	;; merge input lists, dropping NEXT from their heads where
	;; applicable.
	(push next result)
	(setq lists
	      (delq nil
		    (mapcar (lambda (l) (if (eql (car l) next) (cdr l) l))
		            lists)))))
    (if (null result) (car lists) ;; Common case.
      (append (nreverse result) (car lists)))))

(defun derived-mode-all-parents (mode &optional known-children)
  "Return all the parents of MODE, starting with MODE.
This includes the parents set by `define-derived-mode' and additional
ones set by `derived-mode-add-parents'.
The returned list is not fresh, don't modify it.
\n(fn MODE)"               ;`known-children' is for internal use only.
  ;; Can't use `with-memoization' :-(
  (let ((ps (get mode 'derived-mode--all-parents)))
    (cond
     (ps ps)
     ((memq mode known-children)
      ;; These things happen, better not get all worked up about it.
      ;;(error "Cycle in the major mode hierarchy: %S" mode)
      ;; But do try to return something meaningful.
      (memq mode (reverse known-children)))
     (t
      ;; The mode hierarchy (or DAG, actually), is very static, but we
      ;; need to react to changes because `parent' may not be defined
      ;; yet (e.g. it's still just an autoload), so the recursive call
      ;; to `derived-mode-all-parents' may return an
      ;; invalid/incomplete result which we'll need to update when the
      ;; mode actually gets loaded.
      (let* ((new-children (cons mode known-children))
             (get-all-parents
              (lambda (parent)
                ;; Can't use `cl-lib' here (nor `gv') :-(
                ;;(cl-assert (not (equal parent mode)))
                ;;(cl-pushnew mode (get parent 'derived-mode--followers))
                (let ((followers (get parent 'derived-mode--followers)))
                  (unless (memq mode followers)
                    (put parent 'derived-mode--followers
                         (cons mode followers))))
                (derived-mode-all-parents parent new-children)))
             (parent (or (get mode 'derived-mode-parent)
                         ;; If MODE is an alias, then follow the alias.
                         (let ((alias (symbol-function mode)))
                           (and (symbolp alias) alias))))
             (extras (get mode 'derived-mode-extra-parents))
             (all-parents
              (merge-ordered-lists
               (cons (if (and parent (not (memq parent extras)))
                         (funcall get-all-parents parent))
                     (mapcar get-all-parents extras)))))
        ;; Cache the result unless it was affected by `known-children'
        ;; because of a cycle.
        (if (and (memq mode all-parents) known-children)
            (cons mode (remq mode all-parents))
          (put mode 'derived-mode--all-parents (cons mode all-parents))))))))

(defun provided-mode-derived-p (mode &optional modes &rest old-modes)
  "Non-nil if MODE is derived from a mode that is a member of the list MODES.
MODES can also be a single mode instead of a list.
This examines the parent modes set by `define-derived-mode' and also
additional ones set by `derived-mode-add-parents'.
If you just want to check the current `major-mode', use `derived-mode-p'.
We also still support the deprecated calling convention:
\(provided-mode-derived-p MODE &rest MODES)."
  (declare (side-effect-free t)
           (advertised-calling-convention (mode modes) "30.1"))
  (cond
   (old-modes (setq modes (cons modes old-modes)))
   ((not (listp modes)) (setq modes (list modes))))
  (let ((ps (derived-mode-all-parents mode)))
    (while (and modes (not (memq (car modes) ps)))
      (setq modes (cdr modes)))
    (car modes)))

(defun derived-mode-p (&optional modes &rest old-modes)
 "Return non-nil if the current major mode is derived from one of MODES.
MODES should be a list of symbols or a single mode symbol instead of a list.
This examines the parent modes set by `define-derived-mode' and also
additional ones set by `derived-mode-add-parents'.
We also still support the deprecated calling convention:
\(derived-mode-p &rest MODES)."
 (declare (side-effect-free t)
          ;; FIXME: It's cumbersome for external packages to write code which
          ;; accommodates both the old and the new calling conventions *and*
          ;; doesn't cause spurious warnings.  So let's be more lenient
          ;; for now and maybe remove `deprecated-args' for Emacs-31.
          (advertised-calling-convention (modes &rest deprecated-args) "30.1"))
 (provided-mode-derived-p major-mode (if old-modes (cons modes old-modes)
                                       modes)))

(defun derived-mode-set-parent (mode parent)
  "Declare PARENT to be the parent of MODE."
  (put mode 'derived-mode-parent parent)
  (derived-mode--flush mode))

(defun derived-mode-add-parents (mode extra-parents)
  "Add EXTRA-PARENTS to the parents of MODE.
Declares the parents of MODE to be its main parent (as defined
in `define-derived-mode') plus EXTRA-PARENTS, which should be a list
of symbols."
  (put mode 'derived-mode-extra-parents extra-parents)
  (derived-mode--flush mode))

(defun derived-mode--flush (mode)
  (put mode 'derived-mode--all-parents nil)
  (let ((followers (get mode 'derived-mode--followers)))
    (when followers ;; Common case.
      (put mode 'derived-mode--followers nil)
      (mapc #'derived-mode--flush followers))))

(defvar-local major-mode--suspended nil)
(put 'major-mode--suspended 'permanent-local t)

(defun major-mode-suspend ()
  "Exit current major mode, remembering it."
  (let* ((prev-major-mode (or major-mode--suspended
			      (unless (eq major-mode 'fundamental-mode)
			        major-mode))))
    (kill-all-local-variables)
    (setq-local major-mode--suspended prev-major-mode)))

(defun major-mode-restore (&optional avoided-modes)
  "Restore major mode earlier suspended with `major-mode-suspend'.
If there was no earlier suspended major mode, then fallback to `normal-mode',
though trying to avoid AVOIDED-MODES."
  (if major-mode--suspended
      (funcall (prog1 major-mode--suspended
                 (kill-local-variable 'major-mode--suspended)))
    (let ((auto-mode-alist
           (let ((alist (copy-sequence auto-mode-alist)))
             (dolist (mode avoided-modes)
               (setq alist (rassq-delete-all mode alist)))
             alist))
          (magic-fallback-mode-alist
           (let ((alist (copy-sequence magic-fallback-mode-alist)))
             (dolist (mode avoided-modes)
               (setq alist (rassq-delete-all mode alist)))
             alist)))
      (normal-mode))))

;;;; Minor modes.

;; If a minor mode is not defined with define-minor-mode,
;; add it here explicitly.
;; isearch-mode is deliberately excluded, since you should
;; not call it yourself.
(defvar minor-mode-list '(auto-save-mode auto-fill-mode abbrev-mode
					 overwrite-mode view-mode
                                         hs-minor-mode)
  "List of all minor mode functions.")

(defvar global-minor-modes nil
  "A list of the currently enabled global minor modes.
This is a list of symbols.")

(defun add-minor-mode (toggle name &optional keymap after toggle-fun)
  "Register a new minor mode.

This function shouldn't be used directly -- use `define-minor-mode'
instead (which will then call this function).

TOGGLE is a symbol that is the name of a buffer-local variable that
is toggled on or off to say whether the minor mode is active or not.

NAME specifies what will appear in the mode line when the minor mode
is active.  NAME should be either a string starting with a space, or a
symbol whose value is such a string.

Optional KEYMAP is the keymap for the minor mode that will be added
to `minor-mode-map-alist'.

Optional AFTER specifies that TOGGLE should be added after AFTER
in `minor-mode-alist'.

Optional TOGGLE-FUN is an interactive function to toggle the mode.
It defaults to (and should by convention be) TOGGLE.

If TOGGLE has a non-nil `:included' property, an entry for the mode is
included in the mode-line minor mode menu.
If TOGGLE has a `:menu-tag', that is used for the menu item's label."
  (unless (memq toggle minor-mode-list)
    (push toggle minor-mode-list))

  (unless toggle-fun (setq toggle-fun toggle))
  (unless (eq toggle-fun toggle)
    (put toggle :minor-mode-function toggle-fun))
  ;; Add the name to the minor-mode-alist.
  (when name
    (let ((existing (assq toggle minor-mode-alist)))
      (if existing
	  (setcdr existing (list name))
	(let ((tail minor-mode-alist) found)
	  (while (and tail (not found))
	    (if (eq after (caar tail))
		(setq found tail)
	      (setq tail (cdr tail))))
	  (if found
	      (let ((rest (cdr found)))
		(setcdr found nil)
		(nconc found (list (list toggle name)) rest))
	    (push (list toggle name) minor-mode-alist))))))
  ;; Add the toggle to the minor-modes menu if requested.
  (when (get toggle :included)
    (define-key mode-line-mode-menu
      (vector toggle)
      (list 'menu-item
	    (concat
	     (or (get toggle :menu-tag)
		 (if (stringp name) name (symbol-name toggle)))
	     (let ((mode-name (if (symbolp name) (symbol-value name))))
	       (if (and (stringp mode-name) (string-match "[^ ]+" mode-name))
		   (concat " (" (match-string 0 mode-name) ")"))))
	    toggle-fun
	    :button (cons :toggle toggle))))

  ;; Add the map to the minor-mode-map-alist.
  (when keymap
    (let ((existing (assq toggle minor-mode-map-alist)))
      (if existing
	  (setcdr existing keymap)
	(let ((tail minor-mode-map-alist) found)
	  (while (and tail (not found))
	    (if (eq after (caar tail))
		(setq found tail)
	      (setq tail (cdr tail))))
	  (if found
	      (let ((rest (cdr found)))
		(setcdr found nil)
		(nconc found (list (cons toggle keymap)) rest))
	    (push (cons toggle keymap) minor-mode-map-alist)))))))

;;;; Load history

(defsubst autoloadp (object)
  "Non-nil if OBJECT is an autoload."
  (declare (side-effect-free error-free))
  (eq 'autoload (car-safe object)))

;; (defun autoload-type (object)
;;   "Returns the type of OBJECT or `function' or `command' if the type is nil.
;; OBJECT should be an autoload object."
;;   (when (autoloadp object)
;;     (let ((type (nth 3 object)))
;;       (cond ((null type) (if (nth 2 object) 'command 'function))
;;             ((eq 'keymap t) 'macro)
;;             (type)))))

;; (defalias 'autoload-file #'cadr
;;   "Return the name of the file from which AUTOLOAD will be loaded.
;; \n\(fn AUTOLOAD)")

(defun define-symbol-prop (symbol prop val)
  "Define the property PROP of SYMBOL to be VAL.
This is to `put' what `defalias' is to `fset'."
  ;; Can't use `cl-pushnew' here (nor `push' on (cdr foo)).
  ;; (cl-pushnew symbol (alist-get prop
  ;;                               (alist-get 'define-symbol-props
  ;;                                          current-load-list)))
  (let ((sps (assq 'define-symbol-props current-load-list)))
    (unless sps
      (setq sps (list 'define-symbol-props))
      (push sps current-load-list))
    (let ((ps (assq prop sps)))
      (unless ps
        (setq ps (list prop))
        (setcdr sps (cons ps (cdr sps))))
      (unless (member symbol (cdr ps))
        (setcdr ps (cons symbol (cdr ps))))))
  (put symbol prop val))

(defvar comp-native-version-dir)
(defvar native-comp-eln-load-path)
(declare-function native-comp-function-p "data.c")
(declare-function native-comp-unit-file "data.c")
(declare-function subr-native-comp-unit "data.c")
(declare-function comp-el-to-eln-rel-filename "comp.c")

(defun locate-eln-file (eln-file)
  "Locate a native-compiled ELN-FILE by searching its load path.
This function looks in directories named by `native-comp-eln-load-path'."
  (declare (important-return-value t))
  (or (locate-file-internal (concat comp-native-version-dir "/" eln-file)
		   native-comp-eln-load-path)
      (locate-file-internal
       ;; Preloaded *.eln files live in the preloaded/ subdirectory of
       ;; the last entry in `native-comp-eln-load-path'.
       (concat comp-native-version-dir "/preloaded/" eln-file)
       (last native-comp-eln-load-path))))

(defun symbol-file (symbol &optional type native-p)
  "Return the name of the file that defined SYMBOL.
The value is normally an absolute file name.  It can also be nil,
if the definition is not associated with any file.  If SYMBOL
specifies an autoloaded function, the value can be a relative
file name without extension.

If TYPE is nil, then any kind of SYMBOL's definition is acceptable.
If TYPE is `defun', `defvar', or `defface', that specifies function
definition, variable definition, or face definition only.
Otherwise TYPE is assumed to be a symbol property.

If NATIVE-P is non-nil, and SYMBOL was loaded from a .eln file,
this function will return the absolute file name of that .eln file,
if found.  Note that if the .eln file is older than its source .el
file, Emacs won't load such an outdated .eln file, and this function
will not return it.  If the .eln file couldn't be found, or is
outdated, the function returns the corresponding .elc or .el file
instead.

This function only works for symbols defined in Lisp files.  For
symbols that are defined in C files, use `help-C-file-name'
instead."
  (declare (important-return-value t))
  (if (and (or (null type) (eq type 'defun))
	   (symbolp symbol)
	   (autoloadp (symbol-function symbol)))
      (locate-library
       (nth 1 (symbol-function symbol)))
    (if (and native-p (or (null type) (eq type 'defun))
	     (symbolp symbol)
	     (native-comp-available-p)
	     ;; If it's a defun, we have a shortcut.
	     (native-comp-function-p (symbol-function symbol)))
	;; native-comp-unit-file returns unnormalized file names.
	(expand-file-name (native-comp-unit-file (subr-native-comp-unit
						  (symbol-function symbol))))
      (let ((elc-file
	     (catch 'found
	       (pcase-dolist (`(,file . ,elems) load-history)
		 (when (if type
			   (if (eq type 'defvar)
			       ;; Variables are present just as their
			       ;; names.
			       (member symbol elems)
			     ;; Many other types are represented as
			     ;; (TYPE . NAME).
			     (or (member (cons type symbol) elems)
				 (memq
				  symbol
				  (alist-get type
					     (alist-get 'define-symbol-props
							elems)))))
			 ;; We accept all types, so look for variable def
			 ;; and then for any other kind.
			 (or (member symbol elems)
			     (let ((match (rassq symbol elems)))
			       (and match
				    (not (eq 'require (car match)))))))
		   (throw 'found file))))))
	;; If they asked for the .eln file, try to find it.
	(or (and elc-file
		 native-p
		 (native-comp-available-p)
		 (let* ((sans-ext (file-name-sans-extension elc-file))
			(el-file
			 (and (fboundp 'zlib-available-p)
			      (zlib-available-p)
			      (concat sans-ext ".el.gz")))
			(el-file-backup (concat sans-ext ".el")))
		   (or (and el-file (file-exists-p el-file))
		       (and (file-exists-p el-file-backup)
			    (setq el-file el-file-backup))
		       (setq el-file nil))
		   (when (stringp el-file)
		     (let ((eln-file (locate-eln-file
				      (comp-el-to-eln-rel-filename el-file))))
		       ;; Emacs will not load an outdated .eln file,
		       ;; so we mimic this behavior here.
		       (if (file-newer-than-file-p eln-file el-file)
			   eln-file)))))
	    elc-file)))))

(declare-function read-library-name "find-func" nil)

(defun locate-library (library &optional nosuffix path interactive-call)
  "Show the precise file name of Emacs library LIBRARY.
LIBRARY should be a relative file name of the library, a string.
It can omit the suffix (a.k.a. file-name extension) if NOSUFFIX is
nil (which is the default, see below).
This command searches the directories in `load-path' like \\[load-library]
to find the file that `\\[load-library] RET LIBRARY RET' would load.
Optional second arg NOSUFFIX non-nil means don't add suffixes `load-suffixes'
to the specified name LIBRARY.

If the optional third arg PATH is specified, that list of directories
is used instead of `load-path'.

When called from a program, the file name is normally returned as a
string.  When run interactively, the argument INTERACTIVE-CALL is t,
and the file name is displayed in the echo area."
  (interactive (list (read-library-name) nil nil t))
  (let ((file (locate-file library
			   (or path load-path)
			   (append (unless nosuffix (get-load-suffixes))
				   load-file-rep-suffixes))))
    (if interactive-call
	(if file
	    (message "Library is file %s" (abbreviate-file-name file))
	  (message "No library %s in search path" library)))
    file))


;;;; Process stuff.

(defun start-process (name buffer program &rest program-args)
  "Start a program in a subprocess.  Return the process object for it.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer name) to associate with the process.

Process output (both standard output and standard error streams)
goes at end of BUFFER, unless you specify a filter function to
handle the output.  BUFFER may also be nil, meaning that this
process is not associated with any buffer.

PROGRAM is the program file name.  It is searched for in `exec-path'
\(which see).  If nil, just associate a pty with the buffer.  Remaining
arguments PROGRAM-ARGS are strings to give program as arguments.

If you want to separate standard output from standard error, use
`make-process' or invoke the command through a shell and redirect
one of them using the shell syntax.

The process runs in `default-directory' if that is local (as
determined by `unhandled-file-name-directory'), or \"~\"
otherwise.  If you want to run a process in a remote directory
use `start-file-process'."
  (unless (fboundp 'make-process)
    (error "Emacs was compiled without subprocess support"))
  (apply #'make-process
	 (append (list :name name :buffer buffer)
		 (if program
		     (list :command (cons program program-args))))))

(defun process-lines-handling-status (program status-handler &rest args)
  "Execute PROGRAM with ARGS, returning its output as a list of lines.
If STATUS-HANDLER is non-nil, it must be a function with one
argument, which will be called with the exit status of the
program before the output is collected.  If STATUS-HANDLER is
nil, an error is signaled if the program returns with a non-zero
exit status."
  (declare (important-return-value t))
  (with-temp-buffer
    (let ((status (apply #'call-process program nil (current-buffer) nil args)))
      (if status-handler
	  (funcall status-handler status)
	(unless (eq status 0)
	  (error "%s exited with status %s" program status)))
      (goto-char (point-min))
      (let (lines)
	(while (not (eobp))
	  (setq lines (cons (buffer-substring-no-properties
			     (line-beginning-position)
			     (line-end-position))
			    lines))
	  (forward-line 1))
	(nreverse lines)))))

(defun process-lines (program &rest args)
  "Execute PROGRAM with ARGS, returning its output as a list of lines.
Signal an error if the program returns with a non-zero exit status.
Also see `process-lines-ignore-status'."
  (declare (important-return-value t))
  (apply #'process-lines-handling-status program nil args))

(defun process-lines-ignore-status (program &rest args)
  "Execute PROGRAM with ARGS, returning its output as a list of lines.
The exit status of the program is ignored.
Also see `process-lines'."
  (declare (important-return-value t))
  (apply #'process-lines-handling-status program #'ignore args))

(defun process-live-p (process)
  "Return non-nil if PROCESS is alive.
A process is considered alive if its status is `run', `open',
`listen', `connect' or `stop'.  Value is nil if PROCESS is not a
process."
  (and (processp process)
       (memq (process-status process)
	     '(run open listen connect stop))))

(defun process-kill-buffer-query-function ()
  "Ask before killing a buffer that has a running process."
  (let ((process (get-buffer-process (current-buffer))))
    (or (not process)
        (not (memq (process-status process) '(run stop open listen)))
        (not (process-query-on-exit-flag process))
        (yes-or-no-p
	 (format "Buffer %S has a running process; kill it? "
		 (buffer-name (current-buffer)))))))

(add-hook 'kill-buffer-query-functions #'process-kill-buffer-query-function)

;; process plist management

(defun process-get (process propname)
  "Return the value of PROCESS' PROPNAME property.
This is the last value stored with `(process-put PROCESS PROPNAME VALUE)'.

Together with `process-put', this can be used to store and retrieve
miscellaneous values associated with the process."
  (declare (side-effect-free t))
  (plist-get (process-plist process) propname))

(defun process-put (process propname value)
  "Change PROCESS' PROPNAME property to VALUE.
It can be retrieved with `(process-get PROCESS PROPNAME)'.

Together with `process-get', this can be used to store and retrieve
miscellaneous values associated with the process."
  (set-process-plist process
		     (plist-put (process-plist process) propname value)))

(defun memory-limit ()
  "Return an estimate of Emacs virtual memory usage, divided by 1024."
  (declare (ftype (function () integer))
           (side-effect-free error-free))
  (let ((default-directory temporary-file-directory))
    (or (cdr (assq 'vsize (process-attributes (emacs-pid)))) 0)))


;;;; Input and display facilities.

;; The following maps are used by `read-key' to remove all key
;; bindings while calling `read-key-sequence'.  This way the keys
;; returned are independent of the key binding state.

(defconst read-key-empty-map (make-sparse-keymap)
  "Used internally by `read-key'.")

(defconst read-key-full-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t] #'ignore)       ;Dummy binding.

    ;; ESC needs to be unbound so that escape sequences in
    ;; `input-decode-map' are still processed by `read-key-sequence'.
    (define-key map [?\e] nil)
    map)
  "Used internally by `read-key'.")

(defvar read-key-delay 0.01) ;Fast enough for 100Hz repeat rate, hopefully.

(defun read-key (&optional prompt disable-fallbacks)
  "Read a key from the keyboard, return the event thus read.
Contrary to `read-event' this will not return a raw event but instead will
obey the input decoding and translations usually done by `read-key-sequence'.
So escape sequences and keyboard encoding are taken into account.
When there's an ambiguity because the key looks like the prefix of
some sort of escape sequence, the ambiguity is resolved via `read-key-delay'.

Also in contrast to `read-event', input method text conversion
will be disabled while the key sequence is read, so that
character input events will always be generated for keyboard
input.

If the optional argument PROMPT is non-nil, display that as a
prompt.

If the optional argument DISABLE-FALLBACKS is non-nil, all
unbound fallbacks usually done by `read-key-sequence' are
disabled such as discarding mouse down events.  This is generally
what you want as `read-key' temporarily removes all bindings
while calling `read-key-sequence'.  If nil or unspecified, the
only unbound fallback disabled is downcasing of the last event."
  ;; This overriding-terminal-local-map binding also happens to
  ;; disable quail's input methods, so although read-key-sequence
  ;; always inherits the input method, in practice read-key does not
  ;; inherit the input method (at least not if it's based on quail).
  (let ((overriding-terminal-local-map nil)
	(overriding-local-map
         ;; FIXME: Audit existing uses of `read-key' to see if they
         ;; should always specify disable-fallbacks to be more in line
         ;; with `read-event'.
         (if disable-fallbacks read-key-full-map read-key-empty-map))
        (echo-keystrokes 0)
	(old-global-map (current-global-map))
        (timer (run-with-idle-timer
                ;; Wait long enough that Emacs has the time to receive and
                ;; process all the raw events associated with the single-key.
                ;; But don't wait too long, or the user may find the delay
                ;; annoying (or keep hitting more keys, which may then get
                ;; lost or misinterpreted).
                ;; This is relevant only for keys that Emacs perceives as
                ;; "prefixes", such as C-x (because of the C-x 8 map in
                ;; key-translate-table and the C-x @ map in function-key-map)
                ;; or ESC (because of terminal escape sequences in
                ;; input-decode-map).
                read-key-delay t
                (lambda ()
                  (let ((keys (this-command-keys-vector)))
                    (unless (zerop (length keys))
                      ;; `keys' is non-empty, so the user has hit at least
                      ;; one key; there's no point waiting any longer, even
                      ;; though read-key-sequence thinks we should wait
                      ;; for more input to decide how to interpret the
                      ;; current input.
		      ;;
		      ;; As this treatment will completely defeat the
		      ;; purpose of touch screen event conversion,
		      ;; dispense with this timeout when the first
		      ;; event in this vector is a touch-screen event.
		      (unless (memq (car-safe (aref keys 0)) '(touchscreen-begin
							       touchscreen-update
							       touchscreen-end))
			(throw 'read-key keys))))))))
    (unwind-protect
        (progn
	  (use-global-map
           (let ((map (make-sparse-keymap)))
             ;; Don't hide the menu-bar, tab-bar and tool-bar entries.
             (define-key map [menu-bar] (lookup-key global-map [menu-bar]))
             (define-key map [tab-bar]
	       ;; This hack avoids evaluating the :filter (Bug#9922).
	       (or (cdr (assq 'tab-bar global-map))
		   (lookup-key global-map [tab-bar])))
             (define-key map [tool-bar]
	       ;; This hack avoids evaluating the :filter (Bug#9922).
	       (or (cdr (assq 'tool-bar global-map))
		   (lookup-key global-map [tool-bar])))
             map))
          (let* ((keys
                  (catch 'read-key (read-key-sequence-vector prompt nil t
                                                             nil nil t)))
                 (key (aref keys 0)))
            (if (and (> (length keys) 1)
                     (memq key '(mode-line header-line tab-line
                                 left-fringe right-fringe)))
                (aref keys 1)
              key)))
      (cancel-timer timer)
      ;; For some reason, `read-key(-sequence)' leaves the prompt in the echo
      ;; area, whereas `read-event' seems to empty it just before returning
      ;; (bug#22714).  So, let's mimic the behavior of `read-event'.
      (message nil)
      (use-global-map old-global-map))))

(defvar touch-screen-events-received nil
  "Whether a touch screen event has ever been translated.
The value of this variable governs whether `read--potential-mouse-event'
calls `read-key' or `read-event'.")

;; FIXME: Once there's a safe way to transition away from read-event,
;; callers to this function should be updated to that way and this
;; function should be deleted.
(defun read--potential-mouse-event ()
  "Read an event that might be a mouse event.

This function exists for backward compatibility in code packaged
with Emacs.  Do not call it directly in your own packages."
  ;; `xterm-mouse-mode' events must go through `read-key' as they
  ;; are decoded via `input-decode-map'.
  (if (or xterm-mouse-mode
          ;; If a touch screen is being employed, then mouse events
          ;; are subject to translation as well.
          touch-screen-events-received)
      (read-key nil
                ;; Normally `read-key' discards all mouse button
                ;; down events.  However, we want them here.
                t)
    (read-event)))

(defvar read-number-history nil
  "The default history for the `read-number' function.")

(defun read-number (prompt &optional default hist)
  "Read from the minibuffer and return a numeric value, prompting with PROMPT.
DEFAULT specifies a default value to return if the user just types RET.
For historical reasons, the value of DEFAULT is always inserted into
PROMPT, so it's recommended to use `format' instead of `format-prompt'
to generate PROMPT.  HIST specifies a history list variable.  See
`read-from-minibuffer' for details of the HIST argument.

This function is used by the `interactive' code letter \"n\"."
  (let ((n nil)
	(default1 (if (consp default) (car default) default)))
    (when default1
      (setq prompt
	    (if (string-match "\\(\\):[ \t]*\\'" prompt)
		(replace-match (format minibuffer-default-prompt-format default1) t t prompt 1)
	      (replace-regexp-in-string "[ \t]*\\'"
					(format minibuffer-default-prompt-format default1)
					prompt t t))))
    (while
	(progn
	  (let ((str (read-from-minibuffer
		      prompt nil nil nil (or hist 'read-number-history)
		      (when default
			(if (consp default)
			    (mapcar #'number-to-string (delq nil default))
			  (number-to-string default))))))
	    (condition-case nil
		(setq n (cond
			 ((zerop (length str)) default1)
			 ((stringp str) (read str))))
	      (error nil)))
	  (unless (numberp n)
	    (message "Please enter a number.")
	    (sit-for 1)
	    t)))
    n))

(defvar read-char-choice-use-read-key nil
  "If non-nil, use `read-key' when reading a character by `read-char-choice'.
Otherwise, use the minibuffer (this is the default).

When reading via the minibuffer, you can use the normal commands
available in the minibuffer, and can, for instance, temporarily
switch to another buffer, do things there, and then switch back
to the minibuffer before entering the character.  This is not
possible when using `read-key', but using `read-key' may be less
confusing to some users.")

(defun read-char-choice (prompt chars &optional inhibit-keyboard-quit)
  "Read and return one of the characters in CHARS, prompting with PROMPT.
CHARS should be a list of single characters.
The function discards any input character that is not one of CHARS,
and by default shows a message to the effect that it is not one of
the expected characters.

By default, this function uses the minibuffer to read the key
non-modally (see `read-char-from-minibuffer'), and the optional
argument INHIBIT-KEYBOARD-QUIT is ignored.  However, if
`read-char-choice-use-read-key' is non-nil, the modal `read-key'
function is used instead (see `read-char-choice-with-read-key'),
and INHIBIT-KEYBOARD-QUIT is passed to it."
  (if (not read-char-choice-use-read-key)
      ;; We are about to enter recursive-edit, which sets
      ;; 'last-command'.  If the callers of this function have some
      ;; logic based on 'last-command's value (example: 'kill-region'),
      ;; that could interfere with their logic.  So we let-bind
      ;; 'last-command' here to prevent that.
      (let ((last-command last-command))
        (read-char-from-minibuffer prompt chars))
    (read-char-choice-with-read-key prompt chars inhibit-keyboard-quit)))

(defun read-char-choice-with-read-key (prompt chars &optional inhibit-keyboard-quit)
  "Read and return one of the characters in CHARS, prompting with PROMPT.
CHARS should be a list of single characters.
Any input that is not one of CHARS is ignored.

If optional argument INHIBIT-KEYBOARD-QUIT is non-nil, ignore
`keyboard-quit' events while waiting for valid input.

If you bind the variable `help-form' to a non-nil value
while calling this function, then pressing `help-char'
causes it to evaluate `help-form' and display the result."
  (unless (consp chars)
    (error "Called `read-char-choice' without valid char choices"))
  (let (char done show-help (helpbuf " *Char Help*"))
    (let ((cursor-in-echo-area t)
          (executing-kbd-macro executing-kbd-macro)
	  (esc-flag nil))
      (save-window-excursion	      ; in case we call help-form-show
	(while (not done)
	  (unless (get-text-property 0 'face prompt)
	    (setq prompt (propertize prompt 'face 'minibuffer-prompt)))
          ;; Display the on screen keyboard if it exists.
          (frame-toggle-on-screen-keyboard (selected-frame) nil)
	  (setq char (let ((inhibit-quit inhibit-keyboard-quit))
		       (read-key prompt)))
	  (and show-help (buffer-live-p (get-buffer helpbuf))
	       (kill-buffer helpbuf))
	  (cond
	   ((not (numberp char)))
	   ;; If caller has set help-form, that's enough.
	   ;; They don't explicitly have to add help-char to chars.
	   ((and help-form
		 (eq char help-char)
		 (setq show-help t)
		 (help-form-show)))
	   ((memq char chars)
	    (setq done t))
	   ((not inhibit-keyboard-quit)
	    (cond
	     ((and (null esc-flag) (eq char ?\e))
	      (setq esc-flag t))
	     ((memq char '(?\C-g ?\e))
	      (keyboard-quit))))
	   (t
	    (beep)
	    (message "Please type %s"
		     (substitute-command-keys
		      (mapconcat (lambda (c)
				   (format "\\`%s'"
					   (single-key-description c)))
				 chars ", ")))
	    (sit-for 3))))))
    ;; Display the question with the answer.  But without cursor-in-echo-area.
    (message "%s%s" prompt (char-to-string char))
    char))

(defun sit-for (seconds &optional nodisp)
  "Redisplay, then wait for SECONDS seconds; stop when input is available.
SECONDS may be a floating-point value.
\(On operating systems that do not support waiting for fractions of a
second, floating-point values are rounded down to the nearest integer.)

If there's pending input, return nil immediately without redisplaying
and without waiting.
If optional arg NODISP is t, don't redisplay, just wait for input (but
still return nil immediately if there's pending input).

Value is t if waited the full time with no input arriving, and nil otherwise."
  ;; This used to be implemented in C until the following discussion:
  ;; https://lists.gnu.org/r/emacs-devel/2006-07/msg00401.html
  ;; Then it was moved here using an implementation based on an idle timer,
  ;; which was then replaced by the use of read-event.
  (cond
   (noninteractive
    (sleep-for seconds)
    t)
   ((input-pending-p t)
    nil)
   ((or (<= seconds 0)
        ;; We are going to call read-event below, which will record
        ;; the next key as part of the macro, even if that key
        ;; invokes kmacro-end-macro, so if we are recording a macro,
        ;; the macro will recursively call itself.  In addition, when
        ;; that key is removed from unread-command-events, it will be
        ;; recorded the second time, so the macro will have each key
        ;; doubled.  This used to happen if a macro was defined with
        ;; Flyspell mode active (because Flyspell calls sit-for in its
        ;; post-command-hook, see bug #21329.)  To avoid all that, we
        ;; simply disable the wait when we are recording a macro.
        defining-kbd-macro)
    (or nodisp (redisplay)))
   (t
    (or nodisp (redisplay))
    ;; FIXME: we should not read-event here at all, because it's much too
    ;; difficult to reliably "undo" a read-event by pushing it onto
    ;; unread-command-events.
    ;; For bug#14782, we need read-event to do the keyboard-coding-system
    ;; decoding (hence non-nil as second arg under POSIX ttys).
    ;; For bug#15614, we need read-event not to inherit-input-method.
    ;; So we temporarily suspend input-method-function.
    (let ((read (let ((input-method-function nil))
                  (read-event nil t seconds))))
      (or (null read)
	  (progn
            ;; https://lists.gnu.org/r/emacs-devel/2006-10/msg00394.html
            ;; We want `read' appear in the next command's this-command-event
            ;; but not in the current one.
            ;; By pushing (cons t read), we indicate that `read' has not
            ;; yet been recorded in this-command-keys, so it will be recorded
            ;; next time it's read.
            ;; And indeed the `seconds' argument to read-event correctly
            ;; prevented recording this event in the current command's
            ;; this-command-keys.
	    (push (cons t read) unread-command-events)
	    nil))))))

(defun goto-char--read-natnum-interactive (prompt)
  "Get a natural number argument, optionally prompting with PROMPT.
If there is a natural number at point, use it as default."
  (if (and current-prefix-arg (not (consp current-prefix-arg)))
      (list (prefix-numeric-value current-prefix-arg))
    (let* ((number (number-at-point))
           (default (and (natnump number) number)))
      (list (read-number prompt (list default (point)))))))


(defvar read-char-history nil
  "The default history for the `read-char-from-minibuffer' function.")

(defvar read-char-from-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)

    ;; (define-key map [remap self-insert-command] #'read-char-from-minibuffer-insert-char)
    (define-key map [remap exit-minibuffer] #'read-char-from-minibuffer-insert-other)

    (define-key map [remap recenter-top-bottom] #'minibuffer-recenter-top-bottom)
    (define-key map [remap scroll-up-command] #'minibuffer-scroll-up-command)
    (define-key map [remap scroll-down-command] #'minibuffer-scroll-down-command)
    (define-key map [remap scroll-other-window] #'minibuffer-scroll-other-window)
    (define-key map [remap scroll-other-window-down] #'minibuffer-scroll-other-window-down)

    map)
  "Keymap for the `read-char-from-minibuffer' function.")

(defconst read-char-from-minibuffer-map-hash
  (make-hash-table :test 'equal))

(defun read-char-from-minibuffer-insert-char ()
  "Insert the character you type into the minibuffer and exit minibuffer.
Discard all previous input before inserting and exiting the minibuffer."
  (interactive)
  (when (minibufferp)
    (delete-minibuffer-contents)
    (insert last-command-event)
    (exit-minibuffer)))

(defun read-char-from-minibuffer-insert-other ()
  "Reject a disallowed character typed into the minibuffer.
This command is intended to be bound to keys that users are not
allowed to type into the minibuffer.  When the user types any
such key, this command discard all minibuffer input and displays
an error message."
  (interactive)
  (when (minibufferp) ;;FIXME: Why?
    (delete-minibuffer-contents)
    (ding)
    (discard-input)
    (minibuffer-message "Wrong answer")
    (sit-for 2)))

;; Defined in textconv.c.
(defvar overriding-text-conversion-style)

(defun read-char-from-minibuffer (prompt &optional chars history)
  "Read a character from the minibuffer, prompting for it with PROMPT.
Like `read-char', but uses the minibuffer to read and return a character.
Optional argument CHARS, if non-nil, should be a list of characters;
the function will ignore any input that is not one of CHARS.
Optional argument HISTORY, if non-nil, should be a symbol that
specifies the history list variable to use for navigating in input
history using \\`M-p' and \\`M-n', with \\`RET' to select a character from
history.
If you bind the variable `help-form' to a non-nil value
while calling this function, then pressing `help-char'
causes it to evaluate `help-form' and display the result.
There is no need to explicitly add `help-char' to CHARS;
`help-char' is bound automatically to `help-form-show'."

  ;; If text conversion is enabled in this buffer, then it will only
  ;; be disabled the next time `force-mode-line-update' happens.
  (when (and (bound-and-true-p overriding-text-conversion-style)
             (bound-and-true-p text-conversion-style))
    (force-mode-line-update))

  (let* ((overriding-text-conversion-style nil)
         (map (if (consp chars)
                  (or (gethash (list help-form (cons help-char chars))
                               read-char-from-minibuffer-map-hash)
                      (let ((map (make-sparse-keymap))
                            (msg help-form))
                        (set-keymap-parent map read-char-from-minibuffer-map)
                        ;; If we have a dynamically bound `help-form'
                        ;; here, then the `C-h' (i.e., `help-char')
                        ;; character should output that instead of
                        ;; being a command char.
                        (when help-form
                          (define-key map (vector help-char)
                                      (lambda ()
                                        (interactive)
                                        (let ((help-form msg)) ; lexically bound msg
                                          (help-form-show)))))
                        ;; FIXME: We use `read-char-from-minibuffer-insert-char'
                        ;; here only as a kind of alias of `self-insert-command'
                        ;; to prevent those keys from being remapped to
                        ;; `read-char-from-minibuffer-insert-other'.
                        (dolist (char chars)
                          (define-key map (vector char)
                                      #'read-char-from-minibuffer-insert-char))
                        (define-key map [remap self-insert-command]
                                    #'read-char-from-minibuffer-insert-other)
                        (puthash (list help-form (cons help-char chars))
                                 map read-char-from-minibuffer-map-hash)
                        map))
                read-char-from-minibuffer-map))
         ;; Protect this-command when called from pre-command-hook (bug#45029)
         (this-command this-command)
         (result (minibuffer-with-setup-hook
		     (lambda ()
		       (setq-local post-self-insert-hook nil)
		       (add-hook 'post-command-hook
				 (lambda ()
				   (if (<= (1+ (minibuffer-prompt-end))
					  (point-max))
                                       (exit-minibuffer)))
				 nil 'local))
                   ;; Disable text conversion if it is enabled.
                   ;; (bug#65370)
                   (when (fboundp 'set-text-conversion-style)
                     (set-text-conversion-style text-conversion-style))
                   (read-from-minibuffer prompt nil map nil (or history t))))
         (char
          (if (plusp (length result))
              ;; We have a string (with one character), so return the first one.
              (elt result 0)
            ;; The default value is RET.
            (when history (push "\r" (symbol-value history)))
            ?\r)))
    ;; Display the question with the answer.
    (message "%s%s" prompt (char-to-string char))
    char))


;; Behind display-popup-menus-p test.
(declare-function x-popup-dialog "menu.c" (position contents &optional header))

(defvar y-or-n-p-history-variable nil
  "History list symbol to add `y-or-n-p' answers to.")

(defvar y-or-n-p-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)

    (dolist (symbol '(act act-and-show act-and-exit automatic))
      (define-key map (vector 'remap symbol) #'y-or-n-p-insert-y))

    (define-key map [remap skip] #'y-or-n-p-insert-n)

    (dolist (symbol '(backup undo undo-all edit edit-replacement
                      delete-and-edit ignore self-insert-command))
      (define-key map (vector 'remap symbol) #'y-or-n-p-insert-other))

    (define-key map [remap recenter] #'minibuffer-recenter-top-bottom)
    (define-key map [remap scroll-up] #'minibuffer-scroll-up-command)
    (define-key map [remap scroll-down] #'minibuffer-scroll-down-command)
    (define-key map [remap scroll-other-window] #'minibuffer-scroll-other-window)
    (define-key map [remap scroll-other-window-down] #'minibuffer-scroll-other-window-down)

    (define-key map [remap exit] #'y-or-n-p-insert-other)
    (dolist (symbol '(exit-prefix quit))
      (define-key map (vector 'remap symbol) #'abort-recursive-edit))
    (define-key map [escape] #'abort-recursive-edit)

    ;; FIXME: try catch-all instead of explicit bindings:
    ;; (define-key map [remap t] #'y-or-n-p-insert-other)

    map)
  "Keymap that defines additional bindings for `y-or-n-p' answers.")

(defun y-or-n-p-insert-y ()
  "Insert the answer \"y\" and exit the minibuffer of `y-or-n-p'.
Discard all previous input before inserting and exiting the minibuffer."
  (interactive)
  (when (minibufferp)
    (delete-minibuffer-contents)
    (insert "y")
    (exit-minibuffer)))

(defun y-or-n-p-insert-n ()
  "Insert the answer \"n\" and exit the minibuffer of `y-or-n-p'.
Discard all previous input before inserting and exiting the minibuffer."
  (interactive)
  (when (minibufferp)
    (delete-minibuffer-contents)
    (insert "n")
    (exit-minibuffer)))

(defun y-or-n-p-insert-other ()
  "Handle inserting of other answers in the minibuffer of `y-or-n-p'.
Display an error on trying to insert a disallowed character.
Also discard all previous input in the minibuffer."
  (interactive)
  (when (minibufferp)
    (delete-minibuffer-contents)
    (ding)
    (discard-input)
    (minibuffer-message "Please answer y or n")
    (sit-for 2)))

(defvar y-or-n-p-use-read-key nil
  "Use `read-key' when reading answers to \"y or n\" questions by `y-or-n-p'.
Otherwise, use the `read-from-minibuffer' to read the answers.

When reading via the minibuffer, you can use the normal commands
available in the minibuffer, and can, for instance, temporarily
switch to another buffer, do things there, and then switch back
to the minibuffer before entering the character.  This is not
possible when using `read-key', but using `read-key' may be less
confusing to some users.")

(defvar from--tty-menu-p nil
  "Non-nil means the current command was invoked from a TTY menu.")

(declare-function android-detect-keyboard "androidfns.c")

(defvar use-dialog-box-override nil
  "Whether `use-dialog-box-p' should always return t.")

(defun use-dialog-box-p ()
  "Return non-nil if the current command should prompt the user via a dialog box."
  (or use-dialog-box-override
      (and last-input-event                 ; not during startup
           (or (consp last-nonmenu-event)   ; invoked by a mouse event
               (and (null last-nonmenu-event)
                    (consp last-input-event))
               (and (featurep 'android)	; Prefer dialog boxes on
                                        ; Android.
                    (not (android-detect-keyboard))) ; If no keyboard is
                                                     ; connected.
               from--tty-menu-p)            ; invoked via TTY menu
           use-dialog-box)))

(defun y-or-n-p (prompt)
  "Ask user a \"y or n\" question.
Return t if answer is \"y\" and nil if it is \"n\".

PROMPT is the string to display to ask the question; `y-or-n-p'
adds \"(y or n) \" to it.  If PROMPT is a non-empty string, and
it ends with a non-space character, a space character will be
appended to it.

If you bind the variable `help-form' to a non-nil value
while calling this function, then pressing `help-char'
causes it to evaluate `help-form' and display the result.
PROMPT is also updated to show `help-char' like \"(y, n or C-h) \",
where `help-char' is automatically bound to `help-form-show'.

No confirmation of the answer is requested; a single character is
enough.  SPC also means yes, and DEL means no.

To be precise, this function translates user input into responses
by consulting the bindings in `query-replace-map'; see the
documentation of that variable for more information.  In this
case, the useful bindings are `act', `skip', `recenter',
`scroll-up', `scroll-down', and `quit'.
An `act' response means yes, and a `skip' response means no.
A `quit' response means to invoke `abort-recursive-edit'.
If the user enters `recenter', `scroll-up', or `scroll-down'
responses, perform the requested window recentering or scrolling
and ask again.

If dialog boxes are supported, this function will use a dialog box
if `use-dialog-box' is non-nil and the last input event was produced
by a mouse, or by some window-system gesture, or via a menu.

By default, this function uses the minibuffer to read the key.
If `y-or-n-p-use-read-key' is non-nil, `read-key' is used
instead (which means that the user can't change buffers (and the
like) while `y-or-n-p' is running)."
  (let ((answer 'recenter)
	(padded (lambda (prompt &optional dialog)
		  (let ((l (length prompt)))
		    (concat prompt
			    (if (or (zerop l) (eq ?\s (aref prompt (1- l))))
				"" " ")
			    (if dialog ""
                              ;; Don't clobber caller's match data.
                              (save-match-data
                                (substitute-command-keys
                                 (if help-form
                                     (format "(\\`y', \\`n' or \\`%s') "
                                             (key-description
                                              (vector help-char)))
                                   "(\\`y' or \\`n') "))))))))
        ;; Preserve the actual command that eventually called
        ;; `y-or-n-p' (otherwise `repeat' will be repeating
        ;; `exit-minibuffer').
        (real-this-command real-this-command))
    (cond
     (noninteractive
      (setq prompt (funcall padded prompt))
      (let ((temp-prompt prompt))
	(while (not (memq answer '(act skip)))
	  (let ((str (read-string temp-prompt)))
	    (cond ((member str '("y" "Y")) (setq answer 'act))
		  ((member str '("n" "N")) (setq answer 'skip))
		  ((and (member str '("h" "H")) help-form) (print help-form))
		  (t (setq temp-prompt (concat "Please answer y or n.  "
					       prompt))))))))
     ((use-dialog-box-p)
      (setq prompt (funcall padded prompt t)
	    answer (x-popup-dialog t `(,prompt ("Yes" . act) ("No" . skip)))))
     (y-or-n-p-use-read-key
      ;; ¡Beware! when I tried to edebug this code, Emacs got into a weird state
      ;; where all the keys were unbound (i.e. it somehow got triggered
      ;; within read-key, apparently).  I had to kill it.
      (setq prompt (funcall padded prompt))
      (while
          (let* ((scroll-actions '(recenter scroll-up scroll-down
                                            scroll-other-window scroll-other-window-down))
                 (key
                  (let ((cursor-in-echo-area t))
                    (when minibuffer-auto-raise
                      (raise-frame (window-frame (minibuffer-window))))
                    (read-key (propertize (if (memq answer scroll-actions)
                                              prompt
                                            (concat "Please answer y or n.  "
                                                    prompt))
                                          'face 'minibuffer-prompt)))))
            (setq answer (lookup-key query-replace-map (vector key) t))
            (cond
             ((memq answer '(skip act)) nil)
             ((eq answer 'recenter)
              (recenter) t)
             ((eq answer 'scroll-up)
              (ignore-errors (scroll-up-command)) t)
             ((eq answer 'scroll-down)
              (ignore-errors (scroll-down-command)) t)
             ((eq answer 'scroll-other-window)
              (ignore-errors (scroll-other-window)) t)
             ((eq answer 'scroll-other-window-down)
              (ignore-errors (scroll-other-window-down)) t)
             ((or (memq answer '(exit-prefix quit)) (eq key ?\e))
              (signal 'quit nil) t)
             (t t)))
        (ding)
        (discard-input)))
     (t
      (setq prompt (funcall padded prompt))
      (let* ((enable-recursive-minibuffers t)
             (msg help-form)
             ;; Disable text conversion so that real Y or N events are
             ;; sent.
             (overriding-text-conversion-style nil)
             (keymap (let ((map (make-composed-keymap
                                 y-or-n-p-map query-replace-map)))
                       (when help-form
                         ;; Create a new map before modifying
                         (setq map (copy-keymap map))
                         (define-key map (vector help-char)
                           (lambda ()
                             (interactive)
                             (let ((help-form msg)) ; lexically bound msg
                               (help-form-show)))))
                       map))
             ;; Protect this-command when called from pre-command-hook (bug#45029)
             (this-command this-command)
             (str (progn
                    ;; If the minibuffer is already active, the
                    ;; selected window might not change.  Disable
                    ;; text conversion by hand.
                    (when (fboundp 'set-text-conversion-style)
                      (set-text-conversion-style text-conversion-style))
                    (read-from-minibuffer
                     prompt nil keymap nil
                     (or y-or-n-p-history-variable t)))))
        (setq answer (if (member str '("y" "Y")) 'act 'skip)))))
    (let ((ret (eq answer 'act)))
      (unless noninteractive
        (message "%s%c" prompt (if ret ?y ?n)))
      ret)))


;;; Atomic change groups.

(defmacro atomic-change-group (&rest body)
  "Like `progn' but perform BODY as an atomic change group.
This means that if BODY exits abnormally,
all of its changes to the current buffer are undone.
This works regardless of whether undo is enabled in the buffer.

Do not call functions which edit the undo list within BODY; see
`prepare-change-group'.

This mechanism is transparent to ordinary use of undo;
if undo is enabled in the buffer and BODY succeeds, the
user can undo the change normally."
  (declare (indent 0) (debug t))
  (let ((handle (make-symbol "--change-group-handle--"))
	(success (make-symbol "--change-group-success--")))
    `(let ((,handle (prepare-change-group))
	   ;; Don't truncate any undo data in the middle of this.
	   (undo-outer-limit nil)
	   (undo-limit most-positive-fixnum)
	   (undo-strong-limit most-positive-fixnum)
	   (,success nil))
       (unwind-protect
	   (progn
	     ;; This is inside the unwind-protect because
	     ;; it enables undo if that was disabled; we need
	     ;; to make sure that it gets disabled again.
	     (activate-change-group ,handle)
	     (prog1 ,(macroexp-progn body)
	       (setq ,success t)))
	 ;; Either of these functions will disable undo
	 ;; if it was disabled before.
	 (if ,success
	     (accept-change-group ,handle)
	   (cancel-change-group ,handle))))))

(defmacro with-undo-amalgamate (&rest body)
  "Like `progn' but perform BODY with amalgamated undo barriers.

This allows multiple operations to be undone in a single step.
When undo is disabled this behaves like `progn'."
  (declare (indent 0) (debug t))
  (let ((handle (make-symbol "--change-group-handle--")))
    `(let ((,handle (prepare-change-group))
           ;; Don't truncate any undo data in the middle of this,
           ;; otherwise Emacs might truncate part of the resulting
           ;; undo step: we want to mimic the behavior we'd get if the
           ;; undo-boundaries were never added in the first place.
           (undo-outer-limit nil)
           (undo-limit most-positive-fixnum)
           (undo-strong-limit most-positive-fixnum))
       (unwind-protect
           (progn
             (activate-change-group ,handle)
             ,@body)
         (progn
           (accept-change-group ,handle)
           (undo-amalgamate-change-group ,handle))))))

(defun prepare-change-group (&optional buffer)
  "Return a handle for the current buffer's state, for a change group.
If you specify BUFFER, make a handle for BUFFER's state instead.

Pass the handle to `activate-change-group' afterward to initiate
the actual changes of the change group.

To finish the change group, call either `accept-change-group' or
`cancel-change-group' passing the same handle as argument.  Call
`accept-change-group' to accept the changes in the group as final;
call `cancel-change-group' to undo them all.  You should use
`unwind-protect' to make sure the group is always finished.  The call
to `activate-change-group' should be inside the `unwind-protect'.
Once you finish the group, don't use the handle again--don't try to
finish the same group twice.  For a simple example of correct use, see
the source code of `atomic-change-group'.

As long as this handle is still in use, do not call functions
which edit the undo list: if it no longer contains its current
value, Emacs will not be able to cancel the change group.  This
includes any \"amalgamating\" commands, such as `delete-char',
which call `undo-auto-amalgamate'.

The handle records only the specified buffer.  To make a multibuffer
change group, call this function once for each buffer you want to
cover, then use `nconc' to combine the returned values, like this:

  (nconc (prepare-change-group buffer-1)
         (prepare-change-group buffer-2))

You can then activate that multibuffer change group with a single
call to `activate-change-group' and finish it with a single call
to `accept-change-group' or `cancel-change-group'."

  (if buffer
      (list (cons buffer (with-current-buffer buffer buffer-undo-list)))
    (list (cons (current-buffer) buffer-undo-list))))

(defun activate-change-group (handle)
  "Activate a change group made with `prepare-change-group' (which see)."
  (dolist (elt handle)
    (with-current-buffer (car elt)
      (if (eq buffer-undo-list t)
	  (setq buffer-undo-list nil)
	;; Add a boundary to make sure the upcoming changes won't be
	;; merged/combined with any previous changes (bug#33341).
	;; We're not supposed to introduce a real (visible)
        ;; `undo-boundary', tho, so we have to push something else
        ;; that acts like a boundary w.r.t preventing merges while
	;; being harmless.
        ;; We use for that an "empty insertion", but in order to be harmless,
        ;; it has to be at a harmless position.  Currently only
        ;; insertions are ever merged/combined, so we use such a "boundary"
        ;; only when the last change was an insertion and we use the position
        ;; of the last insertion.
        (when (numberp (car-safe (car buffer-undo-list)))
          (push (cons (caar buffer-undo-list) (caar buffer-undo-list))
                buffer-undo-list))))))

(defun accept-change-group (handle)
  "Finish a change group made with `prepare-change-group' (which see).
This finishes the change group by accepting its changes as final."
  (dolist (elt handle)
    (with-current-buffer (car elt)
      (if (eq (cdr elt) t)
	  (setq buffer-undo-list t)))))

(defun cancel-change-group (handle)
  "Finish a change group made with `prepare-change-group' (which see).
This finishes the change group by reverting all of its changes."
  (dolist (elt handle)
    (with-current-buffer (car elt)
      (setq elt (cdr elt))
      (save-restriction
	;; Widen buffer temporarily so if the buffer was narrowed within
	;; the body of `atomic-change-group' all changes can be undone.
	(widen)
	(let ((old-car (car-safe elt))
	      (old-cdr (cdr-safe elt))
	      ;; Use `pending-undo-list' temporarily since `undo-more' needs
	      ;; it, but restore it afterwards so as not to mess with an
	      ;; ongoing sequence of `undo's.
	      (pending-undo-list
	       ;; Use `buffer-undo-list' unconditionally (bug#39680).
	       buffer-undo-list))
          (unwind-protect
              (progn
                ;; Temporarily truncate the undo log at ELT.
                (when (consp elt)
                  (setcar elt nil) (setcdr elt nil))
                ;; Make sure there's no confusion.
                (when (and (consp elt) (not (eq elt (last pending-undo-list))))
                  (error "Undoing to some unrelated state"))
                ;; Undo it all.
                (save-excursion
                  (while (listp pending-undo-list) (undo-more 1)))
                ;; Revert the undo info to what it was when we grabbed
                ;; the state.
                (setq buffer-undo-list elt))
            ;; Reset the modified cons cell ELT to its original content.
            (when (consp elt)
              (setcar elt old-car)
              (setcdr elt old-cdr))))))))

;;;; Display-related functions.

(defun momentary-string-display (string pos &optional exit-char message)
  "Momentarily display STRING in the buffer at POS.
Display remains until next event is input.
If POS is a marker, only its position is used; its buffer is ignored.
Optional third arg EXIT-CHAR can be a character, event or event
description list.  EXIT-CHAR defaults to SPC.  If the input is
EXIT-CHAR it is swallowed; otherwise it is then available as
input (as a command if nothing else).
Display MESSAGE (optional fourth arg) in the echo area.
If MESSAGE is nil, instructions to type EXIT-CHAR are displayed there."
  (or exit-char (setq exit-char ?\s))
  (let ((ol (make-overlay pos pos))
        (str (copy-sequence string)))
    (unwind-protect
        (progn
          (save-excursion
            (overlay-put ol 'after-string str)
            (goto-char pos)
            ;; To avoid trouble with out-of-bounds position
            (setq pos (point))
            ;; If the string end is off screen, recenter now.
            (if (<= (window-end nil t) pos)
                (recenter (/ (window-height) 2))))
          (message (or message "Type %s to continue editing.")
                   (single-key-description exit-char))
	  (let ((event (read-key)))
	    ;; `exit-char' can be an event, or an event description list.
	    (or (eq event exit-char)
		(eq event (event-convert-list exit-char))
		(setq unread-command-events
                      (append (this-single-command-raw-keys)
                              unread-command-events)))))
      (delete-overlay ol))))


;;;; Overlay operations

(defun copy-overlay (o)
  "Return a copy of overlay O."
  (declare (important-return-value t))
  (let ((o1 (if (overlay-buffer o)
                (make-overlay (overlay-start o) (overlay-end o)
                              ;; FIXME: there's no easy way to find the
                              ;; insertion-type of overlay's start and end.
                              (overlay-buffer o))
              (let ((o1 (make-overlay (point-min) (point-min))))
                (delete-overlay o1)
                o1)))
	(props (overlay-properties o)))
    (while props
      (overlay-put o1 (pop props) (pop props)))
    o1))

(defun remove-overlays (&optional beg end name val)
  "Remove overlays between BEG and END that have property NAME with value VAL.
Overlays might be moved and/or split.  If any targeted overlays
start before BEG, the overlays will be altered so that they end
at BEG.  Likewise, if the targeted overlays end after END, they
will be altered so that they start at END.  Overlays that start
at or after BEG and end before END will be removed completely.

BEG and END default respectively to the beginning and end of the
buffer.
Values are compared with `eq'.
If either NAME or VAL are specified, both should be specified."
  ;; This speeds up the loops over overlays.
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (overlay-recenter end)
  (if (< end beg)
      (setq beg (prog1 end (setq end beg))))
  (save-excursion
    (dolist (o (overlays-in beg end))
      (when (eq (overlay-get o name) val)
	;; Either push this overlay outside beg...end
	;; or split it to exclude beg...end
	;; or delete it entirely (if it is contained in beg...end).
	(if (< (overlay-start o) beg)
	    (if (> (overlay-end o) end)
		(progn
		  (move-overlay (copy-overlay o)
				(overlay-start o) beg)
		  (move-overlay o end (overlay-end o)))
	      (move-overlay o (overlay-start o) beg))
	  (if (> (overlay-end o) end)
	      (move-overlay o end (overlay-end o))
	    (delete-overlay o)))))))

;;;; Miscellanea.

(defvar suspend-hook nil
  "Normal hook run by `suspend-emacs', before suspending.")

(defvar suspend-resume-hook nil
  "Normal hook run by `suspend-emacs', after Emacs is continued.")

(defvar after-pdump-load-hook nil
  "Normal hook run after loading the .pdmp file.")

(defvar temp-buffer-show-hook nil
  "Normal hook run by `with-output-to-temp-buffer' after displaying the buffer.
When the hook runs, the temporary buffer is current, and the window it
was displayed in is selected.")

(defvar temp-buffer-setup-hook nil
  "Normal hook run by `with-output-to-temp-buffer' at the start.
When the hook runs, the temporary buffer is current.
This hook is normally set up with a function to put the buffer in Help
mode.")

(defvar user-emacs-directory
  ;; The value does not matter since Emacs sets this at startup.
  nil
  "Directory beneath which additional per-user Emacs-specific files are placed.
Various programs in Emacs store information in this directory.
Note that this should end with a directory separator.
See also `locate-user-emacs-file'.")

;;;; Misc. useful functions.

(defsubst buffer-narrowed-p ()
  "Return non-nil if the current buffer is narrowed."
  (declare (side-effect-free t))
  (/= (- (point-max) (point-min)) (buffer-size)))

(defmacro with-restriction (start end &rest rest)
  "Execute BODY with restrictions set to START and END.

The current restrictions, if any, are restored upon return.

When the optional LABEL argument, which is evaluated to get the
label to use and must yield a non-nil value, is present, inside
BODY, `narrow-to-region' and `widen' can be used only within the
START and END limits.  To gain access to other portions of the
buffer, use `without-restriction' with the same LABEL argument.

\(fn START END [:label LABEL] BODY)"
  (declare (indent 2) (debug t))
  (if (eq (car rest) :label)
      `(save-restriction
         (internal--labeled-narrow-to-region ,start ,end ,(cadr rest))
         ,@(cddr rest))
    `(save-restriction (narrow-to-region ,start ,end) ,@rest)))

(defmacro without-restriction (&rest rest)
  "Execute BODY without restrictions.

The current restrictions, if any, are restored upon return.

When the optional LABEL argument is present, the restrictions set
by `with-restriction' with the same LABEL argument are lifted.

\(fn [:label LABEL] BODY)"
  (declare (indent 0) (debug t))
  (if (eq (car rest) :label)
      `(save-restriction (internal--labeled-widen ,(cadr rest)) ,@(cddr rest))
    `(save-restriction (widen) ,@rest)))

(defun find-tag-default-bounds ()
  "Determine the boundaries of the default tag, based on text at point.
Return a cons cell with the beginning and end of the found tag.
If there is no plausible default, return nil."
  (bounds-of-thing-at-point 'symbol))

(defun find-tag-default ()
  "Determine default tag to search for, based on text at point.
If there is no plausible default, return nil."
  (let ((bounds (find-tag-default-bounds)))
    (when bounds
      (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(defun find-tag-default-as-regexp ()
  "Return regexp that matches the default tag at point.
If there is no tag at point, return nil.

When in a major mode that does not provide its own
`find-tag-default-function', return a regexp that matches the
symbol at point exactly."
  (let ((tag (funcall (or find-tag-default-function
			  (get major-mode 'find-tag-default-function)
			  #'find-tag-default))))
    (if tag (regexp-quote tag))))

(defun find-tag-default-as-symbol-regexp ()
  "Return regexp that matches the default tag at point as symbol.
If there is no tag at point, return nil.

When in a major mode that does not provide its own
`find-tag-default-function', return a regexp that matches the
symbol at point exactly."
  (let ((tag-regexp (find-tag-default-as-regexp)))
    (if (and tag-regexp
	     (eq (or find-tag-default-function
		     (get major-mode 'find-tag-default-function)
		     #'find-tag-default)
		 #'find-tag-default))
	(format "\\_<%s\\_>" tag-regexp)
      tag-regexp)))

(defun play-sound (sound)
  "SOUND is a list of the form `(sound KEYWORD VALUE...)'.
The following keywords are recognized:

  :file FILE - read sound data from FILE.  If FILE isn't an
absolute file name, it is searched in `data-directory'.

  :data DATA - read sound data from string DATA.

Exactly one of :file or :data must be present.

  :volume VOL - set volume to VOL.  VOL must an integer in the
range 0..100 or a float in the range 0..1.0.  If not specified,
don't change the volume setting of the sound device.

  :device DEVICE - play sound on DEVICE.  If not specified,
a system-dependent default device name is used.

Note: :device is currently not supported on Windows."
  (if (fboundp 'play-sound-internal)
      (play-sound-internal sound)
    (error "This Emacs binary lacks sound support")))

(declare-function w32-shell-dos-semantics "w32-fns" nil)

(defun shell-quote-argument (argument &optional posix)
  "Quote ARGUMENT for passing as argument to an inferior shell.

This function is designed to work with the syntax of your system's
standard shell, and might produce incorrect results with unusual shells.
See Info node `(elisp)Security Considerations'.

If the optional POSIX argument is non-nil, ARGUMENT is quoted
according to POSIX shell quoting rules, regardless of the
system's shell."
  (declare (important-return-value t))
  (cond
   ((and (not posix) (eq system-type 'ms-dos))
    ;; Quote using double quotes, but escape any existing quotes in
    ;; the argument with backslashes.
    (let ((result "")
          (start 0)
          end)
      (if (or (null (string-match "[^\"]" argument))
              (< (match-end 0) (length argument)))
          (while (string-match "[\"]" argument start)
            (setq end (match-beginning 0)
                  result (concat result (substring argument start end)
                                 "\\" (substring argument end (1+ end)))
                  start (1+ end))))
      (concat "\"" result (substring argument start) "\"")))

   ((and (not posix) (eq system-type 'windows-nt) (w32-shell-dos-semantics))

    ;; First, quote argument so that CommandLineToArgvW will
    ;; understand it.  See
    ;; https://msdn.microsoft.com/en-us/library/17w5ykft%28v=vs.85%29.aspx
    ;; After we perform that level of quoting, escape shell
    ;; metacharacters so that cmd won't mangle our argument.  If the
    ;; argument contains no double quote characters, we can just
    ;; surround it with double quotes.  Otherwise, we need to prefix
    ;; each shell metacharacter with a caret.

    (setq argument
          ;; escape backslashes at end of string
          (replace-regexp-in-string
           "\\(\\\\*\\)$"
           "\\1\\1"
           ;; escape backslashes and quotes in string body
           (replace-regexp-in-string
            "\\(\\\\*\\)\""
            "\\1\\1\\\\\""
            argument)))

    (if (string-match "[%!\"]" argument)
        (concat
         "^\""
         (replace-regexp-in-string
          "\\([%!()\"<>&|^]\\)"
          "^\\1"
          argument)
         "^\"")
      (concat "\"" argument "\"")))

   (t
    (if (equal argument "")
        "''"
      ;; Quote everything except POSIX filename characters.
      ;; This should be safe enough even for really weird shells.
      (string-replace
       "\n" "'\n'"
       (replace-regexp-in-string "[^-0-9a-zA-Z_./\n]" "\\\\\\&" argument))))
   ))

(defsubst string-to-list (string)
  "Return a list of characters in STRING."
  (declare (side-effect-free t))
  (append string nil))

(defsubst string-to-vector (string)
  "Return a vector of characters in STRING."
  (declare (side-effect-free t))
  (vconcat string))

(defun string-or-null-p (object)
  "Return t if OBJECT is a string or nil.
Otherwise, return nil."
  (declare (pure t) (side-effect-free error-free))
  (or (stringp object) (null object)))

(defun list-of-strings-p (object)
  "Return t if OBJECT is nil or a list of strings."
  (declare (pure t) (side-effect-free error-free))
  (while (and (consp object) (stringp (car object)))
    (setq object (cdr object)))
  (null object))

(defun booleanp (object)
  "Return t if OBJECT is one of the two canonical boolean values: t or nil.
Otherwise, return nil."
  (declare (pure t) (side-effect-free error-free))
  (and (memq object '(nil t)) t))

(defun special-form-p (object)
  "Non-nil if and only if OBJECT is a special form."
  (declare (side-effect-free error-free))
  (if (symbolp object) (setq object (indirect-function object)))
  (and (subrp object) (eq (cdr (subr-arity object)) 'unevalled)))

(defun plistp (object)
  "Non-nil if and only if OBJECT is a valid plist."
  (declare (pure t) (side-effect-free error-free))
  (let ((len (proper-list-p object)))
    (and len (zerop (% len 2)))))

(defun macrop (object)
  "Non-nil if and only if OBJECT is a macro."
  (declare (side-effect-free t))
  (let ((def (indirect-function object)))
    (when (consp def)
      (or (eq 'macro (car def))
          (and (autoloadp def) (memq (nth 4 def) '(macro t)))))))

(defun compiled-function-p (object)
  "Return non-nil if OBJECT is a function that has been compiled.
Does not distinguish between functions implemented in machine code
or byte-code."
  (declare (side-effect-free error-free))
  (or (and (subrp object) (not (eq 'unevalled (cdr (subr-arity object)))))
      (byte-code-function-p object)))

(defun integer-or-null-p (object)
  "Return non-nil if OBJECT is either an integer or nil.
Otherwise, return nil."
  (declare (pure t) (side-effect-free error-free))
  (or (integerp object) (null object)))

(defun field-at-pos (pos)
  "Return the field at position POS, taking stickiness etc into account."
  (declare (important-return-value t))
  (let ((raw-field (get-char-property (field-beginning pos) 'field)))
    (if (eq raw-field 'boundary)
	(get-char-property (1- (field-end pos)) 'field)
      raw-field)))

(defun sha1 (object &optional start end binary)
  "Return the SHA-1 (Secure Hash Algorithm) of an OBJECT.
OBJECT is either a string or a buffer.  Optional arguments START and
END are character positions specifying which portion of OBJECT for
computing the hash.  If BINARY is non-nil, return a 20-byte unibyte
string; otherwise return a 40-character string.

Note that SHA-1 is not collision resistant and should not be used
for anything security-related.  See `secure-hash' for
alternatives."
  (declare (side-effect-free t))
  (secure-hash 'sha1 object start end binary))

(defun function-get (f prop &optional autoload)
  "Return the value of property PROP of function F.
If AUTOLOAD is non-nil and F is autoloaded, try to load it
in the hope that it will set PROP.  If AUTOLOAD is `macro', do it only
if it's an autoloaded macro."
  (declare (important-return-value t))
  (let ((val nil))
    (while (and (symbolp f)
                (null (setq val (get f prop)))
                (fboundp f))
      (let ((fundef (symbol-function f)))
        (if (and autoload (autoloadp fundef)
                 (not (equal fundef
                             (autoload-do-load fundef f
                                               (if (eq autoload 'macro)
                                                   'macro)))))
            nil                         ;Re-try `get' on the same `f'.
          (setq f fundef))))
    val))

;;;; Support for yanking and text properties.
;; Why here in subr.el rather than in simple.el?  --Stef

(defvar yank-handled-properties)
(defvar yank-excluded-properties)

(defun remove-yank-excluded-properties (start end)
  "Process text properties between START and END, inserted for a `yank'.
Perform the handling specified by `yank-handled-properties', then
remove properties specified by `yank-excluded-properties'."
  (let ((inhibit-read-only t))
    (dolist (handler yank-handled-properties)
      (let ((prop (car handler))
            (fun  (cdr handler))
            (run-start start))
        (while (< run-start end)
          (let ((value (get-text-property run-start prop))
                (run-end (next-single-property-change
                          run-start prop nil end)))
            (funcall fun value run-start run-end)
            (setq run-start run-end)))))
    (if (eq yank-excluded-properties t)
        (set-text-properties start end nil)
      (remove-list-of-text-properties start end yank-excluded-properties))))

(defvar yank-undo-function)

(defun insert-for-yank (string)
  "Insert STRING at point for the `yank' command.

This function is like `insert', except it honors the variables
`yank-handled-properties' and `yank-excluded-properties', and the
`yank-handler' text property, in the way that `yank' does.

It also runs the string through `yank-transform-functions'."
  ;; Allow altering the yank string.
  (run-hook-wrapped 'yank-transform-functions
                    (lambda (f) (setq string (funcall f string)) nil))
  (let (to)
    (while (setq to (next-single-property-change 0 'yank-handler string))
      (insert-for-yank-1 (substring string 0 to))
      (setq string (substring string to))))
  (insert-for-yank-1 string))

(defun insert-for-yank-1 (string)
  "Helper for `insert-for-yank', which see."
  (let* ((handler (and (stringp string)
		       (get-text-property 0 'yank-handler string)))
	 (param (or (nth 1 handler) string))
	 (opoint (point))
	 end)

    ;; FIXME: This throws away any yank-undo-function set by previous calls
    ;; to insert-for-yank-1 within the loop of insert-for-yank!
    (setq yank-undo-function t)
    (if (nth 0 handler) ; FUNCTION
	(funcall (car handler) param)
      (insert param))
    (setq end (point))

    (with-silent-modifications
      (unless (nth 2 handler)           ; NOEXCLUDE
        (remove-yank-excluded-properties opoint end))

      ;; If last inserted char has properties, mark them as rear-nonsticky.
      (if (and (> end opoint)
	       (text-properties-at (1- end)))
	  (put-text-property (1- end) end 'rear-nonsticky t)))

    (if (eq yank-undo-function t)		   ; not set by FUNCTION
	(setq yank-undo-function (nth 3 handler))) ; UNDO
    (if (nth 4 handler)				   ; COMMAND
	(setq this-command (nth 4 handler)))))

(defun insert-buffer-substring-no-properties (buffer &optional start end)
  "Insert before point a substring of BUFFER, without text properties.
BUFFER may be a buffer or a buffer name.
Arguments START and END are character positions specifying the substring.
They default to the values of (point-min) and (point-max) in BUFFER."
  (let ((opoint (point)))
    (insert-buffer-substring buffer start end)
    (let ((inhibit-read-only t))
      (set-text-properties opoint (point) nil))))

(defun insert-buffer-substring-as-yank (buffer &optional start end)
  "Insert before point a part of BUFFER, stripping some text properties.
BUFFER may be a buffer or a buffer name.
Arguments START and END are character positions specifying the substring.
They default to the values of (point-min) and (point-max) in BUFFER.
Before insertion, process text properties according to
`yank-handled-properties' and `yank-excluded-properties'."
  ;; Since the buffer text should not normally have yank-handler properties,
  ;; there is no need to handle them here.
  (let ((opoint (point)))
    (insert-buffer-substring buffer start end)
    (remove-yank-excluded-properties opoint (point))))

(defun insert-into-buffer (buffer &optional start end)
  "Insert the contents of the current buffer into BUFFER.
If START/END, only insert that region from the current buffer.
Point in BUFFER will be placed after the inserted text."
  (let ((current (current-buffer)))
    (with-current-buffer buffer
      (insert-buffer-substring current start end))))

(defun replace-buffer-contents (source &optional max-secs max-costs)
  "Replace accessible portion of current buffer with that of SOURCE.
SOURCE can be a buffer or a string that names a buffer.
Interactively, prompt for SOURCE.

The replacement is performed using `replace-region-contents'
which also describes the MAX-SECS and MAX-COSTS arguments and the
return value."
  (declare (obsolete replace-region-contents "31.1"))
  (interactive "bSource buffer: ")
  (replace-region-contents (point-min) (point-max) (get-buffer source)
                           max-secs max-costs))

(defun replace-string-in-region (string replacement &optional start end)
  "Replace STRING with REPLACEMENT in the region from START to END.
The number of replaced occurrences are returned, or nil if STRING
doesn't exist in the region.

If START is nil, use the current point.  If END is nil, use `point-max'.

Comparisons and replacements are done with fixed case."
  (if start
      (when (< start (point-min))
        (error "Start before start of buffer"))
    (setq start (point)))
  (if end
      (when (> end (point-max))
        (error "End after end of buffer"))
    (setq end (point-max)))
  (save-excursion
    (goto-char start)
    (save-restriction
      (narrow-to-region start end)
      (let ((matches 0)
            (case-fold-search nil))
        (while (search-forward string nil t)
          (replace-region-contents (match-beginning 0) (match-end 0)
                                   replacement 0)
          (setq matches (1+ matches)))
        (and (not (zerop matches))
             matches)))))

(defun replace-regexp-in-region (regexp replacement &optional start end)
  "Replace REGEXP with REPLACEMENT in the region from START to END.
The number of replaced occurrences are returned, or nil if REGEXP
doesn't exist in the region.

If START is nil, use the current point.  If END is nil, use `point-max'.

Comparisons and replacements are done with fixed case.

REPLACEMENT can use the following special elements:

  `\\&' in NEWTEXT means substitute original matched text.
  `\\N' means substitute what matched the Nth `\\(...\\)'.
       If Nth parens didn't match, substitute nothing.
  `\\\\' means insert one `\\'.
  `\\?' is treated literally."
  (if start
      (when (< start (point-min))
        (error "Start before start of buffer"))
    (setq start (point)))
  (if end
      (when (> end (point-max))
        (error "End after end of buffer"))
    (setq end (point-max)))
  (save-excursion
    (goto-char start)
    (save-restriction
      (narrow-to-region start end)
      (let ((matches 0)
            (case-fold-search nil))
          (while (re-search-forward regexp nil t)
          (replace-match replacement t)
          (setq matches (1+ matches)))
        (and (not (zerop matches))
             matches)))))

(defun yank-handle-font-lock-face-property (face start end)
  "If `font-lock-defaults' is nil, apply FACE as a `face' property.
START and END denote the start and end of the text to act on.
Do nothing if FACE is nil."
  (and face
       (null font-lock-defaults)
       (put-text-property start end 'face face)))

;; This removes `mouse-face' properties in *Help* buffer buttons:
;; https://lists.gnu.org/r/emacs-devel/2002-04/msg00648.html
(defun yank-handle-category-property (category start end)
  "Apply property category CATEGORY's properties between START and END."
  (when category
    (let ((start2 start))
      (while (< start2 end)
	(let ((end2     (next-property-change start2 nil end))
	      (original (text-properties-at start2)))
	  (set-text-properties start2 end2 (symbol-plist category))
	  (add-text-properties start2 end2 original)
	  (setq start2 end2))))))


;;;; Synchronous shell commands.

(defun start-process-shell-command (name buffer command)
  "Start a program in a subprocess.  Return the process object for it.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer.
COMMAND is the shell command to run."
  ;; We used to use `exec' to replace the shell with the command,
  ;; but that failed to handle (...) and semicolon, etc.
  (start-process name buffer shell-file-name shell-command-switch command))

(defun start-file-process-shell-command (name buffer command)
  "Start a program in a subprocess.  Return the process object for it.
Similar to `start-process-shell-command', but calls `start-file-process'."
  ;; On remote hosts, the local `shell-file-name' might be useless.
  (with-connection-local-variables
   (start-file-process
    name buffer shell-file-name shell-command-switch command)))

(defun call-process-shell-command (command &optional infile buffer display
					   &rest args)
  "Execute the shell command COMMAND synchronously in separate process.
The remaining arguments are optional.
The program's input comes from file INFILE (nil means `/dev/null').
Insert output in BUFFER before point; t means current buffer;
 nil for BUFFER means discard it; 0 means discard and don't wait.
BUFFER can also have the form (REAL-BUFFER STDERR-FILE); in that case,
REAL-BUFFER says what to do with standard output, as above,
while STDERR-FILE says what to do with standard error in the child.
STDERR-FILE may be nil (discard standard error output),
t (mix it with ordinary output), or a file name string.

Fourth arg DISPLAY non-nil means redisplay buffer as output is inserted.
Wildcards and redirection are handled as usual in the shell.

If BUFFER is 0, `call-process-shell-command' returns immediately with value nil.
Otherwise it waits for COMMAND to terminate and returns a numeric exit
status or a signal description string.
If you quit, the process is killed with SIGINT, or SIGKILL if you quit again.

An old calling convention accepted any number of arguments after DISPLAY,
which were just concatenated to COMMAND.  This is still supported but strongly
discouraged."
  (declare (advertised-calling-convention
            (command &optional infile buffer display) "24.5"))
  ;; We used to use `exec' to replace the shell with the command,
  ;; but that failed to handle (...) and semicolon, etc.
  (call-process shell-file-name
		infile buffer display
		shell-command-switch
		(mapconcat #'identity (cons command args) " ")))

(defun process-file-shell-command (command &optional infile buffer display
					   &rest args)
  "Process files synchronously in a separate process.
Similar to `call-process-shell-command', but calls `process-file'."
  (declare (advertised-calling-convention
            (command &optional infile buffer display) "24.5"))
  ;; On remote hosts, the local `shell-file-name' might be useless.
  (with-connection-local-variables
   (process-file
    shell-file-name infile buffer display shell-command-switch
    (mapconcat #'identity (cons command args) " "))))

(defun call-shell-region (start end command &optional delete buffer)
  "Send text from START to END as input to an inferior shell running COMMAND.
Delete the text if fourth arg DELETE is non-nil.

Insert output in BUFFER before point; t means current buffer; nil for
 BUFFER means discard it; 0 means discard and don't wait; and `(:file
 FILE)', where FILE is a file name string, means that it should be
 written to that file (if the file already exists it is overwritten).
BUFFER can also have the form (REAL-BUFFER STDERR-FILE); in that case,
REAL-BUFFER says what to do with standard output, as above,
while STDERR-FILE says what to do with standard error in the child.
STDERR-FILE may be nil (discard standard error output),
t (mix it with ordinary output), or a file name string.

If BUFFER is 0, `call-shell-region' returns immediately with value nil.
Otherwise it waits for COMMAND to terminate
and returns a numeric exit status or a signal description string.
If you quit, the process is killed with SIGINT, or SIGKILL if you quit again.

If COMMAND names a shell (e.g., via `shell-file-name'), keep in mind
that behavior of various shells when commands are piped to their
standard input is shell- and system-dependent, and thus non-portable.
The differences are especially prominent when the region includes
more than one line, i.e. when piping to a shell commands with embedded
newlines."
  (call-process-region start end
                       shell-file-name delete buffer nil
                       shell-command-switch command))

;;;; Lisp macros to do various things temporarily.

(defmacro track-mouse (&rest body)
  "Evaluate BODY with mouse movement events enabled.
Within a `track-mouse' form, mouse motion generates input events that
 you can read with `read-event'.
Normally, mouse motion is ignored."
  (declare (debug (def-body)) (indent 0))
  `(internal--track-mouse (lambda () ,@body)))

(defmacro with-current-buffer (buffer-or-name &rest body)
  "Execute the forms in BODY with BUFFER-OR-NAME temporarily current.
BUFFER-OR-NAME must be a buffer or the name of an existing buffer.
The value returned is the value of the last form in BODY.  See
also `with-temp-buffer'."
  (declare (indent 1) (debug t))
  `(save-current-buffer
     (set-buffer ,buffer-or-name)
     ,@body))

(defun internal--before-with-selected-window (window)
  (let ((other-frame (window-frame window)))
    (list window (selected-window)
          ;; Selecting a window on another frame also changes that
          ;; frame's frame-selected-window.  We must save&restore it.
          (unless (eq (selected-frame) other-frame)
            (frame-selected-window other-frame))
          ;; Also remember the top-frame if on ttys.
          (unless (eq (selected-frame) other-frame)
            (tty-top-frame other-frame)))))

(defun internal--after-with-selected-window (state)
  ;; First reset frame-selected-window.
  (when (window-live-p (nth 2 state))
    ;; We don't use set-frame-selected-window because it does not
    ;; pass the `norecord' argument to Fselect_window.
    (select-window (nth 2 state) 'norecord)
    (and (frame-live-p (nth 3 state))
         (not (eq (tty-top-frame) (nth 3 state)))
         (select-frame (nth 3 state) 'norecord)))
  ;; Then reset the actual selected-window.
  (when (window-live-p (nth 1 state))
    (select-window (nth 1 state) 'norecord)))

(defun generate-new-buffer (name &optional inhibit-buffer-hooks)
  "Create and return a buffer with a name based on NAME.
Choose the buffer's name using `generate-new-buffer-name'.
See `get-buffer-create' for the meaning of INHIBIT-BUFFER-HOOKS."
  (get-buffer-create (generate-new-buffer-name name) inhibit-buffer-hooks))

(defmacro with-selected-window (window &rest body)
  "Execute the forms in BODY with WINDOW as the selected window.
The value returned is the value of the last form in BODY.

This macro saves and restores the selected window, as well as the
selected window of each frame.  It does not change the order of
recently selected windows.  If the previously selected window of
some frame is no longer live at the end of BODY, that frame's
selected window is left alone.  If the selected window is no
longer live, then whatever window is selected at the end of BODY
remains selected.

This macro uses `save-current-buffer' to save and restore the
current buffer, since otherwise its normal operation could
potentially make a different buffer current.  It does not alter
the buffer list ordering."
  (declare (indent 1) (debug t))
  `(let ((save-selected-window--state
          (internal--before-with-selected-window ,window)))
     (save-current-buffer
       (unwind-protect
           (progn (select-window (car save-selected-window--state) 'norecord)
		  ,@body)
         (internal--after-with-selected-window save-selected-window--state)))))

(defmacro with-selected-frame (frame &rest body)
  "Execute the forms in BODY with FRAME as the selected frame.
The value returned is the value of the last form in BODY.

This macro saves and restores the selected frame, and changes the
order of neither the recently selected windows nor the buffers in
the buffer list."
  (declare (indent 1) (debug t))
  (let ((old-frame (make-symbol "old-frame"))
	(old-buffer (make-symbol "old-buffer")))
    `(let ((,old-frame (selected-frame))
	   (,old-buffer (current-buffer)))
       (unwind-protect
	   (progn (select-frame ,frame 'norecord)
		  ,@body)
	 (when (frame-live-p ,old-frame)
	   (select-frame ,old-frame 'norecord))
	 (when (buffer-live-p ,old-buffer)
	   (set-buffer ,old-buffer))))))

(defmacro save-window-excursion (&rest body)
  "Execute BODY, then restore previous window configuration.
This macro saves the window configuration on the selected frame,
executes BODY, then calls `set-window-configuration' to restore
the saved window configuration.  The return value is the last
form in BODY.  The window configuration is also restored if BODY
exits nonlocally.

BEWARE: Most uses of this macro introduce bugs.
E.g. it should not be used to try and prevent some code from opening
a new window, since that window may sometimes appear in another frame,
in which case `save-window-excursion' cannot help."
  (declare (indent 0) (debug t))
  (let ((c (make-symbol "wconfig")))
    `(let ((,c (current-window-configuration)))
       (unwind-protect (progn ,@body)
         (set-window-configuration ,c)))))

(defun internal-temp-output-buffer-show (buffer)
  "Internal function for `with-output-to-temp-buffer'."
  (with-current-buffer buffer
    (set-buffer-modified-p nil)
    (goto-char (point-min)))

  (if temp-buffer-show-function
      (funcall temp-buffer-show-function buffer)
    (with-current-buffer buffer
      (let* ((window
	      (let ((window-combination-limit
		   ;; When `window-combination-limit' equals
		   ;; `temp-buffer' or `temp-buffer-resize' and
		   ;; `temp-buffer-resize-mode' is enabled in this
		   ;; buffer bind it to t so resizing steals space
		   ;; preferably from the window that was split.
		   (if (or (eq window-combination-limit 'temp-buffer)
			   (and (eq window-combination-limit
				    'temp-buffer-resize)
				temp-buffer-resize-mode))
		       t
		     window-combination-limit)))
		(display-buffer buffer)))
	     (frame (and window (window-frame window))))
	(when window
	  (unless (eq frame (selected-frame))
	    (make-frame-visible frame))
	  (setq minibuffer-scroll-window window)
	  (set-window-hscroll window 0)
	  ;; Don't try this with NOFORCE non-nil!
	  (set-window-start window (point-min) t)
	  ;; This should not be necessary.
	  (set-window-point window (point-min))
	  ;; Run `temp-buffer-show-hook', with the chosen window selected.
	  (with-selected-window window
	    (run-hooks 'temp-buffer-show-hook))))))
  ;; Return nil.
  nil)

;; Doc is very similar to with-temp-buffer-window.
(defmacro with-output-to-temp-buffer (bufname &rest body)
  "Bind `standard-output' to buffer BUFNAME, eval BODY, then show that buffer.

This is a convenience macro meant for displaying help buffers and
the like.  It empties the BUFNAME buffer before evaluating BODY
and disables undo in that buffer.

It does not make the buffer current for BODY.  Instead it binds
`standard-output' to that buffer, so that output generated with
`prin1' and similar functions in BODY goes into the buffer.

At the end of BODY, this marks buffer BUFNAME unmodified and displays
it in a window, but does not select it.  The normal way to do this is
by calling `display-buffer', then running `temp-buffer-show-hook'.
However, if `temp-buffer-show-function' is non-nil, it calls that
function instead (and does not run `temp-buffer-show-hook').  The
function gets one argument, the buffer to display.

The return value of `with-output-to-temp-buffer' is the value of the
last form in BODY.  If BODY does not finish normally, the buffer
BUFNAME is not displayed.

This runs the hook `temp-buffer-setup-hook' before BODY,
with the buffer BUFNAME temporarily current.  It runs the hook
`temp-buffer-show-hook' after displaying buffer BUFNAME, with that
buffer temporarily current, and the window that was used to display it
temporarily selected.  But it doesn't run `temp-buffer-show-hook'
if it uses `temp-buffer-show-function'.

By default, the setup hook puts the buffer into Help mode before running BODY.
If BODY does not change the major mode, the show hook makes the buffer
read-only, and scans it for function and variable names to make them into
clickable cross-references.

See the related form `with-temp-buffer-window'."
  (declare (debug t) (indent 1))
  (let ((old-dir (make-symbol "old-dir"))
        (buf (make-symbol "buf")))
    `(let* ((,old-dir default-directory)
            (,buf
             (with-current-buffer (get-buffer-create ,bufname)
               (prog1 (current-buffer)
                 (kill-all-local-variables)
                 ;; FIXME: delete_all_overlays
                 (setq default-directory ,old-dir)
                 (setq buffer-read-only nil)
                 (setq buffer-file-name nil)
                 (setq buffer-undo-list t)
                 (let ((inhibit-read-only t)
                       (inhibit-modification-hooks t))
                   (erase-buffer)
                   (run-hooks 'temp-buffer-setup-hook)))))
            (standard-output ,buf))
       (prog1 (progn ,@body)
         (internal-temp-output-buffer-show ,buf)))))

(defmacro with-temp-file (file &rest body)
  "Create a new buffer, evaluate BODY there, and write the buffer to FILE.
The value returned is the value of the last form in BODY.
The buffer does not run the hooks `kill-buffer-hook',
`kill-buffer-query-functions', and `buffer-list-update-hook'.
See also `with-temp-buffer'."
  (declare (indent 1) (debug t))
  (let ((temp-file (make-symbol "temp-file"))
	(temp-buffer (make-symbol "temp-buffer")))
    `(let ((,temp-file ,file)
           (,temp-buffer (generate-new-buffer " *temp file*" t)))
       (unwind-protect
	   (prog1
	       (with-current-buffer ,temp-buffer
		 ,@body)
	     (with-current-buffer ,temp-buffer
	       (write-region nil nil ,temp-file nil 0)))
	 (and (buffer-name ,temp-buffer)
	      (kill-buffer ,temp-buffer))))))

(defmacro with-temp-message (message &rest body)
  "Display MESSAGE temporarily if non-nil while BODY is evaluated.
The original message is restored to the echo area after BODY has finished.
The value returned is the value of the last form in BODY.
MESSAGE is written to the message log buffer if `message-log-max' is non-nil.
If MESSAGE is nil, the echo area and message log buffer are unchanged.
Use a MESSAGE of \"\" to temporarily clear the echo area."
  (declare (debug t) (indent 1))
  (let ((current-message (make-symbol "current-message"))
	(temp-message (make-symbol "with-temp-message")))
    `(let ((,temp-message ,message)
	   (,current-message))
       (unwind-protect
	   (progn
	     (when ,temp-message
	       (setq ,current-message (current-message))
	       (message "%s" ,temp-message))
	     ,@body)
	 (and ,temp-message
	      (if ,current-message
		  (message "%s" ,current-message)
		(message nil)))))))

(defmacro with-temp-buffer (&rest body)
  "Create a temporary buffer, and evaluate BODY there like `progn'.
The buffer does not run the hooks `kill-buffer-hook',
`kill-buffer-query-functions', and `buffer-list-update-hook'.
See also `with-temp-file' and `with-output-to-string'."
  (declare (indent 0) (debug t))
  (let ((temp-buffer (make-symbol "temp-buffer")))
    `(let ((,temp-buffer (generate-new-buffer " *temp*" t)))
       ;; `kill-buffer' can change current-buffer in some odd cases.
       (with-current-buffer ,temp-buffer
         (unwind-protect
	     (progn ,@body)
           (and (buffer-name ,temp-buffer)
                (kill-buffer ,temp-buffer)))))))

(defmacro with-silent-modifications (&rest body)
  "Execute BODY, pretending it does not modify the buffer.
This macro is typically used around modifications of
text properties that do not really affect the buffer's content.
If BODY performs real modifications to the buffer's text, other
than cosmetic ones, undo data may become corrupted.

This macro will run BODY normally, but doesn't count its buffer
modifications as being buffer modifications.  This affects things
like `buffer-modified-p', checking whether the file is locked by
someone else, running buffer modification hooks, and other things
of that nature."
  (declare (debug t) (indent 0))
  (let ((modified (make-symbol "modified")))
    `(let* ((,modified (buffer-modified-p))
            (buffer-undo-list t)
            (inhibit-read-only t)
            (inhibit-modification-hooks t))
       (unwind-protect
           (progn
             ,@body)
         (when (memq ,modified '(nil autosaved))
           (restore-buffer-modified-p ,modified))))))

(defmacro with-output-to-string (&rest body)
  "Execute BODY, return the text it sent to `standard-output', as a string."
  (declare (indent 0) (debug t))
  `(let ((standard-output (generate-new-buffer " *string-output*" t)))
     (unwind-protect
	 (progn
	   (let ((standard-output standard-output))
	     ,@body)
	   (with-current-buffer standard-output
	     (buffer-string)))
       (kill-buffer standard-output))))

(defmacro with-local-quit (&rest body)
  "Execute BODY, allowing quits to terminate BODY but not escape further.
When a quit terminates BODY, `with-local-quit' returns nil but
requests another quit.  That quit will be processed as soon as quitting
is allowed once again.  (Immediately, if `inhibit-quit' is nil.)"
  (declare (debug t) (indent 0))
  `(condition-case nil
       (let ((inhibit-quit nil))
	 ,@body)
     (quit (setq quit-flag t)
	   ;; This call is to give a chance to handle quit-flag
	   ;; in case inhibit-quit is nil.
	   ;; Without this, it will not be handled until the next function
	   ;; call, and that might allow it to exit thru a condition-case
	   ;; that intends to handle the quit signal next time.
	   (eval '(ignore nil) t))))

(defmacro while-no-input (&rest body)
  "Execute BODY only as long as there's no pending input.
If input arrives, that ends the execution of BODY,
and `while-no-input' returns t.  Quitting makes it return nil.
If BODY finishes, `while-no-input' returns whatever value BODY produced."
  (declare (debug t) (indent 0))
  (let ((catch-sym (make-symbol "input")))
    `(with-local-quit
       (catch ',catch-sym
	 (let ((throw-on-input ',catch-sym)
               val)
           (setq val (or (input-pending-p)
	                 (progn ,@body)))
           (cond
            ;; When input arrives while throw-on-input is non-nil,
            ;; kbd_buffer_store_buffered_event sets quit-flag to the
            ;; value of throw-on-input.  If, when BODY finishes,
            ;; quit-flag still has the same value as throw-on-input, it
            ;; means BODY never tested quit-flag, and therefore ran to
            ;; completion even though input did arrive before it
            ;; finished.  In that case, we must manually simulate what
            ;; 'throw' in process_quit_flag would do, and we must
            ;; reset quit-flag, because leaving it set will cause us
            ;; quit to top-level, which has undesirable consequences,
            ;; such as discarding input etc.  We return t in that case
            ;; because input did arrive during execution of BODY.
            ((eq quit-flag throw-on-input)
             (setq quit-flag nil)
             t)
            ;; This is for when the user actually QUITs during
            ;; execution of BODY.
            (quit-flag
             nil)
            (t val)))))))

(defmacro condition-case-unless-debug (var bodyform &rest handlers)
  "Like `condition-case', except that it does not prevent debugging.
More specifically, if `debug-on-error' is set, then the debugger will
be invoked even if some handler catches the signal.
Note that this doesn't prevent the handler from executing, it just
causes the debugger to be called before running the handler."
  (declare (debug condition-case) (indent 2))
  `(condition-case ,var
       ,bodyform
     ,@(mapcar (lambda (handler)
                 (let ((condition (car handler)))
                   (if (eq condition :success)
                       handler
                     `((debug ,@(if (listp condition) condition
                                  (list condition)))
                       ,@(cdr handler)))))
               handlers)))

(defmacro with-demoted-errors (format &rest body)
  "Run BODY and demote any errors to simple messages.
FORMAT is a string passed to `message' to format any error message.
It should contain a single %-sequence; e.g., \"Error: %S\".

If `debug-on-error' is non-nil, run BODY without catching its errors.
This is to be used around code that is not expected to signal an error
but that should be robust in the unexpected case that an error is signaled."
  (declare (debug t) (indent 1))
  (let* ((err (make-symbol "err"))
         (orig-body body)
         (orig-format format)
         (format (if (and (stringp format) body) format
                   (prog1 "Error: %S"
                     (if format (push format body)))))
         (exp
          `(condition-case-unless-debug ,err
               ,(macroexp-progn body)
             (error (message ,format ,err) nil))))
    (if (eq orig-body body) exp
      ;; The use without `format' is obsolete, let's warn when we bump
      ;; into any such remaining uses.
      (macroexp-warn-and-return
       (format-message "Missing format argument in `with-demoted-errors'")
       exp nil nil
       orig-format))))

(defmacro combine-after-change-calls (&rest body)
  "Execute BODY, but don't call the after-change functions till the end.
If BODY makes changes in the buffer, they are recorded
and the functions on `after-change-functions' are called several times
when BODY is finished.
The return value is the value of the last form in BODY.

If `before-change-functions' is non-nil, then calls to the after-change
functions can't be deferred, so in that case this macro has no effect.

Do not alter `after-change-functions' or `before-change-functions'
in BODY."
  (declare (indent 0) (debug t))
  `(unwind-protect
       (let ((combine-after-change-calls t))
	 . ,body)
     (combine-after-change-execute)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar undo--combining-change-calls nil
  "Non-nil when `combine-change-calls-1' is running.")

(defun combine-change-calls-1 (beg end body)
  "Evaluate BODY, running the change hooks just once, for region \(BEG END).

Firstly, `before-change-functions' is invoked for the region
\(BEG END), then BODY (a function) is evaluated with
`before-change-functions' and `after-change-functions' bound to
nil, then finally `after-change-functions' is invoked on the
updated region (BEG NEW-END) with a calculated OLD-LEN argument.
If `inhibit-modification-hooks' is initially non-nil, the change
hooks are not run.

The result of `combine-change-calls-1' is the value returned by
BODY.  BODY must not make a different buffer current, except
temporarily.  It must not make any changes to the buffer outside
the specified region.  It must not change
`before-change-functions' or `after-change-functions'.

Additionally, the buffer modifications of BODY are recorded on
the buffer's undo list as a single (apply ...) entry containing
the function `undo--wrap-and-run-primitive-undo'."
  (if (markerp beg) (setq beg (marker-position beg)))
  (if (markerp end) (setq end (marker-position end)))
  (let ((old-bul buffer-undo-list)
	(end-marker (copy-marker end t))
	result)
    (if undo--combining-change-calls
	(setq result (funcall body))
      (let ((undo--combining-change-calls t))
	(if (not inhibit-modification-hooks)
	    (run-hook-with-args 'before-change-functions beg end))
	(let ((bcf before-change-functions)
	      (acf after-change-functions)
	      (local-bcf (local-variable-p 'before-change-functions))
	      (local-acf (local-variable-p 'after-change-functions)))
	  (unwind-protect
              ;; FIXME: WIBNI we could just use `inhibit-modification-hooks'?
              (progn
                ;; Ugly Hack: if the body uses syntax-ppss/syntax-propertize
                ;; (e.g. via a regexp-search or sexp-movement triggering
                ;; on-the-fly syntax-propertize), make sure that this gets
                ;; properly refreshed after subsequent changes.
	        (setq-local before-change-functions
                            (if (memq #'syntax-ppss-flush-cache bcf)
                                '(syntax-ppss-flush-cache)))
                (setq-local after-change-functions nil)
	        (setq result (funcall body)))
	    (if local-bcf (setq before-change-functions bcf)
	      (kill-local-variable 'before-change-functions))
	    (if local-acf (setq after-change-functions acf)
	      (kill-local-variable 'after-change-functions))))
	;; If buffer-undo-list is neither t (in which case undo
	;; information is not recorded) nor equal to buffer-undo-list
	;; before body was funcalled (in which case (funcall body) did
	;; not add items to buffer-undo-list) ...
	(unless (or (eq buffer-undo-list t)
		    (eq buffer-undo-list old-bul))
	  (let ((ptr buffer-undo-list) body-undo-list)
	    ;; ... then loop over buffer-undo-list, until the head of
	    ;; buffer-undo-list before body was funcalled is found, or
	    ;; ptr is nil (which may happen if garbage-collect has
	    ;; been called after (funcall body) and has removed
	    ;; entries of buffer-undo-list that were added by (funcall
	    ;; body)), and add these entries to body-undo-list.
	    (while (and ptr (not (eq ptr old-bul)))
	      (push (car ptr) body-undo-list)
	      (setq ptr (cdr ptr)))
	    (setq body-undo-list (nreverse body-undo-list))
	    ;; Warn if garbage-collect has truncated buffer-undo-list
	    ;; behind our back.
	    (when (and old-bul (not ptr))
	      (message
               "combine-change-calls: buffer-undo-list has been truncated"))
	    ;; Add an (apply ...) entry to buffer-undo-list, using
	    ;; body-undo-list ...
	    (push (list 'apply
			(- end end-marker)
			beg
			(marker-position end-marker)
			#'undo--wrap-and-run-primitive-undo
			beg (marker-position end-marker)
			body-undo-list)
		  buffer-undo-list)
	    ;; ... and set the cdr of buffer-undo-list to
	    ;; buffer-undo-list before body was funcalled.
	    (setcdr buffer-undo-list old-bul)))
	(if (not inhibit-modification-hooks)
	    (run-hook-with-args 'after-change-functions
				beg (marker-position end-marker)
				(- end beg)))))
    (set-marker end-marker nil)
    result))

(defmacro combine-change-calls (beg end &rest body)
  "Evaluate BODY, running the change hooks just once.

BODY is a sequence of Lisp forms to evaluate.  BEG and END bound
the region the change hooks will be run for.

Firstly, `before-change-functions' is invoked for the region
\(BEG END), then the BODY forms are evaluated with
`before-change-functions' and `after-change-functions' bound to
nil, and finally `after-change-functions' is invoked on the
updated region.  The change hooks are not run if
`inhibit-modification-hooks' is initially non-nil.

The result of `combine-change-calls' is the value returned by the
last of the BODY forms to be evaluated.  BODY may not make a
different buffer current, except temporarily.  BODY may not
change the buffer outside the specified region.  It must not
change `before-change-functions' or `after-change-functions'.

Additionally, the buffer modifications of BODY are recorded on
the buffer's undo list as a single \(apply ...) entry containing
the function `undo--wrap-and-run-primitive-undo'."
  (declare (debug (form form def-body)) (indent 2))
  `(combine-change-calls-1 ,beg ,end (lambda () ,@body)))

(defun undo--wrap-and-run-primitive-undo (beg end list)
  "Call `primitive-undo' on the undo elements in LIST.

This function is intended to be called purely by `undo' as the
function in an \(apply DELTA BEG END FUNNAME . ARGS) undo
element.  It invokes `before-change-functions' and
`after-change-functions' once each for the entire region \(BEG
END) rather than once for each individual change.

Additionally the fresh \"redo\" elements which are generated on
`buffer-undo-list' will themselves be \"enclosed\" in
`undo--wrap-and-run-primitive-undo'.

Undo elements of this form are generated by the macro
`combine-change-calls'."
  (combine-change-calls beg end
			(while list
			  (setq list (primitive-undo 1 list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-case-table (table &rest body)
  "Execute the forms in BODY with TABLE as the current case table.
The value returned is the value of the last form in BODY."
  (declare (indent 1) (debug t))
  (let ((old-case-table (make-symbol "table"))
	(old-buffer (make-symbol "buffer")))
    `(let ((,old-case-table (current-case-table))
	   (,old-buffer (current-buffer)))
       (unwind-protect
	   (progn (set-case-table ,table)
		  ,@body)
	 (with-current-buffer ,old-buffer
	   (set-case-table ,old-case-table))))))

(defmacro with-file-modes (modes &rest body)
  "Execute BODY with default file permissions temporarily set to MODES.
MODES is as for `set-default-file-modes'."
  (declare (indent 1) (debug t))
  (let ((umask (make-symbol "umask")))
    `(let ((,umask (default-file-modes)))
       (unwind-protect
           (progn
             (set-default-file-modes ,modes)
             ,@body)
         (set-default-file-modes ,umask)))))

(defmacro with-existing-directory (&rest body)
  "Execute BODY with `default-directory' bound to an existing directory.
If `default-directory' is already an existing directory, it's not changed."
  (declare (indent 0) (debug t))
  `(let ((default-directory (seq-find (lambda (dir)
                                        (and dir
                                             (file-exists-p dir)))
                                      (list default-directory
                                            (expand-file-name "~/")
                                            temporary-file-directory
                                            (getenv "TMPDIR")
                                            "/tmp/")
                                      "/")))
     ,@body))

;;; Matching and match data.

(defmacro save-match-data (&rest body)
  "Execute the BODY forms, restoring the global value of the match data.
The value returned is the value of the last form in BODY.
NOTE: The convention in Elisp is that any function, except for a few
exceptions like car/assoc/+/goto-char, can clobber the match data,
so `save-match-data' should normally be used to save *your* match data
rather than your caller's match data."
  ;; It is better not to use backquote here,
  ;; because that makes a bootstrapping problem
  ;; if you need to recompile all the Lisp files using interpreted code.
  (declare (indent 0) (debug t))
  (let ((saved-match-data (make-symbol "saved-match-data")))
    (list 'let
	  (list (list saved-match-data '(match-data)))
	  (list 'unwind-protect
	        (cons 'progn body)
	        (list 'set-match-data saved-match-data t)))))

(defun match-string (num &optional string)
  "Return the string of text matched by the previous search or regexp operation.
NUM specifies the number of the parenthesized sub-expression in the last
regexp whose match to return.  Zero means return the text matched by the
entire regexp or the whole string.

The return value is nil if NUMth pair didn't match anything, or if there
were fewer than NUM sub-expressions in the regexp used in the search.

STRING should be given if the last search was by `string-match'
on STRING.  If STRING is nil, the current buffer should be the
same buffer as the one in which the search/match was performed.

Note that many functions in Emacs modify the match data, so this
function should be called \"close\" to the function that did the
regexp search.  In particular, saying (for instance)
`M-: (looking-at \"[0-9]\") RET' followed by `M-: (match-string 0) RET'
interactively is seldom meaningful, since the Emacs command loop
may modify the match data."
  (declare (side-effect-free t))
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
	(buffer-substring (match-beginning num) (match-end num)))))

(defun match-string-no-properties (num &optional string)
  "Return string of text matched by last search, without text properties.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING.
If STRING is nil, the current buffer should be the same buffer
the search/match was performed in."
  (declare (side-effect-free t))
  (if (match-beginning num)
      (if string
	  (substring-no-properties string (match-beginning num)
				   (match-end num))
	(buffer-substring-no-properties (match-beginning num)
					(match-end num)))))


(defun match-substitute-replacement (replacement
				     &optional fixedcase literal string subexp)
  "Return REPLACEMENT as it will be inserted by `replace-match'.
In other words, all back-references in the form `\\&' and `\\N'
are substituted with actual strings matched by the last search.
Optional FIXEDCASE, LITERAL, STRING and SUBEXP have the same
meaning as for `replace-match'."
  (declare (side-effect-free t))
  (let ((match (match-string 0 string)))
    (save-match-data
      (match-data--translate (- (match-beginning 0)))
      (replace-match replacement fixedcase literal match subexp))))


(defun looking-back (regexp &optional limit greedy)
  "Return non-nil if text before point matches regular expression REGEXP.
Like `looking-at' except matches before point, and is slower.
LIMIT if non-nil speeds up the search by specifying a minimum
starting position, to avoid checking matches that would start
before LIMIT.

If GREEDY is non-nil, extend the match backwards as far as
possible, stopping when a single additional previous character
cannot be part of a match for REGEXP.  When the match is
extended, its starting position is allowed to occur before
LIMIT.

As a general recommendation, try to avoid using `looking-back'
wherever possible, since it is slow."
  (declare
   (advertised-calling-convention (regexp limit &optional greedy) "25.1"))
  (let ((start (point))
	(pos
	 (save-excursion
	   (and (re-search-backward (concat "\\(?:" regexp "\\)\\=") limit t)
		(point)))))
    (if (and greedy pos)
	(save-restriction
	  (narrow-to-region (point-min) start)
	  (while (and (> pos (point-min))
		      (save-excursion
			(goto-char pos)
			(backward-char 1)
			(looking-at (concat "\\(?:"  regexp "\\)\\'"))))
	    (setq pos (1- pos)))
	  (save-excursion
	    (goto-char pos)
	    (looking-at (concat "\\(?:"  regexp "\\)\\'")))))
    (not (null pos))))

(defsubst looking-at-p (regexp)
  "\
Same as `looking-at' except this function does not change the match data."
  (declare (side-effect-free t))
  (looking-at regexp t))

(defsubst string-match-p (regexp string &optional start)
  "\
Same as `string-match' except this function does not change the match data."
  (declare (side-effect-free t))
  (string-match regexp string start t))

(defun subregexp-context-p (regexp pos &optional start)
  "Return non-nil if POS is in a normal subregexp context in REGEXP.
A subregexp context is one where a sub-regexp can appear.
A non-subregexp context is for example within brackets, or within a
repetition bounds operator `\\=\\{...\\}', or right after a `\\'.
If START is non-nil, it should be a position in REGEXP, smaller
than POS, and known to be in a subregexp context."
  (declare (important-return-value t))
  ;; Here's one possible implementation, with the great benefit that it
  ;; reuses the regexp-matcher's own parser, so it understands all the
  ;; details of the syntax.  A disadvantage is that it needs to match the
  ;; error string.
  (condition-case err
      (progn
        (string-match (substring regexp (or start 0) pos) "")
        t)
    (invalid-regexp
     (not (member (cadr err) '("Unmatched [ or [^"
                               "Unmatched \\{"
                               "Trailing backslash")))))
  ;; An alternative implementation:
  ;; (defconst re-context-re
  ;;   (let* ((harmless-ch "[^\\[]")
  ;;          (harmless-esc "\\\\[^{]")
  ;;          (class-harmless-ch "[^][]")
  ;;          (class-lb-harmless "[^]:]")
  ;;          (class-lb-colon-maybe-charclass ":\\([a-z]+:]\\)?")
  ;;          (class-lb (concat "\\[\\(" class-lb-harmless
  ;;                            "\\|" class-lb-colon-maybe-charclass "\\)"))
  ;;          (class
  ;;           (concat "\\[^?]?"
  ;;                   "\\(" class-harmless-ch
  ;;                   "\\|" class-lb "\\)*"
  ;;                   "\\[?]"))     ; special handling for bare [ at end of re
  ;;          (braces "\\\\{[0-9,]+\\\\}"))
  ;;     (concat "\\`\\(" harmless-ch "\\|" harmless-esc
  ;;             "\\|" class "\\|" braces "\\)*\\'"))
  ;;   "Matches any prefix that corresponds to a normal subregexp context.")
  ;; (string-match re-context-re (substring regexp (or start 0) pos))
  )

;;;; split-string

(defconst split-string-default-separators "[ \f\t\n\r\v]+"
  "The default value of separators for `split-string'.

A regexp matching strings of whitespace.  May be locale-dependent
\(as yet unimplemented).  Should not match non-breaking spaces.

Warning: binding this to a different value and using it as default is
likely to have undesired semantics.")

(defun split-string (string &optional separators omit-empty trim)
  "Split STRING into substrings bounded by matches for SEPARATORS.

The beginning and end of STRING, and each match for SEPARATORS, are
splitting points.  The substrings matching SEPARATORS are removed, and
the substrings between the splitting points are collected as a list,
which is returned.

If SEPARATORS is non-nil, it should be a regular expression matching text
that separates, but is not part of, the substrings.  If omitted or nil,
it defaults to `split-string-default-separators', whose value is
normally \"[ \\f\\t\\n\\r\\v]+\", and OMIT-EMPTY is then forced to t.
SEPARATORS should never be a regexp that matches the empty string.

If OMIT-EMPTY is t, zero-length substrings are omitted from the list (so
that for the default value of SEPARATORS leading and trailing whitespace
are effectively trimmed).  If nil, all zero-length substrings are retained,
which correctly parses CSV format, for example.

If TRIM is non-nil, it should be a regular expression to match
text to trim from the beginning and end of each substring.  If trimming
makes the substring empty and OMIT-EMPTY is t, it is dropped from the result.

Note that the effect of `(split-string STRING)' is the same as
`(split-string STRING split-string-default-separators t)'.  In the rare
case that you wish to retain zero-length substrings when splitting on
whitespace, use `(split-string STRING split-string-default-separators)'.

Modifies the match data; use `save-match-data' if necessary."
  (declare (important-return-value t))
  (let* ((keep-empty (and separators (not omit-empty)))
	 (len (length string))
         (trim-left-re (and trim (concat "\\`\\(?:" trim "\\)")))
         (trim-right-re (and trim (concat "\\(?:" trim "\\)\\'")))
         (sep-re (or separators split-string-default-separators))
         (acc nil)
         (next 0)
         (start 0))
    (while
        ;; TODO: The semantics for empty matches are just a copy of
        ;; the original code and make no sense at all. It's just a
        ;; consequence of the original implementation, no thought behind it.
        ;; We should probably error on empty matches, except when
        ;; sep is "" (which is in use by some code) but in that case
        ;; we could provide a faster implementation.
        (let ((sep (string-match sep-re string next)))
          (and sep
               (let ((sep-end (match-end 0)))
                 (when (or keep-empty (< start sep))
                   ;; TODO: Ideally we'd be able to trim in the
                   ;; original string and only make a substring after
                   ;; doing so, but there is no way to bound a regexp
                   ;; search before a certain offset, nor to anchor it
                   ;; at the search boundaries.
                   (let ((item (substring string start sep)))
                     (if trim
                         (let* ((item-beg
                                 (if (string-match trim-left-re item 0)
                                     (match-end 0)
                                   0))
                                (item-len (length item))
                                (item-end
                                 (or (string-match-p trim-right-re
                                                     item item-beg)
                                     item-len)))
                           (when (or (> item-beg 0) (< item-end item-len))
                             (setq item (substring item item-beg item-end)))
                           (when (or keep-empty (< item-beg item-end))
                             (push item acc)))
                       (push item acc))))
                 ;; This ensures progress in case the match was empty.
                 (setq next (max (1+ next) sep-end))
                 (setq start sep-end)
                 (< start len)))))
    ;; field after last separator, if any
    (let ((item (if (= start 0)
                    string    ; optimisation when there is no separator
                  (substring string start))))
      (when trim
        (let* ((item-beg (if (string-match trim-left-re item 0)
                             (match-end 0)
                           0))
               (item-len (length item))
               (item-end (or (string-match-p trim-right-re item item-beg)
                             item-len)))
          (when (or (> item-beg 0) (< item-end item-len))
            (setq item (substring item item-beg item-end)))))
      (when (or keep-empty (not (equal item "")))
        (push item acc)))
    (nreverse acc)))

(defalias 'string-split #'split-string)

(defun combine-and-quote-strings (strings &optional separator)
  "Concatenate the STRINGS, adding the SEPARATOR (default \" \").
This tries to quote the strings to avoid ambiguity such that
  (split-string-and-unquote (combine-and-quote-strings strs)) == strs
Only some SEPARATORs will work properly.

Note that this is not intended to protect STRINGS from
interpretation by shells, use `shell-quote-argument' for that."
  (declare (important-return-value t))
  (let* ((sep (or separator " "))
         (re (concat "[\\\"]" "\\|" (regexp-quote sep))))
    (mapconcat
     (lambda (str)
       (if (string-match re str)
	   (concat "\"" (replace-regexp-in-string "[\\\"]" "\\\\\\&" str) "\"")
	 str))
     strings sep)))

(defun split-string-and-unquote (string &optional separator)
  "Split the STRING into a list of strings.
It understands Emacs Lisp quoting within STRING, such that
  (split-string-and-unquote (combine-and-quote-strings strs)) == strs
The SEPARATOR regexp defaults to \"\\s-+\"."
  (declare (important-return-value t))
  (let ((sep (or separator "\\s-+"))
	(i (string-search "\"" string)))
    (if (null i)
	(split-string string sep t)	; no quoting:  easy
      (append (unless (eq i 0) (split-string (substring string 0 i) sep t))
	      (let ((rfs (read-from-string string i)))
		(cons (car rfs)
		      (split-string-and-unquote (substring string (cdr rfs))
						sep)))))))


;;;; Replacement in strings.

(defun subst-char-in-string (fromchar tochar string &optional inplace)
  "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
  (if (and (not inplace)
           (if (multibyte-string-p string)
               (> (max fromchar tochar) 127)
             (> tochar 255)))
      ;; Avoid quadratic behavior from resizing replacement.
      (let ((res (string-replace (string fromchar) (string tochar) string)))
        (unless (eq res string)
          ;; Mend properties broken by the replacement.
          ;; Not fast, but this case never was.
          (dolist (p (object-intervals string))
            (set-text-properties (nth 0 p) (nth 1 p) (nth 2 p) res)))
        res)
    (let ((i (length string))
	  (newstr (if inplace string (copy-sequence string))))
      (while (plusp i)
        (setq i (1- i))
        (if (eq (aref newstr i) fromchar)
	    (aset newstr i tochar)))
      newstr)))

(defun string-replace (from-string to-string in-string)
  "Replace FROM-STRING with TO-STRING in IN-STRING each time it occurs."
  (declare (pure t) (side-effect-free t))
  (when (equal from-string "")
    (signal 'wrong-length-argument '(0)))
  (let ((start 0)
        (result nil)
        pos)
    (while (setq pos (string-search from-string in-string start))
      (unless (= start pos)
        (push (substring in-string start pos) result))
      (push to-string result)
      (setq start (+ pos (length from-string))))
    (if (null result)
        ;; No replacements were done, so just return the original string.
        in-string
      ;; Get any remaining bit.
      (unless (= start (length in-string))
        (push (substring in-string start) result))
      (apply #'concat (nreverse result)))))

(defun replace-regexp-in-string (regexp rep string &optional
					fixedcase literal subexp start)
  "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING, and omit
the first START characters of STRING from the return value.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function, it is called with the actual text of each
match, and its value is used as the replacement text.  When REP is called,
the match data are the result of matching REGEXP against a substring
of STRING, the same substring that is the actual text of the match which
is passed to REP as its argument.

To replace only the first match (if any), make REGEXP match up to \\\\='
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\\\(foo\\\\).*\\\\\\='\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\""
  (declare (important-return-value t))

  ;; To avoid excessive consing from multiple matches in long strings,
  ;; don't just call `replace-match' continually.  Walk down the
  ;; string looking for matches of REGEXP and building up a (reversed)
  ;; list MATCHES.  This comprises segments of STRING that weren't
  ;; matched interspersed with replacements for segments that were.
  ;; [For a `large' number of replacements it's more efficient to
  ;; operate in a temporary buffer; we can't tell from the function's
  ;; args whether to choose the buffer-based implementation, though it
  ;; might be reasonable to do so for long enough STRING.]
  (let ((l (length string))
	(start (or start 0))
	matches str mb me)
    (save-match-data
      (while (and (< start l) (string-match regexp string start))
	(setq mb (match-beginning 0)
	      me (match-end 0))
	;; If we matched the empty string, make sure we advance by one char
	(when (= me mb) (setq me (min l (1+ mb))))
	;; Generate a replacement for the matched substring.
	;; Operate on only the substring to minimize string consing.
        ;; Translate the match data so that it applies to the matched substring.
        (match-data--translate (- mb))
        (setq str (substring string mb me))
	(setq matches
	      (cons (replace-match (if (stringp rep)
				       rep
				     (funcall rep (match-string 0 str)))
				   fixedcase literal str subexp)
		    (cons (substring string start mb) ; unmatched prefix
			  matches)))
	(setq start me))
      ;; Reconstruct a string from the pieces.
      (setq matches (cons (substring string start l) matches)) ; leftover
      (apply #'concat (nreverse matches)))))

(defsubst string-equal-ignore-case (string1 string2)
  "Compare STRING1 and STRING2 case-insensitively.
Upper-case and lower-case letters are treated as equal.
Unibyte strings are converted to multibyte for comparison.

See also `string-equal'."
  (declare (side-effect-free t))
  (eq t (compare-strings string1 0 nil string2 0 nil t)))

(defun string-prefix-p (prefix string &optional ignore-case)
  "Return non-nil if STRING begins with PREFIX.
PREFIX should be a string; the function returns non-nil if the
characters at the beginning of STRING compare equal with PREFIX.
If IGNORE-CASE is non-nil, the comparison is done without paying attention
to letter-case differences."
  (declare (side-effect-free t))
  (let ((prefix-length (length prefix)))
    (if (> prefix-length (length string)) nil
      (eq t (compare-strings prefix 0 prefix-length string
			     0 prefix-length ignore-case)))))

(defun string-suffix-p (suffix string  &optional ignore-case)
  "Return non-nil if STRING ends with SUFFIX.
SUFFIX should be a string; the function returns non-nil if the
characters at end of STRING compare equal with SUFFIX.
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to letter-case differences."
  (declare (side-effect-free t))
  (let ((start-pos (- (length string) (length suffix))))
    (and (>= start-pos 0)
         (eq t (compare-strings suffix nil nil
                                string start-pos nil ignore-case)))))

(defun bidi-string-mark-left-to-right (str)
  "Return a string that can be safely inserted in left-to-right text.

Normally, inserting a string with right-to-left (RTL) script into
a buffer may cause some subsequent text to be displayed as part
of the RTL segment (usually this affects punctuation characters).
This function returns a string that displays as STR but forces
subsequent text to be displayed as left-to-right.

If STR contains any RTL character, this function returns a string
consisting of STR followed by an invisible left-to-right mark
\(LRM) character.  Otherwise, it returns STR."
  (unless (stringp str)
    (signal 'wrong-type-argument (list 'stringp str)))
  (if (string-match "\\cR" str)
      (concat str (propertize (string ?\x200e) 'invisible t))
    str))

(defun string-greaterp (string1 string2)
  "Return non-nil if STRING1 is greater than STRING2 in lexicographic order.
Case is significant.
Symbols are also allowed; their print names are used instead."
  (declare (pure t) (side-effect-free t))
  (string-lessp string2 string1))


;;;; Specifying things to do later.

(defun load-history-regexp (file)
  "Form a regexp to find FILE in `load-history'.
FILE, a string, is described in the function `eval-after-load'."
  (if (file-name-absolute-p file)
      (setq file (file-truename file)))
  (concat (if (file-name-absolute-p file) "\\`" "\\(\\`\\|/\\)")
	  (regexp-quote file)
	  (if (file-name-extension file)
	      ""
	    ;; Note: regexp-opt can't be used here, since we need to call
	    ;; this before Emacs has been fully started.  2006-05-21
	    (concat "\\(" (mapconcat #'regexp-quote load-suffixes "\\|") "\\)?"))
	  "\\(" (mapconcat #'regexp-quote jka-compr-load-suffixes "\\|")
	  "\\)?\\'"))

(defun load-history-filename-element (file-regexp)
  "Get the first elt of `load-history' whose car matches FILE-REGEXP.
Return nil if there isn't one."
  (let* ((loads load-history)
	 (load-elt (and loads (car loads))))
    (save-match-data
      (while (and loads
		  (not (and (car load-elt)
                            (string-match file-regexp (car load-elt)))))
	(setq loads (cdr loads)
	      load-elt (and loads (car loads)))))
    load-elt))

(defun eval-after-load (file form)
  "Arrange that if FILE is loaded, FORM will be run immediately afterwards.
If FILE is already loaded, evaluate FORM right now.
FORM can be an Elisp expression (in which case it's passed to `eval'),
or a function (in which case it's passed to `funcall' with no argument).

If a matching file is loaded again, FORM will be evaluated again.

If FILE is a string, it may be either an absolute or a relative file
name, and may have an extension (e.g. \".el\") or may lack one, and
additionally may or may not have an extension denoting a compressed
format (e.g. \".gz\").

When FILE is absolute, this first converts it to a true name by chasing
symbolic links.  Only a file of this name (see next paragraph regarding
extensions) will trigger the evaluation of FORM.  When FILE is relative,
a file whose absolute true name ends in FILE will trigger evaluation.

When FILE lacks an extension, a file name with any extension will trigger
evaluation.  Otherwise, its extension must match FILE's.  A further
extension for a compressed format (e.g. \".gz\") on FILE will not affect
this name matching.

Alternatively, FILE can be a feature (i.e. a symbol), in which case FORM
is evaluated at the end of any file that `provide's this feature.
If the feature is provided when evaluating code not associated with a
file, FORM is evaluated immediately after the provide statement.

Usually FILE is just a library name like \"font-lock\" or a feature name
like `font-lock'.

This function makes or adds to an entry on `after-load-alist'.

See also `with-eval-after-load'."
  (declare (indent 1)
           (compiler-macro
            (lambda (whole)
              (if (eq 'quote (car-safe form))
                  ;; Quote with lambda so the compiler can look inside.
                  `(eval-after-load ,file (lambda () ,(nth 1 form)))
                whole))))
  ;; Add this FORM into after-load-alist (regardless of whether we'll be
  ;; evaluating it now).
  (let* ((regexp-or-feature
	  (if (stringp file)
              (setq file (load-history-regexp file))
            file))
	 (elt (assoc regexp-or-feature after-load-alist))
         (func
          (if (functionp form) form
            ;; Try to use the "current" lexical/dynamic mode for `form'.
            (eval `(lambda () ,form) lexical-binding))))
    (unless elt
      (setq elt (list regexp-or-feature))
      (push elt after-load-alist))
    ;; Is there an already loaded file whose name (or `provide' name)
    ;; matches FILE?
    (prog1 (if (if (stringp file)
		   (load-history-filename-element regexp-or-feature)
		 (featurep file))
	       (funcall func))
      (let ((delayed-func
             (if (not (symbolp regexp-or-feature)) func
               ;; For features, the after-load-alist elements get run when
               ;; `provide' is called rather than at the end of the file.
               ;; So add an indirection to make sure that `func' is really run
               ;; "after-load" in case the provide call happens early.
               (lambda ()
                 (if (not load-file-name)
                     ;; Not being provided from a file, run func right now.
                     (funcall func)
                   (let ((lfn load-file-name)
                         ;; Don't use letrec, because equal (in
                         ;; add/remove-hook) could get trapped in a cycle
                         ;; (bug#46326).
                         (fun (make-symbol "eval-after-load-helper")))
                     (fset fun (lambda (file)
                                 (when (equal file lfn)
                                   (remove-hook 'after-load-functions fun)
                                   (funcall func))))
                     (add-hook 'after-load-functions fun 'append)))))))
        ;; Add FORM to the element unless it's already there.
        (unless (member delayed-func (cdr elt))
          (nconc elt (list delayed-func)))))))

(defmacro with-eval-after-load (file &rest body)
  "Execute BODY after FILE is loaded.
FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature.  See `eval-after-load'
for more details about the different forms of FILE and their semantics."
  (declare (indent 1) (debug (form def-body)))
  `(eval-after-load ,file (lambda () ,@body)))

(defvar after-load-functions nil
  "Special hook run after loading a file.
Each function there is called with a single argument, the absolute
name of the file just loaded.")

(defun do-after-load-evaluation (abs-file)
  "Evaluate all `eval-after-load' forms, if any, for ABS-FILE.
ABS-FILE, a string, should be the absolute true name of a file just loaded.
This function is called directly from the C code."
  ;; Run the relevant eval-after-load forms.
  (dolist (a-l-element after-load-alist)
    (when (and (stringp (car a-l-element))
               (string-match-p (car a-l-element) abs-file))
      ;; discard the file name regexp
      (mapc #'funcall (cdr a-l-element))))
  ;; Complain when the user uses obsolete files.
  (when (string-match-p "/obsolete/[^/]*\\'" abs-file)
    ;; Maybe we should just use display-warning?  This seems yucky...
    (let* ((file (file-name-nondirectory abs-file))
           (package (intern (substring file 0
			               (string-match "\\.elc?\\>" file))
                            obarray))
	   (msg (format "Package %s is deprecated" package))
	   (fun (lambda (msg) (message "%s" msg))))
      (when (or (not (fboundp 'byte-compile-warning-enabled-p))
                (byte-compile-warning-enabled-p 'obsolete package))
        (cond
	 ((bound-and-true-p byte-compile-current-file)
	  ;; Don't warn about obsolete files using other obsolete files.
	  (unless (and (stringp byte-compile-current-file)
		       (string-match-p "/obsolete/[^/]*\\'"
				       (expand-file-name
					byte-compile-current-file
					byte-compile-root-dir)))
	    (byte-compile-warn "%s" msg)))
         (noninteractive (funcall fun msg)) ;; No timer will be run!
	 (t (run-with-idle-timer 0 nil fun msg))))))

  ;; Finally, run any other hook.
  (run-hook-with-args 'after-load-functions abs-file))


(defun display-delayed-warnings ()
  "Display delayed warnings from `delayed-warnings-list'.
Used from `delayed-warnings-hook' (which see)."
  (dolist (warning (nreverse delayed-warnings-list))
    (apply #'display-warning warning))
  (setq delayed-warnings-list nil))

(defun collapse-delayed-warnings ()
  "Remove duplicates from `delayed-warnings-list'.
Collapse identical adjacent warnings into one (plus count).
Used from `delayed-warnings-hook' (which see)."
  (let ((count 1)
        collapsed warning)
    (while delayed-warnings-list
      (setq warning (pop delayed-warnings-list))
      (if (equal warning (car delayed-warnings-list))
          (setq count (1+ count))
        (when (> count 1)
          (setcdr warning (cons (format "%s [%d times]" (cadr warning) count)
                                (cddr warning)))
          (setq count 1))
        (push warning collapsed)))
    (setq delayed-warnings-list (nreverse collapsed))))

;; At present this is used only for Emacs internals.
;; Ref https://lists.gnu.org/r/emacs-devel/2012-02/msg00085.html
(defvar delayed-warnings-hook '(collapse-delayed-warnings
                                display-delayed-warnings)
  "Normal hook run to process and display delayed warnings.
By default, this hook contains functions to consolidate the
warnings listed in `delayed-warnings-list', display them, and set
`delayed-warnings-list' back to nil.")

(defun delay-warning (type message &optional level buffer-name)
  "Display a delayed warning.
Aside from going through `delayed-warnings-list', this is equivalent
to `display-warning'."
  (push (list type message level buffer-name) delayed-warnings-list))


;;;; invisibility specs

(defun add-to-invisibility-spec (element)
  "Add ELEMENT to `buffer-invisibility-spec'.
See documentation for `buffer-invisibility-spec' for the kind of elements
that can be added.

If `buffer-invisibility-spec' isn't a list before calling this
function, `buffer-invisibility-spec' will afterwards be a list
with the value `(t ELEMENT)'.  This means that if text exists
that invisibility values that aren't either t or ELEMENT, that
text will become visible."
  (if (eq buffer-invisibility-spec t)
      (setq buffer-invisibility-spec (list t)))
  (setq buffer-invisibility-spec
	(cons element buffer-invisibility-spec)))

(defun remove-from-invisibility-spec (element)
  "Remove ELEMENT from `buffer-invisibility-spec'.
If `buffer-invisibility-spec' isn't a list before calling this
function, it will be made into a list containing just t as the
only list member.  This means that if text exists with non-t
invisibility values, that text will become visible."
  (setq buffer-invisibility-spec
        (if (consp buffer-invisibility-spec)
	    (delete element buffer-invisibility-spec)
          (list t))))

;;;; Syntax tables.

(defmacro with-syntax-table (table &rest body)
  "Evaluate BODY with syntax table of current buffer set to TABLE.
The syntax table of the current buffer is saved, BODY is evaluated, and the
saved table is restored, even in case of an abnormal exit.
Value is what BODY returns."
  (declare (debug t) (indent 1))
  (let ((old-table (make-symbol "table"))
	(old-buffer (make-symbol "buffer")))
    `(let ((,old-table (syntax-table))
	   (,old-buffer (current-buffer)))
       (unwind-protect
	   (progn
	     (set-syntax-table ,table)
	     ,@body)
	 (save-current-buffer
	   (set-buffer ,old-buffer)
	   (set-syntax-table ,old-table))))))

(defun make-syntax-table (&optional oldtable)
  "Return a new syntax table.
Create a syntax table that inherits from OLDTABLE (if non-nil) or
from `standard-syntax-table' otherwise."
  (let ((table (make-char-table 'syntax-table nil)))
    (set-char-table-parent table (or oldtable (standard-syntax-table)))
    table))

(defun syntax-after (pos)
  "Return the raw syntax descriptor for the char after POS.
If POS is outside the buffer's accessible portion, return nil."
  (declare (important-return-value t))
  (unless (or (< pos (point-min)) (>= pos (point-max)))
    (let ((st (if parse-sexp-lookup-properties
		  (get-char-property pos 'syntax-table))))
      (if (consp st) st
	(aref (or st (syntax-table)) (char-after pos))))))

(defun syntax-class (syntax)
  "Return the code for the syntax class described by SYNTAX.

SYNTAX should be a raw syntax descriptor; the return value is a
integer that encodes the corresponding syntax class.  See Info
node `(elisp)Syntax Table Internals' for a list of codes.

If SYNTAX is nil, return nil."
  (declare (pure t) (side-effect-free t))
  (and syntax (logand (car syntax) 65535)))

;; Utility motion commands

(defvar word-move-empty-char-table nil
  "Used in `forward-word-strictly' and `backward-word-strictly'
to countermand the effect of `find-word-boundary-function-table'.")

(defun forward-word-strictly (&optional arg)
  "Move point forward ARG words (backward if ARG is negative).
If ARG is omitted or nil, move point forward one word.
Normally returns t.
If an edge of the buffer or a field boundary is reached, point is left there
and the function returns nil.  Field boundaries are not noticed if
`inhibit-field-text-motion' is non-nil.

This function is like `forward-word', but it is not affected
by `find-word-boundary-function-table'.  It is also not interactive."
  (let ((find-word-boundary-function-table
         (if (char-table-p word-move-empty-char-table)
             word-move-empty-char-table
           (setq word-move-empty-char-table (make-char-table nil)))))
    (forward-word (or arg 1))))

(defun backward-word-strictly (&optional arg)
  "Move backward until encountering the beginning of a word.
With argument ARG, do this that many times.
If ARG is omitted or nil, move point backward one word.

This function is like `backward-word', but it is not affected
by `find-word-boundary-function-table'.  It is also not interactive."
  (let ((find-word-boundary-function-table
         (if (char-table-p word-move-empty-char-table)
             word-move-empty-char-table
           (setq word-move-empty-char-table (make-char-table nil)))))
    (forward-word (- (or arg 1)))))

;;  Whitespace

(defun forward-whitespace (arg)
  "Move point to the end of the next sequence of whitespace chars.
Each such sequence may be a single newline, or a sequence of
consecutive space and/or tab characters.
With prefix argument ARG, do it ARG times if positive, or move
backwards ARG times if negative."
  (interactive "^p")
  (if (natnump arg)
      (re-search-forward "[ \t]+\\|\n" nil 'move arg)
    (while (minusp arg)
      (if (re-search-backward "[ \t]+\\|\n" nil 'move)
	  (or (eq (char-after (match-beginning 0)) ?\n)
	      (skip-chars-backward " \t")))
      (setq arg (1+ arg)))))

;;  Symbols

(defun forward-symbol (arg)
  "Move point to the next position that is the end of a symbol.
A symbol is any sequence of characters that are in either the
word constituent or symbol constituent syntax class.
With prefix argument ARG, do it ARG times if positive, or move
backwards ARG times if negative."
  (interactive "^p")
  (if (natnump arg)
      (re-search-forward "\\(\\sw\\|\\s_\\)+" nil 'move arg)
    (while (minusp arg)
      (if (re-search-backward "\\(\\sw\\|\\s_\\)+" nil 'move)
	  (skip-syntax-backward "w_"))
      (setq arg (1+ arg)))))

;;  Syntax blocks

(defun forward-same-syntax (&optional arg)
  "Move point past all characters with the same syntax class.
With prefix argument ARG, do it ARG times if positive, or move
backwards ARG times if negative."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (minusp arg)
    (skip-syntax-backward
     (char-to-string (char-syntax (char-before))))
    (setq arg (1+ arg)))
  (while (plusp arg)
    (skip-syntax-forward (char-to-string (char-syntax (char-after))))
    (setq arg (1- arg))))


;;;; Text clones

(defvar text-clone--maintaining nil)
(defvar text-clone--pending-overlays nil)

(defun text-clone--maintain (ol1 after beg end &optional _len)
  "Propagate the changes made under the overlay OL1 to the other clones.
This is used on the `modification-hooks' property of text clones."
  (when (and after (not undo-in-progress)
             (not text-clone--maintaining))
    ;; An after-change hook (like this one) should never modify a buffer,
    ;; so record the change and arrange to process it soon.
    (let ((pending (overlay-get ol1 'text-clone--pending)))
      (if pending
          (progn
            (setcar pending (min beg (car pending)))
            (setcdr pending (max end (cdr pending))))
        (overlay-put ol1 'text-clone--pending (cons beg end))
        (push ol1 text-clone--pending-overlays)
        (unless (memq #'text-clone--maintain-overlays
                 (default-value 'post-command-hook))
          ;; Perform the update as soon as possible.
          (add-hook 'post-command-hook #'text-clone--maintain-overlays)
          (run-with-timer 0 nil #'text-clone--maintain-overlays))))))

(defun text-clone--maintain-overlays ()
  (while text-clone--pending-overlays
    (let* ((ol1 (pop text-clone--pending-overlays))
           (pending (overlay-get ol1 'text-clone--pending))
           (margin (if (overlay-get ol1 'text-clone-spreadp) 1 0))
           (beg (max (car pending)
                     (+ (or (overlay-start ol1) (point-max)) margin)))
           (end (min (cdr pending)
                     (- (or (overlay-end ol1) (1- beg)) margin))))
      (overlay-put ol1 'text-clone--pending nil)
      (when (<= beg end)
	(with-current-buffer (overlay-buffer ol1)
	  (save-excursion
	    (when (overlay-get ol1 'text-clone-syntax)
	      ;; Check content of the clone's text.
	      (let ((cbeg (+ (overlay-start ol1) margin))
		    (cend (- (overlay-end ol1) margin)))
		(goto-char cbeg)
		(save-match-data
		  (if (not (re-search-forward
			    (overlay-get ol1 'text-clone-syntax) cend t))
		      ;; Mark the overlay for deletion.
		      (setq end cbeg)
		    (when (< (match-end 0) cend)
		      ;; Shrink the clone at its end.
		      (setq end (min end (match-end 0)))
		      (move-overlay ol1 (overlay-start ol1)
				    (+ (match-end 0) margin)))
		    (when (> (match-beginning 0) cbeg)
		      ;; Shrink the clone at its beginning.
		      (setq beg (max (match-beginning 0) beg))
		      (move-overlay ol1 (- (match-beginning 0) margin)
				    (overlay-end ol1)))))))
	    ;; Now go ahead and update the clones.
	    (let ((head (- beg (overlay-start ol1)))
		  (tail (- (overlay-end ol1) end))
		  (str (buffer-substring beg end))
		  (nothing-left t)
		  (text-clone--maintaining t))
	      (dolist (ol2 (overlay-get ol1 'text-clones))
		(let ((oe (overlay-end ol2)))
		  (unless (or (eq ol1 ol2) (null oe))
		    (setq nothing-left nil)
		    (let ((mod-beg (+ (overlay-start ol2) head)))
		      ;;(overlay-put ol2 'modification-hooks nil)
		      (goto-char (- (overlay-end ol2) tail))
		      (unless (> mod-beg (point))
		        (save-excursion (insert str))
		        (delete-region mod-beg (point)))
		      ;;(overlay-put ol2 'modification-hooks
		      ;; '(text-clone--maintain))
		      ))))
	      (if nothing-left (delete-overlay ol1))))))))
  (remove-hook 'post-command-hook #'text-clone--maintain-overlays)
  (cancel-function-timers #'text-clone--maintain-overlays))

(defun text-clone-create (start end &optional spreadp syntax)
  "Create a text clone of START...END at point.
Text clones are chunks of text that are automatically kept identical:
changes done to one of the clones will be immediately propagated to the other.

The buffer's content at point is assumed to be already identical to
the one between START and END.
If SYNTAX is provided it's a regexp that describes the possible text of
the clones; the clone will be shrunk or killed if necessary to ensure that
its text matches the regexp.
If SPREADP is non-nil it indicates that text inserted before/after the
clone should be incorporated in the clone."
  ;; To deal with SPREADP we can either use an overlay with `nil t' along
  ;; with insert-(behind|in-front-of)-hooks or use a slightly larger overlay
  ;; (with a one-char margin at each end) with `t nil'.
  ;; We opted for a larger overlay because it behaves better in the case
  ;; where the clone is reduced to the empty string (we want the overlay to
  ;; stay when the clone's content is the empty string and we want to use
  ;; `evaporate' to make sure those overlays get deleted when needed).
  ;;
  (let* ((pt-end (+ (point) (- end start)))
  	 (start-margin (if (or (not spreadp) (bobp) (<= start (point-min)))
			   0 1))
  	 (end-margin (if (or (not spreadp)
			     (>= pt-end (point-max))
  			     (>= start (point-max)))
  			 0 1))
         ;; FIXME: Reuse overlays at point to extend dups!
  	 (ol1 (make-overlay (- start start-margin) (+ end end-margin) nil t))
  	 (ol2 (make-overlay (- (point) start-margin) (+ pt-end end-margin) nil t))
	 (dups (list ol1 ol2)))
    (overlay-put ol1 'modification-hooks '(text-clone--maintain))
    (when spreadp (overlay-put ol1 'text-clone-spreadp t))
    (when syntax (overlay-put ol1 'text-clone-syntax syntax))
    ;;(overlay-put ol1 'face 'underline)
    (overlay-put ol1 'evaporate t)
    (overlay-put ol1 'text-clones dups)
    ;;
    (overlay-put ol2 'modification-hooks '(text-clone--maintain))
    (when spreadp (overlay-put ol2 'text-clone-spreadp t))
    (when syntax (overlay-put ol2 'text-clone-syntax syntax))
    ;;(overlay-put ol2 'face 'underline)
    (overlay-put ol2 'evaporate t)
    (overlay-put ol2 'text-clones dups)))

;;;; Mail user agents.

;; Here we include just enough for other packages to be able
;; to define them.

(defun define-mail-user-agent (symbol composefunc sendfunc
				      &optional abortfunc hookvar)
  "Define a symbol to identify a mail-sending package for `mail-user-agent'.

SYMBOL can be any Lisp symbol.  Its function definition and/or
value as a variable do not matter for this usage; we use only certain
properties on its property list, to encode the rest of the arguments.

COMPOSEFUNC is program callable function that composes an outgoing
mail message buffer.  This function should set up the basics of the
buffer without requiring user interaction.  It should populate the
standard mail headers, leaving the `to:' and `subject:' headers blank
by default.

COMPOSEFUNC should accept several optional arguments--the same
arguments that `compose-mail' takes.  See that function's documentation.

SENDFUNC is the command a user would run to send the message.

Optional ABORTFUNC is the command a user would run to abort the
message.  For mail packages that don't have a separate abort function,
this can be `kill-buffer' (the equivalent of omitting this argument).

Optional HOOKVAR is a hook variable that gets run before the message
is actually sent.  Callers that use the `mail-user-agent' may
install a hook function temporarily on this hook variable.
If HOOKVAR is nil, `mail-send-hook' is used.

The properties used on SYMBOL are `composefunc', `sendfunc',
`abortfunc', and `hookvar'."
  (declare (indent defun))
  (put symbol 'composefunc composefunc)
  (put symbol 'sendfunc sendfunc)
  (put symbol 'abortfunc (or abortfunc #'kill-buffer))
  (put symbol 'hookvar (or hookvar 'mail-send-hook)))


(defun backtrace-frames (&optional base)
  "Collect all frames of current backtrace into a list.
If non-nil, BASE should be a function, and frames before its
nearest activation frame are discarded."
  (let ((frames nil))
    (mapbacktrace (lambda (&rest frame) (push frame frames))
                  (or base #'backtrace-frames))
    (nreverse frames)))

(defun backtrace-frame (nframes &optional base)
  "Return the function and arguments NFRAMES up from current execution point.
If non-nil, BASE should be a function, and NFRAMES counts from its
nearest activation frame.  BASE can also be of the form (OFFSET . FUNCTION)
in which case OFFSET will be added to NFRAMES.
If the frame has not evaluated the arguments yet (or is a special form),
the value is (nil FUNCTION ARG-FORMS...).
If the frame has evaluated its arguments and called its function already,
the value is (t FUNCTION ARG-VALUES...).
A &rest arg is represented as the tail of the list ARG-VALUES.
FUNCTION is whatever was supplied as car of evaluated list,
or a lambda expression for macro calls.
If NFRAMES is more than the number of frames, the value is nil."
  (backtrace-frame--internal
   (lambda (evald func args _) `(,evald ,func ,@args))
   nframes (or base #'backtrace-frame)))


(defvar called-interactively-p-functions nil
  "Special hook called to skip special frames in `called-interactively-p'.
The functions are called with 3 arguments: (I FRAME1 FRAME2),
where FRAME1 is a \"current frame\", FRAME2 is the next frame,
I is the index of the frame after FRAME2.  It should return nil
if those frames don't seem special and otherwise, it should return
the number of frames to skip (minus 1).")

(defconst internal--funcall-interactively
  (symbol-function 'funcall-interactively))

(defun called-interactively-p (&optional kind)
  "Return t if the containing function was called by `call-interactively'.
If KIND is `interactive', then return t only if the call was made
interactively by the user, i.e. not in `noninteractive' mode nor
when `executing-kbd-macro'.
If KIND is `any', on the other hand, it will return t for any kind of
interactive call, including being called as the binding of a key or
from a keyboard macro, even in `noninteractive' mode.

This function is very brittle, it may fail to return the intended result when
the code is debugged, advised, or instrumented in some form.  Some macros and
special forms (such as `condition-case') may also sometimes wrap their bodies
in a `lambda', so any call to `called-interactively-p' from those bodies will
indicate whether that lambda (rather than the surrounding function) was called
interactively.

Instead of using this function, it is cleaner and more reliable to give your
function an extra optional argument whose `interactive' spec specifies
non-nil unconditionally (\"p\" is a good way to do this), or via
\(not (or executing-kbd-macro noninteractive)).

The only known proper use of `interactive' for KIND is in deciding
whether to display a helpful message, or how to display it.  If you're
thinking of using it for any other purpose, it is quite likely that
you're making a mistake.  Think: what do you want to do when the
command is called from a keyboard macro?"
  (declare (advertised-calling-convention (kind) "23.1"))
  (when (not (and (eq kind 'interactive)
                  (or executing-kbd-macro noninteractive)))
    (let* ((i 1) ;; 0 is the called-interactively-p frame.
           frame nextframe
           (get-next-frame
            (lambda ()
              (setq frame nextframe)
              (setq nextframe (backtrace-frame i 'called-interactively-p))
              ;; (message "Frame %d = %S" i nextframe)
              (setq i (1+ i)))))
      (funcall get-next-frame) ;; Get the first frame.
      (while
          ;; FIXME: The edebug and advice handling should be made modular and
          ;; provided directly by edebug.el and nadvice.el.
          (progn
            ;; frame    =(backtrace-frame i-2)
            ;; nextframe=(backtrace-frame i-1)
            (funcall get-next-frame)
            ;; `pcase' would be a fairly good fit here, but it sometimes moves
            ;; branches within local functions, which then messes up the
            ;; `backtrace-frame' data we get,
            (or
             ;; Skip special forms (from non-compiled code).
             (and frame (null (car frame)))
             ;; Skip also `interactive-p' (because we don't want to know if
             ;; interactive-p was called interactively but if its caller was).
             (eq (nth 1 frame) 'interactive-p)
             ;; Skip package-specific stack-frames.
             (let ((skip (run-hook-with-args-until-success
                          'called-interactively-p-functions
                          i frame nextframe)))
               (pcase skip
                 ('nil nil)
                 (0 t)
                 (_ (setq i (+ i skip -1)) (funcall get-next-frame)))))))
      ;; Now `frame' should be "the function from which we were called".
      (pcase (cons frame nextframe)
        ;; No subr calls `interactive-p', so we can rule that out.
        (`((,_ ,(pred (lambda (f) (subr-primitive-p (indirect-function f)))) . ,_) . ,_) nil)
        ;; In case #<subr funcall-interactively> without going through the
        ;; `funcall-interactively' symbol (bug#3984).
        (`(,_ . (t ,(pred (lambda (f)
                            (eq internal--funcall-interactively
                                (indirect-function f))))
                   . ,_))
         t)))))

(defun interactive-p ()
  "Return t if the containing function was run directly by user input.
This means that the function was called with `call-interactively'
\(which includes being called as the binding of a key)
and input is currently coming from the keyboard (not a keyboard macro),
and Emacs is not running in batch mode (`noninteractive' is nil).

The only known proper use of `interactive-p' is in deciding whether to
display a helpful message, or how to display it.  If you're thinking
of using it for any other purpose, it is quite likely that you're
making a mistake.  Think: what do you want to do when the command is
called from a keyboard macro or in batch mode?

To test whether your function was called with `call-interactively',
either (i) add an extra optional argument and give it an `interactive'
spec that specifies non-nil unconditionally (such as \"p\"); or (ii)
use `called-interactively-p'.

To test whether a function can be called interactively, use
`commandp'."
  ;; Kept around for now.  See discussion at:
  ;; https://lists.gnu.org/r/emacs-devel/2020-08/msg00564.html
  (declare (ftype (function () boolean))
           (obsolete called-interactively-p "23.2")
           (side-effect-free error-free))
  (called-interactively-p 'interactive))

(defun internal-push-keymap (keymap symbol)
  (let ((map (symbol-value symbol)))
    (unless (memq keymap map)
      (unless (memq 'add-keymap-witness (symbol-value symbol))
        (setq map (make-composed-keymap nil (symbol-value symbol)))
        (push 'add-keymap-witness (cdr map))
        (set symbol map))
      (push keymap (cdr map)))))

(defun internal-pop-keymap (keymap symbol)
  (let ((map (symbol-value symbol)))
    (when (memq keymap map)
      (setf (cdr map) (delq keymap (cdr map))))
    (let ((tail (cddr map)))
      (and (or (null tail) (keymapp tail))
           (eq 'add-keymap-witness (nth 1 map))
           (set symbol tail)))))

(define-obsolete-function-alias
  'set-temporary-overlay-map #'set-transient-map "24.4")

(defvar set-transient-map-timeout nil
  "Timeout in seconds for deactivation of a transient keymap.
If this is a number, it specifies the amount of idle time
after which to deactivate the keymap set by `set-transient-map',
thus overriding the value of the TIMEOUT argument to that function.")

(defvar set-transient-map-timer nil
  "Timer for `set-transient-map-timeout'.")

(defun set-transient-map (map &optional keep-pred on-exit message timeout)
  "Set MAP as a temporary keymap taking precedence over other keymaps.
Normally, MAP is used only once, to look up the very next key.
However, if the optional argument KEEP-PRED is t, MAP stays
active if a key from MAP is used.  KEEP-PRED can also be a
function of no arguments: it is called from `pre-command-hook' and
if it returns non-nil, then MAP stays active.

Optional arg ON-EXIT, if non-nil, specifies a function that is
called, with no arguments, after MAP is deactivated.

Optional arg MESSAGE, if non-nil, requests display of an informative
message after activating the transient map.  If MESSAGE is a string,
it specifies the format string for the message to display, and the %k
specifier in the string is replaced with the list of keys from the
transient map.  Any other non-nil value of MESSAGE means to use the
message format string \"Repeat with %k\".  Upon deactivating the map,
the displayed message will be cleared out.

Optional arg TIMEOUT, if non-nil, should be a number specifying the
number of seconds of idle time after which the map is deactivated.
The variable `set-transient-map-timeout', if non-nil, overrides the
value of TIMEOUT.

This function uses `overriding-terminal-local-map', which takes precedence
over all other keymaps.  As usual, if no match for a key is found in MAP,
the normal key lookup sequence then continues.

This returns an \"exit function\", which can be called with no argument
to deactivate this transient map, regardless of KEEP-PRED."
  (let* ((timeout (or set-transient-map-timeout timeout))
         (message
          (when message
            (let (keys)
              (map-keymap (lambda (key cmd) (and cmd (push key keys))) map)
              (format-spec (if (stringp message) message "Repeat with %k")
                           `((?k . ,(mapconcat
                                     (lambda (key)
                                       (substitute-command-keys
                                        (format "\\`%s'"
                                                (key-description (vector key)))))
                                     keys ", ")))))))
         (clearfun (make-symbol "clear-transient-map"))
         (exitfun
          (lambda ()
            (internal-pop-keymap map 'overriding-terminal-local-map)
            (remove-hook 'pre-command-hook clearfun)
            ;; Clear the prompt after exiting.
            (when message (message ""))
            (when set-transient-map-timer (cancel-timer set-transient-map-timer))
            (when on-exit (funcall on-exit)))))
    ;; Don't use letrec, because equal (in add/remove-hook) could get trapped
    ;; in a cycle. (bug#46326)
    (fset clearfun
          (lambda ()
            (with-demoted-errors "set-transient-map PCH: %S"
              (if (cond
                       ((null keep-pred) nil)
                       ((and (not (eq map (cadr overriding-terminal-local-map)))
                             (memq map (cddr overriding-terminal-local-map)))
                        ;; There's presumably some other transient-map in
                        ;; effect.  Wait for that one to terminate before we
                        ;; remove ourselves.
                        ;; For example, if isearch and C-u both use transient
                        ;; maps, then the lifetime of the C-u should be nested
                        ;; within isearch's, so the pre-command-hook of
                        ;; isearch should be suspended during the C-u one so
                        ;; we don't exit isearch just because we hit 1 after
                        ;; C-u and that 1 exits isearch whereas it doesn't
                        ;; exit C-u.
                        t)
                       ((eq t keep-pred)
                        (let ((mc (lookup-key map (this-command-keys-vector))))
                          ;; We may have a remapped command, so chase
                          ;; down that.
                          (when (and mc (symbolp mc))
                            (setq mc (or (command-remapping mc) mc)))
                          ;; If the key is unbound `this-command` is
                          ;; nil and so is `mc`.
                          (and mc (eq this-command mc))))
                       (t (funcall keep-pred)))
                  ;; Repeat the message for the next command.
                  (when message (message "%s" message))
                (funcall exitfun)))))
    (add-hook 'pre-command-hook clearfun)
    (internal-push-keymap map 'overriding-terminal-local-map)
    (when timeout
      (when set-transient-map-timer (cancel-timer set-transient-map-timer))
      (setq set-transient-map-timer (run-with-idle-timer timeout nil exitfun)))
    (when message (message "%s" message))
    exitfun))

;;;; Progress reporters.

;; Progress reporter has the following structure:
;;
;;	(NEXT-UPDATE-VALUE . [NEXT-UPDATE-TIME
;;			      MIN-VALUE
;;			      MAX-VALUE
;;			      MESSAGE
;;			      MIN-CHANGE
;;                            MIN-TIME
;;                            MESSAGE-SUFFIX])
;;
;; This weirdness is for optimization reasons: we want
;; `progress-reporter-update' to be as fast as possible, so
;; `(car reporter)' is better than `(aref reporter 0)'.
;;
;; NEXT-UPDATE-TIME is a float.  While `float-time' loses a couple
;; digits of precision, it doesn't really matter here.  On the other
;; hand, it greatly simplifies the code.

(defvar progress-reporter-update-functions (list #'progress-reporter-echo-area)
  "Special hook run on progress-reporter updates.
Each function is called with two arguments:
REPORTER is the result of a call to `make-progress-reporter'.
STATE can be one of:
- A float representing the percentage complete in the range 0.0-1.0
for a numeric reporter.
- An integer representing the index which cycles through the range 0-3
for a pulsing reporter.
- The symbol `done' to indicate that the progress reporter is complete.")

(defsubst progress-reporter-update (reporter &optional value suffix)
  "Report progress of an operation, by default, in the echo area.
REPORTER should be the result of a call to `make-progress-reporter'.

If REPORTER is a numerical progress reporter---i.e. if it was
made using non-nil MIN-VALUE and MAX-VALUE arguments to
`make-progress-reporter'---then VALUE should be a number between
MIN-VALUE and MAX-VALUE.

Optional argument SUFFIX is a string to be displayed after REPORTER's
main message and progress text.  If REPORTER is a non-numerical
reporter, then VALUE should be nil, or a string to use instead of
SUFFIX.  SUFFIX is considered obsolete and may be removed in the future.

See `progress-reporter-update-functions' for the list of functions
called on each update.

This function is relatively inexpensive.  If the change since
last update is too small or insufficient time has passed, it does
nothing."
  (when (or (not (numberp value))      ; For pulsing reporter
	    (>= value (car reporter))) ; For numerical reporter
    (progress-reporter-do-update reporter value suffix)))

(defun make-progress-reporter (message &optional min-value max-value
				       current-value min-change min-time
                                       context)
  "Return progress reporter object for use with `progress-reporter-update'.

MESSAGE is shown in the echo area, with a status indicator
appended to the end.  When you call `progress-reporter-done', the
word \"done\" is printed after the MESSAGE.  You can change the
MESSAGE of an existing progress reporter by calling
`progress-reporter-force-update'.

MIN-VALUE and MAX-VALUE, if non-nil, are starting (0% complete)
and final (100% complete) states of operation; the latter should
be larger.  In this case, the status message shows the percentage
progress.

If MIN-VALUE and/or MAX-VALUE is omitted or nil, the status
message shows a \"spinning\", non-numeric indicator.

Optional CURRENT-VALUE is the initial progress; the default is
MIN-VALUE.
Optional MIN-CHANGE is the minimal change in percents to report;
the default is 1%.
CURRENT-VALUE and MIN-CHANGE do not have any effect if MIN-VALUE
and/or MAX-VALUE are nil.

Optional MIN-TIME specifies the minimum interval time between
echo area updates (default is 0.2 seconds.)  If the OS is not
capable of measuring fractions of seconds, this parameter is
effectively rounded up.

Optional CONTEXT can be nil or `async'.  It is consulted by back ends before
showing progress updates.  For example, when CONTEXT is `async',
the echo area progress reports may be muted if the echo area is busy."
  (when (string-match "[[:alnum:]]\\'" message)
    (setq message (concat message "...")))
  (unless min-time
    (setq min-time 0.2))
  (let ((reporter
	 (cons (or min-value 0)
	       ;; FIXME: Use defstruct.
	       (vector (if (>= min-time 0.02)
			   (float-time) nil)
		       min-value
		       max-value
		       message
		       (if min-change (max (min min-change 50) 1) 1)
                       min-time
                       ;; SUFFIX
                       nil
                       ;;
                       context))))
    ;; Force a call to `message' now.
    (progress-reporter-update reporter (or current-value min-value))
    reporter))

(defalias 'progress-reporter-make #'make-progress-reporter)

(defun progress-reporter-text (reporter)
  "Return REPORTER's text."
  (aref (cdr reporter) 3))

(defun progress-reporter-context (reporter)
  "Return REPORTER's context."
  (aref (cdr reporter) 7))

(defun progress-reporter-force-update (reporter &optional value new-message suffix)
  "Report progress of an operation in the echo area unconditionally.

REPORTER, VALUE, and SUFFIX are the same as in `progress-reporter-update'.
NEW-MESSAGE, if non-nil, sets a new message for the reporter."
  (let ((parameters (cdr reporter)))
    (when new-message
      (aset parameters 3 new-message))
    (when (aref parameters 0)
      (aset parameters 0 (float-time)))
    (progress-reporter-do-update reporter value suffix)))

(defvar progress-reporter--pulse-characters ["-" "\\" "|" "/"]
  "Characters to use for pulsing progress reporters.")

(defun progress-reporter-echo-area (reporter state)
  "Progress reporter echo area update function.
REPORTER and STATE are the same as in
`progress-reporter-update-functions'.

Do not emit a message if the reporter context is `async' and the echo
area is busy with something else."
  (let ((text (progress-reporter-text reporter)))
    (unless (and (eq (progress-reporter-context reporter) 'async)
                 (current-message)
                 (not (string-prefix-p text (current-message))))
      (pcase state
        ((pred floatp)
         (if (plusp state)
             (message "%s%d%%" text (* state 100.0))
           (message "%s" text)))
        ((pred integerp)
         (let ((message-log-max nil)
               (pulse-char (aref progress-reporter--pulse-characters
                                 state)))
           (message "%s %s" text pulse-char)))
        ('done
         (message "%sdone" text))))))

(defun progress-reporter-do-update (reporter value &optional suffix)
  (let* ((parameters      (cdr reporter))
	 (update-time     (aref parameters 0))
	 (min-value       (aref parameters 1))
	 (max-value       (aref parameters 2))
	 (enough-time-passed
	  ;; See if enough time has passed since the last update.
	  (or (not update-time)
	      (when (time-less-p update-time nil)
		;; Calculate time for the next update
		(aset parameters 0 (+ update-time (aref parameters 5)))))))
    (cond ((and min-value max-value)
	   ;; Numerical indicator
	   (let* ((one-percent (/ (- max-value min-value) 100.0))
		  (percentage  (if (= max-value min-value)
				   0
				 (truncate (/ (- value min-value)
					      one-percent)))))
	     ;; Calculate NEXT-UPDATE-VALUE.  If we are not printing
	     ;; message because not enough time has passed, use 1
	     ;; instead of MIN-CHANGE.  This makes delays between echo
	     ;; area updates closer to MIN-TIME.
	     (setcar reporter
		     (min (+ min-value (* (+ percentage
					     (if enough-time-passed
						 ;; MIN-CHANGE
						 (aref parameters 4)
					       1))
					  one-percent))
			  max-value))
	     (when (integerp value)
	       (setcar reporter (ceiling (car reporter))))
	     ;; Print message only if enough time has passed
	     (when enough-time-passed
               (if suffix
                   (aset parameters 6 suffix)
                 (setq suffix (or (aref parameters 6) "")))
               (run-hook-with-args 'progress-reporter-update-functions
                                   reporter
                                   (/ percentage 100.0)))))
	  ;; Pulsing indicator
	  (enough-time-passed
           (when (and value (not suffix))
             (setq suffix value))
           (if suffix
               (aset parameters 6 suffix)
             (setq suffix (or (aref parameters 6) "")))
           (let ((index (mod (1+ (car reporter)) 4)))
	     (setcar reporter index)
             (run-hook-with-args 'progress-reporter-update-functions
                                 reporter
                                 index))))))

(defun progress-reporter-done (reporter)
  "Print reporter's message followed by word \"done\" in echo area.
Call the functions on `progress-reporter-update-functions`."
  (run-hook-with-args 'progress-reporter-update-functions
                      reporter
                      'done))

(defmacro dotimes-with-progress-reporter (spec reporter-or-message &rest body)
  "Loop a certain number of times and report progress in the echo area.
Evaluate BODY with VAR bound to successive integers running from
0, inclusive, to COUNT, exclusive.  Then evaluate RESULT to get
the return value (nil if RESULT is omitted).

REPORTER-OR-MESSAGE is a progress reporter object or a string.  In the latter
case, use this string to create a progress reporter.

At each iteration, print the reporter message followed by progress
percentage in the echo area.  After the loop is finished,
print the reporter message followed by the word \"done\".

This macro is a convenience wrapper around `make-progress-reporter' and friends.

\(fn (VAR COUNT [RESULT]) REPORTER-OR-MESSAGE BODY...)"
  (declare (indent 2) (debug ((symbolp form &optional form) form body)))
  (let ((prep (make-symbol "--dotimes-prep--"))
        (end (make-symbol "--dotimes-end--")))
    `(let ((,prep ,reporter-or-message)
           (,end ,(cadr spec)))
       (when (stringp ,prep)
         (setq ,prep (make-progress-reporter ,prep 0 ,end)))
       (dotimes (,(car spec) ,end)
         ,@body
         (progress-reporter-update ,prep (1+ ,(car spec))))
       (progress-reporter-done ,prep)
       (or ,@(cdr (cdr spec)) nil))))

(defmacro dolist-with-progress-reporter (spec reporter-or-message &rest body)
  "Loop over a list and report progress in the echo area.
Evaluate BODY with VAR bound to each car from LIST, in turn.
Then evaluate RESULT to get return value, default nil.

REPORTER-OR-MESSAGE is a progress reporter object or a string.  In the latter
case, use this string to create a progress reporter.

At each iteration, print the reporter message followed by progress
percentage in the echo area.  After the loop is finished,
print the reporter message followed by the word \"done\".

\(fn (VAR LIST [RESULT]) REPORTER-OR-MESSAGE BODY...)"
  (declare (indent 2) (debug ((symbolp form &optional form) form body)))
  (let ((prep (make-symbol "--dolist-progress-reporter--"))
        (count (make-symbol "--dolist-count--"))
        (list (make-symbol "--dolist-list--")))
    `(let ((,prep ,reporter-or-message)
           (,count 0)
           (,list ,(cadr spec)))
       (when (stringp ,prep)
         (setq ,prep (make-progress-reporter ,prep 0 (length ,list))))
       (dolist (,(car spec) ,list)
         ,@body
         (progress-reporter-update ,prep (setq ,count (1+ ,count))))
       (progress-reporter-done ,prep)
       (or ,@(cdr (cdr spec)) nil))))


;;;; Comparing version strings.

(defconst version-separator "."
  "Specify the string used to separate the version elements.

Usually the separator is \".\", but it can be any other string.")


(defconst version-regexp-alist
  '(("^[-._+ ]?snapshot$"                                 . -4)
    ;; treat "1.2.3-20050920" and "1.2-3" as snapshot releases
    ("^[-._+]$"                                           . -4)
    ;; treat "1.2.3-CVS" as snapshot release
    ("^[-._+ ]?\\(cvs\\|git\\|bzr\\|svn\\|hg\\|darcs\\)$" . -4)
    ;; treat "-unknown" the same as snapshots.
    ("^[-._+ ]?unknown$"                                  . -4)
    ("^[-._+ ]?alpha$"                                    . -3)
    ("^[-._+ ]?beta$"                                     . -2)
    ("^[-._+ ]?\\(pre\\|rc\\)$"                           . -1))
  "Specify association between non-numeric version and its priority.

This association is used to handle version string like \"1.0pre2\",
\"0.9alpha1\", etc.  It's used by `version-to-list' (which see) to convert the
non-numeric part of a version string to an integer.  For example:

   String Version    Integer List Version
   \"0.9snapshot\"     (0  9 -4)
   \"1.0-git\"         (1  0 -4)
   \"1.0.cvs\"         (1  0 -4)
   \"1.0pre2\"         (1  0 -1 2)
   \"1.0PRE2\"         (1  0 -1 2)
   \"22.8beta3\"       (22 8 -2 3)
   \"22.8 Beta3\"      (22 8 -2 3)
   \"0.9alpha1\"       (0  9 -3 1)
   \"0.9AlphA1\"       (0  9 -3 1)
   \"0.9 alpha\"       (0  9 -3)

Each element has the following form:

   (REGEXP . PRIORITY)

Where:

REGEXP		regexp used to match non-numeric part of a version string.
		It should begin with the `^' anchor and end with a `$' to
		prevent false hits.  Letter-case is ignored while matching
		REGEXP.

PRIORITY	a negative integer specifying non-numeric priority of REGEXP.")


(defun version-to-list (ver)
  "Convert version string VER into a list of integers.

The version syntax is given by the following EBNF:

   VERSION ::= NUMBER ( SEPARATOR NUMBER )*.

   NUMBER ::= (0|1|2|3|4|5|6|7|8|9)+.

   SEPARATOR ::= `version-separator' (which see)
	       | `version-regexp-alist' (which see).

The NUMBER part is optional if SEPARATOR is a match for an element
in `version-regexp-alist'.

Examples of valid version syntax:

   1.0pre2   1.0.7.5   22.8beta3   0.9alpha1   6.9.30Beta   2.4.snapshot   .5

Examples of invalid version syntax:

   1.0prepre2   1.0..7.5   22.8X3   alpha3.2

Examples of version conversion:

   Version String    Version as a List of Integers
   \".5\"              (0 5)
   \"0.9 alpha\"       (0  9 -3)
   \"0.9AlphA1\"       (0  9 -3 1)
   \"0.9snapshot\"     (0  9 -4)
   \"1.0-git\"         (1  0 -4)
   \"1.0.7.5\"         (1  0  7 5)
   \"1.0.cvs\"         (1  0 -4)
   \"1.0PRE2\"         (1  0 -1 2)
   \"1.0pre2\"         (1  0 -1 2)
   \"22.8 Beta3\"      (22 8 -2 3)
   \"22.8beta3\"       (22 8 -2 3)

See documentation for `version-separator' and `version-regexp-alist'."
  (declare (side-effect-free t))
  (unless (stringp ver)
    (error "Version must be a string"))
  ;; Change .x.y to 0.x.y
  (if (and (>= (length ver) (length version-separator))
	   (string-equal (substring ver 0 (length version-separator))
			 version-separator))
      (setq ver (concat "0" ver)))
  (unless (string-match-p "^[0-9]" ver)
    (error "Invalid version syntax: `%s' (must start with a number)" ver))

  (save-match-data
    (let ((i 0)
	  (case-fold-search t)		; ignore case in matching
	  lst s al)
      ;; Parse the version-string up to a separator until there are none left
      (while (and (setq s (string-match "[0-9]+" ver i))
		  (= s i))
        ;; Add the numeric part to the beginning of the version list;
        ;; lst gets reversed at the end
	(setq lst (cons (string-to-number (substring ver i (match-end 0)))
			lst)
	      i   (match-end 0))
	;; handle non-numeric part
	(when (and (setq s (string-match "[^0-9]+" ver i))
		   (= s i))
	  (setq s (substring ver i (match-end 0))
		i (match-end 0))
	  ;; handle alpha, beta, pre, etc. separator
	  (unless (string= s version-separator)
	    (setq al version-regexp-alist)
	    (while (and al (not (string-match (caar al) s)))
	      (setq al (cdr al)))
	    (cond (al
		   (push (cdar al) lst))
                  ;; Convert 22.3a to 22.3.1, 22.3b to 22.3.2, etc., but only if
                  ;; the letter is the end of the version-string, to avoid
                  ;; 22.8X3 being valid
                  ((and (string-match "^[-._+ ]?\\([a-zA-Z]\\)$" s)
                        (= i (length ver)))
		   (push (- (aref (downcase (match-string 1 s)) 0) ?a -1)
			 lst))
		  (t (error "Invalid version syntax: `%s'" ver))))))
      (nreverse lst))))

(defun version-list-< (l1 l2)
  "Return t if L1, a list specification of a version, is lower than L2.

Note that a version specified by the list (1) is equal to (1 0),
\(1 0 0), (1 0 0 0), etc.  That is, the trailing zeros are insignificant.
Also, a version given by the list (1) is higher than (1 -1), which in
turn is higher than (1 -2), which is higher than (1 -3)."
  (declare (pure t) (side-effect-free t))
  (while (and l1 l2 (= (car l1) (car l2)))
    (setq l1 (cdr l1)
	  l2 (cdr l2)))
  (cond
   ;; l1 not null and l2 not null
   ((and l1 l2) (< (car l1) (car l2)))
   ;; l1 null and l2 null         ==> l1 length = l2 length
   ((and (null l1) (null l2)) nil)
   ;; l1 not null and l2 null     ==> l1 length > l2 length
   (l1 (minusp (version-list-not-zero l1)))
   ;; l1 null and l2 not null     ==> l2 length > l1 length
   (t  (plusp (version-list-not-zero l2)))))


(defun version-list-= (l1 l2)
  "Return t if L1, a list specification of a version, is equal to L2.

Note that a version specified by the list (1) is equal to (1 0),
\(1 0 0), (1 0 0 0), etc.  That is, the trailing zeros are insignificant.
Also, a version given by the list (1) is higher than (1 -1), which in
turn is higher than (1 -2), which is higher than (1 -3)."
  (declare (pure t) (side-effect-free t))
  (while (and l1 l2 (= (car l1) (car l2)))
    (setq l1 (cdr l1)
	  l2 (cdr l2)))
  (cond
   ;; l1 not null and l2 not null
   ((and l1 l2) nil)
   ;; l1 null and l2 null     ==> l1 length = l2 length
   ((and (null l1) (null l2)))
   ;; l1 not null and l2 null ==> l1 length > l2 length
   (l1 (zerop (version-list-not-zero l1)))
   ;; l1 null and l2 not null ==> l2 length > l1 length
   (t  (zerop (version-list-not-zero l2)))))


(defun version-list-<= (l1 l2)
  "Return t if L1, a list specification of a version, is lower or equal to L2.

Note that integer list (1) is equal to (1 0), (1 0 0), (1 0 0 0),
etc.  That is, the trailing zeroes are insignificant.  Also, integer
list (1) is greater than (1 -1) which is greater than (1 -2)
which is greater than (1 -3)."
  (declare (pure t) (side-effect-free t))
  (while (and l1 l2 (= (car l1) (car l2)))
    (setq l1 (cdr l1)
	  l2 (cdr l2)))
  (cond
   ;; l1 not null and l2 not null
   ((and l1 l2) (< (car l1) (car l2)))
   ;; l1 null and l2 null     ==> l1 length = l2 length
   ((and (null l1) (null l2)))
   ;; l1 not null and l2 null ==> l1 length > l2 length
   (l1 (<= (version-list-not-zero l1) 0))
   ;; l1 null and l2 not null ==> l2 length > l1 length
   (t  (<= 0 (version-list-not-zero l2)))))

(defun version-list-not-zero (lst)
  "Return the first non-zero element of LST, which is a list of integers.

If all LST elements are zeros or LST is nil, return zero."
  (declare (pure t) (side-effect-free t))
  (while (and lst (zerop (car lst)))
    (setq lst (cdr lst)))
  (if lst
      (car lst)
    ;; there is no element different of zero
    0))


(defun version< (v1 v2)
  "Return t if version V1 is lower (older) than V2.

Note that version string \"1\" is equal to \"1.0\", \"1.0.0\", \"1.0.0.0\",
etc.  That is, the trailing \".0\"s are insignificant.  Also, version
string \"1\" is higher (newer) than \"1pre\", which is higher than \"1beta\",
which is higher than \"1alpha\", which is higher than \"1snapshot\".
Also, \"-GIT\", \"-CVS\" and \"-NNN\" are treated as snapshot versions."
  (declare (side-effect-free t))
  (version-list-< (version-to-list v1) (version-to-list v2)))

(defun version<= (v1 v2)
  "Return t if version V1 is lower (older) than or equal to V2.

Note that version string \"1\" is equal to \"1.0\", \"1.0.0\", \"1.0.0.0\",
etc.  That is, the trailing \".0\"s are insignificant.  Also, version
string \"1\" is higher (newer) than \"1pre\", which is higher than \"1beta\",
which is higher than \"1alpha\", which is higher than \"1snapshot\".
Also, \"-GIT\", \"-CVS\" and \"-NNN\" are treated as snapshot versions."
  (declare (side-effect-free t))
  (version-list-<= (version-to-list v1) (version-to-list v2)))

(defun version= (v1 v2)
  "Return t if version V1 is equal to V2.

Note that version string \"1\" is equal to \"1.0\", \"1.0.0\", \"1.0.0.0\",
etc.  That is, the trailing \".0\"s are insignificant.  Also, version
string \"1\" is higher (newer) than \"1pre\", which is higher than \"1beta\",
which is higher than \"1alpha\", which is higher than \"1snapshot\".
Also, \"-GIT\", \"-CVS\" and \"-NNN\" are treated as snapshot versions."
  (declare (side-effect-free t))
  (version-list-= (version-to-list v1) (version-to-list v2)))

(defvar package--builtin-versions
  ;; Mostly populated by loaddefs.el.
  `((emacs . ,(version-to-list emacs-version)))
  "Alist giving the version of each versioned builtin package.
I.e. each element of the list is of the form (NAME . VERSION) where
NAME is the package name as a symbol, and VERSION is its version
as a list.")

(defun package--description-file (dir)
  "Return package description file name for package DIR."
  (concat (let ((subdir (file-name-nondirectory
                         (directory-file-name dir))))
            ;; This needs to match only the version strings that can be
            ;; generated by `package-version-join'.
            (if (string-match "\\([^.].*?\\)-\\([0-9]+\\(?:[.][0-9]+\\|\\(?:pre\\|beta\\|alpha\\|snapshot\\)[0-9]+\\)*\\)\\'" subdir)
                (match-string 1 subdir) subdir))
          "-pkg.el"))


;;; Thread support.

(defmacro with-mutex (mutex &rest body)
  "Invoke BODY with MUTEX held, releasing MUTEX when done.
This is the simplest safe way to acquire and release a mutex."
  (declare (indent 1) (debug t))
  (let ((sym (make-symbol "mutex")))
    `(let ((,sym ,mutex))
       (mutex-lock ,sym)
       (unwind-protect
	   (progn ,@body)
	 (mutex-unlock ,sym)))))


;;; Apropos.

(defun apropos-internal (regexp &optional predicate)
  "Show all symbols whose names contain match for REGEXP.
If optional 2nd arg PREDICATE is non-nil, (funcall PREDICATE SYMBOL) is done
for each symbol and a symbol is mentioned only if that returns non-nil.
Return list of symbols found."
  (let (found)
    (mapatoms (lambda (symbol)
                (when (and (string-match regexp (symbol-name symbol))
                           (or (not predicate)
                               (funcall predicate symbol)))
                  (push symbol found))))
    (sort found #'string-lessp)))


;;; Misc.

(defvar definition-prefixes (make-hash-table :test 'equal)
  "Hash table mapping prefixes to the files in which they're used.
This can be used to automatically fetch not-yet-loaded definitions.
More specifically, if there is a value of the form (FILES...) for
a string PREFIX it means that the FILES define variables or functions
with names that start with PREFIX.

Note that it does not imply that all definitions starting with PREFIX can
be found in those files.  E.g. if prefix is \"gnus-article-\" there might
still be definitions of the form \"gnus-article-toto-titi\" in other files,
which would presumably appear in this table under another prefix such as
\"gnus-\" or \"gnus-article-toto-\".")

(defun register-definition-prefixes (file prefixes)
  "Register that FILE uses PREFIXES."
  (dolist (prefix prefixes)
    (puthash prefix (cons file (gethash prefix definition-prefixes))
             definition-prefixes)))

(defconst menu-bar-separator '("--")
  "Separator for menus.")

;; The following statement ought to be in print.c, but `provide' can't
;; be used there.
;; https://lists.gnu.org/r/emacs-devel/2009-08/msg00236.html
(when (hash-table-p (car (read-from-string
			  (prin1-to-string (make-hash-table)))))
  (provide 'hashtable-print-readable))

;; This is used in lisp/Makefile.in and in leim/Makefile.in to
;; generate file names for autoloads, custom-deps, and finder-data.
(defun unmsys--file-name (file)
  "Produce the canonical file name for FILE from its MSYS form.

On systems other than MS-Windows, just returns FILE.
On MS-Windows, converts /d/foo/bar form of file names
passed by MSYS Make into d:/foo/bar that Emacs can grok.

This function is called from lisp/Makefile and leim/Makefile."
  (when (and (eq system-type 'windows-nt)
	     (string-match "\\`/[a-zA-Z]/" file))
    (setq file (concat (substring file 1 2) ":" (substring file 2))))
  file)

(defun flatten-tree (tree)
  "Return a \"flattened\" copy of TREE.
In other words, return a list of the non-nil terminal nodes, or
leaves, of the tree of cons cells rooted at TREE.  Leaves in the
returned list are in the same order as in TREE.

\(flatten-tree \\='(1 (2 . 3) nil (4 5 (6)) 7))
=> (1 2 3 4 5 6 7)"
  (declare (side-effect-free error-free))
  (let (elems)
    (while (consp tree)
      (let ((elem (pop tree)))
        (while (consp elem)
          (push (cdr elem) tree)
          (setq elem (car elem)))
        (if elem (push elem elems))))
    (if tree (push tree elems))
    (nreverse elems)))

;; Technically, `flatten-list' is a misnomer, but we provide it here
;; for discoverability:
(defalias 'flatten-list #'flatten-tree)

(defun string-trim-left (string &optional regexp)
  "Trim STRING of leading string matching REGEXP.

REGEXP defaults to \"[ \\t\\n\\r]+\"."
  (declare (important-return-value t))
  (if (string-match (if regexp
                        (concat "\\`\\(?:" regexp "\\)")
                      "\\`[ \t\n\r]+")
                    string)
      (substring string (match-end 0))
    string))

(defun string-trim-right (string &optional regexp)
  "Trim STRING of trailing string matching REGEXP.

REGEXP defaults to  \"[ \\t\\n\\r]+\"."
  (declare (side-effect-free t))
  (let ((i (string-match-p (if regexp
                               (concat "\\(?:" regexp "\\)\\'")
                             "[ \t\n\r]+\\'")
                           string)))
    (if i (substring string 0 i) string)))

(defun string-trim (string &optional trim-left trim-right)
  "Trim STRING of leading and trailing strings matching TRIM-LEFT and TRIM-RIGHT.

TRIM-LEFT and TRIM-RIGHT default to \"[ \\t\\n\\r]+\"."
  (declare (important-return-value t))
  (let* ((beg (and (string-match (if trim-left
                                     (concat "\\`\\(?:" trim-left "\\)")
                                   "\\`[ \t\n\r]+")
                                 string)
                   (match-end 0)))
         (end (string-match-p (if trim-right
                                  (concat "\\(?:" trim-right "\\)\\'")
                                "[ \t\n\r]+\\'")
                              string beg)))
    (if (or beg end)
        (substring string beg end)
      string)))

(let ((missing (make-symbol "missing")))
  (defsubst hash-table-contains-p (key table)
    "Return non-nil if TABLE has an element with KEY."
    (declare (side-effect-free t)
             (important-return-value t))
    (not (eq (gethash key table missing) missing))))

;; The initial anchoring is for better performance in searching matches.
(defconst regexp-unmatchable "\\`a\\`"
  "Standard regexp guaranteed not to match any string at all.")

(defun run-hook-query-error-with-timeout (hook)
  "Run HOOK, catching errors, and querying the user about whether to continue.
If a function in HOOK signals an error, the user will be prompted
whether to continue or not.  If the user doesn't respond,
evaluation will continue if the user doesn't respond within five
seconds."
  (run-hook-wrapped
   hook
   (lambda (fun)
     (condition-case err
         (funcall fun)
       (error
        (unless (y-or-n-p-with-timeout (format "Error %s; continue?" err)
                                       5 t)
          (error err))))
     ;; Continue running.
     nil)))

(defun internal--fill-string-single-line (str)
  "Fill string STR to `fill-column'.
This is intended for very simple filling while bootstrapping
Emacs itself, and does not support all the customization options
of fill.el (for example `fill-region')."
  (if (< (length str) fill-column)
      str
    (let* ((limit (min fill-column (length str)))
           (fst (substring str 0 limit))
           (lst (substring str limit)))
      (cond ((string-match "\\( \\)$" fst)
             (setq fst (replace-match "\n" nil nil fst 1)))
            ((string-match "^ \\(.*\\)" lst)
             (setq fst (concat fst "\n"))
             (setq lst (match-string 1 lst)))
            ((string-match ".*\\( \\(.+\\)\\)$" fst)
             (setq lst (concat (match-string 2 fst) lst))
             (setq fst (replace-match "\n" nil nil fst 1))))
      (concat fst (internal--fill-string-single-line lst)))))

(defun internal--format-docstring-line (string &rest objects)
  "Format a single line from a documentation string out of STRING and OBJECTS.
Signal an error if STRING contains a newline.
This is intended for internal use only.  Avoid using this for the
first line of a docstring; the first line should be a complete
sentence (see Info node `(elisp) Documentation Tips')."
  (when (string-match "\n" string)
    (error "Unable to fill string containing newline: %S" string))
  (internal--fill-string-single-line (apply #'format string objects)))

(defun json-available-p ()
  "Return non-nil if Emacs has native JSON support."
  t)

(defun ensure-list (object)
  "Return OBJECT as a list.
If OBJECT is already a list, return OBJECT itself.  If it's
not a list, return a one-element list containing OBJECT."
  (declare (side-effect-free error-free))
  (if (listp object)
      object
    (list object)))

(defmacro with-delayed-message (args &rest body)
  "Like `progn', but display MESSAGE if BODY takes longer than TIMEOUT seconds.
The MESSAGE form will be evaluated immediately, but the resulting
string will be displayed only if BODY takes longer than TIMEOUT seconds.

\(fn (TIMEOUT MESSAGE) &rest BODY)"
  (declare (indent 1))
  `(funcall-with-delayed-message ,(car args) ,(cadr args)
                                 (lambda ()
                                   ,@body)))

(defun function-alias-p (func &optional _noerror)
  "Return nil if FUNC is not a function alias.
If FUNC is a function alias, return the function alias chain."
  (declare (advertised-calling-convention (func) "30.1")
           (side-effect-free error-free))
  (let ((chain nil))
    (while (and (symbolp func)
                (setq func (symbol-function func))
                (symbolp func))
      (push func chain))
    (nreverse chain)))

(defun readablep (object)
  "Say whether OBJECT has a readable syntax.
This means that OBJECT can be printed out and then read back
again by the Lisp reader.  This function returns nil if OBJECT is
unreadable, and the printed representation (from `prin1') of
OBJECT if it is readable."
  (declare (side-effect-free error-free))
  (catch 'unreadable
    (let ((print-unreadable-function
           (lambda (_object _escape)
             (throw 'unreadable nil))))
      (prin1-to-string object))))

(defun delete-line ()
  "Delete the current line."
  (delete-region (pos-bol) (pos-bol 2)))

(defun ensure-empty-lines (&optional lines)
  "Ensure that there are LINES number of empty lines before point.
If LINES is nil or omitted, ensure that there is a single empty
line before point.

If called interactively, LINES is given by the prefix argument.

If there are more than LINES empty lines before point, the number
of empty lines is reduced to LINES.

If point is not at the beginning of a line, a newline character
is inserted before adjusting the number of empty lines."
  (interactive "p")
  (unless (bolp)
    (insert "\n"))
  (let ((lines (or lines 1))
        (start (save-excursion
                 (if (re-search-backward "[^\n]" nil t)
                     (+ (point) 2)
                   (point-min)))))
    (cond
     ((> (- (point) start) lines)
      (delete-region (point) (- (point) (- (point) start lines))))
     ((< (- (point) start) lines)
      (insert (make-string (- lines (- (point) start)) ?\n))))))

(defun string-lines (string &optional omit-empty keep-newlines)
  "Split STRING into a list of lines.
If OMIT-EMPTY, empty lines will be removed from the results.
If KEEP-NEWLINES, don't strip trailing newlines from the result
lines."
  (declare (side-effect-free t))
  (if (equal string "")
      (if omit-empty
          nil
        (list ""))
    (let ((lines nil)
          (start 0))
      (while (< start (length string))
        (let ((newline (string-search "\n" string start)))
          (if newline
              (progn
                (when (or (not omit-empty)
                          (not (= start newline)))
                  (let ((line (substring string start
                                         (if keep-newlines
                                             (1+ newline)
                                           newline))))
                    (when (not (and keep-newlines omit-empty
                                    (equal line "\n")))
                      (push line lines))))
                (setq start (1+ newline)))
            ;; No newline in the remaining part.
            (if (zerop start)
                ;; Avoid a string copy if there are no newlines at all.
                (push string lines)
              (push (substring string start) lines))
            (setq start (length string)))))
      (nreverse lines))))

(defvar buffer-match-p--past-warnings nil)

(defun buffer-match-p (condition buffer-or-name &rest args)
  "Return non-nil if BUFFER-OR-NAME matches CONDITION.
CONDITION is either:
- the symbol t, to always match,
- the symbol nil, which never matches,
- a regular expression, to match a buffer name,
- a predicate function that takes BUFFER-OR-NAME plus ARGS as
  arguments, and returns non-nil if the buffer matches,
- a cons-cell, where the car describes how to interpret the cdr.
  The car can be one of the following:
  * `derived-mode': the buffer matches if the buffer's major mode
    is derived from the major mode in the cons-cell's cdr, or from any
    major mode in the list as accepted by `provided-mode-derived-p'.
  * `major-mode': the buffer matches if the buffer's major mode
    is eq to the cons-cell's cdr.  Prefer using `derived-mode'
    instead when both can work.
  * `category': when this function is called from `display-buffer',
    the buffer matches if the caller of `display-buffer' provides
    `(category . SYMBOL)' in its ACTION argument, and SYMBOL is `eq'
    to the cons-cell's cdr.
  * `this-command': the buffer matches if the command now being executed
    is `eq' to or a `memq' of the cons-cell's cdr.
    (This case is not useful when calling `buffer-match-p' directly, but
    is needed to support the `this-command' buffer display condition
    entry.  See Info node `(elisp)Choosing Window'.)
  * `not': the cadr is interpreted as a negation of a condition.
  * `and': the cdr is a list of recursive conditions, that all have
    to be met.
  * `or': the cdr is a list of recursive condition, of which at
    least one has to be met."
  (letrec
      ((buffer (get-buffer buffer-or-name))
       (match
        (lambda (conditions)
          (catch 'match
            (dolist (condition conditions)
              (when (pcase condition
                      ('t t)
                      ((pred stringp)
                       (string-match-p condition (buffer-name buffer)))
                      ((pred functionp)
                       (if (cdr args)
                           ;; New in Emacs>29.1. no need for compatibility hack.
                           (apply condition buffer-or-name args)
                         (condition-case-unless-debug err
                             (apply condition buffer-or-name args)
                           (wrong-number-of-arguments
                            (unless (member condition
                                            buffer-match-p--past-warnings)
                              (message "%s" (error-message-string err))
                              (push condition buffer-match-p--past-warnings))
                            (apply condition buffer-or-name
                                   (if args nil '(nil)))))))
                      (`(category . ,category)
                       (eq (alist-get 'category (cdar args)) category))
                      (`(this-command . ,command-or-commands)
                       (if (listp command-or-commands)
                           (memq this-command command-or-commands)
                         (eq this-command command-or-commands)))
                      (`(major-mode . ,mode)
                       (eq
                        (buffer-local-value 'major-mode buffer)
                        mode))
                      (`(derived-mode . ,mode)
                       (provided-mode-derived-p
                        (buffer-local-value 'major-mode buffer)
                        mode))
                      (`(not . ,cond)
                       (not (funcall match cond)))
                      (`(or . ,args)
                       (funcall match args))
                      (`(and . ,args)
                       (catch 'fail
                         (dolist (c args)
                           (unless (funcall match (list c))
                             (throw 'fail nil)))
                         t)))
                (throw 'match t)))))))
    (funcall match (list condition))))

(defun match-buffers (condition &optional buffers &rest args)
  "Return a list of buffers that match CONDITION, or nil if none match.
See `buffer-match-p' for various supported CONDITIONs.
By default all buffers are checked, but the optional
argument BUFFERS can restrict that: its value should be
an explicit list of buffers to check.
Optional arguments ARGS are passed to `buffer-match-p', for
predicate conditions in CONDITION."
  (let (bufs)
    (dolist (buf (or buffers (buffer-list)))
      (when (apply #'buffer-match-p condition (get-buffer buf) args)
        (push buf bufs)))
    bufs))

(defmacro handler-bind (handlers &rest body)
  "Setup error HANDLERS around execution of BODY.
HANDLERS is a list of (CONDITIONS HANDLER) where
CONDITIONS should be a list of condition names (symbols) or
a single condition name, and HANDLER is a form whose evaluation
returns a function.
When an error is signaled during execution of BODY, if that
error matches CONDITIONS, then the associated HANDLER
function is called with the error object as argument.
HANDLERs can either transfer the control via a non-local exit,
or return normally.  If a handler returns normally, the search for an
error handler continues from where it left off."
  ;; FIXME: Completion support as in `condition-case'?
  (declare (indent 1) (debug ((&rest (sexp form)) body)))
  (let ((args '()))
    (dolist (cond+handler handlers)
      (let ((handler (car (cdr cond+handler)))
            (conds (car cond+handler)))
        (push `',(ensure-list conds) args)
        (push handler args)))
    `(handler-bind-1 (lambda () ,@body) ,@(nreverse args))))

(defmacro with-memoization (place &rest code)
  "Return the value of CODE and stash it in PLACE.
If PLACE's value is non-nil, then don't bother evaluating CODE
and return the value found in PLACE instead."
  (declare (indent 1) (debug (gv-place body)))
  (gv-letplace (getter setter) place
    `(or ,getter
         ,(macroexp-let2 nil val (macroexp-progn code)
            `(progn
               ,(funcall setter val)
               ,val)))))

(defun total-line-spacing (&optional line-spacing-param)
  "Return numeric value of line-spacing, summing it if it's a cons.
   When LINE-SPACING-PARAM is provided, calculate from it instead."
  (let ((v (or line-spacing-param line-spacing)))
    (pcase v
      ((pred numberp) v)
      (`(,above . ,below) (+ above below)))))

;;; subr.el ends here
