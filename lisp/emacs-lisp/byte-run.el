;;; byte-run.el --- byte-compiler support for inlining  -*- lexical-binding: t -*-

;; Copyright (C) 1992, 2001-2022 Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@lucid.com>
;;	Hallvard Furuseth <hbf@ulrik.uio.no>
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

;; interface to selectively inlining functions.
;; This only happens when source-code optimization is turned on.

;;; Code:

(defalias 'function-put
  ;; We don't want people to just use `put' because we can't conveniently
  ;; hook into `put' to remap old properties to new ones.  But for now, there's
  ;; no such remapping, so we just call `put'.
  #'(lambda (function prop value)
      "Set FUNCTION's property PROP to VALUE.
The namespace for PROP is shared with symbols.
So far, FUNCTION can only be a symbol, not a lambda expression."
      (put function prop value)))
(function-put 'defmacro 'doc-string-elt 3)
(function-put 'defmacro 'lisp-indent-function 2)

;; `macro-declaration-function' are both obsolete (as marked at the end of this
;; file) but used in many .elc files.

;; We don't use #' here, because it's an obsolete function, and we
;; can't use `with-suppressed-warnings' here due to how this file is
;; used in the bootstrapping process.
(defvar macro-declaration-function 'macro-declaration-function
  "Function to process declarations in a macro definition.
The function will be called with two args MACRO and DECL.
MACRO is the name of the macro being defined.
DECL is a list `(declare ...)' containing the declarations.
The value the function returns is not used.")

(defalias 'macro-declaration-function
  #'(lambda (macro decl)
      "Process a declaration found in a macro definition.
This is set as the value of the variable `macro-declaration-function'.
MACRO is the name of the macro being defined.
DECL is a list `(declare ...)' containing the declarations.
The return value of this function is not used."
      ;; We can't use `dolist' or `cadr' yet for bootstrapping reasons.
      (let (d)
        ;; Ignore the first element of `decl' (it's always `declare').
        (while (setq decl (cdr decl))
          (setq d (car decl))
          (if (and (consp d)
                   (listp (cdr d))
                   (null (cdr (cdr d))))
              (cond ((eq (car d) 'indent)
                     (put macro 'lisp-indent-function (car (cdr d))))
                    ((eq (car d) 'debug)
                     (put macro 'edebug-form-spec (car (cdr d))))
                    ((eq (car d) 'doc-string)
                     (put macro 'doc-string-elt (car (cdr d))))
                    (t
                     (message "Unknown declaration %s" d)))
            (message "Invalid declaration %s" d))))))

;; We define macro-declaration-alist here because it is needed to
;; handle declarations in macro definitions and this is the first file
;; loaded by loadup.el that uses declarations in macros.  We specify
;; the values as named aliases so that `describe-variable' prints
;; something useful; cf. Bug#40491.  We can only use backquotes inside
;; the lambdas and not for those properties that are used by functions
;; loaded before backquote.el.

(defalias 'byte-run--set-advertised-calling-convention
  #'(lambda (f _args arglist when)
      (list 'set-advertised-calling-convention
            (list 'quote f) (list 'quote arglist) (list 'quote when))))

(defalias 'byte-run--set-obsolete
  #'(lambda (f _args new-name when)
      (list 'make-obsolete
            (list 'quote f) (list 'quote new-name) (list 'quote when))))

(defalias 'byte-run--set-interactive-only
  #'(lambda (f _args instead)
      (list 'function-put (list 'quote f)
            ''interactive-only (list 'quote instead))))

(defalias 'byte-run--set-pure
  #'(lambda (f _args val)
      (list 'function-put (list 'quote f)
            ''pure (list 'quote val))))

(defalias 'byte-run--set-side-effect-free
  #'(lambda (f _args val)
      (list 'function-put (list 'quote f)
            ''side-effect-free (list 'quote val))))

(put 'compiler-macro 'edebug-declaration-spec
     '(&or symbolp ("lambda" &define lambda-list lambda-doc def-body)))

(defalias 'byte-run--set-compiler-macro
  #'(lambda (f args compiler-function)
      (if (not (eq (car-safe compiler-function) 'lambda))
          `(eval-and-compile
             (function-put ',f 'compiler-macro #',compiler-function))
        (let ((cfname (intern (concat (symbol-name f) "--anon-cmacro")))
              ;; Avoid cadr/cddr so we can use `compiler-macro' before
              ;; defining cadr/cddr.
              (data (cdr compiler-function)))
          `(progn
             (eval-and-compile
               (function-put ',f 'compiler-macro #',cfname))
             ;; Don't autoload the compiler-macro itself, since the
             ;; macroexpander will find this file via `f's autoload,
             ;; if needed.
             :autoload-end
             (eval-and-compile
               (defun ,cfname (,@(car data) ,@args)
                 ,@(cdr data))))))))

(defalias 'byte-run--set-doc-string
  #'(lambda (f _args pos)
      (list 'function-put (list 'quote f)
            ''doc-string-elt (list 'quote pos))))

(defalias 'byte-run--set-indent
  #'(lambda (f _args val)
      (list 'function-put (list 'quote f)
            ''lisp-indent-function (list 'quote val))))

(defalias 'byte-run--set-speed
  #'(lambda (f _args val)
      (list 'function-put (list 'quote f)
            ''speed (list 'quote val))))

(defalias 'byte-run--set-completion
  #'(lambda (f _args val)
      (list 'function-put (list 'quote f)
            ''completion-predicate (list 'function val))))

(defalias 'byte-run--set-modes
  #'(lambda (f _args &rest val)
      (list 'function-put (list 'quote f)
            ''command-modes (list 'quote val))))

;; Add any new entries to info node `(elisp)Declare Form'.
(defvar defun-declarations-alist
  (list
   (list 'advertised-calling-convention
         #'byte-run--set-advertised-calling-convention)
   (list 'obsolete #'byte-run--set-obsolete)
   (list 'interactive-only #'byte-run--set-interactive-only)
   ;; FIXME: Merge `pure' and `side-effect-free'.
   (list 'pure #'byte-run--set-pure
         "If non-nil, the compiler can replace calls with their return value.
This may shift errors from run-time to compile-time.")
   (list 'side-effect-free #'byte-run--set-side-effect-free
         "If non-nil, calls can be ignored if their value is unused.
If `error-free', drop calls even if `byte-compile-delete-errors' is nil.")
   (list 'compiler-macro #'byte-run--set-compiler-macro)
   (list 'doc-string #'byte-run--set-doc-string)
   (list 'indent #'byte-run--set-indent)
   (list 'speed #'byte-run--set-speed)
   (list 'completion #'byte-run--set-completion)
   (list 'modes #'byte-run--set-modes))
  "List associating function properties to their macro expansion.
Each element of the list takes the form (PROP FUN) where FUN is
a function.  For each (PROP . VALUES) in a function's declaration,
the FUN corresponding to PROP is called with the function name,
the function's arglist, and the VALUES and should return the code to use
to set this property.

This is used by `declare'.")

(defalias 'byte-run--set-debug
  #'(lambda (name _args spec)
      (list 'progn :autoload-end
	    (list 'put (list 'quote name)
		  ''edebug-form-spec (list 'quote spec)))))

(defalias 'byte-run--set-no-font-lock-keyword
  #'(lambda (name _args val)
      (list 'function-put (list 'quote name)
	    ''no-font-lock-keyword (list 'quote val))))

(defvar macro-declarations-alist
  (cons
   (list 'debug #'byte-run--set-debug)
   (cons
    (list 'no-font-lock-keyword #'byte-run--set-no-font-lock-keyword)
    defun-declarations-alist))
  "List associating properties of macros to their macro expansion.
Each element of the list takes the form (PROP FUN) where FUN is a function.
For each (PROP . VALUES) in a macro's declaration, the FUN corresponding
to PROP is called with the macro name, the macro's arglist, and the VALUES
and should return the code to use to set this property.

This is used by `declare'.")

(defalias 'defmacro
  (cons
   'macro
   #'(lambda (name arglist &optional docstring &rest body)
       "Define NAME as a macro.
When the macro is called, as in (NAME ARGS...),
the function (lambda ARGLIST BODY...) is applied to
the list ARGS... as it appears in the expression,
and the result should be a form to be evaluated instead of the original.
DECL is a declaration, optional, of the form (declare DECLS...) where
DECLS is a list of elements of the form (PROP . VALUES).  These are
interpreted according to `macro-declarations-alist'.
The return value is undefined.

\(fn NAME ARGLIST &optional DOCSTRING DECL &rest BODY)"
       ;; We can't just have `decl' as an &optional argument, because we need
       ;; to distinguish
       ;;    (defmacro foo (arg) (bar) nil)
       ;; from
       ;;    (defmacro foo (arg) (bar)).
       (let ((decls (cond
		     ((eq (car-safe docstring) 'declare)
		      (prog1 (cdr docstring) (setq docstring nil)))
		     ((and (stringp docstring)
			   (eq (car-safe (car body)) 'declare))
		      (prog1 (cdr (car body)) (setq body (cdr body)))))))
	 (if docstring (setq body (cons docstring body))
	   (if (null body) (setq body '(nil))))
	 ;; Can't use backquote because it's not defined yet!
	 (let* ((fun (list 'function (cons 'lambda (cons arglist body))))
		(def (list 'defalias
			   (list 'quote name)
			   (list 'cons ''macro fun)))
		(declarations
		 (mapcar
		  #'(lambda (x)
		      (let ((f (cdr (assq (car x) macro-declarations-alist))))
			(if f (apply (car f) name arglist (cdr x))
			  (macroexp-warn-and-return
			   (format-message
			    "Unknown macro property %S in %S"
			    (car x) name)
			   nil))))
		  decls)))
	   ;; Refresh font-lock if this is a new macro, or it is an
	   ;; existing macro whose 'no-font-lock-keyword declaration
	   ;; has changed.
	   (if (and
		;; If lisp-mode hasn't been loaded, there's no reason
		;; to flush.
		(fboundp 'lisp--el-font-lock-flush-elisp-buffers)
		(or (not (fboundp name)) ;; new macro
		    (and (fboundp name)  ;; existing macro
			 (member `(function-put ',name 'no-font-lock-keyword
						',(get name 'no-font-lock-keyword))
				 declarations))))
	       (lisp--el-font-lock-flush-elisp-buffers))
	   (if declarations
	       (cons 'prog1 (cons def declarations))
	     def))))))

;; Now that we defined defmacro we can use it!
(defmacro defun (name arglist &optional docstring &rest body)
  "Define NAME as a function.
The definition is (lambda ARGLIST [DOCSTRING] BODY...).
See also the function `interactive'.
DECL is a declaration, optional, of the form (declare DECLS...) where
DECLS is a list of elements of the form (PROP . VALUES).  These are
interpreted according to `defun-declarations-alist'.
The return value is undefined.

\(fn NAME ARGLIST &optional DOCSTRING DECL &rest BODY)"
  ;; We can't just have `decl' as an &optional argument, because we need
  ;; to distinguish
  ;;    (defun foo (arg) (toto) nil)
  ;; from
  ;;    (defun foo (arg) (toto)).
  (declare (doc-string 3) (indent 2))
  (or name (error "Cannot define '%s' as a function" name))
  (if (null
       (and (listp arglist)
            (null (delq t (mapcar #'symbolp arglist)))))
      (error "Malformed arglist: %s" arglist))
  (let ((decls (cond
                ((eq (car-safe docstring) 'declare)
                 (prog1 (cdr docstring) (setq docstring nil)))
                ((and (stringp docstring)
		      (eq (car-safe (car body)) 'declare))
                 (prog1 (cdr (car body)) (setq body (cdr body)))))))
    (if docstring (setq body (cons docstring body))
      (if (null body) (setq body '(nil))))
    (let ((declarations
           (mapcar
            #'(lambda (x)
                (let ((f (cdr (assq (car x) defun-declarations-alist))))
                  (cond
                   (f (apply (car f) name arglist (cdr x)))
                   ;; Yuck!!
                   ((and (featurep 'cl)
                         (memq (car x)  ;C.f. cl-do-proclaim.
                               '(special inline notinline optimize warn)))
                    (push (list 'declare x)
                          (if (stringp docstring)
                              (if (eq (car-safe (cadr body)) 'interactive)
                                  (cddr body)
                                (cdr body))
                            (if (eq (car-safe (car body)) 'interactive)
                                (cdr body)
                              body)))
                    nil)
                   (t
                    (macroexp-warn-and-return
                     (format-message "Unknown defun property `%S' in %S"
                                     (car x) name)
                     nil)))))
            decls))
          (def (list 'defalias
                     (list 'quote name)
                     (list 'function
                           (cons 'lambda
                                 (cons arglist body))))))
      (if declarations
          (cons 'prog1 (cons def declarations))
          def))))


;; Redefined in byte-opt.el.
;; This was undocumented and unused for decades.
(defalias 'inline 'progn
  "Like `progn', but when compiled inline top-level function calls in body.
You don't need this.  (See bytecomp.el commentary for more details.)

\(fn BODY...)")

;;; Interface to inline functions.

;; (defmacro proclaim-inline (&rest fns)
;;   "Cause the named functions to be open-coded when called from compiled code.
;; They will only be compiled open-coded when byte-compile-optimize is true."
;;   (cons 'eval-and-compile
;; 	(mapcar (lambda (x)
;; 		   (or (memq (get x 'byte-optimizer)
;; 			     '(nil byte-compile-inline-expand))
;; 		       (error
;; 			"%s already has a byte-optimizer, can't make it inline"
;; 			x))
;; 		   (list 'put (list 'quote x)
;; 			 ''byte-optimizer ''byte-compile-inline-expand))
;; 		fns)))

;; (defmacro proclaim-notinline (&rest fns)
;;   "Cause the named functions to no longer be open-coded."
;;   (cons 'eval-and-compile
;; 	(mapcar (lambda (x)
;; 		   (if (eq (get x 'byte-optimizer) 'byte-compile-inline-expand)
;; 		       (put x 'byte-optimizer nil))
;; 		   (list 'if (list 'eq (list 'get (list 'quote x) ''byte-optimizer)
;; 				   ''byte-compile-inline-expand)
;; 			 (list 'put x ''byte-optimizer nil)))
;; 		fns)))

(defmacro defsubst (name arglist &rest body)
  "Define an inline function.  The syntax is just like that of `defun'.

\(fn NAME ARGLIST &optional DOCSTRING DECL &rest BODY)"
  (declare (debug defun) (doc-string 3))
  (or (memq (get name 'byte-optimizer)
	    '(nil byte-compile-inline-expand))
      (error "`%s' is a primitive" name))
  `(prog1
       (defun ,name ,arglist ,@body)
     (eval-and-compile
       ;; Never native-compile defsubsts as we need the byte
       ;; definition in `byte-compile-unfold-bcf' to perform the
       ;; inlining (Bug#42664, Bug#43280, Bug#44209).
       ,(byte-run--set-speed name nil -1)
       (put ',name 'byte-optimizer 'byte-compile-inline-expand))))

(defvar advertised-signature-table (make-hash-table :test 'eq :weakness 'key))

(defun set-advertised-calling-convention (function signature _when)
  "Set the advertised SIGNATURE of FUNCTION.
This will allow the byte-compiler to warn the programmer when she uses
an obsolete calling convention.  WHEN specifies since when the calling
convention was modified."
  (puthash (indirect-function function) signature
           advertised-signature-table))

(defun make-obsolete (obsolete-name current-name when)
  "Make the byte-compiler warn that function OBSOLETE-NAME is obsolete.
OBSOLETE-NAME should be a function name or macro name (a symbol).

The warning will say that CURRENT-NAME should be used instead.
If CURRENT-NAME is a string, that is the `use instead' message
\(it should end with a period, and not start with a capital).
WHEN should be a string indicating when the function
was first made obsolete, for example a date or a release number."
  (put obsolete-name 'byte-obsolete-info
       ;; The second entry used to hold the `byte-compile' handler, but
       ;; is not used any more nowadays.
       (purecopy (list current-name nil when)))
  obsolete-name)

(defmacro define-obsolete-function-alias ( obsolete-name current-name when
                                           &optional docstring)
  "Set OBSOLETE-NAME's function definition to CURRENT-NAME and mark it obsolete.

\(define-obsolete-function-alias \\='old-fun \\='new-fun \"28.1\" \
\"old-fun's doc.\")

is equivalent to the following two lines of code:

\(defalias \\='old-fun \\='new-fun \"old-fun's doc.\")
\(make-obsolete \\='old-fun \\='new-fun \"28.1\")

WHEN should be a string indicating when the function was first
made obsolete, for example a date or a release number.

See the docstrings of `defalias' and `make-obsolete' for more details."
  (declare (doc-string 4))
  `(progn
     (defalias ,obsolete-name ,current-name ,docstring)
     (make-obsolete ,obsolete-name ,current-name ,when)))

(defun make-obsolete-variable ( obsolete-name current-name when
                                &optional access-type)
  "Make the byte-compiler warn that OBSOLETE-NAME is obsolete.
The warning will say that CURRENT-NAME should be used instead.
If CURRENT-NAME is a string, that is the `use instead' message.
WHEN should be a string indicating when the variable
was first made obsolete, for example a date or a release number.
ACCESS-TYPE if non-nil should specify the kind of access that will trigger
  obsolescence warnings; it can be either `get' or `set'."
  (put obsolete-name 'byte-obsolete-variable
       (purecopy (list current-name access-type when)))
  obsolete-name)


(defmacro define-obsolete-variable-alias ( obsolete-name current-name when
                                           &optional docstring)
  "Make OBSOLETE-NAME a variable alias for CURRENT-NAME and mark it obsolete.

WHEN should be a string indicating when the variable was first
made obsolete, for example a date or a release number.

This macro evaluates all its parameters, and both OBSOLETE-NAME
and CURRENT-NAME should be symbols, so a typical usage would look like:

  (define-obsolete-variable-alias 'foo-thing 'bar-thing \"28.1\")

This macro uses `defvaralias' and `make-obsolete-variable' (which see).
See the Info node `(elisp)Variable Aliases' for more details.

If CURRENT-NAME is a defcustom or a defvar (more generally, any variable
where OBSOLETE-NAME may be set, e.g. in an init file, before the
alias is defined), then the define-obsolete-variable-alias
statement should be evaluated before the defcustom, if user
customizations are to be respected.  The simplest way to achieve
this is to place the alias statement before the defcustom (this
is not necessary for aliases that are autoloaded, or in files
dumped with Emacs).  This is so that any user customizations are
applied before the defcustom tries to initialize the
variable (this is due to the way `defvaralias' works).

For the benefit of Customize, if OBSOLETE-NAME has
any of the following properties, they are copied to
CURRENT-NAME, if it does not already have them:
`saved-value', `saved-variable-comment'."
  (declare (doc-string 4))
  `(progn
     (defvaralias ,obsolete-name ,current-name ,docstring)
     ;; See Bug#4706.
     (dolist (prop '(saved-value saved-variable-comment))
       (and (get ,obsolete-name prop)
            (null (get ,current-name prop))
            (put ,current-name prop (get ,obsolete-name prop))))
     (make-obsolete-variable ,obsolete-name ,current-name ,when)))

;; FIXME This is only defined in this file because the variable- and
;; function- versions are too.  Unlike those two, this one is not used
;; by the byte-compiler (would be nice if it could warn about obsolete
;; faces, but it doesn't really do anything special with faces).
;; It only really affects M-x describe-face output.
(defmacro define-obsolete-face-alias (obsolete-face current-face when)
  "Make OBSOLETE-FACE a face alias for CURRENT-FACE and mark it obsolete.
WHEN should be a string indicating when the face was first made
obsolete, for example a date or a release number."
  `(progn
     (put ,obsolete-face 'face-alias ,current-face)
     ;; Used by M-x describe-face.
     (put ,obsolete-face 'obsolete-face (or (purecopy ,when) t))))

(defmacro dont-compile (&rest body)
  "Like `progn', but the body always runs interpreted (not compiled).
If you think you need this, you're probably making a mistake somewhere."
  (declare (debug t) (indent 0) (obsolete nil "24.4"))
  (list 'eval (list 'quote (if (cdr body) (cons 'progn body) (car body)))))


;; interface to evaluating things at compile time and/or load time
;; these macro must come after any uses of them in this file, as their
;; definition in the file overrides the magic definitions on the
;; byte-compile-macro-environment.

(defmacro eval-when-compile (&rest body)
  "Like `progn', but evaluates the body at compile time if you're compiling.
Thus, the result of the body appears to the compiler as a quoted
constant.  In interpreted code, this is entirely equivalent to
`progn', except that the value of the expression may be (but is
not necessarily) computed at load time if eager macro expansion
is enabled."
  (declare (debug (&rest def-form)) (indent 0))
  (list 'quote (eval (cons 'progn body) lexical-binding)))

(defmacro eval-and-compile (&rest body)
  "Like `progn', but evaluates the body at compile time and at load time.
In interpreted code, this is entirely equivalent to `progn',
except that the value of the expression may be (but is not
necessarily) computed at load time if eager macro expansion is
enabled."
  (declare (debug (&rest def-form)) (indent 0))
  ;; When the byte-compiler expands code, this macro is not used, so we're
  ;; either about to run `body' (plain interpretation) or we're doing eager
  ;; macroexpansion.
  (list 'quote (eval (cons 'progn body) lexical-binding)))

(defun with-no-warnings (&rest body)
  "Like `progn', but prevents compiler warnings in the body."
  (declare (indent 0))
  ;; The implementation for the interpreter is basically trivial.
  (car (last body)))

(defmacro with-suppressed-warnings (warnings &rest body)
  "Like `progn', but prevents compiler WARNINGS in BODY.

WARNINGS is an associative list where the first element of each
item is a warning type, and the rest of the elements in each item
are symbols they apply to.  For instance, if you want to suppress
byte compilation warnings about the two obsolete functions `foo'
and `bar', as well as the function `zot' being called with the
wrong number of parameters, say

\(with-suppressed-warnings ((obsolete foo bar)
                           (callargs zot))
  (foo (bar))
  (zot 1 2))

The warnings that can be suppressed are a subset of the warnings
in `byte-compile-warning-types'; see the variable
`byte-compile-warnings' for a fuller explanation of the warning
types.  The types that can be suppressed with this macro are
`free-vars', `callargs', `redefine', `obsolete',
`interactive-only', `lexical', `mapcar', `constants' and
`suspicious'.

For the `mapcar' case, only the `mapcar' function can be used in
the symbol list.  For `suspicious', only `set-buffer' can be used."
  ;; Note: during compilation, this definition is overridden by the one in
  ;; byte-compile-initial-macro-environment.
  (declare (debug (sexp &optional body)) (indent 1))
  (if (not (and (featurep 'macroexp)
                (boundp 'byte-compile--suppressed-warnings)))
      ;; If `macroexp' is not yet loaded, we're in the middle of
      ;; bootstrapping, so better risk emitting too many warnings
      ;; than risk breaking the bootstrap.
      `(progn ,@body)
    ;; We need to let-bind byte-compile--suppressed-warnings here, so as to
    ;; silence warnings emitted during macro-expansion performed outside of
    ;; byte-compilation.
    (let ((byte-compile--suppressed-warnings
           (append warnings byte-compile--suppressed-warnings)))
      (macroexpand-all (macroexp-progn body)
                       macroexpand-all-environment))))

(defun byte-run--unescaped-character-literals-warning ()
  "Return a warning about unescaped character literals.
If there were any unescaped character literals in the last form
read, return an appropriate warning message as a string.
Otherwise, return nil.  For internal use only."
  ;; This is called from lread.c and therefore needs to be preloaded.
  (if lread--unescaped-character-literals
      (let ((sorted (sort lread--unescaped-character-literals #'<)))
        (format-message "unescaped character literals %s detected, %s expected!"
                        (mapconcat (lambda (char) (format "`?%c'" char))
                                   sorted ", ")
                        (mapconcat (lambda (char) (format "`?\\%c'" char))
                                   sorted ", ")))))

(defun byte-compile-info (string &optional message type)
  "Format STRING in a way that looks pleasing in the compilation output.
If MESSAGE, output the message, too.

If TYPE, it should be a string that says what the information
type is.  This defaults to \"INFO\"."
  (let ((string (format "  %-9s%s" (or type "INFO") string)))
    (when message
      (message "%s" string))
    string))

(defun byte-compile-info-string (&rest args)
  "Format ARGS in a way that looks pleasing in the compilation output."
  (declare (obsolete byte-compile-info "28.1"))
  (byte-compile-info (apply #'format args)))

(defun byte-compile-info-message (&rest args)
  "Message format ARGS in a way that looks pleasing in the compilation output."
  (declare (obsolete byte-compile-info "28.1"))
  (byte-compile-info (apply #'format args) t))


;; I nuked this because it's not a good idea for users to think of using it.
;; These options are a matter of installation preference, and have nothing to
;; with particular source files; it's a mistake to suggest to users
;; they should associate these with particular source files.
;; There is hardly any reason to change these parameters, anyway.
;; --rms.

;; (put 'byte-compiler-options 'lisp-indent-function 0)
;; (defmacro byte-compiler-options (&rest args)
;;   "Set some compilation-parameters for this file.  This will affect only the
;; file in which it appears; this does nothing when evaluated, and when loaded
;; from a .el file.
;;
;; Each argument to this macro must be a list of a key and a value.
;;
;;   Keys:		  Values:		Corresponding variable:
;;
;;   verbose	  t, nil		byte-compile-verbose
;;   optimize	  t, nil, source, byte	byte-compile-optimize
;;   warnings	  list of warnings	byte-compile-warnings
;; 		      Valid elements: (callargs redefine free-vars unresolved)
;;   file-format	  emacs18, emacs19	byte-compile-compatibility
;;
;; For example, this might appear at the top of a source file:
;;
;;     (byte-compiler-options
;;       (optimize t)
;;       (warnings (- free-vars))		; Don't warn about free variables
;;       (file-format emacs19))"
;;   nil)

(make-obsolete-variable 'macro-declaration-function
                        'macro-declarations-alist "24.3")
(make-obsolete 'macro-declaration-function
               'macro-declarations-alist "24.3")

;;; byte-run.el ends here
