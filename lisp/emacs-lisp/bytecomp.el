;;; bytecomp.el --- compilation of Lisp code into byte code -*- lexical-binding: t -*-

;; Copyright (C) 1985-1987, 1992, 1994, 1998, 2000-2026 Free Software
;; Foundation, Inc.

;; Author: Jamie Zawinski <jwz@lucid.com>
;;	Hallvard Furuseth <hbf@ulrik.uio.no>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: lisp
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

;; The Emacs Lisp byte compiler.  This crunches Lisp source into a sort
;; of p-code (`lapcode') which takes up less space and can be interpreted
;; faster.  [`LAP' == `Lisp Assembly Program'.]
;; The user entry points are byte-compile-file and byte-recompile-directory.

;;; Todo:

;; - Turn "not bound at runtime" functions into autoloads.

;;; Code:

;; ========================================================================
;; Entry points:
;;	byte-recompile-directory, byte-compile-file,
;;      byte-recompile-file,
;;     batch-byte-compile, batch-byte-recompile-directory,
;;	byte-compile, compile-defun,
;;	display-call-tree
;; (byte-compile-buffer and byte-compile-and-load-file were turned off
;;  because they are not terribly useful and get in the way of completion.)

;; This version of the byte compiler has the following improvements:
;;  + optimization of compiled code:
;;    - removal of unreachable code;
;;    - removal of calls to side-effectless functions whose return-value
;;      is unused;
;;    - compile-time evaluation of safe constant forms, such as (consp nil)
;;      and (ash 1 6);
;;    - open-coding of literal lambdas;
;;    - peephole optimization of emitted code;
;;    - trivial functions are left uncompiled for speed.
;;  + support for inline functions;
;;  + compile-time evaluation of arbitrary expressions;
;;  + compile-time warning messages for:
;;    - functions being redefined with incompatible arglists;
;;    - functions being redefined as macros, or vice-versa;
;;    - functions or macros defined multiple times in the same file;
;;    - functions being called with the incorrect number of arguments;
;;    - functions being called which are not defined globally, in the
;;      file, or as autoloads;
;;    - assignment and reference of undeclared free variables;
;;    - various syntax errors;
;;  + correct compilation of nested defuns, defmacros, defvars and defsubsts;
;;  + correct compilation of top-level uses of macros;
;;  + the ability to generate a histogram of functions called.

;; User customization variables: M-x customize-group bytecomp

;; New Features:
;;
;;  o	The form `defsubst' is just like `defun', except that the function
;;	generated will be open-coded in compiled code which uses it.  This
;;	means that no function call will be generated, it will simply be
;;	spliced in.  Lisp functions calls are very slow, so this can be a
;;	big win.
;;
;;	You can generally accomplish the same thing with `defmacro', but in
;;	that case, the defined procedure can't be used as an argument to
;;	mapcar, etc.
;;
;;  o	You can also open-code one particular call to a function without
;;	open-coding all calls.  Use the 'inline' form to do this, like so:
;;
;;		(inline (foo 1 2 3))	;; `foo' will be open-coded
;;	or...
;;		(inline			;;  `foo' and `baz' will be
;;		 (foo 1 2 3 (bar 5))	;; open-coded, but `bar' will not.
;;		 (baz 0))
;;
;;  o	It is possible to open-code a function in the same file it is defined
;;	in without having to load that file before compiling it.  The
;;	byte-compiler has been modified to remember function definitions in
;;	the compilation environment in the same way that it remembers macro
;;	definitions.
;;
;;  o  Forms like ((lambda ...) ...) are open-coded.
;;
;;  o  The form `eval-when-compile' is like progn, except that the body
;;     is evaluated at compile-time.  When it appears at top-level, this
;;     is analogous to the Common Lisp idiom (eval-when (compile) ...).
;;     When it does not appear at top-level, it is similar to the
;;     Common Lisp #. reader macro (but not in interpreted code).
;;
;;  o  The form `eval-and-compile' is similar to eval-when-compile, but
;;	the whole form is evalled both at compile-time and at run-time.
;;
;;  o  The command compile-defun is analogous to eval-defun.
;;
;;  o  If you run byte-compile-file on a filename which is visited in a
;;     buffer, and that buffer is modified, you are asked whether you want
;;     to save the buffer before compiling.
;;
;;  o  byte-compiled files now start with the string `;ELC'.
;;     Some versions of `file' can be customized to recognize that.

(require 'backquote)
(require 'macroexp)
(require 'cconv)
(eval-when-compile (require 'compile))
;; Refrain from using cl-lib at run-time here, since it otherwise prevents
;; us from emitting warnings when compiling files which use cl-lib without
;; requiring it! (bug#30635)
(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'subr-x))

;; The feature of compiling in a specific target Emacs version
;; has been turned off because compile time options are a bad idea.
(defgroup bytecomp nil
  "Emacs Lisp byte-compiler."
  :group 'lisp)

(defcustom compilation-safety 1
  "Safety level for generated compiled code.
Possible values are:
  0 - emitted code can misbehave, even crash Emacs, if declarations of
      functions do not correctly describe their actual behavior;
  1 - emitted code is to be generated in a safe manner, even if functions
      are mis-declared.

Note that \"safe\" does not mean \"correct\": if functions are declared
incorrectly, the emitted code might also be incorrect.
This currently affects only code produced by native-compilation."
  :type 'integer
  :safe #'integerp
  :version "30.1")

(defcustom emacs-lisp-file-regexp "\\.el\\'"
  "Regexp which matches Emacs Lisp source files.
If you change this, you might want to set `byte-compile-dest-file-function'.
\(Note that the assumption of a \".elc\" suffix for compiled files
is hard-coded in various places in Emacs.)"
  ;; Eg is_elc in Fload.
  :type 'regexp)

(defcustom byte-compile-dest-file-function #'byte-compile--default-dest-file
  "Function for the function `byte-compile-dest-file' to call.
It should take one argument, the name of an Emacs Lisp source
file name, and return the name of the compiled file.
\(Note that the assumption that the source and compiled files
are found in the same directory is hard-coded in various places in Emacs.)"
  ;; Eg load-prefer-newer, documentation lookup IIRC.
  :type '(choice (const nil) function)
  :version "23.2")

;; This enables file name handlers such as jka-compr
;; to remove parts of the file name that should not be copied
;; through to the output file name.
(defun byte-compiler-base-file-name (filename)
  (let ((handler (find-file-name-handler filename
					 'byte-compiler-base-file-name)))
    (if handler
	(funcall handler 'byte-compiler-base-file-name filename)
      filename)))

;; Sadly automake relies on this misfeature up to at least version 1.15.1.
(if (fboundp 'byte-compile-dest-file)
    (or (featurep 'bytecomp)
        (display-warning 'bytecomp (format-message "\
Changing `byte-compile-dest-file' is obsolete (as of 23.2);
set `byte-compile-dest-file-function' instead.")))
(defun byte-compile-dest-file (filename)
  "Convert an Emacs Lisp source file name to a compiled file name.
If `byte-compile-dest-file-function' is non-nil, uses that
function to do the work.  Otherwise, if FILENAME matches
`emacs-lisp-file-regexp' (by default, files with the extension \".el\"),
replaces the matching part (and anything after it) with \".elc\";
otherwise adds \".elc\"."
  (funcall (or byte-compile-dest-file-function
               #'byte-compile--default-dest-file)
           filename)))

(defun byte-compile--default-dest-file (filename)
  (setq filename (file-name-sans-versions
		  (byte-compiler-base-file-name filename)))
  (cond ((string-match emacs-lisp-file-regexp filename)
	 (concat (substring filename 0 (match-beginning 0)) ".elc"))
	(t (concat filename ".elc"))))

;; This can be the 'byte-compile property of any symbol.
(autoload 'byte-compile-inline-expand "byte-opt")

;; This is the entry point to the lapcode optimizer pass1.
(autoload 'byte-optimize-one-form "byte-opt")
;; This is the entry point to the lapcode optimizer pass2.
(autoload 'byte-optimize-lapcode "byte-opt")

;; This is the entry point to the decompiler, which is used by the
;; disassembler.  The disassembler just requires 'byte-compile, but
;; that doesn't define this function, so this seems to be a reasonable
;; thing to do.
(autoload 'byte-decompile-bytecode "byte-opt")

(defcustom byte-compile-verbose
  (and (not noninteractive) (> baud-rate search-slow-speed))
  "Non-nil means print messages describing progress of byte-compiler."
  :type 'boolean)

(defcustom byte-optimize t
  "Enable optimization in the byte compiler.
Possible values are:
  nil      - no optimization
  t        - all optimizations
  `source' - source-level optimizations only
  `byte'   - code-level optimizations only"
  :type '(choice (const :tag "none" nil)
		 (const :tag "all" t)
		 (const :tag "source-level" source)
		 (const :tag "byte-level" byte)))

(defcustom byte-compile-delete-errors nil
  "If non-nil, the optimizer may delete forms that may signal an error.
This includes variable references and calls to functions such as `car'."
  :type 'boolean)

(defcustom byte-compile-cond-use-jump-table t
  "Compile `cond' clauses to a jump table implementation (using a hash-table)."
  :version "26.1"
  :type 'boolean)
(make-obsolete-variable 'byte-compile-cond-use-jump-table nil "31.1")

(defvar byte-compile-dynamic nil
  "Formerly used to compile function bodies so they load lazily.
This variable no longer has any effect.")
(make-obsolete-variable 'byte-compile-dynamic "not worthwhile any more." "27.1")
;;;###autoload(put 'byte-compile-dynamic 'safe-local-variable 'booleanp)

(defcustom byte-compile-dynamic-docstrings t
  "If non-nil, compile doc strings for lazy access.
We bury the doc strings of functions and variables inside comments in
the file, and bring them into core only when they are actually needed.

When this option is true, if you load the compiled file and then move it,
you won't be able to find the documentation of anything in that file.

To disable this option for a certain file, make it a file-local variable
in the source file.  For example, add this to the first line:
  -*-byte-compile-dynamic-docstrings:nil;-*-
You can also set the variable globally.

This option is enabled by default because it reduces Emacs memory usage."
  :type 'boolean)
;;;###autoload(put 'byte-compile-dynamic-docstrings 'safe-local-variable 'booleanp)

(defvar byte-compile-log-buffer "*Compile-Log*"
  "Name of the byte-compiler's log buffer.")

(defvar byte-compile--known-dynamic-vars nil
  "Variables known to be declared as dynamic, for warning purposes.
Each element is (VAR . FILE), indicating that VAR is declared in FILE.")

(defvar byte-compile--seen-defvars nil
  "All dynamic variable declarations seen so far.")

(defcustom byte-optimize-log nil
  "If non-nil, the byte-compiler will log its optimizations.
If this is `source', then only source-level optimizations will be logged.
If it is `byte', then only byte-level optimizations will be logged.
The information is logged to `byte-compile-log-buffer'."
  :type '(choice (const :tag "none" nil)
		 (const :tag "all" t)
		 (const :tag "source-level" source)
		 (const :tag "byte-level" byte)))

(defcustom byte-compile-error-on-warn nil
  "If true, the byte-compiler reports warnings with `error'."
  :type 'boolean)
;; This needs to be autoloaded because it needs to be available to
;; Emacs before the byte compiler is loaded, otherwise Emacs will not
;; know that this variable is marked as safe until it is too late.
;; (See https://lists.gnu.org/r/emacs-devel/2018-01/msg00261.html )
;;;###autoload(put 'byte-compile-error-on-warn 'safe-local-variable 'booleanp)

(defconst byte-compile-warning-types
  '( callargs constants
     docstrings docstrings-non-ascii-quotes docstrings-wide
     docstrings-control-chars
     empty-body free-vars ignored-return-value interactive-only
     lexical lexical-dynamic make-local
     mapcar                             ; obsolete
     mutate-constant noruntime not-unused obsolete redefine suspicious
     unresolved)
  "The list of warning types used when `byte-compile-warnings' is t.")
(defcustom byte-compile-warnings t
  "List of warnings that the byte-compiler should issue (t for almost all).

Elements of the list may be:

  callargs    function calls with args that don't match the definition.
  constants   let-binding of, or assignment to, constants/nonvariables.
  docstrings  various docstring stylistic issues, such as incorrect use
              of single quotes
  docstrings-non-ascii-quotes
              docstrings that have non-ASCII quotes.
              Only enabled when `docstrings' also is.
  docstrings-wide
              docstrings that are too wide, containing lines longer than both
              `byte-compile-docstring-max-column' and `fill-column' characters.
              Only enabled when `docstrings' also is.
  docstrings-control-chars
              docstrings that contain control characters other than NL and TAB
  empty-body  body argument to a special form or macro is empty.
  free-vars   references to variables not in the current lexical scope.
  ignored-return-value
              function called without using the return value where this
              is likely to be a mistake.
  interactive-only
	      commands that normally shouldn't be called from Lisp code.
  lexical     global/dynamic variables lacking a prefix.
  lexical-dynamic
              lexically bound variable declared dynamic elsewhere
  make-local  calls to `make-variable-buffer-local' that may be incorrect.
  mutate-constant
              code that mutates program constants such as quoted lists.
  noruntime   functions that may not be defined at runtime (typically
              defined only under `eval-when-compile').
  not-unused  warning about using variables with symbol names starting with _.
  obsolete    obsolete variables and functions.
  redefine    function name redefined from a macro to ordinary function or vice
              versa, or redefined to take a different number of arguments.
  suspicious  constructs that usually don't do what the coder wanted.
  unresolved  calls to unknown functions.

If the list begins with `not', then the remaining elements specify warnings to
suppress.  For example, (not free-vars) will suppress the `free-vars' warning.

The t value means \"all non experimental warning types\", and
excludes the types in `byte-compile--emacs-build-warning-types'.
A value of `all' really means all."
  :type `(choice (const :tag "Default selection" t)
                 (const :tag "All" all)
		 (set :menu-tag "Some"
                      ,@(mapcar (lambda (x) `(const ,x))
                                byte-compile-warning-types))))

(defconst byte-compile--emacs-build-warning-types
  '(docstrings-non-ascii-quotes)
  "List of warning types that are only enabled during Emacs builds.
This is typically either warning types that are being phased in
\(but shouldn't be enabled for packages yet), or that are only relevant
for the Emacs build itself.")

(defvar byte-compile--suppressed-warnings nil
  "Dynamically bound by `with-suppressed-warnings' to suppress warnings.")

;;;###autoload
(put 'byte-compile-warnings 'safe-local-variable
     (lambda (v) (or (symbolp v) (all #'symbolp v))))

;;;###autoload
(defun byte-compile-warning-enabled-p (warning &optional symbol)
  "Return non-nil if WARNING is enabled, according to `byte-compile-warnings'."
  (let ((suppress nil))
    (dolist (elem byte-compile--suppressed-warnings)
      (when (and (eq (car elem) warning)
                 (memq symbol (cdr elem)))
        (setq suppress t)))
    (and (not suppress)
         ;; During an Emacs build, we want all warnings.
         (or (eq byte-compile-warnings 'all)
             ;; If t, we want almost all the warnings, but not the
             ;; ones that are Emacs build specific.
             (and (not (memq warning byte-compile--emacs-build-warning-types))
                  (or (eq byte-compile-warnings t)
                      (if (eq (car byte-compile-warnings) 'not)
                          (not (memq warning byte-compile-warnings))
                        (memq warning byte-compile-warnings))))))))

;;;###autoload
(defun byte-compile-disable-warning (warning)
  "Change `byte-compile-warnings' to disable WARNING.
If `byte-compile-warnings' is t, set it to `(not WARNING)'.
Otherwise, if the first element is `not', add WARNING, else remove it.
Normally you should let-bind `byte-compile-warnings' before calling this,
else the global value will be modified."
  (setq byte-compile-warnings
        (cond ((eq byte-compile-warnings t)
               (list 'not warning))
              ((eq (car byte-compile-warnings) 'not)
               (if (memq warning byte-compile-warnings)
                   byte-compile-warnings
                 (append byte-compile-warnings (list warning))))
              (t
               (delq warning byte-compile-warnings)))))

;;;###autoload
(defun byte-compile-enable-warning (warning)
  "Change `byte-compile-warnings' to enable WARNING.
If `byte-compile-warnings' is t, do nothing.  Otherwise, if the
first element is `not', remove WARNING, else add it.
Normally you should let-bind `byte-compile-warnings' before calling this,
else the global value will be modified."
  (or (eq byte-compile-warnings t)
      (setq byte-compile-warnings
            (cond ((eq (car byte-compile-warnings) 'not)
                   (delq warning byte-compile-warnings))
                  ((memq warning byte-compile-warnings)
                   byte-compile-warnings)
                  (t
                   (append byte-compile-warnings (list warning)))))))

(defvar byte-compile-interactive-only-functions nil
  "List of commands that are not meant to be called from Lisp.")
(make-obsolete-variable 'byte-compile-interactive-only-functions
			"use the `interactive-only' symbol property instead."
			"24.4")

(defvar byte-compile-not-obsolete-vars nil
  "List of variables that shouldn't be reported as obsolete.")
(defvar byte-compile-global-not-obsolete-vars nil
  "Global list of variables that shouldn't be reported as obsolete.")

(defvar byte-compile-not-obsolete-funcs nil
  "List of functions that shouldn't be reported as obsolete.")

(defcustom byte-compile-generate-call-tree nil
  "Non-nil means collect call-graph information when compiling.
This records which functions were called and from where.
If the value is t, compilation displays the call graph when it finishes.
If the value is neither t nor nil, compilation asks you whether to
display the graph.

The call tree only lists functions called, not macros used.  Those
functions which the byte-code interpreter knows about directly (eq,
cons, etc.) are not reported.

The call tree also lists those functions which are not known to be
called (that is, to which no calls have been compiled).  Functions
which can be invoked interactively are excluded from this list."
  :type '(choice (const :tag "Yes" t) (const :tag "No" nil)
		 (other :tag "Ask" lambda)))

(defvar byte-compile-call-tree nil
  "Alist of functions and their call tree.
Each element looks like

  (FUNCTION CALLERS CALLS)

where CALLERS is a list of functions that call FUNCTION, and CALLS
is a list of functions for which calls were generated while compiling
FUNCTION.")

(defcustom byte-compile-call-tree-sort 'name
  "If non-nil, sort the call tree.
The values `name', `callers', `calls', `calls+callers'
specify different fields to sort on."
  :type '(choice (const name) (const callers) (const calls)
		 (const calls+callers) (const nil)))

(defvar byte-compile-debug nil
  "If non-nil, byte compile errors will be raised as signals instead of logged.")
(defvar byte-compile-jump-tables nil
  "List of all jump tables used during compilation of this form.")
(defvar byte-compile-constants nil
  "List of all constants encountered during compilation of this form.")
(defvar byte-compile-variables nil
  "List of all variables encountered during compilation of this form.")
(defvar byte-compile-bound-variables nil
  "List of dynamic variables bound in the context of the current form.
This list lives partly on the stack.")
(defvar byte-compile-lexical-variables nil
  "List of variables that have been treated as lexical.
Filled in `cconv-analyze-form' but initialized and consulted here.")
(defvar byte-compile-const-variables nil
  "List of variables declared as constants during compilation of this file.")
(defvar byte-compile-free-references)
(defvar byte-compile-free-assignments)

(defvar byte-compiler-error-flag)

(defvar bytecomp--code-strings nil
  "List of unique bytecode strings in this top-level form, for deduplication.")

(defun byte-compile-recurse-toplevel (form non-toplevel-case)
  "Implement `eval-when-compile' and `eval-and-compile'.
Return the compile-time value of FORM."
  ;; Macroexpand (not macroexpand-all!) form at toplevel in case it
  ;; expands into a toplevel-equivalent `progn'.  See CLHS section
  ;; 3.2.3.1, "Processing of Top Level Forms".  The semantics are very
  ;; subtle: see test/lisp/emacs-lisp/bytecomp-tests.el for interesting
  ;; cases.
  (setf form (macroexp-macroexpand form byte-compile-macro-environment))
  (if (eq (car-safe form) 'progn)
      (cons (car form)
            (mapcar (lambda (subform)
                      (byte-compile-recurse-toplevel
                       subform non-toplevel-case))
                    (cdr form)))
    (funcall non-toplevel-case form)))


(defvar bytecomp--copy-tree-seen)

(defun bytecomp--copy-tree-1 (tree)
  ;; TREE must be a cons.
  (or (gethash tree bytecomp--copy-tree-seen)
      (let* ((next (cdr tree))
             (result (cons nil next))
             (copy result))
        (while (progn
                 (puthash tree copy bytecomp--copy-tree-seen)
                 (let ((a (car tree)))
                   (setcar copy (if (consp a)
                                    (bytecomp--copy-tree-1 a)
                                  a)))
                 (and (consp next)
                      (let ((tail (gethash next bytecomp--copy-tree-seen)))
                        (if tail
                            (progn (setcdr copy tail)
                                   nil)
                          (setq tree next)
                          (setq next (cdr next))
                          (let ((prev copy))
                            (setq copy (cons nil next))
                            (setcdr prev copy)
                            t))))))
        result)))

(defun bytecomp--copy-tree (tree)
  "Make a copy of TREE, preserving any circular structure therein.
Only conses are traversed and duplicated, not arrays or any other structure."
  (if (consp tree)
      (let ((bytecomp--copy-tree-seen (make-hash-table :test #'eq)))
        (bytecomp--copy-tree-1 tree))
    tree))

(defconst byte-compile-initial-macro-environment
  `(
    ;; (byte-compiler-options . (lambda (&rest forms)
    ;;     		       (apply 'byte-compiler-options-handler forms)))
    (declare-function . byte-compile-macroexpand-declare-function)
    (eval-when-compile . ,(lambda (&rest body)
                            (let ((result nil))
                              (byte-compile-recurse-toplevel
                               (macroexp-progn body)
                               (lambda (form)
                                 ;; Insulate the following variables
                                 ;; against changes made in the
                                 ;; subsidiary compilation.  This
                                 ;; prevents spurious warning
                                 ;; messages: "not defined at runtime"
                                 ;; etc.
                                 (let ((byte-compile-unresolved-functions
                                        byte-compile-unresolved-functions)
                                       (byte-compile-new-defuns
                                        byte-compile-new-defuns))
                                   (setf result
                                         (byte-compile-eval
                                          (byte-run-strip-symbol-positions
                                           (byte-compile-top-level
                                            (byte-compile-preprocess form))))))))
                              (list 'quote result))))
    (eval-and-compile . ,(lambda (&rest body)
                           (byte-compile-recurse-toplevel
                            (macroexp-progn body)
                            (lambda (form)
                              ;; Don't compile here, since we don't know
                              ;; whether to compile as byte-compile-form
                              ;; or byte-compile-file-form.
                              (let ((expanded
                                     (macroexpand--all-toplevel
                                      form
                                      macroexpand-all-environment)))
                                (eval (byte-run-strip-symbol-positions
                                       (bytecomp--copy-tree expanded))
                                      (when lexical-binding
                                        (or (append
                                             macroexp--dynvars
                                             byte-compile-bound-variables)
                                            t)))
                                expanded)))))
    (with-suppressed-warnings
        . ,(lambda (warnings &rest body)
             ;; We let-bind `byte-compile--suppressed-warnings' here in order
             ;; to affect warnings emitted during macroexpansion.
             ;; Later `internal--with-suppressed-warnings' binds it again, this
             ;; time in order to affect warnings emitted during the
             ;; compilation itself.
             (if body
                 (let ((byte-compile--suppressed-warnings
                        (append warnings byte-compile--suppressed-warnings)))
                   ;; This function doesn't exist, but is just a placeholder
                   ;; symbol to hook up with the
                   ;; `byte-hunk-handler'/`byte-defop-compiler-1' machinery.
                   `(internal--with-suppressed-warnings
                     ',warnings
                     ,(macroexpand-all `(progn ,@body)
                                       macroexpand-all-environment)))
               (macroexp-warn-and-return
                (format-message "`with-suppressed-warnings' with empty body")
                nil '(empty-body with-suppressed-warnings) t warnings)))))
  "The default macro-environment passed to macroexpand by the compiler.
Placing a macro here will cause a macro to have different semantics when
expanded by the compiler as when expanded by the interpreter.")

(defvar byte-compile-macro-environment byte-compile-initial-macro-environment
  "Alist of macros defined in the file being compiled.
Each element looks like (MACRONAME . DEFINITION).  It is
\(MACRONAME . nil) when a macro is redefined as a function.")

(defvar byte-compile-function-environment nil
  "Alist of functions defined in the file being compiled.
This is so we can inline them when necessary.
Each element looks like (FUNCTIONNAME . DEFINITION).  It is
\(FUNCTIONNAME . nil) when a function is redefined as a macro.
It is \(FUNCTIONNAME . t) when all we know is that it was defined,
and we don't know the definition.  For an autoloaded function, DEFINITION
has the form (autoload . FILENAME).")

(defvar byte-compile-unresolved-functions nil
  "Alist of undefined functions to which calls have been compiled.
Each element in the list has the form (FUNCTION POSITION . CALLS)
where CALLS is a list whose elements are integers (indicating the
number of arguments passed in the function call) or the constant t
if the function is called indirectly.
This variable is only significant whilst compiling an entire buffer.
Used for warnings when a function is not known to be defined or is later
defined with incorrect args.")

(defvar byte-compile-noruntime-functions nil
  "Alist of functions called that may not be defined when the compiled code is run.
Used for warnings about calling a function that is defined during compilation
but won't necessarily be defined when the compiled file is loaded.")

(defvar byte-compile-new-defuns nil
  "List of (runtime) functions defined in this compilation run.
This variable is used to qualify `byte-compile-noruntime-functions' when
outputting warnings about functions not being defined at runtime.")

;; Variables for lexical binding
(defvar byte-compile--lexical-environment nil
  "The current lexical environment.")

(defvar byte-compile-tag-number 0)
(defvar byte-compile-output nil
  "Alist describing contents to put in byte code string.
Each element is (INDEX . VALUE)")
(defvar byte-compile-depth 0 "Current depth of execution stack.")
(defvar byte-compile-maxdepth 0 "Maximum depth of execution stack.")

;; The following is used by comp.el to spill data out of here.
;;
;; Spilling is done in 3 places:
;;
;; - `byte-compile-lapcode' to obtain the map bytecode -> LAP for any
;;   code assembled.
;;
;; - `byte-compile-lambda' to obtain arglist doc and interactive spec
;;   af any lambda compiled (including anonymous).
;;
;; - `byte-compile-file-form-defmumble' to obtain the list of
;;   top-level forms as they would be outputted in the .elc file.
;;

(cl-defstruct byte-to-native-lambda
  byte-func lap)

;; Top level forms:
(cl-defstruct byte-to-native-func-def
  "Named function defined at top-level."
  name c-name byte-func)
(cl-defstruct byte-to-native-top-level
  "All other top-level forms."
  form lexical)

(defvar byte-native-compiling nil
  "Non-nil while native compiling.")
(defvar byte-native-qualities nil
  "To spill default qualities from the compiled file.")
(defvar byte+native-compile nil
  "Non-nil while producing at the same time byte and native code.")
(defvar byte-to-native-lambdas-h nil
  "Hash byte-code -> byte-to-native-lambda.")
(defvar byte-to-native-top-level-forms nil
  "List of top level forms.")
(defvar byte-to-native-output-buffer-file nil
  "Pair holding byte-compilation output buffer, elc filename.")
(defvar byte-to-native-plist-environment nil
  "To spill `overriding-plist-environment'.")


;;; The byte codes; this information is duplicated in bytecomp.c

(defvar byte-code-vector nil
  "An array containing byte-code names indexed by byte-code values.")

(defvar byte-stack+-info nil
  "An array with the stack adjustment for each byte-code.")

(defmacro byte-defop (opcode stack-adjust opname &optional docstring)
  ;; This is a speed-hack for building the byte-code-vector at compile-time.
  ;; We fill in the vector at macroexpand-time, and then after the last call
  ;; to byte-defop, we write the vector out as a constant instead of writing
  ;; out a bunch of calls to aset.
  ;; Actually, we don't fill in the vector itself, because that could make
  ;; it problematic to compile big changes to this compiler; we store the
  ;; values on its plist, and remove them later in -extrude.
  (let ((v1 (or (get 'byte-code-vector 'tmp-compile-time-value)
		(put 'byte-code-vector 'tmp-compile-time-value
		     (make-vector 256 nil))))
	(v2 (or (get 'byte-stack+-info 'tmp-compile-time-value)
		(put 'byte-stack+-info 'tmp-compile-time-value
		     (make-vector 256 nil)))))
    (aset v1 opcode opname)
    (aset v2 opcode stack-adjust))
  (if docstring
      (list 'defconst opname opcode (concat "Byte code opcode " docstring "."))
      (list 'defconst opname opcode)))

(defmacro byte-extrude-byte-code-vectors ()
  (prog1 (list 'setq 'byte-code-vector
		     (get 'byte-code-vector 'tmp-compile-time-value)
		     'byte-stack+-info
		     (get 'byte-stack+-info 'tmp-compile-time-value))
    (put 'byte-code-vector 'tmp-compile-time-value nil)
    (put 'byte-stack+-info 'tmp-compile-time-value nil)))


;; The following opcodes (1-47) use the 3 lowest bits for an immediate
;; argument.

(byte-defop   0  1 byte-stack-ref "for stack reference")
;; Code 0 is actually unused but reserved as invalid code for detecting
;; corrupted bytecode.  Codes 1-7 are stack-ref.

(byte-defop   8  1 byte-varref	"for variable reference")
(byte-defop  16 -1 byte-varset	"for setting a variable")
(byte-defop  24 -1 byte-varbind	"for binding a variable")
(byte-defop  32  0 byte-call	"for calling a function")
(byte-defop  40  0 byte-unbind	"for unbinding special bindings")
;; codes 8-47 are consumed by the preceding opcodes

(byte-defop  48  0 byte-pophandler)
(byte-defop  49 -1 byte-pushconditioncase)
(byte-defop  50 -1 byte-pushcatch)

;; unused: 51-55

(byte-defop  56 -1 byte-nth)
(byte-defop  57  0 byte-symbolp)
(byte-defop  58  0 byte-consp)
(byte-defop  59  0 byte-stringp)
(byte-defop  60  0 byte-listp)
(byte-defop  61 -1 byte-eq)
(byte-defop  62 -1 byte-memq)
(byte-defop  63  0 byte-not)
(byte-defop  64  0 byte-car)
(byte-defop  65  0 byte-cdr)
(byte-defop  66 -1 byte-cons)
(byte-defop  67  0 byte-list1)
(byte-defop  68 -1 byte-list2)
(byte-defop  69 -2 byte-list3)
(byte-defop  70 -3 byte-list4)
(byte-defop  71  0 byte-length)
(byte-defop  72 -1 byte-aref)
(byte-defop  73 -2 byte-aset)
(byte-defop  74  0 byte-symbol-value)
(byte-defop  75  0 byte-symbol-function)
(byte-defop  76 -1 byte-set)
(byte-defop  77 -1 byte-fset)
(byte-defop  78 -1 byte-get)
(byte-defop  79 -2 byte-substring)
(byte-defop  80 -1 byte-concat2)
(byte-defop  81 -2 byte-concat3)
(byte-defop  82 -3 byte-concat4)
(byte-defop  83  0 byte-sub1)
(byte-defop  84  0 byte-add1)
(byte-defop  85 -1 byte-eqlsign)
(byte-defop  86 -1 byte-gtr)
(byte-defop  87 -1 byte-lss)
(byte-defop  88 -1 byte-leq)
(byte-defop  89 -1 byte-geq)
(byte-defop  90 -1 byte-diff)
(byte-defop  91  0 byte-negate)
(byte-defop  92 -1 byte-plus)
(byte-defop  93 -1 byte-max)
(byte-defop  94 -1 byte-min)
(byte-defop  95 -1 byte-mult)
(byte-defop  96  1 byte-point)
(byte-defop  97  0 byte-save-current-buffer-OBSOLETE) ; unused since v20
(byte-defop  98  0 byte-goto-char)
(byte-defop  99  0 byte-insert)
(byte-defop 100  1 byte-point-max)
(byte-defop 101  1 byte-point-min)
(byte-defop 102  0 byte-char-after)
(byte-defop 103  1 byte-following-char)
(byte-defop 104  1 byte-preceding-char)
(byte-defop 105  1 byte-current-column)
(byte-defop 106  0 byte-indent-to)
(byte-defop 107  0 byte-scan-buffer-OBSOLETE) ; no longer generated as of v18
(byte-defop 108  1 byte-eolp)
(byte-defop 109  1 byte-eobp)
(byte-defop 110  1 byte-bolp)
(byte-defop 111  1 byte-bobp)
(byte-defop 112  1 byte-current-buffer)
(byte-defop 113  0 byte-set-buffer)
(byte-defop 114  0 byte-save-current-buffer
  "to make a binding to record the current buffer")
(byte-defop 115  0 byte-set-mark-OBSOLETE)
(byte-defop 116  1 byte-interactive-p-OBSOLETE)

(byte-defop 117  0 byte-forward-char)
(byte-defop 118  0 byte-forward-word)
(byte-defop 119 -1 byte-skip-chars-forward)
(byte-defop 120 -1 byte-skip-chars-backward)
(byte-defop 121  0 byte-forward-line)
(byte-defop 122  0 byte-char-syntax)
(byte-defop 123 -1 byte-buffer-substring)
(byte-defop 124 -1 byte-delete-region)
(byte-defop 125 -1 byte-narrow-to-region)
(byte-defop 126  1 byte-widen)
(byte-defop 127  0 byte-end-of-line)

;; unused: 128

;; These store their argument in the next two bytes
(byte-defop 129  1 byte-constant2
   "for reference to a constant with vector
index >= byte-constant-limit")
(byte-defop 130  0 byte-goto "for unconditional jump")
(byte-defop 131 -1 byte-goto-if-nil "to pop value and jump if it's nil")
(byte-defop 132 -1 byte-goto-if-not-nil "to pop value and jump if it's not nil")
(byte-defop 133 -1 byte-goto-if-nil-else-pop
  "to examine top-of-stack, jump and don't pop it if it's nil,
otherwise pop it")
(byte-defop 134 -1 byte-goto-if-not-nil-else-pop
  "to examine top-of-stack, jump and don't pop it if it's non nil,
otherwise pop it")

(byte-defop 135 -1 byte-return "to pop a value and return it from `byte-code'")
(byte-defop 136 -1 byte-discard "to discard one value from stack")
(byte-defop 137  1 byte-dup     "to duplicate the top of the stack")

(byte-defop 138  0 byte-save-excursion
  "to make a binding to record the buffer, point and mark")
(byte-defop 139  0 byte-save-window-excursion-OBSOLETE
  "to make a binding to record entire window configuration")
(byte-defop 140  0 byte-save-restriction
  "to make a binding to record the current buffer clipping
restrictions")
(byte-defop 141 -1 byte-catch-OBSOLETE   ; Not generated since Emacs 25.
  "for catch.  Takes, on stack, the tag and an expression for
the body")
(byte-defop 142 -1 byte-unwind-protect
  "for unwind-protect.  Takes, on stack, an expression for
the unwind-action")

;; For condition-case.  Takes, on stack, the variable to bind,
;; an expression for the body, and a list of clauses.
;; Not generated since Emacs 25.
(byte-defop 143 -2 byte-condition-case-OBSOLETE)

(byte-defop 144  0 byte-temp-output-buffer-setup-OBSOLETE)
(byte-defop 145 -1 byte-temp-output-buffer-show-OBSOLETE)

;; unused: 146

(byte-defop 147 -2 byte-set-marker)
(byte-defop 148  0 byte-match-beginning)
(byte-defop 149  0 byte-match-end)
(byte-defop 150  0 byte-upcase)
(byte-defop 151  0 byte-downcase)
(byte-defop 152 -1 byte-string=)
(byte-defop 153 -1 byte-string<)
(byte-defop 154 -1 byte-equal)
(byte-defop 155 -1 byte-nthcdr)
(byte-defop 156 -1 byte-elt)
(byte-defop 157 -1 byte-member)
(byte-defop 158 -1 byte-assq)
(byte-defop 159  0 byte-nreverse)
(byte-defop 160 -1 byte-setcar)
(byte-defop 161 -1 byte-setcdr)
(byte-defop 162  0 byte-car-safe)
(byte-defop 163  0 byte-cdr-safe)
(byte-defop 164 -1 byte-nconc)
(byte-defop 165 -1 byte-quo)
(byte-defop 166 -1 byte-rem)
(byte-defop 167  0 byte-numberp)
(byte-defop 168  0 byte-integerp)

;; unused: 169-174
(byte-defop 175 nil byte-listN)
(byte-defop 176 nil byte-concatN)
(byte-defop 177 nil byte-insertN)

(byte-defop 178 -1 byte-stack-set)	; Stack offset in following one byte.
(byte-defop 179 -1 byte-stack-set2)	; Stack offset in following two bytes.

;; unused: 180-181

;; If (following one byte & 0x80) == 0
;;    discard (following one byte & 0x7F) stack entries
;; else
;;    discard (following one byte & 0x7F) stack entries _underneath_ TOS
;;    (that is, if the operand = 0x83,  ... X Y Z T  =>  ... T)
(byte-defop 182 nil byte-discardN)
;; `byte-discardN-preserve-tos' is a pseudo-op that gets turned into
;; `byte-discardN' with the high bit in the operand set (by
;; `byte-compile-lapcode').
(defconst byte-discardN-preserve-tos byte-discardN)

(byte-defop 183 -2 byte-switch
 "to take a hash table and a value from the stack, and jump to
the address the value maps to, if any.")

;; unused: 184-191

(byte-defop 192  1 byte-constant	"for reference to a constant")
;; Codes 193-255 are consumed by `byte-constant', which uses the 6
;; lowest bits for an immediate argument.
(defconst byte-constant-limit 64
  "Exclusive maximum index usable in the `byte-constant' opcode.")

(defconst byte-goto-ops '(byte-goto byte-goto-if-nil byte-goto-if-not-nil
			  byte-goto-if-nil-else-pop
			  byte-goto-if-not-nil-else-pop
                          byte-pushcatch byte-pushconditioncase)
  "List of byte-codes whose offset is a pc.")

(defconst byte-goto-always-pop-ops '(byte-goto-if-nil byte-goto-if-not-nil))

(byte-extrude-byte-code-vectors)

;;; lapcode generator
;;
;; the byte-compiler now does source -> lapcode -> bytecode instead of
;; source -> bytecode, because it's a lot easier to make optimizations
;; on lapcode than on bytecode.
;;
;; Elements of the lapcode list are of the form (<instruction> . <parameter>)
;; where instruction is a symbol naming a byte-code instruction,
;; and parameter is an argument to that instruction, if any.
;;
;; The instruction can be the pseudo-op TAG, which means that this position
;; in the instruction stream is a target of a goto.  (car PARAMETER) will be
;; the PC for this location, and the whole instruction "(TAG pc)" will be the
;; parameter for some goto op.
;;
;; If the operation is varbind, varref, varset or push-constant, then the
;; parameter is (variable/constant . index_in_constant_vector).
;;
;; First, the source code is macroexpanded and optimized in various ways.
;; Then the resultant code is compiled into lapcode.  Another set of
;; optimizations are then run over the lapcode.  Then the variables and
;; constants referenced by the lapcode are collected and placed in the
;; constants-vector.  (This happens now so that variables referenced by dead
;; code don't consume space.)  And finally, the lapcode is transformed into
;; compacted byte-code.
;;
;; A distinction is made between variables and constants because the variable-
;; referencing instructions are more sensitive to the variables being near the
;; front of the constants-vector than the constant-referencing instructions.
;; Also, this lets us notice references to free variables.

(defmacro byte-compile-push-bytecodes (&rest args)
  "Push bytes onto BVAR, and increment CVAR by the number of bytes pushed.
BVAR and CVAR are variables which are updated after evaluating
all the arguments.

\(fn BYTE1 BYTE2 ... BYTEn BVAR CVAR)"
  (let ((byte-exprs (butlast args 2))
	(bytes-var (car (last args 2)))
	(pc-var (car (last args))))
    `(setq ,bytes-var ,(if (null (cdr byte-exprs))
                           `(progn (cl-assert (<= 0 ,(car byte-exprs)))
                                   (cons ,@byte-exprs ,bytes-var))
                         `(nconc (list ,@(reverse byte-exprs)) ,bytes-var))
           ,pc-var (+ ,(length byte-exprs) ,pc-var))))

(defmacro byte-compile-push-bytecode-const2 (opcode const2 bytes pc)
  "Push OPCODE and the two-byte constant CONST2 onto BYTES, and add 3 to PC.
CONST2 may be evaluated multiple times."
  `(byte-compile-push-bytecodes ,opcode (logand ,const2 255) (ash ,const2 -8)
				,bytes ,pc))

(defun byte-compile-lapcode (lap)
  "Turn lapcode LAP into bytecode.  The lapcode is destroyed."
  ;; Lapcode modifications: changes the ID of a tag to be the tag's PC.
  (let ((pc 0)			; Program counter
	op off			; Operation & offset
	opcode			; numeric value of OP
	(bytes '())		; Put the output bytes here
	(patchlist nil))        ; List of gotos to patch
    (dolist (lap-entry lap)
      (setq op (car lap-entry)
	    off (cdr lap-entry))
      (cond
       ((not (symbolp op))
        (error "Non-symbolic opcode `%s'" op))
       ((eq op 'TAG)
        (setcar off pc))
       (t
        (setq opcode
              (if (eq op 'byte-discardN-preserve-tos)
                  ;; byte-discardN-preserve-tos is a pseudo op, which
                  ;; is actually the same as byte-discardN
                  ;; with a modified argument.
                  byte-discardN
                (symbol-value op)))
        (cond ((memq op byte-goto-ops)
               ;; goto
               (byte-compile-push-bytecodes opcode nil (cdr off) bytes pc)
               (push bytes patchlist))
              ((or (and (consp off)
                        ;; Variable or constant reference
                        (progn
                          (setq off (cdr off))
                          (eq op 'byte-constant)))
                   (and (eq op 'byte-constant)
                        (integerp off)))
               ;; constant ref
               (if (< off byte-constant-limit)
                   (byte-compile-push-bytecodes (+ byte-constant off)
                                                bytes pc)
                 (byte-compile-push-bytecode-const2 byte-constant2 off
                                                    bytes pc)))
              ((and (= opcode byte-stack-set)
                    (> off 255))
               ;; Use the two-byte version of byte-stack-set if the
               ;; offset is too large for the normal version.
               (byte-compile-push-bytecode-const2 byte-stack-set2 off
                                                  bytes pc))
              ((and (>= opcode byte-listN)
                    (< opcode byte-discardN))
               ;; These insns all put their operand into one extra byte.
               (byte-compile-push-bytecodes opcode off bytes pc))
              ((= opcode byte-discardN)
               ;; byte-discardN is weird in that it encodes a flag in the
               ;; top bit of its one-byte argument.  If the argument is
               ;; too large to fit in 7 bits, the opcode can be repeated.
               (let ((flag (if (eq op 'byte-discardN-preserve-tos) #x80 0)))
                 (while (> off #x7f)
                   (byte-compile-push-bytecodes opcode (logior #x7f flag)
                                                bytes pc)
                   (setq off (- off #x7f)))
                 (byte-compile-push-bytecodes opcode (logior off flag)
                                              bytes pc)))
              ((null off)
               ;; opcode that doesn't use OFF
               (byte-compile-push-bytecodes opcode bytes pc))
              ((and (eq opcode byte-stack-ref) (eq off 0))
               ;; (stack-ref 0) is really just another name for `dup'.
               (debug)                 ;FIXME: When would this happen?
               (byte-compile-push-bytecodes byte-dup bytes pc))
              ;; The following three cases are for the special
              ;; insns that encode their operand into 0, 1, or 2
              ;; extra bytes depending on its magnitude.
              ((< off 6)
               (byte-compile-push-bytecodes (+ opcode off) bytes pc))
              ((< off 256)
               (byte-compile-push-bytecodes (+ opcode 6) off bytes pc))
              (t
               (byte-compile-push-bytecode-const2 (+ opcode 7) off
                                                  bytes pc))))))
    ;;(if (not (= pc (length bytes)))
    ;;    (error "Compiler error: pc mismatch - %s %s" pc (length bytes)))
    ;; Patch tag PCs into absolute jumps.
    (dolist (bytes-tail patchlist)
      (setq pc (caar bytes-tail))	; Pick PC from goto's tag.
      ;; Splits PC's value into 2 bytes. The jump address is
      ;; "reconstructed" by the `FETCH2' macro in `bytecode.c'.
      (setcar (cdr bytes-tail) (logand pc 255))
      (setcar bytes-tail (ash pc -8))
      ;; FIXME: Replace this by some workaround.
      (or (<= 0 (car bytes-tail) 255) (error "Bytecode overflow")))

    ;; Similarly, replace TAGs in all jump tables with the correct PC index.
    (dolist (hash-table byte-compile-jump-tables)
      (let (alist)
        (maphash #'(lambda (value tag)
                     (setq pc (cadr tag))
                     ;; We don't need to split PC here, as it is stored as a
                     ;; lisp object in the hash table (whereas other goto-*
                     ;; ops store it within 2 bytes in the byte string).
                     ;; De-position any symbols with position in `value'.
                     ;; Since this may change the hash table key, we remove
                     ;; the entry from the table and reinsert it outside the
                     ;; scope of the `maphash'.
                     (setq value (byte-run-strip-symbol-positions value))
                     (push (cons value pc) alist)
                     (remhash value hash-table))
                 hash-table)
        (dolist (elt alist)
          (puthash (car elt) (cdr elt) hash-table))))
    (let ((bytecode (apply #'unibyte-string (nreverse bytes))))
      (when byte-native-compiling
        ;; Spill LAP for the native compiler here.
        (puthash bytecode (make-byte-to-native-lambda :lap lap)
                 byte-to-native-lambdas-h))
      bytecode)))


;;; compile-time evaluation

(defun byte-compile-eval (form)
  "Eval FORM and mark the functions defined therein.
Each function's symbol gets added to `byte-compile-noruntime-functions'."
  (let ((hist-orig load-history)
	(hist-nil-orig current-load-list))
    (prog1 (eval form lexical-binding)
      (when (byte-compile-warning-enabled-p 'noruntime)
	(let* ((hist-new
	        ;; Get new `current-load-list' for the locally defined funs.
	        (cons (butlast current-load-list
	                       (length hist-nil-orig))
	              load-history)))
	  ;; Go through load-history, look for newly loaded files
	  ;; and mark all the functions defined therein.
	  (while (and hist-new (not (eq hist-new hist-orig)))
	    (let ((xs (pop hist-new)))
	      ;; Make sure the file was not already loaded before.
	      (unless (assoc (car xs) hist-orig)
		(dolist (s xs)
		  (pcase s
		    (`(defun . ,f)
		     ;; If `f' has a history, it's presumably because
		     ;; it was already defined beforehand (typically
		     ;; as an autoload).  It could also be because it
		     ;; was defined twice during `form', in which case
		     ;; we arguably should add it to b-c-noruntime-functions,
                     ;; but it's not clear it's worth the trouble
		     ;; trying to recognize that case.
		     (unless (or (get f 'function-history)
                                 (assq f byte-compile-function-environment))
                       (push f byte-compile-noruntime-functions)))))))))))))

;;; byte compiler messages

(defun emacs-lisp-compilation-file-name-or-buffer (str)
  "Return file name or buffer given by STR.
If STR is a \"normal\" filename, just return it.
If STR is something like \"Buffer foo.el\", return #<buffer foo.el>
\(if it is still live) or the string \"foo.el\" otherwise."
  (if (string-match "Buffer \\(.*\\)\\'" str)
      (or (get-buffer (match-string-no-properties 1 str))
          (match-string-no-properties 1 str))
    str))

(defconst emacs-lisp-compilation-parse-errors-filename-function
  #'emacs-lisp-compilation-file-name-or-buffer
  "The value for `compilation-parse-errors-filename-function' for when
we go into `emacs-lisp-compilation-mode'.")

(defcustom emacs-lisp-compilation-search-path '(nil)
  "Directories to search for files named in byte-compile error messages.
Value should be a list of directory names, not file names of
directories.  The value nil as an element means the byte-compile
message buffer `default-directory'."
  :version "27.1"
  :type '(repeat (choice (const :tag "Default" nil)
			 (string :tag "Directory"))))

(defvar-keymap emacs-lisp-compilation-mode-map
  "g" #'emacs-lisp-compilation-recompile)

(defvar emacs-lisp-compilation--current-file nil)

(define-compilation-mode emacs-lisp-compilation-mode "elisp-compile"
  "The variant of `compilation-mode' used for emacs-lisp compilation buffers."
  (setq-local emacs-lisp-compilation--current-file nil))

(defun emacs-lisp-compilation-recompile ()
  "Recompile the previously byte-compiled file."
  (interactive)
  (unless emacs-lisp-compilation--current-file
    (error "No previously compiled file"))
  (unless (stringp emacs-lisp-compilation--current-file)
    (error "Only files can be recompiled"))
  (byte-compile-file emacs-lisp-compilation--current-file))

(defvar byte-compile-current-form nil)
(defvar byte-compile-dest-file nil)
(defvar byte-compile-current-file nil)
(defvar byte-compile-current-group nil)
(defvar byte-compile-current-buffer nil)

;; Log something that isn't a warning.
(defmacro byte-compile-log (format-string &rest args)
  `(and
    byte-optimize
    (memq byte-optimize-log '(t source))
    (let ((print-escape-newlines t)
	  (print-level 4)
	  (print-length 4))
      (byte-compile-log-1
       (format-message
	,format-string
	,@(mapcar
	   (lambda (x) (if (symbolp x) (list 'prin1-to-string x) x))
	   args))))))

;; Log something that isn't a warning.
(defun byte-compile-log-1 (string)
  (with-current-buffer (get-buffer-create byte-compile-log-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (byte-compile-warning-prefix nil nil)
      (cond (noninteractive
	     (message " %s" string))
	    (t
	     (insert (format "%s\n" string)))))))

(defvar byte-compile-last-warned-form nil)
(defvar byte-compile-last-logged-file nil)
(defvar byte-compile-root-dir nil
  "Directory relative to which file names in error messages are written.")

;; FIXME: We should maybe extend abbreviate-file-name with an optional DIR
;; argument to try and use a relative file-name.
(defun byte-compile-abbreviate-file (file &optional dir)
  (let ((f1 (abbreviate-file-name file))
        (f2 (file-relative-name file dir)))
    (if (< (length f2) (length f1)) f2 f1)))

(defun byte-compile--first-symbol-with-pos (form)
  "Return the first symbol with position in form, or nil if none.
Order is by depth-first search."
  (named-let loop ((form form)
                   (depth 10))          ;Arbitrary limit.
    (cond
     ((<= depth 0) nil)                 ;Avoid cycles (bug#58601).
     ((symbol-with-pos-p form) form)
     ((consp form)
      (or (loop (car form) (1- depth))
          (loop (cdr form) (1- depth))))
     ((or (vectorp form) (recordp form))
      (let ((len (length form))
            (i 0)
            (sym nil))
        (while (and (< i len)
                    (not (setq sym (loop (aref form i) (1- depth)))))
          (setq i (1+ i)))
        sym)))))

(defun byte-compile--warning-source-offset ()
  "Return a source offset from `byte-compile-form-stack' or nil if none."
  (let ((sym (byte-compile--first-symbol-with-pos byte-compile-form-stack)))
    (and sym (symbol-with-pos-pos sym))))

;; This is used as warning-prefix for the compiler.
;; It is always called with the warnings buffer current.
(defun byte-compile-warning-prefix (level entry)
  (let* ((inhibit-read-only t)
	 (dir (or byte-compile-root-dir default-directory))
	 (file (cond ((stringp byte-compile-current-file)
		      (format "%s:" (byte-compile-abbreviate-file
                                     byte-compile-current-file dir)))
		     ((bufferp byte-compile-current-file)
		      (format "Buffer %s:"
			      (buffer-name byte-compile-current-file)))
		     ;; We might be simply loading a file that
		     ;; contains explicit calls to byte-compile functions.
		     ((stringp load-file-name)
		      (format "%s:" (byte-compile-abbreviate-file
                                     load-file-name dir)))
		     (t "")))
         (offset (byte-compile--warning-source-offset))
	 (pos (if (and byte-compile-current-file offset)
		  (with-current-buffer byte-compile-current-buffer
                    (let (new-l new-c)
                      (save-excursion
                        (goto-char offset)
                        (setq new-l (1+ (count-lines (point-min)
                                                     (line-beginning-position)))
                              new-c (1+ (current-column)))
                        (format "%d:%d:" new-l new-c))))
		""))
	 (form (if (eq byte-compile-current-form :end) "end of data"
		 (or byte-compile-current-form "toplevel form"))))
    (when (or (and byte-compile-current-file
		   (not (equal byte-compile-current-file
			       byte-compile-last-logged-file)))
	      (and byte-compile-current-form
		   (not (eq byte-compile-current-form
			    byte-compile-last-warned-form))))
      (insert (format "\nIn %s:\n" form)))
    (when level
      (insert (format "%s%s " file pos))))
  (setq byte-compile-last-logged-file byte-compile-current-file
	byte-compile-last-warned-form byte-compile-current-form)
  entry)

;; This no-op function is used as the value of warning-series
;; to tell inner calls to displaying-byte-compile-warnings
;; not to bind warning-series.
(defun byte-compile-warning-series (&rest _ignore)
  nil)

;; (compile-mode) will cause this to be loaded.
(declare-function compilation-forget-errors "compile" ())

;; Log the start of a file in `byte-compile-log-buffer', and mark it as done.
;; Return the position of the start of the page in the log buffer.
;; But do nothing in batch mode.
(defun byte-compile-log-file ()
  (and (not (equal byte-compile-current-file byte-compile-last-logged-file))
       (not noninteractive)
       (with-current-buffer (get-buffer-create byte-compile-log-buffer)
	 (goto-char (point-max))
	 (let* ((inhibit-read-only t)
		(dir (and (stringp byte-compile-current-file)
			  (file-name-directory byte-compile-current-file)))
		(was-same (equal default-directory dir))
		pt)
	   (when dir
	     (unless was-same
	       (insert (format-message "Leaving directory `%s'\n"
                                       default-directory))))
	   (unless (bolp)
	     (insert "\n"))
	   (setq pt (point-marker))
	   (if byte-compile-current-file
	       (insert "\f\nCompiling "
		       (if (stringp byte-compile-current-file)
			   (concat "file " byte-compile-current-file)
			 (concat "in buffer "
                                 (buffer-name byte-compile-current-file)))
		       " at " (current-time-string) "\n")
	     (insert "\f\nCompiling internal form(s) at " (current-time-string) "\n"))
	   (when dir
	     (setq default-directory dir)
	     (unless was-same
	       (insert (format-message "Entering directory `%s'\n"
                                       default-directory))))
	   (setq byte-compile-last-logged-file byte-compile-current-file
		 byte-compile-last-warned-form nil)
	   ;; Do this after setting default-directory.
	   (unless (derived-mode-p 'compilation-mode)
             (emacs-lisp-compilation-mode))
           (setq emacs-lisp-compilation--current-file byte-compile-current-file)
	   (compilation-forget-errors)
	   pt))))

(defvar byte-compile-log-warning-function
  #'byte-compile--log-warning-for-byte-compile
  "Function called when encountering a warning or error.
Called with arguments (STRING POSITION FILL LEVEL).  STRING is a
message describing the problem.  POSITION is a buffer position
where the problem was detected.  FILL is a prefix as in
`warning-fill-prefix'.  LEVEL is the level of the
problem (`:warning' or `:error').  FILL and LEVEL may be nil.")

(defun byte-compile-log-warning (string &optional fill level)
  "Log a byte-compilation warning.
STRING, FILL and LEVEL are as described in
`byte-compile-log-warning-function', which see."
  (funcall byte-compile-log-warning-function
           string
           (or (byte-compile--warning-source-offset)
               (point))
           fill
           level))

(defun byte-compile--log-warning-for-byte-compile (string _position
                                                          &optional
                                                          fill
                                                          level)
  "Log a message STRING in `byte-compile-log-buffer'.
Also log the current function and file if not already done.  If
FILL is non-nil, set `warning-fill-prefix' to four spaces.  LEVEL
is the warning level (`:warning' or `:error').  Do not call this
function directly; use `byte-compile-warn' or
`byte-compile-report-error' instead."
  (let ((warning-prefix-function 'byte-compile-warning-prefix)
	(warning-type-format "")
	(warning-fill-prefix (if fill "    ")))
    (display-warning 'bytecomp string level byte-compile-log-buffer)))

(defun byte-compile-warn (format &rest args)
  "Issue a byte compiler warning; use (format-message FORMAT ARGS...) for message."
  (setq format (apply #'format-message format args))
  (if byte-compile-error-on-warn
      (error "%s" format)		; byte-compile-file catches and logs it
    (byte-compile-log-warning format t :warning)))

(defun byte-compile-warn-x (arg format &rest args)
  "Issue a byte compiler warning.
ARG is the source element (likely a symbol with position) central to
  the warning, intended to supply source position information.
FORMAT and ARGS are as in `byte-compile-warn'."
  (let ((byte-compile-form-stack (cons arg byte-compile-form-stack)))
    (apply #'byte-compile-warn format args)))

;;;###autoload
(defun byte-compile-warn-obsolete (symbol type)
  "Warn that SYMBOL (a variable, function or generalized variable) is obsolete.
TYPE is a string that say which one of these three types it is."
  (when (byte-compile-warning-enabled-p 'obsolete symbol)
    (byte-compile-warn-x
     symbol "%s"
     (macroexp--obsolete-warning
      symbol
      (pcase type
        ("function"
         (get symbol 'byte-obsolete-info))
        ("variable"
         (get symbol 'byte-obsolete-variable))
        ("generalized variable"
         (get symbol 'byte-obsolete-generalized-variable)))
      type))))

(defun byte-compile-report-error (error-info &optional fill)
  "Report Lisp error in compilation.
ERROR-INFO is the error data, in the form of either (ERROR-SYMBOL . DATA)
or STRING.  If FILL is non-nil, set `warning-fill-prefix' to four spaces
when printing the error message."
  (setq byte-compiler-error-flag t)
  (byte-compile-log-warning
   (if (stringp error-info) error-info
     (error-message-string error-info))
   fill :error))

;;; sanity-checking arglists

(defun byte-compile-fdefinition (name macro-p)
  ;; If a function has an entry saying (FUNCTION . t).
  ;; that means we know it is defined but we don't know how.
  ;; If a function has an entry saying (FUNCTION . nil),
  ;; that means treat it as not defined.
  (let* ((list (if macro-p
		   byte-compile-macro-environment
		 byte-compile-function-environment))
	 (env (cdr (assq name list))))
    (or env
	(let ((fn name))
	  (while
              (and (symbolp fn)
		   (fboundp fn)
                   (let ((s (symbol-function fn)))
                     (and
		      (or (symbolp s)
			  (consp s)
			  (and (not macro-p)
                               (or (closurep s)
			           (compiled-function-p s))))
                      (progn
	                (setq fn s)
                        t)))))
          (let ((advertised (get-advertised-calling-convention
                             (if (and (symbolp fn) (fboundp fn))
                                 ;; Could be a subr.
                                 (symbol-function fn)
                               fn))))
            (cond
             ((listp advertised)
              (if macro-p
                  `(macro lambda ,advertised)
                `(lambda ,advertised)))
             ((and (not macro-p) (or (closurep fn) (compiled-function-p fn)))
              fn)
             ((not (consp fn)) nil)
             ((eq 'macro (car fn)) (cdr fn))
             (macro-p nil)
             ((eq 'autoload (car fn)) nil)
             (t fn)))))))

(defun byte-compile-arglist-signature (arglist)
  (cond
   ((listp arglist)
    (let ((args 0)
          opts
          restp)
      (while arglist
        (cond ((eq (car arglist) '&optional)
               (or opts (setq opts 0)))
              ((eq (car arglist) '&rest)
               (if (cdr arglist)
                   (setq restp t
                         arglist nil)))
              (t
               (if opts
                   (setq opts (1+ opts))
		 (setq args (1+ args)))))
        (setq arglist (cdr arglist)))
      (cons args (if restp nil (if opts (+ args opts) args)))))
   ;; Unknown arglist.
   (t '(0))))

(defun byte-compile--function-signature (f)
  ;; Similar to help-function-arglist, except that it returns the info
  ;; in a different format.
  (and (eq 'macro (car-safe f)) (setq f (cdr f)))
  ;; Advice wrappers have "catch all" args, so fetch the actual underlying
  ;; function to find the real arguments.
  (setq f (advice--cd*r f))
  (if (eq (car-safe f) 'declared)
      (byte-compile-arglist-signature (nth 1 f))
    (condition-case nil
        (let ((sig (func-arity f)))
          (if (numberp (cdr sig)) sig (list (car sig))))
      (error '(0)))))

(defun byte-compile-arglist-signatures-congruent-p (old new)
  (not (or
	 (> (car new) (car old))  ; requires more args now
	 (and (null (cdr old))    ; took rest-args, doesn't any more
	      (cdr new))
	 (and (cdr new) (cdr old) ; can't take as many args now
	      (< (cdr new) (cdr old)))
	 )))

(defun byte-compile-arglist-signature-string (signature)
  (cond ((null (cdr signature))
	 (format "%d or more" (car signature)))
	((= (car signature) (cdr signature))
	 (format "%d" (car signature)))
	((= (1+ (car signature)) (cdr signature))
	 (format "%d or %d" (car signature) (cdr signature)))
	(t (format "%d-%d" (car signature) (cdr signature)))))

(defun byte-compile-function-warn (f nargs def)
  (when (and (get f 'byte-obsolete-info)
             (not (memq f byte-compile-not-obsolete-funcs)))
    (byte-compile-warn-obsolete f "function"))

  ;; Check to see if the function will be available at runtime
  ;; and/or remember its arity if it's unknown.
  (or (and (or def (fboundp f))         ; might be a subr or autoload.
           (not (memq f byte-compile-noruntime-functions)))
      (eq f byte-compile-current-form)  ; ## This doesn't work
                                        ; with recursion.
      ;; It's a currently-undefined function.
      ;; Remember number of args in call.
      (let ((cons (assq f byte-compile-unresolved-functions)))
        (if cons
            (or (memq nargs (cddr cons))
                (push nargs (cddr cons)))
          (push (list f
                      (if (symbol-with-pos-p f)
                          (symbol-with-pos-pos f)
                        1)              ; Should never happen.
                      nargs)
                byte-compile-unresolved-functions)))))

(defun byte-compile-emit-callargs-warn (name actual-args min-args max-args)
  (when (byte-compile-warning-enabled-p 'callargs name)
    (byte-compile-warn-x
     name
     "`%s' called with %d argument%s, but %s %s"
     name actual-args
     (if (= 1 actual-args) "" "s")
     (if (< actual-args min-args)
         "requires"
       "accepts only")
     (byte-compile-arglist-signature-string (cons min-args max-args)))))

(defun byte-compile--check-arity-bytecode (form bytecode)
  "Check that the call in FORM matches that allowed by BYTECODE."
  (when (byte-code-function-p bytecode)
    (let* ((actual-args (length (cdr form)))
           (arity (func-arity bytecode))
           (min-args (car arity))
           (max-args (and (numberp (cdr arity)) (cdr arity))))
      (when (or (< actual-args min-args)
                (and max-args (> actual-args max-args)))
        (byte-compile-emit-callargs-warn
         (car form) actual-args min-args max-args)))))

;; Warn if the form is calling a function with the wrong number of arguments.
(defun byte-compile-callargs-warn (form)
  (let* ((def (or (byte-compile-fdefinition (car form) nil)
		  (byte-compile-fdefinition (car form) t)))
         (sig (cond (def (byte-compile--function-signature def))
                    ((subrp (symbol-function (car form)))
                     (subr-arity (symbol-function (car form))))))
	 (ncall (length (cdr form))))
    ;; Check many or unevalled from subr-arity.
    (if (and (cdr-safe sig)
	     (not (numberp (cdr sig))))
	(setcdr sig nil))
    (if sig
	(when (or (< ncall (car sig))
		  (and (cdr sig) (> ncall (cdr sig))))
          (byte-compile-emit-callargs-warn
           (car form) ncall (car sig) (cdr sig))))
    (byte-compile-format-warn form)
    (byte-compile-function-warn (car form) (length (cdr form)) def)))

(defun byte-compile-format-warn (form)
  "Warn if FORM is `format'-like with inconsistent args.
Applies if head of FORM is a symbol with non-nil property
`byte-compile-format-like' and first arg is a constant string.
Then check the number of format fields matches the number of
extra args."
  (when (and (symbolp (car form))
	     (stringp (nth 1 form))
	     (get (car form) 'byte-compile-format-like))
    (let* ((nargs (length (cddr form)))
           (nfields 0)
           (format-str (nth 1 form))
           (len (length format-str))
           (start 0))
      (while (and (< start len)
                  (string-match
                   (rx "%"
                       (? (group (+ digit)) "$")           ; field
                       (* (in "+ #0-"))                    ; flags
                       (* digit)                           ; width
                       (? "." (* digit))                   ; precision
                       (? (group (in "sdibBoxXefgcS%"))))  ; spec
                   format-str start))
        (let ((field (if (match-beginning 1)
                         (string-to-number (match-string 1 format-str))
                       (1+ nfields)))
              (spec (and (match-beginning 2)
                         (aref format-str (match-beginning 2)))))
          (setq start (match-end 0))
          (cond
           ((not spec)
            (byte-compile-warn-x
             form "Bad format sequence in call to `%s' at string offset %d"
             (car form) (match-beginning 0)))
           ((not (eq spec ?%))
            (setq nfields (max field nfields))))))
      (unless (= nargs nfields)
	(byte-compile-warn-x
         (car form) "`%s' called with %d argument%s to fill %d format field%s"
         (car form)
         nargs (if (= nargs 1) "" "s")
         nfields (if (= nfields 1) "" "s"))))))

(dolist (elt '( format message format-message message-box message-or-box
                warn error user-error))
  (put elt 'byte-compile-format-like t))

;; Warn if the function or macro is being redefined with a different
;; number of arguments.
(defun byte-compile-arglist-warn (name arglist macrop)
  ;; This is the first definition.  See if previous calls are compatible.
  (let ((calls (assq name byte-compile-unresolved-functions)))
    (when calls
      (when macrop
        (byte-compile-warn-x name "macro `%s' defined too late" name))
      (setq byte-compile-unresolved-functions
            (delq calls byte-compile-unresolved-functions))
      (let ((nums (delq t (cddr calls))))  ; Ignore higher-order uses.
        (when nums
          (when (and (symbolp name)
                     (eq (function-get name 'byte-optimizer)
                         'byte-compile-inline-expand))
            (byte-compile-warn-x
             name "defsubst `%s' was used before it was defined" name))
          (let ((sig (byte-compile-arglist-signature arglist))
                (min (apply #'min nums))
                (max (apply #'max nums)))
            (when (or (< min (car sig))
                      (and (cdr sig) (> max (cdr sig))))
              (byte-compile-warn-x
               name
               "%s being defined to take %s%s, but was previously called with %s"
               name
               (byte-compile-arglist-signature-string sig)
               (if (equal sig '(1 . 1)) " arg" " args")
               (byte-compile-arglist-signature-string (cons min max)))))))))
  (let* ((old (byte-compile-fdefinition name macrop))
         (initial (and macrop
                       (cdr (assq name
                                  byte-compile-initial-macro-environment)))))
    ;; Assumes an element of b-c-i-macro-env that is a symbol points
    ;; to a defined function.  (Bug#8646)
    (and initial (symbolp initial)
         (setq old (byte-compile-fdefinition initial nil)))
    (when (and old (not (eq old t)))
      (let ((sig1 (byte-compile--function-signature old))
            (sig2 (byte-compile-arglist-signature arglist)))
        (unless (byte-compile-arglist-signatures-congruent-p sig1 sig2)
          (byte-compile-warn-x
           name
           "%s %s used to take %s %s, now takes %s"
           (if macrop "macro" "function")
           name
           (byte-compile-arglist-signature-string sig1)
           (if (equal sig1 '(1 . 1)) "argument" "arguments")
           (byte-compile-arglist-signature-string sig2)))))))

(defun bytecomp--docstring-line-width (str)
  "An approximation of the displayed width of docstring line STR."
  ;; For literal key sequence substitutions (e.g. "\\`C-h'"), just
  ;; remove the markup as `substitute-command-keys' would.
  (when (string-search "\\`" str)
    (setq str (replace-regexp-in-string
               (rx "\\`" (group (* (not "'"))) "'")
               "\\1"
               str t)))
  ;; Heuristic: We can't reliably do `substitute-command-keys'
  ;; substitutions, since the value of a keymap in general can't be
  ;; known at compile time.  So instead, we assume that these
  ;; substitutions are of some constant length.
  (when (string-search "\\[" str)
    (setq str (replace-regexp-in-string
               (rx "\\[" (* (not "]")) "]")
               ;; We assume that substitutions have this length.
               ;; To preserve the non-expansive property of the transform,
               ;; it shouldn't be more than 3 characters long.
               "xxx"
               str t t)))
  (setq str
        (replace-regexp-in-string
         (rx (or
              ;; Ignore some URLs.
              (seq "http" (? "s") "://" (* nonl))
              ;; Ignore these `substitute-command-keys' substitutions.
              (seq "\\" (or "="
                            (seq "<" (* (not ">")) ">")
                            (seq "{" (* (not "}")) "}")))
              ;; Ignore the function signature that's stashed at the end of
              ;; the doc string (in some circumstances).
              (seq bol "(" (+ (any word "-/:[]&"))
                   ;; One or more arguments.
                   (+ " " (or
                           ;; Arguments.
                           (+ (or (syntax symbol)
                                  (any word "-/:[]&=()<>.,?^\\#*'\"")))
                           ;; Argument that is a list.
                           (seq "(" (* (not ")")) ")")))
                   ")")))
         "" str t t))
  (length str))

(defun byte-compile--wide-docstring-p (docstring max-width)
  "Whether DOCSTRING contains a line wider than MAX-WIDTH.
Ignore all `substitute-command-keys' substitutions, except for
the `\\\\=[command]' ones that are assumed to be of a fixed length.
Also ignore URLs."
  (let ((string-len (length docstring))
        (start 0)
        (too-wide nil))
    (while (< start string-len)
      (let ((eol (or (string-search "\n" docstring start)
                     string-len)))
        ;; Since `bytecomp--docstring-line-width' is non-expansive,
        ;; we can safely assume that if the raw length is
        ;; within the allowed width, then so is the transformed width.
        ;; This allows us to avoid the very expensive transformation in
        ;; most cases.
        (if (and (> (- eol start) max-width)
                 (> (bytecomp--docstring-line-width
                     (substring docstring start eol))
                    max-width))
            (progn
              (setq too-wide t)
              (setq start string-len))
          (setq start (1+ eol)))))
    too-wide))

(defcustom byte-compile-docstring-max-column 80
  "Recommended maximum width of doc string lines.
The byte-compiler will emit a warning for documentation strings
containing lines wider than this.  If `fill-column' has a larger
value, it will override this variable."
  :type 'natnum
  :safe #'natnump
  :version "28.1")

(defun byte-compile--list-with-n (list n elem)
  "Return LIST with its Nth element replaced by ELEM."
  (if (eq elem (nth n list))
      list
    (nconc (take n list)
           (list elem)
           (nthcdr (1+ n) list))))

(defun byte-compile--docstring-style-warn (docs kind name)
  "Warn if there are stylistic problems in the docstring DOCS.
Warn if documentation string is too wide.
It is too wide if it has any lines longer than the largest of
`fill-column' and `byte-compile-docstring-max-column'."
  (when (byte-compile-warning-enabled-p 'docstrings)
    (let* ((name (if (eq (car-safe name) 'quote) (cadr name) name))
           (prefix (lambda ()
                     (format "%s%s"
                             kind
                             (if name (format-message " `%S' " name) "")))))
      (let ((col (max byte-compile-docstring-max-column fill-column)))
        (when (and (byte-compile-warning-enabled-p 'docstrings-wide)
                   (byte-compile--wide-docstring-p docs col))
          (byte-compile-warn-x
           name
           "%sdocstring wider than %s characters" (funcall prefix) col)))

      (when (byte-compile-warning-enabled-p 'docstrings-control-chars)
        (let ((start 0)
              (len (length docs)))
          (while (and (< start len)
                      (string-match (rx (intersection (in (0 . 31) 127)
                                                      (not (in "\n\t"))))
                                    docs start))
            (let* ((ofs (match-beginning 0))
                   (c (aref docs ofs)))
              ;; FIXME: it should be possible to use the exact source position
              ;; of the control char in most cases, and it would be helpful
              (byte-compile-warn-x
               name
               "%sdocstring contains control char #x%02x (position %d)"
               (funcall prefix) c ofs)
              (setq start (1+ ofs))))))

      ;; There's a "naked" ' character before a symbol/list, so it
      ;; should probably be quoted with \=.
      (when (string-match-p (rx (| (in " \t") bol)
                                (? (in "\"#"))
                                "'"
                                (in "A-Za-z" "("))
                            docs)
        (byte-compile-warn-x
         name
         (concat "%sdocstring has wrong usage of unescaped single quotes"
                 " (use \\=%c or different quoting such as %c...%c)")
         (funcall prefix) ?' ?` ?'))
      ;; There's a "Unicode quote" in the string -- it should probably
      ;; be an ASCII one instead.
      (when (byte-compile-warning-enabled-p 'docstrings-non-ascii-quotes)
        (when (string-match-p (rx (| " \"" (in " \t") bol)
                                  (in "‘’"))
                              docs)
          (byte-compile-warn-x
           name
           "%sdocstring uses curved single quotes; use %s instead of ‘...’"
           (funcall prefix) "`...'"))))))

(defvar byte-compile--\#$) ; Special value that will print as `#$'.
(defvar byte-compile--docstrings nil "Table of already compiled docstrings.")

(defun byte-compile--docstring (doc kind name &optional is-a-value)
  (byte-compile--docstring-style-warn doc kind name)
  ;; Make docstrings dynamic, when applicable.
  (cond
   ((and byte-compile-dynamic-docstrings
         ;; The native compiler doesn't use those dynamic docstrings.
         (not byte-native-compiling)
         ;; Docstrings can only be dynamic when compiling a file.
         byte-compile--\#$
         (not (equal doc "")))        ; empty lazy strings are pointless
    (let* ((byte-pos (with-memoization
                         ;; Reuse a previously written identical docstring.
                         ;; This is not done out of thriftiness but to try and
                         ;; make sure that "equal" functions remain `equal'.
                         ;; (Often those identical docstrings come from
                         ;; `help-add-fundoc-usage').
                         ;; Needed e.g. for `advice-tests-nadvice'.
                         (gethash doc byte-compile--docstrings)
                       (byte-compile-output-as-comment doc nil)))
           (newdoc (cons byte-compile--\#$ byte-pos)))
      (if is-a-value newdoc (macroexp-quote newdoc))))
   (t doc)))

;; If we have compiled any calls to functions which are not known to be
;; defined, issue a warning enumerating them.
;; `unresolved' in the list `byte-compile-warnings' disables this.
(defun byte-compile-warn-about-unresolved-functions ()
  (let ((byte-compile-current-form :end))
    ;; Separate the functions that will not be available at runtime
    ;; from the truly unresolved ones.
    (dolist (urf byte-compile-unresolved-functions)
      (let ((f (car urf)))
        (when (and (not (memq f byte-compile-new-defuns))
                   (byte-compile-warning-enabled-p 'unresolved))
          (byte-compile-warn-x
           f
           (if (fboundp f)
               "the function `%s' might not be defined at runtime."
             "the function `%s' is not known to be defined.")
           (car urf)))))))


;; Dynamically bound in byte-compile-from-buffer.
;; NB also used in cl.el and cl-macs.el.
(defvar byte-compile--outbuffer)

(defmacro byte-compile-close-variables (&rest body)
  (declare (debug t))
  `(let (;;
         ;; Close over these variables to encapsulate the
         ;; compilation state
         ;;
         (byte-compile-macro-environment
          ;; Copy it because the compiler may patch into the
          ;; macroenvironment.
          (copy-alist byte-compile-initial-macro-environment))
         (byte-compile--outbuffer nil)
         (byte-compile--\#$ nil)
         (byte-compile--docstrings (make-hash-table :test 'equal))
         (overriding-plist-environment nil)
         (byte-compile-function-environment nil)
         (byte-compile-bound-variables nil)
         (byte-compile-lexical-variables nil)
         (byte-compile-const-variables nil)
         (byte-compile-free-references nil)
         (byte-compile-free-assignments nil)
         ;;
         ;; Close over these variables so that `byte-compiler-options'
         ;; can change them on a per-file basis.
         ;;
         (byte-compile-verbose byte-compile-verbose)
         (byte-optimize byte-optimize)
         (byte-compile-dynamic-docstrings
          byte-compile-dynamic-docstrings)
         (byte-compile-warnings byte-compile-warnings)
         ;; Indicate that we're not currently loading some file.
         ;; This is used in `macroexp-file-name' to make sure that
         ;; loading file A which does (byte-compile-file B) won't
         ;; cause macro calls in B to think they come from A.
         (current-load-list (list nil))
         )
     (prog1
         (progn ,@body)
       (when byte-native-compiling
         (setq byte-to-native-plist-environment
               overriding-plist-environment)))))

(defmacro displaying-byte-compile-warnings (&rest body) ;FIXME: namespace!
  (declare (debug (def-body)))
  `(bytecomp--displaying-warnings (lambda () ,@body)))

(defun bytecomp--displaying-warnings (body-fn)
  (let* ((wrapped-body
	  (lambda ()
	    (if byte-compile-debug
	        (funcall body-fn)
	      ;; Use a `handler-bind' to remember the `byte-compile-form-stack'
	      ;; active at the time the error is signaled, so as to
	      ;; get more precise error locations.
	      (let ((form-stack nil))
		(condition-case error-info
		    (handler-bind
		        ((error (lambda (_err)
		                  (setq form-stack byte-compile-form-stack))))
		      (funcall body-fn))
		  (error (let ((byte-compile-form-stack form-stack))
		           (byte-compile-report-error error-info))))))))
	 (warning-series-started
	  (and (markerp warning-series)
	       (eq (marker-buffer warning-series)
		   (get-buffer byte-compile-log-buffer))))
         (byte-compile-form-stack byte-compile-form-stack))
    (if (or (eq warning-series #'byte-compile-warning-series)
	    warning-series-started)
	;; warning-series does come from compilation,
	;; so don't bind it, but maybe do set it.
	(let ((tem (byte-compile-log-file))) ;; Log the file name.
	  (unless warning-series-started
	    (setq warning-series (or tem #'byte-compile-warning-series)))
	  (funcall wrapped-body))
      ;; warning-series does not come from compilation, so bind it.
      (let ((warning-series
	     ;; Log the file name.  Record position of that text.
	     (or (byte-compile-log-file) #'byte-compile-warning-series)))
	(funcall wrapped-body)))))

;;;###autoload
(defun byte-force-recompile (directory)
  "Recompile every `.el' file in DIRECTORY that already has a `.elc' file.
Files in subdirectories of DIRECTORY are processed also."
  (interactive "DByte force recompile (directory): ")
  (byte-recompile-directory directory nil t))

(defvar byte-compile-ignore-files nil
  "List of regexps for files to ignore during byte compilation.")

;;;###autoload
(defun byte-recompile-directory (directory &optional arg force follow-symlinks)
  "Recompile every `.el' file in DIRECTORY that needs recompilation.
This happens when a `.elc' file exists but is older than the `.el' file.
Files in subdirectories of DIRECTORY are processed also.

If the `.elc' file does not exist, normally this function *does not*
compile the corresponding `.el' file.  However, if the prefix argument
ARG is 0, that means do compile all those files.  A nonzero
ARG means ask the user, for each such `.el' file, whether to
compile it.  A nonzero ARG also means ask about each subdirectory
before scanning it.

If the third argument FORCE is non-nil, recompile every `.el' file
that already has a `.elc' file.

This command will normally not follow symlinks when compiling
files.  If FOLLOW-SYMLINKS is non-nil, symlinked `.el' files will
also be compiled."
  (interactive "DByte recompile directory: \nP")
  (if arg (setq arg (prefix-numeric-value arg)))
  (if noninteractive
      nil
    (save-some-buffers
     nil (lambda ()
           (let ((file (buffer-file-name)))
             (and file
                  (string-match-p emacs-lisp-file-regexp file)
                  (file-in-directory-p file directory)))))
    (force-mode-line-update))
  (with-current-buffer (get-buffer-create byte-compile-log-buffer)
    (setq default-directory (expand-file-name directory))
    ;; compilation-mode copies value of default-directory.
    (unless (derived-mode-p 'compilation-mode)
      (emacs-lisp-compilation-mode))
    (let ((directories (list default-directory))
	  (default-directory default-directory)
          (ignore-files-regexp
           (if byte-compile-ignore-files
               (mapconcat #'identity byte-compile-ignore-files "\\|")
             regexp-unmatchable))
	  (skip-count 0)
	  (fail-count 0)
	  (file-count 0)
	  (dir-count 0)
	  last-dir)
      (displaying-byte-compile-warnings
       (while directories
	 (setq directory (car directories))
	 (message "Checking %s..." directory)
         (dolist (source (directory-files directory t))
           (let ((file (file-name-nondirectory source)))
	     (if (file-directory-p source)
		 (and (not (member file '("RCS" "CVS")))
		      (not (eq ?\. (aref file 0)))
                      (or follow-symlinks
		          (not (file-symlink-p source)))
		      ;; This file is a subdirectory.  Handle them differently.
		      (or (null arg) (eq 0 arg)
			  (y-or-n-p (concat "Check " source "? ")))
                      ;; Directory is requested to be ignored
                      (not (string-match-p ignore-files-regexp source))
                      (setq directories (nconc directories (list source))))
               ;; It is an ordinary file.  Decide whether to compile it.
               (if (and (string-match emacs-lisp-file-regexp source)
			;; The next 2 tests avoid compiling lock files
                        (file-readable-p source)
			(not (string-match "\\`\\.#" file))
                        (not (auto-save-file-name-p source))
                        (not (member source (dir-locals--all-files directory)))
                        ;; File is requested to be ignored
                        (not (string-match-p ignore-files-regexp source)))
                   (progn (incf
                           (pcase (byte-recompile-file source force arg)
                             ('no-byte-compile skip-count)
                             ('t file-count)
                             (_ fail-count)))
                          (or noninteractive
                              (message "Checking %s..." directory))
                          (if (not (eq last-dir directory))
                              (setq last-dir directory
                                    dir-count (1+ dir-count)))
                          )))))
	 (setq directories (cdr directories))))
      (message "Done (Total of %d file%s compiled%s%s%s)"
	       file-count (if (= file-count 1) "" "s")
	       (if (> fail-count 0) (format ", %d failed" fail-count) "")
	       (if (> skip-count 0) (format ", %d skipped" skip-count) "")
	       (if (> dir-count 1)
                   (format " in %d directories" dir-count) "")))))

(defvar no-byte-compile nil
  "Non-nil to prevent byte-compiling of Emacs Lisp code.
This is normally set in local file variables at the end of the elisp file:

\;; Local Variables:\n;; no-byte-compile: t\n;; End:") ;Backslash for compile-main.
;;;###autoload(put 'no-byte-compile 'safe-local-variable 'booleanp)

(defun byte-recompile-file (filename &optional force arg load)
  "Recompile FILENAME file if it needs recompilation.
This happens when its `.elc' file is older than itself.

If the `.elc' file exists and is up-to-date, normally this function
*does not* compile FILENAME.  If the prefix argument FORCE is non-nil,
however, it compiles FILENAME even if the destination already
exists and is up-to-date.

If the `.elc' file does not exist, normally this function *does not*
compile FILENAME.  If optional argument ARG is 0, it compiles
the input file even if the `.elc' file does not exist.
Any other non-nil value of ARG means to ask the user.

If compilation is needed, this functions returns the result of
`byte-compile-file'; otherwise it returns `no-byte-compile'."
  (declare (advertised-calling-convention (filename &optional force arg) "28.1"))
  (interactive
   (let ((file buffer-file-name)
	 (file-name nil)
	 (file-dir nil))
     (and file
	  (derived-mode-p 'emacs-lisp-mode)
	  (setq file-name (file-name-nondirectory file)
		file-dir (file-name-directory file)))
     (list (read-file-name (if current-prefix-arg
			       "Byte compile file: "
			     "Byte recompile file: ")
			   file-dir file-name nil)
	   current-prefix-arg)))
  (let ((dest (byte-compile-dest-file filename))
        ;; Expand now so we get the current buffer's defaults
        (filename (expand-file-name filename)))
    (prog1
        (if (if (and dest (file-exists-p dest))
                ;; File was already compiled
                ;; Compile if forced to, or filename newer
                (or force
                    (file-newer-than-file-p filename dest))
              (and arg
                   (or (eq 0 arg)
                       (y-or-n-p (concat "Compile "
                                         filename "? ")))))
            (progn
              (if (and noninteractive (not byte-compile-verbose))
                  (message "Compiling %s..." filename))
              (byte-compile-file filename))
	  'no-byte-compile)
      (when load
        (load (if (and dest (file-exists-p dest)) dest filename))))))

(defun byte-compile--load-dynvars (file)
  (and file (not (equal file ""))
       (with-temp-buffer
         (insert-file-contents file)
         (let ((vars nil)
               var)
           (while (ignore-errors (setq var (read (current-buffer))))
             (push var vars))
           vars))))

(defvar byte-compile-level 0		; bug#13787
  "Depth of a recursive byte compilation.")

(defun byte-write-target-file (buffer target-file)
  "Write BUFFER into TARGET-FILE."
  (with-current-buffer buffer
    ;; We must disable any code conversion here.
    (let* ((coding-system-for-write 'no-conversion)
	   ;; Write to a tempfile so that if another Emacs
	   ;; process is trying to load target-file (eg in a
	   ;; parallel bootstrap), it does not risk getting a
	   ;; half-finished file.  (Bug#4196)
	   (tempfile
	    (make-temp-file (when (file-writable-p target-file)
                              (expand-file-name target-file))))
	   (default-modes (default-file-modes))
	   (temp-modes (logand default-modes #o600))
	   (desired-modes (logand default-modes #o666))
	   (kill-emacs-hook
	    (cons (lambda () (ignore-errors
			  (delete-file tempfile)))
		  kill-emacs-hook)))
      (unless (= temp-modes desired-modes)
        (set-file-modes tempfile desired-modes 'nofollow))
      (write-region (point-min) (point-max) tempfile nil 1)
      ;; This has the intentional side effect that any
      ;; hard-links to target-file continue to
      ;; point to the old file (this makes it possible
      ;; for installed files to share disk space with
      ;; the build tree, without causing problems when
      ;; emacs-lisp files in the build tree are
      ;; recompiled).  Previously this was accomplished by
      ;; deleting target-file before writing it.
      (if byte-native-compiling
          ;; Defer elc final renaming.
          (setf byte-to-native-output-buffer-file
                (cons tempfile target-file))
        (rename-file tempfile target-file t)))))

(defvar bytecomp--inhibit-lexical-cookie-warning nil)

;;;###autoload
(defun byte-compile-file (filename &optional load)
  "Compile a file of Lisp code named FILENAME into a file of byte code.
The output file's name is generated by passing FILENAME to the
function `byte-compile-dest-file' (which see).
The value is non-nil if there were no errors, nil if errors.
If the file sets the file variable `no-byte-compile', it is not
compiled, any existing output file is removed, and the return
value is `no-byte-compile'.

See also `emacs-lisp-byte-compile-and-load'."
  (declare (advertised-calling-convention (filename) "28.1"))
  (interactive
   (let ((file buffer-file-name)
	 (file-dir nil))
     (and file
	  (derived-mode-p 'emacs-lisp-mode)
	  (setq file-dir (file-name-directory file)))
     (list (read-file-name (if current-prefix-arg
			       "Byte compile and load file: "
			     "Byte compile file: ")
			   file-dir buffer-file-name nil)
	   current-prefix-arg)))
  ;; Expand now so we get the current buffer's defaults
  (setq filename (expand-file-name filename))

  ;; If we're compiling a file that's in a buffer and is modified, offer
  ;; to save it first.
  (or noninteractive
      (let ((b (get-file-buffer (expand-file-name filename))))
	(if (and b (buffer-modified-p b)
		 (y-or-n-p (format "Save buffer %s first? " (buffer-name b))))
	    (with-current-buffer b (save-buffer)))))

  ;; Force logging of the file name for each file compiled.
  (setq byte-compile-last-logged-file nil)
  (let ((byte-compile-current-file filename)
        (byte-compile-current-group nil)
	(set-auto-coding-for-load t)
        (byte-compile--seen-defvars nil)
        (byte-compile--known-dynamic-vars
         (byte-compile--load-dynvars (getenv "EMACS_DYNVARS_FILE")))
	target-file input-buffer output-buffer
	byte-compile-dest-file byte-compiler-error-flag)
    (setq target-file (byte-compile-dest-file filename))
    (setq byte-compile-dest-file target-file)
    (with-current-buffer
	;; It would be cleaner to use a temp buffer, but if there was
	;; an error, we leave this buffer around for diagnostics.
	;; Its name is documented in the lispref.
	(setq input-buffer (get-buffer-create
			    (concat " *Compiler Input*"
				    (if (zerop byte-compile-level) ""
				      (format "-%s" byte-compile-level)))))
      (erase-buffer)
      (setq buffer-file-coding-system nil)
      ;; Always compile an Emacs Lisp file as multibyte
      ;; unless the file itself forces unibyte with -*-coding: raw-text;-*-
      (set-buffer-multibyte t)
      (insert-file-contents filename)
      ;; Mimic the way after-insert-file-set-coding can make the
      ;; buffer unibyte when visiting this file.
      (when (or (eq last-coding-system-used 'no-conversion)
		(eq (coding-system-type last-coding-system-used) 5))
	;; For coding systems no-conversion and raw-text...,
	;; edit the buffer as unibyte.
	(set-buffer-multibyte nil))
      ;; Run hooks including the uncompression hook.
      ;; If they change the file name, then change it for the output also.
      (let ((buffer-file-name filename)
            (dmm (default-value 'major-mode))
            ;; Ignore unsafe local variables.
            ;; We only care about a few of them for our purposes.
            (enable-local-variables :safe)
            (enable-local-eval nil))
        (unwind-protect
            (progn
              (setq-default major-mode 'emacs-lisp-mode)
              ;; Arg of t means don't alter enable-local-variables.
              (delay-mode-hooks (normal-mode t)))
          (setq-default major-mode dmm))
        ;; There may be a file local variable setting (bug#10419).
        (setq buffer-read-only nil
              filename buffer-file-name))
      ;; Don't inherit lexical-binding from caller (bug#12938).
      (unless (or (local-variable-p 'lexical-binding)
                  bytecomp--inhibit-lexical-cookie-warning)
        (let ((byte-compile-current-buffer (current-buffer)))
          (displaying-byte-compile-warnings
           (byte-compile-warn-x
            (position-symbol 'a (point-min))
            "file has no `lexical-binding' directive on its first line")))
        (setq-local lexical-binding nil))
      ;; Set the default directory, in case an eval-when-compile uses it.
      (setq default-directory (file-name-directory filename)))
    ;; Check if the file's local variables explicitly specify not to
    ;; compile this file.
    (if (with-current-buffer input-buffer no-byte-compile)
	(progn
	  ;; (message "%s not compiled because of `no-byte-compile: %s'"
	  ;; 	   (byte-compile-abbreviate-file filename)
	  ;; 	   (with-current-buffer input-buffer no-byte-compile))
	  (when (and target-file (file-exists-p target-file))
	    (message "%s deleted because of `no-byte-compile: %s'"
		     (byte-compile-abbreviate-file target-file)
		     (buffer-local-value 'no-byte-compile input-buffer))
	    (condition-case nil (delete-file target-file) (error nil)))
	  ;; We successfully didn't compile this file.
	  'no-byte-compile)
      (when byte-compile-verbose
	(message "Compiling %s..." filename))
      ;; It is important that input-buffer not be current at this call,
      ;; so that the value of point set in input-buffer
      ;; within byte-compile-from-buffer lingers in that buffer.
      (setq output-buffer
	    (save-current-buffer
	      (let ((byte-compile-level (1+ byte-compile-level)))
                (byte-compile-from-buffer input-buffer))))
      (if byte-compiler-error-flag
	  nil
	(when byte-compile-verbose
	  (message "Compiling %s...done" filename))
	(kill-buffer input-buffer)
	(with-current-buffer output-buffer
          (when (and target-file
                     (or (not byte-native-compiling)
                         (and byte-native-compiling byte+native-compile)))
	    (goto-char (point-max))
	    (insert "\n")			; aaah, unix.
	    (cond
	     ((and (file-writable-p target-file)
		   ;; We attempt to create a temporary file in the
		   ;; target directory, so the target directory must be
		   ;; writable.
		   (file-writable-p
		    (file-name-directory
		     ;; Need to expand in case TARGET-FILE doesn't
		     ;; include a directory (Bug#45287).
		     (expand-file-name target-file))))
              (if byte-native-compiling
                  ;; Defer elc production.
                  (setf byte-to-native-output-buffer-file
                        (cons (current-buffer) target-file))
                (byte-write-target-file (current-buffer) target-file))
	      (or noninteractive
		  byte-native-compiling
		  (message "Wrote %s" target-file)))
             ((file-writable-p target-file)
              ;; In case the target directory isn't writable (see e.g. Bug#44631),
              ;; try writing to the output file directly.  We must disable any
              ;; code conversion here.
              (let ((coding-system-for-write 'no-conversion))
                (with-file-modes (logand (default-file-modes) #o666)
                  (write-region (point-min) (point-max) target-file nil 1)))
              (or noninteractive (message "Wrote %s" target-file)))
	     (t
	      ;; This is just to give a better error message than write-region
	      (let ((exists (file-exists-p target-file)))
	        (signal (if exists 'file-error 'file-missing)
		        (list "Opening output file"
			      (if exists
				  "Cannot overwrite file"
			        "Directory not writable or nonexistent")
			      target-file))))))
          (unless byte-native-compiling
	    (kill-buffer (current-buffer))))
	(if (and byte-compile-generate-call-tree
		 (or (eq t byte-compile-generate-call-tree)
		     (y-or-n-p (format "Report call tree for %s? "
                                       filename))))
	    (save-excursion
	      (display-call-tree filename)))
        (let ((gen-dynvars (getenv "EMACS_GENERATE_DYNVARS")))
          (when (and gen-dynvars (not (equal gen-dynvars ""))
                     byte-compile--seen-defvars)
            (let ((dynvar-file (concat target-file ".dynvars"))
                  (print-symbols-bare t))
              (message "Generating %s" dynvar-file)
              (with-temp-buffer
                (dolist (var (delete-dups byte-compile--seen-defvars))
                  (insert (format "%S\n" (cons var filename))))
	        (write-region (point-min) (point-max) dynvar-file)))))
	(if load
            (load target-file))
	t))))

;;; compiling a single function
;;;###autoload
(defun compile-defun (&optional arg)
  "Compile and evaluate the current top-level form.
Print the result in the echo area.
With argument ARG, insert value in current buffer after the form."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (beginning-of-defun)
    (let* ((print-symbols-bare t)       ; For the final `message'.
           (byte-compile-current-file (current-buffer))
	   (byte-compile-current-buffer (current-buffer))
	   (start-read-position (point))
	   (byte-compile-last-warned-form 'nothing)
           (symbols-with-pos-enabled t)
	   (value (eval
		   (displaying-byte-compile-warnings
		    (byte-compile-sexp
                     (let ((form (read-positioning-symbols (current-buffer))))
                       (push form byte-compile-form-stack)
                       (eval-sexp-add-defvars
                        form
                        start-read-position))))
                   lexical-binding)))
      (cond (arg
	     (message "Compiling from buffer... done.")
	     (prin1 value (current-buffer))
	     (insert "\n"))
	    ((message "%s" (prin1-to-string value)))))))

(defun byte-compile-from-buffer (inbuffer)
  (let ((byte-compile-current-buffer inbuffer)
	;; Prevent truncation of flonums and lists as we read and print them
	(float-output-format nil)
	(case-fold-search nil)
	(print-length nil)
	(print-level nil)
        (print-symbols-bare t)
	;; Prevent edebug from interfering when we compile
	;; and put the output into a file.
;; 	(edebug-all-defs nil)
;; 	(edebug-all-forms nil)
	;; Simulate entry to byte-compile-top-level
        (byte-compile-jump-tables nil)
        (byte-compile-constants nil)
	(byte-compile-variables nil)
	(byte-compile-tag-number 0)
	(byte-compile-depth 0)
	(byte-compile-maxdepth 0)
	(byte-compile-output nil)
        (bytecomp--code-strings nil)
	;;	  #### This is bound in b-c-close-variables.
	;;	  (byte-compile-warnings byte-compile-warnings)
        (symbols-with-pos-enabled t))
    (byte-compile-close-variables
     (with-current-buffer
         (setq byte-compile--outbuffer
               (get-buffer-create
                (concat " *Compiler Output*"
                        (if (<= byte-compile-level 1) ""
                          (format "-%s" (1- byte-compile-level))))))
       (set-buffer-multibyte t)
       (erase-buffer)
       ;;	 (emacs-lisp-mode)
       (setq case-fold-search nil))
     (displaying-byte-compile-warnings
      (with-current-buffer inbuffer
	(when byte-compile-dest-file
          (setq byte-compile--\#$
                (copy-sequence ;It needs to be a fresh new object.
                 ;; Also it stands for the `load-file-name' when the `.elc' will
                 ;; be loaded, so make it look like it.
                 byte-compile-dest-file))
	  (byte-compile-insert-header byte-compile-current-file
                                      byte-compile--outbuffer)
          ;; Instruct native-comp to ignore this file.
          (when (bound-and-true-p no-native-compile)
            (with-current-buffer byte-compile--outbuffer
              (insert
               "(when (boundp 'comp--no-native-compile)
  (puthash load-file-name t comp--no-native-compile))\n\n"))))
	(goto-char (point-min))
	;; Should we always do this?  When calling multiple files, it
	;; would be useful to delay this warning until all have been
	;; compiled.  A: Yes!  b-c-u-f might contain dross from a
	;; previous byte-compile.
	(setq byte-compile-unresolved-functions nil)
        (setq byte-compile-noruntime-functions nil)
        (setq byte-compile-new-defuns nil)
        (when byte-native-compiling
          (defvar native-comp-speed)
          (push `(native-comp-speed . ,native-comp-speed) byte-native-qualities)
          (push `(compilation-safety . ,compilation-safety) byte-native-qualities)
          (defvar native-comp-debug)
          (push `(native-comp-debug . ,native-comp-debug) byte-native-qualities)
          (defvar native-comp-compiler-options)
          (push `(native-comp-compiler-options . ,native-comp-compiler-options)
                byte-native-qualities)
          (defvar native-comp-driver-options)
          (push `(native-comp-driver-options . ,native-comp-driver-options)
                byte-native-qualities)
          (defvar no-native-compile)
          (push `(no-native-compile . ,no-native-compile)
                byte-native-qualities))

	;; Compile the forms from the input buffer.
	(while (progn
		 (while (progn (skip-chars-forward " \t\n\^l")
			       (= (following-char) ?\;))
		   (forward-line 1))
		 (not (eobp)))
          (let* ((lread--unescaped-character-literals nil)
                 ;; Don't bind `load-read-function' to
                 ;; `read-positioning-symbols' here.  Calls to `read'
                 ;; at a lower level must not get symbols with
                 ;; position.
                 (form (read-positioning-symbols inbuffer))
                 (warning (byte-run--unescaped-character-literals-warning)))
            (when warning (byte-compile-warn-x form "%s" warning))
	    (byte-compile-toplevel-file-form form)))
	;; Compile pending forms at end of file.
	(byte-compile-flush-pending)
	(byte-compile-warn-about-unresolved-functions)))
     byte-compile--outbuffer)))

(defun byte-compile-insert-header (_filename outbuffer)
  "Insert a header at the start of OUTBUFFER.
Call from the source buffer."
  (let ((optimize byte-optimize))
    (with-current-buffer outbuffer
      (goto-char (point-min))
      ;; The magic number of .elc files is ";ELC", or 0x3B454C43.  After
      ;; that is the file-format version number (18, 19, 20, or 23) as a
      ;; byte, followed by some nulls.  The primary motivation for doing
      ;; this is to get some binary characters up in the first line of
      ;; the file so that `diff' will simply say "Binary files differ"
      ;; instead of actually doing a diff of two .elc files.  An extra
      ;; benefit is that you can add this to /etc/magic:
      ;; 0	string		;ELC		GNU Emacs Lisp compiled file,
      ;; >4	byte		x		version %d
      (insert
       ";ELC"
       (let ((version
              (if (zerop emacs-minor-version)
                  ;; Let's allow silently loading into Emacs-27
                  ;; files compiled with Emacs-28.0.NN since the two can
                  ;; be almost identical (e.g. right after cutting the
                  ;; release branch) and people running the development
                  ;; branch can be presumed to know that it's risky anyway.
                  (1- emacs-major-version) emacs-major-version)))
         ;; Make sure the version is a plain byte that doesn't end the comment!
         (cl-assert (and (> version 13) (< version 128)))
         version)
       "\000\000\000\n"
       ";;; Compiled\n"
       ";;; in Emacs version " emacs-version "\n"
       ";;; with"
       (cond
	((eq optimize 'source) " source-level optimization only")
	((eq optimize 'byte) " byte-level optimization only")
	(optimize " all optimizations")
	(t "out optimization"))
       ".\n\n\n"))))

(defun byte-compile-output-file-form (form)
  ;; Write the given form to the output buffer, being careful of docstrings
  ;; (for `byte-compile-dynamic-docstrings').
  (when byte-native-compiling
    ;; Spill output for the native compiler here
    (push (make-byte-to-native-top-level :form form :lexical lexical-binding)
          byte-to-native-top-level-forms))
  (let ((print-escape-newlines t)
        (print-length nil)
        (print-level nil)
        (print-quoted t)
        (print-gensym t)
        (print-circle t)
        (print-continuous-numbering t)
        (print-number-table (make-hash-table :test #'eq)))
    (when byte-compile--\#$
      (puthash byte-compile--\#$ "#$" print-number-table))
    (princ "\n" byte-compile--outbuffer)
    (prin1 form byte-compile--outbuffer)
    nil))

(defvar byte-compile--for-effect)

(defun byte-compile-keep-pending (form &optional handler)
  (if (memq byte-optimize '(t source))
      (setq form (byte-optimize-one-form form t)))
  ;; To avoid consing up monstrously large forms at load time, we split
  ;; the output regularly.
  (when (nthcdr 300 byte-compile-output)
    (byte-compile-flush-pending))
  (if handler
      (let ((byte-compile--for-effect t))
	(funcall handler form)
	(if byte-compile--for-effect
	    (byte-compile-discard)))
    (byte-compile-form form t))
  nil)

(defun byte-compile-flush-pending ()
  (if byte-compile-output
      (let ((form (byte-compile-out-toplevel t 'file)))
	(cond ((eq (car-safe form) 'progn)
	       (mapc #'byte-compile-output-file-form (cdr form)))
	      (form
	       (byte-compile-output-file-form form)))
	(setq byte-compile-constants nil
	      byte-compile-variables nil
	      byte-compile-depth 0
	      byte-compile-maxdepth 0
	      byte-compile-output nil
              bytecomp--code-strings nil
              byte-compile-jump-tables nil))))

(defun byte-compile-preprocess (form &optional _for-effect)
  (setq form (macroexpand-all form byte-compile-macro-environment))
  ;; FIXME: We should run byte-optimize-form here, but it currently does not
  ;; recurse through all the code, so we'd have to fix this first.
  ;; Maybe a good fix would be to merge byte-optimize-form into
  ;; macroexpand-all.
  ;; (if (memq byte-optimize '(t source))
  ;;     (setq form (byte-optimize-form form for-effect)))
  (cconv-closure-convert form byte-compile-bound-variables))

;; byte-hunk-handlers cannot call this!
(defun byte-compile-toplevel-file-form (top-level-form)
  (macroexp--with-extended-form-stack top-level-form
    (byte-compile-recurse-toplevel
     top-level-form
     (lambda (form)
       (let ((byte-compile-current-form nil)) ; close over this for warnings.
         (byte-compile-file-form (byte-compile-preprocess form t)))))))

;; byte-hunk-handlers can call this.
(defun byte-compile-file-form (form)
  (let (handler)
    (cond ((and (consp form)
                (symbolp (car form))
		(setq handler (get (car form) 'byte-hunk-handler)))
	   (cond ((setq form (funcall handler form))
		  (byte-compile-flush-pending)
		  (byte-compile-output-file-form form))))
	  (t
	   (byte-compile-keep-pending form)))))

;; Functions and variables with doc strings must be output specially,
;; for `byte-compile-dynamic-docstrings'.  Most other things can be output
;; as byte-code.

(put 'autoload 'byte-hunk-handler #'byte-compile-file-form-autoload)
(defun byte-compile-file-form-autoload (form)
  (and (let ((form form))
	 (while (if (setq form (cdr form)) (macroexp-const-p (car form))))
	 (null form))                        ;Constants only
       (memq (eval (nth 5 form)) '(t macro)) ;Macro
       (eval form))                          ;Define the autoload.
  ;; Avoid undefined function warnings for the autoload.
  (pcase (nth 1 form)
    (`',(and (pred symbolp) funsym)
     ;; Don't add it if it's already defined.  Otherwise, it might
     ;; hide the actual definition.  However, do remove any entry from
     ;; byte-compile-noruntime-functions, in case we have an autoload
     ;; of foo-func following an (eval-when-compile (require 'foo)).
     (unless (fboundp funsym)
       (push (cons funsym (cons 'autoload (cdr (cdr form))))
             byte-compile-function-environment))
     ;; If an autoload occurs _before_ the first call to a function,
     ;; byte-compile-callargs-warn does not add an entry to
     ;; byte-compile-unresolved-functions.  Here we mimic the logic
     ;; of byte-compile-callargs-warn so as not to warn if the
     ;; autoload comes _after_ the function call.
     ;; Alternatively, similar logic could go in
     ;; byte-compile-warn-about-unresolved-functions.
     (if (memq funsym byte-compile-noruntime-functions)
         (setq byte-compile-noruntime-functions
               (delq funsym byte-compile-noruntime-functions))
       (setq byte-compile-unresolved-functions
             (delq (assq funsym byte-compile-unresolved-functions)
                   byte-compile-unresolved-functions)))))
  (let* ((doc (nth 3 form))
         (newdoc (if (not (stringp doc)) doc
                   (byte-compile--docstring
                    doc 'autoload (nth 1 form)))))
    (byte-compile-keep-pending (byte-compile--list-with-n form 3 newdoc)
                               #'byte-compile-normal-call)))

(put 'defvar   'byte-hunk-handler #'byte-compile-file-form-defvar)
(put 'defconst 'byte-hunk-handler #'byte-compile-file-form-defvar)

(defun byte-compile--check-prefixed-var (sym)
  (when (and (symbolp sym)
             (not (string-match "[-*/:$]" (symbol-name sym)))
             (byte-compile-warning-enabled-p 'lexical sym))
    (byte-compile-warn-x
     sym "global/dynamic var `%s' lacks a prefix" sym)))

(defun byte-compile--declare-var (sym &optional not-toplevel)
  (byte-compile--check-prefixed-var sym)
  (when (and (not not-toplevel)
             (memq sym byte-compile-lexical-variables))
    (setq byte-compile-lexical-variables
          (delq sym byte-compile-lexical-variables))
    (when (byte-compile-warning-enabled-p 'lexical sym)
      (byte-compile-warn-x sym "Variable `%S' declared after its first use" sym)))
  (push sym byte-compile-bound-variables)
  (push sym byte-compile--seen-defvars))

(defun byte-compile-file-form-defvar (form)
  (byte-compile-defvar form 'toplevel))

(put 'define-abbrev-table 'byte-hunk-handler
     #'byte-compile-file-form-defvar-function)
(put 'defvaralias 'byte-hunk-handler #'byte-compile-file-form-defvar-function)

(defun byte-compile-file-form-defvar-function (form)
  (pcase-let (((or `',name (let name nil)) (nth 1 form)))
    (if name (byte-compile--declare-var name))
    ;; Variable aliases are better declared before the corresponding variable,
    ;; since it makes it more likely that only one of the two vars has a value
    ;; before the `defvaralias' gets executed, which avoids the need to
    ;; merge values.
    (pcase form
      (`(defvaralias ,_ ',newname . ,_)
       (when (memq newname byte-compile-bound-variables)
         (if (byte-compile-warning-enabled-p 'suspicious)
             (byte-compile-warn-x
              newname
              "Alias for `%S' should be declared before its referent"
              newname)))))
    (let ((doc (nth 3 form)))
      (when (stringp doc)
        (setcar (nthcdr 3 form)
                (byte-compile--docstring doc (nth 0 form) name))))
    (byte-compile-keep-pending form)))

(put 'custom-declare-variable 'byte-hunk-handler
     #'byte-compile-file-form-defvar-function)

(put 'custom-declare-face 'byte-hunk-handler
     #'byte-compile--custom-declare-face)
(defun byte-compile--custom-declare-face (form)
  (let ((kind (nth 0 form)) (name (nth 1 form)) (docs (nth 3 form)))
    (when (stringp docs)
      (let ((newdocs (byte-compile--docstring docs kind name)))
        (unless (eq docs newdocs)
          (setq form (byte-compile--list-with-n form 3 newdocs)))))
    (byte-compile-keep-pending form)))

(put 'require 'byte-hunk-handler #'byte-compile-file-form-require)
(defun byte-compile-file-form-require (form)
  (let* ((args (mapcar #'eval (cdr form)))
         ;; The following is for the byte-compile-warn in
         ;; `do-after-load-evaluation' (in subr.el).
         (byte-compile-form-stack (cons (car args) byte-compile-form-stack))
         hist-new prov-cons)
    (apply #'require args)

    ;; Record the functions defined by the require in `byte-compile-new-defuns'.
    (setq hist-new load-history)
    (setq prov-cons (cons 'provide (car args)))
    (while (and hist-new
                (not (member prov-cons (car hist-new))))
      (setq hist-new (cdr hist-new)))
    (when hist-new
      (dolist (x (car hist-new))
        (when (and (consp x)
                   (memq (car x) '(defun t)))
          (push (cdr x) byte-compile-new-defuns)))))
  (byte-compile-keep-pending form 'byte-compile-normal-call))

(put 'progn 'byte-hunk-handler 'byte-compile-file-form-progn)
(put 'prog1 'byte-hunk-handler 'byte-compile-file-form-progn)
(defun byte-compile-file-form-progn (form)
  (mapc #'byte-compile-file-form (cdr form))
  ;; Return nil so the forms are not output twice.
  nil)

(put 'with-no-warnings 'byte-hunk-handler
     'byte-compile-file-form-with-no-warnings)
(defun byte-compile-file-form-with-no-warnings (form)
  ;; cf byte-compile-file-form-progn.
  (let (byte-compile-warnings)
    (mapc 'byte-compile-file-form (cdr form))
    nil))

(put 'internal--with-suppressed-warnings 'byte-hunk-handler
     'byte-compile-file-form-with-suppressed-warnings)
(defun byte-compile-file-form-with-suppressed-warnings (form)
  ;; cf byte-compile-file-form-progn.
  (let ((byte-compile--suppressed-warnings
         (append (cadadr form) byte-compile--suppressed-warnings)))
    (mapc 'byte-compile-file-form (cddr form))
    nil))

;; Automatically evaluate define-obsolete-function-alias etc at top-level.
(put 'make-obsolete 'byte-hunk-handler 'byte-compile-file-form-make-obsolete)
(defun byte-compile-file-form-make-obsolete (form)
  (prog1 (byte-compile-keep-pending form)
    (apply 'make-obsolete
           (mapcar 'eval (cdr form)))))

(defun byte-compile-file-form-defmumble (name macro arglist body rest)
  "Process a `defalias' for NAME.
If MACRO is non-nil, the definition is known to be a macro.
ARGLIST is the list of arguments, if it was recognized or t otherwise.
BODY of the definition, or t if not recognized.
Return non-nil if everything went as planned, or nil to imply that it decided
not to take responsibility for the actual compilation of the code."
  (let* ((this-kind (if macro 'byte-compile-macro-environment
                      'byte-compile-function-environment))
         (that-kind (if macro 'byte-compile-function-environment
                      'byte-compile-macro-environment))
         (this-one (assq name (symbol-value this-kind)))
         (that-one (assq name (symbol-value that-kind)))
         (bare-name (bare-symbol name))
         (byte-compile-current-form name)) ; For warnings.

    (push bare-name byte-compile-new-defuns)
    ;; When a function or macro is defined, add it to the call tree so that
    ;; we can tell when functions are not used.
    (if byte-compile-generate-call-tree
        (or (assq bare-name byte-compile-call-tree)
            (setq byte-compile-call-tree
                  (cons (list bare-name nil nil) byte-compile-call-tree))))

    (if (byte-compile-warning-enabled-p 'redefine name)
        (byte-compile-arglist-warn name arglist macro))

    (if byte-compile-verbose
        (message "Compiling %s... (%s)"
                 (or byte-compile-current-file "") bare-name))
    (cond ((not (or macro (listp body)))
           ;; We do not know positively if the definition is a macro
           ;; or a function, so we shouldn't emit warnings.
           ;; This also silences "multiple definition" warnings for defmethods.
           nil)
          (that-one
           (if (and (byte-compile-warning-enabled-p 'redefine name)
                    ;; Don't warn when compiling the stubs in byte-run...
                    (not (assq bare-name byte-compile-initial-macro-environment)))
               (byte-compile-warn-x
                name
                "`%s' defined multiple times, as both function and macro"
                bare-name))
           (setcdr that-one nil))
          (this-one
           (when (and (byte-compile-warning-enabled-p 'redefine name)
                      ;; Hack: Don't warn when compiling the magic internal
                      ;; byte-compiler macros in byte-run.el...
                      (not (assq bare-name byte-compile-initial-macro-environment)))
             (byte-compile-warn-x
              name
              "%s `%s' defined multiple times in this file"
              (if macro "macro" "function")
              bare-name)))
          ((eq (car-safe (symbol-function bare-name))
               (if macro 'lambda 'macro))
           (when (byte-compile-warning-enabled-p 'redefine bare-name)
             (byte-compile-warn-x
              name
              "%s `%s' being redefined as a %s"
              (if macro "function" "macro")
              bare-name
              (if macro "macro" "function")))
           ;; Shadow existing definition.
           (set this-kind
                (cons (cons bare-name nil)
                      (symbol-value this-kind))))
          )

    (when (and (listp body)
               (stringp (car body))
               (symbolp (car-safe (cdr-safe body)))
               (car-safe (cdr-safe body))
               (stringp (car-safe (cdr-safe (cdr-safe body)))))
      (byte-compile-warn-x
       name "probable `\"' without `\\' in doc string of %s" bare-name))

    (if (not (listp body))
        ;; The precise definition requires evaluation to find out, so it
        ;; will only be known at runtime.
        ;; Tell the caller that we didn't compile it yet.
        nil

      (let ((code (byte-compile-lambda `(lambda ,arglist . ,body))))
        (if this-one
            ;; A definition in b-c-initial-m-e should always take precedence
            ;; during compilation, so don't let it be redefined.  (Bug#8647)
            (or (and macro
                     (assq bare-name byte-compile-initial-macro-environment))
                (setcdr this-one code))
          (set this-kind
               (cons (cons bare-name code)
                     (symbol-value this-kind))))

        (byte-compile-flush-pending)
        (let ((newform `(defalias ',bare-name
                         ,(if macro `'(macro . ,code) code) ,@rest)))
          (when byte-native-compiling
            ;; Don't let `byte-compile-output-file-form' push the form to
            ;; `byte-to-native-top-level-forms' because we want to use
            ;; `make-byte-to-native-func-def' when possible.
            (push
             (if (or macro rest)
                 (make-byte-to-native-top-level
                  :form newform
                  :lexical lexical-binding)
               (make-byte-to-native-func-def :name name
                                             :byte-func code))
             byte-to-native-top-level-forms))
          (let ((byte-native-compiling nil))
           (byte-compile-output-file-form newform)))
        t))))

(defun byte-compile-output-as-comment (exp quoted)
  "Print Lisp object EXP in the output file at point, inside a comment.
Return the file (byte) position it will have.  Leave point after
the inserted text.  If QUOTED is non-nil, print with quoting;
otherwise, print without quoting."
  (with-current-buffer byte-compile--outbuffer
    (let ((position (point)) end)
      ;; Insert EXP, and make it a comment with #@LENGTH.
      (insert " ")
      (if quoted
          (prin1 exp byte-compile--outbuffer)
        (princ exp byte-compile--outbuffer))
      (setq end (point-marker))
      (set-marker-insertion-type end t)

      (goto-char position)
      ;; Quote certain special characters as needed.
      ;; get_doc_string in doc.c does the unquoting.
      (while (search-forward "\^A" end t)
        (replace-match "\^A\^A" t t))
      (goto-char position)
      (while (search-forward "\000" end t)
        (replace-match "\^A0" t t))
      (goto-char position)
      (while (search-forward "\037" end t)
        (replace-match "\^A_" t t))
      (goto-char end)
      (insert "\037")
      (goto-char position)
      (insert "#@" (format "%d" (- (position-bytes end)
                                   (position-bytes position))))

      ;; Save the file position of the object.
      ;; Note we add 1 to skip the space that we inserted before the actual doc
      ;; string, and subtract point-min to convert from an 1-origin Emacs
      ;; position to a file position.
      (prog1
          (- (position-bytes (point)) (point-min) -1)
        (goto-char end)
        (set-marker end nil)))))

(defun byte-compile--reify-function (fun)
  "Return an expression which will evaluate to a function value FUN.
FUN should be an interpreted closure."
  (let* ((args (aref fun 0))
         (body (aref fun 1))
         (env (aref fun 2))
         (docstring (function-documentation fun))
         (iform (interactive-form fun))
         (preamble `(,@(if docstring (list docstring))
                     ,@(if iform (list iform))))
         (renv ()))
    ;; Turn the function's closed vars (if any) into local let bindings.
    (dolist (binding env)
      (cond
       ((consp binding)
        (push `(,(car binding) ',(cdr binding)) renv))
       ((eq binding t))
       (t (push `(defvar ,binding) body))))
    (let ((fun `(lambda ,args ,@preamble ,@body)))
      (if renv `(let ,renv ,fun) fun))))

;;;###autoload
(defun byte-compile (form)
  "If FORM is a symbol, byte-compile its function definition.
If FORM is a lambda or a macro, byte-compile it as a function."
  (displaying-byte-compile-warnings
   (byte-compile-close-variables
    (let* ((lexical-binding lexical-binding)
           (fun (if (symbolp form)
		    (symbol-function form)
		  form))
	   (macro (eq (car-safe fun) 'macro))
           (need-a-value nil))
      (when macro
	(setq need-a-value t)
	(setq fun (cdr fun)))
      (cond
       ;; Up until Emacs-24.1, byte-compile silently did nothing
       ;; when asked to compile something invalid.  So let's tone
       ;; down the complaint from an error to a simple message for
       ;; the known case where signaling an error causes problems.
       ((compiled-function-p fun)
        (message "Function %s is already compiled"
                 (if (symbolp form) form "provided"))
        fun)
       (t
        (when (or (symbolp form) (interpreted-function-p fun))
          ;; `fun' is a function *value*, so try to recover its
          ;; corresponding source code.
          (if (not (interpreted-function-p fun))
              (setq lexical-binding nil)
            (setq lexical-binding (not (null (aref fun 2))))
            (setq fun (byte-compile--reify-function fun)))
          (setq need-a-value t))
        ;; Expand macros.
        (setq fun (byte-compile-preprocess fun))
        (setq fun (byte-compile-top-level fun nil 'eval))
        (when need-a-value
          ;; `byte-compile-top-level' returns an *expression* equivalent to
          ;; the `fun' expression, so we need to evaluate it, tho normally
          ;; this is not needed because the expression is just a constant
          ;; byte-code object, which is self-evaluating.
          (setq fun (eval fun lexical-binding)))
        (if macro (push 'macro fun))
        (if (symbolp form) (fset form fun))
        fun))))))

(defun byte-compile-sexp (sexp)
  "Compile and return SEXP."
  (displaying-byte-compile-warnings
   (byte-compile-close-variables
    (byte-compile-top-level (byte-compile-preprocess sexp)))))

(defun byte-compile-check-lambda-list (list)
  "Check lambda-list LIST for errors."
  (let (vars)
    (while list
      (let ((arg (car list)))
	(cond ((or (not (symbolp arg))
		   (macroexp--const-symbol-p arg t))
	       (error "Invalid lambda variable %s" arg))
	      ((eq arg '&rest)
               (unless (cdr list)
                 (error "&rest without variable name"))
	       (when (cddr list)
		 (error "Garbage following &rest VAR in lambda-list"))
               (when (memq (cadr list) '(&optional &rest))
                 (error "%s following &rest in lambda-list" (cadr list))))
	      ((eq arg '&optional)
               (when (memq '&optional (cdr list))
                 (error "Duplicate &optional")))
	      ((and (memq arg vars)
	            ;; Allow repetitions for unused args.
	            (not (string-match "\\`_" (symbol-name arg))))
	       (byte-compile-warn-x
                arg "repeated variable %s in lambda-list" arg))
	      (t
	       (when (and lexical-binding
	                  (cconv--not-lexical-var-p
	                   arg byte-compile-bound-variables)
	                  (byte-compile-warning-enabled-p 'lexical arg))
	         (byte-compile-warn-x
	          arg
	          "Lexical argument shadows the dynamic variable %S"
	          arg))
	       (push arg vars))))
      (setq list (cdr list)))))


(defun byte-compile-arglist-vars (arglist)
  "Return a list of the variables in the lambda argument list ARGLIST."
  (remq '&rest (remq '&optional arglist)))

(defun byte-compile-make-lambda-lexenv (args)
  "Return a new lexical environment for a lambda expression FORM."
  (let* ((lexenv nil)
         (stackpos 0))
    ;; Add entries for each argument.
    (dolist (arg args)
      (push (cons arg stackpos) lexenv)
      (setq stackpos (1+ stackpos)))
    ;; Return the new lexical environment.
    lexenv))

(defun byte-compile-make-args-desc (arglist)
  (let ((mandatory 0)
        nonrest (rest 0))
    (while (and arglist (not (memq (car arglist) '(&optional &rest))))
      (setq mandatory (1+ mandatory))
      (setq arglist (cdr arglist)))
    (setq nonrest mandatory)
    (when (eq (car arglist) '&optional)
      (setq arglist (cdr arglist))
      (while (and arglist (not (eq (car arglist) '&rest)))
        (setq nonrest (1+ nonrest))
        (setq arglist (cdr arglist))))
    (when arglist
      (setq rest 1))
    (if (> mandatory 127)
        (byte-compile-report-error "Too many (>127) mandatory arguments")
      (logior mandatory
              (ash nonrest 8)
              (ash rest 7)))))

(defun byte-compile--warn-lexical-dynamic (var context)
  (when (byte-compile-warning-enabled-p 'lexical-dynamic var)
    (byte-compile-warn-x
     var
     "`%s' lexically bound in %s here but declared dynamic in: %s"
     var context
     (mapconcat #'identity
                (mapcan (lambda (v) (and (eq var (car v))
                                         (list (cdr v))))
                        byte-compile--known-dynamic-vars)
                ", "))))

(defun byte-compile-lambda (fun &optional reserved-csts)
  "Byte-compile a lambda-expression and return a valid function.
The value is usually a compiled function but may be the original
lambda-expression."
  (unless (eq 'lambda (car-safe fun))
    (error "Not a lambda list: %S" fun))
  (byte-compile-check-lambda-list (nth 1 fun))
  (let* ((arglist (nth 1 fun))
         (bare-arglist (byte-run-strip-symbol-positions arglist)) ; for compile-defun.
         (arglistvars (byte-run-strip-symbol-positions
                       (byte-compile-arglist-vars arglist)))
	 (byte-compile-bound-variables
	  (append (if (not lexical-binding) arglistvars)
                  byte-compile-bound-variables))
	 (body (cdr (cdr fun)))
         ;; Treat a final string literal as a value, not a doc string.
	 (doc (if (and (cdr body) (stringp (car body)))
                  (prog1 (car body)
                    ;; Discard the doc string from the body.
                    (setq body (cdr body)))))
	 (int (assq 'interactive body))
         command-modes)
    (when lexical-binding
      (when arglist
        ;; byte-compile-make-args-desc lost the args's names,
        ;; so preserve them in the docstring.
	(setq doc (help-add-fundoc-usage doc bare-arglist)))
      (dolist (var arglistvars)
        (when (assq var byte-compile--known-dynamic-vars)
          (byte-compile--warn-lexical-dynamic var 'lambda))))
    (when (stringp doc)
      (setq doc (byte-compile--docstring doc "" nil 'is-a-value)))
    ;; Process the interactive spec.
    (when int
      ;; Skip (interactive) if it is in front (the most usual location).
      (if (eq int (car body))
	  (setq body (cdr body)))
      (cond ((consp (cdr int))       ; There is an `interactive' spec.
             ;; Check that the bit after the `interactive' spec is
             ;; just a list of symbols (i.e., modes).
	     (unless (seq-every-p #'symbolp (cdr (cdr int)))
	       (byte-compile-warn-x
                int "malformed `interactive' specification: %s" int))
             (setq command-modes (cdr (cdr int)))
	     ;; If the interactive spec is a call to `list', don't
	     ;; compile it, because `call-interactively' looks at the
	     ;; args of `list'.  Actually, compile it to get warnings,
	     ;; but don't use the result.
	     (let* ((form (nth 1 int))
                    (newform (byte-compile-top-level form)))
	       (while (memq (car-safe form) '(let let* progn save-excursion))
		 (while (consp (cdr form))
		   (setq form (cdr form)))
		 (setq form (car form)))
	       (if (or (not (eq (car-safe form) 'list))
                       ;; For code using lexical-binding, form is not
                       ;; valid lisp, but rather an intermediate form
                       ;; which may include "calls" to
                       ;; internal-make-closure (Bug#29988).
                       lexical-binding)
                   (setq int `(,(car int) ,newform))
                 (setq int (byte-run-strip-symbol-positions int))))) ; for compile-defun.
            ((cdr int)                  ; Invalid (interactive . something).
	     (byte-compile-warn-x int "malformed interactive spec: %s"
				  int))))
    ;; Process the body.
    (let ((compiled
           (byte-compile-top-level (cons 'progn body) nil 'lambda
                                   ;; If doing lexical binding, push a new
                                   ;; lexical environment containing just the
                                   ;; args (since lambda expressions should be
                                   ;; closed by now).
                                   (and lexical-binding
                                        (byte-compile-make-lambda-lexenv
                                         arglistvars))
                                   reserved-csts)))
      ;; Build the actual byte-coded function.
      (cl-assert (eq 'byte-code (car-safe compiled)))
      (let ((out
	     (apply #'make-byte-code
		    (if lexical-binding
			(byte-compile-make-args-desc arglist)
		      bare-arglist)
                    ;; code string, deduplicated
                    (let* ((code (cadr compiled))
                           (prev (member code bytecomp--code-strings)))
                      (if prev
                          (car prev)
                        (push code bytecomp--code-strings)
                        code))
		    (append
		     ;; constants-vector and stack depth
		     (drop 2 compiled)
		     ;; optionally, the doc string.
		     (when (or doc int) (list doc))
		     ;; optionally, the interactive spec (and the modes the
		     ;; command applies to).
		     (cond
		      ;; We have some command modes, so use the vector form.
		      (command-modes
                       (list (vector (nth 1 int) command-modes)))
		      ;; No command modes, use the simple form with just the
		      ;; interactive spec.
		      (int
                       (list (nth 1 int))))))))
	(when byte-native-compiling
          (setf (byte-to-native-lambda-byte-func
                 (gethash (cadr compiled)
                          byte-to-native-lambdas-h))
                out))
	out))))

(defvar byte-compile-reserved-constants 0)

(defun byte-compile-constants-vector ()
  ;; Builds the constants-vector from the current variables and constants.
  ;;   This modifies the constants from (const . nil) to (const . offset).
  ;; To keep the byte-codes to look up the vector as short as possible:
  ;;   First 6 elements are vars, as there are one-byte varref codes for those.
  ;;   Next up to byte-constant-limit are constants, still with one-byte codes.
  ;;   Next variables again, to get 2-byte codes for variable lookup.
  ;;   The rest of the constants and variables need 3-byte byte-codes.
  (let* ((i (1- byte-compile-reserved-constants))
	 (rest (nreverse byte-compile-variables)) ; nreverse because the first
	 (other (nreverse byte-compile-constants)) ; vars often are used most.
	 ret tmp
	 (limits '(5			; Use the 1-byte varref codes,
		   63  ; 1-constlim	;  1-byte byte-constant codes,
		   255			;  2-byte varref codes,
		   65535		;  3-byte codes for the rest.
                   65535))              ;  twice since we step when we swap.
	 limit)
    (while (or rest other)
      (setq limit (car limits))
      (while (and rest (< i limit))
	(cond
	 ((numberp (car rest))
	  (cl-assert (< (car rest) byte-compile-reserved-constants)))
	 ((setq tmp (assq (car (car rest)) ret))
	  (setcdr (car rest) (cdr tmp)))
	 (t
	  (setcdr (car rest) (setq i (1+ i)))
	  (setq ret (cons (car rest) ret))))
	(setq rest (cdr rest)))
      (setq limits (cdr limits)         ;Step
	    rest (prog1 other           ;&Swap.
		   (setq other rest))))
    (apply 'vector (nreverse (mapcar 'car ret)))))

;; Given an expression FORM, compile it and return an equivalent byte-code
;; expression (a call to the function byte-code).
(defun byte-compile-top-level (form &optional for-effect output-type
                                    lexenv reserved-csts)
  ;; OUTPUT-TYPE advises about how form is expected to be used:
  ;;	'eval or nil	-> a single form,
  ;;	'lambda		-> body of a lambda,
  ;;	'file		-> used at file-level.
  (let ((byte-compile--for-effect for-effect)
        (byte-compile-constants nil)
	(byte-compile-variables nil)
	(byte-compile-tag-number 0)
	(byte-compile-depth 0)
	(byte-compile-maxdepth 0)
        (byte-compile--lexical-environment lexenv)
        (byte-compile-reserved-constants (or reserved-csts 0))
	(byte-compile-output nil)
        (byte-compile-jump-tables nil))
    (if (memq byte-optimize '(t source))
	(setq form (byte-optimize-one-form form byte-compile--for-effect)))
    (while (and (eq (car-safe form) 'progn) (null (cdr (cdr form))))
      (setq form (nth 1 form)))
    ;; Set up things for a lexically-bound function.
    (when (and lexical-binding (eq output-type 'lambda))
      ;; See how many arguments there are, and set the current stack depth
      ;; accordingly.
      (setq byte-compile-depth (length byte-compile--lexical-environment))
      ;; If there are args, output a tag to record the initial
      ;; stack-depth for the optimizer.
      (when (> byte-compile-depth 0)
        (byte-compile-out-tag (byte-compile-make-tag))))
    ;; Now compile FORM
    (byte-compile-form form byte-compile--for-effect)
    (byte-compile-out-toplevel byte-compile--for-effect output-type)))

(defun byte-compile-out-toplevel (&optional for-effect output-type)
  ;; OUTPUT-TYPE can be like that of `byte-compile-top-level'.
  (if for-effect
      ;; The stack is empty. Push a value to be returned from (byte-code ..).
      (if (eq (car (car byte-compile-output)) 'byte-discard)
	  (setq byte-compile-output (cdr byte-compile-output))
	(byte-compile-push-constant
	 ;; Push any constant - preferably one which already is used, and
	 ;; a number or symbol - ie not some big sequence.  The return value
	 ;; isn't returned, but it would be a shame if some textually large
	 ;; constant was not optimized away because we chose to return it.
	 (and (not (assq nil byte-compile-constants)) ; Nil is often there.
	      (let ((tmp (reverse byte-compile-constants)))
		(while (and tmp (not (or (symbolp (caar tmp))
					 (numberp (caar tmp)))))
		  (setq tmp (cdr tmp)))
		(caar tmp))))))
  (byte-compile-out 'byte-return 0)
  (setq byte-compile-output (nreverse byte-compile-output))
  (if (memq byte-optimize '(t byte))
      (setq byte-compile-output
	    (byte-optimize-lapcode byte-compile-output)))

  ;; Decompile trivial functions:
  ;; only constants and variables, or a single funcall except in lambdas.
  ;; Except for Lisp_Compiled objects, forms like (foo "hi")
  ;; are still quicker than (byte-code "..." [foo "hi"] 2).
  ;; Note that even (quote foo) must be parsed just as any subr by the
  ;; interpreter, so quote should be compiled into byte-code in some contexts.
  ;; What to leave uncompiled:
  ;;	lambda	-> never.  The compiled form is always faster.
  ;;	eval	-> atom, quote or (function atom atom atom)
  ;;	file	-> as progn, but takes both quotes and atoms, and longer forms.
  (let (rest
	(maycall (not (eq output-type 'lambda))) ; t if we may make a funcall.
	tmp body)
    (cond
     ;; #### This should be split out into byte-compile-nontrivial-function-p.
     ((or (eq output-type 'lambda)
	  (nthcdr (if (eq output-type 'file) 50 8) byte-compile-output)
	  (assq 'TAG byte-compile-output) ; Not necessary, but speeds up a bit.
	  (not (setq tmp (assq 'byte-return byte-compile-output)))
	  (progn
	    (setq rest (nreverse
			(cdr (memq tmp (reverse byte-compile-output)))))
	    (while
                (cond
                 ((memq (car (car rest)) '(byte-varref byte-constant))
                  (setq tmp (car (cdr (car rest))))
                  (if (if (eq (car (car rest)) 'byte-constant)
                          (or (consp tmp)
                              (and (symbolp tmp)
                                   (not (macroexp--const-symbol-p tmp)))))
                      (if maycall
                          (setq body (cons (list 'quote tmp) body)))
                    (setq body (cons tmp body))))
                 ((and maycall
                       ;; Allow a funcall if at most one atom follows it.
                       (null (nthcdr 3 rest))
                       (setq tmp (get (car (car rest)) 'byte-opcode-invert))
                       (or (null (cdr rest))
                           (and (eq output-type 'file)
                                (cdr (cdr rest))
                                (eql (length body) (cdr (car rest))) ;bug#34757
                                (eq (car (nth 1 rest)) 'byte-discard)
                                (progn (setq rest (cdr rest)) t))))
                  (setq maycall nil)	; Only allow one real function call.
                  (setq body (nreverse body))
                  (setq body (list
                              (if (and (eq tmp 'funcall)
                                       (eq (car-safe (car body)) 'quote)
				       (symbolp (nth 1 (car body))))
                                  (cons (nth 1 (car body)) (cdr body))
                                (cons tmp body))))
                  (or (eq output-type 'file)
                      (not (any #'consp (cdr (car body)))))))
	      (setq rest (cdr rest)))
	    rest))
      (let ((byte-compile-vector (byte-compile-constants-vector)))
	(list 'byte-code (byte-compile-lapcode byte-compile-output)
	      byte-compile-vector byte-compile-maxdepth)))
     ;; it's a trivial function
     ((cdr body) (cons 'progn (nreverse body)))
     ((car body)))))

;; Given BODY, compile it and return a new body.
(defun byte-compile-top-level-body (body &optional for-effect)
  (setq body
	(byte-compile-top-level (cons 'progn body) for-effect t))
  (cond ((eq (car-safe body) 'progn)
	 (cdr body))
	(body
	 (list body))))

;; Special macro-expander used during byte-compilation.
(defun byte-compile-macroexpand-declare-function (fn file &rest args)
  (declare (advertised-calling-convention
	    (fn file &optional arglist fileonly) nil))
  (let ((gotargs (and (consp args) (listp (car args))))
	(unresolved (assq fn byte-compile-unresolved-functions)))
    (when unresolved	      ; function was called before declaration
      (if (and gotargs (byte-compile-warning-enabled-p 'callargs))
	  (byte-compile-arglist-warn fn (car args) nil)
	(setq byte-compile-unresolved-functions
	      (delq unresolved byte-compile-unresolved-functions))))
    (push (cons fn (if gotargs
		       (list 'declared (car args))
		     t))                     ; Arglist not specified.
	  byte-compile-function-environment))
  ;; We are stating that it _will_ be defined at runtime.
  (setq byte-compile-noruntime-functions
        (delq fn byte-compile-noruntime-functions))
  ;; Delegate the rest to the normal macro definition.
  (let ((print-symbols-bare t))         ; Possibly redundant binding.
    (macroexpand `(declare-function ,fn ,file ,@args))))


;; This is the recursive entry point for compiling each subform of an
;; expression.
;; If for-effect is non-nil, byte-compile-form will output a byte-discard
;; before terminating (ie no value will be left on the stack).
;; A byte-compile handler may, when byte-compile--for-effect is non-nil, choose
;; output code which does not leave a value on the stack, and then set
;; byte-compile--for-effect to nil (to prevent byte-compile-form from
;; outputting the byte-discard).
;; If a handler wants to call another handler, it should do so via
;; byte-compile-form, or take extreme care to handle byte-compile--for-effect
;; correctly.  (Use byte-compile-form-do-effect to reset the
;; byte-compile--for-effect flag too.)
;;
(defun byte-compile-form (form &optional for-effect)
  (let ((byte-compile--for-effect for-effect))
    (macroexp--with-extended-form-stack form
      (cond
       ((not (consp form))
        (cond ((or (not (symbolp form)) (macroexp--const-symbol-p form))
               (byte-compile-constant form))
              ((and byte-compile--for-effect byte-compile-delete-errors)
               (setq byte-compile--for-effect nil))
              (t (byte-compile-variable-ref form))))
       ((symbolp (car form))
        (let* ((fn (car form))
               (handler (get fn 'byte-compile))
	       (interactive-only
	        (or (function-get fn 'interactive-only)
		    (memq fn byte-compile-interactive-only-functions))))
          (when (memq fn '(set symbol-value run-hooks ;; add-to-list
                               add-hook remove-hook run-hook-with-args
                               run-hook-with-args-until-success
                               run-hook-with-args-until-failure))
            (pcase (cdr form)
              (`(',var . ,_)
               (when (and (memq var byte-compile-lexical-variables)
                          (byte-compile-warning-enabled-p 'lexical var))
                 (byte-compile-warn
                  (format-message "%s cannot use lexical var `%s'" fn var))))))
          ;; Warn about using obsolete hooks.
          (if (memq fn '(add-hook remove-hook))
              (let ((hook (car-safe (cdr form))))
                (if (eq (car-safe hook) 'quote)
                    (byte-compile-check-variable (cadr hook) nil))))
          (when (and (macroexp--const-symbol-p fn)
                     (byte-compile-warning-enabled-p 'suspicious))
            (byte-compile-warn-x fn "`%s' called as a function" fn))
	  (when (and interactive-only
		     (byte-compile-warning-enabled-p 'interactive-only fn))
	    (byte-compile-warn-x fn "`%s' is for interactive use only%s"
			         fn
			         (cond ((stringp interactive-only)
				        (format "; %s"
					        (substitute-command-keys
					         interactive-only)))
				       ((and (symbolp interactive-only)
					     (not (eq interactive-only t)))
				        (format-message "; use `%s' instead."
                                                        interactive-only))
				       (t "."))))
          (let ((mutargs (function-get (car form) 'mutates-arguments)))
            (when mutargs
              (dolist (idx (if (symbolp mutargs)
                               (funcall mutargs form)
                             mutargs))
                (let ((arg (nth idx form)))
                  (when (and (or (and (eq (car-safe arg) 'quote)
                                      (consp (nth 1 arg)))
                                 (arrayp arg))
                             (byte-compile-warning-enabled-p
                              'mutate-constant (car form)))
                    (byte-compile-warn-x form "`%s' on constant %s (arg %d)"
                                         (car form)
                                         (if (consp arg) "list" (type-of arg))
                                         idx))))))

          (let ((funargs (function-get (car form) 'funarg-positions)))
            (dolist (funarg funargs)
              (let ((arg (if (numberp funarg)
                             (nth funarg form)
                           (cadr (memq funarg form)))))
                (when (and (eq 'quote (car-safe arg))
                           (eq 'lambda (car-safe (cadr arg))))
                  (byte-compile-warn-x
                   arg "(lambda %s ...) quoted with %s rather than with #%s"
                   (or (nth 1 (cadr arg)) "()")
                   "'" "'")))))           ; avoid styled quotes

          (if (eq (car-safe (symbol-function (car form))) 'macro)
              (byte-compile-report-error
               (format-message "`%s' defined after use in %S (missing `require' of a library file?)"
                               (car form) form)))

          (when byte-compile--for-effect
            (let ((sef (function-get (car form) 'side-effect-free)))
              (cond
               ((and sef (or (eq sef 'error-free)
                             byte-compile-delete-errors))
                ;; This transform is normally done in the Lisp optimizer,
                ;; so maybe we don't need to bother about it here?
                (setq form (cons 'progn (cdr form)))
                (setq handler #'byte-compile-progn))
               ((and (or sef (function-get (car form) 'important-return-value))
                     ;; Don't warn for arguments to `ignore'.
                     (not (eq byte-compile--for-effect 'for-effect-no-warn))
                     (bytecomp--actually-important-return-value-p form)
                     (byte-compile-warning-enabled-p
                      'ignored-return-value (car form)))
                (byte-compile-warn-x
                 (car form)
                 "value from call to `%s' is unused%s"
                 (car form)
                 (cond ((eq (car form) 'mapcar)
                        "; use `mapc' or `dolist' instead")
                       (t "")))))))

          (if (and handler
                   ;; Make sure that function exists.
                   (and (functionp handler)
                        ;; Ignore obsolete byte-compile function used by former
                        ;; CL code to handle compiler macros (we do it
                        ;; differently now).
                        (not (eq handler 'cl-byte-compile-compiler-macro))))
              (funcall handler form)
            (byte-compile-normal-call form))))
       ((and (byte-code-function-p (car form))
             (memq byte-optimize '(t lap)))
        (byte-compile-unfold-bcf form))
       ((byte-compile-normal-call form)))
      (if byte-compile--for-effect
          (byte-compile-discard)))))

(defun bytecomp--sort-call-in-place-p (form)
  (or (= (length form) 3)                  ; old-style
      (plist-get (cddr form) :in-place)))  ; new-style

(defun bytecomp--actually-important-return-value-p (form)
  "Whether FORM is really a call with a return value that should not go unused.
This assumes the function has the `important-return-value' property."
  (cond ((eq (car form) 'sort)
         (not (bytecomp--sort-call-in-place-p form)))
        (t t)))

(let ((important-return-value-fns
       '(
         ;; These functions are side-effect-free except for the
         ;; behavior of functions passed as argument.
         mapcar mapcan mapconcat
         assoc plist-get plist-member

         ;; It's safe to ignore the value of `nreverse'
         ;; when used on arrays, but most calls pass lists.
         nreverse

         sort      ; special handling (non-destructive calls only)

         match-data

         ;; Warning about these functions causes some false positives that are
         ;; laborious to eliminate; see bug#61730.
         ;;delq delete
         ;;nconc plist-put
         )))
  (dolist (fn important-return-value-fns)
    (put fn 'important-return-value t)))

(defun bytecomp--mutargs-nconc (form)
  ;; For `nconc', all arguments but the last are mutated.
  (number-sequence 1 (- (length form) 2)))

(defun bytecomp--mutargs-sort (form)
  ;; For `sort', the first argument is mutated if the call is in-place.
  (and (bytecomp--sort-call-in-place-p form) '(1)))

(let ((mutating-fns
       ;; FIXME: Should there be a function declaration for this?
       ;;
       ;; (FUNC . ARGS) means that FUNC mutates arguments whose indices are
       ;; in the list ARGS, starting at 1.  ARGS can also be a function
       ;; taking the function call form as argument and returning the
       ;; list of indices.
       '(
         (setcar 1) (setcdr 1) (aset 1)
         (nreverse 1)
         (nconc . bytecomp--mutargs-nconc)
         (nbutlast 1) (ntake 2)
         (sort  . bytecomp--mutargs-sort)
         (delq 2) (delete 2)
         (delete-dups 1) (delete-consecutive-dups 1)
         (plist-put 1)
         (assoc-delete-all 2) (assq-delete-all 2) (rassq-delete-all 2)
         (fillarray 1)
         (store-substring 1)
         (clear-string 1)

         (add-text-properties 4) (put-text-property 5) (set-text-properties 4)
         (remove-text-properties 4) (remove-list-of-text-properties 4)
         (alter-text-property 5)
         (add-face-text-property 5) (add-display-text-property 5)

         (cl-delete 2) (cl-delete-if 2) (cl-delete-if-not 2)
         (cl-delete-duplicates 1)
         (cl-nsubst 3) (cl-nsubst-if 3) (cl-nsubst-if-not 3)
         (cl-nsubstitute 3) (cl-nsubstitute-if 3) (cl-nsubstitute-if-not 3)
         (cl-nsublis 2)
         (cl-nunion 1 2) (cl-nintersection 1) (cl-nset-difference 1)
         (cl-nset-exclusive-or 1 2)
         (cl-nreconc 1)
         (cl-sort 1) (cl-stable-sort 1) (cl-merge 2 3)
         (cl-fill 1) (cl-replace 1)
         )))
  (dolist (entry mutating-fns)
    (put (car entry) 'mutates-arguments (cdr entry))))

;; Record which arguments expect functions, so we can warn when those
;; are accidentally quoted with ' rather than with #'
;; The value of the `funarg-positions' property is a list of function
;; argument positions, starting with 1, and keywords.
(dolist (f '( funcall apply mapcar mapatoms mapconcat mapc maphash
              mapcan map-char-table map-keymap map-keymap-internal
              functionp
              seq-do seq-do-indexed seq-sort seq-group-by
              seq-find seq-count
              seq-filter seq-reduce seq-remove seq-keep
              seq-map seq-map-indexed seq-mapn seq-mapcat
              seq-drop-while seq-take-while
              seq-some seq-every-p
              cl-every cl-some
              cl-mapcar cl-mapcan cl-mapcon cl-mapc cl-mapl cl-maplist
              ))
  (put f 'funarg-positions '(1)))
(dolist (f '( defalias fset
              replace-regexp-in-string
              add-hook remove-hook advice-remove advice--remove-function
              global-set-key local-set-key keymap-global-set keymap-local-set
              set-process-filter set-process-sentinel
              ))
  (put f 'funarg-positions '(2)))
(dolist (f '( assoc assoc-default assoc-delete-all
              plist-get plist-member
              advice-add define-key keymap-set
              run-at-time run-with-idle-timer run-with-timer
              seq-contains seq-contains-p seq-set-equal-p
              seq-position seq-positions seq-uniq
              seq-union seq-intersection seq-difference))
  (put f 'funarg-positions '(3)))
(dolist (f '( cl-find cl-member cl-assoc cl-rassoc cl-position cl-count
              cl-remove cl-delete
              cl-subst cl-nsubst
              cl-substitute cl-nsubstitute
              cl-remove-duplicates cl-delete-duplicates
              cl-union cl-nunion cl-intersection cl-nintersection
              cl-set-difference cl-nset-difference
              cl-set-exclusive-or cl-nset-exclusive-or
              cl-nsublis
              cl-search
              ))
  (put f 'funarg-positions '(:test :test-not :key)))
(dolist (f '( cl-find-if cl-find-if-not cl-member-if cl-member-if-not
              cl-assoc-if cl-assoc-if-not cl-rassoc-if cl-rassoc-if-not
              cl-position-if cl-position-if-not cl-count-if cl-count-if-not
              cl-remove-if cl-remove-if-not cl-delete-if cl-delete-if-not
              cl-reduce cl-adjoin
              cl-subsetp
              ))
  (put f 'funarg-positions '(1 :key)))
(dolist (f '( cl-subst-if cl-subst-if-not cl-nsubst-if cl-nsubst-if-not
              cl-substitute-if cl-substitute-if-not
              cl-nsubstitute-if cl-nsubstitute-if-not
              cl-sort cl-stable-sort
              ))
  (put f 'funarg-positions '(2 :key)))
(dolist (fa '((plist-put 4) (alist-get 5) (add-to-list 5)
              (cl-merge 4 :key)
              (custom-declare-variable :set :get :initialize :safe)
              (define-widget :convert-widget :value-to-internal
                             :value-to-external :match)
              (make-process :filter :sentinel)
              (make-network-process :filter :sentinel)
              (all-completions 2 3) (try-completion 2 3) (test-completion 2 3)
              (completing-read 2 3)
              (sort 2 :key :lessp)
              (seq-sort-by 1 2)
              ))
  (put (car fa) 'funarg-positions (cdr fa)))

(defun byte-compile-normal-call (form)
  (when (and (symbolp (car form))
             (byte-compile-warning-enabled-p 'callargs (car form)))
    (byte-compile-callargs-warn form))
  (if byte-compile-generate-call-tree
      (byte-compile-annotate-call-tree form))

  (byte-compile-push-constant (car form))
  (mapc 'byte-compile-form (cdr form))	; wasteful, but faster.
  (byte-compile-out 'byte-call (length (cdr form))))


;; Splice the given lap code into the current instruction stream.
;; If it has any labels in it, you're responsible for making sure there
;; are no collisions, and that byte-compile-tag-number is reasonable
;; after this is spliced in.  The provided list is destroyed.
(defun byte-compile-inline-lapcode (lap end-depth)
  ;; "Replay" the operations: we used to just do
  ;; (setq byte-compile-output (nconc (nreverse lap) byte-compile-output))
  ;; but that fails to update byte-compile-depth, so we had to assume
  ;; that `lap' ends up adding exactly 1 element to the stack.  This
  ;; happens to be true for byte-code generated by bytecomp.el without
  ;; lexical-binding, but it's not true in general, and it's not true for
  ;; code output by bytecomp.el with lexical-binding.
  ;; We also restore the value of `byte-compile-depth' and remove TAG depths
  ;; accordingly when inlining lapcode containing lap-code, exactly as
  ;; documented in `byte-compile-cond-jump-table'.
  (let ((endtag (byte-compile-make-tag))
        last-jump-tag ;; last TAG we have jumped to
        last-depth ;; last value of `byte-compile-depth'
        last-constant ;; value of the last constant encountered
        last-switch ;; whether the last op encountered was byte-switch
        switch-tags ;; a list of tags that byte-switch could jump to
        ;; a list of tags byte-switch will jump to, if the value doesn't
        ;; match any entry in the hash table
        switch-default-tags)
    (dolist (op lap)
      (cond
       ((eq (car op) 'TAG)
        (when (or (member op switch-tags) (member op switch-default-tags))
          ;; This TAG is used in a jump table, this means the last goto
          ;; was to a done/default TAG, and thus it's cddr should be set to nil.
          (when last-jump-tag
            (setcdr (cdr last-jump-tag) nil))
          ;; Also, restore the value of `byte-compile-depth' to what it was
          ;; before the last goto.
          (setq byte-compile-depth last-depth
                last-jump-tag nil))
        (byte-compile-out-tag op))
       ((memq (car op) byte-goto-ops)
        (setq last-depth byte-compile-depth
              last-jump-tag (cdr op))
        (byte-compile-goto (car op) (cdr op))
        (when last-switch
          ;; The last op was byte-switch, this goto jumps to a "default" TAG
          ;; (when no value in the jump table is satisfied).
          (push (cdr op) switch-default-tags)
          (setcdr (cdr (cdr op)) nil)
          (setq byte-compile-depth last-depth
                last-switch nil)))
       ((eq (car op) 'byte-return)
        (byte-compile-discard (- byte-compile-depth end-depth) t)
        (byte-compile-goto 'byte-goto endtag))
       (t
        (when (eq (car op) 'byte-switch)
          ;; The last constant is a jump table.
          (push last-constant byte-compile-jump-tables)
          (setq last-switch t)
          ;; Push all TAGs in the jump to switch-tags.
          (maphash #'(lambda (_k tag)
                       (push tag switch-tags))
                   last-constant))
        (setq last-constant (and (eq (car op) 'byte-constant) (cadr op)))
        (setq last-depth byte-compile-depth)
        (byte-compile-out (car op) (cdr op)))))
    (byte-compile-out-tag endtag)))

(defun byte-compile-unfold-bcf (form)
  "Inline call to byte-code function."
  (let* ((byte-compile-bound-variables byte-compile-bound-variables)
         (fun (car form))
         (fargs (aref fun 0))
         (start-depth byte-compile-depth)
         (fmax2 (if (numberp fargs) (ash fargs -7)))     ;2*max+rest.
         ;; (fmin (if (numberp fargs) (logand fargs 127)))
         (alen (length (cdr form)))
         (dynbinds ())
         lap)
    (setq lap (byte-decompile-bytecode-1 (aref fun 1) (aref fun 2) t))
    ;; optimized switch bytecode makes it impossible to guess the correct
    ;; `byte-compile-depth', which can result in incorrect inlined code.
    ;; therefore, we do not inline code that uses the `byte-switch'
    ;; instruction.
    (if (assq 'byte-switch lap)
        (byte-compile-normal-call form)
      (mapc 'byte-compile-form (cdr form))
      (unless fmax2
        ;; Old-style byte-code.
        (cl-assert (listp fargs))
        (while fargs
          (pcase (car fargs)
            ('&optional (setq fargs (cdr fargs)))
            ('&rest (setq fmax2 (+ (* 2 (length dynbinds)) 1))
                    (push (cadr fargs) dynbinds)
                    (setq fargs nil))
            (_ (push (pop fargs) dynbinds))))
        (unless fmax2 (setq fmax2 (* 2 (length dynbinds)))))
      (cond
       ((<= (+ alen alen) fmax2)
        ;; Add missing &optional (or &rest) arguments.
        (dotimes (_ (- (/ (1+ fmax2) 2) alen))
          (byte-compile-push-constant nil)))
       ((evenp fmax2)
        (byte-compile-report-error
         (format "Too many arguments for inlined function %S" form))
        (byte-compile-discard (- alen (/ fmax2 2))))
       (t
        ;; Turn &rest args into a list.
        (let ((n (- alen (/ (1- fmax2) 2))))
          (cl-assert (> n 0) nil "problem: fmax2=%S alen=%S n=%S" fmax2 alen n)
          (if (< n 5)
              (byte-compile-out
               (aref [byte-list1 byte-list2 byte-list3 byte-list4] (1- n))
               0)
            (byte-compile-out 'byte-listN n)))))
      (mapc #'byte-compile-dynamic-variable-bind dynbinds)
      (byte-compile-inline-lapcode lap (1+ start-depth))
      ;; Unbind dynamic variables.
      (when dynbinds
        (byte-compile-out 'byte-unbind (length dynbinds)))
      (cl-assert (eq byte-compile-depth (1+ start-depth))
                 nil "Wrong depth start=%s end=%s" start-depth byte-compile-depth))))

(defun byte-compile-check-variable (var access-type)
  "Do various error checks before a use of the variable VAR."
  (cond ((or (not (symbolp var)) (macroexp--const-symbol-p var))
	 (when (byte-compile-warning-enabled-p 'constants
                                               (and (symbolp var) var))
	   (byte-compile-warn-x
            var
            (if (eq access-type 'let-bind)
		"attempt to let-bind %s `%s'"
	      "variable reference to %s `%s'")
	    (if (symbolp var) "constant" "nonvariable")
	    var)))
	((let ((od (get var 'byte-obsolete-variable)))
           (and od
                (not (memq var byte-compile-not-obsolete-vars))
                (not (memq var byte-compile-global-not-obsolete-vars))
                (not (memq var byte-compile-lexical-variables))
                (pcase (nth 1 od)
                  ('set (not (eq access-type 'reference)))
                  ('get (eq access-type 'reference))
                  (_ t))))
	 (byte-compile-warn-obsolete var "variable"))))

(defsubst byte-compile-dynamic-variable-op (base-op var)
  (let ((tmp (assq var byte-compile-variables)))
    (unless tmp
      (setq tmp (list var))
      (push tmp byte-compile-variables))
    (byte-compile-out base-op tmp)))

(defun byte-compile-dynamic-variable-bind (var)
  "Generate code to bind the dynamic variable VAR to the top-of-stack value."
  (byte-compile-check-variable var 'let-bind)
  (push var byte-compile-bound-variables)
  (byte-compile-dynamic-variable-op 'byte-varbind var))

(defun byte-compile-free-vars-warn (arg var &optional assignment)
  "Warn if symbol VAR refers to a free variable.
VAR must not be lexically bound.
ARG is a position argument, used by `byte-compile-warn-x'.
If optional argument ASSIGNMENT is non-nil, this is treated as an
assignment (i.e. `setq')."
  (unless (or (boundp var)
              (memq var byte-compile-bound-variables)
              (memq var (if assignment
                            byte-compile-free-assignments
                          byte-compile-free-references))
              (not (byte-compile-warning-enabled-p 'free-vars var)))
    (let* ((varname (prin1-to-string var))
           (desc (if assignment "assignment" "reference"))
           (suggestions (help-uni-confusable-suggestions varname)))
      (byte-compile-warn-x arg "%s to free variable `%s'%s"
                           desc var
                           (if suggestions (concat "\n  " suggestions) "")))
    (push var (if assignment
                  byte-compile-free-assignments
                byte-compile-free-references))))

(defun byte-compile-variable-ref (var)
  "Generate code to push the value of the variable VAR on the stack."
  (byte-compile-check-variable var 'reference)
  (let ((lex-binding (assq var byte-compile--lexical-environment)))
    (if lex-binding
	;; VAR is lexically bound
        (byte-compile-stack-ref (cdr lex-binding))
      ;; VAR is dynamically bound
      (byte-compile-free-vars-warn var var)
      (byte-compile-dynamic-variable-op 'byte-varref var))))

(defun byte-compile-variable-set (var)
  "Generate code to set the variable VAR from the top-of-stack value."
  (byte-compile-check-variable var 'assign)
  (let ((lex-binding (assq var byte-compile--lexical-environment)))
    (if lex-binding
	;; VAR is lexically bound.
        (byte-compile-stack-set (cdr lex-binding))
      ;; VAR is dynamically bound.
      (byte-compile-free-vars-warn var var t)
      (byte-compile-dynamic-variable-op 'byte-varset var))))

(defmacro byte-compile-get-constant (const)
  `(or (assoc ,const byte-compile-constants #'equal-including-properties)
       (car (setq byte-compile-constants
		  (cons (list ,const) byte-compile-constants)))))

;; Use this when the value of a form is a constant.
;; This obeys byte-compile--for-effect.
(defun byte-compile-constant (const)
  (if byte-compile--for-effect
      (setq byte-compile--for-effect nil)
    (inline (byte-compile-push-constant const))))

;; Use this for a constant that is not the value of its containing form.
;; This ignores byte-compile--for-effect.
(defun byte-compile-push-constant (const)
  (byte-compile-out
   'byte-constant
   (byte-compile-get-constant const)))

;; Compile those primitive ordinary functions
;; which have special byte codes just for speed.

(defmacro byte-defop-compiler (function &optional compile-handler)
  "Add a compiler-form for FUNCTION.
If function is a symbol, then the variable \"byte-SYMBOL\" must name
the opcode to be used.  If function is a list, the first element
is the function and the second element is the bytecode-symbol.
The second element may be nil, meaning there is no opcode.
COMPILE-HANDLER is the function to use to compile this byte-op, or
may be the abbreviations 0, 1, 2, 2-and, 3, 0-1, 1-2, 1-3, or 2-3.
If it is nil, then the handler is \"byte-compile-SYMBOL.\""
  (let (opcode)
    (if (symbolp function)
	(setq opcode (intern (concat "byte-" (symbol-name function))))
      (setq opcode (car (cdr function))
	    function (car function)))
    (let ((fnform
	   (list 'put (list 'quote function) ''byte-compile
		 (list 'quote
		       (or (cdr (assq compile-handler
				      '((0 . byte-compile-no-args)
					(1 . byte-compile-one-arg)
					(2 . byte-compile-two-args)
                                        (2-cmp . byte-compile-cmp)
					(3 . byte-compile-three-args)
					(0-1 . byte-compile-zero-or-one-arg)
					(1-2 . byte-compile-one-or-two-args)
					(2-3 . byte-compile-two-or-three-args)
					(1-3 . byte-compile-one-to-three-args)
					)))
			   compile-handler
			   (intern (concat "byte-compile-"
					   (symbol-name function))))))))
      (if opcode
	  (list 'progn fnform
		(list 'put (list 'quote function)
		      ''byte-opcode (list 'quote opcode))
		(list 'put (list 'quote opcode)
		      ''byte-opcode-invert (list 'quote function)))
	fnform))))

(defmacro byte-defop-compiler-1 (function &optional compile-handler)
  (list 'byte-defop-compiler (list function nil) compile-handler))


(put 'byte-call 'byte-opcode-invert 'funcall)
(put 'byte-list1 'byte-opcode-invert 'list)
(put 'byte-list2 'byte-opcode-invert 'list)
(put 'byte-list3 'byte-opcode-invert 'list)
(put 'byte-list4 'byte-opcode-invert 'list)
(put 'byte-listN 'byte-opcode-invert 'list)
(put 'byte-concat2 'byte-opcode-invert 'concat)
(put 'byte-concat3 'byte-opcode-invert 'concat)
(put 'byte-concat4 'byte-opcode-invert 'concat)
(put 'byte-concatN 'byte-opcode-invert 'concat)
(put 'byte-insertN 'byte-opcode-invert 'insert)

(byte-defop-compiler point		0)
(byte-defop-compiler point-max		0)
(byte-defop-compiler point-min		0)
(byte-defop-compiler following-char	0)
(byte-defop-compiler preceding-char	0)
(byte-defop-compiler current-column	0)
(byte-defop-compiler eolp		0)
(byte-defop-compiler eobp		0)
(byte-defop-compiler bolp		0)
(byte-defop-compiler bobp		0)
(byte-defop-compiler current-buffer	0)
(byte-defop-compiler widen		0)
(byte-defop-compiler end-of-line    0-1)
(byte-defop-compiler forward-char   0-1)
(byte-defop-compiler forward-line   0-1)
(byte-defop-compiler symbolp		1)
(byte-defop-compiler consp		1)
(byte-defop-compiler stringp		1)
(byte-defop-compiler listp		1)
(byte-defop-compiler not		1)
(byte-defop-compiler (null byte-not)	1)
(byte-defop-compiler car		1)
(byte-defop-compiler cdr		1)
(byte-defop-compiler length		1)
(byte-defop-compiler symbol-value	1)
(byte-defop-compiler symbol-function	1)
(byte-defop-compiler (1+ byte-add1)	1)
(byte-defop-compiler (1- byte-sub1)	1)
(byte-defop-compiler goto-char		1)
(byte-defop-compiler char-after		0-1)
(byte-defop-compiler set-buffer		1)
(byte-defop-compiler forward-word	0-1)
(byte-defop-compiler char-syntax	1)
(byte-defop-compiler nreverse		1)
(byte-defop-compiler car-safe		1)
(byte-defop-compiler cdr-safe		1)
(byte-defop-compiler numberp		1)
(byte-defop-compiler integerp		1)
(byte-defop-compiler skip-chars-forward     1-2)
(byte-defop-compiler skip-chars-backward    1-2)
(byte-defop-compiler eq 	 	2)
(byte-defop-compiler memq		2)
(byte-defop-compiler cons		2)
(byte-defop-compiler aref		2)
(byte-defop-compiler set		2)
(byte-defop-compiler fset		2)
(byte-defop-compiler (= byte-eqlsign)	2-cmp)
(byte-defop-compiler (< byte-lss)	2-cmp)
(byte-defop-compiler (> byte-gtr)	2-cmp)
(byte-defop-compiler (<= byte-leq)	2-cmp)
(byte-defop-compiler (>= byte-geq)	2-cmp)
(byte-defop-compiler get		2)
(byte-defop-compiler nth		2)
(byte-defop-compiler substring		1-3)
(byte-defop-compiler (move-marker byte-set-marker) 2-3)
(byte-defop-compiler set-marker	2-3)
(byte-defop-compiler match-beginning	1)
(byte-defop-compiler match-end	1)
(byte-defop-compiler upcase		1)
(byte-defop-compiler downcase		1)
(byte-defop-compiler string=		2)
(byte-defop-compiler string<		2)
(byte-defop-compiler (string-equal byte-string=) 2)
(byte-defop-compiler (string-lessp byte-string<) 2)
(byte-defop-compiler equal		2)
(byte-defop-compiler nthcdr		2)
(byte-defop-compiler elt		2)
(byte-defop-compiler member		2)
(byte-defop-compiler assq		2)
(byte-defop-compiler (rplaca byte-setcar) 2)
(byte-defop-compiler (rplacd byte-setcdr) 2)
(byte-defop-compiler setcar		2)
(byte-defop-compiler setcdr		2)
(byte-defop-compiler buffer-substring	2)
(byte-defop-compiler delete-region	2)
(byte-defop-compiler narrow-to-region	2)
(byte-defop-compiler (% byte-rem)	2)
(byte-defop-compiler aset		3)

(byte-defop-compiler max		byte-compile-min-max)
(byte-defop-compiler min		byte-compile-min-max)
(byte-defop-compiler (+ byte-plus)	byte-compile-variadic-numeric)
(byte-defop-compiler (* byte-mult)	byte-compile-variadic-numeric)

(byte-defop-compiler-1 interactive byte-compile-noop)


(defun byte-compile-subr-wrong-args (form n)
  (when (byte-compile-warning-enabled-p 'callargs (car form))
    (byte-compile-warn-x (car form)
                         "`%s' called with %d arg%s, but requires %s"
                         (car form) (length (cdr form))
                         (if (= 1 (length (cdr form))) "" "s") n)
    ;; Get run-time wrong-number-of-args error.
    (byte-compile-normal-call form)))

(defun byte-compile-no-args (form)
  (if (not (= (length form) 1))
      (byte-compile-subr-wrong-args form "none")
    (byte-compile-out (get (car form) 'byte-opcode) 0)))

(defun byte-compile-one-arg (form)
  (if (not (= (length form) 2))
      (byte-compile-subr-wrong-args form 1)
    (byte-compile-form (car (cdr form)))  ;; Push the argument
    (byte-compile-out (get (car form) 'byte-opcode) 0)))

(defun byte-compile-two-args (form)
  (if (not (= (length form) 3))
      (byte-compile-subr-wrong-args form 2)
    (byte-compile-form (car (cdr form)))  ;; Push the arguments
    (byte-compile-form (nth 2 form))
    (byte-compile-out (get (car form) 'byte-opcode) 0)))

(defun byte-compile-cmp (form)
  "Compile calls to numeric comparisons such as `<', `=' etc."
  ;; Lisp-level transforms should already have reduced valid calls to 2 args,
  ;; but optimizations may have been disabled.
  (let ((l (length form)))
    (cond
     ((= l 3)
      (byte-compile-two-args
       (if (macroexp-const-p (nth 1 form))
           ;; First argument is constant: flip it so that the constant
           ;; is last, which may allow more lapcode optimizations.
           (let* ((op (car form))
                  (flipped-op (cdr (assq op '((< . >) (<= . >=)
                                              (> . <) (>= . <=) (= . =))))))
             (list flipped-op (nth 2 form) (nth 1 form)))
         form)))
     ((= l 2) (byte-compile-form `(progn ,(nth 1 form) t)))
     (t (byte-compile-normal-call form)))))

(defun byte-compile-three-args (form)
  (if (not (= (length form) 4))
      (byte-compile-subr-wrong-args form 3)
    (byte-compile-form (car (cdr form)))  ;; Push the arguments
    (byte-compile-form (nth 2 form))
    (byte-compile-form (nth 3 form))
    (byte-compile-out (get (car form) 'byte-opcode) 0)))

(defun byte-compile-zero-or-one-arg (form)
  (let ((len (length form)))
    (cond ((= len 1) (byte-compile-one-arg (append form '(nil))))
	  ((= len 2) (byte-compile-one-arg form))
	  (t (byte-compile-subr-wrong-args form "0-1")))))

(defun byte-compile-one-or-two-args (form)
  (let ((len (length form)))
    (cond ((= len 2) (byte-compile-two-args (append form '(nil))))
	  ((= len 3) (byte-compile-two-args form))
	  (t (byte-compile-subr-wrong-args form "1-2")))))

(defun byte-compile-two-or-three-args (form)
  (let ((len (length form)))
    (cond ((= len 3) (byte-compile-three-args (append form '(nil))))
	  ((= len 4) (byte-compile-three-args form))
	  (t (byte-compile-subr-wrong-args form "2-3")))))

(defun byte-compile-one-to-three-args (form)
  (let ((len (length form)))
    (cond ((= len 2) (byte-compile-three-args (append form '(nil nil))))
          ((= len 3) (byte-compile-three-args (append form '(nil))))
          ((= len 4) (byte-compile-three-args form))
          (t (byte-compile-subr-wrong-args form "1-3")))))

(defun byte-compile-noop (_form)
  (byte-compile-constant nil))

(defun byte-compile-discard (&optional num preserve-tos)
  "Output byte codes to discard the NUM entries at the top of the stack.
NUM defaults to 1.
If PRESERVE-TOS is non-nil, preserve the top-of-stack value, as if it were
popped before discarding the num values, and then pushed back again after
discarding."
  (if (and (null num) (not preserve-tos))
      ;; common case
      (byte-compile-out 'byte-discard)
    ;; general case
    (unless num
      (setq num 1))
    (when (and preserve-tos (> num 0))
      ;; Preserve the top-of-stack value by writing it directly to the stack
      ;; location which will be at the top-of-stack after popping.
      (byte-compile-stack-set (1- (- byte-compile-depth num)))
      ;; Now we actually discard one less value, since we want to keep
      ;; the eventual TOS
      (setq num (1- num)))
    (while (> num 0)
      (byte-compile-out 'byte-discard)
      (setq num (1- num)))))

(defun byte-compile-stack-ref (stack-pos)
  "Output byte codes to push the value at stack position STACK-POS."
  (let ((dist (- byte-compile-depth (1+ stack-pos))))
    (if (zerop dist)
        ;; A simple optimization
        (byte-compile-out 'byte-dup)
      ;; normal case
      (byte-compile-out 'byte-stack-ref dist))))

(defun byte-compile-stack-set (stack-pos)
  "Output byte codes to store the TOS value at stack position STACK-POS."
  (byte-compile-out 'byte-stack-set (- byte-compile-depth (1+ stack-pos))))

(byte-defop-compiler-1 internal-make-closure byte-compile-make-closure)
(byte-defop-compiler-1 internal-get-closed-var byte-compile-get-closed-var)

(defun byte-compile-make-closure (form)
  "Byte-compile the special `internal-make-closure' form.

This function is never called when `lexical-binding' is nil."
  (if byte-compile--for-effect (setq byte-compile--for-effect nil)
    (let* ((vars (nth 1 form))
           (env (nth 2 form))
           (docstring-exp (nth 3 form))
           (body (nthcdr 4 form))
           (fun
            (byte-compile-lambda `(lambda ,vars . ,body) (length env))))
      (cl-assert (or (> (length env) 0)
		     docstring-exp))	;Otherwise, we don't need a closure.
      (cl-assert (byte-code-function-p fun))
      (byte-compile-form
       (if (macroexp-const-p docstring-exp)
           ;; Use symbols V0, V1 ... as placeholders for closure variables:
           ;; they should be short (to save space in the .elc file), yet
           ;; distinct when disassembled.
           (let* ((dummy-vars (mapcar (lambda (i) (intern (format "V%d" i)))
                                      (number-sequence 0 (1- (length env)))))
                  (opt-args (mapcar (lambda (i) (aref fun i))
                                    (number-sequence 4 (1- (length fun)))))
                  (proto-fun
                   (apply #'make-byte-code
                          (aref fun 0)  ; The arglist is always the 15-bit
                                        ; form, never the list of symbols.
                          (aref fun 1)  ; The byte-code.
                          ;; Prepend dummy cells to the constant vector,
                          ;; to get the indices right when disassembling.
                          (vconcat dummy-vars (aref fun 2))
                          (aref fun 3)  ; Stack depth of function
                          (if docstring-exp
                              (cons
                               (eval (byte-run-strip-symbol-positions
                                      docstring-exp)
                                     t)
                               (cdr opt-args)) ; The interactive spec will
                                               ; have been stripped in
                                               ; `byte-compile-lambda'.
                            opt-args))))
             `(make-closure ,proto-fun ,@env))
         ;; Nontrivial doc string expression: create a bytecode object
         ;; from small pieces at run time.
         `(make-byte-code
           ,(aref fun 0)         ; 15-bit form of arglist descriptor.
           ,(aref fun 1)         ; The byte-code.
           (vconcat (vector . ,env) ,(aref fun 2))  ; constant vector
           ,(aref fun 3)         ; max stack depth
           ,(byte-run-strip-symbol-positions docstring-exp)
           ;; optional interactive spec and anything else, all quoted
           ,@(mapcar (lambda (x) `',x) (drop 5 (append fun nil)))))))))

(defun byte-compile-get-closed-var (form)
  "Byte-compile the special `internal-get-closed-var' form."
  (if byte-compile--for-effect (setq byte-compile--for-effect nil)
    (byte-compile-out 'byte-constant (nth 1 form))))

;; Compile a pure function that accepts zero or more numeric arguments
;; and has an opcode for the binary case.
;; Single-argument calls are assumed to be numeric identity and are
;; compiled as (* x 1) in order to convert markers to numbers and
;; trigger type errors.
(defun byte-compile-variadic-numeric (form)
  (pcase (length form)
    (1
     ;; No args: use the identity value for the operation.
     (byte-compile-constant (eval form lexical-binding)))
    (2
     ;; One arg: compile (OP x) as (* x 1). This is identity for
     ;; all numerical values including -0.0, infinities and NaNs.
     (byte-compile-form (nth 1 form))
     (byte-compile-constant 1)
     (byte-compile-out (get '* 'byte-opcode) 0))
    (3
     (let ((arg1 (nth 1 form))
           (arg2 (nth 2 form)))
       (when (and (memq (car form) '(+ *))
                  (macroexp-const-p arg1))
         ;; Put constant argument last for better LAP optimization.
         (cl-rotatef arg1 arg2))
       (byte-compile-form arg1)
       (byte-compile-form arg2)
       (byte-compile-out (get (car form) 'byte-opcode) 0)))
    (_
     ;; >2 args: compile as a single function call.
     (byte-compile-normal-call form))))

(defun byte-compile-min-max (form)
  "Byte-compile calls to `min' or `max'."
  (if (cdr form)
      (byte-compile-variadic-numeric form)
    ;; No args: warn and emit code that raises an error when executed.
    (byte-compile-normal-call form)))


;; more complicated compiler macros

(byte-defop-compiler list)
(byte-defop-compiler concat)
(byte-defop-compiler (indent-to-column byte-indent-to) byte-compile-indent-to)
(byte-defop-compiler indent-to)
(byte-defop-compiler insert)
(byte-defop-compiler-1 function byte-compile-function-form)
(byte-defop-compiler (- byte-diff) byte-compile-minus)
(byte-defop-compiler (/ byte-quo) byte-compile-quo)
(byte-defop-compiler nconc)

(defun byte-compile-list (form)
  (let ((count (length (cdr form))))
    (cond ((= count 0)
	   (byte-compile-constant nil))
	  ((< count 5)
	   (mapc 'byte-compile-form (cdr form))
	   (byte-compile-out
	    (aref [byte-list1 byte-list2 byte-list3 byte-list4] (1- count)) 0))
	  ((< count 256)
	   (mapc 'byte-compile-form (cdr form))
	   (byte-compile-out 'byte-listN count))
	  (t (byte-compile-normal-call form)))))

(defun byte-compile-concat (form)
  (let ((count (length (cdr form))))
    (cond ((and (< 1 count) (< count 5))
	   (mapc 'byte-compile-form (cdr form))
	   (byte-compile-out
	    (aref [byte-concat2 byte-concat3 byte-concat4] (- count 2))
	    0))
	  ;; Concat of one arg is not a no-op if arg is not a string.
	  ((= count 0)
	   (byte-compile-form ""))
	  ((< count 256)
	   (mapc 'byte-compile-form (cdr form))
	   (byte-compile-out 'byte-concatN count))
	  ((byte-compile-normal-call form)))))

(defun byte-compile-minus (form)
  (if (/= (length form) 2)
      (byte-compile-variadic-numeric form)
    (byte-compile-form (cadr form))
    (byte-compile-out 'byte-negate 0)))

(defun byte-compile-quo (form)
  (if (= (length form) 3)
      (byte-compile-two-args form)
    ;; N-ary `/' is not the left-reduction of binary `/' because if any
    ;; argument is a float, then everything is done in floating-point.
    (byte-compile-normal-call form)))

(defun byte-compile-nconc (form)
  (let ((len (length form)))
    (cond ((= len 1)
	   (byte-compile-constant nil))
	  ((= len 2)
	   ;; nconc of one arg is a noop, even if that arg isn't a list.
	   (byte-compile-form (nth 1 form)))
	  (t
	   (byte-compile-form (car (setq form (cdr form))))
	   (while (setq form (cdr form))
	     (byte-compile-form (car form))
	     (byte-compile-out 'byte-nconc 0))))))

;; (function foo) must compile like 'foo, not like (symbol-function 'foo).
;; Otherwise it will be incompatible with the interpreter,
;; and (funcall (function foo)) will lose with autoloads.

(defun byte-compile-function-form (form)
  (let ((f (nth 1 form)))
    (when (and (symbolp f)
               (byte-compile-warning-enabled-p 'callargs f))
      (byte-compile-function-warn f t (byte-compile-fdefinition f nil)))

    (byte-compile-constant (if (eq 'lambda (car-safe f))
                               (byte-compile-lambda f)
                             f))))

(defun byte-compile-indent-to (form)
  (let ((len (length form)))
    (cond ((= len 2)
	   (byte-compile-form (car (cdr form)))
	   (byte-compile-out 'byte-indent-to 0))
	  ((= len 3)
	   ;; no opcode for 2-arg case.
	   (byte-compile-normal-call form))
	  (t
	   (byte-compile-subr-wrong-args form "1-2")))))

(defun byte-compile-insert (form)
  (cond ((null (cdr form))
	 (byte-compile-constant nil))
	((<= (length form) 256)
	 (mapc 'byte-compile-form (cdr form))
	 (if (cdr (cdr form))
	     (byte-compile-out 'byte-insertN (length (cdr form)))
	   (byte-compile-out 'byte-insert 0)))
	((memq t (mapcar 'consp (cdr (cdr form))))
	 (byte-compile-normal-call form))
	;; We can split it; there is no function call after inserting 1st arg.
	(t
	 (while (setq form (cdr form))
	   (byte-compile-form (car form))
	   (byte-compile-out 'byte-insert 0)
	   (if (cdr form)
	       (byte-compile-discard))))))


(byte-defop-compiler-1 setq)
(byte-defop-compiler-1 quote)

(defun byte-compile-setq (form)
  (cl-assert (= (length form) 3))       ; normalized in macroexp
  (let ((var (nth 1 form))
        (expr (nth 2 form)))
    (byte-compile-form expr)
    (unless byte-compile--for-effect
      (byte-compile-out 'byte-dup 0))
    (byte-compile-variable-set var)
    (setq byte-compile--for-effect nil)))

(byte-defop-compiler-1 set-default)
(defun byte-compile-set-default (form)
  (let ((varexp (car-safe (cdr-safe form))))
    (if (eq (car-safe varexp) 'quote)
        ;; If the varexp is constant, check the var's name.
        (let ((var (car-safe (cdr varexp))))
          (and (or (not (symbolp var))
	           (macroexp--const-symbol-p var t))
               (byte-compile-warning-enabled-p 'constants
                                               (and (symbolp var) var))
               (byte-compile-warn-x
                var
	        "variable assignment to %s `%s'"
	        (if (symbolp var) "constant" "nonvariable")
	        var))))
    (byte-compile-normal-call form)))

(defun byte-compile-quote (form)
  (byte-compile-constant (car (cdr form))))

;;; control structures

(defun byte-compile-body (body &optional for-effect)
  (while (cdr body)
    (byte-compile-form (car body) t)
    (setq body (cdr body)))
  (byte-compile-form (car body) for-effect))

(defsubst byte-compile-body-do-effect (body)
  (byte-compile-body body byte-compile--for-effect)
  (setq byte-compile--for-effect nil))

(defsubst byte-compile-form-do-effect (form)
  (byte-compile-form form byte-compile--for-effect)
  (setq byte-compile--for-effect nil))

(byte-defop-compiler-1 inline byte-compile-progn)
(byte-defop-compiler-1 progn)
(byte-defop-compiler-1 prog1)
(byte-defop-compiler-1 if)
(byte-defop-compiler-1 cond)
(byte-defop-compiler-1 and)
(byte-defop-compiler-1 or)
(byte-defop-compiler-1 while)
(byte-defop-compiler-1 funcall)
(byte-defop-compiler-1 let)
(byte-defop-compiler-1 let* byte-compile-let)
(byte-defop-compiler-1 ignore)

(defun byte-compile-progn (form)
  (byte-compile-body-do-effect (cdr form)))

(defun byte-compile-prog1 (form)
  (byte-compile-form-do-effect (car (cdr form)))
  (byte-compile-body (cdr (cdr form)) t))

(defmacro byte-compile-goto-if (cond discard tag)
  `(byte-compile-goto
    (if ,cond
	(if ,discard 'byte-goto-if-not-nil 'byte-goto-if-not-nil-else-pop)
      (if ,discard 'byte-goto-if-nil 'byte-goto-if-nil-else-pop))
    ,tag))

(defun byte-compile-ignore (form)
  (dolist (arg (cdr form))
    ;; Compile each argument for-effect but suppress unused-value warnings.
    (byte-compile-form arg 'for-effect-no-warn))
  (byte-compile-form nil))

;; Return the list of items in CONDITION-PARAM that match PRED-LIST.
;; Only return items that are not in ONLY-IF-NOT-PRESENT.
(defun byte-compile-find-bound-condition (condition-param
					  pred-list
					  &optional only-if-not-present)
  (let ((result nil)
	(nth-one nil)
	(cond-list
	 (if (memq (car-safe condition-param) pred-list)
	     ;; The condition appears by itself.
	     (list condition-param)
	   ;; If the condition is an `and', look for matches among the
	   ;; `and' arguments.
	   (when (eq 'and (car-safe condition-param))
	     (cdr condition-param)))))

    (dolist (crt cond-list)
      (when (and (memq (car-safe crt) pred-list)
		 (eq 'quote (car-safe (setq nth-one (nth 1 crt))))
		 ;; Ignore if the symbol is already on the unresolved
		 ;; list.
		 (not (assq (nth 1 nth-one) ; the relevant symbol
			    only-if-not-present)))
	(push (nth 1 (nth 1 crt)) result)))
    result))

(defmacro byte-compile-maybe-guarded (condition &rest body)
  "Execute forms in BODY, potentially guarded by CONDITION.
CONDITION is a variable whose value is a test in an `if' or `cond'.
BODY is the code to compile in the first arm of the if or the body of
the cond clause.  If CONDITION's value is of the form (fboundp \\='foo)
or (boundp \\='foo), the relevant warnings from BODY about foo's
being undefined (or obsolete) will be suppressed.

If CONDITION's value is (not (featurep \\='emacs)) or (featurep \\='xemacs),
that suppresses all warnings during execution of BODY."
  (declare (indent 1) (debug t))
  `(byte-compile--maybe-guarded ,condition (lambda () ,@body)))

(defun byte-compile--maybe-guarded (condition body-fun)
  (let* ((fbound-list (byte-compile-find-bound-condition
		       condition '(fboundp functionp)
		       byte-compile-unresolved-functions))
	 (bound-list (byte-compile-find-bound-condition
                      condition '(boundp default-boundp local-variable-p)))
         (new-bound-list
          ;; (seq-difference  byte-compile-bound-variables))
          (delq nil (mapcar (lambda (s)
                              (if (memq s byte-compile-bound-variables) nil s))
                            bound-list)))
	 ;; Maybe add to the bound list.
	 (byte-compile-bound-variables
          (append new-bound-list byte-compile-bound-variables)))
    (mapc #'byte-compile--check-prefixed-var new-bound-list)
    (unwind-protect
	;; If things not being bound at all is ok, so must them being
	;; obsolete.  Note that we add to the existing lists since Tramp
	;; (ab)uses this feature.
        ;; FIXME: If `foo' is obsoleted by `bar', the code below
        ;; correctly arranges to silence the warnings after testing
        ;; existence of `foo', but the warning should also be
        ;; silenced after testing the existence of `bar'.
	(let ((byte-compile-not-obsolete-vars
	       (append byte-compile-not-obsolete-vars bound-list))
	      (byte-compile-not-obsolete-funcs
	       (append byte-compile-not-obsolete-funcs fbound-list)))
	  (funcall body-fun))
      ;; Maybe remove the function symbol from the unresolved list.
      (dolist (fbound fbound-list)
	(when fbound
	  (setq byte-compile-unresolved-functions
		(delq (assq fbound byte-compile-unresolved-functions)
		      byte-compile-unresolved-functions)))))))

(defun byte-compile-if (form)
  (byte-compile-form (car (cdr form)))
  ;; Check whether we have `(if (fboundp ...' or `(if (boundp ...'
  ;; and avoid warnings about the relevant symbols in the consequent.
  (let ((clause (nth 1 form))
	(donetag (byte-compile-make-tag)))
    (if (null (nthcdr 3 form))
	;; No else-forms
	(progn
	  (byte-compile-goto-if nil byte-compile--for-effect donetag)
	  (byte-compile-maybe-guarded clause
	    (byte-compile-form (nth 2 form) byte-compile--for-effect))
	  (byte-compile-out-tag donetag))
      (let ((elsetag (byte-compile-make-tag)))
	(byte-compile-goto 'byte-goto-if-nil elsetag)
	(byte-compile-maybe-guarded clause
	  (byte-compile-form (nth 2 form) byte-compile--for-effect))
	(byte-compile-goto 'byte-goto donetag)
	(byte-compile-out-tag elsetag)
	(byte-compile-maybe-guarded (list 'not clause)
	  (byte-compile-body (cdr (cdr (cdr form))) byte-compile--for-effect))
	(byte-compile-out-tag donetag))))
  (setq byte-compile--for-effect nil))

(defun byte-compile--cond-vars (obj1 obj2)
  ;; We make sure that of OBJ1 and OBJ2, one of them is a symbol,
  ;; and the other is a constant expression whose value can be
  ;; compared with `eq' (with `macroexp-const-p').
  (or
   (and (symbolp obj1) (macroexp-const-p obj2)
        (cons obj1 (eval obj2 lexical-binding)))
   (and (symbolp obj2) (macroexp-const-p obj1)
        (cons obj2 (eval obj1 lexical-binding)))))

(defun byte-compile--common-test (test-1 test-2)
  "Most specific common test of `eq', `eql' and `equal'."
  (cond ((or (eq test-1 'equal) (eq test-2 'equal)) 'equal)
        ((or (eq test-1 'eql)   (eq test-2 'eql))   'eql)
        (t                                          'eq)))

(defun byte-compile--cond-switch-prefix (clauses)
  "Find a switch corresponding to a prefix of CLAUSES, or nil if none.
Return (TAIL VAR TEST CASES), where:
  TAIL is the remaining part of CLAUSES after the switch, including
  any default clause,
  VAR is the variable being switched on,
  TEST is the equality test (`eq', `eql' or `equal'),
  CASES is a list of (VALUES . BODY) where VALUES is a list of values
    corresponding to BODY (always non-empty)."
  (let ((cases nil)                 ; Reversed list of (VALUES BODY).
        (keys nil)                  ; Switch keys seen so far.
        (switch-var nil)
        (switch-test 'eq))
    (while (pcase (car clauses)
             (`((,(and fn (or 'eq 'eql 'equal)) ,expr1 ,expr2) . ,body)
              (let* ((vars (byte-compile--cond-vars expr1 expr2))
                     (var (car vars))
                     (value (cdr vars)))
                (and var (or (eq var switch-var) (not switch-var))
                     (progn
                       (setq switch-var var)
                       (setq switch-test
                             (byte-compile--common-test switch-test fn))
                       (unless (member value keys)
                         (push value keys)
                         (push (cons (list value) (or body '(t))) cases))
                       t))))
             ;; Treat (not X) as (eq X nil).
             (`((,(or 'not 'null) ,(and var (pred symbolp))) . ,body)
              (and (or (eq var switch-var) (not switch-var))
                   (progn
                     (setq switch-var var)
                     (setq switch-test
                           (byte-compile--common-test switch-test 'eq))
                     (unless (memq nil keys)
                       (push nil keys)
                       (push (cons (list nil) (or body '(t))) cases))
                     t)))
             (`((,(and fn (or 'memq 'memql 'member)) ,var ,expr) . ,body)
              (and (symbolp var)
                   (or (eq var switch-var) (not switch-var))
                   (macroexp-const-p expr)
                   ;; Require a non-empty body, since the member
                   ;; function value depends on the switch argument.
                   body
                   (let ((value (eval expr lexical-binding)))
                     (and (proper-list-p value)
                          (progn
                            (setq switch-var var)
                            (setq switch-test
                                  (byte-compile--common-test
                                   switch-test
                                   (cdr (assq fn '((memq   . eq)
                                                   (memql  . eql)
                                                   (member . equal))))))
                            (let ((vals nil))
                              (dolist (elem value)
                                (unless (funcall fn elem keys)
                                  (push elem vals)))
                              (when vals
                                (setq keys (append vals keys))
                                (push (cons (nreverse vals) body) cases)))
                            t))))))
      (setq clauses (cdr clauses)))
    ;; Assume that a single switch is cheaper than two or more discrete
    ;; compare clauses.  This could be tuned, possibly taking into
    ;; account the total number of values involved.
    (and (> (length cases) 1)
         (list clauses switch-var switch-test (nreverse cases)))))

(defun byte-compile-cond-jump-table (switch donetag)
  "Generate code for SWITCH, ending at DONETAG."
  (let* ((var (car switch))
         (test (nth 1 switch))
         (cases (nth 2 switch))
         jump-table test-objects body tag default-tag)
    ;; TODO: Once :linear-search is implemented for `make-hash-table'
    ;; set it to t for cond forms with a small number of cases.
    (let ((nvalues (apply #'+ (mapcar (lambda (case) (length (car case)))
                                      cases))))
      (setq jump-table (make-hash-table
			:test test
			:size nvalues)))
    (setq default-tag (byte-compile-make-tag))
    ;; The structure of byte-switch code:
    ;;
    ;; varref var
    ;; constant #s(hash-table data (val1 (TAG1) val2 (TAG2)))
    ;; switch
    ;; goto DEFAULT-TAG
    ;; TAG1
    ;; <clause body>
    ;; goto DONETAG
    ;; TAG2
    ;; <clause body>
    ;; goto DONETAG
    ;; DEFAULT-TAG
    ;; <body for remaining (non-switch) clauses>
    ;; DONETAG

    (byte-compile-variable-ref var)
    (byte-compile-push-constant jump-table)
    (byte-compile-out 'byte-switch)

    ;; When the opcode argument is `byte-goto', `byte-compile-goto' sets
    ;; `byte-compile-depth' to nil. However, we need `byte-compile-depth'
    ;; to be non-nil for generating tags for all cases. Since
    ;; `byte-compile-depth' will increase by at most 1 after compiling
    ;; all of the clause (which is further enforced by cl-assert below)
    ;; it should be safe to preserve its value.
    (let ((byte-compile-depth byte-compile-depth))
      (byte-compile-goto 'byte-goto default-tag))

    (dolist (case cases)
      (setq tag (byte-compile-make-tag)
            test-objects (car case)
            body (cdr case))
      (byte-compile-out-tag tag)
      (dolist (value test-objects)
        (puthash value tag jump-table))

      (let ((byte-compile-depth byte-compile-depth)
            (init-depth byte-compile-depth))
        ;; Since `byte-compile-body' might increase `byte-compile-depth'
        ;; by 1, not preserving its value will cause it to potentially
        ;; increase by one for every clause body compiled, causing
        ;; depth/tag conflicts or violating asserts down the road.
        ;; To make sure `byte-compile-body' itself doesn't violate this,
        ;; we use `cl-assert'.
        (byte-compile-body body byte-compile--for-effect)
        (cl-assert (or (= byte-compile-depth init-depth)
                       (= byte-compile-depth (1+ init-depth))))
        (byte-compile-goto 'byte-goto donetag)
        (setcdr (cdr donetag) nil)))

    (byte-compile-out-tag default-tag)
    (push jump-table byte-compile-jump-tables)))

(defun byte-compile-cond (clauses)
  (let ((donetag (byte-compile-make-tag))
        nexttag clause)
    (setq clauses (cdr clauses))
    (while clauses
      (let ((switch-prefix (and byte-compile-cond-use-jump-table
                                (byte-compile--cond-switch-prefix clauses))))
        (if switch-prefix
            (progn
              (byte-compile-cond-jump-table (cdr switch-prefix) donetag)
              (setq clause nil)
              (setq clauses (car switch-prefix)))
          (setq clause (car clauses))
          (cond ((or (eq (car clause) t)
                     (and (eq (car-safe (car clause)) 'quote)
                          (car-safe (cdr-safe (car clause)))))
                 ;; Unconditional clause
                 (setq clause (cons t clause)
                       clauses nil))
                ((cdr clauses)
                 (byte-compile-form (car clause))
                 (if (null (cdr clause))
                     ;; First clause is a singleton.
                     (byte-compile-goto-if t byte-compile--for-effect donetag)
                   (setq nexttag (byte-compile-make-tag))
                   (byte-compile-goto 'byte-goto-if-nil nexttag)
                   (byte-compile-maybe-guarded (car clause)
                     (byte-compile-body (cdr clause) byte-compile--for-effect))
                   (byte-compile-goto 'byte-goto donetag)
                   (byte-compile-out-tag nexttag))))
          (setq clauses (cdr clauses)))))
    ;; Last clause
    (let ((guard (car clause)))
      (and (cdr clause) (not (eq guard t))
           (progn (byte-compile-form guard)
                  (byte-compile-goto-if nil byte-compile--for-effect donetag)
                  (setq clause (cdr clause))))
      (byte-compile-maybe-guarded guard
        (byte-compile-body-do-effect clause)))
    (byte-compile-out-tag donetag)))

(defun byte-compile-and (form)
  (let ((failtag (byte-compile-make-tag))
	(args (cdr form)))
    (if (null args)
	(byte-compile-form-do-effect t)
      (byte-compile-and-recursion args failtag))))

;; Handle compilation of a nontrivial `and' call.
;; We use tail recursion so we can use byte-compile-maybe-guarded.
(defun byte-compile-and-recursion (rest failtag)
  (if (cdr rest)
      (progn
	(byte-compile-form (car rest))
	(byte-compile-goto-if nil byte-compile--for-effect failtag)
	(byte-compile-maybe-guarded (car rest)
	  (byte-compile-and-recursion (cdr rest) failtag)))
    (byte-compile-form-do-effect (car rest))
    (byte-compile-out-tag failtag)))

(defun byte-compile-or (form)
  (let ((wintag (byte-compile-make-tag))
	(args (cdr form)))
    (if (null args)
	(byte-compile-form-do-effect nil)
      (byte-compile-or-recursion args wintag))))

;; Handle compilation of a nontrivial `or' call.
;; We use tail recursion so we can use byte-compile-maybe-guarded.
(defun byte-compile-or-recursion (rest wintag)
  (if (cdr rest)
      (progn
	(byte-compile-form (car rest))
	(byte-compile-goto-if t byte-compile--for-effect wintag)
	(byte-compile-maybe-guarded (list 'not (car rest))
	  (byte-compile-or-recursion (cdr rest) wintag)))
    (byte-compile-form-do-effect (car rest))
    (byte-compile-out-tag wintag)))

(defun byte-compile-while (form)
  (let ((endtag (byte-compile-make-tag))
	(looptag (byte-compile-make-tag)))
    (byte-compile-out-tag looptag)
    (byte-compile-form (car (cdr form)))
    (byte-compile-goto-if nil byte-compile--for-effect endtag)
    (byte-compile-body (cdr (cdr form)) t)
    (byte-compile-goto 'byte-goto looptag)
    (byte-compile-out-tag endtag)
    (setq byte-compile--for-effect nil)))

(defun byte-compile-funcall (form)
  (if (cdr form)
      (progn
        (mapc 'byte-compile-form (cdr form))
        (byte-compile-out 'byte-call (length (cdr (cdr form)))))
    (byte-compile-report-error
     (format-message "`funcall' called with no arguments"))
    (byte-compile-form '(signal 'wrong-number-of-arguments '(funcall 0))
                       byte-compile--for-effect)))


;; let binding

(defun byte-compile-push-binding-init (clause)
  "Emit byte-codes to push the initialization value for CLAUSE on the stack.
Return the offset in the form (VAR . OFFSET)."
  (let* ((var (if (consp clause) (car clause) clause)))
    ;; We record the stack position even of dynamic bindings; we'll put
    ;; them in the proper place later.
    (prog1 (cons var byte-compile-depth)
      (if (consp clause)
          (byte-compile-form (cadr clause))
        (byte-compile-push-constant nil)))))

(defun byte-compile-bind (var init-lexenv)
  "Emit byte-codes to bind VAR and update `byte-compile--lexical-environment'.
INIT-LEXENV should be a lexical-environment alist describing the
positions of the init value that have been pushed on the stack.
Return non-nil if the TOS value was popped."
  ;; The mix of lexical and dynamic bindings mean that we may have to
  ;; juggle things on the stack, to move them to TOS for
  ;; dynamic binding.
  (if (not (cconv--not-lexical-var-p var byte-compile-bound-variables))
      ;; VAR is a simple stack-allocated lexical variable.
      (progn (push (assq var init-lexenv)
                   byte-compile--lexical-environment)
             (when (assq var byte-compile--known-dynamic-vars)
               (byte-compile--warn-lexical-dynamic var 'let))
             nil)
    ;; VAR should be dynamically bound.
    (while (assq var byte-compile--lexical-environment)
      ;; This dynamic binding shadows a lexical binding.
      (setq byte-compile--lexical-environment
            (remq (assq var byte-compile--lexical-environment)
                  byte-compile--lexical-environment)))
    (cond
     ((eq var (caar init-lexenv))
      ;; VAR is dynamic and is on the top of the
      ;; stack, so we can just bind it like usual.
      (byte-compile-dynamic-variable-bind var)
      t)
     (t
      ;; VAR is dynamic, but we have to get its
      ;; value out of the middle of the stack.
      (let ((stack-pos (cdr (assq var init-lexenv))))
        (byte-compile-stack-ref stack-pos)
        (byte-compile-dynamic-variable-bind var)
        ;; Now we have to store nil into its temporary
        ;; stack position so it doesn't prevent the value from being GC'd.
        ;; FIXME: Not worth the trouble.
        ;; (byte-compile-push-constant nil)
        ;; (byte-compile-stack-set stack-pos)
        )
      nil))))

(defun byte-compile-unbind (clauses init-lexenv preserve-body-value)
  "Emit byte-codes to unbind the variables bound by CLAUSES.
CLAUSES is a `let'-style variable binding list.  INIT-LEXENV should be a
lexical-environment alist describing the positions of the init value that
have been pushed on the stack.  If PRESERVE-BODY-VALUE is true,
then an additional value on the top of the stack, above any lexical binding
slots, is preserved, so it will be on the top of the stack after all
binding slots have been popped."
  ;; Unbind dynamic variables.
  (let ((num-dynamic-bindings 0))
    (dolist (clause clauses)
      (unless (assq (if (consp clause) (car clause) clause)
                    byte-compile--lexical-environment)
        (setq num-dynamic-bindings (1+ num-dynamic-bindings))))
    (unless (zerop num-dynamic-bindings)
      (byte-compile-out 'byte-unbind num-dynamic-bindings)))
  ;; Pop lexical variables off the stack, possibly preserving the
  ;; return value of the body.
  (when init-lexenv
    ;; INIT-LEXENV contains all init values left on the stack.
    (byte-compile-discard (length init-lexenv) preserve-body-value)))

(defun byte-compile-let (form)
  "Generate code for the `let' or `let*' form FORM."
  (let ((clauses (cadr form))
	(init-lexenv nil)
        (is-let (eq (car form) 'let)))
    (when is-let
      ;; First compute the binding values in the old scope.
      (dolist (var clauses)
        (push (byte-compile-push-binding-init var) init-lexenv)))
    ;; New scope.
    (let ((byte-compile-bound-variables byte-compile-bound-variables)
          (byte-compile--lexical-environment
           byte-compile--lexical-environment))
      ;; Bind the variables.
      ;; For `let', do it in reverse order, because it makes no
      ;; semantic difference, but it is a lot more efficient since the
      ;; values are now in reverse order on the stack.
      (dolist (var (if is-let (reverse clauses) clauses))
        (unless is-let
          (push (byte-compile-push-binding-init var) init-lexenv))
        (let ((var (if (consp var) (car var) var)))
          (if (byte-compile-bind var init-lexenv)
              (pop init-lexenv))))
      ;; Emit the body.
      (let ((init-stack-depth byte-compile-depth))
        (byte-compile-body-do-effect (cdr (cdr form)))
        ;; Unbind both lexical and dynamic variables.
        (cl-assert (or (eq byte-compile-depth init-stack-depth)
                       (eq byte-compile-depth (1+ init-stack-depth))))
        (byte-compile-unbind clauses init-lexenv
                             (> byte-compile-depth init-stack-depth))))))


;;; other tricky macro-like special-forms

(byte-defop-compiler-1 catch)
(byte-defop-compiler-1 unwind-protect)
(byte-defop-compiler-1 condition-case)
(byte-defop-compiler-1 save-excursion)
(byte-defop-compiler-1 save-current-buffer)
(byte-defop-compiler-1 save-restriction)

(defun byte-compile-catch (form)
  (byte-compile-form (car (cdr form)))
  (let ((endtag (byte-compile-make-tag)))
    (byte-compile-goto 'byte-pushcatch endtag)
    (byte-compile-body (cddr form) nil)
    (byte-compile-out 'byte-pophandler)
    (byte-compile-out-tag endtag)))

(defun byte-compile-unwind-protect (form)
  (cl-assert (eq (caddr form) :fun-body))
  (byte-compile-form (nth 3 form))
  (byte-compile-out 'byte-unwind-protect 0)
  (byte-compile-form-do-effect (car (cdr form)))
  (byte-compile-out 'byte-unbind 1))

(defun byte-compile-condition-case (form)
  (let* ((var (nth 1 form))
         (body (nth 2 form))
         (handlers (nthcdr 3 form))
         (depth byte-compile-depth)
         (success-handler (assq :success handlers))
         (failure-handlers (if success-handler
                               (remq success-handler handlers)
                             handlers))
         (clauses (mapcar (lambda (clause)
                            (cons (byte-compile-make-tag) clause))
                          failure-handlers))
         (endtag (byte-compile-make-tag)))
    (unless (symbolp var)
      (byte-compile-warn-x
       var "`%s' is not a variable-name or nil (in condition-case)" var))

    (dolist (clause (reverse clauses))
      (let ((condition (nth 1 clause)))
        (when (and (eq (car-safe condition) 'quote)
                   (cdr condition) (null (cddr condition)))
          (byte-compile-warn-x
           condition "`condition-case' condition should not be quoted: %S"
           condition))
        (when (and (consp condition) (memq :success condition))
          (byte-compile-warn-x
           condition
           "`:success' must be the first element of a `condition-case' handler"))
        (unless (consp condition) (setq condition (list condition)))
        (dolist (c condition)
          (unless (and c (symbolp c))
            (byte-compile-warn-x
             c "`%S' is not a condition name (in condition-case)" c))
          ;; In reality, the `error-conditions' property is only required
          ;; for the argument to `signal', not to `condition-case'.
          ;;(unless (consp (get c 'error-conditions))
          ;;  (byte-compile-warn
          ;;   "`%s' is not a known condition name (in condition-case)"
          ;;   c))
          )
        (byte-compile-push-constant condition))
      (byte-compile-goto 'byte-pushconditioncase (car clause)))

    (byte-compile-form body) ;; byte-compile--for-effect
    (dolist (_ clauses) (byte-compile-out 'byte-pophandler))

    (let ((compile-handler-body
           (lambda (body)
             (let ((byte-compile-bound-variables byte-compile-bound-variables)
                   (byte-compile--lexical-environment
                    byte-compile--lexical-environment))
               (cond
                ((null var) (byte-compile-discard))
                (lexical-binding
                 (push (cons var (1- byte-compile-depth))
                       byte-compile--lexical-environment))
                (t (byte-compile-dynamic-variable-bind var)))

               (byte-compile-body body) ;; byte-compile--for-effect

               (cond
                ((null var))
                (lexical-binding (byte-compile-discard 1 'preserve-tos))
                (t (byte-compile-out 'byte-unbind 1)))))))

      (when success-handler
        (funcall compile-handler-body (cdr success-handler)))

      (byte-compile-goto 'byte-goto endtag)

      (while clauses
        (let ((clause (pop clauses)))
          (setq byte-compile-depth (1+ depth))
          (byte-compile-out-tag (pop clause))
          (dolist (_ clauses) (byte-compile-out 'byte-pophandler))
          (funcall compile-handler-body (cdr clause))
          (byte-compile-goto 'byte-goto endtag)))

      (byte-compile-out-tag endtag))))

(defun byte-compile-save-excursion (form)
  (if (and (eq 'set-buffer (car-safe (car-safe (cdr form))))
           (byte-compile-warning-enabled-p 'suspicious 'set-buffer))
      (byte-compile-warn-x
       form
       "Use `with-current-buffer' rather than save-excursion+set-buffer"))
  (byte-compile-out 'byte-save-excursion 0)
  (byte-compile-body-do-effect (cdr form))
  (byte-compile-out 'byte-unbind 1))

(defun byte-compile-save-restriction (form)
  (byte-compile-out 'byte-save-restriction 0)
  (byte-compile-body-do-effect (cdr form))
  (byte-compile-out 'byte-unbind 1))

(defun byte-compile-save-current-buffer (form)
  (byte-compile-out 'byte-save-current-buffer 0)
  (byte-compile-body-do-effect (cdr form))
  (byte-compile-out 'byte-unbind 1))

;;; top-level forms elsewhere

(byte-defop-compiler-1 defvar)
(byte-defop-compiler-1 defconst byte-compile-defvar)
(byte-defop-compiler-1 autoload)
(byte-defop-compiler-1 lambda byte-compile-lambda-form)

;; If foo.el declares `toto' as obsolete, it is likely that foo.el will
;; actually use `toto' in order for this obsolete variable to still work
;; correctly, so paradoxically, while byte-compiling foo.el, the presence
;; of a make-obsolete-variable call for `toto' is an indication that `toto'
;; should not trigger obsolete-warnings in foo.el.
(byte-defop-compiler-1 make-obsolete-variable)
(defun byte-compile-make-obsolete-variable (form)
  (when (eq 'quote (car-safe (nth 1 form)))
    (push (nth 1 (nth 1 form)) byte-compile-global-not-obsolete-vars))
  (byte-compile-normal-call form))

(defun byte-compile-defvar (form &optional toplevel)
  (let* ((fun (nth 0 form))
	 (var (nth 1 form))
	 (value (nth 2 form))
	 (string (nth 3 form)))
    (byte-compile--declare-var var (not toplevel))
    (if (eq fun 'defconst)
	(push var byte-compile-const-variables))
    (cond
     ((stringp string)
      (setq string (byte-compile--docstring string fun var 'is-a-value)))
     (string
      (byte-compile-warn-x
       string
       "third arg to `%s %s' is not a string: %s"
       fun var string)))
    (if toplevel
        ;; At top-level we emit calls to defvar/defconst.
        (if (and (null (cddr form))       ;No `value' provided.
                 (eq (car form) 'defvar)) ;Just a declaration.
            nil
          (let ((tail (nthcdr 4 form)))
            (when (or tail string) (push string tail))
            (when (cddr form)
              (push (if (not (consp value)) value
                        (byte-compile-top-level value nil 'file))
                    tail))
            `(,fun ,var ,@tail)))
      ;; At non-top-level, since there is no byte code for
      ;; defvar/defconst, we delegate the actual work to the function
      ;; version of the special form, named with a "-1" suffix.
      (byte-compile-form-do-effect
       (cond
        ((eq fun 'defconst)
         `(defconst-1 ',var ,@(byte-compile--list-with-n
                               (nthcdr 2 form) 1 (macroexp-quote string))))
        ((not (cddr form)) `',var) ; A simple (defvar foo) just returns foo.
        (t `(defvar-1 ',var
                      ;; Don't eval `value' if `defvar' wouldn't eval it either.
                      ,(if (macroexp-const-p value) value
                         `(if (boundp ',var) nil ,value))
                      ,@(byte-compile--list-with-n
                         (nthcdr 3 form) 0 (macroexp-quote string)))))))))

(defun byte-compile-autoload (form)
  (and (macroexp-const-p (nth 1 form))
       (macroexp-const-p (nth 5 form))
       (memq (eval (nth 5 form)) '(t macro))  ; macro-p
       (not (fboundp (eval (nth 1 form))))
       (byte-compile-warn-x
        form
	"The compiler ignores `autoload' except at top level.  You should
     probably put the autoload of the macro `%s' at top-level."
	(eval (nth 1 form))))
  (byte-compile-normal-call form))

;; Lambdas in valid places are handled as special cases by various code.
;; The ones that remain are errors.
(defun byte-compile-lambda-form (_form)
  (error "`lambda' used as function name is invalid"))

;; Compile normally, but deal with warnings for the function being defined.
(put 'defalias 'byte-hunk-handler 'byte-compile-file-form-defalias)
;; Used for eieio--defalias as well.
(defun byte-compile-file-form-defalias (form)
  ;; For the compilation itself, we could largely get rid of this hunk-handler,
  ;; if it weren't for the fact that we need to figure out when a defalias
  ;; defines a macro, so as to add it to byte-compile-macro-environment.
  (let ((byte-compile-free-references nil)
        (byte-compile-free-assignments nil))
    (pcase form
      ;; Decompose `form' into:
      ;; - `name' is the name of the defined function.
      ;; - `arg' is the expression to which it is defined.
      ;; - `rest' is the rest of the arguments.
      (`(,_ ',name ,arg . ,rest)
       (let ((doc (car rest)))
         (when (stringp doc)
           (setq rest (byte-compile--list-with-n
                       rest 0
                       (byte-compile--docstring doc (nth 0 form) name)))
           (setq form (nconc (take 3 form) rest))))
       (pcase-let*
           ;; `macro' is non-nil if it defines a macro.
           ;; `fun' is the function part of `arg' (defaults to `arg').
           (((or (and (or `(cons 'macro ,fun)
                          `'(macro . ,(app macroexp-quote fun)))
                      (let macro t))
                 (and (let fun arg) (let macro nil)))
             arg)
            ;; `lam' is the lambda expression in `fun' (or nil if not
            ;; recognized).
            ((or `(,(or 'quote 'function) ,lam) (let lam nil))
             fun)
            ;; `arglist' is the list of arguments (or t if not recognized).
            ;; `body' is the body of `lam' (or t if not recognized).
            ((or `(lambda ,arglist . ,body)
                 (and `(internal-make-closure ,arglist . ,_) (let body t))
                 (and (let arglist t) (let body t)))
             lam))
         (unless (byte-compile-file-form-defmumble
                  name macro arglist body rest)
           (if (not macro)
               (push (cons name (if (listp arglist) `(declared ,arglist) t))
                     byte-compile-function-environment)
             (byte-compile-warn-x
              name "Definition of macro %s not fully recognized" name)
             (push (cons name (eval fun lexical-binding))
                   byte-compile-macro-environment))
           (byte-compile-keep-pending form))))

      ;; We used to just do: (byte-compile-normal-call form)
      ;; But it turns out that this fails to optimize the code.
      ;; So instead we now do the same as what other byte-hunk-handlers do,
      ;; which is to call back byte-compile-file-form and then return nil.
      ;; Except that we can't just call byte-compile-file-form since it would
      ;; call us right back.
      (_ (byte-compile-keep-pending form)))))

(byte-defop-compiler-1 with-no-warnings byte-compile-no-warnings)
(defun byte-compile-no-warnings (form)
  (let (byte-compile-warnings)
    (byte-compile-form (cons 'progn (cdr form)))))

(byte-defop-compiler-1 internal--with-suppressed-warnings
                       byte-compile-suppressed-warnings)
(defun byte-compile-suppressed-warnings (form)
  (let ((byte-compile--suppressed-warnings
         (append (cadadr form) byte-compile--suppressed-warnings)))
    ;; Propagate the for-effect mode explicitly so that warnings about
    ;; ignored return values can be detected and suppressed correctly.
    (byte-compile-form (macroexp-progn (cddr form)) byte-compile--for-effect)
    (setq byte-compile--for-effect nil)))

;; Warn about misuses of make-variable-buffer-local.
(byte-defop-compiler-1 make-variable-buffer-local
                       byte-compile-make-variable-buffer-local)
(defun byte-compile-make-variable-buffer-local (form)
  (if (and (eq (car-safe (car-safe (cdr-safe form))) 'quote)
           (byte-compile-warning-enabled-p 'make-local))
      (byte-compile-warn-x
       form
       "`make-variable-buffer-local' not called at toplevel"))
  (byte-compile-normal-call form))
(put 'make-variable-buffer-local
     'byte-hunk-handler 'byte-compile-form-make-variable-buffer-local)
(defun byte-compile-form-make-variable-buffer-local (form)
  (byte-compile-keep-pending form 'byte-compile-normal-call))

;; Make `make-local-variable' declare the variable locally
;; dynamic - this suppresses some unnecessary warnings
(byte-defop-compiler-1 make-local-variable
                       byte-compile-make-local-variable)
(defun byte-compile-make-local-variable (form)
  (pcase form (`(,_ ',var) (byte-compile--declare-var var)))
  (byte-compile-normal-call form))

;; Warn about mistakes in `defcustom', `defface', `defgroup', `define-widget'

(defvar bytecomp--cus-function)
(defvar bytecomp--cus-name)

(defun bytecomp--cus-warn (form format &rest args)
  "Emit a warning about a `defcustom' type.
FORM is used to provide location, `bytecomp--cus-function' and
`bytecomp--cus-name' for context."
  (let* ((actual-fun (or (cdr (assq bytecomp--cus-function
                                    '((custom-declare-group    . defgroup)
			              (custom-declare-face     . defface)
			              (custom-declare-variable . defcustom))))
                         bytecomp--cus-function))
         (prefix (format "in %s%s: "
                         actual-fun
                         (if bytecomp--cus-name
                             (format " for `%s'" bytecomp--cus-name)
                           ""))))
    (apply #'byte-compile-warn-x form (concat prefix format) args)))

(defun bytecomp--check-cus-type (type)
  "Warn about common mistakes in the `defcustom' type TYPE."
  (let ((invalid-types
         '(
           ;; Lisp type predicates, often confused with customization types:
           functionp numberp integerp fixnump natnump floatp booleanp
           characterp listp stringp consp vectorp symbolp keywordp
           hash-table-p facep
           ;; other mistakes occasionally seen (oh yes):
           or and nil t
           interger intger lits bool boolen constant filename
           kbd any list-of auto
           ;; from botched backquoting
           \, \,@ \`
           )))
    (cond
     ((consp type)
      (let* ((head (car type))
             (tail (cdr type)))
        (while (and (keywordp (car tail)) (cdr tail))
          (setq tail (cddr tail)))
        (cond
         ((plist-member (cdr type) :convert-widget) nil)
         ((let ((tl tail))
            (and (not (keywordp (car tail)))
                 (progn
                   (while (and tl (not (keywordp (car tl))))
                     (setq tl (cdr tl)))
                   (and tl
                        (progn
                          (bytecomp--cus-warn
                           tl "misplaced %S keyword in `%S' type" (car tl) head)
                          t))))))
         ((memq head '(choice radio))
          (unless tail
            (bytecomp--cus-warn type "`%S' without any types inside" head))
          (let ((clauses tail)
                (constants nil)
                (tags nil))
            (while clauses
              (let* ((ty (car clauses))
                     (ty-head (car-safe ty)))
                (when (and (eq ty-head 'other) (cdr clauses))
                  (bytecomp--cus-warn ty "`other' not last in `%S'" head))
                (when (memq ty-head '(const other))
                  (let ((ty-tail (cdr ty))
                        (val nil))
                    (while (and (keywordp (car ty-tail)) (cdr ty-tail))
                      (when (eq (car ty-tail) :value)
                        (setq val (cadr ty-tail)))
                      (setq ty-tail (cddr ty-tail)))
                    (when ty-tail
                      (setq val (car ty-tail)))
                    (when (member val constants)
                      (bytecomp--cus-warn
                       ty "duplicated value in `%S': `%S'" head val))
                    (push val constants)))
                (let ((tag (and (consp ty) (plist-get (cdr ty) :tag))))
                  (when (stringp tag)
                    (when (member tag tags)
                      (bytecomp--cus-warn
                       ty "duplicated :tag string in `%S': %S" head tag))
                    (push tag tags)))
                (bytecomp--check-cus-type ty))
              (setq clauses (cdr clauses)))))
         ((eq head 'cons)
          (unless (= (length tail) 2)
            (bytecomp--cus-warn
             type "`cons' requires 2 type specs, found %d" (length tail)))
          (dolist (ty tail)
            (bytecomp--check-cus-type ty)))
         ((memq head '(list group vector set repeat))
          (unless tail
            (bytecomp--cus-warn type "`%S' without type specs" head))
          (dolist (ty tail)
            (bytecomp--check-cus-type ty)))
         ((memq head '(alist plist))
          (let ((key-tag (memq :key-type (cdr type)))
                (value-tag (memq :value-type (cdr type))))
            (when key-tag
              (bytecomp--check-cus-type (cadr key-tag)))
            (when value-tag
              (bytecomp--check-cus-type (cadr value-tag)))))
         ((memq head '(const other))
          (let* ((value-tag (memq :value (cdr type)))
                 (n (length tail))
                 (val (car tail)))
            (cond
             ((or (> n 1) (and value-tag tail))
              (bytecomp--cus-warn type "`%S' with too many values" head))
             (value-tag
              (setq val (cadr value-tag)))
             ;; ;; This is a useful check but it results in perhaps
             ;; ;; a bit too many complaints.
             ;; ((null tail)
             ;;  (bytecomp--cus-warn
             ;;   type "`%S' without value is implicitly nil" head))
             )
            (when (memq (car-safe val) '(quote function))
              (bytecomp--cus-warn type "`%S' with quoted value: %S" head val))))
         ((eq head 'quote)
          (bytecomp--cus-warn type "type should not be quoted: %S" (cadr type)))
         ((memq head invalid-types)
          (bytecomp--cus-warn type "`%S' is not a valid type" head))
         ((or (not (symbolp head)) (keywordp head))
          (bytecomp--cus-warn type "irregular type `%S'" head))
         )))
     ((or (not (symbolp type)) (keywordp type))
      (bytecomp--cus-warn type "irregular type `%S'" type))
     ((memq type '( list cons group vector choice radio const other
                    function-item variable-item set repeat restricted-sexp))
      (bytecomp--cus-warn type "`%S' without arguments" type))
     ((memq type invalid-types)
      (bytecomp--cus-warn type "`%S' is not a valid type" type))
     )))

(defun bytecomp--check-cus-face-spec (spec)
  "Check for mistakes in a `defface' SPEC argument."
  (when (consp spec)
    (dolist (sp spec)
      (let ((display (car-safe sp))
            (atts (cdr-safe sp)))
        (cond ((listp display)
               (dolist (condition display)
                 (unless (memq (car-safe condition)
                               '(type class background min-colors supports))
                   (bytecomp--cus-warn
                    (list sp spec)
                    "Bad face display condition `%S'" (car condition)))))
              ((not (memq display '(t default)))
               (bytecomp--cus-warn
                (list sp spec) "Bad face display `%S'" display)))
        (when (and (consp atts) (null (cdr atts)))
          (setq atts (car atts)))       ; old (DISPLAY ATTS) syntax
        (while atts
          (let ((attr (car atts))
                (val (cadr atts)))
            (cond
             ((not (keywordp attr))
              (bytecomp--cus-warn
               (list atts sp spec)
               "Non-keyword in face attribute list: `%S'" attr))
             ((null (cdr atts))
              (bytecomp--cus-warn
               (list atts sp spec) "Missing face attribute `%s' value" attr))
             ((memq attr '( :inherit :extend
                            :family :foundry :width :height :weight :slant
                            :foreground :distant-foreground :background
                            :underline :overline :strike-through :box
                            :inverse-video :stipple :font
                            ;; FIXME: obsolete keywords, warn about them too?
                            :bold           ; :bold t   = :weight bold
                            :italic         ; :italic t = :slant italic
                            ))
              (when (eq (car-safe val) 'quote)
                (bytecomp--cus-warn
                 (list val atts sp spec)
                 "Value for face attribute `%s' should not be quoted" attr)))
             ((eq attr :reverse-video)
              (bytecomp--cus-warn
               (list atts sp spec)
               (concat "Face attribute `:reverse-video' has been removed;"
                       " use `:inverse-video' instead")))
             (t
              (bytecomp--cus-warn
               (list atts sp spec)
               "`%s' is not a valid face attribute keyword" attr))))
          (setq atts (cddr atts)))))))

;; Unified handler for multiple functions with similar arguments:
;; (NAME SOMETHING DOC KEYWORD-ARGS...)
(byte-defop-compiler-1 define-widget           bytecomp--custom-declare)
(byte-defop-compiler-1 custom-declare-group    bytecomp--custom-declare)
(byte-defop-compiler-1 custom-declare-face     bytecomp--custom-declare)
(byte-defop-compiler-1 custom-declare-variable bytecomp--custom-declare)
(defun bytecomp--custom-declare (form)
  (when (>= (length form) 4)
    (let* ((name-arg (nth 1 form))
           (name (and (eq (car-safe name-arg) 'quote)
                      (symbolp (nth 1 name-arg))
                      (nth 1 name-arg)))
           (keyword-args (nthcdr 4 form))
           (fun (car form))
           (bytecomp--cus-function fun)
           (bytecomp--cus-name name))

      ;; Check :type
      (when (memq fun '(custom-declare-variable define-widget))
        (let ((type-tag (memq :type keyword-args)))
          (if (null type-tag)
              ;; :type only mandatory for `defcustom'
              (when (eq fun 'custom-declare-variable)
                (bytecomp--cus-warn form "missing :type keyword parameter"))
            (let ((dup-type (memq :type (cdr type-tag))))
              (when dup-type
                (bytecomp--cus-warn
                 dup-type "duplicated :type keyword argument")))
            (let ((type-arg (cadr type-tag)))
              (when (or (null type-arg)
                        (eq (car-safe type-arg) 'quote))
                (bytecomp--check-cus-type (cadr type-arg)))))))

      (when (eq fun 'custom-declare-face)
        (let ((face-arg (nth 2 form)))
          (when (and (eq (car-safe face-arg) 'quote)
                     (consp (cdr face-arg))
                     (null (cddr face-arg)))
            (bytecomp--check-cus-face-spec (nth 1 face-arg)))))

      ;; Check :group
      (when (cond
             ((memq fun '(custom-declare-variable custom-declare-face))
              (not byte-compile-current-group))
             ((eq fun 'custom-declare-group)
              (not (eq name 'emacs))))
        (unless (plist-get keyword-args :group)
          (bytecomp--cus-warn form "fails to specify containing group")))

      ;; Update current group
      (when (and name
                 byte-compile-current-file  ; only when compiling a whole file
		 (eq fun 'custom-declare-group))
        (setq byte-compile-current-group name))

      ;; Check :local
      (when-let* ((val (and (eq fun 'custom-declare-variable)
                            (plist-get keyword-args :local)))
                  (_ (not (member val '(t 'permanent 'permanent-only)))))
        (bytecomp--cus-warn form ":local keyword does not accept %S" val))))

  (byte-compile-normal-call form))


(put 'function-put 'byte-hunk-handler 'byte-compile-define-symbol-prop)
(put 'define-symbol-prop 'byte-hunk-handler 'byte-compile-define-symbol-prop)
(defun byte-compile-define-symbol-prop (form)
  (pcase form
    ((and `(,op ,fun ,prop ,val)
          (guard (and (macroexp-const-p fun)
                      (macroexp-const-p prop)
                      (or (macroexp-const-p val)
                          ;; Also accept anonymous functions, since
                          ;; we're at top-level which implies they're
                          ;; also constants.
                          (pcase val (`(function (lambda . ,_)) t))))))
     (byte-compile-push-constant op)
     (byte-compile-form fun)
     (byte-compile-form prop)
     (let* ((fun (eval fun t))
            (prop (eval prop t))
            (val (if (macroexp-const-p val)
                     (eval val t)
                   (byte-compile-lambda (cadr val)))))
       (push `(,fun
               . (,prop ,val ,@(alist-get fun overriding-plist-environment)))
             overriding-plist-environment)
       (byte-compile-push-constant val)
       (byte-compile-out 'byte-call 3)
       nil))

    (_ (byte-compile-keep-pending form))))



;;; tags

;; Note: Most operations will strip off the 'TAG, but it speeds up
;; optimization to have the 'TAG as a part of the tag.
;; Tags will be (TAG . (tag-number . stack-depth)).
(defun byte-compile-make-tag ()
  (list 'TAG (setq byte-compile-tag-number (1+ byte-compile-tag-number))))


(defun byte-compile-out-tag (tag)
  (setq byte-compile-output (cons tag byte-compile-output))
  (if (cdr (cdr tag))
      (progn
	;; ## remove this someday
	(and byte-compile-depth
             (not (= (cdr (cdr tag)) byte-compile-depth))
             (error "Compiler bug: depth conflict at tag %d" (car (cdr tag))))
        (setq byte-compile-depth (cdr (cdr tag))))
    (setcdr (cdr tag) byte-compile-depth)))

(defun byte-compile-goto (opcode tag)
  (push (cons opcode tag) byte-compile-output)
  (setcdr (cdr tag) (if (memq opcode byte-goto-always-pop-ops)
			(1- byte-compile-depth)
		      byte-compile-depth))
  (setq byte-compile-depth (and (not (eq opcode 'byte-goto))
				(1- byte-compile-depth))))

(defun byte-compile-stack-adjustment (op operand)
  "Return the amount by which an operation adjusts the stack.
OP and OPERAND are as passed to `byte-compile-out'."
  (if (memq op '(byte-call byte-discardN byte-discardN-preserve-tos))
      ;; For calls, OPERAND is the number of args, so we pop OPERAND + 1
      ;; elements, and then push the result, for a total of -OPERAND.
      ;; For discardN*, of course, we just pop OPERAND elements.
      (- operand)
    (or (aref byte-stack+-info (symbol-value op))
	;; Ops with a nil entry in `byte-stack+-info' are byte-codes
	;; that take OPERAND values off the stack and push a result, for
	;; a total of 1 - OPERAND
	(- 1 operand))))

(defun byte-compile-out (op &optional operand)
  "Push the operation onto `byte-compile-output'.
OP is an opcode, a symbol.  OPERAND is either nil or a number or
a one-element list of a Lisp form."
  (when (and (consp operand) (null (cdr operand)))
    (setq operand (byte-run-strip-symbol-positions operand)))
  (push (cons op operand) byte-compile-output)
  (if (eq op 'byte-return)
      ;; This is actually an unnecessary case, because there should be no
      ;; more ops behind byte-return.
      (setq byte-compile-depth nil)
    (setq byte-compile-depth
	  (+ byte-compile-depth (byte-compile-stack-adjustment op operand)))
    (setq byte-compile-maxdepth (max byte-compile-depth byte-compile-maxdepth))
    ;;(if (< byte-compile-depth 0) (error "Compiler error: stack underflow"))
    ))

;;; call tree stuff

(defun byte-compile-annotate-call-tree (form)
  (let ((current-form (byte-run-strip-symbol-positions
                       byte-compile-current-form))
        (bare-car-form (byte-run-strip-symbol-positions (car form)))
        entry)
    ;; annotate the current call
    (if (setq entry (assq bare-car-form byte-compile-call-tree))
	(or (memq current-form (nth 1 entry)) ;callers
	    (setcar (cdr entry)
		    (cons current-form (nth 1 entry))))
      (setq byte-compile-call-tree
	    (cons (list bare-car-form (list current-form) nil)
		  byte-compile-call-tree)))
    ;; annotate the current function
    (if (setq entry (assq current-form byte-compile-call-tree))
	(or (memq bare-car-form (nth 2 entry)) ;called
	    (setcar (cdr (cdr entry))
		    (cons bare-car-form (nth 2 entry))))
      (setq byte-compile-call-tree
	    (cons (list current-form nil (list bare-car-form))
		  byte-compile-call-tree)))))

;; Renamed from byte-compile-report-call-tree
;; to avoid interfering with completion of byte-compile-file.
;;;###autoload
(defun display-call-tree (&optional filename)
  "Display a call graph of a specified file.
This lists which functions have been called, what functions called
them, and what functions they call.  The list includes all functions
whose definitions have been compiled in this Emacs session, as well as
all functions called by those functions.

The call graph does not include macros, inline functions, or
primitives that the byte-code interpreter knows about directly
\(`eq', `cons', etc.).

The call tree also lists those functions which are not known to be called
\(that is, to which no calls have been compiled), and which cannot be
invoked interactively."
  (interactive)
  (message "Generating call tree...")
  (with-output-to-temp-buffer "*Call-Tree*"
    (set-buffer "*Call-Tree*")
    (erase-buffer)
    (message "Generating call tree... (sorting on %s)"
	     (remove-pos-from-symbol byte-compile-call-tree-sort))
    (insert "Call tree for "
	    (cond ((null byte-compile-current-file) (or filename "???"))
		  ((stringp byte-compile-current-file)
		   byte-compile-current-file)
		  (t (buffer-name byte-compile-current-file)))
	    " sorted on "
	    (prin1-to-string (remove-pos-from-symbol
                              byte-compile-call-tree-sort))
	    ":\n\n")
    (if byte-compile-call-tree-sort
	(setq byte-compile-call-tree
	      (sort byte-compile-call-tree
		    (pcase byte-compile-call-tree-sort
                      ('callers
                       (lambda (x y) (< (length (nth 1 x))
                                        (length (nth 1 y)))))
                      ('calls
                       (lambda (x y) (< (length (nth 2 x))
                                        (length (nth 2 y)))))
                      ('calls+callers
                       (lambda (x y) (< (+ (length (nth 1 x))
                                           (length (nth 2 x)))
                                        (+ (length (nth 1 y))
                                           (length (nth 2 y))))))
                      ('name
                       (lambda (x y) (string< (car x) (car y))))
                      (_ (error "`byte-compile-call-tree-sort': `%s' - unknown sort mode"
                                (remove-pos-from-symbol
                                 byte-compile-call-tree-sort)))))))
    (message "Generating call tree...")
    (let ((rest byte-compile-call-tree)
	  (b (current-buffer))
	  f p
	  callers calls)
      (while rest
	(prin1 (car (car rest)) b)
	(setq callers (nth 1 (car rest))
	      calls (nth 2 (car rest)))
	(insert "\t"
	  (cond ((not (fboundp (setq f (car (car rest)))))
		 (if (null f)
		     " <top level>";; shouldn't insert nil then, actually -sk
		   " <not defined>"))
		((symbolp (setq f (symbol-function f))) ;; An alias.
		 (format " ==> %s" f))
		((not (consp f))
		 (format " <%s>" (type-of f)))
		((eq 'macro (car f))
		 (if (compiled-function-p (cdr f))
		     " <compiled macro>"
		   " <macro>"))
		((eq 'lambda (car f))
		 "<function>")
		(t "???"))
	  (format " (%d callers + %d calls = %d)"
		  ;; Does the optimizer eliminate common subexpressions?-sk
		  (length callers)
		  (length calls)
		  (+ (length callers) (length calls)))
	  "\n")
	(if callers
	    (progn
	      (insert "  called by:\n")
	      (setq p (point))
	      (insert "    " (if (car callers)
				 (mapconcat 'symbol-name callers ", ")
			       "<top level>"))
	      (let ((fill-prefix "    "))
		(fill-region-as-paragraph p (point)))
              (unless (= 0 (current-column))
                (insert "\n"))))
	(if calls
	    (progn
	      (insert "  calls:\n")
	      (setq p (point))
	      (insert "    " (mapconcat 'symbol-name calls ", "))
	      (let ((fill-prefix "    "))
		(fill-region-as-paragraph p (point)))
              (unless (= 0 (current-column))
                (insert "\n"))))
	(setq rest (cdr rest)))

      (message "Generating call tree...(finding uncalled functions...)")
      (setq rest byte-compile-call-tree)
      (let (uncalled def)
	(while rest
	  (or (nth 1 (car rest))
	      (null (setq f (caar rest)))
	      (progn
		(setq def (byte-compile-fdefinition f t))
		(and (eq (car-safe def) 'macro)
		     (eq (car-safe (cdr-safe def)) 'lambda)
		     (setq def (cdr def)))
		(functionp def))
	      (progn
		(setq def (byte-compile-fdefinition f nil))
		(and (eq (car-safe def) 'macro)
		     (eq (car-safe (cdr-safe def)) 'lambda)
		     (setq def (cdr def)))
		(commandp def))
	      (setq uncalled (cons f uncalled)))
	  (setq rest (cdr rest)))
	(if uncalled
	    (let ((fill-prefix "  "))
	      (insert "Noninteractive functions not known to be called:\n  ")
	      (setq p (point))
	      (insert (mapconcat 'symbol-name (nreverse uncalled) ", "))
	      (fill-region-as-paragraph p (point))))))
    (message "Generating call tree...done.")))


;;;###autoload
(defun batch-byte-compile-if-not-done ()
  "Like `byte-compile-file' but doesn't recompile if already up to date.
Use this from the command line, with `-batch';
it won't work in an interactive Emacs."
  (batch-byte-compile t))

;;; by crl@newton.purdue.edu
;;;  Only works noninteractively.
;;;###autoload
(defun batch-byte-compile (&optional noforce)
  "Run `byte-compile-file' on the files remaining on the command line.
Use this from the command line, with `-batch';
it won't work in an interactive Emacs.

Each file is processed even if an error occurred previously.  If
a file name denotes a directory, all Emacs Lisp source files in
that directory (that have previously been compiled) will be
recompiled if newer than the compiled files.  In this case,
NOFORCE is ignored.

For example, invoke \"emacs -batch -f batch-byte-compile $emacs/ ~/*.el\".

If NOFORCE is non-nil, don't recompile a file that seems to be
already up-to-date."
  ;; command-line-args-left is what is left of the command line, from
  ;; startup.el.
  (defvar command-line-args-left)	;Avoid 'free variable' warning
  (if (not noninteractive)
      (error "`batch-byte-compile' is to be used only with -batch"))
  ;; Better crash loudly than attempting to recover from undefined
  ;; behavior.
  (setq attempt-stack-overflow-recovery nil
        attempt-orderly-shutdown-on-fatal-signal nil)
  (let ((error nil))
    (while command-line-args-left
      (if (file-directory-p (expand-file-name (car command-line-args-left)))
	  ;; Directory as argument.
	  (let (source dest)
	    (dolist (file (directory-files (car command-line-args-left)))
	      (if (and (string-match emacs-lisp-file-regexp file)
		       (not (auto-save-file-name-p file))
		       (setq source
                             (expand-file-name file
                                               (car command-line-args-left)))
		       (setq dest (byte-compile-dest-file source))
		       (file-exists-p dest)
		       (file-newer-than-file-p source dest))
		  (if (null (batch-byte-compile-file source))
		      (setq error t)))))
	;; Specific file argument
	(if (or (not noforce)
		(let* ((source (car command-line-args-left))
		       (dest (byte-compile-dest-file source)))
		  (or (not (file-exists-p dest))
		      (file-newer-than-file-p source dest))))
	    (if (null (batch-byte-compile-file (car command-line-args-left)))
                (setq error t))))
      (setq command-line-args-left (cdr command-line-args-left)))
    (kill-emacs (if error 1 0))))

(defun batch-byte-compile-file (file)
  (let ((byte-compile-root-dir (or byte-compile-root-dir default-directory)))
    (if debug-on-error
        (byte-compile-file file)
      (condition-case err
          (byte-compile-file file)
        (file-error
         (message (if (cdr err)
                      ">>Error occurred processing %s: %s (%s)"
                    ">>Error occurred processing %s: %s")
                  file
                  (get (car err) 'error-message)
                  (prin1-to-string (cdr err)))
         (let ((destfile (byte-compile-dest-file file)))
           (if (file-exists-p destfile)
               (delete-file destfile)))
         nil)
        (error
         (message (if (cdr err)
                      ">>Error occurred processing %s: %s (%s)"
                    ">>Error occurred processing %s: %s")
                  file
                  (get (car err) 'error-message)
                  (prin1-to-string (cdr err)))
         nil)))))

(defun byte-compile-refresh-preloaded ()
  "Reload any Lisp file that was changed since Emacs was dumped.
Use with caution."
  (let* ((argv0 (car command-line-args))
         (emacs-file (or (and (fboundp 'pdumper-stats)
                              (cdr (nth 2 (pdumper-stats))))
                         (executable-find argv0))))
    (if (not (and emacs-file (file-exists-p emacs-file)))
        (message "Can't find %s to refresh preloaded Lisp files" argv0)
      (dolist (f (reverse load-history))
        (setq f (car f))
        (if (string-match "elc\\'" f) (setq f (substring f 0 -1)))
        (when (and (file-readable-p f)
                   (file-newer-than-file-p f emacs-file)
                   ;; Don't reload the source version of the files below
                   ;; because that causes subsequent byte-compilation to
                   ;; be a lot slower and need a higher max-lisp-eval-depth,
                   ;; so it can cause recompilation to fail.
                   (not (member (file-name-nondirectory f)
                                '("pcase.el" "bytecomp.el" "macroexp.el"
                                  "cconv.el" "byte-opt.el" "comp.el"))))
          (message "Reloading stale %s" (file-name-nondirectory f))
          (condition-case nil
              (load f 'noerror nil 'nosuffix)
            ;; Probably shouldn't happen, but in case of an error, it seems
            ;; at least as useful to ignore it as it is to stop compilation.
            (error nil)))))))

;;;###autoload
(defun batch-byte-recompile-directory (&optional arg)
  "Run `byte-recompile-directory' on the dirs remaining on the command line.
Must be used only with `-batch', and kills Emacs on completion.
For example, invoke `emacs -batch -f batch-byte-recompile-directory .'.

Optional argument ARG is passed as second argument ARG to
`byte-recompile-directory'; see there for its possible values
and corresponding effects."
  ;; command-line-args-left is what is left of the command line (startup.el)
  (defvar command-line-args-left)	;Avoid 'free variable' warning
  (if (not noninteractive)
      (error "batch-byte-recompile-directory is to be used only with -batch"))
  ;; Better crash loudly than attempting to recover from undefined
  ;; behavior.
  (setq attempt-stack-overflow-recovery nil
        attempt-orderly-shutdown-on-fatal-signal nil)
  (or command-line-args-left
      (setq command-line-args-left '(".")))
  (while command-line-args-left
    (byte-recompile-directory (car command-line-args-left) arg)
    (setq command-line-args-left (cdr command-line-args-left)))
  (kill-emacs 0))

;;; Core compiler macros.

(put 'featurep 'compiler-macro
     (lambda (form feature &rest rest)
       ;; Emacs-21's byte-code doesn't run under XEmacs or SXEmacs anyway, so
       ;; we can safely optimize away this test.
       (if (and (member feature '('xemacs 'sxemacs 'emacs)) (not rest))
           (featurep (cadr feature))
         form)))

(defmacro bytecomp--define-negated (fn arity negfn)
  "Define FN with ARITY as the Boolean negation of NEGFN."
  `(put ',fn 'compiler-macro
        (lambda (form &rest args)
          (if (= (length args) ,arity)
              (list 'not (cons ',negfn args))
            form))))

(bytecomp--define-negated /=     2 =    )
(bytecomp--define-negated atom   1 consp)
(bytecomp--define-negated nlistp 1 listp)

;; Report comma operator used outside of backquote.
;; Inside backquote, backquote will transform it before it gets here.

(put '\,  'compiler-macro #'bytecomp--report-comma)
(defun bytecomp--report-comma (form &rest _ignore)
  (macroexp-warn-and-return
   (format-message "`%s' called -- perhaps used not within backquote"
                   (car form))
   form (list 'suspicious (car form)) t))

;; Check for (in)comparable constant values in calls to `eq', `memq' etc.

(defun bytecomp--dodgy-eq-arg-p (x number-ok)
  "Whether X is a bad argument to `eq' (or `eql' if NUMBER-OK is non-nil)."
  (pcase x
    ((or `(quote ,(pred consp)) `(function (lambda . ,_))) t)
    ((or (pred consp) (pred symbolp)) nil)
    ((pred integerp)
     (not (or (<= -536870912 x 536870911) number-ok)))
    ((pred floatp) (not number-ok))
    (_ t)))

(defun bytecomp--value-type-description (x)
  (cond
    ((proper-list-p x) "list")
    ((recordp x) "record")
    (t (symbol-name (type-of x)))))

(defun bytecomp--arg-type-description (x)
  (pcase x
    (`(function (lambda . ,_)) "function")
    (`(quote . ,val) (bytecomp--value-type-description val))
    (_ (bytecomp--value-type-description x))))

(defun bytecomp--warn-dodgy-eq-arg (form type parenthesis)
  (macroexp-warn-and-return
   (format-message "`%s' called with literal %s that may never match (%s)"
                   (car form) type parenthesis)
   form (list 'suspicious (car form)) t))

(defun bytecomp--check-eq-args (form &optional a b &rest _ignore)
  (let* ((number-ok (eq (car form) 'eql))
         (bad-arg (cond ((bytecomp--dodgy-eq-arg-p a number-ok) 1)
                        ((bytecomp--dodgy-eq-arg-p b number-ok) 2))))
    (if bad-arg
        (bytecomp--warn-dodgy-eq-arg
         form
         (bytecomp--arg-type-description (nth bad-arg form))
         (format "arg %d" bad-arg))
      form)))

(put 'eq  'compiler-macro #'bytecomp--check-eq-args)
(put 'eql 'compiler-macro #'bytecomp--check-eq-args)

(defun bytecomp--check-memq-args (form &optional elem list &rest _ignore)
  (let* ((fn (car form))
         (number-ok (eq fn 'memql)))
    (cond
     ((bytecomp--dodgy-eq-arg-p elem number-ok)
      (bytecomp--warn-dodgy-eq-arg
       form (bytecomp--arg-type-description elem) "arg 1"))
     ((and (consp list) (eq (car list) 'quote)
           (proper-list-p (cadr list)))
      (named-let loop ((elts (cadr list)) (i 1))
        (if elts
            (let* ((elt (car elts))
                   (x (cond ((eq fn 'assq) (car-safe elt))
                            ((eq fn 'rassq) (cdr-safe elt))
                            (t elt))))
              (if (or (symbolp x)
                      (and (integerp x)
                           (or (<= -536870912 x 536870911) number-ok))
                      (and (floatp x) number-ok))
                  (loop (cdr elts) (1+ i))
                (bytecomp--warn-dodgy-eq-arg
                 form (bytecomp--value-type-description x)
                 (format "element %d of arg 2" i))))
          form)))
     (t form))))

(put 'memq  'compiler-macro #'bytecomp--check-memq-args)
(put 'memql 'compiler-macro #'bytecomp--check-memq-args)
(put 'assq  'compiler-macro #'bytecomp--check-memq-args)
(put 'rassq 'compiler-macro #'bytecomp--check-memq-args)
(put 'remq  'compiler-macro #'bytecomp--check-memq-args)
(put 'delq  'compiler-macro #'bytecomp--check-memq-args)

;; Implement `char-before', `backward-char' and `backward-word' in
;; terms of `char-after', `forward-char' and `forward-word' which have
;; their own byte-ops.

(put 'char-before 'compiler-macro #'bytecomp--char-before)
(defun bytecomp--char-before (form &optional arg &rest junk-args)
  (if junk-args
      form    ; arity error
    `(char-after (1- (or ,arg (point))))))

(put 'backward-char 'compiler-macro #'bytecomp--backward-char)
(defun bytecomp--backward-char (form &optional arg &rest junk-args)
  (if junk-args
      form    ; arity error
    `(forward-char (- (or ,arg 1)))))

(put 'backward-word 'compiler-macro #'bytecomp--backward-word)
(defun bytecomp--backward-word (form &optional arg &rest junk-args)
  (if junk-args
      form    ; arity error
    `(forward-word (- (or ,arg 1)))))

(defun bytecomp--check-keyword-args (form arglist allowed-keys required-keys)
  (let ((fun (car form)))
    (cl-flet ((missing (form keyword)
		(byte-compile-warn-x
		 form
		 "`%S´ called without required keyword argument %S"
		 fun keyword))
	      (unrecognized (form keyword)
		(byte-compile-warn-x
		 form
		 "`%S´ called with unknown keyword argument %S"
		 fun keyword))
	      (duplicate (form keyword)
		(byte-compile-warn-x
		 form
		 "`%S´ called with repeated keyword argument %S"
		 fun keyword))
              (missing-val (form keyword)
		(byte-compile-warn-x
		 form
		 "missing value for keyword argument %S"
		 keyword)))
      (let* ((seen '())
	     (l arglist))
	(while (consp l)
	  (let ((key (car l)))
	    (cond ((and (keywordp key) (memq key allowed-keys))
		   (cond ((memq key seen)
			  (duplicate l key))
			 (t
			  (push key seen))))
		  (t (unrecognized l key)))
            (when (null (cdr l))
              (missing-val l key)))
	  (setq l (cddr l)))
        (dolist (key required-keys)
	  (unless (memq key seen)
	    (missing form key))))))
  form)

(put 'make-process 'compiler-macro
     #'(lambda (form &rest args)
         (bytecomp--check-keyword-args
          form args
          '(:name
            :buffer :command :coding :noquery :stop :connection-type
            :filter :sentinel :stderr :file-handler)
          '(:name :command))))

(put 'make-pipe-process 'compiler-macro
     #'(lambda (form &rest args)
         (bytecomp--check-keyword-args
          form args
          '(:name :buffer :coding :noquery :stop :filter :sentinel)
          '(:name))))

(put 'make-network-process 'compiler-macro
     #'(lambda (form &rest args)
         (bytecomp--check-keyword-args
          form args
          '(:name
            :buffer :host :service :type :family :local :remote :coding
            :nowait :noquery :stop :filter :filter-multibyte :sentinel
            :log :plist :tls-parameters :server :broadcast :dontroute
            :keepalive :linger :oobinline :priority :reuseaddr :nodelay
            :bindtodevice :use-external-socket)
          '(:name :service))))

(provide 'byte-compile)
(provide 'bytecomp)


;;; report metering (see the hacks in bytecode.c)

(defvar byte-code-meter)
(defun byte-compile-report-ops ()
  (or (boundp 'byte-metering-on)
      (error "You must build Emacs with -DBYTE_CODE_METER to use this"))
  (with-output-to-temp-buffer "*Meter*"
    (set-buffer "*Meter*")
    (let ((i 0) n op off)
      (while (< i 256)
	(setq n (aref (aref byte-code-meter 0) i)
	      off nil)
	(if t				;(not (zerop n))
	    (progn
	      (setq op i)
	      (setq off nil)
	      (cond ((< op byte-nth)
		     (setq off (logand op 7))
		     (setq op (logand op 248)))
		    ((>= op byte-constant)
		     (setq off (- op byte-constant)
			   op byte-constant)))
	      (setq op (aref byte-code-vector op))
	      (insert (format "%-4d" i))
	      (insert (symbol-name op))
	      (if off (insert " [" (int-to-string off) "]"))
	      (indent-to 40)
	      (insert (int-to-string n) "\n")))
	(setq i (1+ i))))))

;; To avoid "lisp nesting exceeds max-lisp-eval-depth" when bytecomp compiles
;; itself, compile some of its most used recursive functions (at load time).
;;
(eval-when-compile
  (or (compiled-function-p (symbol-function 'byte-compile-form))
      (let ((byte-optimize nil)		; do it fast
	    (byte-compile-warnings nil))
	(mapc (lambda (x)
                (unless (native-comp-function-p x)
		  (or noninteractive (message "compiling %s..." x))
		  (byte-compile x)
		  (or noninteractive (message "compiling %s...done" x))))
	      '(byte-compile-normal-call
		byte-compile-form
		byte-compile-body
		;; Inserted some more than necessary, to speed it up.
		byte-compile-top-level
		byte-compile-out-toplevel
		byte-compile-constant
		byte-compile-variable-ref))))
  nil)

(make-obsolete-variable 'bytecomp-load-hook
                        "use `with-eval-after-load' instead." "28.1")
(run-hooks 'bytecomp-load-hook)

;;; bytecomp.el ends here
