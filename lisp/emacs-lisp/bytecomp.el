;;; bytecomp.el --- compilation of Lisp code into byte code

;; Copyright (C) 1985, 1986, 1987, 1992, 1994, 1998, 2000, 2001, 2002
;;   Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@lucid.com>
;;	Hallvard Furuseth <hbf@ulrik.uio.no>
;; Maintainer: FSF
;; Keywords: lisp

;;; This version incorporates changes up to version 2.10 of the
;;; Zawinski-Furuseth compiler.
(defconst byte-compile-version "$Revision: 2.96 $")

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

;; The Emacs Lisp byte compiler.  This crunches lisp source into a sort
;; of p-code (`lapcode') which takes up less space and can be interpreted
;; faster.  [`LAP' == `Lisp Assembly Program'.]
;; The user entry points are byte-compile-file and byte-recompile-directory.

;;; Code:

;; ========================================================================
;; Entry points:
;;	byte-recompile-directory, byte-compile-file,
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

;; User customization variables:
;;
;; byte-compile-verbose	Whether to report the function currently being
;;				compiled in the minibuffer;
;; byte-optimize		Whether to do optimizations; this may be
;;				t, nil, 'source, or 'byte;
;; byte-optimize-log		Whether to report (in excruciating detail)
;;				exactly which optimizations have been made.
;;				This may be t, nil, 'source, or 'byte;
;; byte-compile-error-on-warn	Whether to stop compilation when a warning is
;;				produced;
;; byte-compile-delete-errors	Whether the optimizer may delete calls or
;;				variable references that are side-effect-free
;;				except that they may return an error.
;; byte-compile-generate-call-tree	Whether to generate a histogram of
;;				function calls.  This can be useful for
;;				finding unused functions, as well as simple
;;				performance metering.
;; byte-compile-warnings	List of warnings to issue, or t.  May contain
;;				'free-vars (references to variables not in the
;;					    current lexical scope)
;;				'unresolved (calls to unknown functions)
;;				'callargs  (lambda calls with args that don't
;;					    match the lambda's definition)
;;				'redefine  (function cell redefined from
;;					    a macro to a lambda or vice versa,
;;					    or redefined to take other args)
;;				'obsolete  (obsolete variables and functions)
;;				'noruntime (calls to functions only defined
;;					    within `eval-when-compile')
;; byte-compile-compatibility	Whether the compiler should
;;				generate .elc files which can be loaded into
;;				generic emacs 18.
;; emacs-lisp-file-regexp	Regexp for the extension of source-files;
;;				see also the function byte-compile-dest-file.

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
;;	in without having to load that file before compiling it.  the
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

(or (fboundp 'defsubst)
    ;; This really ought to be loaded already!
    (load-library "byte-run"))

;; The feature of compiling in a specific target Emacs version
;; has been turned off because compile time options are a bad idea.
(defmacro byte-compile-single-version () nil)
(defmacro byte-compile-version-cond (cond) cond)

;; The crud you see scattered through this file of the form
;;   (or (and (boundp 'epoch::version) epoch::version)
;;	  (string-lessp emacs-version "19"))
;; is because the Epoch folks couldn't be bothered to follow the
;; normal emacs version numbering convention.

;; (if (byte-compile-version-cond
;;      (or (and (boundp 'epoch::version) epoch::version)
;; 	 (string-lessp emacs-version "19")))
;;     (progn
;;       ;; emacs-18 compatibility.
;;       (defvar baud-rate (baud-rate))	;Define baud-rate if it's undefined
;;
;;       (if (byte-compile-single-version)
;; 	  (defmacro byte-code-function-p (x) "Emacs 18 doesn't have these." nil)
;; 	(defun byte-code-function-p (x) "Emacs 18 doesn't have these." nil))
;;
;;       (or (and (fboundp 'member)
;; 	       ;; avoid using someone else's possibly bogus definition of this.
;; 	       (subrp (symbol-function 'member)))
;; 	  (defun member (elt list)
;; 	    "like memq, but uses equal instead of eq.  In v19, this is a subr."
;; 	    (while (and list (not (equal elt (car list))))
;; 	      (setq list (cdr list)))
;; 	    list))))


(defgroup bytecomp nil
  "Emacs Lisp byte-compiler"
  :group 'lisp)

(defcustom emacs-lisp-file-regexp (if (eq system-type 'vax-vms)
				      "\\.EL\\(;[0-9]+\\)?$"
				    "\\.el$")
  "*Regexp which matches Emacs Lisp source files.
You may want to redefine the function `byte-compile-dest-file'
if you change this variable."
  :group 'bytecomp
  :type 'regexp)

;; This enables file name handlers such as jka-compr
;; to remove parts of the file name that should not be copied
;; through to the output file name.
(defun byte-compiler-base-file-name (filename)
  (let ((handler (find-file-name-handler filename
					 'byte-compiler-base-file-name)))
    (if handler
	(funcall handler 'byte-compiler-base-file-name filename)
      filename)))

(or (fboundp 'byte-compile-dest-file)
    ;; The user may want to redefine this along with emacs-lisp-file-regexp,
    ;; so only define it if it is undefined.
    (defun byte-compile-dest-file (filename)
      "Convert an Emacs Lisp source file name to a compiled file name."
      (setq filename (byte-compiler-base-file-name filename))
      (setq filename (file-name-sans-versions filename))
      (cond ((eq system-type 'vax-vms)
	     (concat (substring filename 0 (string-match ";" filename)) "c"))
	    ((string-match emacs-lisp-file-regexp filename)
	     (concat (substring filename 0 (match-beginning 0)) ".elc"))
	    (t (concat filename ".elc")))))

;; This can be the 'byte-compile property of any symbol.
(autoload 'byte-compile-inline-expand "byte-opt")

;; This is the entrypoint to the lapcode optimizer pass1.
(autoload 'byte-optimize-form "byte-opt")
;; This is the entrypoint to the lapcode optimizer pass2.
(autoload 'byte-optimize-lapcode "byte-opt")
(autoload 'byte-compile-unfold-lambda "byte-opt")

;; This is the entry point to the decompiler, which is used by the
;; disassembler.  The disassembler just requires 'byte-compile, but
;; that doesn't define this function, so this seems to be a reasonable
;; thing to do.
(autoload 'byte-decompile-bytecode "byte-opt")

(defcustom byte-compile-verbose
  (and (not noninteractive) (> baud-rate search-slow-speed))
  "*Non-nil means print messages describing progress of byte-compiler."
  :group 'bytecomp
  :type 'boolean)

(defcustom byte-compile-compatibility nil
  "*Non-nil means generate output that can run in Emacs 18."
  :group 'bytecomp
  :type 'boolean)

;; (defvar byte-compile-generate-emacs19-bytecodes
;;         (not (or (and (boundp 'epoch::version) epoch::version)
;; 		 (string-lessp emacs-version "19")))
;;   "*If this is true, then the byte-compiler will generate bytecode which
;; makes use of byte-ops which are present only in Emacs 19.  Code generated
;; this way can never be run in Emacs 18, and may even cause it to crash.")

(defcustom byte-optimize t
  "*Enables optimization in the byte compiler.
nil means don't do any optimization.
t means do all optimizations.
`source' means do source-level optimizations only.
`byte' means do code-level optimizations only."
  :group 'bytecomp
  :type '(choice (const :tag "none" nil)
		 (const :tag "all" t)
		 (const :tag "source-level" source)
		 (const :tag "byte-level" byte)))

(defcustom byte-compile-delete-errors t
  "*If non-nil, the optimizer may delete forms that may signal an error.
This includes variable references and calls to functions such as `car'."
  :group 'bytecomp
  :type 'boolean)

(defvar byte-compile-dynamic nil
  "If non-nil, compile function bodies so they load lazily.
They are hidden in comments in the compiled file,
and each one is brought into core when the
function is called.

To enable this option, make it a file-local variable
in the source file you want it to apply to.
For example, add  -*-byte-compile-dynamic: t;-*- on the first line.

When this option is true, if you load the compiled file and then move it,
the functions you loaded will not be able to run.")

(defcustom byte-compile-dynamic-docstrings t
  "*If non-nil, compile doc strings for lazy access.
We bury the doc strings of functions and variables
inside comments in the file, and bring them into core only when they
are actually needed.

When this option is true, if you load the compiled file and then move it,
you won't be able to find the documentation of anything in that file.

To disable this option for a certain file, make it a file-local variable
in the source file.  For example, add this to the first line:
  -*-byte-compile-dynamic-docstrings:nil;-*-
You can also set the variable globally.

This option is enabled by default because it reduces Emacs memory usage."
  :group 'bytecomp
  :type 'boolean)

(defcustom byte-optimize-log nil
  "*If true, the byte-compiler will log its optimizations into *Compile-Log*.
If this is 'source, then only source-level optimizations will be logged.
If it is 'byte, then only byte-level optimizations will be logged."
  :group 'bytecomp
  :type '(choice (const :tag "none" nil)
		 (const :tag "all" t)
		 (const :tag "source-level" source)
		 (const :tag "byte-level" byte)))

(defcustom byte-compile-error-on-warn nil
  "*If true, the byte-compiler reports warnings with `error'."
  :group 'bytecomp
  :type 'boolean)

(defconst byte-compile-warning-types
  '(redefine callargs free-vars unresolved obsolete noruntime))
(defcustom byte-compile-warnings t
  "*List of warnings that the byte-compiler should issue (t for all).
Elements of the list may be be:

  free-vars   references to variables not in the current lexical scope.
  unresolved  calls to unknown functions.
  callargs    lambda calls with args that don't match the definition.
  redefine    function cell redefined from a macro to a lambda or vice
              versa, or redefined to take a different number of arguments.
  obsolete    obsolete variables and functions."
  :group 'bytecomp
  :type '(choice (const :tag "All" t)
		 (set :menu-tag "Some"
		      (const free-vars) (const unresolved)
		      (const callargs) (const redefined)
		      (const obsolete) (const noruntime))))

(defcustom byte-compile-generate-call-tree nil
  "*Non-nil means collect call-graph information when compiling.
This records functions were called and from where.
If the value is t, compilation displays the call graph when it finishes.
If the value is neither t nor nil, compilation asks you whether to display
the graph.

The call tree only lists functions called, not macros used. Those functions
which the byte-code interpreter knows about directly (eq, cons, etc.) are
not reported.

The call tree also lists those functions which are not known to be called
\(that is, to which no calls have been compiled).  Functions which can be
invoked interactively are excluded from this list."
  :group 'bytecomp
  :type '(choice (const :tag "Yes" t) (const :tag "No" nil)
		 (other :tag "Ask" lambda)))

(defvar byte-compile-call-tree nil "Alist of functions and their call tree.
Each element looks like

  \(FUNCTION CALLERS CALLS\)

where CALLERS is a list of functions that call FUNCTION, and CALLS
is a list of functions for which calls were generated while compiling
FUNCTION.")

(defcustom byte-compile-call-tree-sort 'name
  "*If non-nil, sort the call tree.
The values `name', `callers', `calls', `calls+callers'
specify different fields to sort on."
  :group 'bytecomp
  :type '(choice (const name) (const callers) (const calls)
		 (const calls+callers) (const nil)))

(defvar byte-compile-debug nil)

;; (defvar byte-compile-overwrite-file t
;;   "If nil, old .elc files are deleted before the new is saved, and .elc
;; files will have the same modes as the corresponding .el file.  Otherwise,
;; existing .elc files will simply be overwritten, and the existing modes
;; will not be changed.  If this variable is nil, then an .elc file which
;; is a symbolic link will be turned into a normal file, instead of the file
;; which the link points to being overwritten.")

(defvar byte-compile-constants nil
  "List of all constants encountered during compilation of this form.")
(defvar byte-compile-variables nil
  "List of all variables encountered during compilation of this form.")
(defvar byte-compile-bound-variables nil
  "List of variables bound in the context of the current form.
This list lives partly on the stack.")
(defvar byte-compile-free-references)
(defvar byte-compile-free-assignments)

(defvar byte-compiler-error-flag)

(defconst byte-compile-initial-macro-environment
  '(
;;     (byte-compiler-options . (lambda (&rest forms)
;; 			       (apply 'byte-compiler-options-handler forms)))
    (eval-when-compile . (lambda (&rest body)
			   (list 'quote
				 (byte-compile-eval (byte-compile-top-level
						     (cons 'progn body))))))
    (eval-and-compile . (lambda (&rest body)
			  (eval (cons 'progn body))
			  (cons 'progn body))))
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
\(FUNCTIONNAME . nil) when a function is redefined as a macro.")

(defvar byte-compile-unresolved-functions nil
  "Alist of undefined functions to which calls have been compiled.
Used for warnings when the function is not known to be defined or is later
defined with incorrect args.")

(defvar byte-compile-tag-number 0)
(defvar byte-compile-output nil
  "Alist describing contents to put in byte code string.
Each element is (INDEX . VALUE)")
(defvar byte-compile-depth 0 "Current depth of execution stack.")
(defvar byte-compile-maxdepth 0 "Maximum depth of execution stack.")


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


;; unused: 0-7

;; These opcodes are special in that they pack their argument into the
;; opcode word.
;;
(byte-defop   8  1 byte-varref	"for variable reference")
(byte-defop  16 -1 byte-varset	"for setting a variable")
(byte-defop  24 -1 byte-varbind	"for binding a variable")
(byte-defop  32  0 byte-call	"for calling a function")
(byte-defop  40  0 byte-unbind	"for unbinding special bindings")
;; codes 8-47 are consumed by the preceding opcodes

;; unused: 48-55

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
(byte-defop  75  0 byte-symbol-function) ; this was commented out
(byte-defop  76 -1 byte-set)
(byte-defop  77 -1 byte-fset) ; this was commented out
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
(byte-defop  95 -1 byte-mult) ; v19 only
(byte-defop  96  1 byte-point)
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
  "To make a binding to record the current buffer")
(byte-defop 115  0 byte-set-mark-OBSOLETE)
(byte-defop 116  1 byte-interactive-p)

;; These ops are new to v19
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
   "for reference to a constant with vector index >= byte-constant-limit")
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
(byte-defop 139  0 byte-save-window-excursion
  "to make a binding to record entire window configuration")
(byte-defop 140  0 byte-save-restriction
  "to make a binding to record the current buffer clipping restrictions")
(byte-defop 141 -1 byte-catch
  "for catch.  Takes, on stack, the tag and an expression for the body")
(byte-defop 142 -1 byte-unwind-protect
  "for unwind-protect.  Takes, on stack, an expression for the unwind-action")

;; For condition-case.  Takes, on stack, the variable to bind,
;; an expression for the body, and a list of clauses.
(byte-defop 143 -2 byte-condition-case)

;; For entry to with-output-to-temp-buffer.
;; Takes, on stack, the buffer name.
;; Binds standard-output and does some other things.
;; Returns with temp buffer on the stack in place of buffer name.
(byte-defop 144  0 byte-temp-output-buffer-setup)

;; For exit from with-output-to-temp-buffer.
;; Expects the temp buffer on the stack underneath value to return.
;; Pops them both, then pushes the value back on.
;; Unbinds standard-output and makes the temp buffer visible.
(byte-defop 145 -1 byte-temp-output-buffer-show)

;; these ops are new to v19

;; To unbind back to the beginning of this frame.
;; Not used yet, but will be needed for tail-recursion elimination.
(byte-defop 146  0 byte-unbind-all)

;; these ops are new to v19
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

;; unused: 178-191

(byte-defop 192  1 byte-constant	"for reference to a constant")
;; codes 193-255 are consumed by byte-constant.
(defconst byte-constant-limit 64
  "Exclusive maximum index usable in the `byte-constant' opcode.")

(defconst byte-goto-ops '(byte-goto byte-goto-if-nil byte-goto-if-not-nil
			  byte-goto-if-nil-else-pop
			  byte-goto-if-not-nil-else-pop)
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

(defun byte-compile-lapcode (lap)
  "Turns lapcode into bytecode.  The lapcode is destroyed."
  ;; Lapcode modifications: changes the ID of a tag to be the tag's PC.
  (let ((pc 0)			; Program counter
	op off			; Operation & offset
	(bytes '())		; Put the output bytes here
	(patchlist nil)		; List of tags and goto's to patch
	rest rel tmp)
    (while lap
      (setq op (car (car lap))
	    off (cdr (car lap)))
      (cond ((not (symbolp op))
	     (error "Non-symbolic opcode `%s'" op))
	    ((eq op 'TAG)
	     (setcar off pc)
	     (setq patchlist (cons off patchlist)))
	    ((memq op byte-goto-ops)
	     (setq pc (+ pc 3))
	     (setq bytes (cons (cons pc (cdr off))
			       (cons nil
				     (cons (symbol-value op) bytes))))
	     (setq patchlist (cons bytes patchlist)))
	    (t
	     (setq bytes
		   (cond ((cond ((consp off)
				 ;; Variable or constant reference
				 (setq off (cdr off))
				 (eq op 'byte-constant)))
			  (cond ((< off byte-constant-limit)
				 (setq pc (1+ pc))
				 (cons (+ byte-constant off) bytes))
				(t
				 (setq pc (+ 3 pc))
				 (cons (lsh off -8)
				       (cons (logand off 255)
					     (cons byte-constant2 bytes))))))
			 ((<= byte-listN (symbol-value op))
			  (setq pc (+ 2 pc))
			  (cons off (cons (symbol-value op) bytes)))
			 ((< off 6)
			  (setq pc (1+ pc))
			  (cons (+ (symbol-value op) off) bytes))
			 ((< off 256)
			  (setq pc (+ 2 pc))
			  (cons off (cons (+ (symbol-value op) 6) bytes)))
			 (t
			  (setq pc (+ 3 pc))
			  (cons (lsh off -8)
				(cons (logand off 255)
				      (cons (+ (symbol-value op) 7)
					    bytes))))))))
      (setq lap (cdr lap)))
    ;;(if (not (= pc (length bytes)))
    ;;    (error "Compiler error: pc mismatch - %s %s" pc (length bytes)))
    ;; Patch PC into jumps
    (let (bytes)
      (while patchlist
	(setq bytes (car patchlist))
	(cond ((atom (car bytes)))	; Tag
	      (t			; Absolute jump
	       (setq pc (car (cdr (car bytes))))	; Pick PC from tag
	       (setcar (cdr bytes) (logand pc 255))
	       (setcar bytes (lsh pc -8))))
	(setq patchlist (cdr patchlist))))
    (concat (nreverse bytes))))


;;; compile-time evaluation

(defun byte-compile-eval (form)
  "Eval FORM and mark the functions defined therein.
Each function's symbol gets marked with the `byte-compile-noruntime' property."
  (let ((hist-orig load-history)
	(hist-nil-orig current-load-list))
    (prog1 (eval form)
      (when (memq 'noruntime byte-compile-warnings)
	(let ((hist-new load-history)
	      (hist-nil-new current-load-list))
	  ;; Go through load-history, look for newly loaded files
	  ;; and mark all the functions defined therein.
	  (while (and hist-new (not (eq hist-new hist-orig)))
	    (let ((xs (pop hist-new)))
	      ;; Make sure the file was not already loaded before.
	      (unless (assoc (car xs) hist-orig)
		(dolist (s xs)
		  (cond
		   ((symbolp s) (put s 'byte-compile-noruntime t))
		   ((and (consp s) (eq 'autoload (car s)))
		    (put (cdr s) 'byte-compile-noruntime t)))))))
	  ;; Go through current-load-list for the locally defined funs.
	  (while (and hist-nil-new (not (eq hist-nil-new hist-nil-orig)))
	    (let ((s (pop hist-nil-new)))
	      (when (symbolp s)
		(put s 'byte-compile-noruntime t)))))))))


;;; byte compiler messages

(defvar byte-compile-current-form nil)
(defvar byte-compile-dest-file nil)
(defvar byte-compile-current-file nil)
(defvar byte-compile-current-buffer nil)

(defmacro byte-compile-log (format-string &rest args)
  (list 'and
	'byte-optimize
	'(memq byte-optimize-log '(t source))
	(list 'let '((print-escape-newlines t)
		     (print-level 4)
		     (print-length 4))
	      (list 'byte-compile-log-1
		    (cons 'format
		      (cons format-string
			(mapcar
			 (lambda (x)
			   (if (symbolp x) (list 'prin1-to-string x) x))
			 args)))))))

(defvar byte-compile-last-warned-form nil)
(defvar byte-compile-last-logged-file nil)

(defvar byte-compile-read-position nil
  "Character position we began the last `read' from.")
(defvar byte-compile-last-position nil
  "Last known character position in the input.")

;; copied from gnus-util.el
(defun byte-compile-delete-first (elt list)
  (if (eq (car list) elt)
      (cdr list)
    (let ((total list))
      (while (and (cdr list)
		  (not (eq (cadr list) elt)))
	(setq list (cdr list)))
      (when (cdr list)
	(setcdr list (cddr list)))
      total)))

;; The purpose of this function is to iterate through the
;; `read-symbol-positions-list'.  Each time we process, say, a
;; function definition (`defun') we remove `defun' from
;; `read-symbol-positions-list', and set `byte-compile-last-position'
;; to that symbol's character position.  Similarly, if we encounter a
;; variable reference, like in (1+ foo), we remove `foo' from the
;; list.  If our current position is after the symbol's position, we
;; assume we've already passed that point, and look for the next
;; occurence of the symbol.
;; So your're probably asking yourself: Isn't this function a 
;; gross hack?  And the answer, of course, would be yes.
(defun byte-compile-set-symbol-position (sym &optional allow-previous)
  (when byte-compile-read-position
    (let ((last nil))
      (while (progn
	       (setq last byte-compile-last-position)
	       (let* ((entry (assq sym read-symbol-positions-list))
		      (cur (cdr entry)))
		 (setq byte-compile-last-position
		       (if cur
			   (+ byte-compile-read-position cur)
			 last))
		 (setq
		  read-symbol-positions-list
		  (byte-compile-delete-first entry read-symbol-positions-list)))
	       (or (and allow-previous (not (= last byte-compile-last-position)))
		   (> last byte-compile-last-position)))))))

(defun byte-compile-display-log-head-p ()
  (and (not (eq byte-compile-current-form :end))
       (or (and byte-compile-current-file
		(not (equal byte-compile-current-file
			    byte-compile-last-logged-file)))
	   (and byte-compile-last-warned-form
		(not (eq byte-compile-current-form
			 byte-compile-last-warned-form))))))

(defun byte-goto-log-buffer ()
  (set-buffer (get-buffer-create "*Compile-Log*"))
  (unless (eq major-mode 'compilation-mode)
    (compilation-mode)))

;; Log a message STRING in *Compile-Log*.
;; Also log the current function and file if not already done.
(defun byte-compile-log-1 (string &optional fill)
  (let* ((file (cond ((stringp byte-compile-current-file)
		      (format "%s:" byte-compile-current-file))
		     ((bufferp byte-compile-current-file)
		      (format "Buffer %s:"
			      (buffer-name byte-compile-current-file)))
		     (t "")))
	 (pos (if (and byte-compile-current-file
		       (integerp byte-compile-read-position))
		  (with-current-buffer byte-compile-current-buffer
		    (format "%d:%d:" (count-lines (point-min)
						  byte-compile-last-position)
			    (save-excursion
			      (goto-char byte-compile-last-position)
			      (1+ (current-column)))))
		""))
	 (form (or byte-compile-current-form "toplevel form")))
    (cond (noninteractive
	   (when (byte-compile-display-log-head-p)
	     (message "%s In %s" file form))
	   (message "%s%s %s" file pos string))
	  (t
	   (save-excursion
             (byte-goto-log-buffer)
	     (goto-char (point-max))
	     (when (byte-compile-display-log-head-p)
	       (insert (format "\nIn %s" form)))
	     (insert (format "\n%s%s\n%s\n" file pos string))
	     (when (and fill (not (string-match "\n" string)))
	       (let ((fill-prefix "     ") (fill-column 78))
		 (fill-paragraph nil)))))))
  (setq byte-compile-last-logged-file byte-compile-current-file
	byte-compile-last-warned-form byte-compile-current-form))

;; Log the start of a file in *Compile-Log*, and mark it as done.
;; But do nothing in batch mode.
(defun byte-compile-log-file ()
  (and byte-compile-current-file
       (not (equal byte-compile-current-file byte-compile-last-logged-file))
       (not noninteractive)
       (save-excursion
	 (byte-goto-log-buffer)
	 (goto-char (point-max))
	 (insert "\n\^L\nCompiling "
		 (if (stringp byte-compile-current-file)
		     (concat "file " byte-compile-current-file)
		   (concat "buffer " (buffer-name byte-compile-current-file)))
		 " at " (current-time-string) "\n")
	 (setq byte-compile-last-logged-file byte-compile-current-file))))

(defun byte-compile-warn (format &rest args)
  (setq format (apply 'format format args))
  (if byte-compile-error-on-warn
      (error "%s" format)		; byte-compile-file catches and logs it
    (byte-compile-log-1 (concat "warning: " format) t)
    ;; It is useless to flash warnings too fast to be read.
    ;; Besides, they will all be shown at the end.
    ;; (or noninteractive  ; already written on stdout.
    ;;	   (message "Warning: %s" format))
    ))

;;; This function should be used to report errors that have halted
;;; compilation of the current file.
(defun byte-compile-report-error (error-info)
  (setq byte-compiler-error-flag t)
  (byte-compile-log-1
   (concat "error: "
	   (format (if (cdr error-info) "%s (%s)" "%s")
		   (downcase (get (car error-info) 'error-message))
		   (prin1-to-string (cdr error-info))))))

;;; Used by make-obsolete.
(defun byte-compile-obsolete (form)
  (let* ((new (get (car form) 'byte-obsolete-info))
	 (handler (nth 1 new))
	 (when (nth 2 new)))
    (byte-compile-set-symbol-position (car form))
    (if (memq 'obsolete byte-compile-warnings)
	(byte-compile-warn "%s is an obsolete function%s; %s" (car form)
			   (if when (concat " since " when) "")
			   (if (stringp (car new))
			       (car new)
			     (format "use %s instead." (car new)))))
    (funcall (or handler 'byte-compile-normal-call) form)))

;; Compiler options

;; (defvar byte-compiler-valid-options
;;   '((optimize byte-optimize (t nil source byte) val)
;;     (file-format byte-compile-compatibility (emacs18 emacs19)
;; 		 (eq val 'emacs18))
;; ;;     (new-bytecodes byte-compile-generate-emacs19-bytecodes (t nil) val)
;;     (delete-errors byte-compile-delete-errors (t nil) val)
;;     (verbose byte-compile-verbose (t nil) val)
;;     (warnings byte-compile-warnings ((callargs redefine free-vars unresolved))
;; 	      val)))

;; Inhibit v18/v19 selectors if the version is hardcoded.
;; #### This should print a warning if the user tries to change something
;; than can't be changed because the running compiler doesn't support it.
;; (cond
;;  ((byte-compile-single-version)
;;   (setcar (cdr (cdr (assq 'new-bytecodes byte-compiler-valid-options)))
;; 	  (list (byte-compile-version-cond
;; 		 byte-compile-generate-emacs19-bytecodes)))
;;   (setcar (cdr (cdr (assq 'file-format byte-compiler-valid-options)))
;; 	  (if (byte-compile-version-cond byte-compile-compatibility)
;; 	      '(emacs18) '(emacs19)))))

;; (defun byte-compiler-options-handler (&rest args)
;;   (let (key val desc choices)
;;     (while args
;;       (if (or (atom (car args)) (nthcdr 2 (car args)) (null (cdr (car args))))
;; 	  (error "Malformed byte-compiler option `%s'" (car args)))
;;       (setq key (car (car args))
;; 	    val (car (cdr (car args)))
;; 	    desc (assq key byte-compiler-valid-options))
;;       (or desc
;; 	  (error "Unknown byte-compiler option `%s'" key))
;;       (setq choices (nth 2 desc))
;;       (if (consp (car choices))
;; 	  (let (this
;; 		(handler 'cons)
;; 		(ret (and (memq (car val) '(+ -))
;; 			  (copy-sequence (if (eq t (symbol-value (nth 1 desc)))
;; 					     choices
;; 					   (symbol-value (nth 1 desc)))))))
;; 	    (setq choices (car  choices))
;; 	    (while val
;; 	      (setq this (car val))
;; 	      (cond ((memq this choices)
;; 		     (setq ret (funcall handler this ret)))
;; 		    ((eq this '+) (setq handler 'cons))
;; 		    ((eq this '-) (setq handler 'delq))
;; 		    ((error "`%s' only accepts %s" key choices)))
;; 	      (setq val (cdr val)))
;; 	    (set (nth 1 desc) ret))
;; 	(or (memq val choices)
;; 	    (error "`%s' must be one of `%s'" key choices))
;; 	(set (nth 1 desc) (eval (nth 3 desc))))
;;       (setq args (cdr args)))
;;     nil))

;;; sanity-checking arglists

(defun byte-compile-fdefinition (name macro-p)
  (let* ((list (if macro-p
		   byte-compile-macro-environment
		 byte-compile-function-environment))
	 (env (cdr (assq name list))))
    (or env
	(let ((fn name))
	  (while (and (symbolp fn)
		      (fboundp fn)
		      (or (symbolp (symbol-function fn))
			  (consp (symbol-function fn))
			  (and (not macro-p)
			       (byte-code-function-p (symbol-function fn)))))
	    (setq fn (symbol-function fn)))
	  (if (and (not macro-p) (byte-code-function-p fn))
	      fn
	    (and (consp fn)
		 (if (eq 'macro (car fn))
		     (cdr fn)
		   (if macro-p
		       nil
		     (if (eq 'autoload (car fn))
			 nil
		       fn)))))))))

(defun byte-compile-arglist-signature (arglist)
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
	 (format "%d+" (car signature)))
	((= (car signature) (cdr signature))
	 (format "%d" (car signature)))
	(t (format "%d-%d" (car signature) (cdr signature)))))


;; Warn if the form is calling a function with the wrong number of arguments.
(defun byte-compile-callargs-warn (form)
  (let* ((def (or (byte-compile-fdefinition (car form) nil)
		  (byte-compile-fdefinition (car form) t)))
	 (sig (if def
		  (byte-compile-arglist-signature
		   (if (eq 'lambda (car-safe def))
		       (nth 1 def)
		     (if (byte-code-function-p def)
			 (aref def 0)
		       '(&rest def))))
		(if (and (fboundp (car form))
			 (subrp (symbol-function (car form))))
		    (subr-arity (symbol-function (car form))))))
	 (ncall (length (cdr form))))
    ;; Check many or unevalled from subr-arity.
    (if (and (cdr-safe sig)
	     (not (numberp (cdr sig))))
	(setcdr sig nil))
    (if sig
	(when (or (< ncall (car sig))
		(and (cdr sig) (> ncall (cdr sig))))
	  (byte-compile-set-symbol-position (car form))
	  (byte-compile-warn
	   "%s called with %d argument%s, but %s %s"
	   (car form) ncall
	   (if (= 1 ncall) "" "s")
	   (if (< ncall (car sig))
	       "requires"
	     "accepts only")
	   (byte-compile-arglist-signature-string sig)))
      (or (and (fboundp (car form))	; might be a subr or autoload.
	       (not (get (car form) 'byte-compile-noruntime)))
	  (eq (car form) byte-compile-current-form) ; ## this doesn't work
						    ; with recursion.
	  ;; It's a currently-undefined function.
	  ;; Remember number of args in call.
	  (let ((cons (assq (car form) byte-compile-unresolved-functions))
		(n (length (cdr form))))
	    (if cons
		(or (memq n (cdr cons))
		    (setcdr cons (cons n (cdr cons))))
		(setq byte-compile-unresolved-functions
		      (cons (list (car form) n)
			    byte-compile-unresolved-functions))))))))

;; Warn if the function or macro is being redefined with a different
;; number of arguments.
(defun byte-compile-arglist-warn (form macrop)
  (let ((old (byte-compile-fdefinition (nth 1 form) macrop)))
    (if old
	(let ((sig1 (byte-compile-arglist-signature
		      (if (eq 'lambda (car-safe old))
			  (nth 1 old)
			(if (byte-code-function-p old)
			    (aref old 0)
			  '(&rest def)))))
	      (sig2 (byte-compile-arglist-signature (nth 2 form))))
	  (unless (byte-compile-arglist-signatures-congruent-p sig1 sig2)
	    (byte-compile-set-symbol-position (nth 1 form))
	    (byte-compile-warn
	     "%s %s used to take %s %s, now takes %s"
	     (if (eq (car form) 'defun) "function" "macro")
	     (nth 1 form)
	     (byte-compile-arglist-signature-string sig1)
	     (if (equal sig1 '(1 . 1)) "argument" "arguments")
	     (byte-compile-arglist-signature-string sig2))))
      ;; This is the first definition.  See if previous calls are compatible.
      (let ((calls (assq (nth 1 form) byte-compile-unresolved-functions))
	    nums sig min max)
	(if calls
	    (progn
	      (setq sig (byte-compile-arglist-signature (nth 2 form))
		    nums (sort (copy-sequence (cdr calls)) (function <))
		    min (car nums)
		    max (car (nreverse nums)))
	      (when (or (< min (car sig))
		      (and (cdr sig) (> max (cdr sig))))
		(byte-compile-set-symbol-position (nth 1 form))
		(byte-compile-warn
		 "%s being defined to take %s%s, but was previously called with %s"
		 (nth 1 form)
		 (byte-compile-arglist-signature-string sig)
		 (if (equal sig '(1 . 1)) " arg" " args")
		 (byte-compile-arglist-signature-string (cons min max))))

	      (setq byte-compile-unresolved-functions
		    (delq calls byte-compile-unresolved-functions)))))
      )))

(defun byte-compile-print-syms (str1 strn syms)
  (when syms
    (byte-compile-set-symbol-position (car syms) t))
  (cond ((and (cdr syms) (not noninteractive))
	 (let* ((str strn)
		(L (length str))
		s)
	   (while syms
	     (setq s (symbol-name (pop syms))
		   L (+ L (length s) 2))
	     (if (< L (1- fill-column))
		 (setq str (concat str " " s (and syms ",")))
	       (setq str (concat str "\n    " s (and syms ","))
		     L (+ (length s) 4))))
	   (byte-compile-warn "%s" str)))
	((cdr syms)
	 (byte-compile-warn "%s %s"
			    strn
			    (mapconcat #'symbol-name syms ", ")))

	(syms
	 (byte-compile-warn str1 (car syms)))))

;; If we have compiled any calls to functions which are not known to be
;; defined, issue a warning enumerating them.
;; `unresolved' in the list `byte-compile-warnings' disables this.
(defun byte-compile-warn-about-unresolved-functions ()
  (when (memq 'unresolved byte-compile-warnings)
    (let ((byte-compile-current-form :end)
	  (noruntime nil)
	  (unresolved nil))
      ;; Separate the functions that will not be available at runtime
      ;; from the truly unresolved ones.
      (dolist (f byte-compile-unresolved-functions)
	(setq f (car f))
	(if (fboundp f) (push f noruntime) (push f unresolved)))
      ;; Complain about the no-run-time functions
      (byte-compile-print-syms
       "the function `%s' might not be defined at runtime."
       "the following functions might not be defined at runtime:"
       noruntime)
      ;; Complain about the unresolved functions
      (byte-compile-print-syms
       "the function `%s' is not known to be defined."
       "the following functions are not known to be defined:"
       unresolved)))
  nil)


(defsubst byte-compile-const-symbol-p (symbol)
  (or (memq symbol '(nil t))
      (keywordp symbol)))

(defmacro byte-compile-constp (form)
  "Return non-nil if FORM is a constant."
  `(cond ((consp ,form) (eq (car ,form) 'quote))
	 ((not (symbolp ,form)))
	 ((byte-compile-const-symbol-p ,form))))

(defmacro byte-compile-close-variables (&rest body)
  (cons 'let
	(cons '(;;
		;; Close over these variables to encapsulate the
		;; compilation state
		;;
		(byte-compile-macro-environment
		 ;; Copy it because the compiler may patch into the
		 ;; macroenvironment.
		 (copy-alist byte-compile-initial-macro-environment))
		(byte-compile-function-environment nil)
		(byte-compile-bound-variables nil)
		(byte-compile-free-references nil)
		(byte-compile-free-assignments nil)
		;;
		;; Close over these variables so that `byte-compiler-options'
		;; can change them on a per-file basis.
		;;
		(byte-compile-verbose byte-compile-verbose)
		(byte-optimize byte-optimize)
		(byte-compile-compatibility byte-compile-compatibility)
		(byte-compile-dynamic byte-compile-dynamic)
		(byte-compile-dynamic-docstrings
		 byte-compile-dynamic-docstrings)
;; 		(byte-compile-generate-emacs19-bytecodes
;; 		 byte-compile-generate-emacs19-bytecodes)
		(byte-compile-warnings (if (eq byte-compile-warnings t)
					   byte-compile-warning-types
					 byte-compile-warnings))
		)
	      body)))

(defvar byte-compile-warnings-point-max nil)
(defmacro displaying-byte-compile-warnings (&rest body)
  `(let ((byte-compile-warnings-point-max byte-compile-warnings-point-max))
     ;; Log the file name.
     (byte-compile-log-file)
     ;; Record how much is logged now.
     ;; We will display the log buffer if anything more is logged
     ;; before the end of BODY.
     (unless byte-compile-warnings-point-max
       (save-excursion
	 (byte-goto-log-buffer)
	 (setq byte-compile-warnings-point-max (point-max))))
     (unwind-protect
	 (let ((--displaying-byte-compile-warnings-fn (lambda ()
							,@body)))
	   (if byte-compile-debug
	       (funcall --displaying-byte-compile-warnings-fn)
	     (condition-case error-info
		 (funcall --displaying-byte-compile-warnings-fn)
	       (error (byte-compile-report-error error-info)))))
       (with-current-buffer "*Compile-Log*"
	 ;; If there were compilation warnings, display them.
	 (unless (= byte-compile-warnings-point-max (point-max))
	   (select-window
	    (prog1 (selected-window)
	      (select-window (display-buffer (current-buffer)))
	      (goto-char byte-compile-warnings-point-max)
	      (beginning-of-line)
	      (forward-line -1)
	      (recenter 0))))))))


;;;###autoload
(defun byte-force-recompile (directory)
  "Recompile every `.el' file in DIRECTORY that already has a `.elc' file.
Files in subdirectories of DIRECTORY are processed also."
  (interactive "DByte force recompile (directory): ")
  (byte-recompile-directory directory nil t))

;;;###autoload
(defun byte-recompile-directory (directory &optional arg force)
  "Recompile every `.el' file in DIRECTORY that needs recompilation.
This is if a `.elc' file exists but is older than the `.el' file.
Files in subdirectories of DIRECTORY are processed also.

If the `.elc' file does not exist, normally the `.el' file is *not* compiled.
But a prefix argument (optional second arg) means ask user,
for each such `.el' file, whether to compile it.  Prefix argument 0 means
don't ask and compile the file anyway.

A nonzero prefix argument also means ask about each subdirectory.

If the third argument FORCE is non-nil,
recompile every `.el' file that already has a `.elc' file."
  (interactive "DByte recompile directory: \nP")
  (if arg
      (setq arg (prefix-numeric-value arg)))
  (if noninteractive
      nil
    (save-some-buffers)
    (force-mode-line-update))
  (let ((directories (list (expand-file-name directory)))
        (skip-count 0)
        (fail-count 0)
	(file-count 0)
	(dir-count 0)
	last-dir)
    (displaying-byte-compile-warnings
     (while directories
       (setq directory (car directories))
       (message "Checking %s..." directory)
       (let ((files (directory-files directory))
	     source dest)
	 (dolist (file files)
	   (setq source (expand-file-name file directory))
	   (if (and (not (member file '("." ".." "RCS" "CVS")))
		    (file-directory-p source)
		    (not (file-symlink-p source)))
	       ;; This file is a subdirectory.  Handle them differently.
	       (when (or (null arg)
			 (eq 0 arg)
			 (y-or-n-p (concat "Check " source "? ")))
		 (setq directories
		       (nconc directories (list source))))
	     ;; It is an ordinary file.  Decide whether to compile it.
	     (if (and (string-match emacs-lisp-file-regexp source)
		      (file-readable-p source)
		      (not (auto-save-file-name-p source))
		      (setq dest (byte-compile-dest-file source))
		      (if (file-exists-p dest)
			  ;; File was already compiled.
			  (or force (file-newer-than-file-p source dest))
			;; No compiled file exists yet.
			(and arg
			     (or (eq 0 arg)
				 (y-or-n-p (concat "Compile " source "? "))))))
		 (progn (if (and noninteractive (not byte-compile-verbose))
			    (message "Compiling %s..." source))
                        (let ((res (byte-compile-file source)))
                          (cond ((eq res 'no-byte-compile)
                                 (setq skip-count (1+ skip-count)))
                                ((eq res t)
                                 (setq file-count (1+ file-count)))
                                ((eq res nil)
                                 (setq fail-count (1+ fail-count)))))
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
	     (if (> dir-count 1) (format " in %d directories" dir-count) ""))))

(defvar no-byte-compile nil
  "Non-nil to prevent byte-compiling of emacs-lisp code.
This is normally set in local file variables at the end of the elisp file:

;; Local Variables:\n;; no-byte-compile: t\n;; End: ")

;;;###autoload
(defun byte-compile-file (filename &optional load)
  "Compile a file of Lisp code named FILENAME into a file of byte code.
The output file's name is made by appending `c' to the end of FILENAME.
With prefix arg (noninteractively: 2nd arg), LOAD the file after compiling.
The value is non-nil if there were no errors, nil if errors."
;;  (interactive "fByte compile file: \nP")
  (interactive
   (let ((file buffer-file-name)
	 (file-name nil)
	 (file-dir nil))
     (and file
	  (eq (cdr (assq 'major-mode (buffer-local-variables)))
	      'emacs-lisp-mode)
	  (setq file-name (file-name-nondirectory file)
		file-dir (file-name-directory file)))
     (list (read-file-name (if current-prefix-arg
			       "Byte compile and load file: "
			     "Byte compile file: ")
			   file-dir file-name nil)
	   current-prefix-arg)))
  ;; Expand now so we get the current buffer's defaults
  (setq filename (expand-file-name filename))

  ;; If we're compiling a file that's in a buffer and is modified, offer
  ;; to save it first.
  (or noninteractive
      (let ((b (get-file-buffer (expand-file-name filename))))
	(if (and b (buffer-modified-p b)
		 (y-or-n-p (format "Save buffer %s first? " (buffer-name b))))
	    (save-excursion (set-buffer b) (save-buffer)))))

  (let ((byte-compile-current-file filename)
	(byte-compile-last-logged-file nil)
	(set-auto-coding-for-load t)
	target-file input-buffer output-buffer
	byte-compile-dest-file)
    (setq target-file (byte-compile-dest-file filename))
    (setq byte-compile-dest-file target-file)
    (save-excursion
      (setq input-buffer (get-buffer-create " *Compiler Input*"))
      (set-buffer input-buffer)
      (erase-buffer)
      (setq buffer-file-coding-system nil)
      ;; Always compile an Emacs Lisp file as multibyte
      ;; unless the file itself forces unibyte with -*-coding: raw-text;-*-
      (set-buffer-multibyte t)
      (insert-file-contents filename)
      ;; Mimic the way after-insert-file-set-buffer-file-coding-system
      ;; can make the buffer unibyte when visiting this file.
      (when (or (eq last-coding-system-used 'no-conversion)
		(eq (coding-system-type last-coding-system-used) 5))
	;; For coding systems no-conversion and raw-text...,
	;; edit the buffer as unibyte.
	(set-buffer-multibyte nil))
      ;; Run hooks including the uncompression hook.
      ;; If they change the file name, then change it for the output also.
      (let ((buffer-file-name filename)
	    (default-major-mode 'emacs-lisp-mode)
	    (enable-local-eval nil))
        (normal-mode)
        (setq filename buffer-file-name))
      ;; Set the default directory, in case an eval-when-compile uses it.
      (setq default-directory (file-name-directory filename)))
    ;; Check if the file's local variables explicitly specify not to
    ;; compile this file.
    (if (with-current-buffer input-buffer no-byte-compile)
	(progn
	  (message "%s not compiled because of `no-byte-compile: %s'"
		   (file-relative-name filename)
		   (with-current-buffer input-buffer no-byte-compile))
	  (if (file-exists-p target-file)
	      (condition-case nil (delete-file target-file) (error nil)))
	  ;; We successfully didn't compile this file.
	  'no-byte-compile)
      (when byte-compile-verbose
	(message "Compiling %s..." filename))
      (setq byte-compiler-error-flag nil)
      ;; It is important that input-buffer not be current at this call,
      ;; so that the value of point set in input-buffer
      ;; within byte-compile-from-buffer lingers in that buffer.
      (setq output-buffer (byte-compile-from-buffer input-buffer filename))
      (if byte-compiler-error-flag
	  nil
	(when byte-compile-verbose
	  (message "Compiling %s...done" filename))
	(kill-buffer input-buffer)
	(with-current-buffer output-buffer
	  (goto-char (point-max))
	  (insert "\n")			; aaah, unix.
	  (let ((vms-stmlf-recfm t))
	    (if (file-writable-p target-file)
		;; We must disable any code conversion here.
		(let ((coding-system-for-write 'no-conversion))
		  (if (memq system-type '(ms-dos 'windows-nt))
		      (setq buffer-file-type t))
		  (when (file-exists-p target-file)
		    ;; Remove the target before writing it, so that any
		    ;; hard-links continue to point to the old file (this makes
		    ;; it possible for installed files to share disk space with
		    ;; the build tree, without causing problems when emacs-lisp
		    ;; files in the build tree are recompiled).
		    (delete-file target-file))
		  (write-region 1 (point-max) target-file))
	      ;; This is just to give a better error message than write-region
	      (signal 'file-error
		      (list "Opening output file"
			    (if (file-exists-p target-file)
				"cannot overwrite file"
			      "directory not writable or nonexistent")
			    target-file))))
	  (kill-buffer (current-buffer)))
	(if (and byte-compile-generate-call-tree
		 (or (eq t byte-compile-generate-call-tree)
		     (y-or-n-p (format "Report call tree for %s? " filename))))
	    (save-excursion
	      (display-call-tree filename)))
	(if load
	    (load target-file))
	t))))

;;(defun byte-compile-and-load-file (&optional filename)
;;  "Compile a file of Lisp code named FILENAME into a file of byte code,
;;and then load it.  The output file's name is made by appending \"c\" to
;;the end of FILENAME."
;;  (interactive)
;;  (if filename ; I don't get it, (interactive-p) doesn't always work
;;      (byte-compile-file filename t)
;;    (let ((current-prefix-arg '(4)))
;;      (call-interactively 'byte-compile-file))))

;;(defun byte-compile-buffer (&optional buffer)
;;  "Byte-compile and evaluate contents of BUFFER (default: the current buffer)."
;;  (interactive "bByte compile buffer: ")
;;  (setq buffer (if buffer (get-buffer buffer) (current-buffer)))
;;  (message "Compiling %s..." (buffer-name buffer))
;;  (let* ((filename (or (buffer-file-name buffer)
;;		       (concat "#<buffer " (buffer-name buffer) ">")))
;;	 (byte-compile-current-file buffer))
;;    (byte-compile-from-buffer buffer nil))
;;  (message "Compiling %s...done" (buffer-name buffer))
;;  t)

;;; compiling a single function
;;;###autoload
(defun compile-defun (&optional arg)
  "Compile and evaluate the current top-level form.
Print the result in the minibuffer.
With argument, insert value in current buffer after the form."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (beginning-of-defun)
    (let* ((byte-compile-current-file nil)
	   (byte-compile-current-buffer (current-buffer))
	   (byte-compile-read-position (point))
	   (byte-compile-last-position byte-compile-read-position)
	   (byte-compile-last-warned-form 'nothing)
	   (value (eval
		   (let ((read-with-symbol-positions inbuffer)
			 (read-symbol-positions-list nil))
		     (displaying-byte-compile-warnings
		      (byte-compile-sexp (read (current-buffer))))))))
      (cond (arg
	     (message "Compiling from buffer... done.")
	     (prin1 value (current-buffer))
	     (insert "\n"))
	    ((message "%s" (prin1-to-string value)))))))


(defun byte-compile-from-buffer (inbuffer &optional filename)
  ;; Filename is used for the loading-into-Emacs-18 error message.
  (let (outbuffer
	(byte-compile-current-buffer inbuffer)
	(byte-compile-read-position nil)
	(byte-compile-last-position nil)
	;; Prevent truncation of flonums and lists as we read and print them
	(float-output-format nil)
	(case-fold-search nil)
	(print-length nil)
	(print-level nil)
	;; Prevent edebug from interfering when we compile
	;; and put the output into a file.
;; 	(edebug-all-defs nil)
;; 	(edebug-all-forms nil)
	;; Simulate entry to byte-compile-top-level
	(byte-compile-constants nil)
	(byte-compile-variables nil)
	(byte-compile-tag-number 0)
	(byte-compile-depth 0)
	(byte-compile-maxdepth 0)
	(byte-compile-output nil)
	;; This allows us to get the positions of symbols read; it's
	;; new in Emacs 21.4.
	(read-with-symbol-positions inbuffer)
	(read-symbol-positions-list nil)
	;;	  #### This is bound in b-c-close-variables.
	;;	  (byte-compile-warnings (if (eq byte-compile-warnings t)
	;;				     byte-compile-warning-types
	;;				   byte-compile-warnings))
	)
    (byte-compile-close-variables
     (save-excursion
       (setq outbuffer
	     (set-buffer (get-buffer-create " *Compiler Output*")))
       (set-buffer-multibyte t)
       (erase-buffer)
       ;;	 (emacs-lisp-mode)
       (setq case-fold-search nil)
       ;; This is a kludge.  Some operating systems (OS/2, DOS) need to
       ;; write files containing binary information specially.
       ;; Under most circumstances, such files will be in binary
       ;; overwrite mode, so those OS's use that flag to guess how
       ;; they should write their data.  Advise them that .elc files
       ;; need to be written carefully.
       (setq overwrite-mode 'overwrite-mode-binary))
     (displaying-byte-compile-warnings
      (and filename (byte-compile-insert-header filename inbuffer outbuffer))
      (save-excursion
	(set-buffer inbuffer)
	(goto-char 1)

	;; Compile the forms from the input buffer.
	(while (progn
		 (while (progn (skip-chars-forward " \t\n\^l")
			       (looking-at ";"))
		   (forward-line 1))
		 (not (eobp)))
	  (setq byte-compile-read-position (point)
		byte-compile-last-position byte-compile-read-position)
	  (let ((form (read inbuffer)))
	    (byte-compile-file-form form)))
	;; Compile pending forms at end of file.
	(byte-compile-flush-pending)
	(byte-compile-warn-about-unresolved-functions)
	;; Should we always do this?  When calling multiple files, it
	;; would be useful to delay this warning until all have
	;; been compiled.
	(setq byte-compile-unresolved-functions nil))
      ;; Fix up the header at the front of the output
      ;; if the buffer contains multibyte characters.
      (and filename (byte-compile-fix-header filename inbuffer outbuffer))))
    outbuffer))

(defun byte-compile-fix-header (filename inbuffer outbuffer)
  (save-excursion
    (set-buffer outbuffer)
    ;; See if the buffer has any multibyte characters.
    (when (< (point-max) (position-bytes (point-max)))
      (when (byte-compile-version-cond byte-compile-compatibility)
	(error "Version-18 compatibility not valid with multibyte characters"))
      (goto-char (point-min))
      ;; Find the comment that describes the version test.
      (search-forward "\n;;; This file")
      (beginning-of-line)
      (narrow-to-region (point) (point-max))
      ;; Find the line of ballast semicolons.
      (search-forward ";;;;;;;;;;")
      (beginning-of-line)

      (narrow-to-region (point-min) (point))
      (let ((old-header-end (point))
	    delta)
	(goto-char (point-min))
	(delete-region (point) (progn (re-search-forward "^(")
				      (beginning-of-line)
				      (point)))
	(insert ";;; This file contains multibyte non-ASCII characters\n"
		";;; and therefore cannot be loaded into Emacs 19.\n")
	;; Replace "19" or "19.29" with "20", twice.
	(re-search-forward "19\\(\\.[0-9]+\\)")
	(replace-match "20")
	(re-search-forward "19\\(\\.[0-9]+\\)")
	(replace-match "20")
	;; Now compensate for the change in size,
	;; to make sure all positions in the file remain valid.
	(setq delta (- (point-max) old-header-end))
	(goto-char (point-max))
	(widen)
	(delete-char delta)))))

(defun byte-compile-insert-header (filename inbuffer outbuffer)
  (set-buffer inbuffer)
  (let ((dynamic-docstrings byte-compile-dynamic-docstrings)
	(dynamic byte-compile-dynamic))
    (set-buffer outbuffer)
    (goto-char 1)
    ;; The magic number of .elc files is ";ELC", or 0x3B454C43.  After
    ;; that is the file-format version number (18, 19 or 20) as a
    ;; byte, followed by some nulls.  The primary motivation for doing
    ;; this is to get some binary characters up in the first line of
    ;; the file so that `diff' will simply say "Binary files differ"
    ;; instead of actually doing a diff of two .elc files.  An extra
    ;; benefit is that you can add this to /etc/magic:

    ;; 0	string		;ELC		GNU Emacs Lisp compiled file,
    ;; >4	byte		x		version %d

    (insert
     ";ELC"
     (if (byte-compile-version-cond byte-compile-compatibility) 18 20)
     "\000\000\000\n"
     )
    (insert ";;; Compiled by "
	    (or (and (boundp 'user-mail-address) user-mail-address)
		(concat (user-login-name) "@" (system-name)))
	    " on "
	    (current-time-string) "\n;;; from file " filename "\n")
    (insert ";;; in Emacs version " emacs-version "\n")
    (insert ";;; with bytecomp version "
	    (progn (string-match "[0-9.]+" byte-compile-version)
		   (match-string 0 byte-compile-version))
	    "\n;;; "
	    (cond
	     ((eq byte-optimize 'source) "with source-level optimization only")
	     ((eq byte-optimize 'byte) "with byte-level optimization only")
	     (byte-optimize "with all optimizations")
	     (t "without optimization"))
	    (if (byte-compile-version-cond byte-compile-compatibility)
		"; compiled with Emacs 18 compatibility.\n"
	      ".\n"))
    (if dynamic
	(insert ";;; Function definitions are lazy-loaded.\n"))
    (if (not (byte-compile-version-cond byte-compile-compatibility))
	(let (intro-string minimum-version)
	  ;; Figure out which Emacs version to require,
	  ;; and what comment to use to explain why.
	  ;; Note that this fails to take account of whether
	  ;; the buffer contains multibyte characters.  We may have to
	  ;; compensate at the end in byte-compile-fix-header.
	  (if dynamic-docstrings
	      (setq intro-string
		    ";;; This file uses dynamic docstrings, first added in Emacs 19.29.\n"
		    minimum-version "19.29")
	    (setq intro-string
		  ";;; This file uses opcodes which do not exist in Emacs 18.\n"
		  minimum-version "19"))
	  ;; Now insert the comment and the error check.
	  (insert
	   "\n"
	   intro-string
	   ;; Have to check if emacs-version is bound so that this works
	   ;; in files loaded early in loadup.el.
	   "(if (and (boundp 'emacs-version)\n"
	   ;; If there is a name at the end of emacs-version,
	   ;; don't try to check the version number.
	   "\t (< (aref emacs-version (1- (length emacs-version))) ?A)\n"
	   "\t (or (and (boundp 'epoch::version) epoch::version)\n"
	   (format "\t     (string-lessp emacs-version \"%s\")))\n"
		   minimum-version)
	   "    (error \"`"
	   ;; prin1-to-string is used to quote backslashes.
	   (substring (prin1-to-string (file-name-nondirectory filename))
		      1 -1)
	   (format "' was compiled for Emacs %s or later\"))\n\n"
		   minimum-version)
	   ;; Insert semicolons as ballast, so that byte-compile-fix-header
	   ;; can delete them so as to keep the buffer positions
	   ;; constant for the actual compiled code.
	   ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\n"))
      ;; Here if we want Emacs 18 compatibility.
      (when dynamic-docstrings
	(error "Version-18 compatibility doesn't support dynamic doc strings"))
      (when byte-compile-dynamic
	(error "Version-18 compatibility doesn't support dynamic byte code"))
      (insert "(or (boundp 'current-load-list) (setq current-load-list nil))\n"
	      "\n"))))

(defun byte-compile-output-file-form (form)
  ;; writes the given form to the output buffer, being careful of docstrings
  ;; in defun, defmacro, defvar, defconst, autoload and
  ;; custom-declare-variable because make-docfile is so amazingly stupid.
  ;; defalias calls are output directly by byte-compile-file-form-defmumble;
  ;; it does not pay to first build the defalias in defmumble and then parse
  ;; it here.
  (if (and (memq (car-safe form) '(defun defmacro defvar defconst autoload
				   custom-declare-variable))
	   (stringp (nth 3 form)))
      (byte-compile-output-docform nil nil '("\n(" 3 ")") form nil
				   (memq (car form)
					 '(autoload custom-declare-variable)))
    (let ((print-escape-newlines t)
	  (print-length nil)
	  (print-level nil)
	  (print-quoted t)
	  (print-gensym t))
      (princ "\n" outbuffer)
      (prin1 form outbuffer)
      nil)))

(defun byte-compile-output-docform (preface name info form specindex quoted)
  "Print a form with a doc string.  INFO is (prefix doc-index postfix).
If PREFACE and NAME are non-nil, print them too,
before INFO and the FORM but after the doc string itself.
If SPECINDEX is non-nil, it is the index in FORM
of the function bytecode string.  In that case,
we output that argument and the following argument (the constants vector)
together, for lazy loading.
QUOTED says that we have to put a quote before the
list that represents a doc string reference.
`autoload' and `custom-declare-variable' need that."
  ;; We need to examine byte-compile-dynamic-docstrings
  ;; in the input buffer (now current), not in the output buffer.
  (let ((dynamic-docstrings byte-compile-dynamic-docstrings))
    (set-buffer
     (prog1 (current-buffer)
       (set-buffer outbuffer)
       (let (position)

	 ;; Insert the doc string, and make it a comment with #@LENGTH.
	 (and (>= (nth 1 info) 0)
	      dynamic-docstrings
	      (not byte-compile-compatibility)
	      (progn
		;; Make the doc string start at beginning of line
		;; for make-docfile's sake.
		(insert "\n")
		(setq position
		      (byte-compile-output-as-comment
		       (nth (nth 1 info) form) nil))
		(setq position (position-bytes position))
		;; If the doc string starts with * (a user variable),
		;; negate POSITION.
		(if (and (stringp (nth (nth 1 info) form))
			 (> (length (nth (nth 1 info) form)) 0)
			 (eq (aref (nth (nth 1 info) form) 0) ?*))
		    (setq position (- position)))))

	 (if preface
	     (progn
	       (insert preface)
	       (prin1 name outbuffer)))
	 (insert (car info))
	 (let ((print-escape-newlines t)
	       (print-quoted t)
	       ;; For compatibility with code before print-circle,
	       ;; use a cons cell to say that we want
	       ;; print-gensym-alist not to be cleared
	       ;; between calls to print functions.
	       (print-gensym '(t))
	       ;; print-gensym-alist was used before print-circle existed.
	       print-gensym-alist
	       (print-continuous-numbering t)
	       print-number-table
	       (index 0))
	   (prin1 (car form) outbuffer)
	   (while (setq form (cdr form))
	     (setq index (1+ index))
	     (insert " ")
	     (cond ((and (numberp specindex) (= index specindex))
		    (let ((position
			   (byte-compile-output-as-comment
			    (cons (car form) (nth 1 form))
			    t)))
		      (setq position (position-bytes position))
		      (princ (format "(#$ . %d) nil" position) outbuffer)
		      (setq form (cdr form))
		      (setq index (1+ index))))
		   ((= index (nth 1 info))
		    (if position
			(princ (format (if quoted "'(#$ . %d)"  "(#$ . %d)")
				       position)
			       outbuffer)
		      (let ((print-escape-newlines nil))
			(goto-char (prog1 (1+ (point))
				     (prin1 (car form) outbuffer)))
			(insert "\\\n")
			(goto-char (point-max)))))
		   (t
		    (prin1 (car form) outbuffer)))))
	 (insert (nth 2 info))))))
  nil)

(defun byte-compile-keep-pending (form &optional handler)
  (if (memq byte-optimize '(t source))
      (setq form (byte-optimize-form form t)))
  (if handler
      (let ((for-effect t))
	;; To avoid consing up monstrously large forms at load time, we split
	;; the output regularly.
	(and (memq (car-safe form) '(fset defalias))
	     (nthcdr 300 byte-compile-output)
	     (byte-compile-flush-pending))
	(funcall handler form)
	(if for-effect
	    (byte-compile-discard)))
    (byte-compile-form form t))
  nil)

(defun byte-compile-flush-pending ()
  (if byte-compile-output
      (let ((form (byte-compile-out-toplevel t 'file)))
	(cond ((eq (car-safe form) 'progn)
	       (mapc 'byte-compile-output-file-form (cdr form)))
	      (form
	       (byte-compile-output-file-form form)))
	(setq byte-compile-constants nil
	      byte-compile-variables nil
	      byte-compile-depth 0
	      byte-compile-maxdepth 0
	      byte-compile-output nil))))

(defun byte-compile-file-form (form)
  (let ((byte-compile-current-form nil)	; close over this for warnings.
	handler)
    (cond
     ((not (consp form))
      (byte-compile-keep-pending form))
     ((and (symbolp (car form))
	   (setq handler (get (car form) 'byte-hunk-handler)))
      (cond ((setq form (funcall handler form))
	     (byte-compile-flush-pending)
	     (byte-compile-output-file-form form))))
     ((eq form (setq form (macroexpand form byte-compile-macro-environment)))
      (byte-compile-keep-pending form))
     (t
      (byte-compile-file-form form)))))

;; Functions and variables with doc strings must be output separately,
;; so make-docfile can recognise them.  Most other things can be output
;; as byte-code.

(put 'defsubst 'byte-hunk-handler 'byte-compile-file-form-defsubst)
(defun byte-compile-file-form-defsubst (form)
  (cond ((assq (nth 1 form) byte-compile-unresolved-functions)
	 (setq byte-compile-current-form (nth 1 form))
	 (byte-compile-warn "defsubst %s was used before it was defined"
			    (nth 1 form))))
  (byte-compile-file-form
   (macroexpand form byte-compile-macro-environment))
  ;; Return nil so the form is not output twice.
  nil)

(put 'autoload 'byte-hunk-handler 'byte-compile-file-form-autoload)
(defun byte-compile-file-form-autoload (form)
  (and (let ((form form))
	 (while (if (setq form (cdr form)) (byte-compile-constp (car form))))
	 (null form))			;Constants only
       (eval (nth 5 form))		;Macro
       (eval form))			;Define the autoload.
  ;; Avoid undefined function warnings for the autoload.
  (if (and (consp (nth 1 form))
	   (eq (car (nth 1 form)) 'quote)
	   (consp (cdr (nth 1 form)))
	   (symbolp (nth 1 (nth 1 form))))
      (add-to-list 'byte-compile-function-environment
		   (cons (nth 1 (nth 1 form))
			 (cons 'autoload (cdr (cdr form))))))
  (if (stringp (nth 3 form))
      form
    ;; No doc string, so we can compile this as a normal form.
    (byte-compile-keep-pending form 'byte-compile-normal-call)))

(put 'defvar   'byte-hunk-handler 'byte-compile-file-form-defvar)
(put 'defconst 'byte-hunk-handler 'byte-compile-file-form-defvar)
(defun byte-compile-file-form-defvar (form)
  (if (null (nth 3 form))
      ;; Since there is no doc string, we can compile this as a normal form,
      ;; and not do a file-boundary.
      (byte-compile-keep-pending form)
    (if (memq 'free-vars byte-compile-warnings)
	(setq byte-compile-bound-variables
	      (cons (nth 1 form) byte-compile-bound-variables)))
    (cond ((consp (nth 2 form))
	   (setq form (copy-sequence form))
	   (setcar (cdr (cdr form))
		   (byte-compile-top-level (nth 2 form) nil 'file))))
    form))

(put 'custom-declare-variable 'byte-hunk-handler
     'byte-compile-file-form-custom-declare-variable)
(defun byte-compile-file-form-custom-declare-variable (form)
  (if (memq 'free-vars byte-compile-warnings)
      (setq byte-compile-bound-variables
	    (cons (nth 1 (nth 1 form)) byte-compile-bound-variables)))
  form)

(put 'require 'byte-hunk-handler 'byte-compile-file-form-eval-boundary)
(defun byte-compile-file-form-eval-boundary (form)
  (eval form)
  (byte-compile-keep-pending form 'byte-compile-normal-call))

(put 'progn 'byte-hunk-handler 'byte-compile-file-form-progn)
(put 'prog1 'byte-hunk-handler 'byte-compile-file-form-progn)
(put 'prog2 'byte-hunk-handler 'byte-compile-file-form-progn)
(defun byte-compile-file-form-progn (form)
  (mapc 'byte-compile-file-form (cdr form))
  ;; Return nil so the forms are not output twice.
  nil)

;; This handler is not necessary, but it makes the output from dont-compile
;; and similar macros cleaner.
(put 'eval 'byte-hunk-handler 'byte-compile-file-form-eval)
(defun byte-compile-file-form-eval (form)
  (if (eq (car-safe (nth 1 form)) 'quote)
      (nth 1 (nth 1 form))
    (byte-compile-keep-pending form)))

(put 'defun 'byte-hunk-handler 'byte-compile-file-form-defun)
(defun byte-compile-file-form-defun (form)
  (byte-compile-file-form-defmumble form nil))

(put 'defmacro 'byte-hunk-handler 'byte-compile-file-form-defmacro)
(defun byte-compile-file-form-defmacro (form)
  (byte-compile-file-form-defmumble form t))

(defun byte-compile-file-form-defmumble (form macrop)
  (let* ((name (car (cdr form)))
	 (this-kind (if macrop 'byte-compile-macro-environment
		      'byte-compile-function-environment))
	 (that-kind (if macrop 'byte-compile-function-environment
		      'byte-compile-macro-environment))
	 (this-one (assq name (symbol-value this-kind)))
	 (that-one (assq name (symbol-value that-kind)))
	 (byte-compile-free-references nil)
	 (byte-compile-free-assignments nil))
    (byte-compile-set-symbol-position name)
    ;; When a function or macro is defined, add it to the call tree so that
    ;; we can tell when functions are not used.
    (if byte-compile-generate-call-tree
	(or (assq name byte-compile-call-tree)
	    (setq byte-compile-call-tree
		  (cons (list name nil nil) byte-compile-call-tree))))

    (setq byte-compile-current-form name) ; for warnings
    (if (memq 'redefine byte-compile-warnings)
	(byte-compile-arglist-warn form macrop))
    (if byte-compile-verbose
	(message "Compiling %s... (%s)" (or filename "") (nth 1 form)))
    (cond (that-one
	   (if (and (memq 'redefine byte-compile-warnings)
		    ;; don't warn when compiling the stubs in byte-run...
		    (not (assq (nth 1 form)
			       byte-compile-initial-macro-environment)))
	       (byte-compile-warn
		 "%s defined multiple times, as both function and macro"
		 (nth 1 form)))
	   (setcdr that-one nil))
	  (this-one
	   (when (and (memq 'redefine byte-compile-warnings)
		    ;; hack: don't warn when compiling the magic internal
		    ;; byte-compiler macros in byte-run.el...
		    (not (assq (nth 1 form)
			       byte-compile-initial-macro-environment)))
	     (byte-compile-warn "%s %s defined multiple times in this file"
				(if macrop "macro" "function")
				(nth 1 form))))
	  ((and (fboundp name)
		(eq (car-safe (symbol-function name))
		    (if macrop 'lambda 'macro)))
	   (when (memq 'redefine byte-compile-warnings)
	     (byte-compile-warn "%s %s being redefined as a %s"
				(if macrop "function" "macro")
				(nth 1 form)
				(if macrop "macro" "function")))
	   ;; shadow existing definition
	   (set this-kind
		(cons (cons name nil) (symbol-value this-kind))))
	  )
    (let ((body (nthcdr 3 form)))
      (when (and (stringp (car body))
		 (symbolp (car-safe (cdr-safe body)))
		 (car-safe (cdr-safe body))
		 (stringp (car-safe (cdr-safe (cdr-safe body)))))
	(byte-compile-set-symbol-position (nth 1 form))
	(byte-compile-warn "probable `\"' without `\\' in doc string of %s"
			   (nth 1 form))))
    
    ;; Generate code for declarations in macro definitions.
    ;; Remove declarations from the body of the macro definition.
    (when macrop
      (let ((tail (nthcdr 2 form)))
	(when (stringp (car (cdr tail)))
	  (setq tail (cdr tail)))
	(while (and (consp (car (cdr tail)))
		    (eq (car (car (cdr tail))) 'declare))
	  (let ((declaration (car (cdr tail))))
	    (setcdr tail (cdr (cdr tail)))
	    (princ `(if macro-declaration-function
			(funcall macro-declaration-function
				 ',name ',declaration))
		   outbuffer)))))
      
    (let* ((new-one (byte-compile-lambda (cons 'lambda (nthcdr 2 form))))
	   (code (byte-compile-byte-code-maker new-one)))
      (if this-one
	  (setcdr this-one new-one)
	(set this-kind
	     (cons (cons name new-one) (symbol-value this-kind))))
      (if (and (stringp (nth 3 form))
	       (eq 'quote (car-safe code))
	       (eq 'lambda (car-safe (nth 1 code))))
	  (cons (car form)
		(cons name (cdr (nth 1 code))))
	(byte-compile-flush-pending)
	(if (not (stringp (nth 3 form)))
	    ;; No doc string.  Provide -1 as the "doc string index"
	    ;; so that no element will be treated as a doc string.
	    (byte-compile-output-docform
	     (if (byte-compile-version-cond byte-compile-compatibility)
		 "\n(fset '" "\n(defalias '")
	     name
	     (cond ((atom code)
		    (if macrop '(" '(macro . #[" -1 "])") '(" #[" -1 "]")))
		   ((eq (car code) 'quote)
		    (setq code new-one)
		    (if macrop '(" '(macro " -1 ")") '(" '(" -1 ")")))
		   ((if macrop '(" (cons 'macro (" -1 "))") '(" (" -1 ")"))))
	     (append code nil)
	     (and (atom code) byte-compile-dynamic
		  1)
	     nil)
	  ;; Output the form by hand, that's much simpler than having
	  ;; b-c-output-file-form analyze the defalias.
	  (byte-compile-output-docform
	   (if (byte-compile-version-cond byte-compile-compatibility)
	       "\n(fset '" "\n(defalias '")
	   name
	   (cond ((atom code)
		  (if macrop '(" '(macro . #[" 4 "])") '(" #[" 4 "]")))
		 ((eq (car code) 'quote)
		  (setq code new-one)
		  (if macrop '(" '(macro " 2 ")") '(" '(" 2 ")")))
		 ((if macrop '(" (cons 'macro (" 5 "))") '(" (" 5 ")"))))
	   (append code nil)
	   (and (atom code) byte-compile-dynamic
		1)
	   nil))
	(princ ")" outbuffer)
	nil))))

;; Print Lisp object EXP in the output file, inside a comment,
;; and return the file position it will have.
;; If QUOTED is non-nil, print with quoting; otherwise, print without quoting.
(defun byte-compile-output-as-comment (exp quoted)
  (let ((position (point)))
    (set-buffer
     (prog1 (current-buffer)
       (set-buffer outbuffer)

       ;; Insert EXP, and make it a comment with #@LENGTH.
       (insert " ")
       (if quoted
	   (prin1 exp outbuffer)
	 (princ exp outbuffer))
       (goto-char position)
       ;; Quote certain special characters as needed.
       ;; get_doc_string in doc.c does the unquoting.
       (while (search-forward "\^A" nil t)
	 (replace-match "\^A\^A" t t))
       (goto-char position)
       (while (search-forward "\000" nil t)
	 (replace-match "\^A0" t t))
       (goto-char position)
       (while (search-forward "\037" nil t)
	 (replace-match "\^A_" t t))
       (goto-char (point-max))
       (insert "\037")
       (goto-char position)
       (insert "#@" (format "%d" (- (position-bytes (point-max))
				    (position-bytes position))))

       ;; Save the file position of the object.
       ;; Note we should add 1 to skip the space
       ;; that we inserted before the actual doc string,
       ;; and subtract 1 to convert from an 1-origin Emacs position
       ;; to a file position; they cancel.
       (setq position (point))
       (goto-char (point-max))))
    position))



;;;###autoload
(defun byte-compile (form)
  "If FORM is a symbol, byte-compile its function definition.
If FORM is a lambda or a macro, byte-compile it as a function."
  (displaying-byte-compile-warnings
   (byte-compile-close-variables
    (let* ((fun (if (symbolp form)
		    (and (fboundp form) (symbol-function form))
		  form))
	   (macro (eq (car-safe fun) 'macro)))
      (if macro
	  (setq fun (cdr fun)))
      (cond ((eq (car-safe fun) 'lambda)
	     (setq fun (if macro
			   (cons 'macro (byte-compile-lambda fun))
			 (byte-compile-lambda fun)))
	     (if (symbolp form)
		 (defalias form fun)
	       fun)))))))

(defun byte-compile-sexp (sexp)
  "Compile and return SEXP."
  (displaying-byte-compile-warnings
   (byte-compile-close-variables
    (byte-compile-top-level sexp))))

;; Given a function made by byte-compile-lambda, make a form which produces it.
(defun byte-compile-byte-code-maker (fun)
  (cond
   ((byte-compile-version-cond byte-compile-compatibility)
    ;; Return (quote (lambda ...)).
    (list 'quote (byte-compile-byte-code-unmake fun)))
   ;; ## atom is faster than compiled-func-p.
   ((atom fun)				; compiled function.
    ;; generate-emacs19-bytecodes must be on, otherwise byte-compile-lambda
    ;; would have produced a lambda.
    fun)
   ;; b-c-lambda didn't produce a compiled-function, so it's either a trivial
   ;; function, or this is Emacs 18, or generate-emacs19-bytecodes is off.
   ((let (tmp)
      (if (and (setq tmp (assq 'byte-code (cdr-safe (cdr fun))))
	       (null (cdr (memq tmp fun))))
	  ;; Generate a make-byte-code call.
	  (let* ((interactive (assq 'interactive (cdr (cdr fun)))))
	    (nconc (list 'make-byte-code
			 (list 'quote (nth 1 fun)) ;arglist
			 (nth 1 tmp)	;bytes
			 (nth 2 tmp)	;consts
			 (nth 3 tmp))	;depth
		   (cond ((stringp (nth 2 fun))
			  (list (nth 2 fun))) ;doc
			 (interactive
			  (list nil)))
		   (cond (interactive
			  (list (if (or (null (nth 1 interactive))
					(stringp (nth 1 interactive)))
				    (nth 1 interactive)
				  ;; Interactive spec is a list or a variable
				  ;; (if it is correct).
				  (list 'quote (nth 1 interactive))))))))
	;; a non-compiled function (probably trivial)
	(list 'quote fun))))))

;; Turn a function into an ordinary lambda.  Needed for v18 files.
(defun byte-compile-byte-code-unmake (function)
  (if (consp function)
      function;;It already is a lambda.
    (setq function (append function nil)) ; turn it into a list
    (nconc (list 'lambda (nth 0 function))
	   (and (nth 4 function) (list (nth 4 function)))
	   (if (nthcdr 5 function)
	       (list (cons 'interactive (if (nth 5 function)
					    (nthcdr 5 function)))))
	   (list (list 'byte-code
		       (nth 1 function) (nth 2 function)
		       (nth 3 function))))))


(defun byte-compile-check-lambda-list (list)
  "Check lambda-list LIST for errors."
  (let (vars)
    (while list
      (let ((arg (car list)))
	(when (symbolp arg)
	  (byte-compile-set-symbol-position arg))
	(cond ((or (not (symbolp arg))
		   (keywordp arg)
		   (memq arg '(t nil)))
	       (error "Invalid lambda variable %s" arg))
	      ((eq arg '&rest)
	       (unless (cdr list)
		 (error "&rest without variable name"))
	       (when (cddr list)
		 (error "Garbage following &rest VAR in lambda-list")))
	      ((eq arg '&optional)
	       (unless (cdr list)
		 (error "Variable name missing after &optional")))
	      ((memq arg vars)
	       (byte-compile-warn "repeated variable %s in lambda-list" arg))
	      (t
	       (push arg vars))))
      (setq list (cdr list)))))


;; Byte-compile a lambda-expression and return a valid function.
;; The value is usually a compiled function but may be the original
;; lambda-expression.
(defun byte-compile-lambda (fun)
  (unless (eq 'lambda (car-safe fun))
    (error "Not a lambda list: %S" fun))
  (byte-compile-set-symbol-position 'lambda)
  (byte-compile-check-lambda-list (nth 1 fun))
  (let* ((arglist (nth 1 fun))
	 (byte-compile-bound-variables
	  (nconc (and (memq 'free-vars byte-compile-warnings)
		      (delq '&rest (delq '&optional (copy-sequence arglist))))
		 byte-compile-bound-variables))
	 (body (cdr (cdr fun)))
	 (doc (if (stringp (car body))
		  (prog1 (car body)
		    ;; Discard the doc string
		    ;; unless it is the last element of the body.
		    (if (cdr body)
			(setq body (cdr body))))))
	 (int (assq 'interactive body)))
    (cond (int
	   (byte-compile-set-symbol-position 'interactive)
	   ;; Skip (interactive) if it is in front (the most usual location).
	   (if (eq int (car body))
	       (setq body (cdr body)))
	   (cond ((consp (cdr int))
		  (if (cdr (cdr int))
		      (byte-compile-warn "malformed interactive spec: %s"
					 (prin1-to-string int)))
		  ;; If the interactive spec is a call to `list',
		  ;; don't compile it, because `call-interactively'
		  ;; looks at the args of `list'.
		  (let ((form (nth 1 int)))
		    (while (or (eq (car-safe form) 'let)
			       (eq (car-safe form) 'let*)
			       (eq (car-safe form) 'save-excursion))
		      (while (consp (cdr form))
			(setq form (cdr form)))
		      (setq form (car form)))
		    (or (eq (car-safe form) 'list)
			(setq int (list 'interactive
					(byte-compile-top-level (nth 1 int)))))))
		 ((cdr int)
		  (byte-compile-warn "malformed interactive spec: %s"
				     (prin1-to-string int))))))
    (let ((compiled (byte-compile-top-level (cons 'progn body) nil 'lambda)))
      (if (and (eq 'byte-code (car-safe compiled))
	       (not (byte-compile-version-cond
		     byte-compile-compatibility)))
	  (apply 'make-byte-code
		 (append (list arglist)
			 ;; byte-string, constants-vector, stack depth
			 (cdr compiled)
			 ;; optionally, the doc string.
			 (if (or doc int)
			     (list doc))
			 ;; optionally, the interactive spec.
			 (if int
			     (list (nth 1 int)))))
	(setq compiled
	      (nconc (if int (list int))
		     (cond ((eq (car-safe compiled) 'progn) (cdr compiled))
			   (compiled (list compiled)))))
	(nconc (list 'lambda arglist)
	       (if (or doc (stringp (car compiled)))
		   (cons doc (cond (compiled)
				   (body (list nil))))
		 compiled))))))

(defun byte-compile-constants-vector ()
  ;; Builds the constants-vector from the current variables and constants.
  ;;   This modifies the constants from (const . nil) to (const . offset).
  ;; To keep the byte-codes to look up the vector as short as possible:
  ;;   First 6 elements are vars, as there are one-byte varref codes for those.
  ;;   Next up to byte-constant-limit are constants, still with one-byte codes.
  ;;   Next variables again, to get 2-byte codes for variable lookup.
  ;;   The rest of the constants and variables need 3-byte byte-codes.
  (let* ((i -1)
	 (rest (nreverse byte-compile-variables)) ; nreverse because the first
	 (other (nreverse byte-compile-constants)) ; vars often are used most.
	 ret tmp
	 (limits '(5			; Use the 1-byte varref codes,
		   63  ; 1-constlim	;  1-byte byte-constant codes,
		   255			;  2-byte varref codes,
		   65535))		;  3-byte codes for the rest.
	 limit)
    (while (or rest other)
      (setq limit (car limits))
      (while (and rest (not (eq i limit)))
	(if (setq tmp (assq (car (car rest)) ret))
	    (setcdr (car rest) (cdr tmp))
	  (setcdr (car rest) (setq i (1+ i)))
	  (setq ret (cons (car rest) ret)))
	(setq rest (cdr rest)))
      (setq limits (cdr limits)
	    rest (prog1 other
		   (setq other rest))))
    (apply 'vector (nreverse (mapcar 'car ret)))))

;; Given an expression FORM, compile it and return an equivalent byte-code
;; expression (a call to the function byte-code).
(defun byte-compile-top-level (form &optional for-effect output-type)
  ;; OUTPUT-TYPE advises about how form is expected to be used:
  ;;	'eval or nil	-> a single form,
  ;;	'progn or t	-> a list of forms,
  ;;	'lambda		-> body of a lambda,
  ;;	'file		-> used at file-level.
  (let ((byte-compile-constants nil)
	(byte-compile-variables nil)
	(byte-compile-tag-number 0)
	(byte-compile-depth 0)
	(byte-compile-maxdepth 0)
	(byte-compile-output nil))
     (if (memq byte-optimize '(t source))
	 (setq form (byte-optimize-form form for-effect)))
     (while (and (eq (car-safe form) 'progn) (null (cdr (cdr form))))
       (setq form (nth 1 form)))
     (if (and (eq 'byte-code (car-safe form))
	      (not (memq byte-optimize '(t byte)))
	      (stringp (nth 1 form)) (vectorp (nth 2 form))
	      (natnump (nth 3 form)))
	 form
       (byte-compile-form form for-effect)
       (byte-compile-out-toplevel for-effect output-type))))

(defun byte-compile-out-toplevel (&optional for-effect output-type)
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
		(while (and tmp (not (or (symbolp (car (car tmp)))
					 (numberp (car (car tmp))))))
		  (setq tmp (cdr tmp)))
		(car (car tmp)))))))
  (byte-compile-out 'byte-return 0)
  (setq byte-compile-output (nreverse byte-compile-output))
  (if (memq byte-optimize '(t byte))
      (setq byte-compile-output
	    (byte-optimize-lapcode byte-compile-output for-effect)))

  ;; Decompile trivial functions:
  ;; only constants and variables, or a single funcall except in lambdas.
  ;; Except for Lisp_Compiled objects, forms like (foo "hi")
  ;; are still quicker than (byte-code "..." [foo "hi"] 2).
  ;; Note that even (quote foo) must be parsed just as any subr by the
  ;; interpreter, so quote should be compiled into byte-code in some contexts.
  ;; What to leave uncompiled:
  ;;	lambda	-> never.  we used to leave it uncompiled if the body was
  ;;		   a single atom, but that causes confusion if the docstring
  ;;		   uses the (file . pos) syntax.  Besides, now that we have
  ;;		   the Lisp_Compiled type, the compiled form is faster.
  ;;	eval	-> atom, quote or (function atom atom atom)
  ;;	progn	-> as <<same-as-eval>> or (progn <<same-as-eval>> atom)
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
	    (while (cond
		    ((memq (car (car rest)) '(byte-varref byte-constant))
		     (setq tmp (car (cdr (car rest))))
		     (if (if (eq (car (car rest)) 'byte-constant)
			     (or (consp tmp)
				 (and (symbolp tmp)
				      (not (byte-compile-const-symbol-p tmp)))))
			 (if maycall
			     (setq body (cons (list 'quote tmp) body)))
		       (setq body (cons tmp body))))
		    ((and maycall
			  ;; Allow a funcall if at most one atom follows it.
			  (null (nthcdr 3 rest))
			  (setq tmp (get (car (car rest)) 'byte-opcode-invert))
			  (or (null (cdr rest))
			      (and (memq output-type '(file progn t))
				   (cdr (cdr rest))
				   (eq (car (nth 1 rest)) 'byte-discard)
				   (progn (setq rest (cdr rest)) t))))
		     (setq maycall nil)	; Only allow one real function call.
		     (setq body (nreverse body))
		     (setq body (list
				 (if (and (eq tmp 'funcall)
					  (eq (car-safe (car body)) 'quote))
				     (cons (nth 1 (car body)) (cdr body))
				   (cons tmp body))))
		     (or (eq output-type 'file)
			 (not (delq nil (mapcar 'consp (cdr (car body))))))))
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
  (setq body (byte-compile-top-level (cons 'progn body) for-effect t))
  (cond ((eq (car-safe body) 'progn)
	 (cdr body))
	(body
	 (list body))))

;; This is the recursive entry point for compiling each subform of an
;; expression.
;; If for-effect is non-nil, byte-compile-form will output a byte-discard
;; before terminating (ie no value will be left on the stack).
;; A byte-compile handler may, when for-effect is non-nil, choose output code
;; which does not leave a value on the stack, and then set for-effect to nil
;; (to prevent byte-compile-form from outputting the byte-discard).
;; If a handler wants to call another handler, it should do so via
;; byte-compile-form, or take extreme care to handle for-effect correctly.
;; (Use byte-compile-form-do-effect to reset the for-effect flag too.)
;;
(defun byte-compile-form (form &optional for-effect)
  (setq form (macroexpand form byte-compile-macro-environment))
  (cond ((not (consp form))
	 (when (symbolp form)
	   (byte-compile-set-symbol-position form))
	 (cond ((or (not (symbolp form)) (byte-compile-const-symbol-p form))
		(byte-compile-constant form))
	       ((and for-effect byte-compile-delete-errors)
		(setq for-effect nil))
	       (t (byte-compile-variable-ref 'byte-varref form))))
	((symbolp (car form))
	 (let* ((fn (car form))
		(handler (get fn 'byte-compile)))
	   (byte-compile-set-symbol-position fn)
	   (when (byte-compile-const-symbol-p fn)
	     (byte-compile-warn "%s called as a function" fn))
	   (if (and handler
		    (or (not (byte-compile-version-cond
			      byte-compile-compatibility))
			(not (get (get fn 'byte-opcode) 'emacs19-opcode))))
	       (funcall handler form)
	     (if (memq 'callargs byte-compile-warnings)
		 (byte-compile-callargs-warn form))
	     (byte-compile-normal-call form))))
	((and (or (byte-code-function-p (car form))
		  (eq (car-safe (car form)) 'lambda))
	      ;; if the form comes out the same way it went in, that's
	      ;; because it was malformed, and we couldn't unfold it.
	      (not (eq form (setq form (byte-compile-unfold-lambda form)))))
	 (byte-compile-form form for-effect)
	 (setq for-effect nil))
	((byte-compile-normal-call form)))
  (if for-effect
      (byte-compile-discard)))

(defun byte-compile-normal-call (form)
  (if byte-compile-generate-call-tree
      (byte-compile-annotate-call-tree form))
  (byte-compile-push-constant (car form))
  (mapc 'byte-compile-form (cdr form))	; wasteful, but faster.
  (byte-compile-out 'byte-call (length (cdr form))))

(defun byte-compile-variable-ref (base-op var)
  (when (symbolp var)
    (byte-compile-set-symbol-position var))
  (if (or (not (symbolp var)) (byte-compile-const-symbol-p var))
      (byte-compile-warn (if (eq base-op 'byte-varbind)
			     "attempt to let-bind %s %s"
			   "variable reference to %s %s")
			 (if (symbolp var) "constant" "nonvariable")
			 (prin1-to-string var))
    (if (and (get var 'byte-obsolete-variable)
	     (memq 'obsolete byte-compile-warnings))
	(let* ((ob (get var 'byte-obsolete-variable))
	       (when (cdr ob)))
	  (byte-compile-warn "%s is an obsolete variable%s; %s" var
			     (if when (concat " since " when) "")
			     (if (stringp (car ob))
				 (car ob)
			       (format "use %s instead." (car ob))))))
    (if (memq 'free-vars byte-compile-warnings)
	(if (eq base-op 'byte-varbind)
	    (setq byte-compile-bound-variables
		  (cons var byte-compile-bound-variables))
	  (or (boundp var)
	      (memq var byte-compile-bound-variables)
	      (if (eq base-op 'byte-varset)
		  (or (memq var byte-compile-free-assignments)
		      (progn
			(byte-compile-warn "assignment to free variable %s" var)
			(setq byte-compile-free-assignments
			      (cons var byte-compile-free-assignments))))
		(or (memq var byte-compile-free-references)
		    (progn
		      (byte-compile-warn "reference to free variable %s" var)
		      (setq byte-compile-free-references
			    (cons var byte-compile-free-references)))))))))
  (let ((tmp (assq var byte-compile-variables)))
    (or tmp
	(setq tmp (list var)
	      byte-compile-variables (cons tmp byte-compile-variables)))
    (byte-compile-out base-op tmp)))

(defmacro byte-compile-get-constant (const)
  `(or (if (stringp ,const)
	   (assoc ,const byte-compile-constants)
	 (assq ,const byte-compile-constants))
       (car (setq byte-compile-constants
		  (cons (list ,const) byte-compile-constants)))))

;; Use this when the value of a form is a constant.  This obeys for-effect.
(defun byte-compile-constant (const)
  (if for-effect
      (setq for-effect nil)
    (when (symbolp const)
      (byte-compile-set-symbol-position const))
    (byte-compile-out 'byte-constant (byte-compile-get-constant const))))

;; Use this for a constant that is not the value of its containing form.
;; This ignores for-effect.
(defun byte-compile-push-constant (const)
  (let ((for-effect nil))
    (inline (byte-compile-constant const))))


;; Compile those primitive ordinary functions
;; which have special byte codes just for speed.

(defmacro byte-defop-compiler (function &optional compile-handler)
  ;; add a compiler-form for FUNCTION.
  ;; If function is a symbol, then the variable "byte-SYMBOL" must name
  ;; the opcode to be used.  If function is a list, the first element
  ;; is the function and the second element is the bytecode-symbol.
  ;; COMPILE-HANDLER is the function to use to compile this byte-op, or
  ;; may be the abbreviations 0, 1, 2, 3, 0-1, or 1-2.
  ;; If it is nil, then the handler is "byte-compile-SYMBOL."
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
					(3 . byte-compile-three-args)
					(0-1 . byte-compile-zero-or-one-arg)
					(1-2 . byte-compile-one-or-two-args)
					(2-3 . byte-compile-two-or-three-args)
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

(defmacro byte-defop-compiler19 (function &optional compile-handler)
  ;; Just like byte-defop-compiler, but defines an opcode that will only
  ;; be used when byte-compile-compatibility is false.
  (if (and (byte-compile-single-version)
	   byte-compile-compatibility)
      ;; #### instead of doing nothing, this should do some remprops,
      ;; #### to protect against the case where a single-version compiler
      ;; #### is loaded into a world that has contained a multi-version one.
      nil
    (list 'progn
      (list 'put
	(list 'quote
	  (or (car (cdr-safe function))
	      (intern (concat "byte-"
		        (symbol-name (or (car-safe function) function))))))
	''emacs19-opcode t)
      (list 'byte-defop-compiler function compile-handler))))

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

(byte-defop-compiler (dot byte-point)		0)
(byte-defop-compiler (dot-max byte-point-max)	0)
(byte-defop-compiler (dot-min byte-point-min)	0)
(byte-defop-compiler point		0)
;;(byte-defop-compiler mark		0) ;; obsolete
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
;;(byte-defop-compiler read-char	0) ;; obsolete
(byte-defop-compiler interactive-p	0)
(byte-defop-compiler19 widen		0)
(byte-defop-compiler19 end-of-line    0-1)
(byte-defop-compiler19 forward-char   0-1)
(byte-defop-compiler19 forward-line   0-1)
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
;;(byte-defop-compiler set-mark		1) ;; obsolete
(byte-defop-compiler19 forward-word	1)
(byte-defop-compiler19 char-syntax	1)
(byte-defop-compiler19 nreverse		1)
(byte-defop-compiler19 car-safe		1)
(byte-defop-compiler19 cdr-safe		1)
(byte-defop-compiler19 numberp		1)
(byte-defop-compiler19 integerp		1)
(byte-defop-compiler19 skip-chars-forward     1-2)
(byte-defop-compiler19 skip-chars-backward    1-2)
(byte-defop-compiler eq 	 	2)
(byte-defop-compiler memq		2)
(byte-defop-compiler cons		2)
(byte-defop-compiler aref		2)
(byte-defop-compiler set		2)
(byte-defop-compiler (= byte-eqlsign)	2)
(byte-defop-compiler (< byte-lss)	2)
(byte-defop-compiler (> byte-gtr)	2)
(byte-defop-compiler (<= byte-leq)	2)
(byte-defop-compiler (>= byte-geq)	2)
(byte-defop-compiler get		2)
(byte-defop-compiler nth		2)
(byte-defop-compiler substring		2-3)
(byte-defop-compiler19 (move-marker byte-set-marker) 2-3)
(byte-defop-compiler19 set-marker	2-3)
(byte-defop-compiler19 match-beginning	1)
(byte-defop-compiler19 match-end	1)
(byte-defop-compiler19 upcase		1)
(byte-defop-compiler19 downcase		1)
(byte-defop-compiler19 string=		2)
(byte-defop-compiler19 string<		2)
(byte-defop-compiler19 (string-equal byte-string=) 2)
(byte-defop-compiler19 (string-lessp byte-string<) 2)
(byte-defop-compiler19 equal		2)
(byte-defop-compiler19 nthcdr		2)
(byte-defop-compiler19 elt		2)
(byte-defop-compiler19 member		2)
(byte-defop-compiler19 assq		2)
(byte-defop-compiler19 (rplaca byte-setcar) 2)
(byte-defop-compiler19 (rplacd byte-setcdr) 2)
(byte-defop-compiler19 setcar		2)
(byte-defop-compiler19 setcdr		2)
(byte-defop-compiler19 buffer-substring	2)
(byte-defop-compiler19 delete-region	2)
(byte-defop-compiler19 narrow-to-region	2)
(byte-defop-compiler19 (% byte-rem)	2)
(byte-defop-compiler aset		3)

(byte-defop-compiler max		byte-compile-associative)
(byte-defop-compiler min		byte-compile-associative)
(byte-defop-compiler (+ byte-plus)	byte-compile-associative)
(byte-defop-compiler19 (* byte-mult)	byte-compile-associative)

;;####(byte-defop-compiler19 move-to-column	1)
(byte-defop-compiler-1 interactive byte-compile-noop)


(defun byte-compile-subr-wrong-args (form n)
  (byte-compile-set-symbol-position (car form))
  (byte-compile-warn "%s called with %d arg%s, but requires %s"
		     (car form) (length (cdr form))
		     (if (= 1 (length (cdr form))) "" "s") n)
  ;; get run-time wrong-number-of-args error.
  (byte-compile-normal-call form))

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

(defun byte-compile-noop (form)
  (byte-compile-constant nil))

(defun byte-compile-discard ()
  (byte-compile-out 'byte-discard 0))


;; Compile a function that accepts one or more args and is right-associative.
;; We do it by left-associativity so that the operations
;; are done in the same order as in interpreted code.
;; We treat the one-arg case, as in (+ x), like (+ x 0).
;; in order to convert markers to numbers, and trigger expected errors.
(defun byte-compile-associative (form)
  (if (cdr form)
      (let ((opcode (get (car form) 'byte-opcode))
	    (args (copy-sequence (cdr form))))
	(byte-compile-form (car args))
	(setq args (cdr args))
	(or args (setq args '(0)
		       opcode (get '+ 'byte-opcode)))
	(while args
	  (byte-compile-form (car args))
	  (byte-compile-out opcode 0)
	  (setq args (cdr args))))
    (byte-compile-constant (eval form))))


;; more complicated compiler macros

(byte-defop-compiler list)
(byte-defop-compiler concat)
(byte-defop-compiler fset)
(byte-defop-compiler (indent-to-column byte-indent-to) byte-compile-indent-to)
(byte-defop-compiler indent-to)
(byte-defop-compiler insert)
(byte-defop-compiler-1 function byte-compile-function-form)
(byte-defop-compiler-1 - byte-compile-minus)
(byte-defop-compiler19 (/ byte-quo) byte-compile-quo)
(byte-defop-compiler19 nconc)

(defun byte-compile-list (form)
  (let ((count (length (cdr form))))
    (cond ((= count 0)
	   (byte-compile-constant nil))
	  ((< count 5)
	   (mapc 'byte-compile-form (cdr form))
	   (byte-compile-out
	    (aref [byte-list1 byte-list2 byte-list3 byte-list4] (1- count)) 0))
	  ((and (< count 256) (not (byte-compile-version-cond
				    byte-compile-compatibility)))
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
	  ((and (< count 256) (not (byte-compile-version-cond
				    byte-compile-compatibility)))
	   (mapc 'byte-compile-form (cdr form))
	   (byte-compile-out 'byte-concatN count))
	  ((byte-compile-normal-call form)))))

(defun byte-compile-minus (form)
  (if (null (setq form (cdr form)))
      (byte-compile-constant 0)
    (byte-compile-form (car form))
    (if (cdr form)
	(while (setq form (cdr form))
	  (byte-compile-form (car form))
	  (byte-compile-out 'byte-diff 0))
      (byte-compile-out 'byte-negate 0))))

(defun byte-compile-quo (form)
  (let ((len (length form)))
    (cond ((<= len 2)
	   (byte-compile-subr-wrong-args form "2 or more"))
	  (t
	   (byte-compile-form (car (setq form (cdr form))))
	   (while (setq form (cdr form))
	     (byte-compile-form (car form))
	     (byte-compile-out 'byte-quo 0))))))

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

(defun byte-compile-fset (form)
  ;; warn about forms like (fset 'foo '(lambda () ...))
  ;; (where the lambda expression is non-trivial...)
  (let ((fn (nth 2 form))
	body)
    (if (and (eq (car-safe fn) 'quote)
	     (eq (car-safe (setq fn (nth 1 fn))) 'lambda))
	(progn
	  (setq body (cdr (cdr fn)))
	  (if (stringp (car body)) (setq body (cdr body)))
	  (if (eq 'interactive (car-safe (car body))) (setq body (cdr body)))
	  (if (and (consp (car body))
		   (not (eq 'byte-code (car (car body)))))
	      (byte-compile-warn
      "A quoted lambda form is the second argument of fset.  This is probably
     not what you want, as that lambda cannot be compiled.  Consider using
     the syntax (function (lambda (...) ...)) instead.")))))
  (byte-compile-two-args form))

(defun byte-compile-funarg (form)
  ;; (mapcar '(lambda (x) ..) ..) ==> (mapcar (function (lambda (x) ..)) ..)
  ;; for cases where it's guaranteed that first arg will be used as a lambda.
  (byte-compile-normal-call
   (let ((fn (nth 1 form)))
     (if (and (eq (car-safe fn) 'quote)
	      (eq (car-safe (nth 1 fn)) 'lambda))
	 (cons (car form)
	       (cons (cons 'function (cdr fn))
		     (cdr (cdr form))))
       form))))

(defun byte-compile-funarg-2 (form)
  ;; (sort ... '(lambda (x) ..)) ==> (sort ... (function (lambda (x) ..)))
  ;; for cases where it's guaranteed that second arg will be used as a lambda.
  (byte-compile-normal-call
   (let ((fn (nth 2 form)))
     (if (and (eq (car-safe fn) 'quote)
	      (eq (car-safe (nth 1 fn)) 'lambda))
	 (cons (car form)
	       (cons (nth 1 form)
		     (cons (cons 'function (cdr fn))
			   (cdr (cdr (cdr form))))))
       form))))

;; (function foo) must compile like 'foo, not like (symbol-function 'foo).
;; Otherwise it will be incompatible with the interpreter,
;; and (funcall (function foo)) will lose with autoloads.

(defun byte-compile-function-form (form)
  (byte-compile-constant
   (cond ((symbolp (nth 1 form))
	  (nth 1 form))
	 ;; If we're not allowed to use #[] syntax, then output a form like
	 ;; '(lambda (..) (byte-code ..)) instead of a call to make-byte-code.
	 ;; In this situation, calling make-byte-code at run-time will usually
	 ;; be less efficient than processing a call to byte-code.
	 ((byte-compile-version-cond byte-compile-compatibility)
	  (byte-compile-byte-code-unmake (byte-compile-lambda (nth 1 form))))
	 ((byte-compile-lambda (nth 1 form))))))

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
	((and (not (byte-compile-version-cond
		    byte-compile-compatibility))
	      (<= (length form) 256))
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
(byte-defop-compiler-1 setq-default)
(byte-defop-compiler-1 quote)
(byte-defop-compiler-1 quote-form)

(defun byte-compile-setq (form)
  (let ((args (cdr form)))
    (if args
	(while args
	  (byte-compile-form (car (cdr args)))
	  (or for-effect (cdr (cdr args))
	      (byte-compile-out 'byte-dup 0))
	  (byte-compile-variable-ref 'byte-varset (car args))
	  (setq args (cdr (cdr args))))
      ;; (setq), with no arguments.
      (byte-compile-form nil for-effect))
    (setq for-effect nil)))

(defun byte-compile-setq-default (form)
  (let ((args (cdr form))
	setters)
    (while args
      (setq setters
	    (cons (list 'set-default (list 'quote (car args)) (car (cdr args)))
		  setters))
      (setq args (cdr (cdr args))))
    (byte-compile-form (cons 'progn (nreverse setters)))))

(defun byte-compile-quote (form)
  (byte-compile-constant (car (cdr form))))

(defun byte-compile-quote-form (form)
  (byte-compile-constant (byte-compile-top-level (nth 1 form))))


;;; control structures

(defun byte-compile-body (body &optional for-effect)
  (while (cdr body)
    (byte-compile-form (car body) t)
    (setq body (cdr body)))
  (byte-compile-form (car body) for-effect))

(defsubst byte-compile-body-do-effect (body)
  (byte-compile-body body for-effect)
  (setq for-effect nil))

(defsubst byte-compile-form-do-effect (form)
  (byte-compile-form form for-effect)
  (setq for-effect nil))

(byte-defop-compiler-1 inline byte-compile-progn)
(byte-defop-compiler-1 progn)
(byte-defop-compiler-1 prog1)
(byte-defop-compiler-1 prog2)
(byte-defop-compiler-1 if)
(byte-defop-compiler-1 cond)
(byte-defop-compiler-1 and)
(byte-defop-compiler-1 or)
(byte-defop-compiler-1 while)
(byte-defop-compiler-1 funcall)
(byte-defop-compiler-1 apply byte-compile-funarg)
(byte-defop-compiler-1 mapcar byte-compile-funarg)
(byte-defop-compiler-1 mapatoms byte-compile-funarg)
(byte-defop-compiler-1 mapconcat byte-compile-funarg)
(byte-defop-compiler-1 mapc byte-compile-funarg)
(byte-defop-compiler-1 sort byte-compile-funarg-2)
(byte-defop-compiler-1 let)
(byte-defop-compiler-1 let*)

(defun byte-compile-progn (form)
  (byte-compile-body-do-effect (cdr form)))

(defun byte-compile-prog1 (form)
  (byte-compile-form-do-effect (car (cdr form)))
  (byte-compile-body (cdr (cdr form)) t))

(defun byte-compile-prog2 (form)
  (byte-compile-form (nth 1 form) t)
  (byte-compile-form-do-effect (nth 2 form))
  (byte-compile-body (cdr (cdr (cdr form))) t))

(defmacro byte-compile-goto-if (cond discard tag)
  `(byte-compile-goto
    (if ,cond
	(if ,discard 'byte-goto-if-not-nil 'byte-goto-if-not-nil-else-pop)
      (if ,discard 'byte-goto-if-nil 'byte-goto-if-nil-else-pop))
    ,tag))

(defun byte-compile-if (form)
  (byte-compile-form (car (cdr form)))
  (if (null (nthcdr 3 form))
      ;; No else-forms
      (let ((donetag (byte-compile-make-tag)))
	(byte-compile-goto-if nil for-effect donetag)
	(byte-compile-form (nth 2 form) for-effect)
	(byte-compile-out-tag donetag))
    (let ((donetag (byte-compile-make-tag)) (elsetag (byte-compile-make-tag)))
      (byte-compile-goto 'byte-goto-if-nil elsetag)
      (byte-compile-form (nth 2 form) for-effect)
      (byte-compile-goto 'byte-goto donetag)
      (byte-compile-out-tag elsetag)
      (byte-compile-body (cdr (cdr (cdr form))) for-effect)
      (byte-compile-out-tag donetag)))
  (setq for-effect nil))

(defun byte-compile-cond (clauses)
  (let ((donetag (byte-compile-make-tag))
	nexttag clause)
    (while (setq clauses (cdr clauses))
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
		 (byte-compile-goto-if t for-effect donetag)
	       (setq nexttag (byte-compile-make-tag))
	       (byte-compile-goto 'byte-goto-if-nil nexttag)
	       (byte-compile-body (cdr clause) for-effect)
	       (byte-compile-goto 'byte-goto donetag)
	       (byte-compile-out-tag nexttag)))))
    ;; Last clause
    (and (cdr clause) (not (eq (car clause) t))
	 (progn (byte-compile-form (car clause))
		(byte-compile-goto-if nil for-effect donetag)
		(setq clause (cdr clause))))
    (byte-compile-body-do-effect clause)
    (byte-compile-out-tag donetag)))

(defun byte-compile-and (form)
  (let ((failtag (byte-compile-make-tag))
	(args (cdr form)))
    (if (null args)
	(byte-compile-form-do-effect t)
      (while (cdr args)
	(byte-compile-form (car args))
	(byte-compile-goto-if nil for-effect failtag)
	(setq args (cdr args)))
      (byte-compile-form-do-effect (car args))
      (byte-compile-out-tag failtag))))

(defun byte-compile-or (form)
  (let ((wintag (byte-compile-make-tag))
	(args (cdr form)))
    (if (null args)
	(byte-compile-form-do-effect nil)
      (while (cdr args)
	(byte-compile-form (car args))
	(byte-compile-goto-if t for-effect wintag)
	(setq args (cdr args)))
      (byte-compile-form-do-effect (car args))
      (byte-compile-out-tag wintag))))

(defun byte-compile-while (form)
  (let ((endtag (byte-compile-make-tag))
	(looptag (byte-compile-make-tag)))
    (byte-compile-out-tag looptag)
    (byte-compile-form (car (cdr form)))
    (byte-compile-goto-if nil for-effect endtag)
    (byte-compile-body (cdr (cdr form)) t)
    (byte-compile-goto 'byte-goto looptag)
    (byte-compile-out-tag endtag)
    (setq for-effect nil)))

(defun byte-compile-funcall (form)
  (mapc 'byte-compile-form (cdr form))
  (byte-compile-out 'byte-call (length (cdr (cdr form)))))


(defun byte-compile-let (form)
  ;; First compute the binding values in the old scope.
  (let ((varlist (car (cdr form))))
    (while varlist
      (if (consp (car varlist))
	  (byte-compile-form (car (cdr (car varlist))))
	(byte-compile-push-constant nil))
      (setq varlist (cdr varlist))))
  (let ((byte-compile-bound-variables byte-compile-bound-variables) ;new scope
	(varlist (reverse (car (cdr form)))))
    (while varlist
      (byte-compile-variable-ref 'byte-varbind (if (consp (car varlist))
						   (car (car varlist))
						 (car varlist)))
      (setq varlist (cdr varlist)))
    (byte-compile-body-do-effect (cdr (cdr form)))
    (byte-compile-out 'byte-unbind (length (car (cdr form))))))

(defun byte-compile-let* (form)
  (let ((byte-compile-bound-variables byte-compile-bound-variables) ;new scope
	(varlist (copy-sequence (car (cdr form)))))
    (while varlist
      (if (atom (car varlist))
	  (byte-compile-push-constant nil)
	(byte-compile-form (car (cdr (car varlist))))
	(setcar varlist (car (car varlist))))
      (byte-compile-variable-ref 'byte-varbind (car varlist))
      (setq varlist (cdr varlist)))
    (byte-compile-body-do-effect (cdr (cdr form)))
    (byte-compile-out 'byte-unbind (length (car (cdr form))))))


(byte-defop-compiler-1 /= byte-compile-negated)
(byte-defop-compiler-1 atom byte-compile-negated)
(byte-defop-compiler-1 nlistp byte-compile-negated)

(put '/= 'byte-compile-negated-op '=)
(put 'atom 'byte-compile-negated-op 'consp)
(put 'nlistp 'byte-compile-negated-op 'listp)

(defun byte-compile-negated (form)
  (byte-compile-form-do-effect (byte-compile-negation-optimizer form)))

;; Even when optimization is off, /= is optimized to (not (= ...)).
(defun byte-compile-negation-optimizer (form)
  ;; an optimizer for forms where <form1> is less efficient than (not <form2>)
  (byte-compile-set-symbol-position (car form))
  (list 'not
    (cons (or (get (car form) 'byte-compile-negated-op)
	      (error
	       "Compiler error: `%s' has no `byte-compile-negated-op' property"
	       (car form)))
	  (cdr form))))

;;; other tricky macro-like special-forms

(byte-defop-compiler-1 catch)
(byte-defop-compiler-1 unwind-protect)
(byte-defop-compiler-1 condition-case)
(byte-defop-compiler-1 save-excursion)
(byte-defop-compiler-1 save-current-buffer)
(byte-defop-compiler-1 save-restriction)
(byte-defop-compiler-1 save-window-excursion)
(byte-defop-compiler-1 with-output-to-temp-buffer)
(byte-defop-compiler-1 track-mouse)

(defun byte-compile-catch (form)
  (byte-compile-form (car (cdr form)))
  (byte-compile-push-constant
    (byte-compile-top-level (cons 'progn (cdr (cdr form))) for-effect))
  (byte-compile-out 'byte-catch 0))

(defun byte-compile-unwind-protect (form)
  (byte-compile-push-constant
   (byte-compile-top-level-body (cdr (cdr form)) t))
  (byte-compile-out 'byte-unwind-protect 0)
  (byte-compile-form-do-effect (car (cdr form)))
  (byte-compile-out 'byte-unbind 1))

(defun byte-compile-track-mouse (form)
  (byte-compile-form
   (list
    'funcall
    (list 'quote
	  (list 'lambda nil
		(cons 'track-mouse
		      (byte-compile-top-level-body (cdr form))))))))

(defun byte-compile-condition-case (form)
  (let* ((var (nth 1 form))
	 (byte-compile-bound-variables
	  (if var (cons var byte-compile-bound-variables)
	    byte-compile-bound-variables)))
    (byte-compile-set-symbol-position 'condition-case)
    (unless (symbolp var)
      (byte-compile-warn
       "%s is not a variable-name or nil (in condition-case)" var))
    (byte-compile-push-constant var)
    (byte-compile-push-constant (byte-compile-top-level
				 (nth 2 form) for-effect))
    (let ((clauses (cdr (cdr (cdr form))))
	  compiled-clauses)
      (while clauses
	(let* ((clause (car clauses))
               (condition (car clause)))
          (cond ((not (or (symbolp condition)
			  (and (listp condition)
			       (let ((syms condition) (ok t))
				 (while syms
				   (if (not (symbolp (car syms)))
				       (setq ok nil))
				   (setq syms (cdr syms)))
				 ok))))
                 (byte-compile-warn
                   "%s is not a condition name or list of such (in condition-case)"
                   (prin1-to-string condition)))
;;                ((not (or (eq condition 't)
;;			  (and (stringp (get condition 'error-message))
;;			       (consp (get condition 'error-conditions)))))
;;                 (byte-compile-warn
;;                   "%s is not a known condition name (in condition-case)"
;;                   condition))
		)
	  (setq compiled-clauses
		(cons (cons condition
			    (byte-compile-top-level-body
			     (cdr clause) for-effect))
		      compiled-clauses)))
	(setq clauses (cdr clauses)))
      (byte-compile-push-constant (nreverse compiled-clauses)))
    (byte-compile-out 'byte-condition-case 0)))


(defun byte-compile-save-excursion (form)
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

(defun byte-compile-save-window-excursion (form)
  (byte-compile-push-constant
   (byte-compile-top-level-body (cdr form) for-effect))
  (byte-compile-out 'byte-save-window-excursion 0))

(defun byte-compile-with-output-to-temp-buffer (form)
  (byte-compile-form (car (cdr form)))
  (byte-compile-out 'byte-temp-output-buffer-setup 0)
  (byte-compile-body (cdr (cdr form)))
  (byte-compile-out 'byte-temp-output-buffer-show 0))


;;; top-level forms elsewhere

(byte-defop-compiler-1 defun)
(byte-defop-compiler-1 defmacro)
(byte-defop-compiler-1 defvar)
(byte-defop-compiler-1 defconst byte-compile-defvar)
(byte-defop-compiler-1 autoload)
(byte-defop-compiler-1 lambda byte-compile-lambda-form)
(byte-defop-compiler-1 defalias)

(defun byte-compile-defun (form)
  ;; This is not used for file-level defuns with doc strings.
  (if (symbolp (car form))
      (byte-compile-set-symbol-position (car form))
    (byte-compile-set-symbol-position 'defun)
    (error "defun name must be a symbol, not %s" (car form)))
  (byte-compile-two-args ; Use this to avoid byte-compile-fset's warning.
   (list 'fset (list 'quote (nth 1 form))
	 (byte-compile-byte-code-maker
	  (byte-compile-lambda (cons 'lambda (cdr (cdr form)))))))
  (byte-compile-discard)
  (byte-compile-constant (nth 1 form)))

(defun byte-compile-defmacro (form)
  ;; This is not used for file-level defmacros with doc strings.
  (byte-compile-body-do-effect
   (list (list 'fset (list 'quote (nth 1 form))
	       (let ((code (byte-compile-byte-code-maker
			    (byte-compile-lambda
			     (cons 'lambda (cdr (cdr form)))))))
		 (if (eq (car-safe code) 'make-byte-code)
		     (list 'cons ''macro code)
		   (list 'quote (cons 'macro (eval code))))))
	 (list 'quote (nth 1 form)))))

(defun byte-compile-defvar (form)
  ;; This is not used for file-level defvar/consts with doc strings.
  (let ((fun (nth 0 form))
	(var (nth 1 form))
	(value (nth 2 form))
	(string (nth 3 form)))
    (byte-compile-set-symbol-position fun)
    (when (> (length form) 4)
      (byte-compile-warn
       "%s %s called with %d arguments, but accepts only %s"
       fun var (length (cdr form)) 3))
    (when (memq 'free-vars byte-compile-warnings)
      (setq byte-compile-bound-variables
	    (cons var byte-compile-bound-variables)))
    (byte-compile-body-do-effect
     (list
      ;; Put the defined variable in this library's load-history entry
      ;; just as a real defvar would, but only in top-level forms.
      (when (and (cddr form) (null byte-compile-current-form))
	`(push ',var current-load-list))
      (when (> (length form) 3)
	(when (and string (not (stringp string)))
	  (byte-compile-warn "third arg to %s %s is not a string: %s"
			     fun var string))
	`(put ',var 'variable-documentation ,string))
      (if (cddr form)		; `value' provided
	  (if (eq fun 'defconst)
	      ;; `defconst' sets `var' unconditionally.
	      (let ((tmp (make-symbol "defconst-tmp-var")))
		`(let ((,tmp ,value))
		   (eval '(defconst ,var ,tmp))))
	    ;; `defvar' sets `var' only when unbound.
	    `(if (not (boundp ',var)) (setq ,var ,value))))
      `',var))))

(defun byte-compile-autoload (form)
  (byte-compile-set-symbol-position 'autoload)
  (and (byte-compile-constp (nth 1 form))
       (byte-compile-constp (nth 5 form))
       (eval (nth 5 form))  ; macro-p
       (not (fboundp (eval (nth 1 form))))
       (byte-compile-warn
	"The compiler ignores `autoload' except at top level.  You should
     probably put the autoload of the macro `%s' at top-level."
	(eval (nth 1 form))))
  (byte-compile-normal-call form))

;; Lambdas in valid places are handled as special cases by various code.
;; The ones that remain are errors.
(defun byte-compile-lambda-form (form)
  (byte-compile-set-symbol-position 'lambda)
  (error "`lambda' used as function name is invalid"))

;; Compile normally, but deal with warnings for the function being defined.
(defun byte-compile-defalias (form)
  (if (and (consp (cdr form)) (consp (nth 1 form))
	   (eq (car (nth 1 form)) 'quote)
	   (consp (cdr (nth 1 form)))
	   (symbolp (nth 1 (nth 1 form)))
	   (consp (nthcdr 2 form))
	   (consp (nth 2 form))
	   (eq (car (nth 2 form)) 'quote)
	   (consp (cdr (nth 2 form)))
	   (symbolp (nth 1 (nth 2 form))))
      (progn
	(byte-compile-defalias-warn (nth 1 (nth 1 form))
				    (nth 1 (nth 2 form)))
	(setq byte-compile-function-environment
	      (cons (cons (nth 1 (nth 1 form))
			  (nth 1 (nth 2 form)))
		    byte-compile-function-environment))))
  (byte-compile-normal-call form))

;; Turn off warnings about prior calls to the function being defalias'd.
;; This could be smarter and compare those calls with
;; the function it is being aliased to.
(defun byte-compile-defalias-warn (new alias)
  (let ((calls (assq new byte-compile-unresolved-functions)))
    (if calls
	(setq byte-compile-unresolved-functions
	      (delq calls byte-compile-unresolved-functions)))))

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
  (setq byte-compile-output (cons (cons opcode tag) byte-compile-output))
  (setcdr (cdr tag) (if (memq opcode byte-goto-always-pop-ops)
			(1- byte-compile-depth)
		      byte-compile-depth))
  (setq byte-compile-depth (and (not (eq opcode 'byte-goto))
				(1- byte-compile-depth))))

(defun byte-compile-out (opcode offset)
  (setq byte-compile-output (cons (cons opcode offset) byte-compile-output))
  (cond ((eq opcode 'byte-call)
	 (setq byte-compile-depth (- byte-compile-depth offset)))
	((eq opcode 'byte-return)
	 ;; This is actually an unnecessary case, because there should be
	 ;; no more opcodes behind byte-return.
	 (setq byte-compile-depth nil))
	(t
	 (setq byte-compile-depth (+ byte-compile-depth
				     (or (aref byte-stack+-info
					       (symbol-value opcode))
					 (- (1- offset))))
	       byte-compile-maxdepth (max byte-compile-depth
					  byte-compile-maxdepth))))
  ;;(if (< byte-compile-depth 0) (error "Compiler error: stack underflow"))
  )


;;; call tree stuff

(defun byte-compile-annotate-call-tree (form)
  (let (entry)
    ;; annotate the current call
    (if (setq entry (assq (car form) byte-compile-call-tree))
	(or (memq byte-compile-current-form (nth 1 entry)) ;callers
	    (setcar (cdr entry)
		    (cons byte-compile-current-form (nth 1 entry))))
      (setq byte-compile-call-tree
	    (cons (list (car form) (list byte-compile-current-form) nil)
		  byte-compile-call-tree)))
    ;; annotate the current function
    (if (setq entry (assq byte-compile-current-form byte-compile-call-tree))
	(or (memq (car form) (nth 2 entry)) ;called
	    (setcar (cdr (cdr entry))
		    (cons (car form) (nth 2 entry))))
      (setq byte-compile-call-tree
	    (cons (list byte-compile-current-form nil (list (car form)))
		  byte-compile-call-tree)))
    ))

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
primitives that the byte-code interpreter knows about directly \(eq,
cons, etc.\).

The call tree also lists those functions which are not known to be called
\(that is, to which no calls have been compiled\), and which cannot be
invoked interactively."
  (interactive)
  (message "Generating call tree...")
  (with-output-to-temp-buffer "*Call-Tree*"
    (set-buffer "*Call-Tree*")
    (erase-buffer)
    (message "Generating call tree... (sorting on %s)"
	     byte-compile-call-tree-sort)
    (insert "Call tree for "
	    (cond ((null byte-compile-current-file) (or filename "???"))
		  ((stringp byte-compile-current-file)
		   byte-compile-current-file)
		  (t (buffer-name byte-compile-current-file)))
	    " sorted on "
	    (prin1-to-string byte-compile-call-tree-sort)
	    ":\n\n")
    (if byte-compile-call-tree-sort
	(setq byte-compile-call-tree
	      (sort byte-compile-call-tree
		    (cond ((eq byte-compile-call-tree-sort 'callers)
			   (function (lambda (x y) (< (length (nth 1 x))
						      (length (nth 1 y))))))
			  ((eq byte-compile-call-tree-sort 'calls)
			   (function (lambda (x y) (< (length (nth 2 x))
						      (length (nth 2 y))))))
			  ((eq byte-compile-call-tree-sort 'calls+callers)
			   (function (lambda (x y) (< (+ (length (nth 1 x))
							 (length (nth 2 x)))
						      (+ (length (nth 1 y))
							 (length (nth 2 y)))))))
			  ((eq byte-compile-call-tree-sort 'name)
			   (function (lambda (x y) (string< (car x)
							    (car y)))))
			  (t (error "`byte-compile-call-tree-sort': `%s' - unknown sort mode"
				    byte-compile-call-tree-sort))))))
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
		((subrp (setq f (symbol-function f)))
		 " <subr>")
		((symbolp f)
		 (format " ==> %s" f))
		((byte-code-function-p f)
		 "<compiled function>")
		((not (consp f))
		 "<malformed function>")
		((eq 'macro (car f))
		 (if (or (byte-code-function-p (cdr f))
			 (assq 'byte-code (cdr (cdr (cdr f)))))
		     " <compiled macro>"
		   " <macro>"))
		((assq 'byte-code (cdr (cdr f)))
		 "<compiled lambda>")
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
		(fill-region-as-paragraph p (point)))))
	(if calls
	    (progn
	      (insert "  calls:\n")
	      (setq p (point))
	      (insert "    " (mapconcat 'symbol-name calls ", "))
	      (let ((fill-prefix "    "))
		(fill-region-as-paragraph p (point)))))
	(insert "\n")
	(setq rest (cdr rest)))

      (message "Generating call tree...(finding uncalled functions...)")
      (setq rest byte-compile-call-tree)
      (let ((uncalled nil))
	(while rest
	  (or (nth 1 (car rest))
	      (null (setq f (car (car rest))))
	      (byte-compile-fdefinition f t)
	      (commandp (byte-compile-fdefinition f nil))
	      (setq uncalled (cons f uncalled)))
	  (setq rest (cdr rest)))
	(if uncalled
	    (let ((fill-prefix "  "))
	      (insert "Noninteractive functions not known to be called:\n  ")
	      (setq p (point))
	      (insert (mapconcat 'symbol-name (nreverse uncalled) ", "))
	      (fill-region-as-paragraph p (point)))))
      )
    (message "Generating call tree...done.")
    ))


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
Each file is processed even if an error occurred previously.
For example, invoke \"emacs -batch -f batch-byte-compile $emacs/ ~/*.el\".
If NOFORCE is non-nil, don't recompile a file that seems to be
already up-to-date."
  ;; command-line-args-left is what is left of the command line (from startup.el)
  (defvar command-line-args-left)	;Avoid 'free variable' warning
  (if (not noninteractive)
      (error "`batch-byte-compile' is to be used only with -batch"))
  (let ((error nil))
    (while command-line-args-left
      (if (file-directory-p (expand-file-name (car command-line-args-left)))
	  ;; Directory as argument.
	  (let ((files (directory-files (car command-line-args-left)))
		source dest)
	    (dolist (file files)
	      (if (and (string-match emacs-lisp-file-regexp file)
		       (not (auto-save-file-name-p file))
		       (setq source (expand-file-name file
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
  (condition-case err
      (byte-compile-file file)
    (error
     (message (if (cdr err)
		  ">>Error occurred processing %s: %s (%s)"
		  ">>Error occurred processing %s: %s")
	      file
	      (get (car err) 'error-message)
	      (prin1-to-string (cdr err)))
     nil)))

;;;###autoload
(defun batch-byte-recompile-directory ()
  "Runs `byte-recompile-directory' on the dirs remaining on the command line.
Must be used only with `-batch', and kills Emacs on completion.
For example, invoke `emacs -batch -f batch-byte-recompile-directory .'."
  ;; command-line-args-left is what is left of the command line (startup.el)
  (defvar command-line-args-left)	;Avoid 'free variable' warning
  (if (not noninteractive)
      (error "batch-byte-recompile-directory is to be used only with -batch"))
  (or command-line-args-left
      (setq command-line-args-left '(".")))
  (while command-line-args-left
    (byte-recompile-directory (car command-line-args-left) 0)
    (setq command-line-args-left (cdr command-line-args-left)))
  (kill-emacs 0))


(make-obsolete 'dot 'point		"before 19.15")
(make-obsolete 'dot-max 'point-max	"before 19.15")
(make-obsolete 'dot-min 'point-min	"before 19.15")
(make-obsolete 'dot-marker 'point-marker "before 19.15")

(make-obsolete 'buffer-flush-undo 'buffer-disable-undo "before 19.15")
(make-obsolete 'baud-rate "use the baud-rate variable instead" "before 19.15")
(make-obsolete 'compiled-function-p 'byte-code-function-p "before 19.15")
(make-obsolete 'define-function 'defalias "20.1")
(make-obsolete-variable 'auto-fill-hook 'auto-fill-function "before 19.15")
(make-obsolete-variable 'blink-paren-hook 'blink-paren-function "before 19.15")
(make-obsolete-variable 'lisp-indent-hook 'lisp-indent-function "before 19.15")
(make-obsolete-variable 'inhibit-local-variables
		"use enable-local-variables (with the reversed sense)."
		"before 19.15")
(make-obsolete-variable 'unread-command-char
  "use unread-command-events instead.  That variable is a list of events to reread, so it now uses nil to mean `no event', instead of -1."
  "before 19.15")
(make-obsolete-variable 'unread-command-event
  "use unread-command-events; which is a list of events rather than a single event."
  "before 19.15")
(make-obsolete-variable 'suspend-hooks 'suspend-hook "before 19.15")
(make-obsolete-variable 'comment-indent-hook 'comment-indent-function "before 19.15")
(make-obsolete-variable 'meta-flag "Use the set-input-mode function instead." "before 19.34")
(make-obsolete-variable 'executing-macro 'executing-kbd-macro "before 19.34")
(make-obsolete-variable 'before-change-function
  "use before-change-functions; which is a list of functions rather than a single function."
  "before 19.34")
(make-obsolete-variable 'after-change-function
  "use after-change-functions; which is a list of functions rather than a single function."
  "before 19.34")
(make-obsolete-variable 'font-lock-doc-string-face 'font-lock-string-face "before 19.34")
(make-obsolete-variable 'post-command-idle-hook
  "use timers instead, with `run-with-idle-timer'." "before 19.34")
(make-obsolete-variable 'post-command-idle-delay
  "use timers instead, with `run-with-idle-timer'." "before 19.34")

(provide 'byte-compile)
(provide 'bytecomp)


;;; report metering (see the hacks in bytecode.c)

(defun byte-compile-report-ops ()
  (defvar byte-code-meter)
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
  (or (byte-code-function-p (symbol-function 'byte-compile-form))
      (assq 'byte-code (symbol-function 'byte-compile-form))
      (let ((byte-optimize nil)		; do it fast
	    (byte-compile-warnings nil))
	(mapcar (lambda (x)
		  (or noninteractive (message "compiling %s..." x))
		  (byte-compile x)
		  (or noninteractive (message "compiling %s...done" x)))
		'(byte-compile-normal-call
		  byte-compile-form
		  byte-compile-body
		  ;; Inserted some more than necessary, to speed it up.
		  byte-compile-top-level
		  byte-compile-out-toplevel
		  byte-compile-constant
		  byte-compile-variable-ref))))
  nil)

(run-hooks 'bytecomp-load-hook)

;;; bytecomp.el ends here
