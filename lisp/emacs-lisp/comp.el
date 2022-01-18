;;; comp.el --- compilation of Lisp code into native code -*- lexical-binding: t -*-

;; Copyright (C) 2019-2022 Free Software Foundation, Inc.

;; Author: Andrea Corallo <akrl@sdf.com>
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

;; This code is an attempt to make the pig fly.
;; Or, to put it another way to make a 911 out of a turbocharged VW Bug.

;;; Code:

(require 'bytecomp)
(require 'cl-extra)
(require 'cl-lib)
(require 'cl-macs)
(require 'cl-seq)
(require 'gv)
(require 'rx)
(require 'subr-x)
(require 'warnings)
(require 'comp-cstr)

(defgroup comp nil
  "Emacs Lisp native compiler."
  :group 'lisp)

(defcustom native-comp-speed 2
  "Optimization level for native compilation, a number between -1 and 3.
 -1 functions are kept in bytecode form and no native compilation is performed.
  0 native compilation is performed with no optimizations.
  1 light optimizations.
  2 max optimization level fully adherent to the language semantic.
  3 max optimization level, to be used only when necessary.
    Warning: with 3, the compiler is free to perform dangerous optimizations."
  :type 'integer
  :safe #'integerp
  :version "28.1")

(defcustom native-comp-debug (if (eq 'windows-nt system-type) 1 0)
  "Debug level for native compilation, a number between 0 and 3.
This is intended for debugging the compiler itself.
  0 no debug output.
  1 emit debug symbols.
  2 emit debug symbols and dump pseudo C code.
  3 emit debug symbols and dump: pseudo C code, GCC intermediate
  passes and libgccjit log file."
  :type 'integer
  :safe #'natnump
  :version "28.1")

(defcustom native-comp-verbose 0
  "Compiler verbosity for native compilation, a number between 0 and 3.
This is intended for debugging the compiler itself.
  0 no logging.
  1 final LIMPLE is logged.
  2 LAP, final LIMPLE, and some pass info are logged.
  3 max verbosity."
  :type 'integer
  :risky t
  :version "28.1")

(defcustom native-comp-always-compile nil
  "Non-nil means unconditionally (re-)compile all files."
  :type 'boolean
  :version "28.1")

(defcustom native-comp-deferred-compilation-deny-list
  '()
  "List of regexps to exclude matching files from deferred native compilation.
Files whose names match any regexp are excluded from native compilation."
  :type '(repeat regexp)
  :version "28.1")

(defcustom native-comp-bootstrap-deny-list
  '()
  "List of regexps to exclude files from native compilation during bootstrap.
Files whose names match any regexp are excluded from native compilation
during bootstrap."
  :type '(repeat regexp)
  :version "28.1")

(defcustom native-comp-never-optimize-functions
  '(;; The following two are mandatory for Emacs to be working
    ;; correctly (see comment in `advice--add-function'). DO NOT
    ;; REMOVE.
    macroexpand rename-buffer)
  "Primitive functions to exclude from trampoline optimization."
  :type '(repeat symbol)
  :version "28.1")

(defcustom native-comp-async-jobs-number 0
  "Default number of subprocesses used for async native compilation.
Value of zero means to use half the number of the CPU's execution units,
or one if there's just one execution unit."
  :type 'integer
  :risky t
  :version "28.1")

(defcustom native-comp-async-cu-done-functions nil
  "List of functions to call when asynchronous compilation of a file is done.
Each function is called with one argument FILE, the filename whose
compilation has completed."
  :type 'hook
  :version "28.1")

(defcustom native-comp-async-all-done-hook nil
  "Hook run after completing asynchronous compilation of all input files."
  :type 'hook
  :version "28.1")

(defcustom native-comp-async-env-modifier-form nil
  "Form evaluated before compilation by each asynchronous compilation subprocess.
Used to modify the compiler environment."
  :type 'sexp
  :risky t
  :version "28.1")

(defcustom native-comp-async-report-warnings-errors t
  "Whether to report warnings and errors from asynchronous native compilation.

When native compilation happens asynchronously, it can produce
warnings and errors, some of which might not be emitted by a
byte-compilation.  The typical case for that is native-compiling
a file that is missing some `require' of a necessary feature,
while having it already loaded into the environment when
byte-compiling.

As asynchronous native compilation always starts from a pristine
environment, it is more sensitive to such omissions, and might be
unable to compile such Lisp source files correctly.

Set this variable to nil to suppress warnings altogether, or to
the symbol `silent' to log warnings but not pop up the *Warnings*
buffer."
  :type '(choice
          (const :tag "Do not report warnings" nil)
          (const :tag "Report and display warnings" t)
          (const :tag "Report but do not display warnings" silent))
  :version "28.1")

(defcustom native-comp-async-query-on-exit nil
  "Whether to query the user about killing async compilations when exiting.
If this is non-nil, Emacs will ask for confirmation to exit and kill the
asynchronous native compilations if any are running.  If nil, when you
exit Emacs, it will silently kill those asynchronous compilations even
if `confirm-kill-processes' is non-nil."
  :type 'boolean
  :version "28.1")

(defcustom native-comp-compiler-options nil
  "Command line options passed verbatim to GCC compiler.
Note that not all options are meaningful and some options might even
break your Emacs.  Use at your own risk.

Passing these options is only available in libgccjit version 9
and above."
  :type '(repeat string)
  :version "28.1")

(defcustom native-comp-driver-options nil
  "Options passed verbatim to the native compiler's back-end driver.
Note that not all options are meaningful; typically only the options
affecting the assembler and linker are likely to be useful.

Passing these options is only available in libgccjit version 9
and above."
  :type '(repeat string)                ; FIXME is this right?
  :version "28.1")

(defcustom comp-libgccjit-reproducer nil
  "When non-nil produce a libgccjit reproducer.
The reproducer is a file ELNFILENAME_libgccjit_repro.c deposed in
the .eln output directory."
  :type 'boolean
  :version "28.1")

(defcustom native-comp-warning-on-missing-source t
  "Emit a warning if a byte-code file being loaded has no corresponding source.
The source file is necessary for native code file look-up and deferred
compilation mechanism."
  :type 'boolean
  :version "28.1")

(defvar no-native-compile nil
  "Non-nil to prevent native-compiling of Emacs Lisp code.
Note that when `no-byte-compile' is set to non-nil it overrides the value of
`no-native-compile'.
This is normally set in local file variables at the end of the
Emacs Lisp file:

\;; Local Variables:\n;; no-native-compile: t\n;; End:")
;;;###autoload(put 'no-native-compile 'safe-local-variable 'booleanp)

(defvar native-compile-target-directory nil
  "When non-nil force the target directory for the eln files being compiled.")

(defvar comp-log-time-report nil
  "If non-nil, log a time report for each pass.")

(defvar comp-dry-run nil
  "If non-nil, run everything but the C back-end.")

(defconst comp-valid-source-re (rx ".el" (? ".gz") eos)
  "Regexp to match filename of valid input source files.")

(defconst comp-log-buffer-name "*Native-compile-Log*"
  "Name of the native-compiler log buffer.")

(defconst comp-async-buffer-name "*Async-native-compile-log*"
  "Name of the async compilation buffer log.")

(defvar comp-native-compiling nil
  "This gets bound to t during native compilation.
Intended to be used by code that needs to work differently when
native compilation runs.")

(defvar comp-pass nil
  "Every native-compilation pass can bind this to whatever it likes.")

(defvar comp-curr-allocation-class 'd-default
  "Current allocation class.
Can be one of: 'd-default', 'd-impure' or 'd-ephemeral'.  See `comp-ctxt'.")

(defconst comp-passes '(comp-spill-lap
                        comp-limplify
                        comp-fwprop
                        comp-call-optim
                        comp-ipa-pure
                        comp-add-cstrs
                        comp-fwprop
                        comp-tco
                        comp-fwprop
                        comp-remove-type-hints
                        comp-final)
  "Passes to be executed in order.")

(defvar comp-disabled-passes '()
  "List of disabled passes.
For internal use by the test suite only.")

(defvar comp-post-pass-hooks '()
  "Alist whose elements are of the form (PASS FUNCTIONS...).
Each function in FUNCTIONS is run after PASS.
Useful to hook into pass checkers.")

;; FIXME this probably should not be here but... good for now.
(defconst comp-known-type-specifiers
  `(
    ;; Functions we can trust not to be or if redefined should expose
    ;; the same type.  Vast majority of these is either pure or
    ;; primitive, the original list is the union of pure +
    ;; side-effect-free-fns + side-effect-and-error-free-fns:
    (% (function ((or number marker) (or number marker)) number))
    (* (function (&rest (or number marker)) number))
    (+ (function (&rest (or number marker)) number))
    (- (function (&rest (or number marker)) number))
    (/ (function ((or number marker) &rest (or number marker)) number))
    (/= (function ((or number marker) (or number marker)) boolean))
    (1+ (function ((or number marker)) number))
    (1- (function ((or number marker)) number))
    (< (function ((or number marker) &rest (or number marker)) boolean))
    (<= (function ((or number marker) &rest (or number marker)) boolean))
    (= (function ((or number marker) &rest (or number marker)) boolean))
    (> (function ((or number marker) &rest (or number marker)) boolean))
    (>= (function ((or number marker) &rest (or number marker)) boolean))
    (abs (function (number) number))
    (acos (function (number) float))
    (append (function (&rest t) t))
    (aref (function (t fixnum) t))
    (arrayp (function (t) boolean))
    (ash (function (integer integer) integer))
    (asin (function (number) float))
    (assq (function (t list) list))
    (atan (function (number &optional number) float))
    (atom (function (t) boolean))
    (bignump (function (t) boolean))
    (bobp (function () boolean))
    (bolp (function () boolean))
    (bool-vector-count-consecutive (function (bool-vector boolean integer) fixnum))
    (bool-vector-count-population (function (bool-vector) fixnum))
    (bool-vector-not (function (bool-vector &optional bool-vector) bool-vector))
    (bool-vector-p (function (t) boolean))
    (bool-vector-subsetp (function (bool-vector bool-vector) boolean))
    (boundp (function (symbol) boolean))
    (buffer-end (function ((or number marker)) integer))
    (buffer-file-name (function (&optional buffer) string))
    (buffer-list (function (&optional frame) list))
    (buffer-local-variables (function (&optional buffer) list))
    (buffer-modified-p (function (&optional buffer) boolean))
    (buffer-size (function (&optional buffer) integer))
    (buffer-string (function () string))
    (buffer-substring (function ((or integer marker) (or integer marker)) string))
    (bufferp (function (t) boolean))
    (byte-code-function-p (function (t) boolean))
    (capitalize (function (or integer string) (or integer string)))
    (car (function (list) t))
    (car-less-than-car (function (list list) boolean))
    (car-safe (function (t) t))
    (case-table-p (function (t) boolean))
    (cdr (function (list) t))
    (cdr-safe (function (t) t))
    (ceiling (function (number &optional number) integer))
    (char-after (function (&optional (or marker integer)) fixnum))
    (char-before (function (&optional (or marker integer)) fixnum))
    (char-equal (function (integer integer) boolean))
    (char-or-string-p (function (t) boolean))
    (char-to-string (function (fixnum) string))
    (char-width (function (fixnum) fixnum))
    (characterp (function (t &optional t) boolean))
    (charsetp (function (t) boolean))
    (commandp (function (t &optional t) boolean))
    (compare-strings (function (string (or integer marker null) (or integer marker null) string (or integer marker null) (or integer marker null) &optional t) (or (member t) fixnum)))
    (concat (function (&rest sequence) string))
    (cons (function (t t) cons))
    (consp (function (t) boolean))
    (coordinates-in-window-p (function (cons window) boolean))
    (copy-alist (function (list) list))
    (copy-marker (function (&optional (or integer marker) boolean) marker))
    (copy-sequence (function (sequence) sequence))
    (copysign (function (float float) float))
    (cos (function (number) float))
    (count-lines (function ((or integer marker) (or integer marker) &optional t) integer))
    (current-buffer (function () buffer))
    (current-global-map (function () cons))
    (current-indentation (function () integer))
    (current-local-map (function () cons))
    (current-minor-mode-maps (function () cons))
    (current-time (function () cons))
    (current-time-string (function (&optional string boolean) string))
    (current-time-zone (function (&optional string boolean) cons))
    (custom-variable-p (function (symbol) boolean))
    (decode-char (function (cons t) (or fixnum null)))
    (decode-time (function (&optional string symbol symbol) cons))
    (default-boundp (function (symbol) boolean))
    (default-value (function (symbol) t))
    (degrees-to-radians (function (number) float))
    (documentation (function ((or function symbol subr) &optional t) (or null string)))
    (downcase (function ((or fixnum string)) (or fixnum string)))
    (elt (function (sequence integer) t))
    (encode-char (function (fixnum symbol) (or fixnum null)))
    (encode-time (function (cons &rest t) cons))
    (eobp (function () boolean))
    (eolp (function () boolean))
    (eq (function (t t) boolean))
    (eql (function (t t) boolean))
    (equal (function (t t) boolean))
    (error-message-string (function (list) string))
    (eventp (function (t) boolean))
    (exp (function (number) float))
    (expt (function (number number) float))
    (fboundp (function (symbol) boolean))
    (fceiling (function (float) float))
    (featurep (function (symbol &optional symbol) boolean))
    (ffloor (function (float) float))
    (file-directory-p (function (string) boolean))
    (file-exists-p (function (string) boolean))
    (file-locked-p (function (string) boolean))
    (file-name-absolute-p (function (string) boolean))
    (file-newer-than-file-p (function (string string) boolean))
    (file-readable-p (function (string) boolean))
    (file-symlink-p (function (string) boolean))
    (file-writable-p (function (string) boolean))
    (fixnump (function (t) boolean))
    (float (function (number) float))
    (float-time (function (&optional cons) float))
    (floatp (function (t) boolean))
    (floor (function (number &optional number) integer))
    (following-char (function () fixnum))
    (format (function (string &rest t) string))
    (format-time-string (function (string &optional cons symbol) string))
    (frame-first-window (function ((or frame window)) window))
    (frame-root-window (function (&optional (or frame window)) window))
    (frame-selected-window (function (&optional (or frame window)) window))
    (frame-visible-p (function (frame) boolean))
    (framep (function (t) boolean))
    (fround (function (float) float))
    (ftruncate (function (float) float))
    (get (function (symbol symbol) t))
    (get-buffer (function ((or buffer string)) (or buffer null)))
    (get-buffer-window (function (&optional (or buffer string) (or symbol (integer 0 0))) (or null window)))
    (get-file-buffer (function (string) (or null buffer)))
    (get-largest-window (function (&optional t t t) window))
    (get-lru-window (function (&optional t t t) window))
    (getenv (function (string &optional frame) (or null string)))
    (gethash (function (t hash-table &optional t) t))
    (hash-table-count (function (hash-table) integer))
    (hash-table-p (function (t) boolean))
    (identity (function (t) t))
    (ignore (function (&rest t) null))
    (int-to-string (function (number) string))
    (integer-or-marker-p (function (t) boolean))
    (integerp (function (t) boolean))
    (interactive-p (function () boolean))
    (intern-soft (function ((or string symbol) &optional vector) symbol))
    (invocation-directory (function () string))
    (invocation-name (function () string))
    (isnan (function (float) boolean))
    (keymap-parent (function (cons) (or cons null)))
    (keymapp (function (t) boolean))
    (keywordp (function (t) boolean))
    (last (function (list &optional integer) list))
    (lax-plist-get (function (list t) t))
    (ldexp (function (number integer) float))
    (length (function (t) (integer 0 *)))
    (length< (function (sequence fixnum) boolean))
    (length= (function (sequence fixnum) boolean))
    (length> (function (sequence fixnum) boolean))
    (line-beginning-position (function (&optional integer) integer))
    (line-end-position (function (&optional integer) integer))
    (list (function (&rest t) list))
    (listp (function (t) boolean))
    (local-variable-if-set-p (function (symbol &optional buffer) boolean))
    (local-variable-p (function (symbol &optional buffer) boolean))
    (locale-info (function ((member codeset days months paper)) (or null string)))
    (log (function (number number) float))
    (log10 (function (number) float))
    (logand (function (&rest (or integer marker)) integer))
    (logb (function (number) integer))
    (logcount (function (integer) integer))
    (logior (function (&rest (or integer marker)) integer))
    (lognot (function (integer) integer))
    (logxor (function (&rest (or integer marker)) integer))
    ;; (lsh (function ((integer ,most-negative-fixnum *) integer) integer)) ?
    (lsh (function (integer integer) integer))
    (make-byte-code (function ((or fixnum list) string vector integer &optional string t &rest t) vector))
    (make-list (function (integer t) list))
    (make-marker (function () marker))
    (make-string (function (integer fixnum &optional t) string))
    (make-symbol (function (string) symbol))
    (mark (function (&optional t) (or integer null)))
    (mark-marker (function () marker))
    (marker-buffer (function (marker) buffer))
    (markerp (function (t) boolean))
    (max (function ((or number marker) &rest (or number marker)) number))
    (max-char (function () fixnum))
    (member (function (t list) list))
    (memory-limit (function () integer))
    (memq (function (t list) list))
    (memql (function (t list) list))
    (min (function ((or number marker) &rest (or number marker)) number))
    (minibuffer-selected-window (function () window))
    (minibuffer-window (function (&optional frame) window))
    (mod (function ((or number marker) (or number marker)) (or (integer 0 *) (float 0 *))))
    (mouse-movement-p (function (t) boolean))
    (multibyte-char-to-unibyte (function (fixnum) fixnum))
    (natnump (function (t) boolean))
    (next-window (function (&optional window t t) window))
    (nlistp (function (t) boolean))
    (not (function (t) boolean))
    (nth (function (integer list) t))
    (nthcdr (function (integer t) t))
    (null (function (t) boolean))
    (number-or-marker-p (function (t) boolean))
    (number-to-string (function (number) string))
    (numberp (function (t) boolean))
    (one-window-p (function (&optional t t) boolean))
    (overlayp (function (t) boolean))
    (parse-colon-path (function (string) cons))
    (plist-get (function (list t) t))
    (plist-member (function (list t) list))
    (point (function () integer))
    (point-marker (function () marker))
    (point-max (function () integer))
    (point-min (function () integer))
    (preceding-char (function () fixnum))
    (previous-window (function (&optional window t t) window))
    (prin1-to-string (function (t &optional t) string))
    (processp (function (t) boolean))
    (proper-list-p (function (t) integer))
    (propertize (function (string &rest t) string))
    (radians-to-degrees (function (number) float))
    (rassoc (function (t list) list))
    (rassq (function (t list) list))
    (read-from-string (function (string &optional integer integer) cons))
    (recent-keys (function (&optional (or cons null)) vector))
    (recursion-depth (function () integer))
    (regexp-opt (function (list) string))
    (regexp-quote (function (string) string))
    (region-beginning (function () integer))
    (region-end (function () integer))
    (reverse (function (sequence) sequence))
    (round (function (number &optional number) integer))
    (safe-length (function (t) integer))
    (selected-frame (function () frame))
    (selected-window (function () window))
    (sequencep (function (t) boolean))
    (sin (function (number) float))
    (sqrt (function (number) float))
    (standard-case-table (function () char-table))
    (standard-syntax-table (function () char-table))
    (string (function (&rest fixnum) string))
    (string-as-multibyte (function (string) string))
    (string-as-unibyte (function (string) string))
    (string-equal (function ((or string symbol) (or string symbol)) boolean))
    (string-lessp (function ((or string symbol) (or string symbol)) boolean))
    (string-make-multibyte (function (string) string))
    (string-make-unibyte (function (string) string))
    (string-search (function (string string &optional integer) (or integer null)))
    (string-to-char (function (string) fixnum))
    (string-to-multibyte (function (string) string))
    (string-to-number (function (string &optional integer) number))
    (string-to-syntax (function (string) cons))
    (string< (function ((or string symbol) (or string symbol)) boolean))
    (string= (function ((or string symbol) (or string symbol)) boolean))
    (stringp (function (t) boolean))
    (subrp (function (t) boolean))
    (substring (function ((or string vector) &optional integer integer) (or string vector)))
    (sxhash (function (t) integer))
    (sxhash-eq (function (t) integer))
    (sxhash-eql (function (t) integer))
    (sxhash-equal (function (t) integer))
    (symbol-function (function (symbol) t))
    (symbol-name (function (symbol) string))
    (symbol-plist (function (symbol) list))
    (symbol-value (function (symbol) t))
    (symbolp (function (t) boolean))
    (syntax-table (function () char-table))
    (syntax-table-p (function (t) boolean))
    (tan (function (number) float))
    (this-command-keys (function () string))
    (this-command-keys-vector (function () vector))
    (this-single-command-keys (function () vector))
    (this-single-command-raw-keys (function () vector))
    (time-convert (function (t &optional (or boolean integer)) cons))
    (truncate (function (number &optional number) integer))
    (type-of (function (t) symbol))
    (unibyte-char-to-multibyte (function (fixnum) fixnum)) ;; byte is fixnum
    (upcase (function ((or fixnum string)) (or fixnum string)))
    (user-full-name (function (&optional integer) (or string null)))
    (user-login-name (function (&optional integer) (or string null)))
    (user-original-login-name (function (&optional integer) (or string null)))
    (user-real-login-name (function () string))
    (user-real-uid (function () integer))
    (user-uid (function () integer))
    (vconcat (function (&rest sequence) vector))
    (vector (function (&rest t) vector))
    (vectorp (function (t) boolean))
    (visible-frame-list (function () list))
    (wholenump (function (t) boolean))
    (window-configuration-p (function (t) boolean))
    (window-live-p (function (t) boolean))
    (window-valid-p (function (t) boolean))
    (windowp (function (t) boolean))
    (zerop (function (number) boolean))
    ;; Type hints
    (comp-hint-fixnum (function (t) fixnum))
    (comp-hint-cons (function (t) cons))
    ;; Non returning functions
    (throw (function (t t) nil))
    (error (function (string &rest t) nil))
    (signal (function (symbol t) nil)))
  "Alist used for type propagation.")

(defconst comp-known-func-cstr-h
  (cl-loop
   with comp-ctxt = (make-comp-cstr-ctxt)
   with h = (make-hash-table :test #'eq)
   for (f type-spec) in comp-known-type-specifiers
   for cstr = (comp-type-spec-to-cstr type-spec)
   do (puthash f cstr h)
   finally return h)
  "Hash table function -> `comp-constraint'.")

(defconst comp-known-predicates
  '((arrayp              . array)
    (atom		 . atom)
    (characterp          . fixnum)
    (booleanp            . boolean)
    (bool-vector-p       . bool-vector)
    (bufferp             . buffer)
    (natnump             . (integer 0 *))
    (char-table-p	 . char-table)
    (hash-table-p	 . hash-table)
    (consp               . cons)
    (integerp            . integer)
    (floatp              . float)
    (functionp           . (or function symbol))
    (integerp            . integer)
    (keywordp            . keyword)
    (listp               . list)
    (numberp             . number)
    (null		 . null)
    (numberp             . number)
    (sequencep           . sequence)
    (stringp             . string)
    (symbolp             . symbol)
    (vectorp             . vector)
    (integer-or-marker-p . integer-or-marker))
  "Alist predicate -> matched type specifier.")

(defconst comp-known-predicates-h
  (cl-loop
   with comp-ctxt = (make-comp-cstr-ctxt)
   with h = (make-hash-table :test #'eq)
   for (pred . type-spec) in comp-known-predicates
   for cstr = (comp-type-spec-to-cstr type-spec)
   do (puthash pred cstr h)
   finally return h)
  "Hash table function -> `comp-constraint'.")

(defun comp-known-predicate-p (predicate)
  "Return t if PREDICATE is known."
  (when (gethash predicate comp-known-predicates-h) t))

(defun comp-pred-to-cstr (predicate)
  "Given PREDICATE, return the corresponding constraint."
  (gethash predicate comp-known-predicates-h))

(defconst comp-symbol-values-optimizable '(most-positive-fixnum
                                           most-negative-fixnum)
  "Symbol values we can resolve at compile-time.")

(defconst comp-type-hints '(comp-hint-fixnum
                            comp-hint-cons)
  "List of fake functions used to give compiler hints.")

(defconst comp-limple-sets '(set
                             setimm
                             set-par-to-local
                             set-args-to-local
                             set-rest-args-to-local)
  "Limple set operators.")

(defconst comp-limple-assignments `(assume
                                    fetch-handler
                                    ,@comp-limple-sets)
  "Limple operators that clobber the first m-var argument.")

(defconst comp-limple-calls '(call
                              callref
                              direct-call
                              direct-callref)
  "Limple operators used to call subrs.")

(defconst comp-limple-branches '(jump cond-jump)
  "Limple operators used for conditional and unconditional branches.")

(defconst comp-limple-ops `(,@comp-limple-calls
                            ,@comp-limple-assignments
                            ,@comp-limple-branches
                            return)
  "All Limple operators.")

(defvar comp-func nil
  "Bound to the current function by most passes.")

(defvar comp-block nil
  "Bound to the current basic block by some passes.")

(define-error 'native-compiler-error-dyn-func
  "can't native compile a non-lexically-scoped function"
  'native-compiler-error)
(define-error 'native-compiler-error-empty-byte
  "empty byte compiler output"
  'native-compiler-error)


;; Moved early to avoid circularity when comp.el is loaded and
;; `macroexpand' needs to be advised (bug#47049).
;;;###autoload
(defun comp-subr-trampoline-install (subr-name)
  "Make SUBR-NAME effectively advice-able when called from native code."
  (unless (or (null comp-enable-subr-trampolines)
              (memq subr-name native-comp-never-optimize-functions)
              (gethash subr-name comp-installed-trampolines-h))
    (cl-assert (subr-primitive-p (symbol-function subr-name)))
    (comp--install-trampoline
     subr-name
     (or (comp-trampoline-search subr-name)
         (comp-trampoline-compile subr-name)
         ;; Should never happen.
         (cl-assert nil)))))


(cl-defstruct (comp-vec (:copier nil))
  "A re-sizable vector like object."
  (data (make-hash-table :test #'eql) :type hash-table
        :documentation "Payload data.")
  (beg 0 :type integer)
  (end 0 :type natnum))

(defsubst comp-vec-copy (vec)
  "Return a copy of VEC."
  (make-comp-vec :data (copy-hash-table (comp-vec-data vec))
                 :beg (comp-vec-beg vec)
                 :end (comp-vec-end vec)))

(defsubst comp-vec-length (vec)
  "Return the number of elements of VEC."
  (- (comp-vec-end vec) (comp-vec-beg vec)))

(defsubst comp-vec--verify-idx (vec idx)
  "Check whether IDX is in bounds for VEC."
  (cl-assert (and (< idx (comp-vec-end vec))
                  (>= idx (comp-vec-beg vec)))))

(defsubst comp-vec-aref (vec idx)
  "Return the element of VEC whose index is IDX."
  (declare (gv-setter (lambda (val)
                        `(comp-vec--verify-idx ,vec ,idx)
                        `(puthash ,idx ,val (comp-vec-data ,vec)))))
  (comp-vec--verify-idx vec idx)
  (gethash idx (comp-vec-data vec)))

(defsubst comp-vec-append (vec elt)
  "Append ELT into VEC.
Returns ELT."
  (puthash (comp-vec-end vec) elt (comp-vec-data vec))
  (cl-incf (comp-vec-end vec))
  elt)

(defsubst comp-vec-prepend (vec elt)
  "Prepend ELT into VEC.
Returns ELT."
  (puthash (1- (comp-vec-beg vec)) elt (comp-vec-data vec))
  (cl-decf (comp-vec-beg vec))
  elt)



(eval-when-compile
  (defconst comp-op-stack-info
    (cl-loop with h = (make-hash-table)
	     for k across byte-code-vector
	     for v across byte-stack+-info
	     when k
	     do (puthash k v h)
	     finally return h)
    "Hash table lap-op -> stack adjustment."))

(define-hash-table-test 'comp-imm-equal-test #'equal-including-properties
  #'sxhash-equal-including-properties)

(cl-defstruct comp-data-container
  "Data relocation container structure."
  (l () :type list
     :documentation "Constant objects used by functions.")
  (idx (make-hash-table :test 'comp-imm-equal-test) :type hash-table
       :documentation "Obj -> position into the previous field."))

(cl-defstruct (comp-ctxt (:include comp-cstr-ctxt))
  "Lisp side of the compiler context."
  (output nil :type string
          :documentation "Target output file-name for the compilation.")
  (speed native-comp-speed :type number
         :documentation "Default speed for this compilation unit.")
  (debug native-comp-debug :type number
         :documentation "Default debug level for this compilation unit.")
  (compiler-options native-comp-compiler-options :type list
                    :documentation "Options for the GCC compiler.")
  (driver-options native-comp-driver-options :type list
         :documentation "Options for the GCC driver.")
  (top-level-forms () :type list
                   :documentation "List of spilled top level forms.")
  (funcs-h (make-hash-table :test #'equal) :type hash-table
           :documentation "c-name -> comp-func.")
  (sym-to-c-name-h (make-hash-table :test #'eq) :type hash-table
                   :documentation "symbol-function -> c-name.
This is only for optimizing intra CU calls at speed 3.")
  (byte-func-to-func-h (make-hash-table :test #'equal) :type hash-table
                     :documentation "byte-function -> comp-func.
Needed to replace immediate byte-compiled lambdas with the compiled reference.")
  (lambda-fixups-h (make-hash-table :test #'equal) :type hash-table
                   :documentation  "Hash table byte-func -> mvar to fixup.")
  (function-docs (make-hash-table :test #'eql) :type (or hash-table vector)
               :documentation "Documentation index -> documentation")
  (d-default (make-comp-data-container) :type comp-data-container
             :documentation "Standard data relocated in use by functions.")
  (d-impure (make-comp-data-container) :type comp-data-container
            :documentation "Relocated data that cannot be moved into pure space.
This is typically for top-level forms other than defun.")
  (d-ephemeral (make-comp-data-container) :type comp-data-container
               :documentation "Relocated data not necessary after load.")
  (with-late-load nil :type boolean
                  :documentation "When non-nil support late load."))

(cl-defstruct comp-args-base
  (min nil :type integer
       :documentation "Minimum number of arguments allowed."))

(cl-defstruct (comp-args (:include comp-args-base))
  (max nil :type integer
       :documentation "Maximum number of arguments allowed."))

(cl-defstruct (comp-nargs (:include comp-args-base))
  "Describe args when the function signature is of kind:
(ptrdiff_t nargs, Lisp_Object *args)."
  (nonrest nil :type integer
           :documentation "Number of non rest arguments.")
  (rest nil :type boolean
        :documentation "t if rest argument is present."))

(cl-defstruct (comp-block (:copier nil)
                          (:constructor nil))
  "A base class for basic blocks."
  (name nil :type symbol)
  (insns () :type list
         :documentation "List of instructions.")
  (closed nil :type boolean
          :documentation "t if closed.")
  ;; All the following are for SSA and CGF analysis.
  ;; Keep in sync with `comp-clean-ssa'!!
  (in-edges () :type list
            :documentation "List of incoming edges.")
  (out-edges () :type list
             :documentation "List of out-coming edges.")
  (idom nil :type (or null comp-block)
        :documentation "Immediate dominator.")
  (df (make-hash-table) :type (or null hash-table)
      :documentation "Dominance frontier set. Block-name -> block")
  (post-num nil :type (or null number)
            :documentation "Post order number.")
  (final-frame nil :type (or null comp-vec)
             :documentation "This is a copy of the frame when leaving the block.
Is in use to help the SSA rename pass."))

(cl-defstruct (comp-block-lap (:copier nil)
                              (:include comp-block)
                              (:constructor make--comp-block-lap
                                            (addr sp name))) ; Positional
  "A basic block created from lap (real code)."
  ;; These two slots are used during limplification.
  (sp nil :type number
      :documentation "When non-nil indicates the sp value while entering
into it.")
  (addr nil :type number
        :documentation "Start block LAP address.")
  (non-ret-insn nil :type list
                :documentation "Insn known to perform a non local exit.
`comp-fwprop' may identify and store here basic blocks performing
non local exits and mark it rewrite it later.")
  (no-ret nil :type boolean
         :documentation "t when the block is known to perform a
non local exit (ends with an `unreachable' insn)."))

(cl-defstruct (comp-latch (:copier nil)
                          (:include comp-block))
  "A basic block for a latch loop.")

(cl-defstruct (comp-block-cstr (:copier nil)
                               (:include comp-block))
  "A basic block holding only constraints.")

(cl-defstruct (comp-edge (:copier nil) (:constructor make--comp-edge))
  "An edge connecting two basic blocks."
  (src nil :type (or null comp-block))
  (dst nil :type (or null comp-block))
  (number nil :type number
          :documentation "The index number corresponding to this edge in the
 edge hash."))

(defun make-comp-edge (&rest args)
  "Create a `comp-edge' with basic blocks SRC and DST."
  (let ((n (funcall (comp-func-edge-cnt-gen comp-func))))
    (puthash
     n
     (apply #'make--comp-edge :number n args)
     (comp-func-edges-h comp-func))))

(defun comp-block-preds (basic-block)
  "Return the list of predecessors of BASIC-BLOCK."
  (mapcar #'comp-edge-src (comp-block-in-edges basic-block)))

(defun comp-gen-counter ()
  "Return a sequential number generator."
  (let ((n -1))
    (lambda ()
      (cl-incf n))))

(cl-defstruct (comp-func (:copier nil))
  "LIMPLE representation of a function."
  (name nil :type symbol
        :documentation "Function symbol name. Nil indicates anonymous.")
  (c-name nil :type string
          :documentation "The function name in the native world.")
  (byte-func nil
             :documentation "Byte-compiled version.")
  (doc nil :type string
       :documentation "Doc string.")
  (int-spec nil :type list
            :documentation "Interactive form.")
  (lap () :type list
       :documentation "LAP assembly representation.")
  (ssa-status nil :type symbol
       :documentation "SSA status either: nil, `dirty' or t.
Once in SSA form this *must* be set to `dirty' every time the topology of the
CFG is mutated by a pass.")
  (frame-size nil :type integer)
  (vframe-size 0 :type integer)
  (blocks (make-hash-table :test #'eq) :type hash-table
          :documentation "Basic block symbol -> basic block.")
  (lap-block (make-hash-table :test #'equal) :type hash-table
             :documentation "LAP label -> LIMPLE basic block name.")
  (edges-h (make-hash-table) :type hash-table
         :documentation "Hash edge-num -> edge connecting basic two blocks.")
  (block-cnt-gen (funcall #'comp-gen-counter) :type function
                 :documentation "Generates block numbers.")
  (edge-cnt-gen (funcall #'comp-gen-counter) :type function
                :documentation "Generates edges numbers.")
  (has-non-local nil :type boolean
                 :documentation "t if non local jumps are present.")
  (speed nil :type number
         :documentation "Optimization level (see `native-comp-speed').")
  (pure nil :type boolean
        :documentation "t if pure nil otherwise.")
  (type nil :type (or null comp-mvar)
        :documentation "Mvar holding the derived return type."))

(cl-defstruct (comp-func-l (:include comp-func))
  "Lexically-scoped function."
  (args nil :type comp-args-base
        :documentation "Argument specification of the function"))

(cl-defstruct (comp-func-d (:include comp-func))
  "Dynamically-scoped function."
  (lambda-list nil :type list
        :documentation "Original lambda-list."))

(cl-defstruct (comp-mvar (:constructor make--comp-mvar)
                         (:include comp-cstr))
  "A meta-variable being a slot in the meta-stack."
  (id nil :type (or null number)
      :documentation "Unique id when in SSA form.")
  (slot nil :type (or fixnum symbol)
        :documentation "Slot number in the array if a number or
        'scratch' for scratch slot."))

(defun comp-mvar-type-hint-match-p (mvar type-hint)
  "Match MVAR against TYPE-HINT.
In use by the back-end."
  (cl-ecase type-hint
    (cons (comp-cstr-cons-p mvar))
    (fixnum (comp-cstr-fixnum-p mvar))))



(defun comp-ensure-native-compiler ()
  "Make sure Emacs has native compiler support and libgccjit can be loaded.
Signal an error otherwise.
To be used by all entry points."
  (cond
   ((null (featurep 'native-compile))
    (error "Emacs was not compiled with native compiler support (--with-native-compilation)"))
   ((null (native-comp-available-p))
    (error "Cannot find libgccjit library"))))

(defun comp-equality-fun-p (function)
  "Equality functions predicate for FUNCTION."
  (when (memq function '(eq eql equal)) t))

(defun comp-arithm-cmp-fun-p (function)
  "Predicate for arithmetic comparison functions."
  (when (memq function '(= > < >= <=)) t))

(defun comp-set-op-p (op)
  "Assignment predicate for OP."
  (when (memq op comp-limple-sets) t))

(defun comp-assign-op-p (op)
  "Assignment predicate for OP."
  (when (memq op comp-limple-assignments) t))

(defun comp-call-op-p (op)
  "Call predicate for OP."
  (when (memq op comp-limple-calls) t))

(defun comp-branch-op-p (op)
  "Branch predicate for OP."
  (when (memq op comp-limple-branches) t))

(defsubst comp-limple-insn-call-p (insn)
  "Limple INSN call predicate."
  (comp-call-op-p (car-safe insn)))

(defun comp-type-hint-p (func)
  "Type-hint predicate for function name FUNC."
  (when (memq func comp-type-hints) t))

(defun comp-func-unique-in-cu-p (func)
  "Return t if FUNC is known to be unique in the current compilation unit."
  (if (symbolp func)
      (cl-loop with h = (make-hash-table :test #'eq)
               for f being the hash-value in (comp-ctxt-funcs-h comp-ctxt)
               for name = (comp-func-name f)
               when (gethash name h)
                 return nil
               do (puthash name t h)
               finally return t)
    t))

(defsubst comp-symbol-func-to-fun (symbol-funcion)
  "Given a function called SYMBOL-FUNCION return its `comp-func'."
  (gethash (gethash symbol-funcion (comp-ctxt-sym-to-c-name-h
                                    comp-ctxt))
           (comp-ctxt-funcs-h comp-ctxt)))

(defun comp-function-pure-p (f)
  "Return t if F is pure."
  (or (get f 'pure)
      (when-let ((func (comp-symbol-func-to-fun f)))
        (comp-func-pure func))))

(defun comp-alloc-class-to-container (alloc-class)
  "Given ALLOC-CLASS, return the data container for the current context.
Assume allocation class 'd-default as default."
  (cl-struct-slot-value 'comp-ctxt (or alloc-class 'd-default) comp-ctxt))

(defsubst comp-add-const-to-relocs (obj)
  "Keep track of OBJ into the ctxt relocations."
  (puthash obj t (comp-data-container-idx (comp-alloc-class-to-container
                                           comp-curr-allocation-class))))


;;; Log routines.

(defconst comp-limple-lock-keywords
  `((,(rx bol "(comment" (1+ not-newline)) . font-lock-comment-face)
    (,(rx "#(" (group-n 1 "mvar"))
     (1 font-lock-function-name-face))
    (,(rx bol "(" (group-n 1 "phi"))
     (1 font-lock-variable-name-face))
    (,(rx bol "(" (group-n 1 (or "return" "unreachable")))
     (1 font-lock-warning-face))
    (,(rx (group-n 1 (or "entry"
                         (seq (or "entry_" "entry_fallback_" "bb_")
                              (1+ num) (? (or "_latch"
                                              (seq "_cstrs_" (1+ num))))))))
     (1 font-lock-constant-face))
    (,(rx-to-string
       `(seq "(" (group-n 1 (or ,@(mapcar #'symbol-name comp-limple-ops)))))
     (1 font-lock-keyword-face)))
  "Highlights used by `native-comp-limple-mode'.")

(define-derived-mode native-comp-limple-mode fundamental-mode "LIMPLE"
  "Syntax-highlight LIMPLE IR."
  (setf font-lock-defaults '(comp-limple-lock-keywords)))

(cl-defun comp-log (data &optional (level 1) quoted)
  "Log DATA at LEVEL.
LEVEL is a number from 1-3, and defaults to 1; if it is less
than `native-comp-verbose', do nothing.  If `noninteractive', log
with `message'.  Otherwise, log with `comp-log-to-buffer'."
  (when (>= native-comp-verbose level)
    (if noninteractive
        (cl-typecase data
          (atom (message "%s" data))
          (t (dolist (elem data)
               (message "%s" elem))))
      (comp-log-to-buffer data quoted))))

(cl-defun comp-log-to-buffer (data &optional quoted)
  "Log DATA to `comp-log-buffer-name'."
  (let* ((print-f (if quoted #'prin1 #'princ))
         (log-buffer
             (or (get-buffer comp-log-buffer-name)
                 (with-current-buffer (get-buffer-create comp-log-buffer-name)
                   (setf buffer-read-only t)
                   (current-buffer))))
         (log-window (get-buffer-window log-buffer))
         (inhibit-read-only t)
         at-end-p)
    (with-current-buffer log-buffer
      (unless (eq major-mode 'native-comp-limple-mode)
        (native-comp-limple-mode))
      (when (= (point) (point-max))
        (setf at-end-p t))
      (save-excursion
        (goto-char (point-max))
        (cl-typecase data
          (atom (funcall print-f data log-buffer))
          (t (dolist (elem data)
               (funcall print-f elem log-buffer)
               (insert "\n"))))
        (insert "\n"))
      (when (and at-end-p log-window)
        ;; When log window's point is at the end, follow the tail.
        (with-selected-window log-window
          (goto-char (point-max)))))))

(defun comp-prettyformat-mvar (mvar)
  (format "#(mvar %s %s %S)"
          (comp-mvar-id mvar)
          (comp-mvar-slot mvar)
          (comp-cstr-to-type-spec mvar)))

(defun comp-prettyformat-insn (insn)
  (cl-typecase insn
    (comp-mvar (comp-prettyformat-mvar insn))
    (atom (prin1-to-string insn))
    (cons (concat "(" (mapconcat #'comp-prettyformat-insn insn " ") ")"))))

(defun comp-log-func (func verbosity)
  "Log function FUNC at VERBOSITY.
VERBOSITY is a number between 0 and 3."
  (when (>= native-comp-verbose verbosity)
    (comp-log (format "\nFunction: %s\n" (comp-func-name func)) verbosity)
    (cl-loop
     for block-name being each hash-keys of (comp-func-blocks func)
     using (hash-value bb)
     do (comp-log (concat "<" (symbol-name block-name) ">") verbosity)
        (cl-loop
         for insn in (comp-block-insns bb)
         do (comp-log (comp-prettyformat-insn insn) verbosity)))))

(defun comp-log-edges (func)
  "Log edges in FUNC."
  (let ((edges (comp-func-edges-h func)))
    (comp-log (format "\nEdges in function: %s\n"
                      (comp-func-name func))
              2)
    (maphash (lambda (_ e)
               (comp-log (format "n: %d src: %s dst: %s\n"
                                 (comp-edge-number e)
                                 (comp-block-name (comp-edge-src e))
                                 (comp-block-name (comp-edge-dst e)))
                         2))
          edges)))



(defmacro comp-loop-insn-in-block (basic-block &rest body)
  "Loop over all insns in BASIC-BLOCK executing BODY.
Inside BODY, `insn' and `insn-cell'can be used to read or set the
current instruction or its cell."
  (declare (debug (form body))
           (indent defun))
  `(cl-symbol-macrolet ((insn (car insn-cell)))
     (let ((insn-cell (comp-block-insns ,basic-block)))
       (while insn-cell
         ,@body
         (setf insn-cell (cdr insn-cell))))))

;;; spill-lap pass specific code.

(defun comp-lex-byte-func-p (f)
  "Return t if F is a lexically-scoped byte compiled function."
  (and (byte-code-function-p f)
       (fixnump (aref f 0))))

(defun comp-spill-decl-spec (function-name spec)
  "Return the declared specifier SPEC for FUNCTION-NAME."
  (plist-get (cdr (assq function-name byte-to-native-plist-environment))
             spec))

(defun comp-spill-speed (function-name)
  "Return the speed for FUNCTION-NAME."
  (or (comp-spill-decl-spec function-name 'speed)
      (comp-ctxt-speed comp-ctxt)))

;; Autoloaded as might be used by `disassemble-internal'.
;;;###autoload
(defun comp-c-func-name (name prefix &optional first)
  "Given NAME, return a name suitable for the native code.
Add PREFIX in front of it.  If FIRST is not nil, pick the first
available name ignoring compilation context and potential name
clashes."
  ;; Unfortunately not all symbol names are valid as C function names...
  ;; Nassi's algorithm here:
  (let* ((orig-name (if (symbolp name) (symbol-name name) name))
         (crypted (cl-loop with str = (make-string (* 2 (length orig-name)) 0)
	                   for j from 0 by 2
	                   for i across orig-name
	                   for byte = (format "%x" i)
	                   do (aset str j (aref byte 0))
			      (aset str (1+ j) (if (length> byte 1)
						   (aref byte 1)
						 ?\_))
	                   finally return str))
         (human-readable (string-replace
                          "-" "_" orig-name))
         (human-readable (replace-regexp-in-string
                          (rx (not (any "0-9a-z_"))) "" human-readable)))
    (if (null first)
        ;; Prevent C namespace conflicts.
        (cl-loop
         with h = (comp-ctxt-funcs-h comp-ctxt)
         for i from 0
         for c-sym = (concat prefix crypted "_" human-readable "_"
                             (number-to-string i))
         unless (gethash c-sym h)
         return c-sym)
      ;; When called out of a compilation context (ex disassembling)
      ;; pick the first one.
      (concat prefix crypted "_" human-readable "_0"))))

(defun comp-decrypt-arg-list (x function-name)
  "Decrypt argument list X for FUNCTION-NAME."
  (unless (fixnump x)
    (signal 'native-compiler-error-dyn-func function-name))
  (let ((rest (not (= (logand x 128) 0)))
        (mandatory (logand x 127))
        (nonrest (ash x -8)))
    (if (and (null rest)
             (< nonrest 9)) ;; SUBR_MAX_ARGS
        (make-comp-args :min mandatory
                        :max nonrest)
      (make-comp-nargs :min mandatory
                       :nonrest nonrest
                       :rest rest))))

(defsubst comp-byte-frame-size (byte-compiled-func)
  "Return the frame size to be allocated for BYTE-COMPILED-FUNC."
  (aref byte-compiled-func 3))

(defun comp-add-func-to-ctxt (func)
  "Add FUNC to the current compiler context."
  (let ((name (comp-func-name func))
        (c-name (comp-func-c-name func)))
    (puthash name c-name (comp-ctxt-sym-to-c-name-h comp-ctxt))
    (puthash c-name func (comp-ctxt-funcs-h comp-ctxt))))

(cl-defgeneric comp-spill-lap-function (input)
  "Byte-compile INPUT and spill lap for further stages.")

(cl-defmethod comp-spill-lap-function ((function-name symbol))
  "Byte-compile FUNCTION-NAME, spilling data from the byte compiler."
  (unless (comp-ctxt-output comp-ctxt)
    (setf (comp-ctxt-output comp-ctxt)
          (make-temp-file (comp-c-func-name function-name "freefn-")
                          nil ".eln")))
  (let* ((f (symbol-function function-name))
         (c-name (comp-c-func-name function-name "F"))
         (func (make-comp-func-l :name function-name
                                 :c-name c-name
                                 :doc (documentation f t)
                                 :int-spec (interactive-form f)
                                 :speed (comp-spill-speed function-name)
                                 :pure (comp-spill-decl-spec function-name
                                                             'pure))))
      (when (byte-code-function-p f)
        (signal 'native-compiler-error
                "can't native compile an already byte-compiled function"))
      (setf (comp-func-byte-func func)
            (byte-compile (comp-func-name func)))
      (let ((lap (byte-to-native-lambda-lap
                  (gethash (aref (comp-func-byte-func func) 1)
                           byte-to-native-lambdas-h))))
        (cl-assert lap)
        (comp-log lap 2 t)
        (let ((arg-list (aref (comp-func-byte-func func) 0)))
          (setf (comp-func-l-args func)
                (comp-decrypt-arg-list arg-list function-name)
                (comp-func-lap func)
                lap
                (comp-func-frame-size func)
                (comp-byte-frame-size (comp-func-byte-func func))))
        (setf (comp-ctxt-top-level-forms comp-ctxt)
              (list (make-byte-to-native-func-def :name function-name
                                                  :c-name c-name)))
        (comp-add-func-to-ctxt func))))

(cl-defmethod comp-spill-lap-function ((form list))
  "Byte-compile FORM, spilling data from the byte compiler."
  (unless (eq (car-safe form) 'lambda)
    (signal 'native-compiler-error
            "Cannot native-compile, form is not a lambda"))
  (unless (comp-ctxt-output comp-ctxt)
    (setf (comp-ctxt-output comp-ctxt)
          (make-temp-file "comp-lambda-" nil ".eln")))
  (let* ((byte-code (byte-compile form))
         (c-name (comp-c-func-name "anonymous-lambda" "F"))
         (func (if (comp-lex-byte-func-p byte-code)
                   (make-comp-func-l :c-name c-name
                                     :doc (documentation form t)
                                     :int-spec (interactive-form form)
                                     :speed (comp-ctxt-speed comp-ctxt))
                 (make-comp-func-d :c-name c-name
                                   :doc (documentation form t)
                                   :int-spec (interactive-form form)
                                   :speed (comp-ctxt-speed comp-ctxt)))))
    (let ((lap (byte-to-native-lambda-lap
                (gethash (aref byte-code 1)
                         byte-to-native-lambdas-h))))
      (cl-assert lap)
      (comp-log lap 2 t)
      (if (comp-func-l-p func)
          (setf (comp-func-l-args func)
                (comp-decrypt-arg-list (aref byte-code 0) byte-code))
        (setf (comp-func-d-lambda-list func) (cadr form)))
      (setf (comp-func-lap func) lap
            (comp-func-frame-size func) (comp-byte-frame-size
                                         byte-code))
      (setf (comp-func-byte-func func) byte-code
            (comp-ctxt-top-level-forms comp-ctxt)
            (list (make-byte-to-native-func-def :name '--anonymous-lambda
                                                :c-name c-name)))
      (comp-add-func-to-ctxt func))))

(defun comp-intern-func-in-ctxt (_ obj)
  "Given OBJ of type `byte-to-native-lambda', create a function in `comp-ctxt'."
  (when-let ((byte-func (byte-to-native-lambda-byte-func obj)))
    (let* ((lap (byte-to-native-lambda-lap obj))
           (top-l-form (cl-loop
                        for form in (comp-ctxt-top-level-forms comp-ctxt)
                        when (and (byte-to-native-func-def-p form)
                                  (eq (byte-to-native-func-def-byte-func form)
                                      byte-func))
                        return form))
           (name (when top-l-form
                   (byte-to-native-func-def-name top-l-form)))
           (c-name (comp-c-func-name (or name "anonymous-lambda") "F"))
           (func (if (comp-lex-byte-func-p byte-func)
                     (make-comp-func-l
                      :args (comp-decrypt-arg-list (aref byte-func 0)
                                                   name))
                   (make-comp-func-d :lambda-list (aref byte-func 0)))))
      (setf (comp-func-name func) name
            (comp-func-byte-func func) byte-func
            (comp-func-doc func) (documentation byte-func t)
            (comp-func-int-spec func) (interactive-form byte-func)
            (comp-func-c-name func) c-name
            (comp-func-lap func) lap
            (comp-func-frame-size func) (comp-byte-frame-size byte-func)
            (comp-func-speed func) (comp-spill-speed name)
            (comp-func-pure func) (comp-spill-decl-spec name 'pure))

      ;; Store the c-name to have it retrievable from
      ;; `comp-ctxt-top-level-forms'.
      (when top-l-form
        (setf (byte-to-native-func-def-c-name top-l-form) c-name))
      (unless name
        (puthash byte-func func (comp-ctxt-byte-func-to-func-h comp-ctxt)))
      (comp-add-func-to-ctxt func)
      (comp-log (format "Function %s:\n" name) 1)
      (comp-log lap 1 t))))

(cl-defmethod comp-spill-lap-function ((filename string))
  "Byte-compile FILENAME, spilling data from the byte compiler."
  (byte-compile-file filename)
  (when (or (null byte-native-qualities)
            (alist-get 'no-native-compile byte-native-qualities))
    (throw 'no-native-compile nil))
  (unless byte-to-native-top-level-forms
    (signal 'native-compiler-error-empty-byte filename))
  (unless (comp-ctxt-output comp-ctxt)
    (setf (comp-ctxt-output comp-ctxt) (comp-el-to-eln-filename
                                        filename
                                        (or native-compile-target-directory
                                            (when byte+native-compile
                                              (car (last native-comp-eln-load-path)))))))
  (setf (comp-ctxt-speed comp-ctxt) (alist-get 'native-comp-speed
                                               byte-native-qualities)
        (comp-ctxt-debug comp-ctxt) (alist-get 'native-comp-debug
                                               byte-native-qualities)
        (comp-ctxt-compiler-options comp-ctxt) (alist-get 'native-comp-compiler-options
                                                        byte-native-qualities)
        (comp-ctxt-driver-options comp-ctxt) (alist-get 'native-comp-driver-options
                                                        byte-native-qualities)
        (comp-ctxt-top-level-forms comp-ctxt)
        (cl-loop
         for form in (reverse byte-to-native-top-level-forms)
         collect
         (if (and (byte-to-native-func-def-p form)
                  (eq -1
                      (comp-spill-speed (byte-to-native-func-def-name form))))
             (let ((byte-code (byte-to-native-func-def-byte-func form)))
               (remhash byte-code byte-to-native-lambdas-h)
               (make-byte-to-native-top-level
                :form `(defalias
                         ',(byte-to-native-func-def-name form)
                         ,byte-code
                         nil)
                :lexical (comp-lex-byte-func-p byte-code)))
           form)))
  (maphash #'comp-intern-func-in-ctxt byte-to-native-lambdas-h))

(defun comp-spill-lap (input)
  "Byte-compile and spill the LAP representation for INPUT.
If INPUT is a symbol, it is the function-name to be compiled.
If INPUT is a string, it is the filename to be compiled."
  (let ((byte-native-compiling t)
        (byte-to-native-lambdas-h (make-hash-table :test #'eq))
        (byte-to-native-top-level-forms ())
        (byte-to-native-plist-environment ()))
    (comp-spill-lap-function input)))


;;; Limplification pass specific code.

(cl-defstruct (comp-limplify (:copier nil))
  "Support structure used during function limplification."
  (frame nil :type (or null comp-vec)
         :documentation "Meta-stack used to flat LAP.")
  (curr-block nil :type comp-block
              :documentation "Current block being limplified.")
  (sp -1 :type number
      :documentation "Current stack pointer while walking LAP.
Points to the next slot to be filled.")
  (pc 0 :type number
      :documentation "Current program counter while walking LAP.")
  (label-to-addr nil :type hash-table
                 :documentation "LAP hash table -> address.")
  (pending-blocks () :type list
                  :documentation "List of blocks waiting for limplification."))

(defconst comp-lap-eob-ops
  '(byte-goto byte-goto-if-nil byte-goto-if-not-nil byte-goto-if-nil-else-pop
              byte-goto-if-not-nil-else-pop byte-return byte-pushcatch
              byte-switch byte-pushconditioncase)
  "LAP end of basic blocks op codes.")

(defun comp-lap-eob-p (inst)
  "Return t if INST closes the current basic blocks, nil otherwise."
  (when (memq (car inst) comp-lap-eob-ops)
    t))

(defun comp-lap-fall-through-p (inst)
  "Return t if INST falls through, nil otherwise."
  (when (not (memq (car inst) '(byte-goto byte-return)))
    t))

(defsubst comp-sp ()
  "Current stack pointer."
  (declare (gv-setter (lambda (val)
                        `(setf (comp-limplify-sp comp-pass) ,val))))
  (comp-limplify-sp comp-pass))

(defmacro comp-with-sp (sp &rest body)
  "Execute BODY setting the stack pointer to SP.
Restore the original value afterwards."
  (declare (debug (form body))
           (indent defun))
  (let ((sym (gensym)))
    `(let ((,sym (comp-sp)))
       (setf (comp-sp) ,sp)
       (progn ,@body)
       (setf (comp-sp) ,sym))))

(defsubst comp-slot-n (n)
  "Slot N into the meta-stack."
  (comp-vec-aref (comp-limplify-frame comp-pass) n))

(defsubst comp-slot ()
  "Current slot into the meta-stack pointed by sp."
  (comp-slot-n (comp-sp)))

(defsubst comp-slot+1 ()
  "Slot into the meta-stack pointed by sp + 1."
  (comp-slot-n (1+ (comp-sp))))

(defsubst comp-label-to-addr (label)
  "Find the address of LABEL."
  (or (gethash label (comp-limplify-label-to-addr comp-pass))
      (signal 'native-ice (list "label not found" label))))

(defsubst comp-mark-curr-bb-closed ()
  "Mark the current basic block as closed."
  (setf (comp-block-closed (comp-limplify-curr-block comp-pass)) t))

(defun comp-bb-maybe-add (lap-addr &optional sp)
  "If necessary create a pending basic block for LAP-ADDR with stack depth SP.
The basic block is returned regardless it was already declared or not."
  (let ((bb (or (cl-loop  ; See if the block was already limplified.
                 for bb being the hash-value in (comp-func-blocks comp-func)
                 when (and (comp-block-lap-p bb)
                           (equal (comp-block-lap-addr bb) lap-addr))
                   return bb)
                (cl-find-if (lambda (bb) ; Look within the pendings blocks.
                              (and (comp-block-lap-p bb)
                                   (= (comp-block-lap-addr bb) lap-addr)))
                            (comp-limplify-pending-blocks comp-pass)))))
    (if bb
        (progn
          (unless (or (null sp) (= sp (comp-block-lap-sp bb)))
            (signal 'native-ice (list "incoherent stack pointers"
                                      sp (comp-block-lap-sp bb))))
          bb)
      (car (push (make--comp-block-lap lap-addr sp (comp-new-block-sym))
                 (comp-limplify-pending-blocks comp-pass))))))

(defsubst comp-call (func &rest args)
  "Emit a call for function FUNC with ARGS."
  `(call ,func ,@args))

(defun comp-callref (func nargs stack-off)
  "Emit a call using narg abi for FUNC.
NARGS is the number of arguments.
STACK-OFF is the index of the first slot frame involved."
  `(callref ,func ,@(cl-loop repeat nargs
                             for sp from stack-off
                             collect (comp-slot-n sp))))

(cl-defun make-comp-mvar (&key slot (constant nil const-vld) type)
  "`comp-mvar' initializer."
  (let ((mvar (make--comp-mvar :slot slot)))
    (when const-vld
      (comp-add-const-to-relocs constant)
      (setf (comp-cstr-imm mvar) constant))
    (when type
      (setf (comp-mvar-typeset mvar) (list type)))
    mvar))

(defun comp-new-frame (size vsize &optional ssa)
  "Return a clean frame of meta variables of size SIZE and VSIZE.
If SSA is non-nil, populate it with m-var in ssa form."
  (cl-loop with v = (make-comp-vec :beg (- vsize) :end size)
           for i from (- vsize) below size
           for mvar = (if ssa
                          (make-comp-ssa-mvar :slot i)
                        (make-comp-mvar :slot i))
           do (setf (comp-vec-aref v i) mvar)
           finally return v))

(defun comp-emit (insn)
  "Emit INSN into basic block BB."
  (let ((bb (comp-limplify-curr-block comp-pass)))
    (cl-assert (not (comp-block-closed bb)))
    (push insn (comp-block-insns bb))))

(defun comp-emit-set-call (call)
  "Emit CALL assigning the result to the current slot frame.
If the callee function is known to have a return type, propagate it."
  (cl-assert call)
  (comp-emit (list 'set (comp-slot) call)))

(defun comp-copy-slot (src-n &optional dst-n)
  "Set slot number DST-N to slot number SRC-N as source.
If DST-N is specified, use it; otherwise assume it to be the current slot."
  (comp-with-sp (or dst-n (comp-sp))
    (let ((src-slot (comp-slot-n src-n)))
      (cl-assert src-slot)
      (comp-emit `(set ,(comp-slot) ,src-slot)))))

(defsubst comp-emit-annotation (str)
  "Emit annotation STR."
  (comp-emit `(comment ,str)))

(defsubst comp-emit-setimm (val)
  "Set constant VAL to current slot."
  (comp-add-const-to-relocs val)
  ;; Leave relocation index nil on purpose, will be fixed-up in final
  ;; by `comp-finalize-relocs'.
  (comp-emit `(setimm ,(comp-slot) ,val)))

(defun comp-make-curr-block (block-name entry-sp &optional addr)
  "Create a basic block with BLOCK-NAME and set it as current block.
ENTRY-SP is the sp value when entering.
Add block to the current function and return it."
  (let ((bb (make--comp-block-lap addr entry-sp block-name)))
    (setf (comp-limplify-curr-block comp-pass) bb
          (comp-limplify-pc comp-pass) addr
          (comp-limplify-sp comp-pass) (when (comp-block-lap-p bb)
                                         (comp-block-lap-sp bb)))
    (puthash (comp-block-name bb) bb (comp-func-blocks comp-func))
    bb))

(defun comp-latch-make-fill (target)
  "Create a latch pointing to TARGET and fill it.
Return the created latch."
  (let ((latch (make-comp-latch :name (comp-new-block-sym "latch")))
        (curr-bb (comp-limplify-curr-block comp-pass)))
    ;; See `comp-make-curr-block'.
    (setf (comp-limplify-curr-block comp-pass) latch)
    (when (< (comp-func-speed comp-func) 3)
      ;; At speed 3 the programmer is responsible to manually
      ;; place `comp-maybe-gc-or-quit'.
      (comp-emit '(call comp-maybe-gc-or-quit)))
    ;; See `comp-emit-uncond-jump'.
    (comp-emit `(jump ,(comp-block-name target)))
    (comp-mark-curr-bb-closed)
    (puthash (comp-block-name latch) latch (comp-func-blocks comp-func))
    (setf (comp-limplify-curr-block comp-pass) curr-bb)
    latch))

(defun comp-emit-uncond-jump (lap-label)
  "Emit an unconditional branch to LAP-LABEL."
  (cl-destructuring-bind (label-num . stack-depth) lap-label
    (when stack-depth
      (cl-assert (= (1- stack-depth) (comp-sp))))
    (let* ((target-addr (comp-label-to-addr label-num))
           (target (comp-bb-maybe-add target-addr
                                      (comp-sp)))
           (latch (when (< target-addr (comp-limplify-pc comp-pass))
                    (comp-latch-make-fill target)))
           (eff-target-name (comp-block-name (or latch target))))
      (comp-emit `(jump ,eff-target-name))
      (comp-mark-curr-bb-closed))))

(defun comp-emit-cond-jump (a b target-offset lap-label negated)
  "Emit a conditional jump to LAP-LABEL when A and B satisfy EQ.
TARGET-OFFSET is the positive offset on the SP when branching to the target
block.
If NEGATED is non null, negate the tested condition.
Return value is the fall-through block name."
  (cl-destructuring-bind (label-num . label-sp) lap-label
    (let* ((bb (comp-block-name (comp-bb-maybe-add
                                 (1+ (comp-limplify-pc comp-pass))
                                 (comp-sp)))) ; Fall through block.
           (target-sp (+ target-offset (comp-sp)))
           (target-addr (comp-label-to-addr label-num))
           (target (comp-bb-maybe-add target-addr target-sp))
           (latch (when (< target-addr (comp-limplify-pc comp-pass))
                    (comp-latch-make-fill target)))
           (eff-target-name (comp-block-name (or latch target))))
      (when label-sp
        (cl-assert (= (1- label-sp) (+ target-offset (comp-sp)))))
      (comp-emit (if negated
                     (list 'cond-jump a b bb eff-target-name)
		   (list 'cond-jump a b eff-target-name bb)))
      (comp-mark-curr-bb-closed)
      bb)))

(defun comp-emit-handler (lap-label handler-type)
  "Emit a nonlocal-exit handler to LAP-LABEL of type HANDLER-TYPE."
  (cl-destructuring-bind (label-num . label-sp) lap-label
    (cl-assert (= (- label-sp 2) (comp-sp)))
    (setf (comp-func-has-non-local comp-func) t)
    (let* ((guarded-bb (comp-bb-maybe-add (1+ (comp-limplify-pc comp-pass))
                                          (comp-sp)))
           (handler-bb (comp-bb-maybe-add (comp-label-to-addr label-num)
                                          (1+ (comp-sp))))
           (pop-bb (make--comp-block-lap nil (comp-sp) (comp-new-block-sym))))
      (comp-emit (list 'push-handler
                       handler-type
                       (comp-slot+1)
                       (comp-block-name pop-bb)
                       (comp-block-name guarded-bb)))
      (comp-mark-curr-bb-closed)
      ;; Emit the basic block to pop the handler if we got the non local.
      (puthash (comp-block-name pop-bb) pop-bb (comp-func-blocks comp-func))
      (setf (comp-limplify-curr-block comp-pass) pop-bb)
      (comp-emit `(fetch-handler ,(comp-slot+1)))
      (comp-emit `(jump ,(comp-block-name handler-bb)))
      (comp-mark-curr-bb-closed))))

(defun comp-limplify-listn (n)
  "Limplify list N."
  (comp-with-sp (+ (comp-sp) n -1)
    (comp-emit-set-call (comp-call 'cons
                                   (comp-slot)
                                   (make-comp-mvar :constant nil))))
  (cl-loop for sp from (+ (comp-sp) n -2) downto (comp-sp)
           do (comp-with-sp sp
                (comp-emit-set-call (comp-call 'cons
                                               (comp-slot)
                                               (comp-slot+1))))))

(defun comp-new-block-sym (&optional postfix)
  "Return a unique symbol postfixing POSTFIX naming the next new basic block."
  (intern (format (if postfix "bb_%s_%s" "bb_%s")
                  (funcall (comp-func-block-cnt-gen comp-func))
                  postfix)))

(defun comp-fill-label-h ()
  "Fill label-to-addr hash table for the current function."
  (setf (comp-limplify-label-to-addr comp-pass) (make-hash-table :test 'eql))
  (cl-loop for insn in (comp-func-lap comp-func)
           for addr from 0
           do (pcase insn
                (`(TAG ,label . ,_)
                 (puthash label addr (comp-limplify-label-to-addr comp-pass))))))

(defun comp-jump-table-optimizable (jmp-table)
  "Return t if JMP-TABLE can be optimized out."
  (cl-loop
   with labels = (cl-loop for target-label being each hash-value of jmp-table
                          collect target-label)
   with x = (car labels)
   for l in (cdr-safe labels)
   unless (= l x)
     return nil
   finally return t))

(defun comp-emit-switch (var last-insn)
  "Emit a Limple for a lap jump table given VAR and LAST-INSN."
  ;; FIXME this not efficient for big jump tables. We should have a second
  ;; strategy for this case.
  (pcase last-insn
    (`(setimm ,_ ,jmp-table)
     (unless (comp-jump-table-optimizable jmp-table)
       (cl-loop
        for test being each hash-keys of jmp-table
        using (hash-value target-label)
        with len = (hash-table-count jmp-table)
        with test-func = (hash-table-test jmp-table)
        for n from 1
        for last = (= n len)
        for m-test = (make-comp-mvar :constant test)
        for target-name = (comp-block-name (comp-bb-maybe-add
                                            (comp-label-to-addr target-label)
                                            (comp-sp)))
        for ff-bb = (if last
                        (comp-bb-maybe-add (1+ (comp-limplify-pc comp-pass))
                                           (comp-sp))
                      (make--comp-block-lap nil
                                            (comp-sp)
                                            (comp-new-block-sym)))
        for ff-bb-name = (comp-block-name ff-bb)
        if (eq test-func 'eq)
          do (comp-emit (list 'cond-jump var m-test target-name ff-bb-name))
        else
        ;; Store the result of the comparison into the scratch slot before
        ;; emitting the conditional jump.
          do (comp-emit (list 'set (make-comp-mvar :slot 'scratch)
                              (comp-call test-func var m-test)))
             (comp-emit (list 'cond-jump
                              (make-comp-mvar :slot 'scratch)
                              (make-comp-mvar :constant nil)
                              ff-bb-name target-name))
        unless last
        ;; All fall through are artificially created here except the last one.
          do (puthash ff-bb-name ff-bb (comp-func-blocks comp-func))
             (setf (comp-limplify-curr-block comp-pass) ff-bb))))
    (_ (signal 'native-ice
               "missing previous setimm while creating a switch"))))

(defun comp-emit-set-call-subr (subr-name sp-delta)
    "Emit a call for SUBR-NAME.
SP-DELTA is the stack adjustment."
    (let ((subr (symbol-function subr-name))
          (nargs (1+ (- sp-delta))))
      (let* ((arity (func-arity subr))
             (minarg (car arity))
             (maxarg (cdr arity)))
        (when (eq maxarg 'unevalled)
          (signal 'native-ice (list "subr contains unevalled args" subr-name)))
        (if (eq maxarg 'many)
            ;; callref case.
            (comp-emit-set-call (comp-callref subr-name nargs (comp-sp)))
          ;; Normal call.
          (unless (and (>= maxarg nargs) (<= minarg nargs))
            (signal 'native-ice
                    (list "incoherent stack adjustment" nargs maxarg minarg)))
          (let* ((subr-name subr-name)
                 (slots (cl-loop for i from 0 below maxarg
                                 collect (comp-slot-n (+ i (comp-sp))))))
            (comp-emit-set-call (apply #'comp-call (cons subr-name slots))))))))

(eval-when-compile
  (defun comp-op-to-fun (x)
    "Given the LAP op strip \"byte-\" to have the subr name."
    (intern (replace-regexp-in-string "byte-" "" x)))

  (defun comp-body-eff (body op-name sp-delta)
    "Given the original BODY, compute the effective one.
When BODY is `auto', guess function name from the LAP byte-code
name.  Otherwise expect lname fnname."
    (pcase (car body)
      ('auto
       `((comp-emit-set-call-subr ',(comp-op-to-fun op-name) ,sp-delta)))
      ((pred symbolp)
       `((comp-emit-set-call-subr ',(car body) ,sp-delta)))
      (_ body))))

(defmacro comp-op-case (&rest cases)
  "Expand CASES into the corresponding `pcase' expansion.
This is responsible for generating the proper stack adjustment, when known,
and the annotation emission."
  (declare (debug (body))
           (indent defun))
  `(pcase op
     ,@(cl-loop for (op . body) in cases
		for sp-delta = (gethash op comp-op-stack-info)
                for op-name = (symbol-name op)
		if body
		collect `(',op
                          ;; Log all LAP ops except the TAG one.
                          ;; ,(unless (eq op 'TAG)
                          ;;    `(comp-emit-annotation
                          ;;      ,(concat "LAP op " op-name)))
                          ;; Emit the stack adjustment if present.
                          ,(when (and sp-delta (not (eq 0 sp-delta)))
			     `(cl-incf (comp-sp) ,sp-delta))
                          ,@(comp-body-eff body op-name sp-delta))
                else
		collect `(',op (signal 'native-ice
                                       (list "unsupported LAP op" ',op-name))))
     (_ (signal 'native-ice (list "unexpected LAP op" (symbol-name op))))))

(defun comp-limplify-lap-inst (insn)
  "Limplify LAP instruction INSN pushing it in the proper basic block."
  (let ((op (car insn))
        (arg (if (consp (cdr insn))
                 (cadr insn)
               (cdr insn))))
    (comp-op-case
      (TAG
       (cl-destructuring-bind (_TAG label-num . label-sp) insn
         ;; Paranoid?
         (when label-sp
           (cl-assert (= (1- label-sp) (comp-limplify-sp comp-pass))))
         (comp-emit-annotation (format "LAP TAG %d" label-num))))
      (byte-stack-ref
       (comp-copy-slot (- (comp-sp) arg 1)))
      (byte-varref
       (comp-emit-set-call (comp-call 'symbol-value (make-comp-mvar
                                                     :constant arg))))
      (byte-varset
       (comp-emit (comp-call 'set_internal
                             (make-comp-mvar :constant arg)
                             (comp-slot+1))))
      (byte-varbind ;; Verify
       (comp-emit (comp-call 'specbind
                             (make-comp-mvar :constant arg)
                             (comp-slot+1))))
      (byte-call
       (cl-incf (comp-sp) (- arg))
       (comp-emit-set-call (comp-callref 'funcall (1+ arg) (comp-sp))))
      (byte-unbind
       (comp-emit (comp-call 'helper_unbind_n
                             (make-comp-mvar :constant arg))))
      (byte-pophandler
       (comp-emit '(pop-handler)))
      (byte-pushconditioncase
       (comp-emit-handler (cddr insn) 'condition-case))
      (byte-pushcatch
       (comp-emit-handler (cddr insn) 'catcher))
      (byte-nth auto)
      (byte-symbolp auto)
      (byte-consp auto)
      (byte-stringp auto)
      (byte-listp auto)
      (byte-eq auto)
      (byte-memq auto)
      (byte-not
       (comp-emit-set-call (comp-call 'eq (comp-slot-n (comp-sp))
                                      (make-comp-mvar :constant nil))))
      (byte-car auto)
      (byte-cdr auto)
      (byte-cons auto)
      (byte-list1
       (comp-limplify-listn 1))
      (byte-list2
       (comp-limplify-listn 2))
      (byte-list3
       (comp-limplify-listn 3))
      (byte-list4
       (comp-limplify-listn 4))
      (byte-length auto)
      (byte-aref auto)
      (byte-aset auto)
      (byte-symbol-value auto)
      (byte-symbol-function auto)
      (byte-set auto)
      (byte-fset auto)
      (byte-get auto)
      (byte-substring auto)
      (byte-concat2
       (comp-emit-set-call (comp-callref 'concat 2 (comp-sp))))
      (byte-concat3
       (comp-emit-set-call (comp-callref 'concat 3 (comp-sp))))
      (byte-concat4
       (comp-emit-set-call (comp-callref 'concat 4 (comp-sp))))
      (byte-sub1 1-)
      (byte-add1 1+)
      (byte-eqlsign =)
      (byte-gtr >)
      (byte-lss <)
      (byte-leq <=)
      (byte-geq >=)
      (byte-diff -)
      (byte-negate
       (comp-emit-set-call (comp-call 'negate (comp-slot))))
      (byte-plus +)
      (byte-max auto)
      (byte-min auto)
      (byte-mult *)
      (byte-point auto)
      (byte-goto-char auto)
      (byte-insert auto)
      (byte-point-max auto)
      (byte-point-min auto)
      (byte-char-after auto)
      (byte-following-char auto)
      (byte-preceding-char preceding-char)
      (byte-current-column auto)
      (byte-indent-to
       (comp-emit-set-call (comp-call 'indent-to
                                      (comp-slot)
                                      (make-comp-mvar :constant nil))))
      (byte-scan-buffer-OBSOLETE)
      (byte-eolp auto)
      (byte-eobp auto)
      (byte-bolp auto)
      (byte-bobp auto)
      (byte-current-buffer auto)
      (byte-set-buffer auto)
      (byte-save-current-buffer
       (comp-emit (comp-call 'record_unwind_current_buffer)))
      (byte-set-mark-OBSOLETE)
      (byte-interactive-p-OBSOLETE)
      (byte-forward-char auto)
      (byte-forward-word auto)
      (byte-skip-chars-forward auto)
      (byte-skip-chars-backward auto)
      (byte-forward-line auto)
      (byte-char-syntax auto)
      (byte-buffer-substring auto)
      (byte-delete-region auto)
      (byte-narrow-to-region
       (comp-emit-set-call (comp-call 'narrow-to-region
                                      (comp-slot)
                                      (comp-slot+1))))
      (byte-widen
       (comp-emit-set-call (comp-call 'widen)))
      (byte-end-of-line auto)
      (byte-constant2) ; TODO
      ;; Branches.
      (byte-goto
       (comp-emit-uncond-jump (cddr insn)))
      (byte-goto-if-nil
       (comp-emit-cond-jump (comp-slot+1) (make-comp-mvar :constant nil) 0
                            (cddr insn) nil))
      (byte-goto-if-not-nil
       (comp-emit-cond-jump (comp-slot+1) (make-comp-mvar :constant nil) 0
                            (cddr insn) t))
      (byte-goto-if-nil-else-pop
       (comp-emit-cond-jump (comp-slot+1) (make-comp-mvar :constant nil) 1
                            (cddr insn) nil))
      (byte-goto-if-not-nil-else-pop
       (comp-emit-cond-jump (comp-slot+1) (make-comp-mvar :constant nil) 1
                            (cddr insn) t))
      (byte-return
       (comp-emit `(return ,(comp-slot+1))))
      (byte-discard 'pass)
      (byte-dup
       (comp-copy-slot (1- (comp-sp))))
      (byte-save-excursion
       (comp-emit (comp-call 'record_unwind_protect_excursion)))
      (byte-save-window-excursion-OBSOLETE)
      (byte-save-restriction
       (comp-emit (comp-call 'helper_save_restriction)))
      (byte-catch) ;; Obsolete
      (byte-unwind-protect
       (comp-emit (comp-call 'helper_unwind_protect (comp-slot+1))))
      (byte-condition-case) ;; Obsolete
      (byte-temp-output-buffer-setup-OBSOLETE)
      (byte-temp-output-buffer-show-OBSOLETE)
      (byte-unbind-all) ;; Obsolete
      (byte-set-marker auto)
      (byte-match-beginning auto)
      (byte-match-end auto)
      (byte-upcase auto)
      (byte-downcase auto)
      (byte-string= string-equal)
      (byte-string< string-lessp)
      (byte-equal auto)
      (byte-nthcdr auto)
      (byte-elt auto)
      (byte-member auto)
      (byte-assq auto)
      (byte-nreverse auto)
      (byte-setcar auto)
      (byte-setcdr auto)
      (byte-car-safe auto)
      (byte-cdr-safe auto)
      (byte-nconc auto)
      (byte-quo /)
      (byte-rem %)
      (byte-numberp auto)
      (byte-integerp auto)
      (byte-listN
       (cl-incf (comp-sp) (- 1 arg))
       (comp-emit-set-call (comp-callref 'list arg (comp-sp))))
      (byte-concatN
       (cl-incf (comp-sp) (- 1 arg))
       (comp-emit-set-call (comp-callref 'concat arg (comp-sp))))
      (byte-insertN
       (cl-incf (comp-sp) (- 1 arg))
       (comp-emit-set-call (comp-callref 'insert arg (comp-sp))))
      (byte-stack-set
       (comp-copy-slot (1+ (comp-sp)) (- (comp-sp) arg -1)))
      (byte-stack-set2 (cl-assert nil)) ;; TODO
      (byte-discardN
       (cl-incf (comp-sp) (- arg)))
      (byte-switch
       ;; Assume to follow the emission of a setimm.
       ;; This is checked into comp-emit-switch.
       (comp-emit-switch (comp-slot+1)
                         (cl-first (comp-block-insns
                                    (comp-limplify-curr-block comp-pass)))))
      (byte-constant
       (comp-emit-setimm arg))
      (byte-discardN-preserve-tos
       (cl-incf (comp-sp) (- arg))
       (comp-copy-slot (+ arg (comp-sp)))))))

(defun comp-emit-narg-prologue (minarg nonrest rest)
  "Emit the prologue for a narg function."
  (cl-loop for i below minarg
           do (comp-emit `(set-args-to-local ,(comp-slot-n i)))
              (comp-emit '(inc-args)))
  (cl-loop for i from minarg below nonrest
           for bb = (intern (format "entry_%s" i))
           for fallback = (intern (format "entry_fallback_%s" i))
           do (comp-emit `(cond-jump-narg-leq ,i ,fallback ,bb))
              (comp-make-curr-block bb (comp-sp))
              (comp-emit `(set-args-to-local ,(comp-slot-n i)))
              (comp-emit '(inc-args))
              finally (comp-emit '(jump entry_rest_args)))
  (when (/= minarg nonrest)
    (cl-loop for i from minarg below nonrest
             for bb = (intern (format "entry_fallback_%s" i))
             for next-bb = (if (= (1+ i) nonrest)
                               'entry_rest_args
                             (intern (format "entry_fallback_%s" (1+ i))))
             do (comp-with-sp i
                  (comp-make-curr-block bb (comp-sp))
                  (comp-emit-setimm nil)
                  (comp-emit `(jump ,next-bb)))))
  (comp-make-curr-block 'entry_rest_args (comp-sp))
  (comp-emit `(set-rest-args-to-local ,(comp-slot-n nonrest)))
  (setf (comp-sp) nonrest)
  (when (and (> nonrest 8) (null rest))
    (cl-decf (comp-sp))))

(defun comp-limplify-finalize-function (func)
  "Reverse insns into all basic blocks of FUNC."
  (cl-loop for bb being the hash-value in (comp-func-blocks func)
           do (setf (comp-block-insns bb)
                    (nreverse (comp-block-insns bb))))
  (comp-log-func func 2)
  func)

(cl-defgeneric comp-prepare-args-for-top-level (function)
  "Given FUNCTION, return the two arguments for comp--register-...")

(cl-defmethod comp-prepare-args-for-top-level ((function comp-func-l))
  "Lexically-scoped FUNCTION."
  (let ((args (comp-func-l-args function)))
    (cons (make-comp-mvar :constant (comp-args-base-min args))
          (make-comp-mvar :constant (if (comp-args-p args)
                                        (comp-args-max args)
                                      'many)))))

(cl-defmethod comp-prepare-args-for-top-level ((function comp-func-d))
  "Dynamically scoped FUNCTION."
  (cons (make-comp-mvar :constant (func-arity (comp-func-byte-func function)))
        (let ((comp-curr-allocation-class 'd-default))
          ;; Lambda-lists must stay in the same relocation class of
          ;; the object referenced by code to respect uninterned
          ;; symbols.
          (make-comp-mvar :constant (comp-func-d-lambda-list function)))))

(cl-defgeneric comp-emit-for-top-level (form for-late-load)
  "Emit the Limple code for top level FORM.")

(cl-defmethod comp-emit-for-top-level ((form byte-to-native-func-def)
                                       for-late-load)
  (let* ((name (byte-to-native-func-def-name form))
         (c-name (byte-to-native-func-def-c-name form))
         (f (gethash c-name (comp-ctxt-funcs-h comp-ctxt)))
         (args (comp-prepare-args-for-top-level f)))
    (cl-assert (and name f))
    (comp-emit
     `(set ,(make-comp-mvar :slot 1)
           ,(comp-call (if for-late-load
                           'comp--late-register-subr
                         'comp--register-subr)
                       (make-comp-mvar :constant name)
                       (make-comp-mvar :constant c-name)
                       (car args)
                       (cdr args)
                       (setf (comp-func-type f)
                             (make-comp-mvar :constant nil))
                       (make-comp-mvar
                        :constant
                        (list
                         (let* ((h (comp-ctxt-function-docs comp-ctxt))
                                (i (hash-table-count h)))
                           (puthash i (comp-func-doc f) h)
                           i)
                         (comp-func-int-spec f)))
                       ;; This is the compilation unit it-self passed as
                       ;; parameter.
                       (make-comp-mvar :slot 0))))))

(cl-defmethod comp-emit-for-top-level ((form byte-to-native-top-level)
                                       for-late-load)
  (unless for-late-load
    (comp-emit
     (comp-call 'eval
                (let ((comp-curr-allocation-class 'd-impure))
                  (make-comp-mvar :constant
                                  (byte-to-native-top-level-form form)))
                (make-comp-mvar :constant
                                (byte-to-native-top-level-lexical form))))))

(defun comp-emit-lambda-for-top-level (func)
  "Emit the creation of subrs for lambda FUNC.
These are stored in the reloc data array."
  (let ((args (comp-prepare-args-for-top-level func)))
    (let ((comp-curr-allocation-class 'd-impure))
      (comp-add-const-to-relocs (comp-func-byte-func func)))
    (comp-emit
     (comp-call 'comp--register-lambda
                ;; mvar to be fixed-up when containers are
                ;; finalized.
                (or (gethash (comp-func-byte-func func)
                             (comp-ctxt-lambda-fixups-h comp-ctxt))
                    (puthash (comp-func-byte-func func)
                             (make-comp-mvar :constant nil)
                             (comp-ctxt-lambda-fixups-h comp-ctxt)))
                (make-comp-mvar :constant (comp-func-c-name func))
                (car args)
                (cdr args)
                (setf (comp-func-type func)
                      (make-comp-mvar :constant nil))
                (make-comp-mvar
                 :constant
                 (list
                  (let* ((h (comp-ctxt-function-docs comp-ctxt))
                         (i (hash-table-count h)))
                    (puthash i (comp-func-doc func) h)
                    i)
                  (comp-func-int-spec func)))
                ;; This is the compilation unit it-self passed as
                ;; parameter.
                (make-comp-mvar :slot 0)))))

(defun comp-limplify-top-level (for-late-load)
  "Create a Limple function to modify the global environment at load.
When FOR-LATE-LOAD is non-nil, the emitted function modifies only
function definition.

Synthesize a function called `top_level_run' that gets one single
parameter (the compilation unit itself).  To define native
functions, `top_level_run' will call back `comp--register-subr'
into the C code forwarding the compilation unit."
  ;; Once an .eln is loaded and Emacs is dumped 'top_level_run' has no
  ;; reasons to be executed ever again.  Therefore all objects can be
  ;; just ephemeral.
  (let* ((comp-curr-allocation-class 'd-ephemeral)
         (func (make-comp-func-l :name (if for-late-load
                                           'late-top-level-run
                                         'top-level-run)
                                 :c-name (if for-late-load
                                             "late_top_level_run"
                                           "top_level_run")
                                 :args (make-comp-args :min 1 :max 1)
                                 ;; Frame is 2 wide: Slot 0 is the
                                 ;; compilation unit being loaded
                                 ;; (incoming parameter).  Slot 1 is
                                 ;; the last function being
                                 ;; registered.
                                 :frame-size 2
                                 :speed (comp-ctxt-speed comp-ctxt)))
         (comp-func func)
         (comp-pass (make-comp-limplify
                     :curr-block (make--comp-block-lap -1 0 'top-level)
                     :frame (comp-new-frame 1 0))))
    (comp-make-curr-block 'entry (comp-sp))
    (comp-emit-annotation (if for-late-load
                              "Late top level"
                            "Top level"))
    ;; Assign the compilation unit incoming as parameter to the slot frame 0.
    (comp-emit `(set-par-to-local ,(comp-slot-n 0) 0))
    (maphash (lambda (_ func)
               (comp-emit-lambda-for-top-level func))
             (comp-ctxt-byte-func-to-func-h comp-ctxt))
    (mapc (lambda (x) (comp-emit-for-top-level x for-late-load))
          (comp-ctxt-top-level-forms comp-ctxt))
    (comp-emit `(return ,(make-comp-mvar :slot 1)))
    (comp-limplify-finalize-function func)))

(defun comp-addr-to-bb-name (addr)
  "Search for a block starting at ADDR into pending or limplified blocks."
  ;; FIXME Actually we could have another hash for this.
  (cl-flet ((pred (bb)
              (equal (comp-block-lap-addr bb) addr)))
    (if-let ((pending (cl-find-if #'pred
                                  (comp-limplify-pending-blocks comp-pass))))
        (comp-block-name pending)
      (cl-loop for bb being the hash-value in (comp-func-blocks comp-func)
               when (pred bb)
                 return (comp-block-name bb)))))

(defun comp-limplify-block (bb)
  "Limplify basic-block BB and add it to the current function."
  (setf (comp-limplify-curr-block comp-pass) bb
        (comp-limplify-sp comp-pass) (comp-block-lap-sp bb)
        (comp-limplify-pc comp-pass) (comp-block-lap-addr bb))
  (puthash (comp-block-name bb) bb (comp-func-blocks comp-func))
  (cl-loop
   for inst-cell on (nthcdr (comp-limplify-pc comp-pass)
                            (comp-func-lap comp-func))
   for inst = (car inst-cell)
   for next-inst = (car-safe (cdr inst-cell))
   do (comp-limplify-lap-inst inst)
      (cl-incf (comp-limplify-pc comp-pass))
   when (comp-lap-fall-through-p inst)
   do (pcase next-inst
        (`(TAG ,_label . ,label-sp)
         (when label-sp
           (cl-assert (= (1- label-sp) (comp-sp))))
         (let* ((stack-depth (if label-sp
                                 (1- label-sp)
                               (comp-sp)))
                (next-bb (comp-block-name (comp-bb-maybe-add
                                           (comp-limplify-pc comp-pass)
                                           stack-depth))))
           (unless (comp-block-closed bb)
             (comp-emit `(jump ,next-bb))))
         (cl-return)))
   until (comp-lap-eob-p inst)))

(defun comp-limplify-function (func)
  "Limplify a single function FUNC."
  (let* ((frame-size (comp-func-frame-size func))
         (comp-func func)
         (comp-pass (make-comp-limplify
                     :frame (comp-new-frame frame-size 0))))
    (comp-fill-label-h)
    ;; Prologue
    (comp-make-curr-block 'entry (comp-sp))
    (comp-emit-annotation (concat "Lisp function: "
                                  (symbol-name (comp-func-name func))))
    ;; Dynamic functions have parameters bound by the trampoline.
    (when (comp-func-l-p func)
      (let ((args (comp-func-l-args func)))
        (if (comp-args-p args)
            (cl-loop for i below (comp-args-max args)
                     do (cl-incf (comp-sp))
                        (comp-emit `(set-par-to-local ,(comp-slot) ,i)))
          (comp-emit-narg-prologue (comp-args-base-min args)
                                   (comp-nargs-nonrest args)
                                   (comp-nargs-rest args)))))
    (comp-emit '(jump bb_0))
    ;; Body
    (comp-bb-maybe-add 0 (comp-sp))
    (cl-loop for next-bb = (pop (comp-limplify-pending-blocks comp-pass))
             while next-bb
             do (comp-limplify-block next-bb))
    ;; Sanity check against block duplication.
    (cl-loop with addr-h = (make-hash-table)
             for bb being the hash-value in (comp-func-blocks func)
             for addr = (when (comp-block-lap-p bb)
                          (comp-block-lap-addr bb))
             when addr
               do (cl-assert (null (gethash addr addr-h)))
                  (puthash addr t addr-h))
    (comp-limplify-finalize-function func)))

(defun comp-limplify (_)
  "Compute LIMPLE IR for forms in `comp-ctxt'."
  (maphash (lambda (_ f) (comp-limplify-function f))
           (comp-ctxt-funcs-h comp-ctxt))
  (comp-add-func-to-ctxt (comp-limplify-top-level nil))
  (when (comp-ctxt-with-late-load comp-ctxt)
    (comp-add-func-to-ctxt (comp-limplify-top-level t))))


;;; add-cstrs pass specific code.

;; This pass is responsible for adding constraints, these are
;; generated from:
;;
;;  - Conditional branches: each branch taken or non taken can be used
;;    in the CFG to infer information on the tested variables.
;;
;;  - Range propagation under test and branch (when the test is an
;;    arithmetic comparison).
;;
;;  - Type constraint under test and branch (when the test is a
;;    known predicate).
;;
;;  - Function calls: function calls to function assumed to be not
;;    redefinable can be used to add constrains on the function
;;    arguments.  Ex: if we execute successfully (= x y) we know that
;;    afterwards both x and y must satisfy the (or number marker)
;;    type specifier.


(defsubst comp-mvar-used-p (mvar)
  "Non-nil when MVAR is used as lhs in the current function."
  (declare (gv-setter (lambda (val)
			`(puthash ,mvar ,val comp-pass))))
  (gethash mvar comp-pass))

(defun comp-collect-mvars (form)
  "Add rhs m-var present in FORM into `comp-pass'."
  (cl-loop for x in form
           if (consp x)
             do (comp-collect-mvars x)
           else
             when (comp-mvar-p x)
               do (setf (comp-mvar-used-p x) t)))

(defun comp-collect-rhs ()
  "Collect all lhs mvars into `comp-pass'."
  (cl-loop
   for b being each hash-value of (comp-func-blocks comp-func)
   do (cl-loop
       for insn in (comp-block-insns b)
       for (op . args) = insn
       if (comp-assign-op-p op)
         do (comp-collect-mvars (cdr args))
       else
         do (comp-collect-mvars args))))

(defun comp-negate-arithm-cmp-fun (function)
  "Negate FUNCTION.
Return nil if we don't want to emit constraints for its negation."
  (cl-ecase function
    (= nil)
    (> '<=)
    (< '>=)
    (>= '<)
    (<= '>)))

(defun comp-reverse-arithm-fun (function)
  "Reverse FUNCTION."
  (cl-case function
    (= '=)
    (> '<)
    (< '>)
    (>= '<=)
    (<= '>=)
    (t function)))

(defun comp-emit-assume (kind lhs rhs bb negated)
  "Emit an assume of kind KIND for mvar LHS being RHS.
When NEGATED is non-nil, the assumption is negated.
The assume is emitted at the beginning of the block BB."
  (let ((lhs-slot (comp-mvar-slot lhs)))
    (cl-assert lhs-slot)
    (pcase kind
      ((or 'and 'and-nhc)
       (if (comp-mvar-p rhs)
           (let ((tmp-mvar (if negated
                               (make-comp-mvar :slot (comp-mvar-slot rhs))
                             rhs)))
             (push `(assume ,(make-comp-mvar :slot lhs-slot)
                            (,kind ,lhs ,tmp-mvar))
	           (comp-block-insns bb))
             (if negated
                 (push `(assume ,tmp-mvar (not ,rhs))
	               (comp-block-insns bb))))
         ;; If is only a constraint we can negate it directly.
         (push `(assume ,(make-comp-mvar :slot lhs-slot)
                        (,kind ,lhs ,(if negated
                                       (comp-cstr-negation-make rhs)
                                     rhs)))
	       (comp-block-insns bb))))
      ((pred comp-arithm-cmp-fun-p)
       (when-let ((kind (if negated
                            (comp-negate-arithm-cmp-fun kind)
                          kind)))
         (push `(assume ,(make-comp-mvar :slot lhs-slot)
                        (,kind ,lhs
                               ,(if-let* ((vld (comp-cstr-imm-vld-p rhs))
                                          (val (comp-cstr-imm rhs))
                                          (ok (and (integerp val)
                                                   (not (memq kind '(= !=))))))
                                    val
                                  (make-comp-mvar :slot (comp-mvar-slot rhs)))))
	       (comp-block-insns bb))))
      (_ (cl-assert nil)))
    (setf (comp-func-ssa-status comp-func) 'dirty)))

(defun comp-maybe-add-vmvar (op cmp-res insns-seq)
  "If CMP-RES is clobbering OP emit a new constrained mvar and return it.
Return OP otherwise."
  (if-let ((match (eql (comp-mvar-slot op) (comp-mvar-slot cmp-res)))
           (new-mvar (make-comp-mvar
                      :slot
                      (- (cl-incf (comp-func-vframe-size comp-func))))))
      (progn
        (push `(assume ,new-mvar ,op) (cdr insns-seq))
        new-mvar)
    op))

(defun comp-add-new-block-between (bb-symbol bb-a bb-b)
  "Create a new basic-block named BB-SYMBOL and add it between BB-A and BB-B."
  (cl-loop
   with new-bb = (make-comp-block-cstr :name bb-symbol
                                       :insns `((jump ,(comp-block-name bb-b))))
   with new-edge = (make-comp-edge :src bb-a :dst new-bb)
   for ed in (comp-block-in-edges bb-b)
   when (eq (comp-edge-src ed) bb-a)
   do
   ;; Connect `ed' to `new-bb' and disconnect it from `bb-a'.
   (cl-assert (memq ed (comp-block-out-edges bb-a)))
   (setf (comp-edge-src ed) new-bb
         (comp-block-out-edges bb-a) (delq ed (comp-block-out-edges bb-a)))
   (push ed (comp-block-out-edges new-bb))
   ;; Connect `bb-a' `new-bb' with `new-edge'.
   (push new-edge (comp-block-out-edges bb-a))
   (push new-edge (comp-block-in-edges new-bb))
   (setf (comp-func-ssa-status comp-func) 'dirty)
   ;; Add `new-edge' to the current function and return it.
   (cl-return (puthash bb-symbol new-bb (comp-func-blocks comp-func)))
   finally (cl-assert nil)))

;; Cheap substitute to a copy propagation pass...
(defun comp-cond-cstrs-target-mvar (mvar exit-insn bb)
  "Given MVAR, search in BB the original mvar MVAR got assigned from.
Keep on searching till EXIT-INSN is encountered."
  (cl-flet ((targetp (x)
              ;; Ret t if x is an mvar and target the correct slot number.
              (and (comp-mvar-p x)
                   (eql (comp-mvar-slot mvar) (comp-mvar-slot x)))))
    (cl-loop
     with res = nil
     for insn in (comp-block-insns bb)
     when (eq insn exit-insn)
     do (cl-return (and (comp-mvar-p res) res))
     do (pcase insn
          (`(,(pred comp-assign-op-p) ,(pred targetp) ,rhs)
           (setf res rhs)))
     finally (cl-assert nil))))

(defun comp-add-cond-cstrs-target-block (curr-bb target-bb-sym)
  "Return the appropriate basic block to add constraint assumptions into.
CURR-BB is the current basic block.
TARGET-BB-SYM is the symbol name of the target block."
  (let* ((target-bb (gethash target-bb-sym
                             (comp-func-blocks comp-func)))
         (target-bb-in-edges (comp-block-in-edges target-bb)))
    (cl-assert target-bb-in-edges)
    (if (length= target-bb-in-edges 1)
        ;; If block has only one predecessor is already suitable for
        ;; adding constraint assumptions.
        target-bb
      (cl-loop
       ;; Search for the first suitable basic block name.
       for i from 0
       for new-name = (intern (format "%s_cstrs_%d" (symbol-name target-bb-sym)
                                      i))
       until (null (gethash new-name (comp-func-blocks comp-func)))
       finally
       ;; Add it.
       (cl-return (comp-add-new-block-between new-name curr-bb target-bb))))))

(defun comp-add-cond-cstrs-simple ()
  "`comp-add-cstrs' worker function for each selected function."
  (cl-loop
   for b being each hash-value of (comp-func-blocks comp-func)
   do
   (cl-loop
    named in-the-basic-block
    for insn-seq on (comp-block-insns b)
    do
    (pcase insn-seq
      (`((set ,(and (pred comp-mvar-p) tmp-mvar) ,(pred comp-mvar-p))
         ;; (comment ,_comment-str)
         (cond-jump ,tmp-mvar ,obj2 . ,blocks))
       (cl-loop
        for branch-target-cell on blocks
        for branch-target = (car branch-target-cell)
        for negated in '(nil t)
	when (comp-mvar-used-p tmp-mvar)
        do
	(let ((block-target (comp-add-cond-cstrs-target-block b branch-target)))
          (setf (car branch-target-cell) (comp-block-name block-target))
          (comp-emit-assume 'and tmp-mvar obj2 block-target negated))
        finally (cl-return-from in-the-basic-block)))
      (`((cond-jump ,obj1 ,obj2 . ,blocks))
       (cl-loop
        for branch-target-cell on blocks
        for branch-target = (car branch-target-cell)
        for negated in '(nil t)
	when (comp-mvar-used-p obj1)
        do
	(let ((block-target (comp-add-cond-cstrs-target-block b branch-target)))
          (setf (car branch-target-cell) (comp-block-name block-target))
          (comp-emit-assume 'and obj1 obj2 block-target negated))
        finally (cl-return-from in-the-basic-block)))))))

(defun comp-add-cond-cstrs ()
  "`comp-add-cstrs' worker function for each selected function."
  (cl-loop
   for b being each hash-value of (comp-func-blocks comp-func)
   do
   (cl-loop
    named in-the-basic-block
    with prev-insns-seq
    for insns-seq on (comp-block-insns b)
    do
    (pcase insns-seq
      (`((set ,(and (pred comp-mvar-p) cmp-res)
              (,(pred comp-call-op-p)
               ,(and (or (pred comp-equality-fun-p)
                         (pred comp-arithm-cmp-fun-p))
                     fun)
               ,op1 ,op2))
	 ;; (comment ,_comment-str)
	 (cond-jump ,cmp-res ,(pred comp-mvar-p) . ,blocks))
       (cl-loop
        with target-mvar1 = (comp-cond-cstrs-target-mvar op1 (car insns-seq) b)
        with target-mvar2 = (comp-cond-cstrs-target-mvar op2 (car insns-seq) b)
        for branch-target-cell on blocks
        for branch-target = (car branch-target-cell)
        for negated in '(t nil)
        for kind = (cl-case fun
                     (equal 'and-nhc)
                     (eql 'and-nhc)
                     (eq 'and)
                     (t fun))
        when (or (comp-mvar-used-p target-mvar1)
                 (comp-mvar-used-p target-mvar2))
        do
        (let ((block-target (comp-add-cond-cstrs-target-block b branch-target)))
          (setf (car branch-target-cell) (comp-block-name block-target))
          (when (comp-mvar-used-p target-mvar1)
            (comp-emit-assume kind target-mvar1
                              (comp-maybe-add-vmvar op2 cmp-res prev-insns-seq)
                              block-target negated))
          (when (comp-mvar-used-p target-mvar2)
            (comp-emit-assume (comp-reverse-arithm-fun kind)
                              target-mvar2
                              (comp-maybe-add-vmvar op1 cmp-res prev-insns-seq)
                              block-target negated)))
        finally (cl-return-from in-the-basic-block)))
      (`((set ,(and (pred comp-mvar-p) cmp-res)
              (,(pred comp-call-op-p)
               ,(and (pred comp-known-predicate-p) fun)
               ,op))
	 ;; (comment ,_comment-str)
	 (cond-jump ,cmp-res ,(pred comp-mvar-p) . ,blocks))
       (cl-loop
        with target-mvar = (comp-cond-cstrs-target-mvar op (car insns-seq) b)
        with cstr = (comp-pred-to-cstr fun)
        for branch-target-cell on blocks
        for branch-target = (car branch-target-cell)
        for negated in '(t nil)
        when (comp-mvar-used-p target-mvar)
        do
        (let ((block-target (comp-add-cond-cstrs-target-block b branch-target)))
          (setf (car branch-target-cell) (comp-block-name block-target))
          (comp-emit-assume 'and target-mvar cstr block-target negated))
        finally (cl-return-from in-the-basic-block)))
      ;; Match predicate on the negated branch (unless).
      (`((set ,(and (pred comp-mvar-p) cmp-res)
              (,(pred comp-call-op-p)
               ,(and (pred comp-known-predicate-p) fun)
               ,op))
         (set ,neg-cmp-res (call eq ,cmp-res ,(pred comp-cstr-null-p)))
	 (cond-jump ,neg-cmp-res ,(pred comp-mvar-p) . ,blocks))
       (cl-loop
        with target-mvar = (comp-cond-cstrs-target-mvar op (car insns-seq) b)
        with cstr = (comp-pred-to-cstr fun)
        for branch-target-cell on blocks
        for branch-target = (car branch-target-cell)
        for negated in '(nil t)
        when (comp-mvar-used-p target-mvar)
        do
        (let ((block-target (comp-add-cond-cstrs-target-block b branch-target)))
          (setf (car branch-target-cell) (comp-block-name block-target))
          (comp-emit-assume 'and target-mvar cstr block-target negated))
        finally (cl-return-from in-the-basic-block))))
    (setf prev-insns-seq insns-seq))))

(defsubst comp-insert-insn (insn insn-cell)
  "Insert INSN as second insn of INSN-CELL."
  (let ((next-cell (cdr insn-cell))
        (new-cell `(,insn)))
    (setf (cdr insn-cell) new-cell
          (cdr new-cell) next-cell
          (comp-func-ssa-status comp-func) 'dirty)))

(defun comp-emit-call-cstr (mvar call-cell cstr)
  "Emit a constraint CSTR for MVAR after CALL-CELL."
  (let* ((new-mvar (make-comp-mvar :slot (comp-mvar-slot mvar)))
         ;; Have new-mvar as LHS *and* RHS to ensure monotonicity and
         ;; fwprop convergence!!
         (insn `(assume ,new-mvar (and ,new-mvar ,mvar ,cstr))))
    (comp-insert-insn insn call-cell)))

(defun comp-lambda-list-gen (lambda-list)
  "Return a generator to iterate over LAMBDA-LIST."
  (lambda ()
    (cl-case (car lambda-list)
      (&optional
       (setf lambda-list (cdr lambda-list))
       (prog1
           (car lambda-list)
         (setf lambda-list (cdr lambda-list))))
      (&rest
       (cadr lambda-list))
      (t
       (prog1
           (car lambda-list)
         (setf lambda-list (cdr lambda-list)))))))

(defun comp-add-call-cstr ()
  "Add args assumptions for each function of which the type specifier is known."
  (cl-loop
   for bb being each hash-value of (comp-func-blocks comp-func)
   do
   (comp-loop-insn-in-block bb
     (when-let ((match
                 (pcase insn
                   (`(set ,lhs (,(pred comp-call-op-p) ,f . ,args))
                    (when-let ((cstr-f (gethash f comp-known-func-cstr-h)))
                      (cl-values f cstr-f lhs args)))
                   (`(,(pred comp-call-op-p) ,f . ,args)
                    (when-let ((cstr-f (gethash f comp-known-func-cstr-h)))
                      (cl-values f cstr-f nil args))))))
       (cl-multiple-value-bind (f cstr-f lhs args) match
         (cl-loop
          with gen = (comp-lambda-list-gen (comp-cstr-f-args cstr-f))
          for arg in args
          for cstr = (funcall gen)
          for target = (comp-cond-cstrs-target-mvar arg insn bb)
          unless (comp-cstr-p cstr)
            do (signal 'native-ice
                       (list "Incoherent type specifier for function" f))
          when (and target
                    ;; No need to add call constraints if this is t
                    ;; (bug#45812 bug#45705 bug#45751).
                    (not (equal comp-cstr-t cstr))
                    (or (null lhs)
                        (not (eql (comp-mvar-slot lhs)
                                  (comp-mvar-slot target)))))
            do (comp-emit-call-cstr target insn-cell cstr)))))))

(defun comp-add-cstrs (_)
  "Rewrite conditional branches adding appropriate 'assume' insns.
This is introducing and placing 'assume' insns in use by fwprop
to propagate conditional branch test information on target basic
blocks."
  (maphash (lambda (_ f)
             (when (and (>= (comp-func-speed f) 1)
                        ;; No point to run this on dynamic scope as
                        ;; this pass is effective only on local
                        ;; variables.
			(comp-func-l-p f)
                        (not (comp-func-has-non-local f)))
               (let ((comp-func f)
                     (comp-pass (make-hash-table :test #'eq)))
                 (comp-collect-rhs)
		 (comp-add-cond-cstrs-simple)
                 (comp-add-cond-cstrs)
                 (comp-add-call-cstr)
                 (comp-log-func comp-func 3))))
           (comp-ctxt-funcs-h comp-ctxt)))


;;; pure-func pass specific code.

;; Simple IPA pass to infer function purity of functions not
;; explicitly declared as such.  This is effective only at speed 3 to
;; avoid optimizing-out functions and preventing their redefinition
;; being effective.

(defun comp-collect-calls (f)
  "Return a list with all the functions called by F."
  (cl-loop
   with h = (make-hash-table :test #'eq)
   for b being each hash-value of (comp-func-blocks f)
   do (cl-loop
       for insn in (comp-block-insns b)
       do (pcase insn
            (`(set ,_lval (,(pred comp-call-op-p) ,f . ,_rest))
             (puthash f t h))
            (`(,(pred comp-call-op-p) ,f . ,_rest)
             (puthash f t h))))
   finally return (cl-loop
                   for f being each hash-key of h
                   collect (if (stringp f)
                               (comp-func-name
                                (gethash f
                                         (comp-ctxt-funcs-h comp-ctxt)))
                             f))))

(defun comp-pure-infer-func (f)
  "If all functions called by F are pure then F is pure too."
  (when (and (cl-every (lambda (x)
                         (or (comp-function-pure-p x)
                             (eq x (comp-func-name f))))
                       (comp-collect-calls f))
             (not (eq (comp-func-pure f) t)))
    (comp-log (format "%s inferred to be pure" (comp-func-name f)))
    (setf (comp-func-pure f) t)))

(defun comp-ipa-pure (_)
  "Infer function purity."
  (cl-loop
   with pure-n = 0
   for n from 1
   while
   (/= pure-n
       (setf pure-n
             (cl-loop
              for f being each hash-value of (comp-ctxt-funcs-h comp-ctxt)
              when (and (>= (comp-func-speed f) 3)
                        (comp-func-l-p f)
                        (not (comp-func-pure f)))
              do (comp-pure-infer-func f)
              count (comp-func-pure f))))
   finally (comp-log (format "ipa-pure iterated %d times" n))))


;;; SSA pass specific code.
;; After limplification no edges are present between basic blocks and an
;; implicit phi is present for every slot at the beginning of every basic block.
;; This pass is responsible for building all the edges and replace all m-vars
;; plus placing the needed phis.
;; Because the number of phis placed is (supposed) to be the minimum necessary
;; this form is called 'minimal SSA form'.
;; This pass should be run every time basic blocks or m-var are shuffled.

(cl-defun make-comp-ssa-mvar (&rest rest &key _slot _constant _type)
  "Same as `make-comp-mvar' but set the `id' slot."
  (let ((mvar (apply #'make-comp-mvar rest)))
    (setf (comp-mvar-id mvar) (sxhash-eq mvar))
    mvar))

(defun comp-clean-ssa (f)
  "Clean-up SSA for function F."
  (setf (comp-func-edges-h f) (make-hash-table))
  (cl-loop
   for b being each hash-value of (comp-func-blocks f)
   do (setf (comp-block-in-edges b) ()
            (comp-block-out-edges b) ()
            (comp-block-idom b) nil
            (comp-block-df b) (make-hash-table)
            (comp-block-post-num b) nil
            (comp-block-final-frame b) nil
            ;; Prune all phis.
            (comp-block-insns b) (cl-loop for insn in (comp-block-insns b)
                                          unless (eq 'phi (car insn))
                                            collect insn))))

(defun comp-compute-edges ()
  "Compute the basic block edges for the current function."
  (cl-loop with blocks = (comp-func-blocks comp-func)
           for bb being each hash-value of blocks
           for last-insn = (car (last (comp-block-insns bb)))
           for (op first second third forth) = last-insn
           do (cl-case op
                (jump
                 (make-comp-edge :src bb :dst (gethash first blocks)))
                (cond-jump
                 (make-comp-edge :src bb :dst (gethash third blocks))
                 (make-comp-edge :src bb :dst (gethash forth blocks)))
                (cond-jump-narg-leq
                 (make-comp-edge :src bb :dst (gethash second blocks))
                 (make-comp-edge :src bb :dst (gethash third blocks)))
                (push-handler
                 (make-comp-edge :src bb :dst (gethash third blocks))
                 (make-comp-edge :src bb :dst (gethash forth blocks)))
                (return)
                (unreachable)
                (otherwise
                 (signal 'native-ice
                         (list "block does not end with a branch"
                               bb
                               (comp-func-name comp-func)))))
           ;; Update edge refs into blocks.
           finally
           (cl-loop
            for edge being the hash-value in (comp-func-edges-h comp-func)
            do
            (push edge
                  (comp-block-out-edges (comp-edge-src edge)))
            (push edge
                  (comp-block-in-edges (comp-edge-dst edge))))
           (comp-log-edges comp-func)))

(defun comp-collect-rev-post-order (basic-block)
  "Walk BASIC-BLOCK children and return their name in reversed post-order."
  (let ((visited (make-hash-table))
        (acc ()))
    (cl-labels ((collect-rec (bb)
                  (let ((name (comp-block-name bb)))
                    (unless (gethash name visited)
                      (puthash name t visited)
                      (cl-loop for e in (comp-block-out-edges bb)
                               for dst-block = (comp-edge-dst e)
                               do (collect-rec dst-block))
                      (push name acc)))))
      (collect-rec basic-block)
      acc)))

(defun comp-compute-dominator-tree ()
  "Compute immediate dominators for each basic block in current function."
  ;; Originally based on: "A Simple, Fast Dominance Algorithm"
  ;; Cooper, Keith D.; Harvey, Timothy J.; Kennedy, Ken (2001).
  (cl-flet ((intersect (b1 b2)
              (let ((finger1 (comp-block-post-num b1))
                    (finger2 (comp-block-post-num b2)))
                (while (not (= finger1 finger2))
                  (while (< finger1 finger2)
                    (setf b1 (comp-block-idom b1)
                          finger1 (comp-block-post-num b1)))
                  (while (< finger2 finger1)
                    (setf b2 (comp-block-idom b2)
                          finger2 (comp-block-post-num b2))))
                b1))
            (first-processed (l)
              (if-let ((p (cl-find-if (lambda (p) (comp-block-idom p)) l)))
                  p
                (signal 'native-ice "cant't find first preprocessed"))))

    (when-let ((blocks (comp-func-blocks comp-func))
               (entry (gethash 'entry blocks))
               ;; No point to go on if the only bb is 'entry'.
               (bb0 (gethash 'bb_0 blocks)))
      (cl-loop
       with rev-bb-list = (comp-collect-rev-post-order entry)
       with changed = t
       while changed
       initially (progn
                   (comp-log "Computing dominator tree...\n" 2)
                   (setf (comp-block-idom entry) entry)
                   ;; Set the post order number.
                   (cl-loop for name in (reverse rev-bb-list)
                            for b = (gethash name blocks)
                            for i from 0
                            do (setf (comp-block-post-num b) i)))
       do (cl-loop
           for name in (cdr rev-bb-list)
           for b = (gethash name blocks)
           for preds = (comp-block-preds b)
           for new-idom = (first-processed preds)
           initially (setf changed nil)
           do (cl-loop for p in (delq new-idom preds)
                       when (comp-block-idom p)
                       do (setf new-idom (intersect p new-idom)))
           unless (eq (comp-block-idom b) new-idom)
           do (setf (comp-block-idom b) (unless (and (comp-block-lap-p new-idom)
                                                    (comp-block-lap-no-ret
                                                     new-idom))
                                         new-idom)
                    changed t))))))

(defun comp-compute-dominator-frontiers ()
  "Compute the dominator frontier for each basic block in `comp-func'."
  ;; Originally based on: "A Simple, Fast Dominance Algorithm"
  ;; Cooper, Keith D.; Harvey, Timothy J.; Kennedy, Ken (2001).
  (cl-loop with blocks = (comp-func-blocks comp-func)
           for b-name being each hash-keys of blocks
           using (hash-value b)
           for preds = (comp-block-preds b)
           when (length> preds 1) ; All joins
           do (cl-loop for p in preds
                       for runner = p
                       do (while (not (eq runner (comp-block-idom b)))
                            (puthash b-name b (comp-block-df runner))
                            (setf runner (comp-block-idom runner))))))

(defun comp-log-block-info ()
  "Log basic blocks info for the current function."
  (maphash (lambda (name bb)
             (let ((dom (comp-block-idom bb))
                   (df (comp-block-df bb)))
               (comp-log (format "block: %s idom: %s DF %s\n"
                                 name
                                 (when dom (comp-block-name dom))
                                 (cl-loop for b being each hash-keys of df
                                          collect b))
                         3)))
           (comp-func-blocks comp-func)))

(defun comp-place-phis ()
  "Place phi insns into the current function."
  ;; Originally based on: Static Single Assignment Book
  ;; Algorithm 3.1: Standard algorithm for inserting phi-functions
  (cl-flet ((add-phi (slot-n bb)
             ;; Add a phi func for slot SLOT-N at the top of BB.
             (push `(phi ,slot-n) (comp-block-insns bb)))
            (slot-assigned-p (slot-n bb)
             ;; Return t if a SLOT-N was assigned within BB.
             (cl-loop for insn in (comp-block-insns bb)
                      for op = (car insn)
                      when (or (and (comp-assign-op-p op)
                                    (eql slot-n (comp-mvar-slot (cadr insn))))
                               ;; fetch-handler is after a non local
                               ;; therefore clobbers all frame!!!
                               (eq op 'fetch-handler))
                        return t)))

    (cl-loop for i from (- (comp-func-vframe-size comp-func))
                   below (comp-func-frame-size comp-func)
             ;; List of blocks with a definition of mvar i
             for defs-v = (cl-loop with blocks = (comp-func-blocks comp-func)
                                   for b being each hash-value of blocks
                                   when (slot-assigned-p i b)
                                   collect b)
             ;; Set of basic blocks where phi is added.
             for f = ()
             ;; Worklist, set of basic blocks that contain definitions of v.
             for w = defs-v
             do
             (while w
               (let ((x (pop w)))
                 (cl-loop for y being each hash-value of (comp-block-df x)
                          unless (cl-find y f)
                          do (add-phi i y)
                             (push y f)
                             ;; Adding a phi implies mentioning the
                             ;; corresponding slot so in case adjust w.
                             (unless (cl-find y defs-v)
                               (push y w))))))))

(defun comp-dom-tree-walker (bb pre-lambda post-lambda)
  "Dominator tree walker function starting from basic block BB.
PRE-LAMBDA and POST-LAMBDA are called in pre or post-order if non-nil."
  (when pre-lambda
    (funcall pre-lambda bb))
  (when-let ((out-edges (comp-block-out-edges bb)))
    (cl-loop for ed in out-edges
             for child = (comp-edge-dst ed)
             when (eq bb (comp-block-idom child))
             ;; Current block is the immediate dominator then recur.
             do (comp-dom-tree-walker child pre-lambda post-lambda)))
  (when post-lambda
    (funcall post-lambda bb)))

(cl-defstruct (comp-ssa (:copier nil))
  "Support structure used while SSA renaming."
  (frame (comp-new-frame (comp-func-frame-size comp-func)
                         (comp-func-vframe-size comp-func) t)
         :type comp-vec
         :documentation "`comp-vec' of m-vars."))

(defun comp-ssa-rename-insn (insn frame)
  (cl-loop
   for slot-n from (- (comp-func-vframe-size comp-func))
              below (comp-func-frame-size comp-func)
   do
   (cl-flet ((targetp (x)
               ;; Ret t if x is an mvar and target the correct slot number.
               (and (comp-mvar-p x)
                    (eql slot-n (comp-mvar-slot x))))
             (new-lvalue ()
               ;; If is an assignment make a new mvar and put it as l-value.
               (let ((mvar (make-comp-ssa-mvar :slot slot-n)))
                 (setf (comp-vec-aref frame slot-n) mvar
                       (cadr insn) mvar))))
     (pcase insn
       (`(,(pred comp-assign-op-p) ,(pred targetp) . ,_)
        (let ((mvar (comp-vec-aref frame slot-n)))
          (setf (cddr insn) (cl-nsubst-if mvar #'targetp (cddr insn))))
        (new-lvalue))
       (`(fetch-handler . ,_)
        ;; Clobber all no matter what!
        (setf (comp-vec-aref frame slot-n) (make-comp-ssa-mvar :slot slot-n)))
       (`(phi ,n)
        (when (equal n slot-n)
          (new-lvalue)))
       (_
        (let ((mvar (comp-vec-aref frame slot-n)))
          (setcdr insn (cl-nsubst-if mvar #'targetp (cdr insn)))))))))

(defun comp-ssa-rename ()
  "Entry point to rename into SSA within the current function."
  (comp-log "Renaming\n" 2)
  (let ((visited (make-hash-table)))
    (cl-labels ((ssa-rename-rec (bb in-frame)
                  (unless (gethash bb visited)
                    (puthash bb t visited)
                    (cl-loop for insn in (comp-block-insns bb)
                             do (comp-ssa-rename-insn insn in-frame))
                    (setf (comp-block-final-frame bb)
                          (copy-sequence in-frame))
                    (when-let ((out-edges (comp-block-out-edges bb)))
                      (cl-loop
                       for ed in out-edges
                       for child = (comp-edge-dst ed)
                       ;; Provide a copy of the same frame to all children.
                       do (ssa-rename-rec child (comp-vec-copy in-frame)))))))

      (ssa-rename-rec (gethash 'entry (comp-func-blocks comp-func))
                      (comp-new-frame (comp-func-frame-size comp-func)
                                      (comp-func-vframe-size comp-func)
                                      t)))))

(defun comp-finalize-phis ()
  "Fixup r-values into phis in all basic blocks."
  (cl-flet ((finalize-phi (args b)
              ;; Concatenate into args all incoming m-vars for this phi.
              (setcdr args
                      (cl-loop with slot-n = (comp-mvar-slot (car args))
                               for e in (comp-block-in-edges b)
                               for b = (comp-edge-src e)
                               for in-frame = (comp-block-final-frame b)
                               collect (list (comp-vec-aref in-frame slot-n)
                                             (comp-block-name b))))))

    (cl-loop for b being each hash-value of (comp-func-blocks comp-func)
             do (cl-loop for (op . args) in (comp-block-insns b)
                         when (eq op 'phi)
                           do (finalize-phi args b)))))

(defun comp-remove-unreachable-blocks ()
  "Remove unreachable basic blocks.
Return t when one or more block was removed, nil otherwise."
  (cl-loop
   with ret
   for bb being each hash-value of (comp-func-blocks comp-func)
   for bb-name = (comp-block-name bb)
   when (and (not (eq 'entry bb-name))
             (null (comp-block-idom bb)))
   do
   (comp-log (format "Removing block: %s" bb-name) 1)
   (remhash bb-name (comp-func-blocks comp-func))
   (setf (comp-func-ssa-status comp-func) t
              ret t)
   finally return ret))

(defun comp-ssa ()
  "Port all functions into minimal SSA form."
  (maphash (lambda (_ f)
             (let* ((comp-func f)
                    (ssa-status (comp-func-ssa-status f)))
               (unless (eq ssa-status t)
                 (cl-loop
                  when (eq ssa-status 'dirty)
                    do (comp-clean-ssa f)
                  do (comp-compute-edges)
                     (comp-compute-dominator-tree)
                 until (null (comp-remove-unreachable-blocks)))
                 (comp-compute-dominator-frontiers)
                 (comp-log-block-info)
                 (comp-place-phis)
                 (comp-ssa-rename)
                 (comp-finalize-phis)
                 (comp-log-func comp-func 3)
                 (setf (comp-func-ssa-status f) t))))
           (comp-ctxt-funcs-h comp-ctxt)))


;;; propagate pass specific code.
;; A very basic propagation pass follows.
;; This propagates values and types plus ref property in the control flow graph.
;; This is also responsible for removing function calls to pure functions if
;; possible.

(defconst comp-fwprop-max-insns-scan 4500
  ;; Chosen as ~ the greatest required value for full convergence
  ;; native compiling all Emacs code-base.
  "Max number of scanned insn before giving-up.")

(defun comp-copy-insn (insn)
  "Deep copy INSN."
  ;; Adapted from `copy-tree'.
  (if (consp insn)
      (let (result)
	(while (consp insn)
	  (let ((newcar (car insn)))
	    (if (or (consp (car insn)) (comp-mvar-p (car insn)))
		(setf newcar (comp-copy-insn (car insn))))
	    (push newcar result))
	  (setf insn (cdr insn)))
	(nconc (nreverse result)
               (if (comp-mvar-p insn) (comp-copy-insn insn) insn)))
    (if (comp-mvar-p insn)
        (copy-comp-mvar insn)
      insn)))

(defmacro comp-apply-in-env (func &rest args)
  "Apply FUNC to ARGS in the current compilation environment."
  `(let ((env (cl-loop
               for f being the hash-value in (comp-ctxt-funcs-h comp-ctxt)
               for func-name = (comp-func-name f)
               for byte-code = (comp-func-byte-func f)
               when func-name
               collect `(,func-name . ,(symbol-function func-name))
               and do
               (setf (symbol-function func-name) byte-code))))
     (unwind-protect
         (apply ,func ,@args)
       (cl-loop
        for (func-name . def) in env
        do (setf (symbol-function func-name) def)))))

(defun comp-fwprop-prologue ()
  "Prologue for the propagate pass.
Here goes everything that can be done not iteratively (read once).
Forward propagate immediate involed in assignments." ; FIXME: Typo.  Involved or invoked?
  (cl-loop
   for b being each hash-value of (comp-func-blocks comp-func)
   do (cl-loop
       for insn in (comp-block-insns b)
       do (pcase insn
            (`(setimm ,lval ,v)
             (setf (comp-cstr-imm lval) v))))))

(defun comp-mvar-propagate (lval rval)
  "Propagate into LVAL properties of RVAL."
  (setf (comp-mvar-typeset lval) (comp-mvar-typeset rval)
        (comp-mvar-valset lval) (comp-mvar-valset rval)
        (comp-mvar-range lval) (comp-mvar-range rval)
        (comp-mvar-neg lval) (comp-mvar-neg rval)))

(defun comp-function-foldable-p (f args)
  "Given function F called with ARGS, return non-nil when optimizable."
  (and (comp-function-pure-p f)
       (cl-every #'comp-cstr-imm-vld-p args)))

(defun comp-function-call-maybe-fold (insn f args)
  "Given INSN, when F is pure if all ARGS are known, remove the function call.
Return non-nil if the function is folded successfully."
  (cl-flet ((rewrite-insn-as-setimm (insn value)
               ;; See `comp-emit-setimm'.
               (comp-add-const-to-relocs value)
               (setf (car insn) 'setimm
                     (cddr insn) `(,value))))
    (cond
     ((eq f 'symbol-value)
      (when-let* ((arg0 (car args))
                  (const (comp-cstr-imm-vld-p arg0))
                  (ok-to-optim (member (comp-cstr-imm arg0)
                                       comp-symbol-values-optimizable)))
        (rewrite-insn-as-setimm insn (symbol-value (comp-cstr-imm
                                                    (car args))))))
     ((comp-function-foldable-p f args)
      (ignore-errors
        ;; No point to complain here in case of error because we
        ;; should do basic block pruning in order to be sure that this
        ;; is not dead-code.  This is now left to gcc, to be
        ;; implemented only if we want a reliable diagnostic here.
        (let* ((f (if-let (f-in-ctxt (comp-symbol-func-to-fun f))
                      ;; If the function is IN the compilation ctxt
                      ;; and know to be pure.
                      (comp-func-byte-func f-in-ctxt)
                    f))
               (value (comp-apply-in-env f (mapcar #'comp-cstr-imm args))))
          (rewrite-insn-as-setimm insn value)))))))

(defun comp-fwprop-call (insn lval f args)
  "Propagate on a call INSN into LVAL.
F is the function being called with arguments ARGS.
Fold the call in case."
  (unless (comp-function-call-maybe-fold insn f args)
    (when (and (eq 'funcall f)
               (comp-cstr-imm-vld-p (car args)))
      (setf f (comp-cstr-imm (car args))
            args (cdr args)))
    (when-let ((cstr-f (gethash f comp-known-func-cstr-h)))
      (let ((cstr (comp-cstr-f-ret cstr-f)))
        (when (comp-cstr-empty-p cstr)
          ;; Store it to be rewritten as non local exit.
          (setf (comp-block-lap-non-ret-insn comp-block) insn))
        (setf (comp-mvar-range lval) (comp-cstr-range cstr)
              (comp-mvar-valset lval) (comp-cstr-valset cstr)
              (comp-mvar-typeset lval) (comp-cstr-typeset cstr)
              (comp-mvar-neg lval) (comp-cstr-neg cstr))))
    (cl-case f
      (+ (comp-cstr-add lval args))
      (- (comp-cstr-sub lval args))
      (1+ (comp-cstr-add lval `(,(car args) ,comp-cstr-one)))
      (1- (comp-cstr-sub lval `(,(car args) ,comp-cstr-one))))))

(defun comp-fwprop-insn (insn)
  "Propagate within INSN."
  (pcase insn
    (`(set ,lval ,rval)
     (pcase rval
       (`(,(or 'call 'callref) ,f . ,args)
        (comp-fwprop-call insn lval f args))
       (`(,(or 'direct-call 'direct-callref) ,f . ,args)
        (let ((f (comp-func-name (gethash f (comp-ctxt-funcs-h comp-ctxt)))))
          (comp-fwprop-call insn lval f args)))
       (_
        (comp-mvar-propagate lval rval))))
    (`(assume ,lval ,(and (pred comp-mvar-p) rval))
     (comp-mvar-propagate lval rval))
    (`(assume ,lval (,kind . ,operands))
     (cl-case kind
       (and
        (apply #'comp-cstr-intersection lval operands))
       (and-nhc
        (apply #'comp-cstr-intersection-no-hashcons lval operands))
       (not
        ;; Prevent double negation!
        (unless (comp-cstr-neg (car operands))
          (comp-cstr-value-negation lval (car operands))))
       (>
        (comp-cstr-> lval (car operands) (cadr operands)))
       (>=
        (comp-cstr->= lval (car operands) (cadr operands)))
       (<
        (comp-cstr-< lval (car operands) (cadr operands)))
       (<=
        (comp-cstr-<= lval (car operands) (cadr operands)))
       (=
        (comp-cstr-= lval (car operands) (cadr operands)))))
    (`(setimm ,lval ,v)
     (setf (comp-cstr-imm lval) v))
    (`(phi ,lval . ,rest)
     (let* ((from-latch (cl-some
                         (lambda (x)
                           (let* ((bb-name (cadr x))
                                  (bb (gethash bb-name
                                               (comp-func-blocks comp-func))))
                             (or (comp-latch-p bb)
                                 (when (comp-block-cstr-p bb)
                                   (comp-latch-p (car (comp-block-preds bb)))))))
                         rest))
            (prop-fn (if from-latch
                         #'comp-cstr-union-no-range
                       #'comp-cstr-union))
            (rvals (mapcar #'car rest)))
       (apply prop-fn lval rvals)))))

(defun comp-fwprop* ()
  "Propagate for set* and phi operands.
Return t if something was changed."
  (cl-loop named outer
           with modified = nil
           with i = 0
           for b being each hash-value of (comp-func-blocks comp-func)
           do (cl-loop
               with comp-block = b
               for insn in (comp-block-insns b)
               for orig-insn = (unless modified
                                 ;; Save consing after 1st change.
                                 (comp-copy-insn insn))
               do
               (comp-fwprop-insn insn)
               (cl-incf i)
               when (and (null modified) (not (equal insn orig-insn)))
                 do (setf modified t))
               when (> i comp-fwprop-max-insns-scan)
                 do (cl-return-from outer nil)
           finally return modified))

(defun comp-rewrite-non-locals ()
  "Make explicit in LIMPLE non-local exits if identified."
  (cl-loop
   for bb being each hash-value of (comp-func-blocks comp-func)
   for non-local-insn = (and (comp-block-lap-p bb)
                             (comp-block-lap-non-ret-insn bb))
   when non-local-insn
   do
   ;; Rework the current block.
   (let* ((insn-seq (memq non-local-insn (comp-block-insns bb))))
     (setf (comp-block-lap-non-ret-insn bb) ()
           (comp-block-lap-no-ret bb) t
           (comp-block-out-edges bb) ()
           ;; Prune unnecessary insns!
           (cdr insn-seq) '((unreachable))
           (comp-func-ssa-status comp-func) 'dirty))))

(defun comp-fwprop (_)
  "Forward propagate types and consts within the lattice."
  (comp-ssa)
  (comp-dead-code)
  (maphash (lambda (_ f)
             (when (and (>= (comp-func-speed f) 2)
                        ;; FIXME remove the following condition when tested.
                        (not (comp-func-has-non-local f)))
               (let ((comp-func f))
                 (comp-fwprop-prologue)
                 (cl-loop
                  for i from 1 to 100
                  while (comp-fwprop*)
                  finally
                  (when (= i 100)
                    (display-warning
                     'comp
                     (format "fwprop pass jammed into %s?" (comp-func-name f))))
                  (comp-log (format "Propagation run %d times\n" i) 2))
                 (comp-rewrite-non-locals)
                 (comp-log-func comp-func 3))))
           (comp-ctxt-funcs-h comp-ctxt)))


;;; Call optimizer pass specific code.
;; This pass is responsible for the following optimizations:
;; - Call to subrs that are in defined in the C source and are passing through
;;   funcall trampoline gets optimized into normal indirect calls.
;;   This makes effectively this calls equivalent to all the subrs that got
;;   dedicated byte-code ops.
;;   Triggered at native-comp-speed >= 2.
;; - Recursive calls gets optimized into direct calls.
;;   Triggered at native-comp-speed >= 2.
;; - Intra compilation unit procedure calls gets optimized into direct calls.
;;   This can be a big win and even allow gcc to inline but does not make
;;   function in the compilation unit re-definable safely without recompiling
;;   the full compilation unit.
;;   For this reason this is triggered only at native-comp-speed == 3.

(defun comp-func-in-unit (func)
  "Given FUNC return the `comp-fun' definition in the current context.
FUNCTION can be a function-name or byte compiled function."
  (if (symbolp func)
      (comp-symbol-func-to-fun func)
    (cl-assert (byte-code-function-p func))
    (gethash func (comp-ctxt-byte-func-to-func-h comp-ctxt))))

(defun comp-call-optim-form-call (callee args)
  (cl-flet ((fill-args (args total)
              ;; Fill missing args to reach TOTAL
              (append args (cl-loop repeat (- total (length args))
                                    collect (make-comp-mvar :constant nil)))))
    (when (and callee
               (or (symbolp callee)
                   (gethash callee (comp-ctxt-byte-func-to-func-h comp-ctxt)))
               (not (memq callee native-comp-never-optimize-functions)))
      (let* ((f (if (symbolp callee)
                    (symbol-function callee)
                  (cl-assert (byte-code-function-p callee))
                  callee))
             (subrp (subrp f))
             (comp-func-callee (comp-func-in-unit callee)))
        (cond
         ((and subrp (not (subr-native-elisp-p f)))
          ;; Trampoline removal.
          (let* ((callee (intern (subr-name f))) ; Fix aliased names.
                 (maxarg (cdr (subr-arity f)))
                 (call-type (if (if subrp
                                    (not (numberp maxarg))
                                  (comp-nargs-p comp-func-callee))
                                'callref
                              'call))
                 (args (if (eq call-type 'callref)
                           args
                         (fill-args args maxarg))))
            `(,call-type ,callee ,@args)))
         ;; Intra compilation unit procedure call optimization.
         ;; Attention speed 3 triggers this for non self calls too!!
         ((and comp-func-callee
               (comp-func-c-name comp-func-callee)
               (or (and (>= (comp-func-speed comp-func) 3)
                        (comp-func-unique-in-cu-p callee))
                   (and (>= (comp-func-speed comp-func) 2)
                        ;; Anonymous lambdas can't be redefined so are
                        ;; always safe to optimize.
                        (byte-code-function-p callee))))
          (let* ((func-args (comp-func-l-args comp-func-callee))
                 (nargs (comp-nargs-p func-args))
                 (call-type (if nargs 'direct-callref 'direct-call))
                 (args (if (eq call-type 'direct-callref)
                           args
                         (fill-args args (comp-args-max func-args)))))
            `(,call-type ,(comp-func-c-name comp-func-callee) ,@args)))
         ((comp-type-hint-p callee)
          `(call ,callee ,@args)))))))

(defun comp-call-optim-func ()
  "Perform the trampoline call optimization for the current function."
  (cl-loop
   for b being each hash-value of (comp-func-blocks comp-func)
   do (comp-loop-insn-in-block b
        (pcase insn
          (`(set ,lval (callref funcall ,f . ,rest))
           (when-let ((ok (comp-cstr-imm-vld-p f))
                      (new-form (comp-call-optim-form-call
                                 (comp-cstr-imm f) rest)))
             (setf insn `(set ,lval ,new-form))))
          (`(callref funcall ,f . ,rest)
           (when-let ((ok (comp-cstr-imm-vld-p f))
                      (new-form (comp-call-optim-form-call
                                 (comp-cstr-imm f) rest)))
             (setf insn new-form)))))))

(defun comp-call-optim (_)
  "Try to optimize out funcall trampoline usage when possible."
  (maphash (lambda (_ f)
             (when (and (>= (comp-func-speed f) 2)
                        (comp-func-l-p f))
               (let ((comp-func f))
                 (comp-call-optim-func))))
           (comp-ctxt-funcs-h comp-ctxt)))


;;; Dead code elimination pass specific code.
;; This simple pass try to eliminate insns became useful after propagation.
;; Even if gcc would take care of this is good to perform this here
;; in the hope of removing memory references.
;;
;; This pass can be run as last optim.

(defun comp-collect-mvar-ids (insn)
  "Collect the m-var unique identifiers into INSN."
  (cl-loop for x in insn
           if (consp x)
             append (comp-collect-mvar-ids x)
           else
             when (comp-mvar-p x)
               collect (comp-mvar-id x)))

(defun comp-dead-assignments-func ()
  "Clean-up dead assignments into current function.
Return the list of m-var ids nuked."
  (let ((l-vals ())
        (r-vals ()))
    ;; Collect used r and l-values.
    (cl-loop
     for b being each hash-value of (comp-func-blocks comp-func)
     do (cl-loop
         for insn in (comp-block-insns b)
         for (op arg0 . rest) = insn
         if (comp-assign-op-p op)
           do (push (comp-mvar-id arg0) l-vals)
              (setf r-vals (nconc (comp-collect-mvar-ids rest) r-vals))
         else
           do (setf r-vals (nconc (comp-collect-mvar-ids insn) r-vals))))
    ;; Every l-value appearing that does not appear as r-value has no right to
    ;; exist and gets nuked.
    (let ((nuke-list (cl-set-difference l-vals r-vals)))
      (comp-log (format "Function %s\nl-vals %s\nr-vals %s\nNuking ids: %s\n"
                        (comp-func-name comp-func)
                        l-vals
                        r-vals
                        nuke-list)
                3)
      (cl-loop
       for b being each hash-value of (comp-func-blocks comp-func)
       do (comp-loop-insn-in-block b
            (cl-destructuring-bind (op &optional arg0 arg1 &rest rest) insn
              (when (and (comp-assign-op-p op)
                         (memq (comp-mvar-id arg0) nuke-list))
                (setf insn
                      (if (comp-limple-insn-call-p arg1)
                          arg1
                        `(comment ,(format "optimized out: %s"
                                           insn))))))))
      nuke-list)))

(defun comp-dead-code ()
  "Dead code elimination."
  (maphash (lambda (_ f)
             (when (and (>= (comp-func-speed f) 2)
                        ;; FIXME remove the following condition when tested.
                        (not (comp-func-has-non-local f)))
               (cl-loop
                for comp-func = f
                for i from 1
                while (comp-dead-assignments-func)
                finally (comp-log (format "dead code rm run %d times\n" i) 2)
                (comp-log-func comp-func 3))))
           (comp-ctxt-funcs-h comp-ctxt)))


;;; Tail Call Optimization pass specific code.

(defun comp-form-tco-call-seq (args)
  "Generate a TCO sequence for ARGS."
  `(,@(cl-loop for arg in args
               for i from 0
               collect `(set ,(make-comp-mvar :slot i) ,arg))
    (jump bb_0)))

(defun comp-tco-func ()
  "Try to pattern match and perform TCO within the current function."
  (cl-loop
   for b being each hash-value of (comp-func-blocks comp-func)
   do (cl-loop
       named in-the-basic-block
       for insns-seq on (comp-block-insns b)
       do (pcase insns-seq
            (`((set ,l-val (direct-call ,func . ,args))
               ;; (comment ,_comment)
               (return ,ret-val))
             (when (and (string= func (comp-func-c-name comp-func))
                        (eq l-val ret-val))
               (let ((tco-seq (comp-form-tco-call-seq args)))
                 (setf (car insns-seq) (car tco-seq)
                       (cdr insns-seq) (cdr tco-seq)
                       (comp-func-ssa-status comp-func) 'dirty)
                 (cl-return-from in-the-basic-block))))))))

(defun comp-tco (_)
  "Simple peephole pass performing self TCO."
  (maphash (lambda (_ f)
             (when (and (>= (comp-func-speed f) 3)
                        (comp-func-l-p f)
                        (not (comp-func-has-non-local f)))
               (let ((comp-func f))
                 (comp-tco-func)
                 (comp-log-func comp-func 3))))
           (comp-ctxt-funcs-h comp-ctxt)))


;;; Type hint removal pass specific code.

;; This must run after all SSA prop not to have the type hint
;; information overwritten.

(defun comp-remove-type-hints-func ()
  "Remove type hints from the current function.
These are substituted with a normal 'set' op."
  (cl-loop
   for b being each hash-value of (comp-func-blocks comp-func)
   do (comp-loop-insn-in-block b
        (pcase insn
          (`(set ,l-val (call ,(pred comp-type-hint-p) ,r-val))
           (setf insn `(set ,l-val ,r-val)))))))

(defun comp-remove-type-hints (_)
  "Dead code elimination."
  (maphash (lambda (_ f)
             (when (>= (comp-func-speed f) 2)
               (let ((comp-func f))
                 (comp-remove-type-hints-func)
                 (comp-log-func comp-func 3))))
           (comp-ctxt-funcs-h comp-ctxt)))


;;; Final pass specific code.

(defun comp-args-to-lambda-list (args)
  "Return a lambda list for ARGS."
  (cl-loop
   with res
   repeat (comp-args-base-min args)
   do (push t res)
   finally
   (if (comp-args-p args)
       (cl-loop
        with n = (- (comp-args-max args) (comp-args-min args))
        initially (unless (zerop n)
                    (push '&optional res))
        repeat n
        do (push t res))
     (cl-loop
      with n = (- (comp-nargs-nonrest args) (comp-nargs-min args))
      initially (unless (zerop n)
                  (push '&optional res))
      repeat n
      do (push t res)
      finally (when (comp-nargs-rest args)
                (push '&rest res)
                (push 't res))))
   (cl-return (reverse res))))

(defun comp-compute-function-type (_ func)
  "Compute type specifier for `comp-func' FUNC.
Set it into the `type' slot."
  (when (and (comp-func-l-p func)
             (comp-mvar-p (comp-func-type func)))
    (let* ((comp-func (make-comp-func))
           (res-mvar (apply #'comp-cstr-union
                            (make-comp-cstr)
                            (cl-loop
                             with res = nil
                             for bb being the hash-value in (comp-func-blocks
                                                             func)
                             do (cl-loop
                                 for insn in (comp-block-insns bb)
                                 ;; Collect over every exit point the returned
                                 ;; mvars and union results.
                                 do (pcase insn
                                      (`(return ,mvar)
                                       (push mvar res))))
                             finally return res)))
           (type `(function ,(comp-args-to-lambda-list (comp-func-l-args func))
                            ,(comp-cstr-to-type-spec res-mvar))))
      (comp-add-const-to-relocs type)
      ;; Fix it up.
      (setf (comp-cstr-imm (comp-func-type func)) type))))

(defun comp-finalize-container (cont)
  "Finalize data container CONT."
  (setf (comp-data-container-l cont)
        (cl-loop with h = (comp-data-container-idx cont)
                 for obj each hash-keys of h
                 for i from 0
                 do (puthash obj i h)
                 ;; Prune byte-code objects coming from lambdas.
                 ;; These are not anymore necessary as they will be
                 ;; replaced at load time by native-elisp-subrs.
                 ;; Note: we leave the objects in the idx hash table
                 ;; to still be able to retrieve the correct index
                 ;; from the corresponding m-var.
                 collect (if (gethash obj
                                      (comp-ctxt-byte-func-to-func-h comp-ctxt))
                             'lambda-fixup
                           obj))))

(defun comp-finalize-relocs ()
  "Finalize data containers for each relocation class.
Remove immediate duplicates within relocation classes.
Update all insn accordingly."
  ;; Symbols imported by C inlined functions.  We do this here because
  ;; is better to add all objs to the relocation containers before we
  ;; compacting them.
  (mapc #'comp-add-const-to-relocs '(nil t consp listp))

  (let* ((d-default (comp-ctxt-d-default comp-ctxt))
         (d-default-idx (comp-data-container-idx d-default))
         (d-impure (comp-ctxt-d-impure comp-ctxt))
         (d-impure-idx (comp-data-container-idx d-impure))
         (d-ephemeral (comp-ctxt-d-ephemeral comp-ctxt))
         (d-ephemeral-idx (comp-data-container-idx d-ephemeral)))
    ;; We never want compiled lambdas ending up in pure space.  A copy must
    ;; be already present in impure (see `comp-emit-lambda-for-top-level').
    (cl-loop for obj being each hash-keys of d-default-idx
             when (gethash obj (comp-ctxt-lambda-fixups-h comp-ctxt))
               do (cl-assert (gethash obj d-impure-idx))
                  (remhash obj d-default-idx))
    ;; Remove entries in d-impure already present in d-default.
    (cl-loop for obj being each hash-keys of d-impure-idx
             when (gethash obj d-default-idx)
               do (remhash obj d-impure-idx))
    ;; Remove entries in d-ephemeral already present in d-default or
    ;; d-impure.
    (cl-loop for obj being each hash-keys of d-ephemeral-idx
             when (or (gethash obj d-default-idx) (gethash obj d-impure-idx))
               do (remhash obj d-ephemeral-idx))
    ;; Fix-up indexes in each relocation class and fill corresponding
    ;; reloc lists.
    (mapc #'comp-finalize-container (list d-default d-impure d-ephemeral))
    ;; Make a vector from the function documentation hash table.
    (cl-loop with h = (comp-ctxt-function-docs comp-ctxt)
             with v = (make-vector (hash-table-count h) nil)
             for idx being each hash-keys of h
             for doc = (gethash idx h)
             do (setf (aref v idx) doc)
             finally
             do (setf (comp-ctxt-function-docs comp-ctxt) v))
    ;; And now we conclude with the following: We need to pass to
    ;; `comp--register-lambda' the index in the impure relocation
    ;; array to store revived lambdas, but given we know it only now
    ;; we fix it up as last.
    (cl-loop for f being each hash-keys of (comp-ctxt-lambda-fixups-h comp-ctxt)
             using (hash-value mvar)
             with reverse-h = (make-hash-table) ;; Make sure idx is unique.
             for idx = (gethash f d-impure-idx)
             do
             (cl-assert (null (gethash idx reverse-h)))
             (cl-assert (fixnump idx))
             (setf (comp-mvar-valset mvar) ()
                   (comp-mvar-range mvar) (list (cons idx idx)))
             (puthash idx t reverse-h))))

(defun comp-compile-ctxt-to-file (name)
  "Compile as native code the current context naming it NAME.
Prepare every function for final compilation and drive the C back-end."
  (let ((dir (file-name-directory name)))
    (comp-finalize-relocs)
    (maphash (lambda (_ f)
               (comp-log-func f 1))
             (comp-ctxt-funcs-h comp-ctxt))
    (unless (file-exists-p dir)
      ;; In case it's created in the meanwhile.
      (ignore-error file-already-exists
        (make-directory dir t)))
    (comp--compile-ctxt-to-file name)))

(defun comp-final1 ()
  (let (compile-result)
    (comp--init-ctxt)
    (unwind-protect
        (setf compile-result
              (comp-compile-ctxt-to-file (comp-ctxt-output comp-ctxt)))
      (and (comp--release-ctxt)
           compile-result))))

(defvar comp-async-compilation nil
  "Non-nil while executing an asynchronous native compilation.")

(defvar comp-running-batch-compilation nil
  "Non-nil when compilation is driven by any `batch-*-compile' function.")

(defun comp-final (_)
  "Final pass driving the C back-end for code emission."
  (maphash #'comp-compute-function-type (comp-ctxt-funcs-h comp-ctxt))
  (unless comp-dry-run
    ;; Always run the C side of the compilation as a sub-process
    ;; unless during bootstrap or async compilation (bug#45056).  GCC
    ;; leaks memory but also interfere with the ability of Emacs to
    ;; detect when a sub-process completes (TODO understand why).
    (if (or comp-running-batch-compilation comp-async-compilation)
	(comp-final1)
      ;; Call comp-final1 in a child process.
      (let* ((output (comp-ctxt-output comp-ctxt))
             (print-escape-newlines t)
             (print-length nil)
             (print-level nil)
             (print-quoted t)
             (print-gensym t)
             (print-circle t)
             (print-escape-multibyte t)
             (expr `((require 'comp)
                     (setf native-comp-verbose ,native-comp-verbose
                           comp-libgccjit-reproducer ,comp-libgccjit-reproducer
                           comp-ctxt ,comp-ctxt
                           native-comp-eln-load-path ',native-comp-eln-load-path
                           native-comp-compiler-options
                           ',native-comp-compiler-options
                           native-comp-driver-options
                           ',native-comp-driver-options
                           load-path ',load-path)
                     ,native-comp-async-env-modifier-form
                     (message "Compiling %s..." ',output)
                     (comp-final1)))
             (temp-file (make-temp-file
			 (concat "emacs-int-comp-"
				 (file-name-base output) "-")
			 nil ".el")))
	(with-temp-file temp-file
          (insert ";; -*-coding: nil; -*-\n")
          (mapc (lambda (e)
                  (insert (prin1-to-string e)))
                expr))
	(with-temp-buffer
          (unwind-protect
              (if (zerop
                   (call-process (expand-file-name invocation-name
                                                   invocation-directory)
				 nil t t "--batch" "-l" temp-file))
                  (progn
                    (delete-file temp-file)
                    output)
		(signal 'native-compiler-error (buffer-string)))
            (comp-log-to-buffer (buffer-string))))))))


;;; Compiler type hints.
;; Public entry points to be used by user code to give comp
;; suggestions about types.  These are used to implement CL style
;; `cl-the' and hopefully parameter type declaration.
;; Note: types will propagates.
;; WARNING: At speed >= 2 type checking is not performed anymore and suggestions
;; are assumed just to be true. Use with extreme caution...

(defun comp-hint-fixnum (x)
  (declare (gv-setter (lambda (val) `(setf ,x ,val))))
  x)

(defun comp-hint-cons (x)
  (declare (gv-setter (lambda (val) `(setf ,x ,val))))
  x)


;; Primitive function advice machinery

(defun comp-eln-load-path-eff ()
  "Return a list of effective eln load directories.
Account for `native-comp-eln-load-path' and `comp-native-version-dir'."
  (mapcar (lambda (dir)
            (expand-file-name comp-native-version-dir
                              (file-name-as-directory
                               (expand-file-name dir invocation-directory))))
          native-comp-eln-load-path))

(defun comp-trampoline-filename (subr-name)
  "Given SUBR-NAME return the filename containing the trampoline."
  (concat (comp-c-func-name subr-name "subr--trampoline-" t) ".eln"))

(defun comp-make-lambda-list-from-subr (subr)
  "Given SUBR return the equivalent lambda-list."
  (pcase-let ((`(,min . ,max) (subr-arity subr))
              (lambda-list '()))
    (cl-loop repeat min
             do (push (gensym "arg") lambda-list))
    (if (numberp max)
        (cl-loop
         initially (push '&optional lambda-list)
         repeat (- max min)
         do (push (gensym "arg") lambda-list))
      (push '&rest lambda-list)
      (push (gensym "arg") lambda-list))
    (reverse lambda-list)))

(defun comp-trampoline-search (subr-name)
  "Search a trampoline file for SUBR-NAME.
Return the trampoline if found or nil otherwise."
  (cl-loop
   with rel-filename = (comp-trampoline-filename subr-name)
   for dir in (comp-eln-load-path-eff)
   for filename = (expand-file-name rel-filename dir)
   when (file-exists-p filename)
     do (cl-return (native-elisp-load filename))))

(defun comp-trampoline-compile (subr-name)
  "Synthesize compile and return a trampoline for SUBR-NAME."
  (let* ((lambda-list (comp-make-lambda-list-from-subr
                       (symbol-function subr-name)))
         ;; The synthesized trampoline must expose the exact same ABI of
         ;; the primitive we are replacing in the function reloc table.
         (form `(lambda ,lambda-list
                  (let ((f #',subr-name))
                    (,(if (memq '&rest lambda-list) #'apply 'funcall)
                     f
                     ,@(cl-loop
                        for arg in lambda-list
                        unless (memq arg '(&optional &rest))
                        collect arg)))))
         ;; Use speed 1 for compilation speed and not to optimize away
         ;; funcall calls!
         (byte-optimize nil)
         (native-comp-speed 1)
         (lexical-binding t))
    (comp--native-compile
     form nil
     (cl-loop
      for dir in (if native-compile-target-directory
                     (list (expand-file-name comp-native-version-dir
                                             native-compile-target-directory))
                   (comp-eln-load-path-eff))
      for f = (expand-file-name
               (comp-trampoline-filename subr-name)
               dir)
      unless (file-exists-p dir)
        do (ignore-errors
             (make-directory dir t)
             (cl-return f))
      when (file-writable-p f)
        do (cl-return f)
      finally (error "Cannot find suitable directory for output in \
`native-comp-eln-load-path'")))))


;; Some entry point support code.

;;;###autoload
(defun comp-clean-up-stale-eln (file)
  "Remove all FILE*.eln* files found in `native-comp-eln-load-path'.
The files to be removed are those produced from the original source
filename (including FILE)."
  (when (string-match (rx "-" (group-n 1 (1+ hex)) "-" (1+ hex) ".eln" eos)
                      file)
    (cl-loop
     with filename-hash = (match-string 1 file)
     with regexp = (rx-to-string
                    `(seq "-" ,filename-hash "-" (1+ hex) ".eln" eos))
     for dir in (comp-eln-load-path-eff)
     do (cl-loop
         for f in (when (file-exists-p dir)
		    (directory-files dir t regexp t))
         ;; We may not be able to delete the file if we have no write
         ;; permission.
         do (ignore-error file-error
              (comp-delete-or-replace-file f))))))

(defun comp-delete-or-replace-file (oldfile &optional newfile)
  "Replace OLDFILE with NEWFILE.
When NEWFILE is nil just delete OLDFILE.
Takes the necessary steps when dealing with OLDFILE being a
shared library that might be currently loaded into a running Emacs
session."
  (cond ((eq 'windows-nt system-type)
         (ignore-errors (delete-file oldfile))
         (while
             (condition-case _
                 (progn
                   ;; oldfile maybe recreated by another Emacs in
                   ;; between the following two rename-file calls
                   (if (file-exists-p oldfile)
                       (rename-file oldfile (make-temp-file-internal
                                             (file-name-sans-extension oldfile)
                                             nil ".eln.old" nil)
                                    t))
                   (when newfile
                     (rename-file newfile oldfile nil))
                   ;; Keep on trying.
                   nil)
               (file-already-exists
                ;; Done
                t))))
        ;; Remove the old eln instead of copying the new one into it
        ;; to get a new inode and prevent crashes in case the old one
        ;; is currently loaded.
        (t (delete-file oldfile)
           (when newfile
             (rename-file newfile oldfile)))))

(defvar comp-files-queue ()
  "List of Emacs Lisp files to be compiled.")

(defvar comp-async-compilations (make-hash-table :test #'equal)
  "Hash table file-name -> async compilation process.")

(defun comp-async-runnings ()
  "Return the number of async compilations currently running.
This function has the side effect of cleaning-up finished
processes from `comp-async-compilations'"
  (cl-loop
   for file-name in (cl-loop
                     for file-name being each hash-key of comp-async-compilations
                     for prc = (gethash file-name comp-async-compilations)
                     unless (process-live-p prc)
                       collect file-name)
   do (remhash file-name comp-async-compilations))
  (hash-table-count comp-async-compilations))

(defvar comp-num-cpus nil)
(defun comp-effective-async-max-jobs ()
  "Compute the effective number of async jobs."
  (if (zerop native-comp-async-jobs-number)
      (or comp-num-cpus
          (setf comp-num-cpus
		(max 1 (/ (num-processors) 2))))
    native-comp-async-jobs-number))

(defvar comp-last-scanned-async-output nil)
(make-variable-buffer-local 'comp-last-scanned-async-output)
(defun comp-accept-and-process-async-output (process)
  "Accept PROCESS output and check for diagnostic messages."
  (if native-comp-async-report-warnings-errors
      (let ((warning-suppress-types
             (if (eq native-comp-async-report-warnings-errors 'silent)
                 (cons '(comp) warning-suppress-types)
               warning-suppress-types)))
        (with-current-buffer (process-buffer process)
          (save-excursion
            (accept-process-output process)
            (goto-char (or comp-last-scanned-async-output (point-min)))
            (while (re-search-forward "^.*?\\(?:Error\\|Warning\\): .*$"
                                      nil t)
              (display-warning 'comp (match-string 0)))
            (setq comp-last-scanned-async-output (point-max)))))
    (accept-process-output process)))

(defun comp-run-async-workers ()
  "Start compiling files from `comp-files-queue' asynchronously.
When compilation is finished, run `native-comp-async-all-done-hook' and
display a message."
  (if (or comp-files-queue
          (> (comp-async-runnings) 0))
      (unless (>= (comp-async-runnings) (comp-effective-async-max-jobs))
        (cl-loop
         for (source-file . load) = (pop comp-files-queue)
         while source-file
         do (cl-assert (string-match-p comp-valid-source-re source-file) nil
                       "`comp-files-queue' should be \".el\" files: %s"
                       source-file)
         when (or native-comp-always-compile
                  load ; Always compile when the compilation is
                       ; commanded for late load.
                  (file-newer-than-file-p
                   source-file (comp-el-to-eln-filename source-file)))
         do (let* ((expr `((require 'comp)
                           ,(when (boundp 'backtrace-line-length)
                              `(setf backtrace-line-length ,backtrace-line-length))
                           (setf comp-file-preloaded-p ,comp-file-preloaded-p
                                 native-compile-target-directory ,native-compile-target-directory
                                 native-comp-speed ,native-comp-speed
                                 native-comp-debug ,native-comp-debug
                                 native-comp-verbose ,native-comp-verbose
                                 comp-libgccjit-reproducer ,comp-libgccjit-reproducer
                                 comp-async-compilation t
                                 native-comp-eln-load-path ',native-comp-eln-load-path
                                 native-comp-compiler-options
                                 ',native-comp-compiler-options
                                 native-comp-driver-options
                                 ',native-comp-driver-options
                                 load-path ',load-path
                                 warning-fill-column most-positive-fixnum)
                           ,native-comp-async-env-modifier-form
                           (message "Compiling %s..." ,source-file)
                           (comp--native-compile ,source-file ,(and load t))))
                   (source-file1 source-file) ;; Make the closure works :/
                   (temp-file (make-temp-file
                               (concat "emacs-async-comp-"
                                       (file-name-base source-file) "-")
                               nil ".el"))
                   (expr-strings (let ((print-length nil)
                                       (print-level nil))
                                   (mapcar #'prin1-to-string expr)))
                   (_ (progn
                        (with-temp-file temp-file
                          (mapc #'insert expr-strings))
                        (comp-log "\n")
                        (mapc #'comp-log expr-strings)))
                   (load1 load)
                   (process (make-process
                             :name (concat "Compiling: " source-file)
                             :buffer (with-current-buffer
                                         (get-buffer-create
                                          comp-async-buffer-name)
                                       (setf buffer-read-only t)
			               (current-buffer))
                             :command (list
                                       (expand-file-name invocation-name
                                                         invocation-directory)
                                       "--batch" "-l" temp-file)
                             :sentinel
                             (lambda (process _event)
                               (run-hook-with-args
                                'native-comp-async-cu-done-functions
                                source-file)
                               (comp-accept-and-process-async-output process)
                               (ignore-errors (delete-file temp-file))
                               (let ((eln-file (comp-el-to-eln-filename
                                                source-file1)))
                                 (when (and load1
                                            (zerop (process-exit-status
                                                    process))
                                            (file-exists-p eln-file))
                                   (native-elisp-load eln-file
                                                      (eq load1 'late))))
                               (comp-run-async-workers))
                             :noquery (not native-comp-async-query-on-exit))))
              (puthash source-file process comp-async-compilations))
         when (>= (comp-async-runnings) (comp-effective-async-max-jobs))
           do (cl-return)))
    ;; No files left to compile and all processes finished.
    (run-hooks 'native-comp-async-all-done-hook)
    (with-current-buffer (get-buffer-create comp-async-buffer-name)
      (save-excursion
        (let ((buffer-read-only nil))
          (goto-char (point-max))
          (insert "Compilation finished.\n"))))
    ;; `comp-deferred-pending-h' should be empty at this stage.
    ;; Reset it anyway.
    (clrhash comp-deferred-pending-h)))

(defun comp--native-compile (function-or-file &optional with-late-load output)
  "Compile FUNCTION-OR-FILE into native code.
When WITH-LATE-LOAD is non-nil, mark the compilation unit for late
load once it finishes compiling.
This serves as internal implementation of `native-compile' but
allowing for WITH-LATE-LOAD to be controlled is in use also for
the deferred compilation mechanism."
  (comp-ensure-native-compiler)
  (unless (or (functionp function-or-file)
              (stringp function-or-file))
    (signal 'native-compiler-error
            (list "Not a function symbol or file" function-or-file)))
  (catch 'no-native-compile
    (let* ((data function-or-file)
           (comp-native-compiling t)
           (byte-native-qualities nil)
           ;; Have byte compiler signal an error when compilation fails.
           (byte-compile-debug t)
           (comp-ctxt (make-comp-ctxt :output output
                                      :with-late-load with-late-load)))
      (comp-log "\n\n" 1)
      (condition-case err
          (cl-loop
           with report = nil
           for t0 = (current-time)
           for pass in comp-passes
           unless (memq pass comp-disabled-passes)
           do
           (comp-log (format "(%s) Running pass %s:\n"
                             function-or-file pass)
                     2)
           (setf data (funcall pass data))
           (push (cons pass (float-time (time-since t0))) report)
           (cl-loop for f in (alist-get pass comp-post-pass-hooks)
                    do (funcall f data))
           finally
           (when comp-log-time-report
             (comp-log (format "Done compiling %s" data) 0)
             (cl-loop for (pass . time) in (reverse report)
                      do (comp-log (format "Pass %s took: %fs." pass time) 0))))
        (native-compiler-skip)
        (t
         (let ((err-val (cdr err)))
           ;; If we are doing an async native compilation print the
           ;; error in the correct format so is parsable and abort.
           (if (and comp-async-compilation
                    (not (eq (car err) 'native-compiler-error)))
               (progn
                 (message (if err-val
                              "%s: Error: %s %s"
                            "%s: Error %s")
                          function-or-file
                          (get (car err) 'error-message)
                          (car-safe err-val))
                 (kill-emacs -1))
             ;; Otherwise re-signal it adding the compilation input.
	     (signal (car err) (if (consp err-val)
			           (cons function-or-file err-val)
			         (list function-or-file err-val)))))))
      (if (stringp function-or-file)
          data
        ;; So we return the compiled function.
        (native-elisp-load data)))))

(defun native-compile-async-skip-p (file load selector)
  "Return non-nil if FILE's compilation should be skipped.

LOAD and SELECTOR work as described in `native--compile-async'."
  ;; Make sure we are not already compiling `file' (bug#40838).
  (or (gethash file comp-async-compilations)
      (cond
       ((null selector) nil)
       ((functionp selector) (not (funcall selector file)))
       ((stringp selector) (not (string-match-p selector file)))
       (t (error "SELECTOR must be a function a regexp or nil")))
      ;; Also exclude files from deferred compilation if
      ;; any of the regexps in
      ;; `native-comp-deferred-compilation-deny-list' matches.
      (and (eq load 'late)
           (cl-some (lambda (re)
                      (string-match-p re file))
                    native-comp-deferred-compilation-deny-list))))

(defun native--compile-async (files &optional recursively load selector)
  "Compile FILES asynchronously.
FILES is one filename or a list of filenames or directories.

If optional argument RECURSIVELY is non-nil, recurse into
subdirectories of given directories.

If optional argument LOAD is non-nil, request to load the file
after compiling.

The optional argument SELECTOR has the following valid values:

nil -- Select all files.
a string -- A regular expression selecting files with matching names.
a function -- A function selecting files with matching names.

The variable `native-comp-async-jobs-number' specifies the number
of (commands) to run simultaneously.

LOAD can also be the symbol `late'.  This is used internally if
the byte code has already been loaded when this function is
called.  It means that we request the special kind of load
necessary in that situation, called \"late\" loading.

During a \"late\" load, instead of executing all top-level forms
of the original files, only function definitions are
loaded (paying attention to have these effective only if the
bytecode definition was not changed in the meantime)."
  (comp-ensure-native-compiler)
  (unless (member load '(nil t late))
    (error "LOAD must be nil, t or 'late"))
  (unless (listp files)
    (setf files (list files)))
  (let (file-list)
    (dolist (path files)
      (cond ((file-directory-p path)
             (dolist (file (if recursively
                               (directory-files-recursively
                                path comp-valid-source-re)
                             (directory-files path t comp-valid-source-re)))
               (push file file-list)))
            ((file-exists-p path) (push path file-list))
            (t (signal 'native-compiler-error
                       (list "Path not a file nor directory" path)))))
    (dolist (file file-list)
      (if-let ((entry (cl-find file comp-files-queue :key #'car :test #'string=)))
          ;; Most likely the byte-compiler has requested a deferred
          ;; compilation, so update `comp-files-queue' to reflect that.
          (unless (or (null load)
                      (eq load (cdr entry)))
            (cl-substitute (cons file load) (car entry) comp-files-queue
                           :key #'car :test #'string=))

        (unless (native-compile-async-skip-p file load selector)
          (let* ((out-filename (comp-el-to-eln-filename file))
                 (out-dir (file-name-directory out-filename)))
            (unless (file-exists-p out-dir)
              (make-directory out-dir t))
            (if (file-writable-p out-filename)
                (setf comp-files-queue
                      (append comp-files-queue `((,file . ,load))))
              (display-warning 'comp
                               (format "No write access for %s skipping."
                                       out-filename)))))))
    (when (zerop (comp-async-runnings))
      (comp-run-async-workers))))


;;; Compiler entry points.

;;;###autoload
(defun comp-lookup-eln (filename)
  "Given a Lisp source FILENAME return the corresponding .eln file if found.
Search happens in `native-comp-eln-load-path'."
  (cl-loop
   with eln-filename = (comp-el-to-eln-rel-filename filename)
   for dir in native-comp-eln-load-path
   for f = (expand-file-name eln-filename
                             (expand-file-name comp-native-version-dir
                                               (expand-file-name
                                                dir
                                                invocation-directory)))
   when (file-exists-p f)
     do (cl-return f)))

;;;###autoload
(defun native-compile (function-or-file &optional output)
  "Compile FUNCTION-OR-FILE into native code.
This is the synchronous entry-point for the Emacs Lisp native
compiler.
FUNCTION-OR-FILE is a function symbol, a form, or the filename of
an Emacs Lisp source file.
If OUTPUT is non-nil, use it as the filename for the compiled
object.
If FUNCTION-OR-FILE is a filename, return the filename of the
compiled object.  If FUNCTION-OR-FILE is a function symbol or a
form, return the compiled function."
  (comp--native-compile function-or-file nil output))

;;;###autoload
(defun batch-native-compile (&optional for-tarball)
  "Perform batch native compilation of remaining command-line arguments.

Native compilation equivalent of `batch-byte-compile'.
Use this from the command line, with `-batch'; it won't work
in an interactive Emacs session.
Optional argument FOR-TARBALL non-nil means the file being compiled
as part of building the source tarball, in which case the .eln file
will be placed under the native-lisp/ directory (actually, in the
last directory in `native-comp-eln-load-path')."
  (comp-ensure-native-compiler)
  (let ((comp-running-batch-compilation t)
        (native-compile-target-directory
            (if for-tarball
                (car (last native-comp-eln-load-path)))))
    (cl-loop for file in command-line-args-left
             if (or (null byte+native-compile)
                    (cl-notany (lambda (re) (string-match re file))
                               native-comp-bootstrap-deny-list))
             do (comp--native-compile file)
             else
             do (byte-compile-file file))))

;;;###autoload
(defun batch-byte+native-compile ()
  "Like `batch-native-compile', but used for bootstrap.
Generate .elc files in addition to the .eln files.
Force the produced .eln to be outputted in the eln system
directory (the last entry in `native-comp-eln-load-path') unless
`native-compile-target-directory' is non-nil.  If the environment
variable 'NATIVE_DISABLED' is set, only byte compile."
  (comp-ensure-native-compiler)
  (if (equal (getenv "NATIVE_DISABLED") "1")
      (batch-byte-compile)
    (cl-assert (length= command-line-args-left 1))
    (let ((byte+native-compile t)
          (byte-to-native-output-file nil))
      (batch-native-compile)
      (pcase byte-to-native-output-file
        (`(,tempfile . ,target-file)
         (rename-file tempfile target-file t))))))

;;;###autoload
(defun native-compile-async (files &optional recursively load selector)
  "Compile FILES asynchronously.
FILES is one file or a list of filenames or directories.

If optional argument RECURSIVELY is non-nil, recurse into
subdirectories of given directories.

If optional argument LOAD is non-nil, request to load the file
after compiling.

The optional argument SELECTOR has the following valid values:

nil -- Select all files.
a string -- A regular expression selecting files with matching names.
a function -- A function selecting files with matching names.

The variable `native-comp-async-jobs-number' specifies the number
of (commands) to run simultaneously."
  ;; Normalize: we only want to pass t or nil, never e.g. `late'.
  (let ((load (not (not load))))
    (native--compile-async files recursively load selector)))

(provide 'comp)

;; LocalWords: limplified limplified limplification limplify Limple LIMPLE libgccjit elc eln

;;; comp.el ends here
