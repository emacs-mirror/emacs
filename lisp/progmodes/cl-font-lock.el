;;; cl-font-lock.el --- Pretty Common Lisp font locking -*- lexical-binding: t; -*-
;; Copyright (C) 2019-2022 Free Software Foundation, Inc.

;; Author: Yue Daian <sheepduke@gmail.com>
;; Maintainer: Spenser Truex <web@spensertruex.com>
;; Created: 2019-06-16
;; Old-Version: 0.3.0
;; Package-Requires: ((emacs "24.5"))
;; Keywords: lisp wp files convenience
;; URL: https://github.com/cl-font-lock/cl-font-lock

;; This file is part of GNU Emacs

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

;; Highlight all the symbols in the Common Lisp ANSI Standard.
;; Adds font-lock regexes to lisp-mode.

;;;; Todo:

;; - Integrate better into `lisp-mode' (e.g. enable it by default).
;; - Distinguish functions from macros like `pushnew'.

;;; Code:

;; The list of built-in functions and variables was actually not
;; extracted from the standard, but from SBCL with the following
;; (Common Lisp) code:

;; (defvar *functions* nil)
;; (defvar *symbols* nil)
;; (defvar *types* nil)

;; (let ((pack (find-package :common-lisp)))
;;   (do-all-symbols (sym)
;;     (cond
;;       ((not (eql pack (symbol-package sym))) nil)
;;       ((fboundp sym) (pushnew sym *functions*))
;;       ((find-class sym nil) (pushnew sym *types*))
;;       (t (pushnew sym *symbols*)))))


(defvar cl-font-lock-built-in--functions
  '("+" "-" "/" "/=" "<" "<=" "=" ">" ">=" "*" "1-" "1+" "abs" "acons" "acos"
    "acosh" "add-method" "adjoin" "adjustable-array-p" "adjust-array"
    "allocate-instance" "alpha-char-p" "alphanumericp" "and" "append" "apply"
    "apropos" "apropos-list" "aref" "arithmetic-error-operands"
    "arithmetic-error-operation" "array-dimension" "array-dimensions"
    "array-displacement" "array-element-type" "array-has-fill-pointer-p"
    "array-in-bounds-p" "arrayp" "array-rank" "array-row-major-index"
    "array-total-size" "ash" "asin" "asinh" "assoc" "assoc-if" "assoc-if-not"
    "atan" "atanh" "atom" "bit" "bit-and" "bit-andc1" "bit-andc2" "bit-eqv"
    "bit-ior" "bit-nand" "bit-nor" "bit-not" "bit-orc1" "bit-orc2"
    "bit-vector-p" "bit-xor" "boole" "both-case-p" "boundp"
    "broadcast-stream-streams" "butlast" "byte" "byte-position" "byte-size"
    "call-method" "call-next-method" "car" "catch" "cdr" "ceiling"
    "cell-error-name" "change-class" "char" "char/=" "char<" "char<=" "char="
    "char>" "char>=" "character" "characterp" "char-code" "char-downcase"
    "char-equal" "char-greaterp" "char-int" "char-lessp" "char-name"
    "char-not-equal" "char-not-greaterp" "char-not-lessp" "char-upcase" "cis"
    "class-name" "class-of" "clear-input" "clear-output" "close" "clrhash"
    "code-char" "coerce" "compile" "compiled-function-p" "compile-file"
    "compile-file-pathname" "compiler-macro-function" "complement" "complex"
    "complexp" "compute-applicable-methods" "compute-restarts" "concatenate"
    "concatenated-stream-streams" "conjugate" "cons" "consp" "constantly"
    "constantp" "continue" "copy-alist" "copy-list" "copy-pprint-dispatch"
    "copy-readtable" "copy-seq" "copy-structure" "copy-symbol" "copy-tree"
    "cos" "cosh" "count" "count-if" "count-if-not" "decf" "decode-float"
    "decode-universal-time" "delete" "delete-duplicates" "delete-file"
    "delete-if" "delete-if-not" "delete-package" "denominator" "deposit-field"
    "describe" "describe-object" "digit-char" "digit-char-p" "directory"
    "directory-namestring" "disassemble" "do-all-symbols" "documentation"
    "do-external-symbols" "do-symbols" "dpb" "dribble"
    "echo-stream-input-stream" "echo-stream-output-stream" "ed" "eighth" "elt"
    "encode-universal-time" "endp" "enough-namestring"
    "ensure-directories-exist" "ensure-generic-function" "eq" "eql" "equal"
    "equalp" "eval" "evenp" "every" "exp" "export" "expt" "fboundp" "fceiling"
    "fdefinition" "ffloor" "fifth" "file-author" "file-error-pathname"
    "file-length" "file-namestring" "file-position" "file-string-length"
    "file-write-date" "fill" "fill-pointer" "find" "find-all-symbols"
    "find-class" "find-if" "find-if-not" "find-method" "find-package"
    "find-restart" "find-symbol" "finish-output" "first" "float" "float-digits"
    "floatp" "float-precision" "float-radix" "float-sign" "floor" "fmakunbound"
    "force-output" "format" "formatter" "fourth" "fresh-line" "fround"
    "ftruncate" "funcall" "function" "function-keywords"
    "function-lambda-expression" "functionp" "gcd" "gensym" "gentemp" "get"
    "get-decoded-time" "get-dispatch-macro-character" "getf" "gethash"
    "get-internal-real-time" "get-internal-run-time" "get-macro-character"
    "get-output-stream-string" "get-properties" "get-setf-expansion"
    "get-universal-time" "graphic-char-p" "hash-table-count" "hash-table-p"
    "hash-table-rehash-size" "hash-table-rehash-threshold" "hash-table-size"
    "hash-table-test" "host-namestring" "identity" "imagpart" "import" "incf"
    "initialize-instance" "input-stream-p" "inspect" "integer-decode-float"
    "integer-length" "integerp" "interactive-stream-p" "intern" "intersection"
    "invalid-method-error" "invoke-debugger" "invoke-restart"
    "invoke-restart-interactively" "isqrt" "keywordp" "last" "lcm" "ldb"
    "ldb-test" "ldiff" "length" "lisp-implementation-type"
    "lisp-implementation-version" "list" "list\\*" "list-all-packages" "listen"
    "list-length" "listp" "load" "load-logical-pathname-translations"
    "load-time-value" "log" "logand" "logandc1" "logandc2" "logbitp" "logcount"
    "logeqv" "logical-pathname" "logical-pathname-translations" "logior"
    "lognand" "lognor" "lognot" "logorc1" "logorc2" "logtest" "logxor"
    "long-site-name" "loop-finish" "lower-case-p" "machine-instance"
    "machine-type" "machine-version" "macroexpand" "macroexpand-1"
    "macro-function" "make-array" "make-array" "make-broadcast-stream"
    "make-concatenated-stream" "make-condition" "make-dispatch-macro-character"
    "make-echo-stream" "make-hash-table" "make-instance"
    "make-instances-obsolete" "make-list" "make-load-form"
    "make-load-form-saving-slots" "make-method" "make-package" "make-pathname"
    "make-random-state" "make-sequence" "make-string"
    "make-string-input-stream" "make-string-output-stream" "make-symbol"
    "make-synonym-stream" "make-two-way-stream" "makunbound" "map" "mapc"
    "mapcan" "mapcar" "mapcon" "maphash" "map-into" "mapl" "maplist"
    "mask-field" "max" "member" "member-if" "member-if-not" "merge"
    "merge-pathnames" "method-combination-error" "method-qualifiers" "min"
    "minusp" "mismatch" "mod" "muffle-warning" "multiple-value-call"
    "multiple-value-list" "multiple-value-setq" "name-char" "namestring"
    "nbutlast" "nconc" "next-method-p" "nintersection" "ninth"
    "no-applicable-method" "no-next-method" "not" "notany" "notevery" "nreconc"
    "nreverse" "nset-difference" "nset-exclusive-or" "nstring-capitalize"
    "nstring-downcase" "nstring-upcase" "nsublis" "nsubst" "nsubst-if"
    "nsubst-if-not" "nsubstitute" "nsubstitute-if" "nsubstitute-if-not" "nth"
    "nthcdr" "nth-value" "null" "numberp" "numerator" "nunion" "oddp" "open"
    "open-stream-p" "or" "output-stream-p" "package-error-package"
    "package-name" "package-nicknames" "packagep" "package-shadowing-symbols"
    "package-used-by-list" "package-use-list" "pairlis" "parse-integer"
    "parse-namestring" "pathname" "pathname-device" "pathname-directory"
    "pathname-host" "pathname-match-p" "pathname-name" "pathnamep"
    "pathname-type" "pathname-version" "peek-char" "phase" "plusp" "pop"
    "position" "position-if" "position-if-not" "pprint" "pprint-dispatch"
    "pprint-exit-if-list-exhausted" "pprint-fill" "pprint-indent"
    "pprint-linear" "pprint-logical-block" "pprint-newline" "pprint-pop"
    "pprint-tab" "pprint-tabular" "prin1" "prin1-to-string" "princ"
    "princ-to-string" "print" "print-not-readable-object" "print-object"
    "print-unreadable-object" "probe-file" "provide" "psetf" "psetq" "push"
    "pushnew" "quote" "random" "random-state-p" "rassoc" "rassoc-if"
    "rassoc-if-not" "rational" "rationalize" "rationalp" "read" "read-byte"
    "read-char" "read-char-no-hang" "read-delimited-list" "read-from-string"
    "read-line" "read-preserving-whitespace" "read-sequence" "readtable-case"
    "readtablep" "realp" "realpart" "reduce" "reinitialize-instance" "rem"
    "remf" "remhash" "remove" "remove-duplicates" "remove-if" "remove-if-not"
    "remove-method" "remprop" "rename-file" "rename-package" "replace"
    "require" "rest" "restart-name" "revappend" "reverse" "room" "rotatef"
    "round" "row-major-aref" "rplaca" "rplacd" "sbit" "scale-float" "schar"
    "search" "second" "set" "set-difference" "set-dispatch-macro-character"
    "set-exclusive-or" "setf" "set-macro-character" "set-pprint-dispatch"
    "setq" "set-syntax-from-char" "seventh" "shadow" "shadowing-import"
    "shared-initialize" "shiftf" "short-site-name" "signum"
    "simple-bit-vector-p" "simple-condition-format-arguments"
    "simple-condition-format-control" "simple-string-p" "simple-vector-p" "sin"
    "sinh" "sixth" "sleep" "slot-boundp" "slot-exists-p" "slot-makunbound"
    "slot-missing" "slot-unbound" "slot-value" "software-type"
    "software-version" "some" "sort" "special-operator-p" "sqrt" "stable-sort"
    "standard-char-p" "step" "store-value" "stream-element-type"
    "stream-error-stream" "stream-external-format" "streamp" "string"
    "string/=" "string<" "string<=" "string=" "string>" "string>="
    "string-capitalize" "string-downcase" "string-equal" "string-greaterp"
    "string-left-trim" "string-lessp" "string-not-equal" "string-not-greaterp"
    "string-not-lessp" "stringp" "string-right-trim" "string-trim"
    "string-upcase" "sublis" "subseq" "subsetp" "subst" "subst-if"
    "subst-if-not" "substitute" "substitute-if" "substitute-if-not" "subtypep"
    "svref" "sxhash" "symbol-function" "symbol-name" "symbolp" "symbol-package"
    "symbol-plist" "symbol-value" "synonym-stream-symbol" "tailp" "tan" "tanh"
    "tenth" "terpri" "third" "throw" "time" "trace"
    "translate-logical-pathname" "translate-pathname" "tree-equal" "truename"
    "truncate" "two-way-stream-input-stream" "two-way-stream-output-stream"
    "type-error-datum" "type-error-expected-type" "type-of" "typep"
    "unbound-slot-instance" "unexport" "unintern" "union" "unread-char"
    "untrace" "unuse-package" "update-instance-for-different-class"
    "update-instance-for-redefined-class" "upgraded-array-element-type"
    "upgraded-complex-part-type" "upper-case-p" "use-package"
    "user-homedir-pathname" "use-value" "values" "values-list" "vector"
    "vectorp" "vector-pop" "vector-push" "vector-push-extend" "wild-pathname-p"
    "write" "write-byte" "write-char" "write-line" "write-sequence"
    "write-string" "write-to-string" "yes-or-no-p" "y-or-n-p" "zerop"))

(defvar cl-font-lock-built-in--variables
  '("//" "///" "\\*load-pathname\\*" "\\*print-pprint-dispatch\\*"
    "\\*break-on-signals\\*" "\\*load-print\\*" "\\*print-pprint-dispatch\\*"
    "\\*break-on-signals\\*" "\\*load-truename\\*" "\\*print-pretty\\*"
    "\\*load-verbose\\*" "\\*print-radix\\*" "\\*compile-file-pathname\\*"
    "\\*macroexpand-hook\\*" "\\*print-readably\\*"
    "\\*compile-file-pathname\\*" "\\*modules\\*" "\\*print-right-margin\\*"
    "\\*compile-file-truename\\*" "\\*package\\*" "\\*print-right-margin\\*"
    "\\*compile-file-truename\\*" "\\*print-array\\*" "\\*query-io\\*"
    "\\*compile-print\\*" "\\*print-base\\*" "\\*random-state\\*"
    "\\*compile-verbose\\*" "\\*default-pathname-defaults\\*"
    "\\*print-length\\*" "\\*readtable\\*" "\\*error-output\\*"
    "\\*print-level\\*" "\\*standard-input\\*" "\\*print-case\\*"
    "\\*read-base\\*" "\\*compile-verbose\\*" "\\*print-circle\\*"
    "\\*print-lines\\*" "\\*standard-output\\*" "\\*features\\*"
    "\\*print-miser-width\\*" "\\*read-default-float-format\\*"
    "\\*debug-io\\*" "\\*print-escape\\*" "\\*read-eval\\*"
    "\\*debugger-hook\\*" "\\*print-gensym\\*" "\\*read-suppress\\*"
    "\\*terminal-io\\*" "\\*gensym-counter\\*" "\\*print-miser-width\\*"
    "\\*trace-output\\*" "array-dimension-limit" "array-rank-limit"
    "array-total-size-limit" "boole-1" "boole-2" "boole-and" "boole-andc1"
    "boole-andc2" "boole-c1" "boole-c2" "boole-clr" "boole-eqv" "boole-ior"
    "boole-nand" "boole-nor" "boole-orc1" "boole-orc2" "boole-set" "boole-xor"
    "call-arguments-limit" "char-code-limit" "double-float-epsilon"
    "double-float-negative-epsilon" "internal-time-units-per-second"
    "lambda-list-keywords" "lambda-parameters-limit"
    "least-negative-double-float" "least-negative-long-float"
    "least-negative-normalized-double-float"
    "least-negative-normalized-long-float"
    "least-negative-normalized-short-float"
    "least-negative-normalized-single-float" "least-negative-short-float"
    "least-negative-single-float" "least-positive-double-float"
    "least-positive-long-float" "least-positive-normalized-double-float"
    "least-positive-normalized-long-float"
    "least-positive-normalized-short-float"
    "least-positive-normalized-single-float" "least-positive-short-float"
    "least-positive-single-float" "long-float-epsilon"
    "long-float-negative-epsilon" "most-negative-double-float"
    "most-negative-fixnum" "most-negative-long-float"
    "most-negative-short-float" "most-negative-single-float"
    "most-positive-double-float" "most-positive-fixnum"
    "most-positive-long-float" "most-positive-short-float"
    "most-positive-single-float" "multiple-values-limit" "short-float-epsilon"
    "short-float-negative-epsilon" "single-float-epsilon"
    "single-float-negative-epsilon" "pi"))

(defvar cl-font-lock-built-in--types
  '("arithmetic-error" "array" "base-char" "base-string" "bignum" "bit-vector"
    "boolean" "broadcast-stream" "built-in-class" "cell-error" "class"
    "compiled-function" "concatenated-stream" "condition" "control-error"
    "division-by-zero" "double-float" "echo-stream" "end-of-file"
    "extended-char" "file-error" "file-stream" "fixnum"
    "floating-point-inexact" "floating-point-invalid-operation"
    "floating-point-overflow" "floating-point-underflow" "generic-function"
    "hash-table" "integer" "keyword" "long-float" "method" "method-combination"
    "number" "package" "package-error" "parse-error" "print-not-readable"
    "program-error" "random-state" "ratio" "reader-error" "readtable" "real"
    "restart" "sequence" "serious-condition" "short-float" "signed-byte"
    "simple-array" "simple-base-string" "simple-bit-vector" "simple-condition"
    "simple-error" "simple-string" "simple-type-error" "simple-vector"
    "simple-warning" "single-float" "standard-char" "standard-class"
    "standard-generic-function" "standard-method" "standard-object"
    "storage-condition" "stream" "stream-error" "string-stream"
    "structure-class" "structure-object" "style-warning" "symbol"
    "synonym-stream" "two-way-stream" "type-error" "unbound-slot"
    "unbound-variable" "undefined-function" "unsigned-byte" "warning"))

(defvar cl-font-lock-built-in--symbols
  '("compilation-speed" "compiler-macro" "debug" "declaration" "dynamic-extent"
    "ftype" "ignorable" "ignore" "inline" "notinline" "optimize" "otherwise"
    "safety" "satisfies" "space" "special" "speed" "structure" "type"))

(defvar cl-font-lock--character-names
  '("newline" "space" "rubout" "page" "tab" "backspace" "return" "linefeed"))

(defvar cl-font-lock-built-in-keywords
  (mapcar (lambda (s)
            `(,(regexp-opt (symbol-value (car s)) 'symbols)
              . ,(cdr s)))
          '((cl-font-lock-built-in--functions . font-lock-function-name-face)
            (cl-font-lock-built-in--variables . font-lock-variable-name-face)
            (cl-font-lock-built-in--types . font-lock-type-face)
            (cl-font-lock-built-in--symbols . font-lock-builtin-face)
            (cl-font-lock--character-names . font-lock-variable-name-face))))

;;;###autoload
(define-minor-mode cl-font-lock-built-in-mode
  "Highlight built-in functions, variables, and types in `lisp-mode'."
  :global t
  :group 'tools
  (funcall
   (if cl-font-lock-built-in-mode
       #'font-lock-add-keywords
     #'font-lock-remove-keywords)
   'lisp-mode
   cl-font-lock-built-in-keywords))

(provide 'cl-font-lock)

;;; cl-font-lock.el ends here
