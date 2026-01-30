;;; comp-common.el --- common code -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

;; Author: Andrea Corallo <acorallo@gnu.org>
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

;; This file holds common code required by comp.el and comp-run.el.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; These variables and functions are defined in comp.c
(defvar comp-native-version-dir)
(defvar native-comp-eln-load-path)

(defgroup comp-common nil
  "Emacs Lisp native compiler common code."
  :group 'lisp)

(defcustom native-comp-verbose 0
  "Compiler verbosity for native compilation, a number between 0 and 3.
This is intended for debugging the compiler itself.
  0 no logging.
  1 final LIMPLE is logged.
  2 LAP, final LIMPLE, and some pass info are logged.
  3 max verbosity."
  :type 'natnum
  :risky t
  :version "28.1")

(defcustom native-comp-never-optimize-functions
  ;; We used to list those functions here that were advised during
  ;; preload, but we now prefer to disallow preload advices in
  ;; loadup.el (bug#67005).
  '(eval)
  "Primitive functions to exclude from trampoline optimization.

Primitive functions included in this list will not be called
directly by the native-compiled code, which makes trampolines for
those primitives unnecessary in case of function redefinition/advice."
  :type '(repeat symbol)
  :version "30.1")

(defcustom native-comp-async-env-modifier-form nil
  "Form evaluated before compilation by each asynchronous compilation subprocess.
Used to modify the compiler environment."
  :type 'sexp
  :risky t
  :version "28.1")

(defconst comp-primitive-type-specifiers
  `(
    ;; Functions we can trust not to be redefined, or, if redefined,
    ;; to expose the same type.  The vast majority of these are
    ;; either pure or primitive; the original list is the union of
    ;; pure + side-effect-free-fns + side-effect-and-error-free-fns:
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
    (bobp (function () boolean))
    (bolp (function () boolean))
    (bool-vector-count-consecutive
     (function (bool-vector boolean integer) fixnum))
    (bool-vector-count-population (function (bool-vector) fixnum))
    (bool-vector-not (function (bool-vector &optional bool-vector) bool-vector))
    (bool-vector-p (function (t) boolean))
    (bool-vector-subsetp (function (bool-vector bool-vector) boolean))
    (boundp (function (symbol) boolean))
    (buffer-file-name (function (&optional buffer) (or string null)))
    (buffer-list (function (&optional frame) list))
    (buffer-local-variables (function (&optional buffer) list))
    (buffer-modified-p
     (function (&optional buffer) (or boolean (member autosaved))))
    (buffer-size (function (&optional buffer) integer))
    (buffer-string (function () string))
    (buffer-substring
     (function ((or integer marker) (or integer marker)) string))
    (bufferp (function (t) boolean))
    (closurep (function (t) boolean))
    (byte-code-function-p (function (t) boolean))
    (interpreted-function-p (function (t) boolean))
    (capitalize (function ((or integer string)) (or integer string)))
    (car (function (list) t))
    (car-less-than-car (function (list list) boolean))
    (car-safe (function (t) t))
    (case-table-p (function (t) boolean))
    (cdr (function (list) t))
    (cdr-safe (function (t) t))
    (ceiling (function (number &optional number) integer))
    (char-after (function (&optional (or marker integer)) (or fixnum null)))
    (char-before (function (&optional (or marker integer)) (or fixnum null)))
    (char-equal (function (integer integer) boolean))
    (char-or-string-p (function (t) boolean))
    (char-to-string (function (fixnum) string))
    (char-width (function (fixnum) fixnum))
    (characterp (function (t &optional t) boolean))
    (charsetp (function (t) boolean))
    (commandp (function (t &optional t) boolean))
    (compare-strings
     (function (string (or integer marker null) (or integer marker null) string
                       (or integer marker null) (or integer marker null)
                       &optional t)
               (or (member t) fixnum)))
    (concat (function (&rest sequence) string))
    (cons (function (t t) cons))
    (consp (function (t) boolean))
    (coordinates-in-window-p
     (function (cons window)
               (or cons null
                   (member bottom-divider right-divider mode-line header-line
                           tab-line left-fringe right-fringe vertical-line
                           left-margin right-margin))))
    (copy-alist (function (list) list))
    (copy-marker (function (&optional (or integer marker) boolean) marker))
    (copy-sequence (function (sequence) sequence))
    (copysign (function (float float) float))
    (cos (function (number) float))
    (current-buffer (function () buffer))
    (current-global-map (function () cons))
    (current-indentation (function () integer))
    (current-local-map (function () (or cons null)))
    (current-minor-mode-maps (function () (or cons null)))
    (current-time (function () cons))
    (current-time-string (function (&optional (or number list)
                                              (or symbol string cons integer))
                                   string))
    (current-time-zone (function (&optional (or number list)
                                            (or symbol string cons integer))
                                 cons))
    (decode-char (function (cons t) (or fixnum null)))
    (decode-time (function (&optional (or number list)
                                      (or symbol string cons integer)
                                      symbol)
                           cons))
    (default-boundp (function (symbol) boolean))
    (default-value (function (symbol) t))
    (documentation
     (function ((or function symbol subr) &optional t) (or null string)))
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
    (exp (function (number) float))
    (expt (function (number number) number))
    (fboundp (function (symbol) boolean))
    (fceiling (function (float) float))
    (featurep (function (symbol &optional symbol) boolean))
    (ffloor (function (float) float))
    (file-directory-p (function (string) boolean))
    (file-exists-p (function (string) boolean))
    (file-locked-p (function (string) (or boolean string)))
    (file-name-absolute-p (function (string) boolean))
    (file-newer-than-file-p (function (string string) boolean))
    (file-readable-p (function (string) boolean))
    (file-symlink-p (function (string) (or boolean string)))
    (file-writable-p (function (string) boolean))
    (float (function (number) float))
    (float-time (function (&optional (or number list)) float))
    (floatp (function (t) boolean))
    (floor (function (number &optional number) integer))
    (following-char (function () fixnum))
    (format (function (string &rest t) string))
    (format-time-string (function (string &optional (or number list)
                                          (or symbol string cons integer))
                                  string))
    (frame-first-window (function ((or frame window)) window))
    (frame-root-window (function (&optional (or frame window)) window))
    (frame-selected-window (function (&optional (or frame window)) window))
    (frame-visible-p (function (frame) (or boolean (member icon))))
    (framep (function (t) symbol))
    (fround (function (float) float))
    (ftruncate (function (float) float))
    (get (function (symbol symbol) t))
    (get-buffer (function ((or buffer string)) (or buffer null)))
    (get-buffer-window
     (function (&optional (or buffer string) (or symbol (integer 0 0)))
               (or null window)))
    (get-file-buffer (function (string) (or null buffer)))
    (gethash (function (t hash-table &optional t) t))
    (hash-table-count (function (hash-table) integer))
    (hash-table-p (function (t) boolean))
    (identity (function (t) t))
    (integer-or-marker-p (function (t) boolean))
    (integerp (function (t) boolean))
    (intern-soft (function ((or string symbol) &optional (or obarray vector))
                           symbol))
    (invocation-directory (function () string))
    (invocation-name (function () string))
    (isnan (function (float) boolean))
    (keymap-parent (function (cons) (or cons null)))
    (keymapp (function (t) boolean))
    (keywordp (function (t) boolean))
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
    (logand (function (&rest (or integer marker)) integer))
    (logb (function (number) integer))
    (logcount (function (integer) integer))
    (logior (function (&rest (or integer marker)) integer))
    (lognot (function (integer) integer))
    (logxor (function (&rest (or integer marker)) integer))
    ;; (lsh (function ((integer ,most-negative-fixnum *) integer) integer)) ?
    (make-byte-code
     (function ((or fixnum list) string vector integer &optional string t
                &rest t)
               vector))
    (make-list (function (integer t) list))
    (make-marker (function () marker))
    (make-string (function (integer fixnum &optional t) string))
    (make-symbol (function (string) symbol))
    (mark-marker (function () marker))
    (marker-buffer (function (marker) (or buffer null)))
    (markerp (function (t) boolean))
    (max (function ((or number marker) &rest (or number marker)) number))
    (max-char (function (&optional t) fixnum))
    (member (function (t list) list))
    (memq (function (t list) list))
    (memql (function (t list) list))
    (message (function ((or string null) &rest t) (or string null)))
    (min (function ((or number marker) &rest (or number marker)) number))
    (minibuffer-selected-window (function () (or window null)))
    (minibuffer-window (function (&optional frame) window))
    (mod
     (function ((or number marker) (or number marker))
               (or (integer 0 *) (float 0 *))))
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
    (overlayp (function (t) boolean))
    (plist-get (function (list t &optional t) t))
    (plist-member (function (list t &optional t) list))
    (point (function () integer))
    (point-marker (function () marker))
    (point-max (function () integer))
    (point-min (function () integer))
    (preceding-char (function () fixnum))
    (previous-window (function (&optional window t t) window))
    (prin1-to-string (function (t &optional t t) string))
    (processp (function (t) boolean))
    (proper-list-p (function (t) (or fixnum null)))
    (propertize (function (string &rest t) string))
    (rassoc (function (t list) list))
    (rassq (function (t list) list))
    (read-from-string (function (string &optional integer integer) cons))
    (recent-keys (function (&optional (or cons null)) vector))
    (recursion-depth (function () integer))
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
    (string-to-syntax (function (string) (or cons null)))
    (string< (function ((or string symbol) (or string symbol)) boolean))
    (string= (function ((or string symbol) (or string symbol)) boolean))
    (stringp (function (t) boolean))
    (subrp (function (t) boolean))
    (substring
     (function ((or string vector) &optional integer integer) (or string vector)))
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
    (time-convert (function ((or number list) &optional (or symbol integer))
                            (or cons number)))
    (truncate (function (number &optional number) integer))
    (type-of (function (t) symbol))
    (unibyte-char-to-multibyte (function (fixnum) fixnum)) ;; byte is fixnum
    (upcase (function ((or fixnum string)) (or fixnum string)))
    (user-full-name (function (&optional integer) (or string null)))
    (user-login-name (function (&optional integer) (or string null)))
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
    ;; Non returning functions
    (throw (function (t t) nil))
    (signal (function (symbol t) nil)))
  "Alist used for type propagation.")

(defconst comp-limple-calls '(call
                              callref
                              direct-call
                              direct-callref)
  "Limple operators used to call subrs.")

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

(defconst comp-limple-branches '(jump cond-jump)
  "Limple operators used for conditional and unconditional branches.")

(defconst comp-limple-ops `(,@comp-limple-calls
                            ,@comp-limple-assignments
                            ,@comp-limple-branches
                            return)
  "All Limple operators.")

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

(defconst comp-log-buffer-name "*Native-compile-Log*"
  "Name of the native-compiler log buffer.")

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

(define-derived-mode native-comp-limple-mode fundamental-mode "LIMPLE"
  "Syntax-highlight LIMPLE IR."
  (setf font-lock-defaults '(comp-limple-lock-keywords)))

(cl-defun comp-log-to-buffer (data &optional quoted)
  "Log DATA to `comp-log-buffer-name'."
  (let* ((print-f (if quoted #'prin1 #'princ))
         (log-buffer
          (or (get-buffer comp-log-buffer-name)
              (with-current-buffer (get-buffer-create comp-log-buffer-name)
                (unless (derived-mode-p 'compilation-mode)
                  (emacs-lisp-compilation-mode))
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

(defun comp-ensure-native-compiler ()
  "Make sure Emacs has native compiler support and libgccjit can be loaded.
Signal an error otherwise.
To be used by all entry points."
  (cond
   ((null (featurep 'native-compile))
    (error "Emacs was not compiled with native compiler support (--with-native-compilation)"))
   ((null (native-comp-available-p))
    (error "Cannot find libgccjit library"))))

(defun comp-trampoline-filename (subr-name)
  "Given SUBR-NAME return the filename containing the trampoline."
  (concat (comp-c-func-name subr-name "subr--trampoline-" t) ".eln"))

(defun comp-eln-load-path-eff ()
  "Return a list of effective eln load directories.
Account for `native-comp-eln-load-path' and `comp-native-version-dir'."
  (mapcar (lambda (dir)
            (expand-file-name comp-native-version-dir
                              (file-name-as-directory
                               (expand-file-name dir invocation-directory))))
          native-comp-eln-load-path))

;;;###autoload
(defun comp-function-type-spec (function)
  "Return the type specifier of FUNCTION.

This function returns a cons cell whose car is the function specifier,
and cdr is a symbol, either `inferred' or `declared'.  If the symbol is
`inferred', the type specifier is automatically inferred from the code
itself by the native compiler; if it is `declared', the type specifier
comes from `comp-primitive-type-specifiers' or the function type declaration
itself."
  (let ((kind 'declared)
        type-spec)
    (when-let* ((res (assoc function comp-primitive-type-specifiers)))
      ;; Declared primitive
      (setf type-spec (cadr res)))
    (let ((f (and (symbolp function)
                  (symbol-function function))))
      (when (and f (null type-spec))
        (if-let* ((delc-type (function-get function 'function-type)))
            ;; Declared Lisp function
            (setf type-spec delc-type)
          (when (native-comp-function-p f)
            ;; Natively compiled inferred
            (setf kind 'inferred
                  type-spec (subr-type f))))))
    (when type-spec
        (cons type-spec kind))))

(provide 'comp-common)

;;; comp-common.el ends here
