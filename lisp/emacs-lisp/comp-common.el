;;; comp-common.el --- common code -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

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
  '(;; The following two are mandatory for Emacs to be working
    ;; correctly (see comment in `advice--add-function'). DO NOT
    ;; REMOVE.
    macroexpand rename-buffer)
  "Primitive functions to exclude from trampoline optimization.

Primitive functions included in this list will not be called
directly by the natively-compiled code, which makes trampolines for
those primitives unnecessary in case of function redefinition/advice."
  :type '(repeat symbol)
  :version "28.1")

(defcustom native-comp-async-env-modifier-form nil
  "Form evaluated before compilation by each asynchronous compilation subprocess.
Used to modify the compiler environment."
  :type 'sexp
  :risky t
  :version "28.1")

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

(provide 'comp-common)

;;; comp-common.el ends here
