;;; flymake-cc.el --- Flymake support for GNU tools for C/C++     -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022 Free Software Foundation, Inc.

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: languages, c

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

;; Flymake support for C/C++.

;;; Code:

(require 'cl-lib)

(defcustom flymake-cc-command 'flymake-cc-use-special-make-target
  "Command used by the `flymake-cc' backend.
A list of strings, or a symbol naming a function that produces one
such list when called with no arguments in the buffer where the
variable `flymake-mode' is active.

The command should invoke a GNU-style compiler that checks the
syntax of a (Obj)C(++) program passed to it via its standard
input and prints the result on its standard output."
  :type '(choice
          (symbol :tag "Function")
          (repeat :tag "Command(s)" string))
  :version "27.1"
  :group 'flymake-cc)

(defun flymake-cc--make-diagnostics (source)
  "Parse GNU-compatible compilation messages in current buffer.
Return a list of Flymake diagnostic objects for the source buffer
SOURCE."
  ;; TODO: if you can understand it, use `compilation-mode's regexps
  ;; or even some of its machinery here.
  ;;
  ;;    (setq-local compilation-locs
  ;;         (make-hash-table :test 'equal :weakness 'value))
  ;;    (compilation-parse-errors (point-min) (point-max)
  ;;                              'gnu 'gcc-include)
  ;;    (while (next-single-property-change 'compilation-message)
  ;;       ...)
  ;;
  ;; For now, this works minimally well.
  (cl-loop
   while
   (search-forward-regexp
    "^\\(In file included from \\)?\\([^ :]+\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?:\n?\\(.*\\): \\(.*\\)$"
    nil t)
   for msg = (match-string 6)
   for locus = (match-string 2)
   for line = (string-to-number (match-string 3))
   for col = (ignore-errors (string-to-number (match-string 4)))
   for source-buffer = (and (string= locus "<stdin>") source)
   for type = (if (match-string 1)
                  :error
                (save-match-data
                  (assoc-default
                   (match-string 5)
                   '(("error" . :error)
                     ("note" . :note)
                     ("warning" . :warning))
                   #'string-match
                   :error)))
   for diag =
   (cond (source-buffer
          (pcase-let ((`(,beg . ,end)
                       (flymake-diag-region source-buffer line col)))
            (flymake-make-diagnostic source-buffer beg end type msg)))
         (t (flymake-make-diagnostic locus (cons line col) nil type msg)))
   collect diag
   ;; If "In file included from..." matched, then move to end of that
   ;; line.  This helps us collect the diagnostic at its .h locus,
   ;; too.
   when (match-end 1) do (goto-char (match-end 2))))

(defun flymake-cc-use-special-make-target ()
  "Command for checking a file via a CHK_SOURCES Make target."
  (unless (executable-find "make") (error "Make not found"))
  `("make"
    "check-syntax"
    ,(format "CHK_SOURCES=-x %s -c -"
             (cond ((derived-mode-p 'c++-mode) "c++")
                   (t "c")))))

(defvar-local flymake-cc--proc nil "Internal variable for `flymake-cc'.")

;; forward declare this to shoosh compiler (instead of requiring
;; flymake-proc)
;;
(defvar flymake-proc-allowed-file-name-masks)

;;;###autoload
(defun flymake-cc (report-fn &rest _args)
  "Flymake backend for GNU-style C compilers.
This backend uses `flymake-cc-command' (which see) to launch a
process that is passed the current buffer's contents via stdin.
REPORT-FN is Flymake's callback."
  ;; HACK: XXX: Assuming this backend function is run before it in
  ;; `flymake-diagnostic-functions', very hackingly convince the other
  ;; backend `flymake-proc-legacy-backend', which is on by default, to
  ;; disable itself.
  ;;
  (setq-local flymake-proc-allowed-file-name-masks nil)
  (when (process-live-p flymake-cc--proc)
    (kill-process flymake-cc--proc))
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq
       flymake-cc--proc
       (make-process
        :name "gcc-flymake"
        :buffer (generate-new-buffer "*gcc-flymake*")
        :command (if (symbolp flymake-cc-command)
                     (funcall flymake-cc-command)
                   flymake-cc-command)
        :noquery t :connection-type 'pipe
        :sentinel
        (lambda (p _ev)
          (unwind-protect
              (when (eq 'exit (process-status p))
                (when (with-current-buffer source (eq p flymake-cc--proc))
                  (with-current-buffer (process-buffer p)
                    (goto-char (point-min))
                    (let ((diags
                           (flymake-cc--make-diagnostics source)))
                      (if (or diags (zerop (process-exit-status p)))
                          (funcall report-fn diags)
                        ;; non-zero exit with no diags is cause
                        ;; for alarm
                        (funcall report-fn
                                 :panic :explanation
                                 (buffer-substring
                                  (point-min) (progn (goto-char (point-min))
                                                     (line-end-position)))))))))
            (unless (process-live-p p)
              ;; (display-buffer (process-buffer p)) ; uncomment to debug
              (kill-buffer (process-buffer p)))))))
      (process-send-region flymake-cc--proc (point-min) (point-max))
      (process-send-eof flymake-cc--proc))))

(provide 'flymake-cc)
;;; flymake-cc.el ends here
