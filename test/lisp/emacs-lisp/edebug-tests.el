;;; edebug-tests.el --- Edebug test suite   -*- lexical-binding:t -*-

;; Copyright (C) 2017-2020 Free Software Foundation, Inc.

;; Author: Gemini Lasswell

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; These tests focus on Edebug's user interface for setting
;; breakpoints, stepping through and tracing code, and evaluating
;; values used by the code.  In addition there are some tests of
;; Edebug's reader.  There are large parts of Edebug's functionality
;; not covered by these tests, including coverage testing, macro
;; specifications, and the eval list buffer.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'ert-x)
(require 'edebug)
(require 'kmacro)

;; Use `eval-and-compile' because this is used by the macro
;; `edebug-tests-deftest'.
(eval-and-compile
  (defvar edebug-tests-sample-code-file
    (expand-file-name
     "edebug-resources/edebug-test-code.el"
     (file-name-directory (or (bound-and-true-p byte-compile-current-file)
                              load-file-name
                              buffer-file-name)))
    "Name of file containing code samples for Edebug tests."))

(defvar edebug-tests-temp-file nil
  "Name of temp file containing sample code stripped of stop point symbols.")
(defvar edebug-tests-stop-points nil
  "An alist of alists mapping function symbol -> stop point name -> marker.
Used by the tests to refer to locations in `edebug-tests-temp-file'.")
(defvar edebug-tests-messages nil
  "Messages collected during execution of the current test.")

(defvar edebug-tests-@-result 'no-result
  "Return value of `edebug-tests-func', or no-result if there isn't one yet.")

(defvar edebug-tests-failure-in-post-command nil
  "An error trapped in `edebug-tests-post-command'.
Since `should' failures which happen inside `post-command-hook' will
be trapped by the command loop, this preserves them until we get
back to the top level.")

(defvar edebug-tests-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "@"     'edebug-tests-call-instrumented-func)
    (define-key map "C-u"   'universal-argument)
    (define-key map "C-p"   'previous-line)
    (define-key map "C-n"   'next-line)
    (define-key map "C-b"   'backward-char)
    (define-key map "C-a"   'move-beginning-of-line)
    (define-key map "C-e"   'move-end-of-line)
    (define-key map "C-k"   'kill-line)
    (define-key map "M-x"   'execute-extended-command)
    (define-key map "C-M-x" 'eval-defun)
    (define-key map "C-x X b" 'edebug-set-breakpoint)
    (define-key map "C-x X w" 'edebug-where)
    map)
  "Keys used by the keyboard macros in Edebug's tests.")

;;; Macros for defining tests:

(defmacro edebug-tests-with-default-config (&rest body)
  "Create a consistent environment for an Edebug test BODY to run in."
  (declare (debug (body)))
  `(cl-letf* (
              ;; These defcustoms are set to their original value.
              (edebug-setup-hook nil)
              (edebug-all-defs nil)
              (edebug-all-forms nil)
              (edebug-eval-macro-args nil)
              (edebug-save-windows t)
              (edebug-save-displayed-buffer-points nil)
              (edebug-initial-mode 'step)
              (edebug-trace nil)
              (edebug-test-coverage nil)
              (edebug-print-length 50)
              (edebug-print-level 50)
              (edebug-print-circle t)
              (edebug-unwrap-results nil)
              (edebug-on-error t)
              (edebug-on-quit t)
              (edebug-global-break-condition nil)
              (edebug-sit-for-seconds 1)

              ;; sit-on interferes with keyboard macros.
              (edebug-sit-on-break nil)
              (edebug-continue-kbd-macro t))
     ,@body))

(defmacro edebug-tests-with-normal-env (&rest body)
  "Set up the environment for an Edebug test BODY, run it, and clean up."
  (declare (debug (body)))
  `(edebug-tests-with-default-config
    (let ((edebug-tests-failure-in-post-command nil)
          (edebug-tests-temp-file (make-temp-file "edebug-tests-" nil ".el")))
      (edebug-tests-setup-code-file edebug-tests-temp-file)
      (ert-with-message-capture
       edebug-tests-messages
       (unwind-protect
           (with-current-buffer (find-file edebug-tests-temp-file)
             (read-only-mode)
             (setq lexical-binding t)
             (eval-buffer)
             ,@body
             (when edebug-tests-failure-in-post-command
               (signal (car edebug-tests-failure-in-post-command)
                       (cdr edebug-tests-failure-in-post-command))))
         (unload-feature 'edebug-test-code)
         (with-current-buffer (find-file-noselect edebug-tests-temp-file)
           (set-buffer-modified-p nil))
         (ignore-errors (kill-buffer (find-file-noselect
                                      edebug-tests-temp-file)))
         (ignore-errors (delete-file edebug-tests-temp-file)))))))

;; The following macro and its support functions implement an extension
;; to keyboard macros to allow interleaving of keyboard macro
;; events with evaluation of Lisp expressions. The Lisp expressions
;; are called from within `post-command-hook', which is a strategy
;; inspired by `kmacro-step-edit-macro'.

;; Some of the details necessary to get this to work with Edebug are:
;; -- ERT's `should' macros raise errors, and errors within
;;    `post-command-hook' are trapped by the command loop. The
;;    workaround is to trap and save an error inside the hook
;;    function and reraise it after the macro exits.
;; -- `edebug-continue-kbd-macro' must be non-nil.
;; -- Edebug calls `exit-recursive-edit' which turns off keyboard
;;    macro execution. Solved with an advice wrapper for
;;    `exit-recursive-edit' which preserves the keyboard macro state.

(defmacro edebug-tests-run-kbd-macro (&rest macro)
  "Run a MACRO consisting of both keystrokes and test assertions.
MACRO should be a list, where each item is either a keyboard
macro segment (in string or vector form) or a Lisp expression.
Convert the macro segments into keyboard macros and execute them.
After the execution of the last event of each segment, evaluate
the Lisp expressions following the segment."
  (let ((prepared (edebug-tests-prepare-macro macro)))
    `(edebug-tests-run-macro ,@prepared)))

;; Make support functions for edebug-tests-run-kbd-macro
;; available at compile time.
(eval-and-compile
  (defun edebug-tests-prepare-macro (macro)
    "Prepare a MACRO for execution.
MACRO should be a list containing strings, vectors, and Lisp
forms. Convert the strings and vectors to keyboard macros in
vector representation and concatenate them to make a single
keyboard macro.  Also build a list of the same length as the
number of events in the keyboard macro.  Each item in that list
will contain the code to evaluate after the corresponding event
in the keyboard macro, either nil or a thunk built from the forms
in the original list.  Return a list containing the keyboard
macro as the first item, followed by the list of thunks and/or
nils."
    (cl-loop
     for item = (pop macro)
     while item
     for segment = (read-kbd-macro item)
     for thunk = (edebug-tests-wrap-thunk
                  (cl-loop
                   for form in macro
                   until (or (stringp form) (vectorp form))
                   collect form
                   do (pop macro)))
     vconcat segment into segments
     append (edebug-tests-pad-thunk-list (length segment) thunk)
      into thunk-list

     finally return (cons segments thunk-list)))

  (defun edebug-tests-wrap-thunk (body)
    "If BODY is non-nil, wrap it with a lambda form."
    (when body
      `(lambda () ,@body)))

  (defun edebug-tests-pad-thunk-list (length thunk)
    "Return a list with LENGTH elements with THUNK in the last position.
All other elements will be nil."
    (let ((thunk-seg (make-list length nil)))
      (setf (car (last thunk-seg)) thunk)
      thunk-seg)))

;;; Support for test execution:

(defvar edebug-tests-thunks nil
  "List containing thunks to run after each command in a keyboard macro.")
(defvar edebug-tests-kbd-macro-index nil
  "Index into `edebug-tests-run-unpacked-kbd-macro's current keyboard macro.")

(defun edebug-tests-run-macro (kbdmac &rest thunks)
  "Run a keyboard macro and execute a thunk after each command in it.
KBDMAC should be a vector of events and THUNKS a list of the
same length containing thunks and/or nils.  Run the macro, and
after the execution of every command in the macro (which may not
be the same as every keystroke) execute the thunk at the same
index."
  (let* ((edebug-tests-thunks thunks)
         (edebug-tests-kbd-macro-index 0)
         saved-local-map)
    (with-current-buffer (find-file-noselect edebug-tests-temp-file)
      (setq saved-local-map overriding-local-map)
      (setq overriding-local-map edebug-tests-keymap)
      (add-hook 'post-command-hook 'edebug-tests-post-command))
    (advice-add 'exit-recursive-edit
                :around 'edebug-tests-preserve-keyboard-macro-state)
    (unwind-protect
        (kmacro-call-macro nil nil nil kbdmac)
      (advice-remove 'exit-recursive-edit
                     'edebug-tests-preserve-keyboard-macro-state)
      (with-current-buffer (find-file-noselect edebug-tests-temp-file)
        (setq overriding-local-map saved-local-map)
        (remove-hook 'post-command-hook 'edebug-tests-post-command)))))

(defun edebug-tests-preserve-keyboard-macro-state (orig &rest args)
  "Call ORIG with ARGS preserving the value of `executing-kbd-macro'.
Useful to prevent `exit-recursive-edit' from stopping the current
keyboard macro."
  (let ((executing-kbd-macro executing-kbd-macro))
    (apply orig args)))

(defun edebug-tests-post-command ()
  "Run the thunk from `edebug-tests-thunks' matching the keyboard macro index."
  (when (and edebug-tests-kbd-macro-index
             (> executing-kbd-macro-index edebug-tests-kbd-macro-index))
    (let ((thunk (nth (1- executing-kbd-macro-index) edebug-tests-thunks)))
      (when thunk
        (condition-case err
            (funcall thunk)
          (error
           (setq edebug-tests-failure-in-post-command err)
           (signal (car err) (cdr err)))))
      (setq edebug-tests-kbd-macro-index executing-kbd-macro-index))))

(defvar edebug-tests-func nil
  "Instrumented function used to launch Edebug.")
(defvar edebug-tests-args nil
  "Arguments for `edebug-tests-func'.")

(defun edebug-tests-setup-@ (def-name args edebug-it)
  "Set up the binding for @ in `edebug-tests-keymap'.
Find a definition for DEF-NAME in the current buffer and evaluate it.
Set globals so that `edebug-tests-call-instrumented-func' which
is bound to @ for edebug-tests' keyboard macros will call it with
ARGS.  EDEBUG-IT is passed through to `eval-defun'."
  (edebug-tests-locate-def def-name)
  (eval-defun edebug-it)
  (let* ((full-name (concat "edebug-test-code-" def-name))
         (sym (intern-soft full-name)))
    (should (and sym (fboundp sym)))
    (setq edebug-tests-func sym
          edebug-tests-args args)
    (setq edebug-tests-@-result 'no-result)))

(defun edebug-tests-call-instrumented-func ()
  "Call `edebug-tests-func' with `edebug-tests-args' and save the results."
  (interactive)
  (let ((result (apply edebug-tests-func edebug-tests-args)))
    (should (eq edebug-tests-@-result 'no-result))
    (setq edebug-tests-@-result result)))

(defun edebug-tests-should-be-at (def-name point-name)
  "Require that point be at the location in DEF-NAME named POINT-NAME.
DEF-NAME should be the suffix of a definition in the code samples
file (the part after \"edebug-tests\")."
  (let ((stop-point (edebug-tests-get-stop-point def-name point-name)))
    (should (eq (current-buffer) (find-file-noselect edebug-tests-temp-file)))
    (should (eql (point) stop-point))))

(defun edebug-tests-get-stop-point (def-name point-name)
  "Return the position in DEF-NAME of the stop point named POINT-NAME.
DEF-NAME should be the suffix of a definition in the code samples
file (the part after \"edebug-tests\")."
  (let* ((full-name (concat "edebug-test-code-" def-name))(stop-point
         (cdr (assoc point-name
                     (cdr (assoc full-name edebug-tests-stop-points))))))
    (unless stop-point
      (ert-fail (format "%s not found in %s" point-name full-name)))
    stop-point))

(defun edebug-tests-should-match-result-in-messages (value)
  "Require that VALUE (a string) match an Edebug result in *Messages*.
Then clear edebug-tests' saved messages."
  (should (string-match-p (concat "Result: " (regexp-quote value) "$")
                          edebug-tests-messages))
  (setq edebug-tests-messages ""))

(defun edebug-tests-locate-def (def-name)
  "Search for a definition of DEF-NAME from the start of the current buffer.
Place point at the end of DEF-NAME in the buffer."
  (goto-char (point-min))
  (re-search-forward (concat "def\\S-+ edebug-test-code-" def-name)))

(defconst edebug-tests-start-of-next-def-regexp "^(\\S-*def\\S-+ \\(\\S-+\\)"
  "Regexp used to match the start of a definition.")
(defconst edebug-tests-stop-point-regexp "!\\(\\S-+?\\)!"
  "Regexp used to match a stop point annotation in the sample code.")

;;; Set up buffer containing code samples:

(defmacro edebug-tests-deduplicate (name names-and-numbers)
  "Return a unique variation on NAME.
NAME should be a string and NAMES-AND-NUMBERS an alist which can
be used by this macro to retain state.  If NAME for example is
\"symbol\" then the first and subsequent uses of this macro will
evaluate to \"symbol\", \"symbol-1\", \"symbol-2\", etc."
  (let ((g-name (gensym))
        (g-duplicate (gensym)))
    `(let* ((,g-name ,name)
            (,g-duplicate (assoc ,g-name ,names-and-numbers)))
       (if (null ,g-duplicate)
           (progn
             (push (cons ,g-name 0) ,names-and-numbers)
             ,g-name)
         (cl-incf (cdr ,g-duplicate))
         (format "%s-%s" ,g-name (cdr ,g-duplicate))))))

(defun edebug-tests-setup-code-file (tmpfile)
  "Extract stop points and loadable code from the sample code file.
Write the loadable code to a buffer for TMPFILE, and set
`edebug-tests-stop-points' to a map from defined symbols to stop
point names to positions in the file."
  (with-current-buffer (find-file-noselect edebug-tests-sample-code-file)
    (let ((marked-up-code (buffer-string)))
      (with-temp-file tmpfile
        (insert marked-up-code))))

  (with-current-buffer (find-file-noselect tmpfile)
    (let ((stop-points
           ;; Delete all the !name! annotations from the code, but remember
           ;; their names and where they were in an alist.
           (cl-loop
            initially (goto-char (point-min))
            while (re-search-forward edebug-tests-stop-point-regexp nil t)
            for name = (match-string-no-properties 1)
            do (replace-match "")
            collect (cons name (point))))
          names-and-numbers)

      ;; Now build an alist mapping definition names to annotation
      ;; names and positions.
      ;; If duplicate symbols exist in the file, enter them in the
      ;; alist as symbol, symbol-1, symbol-2 etc.
      (setq edebug-tests-stop-points
            (cl-loop
             initially (goto-char (point-min))
             while (re-search-forward edebug-tests-start-of-next-def-regexp
                                      nil t)
             for name =
               (edebug-tests-deduplicate (match-string-no-properties 1)
                                   names-and-numbers)
             for end-of-def =
               (save-match-data
                 (save-excursion
                   (re-search-forward edebug-tests-start-of-next-def-regexp
                                      nil 0)
                   (point)))
             collect (cons name
                           (cl-loop
                            while (and stop-points
                                       (< (cdar stop-points) end-of-def))
                            collect (pop stop-points))))))))

;;; Tests

(ert-deftest edebug-tests-check-keymap ()
  "Verify that `edebug-mode-map' is compatible with these tests.
If this test fails, one of two things is true. Either your
customizations modify `edebug-mode-map', in which case starting
Emacs with the -Q flag should fix the problem, or
`edebug-mode-map' has changed in edebug.el, in which case this
test and possibly others should be updated."
  ;; The reason verify-keybinding is a macro instead of a function is
  ;; that in the event of a failure, it makes the keybinding that
  ;; failed show up in ERT's output.
  (cl-macrolet ((verify-keybinding (key binding)
                   `(should (eq (lookup-key edebug-mode-map ,key)
                                ,binding))))
    (verify-keybinding " " 'edebug-step-mode)
    (verify-keybinding "n" 'edebug-next-mode)
    (verify-keybinding "g" 'edebug-go-mode)
    (verify-keybinding "G" 'edebug-Go-nonstop-mode)
    (verify-keybinding "t" 'edebug-trace-mode)
    (verify-keybinding "T" 'edebug-Trace-fast-mode)
    (verify-keybinding "c" 'edebug-continue-mode)
    (verify-keybinding "C" 'edebug-Continue-fast-mode)
    (verify-keybinding "f" 'edebug-forward-sexp)
    (verify-keybinding "h" 'edebug-goto-here)
    (verify-keybinding "I" 'edebug-instrument-callee)
    (verify-keybinding "i" 'edebug-step-in)
    (verify-keybinding "o" 'edebug-step-out)
    (verify-keybinding "q" 'top-level)
    (verify-keybinding "Q" 'edebug-top-level-nonstop)
    (verify-keybinding "a" 'abort-recursive-edit)
    (verify-keybinding "S" 'edebug-stop)
    (verify-keybinding "b" 'edebug-set-breakpoint)
    (verify-keybinding "u" 'edebug-unset-breakpoint)
    (verify-keybinding "B" 'edebug-next-breakpoint)
    (verify-keybinding "x" 'edebug-set-conditional-breakpoint)
    (verify-keybinding "X" 'edebug-set-global-break-condition)
    (verify-keybinding "r" 'edebug-previous-result)
    (verify-keybinding "e" 'edebug-eval-expression)
    (verify-keybinding "\C-x\C-e" 'edebug-eval-last-sexp)
    (verify-keybinding "E" 'edebug-visit-eval-list)
    (verify-keybinding "w" 'edebug-where)
    (verify-keybinding "v" 'edebug-view-outside) ;; maybe obsolete??
    (verify-keybinding "p" 'edebug-bounce-point)
    (verify-keybinding "P" 'edebug-view-outside) ;; same as v
    (verify-keybinding "W" 'edebug-toggle-save-windows)
    (verify-keybinding "?" 'edebug-help)
    (verify-keybinding "d" 'edebug-pop-to-backtrace)
    (verify-keybinding "-" 'negative-argument)
    (verify-keybinding "=" 'edebug-temp-display-freq-count)
    (should (eq (lookup-key backtrace-mode-map "n") 'backtrace-forward-frame))
    (should (eq (lookup-key backtrace-mode-map "s") 'backtrace-goto-source))))

(ert-deftest edebug-tests-stop-point-at-start-of-first-instrumented-function ()
  "Edebug stops at the beginning of an instrumented function."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "fac" '(0) t)
   (edebug-tests-run-kbd-macro
    "@"   (edebug-tests-should-be-at "fac" "start")
    "SPC" (edebug-tests-should-be-at "fac" "step")
    "g"   (should (equal edebug-tests-@-result 1)))))

(ert-deftest edebug-tests-step-showing-evaluation-results ()
  "Edebug prints expression evaluation results to the echo area."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "concat" '("x" "y" nil) t)
   (edebug-tests-run-kbd-macro
    "@"   (edebug-tests-should-be-at "concat" "start")
    "SPC" (edebug-tests-should-be-at "concat" "flag")
    (edebug-tests-should-match-result-in-messages "nil")
    "SPC" (edebug-tests-should-be-at "concat" "else-start")
    "SPC" (edebug-tests-should-be-at "concat" "else-b")
    (edebug-tests-should-match-result-in-messages "\"y\"")
    "SPC" (edebug-tests-should-be-at "concat" "else-a")
    (edebug-tests-should-match-result-in-messages "\"x\"")
    "SPC" (edebug-tests-should-be-at "concat" "else-concat")
    (edebug-tests-should-match-result-in-messages "\"yx\"")
    "SPC" (edebug-tests-should-be-at "concat" "if")
    (edebug-tests-should-match-result-in-messages "\"yx\"")
    "SPC" (should (equal edebug-tests-@-result "yx")))))

(ert-deftest edebug-tests-set-breakpoint-at-point ()
  "Edebug can set a breakpoint at point."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "concat" '("x" "y" t) t)
   (edebug-tests-run-kbd-macro
    "@"  (edebug-tests-should-be-at "concat" "start")
    "C-n C-e b C-n"  ; Move down, set a breakpoint and move away.
    "g"  (edebug-tests-should-be-at "concat" "then-concat")
    (edebug-tests-should-match-result-in-messages "\"xy\"")
    "g"  (should (equal edebug-tests-@-result "xy")))))

(ert-deftest edebug-tests-set-temporary-breakpoint-at-point ()
  "Edebug can set a temporary breakpoint at point."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "range" '(3) t)
   (edebug-tests-run-kbd-macro
    "@" (edebug-tests-should-be-at "range" "start")
    "C-n C-n C-n C-e"     ; Move down to the end of a sexp in the loop.
    "C-u b"               ; Set a temporary breakpoint.
    "C-n"                 ; Move away.
    "g"   (edebug-tests-should-be-at "range" "loop")
    (edebug-tests-should-match-result-in-messages "(0)")
    "g" (should (equal edebug-tests-@-result '(0 1 2))))))

(ert-deftest edebug-tests-clear-breakpoint ()
  "Edebug can clear a breakpoint."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "range" '(3) t)
   (edebug-tests-run-kbd-macro
    "@"
    (message "after @")
    (edebug-tests-should-be-at "range" "start")
    "C-n C-n C-n C-e b C-n"  ; Move down, set a breakpoint and move away.
    "g"   (edebug-tests-should-be-at "range" "loop")
    (edebug-tests-should-match-result-in-messages "(0)")
    "g"   (edebug-tests-should-be-at "range" "loop")
    (edebug-tests-should-match-result-in-messages "(1 0)")
    "u"  ; Unset the breakpoint.
    "g"   (should (equal edebug-tests-@-result '(0 1 2))))))

(ert-deftest edebug-tests-move-point-to-next-breakpoint ()
  "Edebug can move point to the next breakpoint."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "concat" '("a" "b" nil) t)
   (edebug-tests-run-kbd-macro
    "@"   (edebug-tests-should-be-at "concat" "start")
    "C-n C-e b"      ; Move down, set a breakpoint.
    "C-n b"          ; Set another breakpoint on the next line.
    "C-p C-p C-p"    ; Move back up.
    "B"   (edebug-tests-should-be-at "concat" "then-concat")
    "B"   (edebug-tests-should-be-at "concat" "else-concat")
    "G"   (should (equal edebug-tests-@-result "ba")))))

(ert-deftest edebug-tests-move-point-back-to-stop-point ()
  "Edebug can move point back to a stop point."
  (edebug-tests-with-normal-env
   (let ((test-buffer (get-buffer-create "edebug-tests-temp")))
     (edebug-tests-setup-@ "fac" '(4) t)
     (edebug-tests-run-kbd-macro
      "@"        (edebug-tests-should-be-at "fac" "start")
      "C-n w"    (edebug-tests-should-be-at "fac" "start")
      (pop-to-buffer test-buffer)
      "C-x X w"  (edebug-tests-should-be-at "fac" "start")
      "g"        (should (equal edebug-tests-@-result 24)))
     (ignore-errors (kill-buffer test-buffer)))))

(ert-deftest edebug-tests-jump-to-point ()
  "Edebug can stop at a temporary breakpoint at point."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "range" '(3) t)
   (edebug-tests-run-kbd-macro
    "@" (edebug-tests-should-be-at "range" "start")
    "C-n C-n C-n C-e"     ; Move down to the end of a sexp in the loop.
    "h" (edebug-tests-should-be-at "range" "loop")
    (edebug-tests-should-match-result-in-messages "(0)")
    "g" (should (equal edebug-tests-@-result '(0 1 2))))))

(ert-deftest edebug-tests-jump-forward-one-sexp ()
  "Edebug can run the program for one expression."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "range" '(3) t)
   (edebug-tests-run-kbd-macro
    "@"   (edebug-tests-should-be-at "range" "start")
    "SPC SPC f" (edebug-tests-should-be-at "range" "test")
    "g"   (should (equal edebug-tests-@-result '(0 1 2))))))

(ert-deftest edebug-tests-run-out-of-containing-sexp ()
  "Edebug can run the program until the end of the containing sexp."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "range" '(3) t)
   (edebug-tests-run-kbd-macro
    "@"   (edebug-tests-should-be-at "range" "start")
    "SPC SPC f" (edebug-tests-should-be-at "range" "test")
    "o"   (edebug-tests-should-be-at "range" "end-loop")
    (edebug-tests-should-match-result-in-messages "nil")
    "g"   (should (equal edebug-tests-@-result '(0 1 2))))))

(ert-deftest edebug-tests-observe-breakpoint-in-source ()
  "Edebug will stop at a breakpoint embedded in source code."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "choices" '(8) t)
   (edebug-tests-run-kbd-macro
    "@"  (edebug-tests-should-be-at "choices" "start")
    "g"  (edebug-tests-should-be-at "choices" "edebug")
    "g"  (should (equal edebug-tests-@-result nil)))))

(ert-deftest edebug-tests-set-conditional-breakpoint ()
  "Edebug can set and observe a conditional breakpoint."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "fac" '(5) t)
   (edebug-tests-run-kbd-macro
    "@"  (edebug-tests-should-be-at "fac" "start")
    ;; Set conditional breakpoint at end of next line.
    "C-n C-e x (eql SPC n SPC 3) RET"
    "g"  (edebug-tests-should-be-at "fac" "mult")
    (edebug-tests-should-match-result-in-messages "6 (#o6, #x6, ?\\C-f)")
    "g"  (should (equal edebug-tests-@-result 120)))))

(ert-deftest edebug-tests-error-trying-to-set-breakpoint-in-uninstrumented-code
    ()
  "Edebug refuses to set a breakpoint in uninstrumented code."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "fac" '(5) t)
   (let* ((debug-on-error nil)
          (edebug-on-error nil)
          error-message
          (command-error-function (lambda (&rest args)
                                    (setq error-message (cadar args)))))
     (edebug-tests-run-kbd-macro
      "@"  (edebug-tests-should-be-at "fac" "start")
      "C-u 10 C-n" ; Move down and out of instrumented function.
      "b"  (should (string-match-p "Not inside instrumented form"
                                   error-message))
      ;; The error stopped the keyboard macro. Start it again.
      (should-not executing-kbd-macro)
      (setq executing-kbd-macro t)
      "g"))))

(ert-deftest edebug-tests-set-and-break-on-global-condition ()
  "Edebug can break when a global condition becomes true."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "multiply" '(5 3) t)
   (edebug-tests-run-kbd-macro
    "@"  (edebug-tests-should-be-at "multiply" "start")
    "X (> SPC edebug-test-code-total SPC 10) RET"
    (should edebug-global-break-condition)
    "g"  (edebug-tests-should-be-at "multiply" "setq")
    (should (eql (symbol-value 'edebug-test-code-total) 12))
    "X C-a C-k nil RET" ; Remove suggestion before entering nil.
    "g"  (should (equal edebug-tests-@-result 15)))))

(ert-deftest edebug-tests-trace-showing-results-at-stop-points ()
  "Edebug can trace execution, showing results at stop points."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "concat" '("x" "y" nil) t)
   (edebug-tests-run-kbd-macro
    "@"   (edebug-tests-should-be-at "concat" "start")
    "T"   (should (string-match-p
                   (concat "Result: nil\n.*?"
                           "Result: \"y\"\n.*?"
                           "Result: \"x\"\n.*?"
                           "Result: \"yx\"\n.*?"
                           "Result: \"yx\"\n")
                   edebug-tests-messages))
    (should (equal edebug-tests-@-result "yx")))))

(ert-deftest edebug-tests-trace-showing-results-at-breakpoints ()
  "Edebug can trace execution, showing results at breakpoints."
  (edebug-tests-with-normal-env
   (edebug-tests-locate-def "format-vector-node")
   (edebug-tests-run-kbd-macro "C-u C-M-x C-n C-n C-e C-x X b")
   (edebug-tests-locate-def "format-list-node")
   (edebug-tests-run-kbd-macro "C-u C-M-x C-n C-n C-e C-x X b")
   (edebug-tests-setup-@ "format-node" '(([a b] [c d])) t)
   (edebug-tests-run-kbd-macro
    "@" (edebug-tests-should-be-at "format-node" "start")
    "C" (should (string-match-p
                 (concat "Result: \"ab\"\n.*?"
                         "Result: \"cd\"\n.*?"
                         "Result: \"\\[ab]\\[cd]\"\n")
                 edebug-tests-messages))
    (should (equal edebug-tests-@-result "{[ab][cd]}")))))

(ert-deftest edebug-tests-trace-function-call-and-return ()
  "Edebug can create a trace of function calls and returns."
  (edebug-tests-with-normal-env
   (edebug-tests-locate-def "format-vector-node")
   (eval-defun t)
   (edebug-tests-locate-def "format-list-node")
   (eval-defun t)
   (edebug-tests-setup-@ "format-node" '((a [b])) t)
   (let ((edebug-trace t)
         (trace-start (with-current-buffer
                          (get-buffer-create edebug-trace-buffer) (point-max))))
     (edebug-tests-run-kbd-macro
      "@"   (edebug-tests-should-be-at "format-node" "start")
      "g"   (should (equal edebug-tests-@-result "{a[b]}")))
     (with-current-buffer edebug-trace-buffer
       (should (string=
                "{ edebug-test-code-format-node args: ((a [b]))
:{ edebug-test-code-format-list-node args: ((a [b]))
::{ edebug-test-code-format-node args: (a)
::} edebug-test-code-format-node result: a
::{ edebug-test-code-format-node args: ([b])
:::{ edebug-test-code-format-vector-node args: ([b])
::::{ edebug-test-code-format-node args: (b)
::::} edebug-test-code-format-node result: b
:::} edebug-test-code-format-vector-node result: [b]
::} edebug-test-code-format-node result: [b]
:} edebug-test-code-format-list-node result: {a[b]}
} edebug-test-code-format-node result: {a[b]}
"  (buffer-substring trace-start (point-max))))))))

(ert-deftest edebug-tests-evaluate-expressions ()
  "Edebug can evaluate an expression in the context outside of itself."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "range" '(2) t)
   (edebug-tests-run-kbd-macro
    "@"   (edebug-tests-should-be-at "range" "start")
    "SPC SPC f" (edebug-tests-should-be-at "range" "test")
    (edebug-tests-should-match-result-in-messages "t")
    "e (- SPC num SPC index) RET"
    ;; Edebug just prints the result without "Result:"
    (should (string-match-p
             (regexp-quote "2 (#o2, #x2, ?\\C-b)")
             edebug-tests-messages))
    "g"   (should (equal edebug-tests-@-result '(0 1))))

   ;; Do it again with lexical-binding turned off.
   (setq lexical-binding nil)
   (eval-buffer)
   (should-not lexical-binding)
   (edebug-tests-setup-@ "range" '(2) t)
   (edebug-tests-run-kbd-macro
    "@"   (edebug-tests-should-be-at "range" "start")
    "SPC SPC f" (edebug-tests-should-be-at "range" "test")
    (edebug-tests-should-match-result-in-messages "t")
    "e (- SPC num SPC index) RET"
    ;; Edebug just prints the result without "Result:"
    (should (string-match-p
             (regexp-quote "2 (#o2, #x2, ?\\C-b)")
             edebug-tests-messages))
    "g"   (should (equal edebug-tests-@-result '(0 1))))))

(ert-deftest edebug-tests-step-into-function ()
  "Edebug can step into a function."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "format-node" '([b]) t)
   (edebug-tests-run-kbd-macro
    "@"    (edebug-tests-should-be-at "format-node" "start")
    "SPC SPC SPC SPC"
    (edebug-tests-should-be-at "format-node" "vbefore")
    "i"    (edebug-tests-should-be-at "format-vector-node" "start")
    "g"    (should (equal edebug-tests-@-result "[b]")))))

(ert-deftest edebug-tests-error-stepping-into-subr ()
  "Edebug refuses to step into a C function."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "format-node" '([b]) t)
   (let* ((debug-on-error nil)
          (edebug-on-error nil)
          error-message
          (command-error-function (lambda (&rest args)
                                    (setq error-message (cl-cadar args)))))
     (edebug-tests-run-kbd-macro
      "@"    (edebug-tests-should-be-at "format-node" "start")
      "SPC"  (edebug-tests-should-be-at "format-node" "vectorp")
      "i"    (should (string-match-p "vectorp is a built-in function"
                                     error-message))
      ;; The error stopped the keyboard macro. Start it again.
      (should-not executing-kbd-macro)
      (setq executing-kbd-macro t)
      "g"    (should (equal edebug-tests-@-result "[b]"))))))

(ert-deftest edebug-tests-step-into-macro-error ()
  "Edebug gives an error on trying to step into a macro (Bug#26847)."
  :expected-result :failed
  (ert-fail "Forcing failure because letting this test run aborts the others.")
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "try-flavors" nil t)
   (let* ((debug-on-error nil)
          (edebug-on-error nil)
          (error-message "")
          (command-error-function (lambda (&rest args)
                                    (setq error-message (cl-cadar args)))))
     (edebug-tests-run-kbd-macro
      "@ SPC SPC SPC SPC SPC"
      (edebug-tests-should-be-at "try-flavors" "macro")
      "i" (should (string-match-p "edebug-test-code-try-flavors is a macro"
                                  error-message))
      ;; The error stopped the keyboard macro. Start it again.
      (should-not executing-kbd-macro)
      (setq executing-kbd-macro t)
      "g"   (should (equal edebug-tests-@-result
                           '("chocolate" "strawberry")))))))

(ert-deftest edebug-tests-step-into-generic-method ()
  "Edebug can step into a generic method (Bug#22294)."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "use-methods" nil t)
   (edebug-tests-run-kbd-macro
    "@ SPC" (edebug-tests-should-be-at "use-methods" "number")
    "i"     (edebug-tests-should-be-at "emphasize-1" "start")
    "gg"    (should (equal edebug-tests-@-result
                           '("The number is not 101 or 99, but 100!"
                             "***yes***"))))))

(ert-deftest edebug-tests-break-in-lambda-out-of-defining-context ()
  "Edebug observes a breakpoint in a lambda executed out of defining context."
  (edebug-tests-with-normal-env
   (edebug-tests-locate-def "make-lambda")
   (eval-defun t)
   (goto-char (edebug-tests-get-stop-point "make-lambda" "x"))
   (edebug-set-breakpoint t)
   (edebug-tests-setup-@ "use-lambda" nil t)
   (edebug-tests-run-kbd-macro
    "@g"   (edebug-tests-should-be-at "make-lambda" "x")
    (edebug-tests-should-match-result-in-messages "1 (#o1, #x1, ?\\C-a)")
    "g"    (should (equal edebug-tests-@-result '(11 12 13))))))

(ert-deftest edebug-tests-respects-initial-mode ()
  "Edebug can stop first at breakpoint instead of first instrumented function."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "fac" '(4) t)
   (goto-char (edebug-tests-get-stop-point "fac" "mult"))
   (edebug-set-breakpoint t)
   (setq edebug-initial-mode 'go)
   (edebug-tests-run-kbd-macro
    "@"    (edebug-tests-should-be-at "fac" "mult")
    (edebug-tests-should-match-result-in-messages "1 (#o1, #x1, ?\\C-a)")
    "G"    (should (equal edebug-tests-@-result 24)))))

(ert-deftest edebug-tests-step-through-non-definition ()
  "Edebug can step through a non-defining form."
  (edebug-tests-with-normal-env
   (goto-char (edebug-tests-get-stop-point "try-flavors" "end-unless"))
   (edebug-tests-run-kbd-macro
    "C-u C-M-x"
    "SPC SPC" (edebug-tests-should-be-at "try-flavors" "nutty")
    (edebug-tests-should-match-result-in-messages "nil")
    "SPC"     (edebug-tests-should-be-at "try-flavors" "setq")
    "f"       (edebug-tests-should-be-at "try-flavors" "end-setq")
    (edebug-tests-should-match-result-in-messages "\"chocolate\"")
    "g")))

(ert-deftest edebug-tests-conditional-breakpoints-can-use-lexical-variables ()
  "Edebug can set a conditional breakpoint using a lexical variable. Bug#12685"
  (edebug-tests-with-normal-env
   (should lexical-binding)
   (edebug-tests-setup-@ "fac" '(5) t)
   (edebug-tests-run-kbd-macro
    "@"  (edebug-tests-should-be-at "fac" "start")
    ;; Set conditional breakpoint at end of next line.
    "C-n C-e x (eql SPC n SPC 3) RET"
    "g"  (edebug-tests-should-be-at "fac" "mult")
    (edebug-tests-should-match-result-in-messages
     "6 (#o6, #x6, ?\\C-f)"))))

(ert-deftest edebug-tests-writable-buffer-state-is-preserved ()
  "On Edebug exit writable buffers are still writable (Bug#14144)."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "choices" '(0) t)
   (read-only-mode -1)
   (edebug-tests-run-kbd-macro
    "@g" (should (equal edebug-tests-@-result "zero")))
   (barf-if-buffer-read-only)))

(ert-deftest edebug-tests-list-containing-empty-string-result-printing ()
  "Edebug correctly prints a list containing only an empty string (Bug#17934)."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "empty-string-list" nil t)
   (edebug-tests-run-kbd-macro
    "@ SPC"  (edebug-tests-should-be-at
              "empty-string-list" "step")
    (edebug-tests-should-match-result-in-messages "(\"\")")
    "g")))

(ert-deftest edebug-tests-evaluation-of-current-buffer-bug-19611 ()
  "Edebug can evaluate `current-buffer' in correct context. (Bug#19611)."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "current-buffer" nil t)
   (edebug-tests-run-kbd-macro
    "@"    (edebug-tests-should-be-at
            "current-buffer" "start")
    "SPC SPC SPC" (edebug-tests-should-be-at
                   "current-buffer" "body")
    "e (current-buffer) RET"
    ;; Edebug just prints the result without "Result:"
    (should (string-match-p
             (regexp-quote "*edebug-test-code-buffer*")
             edebug-tests-messages))
    "g"    (should (equal edebug-tests-@-result
                          "current-buffer: *edebug-test-code-buffer*")))))

(ert-deftest edebug-tests-trivial-backquote ()
  "Edebug can instrument a trivial backquote expression (Bug#23651)."
  (edebug-tests-with-normal-env
   (read-only-mode -1)
   (delete-region (point-min) (point-max))
   (insert  "`1")
   (read-only-mode)
   (edebug-eval-defun nil)
   (should (string-match-p (regexp-quote "1 (#o1, #x1, ?\\C-a)")
                           edebug-tests-messages))
   (setq edebug-tests-messages "")

   (setq edebug-initial-mode 'go)
   ;; In Bug#23651 Edebug would hang reading `1.
   (edebug-eval-defun t)))

(ert-deftest edebug-tests-trivial-comma ()
  "Edebug can read a trivial comma expression (Bug#23651)."
  (edebug-tests-with-normal-env
   (read-only-mode -1)
   (delete-region (point-min) (point-max))
   (insert  ",1")
   (read-only-mode)
   (should-error (edebug-eval-defun t))))

(ert-deftest edebug-tests-circular-read-syntax ()
  "Edebug can instrument code using circular read object syntax (Bug#23660)."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "circular-read-syntax" nil t)
   (edebug-tests-run-kbd-macro
    "@"  (should (eql (car edebug-tests-@-result)
                      (cdr edebug-tests-@-result))))))

(ert-deftest edebug-tests-hash-read-syntax ()
  "Edebug can instrument code which uses # read syntax (Bug#25068)."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "hash-read-syntax" nil t)
   (edebug-tests-run-kbd-macro
    "@g"  (should (equal edebug-tests-@-result
                         '(#("abcd" 1 3 (face italic)) 511))))))

(ert-deftest edebug-tests-dotted-forms ()
  "Edebug can instrument code matching the tail of a dotted spec (Bug#6415)."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "use-destructuring-bind" nil t)
   (edebug-tests-run-kbd-macro
    "@ SPC SPC SPC SPC SPC SPC"
    (edebug-tests-should-be-at "use-destructuring-bind" "x")
    (edebug-tests-should-match-result-in-messages "2 (#o2, #x2, ?\\C-b)")
    "SPC"
    (edebug-tests-should-be-at "use-destructuring-bind" "y")
    (edebug-tests-should-match-result-in-messages "3 (#o3, #x3, ?\\C-c)")
    "g"
    (should (equal edebug-tests-@-result 5)))))

(ert-deftest edebug-tests-cl-macrolet ()
  "Edebug can instrument `cl-macrolet' expressions. (Bug#29919)"
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "use-cl-macrolet" '(10) t)
   (edebug-tests-run-kbd-macro
    "@ SPC SPC"
    (edebug-tests-should-be-at "use-cl-macrolet" "func")
    (edebug-tests-should-match-result-in-messages "+")
    "g"
    (should (equal edebug-tests-@-result "The result of applying + to (1 x) is 11")))))

(ert-deftest edebug-tests-backtrace-goto-source ()
  "Edebug can jump to instrumented source from its *Edebug-Backtrace* buffer."
  (edebug-tests-with-normal-env
   (edebug-tests-setup-@ "range" '(2) t)
   (edebug-tests-run-kbd-macro
    "@ SPC SPC"
    (edebug-tests-should-be-at "range" "lt")
    "dns"  ; Pop to backtrace, next frame, goto source.
    (edebug-tests-should-be-at "range" "start")
    "g"
    (should (equal edebug-tests-@-result '(0 1))))))

(provide 'edebug-tests)
;;; edebug-tests.el ends here
