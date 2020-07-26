;;; emacs-module-tests --- Test GNU Emacs modules.  -*- lexical-binding: t; -*-

;; Copyright 2015-2020 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

;;; Commentary:

;; Unit tests for the dynamic module facility.  See Info node `(elisp)
;; Writing Dynamic Modules'.  These tests make use of a small test
;; module in test/data/emacs-module.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'help-fns)

(defconst mod-test-emacs
  (expand-file-name invocation-name invocation-directory)
  "File name of the Emacs binary currently running.")

(eval-and-compile
  (defconst mod-test-file
    (expand-file-name "../test/data/emacs-module/mod-test" invocation-directory)
    "File name of the module test file."))

(require 'mod-test mod-test-file)

(cl-defgeneric emacs-module-tests--generic (_))

(cl-defmethod emacs-module-tests--generic ((_ module-function))
  'module-function)

(cl-defmethod emacs-module-tests--generic ((_ user-ptr))
  'user-ptr)

;;
;; Basic tests.
;;

(ert-deftest mod-test-sum-test ()
  (should (= (mod-test-sum 1 2) 3))
  (let ((descr (should-error (mod-test-sum 1 2 3))))
    (should (eq (car descr) 'wrong-number-of-arguments))
    (should (module-function-p (nth 1 descr)))
    (should (eq 0
                (string-match
                 (concat "#<module function "
                         "\\(at \\(0x\\)?[[:xdigit:]]+\\( from .*\\)?"
                         "\\|Fmod_test_sum from .*\\)>")
                 (prin1-to-string (nth 1 descr)))))
    (should (= (nth 2 descr) 3)))
  (should-error (mod-test-sum "1" 2) :type 'wrong-type-argument)
  (should-error (mod-test-sum 1 "2") :type 'wrong-type-argument)
  ;; The following tests are for 32-bit build --with-wide-int.
  (should (= (mod-test-sum -1 most-positive-fixnum)
             (1- most-positive-fixnum)))
  (should (= (mod-test-sum 1 most-negative-fixnum)
             (1+ most-negative-fixnum)))
  (when (< #x1fffffff most-positive-fixnum)
    (should (= (mod-test-sum 1 #x1fffffff)
               (1+ #x1fffffff)))
    (should (= (mod-test-sum -1 (1+ #x1fffffff))
               #x1fffffff)))
  (should (= (mod-test-sum 1 most-positive-fixnum)
             (1+ most-positive-fixnum)))
  (should (= (mod-test-sum -1 most-negative-fixnum)
             (1- most-negative-fixnum))))

(ert-deftest mod-test-sum-docstring ()
  (should (string= (documentation 'mod-test-sum) "Return A + B\n\n(fn a b)")))

(ert-deftest module-function-object ()
  "Extract and test the implementation of a module function.
This test needs to be changed whenever the implementation
changes."
  (let ((func (symbol-function #'mod-test-sum)))
    (should (module-function-p func))
    (should (functionp func))
    (should (equal (type-of func) 'module-function))
    (should (eq (emacs-module-tests--generic func) 'module-function))
    (should (string-match-p
             (rx bos "#<module function "
                 (or "Fmod_test_sum"
                     (and "at 0x" (+ hex-digit)))
                 (? " from " (* nonl) "mod-test" (* nonl) )
                 ">" eos)
             (prin1-to-string func)))))

;;
;; Non-local exists (throw, signal).
;;

(ert-deftest mod-test-non-local-exit-signal-test ()
  (should-error (mod-test-signal))
  (let (debugger-args backtrace)
    (should-error
     (let ((debugger (lambda (&rest args)
                       (setq debugger-args args
                             backtrace (with-output-to-string (backtrace)))
                       (cl-incf num-nonmacro-input-events)))
           (debug-on-signal t))
       (mod-test-signal)))
    (should (equal debugger-args '(error (error . 56))))
    (should (string-match-p
             (rx bol "  mod-test-signal()" eol)
             backtrace))))

(ert-deftest mod-test-non-local-exit-throw-test ()
  (should (equal
           (catch 'tag
             (mod-test-throw)
             (ert-fail "expected throw"))
           65)))

(ert-deftest mod-test-non-local-exit-funcall-normal ()
  (should (equal (mod-test-non-local-exit-funcall (lambda () 23))
                 23)))

(ert-deftest mod-test-non-local-exit-funcall-signal ()
  (should (equal (mod-test-non-local-exit-funcall
                  (lambda () (signal 'error '(32))))
                 '(signal error (32)))))

(ert-deftest mod-test-non-local-exit-funcall-throw ()
  (should (equal (mod-test-non-local-exit-funcall (lambda () (throw 'tag 32)))
                 '(throw tag 32))))

;;
;; String tests.
;;

(defun multiply-string (s n)
  "Return N copies of S concatenated together."
  (let ((res ""))
    (dotimes (_ n)
      (setq res (concat res s)))
    res))

(ert-deftest mod-test-globref-make-test ()
  (let ((mod-str (mod-test-globref-make))
        (ref-str (multiply-string "abcdefghijklmnopqrstuvwxyz" 100)))
    (garbage-collect) ;; XXX: not enough to really test but it's something..
    (should (string= ref-str mod-str))))

(ert-deftest mod-test-globref-free-test ()
  (should (eq (mod-test-globref-free 1 'a "test" 'b) 'ok)))

(ert-deftest mod-test-globref-reordered ()
  (should (equal (mod-test-globref-reordered) '(t t t nil))))

(ert-deftest mod-test-string-a-to-b-test ()
  (should (string= (mod-test-string-a-to-b "aaa") "bbb")))

;;
;; User-pointer tests.
;;

(ert-deftest mod-test-userptr-fun-test ()
  (let* ((n 42)
         (v (mod-test-userptr-make n))
         (r (mod-test-userptr-get v)))

    (should (eq (type-of v) 'user-ptr))
    (should (eq (emacs-module-tests--generic v) 'user-ptr))
    (should (integerp r))
    (should (= r n))))

;; TODO: try to test finalizer

;;
;; Vector tests.
;;

(ert-deftest mod-test-vector-test ()
  (dolist (s '(2 10 100 1000))
    (dolist (e '(42 foo "foo"))
      (let* ((v-ref (make-vector 2 e))
             (eq-ref (eq (aref v-ref 0) (aref v-ref 1)))
             (v-test (make-vector s nil)))

        (should (eq (mod-test-vector-fill v-test e) t))
        (should (eq (mod-test-vector-eq v-test e) eq-ref))))))

(ert-deftest module--func-arity ()
  (should (equal (func-arity #'mod-test-return-t) '(1 . 1)))
  (should (equal (func-arity #'mod-test-sum) '(2 . 2))))

(ert-deftest module--help-function-arglist ()
  (should (equal (help-function-arglist #'mod-test-return-t :preserve-names)
                 '(arg1)))
  (should (equal (help-function-arglist #'mod-test-return-t)
                 '(arg1)))
  (should (equal (help-function-arglist #'mod-test-sum :preserve-names)
                 '(a b)))
  (should (equal (help-function-arglist #'mod-test-sum)
                 '(arg1 arg2))))

(defmacro module--with-temp-directory (name &rest body)
  "Bind NAME to the name of a temporary directory and evaluate BODY.
NAME must be a symbol.  Delete the temporary directory after BODY
exits normally or non-locally.  NAME will be bound to the
directory name (not the directory file name) of the temporary
directory."
  (declare (indent 1))
  (cl-check-type name symbol)
  `(let ((,name (file-name-as-directory
                 (make-temp-file "emacs-module-test" :directory))))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,name :recursive))))

(defmacro module--test-assertion (pattern &rest body)
  "Test that PATTERN matches the assertion triggered by BODY.
Run Emacs as a subprocess, load the test module `mod-test-file',
and evaluate BODY.  Verify that Emacs aborts and prints a module
assertion message that matches PATTERN.  PATTERN is evaluated and
must evaluate to a regular expression string."
  (declare (indent 1))
  ;; To contain any core dumps.
  `(module--with-temp-directory tempdir
     (with-temp-buffer
       (let* ((default-directory tempdir)
              (status (call-process mod-test-emacs nil t nil
                                    "-batch" "-Q" "-module-assertions"
                                    "-eval" "(setq w32-disable-abort-dialog t)"
                                    "-eval"
                                    ,(prin1-to-string
                                      `(progn
                                         (require 'mod-test ,mod-test-file)
                                         ,@body)))))
         ;; Aborting doesn't raise a signal on MS-DOS/Windows, but
         ;; rather exits with a non-zero status: 2 on MS-DOS (see
         ;; msdos.c:msdos_abort), 3 on Windows, per MSDN documentation
         ;; of 'abort'.
         (if (memq system-type '(ms-dos windows-nt))
             (should (>= status 2))
           (should (stringp status))
           ;; eg "Aborted" or "Abort trap: 6"
           (should (string-prefix-p "Abort" status)))
         (search-backward "Emacs module assertion: ")
         (goto-char (match-end 0))
         (should (string-match-p ,pattern
                                 (buffer-substring-no-properties
                                  (point) (point-max))))))))

(ert-deftest module--test-assertions--load-non-live-object ()
  "Check that -module-assertions verify that non-live objects aren't accessed."
  (skip-unless (or (file-executable-p mod-test-emacs)
                   (and (eq system-type 'windows-nt)
                        (file-executable-p (concat mod-test-emacs ".exe")))))
  ;; This doesn't yet cause undefined behavior.
  (should (eq (mod-test-invalid-store) 123))
  (module--test-assertion (rx "Emacs value not found in "
                              (+ digit) " values of "
                              (+ digit) " environments\n")
    ;; Storing and reloading a local value causes undefined behavior,
    ;; which should be detected by the module assertions.
    (mod-test-invalid-store)
    (mod-test-invalid-load)))

(ert-deftest module--test-assertions--load-non-live-object-with-global-copy ()
  "Check that -module-assertions verify that non-live objects aren't accessed.
This differs from `module--test-assertions-load-non-live-object'
in that it stows away a global reference.  The module assertions
should nevertheless detect the invalid load."
  (skip-unless (or (file-executable-p mod-test-emacs)
                   (and (eq system-type 'windows-nt)
                        (file-executable-p (concat mod-test-emacs ".exe")))))
  ;; This doesn't yet cause undefined behavior.
  (should (eq (mod-test-invalid-store-copy) 123))
  (module--test-assertion (rx "Emacs value not found in "
                              (+ digit) " values of "
                              (+ digit) " environments\n")
    ;; Storing and reloading a local value causes undefined behavior,
    ;; which should be detected by the module assertions.
    (mod-test-invalid-store-copy)
    (mod-test-invalid-load)))

(ert-deftest module--test-assertions--call-emacs-from-gc ()
  "Check that -module-assertions prevents calling Emacs functions
during garbage collection."
  (skip-unless (or (file-executable-p mod-test-emacs)
                   (and (eq system-type 'windows-nt)
                        (file-executable-p (concat mod-test-emacs ".exe")))))
  (module--test-assertion
      (rx "Module function called during garbage collection\n")
    (mod-test-invalid-finalizer)
    (garbage-collect)))

(ert-deftest module--test-assertions--globref-invalid-free ()
  "Check that -module-assertions detects invalid freeing of a
local reference."
    (skip-unless (or (file-executable-p mod-test-emacs)
                   (and (eq system-type 'windows-nt)
                        (file-executable-p (concat mod-test-emacs ".exe")))))
  (module--test-assertion
      (rx "Global value was not found in list of " (+ digit) " globals")
    (mod-test-globref-invalid-free)
    (garbage-collect)))

(ert-deftest module/describe-function-1 ()
  "Check that Bug#30163 is fixed."
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (describe-function-1 #'mod-test-sum)
      (should (equal
               (buffer-substring-no-properties 1 (point-max))
               (format "a module function in `data/emacs-module/mod-test%s'.

(mod-test-sum a b)

Return A + B"
                       module-file-suffix))))))

(ert-deftest module/load-history ()
  "Check that Bug#30164 is fixed."
  (load mod-test-file)
  (cl-destructuring-bind (file &rest entries) (car load-history)
    (should (equal (file-name-sans-extension file) mod-test-file))
    (should (member '(provide . mod-test) entries))
    (should (member '(defun . mod-test-sum) entries))))

(ert-deftest mod-test-sleep-until ()
  "Check that `mod-test-sleep-until' either returns normally or quits.
Interactively, you can try hitting \\[keyboard-quit] to quit."
  (dolist (arg '(nil t))
    ;; Guard against some caller setting `inhibit-quit'.
    (with-local-quit
      (condition-case nil
          (should (eq (with-local-quit
                        ;; Because `inhibit-quit' is nil here, the next
                        ;; form either quits or returns `finished'.
                        (mod-test-sleep-until
                         ;; Interactively, run for 5 seconds to give the
                         ;; user time to quit.  In batch mode, run only
                         ;; briefly since the user can't quit.
                         (time-add nil (if noninteractive 0.1 5))
                         ;; should_quit or process_input
                         arg))
                      'finished))
        (quit)))))

(ert-deftest mod-test-add-nanosecond/valid ()
  (dolist (input (list
                  ;; Some realistic examples.
                  (current-time) (time-to-seconds)
                  (encode-time 12 34 5 6 7 2019 t)
                  ;; Various legacy timestamp forms.
                  '(123 456) '(123 456 789) '(123 456 789 6000)
                  ;; Corner case: this will result in a nanosecond
                  ;; value of 1000000000 after addition.  The module
                  ;; code should handle this correctly.
                  '(123 65535 999999 999000)
                  ;; Seconds since the epoch.
                  123 123.45
                  ;; New (TICKS . HZ) format.
                  '(123456789 . 1000000000)))
    (ert-info ((format "input: %s" input))
      (let ((result (mod-test-add-nanosecond input))
	    (desired-result
	     (let ((hz 1000000000))
	       (time-add (time-convert input hz) (cons 1 hz)))))
        (should (consp result))
        (should (integerp (car result)))
        (should (integerp (cdr result)))
        (should (cl-plusp (cdr result)))
        (should (time-equal-p result desired-result))))))

(ert-deftest mod-test-add-nanosecond/nil ()
  (should (<= (float-time (mod-test-add-nanosecond nil))
              (+ (float-time) 1e-9))))

(ert-deftest mod-test-add-nanosecond/invalid ()
  (dolist (input '(1.0e+INF 1.0e-INF 0.0e+NaN (123) (123.45 6 7) "foo" [1 2]))
    (ert-info ((format "input: %s" input))
      (should-error (mod-test-add-nanosecond input)))))

(ert-deftest mod-test-nanoseconds ()
  "Test truncation when converting to `struct timespec'."
  (dolist (test-case '((0 . 0)
                       (-1 . -1000000000)
                       ((1 . 1000000000) . 1)
                       ((-1 . 1000000000) . -1)
                       ((1 . 1000000000000) . 0)
                       ((-1 . 1000000000000) . -1)
                       ((999 . 1000000000000) . 0)
                       ((-999 . 1000000000000) . -1)
                       ((1000 . 1000000000000) . 1)
                       ((-1000 . 1000000000000) . -1)
                       ((0 0 0 1) . 0)
                       ((0 0 0 -1) . -1)))
    (let ((input (car test-case))
          (expected (cdr test-case)))
      (ert-info ((format "input: %S, expected result: %d" input expected))
        (should (= (mod-test-nanoseconds input) expected))))))

(ert-deftest mod-test-double ()
  (dolist (input (list 0 1 2 -1 42 12345678901234567890
                       most-positive-fixnum (1+ most-positive-fixnum)
                       most-negative-fixnum (1- most-negative-fixnum)))
    (ert-info ((format "input: %d" input))
      (should (= (mod-test-double input) (* 2 input))))))

;;; emacs-module-tests.el ends here
