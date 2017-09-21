;;; flymake-tests.el --- Test suite for flymake

;; Copyright (C) 2011-2017 Free Software Foundation, Inc.

;; Author: Eduard Wiebe <usenet@pusto.de>

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

;;; Code:
(require 'ert)
(require 'flymake)

(defvar flymake-tests-data-directory
  (expand-file-name "lisp/progmodes/flymake-resources"
                    (or (getenv "EMACS_TEST_DIRECTORY")
                        (expand-file-name "../../.."
                                          (or load-file-name
                                              buffer-file-name))))
  "Directory containing flymake test data.")


;;
;;
(cl-defun flymake-tests--call-with-fixture (fn file
                                               &key (severity-predicate
                                                     nil sev-pred-supplied-p))
  "Call FN after flymake setup in FILE, using `flymake-proc`.
SEVERITY-PREDICATE is used to setup
`flymake-proc-warning-predicate'."
  (let* ((file (expand-file-name file flymake-tests-data-directory))
         (visiting (find-buffer-visiting file))
         (buffer (or visiting (find-file-noselect file)))
         (process-environment (cons "LC_ALL=C" process-environment))
         (i 0))
    (unwind-protect
        (with-current-buffer buffer
          (save-excursion
            (when sev-pred-supplied-p
              (setq-local flymake-proc-warning-predicate severity-predicate))
            (goto-char (point-min))
            (flymake-mode 1)
            ;; Weirdness here...  http://debbugs.gnu.org/17647#25
            ;; ... meaning `sleep-for', and even
            ;; `accept-process-output', won't suffice as ways to get
            ;; process filters and sentinels to run, though they do work
            ;; fine in a non-interactive batch session. The only thing
            ;; that will indeed unblock pending process output is
            ;; reading an input event, so, as a workaround, use a dummy
            ;; `read-event' with a very short timeout.
            (unless noninteractive (read-event "" nil 0.1))
            (while (and flymake-is-running (< (setq i (1+ i)) 10))
              (unless noninteractive (read-event "" nil 0.1))
              (sleep-for (+ 0.5 flymake-no-changes-timeout)))
            (funcall fn)))
      (and buffer
           (not visiting)
           (let (kill-buffer-query-functions) (kill-buffer buffer))))))

(cl-defmacro flymake-tests--with-flymake ((file &rest args)
                                          &body body)
  (declare (indent 1)
           (debug (sexp &rest form)))
  `(flymake-tests--call-with-fixture (lambda () ,@body) ,file ,@args))

(ert-deftest warning-predicate-rx-gcc ()
  "Test GCC warning via regexp predicate."
  (skip-unless (and (executable-find "gcc") (executable-find "make")))
  (flymake-tests--with-flymake
      ("test.c" :severity-predicate "^[Ww]arning")
    (flymake-goto-next-error)
    (should (eq 'flymake-warning
                (face-at-point)))))

(ert-deftest warning-predicate-function-gcc ()
  "Test GCC warning via function predicate."
  (skip-unless (and (executable-find "gcc") (executable-find "make")))
  (flymake-tests--with-flymake
      ("test.c" :severity-predicate
       (lambda (msg) (string-match "^[Ww]arning" msg)))
    (flymake-goto-next-error)
    (should (eq 'flymake-warning
                (face-at-point)))))

(ert-deftest warning-predicate-rx-perl ()
  "Test perl warning via regular expression predicate."
  (skip-unless (executable-find "perl"))
  (flymake-tests--with-flymake
      ("test.pl" :severity-predicate "^Scalar value")
    (flymake-goto-next-error)
    (should (eq 'flymake-warning
                (face-at-point)))))

(ert-deftest warning-predicate-function-perl ()
  "Test perl warning via function predicate."
  (skip-unless (executable-find "perl"))
  (flymake-tests--with-flymake
      ("test.pl" :severity-predicate
       (lambda (msg) (string-match "^Scalar value" msg)))
    (flymake-goto-next-error)
    (should (eq 'flymake-warning
                (face-at-point)))))

(ert-deftest errors-and-warnings ()
  "Test GCC warning via function predicate."
  (skip-unless (and (executable-find "gcc") (executable-find "make")))
  (flymake-tests--with-flymake
      ("errors-and-warnings.c")
    (flymake-goto-next-error)
    (should (eq 'flymake-error (face-at-point)))
    (flymake-goto-next-error)
    (should (eq 'flymake-warning (face-at-point)))
    (flymake-goto-next-error)
    (should (eq 'flymake-warning (face-at-point)))
    (flymake-goto-next-error)
    (should (eq 'flymake-error (face-at-point)))
    (should-error (flymake-goto-next-error nil t)) ))

(provide 'flymake-tests)

;;; flymake.el ends here
