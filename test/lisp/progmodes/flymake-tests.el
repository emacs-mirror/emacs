;;; flymake-tests.el --- Test suite for flymake -*- lexical-binding: t -*-

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
(defun flymake-tests--wait-for-backends ()
  ;; Weirdness here...  http://debbugs.gnu.org/17647#25
  ;; ... meaning `sleep-for', and even
  ;; `accept-process-output', won't suffice as ways to get
  ;; process filters and sentinels to run, though they do work
  ;; fine in a non-interactive batch session. The only thing
  ;; that will indeed unblock pending process output is
  ;; reading an input event, so, as a workaround, use a dummy
  ;; `read-event' with a very short timeout.
  (unless noninteractive (read-event "" nil 0.1))
  (cl-loop repeat 5
           for notdone = (cl-set-difference (flymake-running-backends)
                                            (flymake-reporting-backends))
           while notdone
           unless noninteractive do (read-event "" nil 0.1)
           do (sleep-for (+ 0.5 flymake-no-changes-timeout))
           finally (when notdone (ert-fail
                                  (format "Some backends not reporting yet %s"
                                          notdone)))))

(cl-defun flymake-tests--call-with-fixture (fn file
                                               &key (severity-predicate
                                                     nil sev-pred-supplied-p))
  "Call FN after flymake setup in FILE, using `flymake-proc`.
SEVERITY-PREDICATE is used to setup
`flymake-proc-diagnostic-type-pred'"
  (let* ((file (expand-file-name file flymake-tests-data-directory))
         (visiting (find-buffer-visiting file))
         (buffer (or visiting (find-file-noselect file)))
         (process-environment (cons "LC_ALL=C" process-environment))
         (warning-minimum-log-level :error))
    (unwind-protect
        (with-current-buffer buffer
          (save-excursion
            (when sev-pred-supplied-p
              (setq-local flymake-proc-diagnostic-type-pred severity-predicate))
            (goto-char (point-min))
            (unless flymake-mode (flymake-mode 1))
            (flymake-tests--wait-for-backends)
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

(ert-deftest different-diagnostic-types ()
  "Test GCC warning via function predicate."
  (skip-unless (and (executable-find "gcc") (executable-find "make")))
  (let ((flymake-wrap-around nil))
    (flymake-tests--with-flymake
        ("errors-and-warnings.c")
      (flymake-goto-next-error)
      (should (eq 'flymake-error (face-at-point)))
      (flymake-goto-next-error)
      (should (eq 'flymake-note (face-at-point)))
      (flymake-goto-next-error)
      (should (eq 'flymake-warning (face-at-point)))
      (flymake-goto-next-error)
      (should (eq 'flymake-error (face-at-point)))
      (flymake-goto-next-error)
      (should (eq 'flymake-warning (face-at-point)))
      (flymake-goto-next-error)
      (should (eq 'flymake-warning (face-at-point)))
      (should-error (flymake-goto-next-error nil nil t)))))

(ert-deftest included-c-header-files ()
  "Test inclusion of .h header files."
  (skip-unless (and (executable-find "gcc") (executable-find "make")))
  (let ((flymake-wrap-around nil))
    (flymake-tests--with-flymake
        ("some-problems.h")
      (flymake-goto-next-error)
      (should (eq 'flymake-warning (face-at-point)))
      (flymake-goto-next-error)
      (should (eq 'flymake-error (face-at-point)))
      (should-error (flymake-goto-next-error nil nil t)))
    (flymake-tests--with-flymake
        ("no-problems.h")
      (should-error (flymake-goto-next-error nil nil t)))))

(defmacro flymake-tests--assert-set (set
                                     should
                                     should-not)
  (declare (indent 1))
  `(progn
     ,@(cl-loop
        for s in should
        collect `(should (memq (quote ,s) ,set)))
     ,@(cl-loop
        for s in should-not
        collect `(should-not (memq (quote ,s) ,set)))))

(defun flymake-tests--diagnose-words
    (report-fn type words)
  "Helper. Call REPORT-FN with diagnostics for WORDS in buffer."
  (funcall report-fn
           (cl-loop
            for word in words
            append
            (save-excursion
              (goto-char (point-min))
              (cl-loop while (word-search-forward word nil t)
                       collect (flymake-make-diagnostic
                                (current-buffer)
                                (match-beginning 0)
                                (match-end 0)
                                type
                                (concat word " is wrong")))))))

(ert-deftest dummy-backends ()
  "Test many different kinds of backends."
  (with-temp-buffer
    (cl-letf
        (((symbol-function 'error-backend)
          (lambda (report-fn)
            (run-with-timer
             0.5 nil
             #'flymake-tests--diagnose-words report-fn :error '("manha" "prognata"))))
         ((symbol-function 'warning-backend)
          (lambda (report-fn)
            (run-with-timer
             0.5 nil
             #'flymake-tests--diagnose-words report-fn :warning '("ut" "dolor"))))
         ((symbol-function 'sync-backend)
          (lambda (report-fn)
            (flymake-tests--diagnose-words report-fn :note '("quis" "commodo"))))
         ((symbol-function 'panicking-backend)
          (lambda (report-fn)
            (run-with-timer
             0.5 nil
             report-fn :panic :explanation "The spanish inquisition!")))
         ((symbol-function 'crashing-backend)
          (lambda (_report-fn)
            ;; HACK: Shoosh log during tests
            (setq-local warning-minimum-log-level :emergency)
            (error "crashed"))))
      (insert "Lorem ipsum dolor sit amet, consectetur adipiscing
    elit, sed do eiusmod tempor incididunt ut labore et dolore
    manha aliqua. Ut enim ad minim veniam, quis nostrud
    exercitation ullamco laboris nisi ut aliquip ex ea commodo
    consequat. Duis aute irure dolor in reprehenderit in
    voluptate velit esse cillum dolore eu fugiat nulla
    pariatur. Excepteur sint occaecat cupidatat non prognata
    sunt in culpa qui officia deserunt mollit anim id est
    laborum.")
      (let ((flymake-diagnostic-functions
             (list 'error-backend 'warning-backend 'sync-backend
                   'panicking-backend
                   'crashing-backend
                   ))
            (flymake-wrap-around nil))
        (flymake-mode)

        (flymake-tests--assert-set (flymake-running-backends)
          (error-backend warning-backend panicking-backend)
          (crashing-backend))

        (flymake-tests--assert-set (flymake-disabled-backends)
          (crashing-backend)
          (error-backend warning-backend sync-backend
                         panicking-backend))

        (flymake-tests--wait-for-backends)

        (flymake-tests--assert-set (flymake-disabled-backends)
          (crashing-backend panicking-backend)
          (error-backend warning-backend sync-backend))

        (goto-char (point-min))
        (flymake-goto-next-error)
        (should (eq 'flymake-warning (face-at-point))) ; dolor
        (flymake-goto-next-error)
        (should (eq 'flymake-warning (face-at-point))) ; ut
        (flymake-goto-next-error)
        (should (eq 'flymake-error (face-at-point))) ; manha
        (flymake-goto-next-error)
        (should (eq 'flymake-warning (face-at-point))) ; Ut
        (flymake-goto-next-error)
        (should (eq 'flymake-note (face-at-point))) ; quis
        (flymake-goto-next-error)
        (should (eq 'flymake-warning (face-at-point))) ; ut
        (flymake-goto-next-error)
        (should (eq 'flymake-note (face-at-point))) ; commodo
        (flymake-goto-next-error)
        (should (eq 'flymake-warning (face-at-point))) ; dolor
        (flymake-goto-next-error)
        (should (eq 'flymake-error (face-at-point))) ; prognata
        (should-error (flymake-goto-next-error nil nil t))))))

(ert-deftest recurrent-backend ()
  "Test a backend that calls REPORT-FN multiple times"
  (with-temp-buffer
    (let (tick)
      (cl-letf
          (((symbol-function 'eager-backend)
            (lambda (report-fn)
              (funcall report-fn nil :explanation "very eager but no diagnostics")
              (display-buffer (current-buffer))
              (run-with-timer
               0.5 nil
               (lambda ()
                 (flymake-tests--diagnose-words report-fn :warning '("consectetur"))
                 (setq tick t)
                 (run-with-timer
                  0.5 nil
                  (lambda ()
                    (flymake-tests--diagnose-words report-fn :error '("fugiat"))
                    (setq tick t))))))))
        (insert "Lorem ipsum dolor sit amet, consectetur adipiscing
    elit, sed do eiusmod tempor incididunt ut labore et dolore
    manha aliqua. Ut enim ad minim veniam, quis nostrud
    exercitation ullamco laboris nisi ut aliquip ex ea commodo
    consequat. Duis aute irure dolor in reprehenderit in
    voluptate velit esse cillum dolore eu fugiat nulla
    pariatur. Excepteur sint occaecat cupidatat non prognata
    sunt in culpa qui officia deserunt mollit anim id est
    laborum.")
        (let ((flymake-diagnostic-functions
               (list 'eager-backend))
              (flymake-wrap-around nil))
          (flymake-mode)
          (flymake-tests--assert-set (flymake-running-backends)
            (eager-backend) ())
          (cl-loop until tick repeat 4 do (sleep-for 0.2))
          (setq tick nil)
          (goto-char (point-max))
          (flymake-goto-prev-error)
          (should (eq 'flymake-warning (face-at-point))) ; consectetur
          (should-error (flymake-goto-prev-error nil nil t))
          (cl-loop until tick repeat 4 do (sleep-for 0.2))
          (flymake-goto-next-error)
          (should (eq 'flymake-error (face-at-point))) ; fugiat
          (flymake-goto-prev-error)
          (should (eq 'flymake-warning (face-at-point))) ; back at consectetur
          (should-error (flymake-goto-prev-error nil nil t))
          )))))

(provide 'flymake-tests)

;;; flymake.el ends here
