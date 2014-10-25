;;; process-tests.el --- Testing the process facilities

;; Copyright (C) 2013-2014 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)

;; Timeout in seconds; the test fails if the timeout is reached.
(defvar process-test-sentinel-wait-timeout 2.0)

;; Start a process that exits immediately.  Call WAIT-FUNCTION,
;; possibly multiple times, to wait for the process to complete.
(defun process-test-sentinel-wait-function-working-p (wait-function)
  (let ((proc (start-process "test" nil "bash" "-c" "exit 20"))
	(sentinel-called nil)
	(start-time (float-time)))
    (set-process-sentinel proc (lambda (proc msg)
				 (setq sentinel-called t)))
    (while (not (or sentinel-called
		    (> (- (float-time) start-time)
		       process-test-sentinel-wait-timeout)))
      (funcall wait-function))
    (cl-assert (eq (process-status proc) 'exit))
    (cl-assert (= (process-exit-status proc) 20))
    sentinel-called))

(ert-deftest process-test-sentinel-accept-process-output ()
  (should (process-test-sentinel-wait-function-working-p
           #'accept-process-output)))

(ert-deftest process-test-sentinel-sit-for ()
  (should
   (process-test-sentinel-wait-function-working-p (lambda () (sit-for 0.01 t)))))

(when (eq system-type 'windows-nt)
  (ert-deftest process-test-quoted-batfile ()
    "Check that Emacs hides CreateProcess deficiency (bug#18745)."
    (let (batfile)
      (unwind-protect
          (progn
            ;; CreateProcess will fail when both the bat file and 1st
            ;; argument are quoted, so include spaces in both of those
            ;; to force quoting.
            (setq batfile (make-temp-file "echo args" nil ".bat"))
            (with-temp-file batfile
              (insert "@echo arg1 = %1, arg2 = %2\n"))
            (with-temp-buffer
              (call-process batfile nil '(t t) t "x &y")
              (should (string= (buffer-string) "arg1 = \"x &y\", arg2 = \n")))
            (with-temp-buffer
              (call-process-shell-command
               (mapconcat #'shell-quote-argument (list batfile "x &y") " ")
               nil '(t t) t)
              (should (string= (buffer-string) "arg1 = \"x &y\", arg2 = \n"))))
        (when batfile (delete-file batfile))))))

(provide 'process-tests)
