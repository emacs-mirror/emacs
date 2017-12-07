;;; process-tests.el --- Testing the process facilities

;; Copyright (C) 2013-2017 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
  (skip-unless (executable-find "bash"))
  (should (process-test-sentinel-wait-function-working-p
           #'accept-process-output)))

(ert-deftest process-test-sentinel-sit-for ()
  (skip-unless (executable-find "bash"))
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
              (insert "@echo arg1=%1, arg2=%2\n"))
            (with-temp-buffer
              (call-process batfile nil '(t t) t "x &y")
              (should (string= (buffer-string) "arg1=\"x &y\", arg2=\n")))
            (with-temp-buffer
              (call-process-shell-command
               (mapconcat #'shell-quote-argument (list batfile "x &y") " ")
               nil '(t t) t)
              (should (string= (buffer-string) "arg1=\"x &y\", arg2=\n"))))
        (when batfile (delete-file batfile))))))

(ert-deftest process-test-stderr-buffer ()
  (skip-unless (executable-find "bash"))
  (let* ((stdout-buffer (generate-new-buffer "*stdout*"))
	 (stderr-buffer (generate-new-buffer "*stderr*"))
	 (proc (make-process :name "test"
			     :command (list "bash" "-c"
					    (concat "echo hello stdout!; "
						    "echo hello stderr! >&2; "
						    "exit 20"))
			     :buffer stdout-buffer
			     :stderr stderr-buffer))
	 (sentinel-called nil)
	 (start-time (float-time)))
    (set-process-sentinel proc (lambda (proc msg)
				 (setq sentinel-called t)))
    (while (not (or sentinel-called
		    (> (- (float-time) start-time)
		       process-test-sentinel-wait-timeout)))
      (accept-process-output))
    (cl-assert (eq (process-status proc) 'exit))
    (cl-assert (= (process-exit-status proc) 20))
    (should (with-current-buffer stdout-buffer
	      (goto-char (point-min))
	      (looking-at "hello stdout!")))
    (should (with-current-buffer stderr-buffer
	      (goto-char (point-min))
	      (looking-at "hello stderr!")))))

(ert-deftest process-test-stderr-filter ()
  (skip-unless (executable-find "bash"))
  (let* ((sentinel-called nil)
	 (stderr-sentinel-called nil)
	 (stdout-output nil)
	 (stderr-output nil)
	 (stdout-buffer (generate-new-buffer "*stdout*"))
	 (stderr-buffer (generate-new-buffer "*stderr*"))
	 (stderr-proc (make-pipe-process :name "stderr"
					 :buffer stderr-buffer))
	 (proc (make-process :name "test" :buffer stdout-buffer
			     :command (list "bash" "-c"
					    (concat "echo hello stdout!; "
						    "echo hello stderr! >&2; "
						    "exit 20"))
			     :stderr stderr-proc))
	 (start-time (float-time)))
    (set-process-filter proc (lambda (proc input)
			       (push input stdout-output)))
    (set-process-sentinel proc (lambda (proc msg)
				 (setq sentinel-called t)))
    (set-process-filter stderr-proc (lambda (proc input)
				      (push input stderr-output)))
    (set-process-sentinel stderr-proc (lambda (proc input)
					(setq stderr-sentinel-called t)))
    (while (not (or sentinel-called
		    (> (- (float-time) start-time)
		       process-test-sentinel-wait-timeout)))
      (accept-process-output))
    (cl-assert (eq (process-status proc) 'exit))
    (cl-assert (= (process-exit-status proc) 20))
    (should sentinel-called)
    (should (equal 1 (with-current-buffer stdout-buffer
		       (point-max))))
    (should (equal "hello stdout!\n"
		   (mapconcat #'identity (nreverse stdout-output) "")))
    (should stderr-sentinel-called)
    (should (equal 1 (with-current-buffer stderr-buffer
		       (point-max))))
    (should (equal "hello stderr!\n"
		   (mapconcat #'identity (nreverse stderr-output) "")))))

(ert-deftest start-process-should-not-modify-arguments ()
  "`start-process' must not modify its arguments in-place."
  ;; See bug#21831.
  (let* ((path (pcase system-type
                 ((or 'windows-nt 'ms-dos)
                  ;; Make sure the file name uses forward slashes.
                  ;; The original bug was that 'start-process' would
                  ;; convert forward slashes to backslashes.
                  (expand-file-name (executable-find "attrib.exe")))
                 (_ "/bin//sh")))
         (samepath (copy-sequence path)))
    ;; Make sure 'start-process' actually goes all the way and invokes
    ;; the program.
    (should (process-live-p (condition-case nil
                                (start-process "" nil path)
                              (error nil))))
    (should (equal path samepath))))

(provide 'process-tests)
;; process-tests.el ends here.
