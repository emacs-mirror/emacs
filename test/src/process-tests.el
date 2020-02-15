;;; process-tests.el --- Testing the process facilities

;; Copyright (C) 2013-2020 Free Software Foundation, Inc.

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
(require 'puny)

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

(ert-deftest set-process-filter-t ()
  "Test setting process filter to t and back." ;; Bug#36591
  (with-temp-buffer
    (let* ((print-level nil)
           (print-length nil)
           (proc (start-process
                  "test proc" (current-buffer)
                  (concat invocation-directory invocation-name)
                  "-Q" "--batch" "--eval"
                  (prin1-to-string
                   '(let ((s nil) (count 0))
                      (while (setq s (read-from-minibuffer
                                      (format "%d> " count)))
                        (princ s)
                        (princ "\n")
                        (setq count (1+ count))))))))
      (set-process-query-on-exit-flag proc nil)
      (send-string proc "one\n")
      (while (not (equal (buffer-substring
                          (line-beginning-position) (point-max))
                         "1> "))
        (accept-process-output proc))   ; Read "one".
      (should (equal (buffer-string) "0> one\n1> "))
      (set-process-filter proc t)       ; Stop reading from proc.
      (send-string proc "two\n")
      (should-not
       (accept-process-output proc 1))  ; Can't read "two" yet.
      (should (equal (buffer-string) "0> one\n1> "))
      (set-process-filter proc nil)     ; Resume reading from proc.
      (while (not (equal (buffer-substring
                          (line-beginning-position) (point-max))
                         "2> "))
        (accept-process-output proc))   ; Read "Two".
      (should (equal (buffer-string) "0> one\n1> two\n2> ")))))

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

(ert-deftest make-process/noquery-stderr ()
  "Checks that Bug#30031 is fixed."
  (skip-unless (executable-find "sleep"))
  (with-temp-buffer
    (let* ((previous-processes (process-list))
           (process (make-process :name "sleep"
                                  :command '("sleep" "1h")
                                  :noquery t
                                  :connection-type 'pipe
                                  :stderr (current-buffer))))
      (unwind-protect
          (let ((new-processes (cl-set-difference (process-list)
                                                  previous-processes
                                                  :test #'eq)))
            (should new-processes)
            (dolist (process new-processes)
              (should-not (process-query-on-exit-flag process))))
        (kill-process process)))))

;; Return t if OUTPUT could have been generated by merging the INPUTS somehow.
(defun process-tests--mixable (output &rest inputs)
  (while (and output (let ((ins inputs))
                       (while (and ins (not (eq (car (car ins)) (car output))))
                         (setq ins (cdr ins)))
                       (if ins
                           (setcar ins (cdr (car ins))))
                       ins))
    (setq output (cdr output)))
  (not (apply #'append output inputs)))

(ert-deftest make-process/mix-stderr ()
  "Check that `make-process' mixes the output streams if STDERR is nil."
  (skip-unless (executable-find "bash"))
  ;; Frequent random (?) failures on hydra.nixos.org, with no process output.
  ;; Maybe this test should be tagged unstable?  See bug#31214.
  (skip-unless (not (getenv "EMACS_HYDRA_CI")))
  (with-temp-buffer
    (let ((process (make-process
                    :name "mix-stderr"
                    :command (list "bash" "-c"
                                   "echo stdout && echo stderr >&2")
                    :buffer (current-buffer)
                    :sentinel #'ignore
                    :noquery t
                    :connection-type 'pipe)))
      (while (or (accept-process-output process)
		 (process-live-p process)))
      (should (eq (process-status process) 'exit))
      (should (eq (process-exit-status process) 0))
      (should (process-tests--mixable (string-to-list (buffer-string))
                                      (string-to-list "stdout\n")
                                      (string-to-list "stderr\n"))))))

(ert-deftest make-process-w32-debug-spawn-error ()
  "Check that debugger runs on `make-process' failure (Bug#33016)."
  (skip-unless (eq system-type 'windows-nt))
  (let* ((debug-on-error t)
         (have-called-debugger nil)
         (debugger (lambda (&rest _)
                     (setq have-called-debugger t)
                     ;; Allow entering the debugger later in the same
                     ;; test run, before going back to the command
                     ;; loop.
                     (setq internal-when-entered-debugger -1))))
    (should (eq :got-error ;; NOTE: `should-error' would inhibit debugger.
                (condition-case-unless-debug ()
                    ;; Emacs doesn't search for absolute filenames, so
                    ;; the error will be hit in the w32 process spawn
                    ;; code.
                    (make-process :name "test" :command '("c:/No-Such-Command"))
                  (error :got-error))))
    (should have-called-debugger)))

(ert-deftest make-process/file-handler/found ()
  "Check that the ‘:file-handler’ argument of ‘make-process’
works as expected if a file name handler is found."
  (let ((file-handler-calls 0))
    (cl-flet ((file-handler
               (&rest args)
               (should (equal default-directory "test-handler:/dir/"))
               (should (equal args '(make-process :name "name"
                                                  :command ("/some/binary")
                                                  :file-handler t)))
               (cl-incf file-handler-calls)
               'fake-process))
      (let ((file-name-handler-alist (list (cons (rx bos "test-handler:")
                                                 #'file-handler)))
            (default-directory "test-handler:/dir/"))
        (should (eq (make-process :name "name"
                                  :command '("/some/binary")
                                  :file-handler t)
                    'fake-process))
        (should (= file-handler-calls 1))))))

(ert-deftest make-process/file-handler/not-found ()
  "Check that the ‘:file-handler’ argument of ‘make-process’
works as expected if no file name handler is found."
  (let ((file-name-handler-alist ())
        (default-directory invocation-directory)
        (program (expand-file-name invocation-name invocation-directory)))
    (should (processp (make-process :name "name"
                                    :command (list program "--version")
                                    :file-handler t)))))

(ert-deftest make-process/file-handler/disable ()
  "Check ‘make-process’ works as expected if it shouldn’t use the
file name handler."
  (let ((file-name-handler-alist (list (cons (rx bos "test-handler:")
                                             #'process-tests--file-handler)))
        (default-directory "test-handler:/dir/")
        (program (expand-file-name invocation-name invocation-directory)))
    (should (processp (make-process :name "name"
                                    :command (list program "--version"))))))

(defun process-tests--file-handler (operation &rest _args)
  (cl-ecase operation
    (unhandled-file-name-directory "/")
    (make-process (ert-fail "file name handler called unexpectedly"))))

(put #'process-tests--file-handler 'operations
     '(unhandled-file-name-directory make-process))

(ert-deftest make-process/stop ()
  "Check that `make-process' doesn't accept a `:stop' key.
See Bug#30460."
  (should-error
   (make-process :name "test"
                 :command (list (expand-file-name invocation-name
                                                  invocation-directory))
                 :stop t)))

;; All the following tests require working DNS, which appears not to
;; be the case for hydra.nixos.org, so disable them there for now.

(ert-deftest lookup-family-specification ()
  "network-lookup-address-info should only accept valid family symbols."
  (skip-unless (not (getenv "EMACS_HYDRA_CI")))
  (should-error (network-lookup-address-info "google.com" 'both))
  (should (network-lookup-address-info "google.com" 'ipv4))
  (when (featurep 'make-network-process '(:family ipv6))
    (should (network-lookup-address-info "google.com" 'ipv6))))

(ert-deftest lookup-unicode-domains ()
  "Unicode domains should fail"
  (skip-unless (not (getenv "EMACS_HYDRA_CI")))
  (should-error (network-lookup-address-info "faß.de"))
  (should (network-lookup-address-info (puny-encode-domain "faß.de"))))

(ert-deftest unibyte-domain-name ()
  "Unibyte domain names should work"
  (skip-unless (not (getenv "EMACS_HYDRA_CI")))
  (should (network-lookup-address-info (string-to-unibyte "google.com"))))

(ert-deftest lookup-google ()
  "Check that we can look up google IP addresses"
  (skip-unless (not (getenv "EMACS_HYDRA_CI")))
  (let ((addresses-both (network-lookup-address-info "google.com"))
        (addresses-v4 (network-lookup-address-info "google.com" 'ipv4)))
    (should addresses-both)
    (should addresses-v4))
  (when (featurep 'make-network-process '(:family ipv6))
    (should (network-lookup-address-info "google.com" 'ipv6))))

(ert-deftest non-existent-lookup-failure ()
  (skip-unless (not (getenv "EMACS_HYDRA_CI")))
  "Check that looking up non-existent domain returns nil"
  (should (eq nil (network-lookup-address-info "emacs.invalid"))))

(provide 'process-tests)
;; process-tests.el ends here.
