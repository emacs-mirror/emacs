;;; process-tests.el --- Testing the process facilities -*- lexical-binding: t -*-

;; Copyright (C) 2013-2022 Free Software Foundation, Inc.

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

;;

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'puny)
(require 'subr-x)
(require 'dns)
(require 'url-http)

;; Timeout in seconds; the test fails if the timeout is reached.
(defvar process-test-sentinel-wait-timeout 2.0)

;; Start a process that exits immediately.  Call WAIT-FUNCTION,
;; possibly multiple times, to wait for the process to complete.
(defun process-test-sentinel-wait-function-working-p (wait-function)
  (let ((proc (start-process "test" nil "bash" "-c" "exit 20"))
	(sentinel-called nil)
	(start-time (float-time)))
    (set-process-sentinel proc (lambda (_proc _msg)
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
  (with-timeout (60 (ert-fail "Test timed out"))
  (should (process-test-sentinel-wait-function-working-p
           #'accept-process-output))))

(ert-deftest process-test-sentinel-sit-for ()
  (skip-unless (executable-find "bash"))
  (with-timeout (60 (ert-fail "Test timed out"))
  (should
   (process-test-sentinel-wait-function-working-p (lambda () (sit-for 0.01 t))))))

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
  (with-timeout (60 (ert-fail "Test timed out"))
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
    (set-process-sentinel proc (lambda (_proc _msg)
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
	      (looking-at "hello stderr!"))))))

(ert-deftest process-test-stderr-filter ()
  (skip-unless (executable-find "bash"))
  (with-timeout (60 (ert-fail "Test timed out"))
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
    (set-process-filter proc (lambda (_proc input)
			       (push input stdout-output)))
    (set-process-sentinel proc (lambda (_proc _msg)
				 (setq sentinel-called t)))
    (set-process-filter stderr-proc (lambda (_proc input)
				      (push input stderr-output)))
    (set-process-sentinel stderr-proc (lambda (_proc _input)
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
		   (mapconcat #'identity (nreverse stderr-output) ""))))))

(ert-deftest set-process-filter-t ()
  "Test setting process filter to t and back." ;; Bug#36591
  (with-timeout (60 (ert-fail "Test timed out"))
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
      (should (equal (buffer-string) "0> one\n1> two\n2> "))))))

(ert-deftest start-process-should-not-modify-arguments ()
  "`start-process' must not modify its arguments in-place."
  ;; See bug#21831.
  (with-timeout (60 (ert-fail "Test timed out"))
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
    (should (equal path samepath)))))

(ert-deftest make-process/noquery-stderr ()
  "Checks that Bug#30031 is fixed."
  (skip-unless (executable-find "sleep"))
  (with-timeout (60 (ert-fail "Test timed out"))
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
        (kill-process process))))))

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
  (with-timeout (60 (ert-fail "Test timed out"))
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
                                      (string-to-list "stderr\n")))))))

(ert-deftest make-process-w32-debug-spawn-error ()
  "Check that debugger runs on `make-process' failure (Bug#33016)."
  (skip-unless (eq system-type 'windows-nt))
  (with-timeout (60 (ert-fail "Test timed out"))
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
    (should have-called-debugger))))

(ert-deftest make-process/file-handler/found ()
  "Check that the `:file-handler’ argument of `make-process’
works as expected if a file name handler is found."
  (with-timeout (60 (ert-fail "Test timed out"))
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
        (should (= file-handler-calls 1)))))))

(ert-deftest make-process/file-handler/not-found ()
  "Check that the `:file-handler’ argument of `make-process’
works as expected if no file name handler is found."
  (with-timeout (60 (ert-fail "Test timed out"))
  (let ((file-name-handler-alist ())
        (default-directory invocation-directory)
        (program (expand-file-name invocation-name invocation-directory)))
    (should (processp (make-process :name "name"
                                    :command (list program "--version")
                                    :file-handler t))))))

(ert-deftest make-process/file-handler/disable ()
  "Check `make-process’ works as expected if it shouldn’t use the
file name handler."
  (with-timeout (60 (ert-fail "Test timed out"))
  (let ((file-name-handler-alist (list (cons (rx bos "test-handler:")
                                             #'process-tests--file-handler)))
        (default-directory "test-handler:/dir/")
        (program (expand-file-name invocation-name invocation-directory)))
    (should (processp (make-process :name "name"
                                    :command (list program "--version")))))))

(defun process-tests--file-handler (operation &rest _args)
  (cl-ecase operation
    (unhandled-file-name-directory "/")
    (make-process (ert-fail "file name handler called unexpectedly"))))

(put #'process-tests--file-handler 'operations
     '(unhandled-file-name-directory make-process))

(ert-deftest make-process/stop ()
  "Check that `make-process' doesn't accept a `:stop' key.
See Bug#30460."
  (with-timeout (60 (ert-fail "Test timed out"))
  (should-error
   (make-process :name "test"
                 :command (list (expand-file-name invocation-name
                                                  invocation-directory))
                 :stop t))))

;; The following tests require working DNS

;; This will need updating when IANA assign more IPv6 global ranges.
(defun ipv6-is-available ()
  (and (featurep 'make-network-process '(:family ipv6))
       (cl-rassoc-if
        (lambda (elt)
          (and (eq 9 (length elt))
               (= (logand (aref elt 0) #xe000) #x2000)))
        (network-interface-list))))

;; Check if the Internet seems to be working.  Mainly to pacify
;; Debian's CI system.
(defvar internet-is-working
  (progn
    (require 'dns)
    (dns-query "google.com")))

(ert-deftest lookup-family-specification ()
  "`network-lookup-address-info' should only accept valid family symbols."
  (skip-unless internet-is-working)
  (with-timeout (60 (ert-fail "Test timed out"))
  (should-error (network-lookup-address-info "localhost" 'both))
  (should (network-lookup-address-info "localhost" 'ipv4))
  (when (ipv6-is-available)
    (should (network-lookup-address-info "localhost" 'ipv6)))))

(ert-deftest lookup-unicode-domains ()
  "Unicode domains should fail."
  (skip-unless internet-is-working)
  (with-timeout (60 (ert-fail "Test timed out"))
  (should-error (network-lookup-address-info "faß.de"))
  (should (network-lookup-address-info (puny-encode-domain "faß.de")))))

(ert-deftest unibyte-domain-name ()
  "Unibyte domain names should work."
  (skip-unless internet-is-working)
  (with-timeout (60 (ert-fail "Test timed out"))
  (should (network-lookup-address-info (string-to-unibyte "google.com")))))

(ert-deftest lookup-google ()
  "Check that we can look up google IP addresses."
  (skip-unless internet-is-working)
  (with-timeout (60 (ert-fail "Test timed out"))
  (let ((addresses-both (network-lookup-address-info "google.com"))
        (addresses-v4 (network-lookup-address-info "google.com" 'ipv4)))
    (should addresses-both)
    (should addresses-v4))
  (when (and (ipv6-is-available)
             (dns-query "google.com" 'AAAA))
    (should (network-lookup-address-info "google.com" 'ipv6)))))

(ert-deftest non-existent-lookup-failure ()
  "Check that looking up non-existent domain returns nil."
  (skip-unless internet-is-working)
  (with-timeout (60 (ert-fail "Test timed out"))
  (should (eq nil (network-lookup-address-info "emacs.invalid")))))

;; End of tests requiring DNS

(defmacro process-tests--ignore-EMFILE (&rest body)
  "Evaluate BODY, ignoring EMFILE errors."
  (declare (indent 0) (debug t))
  (let ((err (make-symbol "err"))
        (message (make-symbol "message")))
    `(let ((,message (process-tests--EMFILE-message)))
       (condition-case ,err
           ,(macroexp-progn body)
         (file-error
          ;; If we couldn't determine the EMFILE message, just ignore
          ;; all `file-error' signals.
          (and ,message
               (not (string-equal (caddr ,err) ,message))
               (signal (car ,err) (cdr ,err))))))))

(defmacro process-tests--with-buffers (var &rest body)
  "Bind VAR to nil and evaluate BODY.
Afterwards, kill all buffers in the list VAR.  BODY should add
some buffer objects to VAR."
  (declare (indent 1) (debug (symbolp body)))
  (cl-check-type var symbol)
  `(let ((,var nil))
     (unwind-protect
         ,(macroexp-progn body)
       (mapc #'kill-buffer ,var))))

(defmacro process-tests--with-processes (var &rest body)
  "Bind VAR to nil and evaluate BODY.
Afterwards, delete all processes in the list VAR.  BODY should
add some process objects to VAR."
  (declare (indent 1) (debug (symbolp body)))
  (cl-check-type var symbol)
  `(let ((,var nil))
     (unwind-protect
         ,(macroexp-progn body)
       (mapc #'delete-process ,var))))

(defmacro process-tests--with-raised-rlimit (&rest body)
  "Evaluate BODY using a higher limit for the number of open files.
Attempt to set the resource limit for the number of open files
temporarily to the highest possible value."
  (declare (indent 0) (debug t))
  (let ((prlimit (make-symbol "prlimit"))
        (soft (make-symbol "soft"))
        (hard (make-symbol "hard"))
        (pid-arg (make-symbol "pid-arg")))
    `(let ((,prlimit (executable-find "prlimit"))
           (,pid-arg (format "--pid=%d" (emacs-pid)))
           (,soft nil) (,hard nil))
       (cl-flet ((set-limit
                  (value)
                  (cl-check-type value natnum)
                  (when ,prlimit
                    (call-process ,prlimit nil nil nil
                                  ,pid-arg
                                  (format "--nofile=%d:" value)))))
         (when ,prlimit
           (with-temp-buffer
             (when (eql (call-process ,prlimit nil t nil
                                      ,pid-arg "--nofile"
                                      "--raw" "--noheadings"
                                      "--output=SOFT,HARD")
                        0)
               (goto-char (point-min))
               (when (looking-at (rx (group (+ digit)) (+ blank)
                                     (group (+ digit)) ?\n))
                 (setq ,soft (string-to-number
                              (match-string-no-properties 1))
                       ,hard (string-to-number
                              (match-string-no-properties 2))))))
           (and ,soft ,hard (< ,soft ,hard)
                (set-limit ,hard)))
         (unwind-protect
             ,(macroexp-progn body)
           (when ,soft (set-limit ,soft)))))))

(defmacro process-tests--fd-setsize-test (&rest body)
  "Run BODY as a test for FD_SETSIZE overflow.
Try to generate pipe processes until we are close to the
FD_SETSIZE limit.  Within BODY, only a small number of file
descriptors should still be available.  Furthermore, raise the
maximum number of open files in the Emacs process above
FD_SETSIZE."
  (declare (indent 0) (debug t))
  (let ((process (make-symbol "process"))
        (processes (make-symbol "processes"))
        (buffer (make-symbol "buffer"))
        (buffers (make-symbol "buffers"))
        ;; FD_SETSIZE is typically 1024 on Unix-like systems.  On
        ;; MS-Windows we artificially limit FD_SETSIZE to 64, see the
        ;; commentary in w32proc.c.
        (fd-setsize (if (eq system-type 'windows-nt) 64 1024)))
    `(process-tests--with-raised-rlimit
       (process-tests--with-buffers ,buffers
         (process-tests--with-processes ,processes
           ;; First, allocate enough pipes to definitely exceed the
           ;; FD_SETSIZE limit.
           (cl-loop for i from 1 to ,(1+ fd-setsize)
                    for ,buffer = (generate-new-buffer
                                   (format " *pipe %d*" i))
                    do (push ,buffer ,buffers)
                    for ,process = (process-tests--ignore-EMFILE
                                     (make-pipe-process
                                      :name (format "pipe %d" i)
                                      ;; Prevent delete-process from
                                      ;; trying to read from pipe
                                      ;; processes that didn't exit
                                      ;; yet, because no one is
                                      ;; writing to those pipes, and
                                      ;; the read will stall.
                                      :stop (eq system-type 'windows-nt)
                                      :buffer ,buffer
                                      :coding 'no-conversion
                                      :noquery t))
                    while ,process
                    do (push ,process ,processes))
           (unless (cddr ,processes)
             (ert-fail "Couldn't allocate enough pipes"))
           ;; Delete two pipes to test more edge cases.
           (delete-process (pop ,processes))
           (delete-process (pop ,processes))
           ,@body)))))

(defmacro process-tests--with-temp-directory (var &rest body)
  "Bind VAR to the name of a new directory and evaluate BODY.
Afterwards, delete the directory."
  (declare (indent 1) (debug (symbolp body)))
  (cl-check-type var symbol)
  (let ((dir (make-symbol "dir")))
    `(let ((,dir (make-temp-file "emacs-test-" :dir)))
       (unwind-protect
           (let ((,var ,dir))
             ,@body)
         (delete-directory ,dir :recursive)))))

;; Tests for FD_SETSIZE overflow (Bug#24325).  The following tests
;; generate lots of process objects of the various kinds.  Running the
;; tests with assertions enabled should not result in any crashes due
;; to file descriptor set overflow.  These tests first generate lots
;; of unused pipe processes to fill up the file descriptor space.
;; Then, they create a few instances of the process type under test.

(ert-deftest process-tests/fd-setsize-no-crash/make-process ()
  "Check that Emacs doesn't crash when trying to use more than
FD_SETSIZE file descriptors (Bug#24325)."
  (with-timeout (60 (ert-fail "Test timed out"))
    (let ((cat (executable-find "cat")))
      (skip-unless cat)
      (dolist (conn-type '(pipe pty))
        (ert-info ((format "Connection type `%s'" conn-type))
          (process-tests--fd-setsize-test
            (process-tests--with-processes processes
              ;; Start processes until we exhaust the file descriptor
              ;; set size.  We assume that each process requires at
              ;; least one file descriptor.
              (dotimes (i 10)
                (let ((process
                       ;; Failure to allocate more file descriptors
                       ;; should signal `file-error', but not crash.
                       ;; Since we don't know the exact limit, we
                       ;; ignore `file-error'.
                       (process-tests--ignore-EMFILE
                         (make-process :name (format "test %d" i)
                                       :command (list cat)
                                       :connection-type conn-type
                                       :coding 'no-conversion
                                       :noquery t))))
                  (when process (push process processes))))
              ;; We should have managed to start at least one process.
              (should processes)
              (dolist (process processes)
                ;; The process now should either be running, or have
                ;; already failed before `exec'.
                (should (memq (process-status process) '(run exit)))
                (when (process-live-p process)
                  (process-send-eof process))
                (while (accept-process-output process))
                (should (eq (process-status process) 'exit))
                ;; If there's an error between fork and exec, Emacs
                ;; will use exit statuses between 125 and 127, see
                ;; process.h.  This can happen if the child process
                ;; tries to set up terminal device but fails due to
                ;; file number limits.  We don't treat this as an
                ;; error.
                (should (memql (process-exit-status process)
                               '(0 125 126 127)))))))))))

(ert-deftest process-tests/fd-setsize-no-crash/make-pipe-process ()
  "Check that Emacs doesn't crash when trying to use more than
FD_SETSIZE file descriptors (Bug#24325)."
  (with-timeout (60 (ert-fail "Test timed out"))
    (process-tests--fd-setsize-test
      (process-tests--with-buffers buffers
        (process-tests--with-processes processes
          ;; Start processes until we exhaust the file descriptor set
          ;; size.  We assume that each process requires at least one
          ;; file descriptor.
          (dotimes (i 10)
            (let ((buffer (generate-new-buffer (format " *%d*" i))))
              (push buffer buffers)
              (let ((process
                     ;; Failure to allocate more file descriptors
                     ;; should signal `file-error', but not crash.
                     ;; Since we don't know the exact limit, we ignore
                     ;; `file-error'.
                     (process-tests--ignore-EMFILE
                       (make-pipe-process :name (format "test %d" i)
                                          :buffer buffer
                                          :coding 'no-conversion
                                          :noquery t))))
                (when process (push process processes)))))
          ;; We should have managed to start at least one process.
          (should processes))))))

(ert-deftest process-tests/fd-setsize-no-crash/make-network-process ()
  "Check that Emacs doesn't crash when trying to use more than
FD_SETSIZE file descriptors (Bug#24325)."
  (skip-unless (featurep 'make-network-process '(:server t)))
  (skip-unless (featurep 'make-network-process '(:family local)))
  ;; Avoid hang due to connect/accept handshake on Cygwin (bug#49496).
  (skip-unless (not (eq system-type 'cygwin)))
  (with-timeout (60 (ert-fail "Test timed out"))
    (process-tests--with-temp-directory directory
      (process-tests--with-processes processes
        (let* ((num-clients 10)
               (socket-name (expand-file-name "socket" directory))
               ;; Run a UNIX server to connect to.
               (server (make-network-process :name "server"
                                             :server num-clients
                                             :buffer nil
                                             :service socket-name
                                             :family 'local
                                             :coding 'no-conversion
                                             :noquery t)))
          (push server processes)
          (process-tests--fd-setsize-test
            ;; Start processes until we exhaust the file descriptor
            ;; set size.  We assume that each process requires at
            ;; least one file descriptor.
            (dotimes (i num-clients)
              (let ((client
                     ;; Failure to allocate more file descriptors
                     ;; should signal `file-error', but not crash.
                     ;; Since we don't know the exact limit, we ignore
                     ;; `file-error'.
                     (process-tests--ignore-EMFILE
                       (make-network-process
                        :name (format "client %d" i)
                        :service socket-name
                        :family 'local
                        :coding 'no-conversion
                        :noquery t))))
                (when client (push client processes))))
            ;; We should have managed to start at least one process.
            (should processes)))))))

(ert-deftest process-tests/fd-setsize-no-crash/make-serial-process ()
  "Check that Emacs doesn't crash when trying to use more than
FD_SETSIZE file descriptors (Bug#24325)."
  ;; This test cannot be run if PTYs aren't supported.
  (skip-unless (not (eq system-type 'windows-nt)))
  (with-timeout (60 (ert-fail "Test timed out"))
    (process-tests--with-processes processes
      ;; In order to use `make-serial-process', we need to create some
      ;; pseudoterminals.  The easiest way to do that is to start a
      ;; normal process using the `pty' connection type.  We need to
      ;; ensure that the terminal stays around while we connect to it.
      ;; Create the host processes before the dummy pipes so we have a
      ;; high chance of succeeding here.
      (let ((sleep (executable-find "sleep"))
            (tty-names ()))
        (skip-unless sleep)
        (dotimes (i 10)
          (let* ((host (make-process :name (format "tty host %d" i)
                                     :command (list sleep "60")
                                     :buffer nil
                                     :coding 'utf-8-unix
                                     :connection-type 'pty
                                     :noquery t))
                 (tty-name (process-tty-name host)))
            (should (processp host))
            (push host processes)
            ;; FIXME: The assumption below that using :connection 'pty
            ;; in make-process necessarily produces a process with PTY
            ;; connection is unreliable and non-portable.
            ;; make-process can legitimately and silently fall back on
            ;; pipes if allocating a PTY fails (and on MS-Windows it
            ;; always fails).  The following code also assumes that
            ;; process-tty-name produces a file name that can be
            ;; passed to 'stat' and to make-serial-process, which is
            ;; also non-portable.
            (should tty-name)
            (should (file-exists-p tty-name))
            (should-not (member tty-name tty-names))
            (push tty-name tty-names)))
        (process-tests--fd-setsize-test
          (process-tests--with-processes processes
            (process-tests--with-buffers buffers
              (dolist (tty-name tty-names)
                (let ((buffer (generate-new-buffer
                               (format " *%s*" tty-name))))
                  (push buffer buffers)
                  ;; Failure to allocate more file descriptors should
                  ;; signal `file-error', but not crash.  Since we
                  ;; don't know the exact limit, we ignore
                  ;; `file-error'.
                  (let ((process (process-tests--ignore-EMFILE
                                   (make-serial-process
                                    :name (format "test %s" tty-name)
                                    :port tty-name
                                    :speed 9600
                                    :buffer buffer
                                    :coding 'no-conversion
                                    :noquery t))))
                    (when process (push process processes))))))
            ;; We should have managed to start at least one process.
            (should processes)))))))

(defvar process-tests--EMFILE-message :unknown
  "Cached result of the function `process-tests--EMFILE-message'.")

(defun process-tests--EMFILE-message ()
  "Return the error message for the EMFILE POSIX error.
Return nil if that can't be determined."
  (when (eq process-tests--EMFILE-message :unknown)
    (setq process-tests--EMFILE-message
          (with-temp-buffer
            (when (eql (ignore-error 'file-error
                         (call-process "errno" nil t nil "EMFILE"))
                       0)
              (goto-char (point-min))
              (when (looking-at (rx "EMFILE" (+ blank) (+ digit)
                                    (+ blank) (group (+ nonl))))
                (match-string-no-properties 1))))))
  process-tests--EMFILE-message)

(ert-deftest process-tests/sentinel-called ()
  "Check that sentinels are called after processes finish."
  (let ((command (process-tests--emacs-command)))
    (skip-unless command)
    (dolist (conn-type '(pipe pty))
      (ert-info ((format "Connection type: %s" conn-type))
        (process-tests--with-processes processes
          (let* ((calls ())
                 (process (make-process
                           :name "echo"
                           :command (process-tests--eval
                                     command '(print "first"))
                           :noquery t
                           :connection-type conn-type
                           :coding 'utf-8-unix
                           :sentinel (lambda (process message)
                                       (push (list process message)
                                             calls)))))
            (push process processes)
            (while (accept-process-output process))
            (should (equal calls
                           (list (list process "finished\n"))))))))))

(ert-deftest process-tests/sentinel-with-multiple-processes ()
  "Check that sentinels are called in time even when other processes
have written output."
  (let ((command (process-tests--emacs-command)))
    (skip-unless command)
    (dolist (conn-type '(pipe pty))
      (ert-info ((format "Connection type: %s" conn-type))
        (process-tests--with-processes processes
          (let* ((calls ())
                 (process (make-process
                           :name "echo"
                           :command (process-tests--eval
                                     command '(print "first"))
                           :noquery t
                           :connection-type conn-type
                           :coding 'utf-8-unix
                           :sentinel (lambda (process message)
                                       (push (list process message)
                                             calls)))))
            (push process processes)
            (push (make-process
                   :name "bash"
                   :command (process-tests--eval
                             command
                             '(progn (sleep-for 10) (print "second")))
                   :noquery t
                   :connection-type conn-type)
                  processes)
            (while (accept-process-output process))
            (should (equal calls
                           (list (list process "finished\n"))))))))))

(ert-deftest process-tests/multiple-threads-waiting ()
  (skip-unless (fboundp 'make-thread))
  (with-timeout (60 (ert-fail "Test timed out"))
    (process-tests--with-processes processes
      (let ((threads ())
            (cat (executable-find "cat")))
        (skip-unless cat)
        (dotimes (i 10)
          (let* ((name (format "test %d" i))
                 (process (make-process :name name
                                        :command (list cat)
                                        :coding 'no-conversion
                                        :noquery t
                                        :connection-type 'pipe)))
            (push process processes)
            (set-process-thread process nil)
            (push (make-thread
                   (lambda ()
                     (while (accept-process-output process)))
                   name)
                  threads)))
        (mapc #'process-send-eof processes)
        (cl-loop for process in processes
                 and thread in threads
                 do
                 (should-not (thread-join thread))
                 (should-not (thread-last-error))
                 (should (eq (process-status process) 'exit))
                 (should (eql (process-exit-status process) 0)))))))

(defun process-tests--eval (command form)
  "Return a command that evaluates FORM in an Emacs subprocess.
COMMAND must be a list returned by
`process-tests--emacs-command'."
  (let ((print-gensym t)
        (print-circle t)
        (print-length nil)
        (print-level nil)
        (print-escape-control-characters t)
        (print-escape-newlines t)
        (print-escape-multibyte t)
        (print-escape-nonascii t))
    `(,@command "--quick" "--batch" ,(format "--eval=%S" form))))

(defun process-tests--emacs-command ()
  "Return a command to reinvoke the current Emacs instance.
Return nil if that doesn't appear to be possible."
  (when-let ((binary (process-tests--emacs-binary))
             (dump (process-tests--dump-file)))
    (cons binary
          (unless (eq dump :not-needed)
            (list (concat "--dump-file="
                          (file-name-unquote dump)))))))

(defun process-tests--emacs-binary ()
  "Return the filename of the currently running Emacs binary.
Return nil if that can't be determined."
  (and (stringp invocation-name)
       (not (file-remote-p invocation-name))
       (not (file-name-absolute-p invocation-name))
       (stringp invocation-directory)
       (not (file-remote-p invocation-directory))
       (file-name-absolute-p invocation-directory)
       (when-let ((file (process-tests--usable-file-for-reinvoke
                         (expand-file-name invocation-name
                                           invocation-directory))))
         (and (file-executable-p file) file))))

(defun process-tests--dump-file ()
  "Return the filename of the dump file used to start Emacs.
Return nil if that can't be determined.  Return `:not-needed' if
Emacs wasn't started with a dump file."
  (if-let ((stats (and (fboundp 'pdumper-stats) (pdumper-stats))))
      (when-let ((file (process-tests--usable-file-for-reinvoke
                        (cdr (assq 'dump-file-name stats)))))
        (and (file-readable-p file) file))
    :not-needed))

(defun process-tests--usable-file-for-reinvoke (filename)
  "Return a version of FILENAME that can be used to reinvoke Emacs.
Return nil if FILENAME doesn't exist."
  (when (and (stringp filename)
             (not (file-remote-p filename)))
    (cl-callf file-truename filename)
    (and (stringp filename)
         (not (file-remote-p filename))
         (file-name-absolute-p filename)
         (file-regular-p filename)
         filename)))

;; Bug#46284
(ert-deftest process-sentinel-interrupt-event ()
  "Test that interrupting a process on Windows sends \"interrupt\" to sentinel."
  (skip-unless (eq system-type 'windows-nt))
  (with-temp-buffer
    (let* ((proc-buf (current-buffer))
	   ;; Start a new emacs process to wait idly until interrupted.
	   (cmd "emacs -batch --eval=\"(sit-for 50000)\"")
	   (proc (start-file-process-shell-command
                  "test/process-sentinel-signal-event" proc-buf cmd))
	   (events '()))

      ;; Capture any incoming events.
      (set-process-sentinel proc
                            (lambda (_prc event)
			      (push event events)))
      ;; Wait for the process to start.
      (sleep-for 2)
      (should (equal 'run (process-status proc)))
      ;; Interrupt the sub-process and wait for it to die.
      (interrupt-process proc)
      (sleep-for 2)
      ;; Should have received SIGINT...
      (should (equal 'signal (process-status proc)))
      (should (equal 2 (process-exit-status proc)))
      ;; ...and the change description should be "interrupt".
      (should (equal '("interrupt\n") events)))))

(ert-deftest process-async-https-with-delay ()
  "Bug#49449: asynchronous TLS connection with delayed completion."
  (skip-unless (and internet-is-working (gnutls-available-p)))
  (let* ((status nil)
         (buf (url-http
                 #s(url "https" nil nil "elpa.gnu.org" nil
                        "/packages/archive-contents" nil nil t silent t t)
                 (lambda (s) (setq status s))
                 '(nil) nil 'tls)))
    (unwind-protect
        (progn
          ;; Busy-wait for 1 s to allow for the TCP connection to complete.
          (let ((delay 1.0)
                (t0 (float-time)))
            (while (< (float-time) (+ t0 delay))))
          ;; Wait for the entire operation to finish.
          (let ((limit 4.0)
                (t0 (float-time)))
            (while (and (null status)
                        (< (float-time) (+ t0 limit)))
              (sit-for 0.1)))
          (should status)
          (should-not (assq :error status))
          (should buf)
          (should (> (buffer-size buf) 0))
          )
      (when buf
        (kill-buffer buf)))))

(ert-deftest process-num-processors ()
  "Sanity checks for num-processors."
  (should (equal (num-processors) (num-processors)))
  (should (integerp (num-processors)))
  (should (< 0 (num-processors))))

(provide 'process-tests)
;;; process-tests.el ends here
