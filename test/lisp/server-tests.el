;;; server-tests.el --- Emacs server test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'server)
(require 'cl-lib)

(defconst server-tests/can-create-frames-p
  (and (not (memq system-type '(windows-nt ms-dos)))
       (not (member (getenv "TERM") '("dumb" "" nil)))
       (or (not (eq system-type 'cygwin))
           (featurep 'gfilenotify)
           (featurep 'dbus)
           (featurep 'threads)))
  "Non-nil if we can create a new frame in the tests.
Some tests below need to create new frames for the emacsclient.
However, this doesn't work on all platforms.  In particular,
MS-Windows fails to create frames from a batch Emacs session.
The same is true on Cygwin unless Emacs has at least one of the
features gfilenotify, dbus, or threads (bug#65325).  In cases
like that, we just skip the test.")

(defconst server-tests/max-wait-time 5
  "The maximum time to wait in `server-tests/wait-until', in seconds.")

(defconst server-tests/emacsclient
  (if installation-directory
      (expand-file-name "lib-src/emacsclient" installation-directory)
    emacsclient-program-name)
  "The emacsclient binary to test.")

(defmacro server-tests/wait-until (form)
  "Wait until FORM is non-nil, timing out and failing if it takes too long."
  `(let ((start (current-time)))
    (while (not ,form)
      (when (> (float-time (time-since start))
               server-tests/max-wait-time)
        (ert-fail (format "timed out waiting for %S to be non-nil" ',form)))
      (sit-for 0.1))))

(defun server-tests/start-client (args)
  "Run emacsclient, passing ARGS as arguments to it."
  (let ((server-file (process-get server-process :server-file))
        (buffer (generate-new-buffer "emacsclient")))
    (make-process
     :name server-tests/emacsclient
     :buffer buffer
     :command (append (list server-tests/emacsclient
                            (if server-use-tcp
                                "--server-file"
                              "--socket-name")
                            server-file)
                      args))))

(defmacro server-tests/with-server (&rest body)
  "Start the Emacs server, evaluate BODY, and then stop the server."
  (declare (indent 0))
  ;; Override the `server-name' so that these tests don't interfere
  ;; with any existing Emacs servers on the system.
  `(let* ((temporary-file-directory (file-name-as-directory
                                     (make-temp-file "server-tests" t)))
          (server-name (expand-file-name
                        "test-server" temporary-file-directory))
          (server-log t))
     (server-start)
     (ert-info ((lambda ()
                  (with-current-buffer (get-buffer-create server-buffer)
                    (buffer-string)))
                :prefix "Server logs: ")
       (unwind-protect
           (progn (should (processp server-process))
                  ,@body)
         (let ((inhibit-message t))
           (server-start t t))
         (delete-directory temporary-file-directory t)
         (should (null server-process))
         (should (null server-clients))))))

(defmacro server-tests/with-client (client-symbol args exit-status &rest body)
  "Start an Emacs client with ARGS and evaluate BODY.
This binds the client process to CLIENT-SYMBOL.  If EXIT-STATUS is
non-nil, then after BODY is evaluated, make sure the client
process's status matches it."
  (declare (indent 3))
  (let ((exit-status-symbol (make-symbol "exit-status"))
        (starting-client-count-symbol (make-symbol "starting-client-count")))
    `(let ((,starting-client-count-symbol (length server-clients))
           (,exit-status-symbol ,exit-status)
           (,client-symbol (server-tests/start-client ,args)))
       (ert-info ((lambda ()
                    (with-current-buffer (process-buffer ,client-symbol)
                      (buffer-string)))
                  :prefix "Client output: ")
         (server-tests/wait-until
          (or (= (length server-clients)
                 (1+ ,starting-client-count-symbol))
              (eq (process-status ,client-symbol) ,exit-status-symbol)))
         ,@body
         (when ,exit-status-symbol
           (server-tests/wait-until (eq (process-status ,client-symbol)
                                        ,exit-status-symbol)))))))

(defvar server-tests/variable nil)

;;; Tests:

(ert-deftest server-tests/server-start/sets-minor-mode ()
  "Ensure that calling `server-start' also sets `server-mode' properly."
  (server-tests/with-server
    ;; Make sure starting the server activates the minor mode.
    (should (eq server-mode t))
    (should (memq 'server-mode global-minor-modes)))
  ;; Make sure stopping the server deactivates the minor mode.
  (should (eq server-mode nil))
  (should-not (memq 'server-mode global-minor-modes)))

(ert-deftest server-tests/server-start/stop-prompt-with-client ()
  "Ensure that stopping the server prompts when there are clients."
  (skip-unless server-tests/can-create-frames-p)
  (server-tests/with-server
    (server-tests/with-client emacsclient '("-c") 'exit
      (should (length= (frame-list) 2))
      (cl-letf* ((yes-or-no-p-called nil)
                 ((symbol-function 'yes-or-no-p)
                  (lambda (_prompt)
                    (setq yes-or-no-p-called t))))
        (server-start t)
        (should yes-or-no-p-called)))))

(ert-deftest server-tests/server-start/no-stop-prompt-without-client ()
  "Ensure that stopping the server doesn't prompt when there are no clients."
  (server-tests/with-server
    (cl-letf* ((inhibit-message t)
               (yes-or-no-p-called nil)
               ((symbol-function 'yes-or-no-p)
                (lambda (_prompt)
                  (setq yes-or-no-p-called t))))
      (server-start t)
      (should-not yes-or-no-p-called))))

(ert-deftest server-tests/emacsclient/server-edit ()
  "Test that calling `server-edit' from a client buffer exits the client."
  (server-tests/with-server
    (server-tests/with-client emacsclient '("file.txt") 'exit
      (server-tests/wait-until (get-buffer "file.txt"))
      (should (eq (process-status emacsclient) 'run))
      (with-current-buffer "file.txt"
        (server-edit)))))

(ert-deftest server-tests/emacsclient/create-frame ()
  "Test that \"emacsclient -c\" creates a frame."
  (skip-unless server-tests/can-create-frames-p)
  (let ((starting-frame-count (length (frame-list))))
    (server-tests/with-server
      (server-tests/with-client emacsclient '("-c") nil
      (should (length= (frame-list) (1+ starting-frame-count)))
      (should (eq (process-status emacsclient) 'run))
      (should (eq (frame-parameter (car (frame-list)) 'client)
                  (car server-clients)))))
    ;; The client frame should go away after the server stops.
    (should (length= (frame-list) starting-frame-count))))

(ert-deftest server-tests/emacsclient/eval ()
  "Test that \"emacsclient --eval\" works correctly."
  (server-tests/with-server
    (let ((value (random)))
      (server-tests/with-client emacsclient
          (list "--eval" (format "(setq server-tests/variable %d)" value))
          'exit
        (should (= server-tests/variable value))))))

(ert-deftest server-tests/server-force-stop/keeps-frames ()
  "Ensure that `server-force-stop' doesn't delete frames.  See bug#58877.
Note: since that bug is about a behavior when killing Emacs, this
test is somewhat indirect. (Killing the current Emacs instance
would make it hard to check test results!)  Instead, it only
tests that `server-force-stop' doesn't delete frames (and even
then, requires a few tricks to run as a regression test).  So
long as this works, the problem in bug#58877 shouldn't occur."
  (skip-unless server-tests/can-create-frames-p)
  (let* ((starting-frames (frame-list))
         (starting-frame-count (length starting-frames))
         terminal)
    (unwind-protect
        (server-tests/with-server
          (server-tests/with-client emacsclient '("-c") 'exit
            (should (eq (process-status emacsclient) 'run))
            (should (length= (frame-list) (1+ starting-frame-count)))

            ;; Don't delete the terminal for the client; that would
            ;; kill its frame immediately too.  (This is only an issue
            ;; when running these tests via the command line;
            ;; normally, in an interactive session, we don't need to
            ;; worry about this.  But since we want to check that
            ;; `server-force-stop' doesn't delete frames under normal
            ;; circumstances, we need to bypass terminal deletion
            ;; here.)
            (setq terminal (process-get (car server-clients) 'terminal))
            (process-put (car server-clients) 'no-delete-terminal t)

            (server-force-stop))
          ;; Ensure we didn't delete the frame.
          (should (length= (frame-list) (1+ starting-frame-count))))
      ;; Clean up after ourselves and delete the terminal.
      (when (and terminal
                 (eq (terminal-live-p terminal) t)
                 (not (eq system-type 'windows-nt)))
        (delete-terminal terminal)))
    ;; If there are any new frames remaining, delete them.
    (mapc (lambda (frame) (delete-frame frame t))
          (cl-set-difference (frame-list) starting-frames))))

;;; server-tests.el ends here
