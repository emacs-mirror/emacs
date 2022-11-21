;;; server-tests.el --- Emacs server test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

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

(defconst server-tests/emacsclient
  (if installation-directory
      (expand-file-name "lib-src/emacsclient" installation-directory)
    "emacsclient")
  "The emacsclient binary to test.")

(defun server-tests/start-emacsclient (&rest args)
  "Run emacsclient, passing ARGS as arguments to it."
  (let ((socket-name (process-get server-process :server-file)))
    (make-process
     :name server-tests/emacsclient
     :command (append (list server-tests/emacsclient
                            "--socket-name" socket-name)
                      args))))

(defmacro server-tests/with-server (&rest body)
  "Start the Emacs server, evaluate BODY, and then stop the server."
  (declare (indent 0))
  `(progn
     (server-start)
     (unwind-protect
         (progn (should (processp server-process))
                ,@body)
       (let ((inhibit-message t))
         (server-start t t))
       (should (null server-process))
       (should (null server-clients)))))

(defconst server-tests/max-wait-time 5
  "The maximum time to wait in `server-tests/wait-until', in seconds.")

(defmacro server-tests/wait-until (form)
  "Wait until FORM is non-nil, timing out and failing if it takes too long."
  `(let ((start (current-time)))
    (while (not ,form)
      (when (> (float-time (time-since start))
               server-tests/max-wait-time)
        (ert-fail (format "timed out waiting for %S to be non-nil" ',form)))
      (sit-for 0.1))))

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
  (server-tests/with-server
    (let ((yes-or-no-p-called nil)
          (emacsclient (server-tests/start-emacsclient "-c")))
      (server-tests/wait-until (length= (frame-list) 2))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (_prompt)
                   (setq yes-or-no-p-called t))))
        (server-start t)
        (should yes-or-no-p-called))
      (server-tests/wait-until (eq (process-status emacsclient) 'exit)))))

(ert-deftest server-tests/server-start/no-stop-prompt-without-client ()
  "Ensure that stopping the server doesn't prompt when there are no clients."
  (server-tests/with-server
    (let ((yes-or-no-p-called nil))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (_prompt)
                   (setq yes-or-no-p-called t))))
        (let ((inhibit-message t))
          (server-start t))
        (should-not yes-or-no-p-called)))))

(ert-deftest server-tests/emacsclient/server-edit ()
  "Test that calling `server-edit' from a client buffer exits the client."
  (server-tests/with-server
    (let ((emacsclient (server-tests/start-emacsclient "file.txt")))
      (server-tests/wait-until (get-buffer "file.txt"))
      (should (eq (process-status emacsclient) 'run))
      (should (length= server-clients 1))
      (with-current-buffer "file.txt"
        (server-edit))
      (server-tests/wait-until (eq (process-status emacsclient) 'exit)))))

(ert-deftest server-tests/emacsclient/create-frame ()
  "Test that \"emacsclient -c\" creates a frame."
  (server-tests/with-server
    (let ((emacsclient (server-tests/start-emacsclient "-c")))
      (server-tests/wait-until (length= (frame-list) 2))
      (should (eq (process-status emacsclient) 'run))
      (should (length= server-clients 1))
      (should (eq (frame-parameter (car (frame-list)) 'client)
                  (car server-clients)))))
  ;; The client frame should go away after the server stops.
  (should (length= (frame-list) 1)))

(ert-deftest server-tests/emacsclient/eval ()
  "Test that \"emacsclient --eval\" works correctly."
  (server-tests/with-server
    (let ((value (random)))
      (server-tests/start-emacsclient
       "--eval" (format "(setq server-tests/variable %d)" value))
      (server-tests/wait-until (eq server-tests/variable value)))))

(ert-deftest server-tests/server-force-stop/keeps-frames ()
  "Ensure that `server-force-stop' doesn't delete frames.  See bug#58877.
Note: since that bug is about a behavior when killing Emacs, this
test is somewhat indirect. (Killing the current Emacs instance
would make it hard to check test results!)  Instead, it only
tests that `server-force-stop' doesn't delete frames (and even
then, requires a few tricks to run as a regression test).  So
long as this works, the problem in bug#58877 shouldn't occur."
  (let (terminal)
    (unwind-protect
        (server-tests/with-server
          (let ((emacsclient (server-tests/start-emacsclient "-c")))
            (server-tests/wait-until (length= (frame-list) 2))
            (should (eq (process-status emacsclient) 'run))

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
          (should (length= (frame-list) 2)))
      ;; Clean up after ourselves and delete the terminal.
      (when (and terminal
                 (eq (terminal-live-p terminal) t)
                 (not (eq system-type 'windows-nt)))
        (delete-terminal terminal)))))

;;; server-tests.el ends here
