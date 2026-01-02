;;; erc-scenarios-base-local-modules.el --- Local-module tests for ERC -*- lexical-binding: t -*-

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

;;; Commentary:

;; A local module doubles as a minor mode whose mode variable and
;; associated local data can withstand service disruptions.
;; Unfortunately, the current implementation is too unwieldy to be
;; promoted publicly because it doesn't perform any of the boiler
;; plate needed to save and restore buffer-local and "network-local"
;; copies of user options.  Ultimately, a user-friendly framework must
;; fill this void if third-party local modules are ever to become
;; practical.
;;
;; The following tests all use `sasl' because, as of ERC 5.5, it's the
;; only connection-oriented local module.  A fictitious
;; target-oriented module is defined below for testing purposes.

;;; Code:

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(require 'erc-sasl)

;; This asserts that a local module's options and its inclusion in
;; (and absence from) `erc-update-modules' can be let-bound.

(ert-deftest erc-scenarios-base-local-modules--reconnect-let ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "sasl")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'plain 'plain))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect with options let-bound")
      (with-current-buffer
          ;; This won't work unless the library is already loaded
          (let ((erc-modules (cons 'sasl erc-modules))
                (erc-sasl-mechanism 'plain)
                (erc-sasl-password "password123"))
            (erc :server "127.0.0.1"
                 :port port
                 :nick "tester"
                 :user "tester"
                 :full-name "tester"))
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "ExampleOrg"))

      (ert-info ("First connection succeeds")
        (funcall expect 10 "This server is in debug mode")
        (erc-cmd-QUIT "")
        (funcall expect 10 "finished"))

      (should-not (memq 'sasl erc-modules))
      (erc-d-t-wait-for 10 (not (erc-server-process-alive)))
      (erc-cmd-RECONNECT)

      (ert-info ("Second connection succeeds")
        (funcall expect 10 "This server is in debug mode")
        (erc-cmd-QUIT "")
        (funcall expect 10 "finished")))))

;; For local modules, the twin toggle commands `erc-FOO-enable' and
;; `erc-FOO-disable' affect all buffers of a connection, whereas
;; `erc-FOO-mode' continues to operate only on the current buffer.

(ert-deftest erc-scenarios-base-local-modules--toggle-helpers ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/local-modules")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'first 'second 'fourth))
       (port (process-contact dumb-server :service))
       (erc-modules (cons 'sasl erc-modules))
       (expect (erc-d-t-make-expecter))
       (server-buffer-name (format "127.0.0.1:%d" port)))

    (ert-info ("Initial authentication succeeds as expected")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :user "tester"
                                :password "changeme"
                                :full-name "tester")
        (should (string= (buffer-name) server-buffer-name))
        (funcall expect 10 "You are now logged in as tester"))

      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "foonet"))
        (funcall expect 10 "This server is in debug mode")
        (erc-cmd-JOIN "#chan")

        (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
          (funcall expect 20 "She is Lavinia, therefore must"))

        (erc-cmd-QUIT "")
        (funcall expect 10 "finished")))

    (ert-info ("Explicit disabling affects entire session")
      ;; Even though the mode variable is nil (but locally bound) in
      ;; this target buffer, disabling interactively with
      ;; `erc-sasl-disable', deactivates the module session-wide.
      (with-current-buffer "#chan"
        (should-not erc-sasl-mode)
        (should (local-variable-p 'erc-sasl-mode))
        (should (buffer-local-value 'erc-sasl-mode (get-buffer "foonet")))
        (call-interactively #'erc-sasl-disable)
        (should-not (buffer-local-value 'erc-sasl-mode (get-buffer "foonet")))
        (should-not erc-sasl-mode)
        (erc-cmd-RECONNECT)
        (funcall expect 10 "Some enigma, some riddle")
        (should-not erc-sasl-mode)
        (should (local-variable-p 'erc-sasl-mode)))

      (with-current-buffer "foonet"
        (should (local-variable-p 'erc-sasl-mode))
        (should-not erc-sasl-mode)
        (funcall expect 10 "User modes for tester`")
        (erc-cmd-QUIT "")
        (funcall expect 10 "finished")))

    (ert-info ("Enabling works from a target buffer")
      (with-current-buffer "#chan"
        (call-interactively #'erc-sasl-enable)
        (should (local-variable-p 'erc-sasl-mode))
        (should-not erc-sasl-mode)
        (should (buffer-local-value 'erc-sasl-mode (get-buffer "foonet")))
        (erc-cmd-RECONNECT)
        (funcall expect 10 "Well met; good morrow, Titus and Hortensius.")
        (erc-cmd-QUIT ""))

      (with-current-buffer "foonet"
        (should (local-variable-p 'erc-sasl-mode))
        (should erc-sasl-mode)
        (funcall expect 10 "User modes for tester")))))

(defvar-local erc-scenarios-base-local-modules--local-var nil)

(define-erc-module -phony-sblm- nil
  "Test module for `erc-scenarios-base-local-modules--var-persistence'."
  ((when-let* ((vars (or erc--server-reconnecting erc--target-priors)))
     (should (assq 'erc--phony-sblm--mode vars))
     (setq erc-scenarios-base-local-modules--local-var
           (alist-get 'erc-scenarios-base-local-modules--local-var vars)))
   (setq erc-scenarios-base-local-modules--local-var
         (or erc-scenarios-base-local-modules--local-var
             (if erc--target 100 0))))
  ((kill-local-variable 'erc-scenarios-base-local-modules--local-var))
  'local)

;; Note: this file has grown too expensive (time-wise) and must be
;; split up.  When that happens, this test should be rewritten without
;; any time-saving hacks, namely, server-initiated JOINs and an
;; absence of QUITs.  (That said, three connections in under 2 seconds
;; is pretty nice.)

(ert-deftest erc-scenarios-base-local-modules--var-persistence ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/reconnect")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'options 'options 'options))
       (port (process-contact dumb-server :service))
       (erc-modules (cons '-phony-sblm- (remq 'autojoin erc-modules)))
       (expect (erc-d-t-make-expecter))
       (server-buffer-name (format "127.0.0.1:%d" port)))

    (ert-info ("Initial authentication succeeds as expected")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "changeme"
                                :full-name "tester")
        (should (string= (buffer-name) server-buffer-name)))

      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "FooNet"))
        (funcall expect 10 "This server is in debug mode")
        (should erc--phony-sblm--mode)
        (should (eql erc-scenarios-base-local-modules--local-var 0))
        (setq erc-scenarios-base-local-modules--local-var 1)))

    (ert-info ("Save module's local var in target buffer")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (should (eql erc-scenarios-base-local-modules--local-var 100))
        (setq erc-scenarios-base-local-modules--local-var 101)
        (funcall expect 20 "welcome")))

    (with-current-buffer "FooNet" (funcall expect 20 "terminated"))

    (ert-info ("Vars reused when mode was left enabled")
      (with-current-buffer "#chan"
        (erc-cmd-RECONNECT)
        (funcall expect 20 "welcome")
        (should (eql erc-scenarios-base-local-modules--local-var 101))
        (erc--phony-sblm--mode -1))

      (with-current-buffer "FooNet"
        (funcall expect 10 "User modes for tester")
        (should (eql erc-scenarios-base-local-modules--local-var 1))))

    (with-current-buffer "FooNet" (funcall expect 20 "terminated"))

    (ert-info ("Local binding gone when mode disabled in target")
      (with-current-buffer "#chan"
        (erc-cmd-RECONNECT)
        (funcall expect 20 "welcome")
        (should-not erc--phony-sblm--mode)
        (should-not erc-scenarios-base-local-modules--local-var))

      ;; But value retained in server buffer, where mode is active.
      (with-current-buffer "FooNet"
        (funcall expect 10 "User modes for tester")
        (should (eql erc-scenarios-base-local-modules--local-var 1))))))

;;; erc-scenarios-base-local-modules.el ends here
