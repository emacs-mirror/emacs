;;; erc-scenarios-base-local-module-modes.el --- More local-mod ERC tests -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

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
;; made public because it doesn't perform any of the boiler plate
;; needed to save and restore buffer-local and "network-local" copies
;; of user options.  Ultimately, a user-friendly framework must fill
;; this void if third-party local modules are ever to become
;; practical.
;;
;; The following tests all use `sasl' because, as of ERC 5.5, it's the
;; only local module.

;;; Code:

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(require 'erc-sasl)

;; After quitting a session for which `sasl' is enabled, you
;; disconnect and toggle `erc-sasl-mode' off.  You then reconnect
;; using an alternate nickname.  You again disconnect and reconnect,
;; this time immediately, and the mode stays disabled.  Finally, you
;; once again disconnect, toggle the mode back on, and reconnect.  You
;; are authenticated successfully, just like in the initial session.
;;
;; This is meant to show that a user's local mode settings persist
;; between sessions.  It also happens to show (in round four, below)
;; that a server renicking a user on 001 after a 903 is handled just
;; like a user-initiated renick, although this is not the main thrust.

(ert-deftest erc-scenarios-base-local-module-modes--reconnect ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/local-modules")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'first 'second 'third 'fourth))
       (port (process-contact dumb-server :service))
       (erc-modules (cons 'sasl erc-modules))
       (expect (erc-d-t-make-expecter))
       (server-buffer-name (format "127.0.0.1:%d" port)))

    (ert-info ("Round one, initial authentication succeeds as expected")
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

    (ert-info ("Round two, nick rejected, alternate granted")
      (with-current-buffer "foonet"

        (ert-info ("Toggle mode off, reconnect")
          (erc-sasl-mode -1)
          (erc-cmd-RECONNECT))

        (funcall expect 10 "User modes for tester`")
        (should-not (cdr (erc-scenarios-common-buflist "foonet")))
        (should (equal (buffer-name) "foonet"))
        (should-not (cdr (erc-scenarios-common-buflist "#chan")))

        (with-current-buffer "#chan"
          (funcall expect 10 "Some enigma, some riddle"))

        (erc-cmd-QUIT "")
        (funcall expect 10 "finished")))

    (ert-info ("Round three, send alternate nick initially")
      (with-current-buffer "foonet"

        (ert-info ("Keep mode off, reconnect")
          (should-not erc-sasl-mode)
          (should (local-variable-p 'erc-sasl-mode))
          (erc-cmd-RECONNECT))

        (funcall expect 10 "User modes for tester`")
        (should-not (cdr (erc-scenarios-common-buflist "foonet")))
        (should (equal (buffer-name) "foonet"))
        (should-not (cdr (erc-scenarios-common-buflist "#chan")))

        (with-current-buffer "#chan"
          (funcall expect 10 "Let our reciprocal vows be remembered."))

        (erc-cmd-QUIT "")
        (funcall expect 10 "finished")))

    (ert-info ("Round four, authenticated successfully again")
      (with-current-buffer "foonet"

        (ert-info ("Toggle mode on, reconnect")
          (should-not erc-sasl-mode)
          (should (local-variable-p 'erc-sasl-mode))
          (erc-sasl-mode +1)
          (erc-cmd-RECONNECT))

        (funcall expect 10 "User modes for tester")
        (should-not (cdr (erc-scenarios-common-buflist "foonet")))
        (should (equal (buffer-name) "foonet"))
        (should-not (cdr (erc-scenarios-common-buflist "#chan")))

        (with-current-buffer "#chan"
          (funcall expect 10 "Well met; good morrow, Titus and Hortensius."))

        (erc-cmd-QUIT "")))))

;; In contrast to the mode-persistence test above, this one
;; demonstrates that a user reinvoking an entry point declares their
;; intention to reset local-module state for the server buffer.
;; Whether a local-module's state variable is also reset in target
;; buffers up to the module.  That is, by default, they're left alone.

(ert-deftest erc-scenarios-base-local-module-modes--entrypoint ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/local-modules")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'first 'first))
       (port (process-contact dumb-server :service))
       (erc-modules (cons 'sasl erc-modules))
       (expect (erc-d-t-make-expecter))
       (server-buffer-name (format "127.0.0.1:%d" port)))

    (ert-info ("Round one, initial authentication succeeds as expected")
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

        (ert-info ("Toggle local-module off in target buffer")
          (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
            (funcall expect 20 "She is Lavinia, therefore must")
            (erc-sasl-mode -1)))

        (erc-cmd-QUIT "")
        (funcall expect 10 "finished")

        (ert-info ("Toggle mode off")
          (erc-sasl-mode -1)
          (should (local-variable-p 'erc-sasl-mode)))))

    (ert-info ("Reconnecting via entry point discards `erc-sasl-mode' value.")
      ;; If you were to /RECONNECT here, no PASS changeme would be
      ;; sent instead of CAP SASL, resulting in a failure.
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :user "tester"
                                :password "changeme"
                                :full-name "tester")
        (should (string= (buffer-name) server-buffer-name))
        (funcall expect 10 "You are now logged in as tester")

        (erc-d-t-wait-for 10 (equal (buffer-name) "foonet"))
        (funcall expect 10 "User modes for tester")
        (should erc-sasl-mode)) ; obviously

      ;; No other foonet buffer exists, e.g., foonet<2>
      (should-not (cdr (erc-scenarios-common-buflist "foonet")))

      (ert-info ("Target buffer retains local-module state")
        (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
          (funcall expect 20 "She is Lavinia, therefore must")
          (should-not erc-sasl-mode)
          (should (local-variable-p 'erc-sasl-mode))
          (erc-cmd-QUIT ""))))))

;;; erc-scenarios-base-local-module-modes.el ends here
