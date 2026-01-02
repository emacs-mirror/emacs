;;; erc-scenarios-services-misc.el --- Services-misc scenarios -*- lexical-binding: t -*-

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

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(eval-when-compile (require 'erc-join)
                   (require 'erc-services))

(ert-deftest erc-scenarios-services-password ()
  :tags '(:expensive-test)

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "services/password")
       (erc-server-flood-penalty 0.1)
       (erc-modules (cons 'services erc-modules))
       (erc-nickserv-passwords '((Libera.Chat (("joe" . "bar")
                                               ("tester" . "changeme")))))
       (expect (erc-d-t-make-expecter))
       (dumb-server (erc-d-run "localhost" t 'libera))
       (port (process-contact dumb-server :service)))

    (ert-info ("Connect without password")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (erc-d-t-wait-for 5 (eq erc-network 'Libera.Chat))
        (funcall expect 5 "This nickname is registered.")
        (funcall expect 2 "You are now identified")
        (funcall expect 1 "Last login from")
        (erc-cmd-QUIT "")))

    (erc-services-mode -1)

    (should-not (memq 'services erc-modules))))

(ert-deftest erc-scenarios-services-prompt ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "services/password")
       (erc-server-flood-penalty 0.1)
       (inhibit-interaction nil)
       (erc-modules (cons 'services erc-modules))
       (expect (erc-d-t-make-expecter))
       (dumb-server (erc-d-run "localhost" t 'libera))
       (port (process-contact dumb-server :service)))

    (ert-info ("Connect without password")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (let ((inhibit-message noninteractive))
          (ert-simulate-keys "changeme\r"
            (erc-d-t-wait-for 10 (eq erc-network 'Libera.Chat))
            (funcall expect 3 "This nickname is registered.")
            (funcall expect 3 "You are now identified")
            (funcall expect 3 "Last login from")))
        (erc-cmd-QUIT "")))

    (erc-services-mode -1)

    (should-not (memq 'services erc-modules))))

;; A user with `services' enabled connects, quits, and reconnects.  An
;; entry in their netrc matches the network ID, which isn't known when
;; `erc-auth-source-server-function' runs -- initially *or* on
;; reconnect.  It's only seen by `erc-auth-source-services-function'.

(ert-deftest erc-scenarios-services-auth-source-reconnect ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "services/auth-source")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'recon 'recon))
       (port (process-contact dumb-server :service))
       (netrc-file (make-temp-file
                    "auth-source-test" nil nil
                    "machine FooNet login tester password changeme\n"))
       (auth-sources (list netrc-file))
       (auth-source-do-cache nil)
       (erc-modules (cons 'services erc-modules))
       (erc-use-auth-source-for-nickserv-password t) ; do consult
       (erc-prompt-for-nickserv-password nil) ; don't prompt
       (erc-nickserv-alist
        (cons '(FooNet
                "NickServ!NickServ@services.int"
                "This nickname is registered. Please choose"
                "NickServ" "IDENTIFY" nil nil "You are now identified for ")
              erc-nickserv-alist))
       (expect (erc-d-t-make-expecter))
       (erc-scenarios-common-extra-teardown (lambda ()
                                              (delete-file netrc-file))))

    (ert-info ("Server password omitted from initial connection")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :user "tester"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (ert-info ("Services module authenticates")
          (funcall expect 10 "This nickname is registered.")
          (funcall expect 3 "You are now identified"))
        (erc-cmd-JOIN "#chan")
        (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
          (funcall expect 10 "the gallants desire it"))
        (erc-cmd-QUIT "")
        (funcall expect 3 "finished")))

    (ert-info ("Server password withheld on reconnect")
      (with-current-buffer "#chan"
        (erc-cmd-RECONNECT))
      (with-current-buffer "FooNet"
        (funcall expect 10 "This nickname is registered.")
        (funcall expect 3 "You are now identified")
        (with-current-buffer "#chan" ; autojoined
          (funcall expect 10 "the gallants desire it"))
        (erc-cmd-QUIT "")
        (funcall expect 3 "finished")))

    (erc-services-mode -1)))

;; The server rejects your nick during registration, so ERC acquires a
;; placeholder and successfully renicks once the connection is up.
;; See also `erc-scenarios-base-renick-self-auto'.

(ert-deftest erc-scenarios-services-misc--reconnect-retry-nick ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-server-flood-penalty 0.1)
       (erc-scenarios-common-dialog "services/regain")
       (dumb-server (erc-d-run "localhost" t 'reconnect-retry
                               'reconnect-retry-again))
       (port (process-contact dumb-server :service))
       (erc-server-reconnect-function #'erc-server-delayed-reconnect)
       (erc-server-auto-reconnect t)
       (erc-modules `(services-regain sasl ,@erc-modules))
       (erc-services-regain-alist
        '((Libera.Chat . erc-services-retry-nick-on-connect)))
       (expect (erc-d-t-make-expecter)))

    ;; FIXME figure out and explain why this is so.
    (should (featurep 'erc-services))

    (ert-info ("Session succeeds but cut short")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :user "tester"
                                :password "changeme"
                                :full-name "tester")
        (funcall expect 10 "Last login from")
        (erc-cmd-JOIN "#test")))

    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#test"))
      (funcall expect 10 "was created on"))

    (ert-info ("Service restored")
      (with-current-buffer "Libera.Chat"
        (erc-d-t-wait-for 10 erc--server-reconnect-timer)
        (funcall expect 10 "Connection failed!")
        (funcall expect 10 "already in use")
        (funcall expect 10 "changed mode for tester`")
        (funcall expect 10 "Last login from")
        (funcall expect 10 "Your new nickname is tester")))

    (with-current-buffer "#test"
      (funcall expect 10 "tester ")
      (funcall expect 10 "was created on"))))

;; This only asserts that the handler fires and issues the right
;; NickServ command, but it doesn't accurately recreate a
;; disconnection, but it probably should.
(ert-deftest erc-scenarios-services-misc--regain-command ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-server-flood-penalty 0.1)
       (erc-scenarios-common-dialog "services/regain")
       (dumb-server (erc-d-run "localhost" t 'taken-regain))
       (port (process-contact dumb-server :service))
       (erc-server-auto-reconnect t)
       (erc-modules `(services-regain sasl ,@erc-modules))
       (erc-services-regain-alist
        '((ExampleNet . erc-services-issue-regain)))
       (expect (erc-d-t-make-expecter)))

    (should (featurep 'erc-services)) ; see note in prior test

    (with-current-buffer (erc :server "127.0.0.1"
                              :port port
                              :nick "dummy"
                              :user "tester"
                              :password "changeme"
                              :full-name "tester"
                              :id 'ExampleNet)
      (funcall expect 10 "dummy is already in use, trying dummy`")
      (funcall expect 10 "You are now logged in as tester")
      (funcall expect 10 "-NickServ- dummy has been regained.")
      (funcall expect 10 "*** Your new nickname is dummy")
      ;; Works with "given" `:id'.
      (should (and (erc-network) (not (eq (erc-network) 'ExampleNet)))))))

(ert-deftest erc-scenarios-services-misc--regain-command/oftc ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-server-flood-penalty 0.1)
       (erc-scenarios-common-dialog "services/regain")
       (dumb-server (erc-d-run "localhost" t 'taken-regain-oftc))
       (port (process-contact dumb-server :service))
       (erc-modules `(services-regain ,@erc-modules))
       (erc-services-regain-timeout-seconds 1)
       (use-id-p (cl-evenp (truncate (float-time))))
       (erc-services-regain-alist (list (cons (if use-id-p 'oftc 'OFTC)
                                              #'erc-services-issue-regain)))
       (expect (erc-d-t-make-expecter)))

    (with-current-buffer (erc :server "127.0.0.1"
                              :port port
                              :nick "dummy"
                              :user "tester"
                              :full-name "tester"
                              :id (and use-id-p 'oftc))
      (funcall expect 10 "Nickname dummy is already in use, trying dummy`")
      (funcall expect 10 "-NickServ- REGAIN succeed on nickname")
      (funcall expect 10 "*** Your new nickname is dummy")
      (funcall expect 10 "*** dummy has changed mode for dummy to +R"))))

(ert-deftest erc-scenarios-services-misc--ghost-and-retry-nick ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-server-flood-penalty 0.1)
       (erc-scenarios-common-dialog "services/regain")
       (dumb-server (erc-d-run "localhost" t 'taken-ghost))
       (port (process-contact dumb-server :service))
       (erc-server-auto-reconnect t)
       (erc-modules `(services-regain sasl ,@erc-modules))
       (erc-services-regain-alist
        '((FooNet . erc-services-issue-ghost-and-retry-nick)))
       (expect (erc-d-t-make-expecter)))

    (should (featurep 'erc-services)) ; see note in prior test

    (with-current-buffer (erc :server "127.0.0.1"
                              :port port
                              :nick "dummy"
                              :user "tester"
                              :password "changeme"
                              :full-name "tester")
      (funcall expect 10 "dummy is already in use, trying dummy`")
      (funcall expect 10 "You are now logged in as tester")
      (funcall expect 10 "-NickServ- dummy has been ghosted.")
      (funcall expect 10 "*** Your new nickname is dummy"))))

;;; erc-scenarios-services-misc.el ends here
