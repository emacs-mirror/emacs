;;; erc-scenarios-misc-commands.el --- Misc commands for ERC -*- lexical-binding: t -*-

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

;;; Code:

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

;; This defends against a partial regression in which an /MOTD caused
;; 376 and 422 handlers in erc-networks to run.

(ert-deftest erc-scenarios-misc-commands--MOTD ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "commands")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'motd))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to server")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (funcall expect 10 "This is the default Ergo MOTD")
        (funcall expect 10 "debug mode")))

    (ert-info ("Send plain MOTD")
      (with-current-buffer "foonet"
        (erc-cmd-MOTD)
        (funcall expect -0.2 "Unexpected state detected")
        (funcall expect 10 "This is the default Ergo MOTD")))

    (ert-info ("Send MOTD with known target")
      (with-current-buffer "foonet"
        (erc-scenarios-common-say "/MOTD irc1.foonet.org")
        (funcall expect -0.2 "Unexpected state detected")
        (funcall expect 10 "This is the default Ergo MOTD")))

    (ert-info ("Send MOTD with erroneous target")
      (with-current-buffer "foonet"
        (erc-scenarios-common-say "/MOTD fake.foonet.org")
        (funcall expect -0.2 "Unexpected state detected")
        (funcall expect 10 "No such server")
        ;; Message may show up before the handler runs.
        (erc-d-t-wait-for 10
            (not (local-variable-p 'erc-server-402-functions)))
        (should-not (local-variable-p 'erc-server-376-functions))
        (should-not (local-variable-p 'erc-server-422-functions))
        (erc-cmd-QUIT "")))))


(ert-deftest erc-scenarios-misc-commands--SQUERY ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "commands")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'squery))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to server")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (funcall expect 10 "Your connection is secure")))

    (ert-info ("Send SQUERY")
      (with-current-buffer "IRCnet"
        (erc-scenarios-common-say "/SQUERY alis help list")
        (funcall expect -0.1 "Incorrect arguments")
        (funcall expect 10 "See also: HELP EXAMPLES")))))

;; Note that as of ERC 5.6, there is no actual slash-command function
;; named `erc-cmd-vhost'.  At the moment, this test merely exists to
;; assert that the `erc-server-396' response handler updates the rolls
;; correctly.
(ert-deftest erc-scenarios-misc-commands--VHOST ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "commands")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'vhost))
       ;; As of ERC 5.6, we must join a channel before ERC adds itself
       ;; to `erc-server-users'.  Without such an entry, there's
       ;; nothing to update when the 396 arrives.
       (erc-autojoin-channels-alist '((foonet "#chan")))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to server")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "changeme"
                                :full-name "tester")
        (funcall expect 10 "debug mode")))

    (ert-info ("Send VHOST")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (erc-scenarios-common-say "/VHOST tester changeme")
        (funcall expect 10 "visible host")
        (should (string= (erc-server-user-host (erc-get-server-user "tester"))
                         "some.host.test.cc"))))))

;; This tests four related slash commands, /AMSG, /GMSG, /AME, /GME,
;; the latter three introduced by bug#68401.  It mainly asserts
;; correct routing behavior, especially not sending or inserting
;; messages in buffers belonging to disconnected sessions.  Left
;; unaddressed are interactions with the `command-indicator' module
;; (`erc-noncommands-list') and whatever future `echo-message'
;; implementation manifests out of bug#49860.
(ert-deftest erc-scenarios-misc-commands--AMSG-GMSG-AME-GME ()
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "commands")
       (erc-server-flood-penalty 0.1)
       (dumb-server-foonet (erc-d-run "localhost" t "srv-foonet" 'amsg-foonet))
       (dumb-server-barnet (erc-d-run "localhost" t "srv-barnet" 'amsg-barnet))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to foonet and join #foo")
      (with-current-buffer
          (erc :server "127.0.0.1"
               :port (process-contact dumb-server-foonet :service)
               :nick "tester")
        (funcall expect 10 "debug mode")
        (erc-cmd-JOIN "#foo")))

    (ert-info ("Connect to barnet and join #bar")
      (with-current-buffer
          (erc :server "127.0.0.1"
               :port (process-contact dumb-server-barnet :service)
               :nick "tester")
        (funcall expect 10 "debug mode")
        (erc-cmd-JOIN "#bar")))

    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#foo"))
      (funcall expect 10 "welcome"))
    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#bar"))
      (funcall expect 10 "welcome"))

    (ert-info ("/AMSG only sent to issuing context's server")
      (with-current-buffer "foonet"
        (erc-scenarios-common-say "/amsg 1 foonet only"))
      (with-current-buffer "barnet"
        (erc-scenarios-common-say "/amsg 2 barnet only"))
      (with-current-buffer "#foo"
        (funcall expect 10 "<tester> 1 foonet only")
        (funcall expect 10 "<alice> bob: Our queen and all"))
      (with-current-buffer "#bar"
        (funcall expect 10 "<tester> 2 barnet only")
        (funcall expect 10 "<joe> mike: And secretly to greet")))

    (ert-info ("/AME only sent to issuing context's server")
      (with-current-buffer "foonet"
        (erc-scenarios-common-say "/ame 3 foonet only"))
      (with-current-buffer "barnet"
        (erc-scenarios-common-say "/ame 4 barnet only"))
      (with-current-buffer "#foo"
        (funcall expect 10 "* tester 3 foonet only")
        (funcall expect 10 "<alice> bob: You have discharged this"))
      (with-current-buffer "#bar"
        (funcall expect 10 "* tester 4 barnet only")
        (funcall expect 10 "<joe> mike: That same Berowne")))

    (ert-info ("/GMSG and /GME sent to all servers")
      (with-current-buffer "foonet"
        (erc-scenarios-common-say "/gmsg 5 all nets")
        (erc-scenarios-common-say "/gme 6 all nets"))
      (with-current-buffer "#bar"
        (funcall expect 10 "<tester> 5 all nets")
        (funcall expect 10 "* tester 6 all nets")
        (funcall expect 10 "<joe> mike: Mehercle! if their sons")))

    (ert-info ("/GMSG and /GME only sent to connected servers")
      (with-current-buffer "barnet"
        (erc-cmd-QUIT "")
        (funcall expect 10 "ERC finished"))
      (with-current-buffer "#foo"
        (funcall expect 10 "<tester> 5 all nets")
        (funcall expect 10 "* tester 6 all nets")
        (funcall expect 10 "<alice> bob: Stand you!"))
      (with-current-buffer "foonet"
        (erc-scenarios-common-say "/gmsg 7 all live nets")
        (erc-scenarios-common-say "/gme 8 all live nets"))
      ;; Message *not* inserted in disconnected buffer.
      (with-current-buffer "#bar"
        (funcall expect -0.1 "<tester> 7 all live nets")
        (funcall expect -0.1 "* tester 8 all live nets")))

    (with-current-buffer "#foo"
      (funcall expect 10 "<tester> 7 all live nets")
      (funcall expect 10 "* tester 8 all live nets")
      (funcall expect 10 "<bob> alice: Live, and be prosperous;"))))

;;; erc-scenarios-misc-commands.el ends here
