;;; erc-scenarios-base-association-nick.el --- base assoc scenarios -*- lexical-binding: t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.
;;
;; This file is part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(eval-when-compile (require 'erc-join))

;; You register a new nick, disconnect, and log back in, but your nick
;; is not granted, so ERC obtains a backtick'd version.  You open a
;; query buffer for NickServ, and ERC names it using the net-ID (which
;; includes the backtick'd nick) as a suffix.  The original
;; (disconnected) NickServ buffer gets renamed with *its* net-ID as
;; well.  You then identify to NickServ, and the dead session is no
;; longer considered distinct.

(ert-deftest erc-scenarios-base-association-nick-bumped ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/assoc/bumped")
       (dumb-server (erc-d-run "localhost" t 'renicked 'again))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (erc-server-flood-penalty 0.5)
       (erc-server-flood-margin 30))

    (ert-info ("Connect to foonet with nick tester")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (erc-scenarios-common-assert-initial-buf-name nil port)
        (erc-d-t-wait-for 5 (eq erc-network 'foonet))))

    (ert-info ("Create an account for tester and quit")
      (with-current-buffer "foonet"
        (funcall expect 3 "debug mode")

        (erc-cmd-QUERY "NickServ")
        (with-current-buffer "NickServ"
          (erc-scenarios-common-say "REGISTER changeme")
          (funcall expect 5 "Account created")
          (funcall expect 1 "You're now logged in as tester"))

        (with-current-buffer "foonet"
          (erc-cmd-QUIT "")
          (erc-d-t-wait-for 4 (not (erc-server-process-alive)))
          (funcall expect 5 "ERC finished"))))

    (with-current-buffer "foonet"
      (erc-cmd-RECONNECT))

    (erc-d-t-wait-for 10 "Nick request rejection prevents reassociation (good)"
      (get-buffer "foonet/tester`"))

    (ert-info ("Ask NickServ to change nick")
      (with-current-buffer "foonet/tester`"
        (funcall expect 3 "already in use")
        (funcall expect 3 "debug mode")
        (erc-cmd-QUERY "NickServ"))

      (erc-d-t-wait-for 1 "Dead NickServ query buffer renamed, now qualified"
        (get-buffer "NickServ@foonet/tester"))

      (with-current-buffer "NickServ@foonet/tester`" ; new one
        (erc-scenarios-common-say "IDENTIFY tester changeme")
        (funcall expect 5 "You're now logged in as tester")
        (ert-info ("Original buffer found, reused")
          (erc-d-t-wait-for 2 (equal (buffer-name) "NickServ")))))

    (ert-info ("Ours is the only NickServ buffer that remains")
      (should-not (cdr (erc-scenarios-common-buflist "NickServ"))))

    (ert-info ("Visible network ID truncated to one component")
      (should (not (get-buffer "foonet/tester`")))
      (should (not (get-buffer "foonet/tester")))
      (should (get-buffer "foonet")))))

;; A less common variant is when your bouncer switches to an alternate
;; nick while you're disconnected, and upon reconnecting, you get
;; a new nick.

(ert-deftest erc-scenarios-base-association-nick-bumped-mandated-renick ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/assoc/bumped")
       (dumb-server (erc-d-run "localhost" t 'foisted 'refoisted))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (erc-server-flood-penalty 0.5)
       (erc-server-flood-margin 30))

    (ert-info ("Connect to foonet with nick tester")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (erc-scenarios-common-assert-initial-buf-name nil port)
        (erc-d-t-wait-for 5 (eq erc-network 'foonet))))

    (ert-info ("Greet bob and quit")
      (with-current-buffer "foonet"
        (funcall expect 3 "debug mode")

        (erc-cmd-QUERY "bob")
        (with-current-buffer "bob"
          (erc-scenarios-common-say "hi")
          (funcall expect 5 "hola")
          (funcall expect 1 "how r u?"))

        (with-current-buffer "foonet"
          (erc-cmd-QUIT "")
          (erc-d-t-wait-for 4 (not (erc-server-process-alive)))
          (funcall expect 5 "ERC finished"))))

    ;; Since we use reconnect, a new buffer won't be created
    ;; TODO add variant with clean `erc' invocation
    (with-current-buffer "foonet"
      (erc-cmd-RECONNECT))

    (ert-info ("Server-initiated renick")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "foonet/dummy"))
        (should-not (get-buffer "foonet/tester"))
        (funcall expect 15 "debug mode"))

      (erc-d-t-wait-for 1 "Old query renamed, now qualified"
        (get-buffer "bob@foonet/tester"))

      (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "bob@foonet/dummy"))
        (erc-cmd-NICK "tester")
        (ert-info ("Buffers combined")
          (erc-d-t-wait-for 2 (equal (buffer-name) "bob")))))

    (with-current-buffer "foonet"
      (funcall expect 5 "You're now logged in as tester"))

    (ert-info ("Ours is the only bob buffer that remains")
      (should-not (cdr (erc-scenarios-common-buflist "bob"))))

    (ert-info ("Visible network ID truncated to one component")
      (should (not (get-buffer "foonet/dummy")))
      (should (get-buffer "foonet")))))

;;; erc-scenarios-base-association-nick.el ends here
