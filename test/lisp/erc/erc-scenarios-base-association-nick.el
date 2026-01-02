;;; erc-scenarios-base-association-nick.el --- base assoc scenarios -*- lexical-binding: t -*-

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

(eval-when-compile (require 'erc-join))

;; You register a new nick in a dedicated query buffer, disconnect,
;; and log back in, but your nick is not granted (maybe you just
;; turned off SASL).  In any case, ERC obtains a backticked version.
;; You open a query buffer for NickServ, and ERC gives you the
;; existing one.  And after you identify, all buffers retain their
;; names, although your net ID has changed internally.
;;
;; If ERC had instead failed (or intentionally refused) to make the
;; association, you would find yourself with a new NickServ buffer
;; named with a suffix reflecting the new net ID (based on the
;; backticked nick), for example, NickServ@foonet/tester`.  And the
;; original (disconnected) NickServ buffer would also receive a suffix
;; with *its* net-ID, e.g., NickServ@foonet/tester.  Upon identifying
;; yourself, you'd see ERC merge both buffers along with their server
;; buffers.  While this alternate behavior might more accurately
;; reflect reality, it introduces significant inconvenience.  For a
;; clearer example, see the original version of this file introduced
;; by "Add user-oriented test scenarios for ERC".

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
      (erc-cmd-RECONNECT)
      (funcall expect 10 "User modes for tester`"))

    (ert-info ("Server buffer reassociated with new nick")
      (should-not (get-buffer "foonet/tester`")))

    (ert-info ("Ask NickServ to change nick")
      (with-current-buffer "foonet"
        (funcall expect 3 "debug mode")
        (erc-cmd-QUERY "NickServ"))

      (ert-info ( "NickServ buffer reassociated")
        (should-not (get-buffer "NickServ@foonet/tester`"))
        (should-not (get-buffer "NickServ@foonet/tester")))

      (with-current-buffer "NickServ" ; new one
        (erc-scenarios-common-say "IDENTIFY tester changeme")
        (funcall expect 5 "You're now logged in as tester")))

    (ert-info ("Still just one NickServ buffer")
      (should-not (cdr (erc-scenarios-common-buflist "NickServ"))))

    (ert-info ("As well as one server buffer")
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
      (erc-cmd-RECONNECT)
      (funcall expect 10 "User modes for dummy"))

    (ert-info ("Server-initiated renick associated correctly")
      (with-current-buffer "foonet"
        (funcall expect 15 "debug mode")
        (should-not (get-buffer "foonet/dummy"))
        (should-not (get-buffer "foonet/tester")))

      (ert-info ("Old query reassociated")
        (should (get-buffer "bob"))
        (should-not (get-buffer "bob@foonet/tester"))
        (should-not (get-buffer "bob@foonet/dummy")))

      (with-current-buffer "foonet"
        (erc-cmd-NICK "tester")
        (funcall expect 5 "You're now logged in as tester")))

    (ert-info ("Ours is still the only bob buffer that remains")
      (should-not (cdr (erc-scenarios-common-buflist "bob"))))

    (ert-info ("Visible network ID still truncated to one component")
      (should (not (get-buffer "foonet/tester")))
      (should (not (get-buffer "foonet/dummy"))))))

;;; erc-scenarios-base-association-nick.el ends here
