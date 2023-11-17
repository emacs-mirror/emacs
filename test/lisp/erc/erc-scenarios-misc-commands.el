;;; erc-scenarios-misc-commands.el --- Misc commands for ERC -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

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

;;; erc-scenarios-misc-commands.el ends here
