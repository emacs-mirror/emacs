;;; erc-scenarios-base-send-message.el --- `send-message' scenarios -*- lexical-binding: t -*-

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

;; So-called "noncommands" are those that massage input submitted at
;; the prompt and send it on behalf of the user.

(ert-deftest erc-scenarios-base-send-message--noncommands ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/send-message")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'noncommands))
       (erc-modules (cons 'fill-wrap erc-modules))
       (erc-autojoin-channels-alist '((foonet "#chan")))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port (process-contact dumb-server :service)
                                :nick "tester"
                                :full-name "tester")
        (funcall expect 5 "debug mode")))

    (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan"))
      (ert-info ("Send CTCP ACTION")
        (funcall expect 10 "<bob> alice: For hands, to do Rome")
        (erc-scenarios-common-say "/me sad")
        (funcall expect 10 "* tester sad"))

      (ert-info ("Send literal command")
        (funcall expect 10 "<alice> bob: Spotted, detested")
        (erc-scenarios-common-say "/say /me sad")
        (funcall expect 10 "<tester> /me sad"))

      (ert-info ("\"Nested\" `noncommands'")

        (ert-info ("Send version via /SV")
          (funcall expect 10 "<bob> Marcus, my brother!")
          (erc-scenarios-common-say "/sv")
          (funcall expect 10 "<tester> I'm using ERC"))

        (ert-info ("Send module list via /SM")
          (funcall expect 10 "<bob> alice: You still wrangle")
          (erc-scenarios-common-say "/sm")
          (funcall expect 10 "<tester> I'm using the following modules: ")
          (funcall expect 10 "<alice> No, not till Thursday;"))))))


;; This asserts that the `command-indicator' module only inserts
;; prompt-like prefixes for normal slash commands, like /JOIN.

(ert-deftest erc-scenarios-base-send-message--command-indicator ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/send-message")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'noncommands))
       (erc-modules `(command-indicator fill-wrap ,@erc-modules))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port (process-contact dumb-server :service)
                                :nick "tester"
                                :full-name "tester")
        (funcall expect 5 "debug mode")
        (erc-scenarios-common-say "/join #chan")
        (funcall expect 10 "ERC> /join #chan")))

    (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan"))
      (ert-info ("Prompt absent for CTCP ACTION")
        (funcall expect 10 "<bob> alice: For hands, to do Rome")
        (erc-scenarios-common-say "/me sad")
        (funcall expect -0.1 "ERC> /me sad")
        (funcall expect 10 "* tester sad"))

      (ert-info ("Prompt absent for literal command")
        (funcall expect 10 "<alice> bob: Spotted, detested")
        (erc-scenarios-common-say "/say /me sad")
        (funcall expect -0.1 "ERC> /say /me sad")
        (funcall expect 10 "<tester> /me sad"))

      (ert-info ("Prompt absent for /SV")
        (funcall expect 10 "<bob> Marcus, my brother!")
        (erc-scenarios-common-say "/sv")
        (funcall expect -0.1 "ERC> /sv")
        (funcall expect 10 "<tester> I'm using ERC"))

      (ert-info ("Prompt absent module list via /SM")
        (funcall expect 10 "<bob> alice: You still wrangle")
        (erc-scenarios-common-say "/sm")
        (funcall expect -0.1 "ERC> /sm")
        (funcall expect 10 "<tester> I'm using the following modules: ")
        (funcall expect 10 "<alice> No, not till Thursday;"))

      (ert-info ("Prompt present for /QUIT in issuing buffer")
        (erc-scenarios-common-say "/quit")
        (funcall expect 10 "ERC> /quit"))

      (with-current-buffer "foonet"
        (funcall expect 10 "ERC finished")))))

;;; erc-scenarios-base-send-message.el ends here
