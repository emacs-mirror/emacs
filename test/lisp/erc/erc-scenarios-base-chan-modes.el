;;; erc-scenarios-base-chan-modes.el --- Channel mode scenarios -*- lexical-binding: t -*-

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

;; This asserts that a bug present in ERC 5.4+ is now absent.
;; Previously, ERC would attempt to parse a nullary channel mode as if
;; it were a status prefix update, which led to a wrong-type error.
;; This test does not address similar collisions with unary modes,
;; such as "MODE +q foo!*@*", but it should.
(ert-deftest erc-scenarios-base-chan-modes--plus-q ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/modes")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'chan-changed))
       (erc-modules (cons 'fill-wrap erc-modules))
       (erc-autojoin-channels-alist '((Libera.Chat "#chan")))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to Libera.Chat")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port (process-contact dumb-server :service)
                                :nick "tester"
                                :full-name "tester")
        (funcall expect 5 "changed mode")))

    (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan"))
      (should-not erc-channel-key)
      (should-not erc-channel-user-limit)

      (ert-info ("Receive notice that mode has changed")
        (erc-d-t-wait-for 10 (equal erc-channel-modes '("n" "t")))
        (erc-scenarios-common-say "ready before")
        (funcall expect 10 "<Chad> before")
        (funcall expect 10 " has changed mode for #chan to +Qu")
        (erc-d-t-wait-for 10 (equal erc-channel-modes '("Q" "n" "t" "u"))))

      (ert-info ("Key stored locally")
        (erc-scenarios-common-say "ready key")
        (funcall expect 10 "<Chad> doing key")
        (funcall expect 10 " has changed mode for #chan to +k hunter2")
        (should (equal erc-channel-key "hunter2")))

      (ert-info ("Limit stored locally")
        (erc-scenarios-common-say "ready limit")
        (funcall expect 10 "<Chad> doing limit")
        (funcall expect 10 " has changed mode for #chan to +l 3")
        (erc-d-t-wait-for 10 (eql erc-channel-user-limit 3))
        (should (equal erc-channel-modes '("Q" "n" "t" "u"))))

      (ert-info ("Modes removed and local state deletion succeeds")
        (erc-scenarios-common-say "ready drop")
        (funcall expect 10 "<Chad> dropping")
        (funcall expect 10 " has changed mode for #chan to -lu")
        (funcall expect 10 " has changed mode for #chan to -Qk *")
        (erc-d-t-wait-for 10 (equal erc-channel-modes '("n" "t"))))

      (should-not erc-channel-key)
      (should-not erc-channel-user-limit)
      (funcall expect 10 "<Chad> after"))))

;;; erc-scenarios-base-chan-modes.el ends here
