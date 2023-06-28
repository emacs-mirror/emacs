;;; erc-scenarios-match.el --- Misc `erc-match' scenarios -*- lexical-binding: t -*-

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

(require 'erc-stamp)
(require 'erc-match)

;; This defends against a regression in which all matching by the
;; `erc-match-message' fails when `erc-add-timestamp' precedes it in
;; `erc-insert-modify-hook'.  Basically, `erc-match-message' used to
;; expect an `erc-parsed' text property on the first character in a
;; message, which doesn't exist, when the message content is prefixed
;; by a leading timestamp.

(ert-deftest erc-scenarios-match--stamp-left-current-nick ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/reconnect")
       (dumb-server (erc-d-run "localhost" t 'unexpected-disconnect))
       (port (process-contact dumb-server :service))
       (erc-server-flood-penalty 0.1)
       (erc-insert-timestamp-function 'erc-insert-timestamp-left)
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :full-name "tester"
                                :nick "tester")
        (should (memq 'erc-match-message
                      (memq 'erc-add-timestamp erc-insert-modify-hook)))
        ;; The "match type" is `current-nick'.
        (funcall expect 5 "tester")
        (should (eq (get-text-property (1- (point)) 'font-lock-face)
                    'erc-current-nick-face))))))

;; This asserts that when stamps appear before a message,
;; some non-nil invisibility property spans the entire message.
(ert-deftest erc-scenarios-match--stamp-left-fools-invisible ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "join/legacy")
       (dumb-server (erc-d-run "localhost" t 'foonet))
       (port (process-contact dumb-server :service))
       (erc-server-flood-penalty 0.1)
       (erc-insert-timestamp-function 'erc-insert-timestamp-left)
       (erc-timestamp-only-if-changed-flag nil)
       (erc-fools '("bob"))
       (erc-text-matched-hook '(erc-hide-fools))
       (erc-autojoin-channels-alist '((FooNet "#chan")))
       (expect (erc-d-t-make-expecter))
       (hiddenp (lambda ()
                  (and (eq (field-at-pos (pos-bol)) 'erc-timestamp)
                       (get-text-property (pos-bol) 'invisible)
                       (>= (next-single-property-change (pos-bol)
                                                        'invisible nil)
                           (pos-eol))))))

    (ert-info ("Connect")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :full-name "tester"
                                :password "changeme"
                                :nick "tester")
        (should (memq 'erc-match-message
                      (memq 'erc-add-timestamp erc-insert-modify-hook)))
        (funcall expect 5 "This server is in debug mode")))

    (ert-info ("Ensure lines featuring \"bob\" are invisible")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (should (funcall expect 10 "<bob> tester, welcome!"))
        (should (funcall hiddenp))

        ;; Alice's is the only one visible.
        (should (funcall expect 10 "<alice> tester, welcome!"))
        (should (eq (field-at-pos (pos-bol)) 'erc-timestamp))
        (should (get-text-property (pos-bol) 'invisible))
        (should-not (get-text-property (point) 'invisible))

        (should (funcall expect 10 "<bob> alice: But, as it seems"))
        (should (funcall hiddenp))

        (should (funcall expect 10 "<alice> bob: Well, this is the forest"))
        (should (funcall hiddenp))

        (should (funcall expect 10 "<alice> bob: And will you"))
        (should (funcall hiddenp))

        (should (funcall expect 10 "<bob> alice: Live, and be prosperous"))
        (should (funcall hiddenp))

        (should (funcall expect 10 "ERC>"))
        (should-not (get-text-property (pos-bol) 'invisible))
        (should-not (get-text-property (point) 'invisible))))))

(eval-when-compile (require 'erc-join))

;;; erc-scenarios-match.el ends here
