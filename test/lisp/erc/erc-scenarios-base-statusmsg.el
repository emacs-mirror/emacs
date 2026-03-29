;;; erc-scenarios-base-statusmsg.el --- statusmsg tests -*- lexical-binding: t -*-

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

(ert-deftest erc-scenarios-base-statusmsg ()

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/display-message")
       (dumb-server (erc-d-run "localhost" t 'statusmsg))
       (erc-autojoin-channels-alist '((foonet "#mine")))
       (erc-modules (cons 'fill-wrap erc-modules))
       (port (process-contact dumb-server :service))
       (erc-show-speaker-membership-status nil)
       (erc-server-flood-penalty 0.1)
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :user "tester"
                                :full-name "tester")
        (funcall expect 5 "This server is in debug mode")))

    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#mine"))

      (ert-info ("Receive status messages unprefixed")
        (funcall expect 5 "+dummy")
        (funcall expect 5 "(dummy+) hello")
        (should (eq 'statusmsg (erc--get-inserted-msg-prop 'erc--msg)))
        (should (equal "dummy" (erc--get-inserted-msg-prop 'erc--spkr)))
        (should (eq (get-text-property (1- (point)) 'font-lock-face)
                    'erc-default-face))
        (funcall expect 5 "(dummy+) there")
        (should (equal "" (get-text-property (pos-bol) 'display)))

        ;; CTCP ACTION
        (funcall expect 5 "* (dummy+) sad")
        (should (eq 'ctcp-action-statusmsg
                    (erc--get-inserted-msg-prop 'erc--msg)))
        (should (eq (get-text-property (1- (point)) 'font-lock-face)
                    'erc-action-face))
        (funcall expect 5 "* (dummy+) glad")
        (should (equal "" (get-text-property (pos-bol) 'display))))

      (ert-info ("Send status messages")
        ;; We don't have `echo-message' yet, so ERC doesn't currently
        ;; insert commands like "/msg +#mine foo".
        (let ((erc-default-recipients '("+#mine")))
          (erc-send-message "howdy"))
        (funcall expect 5 "(@tester+) howdy")
        (should (eq 'statusmsg-input (erc--get-inserted-msg-prop 'erc--msg)))
        (should (equal "tester" (erc--get-inserted-msg-prop 'erc--spkr)))
        (should (eq (get-text-property (1- (point)) 'font-lock-face)
                    'erc-input-face))
        (let ((erc-default-recipients '("+#mine")))
          (erc-send-message "tenderfoot"))
        (funcall expect 5 "(@tester+) tenderfoot")
        (should (equal "" (get-text-property (pos-bol) 'display)))

        ;; Simulate some "echoed" CTCP ACTION messages since we don't
        ;; actually support that yet.
        (funcall expect 5 "* (@tester+) mad")
        (should (eq 'ctcp-action-statusmsg-input
                    (erc--get-inserted-msg-prop 'erc--msg)))
        (should (equal (get-text-property (1- (point)) 'font-lock-face)
                       '(erc-input-face erc-action-face)))
        (funcall expect 5 "* (@tester+) chad")
        (should (equal "" (get-text-property (pos-bol) 'display))))

      (ert-info ("Receive status messages prefixed")
        (setq erc-show-speaker-membership-status t)
        (erc-scenarios-common-say "/me ready") ; sync
        (funcall expect 5 "* @tester ready")
        (funcall expect 5 "(+dummy+) okie")

        ;; CTCP ACTION
        (funcall expect 5 "* (+dummy+) dokie")
        (funcall expect 5 "* +dummy out")))))

;;; erc-scenarios-base-statusmsg.el ends here
