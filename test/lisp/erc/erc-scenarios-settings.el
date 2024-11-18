;;; erc-scenarios-settings.el --- erc-settings tests -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

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

(require 'erc-settings)

(ert-deftest erc-scenarios-settings/sasl ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-server-flood-penalty 0.1)
       (erc-scenarios-common-dialog "settings")
       (server-foonet (erc-d-run "localhost" t "server-foonet" 'basic-sasl))
       (server-barnet (erc-d-run "localhost" t "server-barnet" 'basic))
       (port-foonet (process-contact server-foonet :service))
       (port-barnet (process-contact server-barnet :service))
       (erc-modules `(settings ,@erc-modules))
       (erc-settings
        `(((and ,(rx-to-string `(: ":" ,(number-to-string port-foonet) eot) t)
                (not erc-server-process-alive))
           (erc-modules `(sasl ,@erc-modules) :eval))
          ((and ,(rx-to-string `(: ":" ,(number-to-string port-barnet) eot) t)
                (not erc-server-process-alive))
           (erc-modules `(fill-wrap ,@erc-modules) :eval)
           (erc-autojoin-channels-alist (("." "#spam"))))
          ((and (network . foonet) erc-open-server-buffer-p)
           (erc-prompt "server!!!")
           (erc-autojoin-channels-alist ((foonet "#chan"))))
          ((and (network . foonet) erc-channel-buffer-p)
           (erc-modules `(keep-place-indicator ,@erc-modules) :eval))))
       (expect (erc-d-t-make-expecter)))

    ;; Simulate `setopt', `custom-set-variables', etc.
    (erc-update-modules)
    (with-current-buffer (erc :server "127.0.0.1"
                              :port port-foonet
                              :nick "tester"
                              :user "tester"
                              :password "changeme"
                              :full-name "tester")
      (funcall expect 10 "This server is in debug mode"))
    (with-current-buffer (erc :server "127.0.0.1"
                              :port port-barnet
                              :nick "tester"
                              :user "tester"
                              :full-name "tester")
      (funcall expect 10 "This server is in debug mode"))

    ;; Sentinel variables for modules we're enabling locally.
    (defvar erc-fill--wrap-last-msg)
    (defvar erc--keep-place-indicator-overlay)

    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
      (funcall expect 10 "<alice> bob: Grows, lives, and dies")
      (should erc--keep-place-indicator-overlay)
      (should-not erc-fill--wrap-last-msg)
      (funcall expect 10 "ERC>"))

    ;; Targets that don't specify their own local modules inherit any
    ;; from their server buffer (if `erc-modules' is locally bound).
    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#spam"))
      (funcall expect 10 "<bob> alice: Ay, like a")
      (should-not erc--keep-place-indicator-overlay)
      (should erc-fill--wrap-last-msg)
      (funcall expect 10 "ERC>"))

    ;; Point in fact: `fill-wrap' is enabled in this query buffer.
    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "alice"))
      (funcall expect 10 "<alice> My hand to thee")
      (should-not erc--keep-place-indicator-overlay)
      (should erc-fill--wrap-last-msg)
      (funcall expect 10 "ERC>"))

    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "foonet"))
      (should-not erc--keep-place-indicator-overlay)
      (should-not  erc-fill--wrap-last-msg)
      (funcall expect 10 "server!!!"))

    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "barnet"))
      (should-not erc--keep-place-indicator-overlay)
      (should erc-fill--wrap-last-msg)
      (funcall expect 10 "ERC>"))

    (erc-settings-mode -1)))

(ert-deftest erc-scenarios-settings/sasl/id ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-server-flood-penalty 0.1)
       (erc-scenarios-common-dialog "settings")
       (server-foonet (erc-d-run "localhost" t "server-foonet" 'basic-sasl))
       (server-barnet (erc-d-run "localhost" t "server-barnet" 'basic))
       (port-foonet (process-contact server-foonet :service))
       (port-barnet (process-contact server-barnet :service))
       (erc-modules `(settings ,@erc-modules))
       (erc-settings
        `(((not erc-server-process-alive) ; common to both servers
           (erc-server "127.0.0.1")
           (erc-nick "tester")
           (erc-email-userid "tester")
           (erc-user-full-name "tester"))
          ;; If this were moved to the end of the list (try it), ERC
          ;; would skip it because (id . x) matches in channels too.
          (erc-channel-buffer-p ; all channels
           (erc-modules `(keep-place-indicator ,@erc-modules) :eval))
          ((id . id-foonet) ; foonet only (channels too)
           (erc-port ,port-foonet)
           (erc-sasl-password "changeme")
           (erc-modules `(sasl ,@erc-modules) :eval)
           (erc-prompt "wee!!!")
           (erc-autojoin-channels-alist ((id-foonet "#chan"))))
          ((id . id-barnet) ; barnet only (channels too)
           (erc-port ,port-barnet)
           (erc-modules `(fill-wrap ,@erc-modules) :eval)
           (erc-autojoin-channels-alist ((id-barnet "#spam"))))))
       (expect (erc-d-t-make-expecter)))

    ;; Simulate `setopt', `custom-set-variables', etc.
    (erc-update-modules)

    ;; Mimic `erc-settings-connect-by-id', except don't use TLS.
    (with-current-buffer (erc :server nil :port nil :nick nil :user nil
                              :password nil :full-name nil :id 'id-foonet)
      (should (equal (buffer-name) "id-foonet"))
      (funcall expect 10 "This server is in debug mode"))
    (with-current-buffer (erc :server nil :port nil :nick nil :user nil
                              :password nil :full-name nil :id 'id-barnet)
      (should (equal (buffer-name) "id-barnet"))
      (funcall expect 10 "This server is in debug mode"))

    ;; Sentinel variables for modules we're enabling locally.
    (defvar erc-fill--wrap-last-msg)
    (defvar erc--keep-place-indicator-overlay)

    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
      (funcall expect 10 "<alice> bob: Grows, lives, and dies")
      (should erc--keep-place-indicator-overlay)
      (should-not erc-fill--wrap-last-msg)
      (funcall expect 10 "wee!!!"))

    ;; Channel-only module overrides were applied correctly.
    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#spam"))
      (funcall expect 10 "<bob> alice: Ay, like a")
      (should erc--keep-place-indicator-overlay)
      (should-not erc-fill--wrap-last-msg)
      (funcall expect 10 "ERC>"))

    ;; Module `fill-wrap' is active in this query buffer because it has
    ;; no query-specific overrides for `erc-modules'.  It instead
    ;; inherits the local value from the server buffer.
    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "alice"))
      (funcall expect 10 "<alice> My hand to thee")
      (should-not erc--keep-place-indicator-overlay)
      (should erc-fill--wrap-last-msg)
      (funcall expect 10 "ERC>"))

    (with-current-buffer "id-foonet"
      (should-not erc--keep-place-indicator-overlay)
      (should-not  erc-fill--wrap-last-msg)
      (funcall expect 10 "wee!!!"))

    (with-current-buffer "id-barnet"
      (should-not erc--keep-place-indicator-overlay)
      (should erc-fill--wrap-last-msg)
      (funcall expect 10 "ERC>"))

    (erc-settings-mode -1)))

;;; erc-scenarios-settings.el ends here
