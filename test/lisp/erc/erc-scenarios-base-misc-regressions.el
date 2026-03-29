;;; erc-scenarios-base-misc-regressions.el --- misc regressions scenarios -*- lexical-binding: t -*-

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

(defun erc-scenarios--rebuffed-gapless-pass-handler (dialog exchange)
  (when (eq (erc-d-dialog-name dialog) 'pass-stub)
    (let* ((match (erc-d-exchange-match exchange 1))
           (sym (if (string= match "foonet") 'foonet 'barnet)))
      (should (member match (list "foonet" "barnet")))
      (erc-d-load-replacement-dialog dialog sym 1))))

(ert-deftest erc-scenarios-base-gapless-connect ()
  "Back-to-back entry-point invocations happen successfully.
Originally from scenario rebuffed/gapless as explained in Bug#48598:
28.0.50; buffer-naming collisions involving bouncers in ERC."
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/gapless-connect")
       (erc-server-flood-penalty 0.1)
       (erc-server-flood-penalty erc-server-flood-penalty)
       (erc-d-tmpl-vars '((token . (group (| "barnet" "foonet")))))
       (erc-d-match-handlers
        (list :pass #'erc-scenarios--rebuffed-gapless-pass-handler))
       (dumb-server (erc-d-run "localhost" t
                               'pass-stub 'pass-stub 'barnet 'foonet))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       erc-autojoin-channels-alist
       erc-server-buffer-foo
       erc-server-buffer-bar)

    (ert-info ("Connect twice to same endpoint without pausing")
      (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                       :port port
                                       :nick "tester"
                                       :password "foonet:changeme"
                                       :full-name "tester")
            erc-server-buffer-bar (erc :server "127.0.0.1"
                                       :port port
                                       :nick "tester"
                                       :password "barnet:changeme"
                                       :full-name "tester")))

    (ert-info ("Returned server buffers are unique")
      (should-not (eq erc-server-buffer-foo erc-server-buffer-bar)))

    (ert-info ("Both connections still alive")
      (should (get-process (format "erc-127.0.0.1-%d" port)))
      (should (get-process (format "erc-127.0.0.1-%d<1>" port))))

    (with-current-buffer erc-server-buffer-bar
      (funcall expect 2 "marked as being away"))

    (with-current-buffer (erc-d-t-wait-for 20 (get-buffer "#bar"))
      (funcall expect 10 "was created on")
      (funcall expect 10 "his second fit"))

    (with-current-buffer (erc-d-t-wait-for 20 (get-buffer "#foo"))
      (funcall expect 10 "was created on")
      (funcall expect 2 "no use of him"))))

;; This defends against a regression in `erc-server-PRIVMSG' caused by
;; the removal of `erc-auto-query'.  When an active channel buffer is
;; killed off and PRIVMSGs arrive targeting it, the buffer should be
;; recreated.  See elsewhere for NOTICE logic, which is more complex.

(ert-deftest erc-scenarios-base-channel-buffer-revival ()
  :tags '(:expensive-test)

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/channel-buffer-revival")
       (dumb-server (erc-d-run "localhost" t 'foonet))
       (port (process-contact dumb-server :service))
       erc-autojoin-channels-alist
       erc-server-buffer-foo)

    (ert-info ("Connect to foonet")
      (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                       :port port
                                       :nick "tester"
                                       :password "changeme"
                                       :full-name "tester"))
      (with-current-buffer erc-server-buffer-foo
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (ert-info ("Server buffer is unique and temp name is absent")
      (erc-d-t-wait-for 10 (get-buffer "FooNet"))
      (should-not (erc-scenarios-common-buflist "127.0.0.1"))
      (with-current-buffer erc-server-buffer-foo
        (erc-cmd-JOIN "#chan")))

    (ert-info ("Channel buffer #chan alive and well")
      (with-current-buffer (erc-d-t-wait-for 8 (get-buffer "#chan"))
        (erc-d-t-search-for 10 "Our queen and all her elves")
        (kill-buffer)))

    (should-not (get-buffer "#chan"))

    (ert-info ("Channel buffer #chan revived")
      (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan"))
        (erc-d-t-search-for 10 "and be prosperous")))))

;;; erc-scenarios-base-misc-regressions.el ends here
