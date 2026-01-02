;;; erc-scenarios-join-timing.el --- Services integration -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

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

;;; Commentary:

;; These tests illustrate behavior related to `erc-autojoin-timing'.
;; When the option's value is `ident', the services module runs code on
;; its behalf via `erc-nickserv-identified-hook'.

;; TODO add variants where `erc-nickserv-identify-mode' is not `both'.

;;; Code:

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(require 'erc-join)
(require 'erc-services)

(defun erc-scenarios-join-timing--services-identify-both (dialog)

  (should (eq erc-nickserv-identify-mode 'both))

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "join/timing")
       (erc-server-flood-penalty 0.1)
       (erc-modules (cons 'services erc-modules))
       (erc-autojoin-channels-alist '((Libera.Chat "#chan")))
       (erc-nickserv-passwords '((Libera.Chat (("tester" . "changeme")))))
       (expect (erc-d-t-make-expecter))
       (dumb-server (erc-d-run "localhost" t dialog))
       (port (process-contact dumb-server :service)))

    (ert-info ("Connect without password")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :user "tester"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (erc-d-t-wait-for 5 (eq erc-network 'Libera.Chat))
        (funcall expect 5 "This nickname is registered.")

        (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
          (funcall expect 10 "created"))

        (funcall expect 2 "You are now identified")
        (funcall expect 2 "You are now logged in as tester")))

    (erc-services-mode -1)
    (should-not (memq 'services erc-modules))))

;; This asserts the default behavior, when `erc-autojoin-timing' is
;; `connect'.  Here, ERC emits the JOIN request before being informed by
;; NickServ that it needs to log in.  The server then holds off on
;; granting the JOIN until authentication has completed.
(ert-deftest erc-scenarios-join-timing/connect ()
  :tags '(:expensive-test)

  (should (eq erc-autojoin-timing 'connect))
  (erc-scenarios-join-timing--services-identify-both 'connect-both))

;; This demos the integration between the `join' and `services' modules
;; that occurs when `erc-autojoin-timing' is `ident'.  Here, ERC
;; arranges not to request a JOIN until it's been authenticated by the
;; server.  Since `erc-autojoin-delay' remains at its default of 30,
;; authentication occurs before the fallback timer fires.
(ert-deftest erc-scenarios-join-timing/ident ()
  :tags '(:expensive-test)

  (should (eq erc-autojoin-timing 'connect))
  (should (= erc-autojoin-delay 30))

  (let ((erc-autojoin-timing 'ident)
        ;; In the interest of time saving, this currently doesn't wait
        ;; around to verify that a second JOIN isn't sent.  However, the
        ;; logic in `erc-autojoin--join' prevents such sending anyway,
        ;; meaning some other means of verification would be needed.
        (erc-autojoin-delay 5))
    (erc-scenarios-join-timing--services-identify-both 'ident-both)))

;;; erc-scenarios-join-timing.el ends here
