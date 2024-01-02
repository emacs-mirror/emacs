;;; erc-scenarios-base-auto-recon.el --- auto-recon scenarios -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Free Software Foundation, Inc.

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

(defun erc-scenarios-base-auto-recon--get-unused-port ()
  (let ((server (make-network-process :name "*erc-scenarios-base-auto-recon*"
                                      :host "localhost"
                                      :service t
                                      :server t)))
    (delete-process server)
    (process-contact server :service)))

;; This demos one possible flavor of intermittent service.
;; It may end up needing to be marked :unstable.

(ert-deftest erc-scenarios-base-auto-recon-unavailable ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-server-flood-penalty 0.1)
       (port (erc-scenarios-base-auto-recon--get-unused-port))
       (erc--server-reconnect-timeout-scale-function (lambda (_) 1))
       (erc-server-auto-reconnect t)
       (erc-server-reconnect-function #'erc-server-delayed-check-reconnect)
       (expect (erc-d-t-make-expecter))
       (erc-scenarios-common-dialog "base/reconnect")
       (dumb-server nil))

    (ert-info ("Dialing fails: nobody home")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (erc-d-t-wait-for 10 (not (erc-server-process-alive)))
        (erc-d-t-wait-for 10 erc--server-reconnect-timer)
        (funcall expect 10 "Opening connection")
        (funcall expect 10 "failed")

        (ert-info ("Reconnect function freezes attempts at 1")
          (funcall expect 10 '(: "reconnecting" (+ nonl) "attempt 1/2"))
          (funcall expect 10 "nobody home")
          (funcall expect 10 '(: "reconnecting" (+ nonl) "attempt 1/2"))
          (funcall expect 10 "nobody home"))))

    (ert-info ("Service appears")
      (setq dumb-server (erc-d-run "localhost" port
                                   'just-eof 'unexpected-disconnect))
      (with-current-buffer (format "127.0.0.1:%d" port)
        (funcall expect 10 "server is in debug mode")
        (should (equal (buffer-name) "FooNet"))))

    (ert-info ("Service interrupted, reconnect starts again")
      (with-current-buffer "FooNet"
        (funcall expect 10 "failed")
        (funcall expect 10 '(: "reconnecting" (+ nonl) "attempt 1/2"))))

    (ert-info ("Service restored")
      (delete-process dumb-server)
      (setq dumb-server (erc-d-run "localhost" port
                                   'just-eof 'unexpected-disconnect))
      (with-current-buffer "FooNet"
        (funcall expect 10 "server is in debug mode")))

    (ert-info ("Service interrupted a third time, reconnect starts yet again")
      (with-current-buffer "FooNet"
        (funcall expect 10 "failed")
        (funcall expect 10 '(: "reconnecting" (+ nonl) "attempt 1/2"))
        (erc-cmd-RECONNECT "cancel")
        (funcall expect 10 "canceled")))))

;; In this test, a listener accepts but doesn't respond to any messages.

(ert-deftest erc-scenarios-base-auto-recon-no-proto ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-server-flood-penalty 0.1)
       (erc-scenarios-common-dialog "base/reconnect")
       (erc-d-auto-pong nil)
       (dumb-server (erc-d-run "localhost" t 'unexpected-disconnect))
       (port (process-contact dumb-server :service))
       (erc--server-reconnect-timeout-scale-function (lambda (_) 1))
       (erc--server-reconnect-timeout-check 0.5)
       (erc-server-auto-reconnect t)
       (erc-server-reconnect-function #'erc-server-delayed-check-reconnect)
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Session succeeds but cut short")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (funcall expect 10 "server is in debug mode")
        (should (equal (buffer-name) "FooNet"))
        (erc-d-t-wait-for 10 erc--server-reconnect-timer)
        (delete-process dumb-server)
        (funcall expect 10 "failed")

        (ert-info ("Reconnect function freezes attempts at 1")
          (funcall expect 10 '(: "reconnecting" (+ nonl) "attempt 1/2"))
          (funcall expect 10 "nobody home")
          (funcall expect 10 "timed out while dialing")
          (funcall expect 10 '(: "reconnecting" (+ nonl) "attempt 1/2"))
          (funcall expect 10 "nobody home"))))

    (ert-info ("Service restored")
      (setq dumb-server (erc-d-run "localhost" port
                                   'just-ping
                                   'ping-pong
                                   'unexpected-disconnect))
      (with-current-buffer "FooNet"
        (funcall expect 30 "server is in debug mode")))

    (ert-info ("Service interrupted again, reconnect starts again")
      (with-current-buffer "FooNet"
        (funcall expect 10 "failed")
        (funcall expect 10 '(: "reconnecting" (+ nonl) "attempt 1/2"))
        (erc-cmd-RECONNECT "cancel")
        (funcall expect 10 "canceled")))))

;;; erc-scenarios-base-auto-recon.el ends here
