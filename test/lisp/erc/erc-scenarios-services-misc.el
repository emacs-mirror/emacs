;;; erc-scenarios-services-misc.el --- Services-misc scenarios -*- lexical-binding: t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

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

(eval-when-compile (require 'erc-join)
                   (require 'erc-services))

(ert-deftest erc-scenarios-services-password ()
  :tags '(:expensive-test)

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "services/password")
       (erc-server-flood-penalty 0.1)
       (erc-modules (cons 'services erc-modules))
       (erc-nickserv-passwords '((Libera.Chat (("joe" . "bar")
                                               ("tester" . "changeme")))))
       (expect (erc-d-t-make-expecter))
       (dumb-server (erc-d-run "localhost" t 'libera))
       (port (process-contact dumb-server :service)))

    (ert-info ("Connect without password")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (erc-d-t-wait-for 5 (eq erc-network 'Libera.Chat))
        (funcall expect 5 "This nickname is registered.")
        (funcall expect 2 "You are now identified")
        (funcall expect 1 "Last login from")
        (erc-cmd-QUIT "")))

    (erc-services-mode -1)

    (should-not (memq 'services erc-modules))))

(ert-deftest erc-scenarios-services-prompt ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "services/password")
       (erc-server-flood-penalty 0.1)
       (inhibit-interaction nil)
       (erc-modules (cons 'services erc-modules))
       (expect (erc-d-t-make-expecter))
       (dumb-server (erc-d-run "localhost" t 'libera))
       (port (process-contact dumb-server :service)))

    (ert-info ("Connect without password")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (ert-simulate-keys "changeme\r"
          (erc-d-t-wait-for 10 (eq erc-network 'Libera.Chat))
          (funcall expect 3 "This nickname is registered.")
          (funcall expect 3 "You are now identified")
          (funcall expect 3 "Last login from"))
        (erc-cmd-QUIT "")))

    (erc-services-mode -1)

    (should-not (memq 'services erc-modules))))

;;; erc-scenarios-services-misc.el ends here
