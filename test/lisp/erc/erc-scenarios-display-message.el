;;; erc-scenarios-display-message.el --- erc-display-message -*- lexical-binding: t -*-

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

(ert-deftest erc-scenarios-display-message--multibuf ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/display-message")
       (dumb-server (erc-d-run "localhost" t 'multibuf))
       (port (process-contact dumb-server :service))
       (erc-server-flood-penalty 0.1)
       (erc-modules (cons 'fill-wrap erc-modules))
       (erc-autojoin-channels-alist '((foonet "#chan")))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (funcall expect 10 "debug mode")))

    (ert-info ("User dummy is a member of #chan")
      (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan"))
        (funcall expect 10 "dummy")))

    (ert-info ("Dummy's QUIT notice in query contains metadata props")
      (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "dummy"))
        (funcall expect 10 "<dummy> hi")
        (funcall expect 10 "*** dummy (~u@rdjcgiwfuwqmc.irc) has quit")
        (should (eq 'QUIT (get-text-property (match-beginning 0) 'erc--msg)))))

    (ert-info ("Dummy's QUIT notice in #chan contains metadata props")
      (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan"))
        (funcall expect 10 "*** dummy (~u@rdjcgiwfuwqmc.irc) has quit")
        (should (eq 'QUIT (get-text-property (match-beginning 0) 'erc--msg)))))

    (with-current-buffer "foonet"
      (erc-cmd-QUIT ""))))

;;; erc-scenarios-display-message.el ends here
