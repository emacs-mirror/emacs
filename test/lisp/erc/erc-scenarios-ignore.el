;;; erc-scenarios-ignore.el --- /IGNORE scenarios ERC -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

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

;; TODO add test covering the same ignored speaker in two different
;; channels on the same server: they should be ignored in both.

;;; Code:

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(ert-deftest erc-scenarios-ignore/basic ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/assoc/multi-net")
       (erc-server-flood-penalty 0.1)
       (dumb-server-foonet (erc-d-run "localhost" t 'foonet))
       (dumb-server-barnet (erc-d-run "localhost" t 'barnet))
       (erc-autojoin-channels-alist '((foonet "#chan") (barnet "#chan")))
       (port-foonet (process-contact dumb-server-foonet :service))
       (port-barnet (process-contact dumb-server-barnet :service))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to two networks")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port-barnet
                                :nick "tester"
                                :password "changeme"
                                :full-name "tester"))
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port-foonet
                                :nick "tester"
                                :password "changeme"
                                :full-name "tester")
        (funcall expect 10 "debug mode")))

    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan@foonet"))
      (funcall expect 10 "<bob> tester, welcome!")
      (funcall expect 10 "<alice> tester, welcome!")
      (erc-scenarios-common-say "/ignore alice 1m")
      (erc-scenarios-common-say "/ignore mike 1h")
      (funcall expect 10 "ignoring alice for 1m0s")
      (funcall expect 10 "<bob> alice: Signior Iachimo")
      (erc-scenarios-common-say "/ignore")
      (funcall expect 20 '(: "alice    5" (any "0-9") "s"))
      (funcall expect 10 '(: "mike     59m5" (any "0-9") "s"))
      (funcall expect -0.1 "<alice>")
      (funcall expect 10 "<bob> alice: The ground is bloody")
      (erc-scenarios-common-say "/unignore alice")
      (funcall expect 10 "<alice>"))

    ;; No <mike> messages were ignored on network barnet.
    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan@barnet"))
      (funcall expect 10 "<mike> tester, welcome!")
      (funcall expect 10 "<joe> tester, welcome!")
      (funcall expect 10 "<mike> joe: Whipp'd")
      (funcall expect 10 "<mike> joe: Double"))))

;;; erc-scenarios-ignore.el ends here
