;;; erc-scenarios-fill-wrap.el --- Fill-wrap module -*- lexical-binding: t -*-

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

;;; Code:

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(defun erc-scenarios-fill-wrap--merged-p ()
  (get-text-property (pos-bol) 'erc-fill--wrap-merge))

;; This asserts that an intervening date stamp between two messages
;; from the same speaker will trigger a break in merge detection, so
;; the second message's speaker tag won't be hidden.
(ert-deftest erc-scenarios-fill-wrap/merge-datestamp ()
  :tags '(:expensive-test)

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "fill/wrap")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'merge-datestamp))
       (erc-stamp--tz t)
       ;; Start at 2023-10-22T06:16:43.445Z
       (erc-stamp--current-time (if (< emacs-major-version 29)
                                    '(25908 23515 445000 0)
                                  '(1697930203445 . 1000)))
       (erc-timer-hook (cons (lambda (&rest _)
                               (setq erc-stamp--current-time
                                     (time-add erc-stamp--current-time 15)))
                             erc-timer-hook))
       (expect (erc-d-t-make-expecter))
       (erc-autojoin-channels-alist '((foonet "#chan" "#control")))
       (erc-modules `(nicks fill-wrap scrolltobottom ,@erc-modules))
       (port (process-contact dumb-server :service)))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :user "tester"
                                :full-name "tester")
        (funcall expect 10 "This server is in debug mode")))

    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "dummy"))
      (funcall expect 10 "<dummy> hi")
      (funcall expect 10 "<dummy> there"))

    (with-current-buffer "#chan"
      (funcall expect 10 "<bob> tester, welcome")

      ;; Force date change.
      (setq erc-stamp--current-time
            (time-add erc-stamp--current-time (* 60 60))))

    (with-current-buffer "#control"
      (erc-send-message "1"))

    (with-current-buffer "#chan"
      (funcall expect 10 "[Sun Oct 22 2023]")
      (funcall expect 10 "<bob> one")
      (should-not (erc-scenarios-fill-wrap--merged-p)))

    (with-current-buffer "#control"
      (erc-send-message "2"))

    (with-current-buffer "dummy"
      (funcall expect 10 "[Sun Oct 22 2023]")
      (funcall expect 10 "<dummy> again")
      (should-not (erc-scenarios-fill-wrap--merged-p)))

    (with-current-buffer "#chan"
      (funcall expect 10 "<alice> bob: He was famous"))

    (erc-scrolltobottom-mode -1)))

;;; erc-scenarios-fill-wrap.el ends here
