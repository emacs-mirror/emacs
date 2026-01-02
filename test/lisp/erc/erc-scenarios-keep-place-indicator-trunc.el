;;; erc-scenarios-keep-place-indicator-trunc.el --- `truncate' integration -*- lexical-binding: t -*-

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

(require 'erc-goodies)

(ert-deftest erc-scenarios-keep-place-indicator-trunc ()
  :tags `(:expensive-test
          ,@(and (getenv "ERC_TESTS_GRAPHICAL") '(:erc--graphical)))

  (when (and noninteractive (= emacs-major-version 27))
    (ert-skip "Times out"))

  (defvar erc-max-buffer-size)
  (defvar erc-truncate-padding-size)

  (erc-scenarios-common-with-noninteractive-in-term
      ((erc-scenarios-common-dialog "keep-place")
       (dumb-server (erc-d-run "localhost" t 'follow))
       (port (process-contact dumb-server :service))
       (erc-modules `( keep-place-indicator scrolltobottom
                       truncate ,@erc-modules))
       (erc-server-flood-penalty 0.1)
       (erc-max-buffer-size 300)
       (erc-truncate-padding-size 200)
       (erc-keep-place-indicator-truncation t)
       (erc-autojoin-channels-alist '((foonet "#chan" "#spam")))
       (expect (erc-d-t-make-expecter)))

    (with-current-buffer (erc :server "127.0.0.1"
                              :port port
                              :full-name "tester"
                              :nick "tester"
                              :user "tester")
      (funcall expect 10 "debug mode"))

    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
      (set-window-buffer nil (current-buffer))
      (delete-other-windows)

      (ert-info ("Truncation occurs because indicator still at start pos")
        (funcall expect 10 "]\n<alice> bob: And what I spake")
        (redisplay)
        (should (= (overlay-start erc--keep-place-indicator-overlay) 2))
        (funcall expect 10 "Yes, faith will I")
        (goto-char (point-max)))

      (switch-to-buffer (erc-d-t-wait-for 10 (get-buffer "#spam"))) ; lower
      (funcall expect 10 "<alice> tester, welcome!")
      (erc-scenarios-common-say "one")
      (erc-scenarios-common-say "two")
      (funcall expect 10 "<bob> Cause they take")
      (erc-scenarios-common-say "three")
      (goto-char (point-max))

      (ert-info ("Truncation limited by indicator")
        (switch-to-buffer "#chan")
        (funcall expect 10 "<bob> Ready")
        (redisplay)
        (funcall expect 10 "]\n<alice> Yes, faith will I" (point-min))
        (should (= (overlay-start erc--keep-place-indicator-overlay)
                   (pos-bol)))
        (should (> (buffer-size) 500)))

      (ert-info ("Normal keep-place behavior still present")
        (switch-to-buffer "#spam")
        (should (< (point) erc-input-marker)))

      (erc-keep-place-mode -1)
      (erc-scrolltobottom-mode -1))))

;;; erc-scenarios-keep-place-indicator-trunc.el ends here
