;;; erc-scenarios-keep-place-indicator.el --- erc-keep-place-indicator-mode -*- lexical-binding: t -*-

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

(require 'erc-goodies)

;; This test shows that the indicator does not update when at least
;; one window remains.  When the last window showing a buffer switches
;; away, the indicator is updated if it's earlier in the buffer.
(ert-deftest erc-scenarios-keep-place-indicator--follow ()
  :tags `(:expensive-test
          ,@(and (getenv "EMACS_EMBA_CI") '(:unstable))
          ,@(and (getenv "ERC_TESTS_GRAPHICAL") '(:erc--graphical)))

  ;; ERC's tests also run in external CI that exports this variable.
  ;; Skip on 27 because `erc-scrolltobottom-all' currently requires 28+.
  (when (or (getenv "CI") (< emacs-major-version 28))
    (ert-skip "Times out intermittently"))

  (should-not erc-scrolltobottom-all)
  (should-not erc-scrolltobottom-mode)
  (should-not erc-keep-place-mode)

  (erc-scenarios-common-with-noninteractive-in-term
      ((erc-scenarios-common-dialog "keep-place")
       (dumb-server (erc-d-run "localhost" t 'follow))
       (port (process-contact dumb-server :service))
       (erc-modules `( keep-place-indicator scrolltobottom fill-wrap
                       ,@erc-modules))
       (erc-keep-place-indicator-follow t)
       (erc-scrolltobottom-all t)
       (erc-server-flood-penalty 0.1)
       (erc-autojoin-channels-alist '((foonet "#chan" "#spam")))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :full-name "tester"
                                :nick "tester"
                                :user "tester")
        (funcall expect 10 "debug mode")))

    (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
      (set-window-buffer nil (current-buffer))
      (delete-other-windows)
      (split-window-below)
      (funcall expect 10 "<bob> tester, welcome!")
      (recenter 0)
      (other-window 1)
      (funcall expect 10 "<alice> tester, welcome!")
      (recenter 0)
      (should (= 2 (length (window-list))))

      (ert-info ("Last window to switch away has point earlier in buffer")
        ;; Lower window, with point later in buffer, switches away first.
        (switch-to-buffer (erc-d-t-wait-for 10 (get-buffer "#spam"))) ; lower
        (other-window 1)
        (switch-to-buffer "#spam") ; upper
        (erc-scenarios-common-say "one")
        (funcall expect 10 "Ay, the heads")

        ;; Overlay has moved to upper window start.
        (switch-to-buffer "#chan")
        (redisplay) ; force overlay to update
        (save-excursion
          (goto-char (window-point))
          (should (looking-back (rx "<bob> tester, welcome!")))
          (should (= (pos-bol) (window-start)))
          (erc-d-t-wait-for 20
              (= (overlay-start erc--keep-place-indicator-overlay) (pos-bol))))
        ;; Lower window is still centered at start.
        (other-window 1)
        (switch-to-buffer "#chan")
        (save-excursion
          (goto-char (window-point))
          (should (looking-back (rx "<alice> tester, welcome!")))
          (should (= (pos-bol) (window-start)))))

      (ert-info ("Last window to switch away has point later in buffer")
        ;; Lower window advances.
        (funcall expect 10 "<bob> alice: Since you can cog")
        (recenter 0)
        (redisplay) ; force ^ to appear on first line

        (other-window 1) ; upper still at indicator, switches first
        (switch-to-buffer "#spam")
        (other-window 1)
        (switch-to-buffer "#spam") ; lower follows, speaks to sync
        (erc-scenarios-common-say "two")
        (funcall expect 10 "<bob> Cause they take")
        (goto-char (point-max))

        ;; Upper switches back first, finds indicator gone.
        (other-window 1)
        (switch-to-buffer "#chan")
        (save-excursion
          (goto-char (window-point))
          (should (looking-back (rx "<bob> tester, welcome!")))
          (should (= (pos-bol) (window-start)))
          (should (> (overlay-start erc--keep-place-indicator-overlay)
                     (pos-eol))))

        ;; Lower window follows, window-start preserved.
        (other-window 1)
        (switch-to-buffer "#chan")
        (save-excursion
          (goto-char (window-point))
          (should (looking-back (rx "you can cog")))
          (should (= (pos-bol) (window-start)
                     (overlay-start erc--keep-place-indicator-overlay)))))

      (ert-info ("Point formerly at prompt resides at last arrived message")
        (erc-send-input-line "#spam" "three")
        (save-excursion (erc-d-t-search-for 10 "Ready"))
        (switch-to-buffer "#spam")
        (should (< (point) erc-input-marker))))

    (erc-keep-place-mode -1)
    (erc-scrolltobottom-mode -1)))

;;; erc-scenarios-keep-place-indicator.el ends here
