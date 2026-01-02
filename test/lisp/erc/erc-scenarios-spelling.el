;;; erc-scenarios-spelling.el --- Basic spelling scenarios -*- lexical-binding: t -*-

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

;;; Code:

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(require 'erc-spelling)

(ert-deftest erc-scenarios-spelling--auto-correct ()
  :tags `(:expensive-test
          :unstable
          ,@(and (getenv "ERC_TESTS_GRAPHICAL") '(:erc--graphical)))

  ;; Allow running locally with SELECTOR=t if user has ispell configured.
  (unless (ignore-errors
            (and (executable-find ispell-program-name)
                 (progn (ispell-check-version) t)
                 (member "american" (ispell-valid-dictionary-list))))
    (ert-skip "Missing ispell program"))

  (ert-with-temp-directory erc-scenarios-spelling

    (erc-scenarios-common-with-noninteractive-in-term
        ((erc-scenarios-common-dialog "spelling")
         (process-environment (cons
                               (format "HOME=%s" erc-scenarios-spelling)
                               process-environment))
         (dumb-server (erc-d-run "localhost" t 'auto-correct))
         (port (process-contact dumb-server :service))
         (expect (erc-d-t-make-expecter))
         (erc-autojoin-channels-alist '((foonet "#chan")))
         (erc-modules (cons 'spelling erc-modules))
         (erc-server-flood-penalty 0.1))

      (ert-info ("Connect to foonet")
        (with-current-buffer (erc :server "127.0.0.1"
                                  :port port
                                  :nick "tester"
                                  :full-name "tester")
          (funcall expect 10 "no longer marked as being")
          (should erc-spelling-mode)
          (should flyspell-mode)))

      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (should erc-spelling-mode)
        (should flyspell-mode)
        (funcall expect 10 "<alice> tester, welcome!")

        ;; Insert a command with one misspelled word.
        (set-window-buffer nil (current-buffer))
        (execute-kbd-macro "\M->/AMSG an/dor /gmsg one fsbot two frob my shoe")
        (funcall expect 10 "shoe")

        (let* ((ovs (overlays-in erc-input-marker (point)))
               (ov1 (pop ovs))
               (ov2 (pop ovs)))
          ;; At this point, flyspell should have done its thing.  There
          ;; should be two overlays: one on "dor" and the other on
          ;; "frob".  The spelling module's modifications should have
          ;; prevented the two valid slash commands as well as "fsbot"
          ;; from being highlighted.
          (should-not ovs)
          (should (flyspell-overlay-p ov1))
          (should (equal "dor" (buffer-substring (overlay-start ov1)
                                                 (overlay-end ov1))))
          (should (flyspell-overlay-p ov2))
          (should (equal "frob" (buffer-substring (overlay-start ov2)
                                                  (overlay-end ov2))))
          (goto-char (overlay-start ov2))

          ;; Depending on the machine, this should become something
          ;; like: "/AMSG an/dor /gmsg one fsbot two Rob my shoe".
          (execute-kbd-macro (key-parse "M-TAB"))
          (should (equal (overlays-in erc-input-marker (point-max))
                         (list ov1)))))

      (when noninteractive
        (erc-spelling-mode -1)))))

;;; erc-scenarios-spelling.el ends here
