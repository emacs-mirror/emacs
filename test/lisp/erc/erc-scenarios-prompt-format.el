;;; erc-scenarios-prompt-format.el --- erc-prompt-format-mode -*- lexical-binding: t -*-

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

(defvar erc-fill-wrap-align-prompt)
(defvar erc-fill-wrap-use-pixels)

(defun erc-scenarios-prompt-format--assert (needle &rest props)
  (save-excursion
    (goto-char erc-insert-marker)
    (should (search-forward needle nil t))
    (pcase-dolist (`(,k . ,v) props)
      (should (equal (get-text-property (point) k) v)))))

;; This makes assertions about the option `erc-fill-wrap-align-prompt'
;; as well as the standard value of `erc-prompt-format'.  One minor
;; omission is that this doesn't check behavior in query buffers.
(ert-deftest erc-scenarios-prompt-format ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/modes")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'chan-changed))
       (erc-modules (cons 'fill-wrap erc-modules))
       (erc-fill-wrap-align-prompt t)
       (erc-fill-wrap-use-pixels nil)
       (erc-prompt #'erc-prompt-format)
       (erc-autojoin-channels-alist '((Libera.Chat "#chan")))
       (expect (erc-d-t-make-expecter))
       ;; Collect samples of `line-prefix' to verify deltas as the
       ;; prompt grows and shrinks.
       (line-prefixes nil)
       (stash-pfx (lambda ()
                    (pcase (get-text-property erc-insert-marker 'line-prefix)
                      (`(space :width (- erc-fill--wrap-value ,n))
                       (car (push n line-prefixes)))))))

    (ert-info ("Connect to Libera.Chat")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port (process-contact dumb-server :service)
                                :nick "tester"
                                :full-name "tester")
        (funcall expect 5 "Welcome to the Libera.Chat")
        (funcall stash-pfx)
        (funcall expect 5 "changed mode")
        ;; New prompt is shorter than default with placeholders, like
        ;; "(foo?)(bar?)" (assuming we win the inherent race).
        (should (>= (car line-prefixes) (funcall stash-pfx)))
        (erc-scenarios-prompt-format--assert "user-" '(display . ("Ziw")))))

    (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan"))
      (should-not erc-channel-key)
      (should-not erc-channel-user-limit)

      (ert-info ("Receive notice that mode has changed")
        (erc-d-t-wait-for 10 (equal erc-channel-modes '("n" "t")))
        (funcall stash-pfx)
        (erc-scenarios-common-say "ready before")
        (funcall expect 10 " has changed mode for #chan to +Qu")
        (erc-d-t-wait-for 10 (equal erc-channel-modes '("Q" "n" "t" "u")))
        ;; Prompt is longer now, so too is the `line-prefix' subtrahend.
        (should (< (car line-prefixes) (funcall stash-pfx)))
        (erc-scenarios-prompt-format--assert "Qntu")
        (erc-scenarios-prompt-format--assert "#chan>"))

      (ert-info ("Key stored locally")
        (erc-scenarios-common-say "ready key")
        (funcall expect 10 " has changed mode for #chan to +k hunter2")
        ;; Prompt has grown by 1.
        (should (< (car line-prefixes) (funcall stash-pfx)))
        (erc-scenarios-prompt-format--assert "Qkntu"))

      (ert-info ("Limit stored locally")
        (erc-scenarios-common-say "ready limit")
        (funcall expect 10 " has changed mode for #chan to +l 3")
        (erc-d-t-wait-for 10 (eql erc-channel-user-limit 3))
        (should (equal erc-channel-modes '("Q" "n" "t" "u")))
        ;; Prompt has grown by 1 again.
        (should (< (car line-prefixes) (funcall stash-pfx)))
        (erc-scenarios-prompt-format--assert "Qklntu"))

      (ert-info ("Modes removed and local state deletion succeeds")
        (erc-scenarios-common-say "ready drop")
        (funcall expect 10 " has changed mode for #chan to -lu")
        (funcall expect 10 " has changed mode for #chan to -Qk *")
        (erc-d-t-wait-for 10 (equal erc-channel-modes '("n" "t")))
        ;; Prompt has shrunk.
        (should (> (car line-prefixes) (funcall stash-pfx)))
        (erc-scenarios-prompt-format--assert "nt"))

      (should-not erc-channel-key)
      (should-not erc-channel-user-limit)
      (funcall expect 10 "<Chad> after"))))

;;; erc-scenarios-prompt-format.el ends here
