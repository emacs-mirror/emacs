;;; erc-desktop-notifications-tests.el --- Notifications tests  -*- lexical-binding:t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:
(require 'erc-desktop-notifications)

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-tests-common)))

(defun erc-desktop-notifications-tests--perform (test)
  (erc-tests-common-make-server-buf)
  (erc-notifications-mode +1)
  (setq erc-server-current-nick "tester")

  (unwind-protect
      (cl-letf* ((calls nil)
                 ((frame-parameter nil 'last-focus-update)
                  t)
                 ((symbol-function 'erc-notifications-notify)
                  (lambda (&rest r) (push r calls))))
        (with-current-buffer (erc--open-target "#chan")
          (funcall test (lambda () (prog1 calls (setq calls nil))))))

    (when noninteractive
      (erc-notifications-mode -1)
      (erc-tests-common-kill-buffers))))

(defun erc-desktop-notifications-tests--populate-chan (test)
  (erc-desktop-notifications-tests--perform
   (lambda (check)
     (erc-tests-common-add-cmem "bob")
     (erc-tests-common-add-cmem "alice")

     (erc-tests-common-simulate-line
      ":irc.foonet.org 353 tester = #chan :alice bob tester")
     (erc-tests-common-simulate-line
      ":irc.foonet.org 366 tester #chan :End of NAMES list")
     (erc-tests-common-simulate-privmsg "bob" "hi tester")

     (should (equal (current-buffer) (get-buffer "#chan")))
     (should (not (eq (current-buffer) (window-buffer)))) ; *ert* or *scratch*
     (funcall test check))))

(ert-deftest erc-desktop-notifications-focused-contexts/default ()
  (should-not erc-desktop-notifications-ignored-when-focused)

  (erc-desktop-notifications-tests--populate-chan
   (lambda (check)

     ;; A private query triggers a notification.
     (erc-tests-common-simulate-line ":bob!~bob@fsf.org PRIVMSG tester yo")
     (should (eq (current-buffer) (get-buffer "bob")))

     ;; A NOTICE command doesn't trigger a notification (unless
     ;; `erc-desktop-notifications--query-NOTICE-p' is non-nil).
     (erc-tests-common-simulate-line ":irc.foonet.org NOTICE tester nope")

     (should (equal (funcall check)
                    '(("bob" "yo")
                      ("bob" "hi tester\n"))))

     ;; Setting the window to the buffer where insertions are happening
     ;; makes no difference: notifications are still sent.
     (erc-tests-common-simulate-line ":bob!~bob@fsf.org PRIVMSG tester ho")

     (ert-with-buffer-selected "#chan"
       (erc-tests-common-simulate-privmsg "alice" "hi tester")

       (should (equal (funcall check)
                      '(("alice" "hi tester\n") ("bob" "ho"))))))))

(ert-deftest erc-desktop-notifications-focused-contexts/unselected ()
  (should-not erc-desktop-notifications-ignored-when-focused)

  (let ((erc-desktop-notifications-ignored-when-focused '(query mention)))

    (erc-desktop-notifications-tests--populate-chan
     (lambda (check)
       (should (equal (funcall check) '(("bob" "hi tester\n"))))

       ;; Buffer #chan is current and displayed in the selected window,
       ;; so no notification is sent.
       (ert-with-buffer-selected "#chan"
         (erc-tests-common-simulate-privmsg "alice" "hi tester")

         ;; A new query arrives for a buffer that doesn't exist.  The
         ;; option `erc-receive-query-display' tells ERC to switch to
         ;; that buffer and show it before insertion.  Therefore, no
         ;; notification is sent.
         (let ((erc-receive-query-display 'buffer))
           (erc-tests-common-simulate-line
            ":bob!~bob@fsf.org PRIVMSG tester yo")))

       (should-not (funcall check))))))

(ert-deftest erc-desktop-notifications-skip-predicates/fools ()
  (erc-desktop-notifications-tests--populate-chan
   (lambda (check)

     ;; A private query triggers a notification.
     (erc-tests-common-simulate-line ":bob!~bob@fsf.org PRIVMSG tester yo")
     (should (eq (current-buffer) (get-buffer "bob")))

     (should (equal (funcall check)
                    '(("bob" "yo")
                      ("bob" "hi tester\n"))))

     (let ((erc-fools '("bob")))

       ;; A query from is suppressed if bob is a fool.
       (erc-tests-common-simulate-line ":bob!~bob@fsf.org PRIVMSG tester ho")
       (should-not (funcall check))

       ;; A mention from bob is suppressed if bob is a fool.
       (with-current-buffer "#chan"
         (erc-tests-common-simulate-privmsg "bob" "hi tester")
         (erc-tests-common-simulate-privmsg "alice" "hi tester")
         (should (equal (funcall check) '(("alice" "hi tester\n")))))))))

;;; erc-desktop-notifications-tests.el ends here
