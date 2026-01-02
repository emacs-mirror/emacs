;;; erc-button-tests.el --- Tests for erc-button  -*- lexical-binding:t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

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
(require 'erc-button)

(require 'ert-x) ; cl-lib
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-tests-common)))

(ert-deftest erc-button-alist--url ()
  (erc-tests-common-init-server-proc "sleep" "1")
  (with-current-buffer (erc--open-target "#chan")
    (let ((verify
           (lambda (p url)
             (should (equal (get-text-property p 'erc-data) (list url)))
             (should (equal (get-text-property p 'mouse-face) 'highlight))
             (should (eq (get-text-property p 'font-lock-face) 'erc-button))
             (should (eq (get-text-property p 'erc-callback)
                         'browse-url-button-open-url)))))
      (goto-char (point-min))

      ;; Most common (unbracketed)
      (erc-display-message nil nil (current-buffer)
                           "Foo https://example.com bar.")
      (search-forward "https")
      (funcall verify (point) "https://example.com")

      ;; The <URL: form> still works despite being removed in ERC 5.6.
      (erc-display-message nil nil (current-buffer)
                           "Foo <URL: https://gnu.org> bar.")
      (search-forward "https")
      (funcall verify (point) "https://gnu.org")

      ;; Bracketed
      (erc-display-message nil nil (current-buffer) "Foo <ftp://gnu.org> bar.")
      (search-forward "ftp")
      (funcall verify (point) "ftp://gnu.org"))

    (when noninteractive
      (kill-buffer))))

(defvar erc-button-tests--form nil)
(defvar erc-button-tests--some-var nil)

(defun erc-button-tests--form (&rest rest)
  (push rest erc-button-tests--form)
  (apply #'erc-button-add-button rest))

(defun erc-button-tests--erc-button-alist--function-as-form (func)
  (erc-tests-common-init-server-proc "sleep" "1")

  (with-current-buffer (erc--open-target "#chan")
    (let* ((erc-button-tests--form nil)
           (entry (list (rx "+1") 0 func #'ignore 0))
           (erc-button-alist (cons entry erc-button-alist)))

      (erc-tests-common-display-message nil 'notice (current-buffer)
                                        "Foo bar baz")
      (erc-tests-common-display-message nil nil (current-buffer) "+1")
      (erc-tests-common-display-message nil 'notice (current-buffer) "Spam")

      (should (equal (pop erc-button-tests--form)
                     '(53 55 ignore nil ("+1") "\\+1")))
      (should-not erc-button-tests--form)
      (goto-char (point-min))
      (search-forward "+")
      (should (equal (get-text-property (point) 'erc-data) '("+1")))
      (should (equal (get-text-property (point) 'mouse-face) 'highlight))
      (should (eq (get-text-property (point) 'font-lock-face) 'erc-button))
      (should (eq (get-text-property (point) 'erc-callback) 'ignore)))

    (when noninteractive
      (kill-buffer))))

(ert-deftest erc-button-alist--function-as-form ()
  (erc-button-tests--erc-button-alist--function-as-form
   #'erc-button-tests--form)

  (erc-button-tests--erc-button-alist--function-as-form
   (symbol-function #'erc-button-tests--form))

  (erc-button-tests--erc-button-alist--function-as-form
   (lambda (&rest r) (push r erc-button-tests--form)
     (apply #'erc-button-add-button r))))

(defun erc-button-tests--erc-button-alist--nil-form (form)
  (erc-tests-common-init-server-proc "sleep" "1")

  (with-current-buffer (erc--open-target "#chan")
    (let* ((erc-button-tests--form nil)
           (entry (list (rx "+1") 0 form #'ignore 0))
           (erc-button-alist (cons entry erc-button-alist)))

      (erc-display-message nil 'notice (current-buffer) "Foo bar baz")
      (erc-display-message nil nil (current-buffer) "+1")
      (erc-display-message nil 'notice (current-buffer) "Spam")
      (should-not erc-button-tests--form)
      (goto-char (point-min))
      (search-forward "+")
      (should-not (get-text-property (point) 'erc-data))
      (should-not (get-text-property (point) 'mouse-face))
      (should-not (get-text-property (point) 'font-lock-face))
      (should-not (get-text-property (point) 'erc-callback)))

    (when noninteractive
      (kill-buffer))))

(ert-deftest erc-button-alist--nil-form ()
  (erc-button-tests--erc-button-alist--nil-form nil)
  (erc-button-tests--erc-button-alist--nil-form 'erc-button-tests--some-var))

(defun erc-button-tests--insert-privmsg (speaker &rest msg-parts)
  (declare (indent 1))
  (let ((msg (erc-format-privmessage speaker
                                     (apply #'concat msg-parts) nil t)))
    (erc-display-message nil nil (current-buffer) msg)))

(defun erc-button-tests--populate (test)
  (let ((inhibit-message noninteractive)
        erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)

    (with-current-buffer
        (cl-letf
            (((symbol-function 'erc-server-connect)
              (lambda (&rest _)
                (setq erc-server-process
                      (start-process "sleep" (current-buffer) "sleep" "1"))
                (set-process-query-on-exit-flag erc-server-process nil))))

          (erc-open "localhost" 6667 "tester" "Tester" 'connect
                    nil nil nil nil nil "tester" 'foonet))

      (with-current-buffer (erc--open-target "#chan")
        (erc-update-channel-member
         "#chan" "alice" "alice" t nil nil nil nil nil "fake" "~u" nil nil t)

        (erc-update-channel-member
         "#chan" "bob" "bob" t nil nil nil nil nil "fake" "~u" nil nil t)

        (erc-display-message
         nil 'notice (current-buffer)
         (concat "This server is in debug mode and is logging all user I/O. "
                 "Blah alice (1) bob (2) blah."))

        (funcall test))

      (when noninteractive
        (kill-buffer "#chan")
        (kill-buffer)))))

(ert-deftest erc-button-next ()
  (erc-button-tests--populate
   (lambda ()
     (erc-button-tests--insert-privmsg "alice"
       "(3) bob (4) come, you are a tedious fool: to the purpose.")

     (erc-button-tests--insert-privmsg "bob"
       "(5) alice (6) Come me to what was done to her.")

     (should (= erc-input-marker (point)))

     ;; Break out of input area
     (erc-button-previous 1)
     (should (looking-at (rx "alice (6)")))

     ;; No next button
     (should-error (erc-button-next 1) :type 'user-error)
     (should (looking-at (rx "alice (6)")))

     ;; Next with negative arg is equivalent to previous
     (erc-button-next -1)
     (should (looking-at (rx "bob> (5)")))

     ;; One past end of button
     (forward-char 3)
     (should (looking-at (rx "> (5)")))
     (should-not (get-text-property (point) 'erc-callback))
     (erc-button-previous 1)
     (should (looking-at (rx "bob> (5)")))

     ;; At end of button
     (forward-char 2)
     (should (looking-at (rx "b> (5)")))
     (erc-button-previous 1)
     (should (looking-at (rx "bob (4)")))

     ;; Skip multiple buttons back
     (erc-button-previous 2)
     (should (looking-at (rx "bob (2)")))

     ;; Skip multiple buttons forward
     (erc-button-next 2)
     (should (looking-at (rx "bob (4)")))

     ;; No error as long as some progress made
     (erc-button-previous 100)
     (should (looking-at (rx "alice (1)")))

     ;; Error when no progress made
     (should-error (erc-button-previous 1) :type 'user-error)
     (should (looking-at (rx "alice (1)"))))))

;; See also `erc-scenarios-networks-announced-missing' in
;; erc-scenarios-misc.el for a more realistic example.
(ert-deftest erc-button--display-error-notice-with-keys ()
  (with-current-buffer (get-buffer-create "*fake*")
    (let ((mode erc-button-mode)
          (inhibit-message noninteractive)
          erc-modules
          erc-kill-channel-hook erc-kill-server-hook erc-kill-buffer-hook)
      (erc-tests-common-prep-for-insertion)
      (erc-tests-common-init-server-proc "sleep" "1")

      (erc-button-mode +1)
      (should (equal (erc-button--display-error-notice-with-keys
                      "If \\[erc-bol] fails, "
                      "see \\[erc-bug] or `erc-mode-map'.")
                     "*** If C-a fails, see M-x erc-bug or `erc-mode-map'."))
      (goto-char (point-min))

      (ert-info ("Keymap substitution succeeds")
        (erc-button-next 1)
        (should (looking-at "C-a"))
        (should (eq (get-text-property (point) 'mouse-face) 'highlight))
        (erc-button-press-button)
        (with-current-buffer "*Help*"
          (goto-char (point-min))
          (should (search-forward "erc-bol" nil t)))
        (erc-button-next 1)
        ;; End of interval correct
        (erc-button-previous 1)
        (should (looking-at "C-a fails")))

      (ert-info ("Extended command mapping succeeds")
        (erc-button-next 1)
        (should (looking-at "M-x erc-bug"))
        (erc-button-press-button)
        (should (eq (get-text-property (point) 'mouse-face) 'highlight))
        (with-current-buffer "*Help*"
          (goto-char (point-min))
          (should (search-forward "erc-bug" nil t))))

      (ert-info ("Symbol-description face preserved") ; mutated by d-e-n-w-k
        (erc-button-next 1)
        (should (equal (get-text-property (point) 'font-lock-face)
                       '(erc-button erc-error-face erc-notice-face)))
        (should (eq (get-text-property (point) 'mouse-face) 'highlight))
        (should (eq erc-button-face 'erc-button))) ; extent evaporates

      (ert-info ("Format when trailing args include non-strings")
        (should (equal (erc-button--display-error-notice-with-keys
                        "abc" " %d def" " 45%s" 123 '\6)
                       "*** abc 123 def 456")))

      (ert-info ("Respects buffer as first argument when given")
        (should (equal (erc-button--display-error-notice-with-keys
                        (make-erc-response) "abc") ; compat
                       "*** abc"))
        (should (equal (erc-button--display-error-notice-with-keys
                        (current-buffer) "abc")
                       "*** abc")))

      (ert-info ("Accounts for nil members when concatenating")
        (should (equal (erc-button--display-error-notice-with-keys
                        "a" nil)
                       "*** a"))
        (should (equal (erc-button--display-error-notice-with-keys
                        "a" nil " b")
                       "*** a b"))
        (should (equal (erc-button--display-error-notice-with-keys
                        "a: %d" nil 1)
                       "*** a: 1"))
        (should (equal (erc-button--display-error-notice-with-keys
                        "a: %d %s" 1 nil)
                       "*** a: 1 nil"))
        (should (equal (erc-button--display-error-notice-with-keys
                        "a: " "%d %s" 1 nil)
                       "*** a: 1 nil"))
        (should (equal (erc-button--display-error-notice-with-keys
                        "a: " nil "%d %s" 1 nil)
                       "*** a: 1 nil")))

      (when noninteractive
        (unless mode
          (erc-button-mode -1))
        (kill-buffer "*Help*")
        (kill-buffer)))))

;;; erc-button-tests.el ends here
