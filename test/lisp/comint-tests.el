;;; comint-testsuite.el

;; Copyright (C) 2010-2020 Free Software Foundation, Inc.

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

;; Tests for comint and related modes.

;;; Code:

(require 'comint)
(require 'ert)

(defvar comint-testsuite-password-strings
  '("foo@example.net's password: " ; ssh
    "Password for foo@example.org: " ; kinit
    "Password for 'https://foo@example.org':"           ; git push Bug#20910
    "Please enter the password for foo@example.org: "   ; kinit
    "Kerberos password for devnull/root <at> GNU.ORG: " ; ksu
    "Enter passphrase: " ; ssh-add
    "Enter passphrase (empty for no passphrase): " ; ssh-keygen
    "Enter same passphrase again: "     ; ssh-keygen
    "Enter your password: "             ; python3 -m twine ... Bug#37636
    "Passphrase for key root@GNU.ORG: " ; plink
    "[sudo] password for user:" ; Ubuntu sudo
    "[sudo] user 的密码：" ; localized
    "PIN for user:"        ; Bug#35523
    "Password (again):"
    "Enter password:"
    "Enter Auth Password:" ; OpenVPN (Bug#35724)
    "Mot de Passe :" ; localized (Bug#29729)
    "Passwort:") ; localized
  "List of strings that should match `comint-password-prompt-regexp'.")

(ert-deftest comint-test-password-regexp ()
  "Test `comint-password-prompt-regexp' against common password strings."
  (dolist (str comint-testsuite-password-strings)
    (should (string-match comint-password-prompt-regexp str))))

(ert-deftest comint-test-no-password-function ()
  "Test that `comint-password-function' not being set does not
alter normal password flow."
  (cl-letf
      (((symbol-function 'read-passwd)
        (lambda (_prompt &optional _confirm _default)
          "PaSsWoRd123")))
    (let ((cat (executable-find "cat")))
      (when cat
        (with-temp-buffer
          (make-comint-in-buffer "test-comint-password" (current-buffer) cat)
          (let ((proc (get-buffer-process (current-buffer))))
            (set-process-query-on-exit-flag proc nil)
            (comint-send-string proc "Password: ")
            (comint-send-eof)
            (while (accept-process-output proc 0.1 nil t))
            (should (string-equal (buffer-substring-no-properties (point-min) (point-max))
                                  "Password: PaSsWoRd123\n"))
            (when (process-live-p proc)
              (kill-process proc))
            (accept-process-output proc 0 1 t)))))))

(ert-deftest comint-test-password-function-with-value ()
  "Test that `comint-password-function' alters normal password
flow.  Hook function returns alternative password."
  (cl-letf
      (((symbol-function 'read-passwd)
        (lambda (_prompt &optional _confirm _default)
          "PaSsWoRd123")))
    (let ((cat (executable-find "cat"))
          (comint-password-function (lambda (_prompt) "MaGiC-PaSsWoRd789")))
      (when cat
        (with-temp-buffer
          (make-comint-in-buffer "test-comint-password" (current-buffer) cat)
          (let ((proc (get-buffer-process (current-buffer))))
            (set-process-query-on-exit-flag proc nil)
            (comint-send-string proc "Password: ")
            (comint-send-eof)
            (while (accept-process-output proc 0.1 nil t))
            (should (string-equal (buffer-substring-no-properties (point-min) (point-max))
                                  "Password: MaGiC-PaSsWoRd789\n"))
            (when (process-live-p proc)
              (kill-process proc))
            (accept-process-output proc 0 1 t)))))))

(ert-deftest comint-test-password-function-with-nil ()
  "Test that `comint-password-function' does not alter the normal
password flow if it returns a nil value."
  (cl-letf
      (((symbol-function 'read-passwd)
        (lambda (_prompt &optional _confirm _default)
          "PaSsWoRd456")))
    (let ((cat (executable-find "cat"))
          (comint-password-function (lambda (_prompt) nil)))
      (when cat
        (with-temp-buffer
          (make-comint-in-buffer "test-comint-password" (current-buffer) cat)
          (let ((proc (get-buffer-process (current-buffer))))
            (set-process-query-on-exit-flag proc nil)
            (comint-send-string proc "Password: ")
            (comint-send-eof)
            (while (accept-process-output proc 0.1 nil t))
            (should (string-equal (buffer-substring-no-properties (point-min) (point-max))
                                  "Password: PaSsWoRd456\n"))
            (when (process-live-p proc)
              (kill-process proc))
            (accept-process-output proc 0 1 t)))))))

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; comint-testsuite.el ends here
