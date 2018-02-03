;;; comint-testsuite.el

;; Copyright (C) 2010-2018 Free Software Foundation, Inc.

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
    "Please enter the password for foo@example.org: "   ; kinit
    "Kerberos password for devnull/root <at> GNU.ORG: " ; ksu
    "Enter passphrase: " ; ssh-add
    "Enter passphrase (empty for no passphrase): " ; ssh-keygen
    "Enter same passphrase again: "     ; ssh-keygen
    "Passphrase for key root@GNU.ORG: " ; plink
    "[sudo] password for user:" ; Ubuntu sudo
    "Password (again):"
    "Enter password:"
    "Mot de Passe:" ; localized
    "Passwort:") ; localized
  "List of strings that should match `comint-password-prompt-regexp'.")

(ert-deftest comint-test-password-regexp ()
  "Test `comint-password-prompt-regexp' against common password strings."
  (dolist (str comint-testsuite-password-strings)
    (should (string-match comint-password-prompt-regexp str))))

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; comint-testsuite.el ends here
