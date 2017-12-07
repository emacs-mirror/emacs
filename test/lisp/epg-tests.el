;;; epg-tests.el --- Test suite for epg.el -*- lexical-binding: t -*-

;; Copyright (C) 2013-2017 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'epg)

(defvar epg-tests-context nil)

(defvar epg-tests-data-directory
  (expand-file-name "data/epg" (getenv "EMACS_TEST_DIRECTORY"))
  "Directory containing epg test data.")

(defun epg-tests-find-usable-gpg-configuration (&optional _require-passphrase)
  (epg-find-configuration 'OpenPGP 'no-cache))

(defun epg-tests-passphrase-callback (_c _k _d)
  ;; Need to create a copy here, since the string will be wiped out
  ;; after the use.
  (copy-sequence "test0123456789"))

(cl-defmacro with-epg-tests ((&optional &key require-passphrase
					require-public-key
					require-secret-key)
			    &rest body)
  "Set up temporary locations and variables for testing."
  (declare (indent 1) (debug (sexp body)))
  `(let* ((epg-tests-home-directory (make-temp-file "epg-tests-homedir" t))
	  (process-environment
	   (append
	    (list "GPG_AGENT_INFO"
		  (format "GNUPGHOME=%s" epg-tests-home-directory))
	    process-environment)))
     (unwind-protect
	 (let ((context (epg-make-context 'OpenPGP)))
           (setf (epg-context-program context)
                 (alist-get 'program
                            (epg-tests-find-usable-gpg-configuration
                             ,(if require-passphrase
                                  `'require-passphrase))))
	   (setf (epg-context-home-directory context)
		 epg-tests-home-directory)
	   ,(if require-passphrase
		`(with-temp-file (expand-file-name
                                  "gpg-agent.conf" epg-tests-home-directory)
                   (insert "pinentry-program "
                           (expand-file-name "dummy-pinentry"
                                             epg-tests-data-directory)
                           "\n")
                   (epg-context-set-passphrase-callback
                    context
                    #'epg-tests-passphrase-callback)))
	   ,(if require-public-key
		`(epg-import-keys-from-file
		  context
		  (expand-file-name "pubkey.asc" epg-tests-data-directory)))
	   ,(if require-secret-key
		`(epg-import-keys-from-file
		  context
		  (expand-file-name "seckey.asc" epg-tests-data-directory)))
	   (with-temp-buffer
	     (make-local-variable 'epg-tests-context)
	     (setq epg-tests-context context)
	     ,@body))
       (when (file-directory-p epg-tests-home-directory)
	 (delete-directory epg-tests-home-directory t)))))

(ert-deftest epg-decrypt-1 ()
  (skip-unless (epg-tests-find-usable-gpg-configuration 'require-passphrase))
  (with-epg-tests (:require-passphrase t)
    (should (equal "test"
		   (epg-decrypt-string epg-tests-context "\
-----BEGIN PGP MESSAGE-----
Version: GnuPG v2

jA0EAwMCE19JBLTvvmhgyRrGGglRbnKkK9PJG8fDwO5ccjysrR7IcdNcnA==
=U8z7
-----END PGP MESSAGE-----")))))

(ert-deftest epg-roundtrip-1 ()
  (skip-unless (epg-tests-find-usable-gpg-configuration 'require-passphrase))
  (with-epg-tests (:require-passphrase t)
    (let ((cipher (epg-encrypt-string epg-tests-context "symmetric" nil)))
      (should (equal "symmetric"
		     (epg-decrypt-string epg-tests-context cipher))))))

(ert-deftest epg-roundtrip-2 ()
  (skip-unless (epg-tests-find-usable-gpg-configuration 'require-passphrase))
  (with-epg-tests (:require-passphrase t
		   :require-public-key t
		   :require-secret-key t)
    (let* ((recipients (epg-list-keys epg-tests-context "joe@example.com"))
	   (cipher (epg-encrypt-string epg-tests-context "public key"
				       recipients nil t)))
      (should (equal "public key"
		     (epg-decrypt-string epg-tests-context cipher))))))

(ert-deftest epg-sign-verify-1 ()
  (skip-unless (epg-tests-find-usable-gpg-configuration 'require-passphrase))
  (with-epg-tests (:require-passphrase t
		   :require-public-key t
		   :require-secret-key t)
    (let (signature verify-result)
      (setf (epg-context-signers epg-tests-context)
	    (epg-list-keys epg-tests-context "joe@example.com"))
      (setq signature (epg-sign-string epg-tests-context "signed" t))
      (epg-verify-string epg-tests-context signature "signed")
      (setq verify-result (epg-context-result-for context 'verify))
      (should (= 1 (length verify-result)))
      (should (eq 'good (epg-signature-status (car verify-result)))))))

(ert-deftest epg-sign-verify-2 ()
  (skip-unless (epg-tests-find-usable-gpg-configuration 'require-passphrase))
  (with-epg-tests (:require-passphrase t
		   :require-public-key t
		   :require-secret-key t)
    (let (signature verify-result)
      (setf (epg-context-signers epg-tests-context)
	    (epg-list-keys epg-tests-context "joe@example.com"))
      (setq signature (epg-sign-string epg-tests-context "clearsigned" 'clear))
      ;; Clearsign signature always ends with a new line.
      (should (equal "clearsigned\n"
		     (epg-verify-string epg-tests-context signature)))
      (setq verify-result (epg-context-result-for context 'verify))
      (should (= 1 (length verify-result)))
      (should (eq 'good (epg-signature-status (car verify-result)))))))

(ert-deftest epg-sign-verify-3 ()
  (skip-unless (epg-tests-find-usable-gpg-configuration 'require-passphrase))
  (with-epg-tests (:require-passphrase t
		   :require-public-key t
		   :require-secret-key t)
    (let (signature verify-result)
      (setf (epg-context-signers epg-tests-context)
	    (epg-list-keys epg-tests-context "joe@example.com"))
      (setq signature (epg-sign-string epg-tests-context "normal signed"))
      (should (equal "normal signed"
		     (epg-verify-string epg-tests-context signature)))
      (setq verify-result (epg-context-result-for context 'verify))
      (should (= 1 (length verify-result)))
      (should (eq 'good (epg-signature-status (car verify-result)))))))

(ert-deftest epg-import-1 ()
  (skip-unless (epg-tests-find-usable-gpg-configuration 'require-passphrase))
  (with-epg-tests (:require-passphrase nil)
    (should (= 0 (length (epg-list-keys epg-tests-context))))
    (should (= 0 (length (epg-list-keys epg-tests-context nil t)))))
  (with-epg-tests (:require-passphrase nil
		   :require-public-key t)
    (should (= 1 (length (epg-list-keys epg-tests-context))))
    (should (= 0 (length (epg-list-keys epg-tests-context nil t)))))
  (with-epg-tests (:require-public-key nil
		   :require-public-key t
		   :require-secret-key t)
    (should (= 1 (length (epg-list-keys epg-tests-context))))
    (should (= 1 (length (epg-list-keys epg-tests-context nil t))))))

(provide 'epg-tests)

;;; epg-tests.el ends here
