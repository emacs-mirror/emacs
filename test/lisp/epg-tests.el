;;; epg-tests.el --- Test suite for epg.el -*- lexical-binding: t -*-

;; Copyright (C) 2013-2026 Free Software Foundation, Inc.

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
(require 'ert-x)
(require 'epg)

(defvar epg-tests-context nil)

(defconst epg-tests--config-program-alist
  ;; The default `epg-config--program-alist' requires gpg2 2.1 or
  ;; greater due to some practical problems with pinentry.  But most
  ;; tests here work fine with 2.0 as well.
  (let ((prog-alist (copy-tree epg-config--program-alist)))
    (setf (alist-get "gpg2"
                     (alist-get 'OpenPGP prog-alist)
                     nil nil #'equal)
          "2.0")
    prog-alist))

(defun epg-tests-find-usable-gpg-configuration
    (&optional require-passphrase require-public-key)
  ;; Clear config cache because we may be using a different
  ;; program-alist.  We do want to update the cache, so that
  ;; `epg-make-context' can use our result.
  (setq epg--configurations nil)
  (epg-find-configuration 'OpenPGP nil
                          ;; The symmetric operations fail on Hydra
                          ;; with gpg 2.0.
                          (if (or (not require-passphrase) require-public-key)
                              epg-tests--config-program-alist)))

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
  `(ert-with-temp-directory epg-tests-home-directory
     (let* ((process-environment
             (append
              (list "GPG_AGENT_INFO"
                    (format "GNUPGHOME=%s" epg-tests-home-directory))
              process-environment)))
       ;; GNUPGHOME is needed to find a usable gpg, so we can't
       ;; check whether to skip any earlier (Bug#23561).
       (let ((epg-config (or (epg-tests-find-usable-gpg-configuration
                           ,require-passphrase ,require-public-key)
                          (ert-skip "No usable gpg config")))
             (context (epg-make-context 'OpenPGP)))
         (setf (epg-context-program context)
               (alist-get 'program epg-config))
         (setf (epg-context-home-directory context)
               epg-tests-home-directory)
         ,(if require-passphrase
              '(with-temp-file (expand-file-name
                                "gpg-agent.conf" epg-tests-home-directory)
                 (insert "pinentry-program "
                         (ert-resource-file "dummy-pinentry")
                         "\n")
                 (epg-context-set-passphrase-callback
                  context
                  #'epg-tests-passphrase-callback)))
         ,(if require-public-key
              '(epg-import-keys-from-file
                context
                (ert-resource-file "pubkey.asc")))
         ,(if require-secret-key
              '(epg-import-keys-from-file
                context
                (ert-resource-file "seckey.asc")))
         (with-temp-buffer
           (setq-local epg-tests-context context)
           ,@body)))))

(ert-deftest epg-decrypt-1 ()
  :expected-result (if (getenv "EMACS_HYDRA_CI") :failed :passed) ; fixme
  (with-epg-tests (:require-passphrase t)
    (should (equal "test"
		   (epg-decrypt-string epg-tests-context "\
-----BEGIN PGP MESSAGE-----

jA0ECQMCdW8+qtS9Tin/0jUBO1/9Oz69BWPmtFKEeBM62WpFP4o1+bNzdxogdyeg
+WTt292OD0yV85m5UqvLgp4ttVUmAw==
=K5Eh
-----END PGP MESSAGE-----
")))))

(defun epg--gnupg-version-is-not-buggy ()
  ;; We need to skip some versions of GnuPG, as they make tests hang.
  ;; See Bug#63256 and https://dev.gnupg.org/T6481 as well as PROBLEMS.
  ;; Known bad versions for now are 2.4.1--2.4.3.
  (not (string-match (rx bos "gpg (GnuPG) 2.4." (+ digit))
                     (shell-command-to-string "gpg --version"))))

(ert-deftest epg-roundtrip-1 ()
  :expected-result (if (getenv "EMACS_HYDRA_CI") :failed :passed) ; fixme
  (skip-unless (epg--gnupg-version-is-not-buggy))
  (with-epg-tests (:require-passphrase t)
    (let ((cipher (epg-encrypt-string epg-tests-context "symmetric" nil)))
      (should (equal "symmetric"
		     (epg-decrypt-string epg-tests-context cipher))))))

(ert-deftest epg-roundtrip-2 ()
  (skip-unless (epg--gnupg-version-is-not-buggy))
  (with-epg-tests (:require-passphrase t
		   :require-public-key t
		   :require-secret-key t)
    (let* ((recipients (epg-list-keys epg-tests-context "alice@openpgp.example"))
	   (cipher (epg-encrypt-string epg-tests-context "public key"
				       recipients nil t)))
      (should (equal "public key"
		     (epg-decrypt-string epg-tests-context cipher))))))

(ert-deftest epg-sign-verify-1 ()
  (with-epg-tests (:require-passphrase t
		   :require-public-key t
		   :require-secret-key t)
    (let (signature verify-result)
      (setf (epg-context-signers epg-tests-context)
	    (epg-list-keys epg-tests-context "alice@openpgp.example"))
      (setq signature (epg-sign-string epg-tests-context "signed" t))
      (epg-verify-string epg-tests-context signature "signed")
      (setq verify-result (epg-context-result-for context 'verify))
      (should (= 1 (length verify-result)))
      (should (eq 'good (epg-signature-status (car verify-result)))))))

(ert-deftest epg-sign-verify-2 ()
  (with-epg-tests (:require-passphrase t
		   :require-public-key t
		   :require-secret-key t)
    (let (signature verify-result)
      (setf (epg-context-signers epg-tests-context)
	    (epg-list-keys epg-tests-context "alice@openpgp.example"))
      (setq signature (epg-sign-string epg-tests-context "clearsigned" 'clear))
      ;; Clearsign signature always ends with a new line.
      (should (equal "clearsigned\n"
		     (epg-verify-string epg-tests-context signature)))
      (setq verify-result (epg-context-result-for context 'verify))
      (should (= 1 (length verify-result)))
      (should (eq 'good (epg-signature-status (car verify-result)))))))

(ert-deftest epg-sign-verify-3 ()
  (with-epg-tests (:require-passphrase t
		   :require-public-key t
		   :require-secret-key t)
    (let (signature verify-result)
      (setf (epg-context-signers epg-tests-context)
	    (epg-list-keys epg-tests-context "alice@openpgp.example"))
      (setq signature (epg-sign-string epg-tests-context "normal signed"))
      (should (equal "normal signed"
		     (epg-verify-string epg-tests-context signature)))
      (setq verify-result (epg-context-result-for context 'verify))
      (should (= 1 (length verify-result)))
      (should (eq 'good (epg-signature-status (car verify-result)))))))

(ert-deftest epg-import-1 ()
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
