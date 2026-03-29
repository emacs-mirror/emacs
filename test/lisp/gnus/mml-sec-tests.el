;;; mml-sec-tests.el --- Tests mml-sec.el, see README-mml-secure.txt.  -*- lexical-binding:t -*-

;; Copyright (C) 2015, 2020-2026 Free Software Foundation, Inc.

;; Author: Jens Lechtenbörger <jens.lechtenboerger@fsfe.org>

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

(require 'message)
(require 'epa)
(require 'epg)
(require 'mml-sec)
(require 'gnus-sum)

(defvar with-smime nil
  "If nil, exclude S/MIME from tests as passphrases need to entered manually.
Mostly, the empty passphrase is used.  However, the keys for
 \"No Expiry two UIDs\" have the passphrase \"Passphrase\" (for OpenPGP as well
 as S/MIME).")

(defun test-conf ()
  ;; Emacs doesn't have support for finding the name of the PGP agent
  ;; on MacOS, so disable the checks.
  (and (not (eq system-type 'darwin))
       (ignore-errors (epg-find-configuration 'OpenPGP))))

(defun enc-standards ()
  (if with-smime '(enc-pgp enc-pgp-mime enc-smime)
    '(enc-pgp enc-pgp-mime)))
(defun enc-sign-standards ()
  (if with-smime
      '(enc-sign-pgp enc-sign-pgp-mime enc-sign-smime)
    '(enc-sign-pgp enc-sign-pgp-mime)))
(defun sign-standards ()
  (if with-smime
      '(sign-pgp sign-pgp-mime sign-smime)
    '(sign-pgp sign-pgp-mime)))

(defvar mml-smime-use)

(defun mml-secure-test-fixture (body &optional interactive)
  "Setup GnuPG home containing test keys and prepare environment for BODY.
If optional INTERACTIVE is non-nil, allow questions to the user in case of
key problems.
This fixture temporarily unsets GPG_AGENT_INFO to enable passphrase tests,
which will neither work with gpgsm nor GnuPG 2.1 any longer, I guess.
Actually, I'm not sure why people would want to cache passwords in Emacs
instead of gpg-agent."
  (let ((agent-info (getenv "GPG_AGENT_INFO"))
	(gpghome (getenv "GNUPGHOME")))
    (unwind-protect
        (let ((epg-gpg-home-directory (ert-resource-directory))
	      (mml-smime-use 'epg)
	      ;; Create debug output in empty epg-debug-buffer.
	      (epg-debug t)
	      (epg-debug-buffer (get-buffer-create " *epg-test*"))
	      (mml-secure-fail-when-key-problem (not interactive)))
	  (with-current-buffer epg-debug-buffer
	    (erase-buffer))
	  ;; Unset GPG_AGENT_INFO to enable passphrase caching inside Emacs.
	  ;; Just for testing.  Jens does not recommend this for daily use.
	  (setenv "GPG_AGENT_INFO")
	  ;; Set GNUPGHOME as gpg-agent started by gpgsm does
	  ;; not look in the proper places otherwise, see:
	  ;; https://bugs.gnupg.org/gnupg/issue2126
	  (setenv "GNUPGHOME" epg-gpg-home-directory)
          (unwind-protect
	      (funcall body)
            (mml-sec-test--kill-gpg-agent)))
      (setenv "GPG_AGENT_INFO" agent-info)
      (setenv "GNUPGHOME" gpghome))))

(defun mml-secure-test-message-setup (method to from &optional text bcc)
  "Setup a buffer with MML METHOD, TO, and FROM headers.
Optionally, a message TEXT and BCC header can be passed."
  (with-temp-buffer
    (when bcc (insert (format "Bcc: %s\n" bcc)))
    (insert (format "To: %s
From: %s
Subject: Test
%s\n" to from mail-header-separator))
    (if text
	(insert (format "%s" text))
      (spook))
    (cond ((eq method 'enc-pgp-mime)
	   (mml-secure-message-encrypt-pgpmime 'nosig))
	  ((eq method 'enc-sign-pgp-mime)
	   (mml-secure-message-encrypt-pgpmime))
	  ((eq method 'enc-pgp) (mml-secure-message-encrypt-pgp 'nosig))
	  ((eq method 'enc-sign-pgp) (mml-secure-message-encrypt-pgp))
	  ((eq method 'enc-smime) (mml-secure-message-encrypt-smime 'nosig))
	  ((eq method 'enc-sign-smime) (mml-secure-message-encrypt-smime))
	  ((eq method 'sign-pgp-mime) (mml-secure-message-sign-pgpmime))
	  ((eq method 'sign-pgp) (mml-secure-message-sign-pgp))
	  ((eq method 'sign-smime) (mml-secure-message-sign-smime))
	  (t (error "Unknown method")))
    (buffer-string)))

(defun mml-secure-test-mail-fixture (method to from body2
					    &optional interactive)
  "Setup buffer encrypted using METHOD for TO from FROM, call BODY2.
Pass optional INTERACTIVE to mml-secure-test-fixture."
  (mml-secure-test-fixture
   (lambda ()
     (let ((_context (if (memq method '(enc-smime enc-sign-smime sign-smime))
                         (epg-make-context 'CMS)
                       (epg-make-context 'OpenPGP)))
	   ;; Verify and decrypt by default.
	   (mm-verify-option 'known)
	   (mm-decrypt-option 'known)
	   (plaintext "The Magic Words are Squeamish Ossifrage"))
       (with-temp-buffer
	 (insert (mml-secure-test-message-setup method to from plaintext))
	 (message-options-set-recipient)
	 (message-encode-message-body)
	 ;; Replace separator line with newline.
	 (goto-char (point-min))
	 (re-search-forward
	  (concat "^" (regexp-quote mail-header-separator) "\n"))
	 (replace-match "\n")
	 ;; The following treatment of handles, plainbuf, and multipart
	 ;; resulted from trial-and-error.
	 ;; Someone with more knowledge on how to decrypt messages and verify
	 ;; signatures might know more appropriate functions to invoke
	 ;; instead.
	 (let* ((handles (or (mm-dissect-buffer)
			     (mm-uu-dissect)))
		(isplain (bufferp (car handles)))
		(ismultipart (equal (car handles) "multipart/mixed"))
		(plainbuf (if isplain
			      (car handles)
			    (if ismultipart
				(car (cadadr handles))
			      (caadr handles))))
		(decrypted
		 (with-current-buffer plainbuf (buffer-string)))
		(gnus-info
		 (if isplain
		     nil
		   (if ismultipart
		       (or (mm-handle-multipart-ctl-parameter
			    (cadr handles) 'gnus-details)
			   (mm-handle-multipart-ctl-parameter
			    (cadr handles) 'gnus-info))
		     (mm-handle-multipart-ctl-parameter
		      handles 'gnus-info)))))
	   (funcall body2 gnus-info plaintext decrypted)))))
   interactive))

;; TODO If the variable BODY3 is renamed to BODY, an infinite recursion
;; occurs.  Emacs bug?
(defun mml-secure-test-key-fixture (body3)
  "Customize unique keys for sub@example.org and call BODY3.
For OpenPGP, we have:
- 1E6B FA97 3D9E 3103 B77F  D399 C399 9CF1 268D BEA2
  uid                  Different subkeys <sub@example.org>
- 1463 2ECA B9E2 2736 9C8D  D97B F7E7 9AB7 AE31 D471
  uid                  Second Key Pair <sub@example.org>

For S/MIME:
          ID: 0x479DC6E2
      Subject: /CN=Second Key Pair
          aka: sub@example.org
  fingerprint: 0E:58:22:9B:80:EE:33:95:9F:F7:18:FE:EF:25:40:2B:47:9D:C6:E2

           ID: 0x5F88E9FC
      Subject: /CN=Different subkeys
          aka: sub@example.org
  fingerprint: 4F:96:2A:B7:F4:76:61:6A:78:3D:72:AA:40:35:D5:9B:5F:88:E9:FC

In both cases, the first key is customized for signing and encryption."
  (mml-secure-test-fixture
   (lambda ()
     (let* ((mml-secure-key-preferences
	     '((OpenPGP (sign) (encrypt)) (CMS (sign) (encrypt))))
	    (pcontext (epg-make-context 'OpenPGP))
	    (pkey (epg-list-keys pcontext "2FAF8726121EB3C6"))
	    (scontext (epg-make-context 'CMS))
	    (skey (epg-list-keys scontext "0x479DC6E2")))
       (mml-secure-cust-record-keys pcontext 'encrypt "sub@example.org" pkey)
       (mml-secure-cust-record-keys pcontext 'sign "sub@example.org" pkey)
       (mml-secure-cust-record-keys scontext 'encrypt "sub@example.org" skey)
       (mml-secure-cust-record-keys scontext 'sign "sub@example.org" skey)
       (funcall body3)))))

(ert-deftest mml-secure-key-checks ()
  "Test mml-secure-check-user-id and mml-secure-check-sub-key on sample keys."
  (skip-unless (test-conf))
  (mml-secure-test-fixture
   (lambda ()
     (let* ((context (epg-make-context 'OpenPGP))
	    (keys1 (epg-list-keys context "expired@example.org"))
	    (keys2 (epg-list-keys context "no-exp@example.org"))
	    (keys3 (epg-list-keys context "sub@example.org"))
	    (keys4 (epg-list-keys context "revoked-uid@example.org"))
	    (keys5 (epg-list-keys context "disabled@example.org"))
	    (keys6 (epg-list-keys context "sign@example.org"))
	    (keys7 (epg-list-keys context "jens.lechtenboerger@fsfe"))
	    )
       (should (and (= 1 (length keys1)) (= 1 (length keys2))
		    (= 2 (length keys3))
		    (= 1 (length keys4)) (= 1 (length keys5))
		    ))
       ;; key1 is expired
       (should-not (mml-secure-check-user-id (car keys1) "expired@example.org"))
       (should-not (mml-secure-check-sub-key context (car keys1) 'encrypt))
       (should-not (mml-secure-check-sub-key context (car keys1) 'sign))

       ;; key2 does not expire, but does not have the UID expired@example.org
       (should-not (mml-secure-check-user-id (car keys2) "expired@example.org"))
       (should (mml-secure-check-user-id (car keys2) "no-exp@example.org"))
       (should (mml-secure-check-sub-key context (car keys2) 'encrypt))
       (should (mml-secure-check-sub-key context (car keys2) 'sign))

       ;; Two keys exist for sub@example.org.
       (should (mml-secure-check-user-id (car keys3) "sub@example.org"))
       (should (mml-secure-check-sub-key context (car keys3) 'encrypt))
       (should (mml-secure-check-sub-key context (car keys3) 'sign))
       (should (mml-secure-check-user-id (cadr keys3) "sub@example.org"))
       (should (mml-secure-check-sub-key context (cadr keys3) 'encrypt))
       (should (mml-secure-check-sub-key context (cadr keys3) 'sign))

       ;; The UID revoked-uid@example.org is revoked.  The key itself is
       ;; usable, though (with the UID sub@example.org).
       (should-not
	(mml-secure-check-user-id (car keys4) "revoked-uid@example.org"))
       (should (mml-secure-check-sub-key context (car keys4) 'encrypt))
       (should (mml-secure-check-sub-key context (car keys4) 'sign))
       (should (mml-secure-check-user-id (car keys4) "sub@example.org"))

       ;; The next key is disabled and, thus, unusable.
       (should (mml-secure-check-user-id (car keys5) "disabled@example.org"))
       (should-not (mml-secure-check-sub-key context (car keys5) 'encrypt))
       (should-not (mml-secure-check-sub-key context (car keys5) 'sign))

       ;; The next key has multiple subkeys.
       ;; 167C1C27A9D25305 is valid sign subkey, 2DD796DBDAC43424 is expired
       (should (mml-secure-check-sub-key context (car keys6) 'sign "167C1C27A9D25305"))
       (should-not
	(mml-secure-check-sub-key context (car keys6) 'sign "2DD796DBDAC43424"))
       ;; 8D850AA2B34936F9 is encrypt subkey
       (should
	(mml-secure-check-sub-key context (car keys6) 'encrypt "8D850AA2B34936F9"))
       (should-not
	(mml-secure-check-sub-key context (car keys6) 'sign "8D850AA2B34936F9"))
       (should-not
	(mml-secure-check-sub-key context (car keys6) 'encrypt "167C1C27A9D25305"))

       ;; The final key is just a public key.
       (should (mml-secure-check-sub-key context (car keys7) 'encrypt))
       (should-not (mml-secure-check-sub-key context (car keys7) 'sign))
       ))))

(ert-deftest mml-secure-find-usable-keys-1 ()
  "Make sure that expired and disabled keys and revoked UIDs are not used."
  (skip-unless (test-conf))
  (mml-secure-test-fixture
   (lambda ()
     (let ((context (epg-make-context 'OpenPGP)))
       (should-not
	(mml-secure-find-usable-keys context "expired@example.org" 'encrypt))
       (should-not
	(mml-secure-find-usable-keys context "expired@example.org" 'sign))

       (should-not
	(mml-secure-find-usable-keys context "disabled@example.org" 'encrypt))
       (should-not
	(mml-secure-find-usable-keys context "disabled@example.org" 'sign))

       (should-not
	(mml-secure-find-usable-keys
	 context "<revoked-uid@example.org>" 'encrypt))
       (should-not
	(mml-secure-find-usable-keys
	 context "<revoked-uid@example.org>" 'sign))
       ;; Same test without ankles.  Will fail for Ma Gnus v0.14 and earlier.
       (should-not
	(mml-secure-find-usable-keys
	 context "revoked-uid@example.org" 'encrypt))

       ;; Expired key should not be usable.
       ;; Will fail for Ma Gnus v0.14 and earlier.
       ;; sign@example.org has the expired subkey 0x2DD796DBDAC43424.
       (should-not
	(mml-secure-find-usable-keys context "0x2DD796DBDAC43424" 'sign))

       (should
	(mml-secure-find-usable-keys context "no-exp@example.org" 'encrypt))
       (should
	(mml-secure-find-usable-keys context "no-exp@example.org" 'sign))
       ))))

(ert-deftest mml-secure-find-usable-keys-2 ()
  "Test different ways to search for keys."
  (skip-unless (test-conf))
  (mml-secure-test-fixture
   (lambda ()
     (let ((context (epg-make-context 'OpenPGP)))
       ;; Plain substring search is not supported.
       (should
	(= 0 (length
	      (mml-secure-find-usable-keys context "No Expiry" 'encrypt))))
       (should
	(= 0 (length
	      (mml-secure-find-usable-keys context "No Expiry" 'sign))))

       ;; Search for e-mail addresses works with and without ankle brackets.
       (should
	(= 1 (length (mml-secure-find-usable-keys
		      context "<no-exp@example.org>" 'encrypt))))
       (should
	(= 1 (length (mml-secure-find-usable-keys
		      context "<no-exp@example.org>" 'sign))))
       (should
	(= 1 (length (mml-secure-find-usable-keys
		      context "no-exp@example.org" 'encrypt))))
       (should
	(= 1 (length (mml-secure-find-usable-keys
		      context "no-exp@example.org" 'sign))))

       ;; Use full UID string.
       (should
	(= 1 (length (mml-secure-find-usable-keys
		      context "No Expiry <no-exp@example.org>" 'encrypt))))
       (should
	(= 1 (length (mml-secure-find-usable-keys
		      context "No Expiry <no-exp@example.org>" 'sign))))

       ;; If just the public key is present, only encryption is possible.
       ;; Search works with key IDs, with and without prefix "0x".
       (should
	(= 1 (length (mml-secure-find-usable-keys
		      context "CA9EA5175C9043FB" 'encrypt))))
       (should
	(= 1 (length (mml-secure-find-usable-keys
		      context "0xCA9EA5175C9043FB" 'encrypt))))
       (should
	(= 0 (length (mml-secure-find-usable-keys
		      context "CA9EA5175C9043FB" 'sign))))
       (should
	(= 0 (length (mml-secure-find-usable-keys
		      context "0xCA9EA5175C9043FB" 'sign))))
       ))))

(ert-deftest mml-secure-select-preferred-keys-1 ()
  "If only one key exists for an e-mail address, it is the preferred one."
  (skip-unless (test-conf))
  (mml-secure-test-fixture
   (lambda ()
     (let ((context (epg-make-context 'OpenPGP)))
       (should (equal "0281C7D97E90771C0D9A61BFA049C1E9179C086B"
		      (mml-secure-fingerprint
		       (car (mml-secure-select-preferred-keys
			     context '("no-exp@example.org") 'encrypt)))))))))

(ert-deftest mml-secure-select-preferred-keys-2 ()
  "If multiple keys exists for an e-mail address, customization is necessary."
  (skip-unless (test-conf))
  (mml-secure-test-fixture
   (lambda ()
     (let* ((context (epg-make-context 'OpenPGP))
	    (mml-secure-key-preferences
	     '((OpenPGP (sign) (encrypt)) (CMS (sign) (encrypt))))
	    (pref (car (mml-secure-find-usable-keys
			context "sub@example.org" 'encrypt))))
       (should-error (mml-secure-select-preferred-keys
		      context '("sub@example.org") 'encrypt))
       (mml-secure-cust-record-keys
	context 'encrypt "sub@example.org" (list pref))
       (should (mml-secure-select-preferred-keys
		context '("sub@example.org") 'encrypt))
       (should-error (mml-secure-select-preferred-keys
		      context '("sub@example.org") 'sign))
       (should (mml-secure-select-preferred-keys
                context '("sub@example.org") 'encrypt))
       (should
	(equal (list (mml-secure-fingerprint pref))
	       (mml-secure-cust-fpr-lookup context 'encrypt "sub@example.org")))
       (should (mml-secure-cust-remove-keys context 'encrypt "sub@example.org"))
       (should-error (mml-secure-select-preferred-keys
                      context '("sub@example.org") 'encrypt))))))

(ert-deftest mml-secure-select-preferred-keys-3 ()
  "Expired customized keys are removed if multiple keys are available."
  (skip-unless (test-conf))
  (mml-secure-test-fixture
   (lambda ()
     (let ((context (epg-make-context 'OpenPGP))
	   (mml-secure-key-preferences
	    '((OpenPGP (sign) (encrypt)) (CMS (sign) (encrypt)))))
       ;; sub@example.org has two keys (2FAF8726121EB3C6, 8E7FEE76BB1FB195).
       ;; Normal preference works.
       (mml-secure-cust-record-keys
        context 'encrypt "sub@example.org" (epg-list-keys context "2FAF8726121EB3C6"))
       (should (mml-secure-select-preferred-keys
		context '("sub@example.org") 'encrypt))
       (mml-secure-cust-remove-keys context 'encrypt "sub@example.org")

       ;; Fake preference for expired (unrelated) key 22F24E21C5010683,
       ;; results in error (and automatic removal of outdated preference).
       (mml-secure-cust-record-keys
        context 'encrypt "sub@example.org" (epg-list-keys context "22F24E21C5010683"))
       (should-error (mml-secure-select-preferred-keys
		      context '("sub@example.org") 'encrypt))
       (should-not
	(mml-secure-cust-remove-keys context 'encrypt "sub@example.org"))))))

(ert-deftest mml-secure-select-preferred-keys-4 ()
  "Multiple keys can be recorded per recipient or signature."
  (skip-unless (test-conf))
  (skip-unless (ignore-errors (epg-find-configuration 'CMS)))
  (mml-secure-test-fixture
   (lambda ()
     (let ((pcontext (epg-make-context 'OpenPGP))
	   (scontext (epg-make-context 'CMS))
	   (pkeys '("4D661F67B8BC4F7F1C53C2232FAF8726121EB3C6"
		    "EB67A6310389C9AE8A5695908E7FEE76BB1FB195"))
	   (skeys  '("0x5F88E9FC" "0x479DC6E2"))
	   (mml-secure-key-preferences
	    '((OpenPGP (sign) (encrypt)) (CMS (sign) (encrypt)))))

       ;; OpenPGP preferences via pcontext
       (dolist (key pkeys nil)
	 (mml-secure-cust-record-keys
	  pcontext 'encrypt "sub@example.org" (epg-list-keys pcontext key))
	 (mml-secure-cust-record-keys
	  pcontext 'sign "sub@example.org" (epg-list-keys pcontext key 'secret)))
       (let ((p-e-fprs (mml-secure-cust-fpr-lookup
			pcontext 'encrypt "sub@example.org"))
	     (p-s-fprs (mml-secure-cust-fpr-lookup
			pcontext 'sign "sub@example.org")))
	 (should (= 2 (length p-e-fprs)))
	 (should (= 2 (length p-s-fprs)))
	 (should (member "4D661F67B8BC4F7F1C53C2232FAF8726121EB3C6" p-e-fprs))
	 (should (member "EB67A6310389C9AE8A5695908E7FEE76BB1FB195" p-e-fprs))
	 (should (member "4D661F67B8BC4F7F1C53C2232FAF8726121EB3C6" p-s-fprs))
	 (should (member "EB67A6310389C9AE8A5695908E7FEE76BB1FB195" p-s-fprs)))
       ;; Duplicate record does not change anything.
       (mml-secure-cust-record-keys
	pcontext 'encrypt "sub@example.org"
	(epg-list-keys pcontext "4D661F67B8BC4F7F1C53C2232FAF8726121EB3C6"))
       (mml-secure-cust-record-keys
	pcontext 'sign "sub@example.org"
	(epg-list-keys pcontext "4D661F67B8BC4F7F1C53C2232FAF8726121EB3C6"))
       (let ((p-e-fprs (mml-secure-cust-fpr-lookup
			pcontext 'encrypt "sub@example.org"))
	     (p-s-fprs (mml-secure-cust-fpr-lookup
			pcontext 'sign "sub@example.org")))
	 (should (= 2 (length p-e-fprs)))
	 (should (= 2 (length p-s-fprs))))

       ;; S/MIME preferences via scontext
       (dolist (key skeys nil)
	 (mml-secure-cust-record-keys
	  scontext 'encrypt "sub@example.org"
	  (epg-list-keys scontext key))
	 (mml-secure-cust-record-keys
	  scontext 'sign "sub@example.org"
	  (epg-list-keys scontext key 'secret)))
       (let ((s-e-fprs (mml-secure-cust-fpr-lookup
			scontext 'encrypt "sub@example.org"))
	     (s-s-fprs (mml-secure-cust-fpr-lookup
			scontext 'sign "sub@example.org")))
	 (should (= 2 (length s-e-fprs)))
	 (should (= 2 (length s-s-fprs))))
       ))))

(defun mml-secure-test-en-decrypt
    (method to from
	    &optional checksig checkplain enc-keys expectfail interactive)
  "Encrypt message using METHOD, addressed to TO, from FROM.
If optional CHECKSIG is non-nil, it must be a number, and a signature check is
performed; the number indicates how many signatures are expected.
If optional CHECKPLAIN is non-nil, the expected plaintext should be obtained
via decryption.
If optional ENC-KEYS is non-nil, it is a list of pairs of encryption keys (for
OpenPGP and S/SMIME) expected in `epg-debug-buffer'.
If optional EXPECTFAIL is non-nil, a decryption failure is expected.
Pass optional INTERACTIVE to mml-secure-test-mail-fixture."
  (mml-secure-test-mail-fixture method to from
   (lambda (gnus-info plaintext decrypted)
     (if expectfail
	 (should-not (equal plaintext decrypted))
       (when checkplain
	 (should (equal plaintext decrypted)))
       (let ((protocol (if (memq method
				 '(enc-smime enc-sign-smime sign-smime))
			   'CMS
			 'OpenPGP)))
	 (when checksig
	   (let* ((context (epg-make-context protocol))
		  (signer-names (mml-secure-signer-names protocol from))
		  (signer-keys (mml-secure-signers context signer-names))
		  (signer-fprs (mapcar 'mml-secure-fingerprint signer-keys)))
	     (should (eq checksig (length signer-fprs)))
	     (if (eq checksig 0)
		 ;; First key in keyring
		 (should (string-match-p
			  (concat "Good signature from "
				  (if (eq protocol 'CMS)
				      "0E58229B80EE33959FF718FEEF25402B479DC6E2"
				    "A049C1E9179C086B"))
			  gnus-info)))
	     (dolist (fpr signer-fprs nil)
	       ;; OpenPGP: "Good signature from A049C1E9179C086B No Expiry <no-exp@example.org> (trust undefined) created ..."
	       ;; S/MIME:  "Good signature from D06AA118653CC38E9D0CAF56ED7A2135E1582177 /CN=No Expiry (trust full) ..."
	       (should (string-match-p
			(concat "Good signature from "
				(if (eq protocol 'CMS)
				    fpr
				  (substring fpr -16 nil)))
			gnus-info)))))
	 (when enc-keys
	   (with-current-buffer epg-debug-buffer
	     (goto-char (point-min))
	     ;; The following regexp does not necessarily match at the
	     ;; start of the line as a path may or may not be present.
	     ;; Also note that gpg.* matches gpg2 and gpgsm as well.
	     (let* ((line (concat "gpg.*--encrypt.*$"))
		    (end (re-search-forward line))
		    (match (match-string 0)))
	       (should (and end match))
	       (dolist (pair enc-keys nil)
		 (let ((fpr (if (eq protocol 'OpenPGP)
				(car pair)
			      (cdr pair))))
		   (should (string-match-p (concat "-r " fpr) match))))
	       (goto-char (point-max))
	       ))))))
   interactive))

(defvar mml-smime-cache-passphrase)
(defvar mml2015-cache-passphrase)
(defvar mml1991-cache-passphrase)

(defun mml-secure-test-en-decrypt-with-passphrase
    (method to from checksig jl-passphrase do-cache
	    &optional enc-keys expectfail)
  "Call mml-secure-test-en-decrypt with changed passphrase caching.
Args METHOD, TO, FROM, CHECKSIG are passed to mml-secure-test-en-decrypt.
JL-PASSPHRASE is fixed as return value for `read-passwd',
boolean DO-CACHE determines whether to cache the passphrase.
If optional ENC-KEYS is non-nil, it is a list of encryption keys expected
in `epg-debug-buffer'.
If optional EXPECTFAIL is non-nil, a decryption failure is expected."
  (let ((mml-secure-cache-passphrase do-cache)
	(mml1991-cache-passphrase do-cache)
	(mml2015-cache-passphrase do-cache)
	(mml-smime-cache-passphrase do-cache)
	)
    (cl-letf (((symbol-function 'read-passwd)
               (lambda (_prompt &optional _confirm _default) jl-passphrase)))
      (mml-secure-test-en-decrypt method to from checksig t enc-keys expectfail)
      )))

(ert-deftest mml-secure-en-decrypt-1 ()
  "Encrypt message; then decrypt and test for expected result.
In this test, the single matching key is chosen automatically."
  (skip-unless (test-conf))
  (dolist (method (enc-standards) nil)
    ;; no-exp@example.org with single encryption key
    (mml-secure-test-en-decrypt
     method "no-exp@example.org" "sub@example.org" nil t
     (list (cons "A049C1E9179C086B" "ED7A2135E1582177")))))

(ert-deftest mml-secure-en-decrypt-2 ()
  "Encrypt message; then decrypt and test for expected result.
In this test, the encryption key needs to fixed among multiple ones."
  (skip-unless (test-conf))
  (skip-unless (ignore-errors (epg-find-configuration 'CMS)))
  ;; sub@example.org with multiple candidate keys,
  ;; fixture customizes preferred ones.
  (mml-secure-test-key-fixture
   (lambda ()
     (dolist (method (enc-standards) nil)
       (mml-secure-test-en-decrypt
	method "sub@example.org" "no-exp@example.org" nil t
	(list (cons "2FAF8726121EB3C6" "EF25402B479DC6E2")))))))

(ert-deftest mml-secure-en-decrypt-3 ()
  "Encrypt message; then decrypt and test for expected result.
In this test, encrypt-to-self variables are set to t."
  ;; Random failures with "wrong-type-argument stringp nil".
  ;; Seems unlikely to be specific to hydra.nixos.org...
  :tags (if (getenv "EMACS_HYDRA_CI") '(:unstable))
  (skip-unless (test-conf))
  (skip-unless (ignore-errors (epg-find-configuration 'CMS)))
  ;; sub@example.org with multiple candidate keys,
  ;; fixture customizes preferred ones.
  (mml-secure-test-key-fixture
   (lambda ()
     (let ((mml-secure-openpgp-encrypt-to-self t)
	   (mml-secure-smime-encrypt-to-self t))
       (dolist (method (enc-standards) nil)
	 (mml-secure-test-en-decrypt
	  method "sub@example.org" "no-exp@example.org" nil t
	  (list (cons "2FAF8726121EB3C6" "EF25402B479DC6E2")
		(cons "A049C1E9179C086B" "ED7A2135E1582177"))))))))

(ert-deftest mml-secure-en-decrypt-4 ()
  "Encrypt message; then decrypt and test for expected result.
In this test, encrypt-to-self variables are set to lists."
  (skip-unless (test-conf))
  ;; Send from sub@example.org, which has two keys; encrypt to both.
  (let ((mml-secure-openpgp-encrypt-to-self
	 '("2FAF8726121EB3C6" "8E7FEE76BB1FB195"))
	(mml-secure-smime-encrypt-to-self
	 '("EF25402B479DC6E2" "4035D59B5F88E9FC")))
    (dolist (method (enc-standards) nil)
      (mml-secure-test-en-decrypt
       method "no-exp@example.org" "sub@example.org" nil t
       (list (cons "2FAF8726121EB3C6" "EF25402B479DC6E2")
	     (cons "8E7FEE76BB1FB195" "4035D59B5F88E9FC"))))))

(ert-deftest mml-secure-en-decrypt-sign-1-1-single ()
  "Sign and encrypt message; then decrypt and test for expected result.
In this test, just multiple encryption and signing keys may be available."
  :tags '(:unstable)
  (skip-unless (test-conf))
  (mml-secure-test-key-fixture
   (lambda ()
     (let ((mml-secure-openpgp-sign-with-sender t)
	   (mml-secure-smime-sign-with-sender t))
       (dolist (method (enc-sign-standards) nil)
	 ;; no-exp with just one key
	 (mml-secure-test-en-decrypt
	  method "no-exp@example.org" "no-exp@example.org" 1 t)
	 ;; customized choice for encryption key
	 (mml-secure-test-en-decrypt
	  method "sub@example.org" "no-exp@example.org" 1 t)
	 ;; customized choice for signing key
	 (mml-secure-test-en-decrypt
	  method "no-exp@example.org" "sub@example.org" 1 t)
	 ;; customized choice for both keys
	 (mml-secure-test-en-decrypt
	  method "sub@example.org" "sub@example.org" 1 t)
	 )))))

(ert-deftest mml-secure-en-decrypt-sign-1-2-double ()
  "Sign and encrypt message; then decrypt and test for expected result.
In this test, just multiple encryption and signing keys may be available."
  :tags '(:unstable)
  (skip-unless (test-conf))
  (mml-secure-test-key-fixture
   (lambda ()
     (let ((mml-secure-openpgp-sign-with-sender t)
	   (mml-secure-smime-sign-with-sender t))
       ;; Now use both keys to sign.  The customized one via sign-with-sender,
       ;; the other one via the following setting.
       (let ((mml-secure-openpgp-signers '("8E7FEE76BB1FB195"))
	     (mml-secure-smime-signers '("0x5F88E9FC")))
	 (dolist (method (enc-sign-standards) nil)
	   (mml-secure-test-en-decrypt
	    method "no-exp@example.org" "sub@example.org" 2 t)))))))

(ert-deftest mml-secure-en-decrypt-sign-1-3-double ()
  "Sign and encrypt message; then decrypt and test for expected result.
In this test, just multiple encryption and signing keys may be available."
  :tags '(:unstable)
  (skip-unless (test-conf))
  (mml-secure-test-key-fixture
   (lambda ()
     ;; Now use both keys for sub@example.org to sign an e-mail from
     ;; a different address (without associated keys).
     (let ((mml-secure-openpgp-sign-with-sender nil)
	   (mml-secure-smime-sign-with-sender nil)
	   (mml-secure-openpgp-signers
	    '("8E7FEE76BB1FB195" "2FAF8726121EB3C6"))
	   (mml-secure-smime-signers '("0x5F88E9FC" "0x479DC6E2")))
       (dolist (method (enc-sign-standards) nil)
	 (mml-secure-test-en-decrypt
	  method "no-exp@example.org" "no-keys@example.org" 2 t))))))

(ert-deftest mml-secure-en-decrypt-sign-2 ()
  "Sign and encrypt message; then decrypt and test for expected result.
In this test, lists of encryption and signing keys are customized."
  :tags '(:unstable)
  (skip-unless (test-conf))
  (mml-secure-test-key-fixture
   (lambda ()
     (let ((mml-secure-key-preferences
	    '((OpenPGP (sign) (encrypt)) (CMS (sign) (encrypt))))
	   (pcontext (epg-make-context 'OpenPGP))
	   (scontext (epg-make-context 'CMS))
	   (mml-secure-openpgp-sign-with-sender t)
	   (mml-secure-smime-sign-with-sender t))
       (dolist (key '("8E7FEE76BB1FB195" "2FAF8726121EB3C6") nil)
	 (mml-secure-cust-record-keys
	  pcontext 'encrypt "sub@example.org" (epg-list-keys pcontext key))
	 (mml-secure-cust-record-keys
	  pcontext 'sign "sub@example.org" (epg-list-keys pcontext key t)))
       (dolist (key '("0x5F88E9FC" "0x479DC6E2") nil)
	 (mml-secure-cust-record-keys
	  scontext 'encrypt "sub@example.org" (epg-list-keys scontext key))
	 (mml-secure-cust-record-keys
	  scontext 'sign "sub@example.org" (epg-list-keys scontext key t)))
       (dolist (method (enc-sign-standards) nil)
	 ;; customized choice for encryption key
	 (mml-secure-test-en-decrypt
	  method "sub@example.org" "no-exp@example.org" 1 t)
	 ;; customized choice for signing key
	 (mml-secure-test-en-decrypt
	  method "no-exp@example.org" "sub@example.org" 2 t)
	 ;; customized choice for both keys
	 (mml-secure-test-en-decrypt
	  method "sub@example.org" "sub@example.org" 2 t)
	 )))))

(ert-deftest mml-secure-en-decrypt-sign-3 ()
  "Sign and encrypt message; then decrypt and test for expected result.
Use sign-with-sender and encrypt-to-self."
  :tags '(:unstable)
  (skip-unless (test-conf))
  (mml-secure-test-key-fixture
   (lambda ()
     (let ((mml-secure-openpgp-sign-with-sender t)
	   (mml-secure-openpgp-encrypt-to-self t)
	   (mml-secure-smime-sign-with-sender t)
	   (mml-secure-smime-encrypt-to-self t))
       (dolist (method (enc-sign-standards) nil)
	 (mml-secure-test-en-decrypt
	  method "sub@example.org" "no-exp@example.org" 1 t
	  (list (cons "2FAF8726121EB3C6" "EF25402B479DC6E2")
		(cons "A049C1E9179C086B" "ED7A2135E1582177"))))
       ))))

(ert-deftest mml-secure-sign-verify-1 ()
  "Sign message with sender; then verify and test for expected result."
  (skip-unless (test-conf))
  (skip-unless (ignore-errors (epg-find-configuration 'CMS)))
  (mml-secure-test-key-fixture
   (lambda ()
     (dolist (method (sign-standards) nil)
       (let ((mml-secure-openpgp-sign-with-sender t)
	     (mml-secure-smime-sign-with-sender t))
	 ;; A single signing key for sender sub@example.org is customized
	 ;; in the fixture.
	 (mml-secure-test-en-decrypt
	  method "uid1@example.org" "sub@example.org" 1 nil)

	 ;; From sub@example.org, sign with two keys;
	 ;; sign-with-sender and one from signers-variable:
	 (let ((mml-secure-openpgp-signers '("A049C1E9179C086B"))
	       (mml-secure-smime-signers
		'("D06AA118653CC38E9D0CAF56ED7A2135E1582177")))
	   (mml-secure-test-en-decrypt
	    method "no-exp@example.org" "sub@example.org" 2 nil))
	 )))))

(ert-deftest mml-secure-sign-verify-3 ()
  "Try to sign message with expired OpenPGP subkey, which raises an error.
With Ma Gnus v0.14 and earlier a signature would be created with a wrong key."
  (skip-unless (test-conf))
  (should-error
   (mml-secure-test-key-fixture
    (lambda ()
      (let ((with-smime nil)
	    (mml-secure-openpgp-sign-with-sender nil)
	    (mml-secure-openpgp-signers '("2DD796DBDAC43424")))
	(dolist (method (sign-standards) nil)
	  (mml-secure-test-en-decrypt
	   method "no-exp@example.org" "sign@example.org" 1 nil)
	  ))))))

;; TODO Passphrase passing and caching in Emacs does not seem to work
;; with gpgsm at all.
;; Independently of caching settings, a pinentry dialog is displayed.
;; Thus, the following tests require the user to enter the correct gpgsm
;; passphrases at the correct points in time.  (Either empty string or
;; "Passphrase".)
(ert-deftest mml-secure-en-decrypt-passphrase-cache ()
  "Encrypt message; then decrypt and test for expected result.
In this test, a key is used that requires the passphrase \"Passphrase\".
In the first decryption this passphrase is hardcoded, in the second one it
 is taken from a cache."
  (skip-unless (test-conf))
  (ert-skip "Requires passphrase")
  (mml-secure-test-key-fixture
   (lambda ()
     (dolist (method (enc-standards) nil)
       (mml-secure-test-en-decrypt-with-passphrase
	method "uid1@example.org" "sub@example.org" nil
	;; Beware!  For passphrases copy-sequence is necessary, as they may
	;; be erased, which actually changes the function's code and causes
	;; multiple invocations to fail.  I was surprised...
	(copy-sequence "Passphrase") t)
       (mml-secure-test-en-decrypt-with-passphrase
	method "uid1@example.org" "sub@example.org" nil
	(copy-sequence "Incorrect") t)))))

(defun mml-secure-en-decrypt-passphrase-no-cache (method)
  "Encrypt message with METHOD; then decrypt and test for expected result.
In this test, a key is used that requires the passphrase \"Passphrase\".
In the first decryption this passphrase is hardcoded, but caching disabled.
So the second decryption fails."
  (mml-secure-test-key-fixture
   (lambda ()
     (mml-secure-test-en-decrypt-with-passphrase
      method "uid1@example.org" "sub@example.org" nil
      (copy-sequence "Passphrase") nil)
     (mml-secure-test-en-decrypt-with-passphrase
      method "uid1@example.org" "sub@example.org" nil
      (copy-sequence "Incorrect") nil nil t))))

(ert-deftest mml-secure-en-decrypt-passphrase-no-cache-openpgp-todo ()
  "Passphrase caching with OpenPGP only for GnuPG 1.x."
  (skip-unless (test-conf))
  (skip-unless (string< (cdr (assq 'version (epg-find-configuration 'OpenPGP)))
			"2"))
  (mml-secure-en-decrypt-passphrase-no-cache 'enc-pgp)
  (mml-secure-en-decrypt-passphrase-no-cache 'enc-pgp-mime))

(ert-deftest mml-secure-en-decrypt-passphrase-no-cache-smime-todo ()
  "Passphrase caching does not work with S/MIME (and gpgsm)."
  :expected-result :failed
  (skip-unless (test-conf))
  (if with-smime
      (mml-secure-en-decrypt-passphrase-no-cache 'enc-smime)
    (should nil)))


;; Test truncation of question in y-or-n-p.
(defun mml-secure-select-preferred-keys-todo ()
  "Manual customization with truncated question."
  (mml-secure-test-key-fixture
   (lambda ()
     (mml-secure-test-en-decrypt
      'enc-pgp-mime
      "jens.lechtenboerger@informationelle-selbstbestimmung-im-internet.de"
      "no-exp@example.org" nil t nil nil t))))

(defun mml-secure-select-preferred-keys-ok ()
  "Manual customization with entire question."
  (mml-secure-test-fixture
   (lambda ()
     (mml-secure-select-preferred-keys
      (epg-make-context 'OpenPGP)
      '("jens.lechtenboerger@informationelle-selbstbestimmung-im-internet.de")
      'encrypt))
   t))


;; ERT entry points
(defun mml-secure-run-tests ()
    "Run all tests with defaults."
  (ert-run-tests-batch))

(defun mml-secure-run-tests-with-gpg2 ()
  "Run all tests with gpg2 instead of gpg."
  (let* ((epg-gpg-program "gpg2"); ~/local/gnupg-2.1.9/PLAY/inst/bin/gpg2
	 (gpg-version (cdr (assq 'version (epg-find-configuration 'OpenPGP))))
	 ;; Empty passphrases do not seem to work with gpgsm in 2.1.x:
	 ;; https://lists.gnupg.org/pipermail/gnupg-users/2015-October/054575.html
	 (with-smime (string< gpg-version "2.1")))
    (ert-run-tests-batch)))

(defun mml-secure-run-tests-without-smime ()
    "Skip S/MIME tests (as they require manual passphrase entry)."
  (let ((with-smime nil))
    (ert-run-tests-batch)))

(defun mml-sec-test--kill-gpg-agent ()
  (dolist (pid (list-system-processes))
    (let ((atts (process-attributes pid)))
      (when (and (equal (cdr (assq 'user atts)) (user-login-name))
                 (or (equal (cdr (assq 'comm atts)) "gpg-agent")
		     (equal (cdr (assq 'comm atts)) "scdaemon"))
                 (string-match
                  (concat "homedir.*"
                          (regexp-quote (directory-file-name
                                         (ert-resource-directory))))
                  (cdr (assq 'args atts))))
        (call-process "kill" nil nil nil (format "%d" pid))))))

;;; mml-sec-tests.el ends here
