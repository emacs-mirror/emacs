;;; gnutls-tests.el --- Test suite for gnutls.el

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>

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

;; Run this with `GNUTLS_TEST_VERBOSE=1' to get verbose debugging.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gnutls)
(require 'hex-util)

(defvar gnutls-tests-message-prefix "")

(defsubst gnutls-tests-message (format-string &rest args)
  (when (getenv "GNUTLS_TEST_VERBOSE")
    (apply #'message (concat "gnutls-tests: " gnutls-tests-message-prefix format-string) args)))

;; Minor convenience to see strings more easily (without binary data).
(defsubst gnutls-tests-hexstring-equal (a b)
  (and (stringp a) (stringp b) (string-equal (encode-hex-string a) (encode-hex-string b))))

(defvar gnutls-tests-internal-macs-upcased
  (mapcar (lambda (sym) (cons sym (intern (upcase (symbol-name sym)))))
          (secure-hash-algorithms)))

(defvar gnutls-tests-tested-macs
  (when (gnutls-available-p)
    (cl-remove-duplicates
     (append (mapcar #'cdr gnutls-tests-internal-macs-upcased)
             (mapcar #'car (gnutls-macs))))))

(defvar gnutls-tests-tested-digests
  (when (gnutls-available-p)
    (cl-remove-duplicates
     (append (mapcar #'cdr gnutls-tests-internal-macs-upcased)
             (mapcar #'car (gnutls-digests))))))

(defvar gnutls-tests-tested-ciphers
  (when (gnutls-available-p)
    (cl-remove-duplicates
     ;; these cause FPEs or SEGVs
     (cl-remove-if (lambda (e) (memq e '(ARCFOUR-128)))
                   (mapcar #'car (gnutls-ciphers))))))

(defvar gnutls-tests-mondo-strings
  (list
   ""
   "some data"
   "lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data lots and lots of data "
   "data and more data to go over the block limit!"
   "data and more data to go over the block limit"
   (format "some random data %d%d" (random) (random))))

(ert-deftest test-gnutls-000-availability ()
  "Test the GnuTLS hashes and ciphers availability."
  (skip-unless (memq 'gnutls3 (gnutls-available-p)))
  (setq gnutls-tests-message-prefix "availability: ")
  (should (> (length gnutls-tests-internal-macs-upcased) 5))
  (let ((macs (gnutls-macs))
        (digests (gnutls-digests))
        (ciphers (gnutls-ciphers)))
    (dolist (mac gnutls-tests-tested-macs)
      (let ((plist (cdr (assq mac macs))))
        (gnutls-tests-message "MAC %s %S" mac plist)
        (dolist (prop '(:mac-algorithm-id :mac-algorithm-length :mac-algorithm-keysize :mac-algorithm-noncesize))
          (should (plist-get plist prop)))
        (should (eq 'gnutls-mac-algorithm (plist-get plist :type)))))
    (dolist (digest gnutls-tests-tested-digests)
      (let ((plist (cdr (assq digest digests))))
        (gnutls-tests-message "digest %s %S" digest plist)
        (dolist (prop '(:digest-algorithm-id :digest-algorithm-length))
          (should (plist-get plist prop)))
        (should (eq 'gnutls-digest-algorithm (plist-get plist :type)))))
    (dolist (cipher gnutls-tests-tested-ciphers)
      (let ((plist (cdr (assq cipher ciphers))))
        (gnutls-tests-message "cipher %s %S" cipher plist)
        (dolist (prop '(:cipher-id :cipher-blocksize :cipher-keysize :cipher-ivsize))
          (should (plist-get plist prop)))
        (should (eq 'gnutls-symmetric-cipher (plist-get plist :type)))))))

(ert-deftest test-gnutls-000-data-extractions ()
  "Test the GnuTLS data extractions against the built-in `secure-hash'."
  (skip-unless (memq 'digests (gnutls-available-p)))
  (setq gnutls-tests-message-prefix "data extraction: ")
  (dolist (input gnutls-tests-mondo-strings)
    ;; Test buffer extraction
    (with-temp-buffer
      (insert input)
      (insert "not ASCII: не e английски")
      (dolist (step '(0 1 2 3 4 5))
        (let ((spec (list (current-buffer) ; a buffer spec
                          (point-min)
                          (max (point-min) (- step (point-max)))))
              (spec2 (list (buffer-string) ; a string spec
                           (point-min)
                           (max (point-min) (- step (point-max))))))
          (should (gnutls-tests-hexstring-equal
                   (gnutls-hash-digest 'MD5 spec)
                   (apply 'secure-hash 'md5 (append spec '(t)))))
          (should (gnutls-tests-hexstring-equal
                   (gnutls-hash-digest 'MD5 spec2)
                   (apply 'secure-hash 'md5 (append spec2 '(t))))))))))

(ert-deftest test-gnutls-001-hashes-internal-digests ()
  "Test the GnuTLS hash digests against the built-in `secure-hash'."
  (skip-unless (memq 'digests (gnutls-available-p)))
  (setq gnutls-tests-message-prefix "digest internal verification: ")
  (let ((macs (gnutls-macs)))
    (dolist (mcell gnutls-tests-internal-macs-upcased)
      (let ((plist (cdr (assq (cdr mcell) macs))))
        (gnutls-tests-message "Checking digest MAC %S %S" mcell plist)
        (dolist (input gnutls-tests-mondo-strings)
          ;; Test buffer extraction
          (with-temp-buffer
            (insert input)
            (should (gnutls-tests-hexstring-equal
                     (gnutls-hash-digest (cdr mcell) (current-buffer))
                     (secure-hash (car mcell) (current-buffer) nil nil t))))
          (should (gnutls-tests-hexstring-equal
                   (gnutls-hash-digest (cdr mcell) input)
                   (secure-hash (car mcell) input nil nil t))))))))

(ert-deftest test-gnutls-002-hashes-digests ()
  "Test some GnuTLS hash digests against pre-defined outputs."
  (skip-unless (memq 'digests (gnutls-available-p)))
  (setq gnutls-tests-message-prefix "digest external verification: ")
  (let ((macs (gnutls-macs)))
    (dolist (test '(("57edf4a22be3c955ac49da2e2107b67a" "12345678901234567890123456789012345678901234567890123456789012345678901234567890" MD5)
                    ("d174ab98d277d9f5a5611c2c9f419d9f" "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" MD5)
                    ("c3fcd3d76192e4007dfb496cca67e13b" "abcdefghijklmnopqrstuvwxyz" MD5)
                    ("f96b697d7cb7938d525a2f31aaf161d0" "message digest" MD5)
                    ("900150983cd24fb0d6963f7d28e17f72" "abc" MD5)
                    ("0cc175b9c0f1b6a831c399e269772661" "a" MD5)
                    ("a9993e364706816aba3e25717850c26c9cd0d89d" "abc" SHA1)
                    ("a9993e364706816aba3e25717850c26c9cd0d89d" "abc" "SHA1"))) ; check string ID for digest
      (pcase-let ((`(,hash ,input ,mac) test))
        (let ((plist (cdr (assq mac macs)))
              result resultb)
        (gnutls-tests-message "%s %S" mac plist)
        (setq result (encode-hex-string (gnutls-hash-digest mac input)))
        (gnutls-tests-message "%S => result %S" test result)
        (should (string-equal result hash))
        ;; Test buffer extraction
        (with-temp-buffer
          (insert input)
          (setq resultb (encode-hex-string (gnutls-hash-digest mac (current-buffer))))
          (gnutls-tests-message "%S => result from buffer %S" test resultb)
          (should (string-equal resultb hash))))))))

(ert-deftest test-gnutls-003-hashes-hmacs ()
  "Test some predefined GnuTLS HMAC outputs for SHA256."
  (skip-unless (memq 'macs (gnutls-available-p)))
  (setq gnutls-tests-message-prefix "HMAC verification: ")
  (let ((macs (gnutls-macs)))
    (dolist (test '(("f5c5021e60d9686fef3bb0414275fe4163bece61d9a95fec7a273746a437b986" "hello\n" "test" SHA256)
                    ("46b75292b81002fd873e89c532a1b8545d6efc9822ee938feba6de2723161a67" "more and more data goes into a file to exceed the buffer size" "test" SHA256)
                    ("81568ba71fa2c5f33cc84bf362466988f98eba3735479100b4e8908acad87ac4" "more and more data goes into a file to exceed the buffer size" "very long key goes here to exceed the key size" SHA256)
                    ("4bc830005783a73b8112f4bd5f4aa5f92e05b51e9b55c0cd6f9a7bee48371def" "more and more data goes into a file to exceed the buffer size" "" "SHA256") ; check string ID for HMAC
                    ("4bc830005783a73b8112f4bd5f4aa5f92e05b51e9b55c0cd6f9a7bee48371def" "more and more data goes into a file to exceed the buffer size" "" SHA256)))
      (pcase-let ((`(,hash ,input ,key ,mac) test))
        (let ((plist (cdr (assq mac macs)))
              result)
          (gnutls-tests-message "%s %S" mac plist)
          (setq result (encode-hex-string (gnutls-hash-mac mac (copy-sequence key) input)))
          (gnutls-tests-message "%S => result %S" test result)
          (should (string-equal result hash)))))))


(defun gnutls-tests-pad-or-trim (s exact)
  "Pad or trim string S to EXACT numeric size."
  (if (and (consp s) (eq 'iv-auto (nth 0 s)))
      s
    (let ((e (number-to-string exact)))
      (format (concat "%" e "." e "s") s))))

(defun gnutls-tests-pad-to-multiple (s blocksize)
  "Pad string S to BLOCKSIZE numeric size."
  (let* ((e (if (string= s "")
               blocksize
              (* blocksize (ceiling (length s) blocksize))))
         (out (concat s (make-string (- e (length s)) ? ))))
    ;; (gnutls-tests-message "padding %S to length %d for blocksize %d: => %S" s e blocksize out)
    out))

;; ;;; Testing from the command line:
;; ;;; echo e36a9d13c15a6df23a59a6337d6132b8f7cd5283cb4784b81141b52343a18e5f5e5ee8f5553c23167409dd222478bc30 | perl -lne 'print pack "H*", $_' | openssl enc -aes-128-ctr -d  -nosalt -K 6d796b657932 -iv 696e697432 | od -x
(ert-deftest test-gnutls-004-symmetric-ciphers ()
  "Test the GnuTLS symmetric ciphers"
  (skip-unless (memq 'ciphers (gnutls-available-p)))
  (setq gnutls-tests-message-prefix "symmetric cipher verification: ")
  ;; we expect at least 10 ciphers
  (should (> (length (gnutls-ciphers)) 10))
  (let ((keys '("mykey" "mykey2"))
        (inputs gnutls-tests-mondo-strings)
        (ivs '("" "-abc123-" "init" "ini2"))
        (ciphers (cl-remove-if
                  (lambda (c) (plist-get (cdr (assq c (gnutls-ciphers)))
                                    :cipher-aead-capable))
                  gnutls-tests-tested-ciphers)))

    (dolist (cipher ciphers)
      (dolist (iv ivs)
        (dolist (input inputs)
          (dolist (key keys)
            (gnutls-tests-message "%S, starting key %S IV %S input %S" (assq cipher (gnutls-ciphers)) key iv input)
            (let* ((cplist (cdr (assq cipher (gnutls-ciphers))))
                   (key (gnutls-tests-pad-or-trim key (plist-get cplist :cipher-keysize)))
                   (input (gnutls-tests-pad-to-multiple input (plist-get cplist :cipher-blocksize)))
                   (iv (gnutls-tests-pad-or-trim iv (plist-get cplist :cipher-ivsize)))
                   (output (gnutls-symmetric-encrypt cplist (copy-sequence key) iv input))
                   (data (nth 0 output))
                   (actual-iv (nth 1 output))
                   (reverse-output (gnutls-symmetric-decrypt cplist (copy-sequence key) actual-iv data))
                   (reverse (nth 0 reverse-output)))
              (gnutls-tests-message "%s %S" cipher cplist)
              (gnutls-tests-message "key %S IV %S input %S => hexdata %S and reverse %S" key iv input (encode-hex-string data) reverse)
              (should-not (gnutls-tests-hexstring-equal input data))
              (should-not (gnutls-tests-hexstring-equal data reverse))
              (should (gnutls-tests-hexstring-equal input reverse)))))))))

(ert-deftest test-gnutls-005-aead-ciphers ()
  "Test the GnuTLS AEAD ciphers"
  (skip-unless (memq 'AEAD-ciphers (gnutls-available-p)))
  (setq gnutls-tests-message-prefix "AEAD verification: ")
  (let ((keys '("mykey" "mykey2"))
        (inputs gnutls-tests-mondo-strings)
        (ivs '("" "-abc123-" "init" "ini2"))
        (auths '(nil
                 ""
                 "auth data"
                 "auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data auth and auth of data "
                 "AUTH data and more data to go over the block limit!"
                 "AUTH data and more data to go over the block limit"))
        (ciphers (cl-remove-if
                  (lambda (c) (or (null (plist-get (cdr (assq c (gnutls-ciphers)))
                                              :cipher-aead-capable))))
                  gnutls-tests-tested-ciphers))
        actual-ivlist)

    (dolist (cipher ciphers)
      (dolist (input inputs)
        (dolist (auth auths)
          (dolist (key keys)
            (let* ((cplist (cdr (assq cipher (gnutls-ciphers))))
                   (key (gnutls-tests-pad-or-trim key (plist-get cplist :cipher-keysize)))
                   (input (gnutls-tests-pad-to-multiple input (plist-get cplist :cipher-blocksize)))
                   (ivsize (plist-get cplist :cipher-ivsize)))
              (should (>= ivsize 12))   ; as per the RFC
              (dolist (iv (append ivs (list (list 'iv-auto ivsize))))

                (gnutls-tests-message "%S, starting key %S IV %S input %S auth %S" (assq cipher (gnutls-ciphers)) key iv input auth)
                (let* ((iv (gnutls-tests-pad-or-trim iv (plist-get cplist :cipher-ivsize)))
                       (output (gnutls-symmetric-encrypt cplist (copy-sequence key) iv input (copy-sequence auth)))
                       (data (nth 0 output))
                       (actual-iv (nth 1 output))
                       (reverse-output (gnutls-symmetric-decrypt cplist (copy-sequence key) actual-iv data auth))
                       (reverse (nth 0 reverse-output)))
                  ;; GNUTLS_RND_NONCE should be good enough to ensure this.
                  (should-not (member (secure-hash 'sha384 actual-iv 0 ivsize) actual-ivlist))
                  (cond
                   ((stringp iv)
                    (should (equal iv actual-iv)))
                   ((consp iv)
                    (push (secure-hash 'sha384 actual-iv 0 ivsize) actual-ivlist)
                    (gnutls-tests-message "IV list length: %d" (length actual-ivlist))))

                  (gnutls-tests-message "%s %S" cipher cplist)
                  (gnutls-tests-message "key %S IV %S input %S auth %S => hexdata %S and reverse %S" key iv input auth (encode-hex-string data) reverse)
                  (should-not (gnutls-tests-hexstring-equal input data))
                  (should-not (gnutls-tests-hexstring-equal data reverse))
                  (should (gnutls-tests-hexstring-equal input reverse)))))))))))

(provide 'gnutls-tests)
;;; gnutls-tests.el ends here
