;; uuid-tests.el --- unit tests for uuid.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Andrew Hyatt <ahyatt@gmail.com>

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
;; Unit tests for uuid.el.

;;; Code:

(require 'uuid)

(ert-deftest uuid-v4-input ()
  "Verifies that a UUID-V4 can be parsed and detected correctly."
  ;; Example comes from RFC 9562, A.3.
  (let ((id (uuid-from-string "919108f7-52d1-4320-9bac-f847db4148a8")))
    (should (cl-typep id '(uuid-v 4)))
    (should (equal (uuid-to-string id) "919108f7-52d1-4320-9bac-f847db4148a8"))
    (should (equal (uuid--high id) #x919108f752d1))
    (should (equal (uuid--mid id) #x320))
    (should (equal (uuid--low id) #x1bacf847db4148a8))
    (should (equal (uuid--var id) 2))
    (should (equal (uuid--ver id) 4))))

(ert-deftest uuid-v4-generator ()
  "Verifies that a generated UUID-V4 is valid."
  (let ((id (uuid-v4)))
    (should (cl-typep id '(uuid-v 4)))
    (should (equal (uuid--var id) 2))
    (should (equal (uuid--ver id) 4))))

(ert-deftest uuid-v4-generator-custom-rnd ()
  "Verifies that a UUID-V4 uses the RND function provided to it."
  (let ((id (uuid-v4 :rng (lambda (n) 23))))
    (should (cl-typep id '(uuid-v 4)))
    (should (equal (uuid--var id) 2))
    (should (equal (uuid--ver id) 4))
    (should (equal (uuid--high id) 23))
    (should (equal (uuid--mid id) 23))
    (should (equal (uuid--low id) 23))))

(ert-deftest uuid-v5-input ()
  "Verifies that a UUIDv5 can be parsed and detected correctly."
  ;; Example comes from RFC 9562, A.4.
  (let ((id (uuid-from-string "2ed6657d-e927-568b-95e1-2665a8aea6a2")))
    (should (cl-typep id '(uuid-v 5)))
    (should (equal (uuid-to-string id) "2ed6657d-e927-568b-95e1-2665a8aea6a2"))
    (should (equal (uuid--high id) #x2ed6657de927))
    (should (equal (uuid--mid id) #x68b))
    (should (equal (uuid--low id) #x015e12665a8aea6a2))
    (should (equal (uuid--var id) 2))
    (should (equal (uuid--ver id) 5))))

(ert-deftest uuid-v5-generator ()
  "Verifies that a UUIDv5 can be generated correctly."
  ;; Example comes from RFC 9562, A.4.
  (let ((id (uuid-v5 'dns "www.example.com")))
    (should (cl-typep id '(uuid-v 5)))
    (should (equal (uuid-to-string id) "2ed6657d-e927-568b-95e1-2665a8aea6a2"))
    (should (equal (uuid--high id) #x2ed6657de927))
    (should (equal (uuid--mid id) #x68b))
    (should (equal (uuid--low id) #x015e12665a8aea6a2))))

(ert-deftest uuid-v5-invalid-namespace ()
  "Verifies an invalid namespace errors out."
  (should-error (uuid-v5 'invalid "www.example.com")
                :type 'uuid-invalid-namespace))

(ert-deftest uuid-v7-input ()
  "Verifies that a UUIDv7 can be parsed and detected correctly."
  ;; Example comes from RFC 9562, A.5.
  (let ((id (uuid-from-string "017f22e2-79b0-7cc3-98c4-dc0c0c07398f")))
    (should (cl-typep id '(uuid-v 7)))
    (should (equal (uuid-to-string id) "017f22e2-79b0-7cc3-98c4-dc0c0c07398f"))
    (should (equal (uuid--high id) #x017f22e279b0))
    (should (equal (uuid--mid id) #xcc3))
    (should (equal (uuid--low id) #x18c4dc0c0c07398f))
    (should (equal (uuid--var id) 2))
    (should (equal (uuid--ver id) 7))))

(ert-deftest uuid-v7-generator ()
  "Verifies that a UUIDv7 can be generated correctly."
  (let ((id (uuid-v7)))
    (should (cl-typep id '(uuid-v 7)))
    (should (equal (uuid--var id) 2))
    (should (equal (uuid--ver id) 7))))

(ert-deftest uuid-v7-generator-known-rnd-and-ts ()
  "Verifies that a UUIDv7 uses random, timestamps correctly."
  (cl-letf (((symbol-function 'float-time)
             (lambda () 0.123)))
    (let ((id (uuid-v7 :rng (lambda (n) 789))))
      (should (cl-typep id '(uuid-v 7)))
      (should (equal (uuid--var id) 2))
      (should (equal (uuid--ver id) 7))
      ;; 123 = 0x7b
      ;; 789 = 0x315
      (should (equal (uuid-to-string id) "00000000-007b-7315-8000-000000000315")))))

(ert-deftest uuid-bytes ()
  "Verifies that a UUID can be converted to bytes and back."
  (let* ((id (uuid-v4))
         (bytes (uuid-to-bytes id))
         (id2 (uuid-from-bytes bytes)))
    (should (equal id id2))
    (should (= (length bytes) 16))
    (should-not (multibyte-string-p bytes))))

(ert-deftest uuid-to-number ()
  "Test UUIDs can convert into the correct number."
  (should (equal 193491124287564075115561252409011423400
                 (uuid-to-number (uuid-from-string "919108f7-52d1-4320-9bac-f847db4148a8")))))

(ert-deftest uuid-invalid-string ()
  "Verifies that an invalid UUID string is rejected."
  (should-error (uuid-from-string "invaluuid-string")
                :type 'uuid-invalid-string)
  ;; Too short
  (should-error (uuid-from-string "017f22e2-79b0-7cc3-98c4-dc0c0c07398")
                :type 'uuid-invalid-string)
  ;; Too long
  (should-error (uuid-from-string "017f22e2-79b0-7cc3-98c4-dc0c0c07398ff"))
  ;; Not valid hex
  (should-error (uuid-from-string "017f22e2-79b0-7cc3-98c4-dc0c0c07398zz")))

(ert-deftest uuid-invalid-bytes ()
  "Verifies that an invalid UUID byte string is rejected."
  (should-error (uuid-from-bytes "short-bytes")
                :type 'uuid-invalid-bytes)
  ;; Too long
  (should-error (uuid-from-bytes (make-string 17 ?\0)))
  ;; Multibyte
  (should-error (uuid-from-bytes (encode-coding-string (make-string 17 ?\0) 'utf-8))))

(ert-deftest uuid-unsupported-version ()
  "Verifies that non v4,5, or 7 versions are a usable UUID."
  (dolist (uuid-str
           '("017f22e2-79b0-8cc3-98c4-dc0c0c07398f"  ;; UUIDv8
             "d5103018-6530-328e-b76b-9132edeba856"   ;; UUIDv3
             "358f9528-5e27-11f1-b58b-def57c109056"  ;; UUIDv1
             ))
    (let* ((id (uuid-from-string uuid-str)))
      (should (equal id (uuid-from-bytes (uuid-to-bytes id))))
      (should (uuid-p id))
      (dolist (v '(4 5 7))
        (should-not (cl-typep id `(uuid-v ,v))))
      (should (equal (uuid-to-string id) uuid-str)))))

(ert-deftest uuid-nil ()
  "Tests the nil UUID handling."
  (should (equal (uuid-to-string uuid-nil) "00000000-0000-0000-0000-000000000000"))
  (should (equal 0 (uuid-to-number uuid-nil))))

(ert-deftest uuid-max ()
  "Tests the max UUID handling."
  (should (equal (uuid-to-string uuid-max) "ffffffff-ffff-ffff-ffff-ffffffffffff"))
  (should (= 340282366920938463463374607431768211455 (uuid-to-number uuid-max))))

;;; uuid-tests.el ends here
