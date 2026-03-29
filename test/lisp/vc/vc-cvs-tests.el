;;; vc-cvs-tests.el --- tests for vc/vc-cvs.el  -*- lexical-binding:t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

;; Author: Olivier Certner <olce.emacs@certner.fr>
;; Maintainer: emacs-devel@gnu.org

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

(require 'vc-cvs)

(ert-deftest vc-cvs-test-parse-root--local-no-method ()
  (vc-cvs-test--check-parse-root
   "/home/joe/repo"
   '("local" nil nil "/home/joe/repo")))

(ert-deftest vc-cvs-test-parse-root--local-windows-drive-letter ()
  (vc-cvs-test--check-parse-root
   ":local:c:/users/joe/repo"
   '("local" nil nil "c:/users/joe/repo")))

(ert-deftest vc-cvs-test-parse-root--ext-no-method-host-no-port-colon ()
  (vc-cvs-test--check-parse-root
   "host/home/serv/repo"
   '("ext" nil "host" "/home/serv/repo")))

(ert-deftest vc-cvs-test-parse-root--pserver-host-no-port-colon ()
  (vc-cvs-test--check-parse-root
   ":pserver:host/home/serv/repo"
   '("pserver" nil "host" "/home/serv/repo")))

(ert-deftest vc-cvs-test-parse-root--pserver-host-port-colon ()
  (vc-cvs-test--check-parse-root
   ":pserver:host:/home/serv/repo"
   '("pserver" nil "host" "/home/serv/repo")))

(ert-deftest vc-cvs-test-parse-root--ext-no-method-user-host-no-port-colon ()
  (vc-cvs-test--check-parse-root
   "usr@host/home/serv/repo"
   '("ext" "usr" "host" "/home/serv/repo")))

(ert-deftest vc-cvs-test-parse-root--ext-no-method-user-host-port-colon ()
  (vc-cvs-test--check-parse-root
   "usr@host:/home/serv/repo"
   '("ext" "usr" "host" "/home/serv/repo")))

(ert-deftest vc-cvs-test-parse-root--pserver-user-password-host-no-port-colon ()
  (vc-cvs-test--check-parse-root
   ":pserver:usr:passwd@host/home/serv/repo"
   '("pserver" "usr" "host" "/home/serv/repo")))

(ert-deftest vc-cvs-test-parse-root--pserver-user-password-host-port-colon ()
  (vc-cvs-test--check-parse-root
   ":pserver:usr:passwd@host:/home/serv/repo"
   '("pserver" "usr" "host" "/home/serv/repo")))

(ert-deftest vc-cvs-test-parse-root--pserver-user-password-host-port ()
  (vc-cvs-test--check-parse-root
   ":pserver:usr:passwd@host:28/home/serv/repo"
   '("pserver" "usr" "host" "/home/serv/repo")))

;; Next 3 tests are just to err on the side of caution.  It doesn't
;; seem that CVS 1.12 can ever produce such lines.

(ert-deftest
    vc-cvs-test-parse-root--ext-no-method-user-password-host-no-port-colon
    ()
  (vc-cvs-test--check-parse-root
   "usr:passwd@host/home/serv/repo"
   '("ext" "usr" "host" "/home/serv/repo")))

(ert-deftest
    vc-cvs-test-parse-root--ext-no-method-user-password-host-port-colon
    ()
  (vc-cvs-test--check-parse-root
   "usr:passwd@host:/home/serv/repo"
   '("ext" "usr" "host" "/home/serv/repo")))

(ert-deftest
    vc-cvs-test-parse-root--ext-no-method-user-password-host-port
    ()
  (vc-cvs-test--check-parse-root
   "usr:passwd@host:28/home/serv/repo"
   '("ext" "usr" "host" "/home/serv/repo")))


(defun vc-cvs-test--check-parse-root (input expected-output)
  (should (equal (vc-cvs-parse-root input) expected-output)))

;;; vc-cvs-tests.el ends here
