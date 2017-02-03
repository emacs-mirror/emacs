;;; auth-source-tests.el --- Tests for auth-source.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2017 Free Software Foundation, Inc.

;; Author: Damien Cassou <damien@cassou.me>,
;;         Nicolas Petton <nicolas@petton.fr>

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'auth-source)

(defvar secrets-enabled t
  "Enable the secrets backend to test its features.")

(defun auth-source-ensure-ignored-backend (source)
    (auth-source-validate-backend source '((:source . "")
                                           (:type . ignore))))

(defun auth-source-validate-backend (source validation-alist)
  (let ((backend (auth-source-backend-parse source)))
    (should (auth-source-backend-p backend))
    (dolist (pair validation-alist)
      (should (equal (eieio-oref backend (car pair)) (cdr pair))))))

(ert-deftest auth-source-backend-parse-macos-keychain ()
  (auth-source-validate-backend '(:source (:macos-keychain-generic foobar))
                                '((:source . "foobar")
                                  (:type . macos-keychain-generic)
                                  (:search-function . auth-source-macos-keychain-search)
                                  (:create-function . auth-source-macos-keychain-create))))

(ert-deftest auth-source-backend-parse-macos-keychain-generic-string ()
  (auth-source-validate-backend "macos-keychain-generic:foobar"
                                '((:source . "foobar")
                                  (:type . macos-keychain-generic)
                                  (:search-function . auth-source-macos-keychain-search)
                                  (:create-function . auth-source-macos-keychain-create))))

(ert-deftest auth-source-backend-parse-macos-keychain-internet-string ()
  (auth-source-validate-backend "macos-keychain-internet:foobar"
                                '((:source . "foobar")
                                  (:type . macos-keychain-internet)
                                  (:search-function . auth-source-macos-keychain-search)
                                  (:create-function . auth-source-macos-keychain-create))))

(ert-deftest auth-source-backend-parse-macos-keychain-internet-symbol ()
  (auth-source-validate-backend 'macos-keychain-internet
                                '((:source . "default")
                                  (:type . macos-keychain-internet)
                                  (:search-function . auth-source-macos-keychain-search)
                                  (:create-function . auth-source-macos-keychain-create))))

(ert-deftest auth-source-backend-parse-macos-keychain-generic-symbol ()
  (auth-source-validate-backend 'macos-keychain-generic
                                '((:source . "default")
                                  (:type . macos-keychain-generic)
                                  (:search-function . auth-source-macos-keychain-search)
                                  (:create-function . auth-source-macos-keychain-create))))

(ert-deftest auth-source-backend-parse-macos-keychain-internet-default-string ()
  (auth-source-validate-backend 'macos-keychain-internet
                                '((:source . "default")
                                  (:type . macos-keychain-internet)
                                  (:search-function . auth-source-macos-keychain-search)
                                  (:create-function . auth-source-macos-keychain-create))))

(ert-deftest auth-source-backend-parse-plstore ()
  (auth-source-validate-backend '(:source "foo.plist")
                                '((:source . "foo.plist")
                                  (:type . plstore)
                                  (:search-function . auth-source-plstore-search)
                                  (:create-function . auth-source-plstore-create))))

(ert-deftest auth-source-backend-parse-netrc ()
  (auth-source-validate-backend '(:source "foo")
                                '((:source . "foo")
                                  (:type . netrc)
                                  (:search-function . auth-source-netrc-search)
                                  (:create-function . auth-source-netrc-create))))

(ert-deftest auth-source-backend-parse-netrc-string ()
  (auth-source-validate-backend "foo"
                                '((:source . "foo")
                                  (:type . netrc)
                                  (:search-function . auth-source-netrc-search)
                                  (:create-function . auth-source-netrc-create))))

(ert-deftest auth-source-backend-parse-secrets ()
  (provide 'secrets) ; simulates the presence of the `secrets' package
  (let ((secrets-enabled t))
    (auth-source-validate-backend '(:source (:secrets "foo"))
                                  '((:source . "foo")
                                    (:type . secrets)
                                    (:search-function . auth-source-secrets-search)
                                    (:create-function . auth-source-secrets-create)))))

(ert-deftest auth-source-backend-parse-secrets-strings ()
  (provide 'secrets) ; simulates the presence of the `secrets' package
  (let ((secrets-enabled t))
    (auth-source-validate-backend "secrets:foo"
                                  '((:source . "foo")
                                    (:type . secrets)
                                    (:search-function . auth-source-secrets-search)
                                    (:create-function . auth-source-secrets-create)))))

(ert-deftest auth-source-backend-parse-secrets-alias ()
  (provide 'secrets) ; simulates the presence of the `secrets' package
  (let ((secrets-enabled t))
    ;; Redefine `secrets-get-alias' to map 'foo to "foo"
    (cl-letf (((symbol-function 'secrets-get-alias) (lambda (_) "foo")))
      (auth-source-validate-backend '(:source (:secrets foo))
                                    '((:source . "foo")
                                      (:type . secrets)
                                      (:search-function . auth-source-secrets-search)
                                      (:create-function . auth-source-secrets-create))))))

(ert-deftest auth-source-backend-parse-secrets-symbol ()
  (provide 'secrets) ; simulates the presence of the `secrets' package
  (let ((secrets-enabled t))
    ;; Redefine `secrets-get-alias' to map 'default to "foo"
    (cl-letf (((symbol-function 'secrets-get-alias) (lambda (_) "foo")))
      (auth-source-validate-backend 'default
                                    '((:source . "foo")
                                      (:type . secrets)
                                      (:search-function . auth-source-secrets-search)
                                      (:create-function . auth-source-secrets-create))))))

(ert-deftest auth-source-backend-parse-secrets-no-alias ()
  (provide 'secrets) ; simulates the presence of the `secrets' package
  (let ((secrets-enabled t))
    ;; Redefine `secrets-get-alias' to map 'foo to nil (so that
    ;; "Login" is used by default
    (cl-letf (((symbol-function 'secrets-get-alias) (lambda (_) nil)))
      (auth-source-validate-backend '(:source (:secrets foo))
                                    '((:source . "Login")
                                      (:type . secrets)
                                      (:search-function . auth-source-secrets-search)
                                      (:create-function . auth-source-secrets-create))))))

(ert-deftest auth-source-backend-parse-invalid-or-nil-source ()
  (provide 'secrets) ; simulates the presence of the `secrets' package
  (let ((secrets-enabled t))
    (auth-source-ensure-ignored-backend nil)
    (auth-source-ensure-ignored-backend '(:source '(foo)))
    (auth-source-ensure-ignored-backend '(:source nil))))

(defun auth-source--test-netrc-parse-entry (entry host user port)
  "Parse a netrc entry from buffer."
  (auth-source-forget-all-cached)
  (setq port (auth-source-ensure-strings port))
  (with-temp-buffer
    (insert entry)
    (goto-char (point-min))
    (let* ((check (lambda(alist)
		    (and alist
			 (auth-source-search-collection
			  host
			  (or
			   (auth-source--aget alist "machine")
			   (auth-source--aget alist "host")
			   t))
			 (auth-source-search-collection
			  user
			  (or
			   (auth-source--aget alist "login")
			   (auth-source--aget alist "account")
			   (auth-source--aget alist "user")
			   t))
			 (auth-source-search-collection
			  port
			  (or
			   (auth-source--aget alist "port")
			   (auth-source--aget alist "protocol")
			   t)))))
	   (entries (auth-source-netrc-parse-entries check 1)))
      entries)))

(ert-deftest auth-source-test-netrc-parse-entry ()
  (should (equal (auth-source--test-netrc-parse-entry
                  "machine mymachine1 login user1 password pass1\n" t t t)
                 '((("password" . "pass1")
                    ("login" . "user1")
                    ("machine" . "mymachine1")))))
  (should (equal (auth-source--test-netrc-parse-entry
                  "machine mymachine1 login user1 password pass1 port 100\n"
                  t t t)
                 '((("port" . "100")
                    ("password" . "pass1")
                    ("login" . "user1")
                    ("machine" . "mymachine1"))))))

(ert-deftest auth-source-test-format-prompt ()
  (should (equal (auth-source-format-prompt "test %u %h %p" '((?u "user") (?h "host")))
                 "test user host %p")))

(ert-deftest auth-source-test-remembrances-of-things-past ()
  (let ((password-cache t)
        (password-data (make-vector 7 0)))
    (auth-source-remember '(:host "wedd") '(4 5 6))
    (should (auth-source-remembered-p '(:host "wedd")))
    (should-not (auth-source-remembered-p '(:host "xedd")))
    (auth-source-remember '(:host "xedd") '(1 2 3))
    (should (auth-source-remembered-p '(:host "xedd")))
    (should-not (auth-source-remembered-p '(:host "zedd")))
    (should (auth-source-recall '(:host "xedd")))
    (should-not (auth-source-recall nil))
    (auth-source-forget+ :host t)
    (should-not (auth-source-remembered-p '(:host "xedd")))
    (should-not (auth-source-remembered-p '(:host t)))))

(provide 'auth-source-tests)
;;; auth-source-tests.el ends here
