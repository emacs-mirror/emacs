;;; auth-source-tests.el --- Tests for auth-source.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2020 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'auth-source)
(require 'secrets)

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

(ert-deftest auth-source-test-netrc-parse-one ()
  (should (equal (auth-source--test-netrc-parse-one--all
                  "machine host1\n# comment\n")
                 '("machine" "host1")))
  (should (equal (auth-source--test-netrc-parse-one--all
                  "machine host1\n  \n  \nmachine host2\n")
                 '("machine" "host1" "machine" "host2"))))

(defun auth-source--test-netrc-parse-one--all (text)
  "Parse TEXT with `auth-source-netrc-parse-one' until end,return list."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((one (auth-source-netrc-parse-one)) all)
      (while one
        (push one all)
        (setq one (auth-source-netrc-parse-one)))
      (nreverse all))))

(ert-deftest auth-source-test-format-prompt ()
  (should (equal (auth-source-format-prompt "test %u %h %p" '((?u "user") (?h "host")))
                 "test user host %p")))

(ert-deftest auth-source-test-remembrances-of-things-past ()
  (let ((password-cache t)
        (password-data (copy-hash-table password-data)))
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

(ert-deftest auth-source-test-searches ()
  "Test auth-source searches with various parameters"
  :tags '(auth-source auth-source/netrc)
  (let* ((entries '("machine a1 port a2 user a3 password a4"
                    "machine b1 port b2 user b3 password b4"
                    "machine c1 port c2 user c3 password c4"))
         ;; First element: test description.
         ;; Second element: expected return data, serialized to a string.
         ;; Rest of elements: the parameters for `auth-source-search'.
         (tests '(("any host, max 1"
                   "((:host \"a1\" :port \"a2\" :user \"a3\" :secret \"a4\"))"
                   :max 1 :host t)
                  ("any host, default max is 1"
                   "((:host \"a1\" :port \"a2\" :user \"a3\" :secret \"a4\"))"
                   :host t)
                  ("any host, boolean return"
                   "t"
                   :host t :max 0)
                  ("no parameters, default max is 1"
                   "((:host \"a1\" :port \"a2\" :user \"a3\" :secret \"a4\"))"
                   )
                  ("host c1, default max is 1"
                   "((:host \"c1\" :port \"c2\" :user \"c3\" :secret \"c4\"))"
                   :host "c1")
                  ("host list of (c1), default max is 1"
                   "((:host \"c1\" :port \"c2\" :user \"c3\" :secret \"c4\"))"
                   :host ("c1"))
                  ("any host, max 4"
                   "((:host \"a1\" :port \"a2\" :user \"a3\" :secret \"a4\") (:host \"b1\" :port \"b2\" :user \"b3\" :secret \"b4\") (:host \"c1\" :port \"c2\" :user \"c3\" :secret \"c4\"))"
                   :host t :max 4)
                  ("host b1, default max is 1"
                  "((:host \"b1\" :port \"b2\" :user \"b3\" :secret \"b4\"))"
                   :host "b1")
                  ("host b1, port b2, user b3, default max is 1"
                  "((:host \"b1\" :port \"b2\" :user \"b3\" :secret \"b4\"))"
                   :host "b1" :port "b2" :user "b3")
                  ))

         (netrc-file (make-temp-file "auth-source-test" nil nil
                                     (mapconcat 'identity entries "\n")))
         (auth-sources (list netrc-file))
         (auth-source-do-cache nil)
         found found-as-string)

    (dolist (test tests)
      (cl-destructuring-bind (testname needed &rest parameters) test
        (setq found (apply #'auth-source-search parameters))
        (when (listp found)
          (dolist (f found)
            (setf f (plist-put f :secret
	                       (let ((secret (plist-get f :secret)))
		                 (if (functionp secret)
		                     (funcall secret)
		                   secret))))))

        (setq found-as-string (format "%s: %S" testname found))
        ;; (message "With parameters %S found: [%s] needed: [%s]" parameters found-as-string needed)
        (should (equal found-as-string (concat testname ": " needed)))))
    (delete-file netrc-file)))

(ert-deftest auth-source-test-secrets-create-secret ()
  (skip-unless secrets-enabled)
  ;; The "session" collection is temporary for the lifetime of the
  ;; Emacs process.  Therefore, we don't care to delete it.
  (let ((auth-sources '((:source (:secrets "session"))))
        (auth-source-save-behavior t)
        (host (md5 (concat (prin1-to-string process-environment)
			   (current-time-string))))
        (passwd (md5 (concat (prin1-to-string process-environment)
			     (current-time-string) (current-time-string))))
        auth-info auth-passwd)
    ;; Redefine `read-*' in order to avoid interactive input.
    (cl-letf (((symbol-function 'read-passwd) (lambda (_) passwd))
              ((symbol-function 'read-string)
               (lambda (_prompt _initial _history default) default)))
      (setq auth-info
            (car (auth-source-search
                  :max 1 :host host :require '(:user :secret) :create t))))
    (should (functionp (plist-get auth-info :save-function)))
    (funcall (plist-get auth-info :save-function))

    ;; Check, that the item has been created indeed.
    (auth-source-forget+ :host t)
    (setq auth-info (car (auth-source-search :host host))
	  auth-passwd (plist-get auth-info :secret)
	  auth-passwd (if (functionp auth-passwd)
			  (funcall auth-passwd)
			auth-passwd))
    (should (string-equal (plist-get auth-info :user) (user-login-name)))
    (should (string-equal (plist-get auth-info :host) host))
    (should (string-equal auth-passwd passwd))

    ;; Cleanup.
    ;; Should use `auth-source-delete' when implemented for :secrets backend.
    (secrets-delete-item
     "session"
     (format "%s@%s" (plist-get auth-info :user) (plist-get auth-info :host)))))

(ert-deftest auth-source-delete ()
  (let* ((netrc-file (make-temp-file "auth-source-test" nil nil "\
machine a1 port a2 user a3 password a4
machine b1 port b2 user b3 password b4
machine c1 port c2 user c3 password c4\n"))
         (auth-sources (list netrc-file))
         (auth-source-do-cache nil)
         (expected '((:host "a1" :port "a2" :user "a3" :secret "a4")))
         (parameters '(:max 1 :host t)))
    (unwind-protect
        (let ((found (apply #'auth-source-delete parameters)))
          (dolist (f found)
            (let ((s (plist-get f :secret)))
              (setf f (plist-put f :secret
                                 (if (functionp s) (funcall s) s)))))
          ;; Note: The netrc backend doesn't delete anything, so
          ;; this is actually the same as `auth-source-search'.
          (should (equal found expected)))
      (delete-file netrc-file))))

(provide 'auth-source-tests)
;;; auth-source-tests.el ends here
