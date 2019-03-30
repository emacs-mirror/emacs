;;; auth-source-pass-tests.el --- Tests for auth-source-pass.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2017-2019 Free Software Foundation, Inc.

;; Author: Damien Cassou <damien.cassou@gmail.com>

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

;; Tests for auth-source-pass.el

;;; Code:

(require 'ert)

(require 'auth-source-pass)

(eval-when-compile (require 'cl-lib))

(ert-deftest auth-source-pass-parse-simple ()
  (let ((content "pass\nkey1:val1\nkey2:val2\n"))
    (should (equal (auth-source-pass--parse-data content)
                   '(("key1" . "val1")
                     ("key2" . "val2"))))))

(ert-deftest auth-source-pass-parse-with-dash-line ()
  (let ((content "pass\n--\nkey1:val1\nkey2:val2\n"))
    (should (equal (auth-source-pass--parse-data content)
                   '(("key1" . "val1")
                     ("key2" . "val2"))))))

(ert-deftest auth-source-pass-parse-with-trailing-spaces ()
  (let ((content "pass\n--\nkey1 :val1   \nkey2:   val2\n\n"))
    (should (equal (auth-source-pass--parse-data content)
                   '(("key1" . "val1")
                     ("key2" . "val2"))))))

(defvar auth-source-pass--debug-log nil
  "Contains a list of all messages passed to `auth-source-do-debug`.")

(defun auth-source-pass--should-have-message-containing (regexp)
  "Assert that at least one `auth-source-do-debug` matched REGEXP."
  (should (seq-find (lambda (message)
                      (string-match regexp message))
                    auth-source-pass--debug-log)))

(defun auth-source-pass--debug (&rest msg)
  "Format MSG and add that to `auth-source-pass--debug-log`.
This function is intended to be set to `auth-source-debug`."
  (add-to-list 'auth-source-pass--debug-log (apply #'format msg) t))

(defmacro auth-source-pass--with-store (store &rest body)
  "Use STORE as password-store while executing BODY."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'auth-source-pass-parse-entry) (lambda (entry) (cdr (cl-find entry ,store :key #'car :test #'string=))) )
             ((symbol-function 'auth-source-pass-entries) (lambda () (mapcar #'car ,store)))
             ((symbol-function 'auth-source-pass--entry-valid-p) (lambda (_entry) t)))
     (let ((auth-source-debug #'auth-source-pass--debug)
           (auth-source-pass--debug-log nil))
       ,@body)))

(ert-deftest auth-source-pass-any-host ()
  (auth-source-pass--with-store '(("foo" ("port" . "foo-port") ("host" . "foo-user"))
                                  ("bar"))
    (should-not (auth-source-pass-search :host t))))

(ert-deftest auth-source-pass-undefined-host ()
  (auth-source-pass--with-store '(("foo" ("port" . "foo-port") ("host" . "foo-user"))
                                  ("bar"))
    (should-not (auth-source-pass-search :host nil))))


(ert-deftest auth-source-pass-find-match-matching-at-entry-name ()
  (auth-source-pass--with-store '(("foo"))
    (should (equal (auth-source-pass--find-match "foo" nil nil)
                   "foo"))))

(ert-deftest auth-source-pass-find-match-matching-at-entry-name-part ()
  (auth-source-pass--with-store '(("foo"))
    (should (equal (auth-source-pass--find-match "https://foo" nil nil)
                   "foo"))))

(ert-deftest auth-source-pass-find-match-matching-at-entry-name-ignoring-user ()
  (auth-source-pass--with-store '(("foo"))
    (should (equal (auth-source-pass--find-match "https://SomeUser@foo" nil nil)
                   "foo"))))

(ert-deftest auth-source-pass-find-match-matching-at-entry-name-with-user ()
  (auth-source-pass--with-store '(("SomeUser@foo"))
    (should (equal (auth-source-pass--find-match "https://SomeUser@foo" nil nil)
                   "SomeUser@foo"))))

(ert-deftest auth-source-pass-find-match-matching-at-entry-name-prefer-full ()
  (auth-source-pass--with-store '(("SomeUser@foo") ("foo"))
    (should (equal (auth-source-pass--find-match "https://SomeUser@foo" nil nil)
                   "SomeUser@foo"))))

(ert-deftest auth-source-pass-find-match-matching-at-entry-name-prefer-full-reversed ()
  (auth-source-pass--with-store '(("foo") ("SomeUser@foo"))
    (should (equal (auth-source-pass--find-match "https://SomeUser@foo" nil nil)
                   "SomeUser@foo"))))

(ert-deftest auth-source-pass-find-match-matching-at-entry-name-without-subdomain ()
  (auth-source-pass--with-store '(("bar.com"))
    (should (equal (auth-source-pass--find-match "foo.bar.com" nil nil)
                   "bar.com"))))

(ert-deftest auth-source-pass-find-match-matching-at-entry-name-without-subdomain-with-user ()
  (auth-source-pass--with-store '(("someone@bar.com"))
    (should (equal (auth-source-pass--find-match "foo.bar.com" "someone" nil)
                   "someone@bar.com"))))

(ert-deftest auth-source-pass-find-match-matching-at-entry-name-without-subdomain-with-bad-user ()
  (auth-source-pass--with-store '(("someoneelse@bar.com"))
    (should (equal (auth-source-pass--find-match "foo.bar.com" "someone" nil)
                   nil))))

(ert-deftest auth-source-pass-find-match-matching-at-entry-name-without-subdomain-prefer-full ()
  (auth-source-pass--with-store '(("bar.com") ("foo.bar.com"))
    (should (equal (auth-source-pass--find-match "foo.bar.com" nil nil)
                   "foo.bar.com"))))

(ert-deftest auth-source-pass-dont-match-at-folder-name ()
  (auth-source-pass--with-store '(("foo.bar.com/foo"))
    (should (equal (auth-source-pass--find-match "foo.bar.com" nil nil)
                   nil))))

(ert-deftest auth-source-pass-find-match-matching-extracting-user-from-host ()
  (auth-source-pass--with-store '(("foo.com/bar"))
    (should (equal (auth-source-pass--find-match "https://bar@foo.com" nil nil)
                   "foo.com/bar"))))

(ert-deftest auth-source-pass-search-with-user-first ()
  (auth-source-pass--with-store '(("foo") ("user@foo"))
    (should (equal (auth-source-pass--find-match "foo" "user" nil)
                   "user@foo"))
    (auth-source-pass--should-have-message-containing "Found 1 match")))

(ert-deftest auth-source-pass-give-priority-to-desired-user ()
  (auth-source-pass--with-store '(("foo") ("subdir/foo" ("user" . "someone")))
    (should (equal (auth-source-pass--find-match "foo" "someone" nil)
                   "subdir/foo"))
    (auth-source-pass--should-have-message-containing "Found 2 matches")
    (auth-source-pass--should-have-message-containing "matching user field")))

(ert-deftest auth-source-pass-give-priority-to-desired-user-reversed ()
  (auth-source-pass--with-store '(("foo" ("user" . "someone")) ("subdir/foo"))
    (should (equal (auth-source-pass--find-match "foo" "someone" nil)
                   "foo"))
    (auth-source-pass--should-have-message-containing "Found 2 matches")
    (auth-source-pass--should-have-message-containing "matching user field")))

(ert-deftest auth-source-pass-return-first-when-several-matches ()
  (auth-source-pass--with-store '(("foo") ("subdir/foo"))
    (should (equal (auth-source-pass--find-match "foo" nil nil)
                   "foo"))
    (auth-source-pass--should-have-message-containing "Found 2 matches")
    (auth-source-pass--should-have-message-containing "the first one")))

(ert-deftest auth-source-pass-make-divansantana-happy ()
  (auth-source-pass--with-store '(("host.com"))
    (should (equal (auth-source-pass--find-match "smtp.host.com" "myusername@host.co.za" nil)
                   "host.com"))))

(ert-deftest auth-source-pass-find-host-without-port ()
  (auth-source-pass--with-store '(("host.com"))
    (should (equal (auth-source-pass--find-match "host.com:8888" "someuser" nil)
                   "host.com"))))

(defmacro auth-source-pass--with-store-find-foo (store &rest body)
  "Use STORE while executing BODY.  \"foo\" is the matched entry."
  (declare (indent 1))
  `(auth-source-pass--with-store ,store
     (cl-letf (((symbol-function 'auth-source-pass-find-match)
                (lambda (_host _user)
                  "foo")))
       ,@body)))

(ert-deftest auth-source-pass-build-result-return-parameters ()
  (auth-source-pass--with-store-find-foo '(("foo"))
    (let ((result (auth-source-pass--build-result "foo" 512 "user")))
      (should (equal (plist-get result :port) 512))
      (should (equal (plist-get result :user) "user")))))

(ert-deftest auth-source-pass-build-result-return-entry-values ()
  (auth-source-pass--with-store-find-foo '(("foo" ("port" . 512) ("user" . "anuser")))
    (let ((result (auth-source-pass--build-result "foo" nil nil)))
      (should (equal (plist-get result :port) 512))
      (should (equal (plist-get result :user) "anuser")))))

(ert-deftest auth-source-pass-build-result-entry-takes-precedence ()
  (auth-source-pass--with-store-find-foo '(("foo" ("port" . 512) ("user" . "anuser")))
    (let ((result (auth-source-pass--build-result "foo" 1024 "anotheruser")))
      (should (equal (plist-get result :port) 512))
      (should (equal (plist-get result :user) "anuser")))))

(ert-deftest auth-source-pass-build-result-passes-full-host-to-find-match ()
  (let (passed-host)
    (cl-letf (((symbol-function 'auth-source-pass--find-match)
               (lambda (host _user _port) (setq passed-host host))))
      (auth-source-pass--build-result "https://user@host.com:123" nil nil)
      (should (equal passed-host "https://user@host.com:123"))
      (auth-source-pass--build-result "https://user@host.com" nil nil)
      (should (equal passed-host "https://user@host.com"))
      (auth-source-pass--build-result "user@host.com" nil nil)
      (should (equal passed-host "user@host.com"))
      (auth-source-pass--build-result "user@host.com:443" nil nil)
      (should (equal passed-host "user@host.com:443")))))

(ert-deftest auth-source-pass-only-return-entries-that-can-be-open ()
  (cl-letf (((symbol-function 'auth-source-pass-entries)
             (lambda () '("foo.site.com" "bar.site.com" "mail/baz.site.com/scott")))
            ((symbol-function 'auth-source-pass--entry-valid-p)
             ;; only foo.site.com and "mail/baz.site.com/scott" are valid
             (lambda (entry) (member entry '("foo.site.com" "mail/baz.site.com/scott")))))
    (should (equal (auth-source-pass--find-all-by-entry-name "foo.site.com" "someuser")
                   '("foo.site.com")))
    (should (equal (auth-source-pass--find-all-by-entry-name "bar.site.com" "someuser")
                   '()))
    (should (equal (auth-source-pass--find-all-by-entry-name "baz.site.com" "scott")
                   '("mail/baz.site.com/scott")))))

(ert-deftest auth-source-pass-entry-is-not-valid-when-unreadable ()
  (cl-letf (((symbol-function 'auth-source-pass--read-entry)
             (lambda (entry)
               ;; only foo is a valid entry
               (if (string-equal entry "foo")
                   "password"
                 nil))))
    (should (auth-source-pass--entry-valid-p "foo"))
    (should-not (auth-source-pass--entry-valid-p "bar"))))

(ert-deftest auth-source-pass-can-start-from-auth-source-search ()
  (auth-source-pass--with-store '(("gitlab.com" ("user" . "someone")))
    (auth-source-pass-enable)
    (let ((result (car (auth-source-search :host "gitlab.com"))))
      (should (equal (plist-get result :user) "someone"))
      (should (equal (plist-get result :host) "gitlab.com")))))

(provide 'auth-source-pass-tests)

;;; auth-source-pass-tests.el ends here
