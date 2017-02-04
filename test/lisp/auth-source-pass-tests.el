;;; auth-source-pass-tests.el --- Tests for auth-source-pass.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Damien Cassou

;; Author: Damien Cassou <damien.cassou@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for auth-source-pass.el

;;; Code:

(require 'ert)

(require 'auth-source-pass)

(eval-when-compile (require 'cl-macs))

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

(defmacro auth-source-pass--deftest (name arglist store &rest body)
  "Define a new ert-test NAME with ARGLIST using STORE as password-store.
BODY is a sequence of instructions that will be evaluated.

This macro overrides `auth-source-pass-parse-entry' and `auth-source-pass-entries' to
test code without touching the file system."
  (declare (indent 3))
  `(ert-deftest ,name ,arglist
     (cl-letf (((symbol-function 'auth-source-pass-parse-entry) (lambda (entry) (cdr (cl-find entry ,store :key #'car :test #'string=))) )
               ((symbol-function 'auth-source-pass-entries) (lambda () (mapcar #'car ,store)))
               ((symbol-function 'auth-source-pass--entry-valid-p) (lambda (_entry) t)))
       (let ((auth-source-debug #'auth-source-pass--debug)
             (auth-source-pass--debug-log nil))
         ,@body))))

(auth-source-pass--deftest auth-source-pass-find-match-matching-at-entry-name ()
           '(("foo"))
  (should (equal (auth-source-pass--find-match "foo" nil)
                 "foo")))

(auth-source-pass--deftest auth-source-pass-find-match-matching-at-entry-name-part ()
                   '(("foo"))
  (should (equal (auth-source-pass--find-match "https://foo" nil)
                 "foo")))

(auth-source-pass--deftest auth-source-pass-find-match-matching-at-entry-name-ignoring-user ()
                   '(("foo"))
  (should (equal (auth-source-pass--find-match "https://SomeUser@foo" nil)
                 "foo")))

(auth-source-pass--deftest auth-source-pass-find-match-matching-at-entry-name-with-user ()
                   '(("SomeUser@foo"))
                   (should (equal (auth-source-pass--find-match "https://SomeUser@foo" nil)
                                  "SomeUser@foo")))

(auth-source-pass--deftest auth-source-pass-find-match-matching-at-entry-name-prefer-full ()
                   '(("SomeUser@foo") ("foo"))
                   (should (equal (auth-source-pass--find-match "https://SomeUser@foo" nil)
                                  "SomeUser@foo")))

;; same as previous one except the store is in another order
(auth-source-pass--deftest auth-source-pass-find-match-matching-at-entry-name-prefer-full-reversed ()
                   '(("foo") ("SomeUser@foo"))
                   (should (equal (auth-source-pass--find-match "https://SomeUser@foo" nil)
                                  "SomeUser@foo")))

(auth-source-pass--deftest auth-source-pass-find-match-matching-at-entry-name-without-subdomain ()
                   '(("bar.com"))
                   (should (equal (auth-source-pass--find-match "foo.bar.com" nil)
                                  "bar.com")))

(auth-source-pass--deftest auth-source-pass-find-match-matching-at-entry-name-without-subdomain-with-user ()
                   '(("someone@bar.com"))
                   (should (equal (auth-source-pass--find-match "foo.bar.com" "someone")
                                  "someone@bar.com")))

(auth-source-pass--deftest auth-source-pass-find-match-matching-at-entry-name-without-subdomain-with-bad-user ()
                   '(("someoneelse@bar.com"))
                   (should (equal (auth-source-pass--find-match "foo.bar.com" "someone")
                                  nil)))

(auth-source-pass--deftest auth-source-pass-find-match-matching-at-entry-name-without-subdomain-prefer-full ()
                   '(("bar.com") ("foo.bar.com"))
                   (should (equal (auth-source-pass--find-match "foo.bar.com" nil)
                                  "foo.bar.com")))

(auth-source-pass--deftest auth-source-pass-dont-match-at-folder-name ()
           '(("foo.bar.com/foo"))
  (should (equal (auth-source-pass--find-match "foo.bar.com" nil)
                 nil)))

(auth-source-pass--deftest auth-source-pass-search-with-user-first ()
           '(("foo") ("user@foo"))
  (should (equal (auth-source-pass--find-match "foo" "user")
                 "user@foo"))
  (auth-source-pass--should-have-message-containing "Found 1 match"))

(auth-source-pass--deftest auth-source-pass-give-priority-to-desired-user ()
           '(("foo") ("subdir/foo" ("user" . "someone")))
  (should (equal (auth-source-pass--find-match "foo" "someone")
                 "subdir/foo"))
  (auth-source-pass--should-have-message-containing "Found 2 matches")
  (auth-source-pass--should-have-message-containing "matching user field"))

(auth-source-pass--deftest auth-source-pass-give-priority-to-desired-user-reversed ()
           '(("foo" ("user" . "someone")) ("subdir/foo"))
  (should (equal (auth-source-pass--find-match "foo" "someone")
                 "foo"))
  (auth-source-pass--should-have-message-containing "Found 2 matches")
  (auth-source-pass--should-have-message-containing "matching user field"))

(auth-source-pass--deftest auth-source-pass-return-first-when-several-matches ()
           '(("foo") ("subdir/foo"))
  (should (equal (auth-source-pass--find-match "foo" nil)
                 "foo"))
  (auth-source-pass--should-have-message-containing "Found 2 matches")
  (auth-source-pass--should-have-message-containing "the first one"))

(auth-source-pass--deftest auth-source-pass-make-divansantana-happy ()
           '(("host.com"))
  (should (equal (auth-source-pass--find-match "smtp.host.com" "myusername@host.co.za")
                 "host.com")))

(ert-deftest auth-source-pass-hostname ()
  (should (equal (auth-source-pass--hostname "https://foo.bar") "foo.bar"))
  (should (equal (auth-source-pass--hostname "http://foo.bar") "foo.bar"))
  (should (equal (auth-source-pass--hostname "https://SomeUser@foo.bar") "foo.bar")))

(ert-deftest auth-source-pass-hostname-with-user ()
  (should (equal (auth-source-pass--hostname-with-user "https://foo.bar") "foo.bar"))
  (should (equal (auth-source-pass--hostname-with-user "http://foo.bar") "foo.bar"))
  (should (equal (auth-source-pass--hostname-with-user "https://SomeUser@foo.bar") "SomeUser@foo.bar")))

(defmacro auth-source-pass--deftest-build-result (name arglist store &rest body)
  "Define a new ert-test NAME with ARGLIST using STORE as password-store.
BODY is a sequence of instructions that will be evaluated.

This macro overrides `auth-source-pass-parse-entry',
`auth-source-pass-entries', and `auth-source-pass--find-match' to
ease testing."
  (declare (indent 3))
  `(auth-source-pass--deftest ,name ,arglist ,store
     (cl-letf (((symbol-function 'auth-source-pass-find-match)
                (lambda (_host _user)
                  "foo")))
       ,@body)))

(auth-source-pass--deftest-build-result auth-source-pass-build-result-return-parameters ()
                        '(("foo"))
  (let ((result (auth-source-pass--build-result "foo" 512 "user")))
    (should (equal (plist-get result :port) 512))
    (should (equal (plist-get result :user) "user"))))

(auth-source-pass--deftest-build-result auth-source-pass-build-result-return-entry-values ()
                        '(("foo" ("port" . 512) ("user" . "anuser")))
  (let ((result (auth-source-pass--build-result "foo" nil nil)))
    (should (equal (plist-get result :port) 512))
    (should (equal (plist-get result :user) "anuser"))))

(auth-source-pass--deftest-build-result auth-source-pass-build-result-entry-takes-precedence ()
                        '(("foo" ("port" . 512) ("user" . "anuser")))
  (let ((result (auth-source-pass--build-result "foo" 1024 "anotheruser")))
    (should (equal (plist-get result :port) 512))
    (should (equal (plist-get result :user) "anuser"))))

(ert-deftest auth-source-pass-only-return-entries-that-can-be-open ()
  (cl-letf (((symbol-function 'auth-source-pass-entries)
             (lambda () '("foo.site.com" "bar.site.com")))
            ((symbol-function 'auth-source-pass--entry-valid-p)
             ;; only foo.site.com is valid
             (lambda (entry) (string-equal entry "foo.site.com"))))
    (should (equal (auth-source-pass--find-all-by-entry-name "foo.site.com")
                   '("foo.site.com")))
    (should (equal (auth-source-pass--find-all-by-entry-name "bar.site.com")
                   '()))))

(ert-deftest auth-source-pass-entry-is-not-valid-when-unreadable ()
  (cl-letf (((symbol-function 'auth-source-pass--read-entry)
             (lambda (entry)
               ;; only foo is a valid entry
               (if (string-equal entry "foo")
                   "password"
                 nil))))
    (should (auth-source-pass--entry-valid-p "foo"))
    (should-not (auth-source-pass--entry-valid-p "bar"))))

(provide 'auth-source-pass-tests)

;;; auth-source-pass-tests.el ends here
