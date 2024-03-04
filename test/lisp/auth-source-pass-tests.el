;;; auth-source-pass-tests.el --- Tests for auth-source-pass.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2017-2024 Free Software Foundation, Inc.

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

(require 'ert-x)

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

(ert-deftest auth-source-pass-parse-with-colons-in-data ()
  (let ((content "pass\n--\nkey1 :val1\nkey2: please: keep my space after colon\n\n"))
    (should (equal (auth-source-pass--parse-data content)
                   '(("key1" . "val1")
                     ("key2" . "please: keep my space after colon"))))))

(defvar auth-source-pass--debug-log nil
  "Contains a list of all messages passed to `auth-source-do-debug'.")

(defun auth-source-pass--have-message-matching (regexp)
  "Return non-nil if at least one `auth-source-do-debug' match REGEXP."
  (seq-find (lambda (message)
              (string-match regexp message))
            auth-source-pass--debug-log))

(defun auth-source-pass--explain--have-message-matching (regexp)
  "Explainer function for `auth-source-pass--have-message-matching'.
REGEXP is the same as in `auth-source-pass--have-message-matching'."
  `(regexp
    ,regexp
    messages
    ,(mapconcat #'identity auth-source-pass--debug-log "\n- ")))

(put #'auth-source-pass--have-message-matching 'ert-explainer #'auth-source-pass--explain--have-message-matching)

(defun auth-source-pass--debug (&rest msg)
  "Format MSG and add that to `auth-source-pass--debug-log'.
This function is intended to be set to `auth-source-debug'."
  (add-to-list 'auth-source-pass--debug-log (apply #'format msg) t))

(defvar auth-source-pass--parse-log nil)

(defmacro auth-source-pass--with-store (store &rest body)
  "Use STORE as password-store while executing BODY."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'auth-source-pass-parse-entry)
              (lambda (entry)
                (add-to-list 'auth-source-pass--parse-log entry)
                (cdr (cl-find entry ,store :key #'car :test #'string=))))
             ((symbol-function 'auth-source-pass-entries) (lambda () (mapcar #'car ,store))))
     (let ((auth-source-debug #'auth-source-pass--debug)
           (auth-source-pass--debug-log nil)
           (auth-source-pass--parse-log nil))
       ,@body)))

(defun auth-source-pass--explain-match-entry-p (entry hostname &optional user port)
  "Explainer function for `auth-source-pass-match-entry-p'.

ENTRY, HOSTNAME, USER and PORT are the same as in
`auth-source-pass-match-entry-p'."
  `(entry
    ,entry
    store
    ,(auth-source-pass-entries)
    matching-entries
    ,(auth-source-pass--matching-entries hostname user port)))

(put 'auth-source-pass-match-entry-p 'ert-explainer #'auth-source-pass--explain-match-entry-p)

(defun auth-source-pass--includes-sorted-entries (entries hostname &optional user port)
  "Return non-nil if ENTRIES matching the parameters are found in store.
ENTRIES should be sorted from most specific to least specific.

HOSTNAME, USER and PORT are passed unchanged to
`auth-source-pass--matching-entries'."
  (if (seq-empty-p entries)
      t
    (and
     (auth-source-pass-match-entry-p (car entries) hostname user port)
     (auth-source-pass--includes-sorted-entries (cdr entries) hostname user port))))

(defun auth-source-pass--explain-includes-sorted-entries (entries hostname &optional user port)
  "Explainer function for `auth-source-pass--includes-sorted-entries'.

ENTRIES, HOSTNAME, USER and PORT are the same as in
`auth-source-pass--includes-sorted-entries'."
  `(store
    ,(auth-source-pass-entries)
    matching-entries
    ,(auth-source-pass--matching-entries hostname user port)
    entries
    ,entries))

(put 'auth-source-pass--includes-sorted-entries 'ert-explainer #'auth-source-pass--explain-includes-sorted-entries)

(defun auth-source-pass--explain-match-any-entry-p (hostname &optional user port)
  "Explainer function for `auth-source-pass-match-any-entry-p'.

HOSTNAME, USER and PORT are the same as in `auth-source-pass-match-any-entry-p'."
  `(store
    ,(auth-source-pass-entries)
    matching-entries
    ,(auth-source-pass--matching-entries hostname user port)))

(put 'auth-source-pass-match-any-entry-p 'ert-explainer #'auth-source-pass--explain-match-any-entry-p)

(defun auth-source-pass--matching-entries (hostname &optional user port)
  "Return password-store entries matching HOSTNAME, USER, PORT.

The result is a list of lists of password-store entries.  Each
sublist contains the password-store entries whose names match a
suffix in `auth-source-pass--generate-entry-suffixes'.  The
result is ordered the same way as the suffixes."
  (let ((entries (auth-source-pass-entries)))
    (mapcar (lambda (suffix) (auth-source-pass--entries-matching-suffix suffix entries))
            (auth-source-pass--generate-entry-suffixes hostname user port))))

(defun auth-source-pass-match-entry-p (entry hostname &optional user port)
  "Return non-nil if an ENTRY matching the parameters is found in store.

HOSTNAME, USER and PORT are passed unchanged to
`auth-source-pass--matching-entries'."
  (cl-find-if
   (lambda (entries) (cl-find entry entries :test #'string=))
   (auth-source-pass--matching-entries hostname user port)))

(defun auth-source-pass-match-any-entry-p (hostname &optional user port)
  "Return non-nil if there is at least one entry matching the parameters.

HOSTNAME, USER and PORT are passed unchanged to
`auth-source-pass--matching-entries'."
  (cl-find-if #'identity (auth-source-pass--matching-entries hostname user port)))

(ert-deftest auth-source-pass-any-host ()
  (auth-source-pass--with-store '(("foo" ("port" . "foo-port") ("host" . "foo-user"))
                                  ("bar"))
    (let ((inhibit-message t)) ; silence "... does not handle host wildcards."
      (should-not (auth-source-pass-search :host t)))))

(ert-deftest auth-source-pass-undefined-host ()
  (auth-source-pass--with-store '(("foo" ("port" . "foo-port") ("host" . "foo-user"))
                                  ("bar"))
    (should-not (auth-source-pass-search :host nil))))

(ert-deftest auth-source-pass-not-found ()
  (auth-source-pass--with-store '(("foo" ("port" . "foo-port") ("host" . "foo-user"))
                                  ("bar"))
    (should-not (auth-source-pass-search :host "baz"))))

(ert-deftest auth-source-pass--disambiguate-extract-host-from-hostname ()
  ;; no user or port
  (should (equal (cl-first (auth-source-pass--disambiguate "foo")) "foo"))
  ;; only user
  (should (equal (cl-first (auth-source-pass--disambiguate "user@foo")) "foo"))
  ;; only port
  (should (equal (cl-first (auth-source-pass--disambiguate "https://foo")) "foo"))
  (should (equal (cl-first (auth-source-pass--disambiguate "foo:80")) "foo"))
  ;; both user and port
  (should (equal (cl-first (auth-source-pass--disambiguate "https://user@foo")) "foo"))
  (should (equal (cl-first (auth-source-pass--disambiguate "user@foo:80")) "foo"))
  ;; all of the above with a trailing path
  (should (equal (cl-first (auth-source-pass--disambiguate "foo/path")) "foo"))
  (should (equal (cl-first (auth-source-pass--disambiguate "user@foo/path")) "foo"))
  (should (equal (cl-first (auth-source-pass--disambiguate "https://foo/path")) "foo"))
  (should (equal (cl-first (auth-source-pass--disambiguate "foo:80/path")) "foo"))
  (should (equal (cl-first (auth-source-pass--disambiguate "https://user@foo/path")) "foo"))
  (should (equal (cl-first (auth-source-pass--disambiguate "user@foo:80/path")) "foo")))

(ert-deftest auth-source-pass--disambiguate-extract-user-from-hostname ()
  ;; no user or port
  (should (equal (cl-second (auth-source-pass--disambiguate "foo")) nil))
  ;; only user
  (should (equal (cl-second (auth-source-pass--disambiguate "user@foo")) "user"))
  ;; only port
  (should (equal (cl-second (auth-source-pass--disambiguate "https://foo")) nil))
  (should (equal (cl-second (auth-source-pass--disambiguate "foo:80")) nil))
  ;; both user and port
  (should (equal (cl-second (auth-source-pass--disambiguate "https://user@foo")) "user"))
  (should (equal (cl-second (auth-source-pass--disambiguate "user@foo:80")) "user"))
  ;; all of the above with a trailing path
  (should (equal (cl-second (auth-source-pass--disambiguate "foo/path")) nil))
  (should (equal (cl-second (auth-source-pass--disambiguate "user@foo/path")) "user"))
  (should (equal (cl-second (auth-source-pass--disambiguate "https://foo/path")) nil))
  (should (equal (cl-second (auth-source-pass--disambiguate "foo:80/path")) nil))
  (should (equal (cl-second (auth-source-pass--disambiguate "https://user@foo/path")) "user"))
  (should (equal (cl-second (auth-source-pass--disambiguate "user@foo:80/path")) "user")))

(ert-deftest auth-source-pass--disambiguate-prefer-user-parameter ()
  ;; no user or port
  (should (equal (cl-second (auth-source-pass--disambiguate "foo" "user2")) "user2"))
  ;; only user
  (should (equal (cl-second (auth-source-pass--disambiguate "user@foo" "user2")) "user2"))
  ;; only port
  (should (equal (cl-second (auth-source-pass--disambiguate "https://foo" "user2")) "user2"))
  (should (equal (cl-second (auth-source-pass--disambiguate "foo:80" "user2")) "user2"))
  ;; both user and port
  (should (equal (cl-second (auth-source-pass--disambiguate "https://user@foo" "user2")) "user2"))
  (should (equal (cl-second (auth-source-pass--disambiguate "user@foo:80" "user2")) "user2"))
  ;; all of the above with a trailing path
  (should (equal (cl-second (auth-source-pass--disambiguate "foo/path" "user2")) "user2"))
  (should (equal (cl-second (auth-source-pass--disambiguate "user@foo/path" "user2")) "user2"))
  (should (equal (cl-second (auth-source-pass--disambiguate "https://foo/path" "user2")) "user2"))
  (should (equal (cl-second (auth-source-pass--disambiguate "foo:80/path" "user2")) "user2"))
  (should (equal (cl-second (auth-source-pass--disambiguate "https://user@foo/path" "user2")) "user2"))
  (should (equal (cl-second (auth-source-pass--disambiguate "user@foo:80/path" "user2")) "user2")))

(ert-deftest auth-source-pass--disambiguate-extract-port-from-hostname ()
  ;; no user or port
  (should (equal (cl-third (auth-source-pass--disambiguate "foo")) "443"))
  ;; only user
  (should (equal (cl-third (auth-source-pass--disambiguate "user@foo")) "443"))
  ;; only port
  (should (equal (cl-third (auth-source-pass--disambiguate "https://foo")) "443"))
  (should (equal (cl-third (auth-source-pass--disambiguate "foo:80")) "80"))
  ;; both user and port
  (should (equal (cl-third (auth-source-pass--disambiguate "https://user@foo")) "443"))
  (should (equal (cl-third (auth-source-pass--disambiguate "user@foo:80")) "80"))
  ;; all of the above with a trailing path
  (should (equal (cl-third (auth-source-pass--disambiguate "foo/path")) "443"))
  (should (equal (cl-third (auth-source-pass--disambiguate "user@foo/path")) "443"))
  (should (equal (cl-third (auth-source-pass--disambiguate "https://foo/path")) "443"))
  (should (equal (cl-third (auth-source-pass--disambiguate "foo:80/path")) "80"))
  (should (equal (cl-third (auth-source-pass--disambiguate "https://user@foo/path")) "443"))
  (should (equal (cl-third (auth-source-pass--disambiguate "user@foo:80/path")) "80")))

(ert-deftest auth-source-pass--disambiguate-prefer-port-parameter ()
  ;; no user or port
  (should (equal (cl-third (auth-source-pass--disambiguate "foo" "user2" "8080")) "8080"))
  ;; only user
  (should (equal (cl-third (auth-source-pass--disambiguate "user@foo" "user2" "8080")) "8080"))
  ;; only port
  (should (equal (cl-third (auth-source-pass--disambiguate "https://foo" "user2" "8080")) "8080"))
  (should (equal (cl-third (auth-source-pass--disambiguate "foo:80" "user2" "8080")) "8080"))
  ;; both user and port
  (should (equal (cl-third (auth-source-pass--disambiguate "https://user@foo" "user2" "8080")) "8080"))
  (should (equal (cl-third (auth-source-pass--disambiguate "user@foo:80" "user2" "8080")) "8080"))
  ;; all of the above with a trailing path
  (should (equal (cl-third (auth-source-pass--disambiguate "foo/path" "user2" "8080")) "8080"))
  (should (equal (cl-third (auth-source-pass--disambiguate "user@foo/path" "user2" "8080")) "8080"))
  (should (equal (cl-third (auth-source-pass--disambiguate "https://foo/path" "user2" "8080")) "8080"))
  (should (equal (cl-third (auth-source-pass--disambiguate "foo:80/path" "user2" "8080")) "8080"))
  (should (equal (cl-third (auth-source-pass--disambiguate "https://user@foo/path" "user2" "8080")) "8080"))
  (should (equal (cl-third (auth-source-pass--disambiguate "user@foo:80/path" "user2" "8080")) "8080")))

(ert-deftest auth-source-pass-find-match-minimal-parsing ()
  (let ((store-contents
         '(("baz" ("secret" . "baz password"))
           ("baz:123" ("secret" . "baz:123 password"))
           ("baz/foo" ("secret" . "baz/foo password"))
           ("foo@baz" ("secret" . "foo@baz password"))
           ("baz:123/foo" ("secret" . "baz:123/foo password"))
           ("foo@baz:123" ("secret" . "foo@baz:123 password"))
           ("bar.baz" ("secret" . "bar.baz password"))
           ("bar.baz:123" ("secret" . "bar.baz:123 password"))
           ("bar.baz/foo" ("secret" . "bar.baz/foo password"))
           ("foo@bar.baz" ("secret" . "foo@bar.baz password"))
           ("bar.baz:123/foo" ("secret" . "bar.baz:123/foo password"))
           ("foo@bar.baz:123" ("secret" . "foo@bar.baz:123 password")))))
    (auth-source-pass--with-store store-contents
      (auth-source-pass--find-match "bar.baz" "foo" "123")
      (should (equal auth-source-pass--parse-log '("foo@bar.baz:123"))))
    (auth-source-pass--with-store store-contents
      (auth-source-pass--find-match "bar.baz" "foo" nil)
      (should (equal auth-source-pass--parse-log '("foo@bar.baz"))))
    (auth-source-pass--with-store store-contents
      (auth-source-pass--find-match "bar.baz" nil "123")
      (should (equal auth-source-pass--parse-log '("bar.baz:123"))))
    (auth-source-pass--with-store store-contents
      (auth-source-pass--find-match "bar.baz" nil nil)
      (should (equal auth-source-pass--parse-log '("bar.baz"))))
    (auth-source-pass--with-store store-contents
      (auth-source-pass--find-match "baz" nil nil)
      (should (equal auth-source-pass--parse-log '("baz"))))
    (auth-source-pass--with-store
        '(("dir1/bar.com" ("key" . "val"))
          ("dir2/bar.com" ("key" . "val"))
          ("dir3/bar.com" ("key" . "val")))
      (auth-source-pass--find-match "bar.com" nil nil)
      (should (= (length auth-source-pass--parse-log) 1)))))

(ert-deftest auth-source-pass--find-match-return-parsed-data ()
  (auth-source-pass--with-store '(("bar.com" ("key" . "val")))
    (should (consp (auth-source-pass--find-match "bar.com" nil nil))))
  (auth-source-pass--with-store '(("dir1/bar.com" ("key1" . "val1")) ("dir2/bar.com" ("key2" . "val2")))
    (should (consp (auth-source-pass--find-match "bar.com" nil nil)))))

(ert-deftest auth-source-pass--matching-entries ()
  (auth-source-pass--with-store '(("bar.com"))
    (should (auth-source-pass-match-entry-p "bar.com" "bar.com"))
    ;; match even if sub-domain is asked for
    (should (auth-source-pass-match-entry-p "bar.com" "foo.bar.com"))
    ;; match even if a user is asked for
    (should (auth-source-pass-match-entry-p "bar.com" "bar.com" "user"))
    ;; match even if user as an @ sign
    (should (auth-source-pass-match-entry-p "bar.com" "bar.com" "user@someplace"))
    ;; match even if a port is asked for
    (should (auth-source-pass-match-entry-p "bar.com" "bar.com" nil "8080"))
    ;; match even if a user and a port are asked for
    (should (auth-source-pass-match-entry-p "bar.com" "bar.com" "user" "8080"))
    ;; don't match if a '.' is replaced with another character
    (auth-source-pass--with-store '(("barXcom"))
      (should-not (auth-source-pass-match-any-entry-p "bar.com" nil nil)))))

(ert-deftest auth-source-pass--matching-entries-find-entries-with-a-username ()
  (auth-source-pass--with-store '(("user@foo"))
    (should (auth-source-pass-match-entry-p "user@foo" "foo" "user")))
  ;; match even if sub-domain is asked for
  (auth-source-pass--with-store '(("user@bar.com"))
    (should (auth-source-pass-match-entry-p "user@bar.com" "foo.bar.com" "user")))
  ;; don't match if no user is asked for
  (auth-source-pass--with-store '(("user@foo"))
    (should-not (auth-source-pass-match-any-entry-p "foo")))
  ;; don't match if user is different
  (auth-source-pass--with-store '(("user1@foo"))
    (should-not (auth-source-pass-match-any-entry-p "foo" "user2")))
  ;; don't match if sub-domain is asked for but user is different
  (auth-source-pass--with-store '(("user1@bar.com"))
    (should-not (auth-source-pass-match-any-entry-p "foo.bar.com" "user2"))))

(ert-deftest auth-source-pass--matching-entries-find-entries-with-a-port ()
  (auth-source-pass--with-store '(("bar.com:8080"))
    (should (auth-source-pass-match-entry-p "bar.com:8080" "bar.com" nil "8080"))))

(ert-deftest auth-source-pass--matching-entries-find-entries-with-a-port-when-passed-multiple-ports ()
  (auth-source-pass--with-store '(("bar.com:8080"))
    (should (auth-source-pass-match-entry-p "bar.com:8080" "bar.com" nil '("http" "https" "80" "8080")))))

(ert-deftest auth-source-pass--matching-entries-find-entries-with-slash ()
  ;; match if entry filename matches user
  (auth-source-pass--with-store '(("foo.com/user"))
    (should (auth-source-pass-match-entry-p "foo.com/user" "foo.com" "user")))
  ;; match with port if entry filename matches user
  (auth-source-pass--with-store '(("foo.com:8080/user"))
    (should (auth-source-pass-match-entry-p "foo.com:8080/user" "foo.com" "user" "8080")))
  ;; don't match if entry filename doesn't match user
  (auth-source-pass--with-store '(("foo.com/baz"))
    (should-not (auth-source-pass-match-any-entry-p "foo.com" "user"))))

(ert-deftest auth-source-pass-matching-entries-with-custom-separator ()
  (let ((auth-source-pass-port-separator "#"))
    (auth-source-pass--with-store '(("bar.com#443/someone"))
      (should (auth-source-pass-match-entry-p "bar.com#443/someone" "bar.com" "someone" "443")))))

(ert-deftest auth-source-pass--matching-entries-sort-results ()
  (auth-source-pass--with-store '(("user@foo") ("foo"))
    (should (auth-source-pass--includes-sorted-entries '("user@foo" "foo") "foo" "user")))
  ;; same, but store is reversed
  (auth-source-pass--with-store '(("foo") ("user@foo"))
    (should (auth-source-pass--includes-sorted-entries '("user@foo" "foo") "foo" "user")))
  ;; with sub-domain
  (auth-source-pass--with-store '(("bar.com") ("foo.bar.com"))
    (should (auth-source-pass--includes-sorted-entries '("foo.bar.com" "bar.com") "foo.bar.com")))
  ;; matching user in the entry data takes priority
  (auth-source-pass--with-store '(("dir1/bar.com") ("dir2/bar.com" ("user" . "user")))
    (should (auth-source-pass--includes-sorted-entries
             '("dir2/bar.com" "dir1/bar.com")
             "bar.com" "user")))
  ;; same, but store is reversed
  (auth-source-pass--with-store '(("dir2/bar.com" ("user" . "user")) ("dir1/bar.com"))
    (should (auth-source-pass--includes-sorted-entries
             '("dir2/bar.com" "dir1/bar.com")
             "bar.com" "user"))))

(ert-deftest auth-source-pass-all-supported-organizations ()
  ;; test every possible entry to store this data: user=rms host=gnu.org port=22
  (dolist (entry '(;; only host name
                   "gnu.org"
                   ;; hostname + user
                   "gnu.org/rms" "rms@gnu.org"
                   ;; hostname + port
                   "gnu.org:22"
                   ;; hostname + user + port
                   "gnu.org:22/rms" "rms@gnu.org:22"
                   ;; all of the above in a random folder
                   "a/b/gnu.org"
                   "a/b/gnu.org/rms" "a/b/rms@gnu.org"
                   "a/b/gnu.org:22"
                   "a/b/gnu.org:22/rms" "a/b/rms@gnu.org:22"))
    (auth-source-pass--with-store `((,entry))
      (should (auth-source-pass-match-entry-p entry "gnu.org" "rms" "22")))))

(defmacro auth-source-pass--with-store-find-foo (store &rest body)
  "Use STORE while executing BODY.  \"foo\" is the matched entry."
  (declare (indent 1))
  `(auth-source-pass--with-store ,store
     (cl-letf (((symbol-function 'auth-source-pass-find-match)
                (lambda (_host _user)
                  "foo")))
       ,@body)))

(ert-deftest auth-source-pass-build-result-return-parameters ()
  (auth-source-pass--with-store-find-foo
      '(("foo" ("secret" . "foo password")))
    (let ((result (auth-source-pass--build-result "foo" 512 "user")))
      (should (equal (plist-get result :host) "foo"))
      (should (equal (plist-get result :port) 512))
      (should (equal (plist-get result :user) "user")))))

(ert-deftest auth-source-pass-build-result-return-entry-values ()
  (auth-source-pass--with-store-find-foo '(("foo" ("port" . 512) ("user" . "anuser")))
    (let ((result (auth-source-pass--build-result "foo" nil nil)))
      (should (equal (plist-get result :host) "foo"))
      (should (equal (plist-get result :port) 512))
      (should (equal (plist-get result :user) "anuser")))))

(ert-deftest auth-source-pass-build-result-entry-takes-precedence ()
  (auth-source-pass--with-store-find-foo '(("foo" ("host" . "bar") ("port" . 512) ("user" . "anuser")))
    (let ((result (auth-source-pass--build-result "foo" 1024 "anotheruser")))
      (should (equal (plist-get result :host) "bar"))
      (should (equal (plist-get result :port) 512))
      (should (equal (plist-get result :user) "anuser")))))

(ert-deftest auth-source-pass-build-result-with-multiple-hosts ()
  (auth-source-pass--with-store-find-foo
      '(("foo" ("secret" . "foo password")))
    (let ((result (auth-source-pass--build-result '("bar" "foo") 512 "user")))
      (should (equal (plist-get result :host) "foo"))
      (should (equal (plist-get result :port) 512))
      (should (equal (plist-get result :user) "user")))))

(ert-deftest auth-source-pass-build-result-with-multiple-hosts-no-match ()
  (auth-source-pass--with-store-find-foo
      '(("foo" ("secret" . "foo password")))
    (should-not (auth-source-pass--build-result '("bar" "baz") 512 "user"))))

(ert-deftest auth-source-pass-can-start-from-auth-source-search ()
  (auth-source-pass--with-store '(("gitlab.com" ("user" . "someone")))
    (auth-source-pass-enable)
    ;; This also asserts an aspect of traditional search behavior
    ;; relative to `auth-source-pass-extra-query-keywords'.
    (let* ((auth-source-pass-extra-query-keywords nil)
           (result (car (auth-source-search :host "gitlab.com"))))
      (should (equal (plist-get result :user) "someone"))
      (should (equal (plist-get result :host) "gitlab.com")))))

(ert-deftest auth-source-pass-prints-meaningful-debug-log ()
  (auth-source-pass--with-store '()
    (auth-source-pass--find-match "gitlab.com" nil nil)
    (should (auth-source-pass--have-message-matching
             "entries matching hostname=\"gitlab.com\""))
    (should (auth-source-pass--have-message-matching
             "corresponding suffixes to search for: .*\"gitlab.com\""))
    (should (auth-source-pass--have-message-matching
             "found no entries matching \"gitlab.com\"")))
  (auth-source-pass--with-store '(("gitlab.com"))
    (auth-source-pass--find-match "gitlab.com" nil nil)
    (should (auth-source-pass--have-message-matching
             "found 1 entry matching \"gitlab.com\": \"gitlab.com\"")))
  (auth-source-pass--with-store '(("a/gitlab.com") ("b/gitlab.com"))
    (auth-source-pass--find-match "gitlab.com" nil nil)
    (should (auth-source-pass--have-message-matching
             "found 2 entries matching \"gitlab.com\": (\"a/gitlab.com\" \"b/gitlab.com\")"))))


;;;; Option `auth-source-pass-extra-query-keywords' (bug#58985)

;; No entry has the requested port, but a result is still returned.

(ert-deftest auth-source-pass-extra-query-keywords--wild-port-miss-netrc ()
  (ert-with-temp-file netrc-file
    :text "\
machine x.com password a
machine x.com port 42 password b
"
    (let* ((auth-sources (list netrc-file))
           (auth-source-do-cache nil)
           (results (auth-source-search :host "x.com" :port 22 :max 2)))
      (dolist (result results)
        (setf (plist-get result :secret) (auth-info-password result)))
      (should (equal results '((:host "x.com" :secret "a")))))))

(ert-deftest auth-source-pass-extra-query-keywords--wild-port-miss ()
  (auth-source-pass--with-store '(("x.com" (secret . "a"))
                                  ("x.com:42" (secret . "b")))
    (auth-source-pass-enable)
    (let* ((auth-source-pass-extra-query-keywords t)
           (results (auth-source-search :host "x.com" :port 22 :max 2)))
      (dolist (result results)
        (setf (plist-get result :secret) (auth-info-password result)))
      (should (equal results '((:host "x.com" :secret "a")))))))

;; One of two entries has the requested port, both returned.

(ert-deftest auth-source-pass-extra-query-keywords--wild-port-hit-netrc ()
  (ert-with-temp-file netrc-file
    :text "\
machine x.com password a
machine x.com port 42 password b
"
    (let* ((auth-sources (list netrc-file))
           (auth-source-do-cache nil)
           (results (auth-source-search :host "x.com" :port 42 :max 2)))
      (dolist (result results)
        (setf (plist-get result :secret) (auth-info-password result)))
      (should (equal results '((:host "x.com" :secret "a")
                               (:host "x.com" :port "42" :secret "b")))))))

(ert-deftest auth-source-pass-extra-query-keywords--wild-port-hit ()
  (auth-source-pass--with-store '(("x.com" (secret . "a"))
                                  ("x.com:42" (secret . "b")))
    (auth-source-pass-enable)
    (let* ((auth-source-pass-extra-query-keywords t)
           (results (auth-source-search :host "x.com" :port 42 :max 2)))
      (dolist (result results)
        (setf (plist-get result :secret) (auth-info-password result)))
      (should (equal results
                     '((:host "x.com" :secret "a")
                       (:host "x.com" :port 42 :secret "b")))))))

;; No entry has the requested port, but :port is required, so search fails.

(ert-deftest auth-source-pass-extra-query-keywords--wild-port-req-miss-netrc ()
  (ert-with-temp-file netrc-file
    :text "\
machine x.com password a
machine x.com port 42 password b
"
    (let* ((auth-sources (list netrc-file))
           (auth-source-do-cache nil)
           (results (auth-source-search
                     :host "x.com" :port 22 :require '(:port) :max 2)))
      (should-not results))))

(ert-deftest auth-source-pass-extra-query-keywords--wild-port-req-miss ()
  (let ((auth-source-pass-extra-query-keywords t))
    (auth-source-pass--with-store '(("x.com" (secret . "a"))
                                    ("x.com:42" (secret . "b")))
      (auth-source-pass-enable)
      (should-not (auth-source-search
                   :host "x.com" :port 22 :require '(:port) :max 2)))))

;; Specifying a :host without a :user finds a lone entry and does not
;; include extra fields (i.e., :port nil) in the result.
;; https://lists.gnu.org/archive/html/emacs-devel/2022-11/msg00130.html

(ert-deftest auth-source-pass-extra-query-keywords--netrc-akib ()
  (ert-with-temp-file netrc-file
    :text "\
machine x.com password a
machine disroot.org user akib password b
machine z.com password c
"
    (let* ((auth-sources (list netrc-file))
           (auth-source-do-cache nil)
           (results (auth-source-search :host "disroot.org" :max 2)))
      (dolist (result results)
        (setf (plist-get result :secret) (auth-info-password result)))
      (should (equal results
                     '((:host "disroot.org" :user "akib" :secret "b")))))))

(ert-deftest auth-source-pass-extra-query-keywords--akib ()
  (auth-source-pass--with-store '(("x.com" (secret . "a"))
                                  ("akib@disroot.org" (secret . "b"))
                                  ("z.com" (secret . "c")))
    (auth-source-pass-enable)
    (let* ((auth-source-pass-extra-query-keywords t)
           (results (auth-source-search :host "disroot.org" :max 2)))
      (dolist (result results)
        (setf (plist-get result :secret) (auth-info-password result)))
      (should (equal results
                     '((:host "disroot.org" :user "akib" :secret "b")))))))

;; Searches for :host are case-sensitive, and a returned host isn't
;; normalized.

(ert-deftest auth-source-pass-extra-query-keywords--netrc-host ()
  (ert-with-temp-file netrc-file
    :text "\
machine libera.chat password a
machine Libera.Chat password b
"
    (let* ((auth-sources (list netrc-file))
           (auth-source-do-cache nil)
           (results (auth-source-search :host "Libera.Chat" :max 2)))
      (dolist (result results)
        (setf (plist-get result :secret) (auth-info-password result)))
      (should (equal results '((:host "Libera.Chat" :secret "b")))))))

(ert-deftest auth-source-pass-extra-query-keywords--host ()
  (auth-source-pass--with-store '(("libera.chat" (secret . "a"))
                                  ("Libera.Chat" (secret . "b")))
    (auth-source-pass-enable)
    (let* ((auth-source-pass-extra-query-keywords t)
           (results (auth-source-search :host "Libera.Chat" :max 2)))
      (dolist (result results)
        (setf (plist-get result :secret) (auth-info-password result)))
      (should (equal results
                     '((:host "Libera.Chat" :secret "b")))))))


;; A retrieved store entry mustn't be nil regardless of whether its
;; path contains port or user components.

(ert-deftest auth-source-pass-extra-query-keywords--baseline ()
  (let ((auth-source-pass-extra-query-keywords t))
    (auth-source-pass--with-store '(("x.com"))
      (auth-source-pass-enable)
      (should-not (auth-source-search :host "x.com")))))

;; Output port type (int or string) matches that of input parameter.

(ert-deftest auth-source-pass-extra-query-keywords--port-type ()
  (let ((auth-source-pass-extra-query-keywords t)
        (f (lambda (r) (setf (plist-get r :secret) (auth-info-password r)) r)))
    (auth-source-pass--with-store '(("x.com:42" (secret . "a")))
      (auth-source-pass-enable)
      (should (equal (mapcar f (auth-source-search :host "x.com" :port 42))
                     '((:host "x.com" :port 42 :secret "a")))))
    (auth-source-pass--with-store '(("x.com:42" (secret . "a")))
      (auth-source-pass-enable)
      (should (equal (mapcar f (auth-source-search :host "x.com" :port "42"))
                     '((:host "x.com" :port "42" :secret "a")))))))

;; Match precision sometimes takes a back seat to the traversal
;; ordering.  Specifically, the :host (h1, ...) args hold greater sway
;; over the output because they determine the first coordinate in the
;; sequence of (host, user, port) combinations visited.  (Taking a
;; tree-wise view, these become the depth-1 nodes in a DFS.)

;; Note that all trailing /user forms are demoted for the sake of
;; predictability (see tests further below for details).  This means
;; that, in the following test, /bar is held in limbo, followed by
;; /foo, but they both retain priority over "gnu.org", as noted above.

(ert-deftest auth-source-pass-extra-query-keywords--hosts-first ()
  (auth-source-pass--with-store '(("x.com:42/bar" (secret . "a"))
                                  ("gnu.org" (secret . "b"))
                                  ("x.com" (secret . "c"))
                                  ("fake.com" (secret . "d"))
                                  ("x.com/foo" (secret . "e")))
    (auth-source-pass-enable)
    (let* ((auth-source-pass-extra-query-keywords t)
           (results (auth-source-search :host '("x.com" "gnu.org") :max 3)))
      (dolist (result results)
        (setf (plist-get result :secret) (auth-info-password result)))
      (should (equal results
                     ;; Notice gnu.org is never considered ^
                     '((:host "x.com" :secret "c")
                       (:host "x.com" :user "bar" :port "42" :secret "a")
                       (:host "x.com" :user "foo" :secret "e")))))))

;; This is another example given in the bug thread.

(ert-deftest auth-source-pass-extra-query-keywords--ambiguous-user-host ()
  (auth-source-pass--with-store '(("foo.com/bar.org" (secret . "a"))
                                  ("foo.com" (secret . "b"))
                                  ("bar.org" (secret . "c"))
                                  ("fake.com" (secret . "d")))
    (auth-source-pass-enable)
    (let* ((auth-source-pass-extra-query-keywords t)
           (results (auth-source-search :host "bar.org" :max 3)))
      (dolist (result results)
        (setf (plist-get result :secret) (auth-info-password result)))
      (should (equal results '((:host "bar.org" :secret "c")))))))

;; This conveys the same idea as `user-priorities', just below, but
;; with slightly more realistic and less legible values.

(ert-deftest auth-source-pass-extra-query-keywords--suffixed-user ()
  (let ((store (sort (copy-sequence '(("x.com:42/s p@m" (secret . "a"))
                                      ("s p@m@x.com" (secret . "b"))
                                      ("x.com" (secret . "?"))
                                      ("s p@m@y.org" (secret . "c"))
                                      ("fa ke" (secret . "?"))
                                      ("fa ke/s p@m" (secret . "d"))
                                      ("y.org/s p@m" (secret . "?"))
                                      ("s p@m@fa ke" (secret . "e"))))
                     (lambda (&rest _) (zerop (random 2))))))
    (auth-source-pass--with-store store
      (auth-source-pass-enable)
      (let* ((auth-source-pass-extra-query-keywords t)
             (results (auth-source-search :host '("x.com" "fa ke" "y.org")
                                          :user "s p@m"
                                          :require '(:user) :max 5)))
        (dolist (result results)
          (setf (plist-get result :secret) (auth-info-password result)))
        (should (equal results
                       '((:host "x.com" :user "s p@m" :secret "b")
                         (:host "x.com" :user "s p@m" :port "42" :secret "a")
                         (:host "fa ke" :user "s p@m" :secret "e")
                         (:host "fa ke" :user "s p@m" :secret "d")
                         (:host "y.org" :user "s p@m" :secret "c"))))))))

;; This is a more distilled version of `suffixed-user', above.  It
;; better illustrates that search order takes precedence over "/user"
;; demotion because otherwise * and ** would be swapped, below.  It
;; follows that omitting the :port 2, gets you {u@h:1, u@h:2, h:1/u,
;; h:2/u, u@g:1}.

(ert-deftest auth-source-pass-extra-query-keywords--user-priorities ()
  (let ((store (sort (copy-sequence '(("h:1/u" (secret . "/"))
                                      ("h:2/u" (secret . "/"))
                                      ("u@h:1" (secret . "@"))
                                      ("u@h:2" (secret . "@"))
                                      ("g:1/u" (secret . "/"))
                                      ("g:2/u" (secret . "/"))
                                      ("u@g:1" (secret . "@"))
                                      ("u@g:2" (secret . "@"))))
                     (lambda (&rest _) (zerop (random 2))))))
    (auth-source-pass--with-store store
      (auth-source-pass-enable)
      (let* ((auth-source-pass-extra-query-keywords t)
             (results (auth-source-search :host '("h" "g")
                                          :port 2
                                          :max 5)))
        (dolist (result results)
          (setf (plist-get result :secret) (auth-info-password result)))
        (should (equal results
                       '((:host "h" :user "u" :port 2 :secret "@")
                         (:host "h" :user "u" :port 2 :secret "/") ; *
                         (:host "g" :user "u" :port 2 :secret "@") ; **
                         (:host "g" :user "u" :port 2 :secret "/"))))))))

(provide 'auth-source-pass-tests)

;;; auth-source-pass-tests.el ends here
