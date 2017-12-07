;;; url-auth-tests.el --- Test suite for url-auth.

;; Copyright (C) 2015-2017 Free Software Foundation, Inc.

;; Author: Jarno Malmari <jarno@malmari.fi>

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

;; Test HTTP authentication methods.

;;; Code:

(require 'ert)
(require 'url-auth)

(defvar url-auth-test-challenges nil
  "List of challenges for testing.
Each challenge is a plist.  Values are as presented by the
server's WWW-Authenticate header field.")

;; Set explicitly for easier modification for re-runs.
(setq url-auth-test-challenges
      (list
       (list :qop "auth"
             :nonce "uBr3+qkQBybTr/dKWkmpUqVO7SaEwWYzyTKO7g==$"
             :uri "/random/path"
             :method "GET"
             :realm "Some test realm"
             :cnonce "YWU4NDcxYWMxMDAxMjlkMjAwMDE4MjI5MDAwMGY4NGQ="
             :nc "00000001"
             :username "jytky"
             :password "xi5Ac2HEfKt1lKKO05DCSqsK0u7hqqtsT"
             :expected-ha1 "af521db3a83abd91262fead04fa31892"
             :expected-ha2 "e490a6a147c79404b365d1f6059ddda5"
             :expected-response "ecb6396e93b9e09e31f19264cfd8f854")
       (list :nonce "a1be8a3065e00c5bf190ad499299aea5"
             :opaque "d7c2a27230fc8c74bb6e06be8c9cd189"
             :realm "The Test Realm"
             :username "user"
             :password "passwd"
             :uri "/digest-auth/auth/user/passwd"
             :method "GET"
             :expected-ha1 "19c41161a8720edaeb7922ef8531137d"
             :expected-ha2 "b44272ea65ee4af7fb26c5dba58f6863"
             :expected-response "46c47a6d8e1fa95a3efcf49724af3fe7")
       (list :nonce "servernonce"
             :username "user"
             :password "passwd"
             :realm "The Test Realm 1"
             :uri "/digest-auth/auth/user/passwd"
             :method "GET"
             :expected-ha1 "00f848f943c9a05dd06c932a7334f120"
             :expected-ha2 "b44272ea65ee4af7fb26c5dba58f6863"
             :expected-response "b8a48cdc9aa9e514509a5a5c53d4e8cf")
       (list :nonce "servernonce"
             :username "user"
             :password "passwd"
             :realm "The Test Realm 2"
             :uri "/digest-auth/auth/user/passwd"
             :method "GET"
             :expected-ha1 "74d6abd3651d6b8260733d8a4c37ec1a"
             :expected-ha2 "b44272ea65ee4af7fb26c5dba58f6863"
             :expected-response "0d84884d967e04440efc77e9e2b5b561")))

(ert-deftest url-auth-test-colonjoin ()
  "Check joining strings with `:'."
  (should (string= (url-digest-auth-colonjoin) ""))
  (should (string= (url-digest-auth-colonjoin nil) ""))
  (should (string= (url-digest-auth-colonjoin nil nil nil) "::"))
  (should (string= (url-digest-auth-colonjoin "") ""))
  (should (string= (url-digest-auth-colonjoin "" "") ":"))
  (should (string= (url-digest-auth-colonjoin "one") "one"))
  (should (string= (url-digest-auth-colonjoin "one" "two" "three") "one:two:three")))

(ert-deftest url-auth-test-digest-ha1 ()
  "Check HA1 computation."
  (dolist (row url-auth-test-challenges)
    (should (string= (url-digest-auth-make-ha1 (plist-get row :username)
                                               (plist-get row :realm)
                                               (plist-get row :password))
                     (plist-get row :expected-ha1)
                     ))))

(ert-deftest url-auth-test-digest-ha2 ()
  "Check HA2 computation."
  (dolist (row url-auth-test-challenges)
    (should (string= (url-digest-auth-make-ha2 (plist-get row :method)
                                               (plist-get row :uri))
                     (plist-get row :expected-ha2)))))

(ert-deftest url-auth-test-digest-request-digest ()
  "Check digest response value."
  (dolist (row url-auth-test-challenges)
    (should (string= (plist-get row :expected-response)
                     (if (plist-member row :qop)
                         (url-digest-auth-make-request-digest-qop
                          (plist-get row :qop)
                          (plist-get row :expected-ha1)
                          (plist-get row :expected-ha2)
                          (plist-get row :nonce)
                          (plist-get row :nc)
                          (plist-get row :cnonce))
                       (url-digest-auth-make-request-digest
                        (plist-get row :expected-ha1)
                        (plist-get row :expected-ha2)
                        (plist-get row :nonce)))))))

(ert-deftest url-auth-test-digest-create-key ()
  "Check user credentials in their hashed form."
  (dolist (challenge url-auth-test-challenges)
    (let ((key (url-digest-auth-create-key (plist-get challenge :username)
                                           (plist-get challenge :password)
                                           (plist-get challenge :realm)
                                           (plist-get challenge :method)
                                           (plist-get challenge :uri))))
      (should (= (length key) 2))
      (should (string= (nth 0 key) (plist-get challenge :expected-ha1)))
      (should (string= (nth 1 key) (plist-get challenge :expected-ha2)))
      )))

(ert-deftest url-auth-test-digest-auth-retrieve-cache ()
  "Check how the entry point retrieves cached authentication.
Essential is how realms and paths are matched."

  (let* ((url-digest-auth-storage
          '(("example.org:80"
             ("/path/auth1" "auth1user" "key")
             ("/path" "pathuser" "key")
             ("/" "rootuser" "key")
             ("realm1" "realm1user" "key")
             ("realm2" "realm2user" "key")
             ("/path/auth2" "auth2user" "key"))
            ("example.org:443"
             ("realm" "secure_user" "key"))
            ("rootless.org:80"          ; no "/" entry for this on purpose
             ("/path" "pathuser" "key")
             ("realm" "realmuser" "key"))))
         (attrs (list (cons "nonce" "servernonce")))
         auth)

    (dolist (row (list
                  ;; If :expected-user is `nil' it indicates
                  ;; authentication information shouldn't be found.

                  ;; non-existent server
                  (list :url "http://other.com/path"
                        :realm nil :expected-user nil)

                  ;; unmatched port
                  (list :url "http://example.org:444/path"
                        :realm nil :expected-user nil)

                  ;; root, no realm
                  (list :url "http://example.org/"
                        :realm nil :expected-user "rootuser")

                  ;; root, no realm, explicit port
                  (list :url "http://example.org:80/"
                        :realm nil :expected-user "rootuser")

                  (list :url "http://example.org/unknown"
                        :realm nil :expected-user "rootuser")

                  ;; realm specified, overrides any path
                  (list :url "http://example.org/"
                        :realm "realm1" :expected-user "realm1user")

                  ;; realm specified, overrides any path
                  (list :url "http://example.org/"
                        :realm "realm2" :expected-user "realm2user")

                  ;; authentication determined by path
                  (list :url "http://example.org/path/auth1/query"
                        :realm nil :expected-user "auth1user")

                  ;; /path shadows /path/auth2, hence pathuser is expected
                  (list :url "http://example.org/path/auth2/query"
                        :realm nil :expected-user "pathuser")

                  (list :url "https://example.org/path"
                        :realm nil :expected-user "secure_user")

                  ;; not really secure user but using the same port
                  (list :url "http://example.org:443/path"
                        :realm nil :expected-user "secure_user")

                  ;; preferring realm user over path, even though no
                  ;; realm specified (not sure why)
                  (list :url "http://rootless.org/"
                        :realm nil :expected-user "realmuser")
                  ;; second variant for the same case
                  (list :url "http://rootless.org/unknown/path"
                        :realm nil :expected-user "realmuser")

                  ;; path match
                  (list :url "http://rootless.org/path/query?q=a"
                        :realm nil :expected-user "pathuser")

                  ;; path match, realm match, prefer realm
                  (list :url "http://rootless.org/path/query?q=a"
                        :realm "realm" :expected-user "realmuser")
                  ))
      (setq auth (url-digest-auth (plist-get row :url)
                                  nil nil
                                  (plist-get row :realm) attrs))
      (if (plist-get row :expected-user)
          (progn (should auth)
                 (should (string-match ".*username=\"\\(.*?\\)\".*" auth))
                 (should (string= (match-string 1 auth)
                                  (plist-get row :expected-user))))
        (should-not auth)))))

(ert-deftest url-auth-test-digest-auth ()
  "Check common authorization string contents.
Challenges with qop are not checked for response since a unique
cnonce is used for generating them which is not mocked by the
test and cannot be passed by arguments to `url-digest-auth'."
  (dolist (challenge url-auth-test-challenges)
    (let* ((attrs (append
                   (list (cons "nonce" (plist-get challenge :nonce)))
                   (if (plist-get challenge :qop)
                       (list (cons "qop" (plist-get challenge :qop))))))
           (url (concat "http://example.org" (plist-get challenge :uri)))
           url-digest-auth-storage
           auth)
      ;; Add authentication info to cache so `url-digest-auth' can
      ;; complete without prompting minibuffer input.
      (setq url-digest-auth-storage
            (list
             (list "example.org:80"
                   (cons (or (plist-get challenge :realm) "/")
                         (cons (plist-get challenge :username)
                               (url-digest-auth-create-key
                                (plist-get challenge :username)
                                (plist-get challenge :password)
                                (plist-get challenge :realm)
                                (plist-get challenge :method)
                                (plist-get challenge :uri)))))))
      (setq auth (url-digest-auth (url-generic-parse-url url) nil nil
                                  (plist-get challenge :realm) attrs))
      (should auth)
      (should (string-prefix-p "Digest " auth))
      (should (string-match ".*username=\"\\(.*?\\)\".*" auth))
      (should (string= (match-string 1 auth)
                       (plist-get challenge :username)))
      (should (string-match ".*realm=\"\\(.*?\\)\".*" auth))
      (should (string= (match-string 1 auth)
                       (plist-get challenge :realm)))

      (if (plist-member challenge :qop)
          (progn
            ;; We don't know these, just check that they exists.
            (should (string-match-p ".*response=\".*?\".*" auth))
            (should (string-match-p ".*nc=\".*?\".*" auth))
            (should (string-match-p ".*cnonce=\".*?\".*" auth)))
        (should (string-match ".*response=\"\\(.*?\\)\".*" auth))
        (should (string= (match-string 1 auth)
                         (plist-get challenge :expected-response))))
        )))

(ert-deftest url-auth-test-digest-auth-opaque ()
  "Check that `opaque' value is added to result when presented by
the server."
  (let* ((url-digest-auth-storage
          '(("example.org:80" ("/" "user" "key"))))
         (attrs (list (cons "nonce" "anynonce")))
         auth)
    ;; Get authentication info from cache without `opaque'.
    (setq auth (url-digest-auth "http://example.org/path" nil nil nil attrs))
    (should auth)
    (should-not (string-match-p "opaque=" auth))

    ;; Add `opaque' to attributes.
    (push (cons "opaque" "opaque-value") attrs)
    (setq auth (url-digest-auth "http://example.org/path" nil nil nil attrs))
    (should auth)
    (should (string-match ".*opaque=\"\\(.*?\\)\".*" auth))
    (should (string= (match-string 1 auth) "opaque-value"))))

(provide 'url-auth-tests)
;;; url-auth-tests.el ends here
