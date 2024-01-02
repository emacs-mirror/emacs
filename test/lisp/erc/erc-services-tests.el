;;; erc-services-tests.el --- Tests for erc-services.  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO: move the auth-source tests somewhere else.  They've been
;; stashed here for pragmatic reasons.

;;; Code:

(require 'ert-x)
(require 'erc-services)
(require 'erc-compat)
(require 'secrets)

;;;; Core auth-source

(ert-deftest erc--auth-source-determine-params-merge ()
  (let ((erc-session-server "irc.gnu.org")
        (erc-server-announced-name "my.gnu.org")
        (erc-session-port 6697)
        (erc-network 'fake)
        (erc-server-current-nick "tester")
        (erc-networks--id (erc-networks--id-create 'GNU.chat)))

    (should (equal (erc--auth-source-determine-params-merge)
                   '(:host ("GNU.chat" "my.gnu.org" "irc.gnu.org")
                           :port ("6697" "irc")
                           :require (:secret))))

    (should (equal (erc--auth-source-determine-params-merge :host "fake")
                   '(:host ("fake" "GNU.chat" "my.gnu.org" "irc.gnu.org")
                           :port ("6697" "irc")
                           :require (:secret))))

    (should (equal (erc--auth-source-determine-params-merge
                    :host '("fake") :require :host)
                   '(:host ("fake" "GNU.chat" "my.gnu.org" "irc.gnu.org")
                           :require (:host :secret)
                           :port ("6697" "irc"))))

    (should (equal (erc--auth-source-determine-params-merge
                    :host '("fake" "GNU.chat") :port "1234" :x "x")
                   '(:host ("fake" "GNU.chat" "my.gnu.org" "irc.gnu.org")
                           :port ("1234" "6697" "irc")
                           :x ("x")
                           :require (:secret))))))

(defun erc-services-tests--wrap-search (s)
  (lambda (&rest r) (erc--unfun (apply s r))))

;; Some of the following may be related to bug#23438.

(defun erc-services-tests--auth-source-standard (search)
  (setq search (erc-services-tests--wrap-search search))

  (ert-info ("Session wins")
    (let ((erc-session-server "irc.gnu.org")
          (erc-server-announced-name "my.gnu.org")
          (erc-session-port 6697)
          (erc-network 'fake)
          (erc-server-current-nick "tester")
          (erc-networks--id (erc-networks--id-create 'GNU.chat)))
      (should (string= (funcall search :user "#chan") "foo"))))

  (ert-info ("Network wins")
    (let* ((erc-session-server "irc.gnu.org")
           (erc-server-announced-name "my.gnu.org")
           (erc-session-port 6697)
           (erc-network 'GNU.chat)
           (erc-server-current-nick "tester")
           (erc-networks--id (erc-networks--id-create nil)))
      (should (string= (funcall search :user "#chan") "foo"))))

  (ert-info ("Announced wins")
    (let ((erc-session-server "irc.gnu.org")
          (erc-server-announced-name "my.gnu.org")
          (erc-session-port 6697)
          erc-network
          (erc-networks--id (erc-networks--id-create nil)))
      (should (string= (funcall search :user "#chan") "baz")))))

(defun erc-services-tests--auth-source-announced (search)
  (setq search (erc-services-tests--wrap-search search))
  (let* ((erc--isupport-params (make-hash-table))
         (erc-server-parameters '(("CHANTYPES" . "&#")))
         (erc--target (erc--target-from-string "&chan")))

    (ert-info ("Announced prioritized")

      (ert-info ("Announced wins")
        (let* ((erc-session-server "irc.gnu.org")
               (erc-server-announced-name "my.gnu.org")
               (erc-session-port 6697)
               (erc-network 'GNU.chat)
               (erc-server-current-nick "tester")
               (erc-networks--id (erc-networks--id-create nil)))
          (should (string= (funcall search :user "#chan") "baz"))))

      (ert-info ("Peer next")
        (let* ((erc-server-announced-name "irc.gnu.org")
               (erc-session-port 6697)
               (erc-network 'GNU.chat)
               (erc-server-current-nick "tester")
               (erc-networks--id (erc-networks--id-create nil)))
          (should (string= (funcall search :user "#chan") "bar"))))

      (ert-info ("Network used as fallback")
        (let* ((erc-session-port 6697)
               (erc-network 'GNU.chat)
               (erc-server-current-nick "tester")
               (erc-networks--id (erc-networks--id-create nil)))
          (should (string= (funcall search :user "#chan") "foo")))))))

(defun erc-services-tests--auth-source-overrides (search)
  (setq search (erc-services-tests--wrap-search search))
  (let* ((erc-session-server "irc.gnu.org")
         (erc-server-announced-name "my.gnu.org")
         (erc-network 'GNU.chat)
         (erc-server-current-nick "tester")
         (erc-networks--id (erc-networks--id-create nil))
         (erc-session-port 6667))

    (ert-info ("Specificity and overrides")

      (ert-info ("More specific port")
        (let ((erc-session-port 6697))
          (should (string= (funcall search :user "#chan") "spam"))))

      (ert-info ("More specific user (network loses)")
        (should (string= (funcall search :user '("#fsf")) "42")))

      (ert-info ("Actual override")
        (should (string= (funcall search :port "6667") "sesame")))

      (ert-info ("Overrides don't interfere with post-processing")
        (should (string= (funcall search :host "MyHost") "123"))))))

;; auth-source netrc backend

(defvar erc-services-tests--auth-source-entries
  '("machine irc.gnu.org port irc user \"#chan\" password bar"
    "machine my.gnu.org port irc user \"#chan\" password baz"
    "machine GNU.chat port irc user \"#chan\" password foo"))

;; FIXME explain what this is for
(defun erc-services-tests--auth-source-shuffle (&rest extra)
  (string-join `(,@(sort (append erc-services-tests--auth-source-entries extra)
                         (lambda (&rest _) (zerop (random 2))))
                 "")
               "\n"))

(ert-deftest erc--auth-source-search--netrc-standard ()
  (ert-with-temp-file netrc-file
    :prefix "erc--auth-source-search--standard"
    :text (erc-services-tests--auth-source-shuffle)

    (let ((auth-sources (list netrc-file))
          (auth-source-do-cache nil))
      (erc-services-tests--auth-source-standard #'erc-auth-source-search))))

(ert-deftest erc--auth-source-search--netrc-announced ()
  (ert-with-temp-file netrc-file
    :prefix "erc--auth-source-search--announced"
    :text (erc-services-tests--auth-source-shuffle)

    (let ((auth-sources (list netrc-file))
          (auth-source-do-cache nil))
      (erc-services-tests--auth-source-announced #'erc-auth-source-search))))

(ert-deftest erc--auth-source-search--netrc-overrides ()
  (ert-with-temp-file netrc-file
    :prefix "erc--auth-source-search--overrides"
    :text (erc-services-tests--auth-source-shuffle
           "machine GNU.chat port 6697 user \"#chan\" password spam"
           "machine my.gnu.org port irc user \"#fsf\" password 42"
           "machine irc.gnu.org port 6667 password sesame"
           "machine MyHost port irc password 456"
           "machine MyHost port 6667 password 123")

    (let ((auth-sources (list netrc-file))
          (auth-source-do-cache nil))
      (erc-services-tests--auth-source-overrides #'erc-auth-source-search))))

;; auth-source plstore backend

(defun erc-services-test--call-with-plstore (&rest args)
  (advice-add 'epg-decrypt-string :override
              (lambda (&rest r) (prin1-to-string (cadr r)))
              '((name . erc--auth-source-plstore)))
  (advice-add 'epg-find-configuration :override
              (lambda (&rest _) "" '((program . "/bin/true")))
              '((name . erc--auth-source-plstore)))
  (unwind-protect
      (apply #'erc-auth-source-search args)
    (advice-remove 'epg-decrypt-string 'erc--auth-source-plstore)
    (advice-remove 'epg-find-configuration 'erc--auth-source-plstore)))

(defvar erc-services-tests--auth-source-plstore-standard-entries
  '(("ba950d38118a76d71f9f0591bb373d6cb366a512"
     :secret-secret t
     :host "irc.gnu.org"
     :user "#chan"
     :port "irc")
    ("7f17ca445d11158065e911a6d0f4cbf52ca250e3"
     :secret-secret t
     :host "my.gnu.org"
     :user "#chan"
     :port "irc")
    ("fcd3c8bd6daf4509de0ad6ee98e744ce0fca9377"
     :secret-secret t
     :host "GNU.chat"
     :user "#chan"
     :port "irc")))

(defvar erc-services-tests--auth-source-plstore-standard-secrets
  '(("ba950d38118a76d71f9f0591bb373d6cb366a512" :secret "bar")
    ("7f17ca445d11158065e911a6d0f4cbf52ca250e3" :secret "baz")
    ("fcd3c8bd6daf4509de0ad6ee98e744ce0fca9377" :secret "foo")))

(ert-deftest erc--auth-source-search--plstore-standard ()
  (ert-with-temp-file plstore-file
    :suffix ".plist"
    :text (concat ";;; public entries -*- mode: plstore -*- \n"
                  (prin1-to-string
                   erc-services-tests--auth-source-plstore-standard-entries)
                  "\n;;; secret entries\n"
                  (prin1-to-string
                   erc-services-tests--auth-source-plstore-standard-secrets)
                  "\n")

    (let ((auth-sources (list plstore-file))
          (auth-source-do-cache nil))
      (erc-services-tests--auth-source-standard
       #'erc-services-test--call-with-plstore))
    (kill-buffer (get-file-buffer plstore-file))))

(ert-deftest erc--auth-source-search--plstore-announced ()
  (ert-with-temp-file plstore-file
    :suffix ".plist"
    :text (concat ";;; public entries -*- mode: plstore -*- \n"
                  (prin1-to-string
                   erc-services-tests--auth-source-plstore-standard-entries)
                  "\n;;; secret entries\n"
                  (prin1-to-string
                   erc-services-tests--auth-source-plstore-standard-secrets)
                  "\n")

    (let ((auth-sources (list plstore-file))
          (auth-source-do-cache nil))
      (erc-services-tests--auth-source-announced
       #'erc-services-test--call-with-plstore))
    (kill-buffer (get-file-buffer plstore-file))))

(ert-deftest erc--auth-source-search--plstore-overrides ()
  (ert-with-temp-file plstore-file
    :suffix ".plist"
    :text (concat
           ";;; public entries -*- mode: plstore -*- \n"
           (prin1-to-string
            `(,@erc-services-tests--auth-source-plstore-standard-entries
              ("1b3fab249a8dff77a4d8fe7eb4b0171b25cc711a"
               :secret-secret t :host "GNU.chat" :user "#chan" :port "6697")
              ("6cbcdc39476b8cfcca6f3e9a7876f41ec3f708cc"
               :secret-secret t :host "my.gnu.org" :user "#fsf" :port "irc")
              ("a33e2b3bd2d6f33995a4b88710a594a100c5e41d"
               :secret-secret t :host "irc.gnu.org" :port "6667")
              ("ab2fd349b2b7d6a9215bb35a92d054261b0b1537"
               :secret-secret t :host "MyHost" :port "irc")
              ("61a6bd552059494f479ff720e8de33e22574650a"
               :secret-secret t :host "MyHost" :port "6667")))
           "\n;;; secret entries\n"
           (prin1-to-string
            `(,@erc-services-tests--auth-source-plstore-standard-secrets
              ("1b3fab249a8dff77a4d8fe7eb4b0171b25cc711a" :secret "spam")
              ("6cbcdc39476b8cfcca6f3e9a7876f41ec3f708cc" :secret "42")
              ("a33e2b3bd2d6f33995a4b88710a594a100c5e41d" :secret "sesame")
              ("ab2fd349b2b7d6a9215bb35a92d054261b0b1537" :secret "456")
              ("61a6bd552059494f479ff720e8de33e22574650a" :secret "123")))
           "\n")

    (let ((auth-sources (list plstore-file))
          (auth-source-do-cache nil))
      (erc-services-tests--auth-source-overrides
       #'erc-services-test--call-with-plstore))
    (kill-buffer (get-file-buffer plstore-file))))

;; auth-source JSON backend

(defvar erc-services-tests--auth-source-json-standard-entries
  [(:host "irc.gnu.org" :port "irc" :user "#chan" :secret "bar")
   (:host "my.gnu.org" :port "irc" :user "#chan" :secret "baz")
   (:host "GNU.chat" :port "irc" :user "#chan" :secret "foo")])

(ert-deftest erc--auth-source-search--json-standard ()
  (ert-with-temp-file json-store
    :suffix ".json"
    :text (let ((json-object-type 'plist))
            (json-encode
             erc-services-tests--auth-source-json-standard-entries))
    (let ((auth-sources (list json-store))
          (auth-source-do-cache nil))
      (erc-services-tests--auth-source-standard #'erc-auth-source-search))))

(ert-deftest erc--auth-source-search--json-announced ()
  (ert-with-temp-file plstore-file
    :suffix ".json"
    :text (let ((json-object-type 'plist))
            (json-encode
             erc-services-tests--auth-source-json-standard-entries))

    (let ((auth-sources (list plstore-file))
          (auth-source-do-cache nil))
      (erc-services-tests--auth-source-announced #'erc-auth-source-search))))

(ert-deftest erc--auth-source-search--json-overrides ()
  (ert-with-temp-file json-file
    :suffix ".json"
    :text (let ((json-object-type 'plist))
            (json-encode
             (vconcat
              erc-services-tests--auth-source-json-standard-entries
              [(:secret "spam" :host "GNU.chat" :user "#chan" :port "6697")
               (:secret "42" :host "my.gnu.org" :user "#fsf" :port "irc")
               (:secret "sesame" :host "irc.gnu.org" :port "6667")
               (:secret "456" :host "MyHost" :port "irc")
               (:secret "123" :host "MyHost" :port "6667")])))

    (let ((auth-sources (list json-file))
          (auth-source-do-cache nil))
      (erc-services-tests--auth-source-overrides #'erc-auth-source-search))))

;; auth-source-secrets backend

(defvar erc-services-tests--auth-source-secrets-standard-entries
  '(("#chan@irc.gnu.org:irc" ; label
     (:host . "irc.gnu.org")
     (:user . "#chan")
     (:port . "irc")
     (:xdg:schema . "org.freedesktop.Secret.Generic"))
    ("#chan@my.gnu.org:irc"
     (:host . "my.gnu.org")
     (:user . "#chan")
     (:port . "irc")
     (:xdg:schema . "org.freedesktop.Secret.Generic"))
    ("#chan@GNU.chat:irc"
     (:host . "GNU.chat")
     (:user . "#chan")
     (:port . "irc")
     (:xdg:schema . "org.freedesktop.Secret.Generic"))))

(defvar erc-services-tests--auth-source-secrets-standard-secrets
  '(("#chan@irc.gnu.org:irc" . "bar")
    ("#chan@my.gnu.org:irc" . "baz")
    ("#chan@GNU.chat:irc" . "foo")))

(ert-deftest erc--auth-source-search--secrets-standard ()
  (skip-unless (bound-and-true-p secrets-enabled))
  (let ((auth-sources '("secrets:Test"))
        (auth-source-do-cache nil)
        (entries erc-services-tests--auth-source-secrets-standard-entries)
        (secrets erc-services-tests--auth-source-secrets-standard-secrets))

    (cl-letf (((symbol-function 'secrets-search-items)
               (lambda (col &rest r)
                 (should (equal col "Test"))
                 (should (plist-get r :user))
                 (map-keys entries)))
              ((symbol-function 'secrets-get-secret)
               (lambda (col label)
                 (should (equal col "Test"))
                 (assoc-default label secrets)))
              ((symbol-function 'secrets-get-attributes)
               (lambda (col label)
                 (should (equal col "Test"))
                 (assoc-default label entries))))

      (erc-services-tests--auth-source-standard #'erc-auth-source-search))))

(ert-deftest erc--auth-source-search--secrets-announced ()
  (skip-unless (bound-and-true-p secrets-enabled))
  (let ((auth-sources '("secrets:Test"))
        (auth-source-do-cache nil)
        (entries erc-services-tests--auth-source-secrets-standard-entries)
        (secrets erc-services-tests--auth-source-secrets-standard-secrets))

    (cl-letf (((symbol-function 'secrets-search-items)
               (lambda (col &rest r)
                 (should (equal col "Test"))
                 (should (plist-get r :user))
                 (map-keys entries)))
              ((symbol-function 'secrets-get-secret)
               (lambda (col label)
                 (should (equal col "Test"))
                 (assoc-default label secrets)))
              ((symbol-function 'secrets-get-attributes)
               (lambda (col label)
                 (should (equal col "Test"))
                 (assoc-default label entries))))

      (erc-services-tests--auth-source-announced #'erc-auth-source-search))))

(ert-deftest erc--auth-source-search--secrets-overrides ()
  (skip-unless (bound-and-true-p secrets-enabled))
  (let ((auth-sources '("secrets:Test"))
        (auth-source-do-cache nil)
        (entries `(,@erc-services-tests--auth-source-secrets-standard-entries
                   ("#chan@GNU.chat:6697"
                    (:host . "GNU.chat") (:user . "#chan") (:port . "6697")
                    (:xdg:schema . "org.freedesktop.Secret.Generic"))
                   ("#fsf@my.gnu.org:irc"
                    (:host . "my.gnu.org") (:user . "#fsf") (:port . "irc")
                    (:xdg:schema . "org.freedesktop.Secret.Generic"))
                   ("irc.gnu.org:6667"
                    (:host . "irc.gnu.org") (:port . "6667")
                    (:xdg:schema . "org.freedesktop.Secret.Generic"))
                   ("MyHost:irc"
                    (:host . "MyHost") (:port . "irc")
                    (:xdg:schema . "org.freedesktop.Secret.Generic"))
                   ("MyHost:6667"
                    (:host . "MyHost") (:port . "6667")
                    (:xdg:schema . "org.freedesktop.Secret.Generic"))))
        (secrets `(,@erc-services-tests--auth-source-secrets-standard-secrets
                   ("#chan@GNU.chat:6697" . "spam")
                   ("#fsf@my.gnu.org:irc" . "42" )
                   ("irc.gnu.org:6667" . "sesame")
                   ("MyHost:irc" . "456")
                   ("MyHost:6667" . "123"))))

    (cl-letf (((symbol-function 'secrets-search-items)
               (lambda (col &rest _)
                 (should (equal col "Test"))
                 (map-keys entries)))
              ((symbol-function 'secrets-get-secret)
               (lambda (col label)
                 (should (equal col "Test"))
                 (assoc-default label secrets)))
              ((symbol-function 'secrets-get-attributes)
               (lambda (col label)
                 (should (equal col "Test"))
                 (assoc-default label entries))))

      (erc-services-tests--auth-source-overrides #'erc-auth-source-search))))

;; auth-source-pass backend

(require 'auth-source-pass)

;; `auth-source-pass--find-match-unambiguous' returns something like:
;;
;;   (list :host "irc.gnu.org"
;;         :port "6697"
;;         :user "rms"
;;         :secret
;;         #[0 "\301\302\300\"\207"
;;             [((secret . "freedom")) auth-source-pass--get-attr secret] 3])
;;
;; This function gives ^ (faked here to avoid gpg and file IO).  See
;; `auth-source-pass--with-store' in ../auth-source-pass-tests.el
(defun erc-services-tests--asp-parse-entry (store entry)
  (when-let ((found (cl-find entry store :key #'car :test #'string=)))
    (list (assoc 'secret (cdr found)))))

(defvar erc-join-tests--auth-source-pass-entries
  '(("irc.gnu.org:irc/#chan" (secret . "bar"))
    ("my.gnu.org:irc/#chan" (secret . "baz"))
    ("GNU.chat:irc/#chan" (secret . "foo"))))

(ert-deftest erc--auth-source-search--pass-standard ()
  (let ((store erc-join-tests--auth-source-pass-entries)
        (auth-sources '(password-store))
        (auth-source-do-cache nil))

    (cl-letf (((symbol-function 'auth-source-pass-parse-entry)
               (apply-partially #'erc-services-tests--asp-parse-entry store))
              ((symbol-function 'auth-source-pass-entries)
               (lambda () (mapcar #'car store))))

      (erc-services-tests--auth-source-standard #'erc-auth-source-search))))

(ert-deftest erc--auth-source-search--pass-announced ()
  (let ((store erc-join-tests--auth-source-pass-entries)
        (auth-sources '(password-store))
        (auth-source-do-cache nil))

    (cl-letf (((symbol-function 'auth-source-pass-parse-entry)
               (apply-partially #'erc-services-tests--asp-parse-entry store))
              ((symbol-function 'auth-source-pass-entries)
               (lambda () (mapcar #'car store))))

      (erc-services-tests--auth-source-announced #'erc-auth-source-search))))

(ert-deftest erc--auth-source-search--pass-overrides ()
  (let ((store
         `(,@erc-join-tests--auth-source-pass-entries
           ("GNU.chat:6697/#chan" (secret . "spam"))
           ("my.gnu.org:irc/#fsf" (secret . "42"))
           ("irc.gnu.org:6667" (secret . "sesame"))
           ("MyHost:irc" (secret . "456"))
           ("MyHost:6667" (secret . "123"))))
        (auth-sources '(password-store))
        (auth-source-do-cache nil))

    (cl-letf (((symbol-function 'auth-source-pass-parse-entry)
               (apply-partially #'erc-services-tests--asp-parse-entry store))
              ((symbol-function 'auth-source-pass-entries)
               (lambda () (mapcar #'car store))))

      (erc-services-tests--auth-source-overrides #'erc-auth-source-search))))

;;;; The services module

(ert-deftest erc-nickserv-get-password ()
  (should erc-prompt-for-nickserv-password)
  (ert-with-temp-file netrc-file
    :prefix "erc-nickserv-get-password"
    :text (mapconcat 'identity
                     '("machine GNU/chat port 6697 user bob password spam"
                       "machine FSF.chat port 6697 user bob password sesame"
                       "machine MyHost port irc password 123")
                     "\n")

    (let* ((auth-sources (list netrc-file))
           (auth-source-do-cache nil)
           (erc-nickserv-passwords '((FSF.chat (("alice" . "foo")
                                                ("joe" . "bar")))))
           (erc-use-auth-source-for-nickserv-password t)
           (erc-session-server "irc.gnu.org")
           (erc-server-announced-name "my.gnu.org")
           (erc-network 'FSF.chat)
           (erc-server-current-nick "tester")
           (erc-networks--id (erc-networks--id-create nil))
           (erc-session-port 6697)
           (search (erc-services-tests--wrap-search
                    #'erc-nickserv-get-password)))

      (ert-info ("Lookup custom option")
        (should (string= (funcall search "alice") "foo")))

      (ert-info ("Auth source")
        (ert-info ("Network")
          (should (string= (funcall search "bob") "sesame")))

        (ert-info ("Network ID")
          (let ((erc-networks--id (erc-networks--id-create 'GNU/chat)))
            (should (string= (funcall search "bob") "spam")))))

      (ert-info ("Read input")
        (should (string=
                 (ert-simulate-keys "baz\r" (erc-nickserv-get-password "mike"))
                 "baz")))

      (ert-info ("Failed")
        (should-not (ert-simulate-keys "\r"
                      (erc-nickserv-get-password "fake")))))))


;;; erc-services-tests.el ends here
