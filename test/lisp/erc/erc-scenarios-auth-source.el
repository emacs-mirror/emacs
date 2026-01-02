;;; erc-scenarios-auth-source.el --- auth-source scenarios -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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

;; Commentary:

;; For practical reasons (mainly lack of imagination), this file
;; contains tests for both server-password and NickServ contexts.

;;; Code:

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(eval-when-compile (require 'erc-join)
                   (require 'erc-services))

(defun erc-scenarios-common--auth-source (id dialog &rest rest)
  (push "machine GNU.chat port %d user \"#chan\" password spam" rest)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/auth-source")
       (dumb-server (erc-d-run "localhost" t dialog))
       (port (process-contact dumb-server :service))
       (ents `(,@(mapcar (lambda (fmt) (format fmt port)) rest)
               "machine MyHost port irc password 123"))
       (netrc-file (make-temp-file "auth-source-test" nil nil
                                   (string-join ents "\n")))
       (auth-sources (list netrc-file))
       (auth-source-do-cache nil)
       (erc-port (and (eq erc-port 'test) (number-to-string port)))
       (erc-scenarios-common-extra-teardown (lambda ()
                                              (delete-file netrc-file)))
       ;; With a `cl-defun', a keyword's presence prevents the default
       ;; init form from being evaluated, even if its value is nil.
       (args `( :server "127.0.0.1"
                ,@(and (null erc-port) (list :port port))
                :nick "tester"
                :full-name "tester"
                :id ,id)))

    (ert-info ("Connect")
      (with-current-buffer (apply #'erc args)
        (should (string= (buffer-name) (if id
                                           (symbol-name id)
                                         (format "127.0.0.1:%d" port))))
        (erc-d-t-wait-for 10 (eq erc-network 'FooNet))))))

(ert-deftest erc-scenarios-base-auth-source-server--dialed ()
  :tags '(:expensive-test)
  (let ((erc-port 'test))
    (erc-scenarios-common--auth-source
     nil 'foonet
     "machine GNU.chat port %d user tester password fake"
     "machine FooNet port %d user tester password fake"
     "machine 127.0.0.1 port \"%s\" user tester password changeme" ; correct
     "machine 127.0.0.1 port %d user imposter password fake")))

(ert-deftest erc-scenarios-base-auth-source-server--netid ()
  :tags '(:expensive-test)
  (erc-scenarios-common--auth-source
   'MySession 'foonet
   "machine MySession port %d user tester password changeme"
   "machine 127.0.0.1 port %d user tester password fake"
   "machine FooNet port %d user tester password fake"))

(ert-deftest erc-scenarios-base-auth-source-server--netid-custom ()
  :tags '(:expensive-test)
  (let ((erc-auth-source-server-function
         (lambda (&rest _) (erc-auth-source-search :host "MyHost"))))
    (erc-scenarios-common--auth-source
     'MySession 'foonet
     "machine 127.0.0.1 port %d user tester password fake"
     "machine MyHost port %d user tester password changeme"
     "machine MySession port %d user tester password fake")))

(ert-deftest erc-scenarios-base-auth-source-server--nopass ()
  :tags '(:expensive-test)
  (let (erc-auth-source-server-function)
    (erc-scenarios-common--auth-source nil 'nopass)))

(ert-deftest erc-scenarios-base-auth-source-server--nopass-netid ()
  :tags '(:expensive-test)
  (let (erc-auth-source-server-function)
    (erc-scenarios-common--auth-source 'MySession 'nopass)))

;; Identify via auth source with no initial password

(defun erc-scenarios-common--services-auth-source (&rest rest)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "services/auth-source")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'libera))
       (port (process-contact dumb-server :service))
       (ents `(,@(mapcar (lambda (fmt) (format fmt port)) rest)
               "machine MyHost port irc password 123"))
       (netrc-file (make-temp-file "auth-source-test" nil nil
                                   (string-join ents "\n")))
       (auth-sources (list netrc-file))
       (auth-source-do-cache nil)
       (erc-modules (cons 'services erc-modules))
       (erc-use-auth-source-for-nickserv-password t) ; do consult for NickServ
       (expect (erc-d-t-make-expecter))
       (erc-scenarios-common-extra-teardown (lambda ()
                                              (delete-file netrc-file))))

    (cl-letf (((symbol-function 'read-passwd)
               (lambda (&rest _) (error "Unexpected read-passwd call"))))
      (ert-info ("Connect without password")
        (with-current-buffer (erc :server "127.0.0.1"
                                  :port port
                                  :nick "tester"
                                  :full-name "tester")
          (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
          (erc-d-t-wait-for 8 (eq erc-network 'Libera.Chat))
          (funcall expect 3 "This nickname is registered.")
          (funcall expect 3 "You are now identified")
          (funcall expect 3 "Last login from")
          (erc-cmd-QUIT ""))))

    (erc-services-mode -1)

    (should-not (memq 'services erc-modules))))

;; These tests are about authenticating to nick services

(ert-deftest erc-scenarios-services-auth-source--network ()
  :tags '(:expensive-test)
  ;; Skip consulting auth-source for the server password (PASS).
  (let (erc-auth-source-server-function)
    (erc-scenarios-common--services-auth-source
     "machine 127.0.0.1 port %d user tester password spam"
     "machine zirconium.libera.chat port %d user tester password fake"
     "machine Libera.Chat port %d user tester password changeme")))

(ert-deftest erc-scenarios-services-auth-source--network-connect-lookup ()
  :tags '(:expensive-test)
  ;; Do consult auth-source for the server password (and find nothing)
  (erc-scenarios-common--services-auth-source
   "machine zirconium.libera.chat port %d user tester password fake"
   "machine Libera.Chat port %d user tester password changeme"))

(ert-deftest erc-scenarios-services-auth-source--announced ()
  :tags '(:expensive-test)
  (let (erc-auth-source-server-function)
    (erc-scenarios-common--services-auth-source
     "machine 127.0.0.1 port %d user tester password spam"
     "machine zirconium.libera.chat port %d user tester password changeme")))

(ert-deftest erc-scenarios-services-auth-source--dialed ()
  :tags '(:expensive-test)
  ;; Support legacy host -> domain name
  ;; (likely most common in real configs)
  (let (erc-auth-source-server-function)
    (erc-scenarios-common--services-auth-source
     "machine 127.0.0.1 port %d user tester password changeme")))

(ert-deftest erc-scenarios-services-auth-source--custom ()
  :tags '(:expensive-test)
  (let (erc-auth-source-server-function
        (erc-auth-source-services-function
         (lambda (&rest _) (erc-auth-source-search :host "MyAccount"))))
    (erc-scenarios-common--services-auth-source
     "machine zirconium.libera.chat port %d user tester password spam"
     "machine MyAccount port %d user tester password changeme"
     "machine 127.0.0.1 port %d user tester password fake")))

;;; erc-scenarios-auth-source.el ends here
