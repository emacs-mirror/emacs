;;; erc-scenarios-sasl.el --- SASL tests for ERC -*- lexical-binding: t -*-

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

;;; Code:

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(require 'erc-sasl)

(ert-deftest erc-scenarios-sasl--plain ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "sasl")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'plain))
       (port (process-contact dumb-server :service))
       (erc-modules (cons 'sasl erc-modules))
       (erc-sasl-password "password123")
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :user "tester"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (ert-info ("Notices received")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "ExampleOrg"))
        (funcall expect 10 "This server is in debug mode")
        ;; Regression "\0\0\0\0 ..." caused by (fillarray passphrase 0)
        (should (string= erc-sasl-password "password123"))))))

;; The user's unreasonably long password is apportioned into chunks on
;; the way out the door.

(ert-deftest erc-scenarios-sasl--plain-overlong-split ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "sasl")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'plain-overlong-split))
       (port (process-contact dumb-server :service))
       (erc-modules (cons 'sasl erc-modules))
       (erc-sasl-password
        (concat
         "Est ut beatae omnis ipsam. "
         "Quis fugiat deleniti totam qui. "
         "Ipsum quam a dolorum tempora velit laborum odit. "
         "Et saepe voluptate sed cumque vel. "
         "Voluptas sint ab pariatur libero veritatis corrupti. "
         "Vero iure omnis ullam. "
         "Vero beatae dolores facere fugiat ipsam. "
         "Ea est pariatur minima nobis sunt aut ut. "
         "Dolores ut laudantium maiores temporibus voluptates. "
         "Reiciendis impedit omnis et unde delectus quas ab. "
         "Quae eligendi necessitatibus doloribus "
         "molestias tempora magnam assumenda."))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "emersion"
                                :user "emersion"
                                :full-name "emersion")
        (funcall expect 10 "This server is in debug mode")
        (erc-cmd-QUIT "")))))

(ert-deftest erc-scenarios-sasl--plain-overlong-aligned ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "sasl")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'plain-overlong-aligned))
       (port (process-contact dumb-server :service))
       (erc-modules (cons 'sasl erc-modules))
       (erc-sasl-password
        (concat
         "Est ut beatae omnis ipsam. "
         "Quis fugiat deleniti totam qui. "
         "Ipsum quam a dolorum tempora velit laborum odit. "
         "Et saepe voluptate sed cumque vel. "
         "Voluptas sint ab pariatur libero veritatis corrupti. "
         "Vero iure omnis ullam. Vero beatae dolores facere fugiat ipsam. "
         "Ea est pariatur minima nobis"))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "emersion"
                                :user "emersion"
                                :full-name "emersion")
        (funcall expect 10 "This server is in debug mode")
        (erc-cmd-QUIT "")))))

(ert-deftest erc-scenarios-sasl--external ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "sasl")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'external))
       (port (process-contact dumb-server :service))
       (erc-modules (cons 'sasl erc-modules))
       (erc-sasl-mechanism 'external)
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :user "tester"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (ert-info ("Notices received")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "ExampleOrg"))
        (funcall expect 10 "Authentication successful")
        (funcall expect 10 "This server is in debug mode")))))

(ert-deftest erc-scenarios-sasl--plain-fail ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "sasl")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'plain-failed))
       (port (process-contact dumb-server :service))
       (erc-modules (cons 'sasl erc-modules))
       (erc-sasl-password "wrong")
       (erc-sasl-mechanism 'plain)
       (erc--warnings-buffer-name  "*ERC test warnings*")
       (warnings-buffer (get-buffer-create erc--warnings-buffer-name))
       (inhibit-message noninteractive)
       (expect (erc-d-t-make-expecter)))

    (with-current-buffer (erc :server "127.0.0.1"
                              :port port
                              :nick "tester"
                              :user "tester"
                              :full-name "tester")
      (funcall expect 10 "Opening connection")
      (funcall expect 20 "SASL authentication failed")
      (funcall expect 20 "Connection failed!")
      (should-not (erc-server-process-alive)))

    (with-current-buffer warnings-buffer
      (funcall expect 10 "please review SASL settings")))

  (when noninteractive
    (should-not (get-buffer "*ERC test warnings*"))))

(defun erc-scenarios--common--sasl (mech)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "sasl")
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t mech))
       (port (process-contact dumb-server :service))
       (erc-modules (cons 'sasl erc-modules))
       (erc-sasl-user :nick)
       (erc-sasl-mechanism mech)
       (mock-rvs (list "c5RqLCZy0L4fGkKAZ0hujFBs" ""))
       (sasl-unique-id-function (lambda () (pop mock-rvs)))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "jilles"
                                :password "sesame"
                                :full-name "jilles")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (ert-info ("Notices received")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "jaguar"))
        (funcall expect 10 "Found your hostname")
        (funcall expect 20 "marked as being away")))))

(ert-deftest erc-scenarios-sasl--scram-sha-1 ()
  :tags '(:expensive-test)
  (let ((erc-sasl-authzid "jilles"))
    (erc-scenarios--common--sasl 'scram-sha-1)))

(ert-deftest erc-scenarios-sasl--scram-sha-256 ()
  :tags '(:expensive-test)
  (unless (featurep 'sasl-scram-sha256)
    (ert-skip "Emacs lacks sasl-scram-sha256"))
  (erc-scenarios--common--sasl 'scram-sha-256))

;;; erc-scenarios-sasl.el ends here
