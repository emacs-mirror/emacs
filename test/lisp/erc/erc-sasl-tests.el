;;; erc-sasl-tests.el --- Tests for erc-sasl.  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert-x)
(require 'erc-sasl)

(ert-deftest erc-sasl--mechanism-offered-p ()
  (let ((erc-sasl--options '((mechanism . external))))
    (should (erc-sasl--mechanism-offered-p "foo,external"))
    (should (erc-sasl--mechanism-offered-p "external,bar"))
    (should (erc-sasl--mechanism-offered-p "foo,external,bar"))
    (should-not (erc-sasl--mechanism-offered-p "fooexternal"))
    (should-not (erc-sasl--mechanism-offered-p "externalbar"))))

(ert-deftest erc-sasl--read-password--basic ()
  (ert-info ("Explicit erc-sasl-password")
    (let ((erc-sasl--options '((password . "foo"))))
      (should (string= (erc-sasl--read-password nil) "foo"))))

  (ert-info ("Explicit session password")
    (let ((erc-session-password "foo")
          (erc-sasl--options '((password . :password))))
      (should (string= (erc-sasl--read-password nil) "foo"))))

  (ert-info ("Prompt when no authfn and :password resolves to nil")
    (let ((erc-session-password nil)
          (inhibit-message noninteractive)
          (erc-sasl--options
           '((password . :password) (user . :user) (authfn))))
      (should (string= (ert-simulate-keys "bar\r"
                         (erc-sasl--read-password "?"))
                       "bar"))))

  (ert-info ("Prompt when auth-source fails and `erc-session-password' null")
    (should-not erc-session-password)
    (let ((inhibit-message noninteractive)
          (erc-sasl--options '((password) (authfn . ignore))))
      (should (string= (ert-simulate-keys "baz\r"
                         (erc-sasl--read-password "pwd:"))
                       "baz")))))

;; This mainly tests `erc-sasl-auth-source-password-as-host'.

(ert-deftest erc-sasl--read-password--auth-source ()
  (ert-with-temp-file netrc-file
    :text (string-join
           (list
            ;; If you swap these first 2 lines, *1 below fails
            "machine FSF.chat port 6697 user bob password sesame"
            "machine GNU/chat port 6697 user bob password spam"
            "machine MyHost port irc password 123")
           "\n")
    (let* ((auth-sources (list netrc-file))
           (erc-session-server "irc.gnu.org")
           (erc-session-port 6697)
           (erc-networks--id (erc-networks--id-create nil))
           erc-server-announced-name ; too early
           auth-source-do-cache
           ;;
           (fn #'erc-sasl-auth-source-password-as-host)
           calls)

      (advice-add 'erc-auth-source-search :before
                  (lambda (&rest r) (push r calls))
                  '((name . erc-sasl--read-password--auth-source)))

      (ert-info ("Symbol as password specifies machine")
        (let ((erc-sasl--options
               `((user . "bob") (password . FSF.chat) (authfn . ,fn))))
          (should (string= (erc-sasl--read-password nil) "sesame"))
          (should (equal (pop calls) '(:user "bob" :host "FSF.chat")))))

      (ert-info (":password as password resolved to machine")
        (let ((erc-session-password "FSF.chat")
              (erc-sasl--options
               `((user . "bob") (password . :password) (authfn . ,fn))))
          (should (string= (erc-sasl--read-password nil) "sesame"))
          (should (equal (pop calls) '(:user "bob" :host "FSF.chat")))))

      (ert-info (":user resolved to `erc-session-username'") ; *1
        (let ((erc-session-username "bob")
              (erc-sasl--options `((user . :user) (password) (authfn . ,fn)))
              (erc-networks--id (erc-networks--id-create 'GNU/chat)))
          (should (string= (erc-sasl--read-password nil) "spam"))
          (should (equal (pop calls) '(:user "bob")))))

      (ert-info (":user resolved to current nick") ; *1
        (let ((erc-server-current-nick "bob")
              (erc-sasl--options `((user . :nick) (password) (authfn . ,fn)))
              (erc-networks--id (erc-networks--id-create 'GNU/chat)))
          (should (string= (erc-sasl--read-password nil) "spam"))
          (should (equal (pop calls) '(:user "bob")))))

      (ert-info ("Symbol as password, entry lacks user field")
        (let ((erc-server-current-nick "fake")
              (erc-sasl--options
               `((user . :nick) (password . MyHost) (authfn . ,fn)))
              (erc-networks--id (erc-networks--id-create 'GNU/chat)))
          (should (string= (erc-sasl--read-password nil) "123"))
          (should (equal (pop calls) '(:user "fake" :host "MyHost")))))

      (advice-remove 'erc-auth-source-search
                     'erc-sasl--read-password--auth-source))))

(ert-deftest erc-sasl-create-client--plain ()
  (let* ((erc-session-password "password123")
         (erc-session-username "tester")
         (erc-sasl--options '((user . :user) (password . :password)))
         (erc-session-port 1667)
         (erc-session-server "localhost")
         (client (erc-sasl--create-client 'plain))
         (result (sasl-next-step client nil)))
    (should (equal (format "%S" [erc-sasl--plain-response
                                 "\0tester\0password123"])
                   (format "%S" result)))
    (should (string= (sasl-step-data result) "\0tester\0password123"))
    (should-not (sasl-next-step client result)))
  (should (equal (assoc-default "PLAIN" sasl-mechanism-alist) '(sasl-plain))))

(ert-deftest erc-sasl-create-client--external ()
  (let* ((erc-server-current-nick "tester")
         (erc-sasl--options '((user . :nick) (password . :password)))
         (client (erc-sasl--create-client 'external)) ; unused ^
         (result (sasl-next-step client nil)))
    (should (equal (format "%S" [ignore nil]) (format "%S" result)))
    (should-not (sasl-step-data result))
    (should-not (sasl-next-step client result)))
  (should-not (member "EXTERNAL" sasl-mechanisms))
  (should-not (assoc-default "EXTERNAL" sasl-mechanism-alist)))

(ert-deftest erc-sasl-create-client--scram-sha-1 ()
  (let* ((erc-sasl--options '((user . "jilles") (password . "sesame")
                              (authzid . "jilles")))
         (mock-rvs (list "c5RqLCZy0L4fGkKAZ0hujFBs" ""))
         (sasl-unique-id-function (lambda () (pop mock-rvs)))
         (client (erc-sasl--create-client 'scram-sha-1))
         (step (sasl-next-step client nil)))
    (ert-info ("Client's initial request")
      (let ((req "n,a=jilles,n=jilles,r=c5RqLCZy0L4fGkKAZ0hujFBs"))
        (should (equal (format "%S"
                               `[erc-compat--29-sasl-scram-client-first-message
                                 ,req])
                       (format "%S" step)))
        (should (string= (sasl-step-data step) req))))
    (ert-info ("Server's initial response")
      (let ((resp (concat "r=c5RqLCZy0L4fGkKAZ0hujFBsXQoKcivqCw9iDZPSpb,"
                          "s=5mJO6d4rjCnsBU1X,"
                          "i=4096"))
            (req (concat "c=bixhPWppbGxlcyw=,"
                         "r=c5RqLCZy0L4fGkKAZ0hujFBsXQoKcivqCw9iDZPSpb,"
                         "p=OVUhgPu8wEm2cDoVLfaHzVUYPWU=")))
        (sasl-step-set-data step resp)
        (setq step (sasl-next-step client step))
        (should (equal (format "%S"
                               `[erc-sasl--scram-sha-1-client-final-message
                                 ,req])
                       (format "%S" step)))
        (should (string= (sasl-step-data step) req))))
    (ert-info ("Server's final message")
      (let ((resp "v=ZWR23c9MJir0ZgfGf5jEtLOn6Ng="))
        (sasl-step-set-data step resp)
        (setq step (sasl-next-step client step))
        (should-not (sasl-step-data step)))))
  (should (eq sasl-unique-id-function #'sasl-unique-id-function)))

(ert-deftest erc-sasl-create-client--scram-sha-256 ()
  (unless (featurep 'sasl-scram-sha256)
    (ert-skip "Emacs lacks sasl-scram-sha256"))
  (let* ((erc-server-current-nick "jilles")
         (erc-session-password "sesame")
         (erc-sasl--options '((user . :nick) (password . :password)
                              (authzid . "jilles")))
         (mock-rvs (list "c5RqLCZy0L4fGkKAZ0hujFBs" ""))
         (sasl-unique-id-function (lambda () (pop mock-rvs)))
         (client (erc-sasl--create-client 'scram-sha-256))
         (step (sasl-next-step client nil)))
    (ert-info ("Client's initial request")
      (let ((req "n,a=jilles,n=jilles,r=c5RqLCZy0L4fGkKAZ0hujFBs"))
        (should (equal (format "%S"
                               `[erc-compat--29-sasl-scram-client-first-message
                                 ,req])
                       (format "%S" step)))
        (should (string= (sasl-step-data step) req))))
    (ert-info ("Server's initial response")
      (let ((resp (concat
                   "r=c5RqLCZy0L4fGkKAZ0hujFBse697140729d8445fb95ec94ceacb14b3,"
                   "s=MTk2M2VkMzM5ZmU0NDRiYmI0MzIyOGVhN2YwNzYwNmI=,"
                   "i=4096"))
            (req (concat
                  "c=bixhPWppbGxlcyw=,"
                  "r=c5RqLCZy0L4fGkKAZ0hujFBse697140729d8445fb95ec94ceacb14b3,"
                  "p=1vDesVBzJmv0lX0Ae1kHFtdVHkC6j4gISKVqaR45HFg=")))
        (sasl-step-set-data step resp)
        (setq step (sasl-next-step client step))
        (should (equal (format "%S"
                               `[erc-sasl--scram-sha-256-client-final-message
                                 ,req])
                       (format "%S" step)))
        (should (string= (sasl-step-data step) req))))
    (ert-info ("Server's final message")
      (let ((resp "v=gUePTYSZN9xgcE06KSyKO9fUmSwH26qifoapXyEs75s="))
        (sasl-step-set-data step resp)
        (setq step (sasl-next-step client step))
        (should-not (sasl-step-data step)))))
  (should (eq sasl-unique-id-function #'sasl-unique-id-function)))

(ert-deftest erc-sasl-create-client--scram-sha-256--no-authzid ()
  (unless (featurep 'sasl-scram-sha256)
    (ert-skip "Emacs lacks sasl-scram-sha256"))
  (let* ((erc-server-current-nick "jilles")
         (erc-session-password "sesame")
         (erc-sasl--options '((user . :nick) (password . :password) (authzid)))
         (mock-rvs (list "c5RqLCZy0L4fGkKAZ0hujFBs" ""))
         (sasl-unique-id-function (lambda () (pop mock-rvs)))
         (client (erc-sasl--create-client 'scram-sha-256))
         (step (sasl-next-step client nil)))
    (ert-info ("Client's initial request")
      (let ((req "n,,n=jilles,r=c5RqLCZy0L4fGkKAZ0hujFBs"))
        (should (equal (format "%S"
                               `[erc-compat--29-sasl-scram-client-first-message
                                 ,req])
                       (format "%S" step)))
        (should (string= (sasl-step-data step) req))))
    (ert-info ("Server's initial response")
      (let ((resp (concat
                   "r=c5RqLCZy0L4fGkKAZ0hujFBsd4067f0afdb54c3dbd4fe645b84cae37,"
                   "s=ZTg1MmE1YmFhZGI1NDcyMjk3NzYwZmRjZDM3Y2I1OTM=,"
                   "i=4096"))
            (req (concat
                  "c=biws,"
                  "r=c5RqLCZy0L4fGkKAZ0hujFBsd4067f0afdb54c3dbd4fe645b84cae37,"
                  "p=LP4sjJrjJKp5qTsARyZCppXpKLu4FMM284hNESPvGhI=")))
        (sasl-step-set-data step resp)
        (setq step (sasl-next-step client step))
        (should (equal (format "%S"
                               `[erc-sasl--scram-sha-256-client-final-message
                                 ,req])
                       (format "%S" step)))
        (should (string= (sasl-step-data step) req))))
    (ert-info ("Server's final message")
      (let ((resp "v=847WXfnmReGyE1qlq1And6R4bPBNROTZ7EMS/QrJtUM="))
        (sasl-step-set-data step resp)
        (setq step (sasl-next-step client step))
        (should-not (sasl-step-data step)))))
  (should (eq sasl-unique-id-function #'sasl-unique-id-function)))

(ert-deftest erc-sasl-create-client--scram-sha-512--no-authzid ()
  (unless (featurep 'sasl-scram-sha256)
    (ert-skip "Emacs lacks sasl-scram-sha512"))
  (let* ((erc-server-current-nick "jilles")
         (erc-session-password "sesame")
         (erc-sasl--options '((user . :nick) (password . :password) (authzid)))
         (mock-rvs (list "c5RqLCZy0L4fGkKAZ0hujFBs" ""))
         (sasl-unique-id-function (lambda () (pop mock-rvs)))
         (client (erc-sasl--create-client 'scram-sha-512))
         (step (sasl-next-step client nil)))
    (ert-info ("Client's initial request")
      (let ((req "n,,n=jilles,r=c5RqLCZy0L4fGkKAZ0hujFBs"))
        (should (equal (format "%S"
                               `[erc-compat--29-sasl-scram-client-first-message
                                 ,req])
                       (format "%S" step)))
        (should (string= (sasl-step-data step) req))))
    (ert-info ("Server's initial response")
      (let ((resp (concat
                   "r=c5RqLCZy0L4fGkKAZ0hujFBs54c592745ce14e559fcc3f27b15464f6,"
                   "s=YzMzOWZiY2U0YzcwNDA0M2I4ZGE2M2ZjOTBjODExZTM=,"
                   "i=4096"))
            (req (concat
                  "c=biws,"
                  "r=c5RqLCZy0L4fGkKAZ0hujFBs54c592745ce14e559fcc3f27b15464f6,"
                  "p=vMBb9tKxFAfBtel087/GLbo4objAIYr1wM+mFv/jYLKXE"
                  "NUF0vynm81qQbywQE5ScqFFdAfwYMZq/lj4s0V1OA==")))
        (sasl-step-set-data step resp)
        (setq step (sasl-next-step client step))
        (should (equal (format
                        "%S" `[erc-sasl--scram-sha-512-client-final-message
                               ,req])
                       (format "%S" step)))
        (should (string= (sasl-step-data step) req))))
    (ert-info ("Server's final message")
      (let ((resp (concat "v=Va7NIvt8wCdhvxnv+bZriSxGoto6On5EVnRHO/ece8zs0"
                          "qpQassdqir1Zlwh3e3EmBq+kcSy+ClNCsbzBpXe/w==")))
        (sasl-step-set-data step resp)
        (setq step (sasl-next-step client step))
        (should-not (sasl-step-data step)))))
  (should (eq sasl-unique-id-function #'sasl-unique-id-function)))

(defconst erc-sasl-tests-ecdsa-key-file "
-----BEGIN EC PARAMETERS-----
BggqhkjOPQMBBw==
-----END EC PARAMETERS-----
-----BEGIN EC PRIVATE KEY-----
MHcCAQEEIIJueQ3W2IrGbe9wKdOI75yGS7PYZSj6W4tg854hlsvmoAoGCCqGSM49
AwEHoUQDQgAEAZmaVhNSMmV5r8FXPvKuMnqDKyIA9pDHN5TNMfiF3mMeikGgK10W
IRX9cyi2wdYg9mUUYyh9GKdBCYHGUJAiCA==
-----END EC PRIVATE KEY-----
")

(ert-deftest erc-sasl-create-client-ecdsa ()
  :tags '(:unstable)
  ;; This is currently useless because it just roundtrips shelling out
  ;; to pkeyutl.
  (ert-skip "Placeholder for manual debugging")
  (unless (executable-find "openssl")
    (ert-skip "System lacks openssl"))

  (ert-with-temp-file keyfile
    :prefix "ecdsa_key"
    :suffix ".pem"
    :text erc-sasl-tests-ecdsa-key-file

    (erc-mode)
    (erc--initialize-markers (point) nil)
    (setq erc-server-process (make-process :name "sleep"
                                           :buffer (current-buffer)
                                           :command '("sleep" "1")
                                           :noquery t)
          erc-session-username "jilles")
    (let ((erc-sasl-mechanism 'ecdsa-nist256p-challenge)
          (erc-sasl-password keyfile))
      (erc-sasl-mode +1))

    (let* ((client (erc-sasl--state-client erc-sasl--state))
           (step (sasl-next-step client nil)))
      (ert-info ("Client's initial request")
        (should (equal (format "%S" [erc-sasl--ecdsa-first "jilles"])
                       (format "%S" step)))
        (should (string= (sasl-step-data step) "jilles")))
      (ert-info ("Server's initial response")
        (let ((resp (concat "\0\1\2\3\4\5\6\7\10\11\12\13\14\15\16\17\20"
                            "\21\22\23\24\25\26\27\30\31\32\33\34\35\36\37")))
          (sasl-step-set-data step resp)
          (setq step (sasl-next-step client step))
          (ert-with-temp-file sigfile
            :prefix "ecdsa_sig"
            :suffix ".sig"
            :text (sasl-step-data step)
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert resp)
              (let ((ec (call-process-region
                         (point-min) (point-max)
                         "openssl" 'delete t nil "pkeyutl"
                         "-inkey" keyfile "-sigfile" sigfile
                         "-verify")))
                (unless (zerop ec)
                  (message "%s" (buffer-string)))
                (should (zerop ec)))))))
      (should-not (sasl-next-step client step)))))

;;; erc-sasl-tests.el ends here
