;;; em-tramp-tests.el --- em-tramp test suite  -*- lexical-binding:t -*-

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

(require 'ert)
(require 'em-tramp)
(require 'tramp)

(defmacro em-tramp-test/should-replace-command (form replacement)
  "Check that calling FORM results in it being replaced with REPLACEMENT."
  (declare (indent 1))
  `(should (equal
            (catch 'eshell-replace-command ,form)
            (list 'eshell-with-copied-handles
                  (list 'eshell-do-command ,replacement)))))

(ert-deftest em-tramp-test/su-default ()
  "Test Eshell `su' command with no arguments."
  (em-tramp-test/should-replace-command (eshell/su)
    `(eshell-named-command
      "cd"
      (list ,(format "/su:root@%s:%s"
                     tramp-default-host default-directory)))))

(ert-deftest em-tramp-test/su-user ()
  "Test Eshell `su' command with USER argument."
  (em-tramp-test/should-replace-command (eshell/su "USER")
    `(eshell-named-command
      "cd"
      (list ,(format "/su:USER@%s:%s"
                     tramp-default-host default-directory)))))

(ert-deftest em-tramp-test/su-login ()
  "Test Eshell `su' command with -/-l/--login option."
  (dolist (args '(("--login")
                  ("-l")
                  ("-")))
    (em-tramp-test/should-replace-command (apply #'eshell/su args)
      `(eshell-named-command
        "cd"
        (list ,(format "/su:root@%s:~/" tramp-default-host))))))

(ert-deftest em-tramp-test/sudo-basic ()
  "Test Eshell `sudo' command with default user."
  (let ((sudo-directory (format "/sudo:root@%s:%s"
                                tramp-default-host default-directory)))
    (should (equal (catch 'eshell-replace-command
                     (eshell/sudo "echo" "hi"))
                   `(let ((default-directory ,sudo-directory))
                      (eshell-named-command '"echo" '("hi")))))
    (should (equal (catch 'eshell-replace-command
                     (eshell/sudo "echo" "-u" "hi"))
                   `(let ((default-directory ,sudo-directory))
                      (eshell-named-command '"echo" '("-u" "hi")))))))

(ert-deftest em-tramp-test/sudo-user ()
  "Test Eshell `sudo' command with specified user."
  (let ((sudo-directory (format "/sudo:USER@%s:%s"
                                tramp-default-host default-directory)))
    (should (equal (catch 'eshell-replace-command
                     (eshell/sudo "-u" "USER" "echo" "hi"))
                   `(let ((default-directory ,sudo-directory))
                      (eshell-named-command '"echo" '("hi")))))
    (should (equal (catch 'eshell-replace-command
                     (eshell/sudo "-u" "USER" "echo" "-u" "hi"))
                   `(let ((default-directory ,sudo-directory))
                      (eshell-named-command '"echo" '("-u" "hi")))))))

(ert-deftest em-tramp-test/sudo-shell ()
  "Test Eshell `sudo' command with -s/--shell option."
  (dolist (args '(("--shell")
                  ("-s")))
    (em-tramp-test/should-replace-command (apply #'eshell/sudo args)
      `(eshell-named-command
        "cd"
        (list ,(format "/sudo:root@%s:%s"
                       tramp-default-host default-directory))))))

(ert-deftest em-tramp-test/sudo-user-shell ()
  "Test Eshell `sudo' command with -s and -u options."
  (em-tramp-test/should-replace-command (eshell/sudo "-u" "USER" "-s")
    `(eshell-named-command
      "cd"
      (list ,(format "/sudo:USER@%s:%s"
                     tramp-default-host default-directory)))))

(ert-deftest em-tramp-test/doas-basic ()
  "Test Eshell `doas' command with default user."
  (let ((doas-directory (format "/doas:root@%s:%s"
                                tramp-default-host default-directory)))
    (should (equal (catch 'eshell-replace-command
                     (eshell/doas "echo" "hi"))
                   `(let ((default-directory ,doas-directory))
                      (eshell-named-command '"echo" '("hi")))))
    (should (equal (catch 'eshell-replace-command
                     (eshell/doas "echo" "-u" "hi"))
                   `(let ((default-directory ,doas-directory))
                      (eshell-named-command '"echo" '("-u" "hi")))))))

(ert-deftest em-tramp-test/doas-user ()
  "Test Eshell `doas' command with specified user."
  (let ((doas-directory (format "/doas:USER@%s:%s"
                                tramp-default-host default-directory)))
    (should (equal (catch 'eshell-replace-command
                     (eshell/doas "-u" "USER" "echo" "hi"))
                   `(let ((default-directory ,doas-directory))
                      (eshell-named-command '"echo" '("hi")))))
    (should (equal (catch 'eshell-replace-command
                     (eshell/doas "-u" "USER" "echo" "-u" "hi"))
                   `(let ((default-directory ,doas-directory))
                      (eshell-named-command '"echo" '("-u" "hi")))))))

(ert-deftest em-tramp-test/doas-shell ()
  "Test Eshell `doas' command with -s/--shell option."
  (dolist (args '(("--shell")
                  ("-s")))
    (em-tramp-test/should-replace-command (apply #'eshell/doas args)
      `(eshell-named-command
        "cd"
        (list ,(format "/doas:root@%s:%s"
                       tramp-default-host default-directory))))))

(ert-deftest em-tramp-test/doas-user-shell ()
  "Test Eshell `doas' command with -s and -u options."
  (em-tramp-test/should-replace-command (eshell/doas "-u" "USER" "-s")
    `(eshell-named-command
      "cd"
      (list ,(format "/doas:USER@%s:%s"
                     tramp-default-host default-directory)))))

;;; em-tramp-tests.el ends here
