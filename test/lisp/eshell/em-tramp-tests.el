;;; em-tramp-tests.el --- em-tramp test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

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

(ert-deftest em-tramp-test/su-default ()
  "Test Eshell `su' command with no arguments."
  (should (equal
           (catch 'eshell-replace-command (eshell/su))
           `(eshell-trap-errors
             (eshell-named-command
              "cd"
              (list ,(format "/su:root@%s:%s"
                             tramp-default-host default-directory)))))))

(ert-deftest em-tramp-test/su-user ()
  "Test Eshell `su' command with USER argument."
  (should (equal
           (catch 'eshell-replace-command (eshell/su "USER"))
           `(eshell-trap-errors
             (eshell-named-command
              "cd"
              (list ,(format "/su:USER@%s:%s"
                             tramp-default-host default-directory)))))))

(ert-deftest em-tramp-test/su-login ()
  "Test Eshell `su' command with -/-l/--login option."
  (dolist (args '(("--login")
                  ("-l")
                  ("-")))
    (should (equal
             (catch 'eshell-replace-command (apply #'eshell/su args))
             `(eshell-trap-errors
               (eshell-named-command
                "cd"
                (list ,(format "/su:root@%s:~/" tramp-default-host))))))))

(defun mock-eshell-named-command (&rest args)
  "Dummy function to test Eshell `sudo' command rewriting."
  (list default-directory args))

(ert-deftest em-tramp-test/sudo-basic ()
  "Test Eshell `sudo' command with default user."
  (cl-letf (((symbol-function 'eshell-named-command)
             #'mock-eshell-named-command))
    (should (equal
             (catch 'eshell-external (eshell/sudo "echo" "hi"))
             `(,(format "/sudo:root@%s:%s" tramp-default-host default-directory)
               ("echo" ("hi")))))
    (should (equal
             (catch 'eshell-external (eshell/sudo "echo" "-u" "hi"))
             `(,(format "/sudo:root@%s:%s" tramp-default-host default-directory)
               ("echo" ("-u" "hi")))))))

(ert-deftest em-tramp-test/sudo-user ()
  "Test Eshell `sudo' command with specified user."
  (cl-letf (((symbol-function 'eshell-named-command)
             #'mock-eshell-named-command))
    (should (equal
             (catch 'eshell-external (eshell/sudo "-u" "USER" "echo" "hi"))
             `(,(format "/sudo:USER@%s:%s" tramp-default-host default-directory)
               ("echo" ("hi")))))
    (should (equal
             (catch 'eshell-external (eshell/sudo "-u" "USER" "echo" "-u" "hi"))
             `(,(format "/sudo:USER@%s:%s" tramp-default-host default-directory)
               ("echo" ("-u" "hi")))))))

(ert-deftest em-tramp-test/sudo-shell ()
  "Test Eshell `sudo' command with -s/--shell option."
  (dolist (args '(("--shell")
                  ("-s")))
    (should (equal
             (catch 'eshell-replace-command (apply #'eshell/sudo args))
             `(eshell-trap-errors
               (eshell-named-command
                "cd"
                (list ,(format "/sudo:root@%s:%s"
                               tramp-default-host default-directory))))))))

(ert-deftest em-tramp-test/sudo-user-shell ()
  "Test Eshell `sudo' command with -s and -u options."
  (should (equal
           (catch 'eshell-replace-command (eshell/sudo "-u" "USER" "-s"))
           `(eshell-trap-errors
             (eshell-named-command
              "cd"
              (list ,(format "/sudo:USER@%s:%s"
                             tramp-default-host default-directory)))))))

(ert-deftest em-tramp-test/doas-basic ()
  "Test Eshell `doas' command with default user."
  (cl-letf (((symbol-function 'eshell-named-command)
             #'mock-eshell-named-command))
    (should (equal
             (catch 'eshell-external (eshell/doas "echo" "hi"))
             `(,(format "/doas:root@%s:%s"
                        tramp-default-host default-directory)
               ("echo" ("hi")))))
    (should (equal
             (catch 'eshell-external (eshell/doas "echo" "-u" "hi"))
             `(,(format "/doas:root@%s:%s"
                        tramp-default-host default-directory)
               ("echo" ("-u" "hi")))))))

(ert-deftest em-tramp-test/doas-user ()
  "Test Eshell `doas' command with specified user."
  (cl-letf (((symbol-function 'eshell-named-command)
             #'mock-eshell-named-command))
    (should (equal
             (catch 'eshell-external (eshell/doas "-u" "USER" "echo" "hi"))
             `(,(format "/doas:USER@%s:%s"
                        tramp-default-host default-directory)
               ("echo" ("hi")))))
    (should (equal
             (catch 'eshell-external
               (eshell/doas "-u" "USER" "echo" "-u" "hi"))
             `(,(format "/doas:USER@%s:%s"
                        tramp-default-host default-directory)
               ("echo" ("-u" "hi")))))))

(ert-deftest em-tramp-test/doas-shell ()
  "Test Eshell `doas' command with -s/--shell option."
  (dolist (args '(("--shell")
                  ("-s")))
    (should (equal
             (catch 'eshell-replace-command (apply #'eshell/doas args))
             `(eshell-trap-errors
               (eshell-named-command
                "cd"
                (list ,(format "/doas:root@%s:%s"
                               tramp-default-host default-directory))))))))

(ert-deftest em-tramp-test/doas-user-shell ()
  "Test Eshell `doas' command with -s and -u options."
  (should (equal
           (catch 'eshell-replace-command (eshell/doas "-u" "USER" "-s"))
           `(eshell-trap-errors
             (eshell-named-command
              "cd"
              (list ,(format "/doas:USER@%s:%s"
                             tramp-default-host default-directory)))))))

;;; em-tramp-tests.el ends here
