;;; erc-match-tests.el --- Tests for erc-match.  -*- lexical-binding:t -*-

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

;;; Commentary:
;;; Code:

(require 'ert-x)
(require 'erc-match)


(ert-deftest erc-add-entry-to-list ()
  (let ((erc-pals '("z"))
        (erc-match-quote-when-adding 'ask))

    (ert-info ("Default (ask)")
      (ert-simulate-keys "\t\ry\r"
        (erc-add-entry-to-list 'erc-pals "?" '((".")) nil)
        (should (equal (pop erc-pals) "\\.")))

      (ert-info ("Inverted")
        (ert-simulate-keys "\t\ry\r"
          (erc-add-entry-to-list 'erc-pals "?" '((".")) nil)
          (should (equal (pop erc-pals) "\\."))))

      (ert-info ("Skipped")
        (ert-simulate-keys "\t\r"
          (erc-add-entry-to-list 'erc-pals "?" '(("x")) nil)
          (should (equal (pop erc-pals) "x")))))

    (ert-info ("Verbatim")
      (setq erc-match-quote-when-adding nil)
      (ert-simulate-keys "\t\r"
        (erc-add-entry-to-list 'erc-pals "?" '((".")) nil)
        (should (equal (pop erc-pals) ".")))

      (ert-info ("Inverted")
        (ert-simulate-keys "\t\r"
          (erc-add-entry-to-list 'erc-pals "?" '((".")) t)
          (should (equal (pop erc-pals) "\\.")))))

    (ert-info ("Quoted")
      (setq erc-match-quote-when-adding t)
      (ert-simulate-keys "\t\r"
        (erc-add-entry-to-list 'erc-pals "?" '((".")) nil)
        (should (equal (pop erc-pals) "\\.")))

      (ert-info ("Inverted")
        (ert-simulate-keys "\t\r"
          (erc-add-entry-to-list 'erc-pals "?" '((".")) t)
          (should (equal (pop erc-pals) ".")))))

    (should (equal erc-pals '("z")))))

(ert-deftest erc-pals ()
  (with-temp-buffer
    (setq erc-server-process (start-process "true" (current-buffer) "true")
          erc-server-users (make-hash-table :test #'equal))
    (set-process-query-on-exit-flag erc-server-process nil)
    (erc-add-server-user "FOO[m]" (make-erc-server-user :nickname "foo[m]"))
    (erc-add-server-user "tester" (make-erc-server-user :nickname "tester"))

    (let ((erc-match-quote-when-adding t)
          erc-pals calls rvs)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest r) (push r calls) (pop rvs))))

        (ert-info ("`erc-add-pal'")
          (push "foo[m]" rvs)
          (ert-simulate-command '(erc-add-pal))
          (should (equal (cadr (pop calls)) '(("tester") ("foo[m]"))))
          (should (equal erc-pals '("foo\\[m]"))))

        (ert-info ("`erc-match-pal-p'")
          (should (erc-match-pal-p "FOO[m]!~u@example.net" nil)))

        (ert-info ("`erc-delete-pal'")
          (push "foo\\[m]" rvs)
          (ert-simulate-command '(erc-delete-pal))
          (should (equal (cadr (pop calls)) '(("foo\\[m]"))))
          (should-not erc-pals))

        (ert-info ("`erc-add-pal' verbatim")
          (push "foo[m]" rvs)
          (ert-simulate-command '(erc-add-pal (4)))
          (should (equal (cadr (pop calls)) '(("tester") ("foo[m]"))))
          (should (equal erc-pals '("foo[m]"))))))))

(ert-deftest erc-fools ()
  (with-temp-buffer
    (setq erc-server-process (start-process "true" (current-buffer) "true")
          erc-server-users (make-hash-table :test #'equal))
    (set-process-query-on-exit-flag erc-server-process nil)
    (erc-add-server-user "FOO[m]" (make-erc-server-user :nickname "foo[m]"))
    (erc-add-server-user "tester" (make-erc-server-user :nickname "tester"))

    (let ((erc-match-quote-when-adding t)
          erc-fools calls rvs)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest r) (push r calls) (pop rvs))))

        (ert-info ("`erc-add-fool'")
          (push "foo[m]" rvs)
          (ert-simulate-command '(erc-add-fool))
          (should (equal (cadr (pop calls)) '(("tester") ("foo[m]"))))
          (should (equal erc-fools '("foo\\[m]"))))

        (ert-info ("`erc-match-fool-p'")
          (should (erc-match-fool-p "FOO[m]!~u@example.net" ""))
          (should (erc-match-fool-p "tester!~u@example.net" "FOO[m]: die")))

        (ert-info ("`erc-delete-fool'")
          (push "foo\\[m]" rvs)
          (ert-simulate-command '(erc-delete-fool))
          (should (equal (cadr (pop calls)) '(("foo\\[m]"))))
          (should-not erc-fools))

        (ert-info ("`erc-add-fool' verbatim")
          (push "foo[m]" rvs)
          (ert-simulate-command '(erc-add-fool (4)))
          (should (equal (cadr (pop calls)) '(("tester") ("foo[m]"))))
          (should (equal erc-fools '("foo[m]"))))))))

(ert-deftest erc-keywords ()
  (let ((erc-match-quote-when-adding t)
        erc-keywords calls rvs)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest r) (push r calls) (pop rvs))))

      (ert-info ("`erc-add-keyword'")
        (push "[cit. needed]" rvs)
        (ert-simulate-command '(erc-add-keyword))
        (should (equal (cadr (pop calls)) nil))
        (should (equal erc-keywords '("\\[cit\\. needed]"))))

      (ert-info ("`erc-match-keyword-p'")
        (should (erc-match-keyword-p nil "is pretty [cit. needed]")))

      (ert-info ("`erc-delete-keyword'")
        (push "\\[cit\\. needed]" rvs)
        (ert-simulate-command '(erc-delete-keyword))
        (should (equal (cadr (pop calls)) '(("\\[cit\\. needed]"))))
        (should-not erc-keywords))

      (ert-info ("`erc-add-keyword' verbatim")
        (push "[...]" rvs)
        (ert-simulate-command '(erc-add-keyword (4)))
        (should (equal (cadr (pop calls)) nil))
        (should (equal erc-keywords '("[...]")))))))

(ert-deftest erc-dangerous-hosts ()
  (let ((erc-match-quote-when-adding t)
        erc-dangerous-hosts calls rvs)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest r) (push r calls) (pop rvs))))

      (ert-info ("`erc-add-dangerous-host'")
        (push "example.net" rvs)
        (ert-simulate-command '(erc-add-dangerous-host))
        (should (equal (cadr (pop calls)) nil))
        (should (equal erc-dangerous-hosts '("example\\.net"))))

      (ert-info ("`erc-match-dangerous-host-p'")
        (should (erc-match-dangerous-host-p "FOO[m]!~u@example.net" nil)))

      (ert-info ("`erc-delete-dangerous-host'")
        (push "example\\.net" rvs)
        (ert-simulate-command '(erc-delete-dangerous-host))
        (should (equal (cadr (pop calls)) '(("example\\.net"))))
        (should-not erc-dangerous-hosts))

      (ert-info ("`erc-add-dangerous-host' verbatim")
        (push "example.net" rvs)
        (ert-simulate-command '(erc-add-dangerous-host (4)))
        (should (equal (cadr (pop calls)) nil))
        (should (equal erc-dangerous-hosts '("example.net")))))))

;;; erc-match-tests.el ends here
