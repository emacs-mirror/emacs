;;; message-mode-tests.el --- Tests for message-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2020 Free Software Foundation, Inc.

;; Author: João Távora <joaotavora@gmail.com>

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

;; This file contains tests for message-mode.

;;; Code:

(require 'message)
(require 'ert)
(require 'ert-x)

(require 'cl-lib)

(ert-deftest message-mode-propertize ()
  (with-temp-buffer
    (unwind-protect
        (let (message-auto-save-directory)
          (message-mode)
          (insert "here's an opener (\n"
                  "here's a sad face :-(\n"
                  "> here's citing someone with an opener (\n"
                  "and here's a closer ")
          (let ((last-command-event ?\)))
            (ert-simulate-command '(self-insert-command 1)))
          ;; Auto syntax propertization doesn't kick in until
          ;; parse-sexp-lookup-properties is set.
          (setq-local parse-sexp-lookup-properties t)
          (backward-sexp)
          (should (string= "here's an opener "
                           (buffer-substring-no-properties
                            (line-beginning-position)
                            (point))))
          (forward-sexp)
          (should (string= "and here's a closer )"
                           (buffer-substring-no-properties
                            (line-beginning-position)
                            (point)))))
      (set-buffer-modified-p nil))))


(ert-deftest message-strip-subject-trailing-was ()
  (cl-letf (((symbol-function 'message-talkative-question) nil))
    (with-temp-buffer
      (let ((no-was "Re: Foo ")
            (with-was "Re: Foo \t (was: Bar ) ")
            (stripped-was "Re: Foo")
            reply)

        ;; Test unconditional stripping
        (setq-local message-subject-trailing-was-query t)
        (should (string= no-was (message-strip-subject-trailing-was no-was)))
        (should (string= stripped-was
                         (message-strip-subject-trailing-was with-was)))

        ;; Test asking
        (setq-local message-subject-trailing-was-query 'ask)
        (fset 'message-talkative-question
              (lambda (_ question show text)
                (should (string= "Strip `(was: <old subject>)' in subject? "
                                 question))
                (should show)
                (should (string-match
                         (concat
                          "Strip `(was: <old subject>)' in subject "
                          "and use the new one instead\\?\n\n"
                          "Current subject is:   \"\\(.*\\)\"\n\n"
                          "New subject would be: \"\\(.*\\)\"\n\n"
                          "See the variable "
                          "`message-subject-trailing-was-query' "
                          "to get rid of this query.")
                         text))
                (should (string= (match-string 1 text) with-was))
                (should (string= (match-string 2 text) stripped-was))
                reply))
        (message-strip-subject-trailing-was with-was)
        (should (string= with-was
                         (message-strip-subject-trailing-was with-was)))
        (setq reply t)
        (should (string= stripped-was
                         (message-strip-subject-trailing-was with-was)))))))

(ert-deftest message-all-recipients ()
  (ert-with-test-buffer (:name "message")
    (insert "To: Person 1 <p1@p1.org>, Person 2 <p2@p2.org>\n")
    (insert "Cc: Person 3 <p3@p3.org>, Person 4 <p4@p4.org>\n")
    (insert "Bcc: Person 5 <p5@p5.org>, Person 6 <p6@p6.org>\n")
    (should (equal (message-all-recipients)
                   '(("Person 1" "p1@p1.org")
                     ("Person 2" "p2@p2.org")
                     ("Person 3" "p3@p3.org")
                     ("Person 4" "p4@p4.org")
                     ("Person 5" "p5@p5.org")
                     ("Person 6" "p6@p6.org"))))))

(ert-deftest message-all-epg-keys-available-p ()
  (skip-unless (epg-check-configuration (epg-find-configuration 'OpenPGP)))
  (let ((person1 '("Person 1" "p1@p1.org"))
        (person2 '("Person 2" "p2@p2.org"))
        (person3 '("Person 3" "p3@p3.org"))
        (recipients nil)
        (keyring '("p1@p1.org" "p2@p2.org")))
    (cl-letf (((symbol-function 'epg-list-keys)
               (lambda (_ email) (cl-find email keyring :test #'string=)))
              ((symbol-function 'message-all-recipients)
               (lambda () recipients)))

      (setq recipients (list))
      (should (message-all-epg-keys-available-p))

      (setq recipients (list person1))
      (should (message-all-epg-keys-available-p))

      (setq recipients (list person1 person2))
      (should (message-all-epg-keys-available-p))

      (setq recipients (list person3))
      (should-not (message-all-epg-keys-available-p))

      (setq recipients (list person1 person3))
      (should-not (message-all-epg-keys-available-p))

      (setq recipients (list person3 person1))
      (should-not (message-all-epg-keys-available-p))

      (setq recipients (list person1 person2 person3))
      (should-not (message-all-epg-keys-available-p)))))

(ert-deftest message-alter-repeat-address ()
  (should (equal (message--alter-repeat-address
                  "Lars Ingebrigtsen <larsi@gnus.org>")
                 "Lars Ingebrigtsen <larsi@gnus.org>"))

  (should (equal (message--alter-repeat-address
                      "\"larsi@gnus.org\" <larsi@gnus.org>")
                     "larsi@gnus.org")))

(provide 'message-mode-tests)

;;; message-mode-tests.el ends here
