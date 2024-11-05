;;; eudc-tests.el --- tests for eudc.el -*- lexical-binding: t -*-

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

(require 'eudc)

(ert-deftest eudc--plist-member ()
  "Test `eudc--plist-member' behavior."
  (dolist (obj '(a (a . a) (a a . a)))
    (should-error (eudc--plist-member obj nil) :type 'wrong-type-argument))
  (dolist (plist '((nil) (a) (a a a)))
    (let ((err `(wrong-type-argument plistp ,(copy-sequence plist))))
      (dolist (key '(nil a))
        (should (equal err (should-error (eudc--plist-member plist key)))))))
  (let ((-nil (string ?n ?i ?l))
        (-a (string ?a)))
    (should-not (eudc--plist-member () nil))
    (should-not (eudc--plist-member () 'a))
    (should-not (eudc--plist-member '(nil nil) 'a))
    (should-not (eudc--plist-member '(nil a) 'a))
    (should-not (eudc--plist-member '(a nil) nil))
    (should-not (eudc--plist-member '(a a) nil))
    (should-not (eudc--plist-member '("nil" a) nil))
    (should-not (eudc--plist-member '("nil" a) -nil))
    (should-not (eudc--plist-member '("a" a) nil))
    (should-not (eudc--plist-member '("a" a) -a))
    (should-not (eudc--plist-member '(nil a nil a) 'a))
    (should-not (eudc--plist-member '(nil a "a" a) -a))
    (should (equal (eudc--plist-member '(nil nil) nil) '(nil nil)))
    (should (equal (eudc--plist-member '(nil a) nil) '(nil a)))
    (should (equal (eudc--plist-member '(a nil) 'a) '(a nil)))
    (should (equal (eudc--plist-member '(a a) 'a) '(a a)))
    (should (equal (eudc--plist-member '(nil nil a nil) 'a) '(a nil)))
    (should (equal (eudc--plist-member '(nil a a a) 'a) '(a a)))
    (should (equal (eudc--plist-member '(a a a a) 'a) '(a a a a)))))

(ert-deftest eudc-plist-member ()
  "Test `eudc-plist-member' behavior."
  (dolist (obj '(a (a . a) (a a . a)))
    (should-error (eudc-plist-member obj nil) :type 'wrong-type-argument))
  (dolist (plist '((nil) (a) (a a a)))
    (let ((err `(wrong-type-argument plistp ,(copy-sequence plist))))
      (dolist (key '(nil a))
        (should (equal err (should-error (eudc-plist-member plist key)))))))
  (let ((-nil (string ?n ?i ?l))
        (-a (string ?a)))
    (should-not (eudc-plist-member () nil))
    (should-not (eudc-plist-member () 'a))
    (should-not (eudc-plist-member '(nil nil) 'a))
    (should-not (eudc-plist-member '(nil a) 'a))
    (should-not (eudc-plist-member '(a nil) nil))
    (should-not (eudc-plist-member '(a a) nil))
    (should-not (eudc-plist-member '("nil" a) nil))
    (should-not (eudc-plist-member '("nil" a) -nil))
    (should-not (eudc-plist-member '("a" a) nil))
    (should-not (eudc-plist-member '("a" a) -a))
    (should-not (eudc-plist-member '(nil a nil a) 'a))
    (should-not (eudc-plist-member '(nil a "a" a) -a))
    (should (eq t (eudc-plist-member '(nil nil) nil)))
    (should (eq t (eudc-plist-member '(nil a) nil)))
    (should (eq t (eudc-plist-member '(a nil) 'a)))
    (should (eq t (eudc-plist-member '(a a) 'a)))
    (should (eq t (eudc-plist-member '(nil nil a nil) 'a)))
    (should (eq t (eudc-plist-member '(nil a a a) 'a)))
    (should (eq t (eudc-plist-member '(a a a a) 'a)))))

(ert-deftest eudc-plist-get ()
  "Test `eudc-plist-get' behavior."
  (dolist (obj '(a (a . a) (a a . a)))
    (should-error (eudc-plist-get obj nil) :type 'wrong-type-argument))
  (dolist (plist '((nil) (a) (a a a)))
    (let ((err `(wrong-type-argument plistp ,(copy-sequence plist))))
      (dolist (key '(nil a))
        (should (equal err (should-error (eudc-plist-get plist key)))))))
  (let ((-nil (string ?n ?i ?l))
        (-a (string ?a)))
    (should-not (eudc-plist-get () nil))
    (should-not (eudc-plist-get () 'a))
    (should-not (eudc-plist-get '(nil nil) nil))
    (should-not (eudc-plist-get '(nil nil) 'a))
    (should-not (eudc-plist-get '(nil a) 'a))
    (should-not (eudc-plist-get '(a nil) nil))
    (should-not (eudc-plist-get '(a nil) 'a))
    (should-not (eudc-plist-get '(a a) nil))
    (should-not (eudc-plist-get '("nil" a) nil))
    (should-not (eudc-plist-get '("nil" a) -nil))
    (should-not (eudc-plist-get '("a" a) nil))
    (should-not (eudc-plist-get '("a" a) -a))
    (should-not (eudc-plist-get '(nil nil nil a) nil))
    (should-not (eudc-plist-get '(nil a nil a) 'a))
    (should-not (eudc-plist-get '(nil a "a" a) -a))
    (should-not (eudc-plist-get '(a nil a a) 'a))
    (should (eq 'a (eudc-plist-get '(nil a) nil)))
    (should (eq 'a (eudc-plist-get '(a a) 'a)))
    (should (eq 'a (eudc-plist-get '(a a a nil) 'a)))
    (should (eq 'b (eudc-plist-get () nil 'b)))
    (should (eq 'b (eudc-plist-get () 'a 'b)))
    (should (eq 'b (eudc-plist-get '(nil a "a" a) -a 'b)))
    (should (eq 'b (eudc-plist-get '(a nil "nil" nil) -nil 'b)))))

(ert-deftest eudc-lax-plist-get ()
  "Test `eudc-lax-plist-get' behavior."
  (dolist (obj '(a (a . a) (a a . a)))
    (should-error (eudc-lax-plist-get obj nil) :type 'wrong-type-argument))
  (dolist (plist '((nil) (a) (a a a)))
    (let ((err `(wrong-type-argument plistp ,(copy-sequence plist))))
      (dolist (key '(nil a))
        (should (equal err (should-error (eudc-lax-plist-get plist key)))))))
  (let ((-nil (string ?n ?i ?l))
        (-a (string ?a)))
    (should-not (eudc-lax-plist-get () nil))
    (should-not (eudc-lax-plist-get () 'a))
    (should-not (eudc-lax-plist-get '(nil nil) nil))
    (should-not (eudc-lax-plist-get '(nil nil) 'a))
    (should-not (eudc-lax-plist-get '(nil a) 'a))
    (should-not (eudc-lax-plist-get '(a nil) nil))
    (should-not (eudc-lax-plist-get '(a nil) 'a))
    (should-not (eudc-lax-plist-get '(a a) nil))
    (should-not (eudc-lax-plist-get '("nil" a) nil))
    (should-not (eudc-lax-plist-get '("nil" a) 'a))
    (should-not (eudc-lax-plist-get '("a" a) nil))
    (should-not (eudc-lax-plist-get '("a" a) 'a))
    (should-not (eudc-lax-plist-get '(nil nil nil a) nil))
    (should-not (eudc-lax-plist-get '(nil a nil a) 'a))
    (should-not (eudc-lax-plist-get '(nil a "a" a) 'a))
    (should-not (eudc-lax-plist-get '(a nil a a) 'a))
    (should (eq 'a (eudc-lax-plist-get '(nil a) nil)))
    (should (eq 'a (eudc-lax-plist-get '(a a) 'a)))
    (should (eq 'a (eudc-lax-plist-get '(a a a nil) 'a)))
    (should (eq 'b (eudc-lax-plist-get () nil 'b)))
    (should (eq 'b (eudc-lax-plist-get () 'a 'b)))
    (should (eq 'a (eudc-lax-plist-get '("nil" a) -nil)))
    (should (eq 'a (eudc-lax-plist-get '("a" a) -a)))
    (should (eq 'a (eudc-lax-plist-get '(nil a "a" a) -a)))
    (should (eq 'b (eudc-lax-plist-get '(nil a "a" a) 'a 'b)))
    (should (eq 'b (eudc-lax-plist-get '(a nil "nil" nil) nil 'b)))))

;; eudc-rfc5322-quote-phrase (string)
(ert-deftest eudc-test-rfc5322-quote-phrase ()
  "Tests for RFC5322 compliant phrase quoting."
  ;; atext-token "[:alpha:][:digit:]!#$%&'*+/=?^_`{|}~-"
  (should (equal (eudc-rfc5322-quote-phrase "Foo Bar !#$%&'*+/=?^_`{|}~-")
                 "Foo Bar !#$%&'*+/=?^_`{|}~-"))
  (should (equal (eudc-rfc5322-quote-phrase "Foo, Bar !#$%&'*+/=?^_`{|}~-")
                 "\"Foo, Bar !#$%&'*+/=?^_`{|}~-\"")))

;; eudc-rfc5322-valid-comment-p (string)
(ert-deftest eudc-test-rfc5322-valid-comment-p ()
  "Tests for RFC5322 compliant comments."
  ;; cctext-token "\u005D-\u007E\u002A-\u005B\u0021-\u0027" + fwsp-token (TAB, LF, SPC)
  ;; Printable US-ASCII characters not including "(", ")", or "\".
  (let ((good-chars (append (number-sequence #x09 #x0a)
                            (number-sequence #x20 #x20)
                            (number-sequence #x21 #x27)
                            (number-sequence #x2a #x5b)
                            (number-sequence #x5d #x7e)))
        (bad-chars  (append (number-sequence #x00 #x08)
                            (number-sequence #x0b #x1f)
                            (number-sequence #x28 #x29)
                            (number-sequence #x5c #x5c)
                            (number-sequence #x7f #xff))))
    (dolist (gc good-chars)
      (should (eq (eudc-rfc5322-valid-comment-p (format "%c" gc)) t)))
    (dolist (bc bad-chars)
      (should (eq (eudc-rfc5322-valid-comment-p (format "%c" bc)) nil)))))

;; eudc-rfc5322-make-address (address &optional firstname name comment)
(ert-deftest eudc-test-make-address ()
  "Tests for RFC5322 compliant email address formatting."
  (should (equal (eudc-rfc5322-make-address "")
                 nil))
  (should (equal (eudc-rfc5322-make-address nil)
                 nil))
  (should (equal (eudc-rfc5322-make-address "j.sixpack@example.org")
                 "j.sixpack@example.org"))
  (should (equal (eudc-rfc5322-make-address "<j.sixpack@example.org>")
                 "<j.sixpack@example.org>"))
  (should (equal (eudc-rfc5322-make-address "j.sixpack@example.org"
                                            "Joey")
                 "Joey <j.sixpack@example.org>"))
  (should (equal (eudc-rfc5322-make-address "j.sixpack@example.org"
                                            "Joey"
                                            "Sixpack")
                 "Joey Sixpack <j.sixpack@example.org>"))
  (should (equal (eudc-rfc5322-make-address "j.sixpack@example.org"
                                            "Joey"
                                            "Sixpack"
                                            "ten-packs are fine, too")
                 "Joey Sixpack <j.sixpack@example.org> \
(ten-packs are fine, too)"))
  (should (equal (eudc-rfc5322-make-address "j.sixpack@example.org"
                                            ""
                                            "Sixpack, Joey")
                 "\"Sixpack, Joey\" <j.sixpack@example.org>"))
  (should (equal (eudc-rfc5322-make-address "j.sixpack@example.org"
                                            nil
                                            "Sixpack, Joey")
                 "\"Sixpack, Joey\" <j.sixpack@example.org>"))
  (should (equal (eudc-rfc5322-make-address "j.sixpack@example.org"
                                            nil
                                            nil
                                            "Duh!")
                 "j.sixpack@example.org (Duh!)"))
  (should (equal (eudc-rfc5322-make-address "j.sixpack@example.org"
                                            nil
                                            nil
                                            "Duh\\!")
                 "j.sixpack@example.org")))

(require 'ert-x) ; ert-with-temp-directory

(defvar ecomplete-database-file (ert-resource-file "ecompleterc"))

(ert-deftest eudcb-ecomplete ()
  "Test the ecomplete back-end."
  (ert-with-temp-directory home
    (with-environment-variables (("HOME" home))
      (let ((eudc-ignore-options-file t))
        (should (equal (eudc-ecomplete-query-internal '((mail . "brigts")))
                       '(((mail . "Lars Ingebrigtsen <larsi@ecomplete.org>")))))
        (should (equal (eudc-ecomplete-query-internal '((mail . "karl")))
                       '(((mail . "Karl Fogel <kfogel@ecomplete.com>")))))
        (should (equal (eudc-ecomplete-query-internal '((mail . "behs")))
                       '(((mail . "behse@ecomplete.org")))))
        (should (equal (eudc-ecomplete-query-internal '((mail . "louie")))
                       nil))))))

(ert-with-temp-directory
 home
 (ert-deftest eudcb-mailabbrev ()
   "Test the mailabbrev back-end."
   (with-environment-variables
    (("HOME" home))
    (let ((mail-personal-alias-file (ert-resource-file "mailrc"))
          (eudc-ignore-options-file t))
      (should (equal (eudc-mailabbrev-query-internal '((email . "lars")))
                     '(((email . "larsi@mail-abbrev.com")
                        (name . "Lars Ingebrigtsen")))))
      (should (equal (eudc-mailabbrev-query-internal '((name . "lars")))
                     '(((email . "larsi@mail-abbrev.com")
                        (name . "Lars Ingebrigtsen")))))
      (should (equal (eudc-mailabbrev-query-internal '((phone . "lars")))
                     nil))
      (should (equal (eudc-mailabbrev-query-internal '((firstname . "karl")))
                     '(((email . "kfogel@mail-abbrev.com")
                        (name . "Karl Fogel")))))
      (should (equal (eudc-mailabbrev-query-internal '((email . "louie")))
                     nil))
      (should (equal (eudc-mailabbrev-query-internal '((name . "emacsheroes")))
                     '(((email . "Lars Ingebrigtsen <larsi@mail-abbrev.com>, \
Karl Fogel <kfogel@mail-abbrev.com")))))))))

(require 'ldap)
(ert-deftest eudcb-ldap ()
  "Test the LDAP back-end."
  (skip-unless (and (file-exists-p "/usr/sbin/slapd")
                    (file-exists-p "/usr/bin/ldapsearch")))
  (cd (concat (ert-resource-directory) ".."))
  (let ((ldap-process
         (start-process "slapd" "*slapd*" "/usr/sbin/slapd"
                        "-h" "ldap://127.0.0.1:3899" "-d" "0" "-4"
                        "-f" (ert-resource-file "slapd.conf")))
        (ldap-host-parameters-alist '(("ldap://localhost:3899"
                                       base "dc=gnu,dc=org" auth simple)))
        (eudc-server-hotlist '(("ldap://localhost:3899" . ldap)))
        (eudc-ignore-options-file t))
    (catch 'sldapd-up
      (dotimes (_tries 20)
        (when (eudc-query-with-words '("emacs-ert-test-1"))
          (throw 'sldapd-up nil)))
      (kill-process ldap-process)
      (error "Failed to confirm slapd is running"))
    (should (equal (with-temp-buffer
                     (insert "emacs-ert-test-1")
                     (eudc-expand-try-all)
                     (buffer-string))
                   "Emacs ERT1 <emacs-ert-test-1@ldap.gnu.org>"))
    (kill-process ldap-process)))

(eval-and-compile
  (push (expand-file-name "../elpa/packages/bbdb/lisp" source-directory)
        load-path)
  (defvar bbdb-file)
  (require 'bbdb nil t))

(ert-deftest eudcb-bbdb ()
  "Test the BBDB back-end."
  (skip-unless (featurep 'bbdb))
  (let ((bbdb-file (ert-resource-file "bbdb"))
        (eudc-server-hotlist '(("" . bbdb)))
        (eudc-ignore-options-file t))
    (should (equal (with-temp-buffer
                     (insert "emacs-ert-test-3")
                     (eudc-expand-try-all)
                     (buffer-string))
                   "Emacs ERT3 <emacs-ert-test-3@bbdb.gnu.org>"))))

(provide 'eudc-tests)
;;; eudc-tests.el ends here
