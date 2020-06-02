;;; sql-tests.el --- Tests for sql.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2020 Free Software Foundation, Inc.

;; Author: Simen Heggestøyl <simenheg@gmail.com>
;; Keywords:

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

;;

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'sql)

(ert-deftest sql-tests-postgres-list-databases ()
  "Test that output from `psql -ltX' is parsed correctly."
  (cl-letf
      (((symbol-function 'executable-find)
        (lambda (_command) t))
       ((symbol-function 'process-lines)
        (lambda (_program &rest _args)
          '(" db-name-1 | foo-user | UTF8     | en_US.UTF-8 | en_US.UTF-8 | "
            " db_name_2 | foo-user | UTF8     | en_US.UTF-8 | en_US.UTF-8 | "
            ""))))
    (should (equal (sql-postgres-list-databases)
                   '("db-name-1" "db_name_2")))))

(ert-deftest sql-tests-postgres-list-databases-error ()
  "Test that nil is returned when `psql -ltX' fails."
  (cl-letf
      (((symbol-function 'executable-find)
        (lambda (_command) t))
       ((symbol-function 'process-lines)
        (lambda (_program &rest _args)
          (error "some error"))))
    (should-not (sql-postgres-list-databases))))

;;; Check Connection Password Handling/Wallet

(defvar sql-test-login-params nil)
(defmacro with-sql-test-connect-harness (id login-params connection expected)
  "Set-up and tear-down SQL connect related test.

Identify tests by ID.  Set :sql-login dialect attribute to
LOGIN-PARAMS.  Provide the CONNECTION parameters and the EXPECTED
string of values passed to the comint function for validation."
  (declare (indent 2))
  `(cl-letf
       ((sql-test-login-params ' ,login-params)
        ((symbol-function 'sql-comint-test)
         (lambda (product options &optional buf-name)
           (with-current-buffer (get-buffer-create buf-name)
             (insert (pp-to-string (list product options sql-user sql-password sql-server sql-database))))))
        ((symbol-function 'sql-run-test)
         (lambda (&optional buffer)
           (interactive "P")
           (sql-product-interactive 'sqltest buffer)))
        (sql-user nil)
        (sql-server nil)
        (sql-database nil)
        (sql-product-alist
         '((ansi)
           (sqltest
            :name "SqlTest"
            :sqli-login sql-test-login-params
            :sqli-comint-func sql-comint-test)))
        (sql-connection-alist
         '((,(format "test-%s" id)
            ,@connection)))
        (sql-password-wallet
         (list
          (make-temp-file
           "sql-test-netrc" nil nil
           (mapconcat #'identity
                      '("machine aMachine user aUserName password \"netrc-A aPassword\""
                        "machine aServer user aUserName password \"netrc-B aPassword\""
                        "machine aMachine server aServer user aUserName password \"netrc-C aPassword\""
                        "machine aMachine database aDatabase user aUserName password \"netrc-D aPassword\""
                        "machine aDatabase user aUserName password \"netrc-E aPassword\""
                        "machine aMachine server aServer database aDatabase user aUserName password \"netrc-F aPassword\""
                        "machine \"aServer/aDatabase\" user aUserName password \"netrc-G aPassword\""
                        ) "\n")))))

     (let* ((connection ,(format "test-%s" id))
            (buffername (format "*SQL: ERT TEST <%s>*" connection)))
       (when (get-buffer buffername)
         (kill-buffer buffername))
       (sql-connect connection buffername)
       (should (get-buffer buffername))
       (should (string-equal (with-current-buffer buffername (buffer-string)) ,expected))
       (when (get-buffer buffername)
         (kill-buffer buffername))
       (delete-file (car sql-password-wallet)))))

(ert-deftest sql-test-connect ()
  "Test of basic `sql-connect'."
  (with-sql-test-connect-harness 1 (user password server database)
    ((sql-product 'sqltest)
     (sql-user "aUserName")
     (sql-password "test-1 aPassword")
     (sql-server "aServer")
     (sql-database "aDatabase"))
    "(sqltest nil \"aUserName\" \"test-1 aPassword\" \"aServer\" \"aDatabase\")\n"))

(ert-deftest sql-test-connect-password-func ()
  "Test of password function."
  (with-sql-test-connect-harness 2 (user password server database)
    ((sql-product 'sqltest)
     (sql-user "aUserName")
     (sql-password (lambda () (concat [?t ?e ?s ?t ?- ?2 ?\s
                                     ?a ?P ?a ?s ?s ?w ?o ?r ?d])))
     (sql-server "aServer")
     (sql-database "aDatabase"))
    "(sqltest nil \"aUserName\" \"test-2 aPassword\" \"aServer\" \"aDatabase\")\n"))

(ert-deftest sql-test-connect-wallet-server-database ()
  "Test of password function."
  (with-sql-test-connect-harness 3 (user password server database)
    ((sql-product 'sqltest)
     (sql-user "aUserName")
     (sql-server "aServer")
     (sql-database "aDatabase"))
    "(sqltest nil \"aUserName\" \"netrc-G aPassword\" \"aServer\" \"aDatabase\")\n"))

(ert-deftest sql-test-connect-wallet-database ()
  "Test of password function."
  (with-sql-test-connect-harness 4 (user password database)
    ((sql-product 'sqltest)
     (sql-user "aUserName")
     (sql-database "aDatabase"))
    "(sqltest nil \"aUserName\" \"netrc-E aPassword\" nil \"aDatabase\")\n"))

(ert-deftest sql-test-connect-wallet-server ()
  "Test of password function."
  (with-sql-test-connect-harness 5 (user password server)
    ((sql-product 'sqltest)
     (sql-user "aUserName")
     (sql-server "aServer"))
    "(sqltest nil \"aUserName\" \"netrc-B aPassword\" \"aServer\" nil)\n"))

;;; Set/Get Product Features

(defvar sql-test-feature-value-a nil "Indirect value A.")
(defvar sql-test-feature-value-b nil "Indirect value B.")
(defvar sql-test-feature-value-c nil "Indirect value C.")
(defvar sql-test-feature-value-d nil "Indirect value D.")
(defmacro sql-test-product-feature-harness (&rest action)
  "Set-up and tear-down of testing product/feature API.

Perform ACTION and validate results"
  (declare (indent 2))
  `(cl-letf
       ((sql-product-alist
         (list (list 'a :X 1 :Y 2 :Z 'sql-test-feature-value-a)
               (list 'b :X 3      :Z 'sql-test-feature-value-b)
               (list 'c      :Y 6 :Z 'sql-test-feature-value-c)
               (list 'd :X 7 :Y 8                            )))
        (sql-indirect-features '(:Z :W))
        (sql-test-feature-value-a "original A")
        (sql-test-feature-value-b "original B")
        (sql-test-feature-value-c "original C")
        (sql-test-feature-value-d "original D"))
     ,@action))

(ert-deftest sql-test-add-product ()
  "Add a product"

  (sql-test-product-feature-harness
      (sql-add-product 'xyz "XyzDb")

      (should (equal (pp-to-string (assoc 'xyz sql-product-alist))
                     "(xyz :name \"XyzDb\")\n"))))

(ert-deftest sql-test-add-existing-product ()
  "Add a product that already exists."

  (sql-test-product-feature-harness
      (should-error (sql-add-product 'a "Aaa"))
      (should (equal (pp-to-string (assoc 'a sql-product-alist))
                     "(a :X 1 :Y 2 :Z sql-test-feature-value-a)\n"))))

(ert-deftest sql-test-set-feature ()
  "Add a feature"

  (sql-test-product-feature-harness
      (sql-set-product-feature 'b :Y 4)
      (should (equal (pp-to-string (assoc 'b sql-product-alist))
                     "(b :Y 4 :X 3 :Z sql-test-feature-value-b)\n"))))

(ert-deftest sql-test-set-indirect-feature ()
  "Set a new indirect feature"

  (sql-test-product-feature-harness
      (sql-set-product-feature 'd :Z 'sql-test-feature-value-d)
      (should (equal (pp-to-string (assoc 'd sql-product-alist))
                     "(d :Z sql-test-feature-value-d :X 7 :Y 8)\n"))))

(ert-deftest sql-test-set-existing-feature ()
  "Set an existing feature."

  (sql-test-product-feature-harness
      (sql-set-product-feature 'b :X 33)
      (should (equal (pp-to-string (assoc 'b sql-product-alist))
                     "(b :X 33 :Z sql-test-feature-value-b)\n"))))

(ert-deftest sql-test-set-existing-indirect-feature ()
  "Set an existing indirect feature."

  (sql-test-product-feature-harness
      (should (equal sql-test-feature-value-b "original B"))
      (sql-set-product-feature 'b :Z "Hurray!")
    (should (equal (pp-to-string (assoc 'b sql-product-alist))
                   "(b :X 3 :Z sql-test-feature-value-b)\n")) ;; unchanged
    (should (equal sql-test-feature-value-b "Hurray!"))))

(ert-deftest sql-test-set-missing-product ()
  "Add a feature to a missing product."

  (sql-test-product-feature-harness
      (should-error (sql-set-product-feature 'x :Y 4))
      (should-not (assoc 'x sql-product-alist))))

(ert-deftest sql-test-get-feature ()
  "Get a feature value."

  (sql-test-product-feature-harness
      (should (equal (sql-get-product-feature 'c :Y) 6))))

(ert-deftest sql-test-get-indirect-feature ()
  "Get a feature indirect value."

  (sql-test-product-feature-harness
      (should (equal (sql-get-product-feature 'c :Z nil t) 'sql-test-feature-value-c))
      (should (equal sql-test-feature-value-c "original C"))
    (should (equal (sql-get-product-feature 'c :Z) "original C"))))

(ert-deftest sql-test-get-missing-product ()
  "Get a feature value from a missing product."

  (sql-test-product-feature-harness
      (should-error (sql-get-product-feature 'x :Y))))

(ert-deftest sql-test-get-missing-feature ()
  "Get a missing feature value."

  (sql-test-product-feature-harness
      (should-not (sql-get-product-feature 'c :X))))

(ert-deftest sql-test-get-missing-indirect-feature ()
  "Get a missing indirect feature value."

  (sql-test-product-feature-harness
      (should-not (sql-get-product-feature 'd :Z))))

;;; SQL Oracle SCAN/DEFINE
(defmacro sql-tests-placeholder-filter-harness (orig repl outp)
  "Set-up and tear-down of testing of placeholder filter.

The placeholder in ORIG will be replaced by REPL which should
yield OUTP."

  (declare (indent 0))
  `(let ((syntab (syntax-table))
         (sql-oracle-scan-on t))
     (set-syntax-table sql-mode-syntax-table)

     (cl-letf
         (((symbol-function 'read-from-minibuffer)
           (lambda (&rest _) ,repl)))

       (should (equal (sql-placeholders-filter ,orig) ,outp)))

     (set-syntax-table syntab)))

(ert-deftest sql-tests-placeholder-filter-simple ()
  "Test that placeholder relacement of simple replacement text."
  (sql-tests-placeholder-filter-harness
    "select '&x' from dual;" "XX"
    "select 'XX' from dual;"))

(ert-deftest sql-tests-placeholder-filter-ampersand ()
  "Test that placeholder relacement of replacement text with ampersand."
  (sql-tests-placeholder-filter-harness
    "select '&x' from dual;" "&Y"
    "select '&Y' from dual;")

  (sql-tests-placeholder-filter-harness
    "select '&x' from dual;" "Y&"
    "select 'Y&' from dual;")

  (sql-tests-placeholder-filter-harness
    "select '&x' from dual;" "Y&Y"
    "select 'Y&Y' from dual;"))

(ert-deftest sql-tests-placeholder-filter-period ()
  "Test that placeholder relacement of token terminated by a period."
  (sql-tests-placeholder-filter-harness
    "select '&x.' from dual;" "&Y"
    "select '&Y' from dual;")

  (sql-tests-placeholder-filter-harness
    "select '&x.y' from dual;" "&Y"
    "select '&Yy' from dual;")

  (sql-tests-placeholder-filter-harness
    "select '&x..y' from dual;" "&Y"
    "select '&Y.y' from dual;"))

;; Buffer naming
(defmacro sql-tests-buffer-naming-harness (product &rest action)
  "Set-up and tear-down of test of buffer naming.

The ACTION will be tested after set-up of PRODUCT."

  (declare (indent 1))
  `(progn
     (ert--skip-unless (executable-find sql-sqlite-program))
     (let (new-bufs)
       (cl-letf
           (((symbol-function 'make-comint-in-buffer)
             (lambda (_name buffer _program &optional _startfile &rest _switches)
               (let ((b (get-buffer-create buffer)))
                 (message ">>make-comint-in-buffer %S" b)
                 (cl-pushnew b new-bufs) ;; Keep track of what we create
                 b))))

         (let (,(intern (format "sql-%s-login-params" product)))
           ,@action)

         (let (kill-buffer-query-functions) ;; Kill what we create
           (mapc #'kill-buffer new-bufs))))))

(ert-deftest sql-tests-buffer-naming-default ()
  "Test buffer naming."
  (sql-tests-buffer-naming-harness sqlite
    (sql-sqlite)
    (message ">> %S" (current-buffer))
    (should (equal (buffer-name) "*SQL: SQLite*"))))

(ert-deftest sql-tests-buffer-naming-multiple ()
  "Test buffer naming of multiple buffers."
  (sql-tests-buffer-naming-harness sqlite
    (sql-sqlite)
    (should (equal (buffer-name) "*SQL: SQLite*"))

    (switch-to-buffer "*scratch*")

    (sql-sqlite)
    (should (equal (buffer-name) "*SQL: SQLite*"))))

(ert-deftest sql-tests-buffer-naming-explicit ()
  "Test buffer naming with explicit name."
  (sql-tests-buffer-naming-harness sqlite
    (sql-sqlite "A")
    (should (equal (buffer-name) "*SQL: A*"))

    (switch-to-buffer "*scratch*")

    (sql-sqlite "A")
    (should (equal (buffer-name) "*SQL: A*"))))

(ert-deftest sql-tests-buffer-naming-universal-argument ()
  "Test buffer naming with explicit name."
  (sql-tests-buffer-naming-harness sqlite
    (cl-letf
        (((symbol-function 'read-string)
          (lambda (_prompt &optional _initial-input _history _default-value _inherit-input-method)
            "1")))
      (sql-sqlite '(4))
      (should (equal (buffer-name) "*SQL: 1*")))

    (switch-to-buffer "*scratch*")

    (cl-letf
        (((symbol-function 'read-string)
          (lambda (_prompt &optional _initial-input _history _default-value _inherit-input-method)
            "2")))
      (sql-sqlite '(16))
      (should (equal (buffer-name) "*SQL: 2*")))))

(ert-deftest sql-tests-buffer-naming-existing ()
  "Test buffer naming with an existing non-SQLi buffer."
  (sql-tests-buffer-naming-harness sqlite
    (get-buffer-create "*SQL: exist*")

    (cl-letf
        (((symbol-function 'read-string)
          (lambda (_prompt &optional _initial-input _history _default-value _inherit-input-method)
            "exist")))
      (sql-sqlite '(4))
      (should (equal (buffer-name) "*SQL: exist-1*")))

    (kill-buffer "*SQL: exist*")))


(provide 'sql-tests)
;;; sql-tests.el ends here
