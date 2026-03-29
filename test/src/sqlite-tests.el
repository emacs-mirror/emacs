;;; sqlite-tests.el --- Tests for sqlite.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

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

(require 'ert)
(require 'ert-x)

(declare-function sqlite-execute "sqlite.c")
(declare-function sqlite-close "sqlite.c")
(declare-function sqlitep "sqlite.c")
(declare-function sqlite-available-p "sqlite.c")
(declare-function sqlite-finalize "sqlite.c")
(declare-function sqlite-next "sqlite.c")
(declare-function sqlite-more-p "sqlite.c")
(declare-function sqlite-select "sqlite.c")
(declare-function sqlite-open "sqlite.c")
(declare-function sqlite-load-extension "sqlite.c")
(declare-function sqlite-version "sqlite.c")
(declare-function sqlite-execute-batch "sqlite.c")

(ert-deftest sqlite-select ()
  (skip-unless (sqlite-available-p))
  (let ((db (sqlite-open)))
    (should (eq (type-of db) 'sqlite))
    (should (sqlitep db))
    (should-not (sqlitep 'foo))

    (should
     (zerop
      (sqlite-execute
       db "create table if not exists test1 (col1 text, col2 integer, col3 float, col4 blob)")))

    (should-error
     (sqlite-execute
      db "insert into test1 (col1, col2, col3, col4) values ('foo', 2, 9.45, 'bar', 'zot')"))

    (should
     (=
      (sqlite-execute
       db "insert into test1 (col1, col2, col3, col4) values ('foo', 2, 9.45, 'bar')")
      1))

    (should
     (equal
      (sqlite-select  db "select * from test1" nil 'full)
      '(("col1" "col2" "col3" "col4") ("foo" 2 9.45 "bar"))))))

(ert-deftest sqlite-set ()
  (skip-unless (sqlite-available-p))
  (let ((db (sqlite-open))
        set)
    (should
     (zerop
      (sqlite-execute
       db "create table if not exists test1 (col1 text, col2 integer)")))

    (should
     (=
      (sqlite-execute db "insert into test1 (col1, col2) values ('foo', 1)")
      1))
    (should
     (=
      (sqlite-execute db "insert into test1 (col1, col2) values ('bar', 2)")
      1))

    (setq set (sqlite-select db "select * from test1" nil 'set))
    (should (sqlitep set))
    (should (sqlite-more-p set))
    (should (equal (sqlite-next set)
                   '("foo" 1)))
    (should (equal (sqlite-next set)
                   '("bar" 2)))
    (should-not (sqlite-next set))
    (should-not (sqlite-more-p set))
    (sqlite-finalize set)
    (should-error (sqlite-next set))))

(ert-deftest sqlite-chars ()
  (skip-unless (sqlite-available-p))
  (let (db)
    (setq db (sqlite-open))
    (sqlite-execute
     db "create table if not exists test2 (col1 text, col2 integer)")
    (sqlite-execute
     db "insert into test2 (col1, col2) values ('fóo', 3)")
    (sqlite-execute
     db "insert into test2 (col1, col2) values ('fóo', 3)")
    (sqlite-execute
     db "insert into test2 (col1, col2) values ('fo', 4)")
    (should
     (equal (sqlite-select db "select * from test2" nil 'full)
            '(("col1" "col2") ("fóo" 3) ("fóo" 3) ("fo" 4))))))

(ert-deftest sqlite-numbers ()
  (skip-unless (sqlite-available-p))
  (let (db)
    (setq db (sqlite-open))
    (sqlite-execute
     db "create table if not exists test3 (col1 integer)")
    (let ((big (expt 2 50))
          (small (expt 2 10)))
      (sqlite-execute db (format "insert into test3 values (%d)" small))
      (sqlite-execute db (format "insert into test3 values (%d)" big))
      (should
       (equal
        (sqlite-select db "select * from test3")
        (list (list small) (list big)))))))

(ert-deftest sqlite-param ()
  (skip-unless (sqlite-available-p))
  (let (db)
    (setq db (sqlite-open))
    (sqlite-execute
     db "create table if not exists test4 (col1 text, col2 number)")
    (sqlite-execute db "insert into test4 values (?, ?)" (list "foo" 1))
    (should
     (equal
      (sqlite-select db "select * from test4 where col2 = ?" '(1))
      '(("foo" 1))))
    (should
     (equal
      (sqlite-select db "select * from test4 where col2 = ?" [1])
      '(("foo" 1))))))

(ert-deftest sqlite-binary ()
  (skip-unless (sqlite-available-p))
  (let (db)
    (setq db (sqlite-open))
    (sqlite-execute
     db "create table if not exists test5 (col1 text, col2 number)")
    (let ((string (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert 0 1 2)
                    (buffer-string))))
      (should-not (multibyte-string-p string))
      (sqlite-execute
       db "insert into test5 values (?, ?)" (list string 2))
      (let ((out (caar
                  (sqlite-select db "select col1 from test5 where col2 = 2"))))
        (should (equal out string))))))

(ert-deftest sqlite-different-dbs ()
  (skip-unless (sqlite-available-p))
  (let (db1 db2)
    (setq db1 (sqlite-open))
    (setq db2 (sqlite-open))
    (sqlite-execute
     db1 "create table if not exists test6 (col1 text, col2 number)")
    (sqlite-execute
     db2 "create table if not exists test6 (col1 text, col2 number)")
    (sqlite-execute
     db1 "insert into test6 values (?, ?)" '("foo" 2))
    (should (sqlite-select db1 "select * from test6"))
    (should-not (sqlite-select db2 "select * from test6"))))

(ert-deftest sqlite-close-dbs ()
  (skip-unless (sqlite-available-p))
  (let (db)
    (setq db (sqlite-open))
    (sqlite-execute
     db "create table if not exists test6 (col1 text, col2 number)")
    (sqlite-execute db "insert into test6 values (?, ?)" '("foo" 2))
    (should (sqlite-select db "select * from test6"))
    (sqlite-close db)
    (should-error (sqlite-select db "select * from test6"))))

(ert-deftest sqlite-load-extension ()
  (skip-unless (sqlite-available-p))
  (skip-unless (fboundp 'sqlite-load-extension))
  (let (db)
    (setq db (sqlite-open))
    (should-error
     (sqlite-load-extension db "/usr/lib/sqlite3/notpcre.so"))
    (should-error
     (sqlite-load-extension db "/usr/lib/sqlite3/n"))
    (should-error
     (sqlite-load-extension db "/usr/lib/sqlite3/"))
    (should-error
     (sqlite-load-extension db "/usr/lib/sqlite3"))
    (if (eq system-type 'windows-nt)
        (should
         (eq (sqlite-load-extension db "/usr/lib/sqlite3/pcre.dll")
             (file-readable-p "/usr/lib/sqlite3/pcre.dll")))
      (should
       (eq (sqlite-load-extension db "/usr/lib/sqlite3/pcre.so")
           (file-readable-p "/usr/lib/sqlite3/pcre.so"))))

    (should-error
     (sqlite-load-extension
      db "/usr/lib/x86_64-linux-gnu/libsqlite3_mod_notcsvtable.so"))
    (should-error
     (sqlite-load-extension
      db "/usr/lib/x86_64-linux-gnu/libsqlite3_mod_csvtablen.so"))
    (should-error
     (sqlite-load-extension
      db "/usr/lib/x86_64-linux-gnu/libsqlite3_mod_csvtable"))
    (if (eq system-type 'windows-nt)
        (should
         (eq (sqlite-load-extension db "/usr/lib/sqlite3/csvtable.dll")
             (file-readable-p "/usr/lib/sqlite3/csvtable.dll")))
      (should
       (eq (sqlite-load-extension db "/usr/lib/x86_64-linux-gnu/libsqlite3_mod_csvtable.so")
           (file-readable-p "/usr/lib/x86_64-linux-gnu/libsqlite3_mod_csvtable.so"))))))

(ert-deftest sqlite-blob ()
  (skip-unless (sqlite-available-p))
  (let (db)
    (progn
      (setq db (sqlite-open))
      (sqlite-execute
       db "create table if not exists test10 (col1 text, col2 blob, col3 numbre)")
      (let ((string (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (insert 0 1 2)
                      (buffer-string))))
        (should-not (multibyte-string-p string))
        (sqlite-execute
         db "insert into test10 values (?, ?, 1)"
         (list string
               (propertize string
                           'coding-system 'binary)))
        (cl-destructuring-bind
            (c1 c2 _)
            (car (sqlite-select db "select * from test10 where col3 = 1"))
          (should (equal c1 string))
          (should (equal c2 string))
          (should (multibyte-string-p c1))
          (should-not (multibyte-string-p c2)))))))

(ert-deftest sqlite-returning ()
  (skip-unless (sqlite-available-p))
  (skip-unless (version<= "3.35" (sqlite-version)))
  (let (db)
    (progn
      (setq db (sqlite-open))
      (sqlite-execute db "CREATE TABLE people1 (people_id INTEGER PRIMARY KEY, first TEXT, last TEXT)")
      (should (null (sqlite-select db "select * from people1")))
      (should
       (equal
        (sqlite-execute db "INSERT INTO people1 (first, last) values (?, ?) RETURNING people_id, first"
		        '("Joe" "Doe"))
        '((1 "Joe")))))))

(ert-deftest sqlite-multiple-statements ()
  (skip-unless (sqlite-available-p))
  (let ((db (sqlite-open nil))
        (query (with-temp-buffer
                 (insert "-- -*- sql-product: sqlite -*-

-- I 💘 emojis

CREATE TABLE settings (
  name TEXT NOT NULL,
  value TEXT,
  section TEXT NOT NULL,
  PRIMARY KEY (section, name)
);

CREATE TABLE tags📎 (
  name TEXT PRIMARY KEY NOT NULL
);

-- CREATE TABLE todo_states (id INTEGER PRIMARY KEY, name TEXT NOT NULL);
")
                 (buffer-string))))
    (sqlite-execute-batch db query)
    (should (equal '(("settings") ("tags📎"))
                   (sqlite-select db "select name from sqlite_master where type = 'table' and name not like 'sqlite_%' order by name")))))

;;; sqlite-tests.el ends here
