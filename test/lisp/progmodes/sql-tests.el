;;; sql-tests.el --- Tests for sql.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018 Free Software Foundation, Inc.

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

(provide 'sql-tests)
;;; sql-tests.el ends here
