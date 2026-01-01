;;; sqlite.el --- Functions for interacting with sqlite3 databases  -*- lexical-binding: t; -*-

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

(eval-when-compile (require 'cl-lib))

(declare-function sqlite-transaction "sqlite.c")
(declare-function sqlite-commit "sqlite.c")
(declare-function sqlite-rollback "sqlite.c")

(defmacro with-sqlite-transaction (db &rest body)
  "Execute BODY while holding a transaction for DB.
If BODY completes normally, commit the changes and return
the value of BODY.
If BODY signals an error, or transaction commit fails, roll
back the transaction changes before allowing the signal to
propagate."
  (declare (indent 1) (debug (form body)))
  (cl-with-gensyms (db-var func-var res-var commit-var)
    `(let ((,db-var ,db)
           (,func-var (lambda () ,@body))
           ,res-var ,commit-var)
       (if (sqlite-available-p)
           (unwind-protect
               (progn
                 (sqlite-transaction ,db-var)
                 (setq ,res-var (funcall ,func-var))
                 (setq ,commit-var (sqlite-commit ,db-var))
                 ,res-var)
             (or ,commit-var (sqlite-rollback ,db-var)))
         (funcall ,func-var)))))

(provide 'sqlite)

;;; sqlite.el ends here
