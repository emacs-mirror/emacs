;;; sqlite-mode.el --- Mode for examining sqlite3 database files  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

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

(defvar-keymap sqlite-mode-map
  "g" #'sqlite-mode-list-tables
  "c" #'sqlite-mode-list-columns
  "RET" #'sqlite-mode-list-data)

(define-derived-mode sqlite-mode special-mode "Sqlite"
  "This mode lists the contents of an .sqlite3 file"
  :interactive nil
  (buffer-disable-undo)
  (setq-local buffer-read-only t
              truncate-lines t))

(defvar sqlite--db nil)

;;;###autoload
(defun sqlite-mode-open-file (file)
  "Browse the contents of an sqlite file."
  (interactive "fSQLite file name: ")
  (pop-to-buffer (get-buffer-create
                  (format "*SQLite %s*" (file-name-nondirectory file))))
  (sqlite-mode)
  (setq-local sqlite--db (sqlite-open file))
  (sqlite-mode-list-tables))

(defun sqlite-mode-list-tables ()
  "Re-list the tables from the currently selected database."
  (interactive nil sqlite-mode)
  (let ((inhibit-read-only t)
        (db sqlite--db)
        (entries nil))
    (erase-buffer)
    (dolist (table (sqlite-select db "select name from sqlite_schema where type = 'table' and name not like 'sqlite_%' order by name"))
      (push (list (car table)
                  (caar (sqlite-select db (format "select count(*) from %s"
                                                  (car table)))))
            entries))
    (sqlite-mode--tablify '("Table Name" "Number of Rows")
                          (nreverse entries))
    (goto-char (point-min))))

(defun sqlite-mode--tablify (columns rows &optional prefix)
  (let ((widths
         (mapcar
          (lambda (i)
            (1+ (seq-max (mapcar (lambda (row)
                                   (length (format "%s" (nth i row))))
                                 (cons columns rows)))))
          (number-sequence 0 (1- (length columns))))))
    (when prefix
      (insert prefix))
    (dotimes (i (length widths))
      (insert (propertize (format (format "%%-%ds" (nth i widths))
                                  (nth i columns))
                          'face 'header-line)))
    (insert "\n")
    (dolist (row rows)
      (let ((start (point)))
        (when prefix
          (insert prefix))
        (dotimes (i (length widths))
          (insert (format (format "%%%s%ds"
                                  (if (numberp (nth i row))
                                      "" "-")
                                  (nth i widths))
                          (or (nth i row) ""))))
        (put-text-property start (point) 'sqlite--row row)
        (insert "\n")))))

(defun sqlite-mode-list-columns ()
  "List the columns of the table under point."
  (interactive nil sqlite-mode)
  (let ((row (get-text-property (point) 'sqlite--row)))
    (unless row
      (user-error "No table under point"))
    (let ((columns (sqlite-mode--column-names (car row)))
          (inhibit-read-only t))
      (save-excursion
        (forward-line 1)
        (if (looking-at " ")
            ;; Delete the info.
            (delete-region (point) (if (re-search-forward "^[^ ]" nil t)
                                       (match-beginning 0)
                                     (point-max)))
          ;; Insert the info.
          (dolist (column columns)
            (insert (format "  %s\n" column))))))))

(defun sqlite-mode--column-names (table)
  (let ((sql
         (caar
          (sqlite-select
           sqlite--db
           "select sql from sqlite_master where tbl_name = ? AND type = 'table'"
           (list table)))))
    (mapcar
     #'string-trim
     (split-string (replace-regexp-in-string "^.*(\\|)$" "" sql) ","))))

(defun sqlite-mode-list-data ()
  "List the data from the table under poing."
  (interactive nil sqlite-mode)
  (let ((row (get-text-property (point) 'sqlite--row)))
    (unless row
      (user-error "No table under point"))
    (let ((stmt (sqlite-select sqlite--db
                               (format "select * from %s" (car row)) nil 'set))
          (inhibit-read-only t))
      (save-excursion
        (forward-line 1)
        (if (looking-at " ")
            ;; Delete the info.
            (delete-region (point) (if (re-search-forward "^[^ ]" nil t)
                                       (match-beginning 0)
                                     (point-max)))
          (sqlite--mode--list-data stmt))))))

(defun sqlite-mode--more-data (stmt)
  (let ((inhibit-read-only t))
    (beginning-of-line)
    (delete-region (point) (progn (forward-line 1) (point)))
    (sqlite--mode--list-data stmt)))

(defun sqlite--mode--list-data (stmt)
  (let ((rows
         (cl-loop for i from 0 upto 1000
                  for row = (sqlite-next stmt)
                  while row
                  collect row)))
    (sqlite-mode--tablify (sqlite-columns stmt) rows "  ")
    (when (sqlite-more-p stmt)
      (insert (buttonize "  More data...\n"
                         #'sqlite-mode--more-data stmt)))))

(provide 'sqlite-mode)

;;; sqlite-mode.el ends here
