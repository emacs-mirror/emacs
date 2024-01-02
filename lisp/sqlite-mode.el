;;; sqlite-mode.el --- Mode for examining sqlite3 database files  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

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
(eval-when-compile (require 'subr-x))

(declare-function sqlite-execute "sqlite.c")
(declare-function sqlite-more-p "sqlite.c")
(declare-function sqlite-next "sqlite.c")
(declare-function sqlite-columns "sqlite.c")
(declare-function sqlite-finalize "sqlite.c")
(declare-function sqlite-select "sqlite.c")
(declare-function sqlite-open "sqlite.c")

(defvar-keymap sqlite-mode-map
  "g" #'sqlite-mode-list-tables
  "c" #'sqlite-mode-list-columns
  "RET" #'sqlite-mode-list-data
  "DEL" #'sqlite-mode-delete)

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
  (unless (sqlite-available-p)
    (error "This Emacs doesn't have SQLite support, so it can't view SQLite files"))
  (if (file-remote-p file)
      (error "Remote SQLite files are not yet supported"))
  (pop-to-buffer (get-buffer-create
                  (format "*SQLite %s*" (file-name-nondirectory file))))
  (sqlite-mode)
  (setq-local sqlite--db (sqlite-open file))
  (unless (sqlitep sqlite--db)
    (error "`sqlite-open' failed to open SQLite file"))
  (add-hook 'kill-buffer-hook (lambda () (sqlite-close sqlite--db)) nil t)
  (sqlite-mode-list-tables))

(defun sqlite-mode-list-tables ()
  "Re-list the tables from the currently selected database."
  (interactive nil sqlite-mode)
  (let ((inhibit-read-only t)
        (db sqlite--db)
        (entries nil))
    (erase-buffer)
    (dolist (table (sqlite-select db "select name from sqlite_master where type = 'table' and name not like 'sqlite_%' order by name"))
      (push (list (car table)
                  (caar (sqlite-select db (format "select count(*) from %s"
                                                  (car table)))))
            entries))
    (sqlite-mode--tablify '("Table Name" "Number of Rows")
                          (nreverse entries)
                          'table)
    (goto-char (point-min))))

(defun sqlite-mode--tablify (columns rows type &optional prefix)
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
      (insert (propertize (format (format "%%-%ds " (nth i widths))
                                  (nth i columns))
                          'face 'header-line)))
    (insert "\n")
    (dolist (row rows)
      (let ((start (point)))
        (when prefix
          (insert prefix))
        (dotimes (i (length widths))
          (let ((elem (nth i row)))
            (insert (format (format "%%%s%ds "
                                    (if (numberp elem)
                                        "" "-")
                                    (nth i widths))
                            (if (numberp elem)
                                (nth i row)
                              (string-replace "\n" " " (or elem "")))))))
        (put-text-property start (point) 'sqlite--row row)
        (put-text-property start (point) 'sqlite--type type)
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
            (delete-region (point) (if (re-search-forward "^[^ \t]" nil t)
                                       (match-beginning 0)
                                     (point-max)))
          ;; Insert the info.
          (dolist (column columns)
            (insert (format "  %s\n" column))))))))

(defun sqlite-mode--column-names (table)
  "Return a list of the column names for TABLE."
  (mapcar (lambda (row) (nth 1 row)) (sqlite-select sqlite--db (format "pragma table_info(%s)" table))))

(defun sqlite-mode-list-data ()
  "List the data from the table under point."
  (interactive nil sqlite-mode)
  (let ((row (and (eq (get-text-property (point) 'sqlite--type) 'table)
                  (get-text-property (point) 'sqlite--row))))
    (unless row
      (user-error "No table under point"))
    (let ((inhibit-read-only t))
      (save-excursion
        (forward-line 1)
        (if (looking-at " ")
            ;; Delete the info.
            (delete-region (point) (if (re-search-forward "^[^ ]" nil t)
                                       (match-beginning 0)
                                     (point-max)))
          (sqlite--mode--list-data (list (car row) 0)))))))

(defun sqlite-mode--more-data (stmt)
  (let ((inhibit-read-only t))
    (beginning-of-line)
    (delete-region (point) (progn (forward-line 1) (point)))
    (sqlite--mode--list-data stmt)))

(defun sqlite--mode--list-data (data)
  (let* ((table (car data))
         (rowid (cadr data))
         stmt)
    (unwind-protect
        (progn
          (setq stmt
                (sqlite-select
                 sqlite--db
                 (format "select rowid, * from %s where rowid >= ?" table)
                 (list rowid)
                 'set))
          (sqlite-mode--tablify (sqlite-columns stmt)
                                (cl-loop for i from 0 upto 1000
                                         for row = (sqlite-next stmt)
                                         while row
                                         do (setq rowid (car row))
                                         collect row)
                                (cons 'row table)
                                "  ")
          (when (sqlite-more-p stmt)
            (insert (buttonize "  More data...\n" #'sqlite-mode--more-data
                               (list table rowid)))))
      (when stmt
        (sqlite-finalize stmt)))))

(defun sqlite-mode-delete ()
  "Delete the row under point."
  (interactive nil sqlite-mode)
  (let ((table (get-text-property (point) 'sqlite--type))
        (row (get-text-property (point) 'sqlite--row))
        (inhibit-read-only t))
    (when (or (not (consp table))
              (not (eq (car table) 'row)))
      (user-error "No row under point"))
    (unless (yes-or-no-p "Really delete the row under point? ")
      (user-error "Not deleting"))
    (sqlite-execute
     sqlite--db
     (format "delete from %s where %s"
             (cdr table)
             (string-join
              (mapcar (lambda (column)
                        (format "%s = ?" (car (split-string column " "))))
                      (cons "rowid" (sqlite-mode--column-names (cdr table))))
              " and "))
     row)
    (delete-region (line-beginning-position) (progn (forward-line 1) (point)))))

(provide 'sqlite-mode)

;;; sqlite-mode.el ends here
