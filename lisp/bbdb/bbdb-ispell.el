;;; bbdb-ispell.el --- export names from BBDB to personal ispell dictionaries -*- lexical-binding: t -*-

;; Copyright (C) 2011-2017  Free Software Foundation, Inc.

;; Author: Ivan Kanis <ivan.kanis@googlemail.com>

;; This file is part of the Insidious Big Brother Database (aka BBDB),

;; BBDB is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; BBDB is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with BBDB.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Names are often not recognized by the standard ispell dictionaries.
;; `bbdb-ispell-export' exports the names from your BBDB records to your
;; personal ispell dictionaries.
;; The personal dictionaries are in `bbdb-ispell-dictionary-list'
;; The BBDB fields for this are in `bbdb-ispell-field-list'.
;; Exclude words via `bbdb-ispell-min-word-length' and `bbdb-ispell-ignore-re'.
;;
;; Bugs:
;; Save your personal directories before running this code.  I had my
;; dictionary truncated while debugging.  It shouldn't happen
;; but better be safe than sorry...
;;
;; See the BBDB info manual for documentation.

;;; Code:

(require 'ispell)
(require 'bbdb)

(defcustom bbdb-ispell-dictionary-list '("default")
  "List of ispell personal dictionaries.
Allowed elements are as in the return value of `ispell-valid-dictionary-list'."
  :group 'bbdb-utilities-ispell
  :type (cons 'set (mapcar (lambda (dict) `(string ,dict))
                           (ispell-valid-dictionary-list))))

(defcustom bbdb-ispell-field-list '(name organization aka)
  "List of fields of each BBDB record considered for the personal dictionary."
  :group 'bbdb-utilities-ispell
  :type (list 'repeat
              (append '(choice) (mapcar (lambda (field) `(const ,field))
                                        '(name organization affix aka address))
                      '((symbol :tag "xfield")))))

(defcustom bbdb-ispell-min-word-length 3
  "Words with fewer characters are ignored."
  :group 'bbdb-utilities-ispell
  :type 'number)

(defcustom bbdb-ispell-ignore-re "[^[:alpha:]]"
  "Words matching this regexp are ignored."
  :group 'bbdb-utilities-ispell
  :type 'regexp)

;; Internal variable
(defvar bbdb-ispell-word-list nil
  "List of words extracted from the BBDB records.")

;;;###autoload
(defun bbdb-ispell-export ()
  "Export BBDB records to ispell personal dictionaries."
  (interactive)
  (message "Exporting to personal dictionary...")
  (let (bbdb-ispell-word-list)
    ;; Collect words from BBDB records.
    (dolist (record (bbdb-records))
      (dolist (field bbdb-ispell-field-list)
        (bbdb-ispell-collect-words (bbdb-record-field record field))))

    ;; Update personal dictionaries
    (dolist (dict (or bbdb-ispell-dictionary-list '("default")))
      (ispell-change-dictionary dict)
      ;; Initialize variables and dicts alists
      (ispell-set-spellchecker-params)
      (ispell-init-process)
      ;; put in verbose mode
      (ispell-send-string "%\n")
      (let (new)
        (dolist (word (delete-dups bbdb-ispell-word-list))
          (ispell-send-string (concat "^" word "\n"))
          (while (progn
                   (ispell-accept-output)
                   (not (string= "" (car ispell-filter)))))
          ;; remove extra \n
          (setq ispell-filter (cdr ispell-filter))
          (when (and ispell-filter
                     (listp ispell-filter)
                     (not (eq (ispell-parse-output (car ispell-filter)) t)))
            ;; ok the word doesn't exist, add it
            (ispell-send-string (concat "*" word "\n"))
            (setq new t)))
        (when new
          ;; Save dictionary:
          ;; aspell doesn't tell us when it completed the saving.
          ;; So we send it another word for spellchecking.
          (ispell-send-string "#\n^hello\n")
          (while (progn
                   (ispell-accept-output)
                   (not (string= "" (car ispell-filter)))))))))
  (message "Exporting to personal dictionary...done"))

(defun bbdb-ispell-collect-words (field)
  "Parse BBDB FIELD and collect words in `bbdb-ispell-word-list'."
  ;; Ignore everything in FIELD that is not a string or a sequence.
  (cond ((stringp field)
         (dolist (word (split-string field))
           (if (and (>= (length word) bbdb-ispell-min-word-length)
                    (not (string-match bbdb-ispell-ignore-re word)))
               (push word bbdb-ispell-word-list))))
        ((sequencep field) (mapc 'bbdb-ispell-collect-words field))))

(provide 'bbdb-ispell)

;;; bbdb-ispell.el ends here
