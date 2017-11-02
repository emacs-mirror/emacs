;;; bbdb-migrate.el --- migration functions for BBDB -*- lexical-binding: t -*-

;; Copyright (C) 2010-2017  Free Software Foundation, Inc.

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
;;; This file contains the migration functions for BBDB.
;;; See the BBDB info manual for documentation.

;; Changes in `bbdb-file-format':
;;   3  Date format for `creation-date' and `timestamp' changed
;;      from "dd mmm yy" (ex: 25 Sep 97) to "yyyy-mm-dd" (ex: 1997-09-25).
;;   4  Country field added.
;;   5  Streets are lists.
;;   6  Postcodes are plain strings.
;;   7  New field `affix'.  Organizations are a list.
;;      Xfields is always a list.
;;  (8  Skipped format in "official BBDB": Some BBDB users introduced
;;      an xfield uuid in their format 8.  To bring them back, we jump
;;      straight from 7 to 9.)
;;   9  New field uuid.  Make `creation-date' and `timestamp' immutable fields.


;;; Code:

(require 'bbdb)

;;; Migrating the BBDB

(defvar bbdb-migrate-uuid-xfield 'uuid
  "Xfield holding a uuid in file format 8.")

;;;###autoload
(defun bbdb-migrate (records old)
  "Migrate RECORDS from format OLD to `bbdb-file-format'."
  ;; Some BBDB files were corrupted by random outer layers of
  ;; parentheses surrounding the actual correct data.  We attempt to
  ;; compensate for this.
  (while (and (consp records)
	      (listp (car records))
	      (null (cdr records)))
    (setq records (car records)))

  ;; `bbdb-migrate-lambda' uses the usual functions to access and set
  ;; the fields of a record.  So if a new record format changes
  ;; the set of fields, we need to make these changes first.

  ;; Format 7: Add new field `affix'.
  (if (< old 7)
      (let (new-records)
        (dolist (record records)
          (push (vector (elt record 0) (elt record 1) nil
                        (elt record 2) (elt record 3) (elt record 4)
                        (elt record 5) (elt record 6) (elt record 7)
                        (elt record 8))
                new-records))
        (setq records (nreverse new-records))))

  ;; Format 9: New field `uuid'.
  ;; Make `creation-date' and `timestamp' immutable fields.
  (if (< old 9)
      (let (new-records)
        (dolist (record records)
          (let ((uuid (or (cdr (assq bbdb-migrate-uuid-xfield (elt record 8)))
                          (bbdb-uuid)))
                (creation-date (or (cdr (assq 'creation-date (elt record 8)))
                                   (format-time-string bbdb-time-stamp-format nil t)))
                (timestamp (or (cdr (assq 'timestamp (elt record 8)))
                               (format-time-string bbdb-time-stamp-format nil t))))
            (push (vector (elt record 0) (elt record 1) (elt record 2)
                          (elt record 3) (elt record 4) (elt record 5)
                          (elt record 6) (elt record 7)
                          (let ((xfields (elt record 8)))
                            (dolist (elt '(uuid creation-date timestamp))
                              (setq xfields (assq-delete-all elt xfields)))
                            xfields)
                          uuid creation-date timestamp
                          (elt record 9))
                  new-records)))
        (setq records (nreverse new-records))))

  (mapc (bbdb-migrate-lambda old) records)
  records)

(defconst bbdb-migrate-alist
  '((3 (bbdb-record-xfields bbdb-record-set-xfields
        bbdb-migrate-dates))
    (4 (bbdb-record-address bbdb-record-set-address
        bbdb-migrate-add-country))
    (5 (bbdb-record-address bbdb-record-set-address
        bbdb-migrate-streets-to-list))
    (6 (bbdb-record-address bbdb-record-set-address
        bbdb-migrate-postcode-to-string))
    (7 (bbdb-record-xfields bbdb-record-set-xfields
        bbdb-migrate-xfields-to-list)
       (bbdb-record-organization bbdb-record-set-organization
        bbdb-migrate-organization-to-list)))
  ;; Formats 8 and 9: do nothing
  "Alist (VERSION . CHANGES).
CHANGES is a list with elements (GET SET FUNCTION) that expands
to action (SET record (FUNCTION (GET record))).")

(defun bbdb-migrate-lambda (old)
  "Return the function to migrate from OLD to `bbdb-file-format'.
The manipulations are defined by `bbdb-migrate-alist'."
  (let (spec)
    (while (<= old bbdb-file-format)
      (setq spec (append spec (cdr (assoc old bbdb-migrate-alist)))
            old (1+ old)))
    `(lambda (record)
       ,@(mapcar (lambda (change)
                   ;; (SET record (FUNCTION (GET record)))
                   `(,(nth 1 change) record ; SET
                     (,(nth 2 change) ; FUNCTION
                      (,(nth 0 change) record)))) ; GET
                 spec)
       record)))

(defun bbdb-migrate-postcode-to-string (addresses)
  "Make all postcodes plain strings.
This uses the code that used to be in `bbdb-address-postcode'."
  ;; apply the function to all addresses in the list and return a
  ;; modified list of addresses
  (mapcar (lambda (address)
            (let ((postcode (bbdb-address-postcode address)))
              (bbdb-address-set-postcode
               address
               (cond ((stringp postcode)
                      postcode)
                     ;; nil or zero
                     ((or (zerop postcode)
                          (null postcode))
                      "")
                     ;; a number
                     ((numberp postcode)
                      (format "%d" postcode))
                     ;; list with two strings
                     ((and (stringp (nth 0 postcode))
                           (stringp (nth 1 postcode)))
                      ;; the second string starts with 4 digits
                      (if (string-match "^[0-9][0-9][0-9][0-9]"
                                        (nth 1 postcode))
                          (format "%s-%s" (nth 0 postcode) (nth 1 postcode))
                        ;; ("abc" "efg")
                        (format "%s %s" (nth 0 postcode) (nth 1 postcode))))
                     ;; list with two numbers
                     ((and (integerp (nth 0 postcode))
                           (integerp (nth 1 postcode)))
                      (format "%05d-%04d" (nth 0 postcode) (nth 1 postcode)))
                     ;; list with a string and a number
                     ((and (stringp (nth 0 postcode))
                           (integerp (nth 1 postcode)))
                      (format "%s-%d" (nth 0 postcode) (nth 1 postcode)))
                     ;; ("SE" (123 45))
                     ((and (stringp (nth 0 postcode))
                           (integerp (nth 0 (nth 1 postcode)))
                           (integerp (nth 1 (nth 1 postcode))))
                      (format "%s-%d %d" (nth 0 postcode) (nth 0 (nth 1 postcode))
                              (nth 1 (nth 1 postcode))))
                     ;; last possibility
                     (t (format "%s" postcode)))))
            address)
          addresses))

(defun bbdb-migrate-dates (xfields)
  "Change date formats.
Formats are changed in timestamp and creation-date fields from
\"dd mmm yy\" to \"yyyy-mm-dd\"."
  (unless (stringp xfields)
    (mapc (lambda (xfield)
            (when (memq (car xfield) '(creation-date timestamp))
              (bbdb-migrate-date xfield)))
          xfields)
    xfields))

(defun bbdb-migrate-date (field)
  "Convert date field FIELD from \"dd mmm yy\" to \"yyyy-mm-dd\"."
  (let* ((date (cdr field))
         (parsed (timezone-parse-date (concat date " 00:00:00"))))
    ;; If `timezone-parse-date' cannot make sense of its arg DATE
    ;; it returns ["0" "0" "0" "0" nil].
    (if (equal parsed ["0" "0" "0" "0" nil])
        (setq parsed (timezone-parse-date date)))
    (when (equal parsed ["0" "0" "0" "0" nil])
      (cond ((string-match
              "^\\([0-9]\\{4\\}\\)[-/]\\([ 0-9]?[0-9]\\)[-/]\\([ 0-9]?[0-9]\\)" date)
             (setq parsed (vector (match-string 1 date) (match-string 2 date)
                                  (match-string 3 date))))
            ((string-match
              "^\\([ 0-9]?[0-9]\\)[-/]\\([ 0-9]?[0-9]\\)[-/]\\([0-9]\\{4\\}\\)" date)
             (setq parsed (vector (match-string 3 date) (match-string 1 date)
                                  (match-string 2 date))))))

    ;; We need numbers for the following sanity check
    (dotimes (i 3)
      (if (stringp (aref parsed i))
          (aset parsed i (string-to-number (aref parsed i)))))

    ;; Sanity check
    (if (and (< 0 (aref parsed 0))
             (< 0 (aref parsed 1)) (< (aref parsed 1) 13)
             (< 0 (aref parsed 2))
             (<= (aref parsed 2)
                 (timezone-last-day-of-month (aref parsed 1) (aref parsed 0))))
        (setcdr field (format "%04d-%02d-%02d" (aref parsed 0)
                              (aref parsed 1) (aref parsed 2)))
      (error "BBDB cannot parse %s header value %S for upgrade"
             field date))))

(defun bbdb-migrate-add-country (addrl)
  "Add a country field to each address in the address list."
  (mapcar (lambda (address) (vconcat address [bbdb-default-country])) addrl))

(defun bbdb-migrate-streets-to-list (addrl)
  "Convert the streets to a list."
  (mapcar (lambda (address)
            (vector (aref address 0) ; key
                    (delq nil (delete "" ; nuke empties
                                      (list (aref address 1) ; street1
                                            (aref address 2) ; street2
                                            (aref address 3))));street3
                    (aref address 4) ; city
                    (aref address 5) ; state
                    (aref address 6) ; postcode
                    (aref address 7))) ; country
          addrl))

(defun bbdb-migrate-xfields-to-list (xfields)
  "Migrate XFIELDS to list."
  (if (stringp xfields)
      `((notes . ,xfields))
    xfields))

(defun bbdb-migrate-organization-to-list (organization)
  "Migrate ORGANIZATION to list."
  (if (stringp organization)
      (bbdb-split 'organization organization)
    organization))

;;;###autoload
(defun bbdb-undocumented-variables (&optional name-space message)
  "Return list of undocumented variables in NAME-SPACE.
NAME-SPACE defaults to \"bbdb-\".  Use a prefix arg to specify NAME-SPACE
interactively.  If MESSAGE is non-nil (as in interactive calls) display
the list in the message area.

This command may come handy to identify BBDB variables in your init file
that are not used anymore by the current version of BBDB.  Yet this fails
for outdated BBDB variables that are set via your personal `custom-file'."
  (interactive (list (if current-prefix-arg
                         (read-string "Name space: ")) t))
  (let ((re (concat "\\`" (or name-space "bbdb-"))) list)
    (mapatoms (lambda (vv)
                (if (and (boundp vv)
                         (string-match re (symbol-name vv))
                         (not (get vv 'variable-documentation))
                         (not (get vv 'byte-obsolete-variable)))
                    (push vv list))))
    (if message
        (if list
            (apply 'message (concat "Undocumented variables: "
                                    (mapconcat (lambda (_m) "%s") list " ")) list)
          (message "No undocumented variables `%s...'" name-space)))
    list))

(provide 'bbdb-migrate)

;;; bbdb-migrate.el ends here
