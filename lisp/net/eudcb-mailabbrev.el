;;; eudcb-mailabbrev.el --- EUDC - mailabbrev backend -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

;; Author: Alexander Adolf
;; Package: eudc

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

;;    This library provides an interface to the mailabbrev package as
;;    an EUDC data source.

;;; Usage:

;;    No setup is required, since there is an entry for this backend
;;    in `eudc-server-hotlist' by default.
;;
;;    For example, if your `mail-personal-alias-file' (typically
;;    ~/.mailrc) contains:
;;
;;    alias lars "Lars <larsi@mail-abbrev.com>"
;;
;;    Then:
;;
;;    C-x m lars C-u M-x eudc-expand-try-all RET
;;
;;    will expand the correct email address into the To: field of the
;;    new message.

;;; Code:

(require 'eudc)
(require 'mailabbrev)
(require 'mail-parse)

;; hook ourselves into the EUDC framework
(eudc-protocol-set 'eudc-query-function
		   'eudc-mailabbrev-query-internal
		   'mailabbrev)
(eudc-protocol-set 'eudc-list-attributes-function
		   nil
		   'mailabbrev)
(eudc-protocol-set 'eudc-protocol-attributes-translation-alist
		   nil
		   'mailabbrev)
(eudc-protocol-set 'eudc-protocol-has-default-query-attributes
		   nil
		   'mailabbrev)
;;;###autoload
(defun eudc-mailabbrev-query-internal (query &optional _return-attrs)
  "Query `mailabbrev' with QUERY.
QUERY is a list of cons cells (ATTR . VALUE).  Since `mailabbrev'
does not provide attributes in the usual sense, only the email,
name, and firstname attributes in the QUERY are considered, and
their values are matched against the alias names in the mailrc
file.  When a mailrc alias is a distribution list, that is it
expands to more that one email address, the individual recipient
specifications are formatted using `eudc-rfc5322-make-address',
and returned as a comma-separated list in the email address
attribute.

RETURN-ATTRS is a list of attributes to return, defaulting to
`eudc-default-return-attributes'."
  (mail-abbrevs-setup)
  (let (result)
    (dolist (term query)
      (let* ((attr (car term))
             (value (cdr term))
             (soft (intern-soft value mail-abbrevs))
             (raw-matches (and
                           (boundp soft)
                           (symbol-value soft))))
        (when (and raw-matches
                   (memq attr '(email firstname name)))
          (let* ((matches (split-string raw-matches ", "))
                 (num-matches (length matches)))
            (if (> num-matches 1)
                ;; multiple matches: distribution list
                (let ((distr-str (string)))
                  (dolist (recipient matches)
                    ;; try to decompose email construct
                    (let* ((decoded (mail-header-parse-address recipient t))
                           (name (cdr decoded))
                           (email (car decoded)))
                      (if decoded
                          ;; decoding worked, push rfc5322 rendered address
                          (setq distr-str
                                (copy-sequence
                                 (concat distr-str ", "
                                         (eudc-rfc5322-make-address email
                                                                    nil
                                                                    name))))
                        ;; else, just forward the value as-is
                        (setq distr-str
                              (copy-sequence
                               (concat distr-str ", " recipient))))))
                  ;; push result, removing the leading ", "
                  (push (list (cons 'email (substring distr-str 2 -1)))
                        result))
              ;; simple case: single match
              (let* ((match (car matches))
                     (decoded (mail-header-parse-address match t))
                     (name (cdr decoded))
                     (email (car decoded)))
                (if decoded
                    ;; decoding worked, push individual fields
                    (push `((email . ,email)
                            ,@(when name (list (cons 'name name))))
                          result)
                  ;; else, just forward the value as-is
                  (push (list (cons 'email match)) result))))))))
    result))

(eudc-register-protocol 'mailabbrev)

(provide 'eudcb-mailabbrev)

;;; eudcb-mailabbrev.el ends here
