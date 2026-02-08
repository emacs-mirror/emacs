;;; eudcb-ecomplete.el --- EUDC - ecomplete backend -*- lexical-binding: t -*-

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

;;    This library provides an interface to the ecomplete package as
;;    an EUDC data source.

;;; Usage:

;;    No setup is required, since there is an entry for this backend
;;    in `eudc-server-hotlist' by default.
;;
;;    For example, if your `ecomplete-database-file' (typically
;;    ~/.emacs.d/ecompleterc) contains:
;;
;;    ((mail ("larsi@gnus.org" 38154 1516109510 "Lars <larsi@ecomplete.org>")))
;;
;;    Then:
;;
;;    C-x m lars C-u M-x eudc-expand-try-all RET
;;
;;    should expand the email address into the To: field of the new
;;    message.

;;; Code:

(require 'eudc)
(require 'ecomplete)
(require 'mail-parse)

(defvar eudc-ecomplete-attributes-translation-alist
  '((email     . mail))
  "See `eudc-protocol-attributes-translation-alist'.
The back-end-specific attribute names are used as the \"type\" of
entry when searching, and they must hence match the types you use
in your ecompleterc database file.")

;; hook ourselves into the EUDC framework
(eudc-protocol-set 'eudc-query-function
		   'eudc-ecomplete-query-internal
		   'ecomplete)
(eudc-protocol-set 'eudc-list-attributes-function
		   nil
		   'ecomplete)
(eudc-protocol-set 'eudc-protocol-attributes-translation-alist
		   'eudc-ecomplete-attributes-translation-alist
		   'ecomplete)
(eudc-protocol-set 'eudc-protocol-has-default-query-attributes
		   nil
		   'ecomplete)

;;;###autoload
(defun eudc-ecomplete-query-internal (query &optional _return-attrs)
  "Query `ecomplete' with QUERY.
QUERY is a list of cons cells (ATTR . VALUE).  Since `ecomplete'
does not provide attributes in the usual sense, the
back-end-specific attribute names in
`eudc-ecomplete-attributes-translation-alist' are used as the
KEY (that is, the \"type\" of match) when looking for matches in
`ecomplete-database'.

RETURN-ATTRS is ignored." ; FIXME: why is this being ignored?
  (ecomplete-setup)
  (let ((email-attr (car (eudc-translate-attribute-list '(email))))
        result)
    (dolist (term query)
      (let* ((attr (car term))
             (value (cdr term))
             (matches (ecomplete-get-matches attr value)))
        (when matches
          (dolist (match (split-string (string-trim (substring-no-properties
                                                     matches))
                                       "[\n\r]"))
            ;; Try to decompose the email address.
            (let* ((decoded (mail-header-parse-address match t))
                   (name (cdr decoded))
                   (email (car decoded)))
              (if (and decoded (eq attr email-attr))
                  ;; The email could be decomposed, push individual
                  ;; fields.
                  (push `((,attr . ,email)
                          ,@(when name (list (cons 'name name))))
                        result)
                ;; Otherwise just forward the value as-is.
                (push (list (cons attr match)) result)))))))
    result))

(eudc-register-protocol 'ecomplete)

(provide 'eudcb-ecomplete)
;;; eudcb-ecomplete.el ends here
