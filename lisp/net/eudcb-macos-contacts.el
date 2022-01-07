;;; eudcb-macos-contacts.el --- EUDC - macOS Contacts backend  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022 Free Software Foundation, Inc.

;; Author: Alexander Adolf

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
;;    This library provides an interface to the macOS Contacts app as
;;    an EUDC data source.  It uses AppleScript to interface with the
;;    Contacts app on localhost, so no 3rd party tools are needed.

;;; Usage:
;;    (require 'eudcb-macos-contacts)
;;    (eudc-macos-contacts-set-server "localhost")

;;; Code:

(require 'eudc)
(require 'executable)

;;{{{      Internal cooking

(defvar eudc-macos-contacts-conversion-alist nil)

;; hook ourselves into the EUDC framework
(eudc-protocol-set 'eudc-query-function
		   'eudc-macos-contacts-query-internal
		   'macos-contacts)
(eudc-protocol-set 'eudc-list-attributes-function
		   nil
		   'macos-contacts)
(eudc-protocol-set 'eudc-macos-contacts-conversion-alist
		   nil
		   'macos-contacts)
(eudc-protocol-set 'eudc-protocol-has-default-query-attributes
		   nil
		   'macos-contacts)

(defun eudc-macos-contacts-search-helper (str)
  "Helper function to query the Contacts app via AppleScript.
Searches for all persons with a case-insensitive substring match
of STR in any of their name fields (first, middle, or last)."
  (if (executable-find "osascript")
      (call-process "osascript" nil t nil
		    "-e"
		    (format "
set results to {}
tell application \"Address Book\"
	set pList to every person whose (name contains \"%s\")
	repeat with pers in pList
		repeat with emailAddr in emails of pers
			set results to results & {name of pers & \":\" & value ¬
			of emailAddr & \"\n\"}
		end repeat
	end repeat
	get results as text
end tell" str))
    (message (concat "[eudc] Error in macOS Contacts backend: "
		     "`osascript' executable not found. "
		     "Is this is a macOS 10.0 or later system?"))))

(defun eudc-macos-contacts-query-internal (query &optional _return-attrs)
  "Query macOS Contacts with QUERY.
QUERY is a list of cons cells (ATTR . VALUE) where ATTRs should be valid
macOS Contacts attribute names.
RETURN-ATTRS is a list of attributes to return, defaulting to
`eudc-default-return-attributes'."
  (let ((macos-contacts-buffer (get-buffer-create " *macOS Contacts*"))
	result)
    (with-current-buffer macos-contacts-buffer
      (erase-buffer)
      (dolist (term query)
	(eudc-macos-contacts-search-helper (cdr term)))
      (delete-duplicate-lines (point-min) (point-max))
      (goto-char (point-min))
      (while (not (eobp))
	(if (not (equal (line-beginning-position) (line-end-position)))
	    (let* ((args (split-string (buffer-substring
					(point) (line-end-position))
				       ":"))
		   (name (nth 0 args))
		   (email (nth 1 args)))
	      (setq result (cons `((name . ,name)
				   (email . ,email))
				 result))))
	(forward-line))
      result)))

;;}}}

;;{{{      High-level interfaces (interactive functions)

(defun eudc-macos-contacts-set-server (dummy)
  "Set the EUDC server to macOS Contacts app.
The server in DUMMY is not actually used, since this backend
always and implicitly connects to an instance of the Contacts app
running on the local host."
  (interactive)
  (eudc-set-server dummy 'macos-contacts)
  (message "[eudc] macOS Contacts app server selected"))

;;}}}

(eudc-register-protocol 'macos-contacts)

(provide 'eudcb-macos-contacts)

;;; eudcb-macos-contacts.el ends here
