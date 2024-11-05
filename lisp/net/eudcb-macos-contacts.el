;;; eudcb-macos-contacts.el --- EUDC - macOS Contacts backend  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

;;    This library provides an interface to the macOS Contacts app as
;;    an EUDC data source.  It uses AppleScript to interface with the
;;    Contacts app on localhost, so no 3rd party tools are needed.

;;; Usage:

;;    To load the library, first `require' it:
;;
;;      (require 'eudcb-macos-contacts)
;;
;;    In the simplest case then just use:
;;
;;      (eudc-macos-contacts-set-server "localhost")
;;
;;    When using `eudc-server-hotlist', instead use:
;;
;;      (add-to-list 'eudc-server-hotlist '("localhost" . macos-contacts))

;;; Code:

(require 'eudc)
(require 'executable)

;;{{{      Internal cooking

(defvar eudc-macos-contacts-attributes-translation-alist
  '((name      . last_name)
    (firstname . first_name)
    (email     . email)
    (phone     . phone)
    (title     . job_title)
    (o         . organization)
    (ou        . department))
  "See `eudc-protocol-attributes-translation-alist'.")

(defconst eudc-macos-contacts--unsearchable-attributes
  '(email phone)
  "See `eudc-macos-contacts-search-helper'.")

;; hook ourselves into the EUDC framework
(eudc-protocol-set 'eudc-query-function
                   'eudc-macos-contacts-query-internal
                   'macos-contacts)
(eudc-protocol-set 'eudc-list-attributes-function
                   nil
                   'macos-contacts)
(eudc-protocol-set 'eudc-protocol-attributes-translation-alist
                   'eudc-macos-contacts-attributes-translation-alist
                   'macos-contacts)
(eudc-protocol-set 'eudc-protocol-has-default-query-attributes
                   nil
                   'macos-contacts)

(defun eudc-macos-contacts-search-helper (query)
  "Helper function to query the Contacts app via AppleScript.
Searches for all persons matching QUERY.  QUERY is a list of cons
cells (ATTR . VALUE) where ATTRs should be valid macOS Contacts
attribute names with space characters replaced by `_' characters.
Thus, to for instance search for the \"first name\" attribute in
the Contacts app, the corresponding ATTR would be the symbol
`first_name'.

Note that due to the way the Contacts app exposes its data via
AppleScript, the attributes listed in
`eudc-macos-contacts--unsearchable-attributes' can not be searched
efficiently.  If and when one of these attributes appears in
QUERY, it is thus skipped, and the query is composed from the
other attributes in the QUERY."
  (let ((crit-idx 0)
        (query-str (string)))
    ;; assemble a query string for use in an AppleScript "whose"
    ;; filter clause; generally, this has the form
    ;; (ATTR1 contains "VALUE1") and (ATTR2 contains "VALUE2") and ...
    (dolist (criterion query)
      (let ((attr (string-replace "_" " " (symbol-name (car criterion))))
            (term (cdr criterion)))
        ;; defend against unusable attribute names as they cause
        ;; AppleScript to emit an error message, which in turn will
        ;; cause elisp errors during results parsing in
        ;; `eudc-macos-contacts-query-internal'
        (if (or (not (rassq (car criterion)
                            eudc-macos-contacts-attributes-translation-alist))
                (memq (car criterion)
                      eudc-macos-contacts--unsearchable-attributes))
            (message (concat "[eudc] Warning in macOS Contacts backend: "
                             "can not search in attribute "
                             (format "\"%s\"; skipping it."  attr)))
          (progn
            (when (> crit-idx 0)
              (setq query-str (concat query-str " and ")))
            (setq query-str (concat query-str
                                    (format "(%s contains \"%s\")" attr term)))
            (setq crit-idx (1+ crit-idx))))))
    ;; if a useful query string could be assembled, insert it into the
    ;; AppleScript template, and run the resulting script; results are
    ;; captured in the current buffer
    (if (not (string= query-str ""))
        (if (executable-find "osascript")
            (call-process "osascript" nil t nil
                          "-e"
                          (format "
on joinLines(theText)
        if (theText is missing value) or (theText is \"\") then
                return \"\"
        else
                set thePars to paragraphs of theText
                set result to {}
                repeat with para in thePars
                        set result to result & {para & space}
                end repeat
                return text 1 thru -2 of (result as text)
        end if
end joinLines

on run
        set results to {}
        tell application \"Address Book\"
                set pList to every person whose %s
                repeat with pers in pList
                        set pText to ¬
                                first name of pers & \":\" & ¬
                                last name of pers & \":\"
                        if (job title of pers is not missing value) then ¬
                                set pText to pText ¬
                                        & my joinLines(job title of pers)
                        set pText to pText & \":\"
                        if (department of pers is not missing value) then ¬
                                set pText to pText ¬
                                        & my joinLines(department of pers)
                        set pText to pText & \":\"
                        if (organization of pers is not missing value) then ¬
                                set pText to pText ¬
                                        & my joinLines(organization of pers)
                        set pText to pText & \":\"
                        if (count emails of pers) > 0 then
                                repeat with emailAddr in emails of pers
                                        set pText to pText & value ¬
                                                of emailAddr & \",\"
                                end repeat
                                set pText to text 1 thru -2 of pText
                        end if
                        set pText to pText & \":\"
                        if (count phones of pers) > 0 then
                                repeat with phoneNmbr in phones of pers
                                        set pText to pText & value ¬
                                                of phoneNmbr & \",\"
                                end repeat
                                set pText to text 1 thru -2 of pText
                        end if
                        set results to results & {pText & \"\n\"}
                end repeat
                get results as text
        end tell
end run
" query-str))
          (message (concat "[eudc] Error in macOS Contacts backend: "
                           "`osascript' executable not found. "
                           "Is this is a macOS 10.0 or later system?"))))))

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
      (eudc-macos-contacts-search-helper query)
      (delete-duplicate-lines (point-min) (point-max))
      (goto-char (point-min))
      (while (not (eobp))
        (if (not (equal (line-beginning-position) (line-end-position)))
            (let ((keys '(first_name last_name job_title department
                          organization email phone))
                  record)
              (dolist (field (split-string (buffer-substring
                                            (point) (line-end-position))
                                           ":"))
                (let ((key (pop keys)))
                  (unless (string= "" field)
                    (pcase key
                      ((or 'email 'phone) (dolist (x (split-string field ","))
                                            (push (cons key x) record)))
                      (_ (push (cons key field) record))))))
              (unless (length= record 0)
                (push (nreverse record) result))))
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
