;;; auth-source-pass.el --- Integrate auth-source with password-store -*- lexical-binding: t -*-

;; Copyright (C) 2015, 2017 Free Software Foundation, Inc.

;; Author: Damien Cassou <damien@cassou.me>,
;;         Nicolas Petton <nicolas@petton.fr>
;; Version: 2.0.0
;; Package-Requires: ((emacs "24.4")
;; Created: 07 Jun 2015
;; Keywords: pass password-store auth-source username password login

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

;; Integrates password-store (http://passwordstore.org/) within
;; auth-source.

;;; Code:

(require 'seq)
(eval-when-compile (require 'subr-x))
(eval-when-compile
  (require 'cl-lib))
(require 'auth-source)
(require 'url-parse)

(cl-defun auth-source-pass-search (&rest spec
                                         &key backend type host user port
                                         &allow-other-keys)
  "Given a property list SPEC, return search matches from the :backend.
See `auth-source-search' for details on SPEC."
  (cl-assert (or (null type) (eq type (oref backend type)))
             t "Invalid password-store search: %s %s")
  (when (listp host)
    ;; Take the first non-nil item of the list of hosts
    (setq host (seq-find #'identity host)))
  (list (auth-source-pass--build-result host port user)))

(defun auth-source-pass--build-result (host port user)
  "Build auth-source-pass entry matching HOST, PORT and USER."
  (let ((entry (auth-source-pass--find-match host user)))
    (when entry
      (let ((retval (list
                     :host host
                     :port (or (auth-source-pass-get "port" entry) port)
                     :user (or (auth-source-pass-get "user" entry) user)
                     :secret (lambda () (auth-source-pass-get 'secret entry)))))
        (auth-source-pass--do-debug "return %s as final result (plus hidden password)"
                                    (seq-subseq retval 0 -2)) ;; remove password
        retval))))

;;;###autoload
(defun auth-source-pass-enable ()
  "Enable auth-source-password-store."
  ;; To add password-store to the list of sources, evaluate the following:
  (add-to-list 'auth-sources 'password-store)
  ;; clear the cache (required after each change to #'auth-source-pass-search)
  (auth-source-forget-all-cached))

(defvar auth-source-pass-backend
  (auth-source-backend
   (format "Password store")
   :source "." ;; not used
   :type 'password-store
   :search-function #'auth-source-pass-search)
  "Auth-source backend for password-store.")

(defun auth-source-pass-backend-parse (entry)
  "Create a password-store auth-source backend from ENTRY."
  (when (eq entry 'password-store)
    (auth-source-backend-parse-parameters entry auth-source-pass-backend)))

(add-hook 'auth-source-backend-parser-functions #'auth-source-pass-backend-parse)


(defun auth-source-pass-get (key entry)
  "Return the value associated to KEY in the password-store entry ENTRY.

ENTRY is the name of a password-store entry.
The key used to retrieve the password is the symbol `secret'.

The convention used as the format for a password-store file is
the following (see http://www.passwordstore.org/#organization):

secret
key1: value1
key2: value2"
  (let ((data (auth-source-pass-parse-entry entry)))
    (or (cdr (assoc key data))
        (and (string= key "user")
             (cdr (assoc "username" data))))))

(defun auth-source-pass--read-entry (entry)
  "Return a string with the file content of ENTRY."
  (with-temp-buffer
    (insert-file-contents (expand-file-name
                           (format "%s.gpg" entry)
                           "~/.password-store"))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun auth-source-pass-parse-entry (entry)
  "Return an alist of the data associated with ENTRY.

ENTRY is the name of a password-store entry."
  (let ((file-contents (ignore-errors (auth-source-pass--read-entry entry))))
    (and file-contents
         (cons `(secret . ,(auth-source-pass--parse-secret file-contents))
               (auth-source-pass--parse-data file-contents)))))

(defun auth-source-pass--parse-secret (contents)
  "Parse the password-store data in the string CONTENTS and return its secret.
The secret is the first line of CONTENTS."
  (car (split-string contents "\\\n" t)))

(defun auth-source-pass--parse-data (contents)
  "Parse the password-store data in the string CONTENTS and return an alist.
CONTENTS is the contents of a password-store formatted file."
  (let ((lines (split-string contents "\\\n" t "\\\s")))
    (seq-remove #'null
                (mapcar (lambda (line)
                          (let ((pair (mapcar (lambda (s) (string-trim s))
                                              (split-string line ":"))))
                            (when (> (length pair) 1)
                              (cons (car pair)
                                    (mapconcat #'identity (cdr pair) ":")))))
                        (cdr lines)))))

(defun auth-source-pass--hostname (host)
  "Extract hostname from HOST."
  (let ((url (url-generic-parse-url host)))
    (or (url-host url) host)))

(defun auth-source-pass--hostname-with-user (host)
  "Extract hostname and user from HOST."
  (let* ((url (url-generic-parse-url host))
         (user (url-user url))
         (hostname (url-host url)))
    (cond
     ((and user hostname) (format "%s@%s" user hostname))
     (hostname hostname)
     (t host))))

(defun auth-source-pass--user (host)
  "Extract user from HOST and return it.
Return nil if no match was found."
  (url-user (url-generic-parse-url host)))

(defun auth-source-pass--do-debug (&rest msg)
  "Call `auth-source-do-debug` with MSG and a prefix."
  (apply #'auth-source-do-debug
         (cons (concat "auth-source-password-store: " (car msg))
               (cdr msg))))

(defun auth-source-pass--select-one-entry (entries user)
  "Select one entry from ENTRIES by searching for a field matching USER."
  (let ((number (length entries))
        (entry-with-user
         (and user
              (seq-find (lambda (entry)
                          (string-equal (auth-source-pass-get "user" entry) user))
                        entries))))
    (auth-source-pass--do-debug "found %s matches: %s" number
                                (mapconcat #'identity entries ", "))
    (if entry-with-user
        (progn
          (auth-source-pass--do-debug "return %s as it contains matching user field"
                                      entry-with-user)
          entry-with-user)
      (auth-source-pass--do-debug "return %s as it is the first one" (car entries))
      (car entries))))

(defun auth-source-pass--entry-valid-p (entry)
  "Return t iff ENTRY can be opened.
Also displays a warning if not.  This function is slow, don't call it too
often."
  (if (auth-source-pass-parse-entry entry)
      t
    (auth-source-pass--do-debug "entry '%s' is not valid" entry)
    nil))

;; TODO: add tests for that when `assess-with-filesystem' is included
;; in Emacs
(defun auth-source-pass-entries ()
  "Return a list of all password store entries."
  (let ((store-dir (expand-file-name "~/.password-store/")))
    (mapcar
     (lambda (file) (file-name-sans-extension (file-relative-name file store-dir)))
     (directory-files-recursively store-dir "\.gpg$"))))

(defun auth-source-pass--find-all-by-entry-name (entryname user)
  "Search the store for all entries either matching ENTRYNAME/USER or ENTRYNAME.
Only return valid entries as of `auth-source-pass--entry-valid-p'."
  (seq-filter (lambda (entry)
                (and
                 (or
                  (let ((components-host-user
                         (member entryname (split-string entry "/"))))
                    (and (= (length components-host-user) 2)
                         (string-equal user (cadr components-host-user))))
                  (string-equal entryname (file-name-nondirectory entry)))
                 (auth-source-pass--entry-valid-p entry)))
              (auth-source-pass-entries)))

(defun auth-source-pass--find-one-by-entry-name (entryname user)
  "Search the store for an entry matching ENTRYNAME.
If USER is non nil, give precedence to entries containing a user field
matching USER."
  (auth-source-pass--do-debug "searching for '%s' in entry names (user: %s)"
                              entryname
                              user)
  (let ((matching-entries (auth-source-pass--find-all-by-entry-name entryname user)))
    (pcase (length matching-entries)
      (0 (auth-source-pass--do-debug "no match found")
         nil)
      (1 (auth-source-pass--do-debug "found 1 match: %s" (car matching-entries))
         (car matching-entries))
      (_ (auth-source-pass--select-one-entry matching-entries user)))))

(defun auth-source-pass--find-match (host user)
  "Return a password-store entry name matching HOST and USER.
If many matches are found, return the first one.  If no match is
found, return nil."
  (or
   (if (auth-source-pass--user host)
       ;; if HOST contains a user (e.g., "user@host.com"), <HOST>
       (auth-source-pass--find-one-by-entry-name (auth-source-pass--hostname-with-user host) user)
     ;; otherwise, if USER is provided, search for <USER>@<HOST>
     (when (stringp user)
       (auth-source-pass--find-one-by-entry-name (concat user "@" (auth-source-pass--hostname host)) user)))
   ;; if that didn't work, search for HOST without its user component, if any
   (auth-source-pass--find-one-by-entry-name (auth-source-pass--hostname host) user)
   ;; if that didn't work, search for HOST with user extracted from it
   (auth-source-pass--find-one-by-entry-name
    (auth-source-pass--hostname host) (auth-source-pass--user host))
   ;; if that didn't work, remove subdomain: foo.bar.com -> bar.com
   (let ((components (split-string host "\\.")))
     (when (= (length components) 3)
       ;; start from scratch
       (auth-source-pass--find-match (mapconcat 'identity (cdr components) ".") user)))))

(provide 'auth-source-pass)
;;; auth-source-pass.el ends here
