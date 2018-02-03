;;; url-tramp.el --- file-name-handler magic invoking Tramp for some protocols

;; Copyright (C) 2014-2018 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, data, processes, hypermedia

;; This file is part of GNU Emacs.
;;
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

;;; Code:

(require 'url-parse)
(require 'tramp)
(require 'password-cache)

;;;###autoload
(defcustom url-tramp-protocols '("ftp" "ssh" "scp" "rsync" "telnet")
  "List of URL protocols for which the work is handled by Tramp.
They must also be covered by `url-handler-regexp'."
  :group 'url
  :version "25.1"
  :type '(repeat string))

(defun url-tramp-convert-url-to-tramp (url)
  "Convert URL to a Tramp file name.
If URL contains a password, it will be added to the `password-data' cache.
In case URL is not convertible, nil is returned."
  (let* ((obj (url-generic-parse-url (and (stringp url) url)))
         (port
          (and (natnump (url-portspec obj))
               (number-to-string (url-portspec obj)))))
    (when (member (url-type obj) url-tramp-protocols)
      (when (url-password obj)
	(password-cache-add
	 (tramp-make-tramp-file-name
	  (url-type obj) (url-user obj) nil
          (url-host obj) port "")
	 (url-password obj)))
      (tramp-make-tramp-file-name
       (url-type obj) (url-user obj) nil
       (url-host obj) port (url-filename obj)))))

(defun url-tramp-convert-tramp-to-url (file)
  "Convert FILE, a Tramp file name, to a URL.
In case FILE is not convertible, nil is returned."
  (let* ((obj (ignore-errors (tramp-dissect-file-name file)))
         (port
          (and (stringp (tramp-file-name-port obj))
               (string-to-number (tramp-file-name-port obj)))))
    (when (member (tramp-file-name-method obj) url-tramp-protocols)
      (url-recreate-url
       (url-parse-make-urlobj
	(tramp-file-name-method obj)
	(tramp-file-name-user obj)
	nil ; password.
	(tramp-file-name-host obj)
	port
	(tramp-file-name-localname obj)
	nil nil t))))) ; target attributes fullness.

;;;###autoload
(defun url-tramp-file-handler (operation &rest args)
  "Function called from the `file-name-handler-alist' routines.
OPERATION is what needs to be done.  ARGS are the arguments that
would have been passed to OPERATION."
  (let ((default-directory (url-tramp-convert-url-to-tramp default-directory))
	(args (mapcar 'url-tramp-convert-url-to-tramp args)))
    (url-tramp-convert-tramp-to-url (apply operation args))))

(provide 'url-tramp)

;;; url-tramp.el ends here
