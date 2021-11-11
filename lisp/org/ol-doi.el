;;; ol-doi.el --- DOI links support in Org           -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <mail@nicolasgoaziou.fr>

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

;; This library introduces the "doi" link type in Org, and provides
;; code for opening and exporting such links.

;;; Code:

(require 'ol)

(defcustom org-link-doi-server-url "https://doi.org/"
  "The URL of the DOI server."
  :group 'org-link-follow
  :version "24.3"
  :type 'string
  :safe #'stringp)

(defun org-link-doi-open (path arg)
  "Open a \"doi\" type link.
PATH is a the path to search for, as a string."
  (browse-url (url-encode-url (concat org-link-doi-server-url path)) arg))

(defun org-link-doi-export (path desc backend info)
  "Export a \"doi\" type link.
PATH is the DOI name.  DESC is the description of the link, or
nil.  BACKEND is a symbol representing the backend used for
export.  INFO is a a plist containing the export parameters."
  (let ((uri (concat org-link-doi-server-url path)))
    (pcase backend
      (`html
       (format "<a href=\"%s\">%s</a>" uri (or desc uri)))
      (`latex
       (if desc (format "\\href{%s}{%s}" uri desc)
	 (format "\\url{%s}" uri)))
      (`ascii
       (if (not desc) (format "<%s>" uri)
         (concat (format "[%s]" desc)
		 (and (not (plist-get info :ascii-links-to-notes))
		      (format " (<%s>)" uri)))))
      (`texinfo
       (if (not desc) (format "@uref{%s}" uri)
         (format "@uref{%s, %s}" uri desc)))
      (_ uri))))

(org-link-set-parameters "doi"
                         :follow #'org-link-doi-open
                         :export #'org-link-doi-export)


(provide 'org-link-doi)
(provide 'ol-doi)
;;; ol-doi.el ends here
