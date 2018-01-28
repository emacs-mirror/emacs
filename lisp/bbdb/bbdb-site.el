;;; bbdb-site.el --- site-specific variables for BBDB -*- lexical-binding: t -*-

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

;;; Code:

(defconst bbdb-version "@PACKAGE_VERSION@" "Version of BBDB.")

(if (< emacs-major-version 24)
  (error "BBDB %s requires GNU Emacs 24 or later" bbdb-version))

(defcustom bbdb-tex-path
  (let* ((default "@pkgdatadir@")
         (dir (cond ((file-accessible-directory-p default)
                     default)
                    (load-file-name
                     (expand-file-name "tex/" (file-name-directory load-file-name)))
                    (t
                     (let ((f (locate-file "tex/bbdb.sty" load-path)))
                       (if f (file-name-directory f)))))))
    (if dir (list dir)))
  "List of directories with the BBDB TeX files.
If this is t assume that these files reside in directories
that are part of the regular TeX search path."
  :group 'bbdb-utilities-tex
  :type '(choice (const :tag "Files in TeX path" t)
                 (repeat (directory :tag "Directory"))))

(provide 'bbdb-site)

;;; bbdb-site.el ends here
