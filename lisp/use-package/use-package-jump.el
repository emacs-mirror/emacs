;;; use-package-jump.el --- Attempt to jump to a use-package declaration  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2024 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>

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

;; Provides the command `M-x use-package-jump-to-package-form'.  However, it
;; only works if the package being jumped to was required during
;; initialization.  If it was autoloaded, it will not work.
;; Improvements are needed.
;;
;; See the `use-package' info manual for more information.

;;; Code:

(require 'use-package-core)

(defun use-package-find-require (package)
  "Find file that required PACKAGE by searching `load-history'.
Returns an absolute file path or nil if none is found."
  (catch 'suspect
    (dolist (filespec load-history)
      (dolist (entry (cdr filespec))
        (when (equal entry (cons 'require package))
          (throw 'suspect (car filespec)))))))

;;;###autoload
(defun use-package-jump-to-package-form (package)
  "Attempt to find and jump to the `use-package' form that loaded PACKAGE.
This will only find the form if that form actually required
PACKAGE.  If PACKAGE was previously required then this function
will jump to the file that originally required PACKAGE instead."
  (interactive (list (completing-read "Package: " features)))
  (let* ((package (if (stringp package) (intern package) package))
         (requiring-file (use-package-find-require package))
         file location)
    (if (null requiring-file)
        (user-error "Can't find file requiring file; may have been autoloaded")
      (setq file (if (string= (file-name-extension requiring-file) "elc")
                     (concat (file-name-sans-extension requiring-file) ".el")
                   requiring-file))
      (when (file-exists-p file)
        (find-file-other-window file)
        (save-excursion
          (goto-char (point-min))
          (setq location
                (re-search-forward
                 (format (eval use-package-form-regexp-eval) package) nil t)))
        (if (null location)
            (message "No use-package form found.")
          (goto-char location)
          (beginning-of-line))))))

(provide 'use-package-jump)

;;; use-package-jump.el ends here
