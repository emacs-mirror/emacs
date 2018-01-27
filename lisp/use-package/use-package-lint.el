;;; use-package-lint.el --- Attempt to find errors in use-package declarations  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2017 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Created: 17 Jun 2012
;; Modified: 3 Dec 2017
;; Version: 1.0
;; Package-Requires: ((emacs "24.3") (use-package "2.4"))
;; Keywords: dotemacs startup speed config package
;; URL: https://github.com/jwiegley/use-package

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Provides the command `M-x use-package-lint'.

;;; Code:

(require 'cl-lib)
(require 'use-package-core)

(defun use-package-lint-declaration (name plist)
  (dolist (path (plist-get plist :load-path))
    (unless (file-exists-p path)
      (display-warning
       'use-package
       (format "%s :load-path does not exist: %s"
               name path) :error)))

  (unless (or (plist-member plist :disabled)
              (plist-get plist :no-require)
              (locate-library (use-package-as-string name) nil
                              (plist-get plist :load-path)))
    (display-warning
     'use-package
     (format "%s module cannot be located" name) :error))

  ;; (dolist (command (plist-get plist :commands))
  ;;   (unless (string= (find-lisp-object-file-name command nil)
  ;;                    (locate-library (use-package-as-string name) nil
  ;;                                    (plist-get plist :load-path)))
  ;;     (display-warning
  ;;      'use-package
  ;;      (format "%s :command is from different path: %s"
  ;;              name (symbol-name command)) :error)))
  )

;;;###autoload
(defun use-package-lint ()
  "Check for errors in use-package declarations.
For example, if the module's `:if' condition is met, but even
with the specified `:load-path' the module cannot be found."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((re (eval use-package-form-regexp-eval)))
      (while (re-search-forward re nil t)
        (goto-char (match-beginning 0))
        (let ((decl (read (current-buffer))))
          (when (eq (car decl) 'use-package)
            (use-package-lint-declaration
             (use-package-as-string (cadr decl))
             (use-package-normalize-keywords
              (cadr decl) (cddr decl)))))))))

(provide 'use-package-lint)

;;; use-package-lint.el ends here
