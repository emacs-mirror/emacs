;;; use-package-ensure-system-package.el --- auto install system packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Justin Talbott <justin@waymondo.com>
;; Keywords: convenience, tools, extensions
;; URL: https://github.com/waymondo/use-package-ensure-system-package
;; Version: 0.2
;; Package-Requires: ((use-package "2.1") (system-packages "1.0.4"))
;; Filename: use-package-ensure-system-package.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The `:ensure-system-package` keyword allows you to ensure system
;; binaries exist alongside your `use-package` declarations.
;;

;;; Code:

(require 'use-package)
(require 'system-packages nil t)

(eval-when-compile
  (declare-function system-packages-get-command "system-packages"))

(defvar use-package-ensure-system-package--custom-packages '()
  "List of custom packages installed.")

(defun use-package-ensure-system-package-consify (arg)
  "Turn ARG into a cons of (`package-name' . `install-command')."
  (cond
   ((stringp arg)
    (cons arg `(system-packages-install ,arg)))
   ((symbolp arg)
    (cons arg `(system-packages-install ,(symbol-name arg))))
   ((consp arg)
    (cond
     ((not (cdr arg))
      (use-package-ensure-system-package-consify (car arg)))
     ((stringp (cdr arg))
      (progn
	(push (cdr arg) use-package-ensure-system-package--custom-packages)
	(cons (car arg) `(async-shell-command ,(cdr arg)))))
     (t
      (cons (car arg)
	    `(system-packages-install ,(symbol-name (cdr arg)))))))))

(defun use-package-ensure-system-package-update-custom-packages ()
  (interactive)
  (dolist (cmd use-package-ensure-system-package--custom-packages)
    (async-shell-command cmd)))

;;;###autoload
(defun use-package-normalize/:ensure-system-package (_name-symbol keyword args)
  "Turn ARGS into a list of conses of (`package-name' . `install-command')."
  (use-package-as-one (symbol-name keyword) args
    (lambda (_label arg)
      (cond
       ((and (listp arg) (listp (cdr arg)))
        (mapcar #'use-package-ensure-system-package-consify arg))
       (t
        (list (use-package-ensure-system-package-consify arg)))))))

(defun use-package-ensure-system-package-exists? (file-or-exe)
  "If variable is a string, ensure the file path exists.
If it is a symbol, ensure the binary exist."
  (if (stringp file-or-exe)
      (file-exists-p file-or-exe)
    (executable-find (symbol-name file-or-exe))))


;;;###autoload
(defun use-package-handler/:ensure-system-package (name _keyword arg rest state)
  "Execute the handler for `:ensure-system-package' keyword in `use-package'."
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (mapcar #'(lambda (cons)
                 `(unless (use-package-ensure-system-package-exists? ',(car cons))
		    ,(cdr cons))) arg)
     body)))

(add-to-list 'use-package-keywords :ensure-system-package t)

(provide 'use-package-ensure-system-package)

;;; use-package-ensure-system-package.el ends here
