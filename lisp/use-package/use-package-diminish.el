;;; use-package-diminish.el --- Support for the :diminish keyword  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2022 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>

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

;; Provides support for the :diminish keyword, which is made available by
;; default by requiring `use-package'.

;;; Code:

(require 'use-package-core)

(defun use-package-normalize-diminish (name label arg &optional recursed)
  "Normalize the arguments to diminish down to a list of one of two forms:
     SYMBOL
     (SYMBOL . STRING)"
  (cond
   ((not arg)
    (list (use-package-as-mode name)))
   ((use-package-non-nil-symbolp arg)
    (list arg))
   ((stringp arg)
    (list (cons (use-package-as-mode name) arg)))
   ((and (consp arg) (stringp (cdr arg)))
    (list arg))
   ((and (not recursed) (listp arg) (listp (cdr arg)))
    (mapcar #'(lambda (x) (car (use-package-normalize-diminish
                           name label x t))) arg))
   (t
    (use-package-error
     (concat label " wants a string, symbol, "
             "(symbol . string) or list of these")))))

;;;###autoload
(defun use-package-normalize/:diminish (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    (apply-partially #'use-package-normalize-diminish name) t))

;;;###autoload
(defun use-package-handler/:diminish (name _keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (mapcar #'(lambda (var)
                 `(if (fboundp 'diminish)
                      ,(if (consp var)
                           `(diminish ',(car var) ,(cdr var))
                         `(diminish ',var))))
             arg)
     body)))

(add-to-list 'use-package-keywords :diminish t)

(provide 'use-package-diminish)

;;; use-package-diminish.el ends here
