;;; use-package-delight.el --- Support for the :delight keyword  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2023 Free Software Foundation, Inc.

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

;; Provides support for the :delight keyword, which is made available
;; by default by requiring `use-package'.  Using it requires the
;; `delight' package to be installed (available on GNU ELPA).
;;
;; See the `use-package' info manual for more information.

;;; Code:

(require 'use-package-core)

(defun use-package-normalize-delight (name args)
  "Normalize ARGS for a single call to `delight'."
  (when (eq :eval (car args))
    ;; Handle likely common mistake.
    (use-package-error ":delight mode line constructs must be quoted"))
  (cond ((and (= (length args) 1)
              (use-package-non-nil-symbolp (car args)))
         `(,(nth 0 args) nil ,name))
        ((= (length args) 2)
         `(,(nth 0 args) ,(nth 1 args) ,name))
        ((= (length args) 3)
         args)
        (t
         (use-package-error
          ":delight expects `delight' arguments or a list of them"))))

;;;###autoload
(defun use-package-normalize/:delight (name _keyword args)
  "Normalize arguments to delight."
  (cond ((null args)
         `((,(use-package-as-mode name) nil ,name)))
        ((and (= (length args) 1)
              (use-package-non-nil-symbolp (car args)))
         `((,(car args) nil ,name)))
        ((and (= (length args) 1)
              (stringp (car args)))
         `((,(use-package-as-mode name) ,(car args) ,name)))
        ((and (= (length args) 1)
              (listp (car args))
              (eq 'quote (caar args)))
         `((,(use-package-as-mode name) ,@(cdar args) ,name)))
        ((and (= (length args) 2)
              (listp (nth 1 args))
              (eq 'quote (car (nth 1 args))))
         `((,(car args) ,@(cdr (nth 1 args)) ,name)))
        (t (mapcar
            (apply-partially #'use-package-normalize-delight name)
            (if (use-package-non-nil-symbolp (car args))
                (list args)
              args)))))

;;;###autoload
(defun use-package-handler/:delight (name _keyword args rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     body
     `((if (fboundp 'delight)
           (delight '(,@args)))))))

(add-to-list 'use-package-keywords :delight t)

(provide 'use-package-delight)

;;; use-package-delight.el ends here
