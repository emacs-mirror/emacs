;;; up-delight.el --- Support for the :delight keyword

;; Copyright (C) 2012-2017 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Created: 17 Jun 2012
;; Modified: 3 Dec 2017
;; Version: 1.0
;; Package-Requires: ((emacs "24.3") (use-package "2.4") (delight "1.5"))
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

;; Provides support for the :delight keyword, which is made available by
;; default by requiring `use-package'.

;;; Code:

(require 'up-core)

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

(defun use-package-normalize/:delight (name keyword args)
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

(defun use-package-handler/:delight (name keyword args rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     body
     `((if (fboundp 'delight)
           (delight '(,@args)))))))

(add-to-list 'use-package-keywords :delight t)

(provide 'up-delight)
