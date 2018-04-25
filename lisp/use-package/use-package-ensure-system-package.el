;;; use-package-ensure-system-package.el --- auto install system packages  -*- lexical: t; -*-

;; Copyright (C) 2017 Justin Talbott

;; Author: Justin Talbott <justin@waymondo.com>
;; Keywords: convenience, tools, extensions
;; URL: https://github.com/waymondo/use-package-ensure-system-package
;; Version: 0.1
;; Package-Requires: ((use-package "2.1") (system-packages "1.0.4"))
;; Filename: use-package-ensure-system-package.el
;; License: GNU General Public License version 3, or (at your option) any later version
;;

;;; Commentary:
;;
;; The `:ensure-system-package` keyword allows you to ensure system
;; binaries exist alongside your `use-package` declarations.
;;

;;; Code:

(require 'use-package)
(require 'system-packages nil t)

(eval-when-compile
  (defvar system-packages-package-manager)
  (defvar system-packages-supported-package-managers)
  (defvar system-packages-use-sudo))

(defun use-package-ensure-system-package-install-command (pack)
  "Return the default install command for PACK."
  (system-packages-get-command 'install pack))

(defun use-package-ensure-system-package-consify (arg)
  "Turn `arg' into a cons of (`package-name' . `install-command')."
  (cond
   ((stringp arg)
    (cons arg (use-package-ensure-system-package-install-command arg)))
   ((symbolp arg)
    (cons arg (use-package-ensure-system-package-install-command (symbol-name arg))))
   ((consp arg)
    (if (stringp (cdr arg))
        arg
      (cons (car arg)
            (use-package-ensure-system-package-install-command (symbol-name (cdr arg))))))))

;;;###autoload
(defun use-package-normalize/:ensure-system-package (name-symbol keyword args)
  "Turn `arg' into a list of cons-es of (`package-name' . `install-command')."
  (use-package-only-one (symbol-name keyword) args
    (lambda (label arg)
      (cond
       ((and (listp arg) (listp (cdr arg)))
        (mapcar #'use-package-ensure-system-package-consify arg))
       (t
        (list (use-package-ensure-system-package-consify arg)))))))

;;;###autoload
(defun use-package-handler/:ensure-system-package (name keyword arg rest state)
  "Execute the handler for `:ensure-system-package' keyword in `use-package'."
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (mapcar #'(lambda (cons)
                 `(unless (executable-find (symbol-name ',(car cons)))
                    (async-shell-command ,(cdr cons)))) arg)
     body)))

(add-to-list 'use-package-keywords :ensure-system-package t)

(provide 'use-package-ensure-system-package)

;;; use-package-ensure-system-package.el ends here
