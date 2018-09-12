;;; use-package-ensure-system-package.el --- auto install system packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Justin Talbott

;; Author: Justin Talbott <justin@waymondo.com>
;; Keywords: convenience, tools, extensions
;; URL: https://github.com/waymondo/use-package-ensure-system-package
;; Version: 0.2
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
  (declare-function system-packages-get-command "system-packages"))


(defun use-package-ensure-system-package-consify (arg)
  "Turn `arg' into a cons of (`package-name' . `install-command')."
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
      (cons (car arg) `(async-shell-command ,(cdr arg))))
     (t
      (cons (car arg)
	    `(system-packages-install ,(symbol-name (cdr arg)))))))))

;;;###autoload
(defun use-package-normalize/:ensure-system-package (_name-symbol keyword args)
  "Turn `arg' into a list of cons-es of (`package-name' . `install-command')."
  (use-package-only-one (symbol-name keyword) args
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
