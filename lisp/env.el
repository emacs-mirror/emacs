;;; env.el --- functions to manipulate environment variables.

;;; Copyright Free Software Foundation 1991

;; Maintainer: FSF
;; Keywords: processes, unix

;;; This file is part of GNU Emacs.

;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; UNIX processes inherit a list of name-to-string associations from
;; their parents called their `environment'; these are commonly used
;; to control program options.  This package permits you to set
;; environment variables to be passed to any sub-process run under Emacs.

;;; Code:

;;;###autoload
(defun setenv (variable &optional value)
  "Set the value of the environment variable named VARIABLE to VALUE.
VARIABLE should be a string.  VALUE is optional; if not provided or is
`nil', the environment variable VARIABLE will be removed.  
This function works by modifying `process-environment'."
  (interactive "sSet environment variable: \nsSet %s to value: ")
  (if (string-match "=" variable)
      (error "Environment variable name `%s' contains `='" variable)
    (let ((pattern (concat "\\`" (regexp-quote (concat variable "="))))
	  (case-fold-search nil)
	  (scan process-environment))
      (while scan
	(cond
	 ((string-match pattern (car scan))
          (if (eq nil value)
              (setq process-environment (delq (car scan) process-environment))
            (setcar scan (concat variable "=" value)))
	  (setq scan nil))
	 ((null (setq scan (cdr scan)))
	  (setq process-environment
		(cons (concat variable "=" value) process-environment))))))))

(provide 'env)

;;; env.el ends here
