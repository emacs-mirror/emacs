;;; setenv.el --- functions to manipulate environment variables.

;;; Copyright Free Software Foundation 1991

;;; This file is part of GNU Emacs.

;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(defun setenv (variable value)
  "Set the value of the environment variable named VARIABLE to VALUE.
VARIABLE and VALUE should both be strings.
This function works by modifying process-environment."
  (if (string-match "=" variable)
      (error "name of environment variable contains an '=' character")
    (let ((pattern (concat "^" (regexp-quote (concat variable "="))))
	  (scan process-environment))
      (while scan
	(cond
	 ((string-match pattern (car scan))
	  (setcar scan (concat variable "=" value))
	  (setq scan nil))
	 ((null (setq scan (cdr scan)))
	  (setq process-environment
		(cons (concat variable "=" value) process-environment))))))))

(provide 'setenv)

;;; setenv.el ends here
