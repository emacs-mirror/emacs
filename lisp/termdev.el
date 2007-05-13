;;; termdev.el --- functions for dealing with terminals

;; Copyright (C) 2005 Free Software Foundation, Inc.

;; Author: Karoly Lorentey <karoly@lorentey.hu>
;; Created: 2005-12-22
;; Keywords: internal

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(substitute-key-definition 'suspend-emacs 'suspend-frame global-map)

(defun terminal-id (&optional terminal)
  "Return the numerical id of terminal TERMINAL.

TERMINAL can be a terminal id (an integer), a frame, or
nil (meaning the selected frame's terminal).  Alternatively,
TERMINAL may be the name of an X display
device (HOST.SERVER.SCREEN) or a tty device file."
  (cond
   ((integerp terminal)
    (if (terminal-live-p terminal)
	terminal
      (signal 'wrong-type-argument (list 'terminal-live-p terminal))))
   ((or (null terminal) (framep terminal))
    (frame-terminal terminal))
   ((stringp terminal)
    (let ((f (car (filtered-frame-list (lambda (frame)
					 (or (equal (frame-parameter frame 'display) terminal)
					     (equal (frame-parameter frame 'tty) terminal)))))))
      (or f (error "Display %s does not exist" terminal))
      (frame-terminal f)))
   (t
    (error "Invalid argument %s in `terminal-id'" terminal))))

;; (defun terminal-getenv (variable &optional terminal global-ok)
;;   "Get the value of VARIABLE in the client environment of TERMINAL.
;; VARIABLE should be a string.  Value is nil if VARIABLE is undefined in
;; the environment.  Otherwise, value is a string.

;; If TERMINAL has an associated emacsclient process, then
;; `terminal-getenv' looks up VARIABLE in the environment of that
;; process; otherwise the function consults the global environment,
;; i.e., the environment of the Emacs process itself.

;; If GLOBAL-OK is non-nil, and VARIABLE is not defined in the
;; terminal-local environment, then `terminal-getenv' will return
;; its value in the global environment instead.

;; TERMINAL can be a terminal id, a frame, or nil (meaning the
;; selected frame's terminal)."
;;   (setq terminal (terminal-id terminal))
;;   (if (null (terminal-parameter terminal 'environment))
;;       (getenv variable)
;;     (if (multibyte-string-p variable)
;; 	(setq variable (encode-coding-string variable locale-coding-system)))
;;     (let ((env (terminal-parameter terminal 'environment))
;; 	  result entry)
;;       (while (and env (null result))
;; 	(setq entry (car env)
;; 	      env (cdr env))
;; 	(if (and (> (length entry) (length variable))
;; 		 (eq ?= (aref entry (length variable)))
;; 		 (equal variable (substring entry 0 (length variable))))
;; 	    (setq result (substring entry (+ (length variable) 1)))))
;;       (if (and global-ok (null result))
;; 	  (getenv variable)
;; 	(and result (decode-coding-string result locale-coding-system))))))

;; (defun terminal-setenv (variable &optional value terminal)
;;   "Set the value of VARIABLE in the environment of TERMINAL.
;; VARIABLE should be string.  VALUE is optional; if not provided or
;; nil, the environment variable VARIABLE is removed.  Returned
;; value is the new value of VARIABLE, or nil if it was removed from
;; the environment.

;; If TERMINAL was created by an emacsclient invocation, then the
;; variable is set in the environment of the emacsclient process;
;; otherwise the function changes the environment of the Emacs
;; process itself.

;; TERMINAL can be a terminal id, a frame, or nil (meaning the
;; selected frame's terminal)."
;;   (if (null (terminal-parameter terminal 'environment))
;;       (setenv variable value)
;;     (with-terminal-environment terminal variable
;;       (setenv variable value))))

;; (defun terminal-setenv-internal (variable value terminal)
;;   "Set the value of VARIABLE in the environment of TERMINAL.
;; The caller is responsible to ensure that both VARIABLE and VALUE
;; are usable in environment variables and that TERMINAL is a
;; remote terminal."
;;   (if (multibyte-string-p variable)
;;       (setq variable (encode-coding-string variable locale-coding-system)))
;;   (if (and value (multibyte-string-p value))
;;       (setq value (encode-coding-string value locale-coding-system)))
;;   (let ((env (terminal-parameter terminal 'environment))
;; 	found)
;;     (while (and env (not found))
;;       (if (and (> (length (car env)) (length variable))
;; 		 (eq ?= (aref (car env) (length variable)))
;; 		 (equal variable (substring (car env) 0 (length variable))))
;; 	  (progn
;; 	    (if value
;; 		(setcar env (concat variable "=" value))
;; 	      (set-terminal-parameter terminal 'environment
;; 				      (delq (car env)
;; 					    (terminal-parameter terminal
;; 								'environment))))
;; 	    (setq found t))
;; 	(setq env (cdr env))))
;;     (cond
;;      ((and value found)
;;       (setcar env (concat variable "=" value)))
;;      ((and value (not found))
;;       (set-terminal-parameter terminal 'environment
;; 			      (cons (concat variable "=" value)
;; 				    (terminal-parameter terminal
;; 							'environment))))
;;      ((and (not value) found)
;;       (set-terminal-parameter terminal 'environment
;; 			      (delq (car env)
;; 				    (terminal-parameter terminal
;; 							'environment)))))))

;; (defmacro with-terminal-environment (terminal vars &rest body)
;;   "Evaluate BODY with environment variables VARS set to those of TERMINAL.
;; The environment variables are then restored to their previous values.

;; VARS should be a single string, a list of strings, or t for all
;; environment variables.

;; TERMINAL can be a terminal id, a frame, or nil (meaning the
;; selected frame's terminal).

;; If BODY uses `setenv' to change environment variables in VARS,
;; then the new variable values will be remembered for TERMINAL, and
;; `terminal-getenv' will return them even outside BODY."
;;   (declare (indent 2))
;;   (let ((var (make-symbol "var"))
;; 	(term (make-symbol "term"))
;; 	(v (make-symbol "v"))
;; 	(old-env (make-symbol "old-env")))
;;     `(let ((,term ,terminal)		; Evaluate arguments only once.
;; 	   (,v ,vars))
;;        (if (stringp ,v)
;; 	   (setq ,v (list ,v)))
;;        (cond
;; 	((null (terminal-parameter ,term 'environment))
;; 	 ;; Not a remote terminal; nothing to do.
;; 	 (progn ,@body))
;; 	((eq ,v t)
;; 	 ;; Switch the entire process-environment.
;; 	 (let (,old-env process-environment)
;; 	   (setq process-environment (terminal-parameter ,term 'environment))
;; 	   (unwind-protect
;; 	       (progn ,@body)
;; 	     (set-terminal-parameter ,term 'environment process-environment)
;; 	     (setq process-environment ,old-env))))
;; 	(t
;; 	 ;; Do only a set of variables.
;; 	 (let (,old-env)
;; 	   (dolist (,var ,v)
;; 	     (setq ,old-env (cons (cons ,var (getenv ,var)) ,old-env))
;; 	     (setenv ,var (terminal-getenv ,var ,term)))
;; 	   (unwind-protect
;; 	       (progn ,@body)
;; 	     ;; Split storing new values and restoring old ones so
;; 	     ;; that we DTRT even if a variable is specified twice in
;; 	     ;; VARS.
;; 	     (dolist (,var ,v)
;; 	       (terminal-setenv-internal ,var (getenv ,var) ,term))
;; 	     (dolist (,var ,old-env)
;; 	       (setenv (car ,var) (cdr ,var))))))))))

(provide 'termdev)

;;; arch-tag: 4c4df277-1ec1-4f56-bfde-7f156fe62fb2
;;; termdev.el ends here
