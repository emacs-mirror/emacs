;;; ob-eshell.el --- Babel Functions for Eshell      -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022 Free Software Foundation, Inc.

;; Author: stardiviner <numbchild@gmail.com>
;; Maintainer: stardiviner <numbchild@gmail.com>
;; Homepage: https://github.com/stardiviner/ob-eshell
;; Keywords: literate programming, reproducible research

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

;; Org Babel support for evaluating Eshell source code.

;;; Code:
(require 'ob)
(require 'eshell)

(declare-function eshell-send-input "esh-mode"
                  (&optional use-region queue-p no-newline))

(defvar eshell-last-output-start)
(defvar eshell-last-output-end)
(defvar eshell-last-input-end)

(defvar org-babel-default-header-args:eshell '())

(defun org-babel-execute:eshell (body params)
  "Execute a block of Eshell code BODY with PARAMS.
This function is called by `org-babel-execute-src-block'.

The BODY can be any code which allowed executed in Eshell.
Eshell allow to execute normal shell command and Elisp code.
More details please reference Eshell Info.

The PARAMS are variables assignments."
  (let* ((session (org-babel-eshell-initiate-session
		   (cdr (assq :session params))))
	 (full-body (org-babel-expand-body:generic
		     body params (org-babel-variable-assignments:eshell params))))
    (if session
	(progn
	  (with-current-buffer session
	    (dolist (line (split-string full-body "\n"))
	      (goto-char eshell-last-output-end)
	      (insert line)
	      (eshell-send-input))
	    ;; get output of last input
	    ;; TODO: collect all output instead of last command's output.
	    (goto-char eshell-last-input-end)
	    (buffer-substring-no-properties (point) eshell-last-output-start)))
      (with-temp-buffer
	(eshell-command full-body t)
	(buffer-string)))))

(defun org-babel-prep-session:eshell (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-eshell-initiate-session session))
	 ;; Eshell session buffer is read from variable `eshell-buffer-name'.
	 (eshell-buffer-name session)
	 (var-lines (org-babel-variable-assignments:eshell params)))
    (call-interactively #'eshell)
    (mapc #'eshell-command var-lines)
    session))

(defun ob-eshell-session-live-p (session)
  "Non-nil if Eshell SESSION exists."
  (get-buffer session))

(defun org-babel-eshell-initiate-session (&optional session _params)
  "Initiate a session named SESSION."
  (when (and session (not (string= session "none")))
    (save-window-excursion
      (unless (ob-eshell-session-live-p session)
	(let ((eshell-buffer-name session)) (eshell))))
    session))

(defun org-babel-variable-assignments:eshell (params)
  "Convert ob-eshell :var specified variables into Eshell variables assignments."
  (mapcar
   (lambda (pair)
     (format "(setq %s %S)" (car pair) (cdr pair)))
   (org-babel--get-vars params)))

(defun org-babel-load-session:eshell (session body params)
  "Load BODY into SESSION with PARAMS."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:eshell session params)))
      (with-current-buffer buffer
	(goto-char (point-max))
	(insert (org-babel-chomp body)))
      buffer)))

(provide 'ob-eshell)

;;; ob-eshell.el ends here
