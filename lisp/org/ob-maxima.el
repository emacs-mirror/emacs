;;; ob-maxima.el --- org-babel functions for maxima evaluation

;; Copyright (C) 2009-2011  Free Software Foundation, Inc.

;; Author: Eric S Fraga
;;	   Eric Schulte
;; Keywords: literate programming, reproducible research, maxima
;; Homepage: http://orgmode.org
;; Version: 7.7

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating maxima entries.
;;
;; This differs from most standard languages in that
;;
;; 1) there is no such thing as a "session" in maxima
;;
;; 2) we are generally only going to return output from maxima
;;
;; 3) we are adding the "cmdline" header argument
;;
;; 4) there are no variables

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:maxima '())

(defun org-babel-maxima-expand (body params)
  "Expand a block of Maxima code according to its header arguments."
  body)

(defun org-babel-execute:maxima (body params)
  "Execute a block of Maxima entries with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing Maxima source code block")
  (let* ((result-params (split-string (or (cdr (assoc :results params)) "")))
	 (cmdline (cdr (assoc :cmdline params)))
	 (in-file (org-babel-temp-file "maxima-"))
	 (cmd (format "maxima --very-quiet -r 'batchload(%S)$' %s"
		      in-file cmdline)))
    (with-temp-file in-file (insert body))
    (message cmd)
    ((lambda (raw) ;; " | grep -v batch | grep -v 'replaced' | sed '/^$/d' "
       (mapconcat
	#'identity
	(delq nil
	      (mapcar (lambda (line)
			(unless (or (string-match "batch" line)
				    (string-match "^rat: replaced .*$" line)
				    (= 0 (length line)))
			  line))
		      (split-string raw "[\r\n]"))) "\n"))
     (org-babel-eval cmd ""))))

(defun org-babel-prep-session:maxima (session params)
  (error "Maxima does not support sessions"))

(provide 'ob-maxima)

;; arch-tag: d86c97ac-7eab-4349-8d8b-302dd09779a8

;;; ob-maxima.el ends here
