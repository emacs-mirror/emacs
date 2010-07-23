;;; ob-ditaa.el --- org-babel functions for ditaa evaluation

;; Copyright (C) 2009, 2010  Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 7.01

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

;; Org-Babel support for evaluating ditaa source code.
;;
;; This differs from most standard languages in that
;;
;; 1) there is no such thing as a "session" in ditaa
;;
;; 2) we are generally only going to return results of type "file"
;;
;; 3) we are adding the "file" and "cmdline" header arguments
;;
;; 4) there are no variables (at least for now)

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:ditaa
  '((:results . "file") (:exports . "results"))
  "Default arguments for evaluating a ditaa source block.")

(defun org-babel-expand-body:ditaa (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body." body)

(defvar org-ditaa-jar-path)
(defun org-babel-execute:ditaa (body params)
  "Execute a block of Ditaa code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let ((result-params (split-string (or (cdr (assoc :results params)) "")))
        (out-file (cdr (assoc :file params)))
        (cmdline (cdr (assoc :cmdline params)))
        (in-file (make-temp-file "org-babel-ditaa")))
    (unless (file-exists-p org-ditaa-jar-path)
      (error "Could not find ditaa.jar at %s" org-ditaa-jar-path))
    (with-temp-file in-file (insert body))
    (message (concat "java -jar " org-ditaa-jar-path " " cmdline " " in-file " " out-file))
    (shell-command (concat "java -jar " (shell-quote-argument org-ditaa-jar-path) " " cmdline " " in-file " " out-file))
    out-file))

(defun org-babel-prep-session:ditaa (session params)
  "Return an error because ditaa does not support sessions."
  (error "Ditaa does not support sessions"))

(provide 'ob-ditaa)

;; arch-tag: 492cd006-07d9-4fac-bef6-5bb60b48842e

;;; ob-ditaa.el ends here
