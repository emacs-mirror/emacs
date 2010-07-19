;;; ob-ocaml.el --- org-babel functions for ocaml evaluation

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

;; Org-Babel support for evaluating ocaml source code.  This one will
;; be sort of tricky because ocaml programs must be compiled before
;; they can be run, but ocaml code can also be run through an
;; interactive interpreter.
;;
;; For now lets only allow evaluation using the ocaml interpreter.

;;; Requirements:

;; - tuareg-mode :: http://www-rocq.inria.fr/~acohen/tuareg/

;;; Code:
(require 'ob)
(require 'ob-comint)
(require 'comint)
(eval-when-compile (require 'cl))

(declare-function tuareg-run-caml "ext:tuareg" ())
(declare-function tuareg-interactive-send-input "ext:tuareg" ())

(add-to-list 'org-babel-tangle-lang-exts '("ocaml" . "ml"))

(defvar org-babel-default-header-args:ocaml '())

(defvar org-babel-ocaml-eoe-indicator "\"org-babel-ocaml-eoe\";;")
(defvar org-babel-ocaml-eoe-output "org-babel-ocaml-eoe")

(defun org-babel-expand-body:ocaml (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (nth 1 (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat
      (lambda (pair) (format "let %s = %s;;" (car pair)
			(org-babel-ocaml-elisp-to-ocaml (cdr pair))))
      vars "\n") "\n" body "\n")))

(defun org-babel-execute:ocaml (body params)
  "Execute a block of Ocaml code with Babel."
  (let* ((processed-params (org-babel-process-params params))
         (vars (nth 1 processed-params))
         (full-body (org-babel-expand-body:ocaml body params processed-params))
         (session (org-babel-prep-session:ocaml
		   (cdr (assoc :session params)) params))
         (raw (org-babel-comint-with-output
		  (session org-babel-ocaml-eoe-output t full-body)
		(insert
		 (concat
		  (org-babel-chomp full-body)"\n"org-babel-ocaml-eoe-indicator))
		(tuareg-interactive-send-input)))
	 (clean
	  (car (let ((re (regexp-quote org-babel-ocaml-eoe-output)) out)
		 (delq nil (mapcar (lambda (line)
				     (if out
					 (progn (setq out nil) line)
				       (when (string-match re line)
					 (progn (setq out t) nil))))
				 (mapcar #'org-babel-trim (reverse raw))))))))
    (org-babel-reassemble-table
     (org-babel-ocaml-parse-output (org-babel-trim clean))
     (org-babel-pick-name
      (nth 4 processed-params) (cdr (assoc :colnames params)))
     (org-babel-pick-name
      (nth 5 processed-params) (cdr (assoc :rownames params))))))

(defvar tuareg-interactive-buffer-name)
(defun org-babel-prep-session:ocaml (session params)
  "Prepare SESSION according to the header arguments in PARAMS."
  (require 'tuareg)
  (let ((tuareg-interactive-buffer-name (if (and (not (string= session "none"))
                                                 (not (string= session "default"))
                                                 (stringp session))
                                            session
                                          tuareg-interactive-buffer-name)))
    (save-window-excursion (tuareg-run-caml)
                           (get-buffer tuareg-interactive-buffer-name))))

(defun org-babel-ocaml-elisp-to-ocaml (val)
  "Return a string of ocaml code which evaluates to VAL."
  (if (listp val)
      (concat "[|" (mapconcat #'org-babel-ocaml-elisp-to-ocaml val "; ") "|]")
    (format "%S" val)))

(defun org-babel-ocaml-parse-output (output)
  "Parse OUTPUT.
OUTPUT is string output from an ocaml process."
  (let ((regexp "%s = \\(.+\\)$"))
    (cond
     ((string-match (format regexp "string") output)
      (org-babel-read (match-string 1 output)))
     ((or (string-match (format regexp "int") output)
          (string-match (format regexp "float") output))
      (string-to-number (match-string 1 output)))
     ((string-match (format regexp "list") output)
      (org-babel-ocaml-read-list (match-string 1 output)))
     ((string-match (format regexp "array") output)
      (org-babel-ocaml-read-array (match-string 1 output)))
     (t (message "don't recognize type of %s" output) output))))

(defun org-babel-ocaml-read-list (results)
  "Convert RESULTS into an elisp table or string.
If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-read
   (if (and (stringp results) (string-match "^\\[.+\\]$" results))
       (org-babel-read
        (replace-regexp-in-string
         "\\[" "(" (replace-regexp-in-string
                    "\\]" ")" (replace-regexp-in-string
                               "; " " " (replace-regexp-in-string
                                         "'" "\"" results)))))
     results)))

(defun org-babel-ocaml-read-array (results)
  "Convert RESULTS into an elisp table or string.
If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-read
   (if (and (stringp results) (string-match "^\\[.+\\]$" results))
       (org-babel-read
	(concat
	 "'" (replace-regexp-in-string
	      "\\[|" "(" (replace-regexp-in-string
			  "|\\]" ")" (replace-regexp-in-string
				      "; " " " (replace-regexp-in-string
						"'" "\"" results))))))
     results)))

(provide 'ob-ocaml)

;; arch-tag: 2e815f4d-365e-4d69-b1df-dd17fdd7b7b7

;;; ob-ocaml.el ends here
