;;; ob-maxima.el --- Babel Functions for Maxima      -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2025 Free Software Foundation, Inc.

;; Author: Eric S Fraga
;;	Eric Schulte
;; Keywords: literate programming, reproducible research, maxima
;; URL: https://orgmode.org

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

;; Org-Babel support for evaluating maxima entries.
;;
;; This differs from most standard languages in that
;; 1) there is no such thing as a "session" in maxima
;; 2) we are adding the "cmdline" header argument

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ob)

(defconst org-babel-header-args:maxima
  '((batch               . ((batchload batch load)))
    (graphics-pkg        . ((plot draw))))
  "Maxima-specific header arguments.")

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("maxima" . "max"))

(defvar org-babel-default-header-args:maxima '())

(defcustom org-babel-maxima-command
  (if (boundp 'maxima-command) maxima-command "maxima")
  "Command used to call maxima on the shell."
  :group 'org-babel
  :type 'string)

(defvar org-babel-maxima--command-arguments-default
  "--very-quiet"
  "Command-line arguments sent to Maxima by default.
If the `:batch' header argument is set to `batchload' or unset,
then the `:cmdline' header argument is appended to this default;
otherwise, if the `:cmdline' argument is set, it over-rides this
default.  See `org-babel-maxima-command' and
`org-babel-execute:maxima'.")

(defvar org-babel-maxima--graphic-package-options
  '((plot . "(set_plot_option ('[gnuplot_term, %s]), set_plot_option ('[gnuplot_out_file, %S]))$")
    (draw . "(load(draw), set_draw_defaults(terminal='%s,file_name=%S))$"))
  "An alist of graphics packages and Maxima code.
Each element is a cons (PACKAGE-NAME . FORMAT-STRING).
FORMAT-STRING contains Maxima code to configure the graphics
package; it must contain `%s' to set the terminal and `%S' to set
the filename, in that order.  The default graphics package is
`plot'; `draw' is also supported.  See
`org-babel-maxima-expand'.")

(defvar org-babel-maxima--default-epilogue
  '((graphical-output . "gnuplot_close ()$")
    (non-graphical-output . ""))
  "The final Maxima code executed in a source block.
An alist with the epilogue for graphical and non-graphical
output.  See `org-babel-maxima-expand'.")

(defun org-babel-maxima-expand (body params)
  "Expand Maxima BODY according to its header arguments from PARAMS."
  (let* ((vars (org-babel--get-vars params))
         (graphic-file (ignore-errors (org-babel-graphical-output-file params)))
	 (epilogue (cdr (assq :epilogue params)))
	 (prologue (cdr (assq :prologue params))))
    (mapconcat 'identity
               (delq nil
	             (list
		      ;; Any code from the specified prologue at the start.
		      prologue
		      ;; graphic output
		      (if graphic-file
                          (let* ((graphics-pkg (intern (or (cdr (assq :graphics-pkg params)) "plot")))
                                 (graphic-format-string (cdr (assq graphics-pkg org-babel-maxima--graphic-package-options)))
                                 (graphic-terminal (file-name-extension graphic-file))
                                 (graphic-file (if (eq graphics-pkg 'plot) graphic-file (file-name-sans-extension graphic-file))))
                            (format graphic-format-string graphic-terminal graphic-file)))
		      ;; variables
		      (mapconcat 'org-babel-maxima-var-to-maxima vars "\n")
		      ;; body
		      body
		      ;; Any code from the specified epilogue at the end.
		      epilogue
		      (if graphic-file
                          (cdr (assq :graphical-output org-babel-maxima--default-epilogue))
                        (cdr (assq :non-graphical-output org-babel-maxima--default-epilogue)))))
	       "\n")))

(defvar org-babel-maxima--output-filter-regexps
  '("batch"                     ;; remove the `batch' or `batchload' line
    "^rat: replaced .*$"        ;; remove notices from `rat'
    "^;;; Loading #P"           ;; remove notices from the lisp implementation
    "^read and interpret"       ;; remove notice from `batch'
    "^(%\\([i]-?[0-9]+\\))[ ]$" ;; remove empty input lines from `batch'-ing
    )
  "Regexps to remove extraneous lines from Maxima's output.
See `org-babel-maxima--output-filter'.")

(defun org-babel-maxima--output-filter (line)
  "Filter empty or undesired lines from Maxima output.
Return nil if LINE is zero-length or it matches a regexp in
`org-babel-maxima--output-filter'; otherwise, return LINE."
  (unless (or (= 0 (length line))
              (cl-some #'(lambda(r) (string-match r line))
                       org-babel-maxima--output-filter-regexps))
    line))

(defun org-babel-execute:maxima (body params)
  "Execute Maxima BODY according to PARAMS.
This function is called by `org-babel-execute-src-block'."
  (unless noninteractive (message "Executing Maxima source code block"))
  (let ((result-params (split-string (or (cdr (assq :results params)) "")))
	(result
	 (let* ((cmdline (or (cdr (assq :cmdline params)) ""))
                (batch/load (or (cdr (assq :batch params)) "batchload"))
                (cmdline (if (or (equal cmdline "") (equal batch/load "batchload"))
                             ;; legacy behavior:
                             ;; ensure that --very-quiet is on command-line by default
                             (concat cmdline " " org-babel-maxima--command-arguments-default)
                           ;; if using an alternate loader, :cmdline overwrites default
                           cmdline))
		(in-file (org-babel-temp-file "maxima-" ".max"))
                (cmd (format "%s -r %s %s"
			     org-babel-maxima-command
                             (shell-quote-argument
                              ;; bind linenum to 0 so the first line
                              ;; of in-file has line number 1
                              (format "(linenum:0, %s(%S))$" batch/load in-file))
                             cmdline)))
	   (with-temp-file in-file (insert (org-babel-maxima-expand body params)))
	   (unless noninteractive (message cmd))
           ;; " | grep -v batch | grep -v 'replaced' | sed '/^$/d' "
	   (let ((raw (org-babel-eval cmd "")))
             (mapconcat
              #'identity
              (delq nil
                    (mapcar #'org-babel-maxima--output-filter
                            (split-string raw "[\r\n]"))) "\n")))))
    (if (ignore-errors (org-babel-graphical-output-file params))
	nil
      (org-babel-result-cond result-params
	result
	(let ((tmp-file (org-babel-temp-file "maxima-res-")))
	  (with-temp-file tmp-file (insert result))
	  (org-babel-import-elisp-from-file tmp-file))))))


(defun org-babel-prep-session:maxima (_session _params)
"Throw an error.  Maxima does not support sessions."
  (error "Maxima does not support sessions"))

(defun org-babel-maxima-var-to-maxima (pair)
  "Convert an elisp variable-value PAIR to maxima code."
  (let ((var (car pair))
        (val (cdr pair)))
    (when (symbolp val)
      (setq val (symbol-name val))
      (when (= (length val) 1)
        (setq val (string-to-char val))))
    (format "%S: %s$" var
	    (org-babel-maxima-elisp-to-maxima val))))

(defun org-babel-maxima-elisp-to-maxima (val)
  "Return a string of maxima code which evaluates to VAL."
  (if (listp val)
      (concat "[" (mapconcat #'org-babel-maxima-elisp-to-maxima val ", ") "]")
    (format "%s" val)))

(provide 'ob-maxima)

;;; ob-maxima.el ends here
