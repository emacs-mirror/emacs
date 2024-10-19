;;; ob-gnuplot.el --- Babel Functions for Gnuplot    -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2024 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Maintainer: Ihor Radchenko <yantar92 at posteo dot net>
;; Keywords: literate programming, reproducible research
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

;; Org-Babel support for evaluating gnuplot source code.
;;
;; This differs from most standard languages in that
;;
;; 1) we are generally only going to return results of type "file"
;;
;; 2) we are adding the "file" and "cmdline" header arguments

;;; Requirements:

;; - gnuplot :: https://www.gnuplot.info/
;;
;; - gnuplot-mode :: you can search the web for the latest active one.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ob)
(require 'org-macs)

(declare-function org-time-string-to-time "org" (s))
(declare-function orgtbl-to-generic "org-table" (table params))
(declare-function gnuplot-mode "ext:gnuplot-mode" ())
(declare-function gnuplot-send-string-to-gnuplot "ext:gnuplot-mode" (str txt))
(declare-function gnuplot-send-buffer-to-gnuplot "ext:gnuplot-mode" ())

(defvar org-babel-temporary-directory)

(defvar org-babel-default-header-args:gnuplot
  '((:results . "file") (:exports . "results") (:session . nil))
  "Default arguments to use when evaluating a gnuplot source block.")

(defvar org-babel-header-args:gnuplot
  '((title	. :any)
    (lines	. :any)
    (sets	. :any)
    (x-labels	. :any)
    (y-labels	. :any)
    (timefmt	. :any)
    (time-ind	. :any)
    (missing	. :any)
    (term       . :any))
  "Gnuplot specific header args.")

(defvar org-babel-gnuplot-timestamp-fmt nil) ; Dynamically scoped.

(defvar *org-babel-gnuplot-missing* nil)

(defcustom *org-babel-gnuplot-terms*
  '((eps . "postscript eps"))
  "List of file extensions and the associated gnuplot terminal."
  :group 'org-babel
  :type '(repeat (cons (symbol :tag "File extension")
		       (string :tag "Gnuplot terminal"))))

(defun org-babel-gnuplot-process-vars (params)
  "Extract variables from PARAMS and process the variables.
Dumps all vectors into files and returns an association list
of variable names and the related value to be used in the gnuplot
code."
  (let ((*org-babel-gnuplot-missing* (cdr (assq :missing params))))
    (mapcar
     (lambda (pair)
       (cons
	(car pair) ;; variable name
	(let* ((val (cdr pair)) ;; variable value
	       (lp  (proper-list-p val)))
	  (if lp
	      (org-babel-gnuplot-table-to-data
	       (let* ((first  (car val))
		      (tablep (or (listp first) (symbolp first))))
		 (if tablep val (mapcar 'list val)))
               ;; Make temporary file name stable with respect to data.
               ;; If we do not do it, :cache argument becomes useless.
               (org-babel-temp-stable-file (cons val params) "gnuplot-")
               params)
	    (if (and (stringp val)
		     (file-remote-p val)  ;; check if val is a remote file
		     (file-exists-p val)) ;; call to file-exists-p is slow, maybe remove it
		(let* ((local-name (concat ;; create a unique filename to avoid multiple downloads
				    (org-babel-temp-directory)
				    "/gnuplot/"
				    (file-remote-p val 'host)
				    (org-babel-local-file-name val))))
		  (if (and (file-exists-p local-name) ;; only download file if remote is newer
			   (file-newer-than-file-p local-name val))
		      local-name
		    (make-directory (file-name-directory local-name) t)
		    (copy-file val local-name t)
		    ))
	      val
	      )))))
     (org-babel--get-vars params))))

(defun org-babel-expand-body:gnuplot (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (save-window-excursion
    (let* ((vars (org-babel-gnuplot-process-vars params))
           (out-file (cdr (assq :file params)))
	   (prologue (cdr (assq :prologue params)))
	   (epilogue (cdr (assq :epilogue params)))
	   (term (or (cdr (assq :term params))
                     (when out-file
		       (let ((ext (file-name-extension out-file)))
			 (or (cdr (assoc (intern (downcase ext))
					 *org-babel-gnuplot-terms*))
			     ext)))))
           (title (cdr (assq :title params)))
           (lines (cdr (assq :line params)))
           (sets (cdr (assq :set params)))
           (missing (cdr (assq :missing params)))
           (x-labels (cdr (assq :xlabels params)))
           (y-labels (cdr (assq :ylabels params)))
           (timefmt (cdr (assq :timefmt params)))
           (time-ind (or (cdr (assq :timeind params))
                         (when timefmt 1)))
	   (directory default-directory)
	   (add-to-body (lambda (text) (setq body (concat text "\n" body)))))
      ;; append header argument settings to body
      (when missing (funcall add-to-body (format "set datafile missing '%s'" missing)))
      (when title (funcall add-to-body (format "set title '%s'" title)))
      (when lines (mapc (lambda (el) (funcall add-to-body el)) lines))
      (when sets
	(mapc (lambda (el) (funcall add-to-body (format "set %s" el))) sets))
      (when x-labels
	(funcall add-to-body
		 (format "set xtics (%s)"
			 (mapconcat (lambda (pair)
				      (format "\"%s\" %d"
					      (cdr pair) (car pair)))
				    x-labels ", "))))
      (when y-labels
	(funcall add-to-body
		 (format "set ytics (%s)"
			 (mapconcat (lambda (pair)
				      (format "\"%s\" %d"
					      (cdr pair) (car pair)))
				    y-labels ", "))))
      (when time-ind
	(funcall add-to-body "set xdata time")
	(funcall add-to-body (concat "set timefmt \""
				     (or timefmt
					 "%Y-%m-%d-%H:%M:%S") "\"")))
      (when out-file
	;; set the terminal at the top of the block
	(funcall add-to-body (format "set output \"%s\"" out-file))
	;; and close the terminal at the bottom of the block
	(setq body (concat body "\nset output\n")))
      (when term (funcall add-to-body (format "set term %s" term)))
      ;; insert variables into code body: this should happen last
      ;; placing the variables at the *top* of the code in case their
      ;; values are used later
      (funcall add-to-body
	       (mapconcat #'identity
			  (org-babel-variable-assignments:gnuplot params)
			  "\n"))
      ;; replace any variable names preceded by '$' with the actual
      ;; value of the variable
      (mapc (lambda (pair)
	      (setq body (replace-regexp-in-string
			  (format "\\$%s" (car pair)) (cdr pair) body t t)))
	    vars)
      (when prologue (funcall add-to-body prologue))
      (when epilogue (setq body (concat body "\n" epilogue)))
      ;; Setting the directory needs to be done first so that
      ;; subsequent 'output' directive goes to the right place.
      (when directory (funcall add-to-body (format "cd '%s'" directory))))
    body))

(defun org-babel-execute:gnuplot (body params)
  "Execute Gnuplot BODY according to PARAMS.
This function is called by `org-babel-execute-src-block'."
  (org-require-package 'gnuplot)
  (let ((session (cdr (assq :session params)))
        (result-type (cdr (assq :results params)))
        (body (org-babel-expand-body:gnuplot body params))
	output)
    (save-window-excursion
      ;; evaluate the code body with gnuplot
      (if (string= session "none")
          (let ((script-file (org-babel-temp-file "gnuplot-script-")))
            (with-temp-file script-file
              (insert (concat body "\n")))
            (unless noninteractive (message "gnuplot \"%s\"" script-file))
            (setq output
                  (shell-command-to-string
		   (format
		    "gnuplot \"%s\""
		    (org-babel-process-file-name
		     script-file
		     (if (member system-type '(cygwin windows-nt ms-dos))
			 t nil)))))
            (unless noninteractive (message "%s" output)))
        (with-temp-buffer
          (insert (concat body "\n"))
          (gnuplot-mode)
          (gnuplot-send-buffer-to-gnuplot)))
      (if (member "output" (split-string result-type))
          output
	nil)))) ;; signal that output has already been written to file

(defun org-babel-prep-session:gnuplot (session params)
  "Prepare SESSION according to the header arguments in PARAMS."
  (let* ((session (org-babel-gnuplot-initiate-session session))
         (var-lines (org-babel-variable-assignments:gnuplot params)))
    (unless noninteractive (message "%S" session))
    (org-babel-comint-in-buffer session
      (dolist (var-line  var-lines)
	(insert var-line)
	(comint-send-input nil t)
	(org-babel-comint-wait-for-output session)
	(sit-for .1)
	(goto-char (point-max))))
    session))

(defun org-babel-load-session:gnuplot (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:gnuplot session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))

(defun org-babel-variable-assignments:gnuplot (params)
  "Return list of gnuplot statements assigning the block's variables.
PARAMS is src block parameters alist defining variable assignments."
  (mapcar
   (lambda (pair) (format "%s = \"%s\"" (car pair) (cdr pair)))
   (org-babel-gnuplot-process-vars params)))

(defvar gnuplot-buffer)
(defun org-babel-gnuplot-initiate-session (&optional session _params)
  "Initiate a gnuplot session.
If there is not a current inferior-process-buffer in SESSION
then create one.  Return the initialized session.  The current
`gnuplot-mode' doesn't provide support for multiple sessions."
  (org-require-package 'gnuplot)
  (unless (string= session "none")
    (save-window-excursion
      (gnuplot-send-string-to-gnuplot "" "line")
      gnuplot-buffer)))

(defun org-babel-gnuplot-quote-timestamp-field (s)
  "Convert S from timestamp to Unix time and export to gnuplot."
  (format-time-string org-babel-gnuplot-timestamp-fmt
		      (org-time-string-to-time s)))

(defvar org-table-number-regexp)
(defvar org-ts-regexp3)
(defun org-babel-gnuplot-quote-tsv-field (s)
  "Quote S for export to gnuplot."
  (unless (stringp s)
    (setq s (format "%s" s)))
  (if (string-match org-table-number-regexp s) s
    (if (string-match org-ts-regexp3 s)
	(org-babel-gnuplot-quote-timestamp-field s)
      (if (zerop (length s))
	  (or *org-babel-gnuplot-missing* s)
	(if (string-match "[ \"]" s)
	    (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"")
		    "\"")
	  s)))))

(defun org-babel-gnuplot-table-to-data (table data-file params)
  "Export TABLE to DATA-FILE in a format readable by gnuplot.
Pass PARAMS through to `orgtbl-to-generic' when exporting TABLE."
  (require 'ox-org)
  (require 'ox-ascii)
  (declare-function org-export-create-backend "ox")
  (with-temp-file data-file
    (insert (let ((org-babel-gnuplot-timestamp-fmt
		   (or (plist-get params :timefmt) "%Y-%m-%d-%H:%M:%S"))
                  ;; Create custom limited backend that will disable
                  ;; advanced ASCII export features that may alter the
                  ;; original data.
                  (ob-gnuplot-data
                   (org-export-create-backend
                    :parent 'ascii
                    :transcoders
                    `(;; Do not try to resolve links.  Export them verbatim.
                      (link . (lambda (link _ _) (org-element-interpret-data link)))
                      ;; Drop emphasis markers from verbatim and code.
                      ;; This way, data can use verbatim when escaping
                      ;; is necessary and yet be readable by Gnuplot,
                      ;; which is not aware about Org's markup.
                      (verbatim . (lambda (verbatim _ _) (org-element-property :value verbatim)))
                      (code . (lambda (code _ _) (org-element-property :value code)))))))
	      (orgtbl-to-generic
	       table
	       (org-combine-plists
		`( :sep "\t" :fmt org-babel-gnuplot-quote-tsv-field
                   ;; Two setting below are needed to make :fmt work.
                   :raw t
                   :backend ,ob-gnuplot-data)
		params)))))
  data-file)

(provide 'ob-gnuplot)

;;; ob-gnuplot.el ends here
