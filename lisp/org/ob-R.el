;;; ob-R.el --- Babel Functions for R                -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2024 Free Software Foundation, Inc.

;; Author: Eric Schulte
;;	Dan Davison
;; Maintainer: Jeremie Juste <jeremiejuste@gmail.com>
;; Keywords: literate programming, reproducible research, R, statistics
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

;; Org-Babel support for evaluating R code

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'cl-lib)
(require 'ob)

(declare-function orgtbl-to-tsv "org-table" (table params))
(declare-function run-ess-r "ext:ess-r-mode" (&optional start-args))
(declare-function inferior-ess-send-input "ext:ess-inf" ())
(declare-function ess-make-buffer-current "ext:ess-inf" ())
(declare-function ess-eval-buffer "ext:ess-inf" (vis))
(declare-function ess-wait-for-process "ext:ess-inf"
		  (&optional proc sec-prompt wait force-redisplay))

(defconst org-babel-header-args:R
  '((width		 . :any)
    (height		 . :any)
    (bg			 . :any)
    (units		 . :any)
    (pointsize		 . :any)
    (antialias		 . :any)
    (quality		 . :any)
    (compression	 . :any)
    (res		 . :any)
    (type		 . :any)
    (family		 . :any)
    (title		 . :any)
    (fonts		 . :any)
    (version		 . :any)
    (paper		 . :any)
    (encoding		 . :any)
    (pagecentre		 . :any)
    (colormodel		 . :any)
    (useDingbats	 . :any)
    (horizontal		 . :any)
    (async               . ((yes no)))
    (results             . ((file list vector table scalar verbatim)
			    (raw html latex org code pp drawer)
			    (replace silent none append prepend)
			    (output value graphics))))
  "R-specific header arguments.")

(defconst ob-R-safe-header-args
  (append org-babel-safe-header-args
	  '(:width :height :bg :units :pointsize :antialias :quality
		   :compression :res :type :family :title :fonts
		   :version :paper :encoding :pagecentre :colormodel
		   :useDingbats :horizontal))
  "Header args which are safe for R babel blocks.

See `org-babel-safe-header-args' for documentation of the format of
this variable.")

(defvar org-babel-default-header-args:R '())
(put 'org-babel-default-header-args:R 'safe-local-variable
     (org-babel-header-args-safe-fn ob-R-safe-header-args))

(defcustom org-babel-R-command "R --slave --no-save"
  "Name of command to use for executing R code."
  :group 'org-babel
  :version "24.1"
  :type 'string)

;; The usage of utils::read.table() ensures that the command
;; read.table() can be found even in circumstances when the utils
;; package is not in the search path from R.
(defconst ob-R-transfer-variable-table-with-header
  "%s <- local({
     con <- textConnection(
       %S
     )
     res <- utils::read.table(
       con,
       header    = %s,
       row.names = %s,
       sep       = \"\\t\",
       as.is     = TRUE
     )
     close(con)
     res
   })"
  "R code used to transfer a table defined as a variable from org to R.

This function is used when the table contains a header.")

(defconst ob-R-transfer-variable-table-without-header
  "%s <- local({
     con <- textConnection(
       %S
     )
     res <- utils::read.table(
       con,
       header    = %s,
       row.names = %s,
       sep       = \"\\t\",
       as.is     = TRUE,
       fill      = TRUE,
       col.names = paste(\"V\", seq_len(%d), sep =\"\")
     )
     close(con)
     res
   })"
  "R code used to transfer a table defined as a variable from org to R.

This function is used when the table does not contain a header.")

(defun org-babel-expand-body:R (body params &optional _graphics-file)
  "Expand BODY according to PARAMS, return the expanded body."
  (mapconcat 'identity
	     (append
	      (when (cdr (assq :prologue params))
		(list (cdr (assq :prologue params))))
	      (org-babel-variable-assignments:R params)
	      (list body)
	      (when (cdr (assq :epilogue params))
		(list (cdr (assq :epilogue params)))))
	     "\n"))

(defun org-babel-execute:R (body params)
  "Execute a block of R code BODY according to PARAMS.
This function is called by `org-babel-execute-src-block'."
  (save-excursion
    (let* ((result-params (cdr (assq :result-params params)))
	   (result-type (cdr (assq :result-type params)))
           (async (org-babel-comint-use-async params))
           (session (org-babel-R-initiate-session
		     (cdr (assq :session params)) params))
	   (graphics-file (and (member "graphics" (assq :result-params params))
			       (org-babel-graphical-output-file params)))
	   (colnames-p (unless graphics-file (cdr (assq :colnames params))))
	   (rownames-p (unless graphics-file (cdr (assq :rownames params))))
	   (full-body
	    (let ((inside
		   (list (org-babel-expand-body:R body params graphics-file))))
	      (mapconcat 'identity
			 (if graphics-file
			     (append
			      (list (org-babel-R-construct-graphics-device-call
				     graphics-file params))
			      inside
			      (list "},error=function(e){plot(x=-1:1, y=-1:1, type='n', xlab='', ylab='', axes=FALSE); text(x=0, y=0, labels=e$message, col='red'); paste('ERROR', e$message, sep=' : ')}); dev.off()"))
			   inside)
			 "\n")))
	   (result
	    (org-babel-R-evaluate
	     session full-body result-type result-params
	     (or (equal "yes" colnames-p)
		 (org-babel-pick-name
		  (cdr (assq :colname-names params)) colnames-p))
	     (or (equal "yes" rownames-p)
		 (org-babel-pick-name
		  (cdr (assq :rowname-names params)) rownames-p))
             async)))
      (if graphics-file nil result))))

(defun org-babel-prep-session:R (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-R-initiate-session session params))
	 (var-lines (org-babel-variable-assignments:R params)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (end-of-line 1) (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session))
	    var-lines))
    session))

(defun org-babel-load-session:R (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:R session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))

;; helper functions

(defun org-babel-variable-assignments:R (params)
  "Return list of R statements assigning the block's variables.
Retrieve variables from PARAMS."
  (let ((vars (org-babel--get-vars params)))
    (mapcar
     (lambda (pair)
       (org-babel-R-assign-elisp
	(car pair) (cdr pair)
	(equal "yes" (cdr (assq :colnames params)))
	(equal "yes" (cdr (assq :rownames params)))))
     (mapcar
      (lambda (i)
	(cons (car (nth i vars))
	      (org-babel-reassemble-table
	       (cdr (nth i vars))
	       (cdr (nth i (cdr (assq :colname-names params))))
	       (cdr (nth i (cdr (assq :rowname-names params)))))))
      (number-sequence 0 (1- (length vars)))))))

(defun org-babel-R-quote-tsv-field (s)
  "Quote field S for export to R."
  (if (stringp s)
      (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\"")
    (format "%S" s)))

(defun org-babel-R-assign-elisp (name value colnames-p rownames-p)
  "Construct R code assigning the elisp VALUE to a variable named NAME."
  (if (listp value)
      (let* ((lengths (mapcar 'length (cl-remove-if-not 'listp value)))
	     (max (if lengths (apply 'max lengths) 0))
	     (min (if lengths (apply 'min lengths) 0)))
        ;; Ensure VALUE has an orgtbl structure (depth of at least 2).
        (unless (listp (car value)) (setq value (mapcar 'list value)))
	(let ((file (orgtbl-to-tsv value '(:fmt org-babel-R-quote-tsv-field)))
	      (header (if (or (eq (nth 1 value) 'hline) colnames-p)
			  "TRUE" "FALSE"))
	      (row-names (if rownames-p "1" "NULL")))
	  (if (= max min)
	      (format ob-R-transfer-variable-table-with-header
		      name file header row-names)
	    (format ob-R-transfer-variable-table-without-header
		    name file header row-names max))))
    (cond ((integerp value) (format "%s <- %s" name (concat (number-to-string value) "L")))
	  ((floatp   value) (format "%s <- %s" name value))
	  ((stringp  value) (format "%s <- %S" name (org-no-properties value)))
	  (t                (format "%s <- %S" name (prin1-to-string value))))))


(defvar ess-current-process-name) ; dynamically scoped
(defvar ess-local-process-name)   ; dynamically scoped
(defvar ess-ask-for-ess-directory) ; dynamically scoped
(defvar ess-gen-proc-buffer-name-function) ; defined in ess-inf.el
(defun org-babel-R-initiate-session (session params)
  "Create or return the current R SESSION buffer.
Use PARAMS to set default directory when creating a new session."
  (unless (string= session "none")
    (let* ((session (or session "*R*"))
	   (ess-ask-for-ess-directory
	    (and (boundp 'ess-ask-for-ess-directory)
		 ess-ask-for-ess-directory
		 (not (cdr (assq :dir params)))))
           ;; Make ESS name the process buffer as SESSION.
           (ess-gen-proc-buffer-name-function
            (lambda (_) session)))
      (if (org-babel-comint-buffer-livep session)
	  session
	(save-window-excursion
	  (when (get-buffer session)
	    ;; Session buffer exists, but with dead process
	    (set-buffer session))
          (org-require-package 'ess-r-mode "ESS")
          (set-buffer (run-ess-r))
	  (let ((R-proc (get-process (or ess-local-process-name
					 ess-current-process-name))))
	    (while (process-get R-proc 'callbacks)
	      (ess-wait-for-process R-proc)))
	  (current-buffer))))))

(defun org-babel-R-associate-session (session)
  "Associate R code buffer with an R session.
Make SESSION be the inferior ESS process associated with the
current code buffer."
  (when-let ((process (get-buffer-process session)))
    (setq ess-local-process-name (process-name process))
    (ess-make-buffer-current))
  (setq-local ess-gen-proc-buffer-name-function (lambda (_) session)))

(defvar org-babel-R-graphics-devices
  '((:bmp "bmp" "filename")
    (:jpg "jpeg" "filename")
    (:jpeg "jpeg" "filename")
    (:tikz "tikz" "file")
    (:tiff "tiff" "filename")
    (:png "png" "filename")
    (:svg "svg" "file")
    (:pdf "pdf" "file")
    (:ps "postscript" "file")
    (:postscript "postscript" "file"))
  "An alist mapping graphics file types to R functions.

Each member of this list is a list with three members:
1. the file extension of the graphics file, as an elisp :keyword
2. the R graphics device function to call to generate such a file
3. the name of the argument to this function which specifies the
   file to write to (typically \"file\" or \"filename\")")

(defun org-babel-R-construct-graphics-device-call (out-file params)
  "Construct the call to the graphics device."
  (let* ((allowed-args '(:width :height :bg :units :pointsize
				:antialias :quality :compression :res
				:type :family :title :fonts :version
				:paper :encoding :pagecentre :colormodel
				:useDingbats :horizontal))
	 (device (file-name-extension out-file))
	 (device-info (or (assq (intern (concat ":" device))
				org-babel-R-graphics-devices)
                          (assq :png org-babel-R-graphics-devices)))
         (extra-args (cdr (assq :R-dev-args params))) filearg args)
    (setq device (nth 1 device-info))
    (setq filearg (nth 2 device-info))
    (setq args (mapconcat
		(lambda (pair)
		  (if (member (car pair) allowed-args)
		      (format ",%s=%S"
			      (substring (symbol-name (car pair)) 1)
			      (cdr pair)) ""))
		params ""))
    (format "%s(%s=\"%s\"%s%s%s); tryCatch({"
	    device filearg out-file args
	    (if extra-args "," "") (or extra-args ""))))

(defconst org-babel-R-eoe-indicator "'org_babel_R_eoe'")
(defconst org-babel-R-eoe-output "[1] \"org_babel_R_eoe\"")

(defconst org-babel-R-write-object-command "{
    function(object,transfer.file) {
        object
        invisible(
            if (
                inherits(
                    try(
                        {
                            tfile<-tempfile()
                            write.table(object, file=tfile, sep=\"\\t\",
                                        na=\"\",row.names=%s,col.names=%s,
                                        quote=FALSE)
                            file.rename(tfile,transfer.file)
                        },
                        silent=TRUE),
                    \"try-error\"))
                {
                    if(!file.exists(transfer.file))
                        file.create(transfer.file)
                }
            )
    }
}(object=%s,transfer.file=\"%s\")"
  "Template for an R command to evaluate a block of code and write result to file.

Has four %s escapes to be filled in:
1. Row names, \"TRUE\" or \"FALSE\"
2. Column names, \"TRUE\" or \"FALSE\"
3. The code to be run (must be an expression, not a statement)
4. The name of the file to write to")

(defun org-babel-R-evaluate
    (session body result-type result-params column-names-p row-names-p async)
  "Evaluate R code in BODY."
  (if session
      (if async
          (ob-session-async-org-babel-R-evaluate-session
           session body result-type column-names-p row-names-p)
        (org-babel-R-evaluate-session
         session body result-type result-params column-names-p row-names-p))
    (org-babel-R-evaluate-external-process
     body result-type result-params column-names-p row-names-p)))

(defun org-babel-R-evaluate-external-process
    (body result-type result-params column-names-p row-names-p)
  "Evaluate BODY in external R process.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as elisp."
  (cl-case result-type
    (value
     (let ((tmp-file (org-babel-temp-file "R-")))
       (org-babel-eval org-babel-R-command
		       (format org-babel-R-write-object-command
			       (if row-names-p "TRUE" "FALSE")
			       (if column-names-p
				   (if row-names-p "NA" "TRUE")
				 "FALSE")
			       (format "{function ()\n{\n%s\n}}()" body)
			       (org-babel-process-file-name tmp-file 'noquote)))
       (org-babel-R-process-value-result
	(org-babel-result-cond result-params
	  (with-temp-buffer
	    (insert-file-contents tmp-file)
	    (org-babel-chomp (buffer-string) "\n"))
	  (org-babel-import-elisp-from-file tmp-file '(16)))
	column-names-p)))
    (output (org-babel-eval org-babel-R-command body))))

(defvar ess-eval-visibly-p)

(defun org-babel-R-evaluate-session
    (session body result-type result-params column-names-p row-names-p)
  "Evaluate BODY in SESSION.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as elisp."
  (cl-case result-type
    (value
     (with-temp-buffer
       (insert (org-babel-chomp body))
       (let ((ess-local-process-name
	      (process-name (get-buffer-process session)))
	     (ess-eval-visibly-p nil))
	 (ess-eval-buffer nil)))
     (let ((tmp-file (org-babel-temp-file "R-")))
       (org-babel-comint-eval-invisibly-and-wait-for-file
	session tmp-file
	(format org-babel-R-write-object-command
		(if row-names-p "TRUE" "FALSE")
		(if column-names-p
		    (if row-names-p "NA" "TRUE")
		  "FALSE")
		".Last.value" (org-babel-process-file-name tmp-file 'noquote)))
       (org-babel-R-process-value-result
	(org-babel-result-cond result-params
	  (with-temp-buffer
	    (insert-file-contents tmp-file)
	    (org-babel-chomp (buffer-string) "\n"))
	  (org-babel-import-elisp-from-file tmp-file '(16)))
	column-names-p)))
    (output
     (mapconcat
      'org-babel-chomp
      (butlast
       (delq nil
	     (mapcar
	      (lambda (line) (when (> (length line) 0) line))
	      (mapcar
	       (lambda (line) ;; cleanup extra prompts left in output
		 (if (string-match
		      "^\\([>+.]\\([ ][>.+]\\)*[ ]\\)"
		      (car (split-string line "\n")))
		     (substring line (match-end 1))
		   line))
	       (with-current-buffer session
		 (let ((comint-prompt-regexp (concat "^" comint-prompt-regexp)))
		   (org-babel-comint-with-output (session org-babel-R-eoe-output)
		     (insert (mapconcat 'org-babel-chomp
					(list body org-babel-R-eoe-indicator)
					"\n"))
		     (inferior-ess-send-input)))))))) "\n"))))

(defun org-babel-R-process-value-result (result column-names-p)
  "R-specific processing of return value.
Insert hline if column names in output have been requested."
  (if column-names-p
      (condition-case nil
	  (cons (car result) (cons 'hline (cdr result)))
	(error "Could not parse R result"))
    result))


;;; async evaluation

(defconst ob-session-async-R-indicator "'ob_comint_async_R_%s_%s'")

(defun ob-session-async-org-babel-R-evaluate-session
    (session body result-type column-names-p row-names-p)
  "Asynchronously evaluate BODY in SESSION.
Returns a placeholder string for insertion, to later be replaced
by `org-babel-comint-async-filter'."
  (org-babel-comint-async-register
   session (current-buffer)
   "^\\(?:[>.+] \\)*\\[1\\] \"ob_comint_async_R_\\(start\\|end\\|file\\)_\\(.+\\)\"$"
   'org-babel-chomp
   'ob-session-async-R-value-callback)
  (cl-case result-type
    (value
     (let ((tmp-file (org-babel-temp-file "R-")))
       (with-temp-buffer
         (insert
          (org-babel-chomp body))
         (let ((ess-local-process-name
                (process-name (get-buffer-process session))))
           (ess-eval-buffer nil)))
       (with-temp-buffer
	 (insert
	  (mapconcat
           'org-babel-chomp
           (list (format org-babel-R-write-object-command
                         (if row-names-p "TRUE" "FALSE")
                         (if column-names-p
                             (if row-names-p "NA" "TRUE")
                           "FALSE")
                         ".Last.value"
                         (org-babel-process-file-name tmp-file 'noquote))
                 (format ob-session-async-R-indicator
                         "file" tmp-file))
           "\n"))
	 (let ((ess-local-process-name
		(process-name (get-buffer-process session))))
	   (ess-eval-buffer nil)))
       tmp-file))
    (output
     (let ((uuid (org-id-uuid))
           (ess-local-process-name
            (process-name (get-buffer-process session)))
           (ess-eval-visibly-p nil))
       (with-temp-buffer
         (insert (format ob-session-async-R-indicator
			 "start" uuid))
         (insert "\n")
         (insert body)
         (insert "\n")
         (insert (format ob-session-async-R-indicator
			 "end" uuid))
         (ess-eval-buffer nil ))
       uuid))))

(defun ob-session-async-R-value-callback (params tmp-file)
  "Callback for async value results.
Assigned locally to `org-babel-comint-async-file-callback' in R
comint buffers used for asynchronous Babel evaluation."
  (let* ((graphics-file (and (member "graphics" (assq :result-params params))
			     (org-babel-graphical-output-file params)))
	 (colnames-p (unless graphics-file (cdr (assq :colnames params)))))
    (org-babel-R-process-value-result
     (org-babel-result-cond (assq :result-params params)
       (with-temp-buffer
         (insert-file-contents tmp-file)
         (org-babel-chomp (buffer-string) "\n"))
       (org-babel-import-elisp-from-file tmp-file '(16)))
     (or (equal "yes" colnames-p)
	 (org-babel-pick-name
	  (cdr (assq :colname-names params)) colnames-p)))))



;;; ob-session-async-R.el ends here


(provide 'ob-R)

;;; ob-R.el ends here
