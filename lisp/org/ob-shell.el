;;; ob-shell.el --- Babel Functions for Shell Evaluation -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2025 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Maintainer: Matthew Trzcinski <matt@excalamus.com>
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

;; Org-Babel support for evaluating shell source code.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ob)
(require 'org-macs)
(require 'shell)
(require 'cl-lib)

(declare-function org-babel-comint-in-buffer "ob-comint" (buffer &rest body)
		  t)
(declare-function org-babel-comint-wait-for-output "ob-comint" (buffer))
(declare-function org-babel-comint-buffer-livep "ob-comint" (buffer))
(declare-function org-babel-comint-with-output "ob-comint" (meta &rest body)
		  t)
(declare-function orgtbl-to-generic "org-table" (table params))

(defvar org-babel-default-header-args:shell '())

(defconst org-babel-header-args:shell
  '((async               . ((yes no))))
  "Shell-specific header arguments.")

(defvar org-babel-shell-names)

(defconst org-babel-shell-set-prompt-commands
  '(;; Fish has no PS2 equivalent.
    ("fish" . "function fish_prompt\n\techo \"%s\"\nend")
    ;; prompt2 is like PS2 in POSIX shells.
    ("csh" . "set prompt=\"%s\"\nset prompt2=\"\"")
    ;; PROMPT_COMMAND can override PS1 settings.  Disable it.
    ;; Disable PS2 to avoid garbage in multi-line inputs.
    (t . "PROMPT_COMMAND=;PS1=\"%s\";PS2="))
  "Alist assigning shells with their prompt setting command.

Each element of the alist associates a shell type from
`org-babel-shell-names' with a template used to create a command to
change the default prompt.  The template is an argument to `format'
that will be called with a single additional argument: prompt string.

The fallback association template is defined in (t . \"template\")
alist element.")

(defun org-babel-shell-initialize ()
  "Define execution functions associated to shell names.
This function has to be called whenever `org-babel-shell-names'
is modified outside the Customize interface."
  (interactive)
  (dolist (name org-babel-shell-names)
    (let ((fname (intern (concat "org-babel-execute:" name))))
      (defalias fname
        (lambda (body params)
	  (:documentation
           (format "Execute a block of %s commands with Babel." name))
	  (let ((explicit-shell-file-name name)
                (shell-file-name name))
	    (org-babel-execute:shell body params))))
      (put fname 'definition-name 'org-babel-shell-initialize))
    (defalias (intern (concat "org-babel-variable-assignments:" name))
      #'org-babel-variable-assignments:shell
      (format "Return list of %s statements assigning to the block's \
variables."
	      name))
    (funcall (if (fboundp 'defvar-1) #'defvar-1 #'set) ;Emacs-29
             (intern (concat "org-babel-default-header-args:" name))
             org-babel-default-header-args:shell)
    (funcall (if (fboundp 'defvar-1) #'defvar-1 #'set) ;Emacs-29
             (intern (concat "org-babel-header-args:" name))
             org-babel-header-args:shell)))

(defcustom org-babel-shell-names
  '("sh" "bash" "zsh" "fish" "csh" "ash" "dash" "ksh" "mksh" "posh")
  "List of names of shell supported by babel shell code blocks.
Call `org-babel-shell-initialize' when modifying this variable
outside the Customize interface."
  :group 'org-babel
  :type '(repeat (string :tag "Shell name: "))
  :set (lambda (symbol value)
	 (set-default-toplevel-value symbol value)
	 (org-babel-shell-initialize)))

(defcustom org-babel-shell-results-defaults-to-output t
  "Let shell execution defaults to \":results output\".

When set to t, use \":results output\" when no :results setting
is set.  This is especially useful for inline source blocks.

When set to nil, stick to the convention of using :results value
as the default setting when no :results is set, the \"value\" of
a shell execution being its exit code."
  :group 'org-babel
  :type 'boolean
  :package-version '(Org . "9.4"))

(defun org-babel-execute:shell (body params)
  "Execute Shell BODY according to PARAMS.
This function is called by `org-babel-execute-src-block'."
  (let* ((session (org-babel-sh-initiate-session
		   (cdr (assq :session params))))
	 (stdin (let ((stdin (cdr (assq :stdin params))))
                  (when stdin (org-babel-sh-var-to-string
                               (org-babel-ref-resolve stdin)))))
	 (results-params (cdr (assq :result-params params)))
	 (value-is-exit-status
	  (or (and
	       (equal '("replace") results-params)
	       (not org-babel-shell-results-defaults-to-output))
	      (member "value" results-params)))
	 (cmdline (cdr (assq :cmdline params)))
         (full-body (concat
		     (org-babel-expand-body:generic
		      body params (org-babel-variable-assignments:shell params))
		     (when value-is-exit-status "\necho $?"))))
    (org-babel-reassemble-table
     (org-babel-sh-evaluate session full-body params stdin cmdline)
     (org-babel-pick-name
      (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
     (org-babel-pick-name
      (cdr (assq :rowname-names params)) (cdr (assq :rownames params))))))

(defun org-babel-prep-session:shell (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-sh-initiate-session session))
	 (var-lines (org-babel-variable-assignments:shell params)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session))
	    var-lines))
    session))

(defun org-babel-load-session:shell (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:shell session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))


;;; Helper functions
(defun org-babel--variable-assignments:sh-generic
    (varname values &optional sep hline)
  "Return a list of statements declaring the values as a generic variable."
  (format "%s=%s" varname (org-babel-sh-var-to-sh values sep hline)))

(defun org-babel--variable-assignments:fish
    (varname values &optional sep hline)
  "Return a list of statements declaring the values as a fish variable."
  (format "set %s %s" varname (org-babel-sh-var-to-sh values sep hline)))

(defun org-babel--variable-assignments:bash_array
    (varname values &optional sep hline)
  "Return a list of statements declaring the values as a bash array."
  (format "unset %s\ndeclare -a %s=( %s )"
	  varname varname
	  (mapconcat
	   (lambda (value) (org-babel-sh-var-to-sh value sep hline))
	   values
	   " ")))

(defun org-babel--variable-assignments:bash_assoc
    (varname values &optional sep hline)
  "Return a list of statements declaring the values as bash associative array."
  (format "unset %s\ndeclare -A %s\n%s"
	  varname varname
	  (mapconcat
	   (lambda (items)
	     (format "%s[%s]=%s"
		     varname
		     (org-babel-sh-var-to-sh (car items) sep hline)
		     (org-babel-sh-var-to-sh (cdr items) sep hline)))
	   values
	   "\n")))

(defun org-babel--variable-assignments:bash (varname values &optional sep hline)
  "Represent the parameters as useful Bash shell variables."
  (pcase values
    (`((,_ ,_ . ,_) . ,_)		;two-dimensional array
     (org-babel--variable-assignments:bash_assoc varname values sep hline))
    (`(,_ . ,_)				;simple list
     (org-babel--variable-assignments:bash_array varname values sep hline))
    (_					;scalar value
     (org-babel--variable-assignments:sh-generic varname values sep hline))))

(defun org-babel-variable-assignments:shell (params)
  "Return list of shell statements assigning the block's variables."
  (let ((sep (cdr (assq :separator params)))
	(hline (when (string= "yes" (cdr (assq :hlines params)))
		 (or (cdr (assq :hline-string params))
		     "hline"))))
    (mapcar
     (lambda (pair)
       (if (string-suffix-p "bash" shell-file-name)
	   (org-babel--variable-assignments:bash
            (car pair) (cdr pair) sep hline)
         (if (string-suffix-p "fish" shell-file-name)
	     (org-babel--variable-assignments:fish
              (car pair) (cdr pair) sep hline)
           (org-babel--variable-assignments:sh-generic
	    (car pair) (cdr pair) sep hline))))
     (org-babel--get-vars params))))

(defun org-babel-sh-var-to-sh (var &optional sep hline)
  "Convert an elisp value to a shell variable.
Convert an elisp var into a string of shell commands specifying a
var of the same value."
  (concat "'" (replace-regexp-in-string
	       "'" "'\"'\"'"
	       (org-babel-sh-var-to-string var sep hline))
	  "'"))

(defun org-babel-sh-var-to-string (var &optional sep hline)
  "Convert an elisp value to a string."
  (let ((echo-var (lambda (v) (if (stringp v) v (format "%S" v)))))
    (cond
     ((and (listp var) (or (listp (car var)) (eq (car var) 'hline)))
      (orgtbl-to-generic var  (list :sep (or sep "\t") :fmt echo-var
				    :hline hline)))
     ((listp var)
      (mapconcat echo-var var "\n"))
     (t (funcall echo-var var)))))

(defvar org-babel-sh-eoe-indicator "echo 'org_babel_sh_eoe'"
  "String to indicate that evaluation has completed.")
(defvar org-babel-sh-eoe-output "org_babel_sh_eoe"
  "String to indicate that evaluation has completed.")
(defvar org-babel-sh-prompt "org_babel_sh_prompt> "
  "String to set prompt in session shell.")

(defun org-babel-sh-initiate-session (&optional session _params)
  "Initiate a session named SESSION according to PARAMS."
  (when (and session (not (string= session "none")))
    (save-window-excursion
      (or (org-babel-comint-buffer-livep session)
          (progn
	    (shell session)
            ;; Set unique prompt for easier analysis of the output.
            (org-babel-comint-wait-for-output (current-buffer))
            (org-babel-comint-input-command
             (current-buffer)
             (format
              (or (cdr (assoc (file-name-nondirectory shell-file-name)
                              org-babel-shell-set-prompt-commands))
                  (alist-get t org-babel-shell-set-prompt-commands))
              org-babel-sh-prompt))
            (setq-local
             org-babel-comint-prompt-regexp-old comint-prompt-regexp
             comint-prompt-regexp
             (concat "^" (regexp-quote org-babel-sh-prompt)
                     " *"))
	    ;; Needed for Emacs 23 since the marker is initially
	    ;; undefined and the filter functions try to use it without
	    ;; checking.
	    (set-marker comint-last-output-start (point))
	    (get-buffer (current-buffer)))))))

(defconst ob-shell-async-indicator "echo 'ob_comint_async_shell_%s_%s'"
  "Session output delimiter template.
See `org-babel-comint-async-indicator'.")

(defun ob-shell-async-chunk-callback (string)
  "Filter applied to results before insertion.
See `org-babel-comint-async-chunk-callback'."
  (replace-regexp-in-string comint-prompt-regexp "" string))

(defun org-babel-sh-evaluate (session body &optional params stdin cmdline)
  "Pass BODY to the Shell process in BUFFER.
If RESULT-TYPE equals `output' then return a list of the outputs
of the statements in BODY, if RESULT-TYPE equals `value' then
return the value of the last statement in BODY."
  (let* ((shebang (cdr (assq :shebang params)))
         (async (org-babel-comint-use-async params))
	 (results-params (cdr (assq :result-params params)))
	 (value-is-exit-status
	  (or (and
	       (equal '("replace") results-params)
	       (not org-babel-shell-results-defaults-to-output))
	      (member "value" results-params)))
	 (results
	  (cond
	   ((or stdin cmdline)	       ; external shell script w/STDIN
	    (let ((script-file (org-babel-temp-file "sh-script-"))
		  (stdin-file (org-babel-temp-file "sh-stdin-"))
		  (padline (not (string= "no" (cdr (assq :padline params))))))
	      (with-temp-file script-file
		(when shebang (insert shebang "\n"))
		(when padline (insert "\n"))
		(insert body))
	      (set-file-modes script-file #o755)
	      (with-temp-file stdin-file (insert (or stdin "")))
	      (with-temp-buffer
                (with-connection-local-variables
                 (apply #'process-file
                        (if shebang (file-local-name script-file)
                          shell-file-name)
		        stdin-file
                        (current-buffer)
                        nil
                        (if shebang (when cmdline (list cmdline))
                          (list shell-command-switch
                                (concat (file-local-name script-file)  " " cmdline)))))
		(buffer-string))))
	   (session			; session evaluation
            (if async
                (progn
                  (let ((uuid (org-id-uuid)))
                    (org-babel-comint-async-register
                     session
                     (current-buffer)
                     "ob_comint_async_shell_\\(start\\|end\\|file\\)_\\(.+\\)"
                     'ob-shell-async-chunk-callback
                     nil)
                    (org-babel-comint-async-delete-dangling-and-eval
                        session
                      (insert (format ob-shell-async-indicator "start" uuid))
                      (comint-send-input nil t)
                      (insert (org-trim body))
                      (comint-send-input nil t)
                      (insert (format ob-shell-async-indicator "end" uuid))
                      (comint-send-input nil t))
                    uuid))
	      (mapconcat
	       #'org-babel-sh-strip-weird-long-prompt
	       (mapcar
	        #'org-trim
	        (butlast ; Remove eoe indicator
	         (org-babel-comint-with-output
		     (session org-babel-sh-eoe-output t body)
                   (insert (org-trim body) "\n"
                           org-babel-sh-eoe-indicator)
		   (comint-send-input nil t))
                 ;; Remove `org-babel-sh-eoe-indicator' output line.
	         1))
	       "\n")))
	   ;; External shell script, with or without a predefined
	   ;; shebang.
	   ((org-string-nw-p shebang)
	    (let ((script-file (org-babel-temp-file "sh-script-"))
		  (padline (not (equal "no" (cdr (assq :padline params))))))
	      (with-temp-file script-file
		(insert shebang "\n")
		(when padline (insert "\n"))
		(insert body))
	      (set-file-modes script-file #o755)
              (if (file-remote-p script-file)
                  ;; Run remote script using its local path as COMMAND.
                  ;; The remote execution is ensured by setting
                  ;; correct `default-directory'.
                  (let ((default-directory (file-name-directory script-file)))
                    (org-babel-eval (file-local-name script-file) ""))
	        (org-babel-eval script-file ""))))
	   (t (org-babel-eval shell-file-name (org-trim body))))))
    (when (and results value-is-exit-status)
      (setq results (car (reverse (split-string results "\n" t)))))
    (when results
      (let ((result-params (cdr (assq :result-params params))))
        (org-babel-result-cond result-params
          results
          (let ((tmp-file (org-babel-temp-file "sh-")))
            (with-temp-file tmp-file (insert results))
            (org-babel-import-elisp-from-file tmp-file)))))))

(defun org-babel-sh-strip-weird-long-prompt (string)
  "Remove prompt cruft from a string of shell output."
  (while (string-match "^% +[\r\n$]+ *" string)
    (setq string (substring string (match-end 0))))
  string)

(provide 'ob-shell)

;;; ob-shell.el ends here
