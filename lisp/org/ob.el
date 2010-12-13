;;; ob.el --- working with code blocks in org-mode

;; Copyright (C) 2009, 2010  Free Software Foundation, Inc.

;; Author: Eric Schulte, Dan Davison
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 7.4

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

;; See the online documentation for more information
;;
;;   http://orgmode.org/worg/org-contrib/babel/

;;; Code:
(eval-when-compile
  (require 'org-list)
  (require 'cl))
(require 'ob-eval)
(require 'org-macs)

(defvar org-babel-call-process-region-original)
(declare-function show-all "outline" ())
(declare-function tramp-compat-make-temp-file "tramp-compat"
                  (filename &optional dir-flag))
(declare-function tramp-dissect-file-name "tramp" (name &optional nodefault))
(declare-function tramp-file-name-user "tramp" (vec))
(declare-function tramp-file-name-host "tramp" (vec))
(declare-function with-parsed-tramp-file-name "tramp" (filename var &rest body))
(declare-function org-icompleting-read "org" (&rest args))
(declare-function org-edit-src-code "org-src"
                  (&optional context code edit-buffer-name quietp))
(declare-function org-edit-src-exit "org-src"  (&optional context))
(declare-function org-open-at-point "org" (&optional in-emacs reference-buffer))
(declare-function org-save-outline-visibility "org" (use-markers &rest body))
(declare-function org-outline-overlay-data "org" (&optional use-markers))
(declare-function org-set-outline-overlay-data "org" (data))
(declare-function org-narrow-to-subtree "org" ())
(declare-function org-entry-get "org"
		  (pom property &optional inherit literal-nil))
(declare-function org-make-options-regexp "org" (kwds &optional extra))
(declare-function org-do-remove-indentation "org" (&optional n))
(declare-function org-show-context "org" (&optional key))
(declare-function org-at-table-p "org" (&optional table-type))
(declare-function org-cycle "org" (&optional arg))
(declare-function org-uniquify "org" (list))
(declare-function org-current-level "org" ())
(declare-function org-table-import "org-table" (file arg))
(declare-function org-add-hook "org-compat"
		  (hook function &optional append local))
(declare-function org-table-align "org-table" ())
(declare-function org-table-end "org-table" (&optional table-type))
(declare-function orgtbl-to-generic "org-table" (table params))
(declare-function orgtbl-to-orgtbl "org-table" (table params))
(declare-function org-babel-lob-get-info "ob-lob" nil)
(declare-function org-babel-ref-split-args "ob-ref" (arg-string))
(declare-function org-babel-ref-parse "ob-ref" (assignment))
(declare-function org-babel-ref-resolve "ob-ref" (ref))
(declare-function org-babel-lob-execute-maybe "ob-lob" ())
(declare-function org-number-sequence "org-compat" (from &optional to inc))
(declare-function org-in-item-p "org-list" ())
(declare-function org-list-parse-list "org-list" (&optional delete))
(declare-function org-list-to-generic "org-list" (LIST PARAMS))
(declare-function org-list-bottom-point "org-list" ())

(defgroup org-babel nil
  "Code block evaluation and management in `org-mode' documents."
  :tag "Babel"
  :group 'org)

(defcustom org-confirm-babel-evaluate t
  "Confirm before evaluation.
Require confirmation before interactively evaluating code
blocks in Org-mode buffers.  The default value of this variable
is t, meaning confirmation is required for any code block
evaluation.  This variable can be set to nil to inhibit any
future confirmation requests.  This variable can also be set to a
function which takes two arguments the language of the code block
and the body of the code block.  Such a function should then
return a non-nil value if the user should be prompted for
execution or nil if no prompt is required.

Warning: Disabling confirmation may result in accidental
evaluation of potentially harmful code.  It may be advisable
remove code block execution from C-c C-c as further protection
against accidental code block evaluation.  The
`org-babel-no-eval-on-ctrl-c-ctrl-c' variable can be used to
remove code block execution from the C-c C-c keybinding."
    :group 'org-babel
    :type '(choice boolean function))
;; don't allow this variable to be changed through file settings
(put 'org-confirm-babel-evaluate 'safe-local-variable (lambda (x) (eq x t)))

(defcustom org-babel-no-eval-on-ctrl-c-ctrl-c nil
  "Remove code block evaluation from the C-c C-c key binding."
  :group 'org-babel
  :type 'boolean)

(defvar org-babel-src-name-regexp
  "^[ \t]*#\\+\\(srcname\\|source\\|function\\):[ \t]*"
  "Regular expression used to match a source name line.")

(defvar org-babel-multi-line-header-regexp
  "^[ \t]*#\\+headers?:[ \t]*\\([^\n]*\\)$"
  "Regular expression used to match multi-line header arguments.")

(defvar org-babel-src-name-w-name-regexp
  (concat org-babel-src-name-regexp
	  "\\("
	  org-babel-multi-line-header-regexp
	  "\\)*"
	  "\\([^ ()\f\t\n\r\v]+\\)\\(\(\\(.*\\)\)\\|\\)")
  "Regular expression matching source name lines with a name.")

(defvar org-babel-src-block-regexp
  (concat
   ;; (1) indentation                     (2) lang
   "^\\([ \t]*\\)#\\+begin_src[ \t]+\\([^ \f\t\n\r\v]+\\)[ \t]*"
   ;; (3) switches
   "\\([^\":\n]*\"[^\"\n*]*\"[^\":\n]*\\|[^\":\n]*\\)"
   ;; (4) header arguments
   "\\([^\n]*\\)\n"
   ;; (5) body
   "\\([^\000]+?\n\\)[ \t]*#\\+end_src")
  "Regexp used to identify code blocks.")

(defvar org-babel-inline-src-block-regexp
  (concat
   ;; (1) replacement target (2) lang
   "[ \f\t\n\r\v]\\(src_\\([^ \f\t\n\r\v]+\\)"
   ;; (3,4) (unused, headers)
   "\\(\\|\\[\\(.*?\\)\\]\\)"
   ;; (5) body
   "{\\([^\f\n\r\v]+?\\)}\\)")
  "Regexp used to identify inline src-blocks.")

(defun org-babel-get-header (params key &optional others)
  "Select only header argument of type KEY from a list.
Optional argument OTHERS indicates that only the header that do
not match KEY should be returned."
  (delq nil
	(mapcar
	 (lambda (p) (when (funcall (if others #'not #'identity) (eq (car p) key)) p))
	 params)))

(defun org-babel-get-src-block-info (&optional light)
  "Get information on the current source block.

Optional argument LIGHT does not resolve remote variable
references; a process which could likely result in the execution
of other code blocks.

Returns a list
 (language body header-arguments-alist switches name indent)."
  (let ((case-fold-search t) head info name indent)
    ;; full code block
    (if (setq head (org-babel-where-is-src-block-head))
	(save-excursion
	  (goto-char head)
	  (setq info (org-babel-parse-src-block-match))
	  (setq indent (car (last info)))
	  (setq info (butlast info))
	  (while (and (forward-line -1)
		      (looking-at org-babel-multi-line-header-regexp))
	    (setf (nth 2 info)
		  (org-babel-merge-params
		   (org-babel-parse-header-arguments (match-string 1))
		   (nth 2 info))))
	  (when (looking-at org-babel-src-name-w-name-regexp)
	    (setq name (org-babel-clean-text-properties (match-string 4)))
	    (when (match-string 6)
	      (setf (nth 2 info) ;; merge functional-syntax vars and header-args
		    (org-babel-merge-params
		     (mapcar (lambda (ref) (cons :var ref))
			     (org-babel-ref-split-args (match-string 6)))
		     (nth 2 info))))))
      ;; inline source block
      (when (save-excursion (re-search-backward "[ \f\t\n\r\v]" nil t)
			    (looking-at org-babel-inline-src-block-regexp))
	(setq info (org-babel-parse-inline-src-block-match))))
    ;; resolve variable references and add summary parameters
    (when (and info (not light))
      (setf (nth 2 info) (org-babel-process-params (nth 2 info))))
    (when info (append info (list name indent)))))

(defun org-babel-confirm-evaluate (info)
  "Confirm evaluation of the code block INFO.
This behavior can be suppressed by setting the value of
`org-confirm-babel-evaluate' to nil, in which case all future
interactive code block evaluations will proceed without any
confirmation from the user.

Note disabling confirmation may result in accidental evaluation
of potentially harmful code."
  (let* ((eval (or (cdr (assoc :eval (nth 2 info)))
		   (when (assoc :noeval (nth 2 info)) "no")))
	 (query (or (equal eval "query")
		    (if (functionp org-confirm-babel-evaluate)
			(funcall org-confirm-babel-evaluate
				 (nth 0 info) (nth 1 info))
		      org-confirm-babel-evaluate))))
    (if (or (equal eval "never") (equal eval "no")
	    (and query
		 (not (yes-or-no-p
		       (format "Evaluate this%scode block%son your system? "
			       (if info (format " %s " (nth 0 info)) " ")
			       (if (nth 4 info)
				   (format " (%s) " (nth 4 info)) " "))))))
	(prog1 nil (message "Evaluation %s"
			    (if (or (equal eval "never") (equal eval "no"))
				"Disabled" "Aborted")))
      t)))

;;;###autoload
(defun org-babel-execute-safely-maybe ()
  (unless org-babel-no-eval-on-ctrl-c-ctrl-c
    (org-babel-execute-maybe)))

(add-hook 'org-ctrl-c-ctrl-c-hook 'org-babel-execute-safely-maybe)

;;;###autoload
(defun org-babel-execute-maybe ()
  (interactive)
  (or (org-babel-execute-src-block-maybe)
      (org-babel-lob-execute-maybe)))

(defun org-babel-execute-src-block-maybe ()
  "Conditionally execute a source block.
Detect if this is context for a Babel src-block and if so
then run `org-babel-execute-src-block'."
  (interactive)
  (let ((info (org-babel-get-src-block-info)))
    (if info
	(progn (org-babel-eval-wipe-error-buffer)
	       (org-babel-execute-src-block current-prefix-arg info) t) nil)))

;;;###autoload
(defun org-babel-expand-src-block-maybe ()
  "Conditionally expand a source block.
Detect if this is context for a org-babel src-block and if so
then run `org-babel-expand-src-block'."
  (interactive)
  (let ((info (org-babel-get-src-block-info)))
    (if info
	(progn (org-babel-expand-src-block current-prefix-arg info) t)
      nil)))

;;;###autoload
(defun org-babel-load-in-session-maybe ()
  "Conditionally load a source block in a session.
Detect if this is context for a org-babel src-block and if so
then run `org-babel-load-in-session'."
  (interactive)
  (let ((info (org-babel-get-src-block-info)))
    (if info
	(progn (org-babel-load-in-session current-prefix-arg info) t)
      nil)))

(add-hook 'org-metaup-hook 'org-babel-load-in-session-maybe)

;;;###autoload
(defun org-babel-pop-to-session-maybe ()
  "Conditionally pop to a session.
Detect if this is context for a org-babel src-block and if so
then run `org-babel-pop-to-session'."
  (interactive)
  (let ((info (org-babel-get-src-block-info)))
    (if info (progn (org-babel-pop-to-session current-prefix-arg info) t) nil)))

(add-hook 'org-metadown-hook 'org-babel-pop-to-session-maybe)

(defconst org-babel-header-arg-names
  '(cache cmdline colnames dir exports file noweb results
	  session tangle var eval noeval comments)
  "Common header arguments used by org-babel.
Note that individual languages may define their own language
specific header arguments as well.")

(defvar org-babel-default-header-args
  '((:session . "none") (:results . "replace") (:exports . "code")
    (:cache . "no") (:noweb . "no") (:hlines . "no") (:tangle . "no"))
  "Default arguments to use when evaluating a source block.")

(defvar org-babel-default-inline-header-args
  '((:session . "none") (:results . "silent") (:exports . "results"))
  "Default arguments to use when evaluating an inline source block.")

(defvar org-babel-current-buffer-properties nil
  "Local cache for buffer properties.")
(make-variable-buffer-local 'org-babel-current-buffer-properties)

(defvar org-babel-result-regexp
  "^[ \t]*#\\+res\\(ults\\|name\\)\\(\\[\\([[:alnum:]]+\\)\\]\\)?\\:[ \t]*"
  "Regular expression used to match result lines.
If the results are associated with a hash key then the hash will
be saved in the second match data.")

(defvar org-babel-result-w-name-regexp
  (concat org-babel-result-regexp
	  "\\([^ ()\f\t\n\r\v]+\\)\\(\(\\(.*\\)\)\\|\\)"))

(defvar org-babel-min-lines-for-block-output 10
  "The minimum number of lines for block output.
If number of lines of output is equal to or exceeds this
value, the output is placed in a #+begin_example...#+end_example
block. Otherwise the output is marked as literal by inserting
colons at the starts of the lines. This variable only takes
effect if the :results output option is in effect.")

(defvar org-babel-noweb-error-langs nil
  "Languages for which Babel will raise literate programming errors.
List of languages for which errors should be raised when the
source code block satisfying a noweb reference in this language
can not be resolved.")

(defvar org-babel-hash-show 4
  "Number of initial characters to show of a hidden results hash.")

(defvar org-babel-after-execute-hook nil
  "Hook for functions to be called after `org-babel-execute-src-block'")
(defun org-babel-named-src-block-regexp-for-name (name)
  "This generates a regexp used to match a src block named NAME."
  (concat org-babel-src-name-regexp (regexp-quote name) "[ \t\n]*"
	  (substring org-babel-src-block-regexp 1)))

;;; functions
(defvar call-process-region)
;;;###autoload

(defun org-babel-execute-src-block (&optional arg info params)
  "Execute the current source code block.
Insert the results of execution into the buffer.  Source code
execution and the collection and formatting of results can be
controlled through a variety of header arguments.

With prefix argument ARG, force re-execution even if a an
existing result cached in the buffer would otherwise have been
returned.

Optionally supply a value for INFO in the form returned by
`org-babel-get-src-block-info'.

Optionally supply a value for PARAMS which will be merged with
the header arguments specified at the front of the source code
block."
  (interactive)
  (let ((info (or info (org-babel-get-src-block-info))))
    (when (org-babel-confirm-evaluate info)
      (let* ((lang (nth 0 info))
	     (params (if params
			 (org-babel-process-params
			  (org-babel-merge-params (nth 2 info) params))
		       (nth 2 info)))
	     (cache? (and (not arg) (cdr (assoc :cache params))
			  (string= "yes" (cdr (assoc :cache params)))))
	     (result-params (cdr (assoc :result-params params)))
	     (new-hash (when cache? (org-babel-sha1-hash info)))
	     (old-hash (when cache? (org-babel-result-hash info)))
	     (body (setf (nth 1 info)
			 (let ((noweb (cdr (assoc :noweb params))))
			   (if (and noweb
				    (or (string= "yes" noweb)
					(string= "tangle" noweb)))
			       (org-babel-expand-noweb-references info)
			     (nth 1 info)))))
	     (cmd (intern (concat "org-babel-execute:" lang)))
	     (dir (cdr (assoc :dir params)))
	     (default-directory
	       (or (and dir (file-name-as-directory dir)) default-directory))
	     (org-babel-call-process-region-original
	      (if (boundp 'org-babel-call-process-region-original)
		  org-babel-call-process-region-original
		(symbol-function 'call-process-region)))
	     (indent (car (last info)))
	     result)
	(unwind-protect
	    (flet ((call-process-region (&rest args)
		    (apply 'org-babel-tramp-handle-call-process-region args)))
	      (unless (fboundp cmd)
		(error "No org-babel-execute function for %s!" lang))
	      (if (and (not arg) new-hash (equal new-hash old-hash))
		  (save-excursion ;; return cached result
		    (goto-char (org-babel-where-is-src-block-result nil info))
		    (end-of-line 1) (forward-char 1)
		    (setq result (org-babel-read-result))
		    (message (replace-regexp-in-string
			      "%" "%%" (format "%S" result))) result)
		(message "executing %s code block%s..."
			 (capitalize lang)
			 (if (nth 4 info) (format " (%s)" (nth 4 info)) ""))
		(setq result
		      ((lambda (result)
			 (cond
			  ((member "file" result-params)
			   (cdr (assoc :file params)))
			  ((and (eq (cdr (assoc :result-type params)) 'value)
				(or (member "vector" result-params)
				    (member "table" result-params))
				(not (listp result)))
			   (list (list result)))
			  (t result)))
		       (funcall cmd body params)))
		(org-babel-insert-result
		 result result-params info new-hash indent lang)
		(run-hooks 'org-babel-after-execute-hook)
		result))
	  (setq call-process-region 'org-babel-call-process-region-original))))))

(defun org-babel-expand-body:generic (body params &optional var-lines)
  "Expand BODY with PARAMS.
Expand a block of code with org-babel according to it's header
arguments.  This generic implementation of body expansion is
called for languages which have not defined their own specific
org-babel-expand-body:lang function."
  (mapconcat #'identity (append var-lines (list body)) "\n"))

;;;###autoload
(defun org-babel-expand-src-block (&optional arg info params)
  "Expand the current source code block.
Expand according to the source code block's header
arguments and pop open the results in a preview buffer."
  (interactive)
  (let* ((info (or info (org-babel-get-src-block-info)))
         (lang (nth 0 info))
	 (params (setf (nth 2 info)
                       (sort (org-babel-merge-params (nth 2 info) params)
                             (lambda (el1 el2) (string< (symbol-name (car el1))
						   (symbol-name (car el2)))))))
         (body (setf (nth 1 info)
		     (if (and (cdr (assoc :noweb params))
                              (string= "yes" (cdr (assoc :noweb params))))
			 (org-babel-expand-noweb-references info) (nth 1 info))))
         (expand-cmd (intern (concat "org-babel-expand-body:" lang)))
	 (assignments-cmd (intern (concat "org-babel-variable-assignments:" lang)))
         (expanded
	  (if (fboundp expand-cmd) (funcall expand-cmd body params)
	    (org-babel-expand-body:generic
	     body params (and (fboundp assignments-cmd) (funcall assignments-cmd params))))))
    (org-edit-src-code
     nil expanded (concat "*Org-Babel Preview " (buffer-name) "[ " lang " ]*"))))

;;;###autoload
(defun org-babel-load-in-session (&optional arg info)
  "Load the body of the current source-code block.
Evaluate the header arguments for the source block before
entering the session.  After loading the body this pops open the
session."
  (interactive)
  (let* ((info (or info (org-babel-get-src-block-info)))
         (lang (nth 0 info))
         (params (nth 2 info))
         (body (setf (nth 1 info)
		     (if (and (cdr (assoc :noweb params))
                              (string= "yes" (cdr (assoc :noweb params))))
                         (org-babel-expand-noweb-references info)
		       (nth 1 info))))
         (session (cdr (assoc :session params)))
	 (dir (cdr (assoc :dir params)))
	 (default-directory
	   (or (and dir (file-name-as-directory dir)) default-directory))
	 (cmd (intern (concat "org-babel-load-session:" lang))))
    (unless (fboundp cmd)
      (error "No org-babel-load-session function for %s!" lang))
    (pop-to-buffer (funcall cmd session body params))
    (end-of-line 1)))

;;;###autoload
(defun org-babel-initiate-session (&optional arg info)
  "Initiate session for current code block.
If called with a prefix argument then resolve any variable
references in the header arguments and assign these variables in
the session. Copy the body of the code block to the kill ring."
  (interactive "P")
  (let* ((info (or info (org-babel-get-src-block-info (not arg))))
         (lang (nth 0 info))
         (body (nth 1 info))
         (params (nth 2 info))
         (session (cdr (assoc :session params)))
	 (dir (cdr (assoc :dir params)))
	 (default-directory
	   (or (and dir (file-name-as-directory dir)) default-directory))
	 (init-cmd (intern (format "org-babel-%s-initiate-session" lang)))
	 (prep-cmd (intern (concat "org-babel-prep-session:" lang))))
    (if (and (stringp session) (string= session "none"))
	(error "This block is not using a session!"))
    (unless (fboundp init-cmd)
      (error "No org-babel-initiate-session function for %s!" lang))
    (with-temp-buffer (insert (org-babel-trim body))
                      (copy-region-as-kill (point-min) (point-max)))
    (when arg
      (unless (fboundp prep-cmd)
	(error "No org-babel-prep-session function for %s!" lang))
      (funcall prep-cmd session params))
    (funcall init-cmd session params)))

;;;###autoload
(defun org-babel-switch-to-session (&optional arg info)
  "Switch to the session of the current code block.
Uses `org-babel-initiate-session' to start the session. If called
with a prefix argument then this is passed on to
`org-babel-initiate-session'."
  (interactive "P")
  (pop-to-buffer (org-babel-initiate-session arg info))
  (end-of-line 1))

(defalias 'org-babel-pop-to-session 'org-babel-switch-to-session)

;;;###autoload
(defun org-babel-switch-to-session-with-code (&optional arg info)
  "Switch to code buffer and display session."
  (interactive "P")
  (flet ((swap-windows
	  ()
	  (let ((other-window-buffer (window-buffer (next-window))))
	    (set-window-buffer (next-window) (current-buffer))
	    (set-window-buffer (selected-window) other-window-buffer))
	  (other-window 1)))
    (let ((info (org-babel-get-src-block-info))
	  (org-src-window-setup 'reorganize-frame))
      (save-excursion
	(org-babel-switch-to-session arg info))
      (org-edit-src-code))
    (swap-windows)))

(defmacro org-babel-do-in-edit-buffer (&rest body)
  "Evaluate BODY in edit buffer if there is a code block at point.
Return t if a code block was found at point, nil otherwise."
  `(let ((org-src-window-setup 'switch-invisibly))
     (when (and (org-babel-where-is-src-block-head)
		(org-edit-src-code nil nil nil 'quietly))
       (unwind-protect (progn ,@body)
	 (if (org-bound-and-true-p org-edit-src-from-org-mode)
	     (org-edit-src-exit)))
       t)))

(defun org-babel-do-key-sequence-in-edit-buffer (key)
  "Read key sequence and execute the command in edit buffer.
Enter a key sequence to be executed in the language major-mode
edit buffer. For example, TAB will alter the contents of the
Org-mode code block according to the effect of TAB in the
language major-mode buffer. For languages that support
interactive sessions, this can be used to send code from the Org
buffer to the session for evaluation using the native major-mode
evaluation mechanisms."
  (interactive "kEnter key-sequence to execute in edit buffer: ")
  (org-babel-do-in-edit-buffer
   (call-interactively
    (key-binding (or key (read-key-sequence nil))))))

(defvar org-bracket-link-regexp)
;;;###autoload
(defun org-babel-open-src-block-result (&optional re-run)
  "If `point' is on a src block then open the results of the
source code block, otherwise return nil.  With optional prefix
argument RE-RUN the source-code block is evaluated even if
results already exist."
  (interactive "P")
  (when (org-babel-get-src-block-info)
    (save-excursion
      ;; go to the results, if there aren't any then run the block
      (goto-char (or (and (not re-run) (org-babel-where-is-src-block-result))
                     (progn (org-babel-execute-src-block)
                            (org-babel-where-is-src-block-result))))
      (end-of-line 1)
      (while (looking-at "[\n\r\t\f ]") (forward-char 1))
      ;; open the results
      (if (looking-at org-bracket-link-regexp)
          ;; file results
          (org-open-at-point)
        (let ((results (org-babel-read-result)))
          (flet ((echo-res (result)
                           (if (stringp result) result (format "%S" result))))
            (pop-to-buffer (get-buffer-create "org-babel-results"))
            (delete-region (point-min) (point-max))
            (if (listp results)
                ;; table result
                (insert (orgtbl-to-generic results '(:sep "\t" :fmt echo-res)))
              ;; scalar result
              (insert (echo-res results))))))
      t)))

;;;###autoload
(defmacro org-babel-map-src-blocks (file &rest body)
  "Evaluate BODY forms on each source-block in FILE.
If FILE is nil evaluate BODY forms on source blocks in current
buffer.  During evaluation of BODY the following local variables
are set relative to the currently matched code block.

full-block ------- string holding the entirety of the code block
beg-block -------- point at the beginning of the code block
end-block -------- point at the end of the matched code block
lang ------------- string holding the language of the code block
beg-lang --------- point at the beginning of the lang
end-lang --------- point at the end of the lang
switches --------- string holding the switches
beg-switches ----- point at the beginning of the switches
end-switches ----- point at the end of the switches
header-args ------ string holding the header-args
beg-header-args -- point at the beginning of the header-args
end-header-args -- point at the end of the header-args
body ------------- string holding the body of the code block
beg-body --------- point at the beginning of the body
end-body --------- point at the end of the body"
  (declare (indent 1))
  (let ((tempvar (make-symbol "file")))
    `(let* ((,tempvar ,file)
	    (visited-p (or (null ,tempvar)
			   (get-file-buffer (expand-file-name ,tempvar))))
	    (point (point)) to-be-removed)
       (save-window-excursion
	 (when ,tempvar (find-file ,tempvar))
	 (setq to-be-removed (current-buffer))
	 (goto-char (point-min))
	 (while (re-search-forward org-babel-src-block-regexp nil t)
	   (goto-char (match-beginning 0))
	   (let ((full-block (match-string 0))
		 (beg-block (match-beginning 0))
		 (end-block (match-end 0))
		 (lang (match-string 2))
		 (beg-lang (match-beginning 2))
		 (end-lang (match-end 2))
		 (switches (match-string 3))
		 (beg-switches (match-beginning 3))
		 (end-switches (match-end 3))
		 (header-args (match-string 4))
		 (beg-header-args (match-beginning 4))
		 (end-header-args (match-end 4))
		 (body (match-string 5))
		 (beg-body (match-beginning 5))
		 (end-body (match-end 5)))
	     ,@body
	     (goto-char end-block))))
       (unless visited-p (kill-buffer to-be-removed))
       (goto-char point))))

;;;###autoload
(defun org-babel-execute-buffer (&optional arg)
  "Execute source code blocks in a buffer.
Call `org-babel-execute-src-block' on every source block in
the current buffer."
  (interactive "P")
  (org-save-outline-visibility t
    (org-babel-map-src-blocks nil
      (org-babel-execute-src-block arg))))

;;;###autoload
(defun org-babel-execute-subtree (&optional arg)
  "Execute source code blocks in a subtree.
Call `org-babel-execute-src-block' on every source block in
the current subtree."
  (interactive "P")
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (org-babel-execute-buffer arg)
      (widen))))

;;;###autoload
(defun org-babel-sha1-hash (&optional info)
  "Generate an sha1 hash based on the value of info."
  (interactive)
  (let ((print-level nil)
	(info (or info (org-babel-get-src-block-info))))
    (setf (nth 2 info)
	  (sort (copy-sequence (nth 2 info))
		(lambda (a b) (string< (car a) (car b)))))
    (let ((hash (sha1
		 (format "%s-%s"
			 (mapconcat
			  #'identity
			  (delq nil
				(mapcar
				 (lambda (arg)
				   (let ((v (cdr arg)))
				     (when (and v (not (and (sequencep v)
							    (not (consp v))
							    (= (length v) 0))))
				       (format "%S" v))))
				 (nth 2 info))) ":")
			 (nth 1 info)))))
      (when (interactive-p) (message hash))
      hash)))

(defun org-babel-result-hash (&optional info)
  "Return the in-buffer hash associated with INFO."
  (org-babel-where-is-src-block-result nil info)
  (org-babel-clean-text-properties (match-string 3)))

(defun org-babel-hide-hash ()
  "Hide the hash in the current results line.
Only the initial `org-babel-hash-show' characters of the hash
will remain visible."
  (add-to-invisibility-spec '(org-babel-hide-hash . t))
  (save-excursion
    (when (and (re-search-forward org-babel-result-regexp nil t)
               (match-string 3))
      (let* ((start (match-beginning 3))
             (hide-start (+ org-babel-hash-show start))
             (end (match-end 3))
             (hash (match-string 3))
             ov1 ov2)
        (setq ov1 (make-overlay start hide-start))
        (setq ov2 (make-overlay hide-start end))
        (overlay-put ov2 'invisible 'org-babel-hide-hash)
        (overlay-put ov1 'babel-hash hash)))))

(defun org-babel-hide-all-hashes ()
  "Hide the hash in the current buffer.
Only the initial `org-babel-hash-show' characters of each hash
will remain visible.  This function should be called as part of
the `org-mode-hook'."
  (save-excursion
    (while (re-search-forward org-babel-result-regexp nil t)
      (goto-char (match-beginning 0))
      (org-babel-hide-hash)
      (goto-char (match-end 0)))))
(add-hook 'org-mode-hook 'org-babel-hide-all-hashes)

(defun org-babel-hash-at-point (&optional point)
  "Return the value of the hash at POINT.
The hash is also added as the last element of the kill ring.
This can be called with C-c C-c."
  (interactive)
  (let ((hash (car (delq nil (mapcar
			      (lambda (ol) (overlay-get ol 'babel-hash))
                              (overlays-at (or point (point))))))))
    (when hash (kill-new hash) (message hash))))
(add-hook 'org-ctrl-c-ctrl-c-hook 'org-babel-hash-at-point)

(defun org-babel-result-hide-spec ()
  "Hide portions of results lines.
Add `org-babel-hide-result' as an invisibility spec for hiding
portions of results lines."
  (add-to-invisibility-spec '(org-babel-hide-result . t)))
(add-hook 'org-mode-hook 'org-babel-result-hide-spec)

(defvar org-babel-hide-result-overlays nil
  "Overlays hiding results.")

(defun org-babel-result-hide-all ()
  "Fold all results in the current buffer."
  (interactive)
  (org-babel-show-result-all)
  (save-excursion
    (while (re-search-forward org-babel-result-regexp nil t)
      (save-excursion (goto-char (match-beginning 0))
                      (org-babel-hide-result-toggle-maybe)))))

(defun org-babel-show-result-all ()
  "Unfold all results in the current buffer."
  (mapc 'delete-overlay org-babel-hide-result-overlays)
  (setq org-babel-hide-result-overlays nil))

;;;###autoload
(defun org-babel-hide-result-toggle-maybe ()
  "Toggle visibility of result at point."
  (interactive)
  (let ((case-fold-search t))
    (if (save-excursion
          (beginning-of-line 1)
          (looking-at org-babel-result-regexp))
        (progn (org-babel-hide-result-toggle)
               t) ;; to signal that we took action
      nil))) ;; to signal that we did not

(defun org-babel-hide-result-toggle (&optional force)
  "Toggle the visibility of the current result."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward org-babel-result-regexp nil t)
        (let ((start (progn (beginning-of-line 2) (- (point) 1)))
              (end (progn (goto-char (- (org-babel-result-end) 1)) (point)))
              ov)
          (if (memq t (mapcar (lambda (overlay)
                                (eq (overlay-get overlay 'invisible)
				    'org-babel-hide-result))
                              (overlays-at start)))
              (if (or (not force) (eq force 'off))
                  (mapc (lambda (ov)
                          (when (member ov org-babel-hide-result-overlays)
                            (setq org-babel-hide-result-overlays
                                  (delq ov org-babel-hide-result-overlays)))
                          (when (eq (overlay-get ov 'invisible)
                                    'org-babel-hide-result)
                            (delete-overlay ov)))
                        (overlays-at start)))
            (setq ov (make-overlay start end))
            (overlay-put ov 'invisible 'org-babel-hide-result)
            ;; make the block accessible to isearch
            (overlay-put
             ov 'isearch-open-invisible
             (lambda (ov)
               (when (member ov org-babel-hide-result-overlays)
                 (setq org-babel-hide-result-overlays
                       (delq ov org-babel-hide-result-overlays)))
               (when (eq (overlay-get ov 'invisible)
                         'org-babel-hide-result)
                 (delete-overlay ov))))
            (push ov org-babel-hide-result-overlays)))
      (error "Not looking at a result line"))))

;; org-tab-after-check-for-cycling-hook
(add-hook 'org-tab-first-hook 'org-babel-hide-result-toggle-maybe)
;; Remove overlays when changing major mode
(add-hook 'org-mode-hook
	  (lambda () (org-add-hook 'change-major-mode-hook
				   'org-babel-show-result-all 'append 'local)))

(defvar org-file-properties)
(defun org-babel-params-from-properties (&optional lang)
  "Retrieve parameters specified as properties.
Return an association list of any source block params which
may be specified in the properties of the current outline entry."
  (save-match-data
    (let (val sym)
      (delq nil
	    (mapcar
	     (lambda (header-arg)
	       (and (setq val
			  (or (condition-case nil
				  (org-entry-get (point) header-arg t)
				(error nil))
			      (cdr (assoc header-arg org-file-properties))))
		    (cons (intern (concat ":" header-arg))
			  (org-babel-read val))))
	     (mapcar
	      'symbol-name
	      (append
	       org-babel-header-arg-names
	       (progn
		 (setq sym (intern (concat "org-babel-header-arg-names:" lang)))
		 (and (boundp sym) (eval sym))))))))))

(defun org-babel-params-from-buffer ()
  "Retrieve per-buffer parameters.
 Return an association list of any source block params which
may be specified at the top of the current buffer."
  (or org-babel-current-buffer-properties
      (setq org-babel-current-buffer-properties
	    (save-match-data
	      (save-excursion
		(save-restriction
		  (widen)
		  (goto-char (point-min))
		  (when (re-search-forward
			 (org-make-options-regexp (list "BABEL")) nil t)
		    (org-babel-parse-header-arguments
		     (org-match-string-no-properties 2)))))))))

(defvar org-src-preserve-indentation)
(defun org-babel-parse-src-block-match ()
  "Parse the results from a match of the `org-babel-src-block-regexp'."
  (let* ((block-indentation (length (match-string 1)))
	 (lang (org-babel-clean-text-properties (match-string 2)))
         (lang-headers (intern (concat "org-babel-default-header-args:" lang)))
	 (switches (match-string 3))
         (body (org-babel-clean-text-properties (match-string 5)))
	 (preserve-indentation (or org-src-preserve-indentation
				   (string-match "-i\\>" switches))))
    (list lang
          ;; get block body less properties, protective commas, and indentation
          (with-temp-buffer
            (save-match-data
              (insert (org-babel-strip-protective-commas body))
	      (unless preserve-indentation (org-do-remove-indentation))
              (buffer-string)))
	  (org-babel-merge-params
	   org-babel-default-header-args
	   (org-babel-params-from-buffer)
           (org-babel-params-from-properties lang)
	   (if (boundp lang-headers) (eval lang-headers) nil)
	   (org-babel-parse-header-arguments
            (org-babel-clean-text-properties (or (match-string 4) ""))))
	  switches
	  block-indentation)))

(defun org-babel-parse-inline-src-block-match ()
  "Parse the results from a match of the `org-babel-inline-src-block-regexp'."
  (let* ((lang (org-babel-clean-text-properties (match-string 2)))
         (lang-headers (intern (concat "org-babel-default-header-args:" lang))))
    (list lang
          (org-babel-strip-protective-commas
           (org-babel-clean-text-properties (match-string 5)))
          (org-babel-merge-params
           org-babel-default-inline-header-args
	   (org-babel-params-from-buffer)
           (org-babel-params-from-properties lang)
           (if (boundp lang-headers) (eval lang-headers) nil)
           (org-babel-parse-header-arguments
            (org-babel-clean-text-properties (or (match-string 4) "")))))))

(defun org-babel-parse-header-arguments (arg-string)
  "Parse a string of header arguments returning an alist."
  (when (> (length arg-string) 0)
    (delq nil
	  (mapcar
	   (lambda (arg)
	     (if (string-match
		  "\\([^ \f\t\n\r\v]+\\)[ \f\t\n\r\v]+\\([^ \f\t\n\r\v]+.*\\)"
		  arg)
		 (cons (intern (match-string 1 arg))
		       (org-babel-read (org-babel-chomp (match-string 2 arg))))
	       (cons (intern (org-babel-chomp arg)) nil)))
	   (let ((balance 0) (partial nil) (lst nil) (last 0))
	     (mapc (lambda (ch)  ; split on [] balanced instances of [ \t]:
		     (setq balance (+ balance
				      (cond ((equal 91 ch) 1)
					    ((equal 93 ch) -1)
					    (t 0))))
		     (setq partial (cons ch partial))
		     (when (and (= ch 58) (= balance 0)
				(or (= last 32) (= last 9)))
		       (setq lst (cons (apply #'string (nreverse (cddr partial)))
				       lst))
		       (setq partial (list ch)))
		     (setq last ch))
		   (string-to-list arg-string))
	     (nreverse (cons (apply #'string (nreverse partial)) lst)))))))

(defun org-babel-process-params (params)
  "Expand variables in PARAMS and add summary parameters."
  (let* ((vars-and-names (org-babel-disassemble-tables
			  (mapcar (lambda (el)
				    (if (consp (cdr el))
					(cdr el) (org-babel-ref-parse (cdr el))))
				  (org-babel-get-header params :var))
			  (cdr (assoc :hlines params))
			  (cdr (assoc :colnames params))
			  (cdr (assoc :rownames params))))
	 (result-params (append
			 (split-string (or (cdr (assoc :results params)) ""))
			 (cdr (assoc :result-params params)))))
    (append
     (mapcar (lambda (var) (cons :var var)) (car vars-and-names))
     (list
      (cons :colname-names (cadr  vars-and-names))
      (cons :rowname-names (caddr vars-and-names))
      (cons :result-params result-params)
      (cons :result-type  (cond ((member "output" result-params) 'output)
				((member "value" result-params) 'value)
				(t 'value))))
     (org-babel-get-header params :var 'other))))

;; row and column names
(defun org-babel-del-hlines (table)
  "Remove all 'hlines from TABLE."
  (remove 'hline table))

(defun org-babel-get-colnames (table)
  "Return the column names of TABLE.
Return a cons cell, the `car' of which contains the TABLE less
colnames, and the `cdr' of which contains a list of the column
names."
  (if (equal 'hline (nth 1 table))
      (cons (cddr table) (car table))
    (cons (cdr table) (car table))))

(defun org-babel-get-rownames (table)
  "Return the row names of TABLE.
Return a cons cell, the `car' of which contains the TABLE less
colnames, and the `cdr' of which contains a list of the column
names.  Note: this function removes any hlines in TABLE."
  (flet ((trans (table) (apply #'mapcar* #'list table)))
    (let* ((width (apply 'max
			 (mapcar (lambda (el) (if (listp el) (length el) 0)) table)))
           (table (trans (mapcar (lambda (row)
                                   (if (not (equal row 'hline))
                                       row
                                     (setq row '())
                                     (dotimes (n width)
				       (setq row (cons 'hline row)))
                                     row))
                                 table))))
      (cons (mapcar (lambda (row) (if (equal (car row) 'hline) 'hline row))
                    (trans (cdr table)))
            (remove 'hline (car table))))))

(defun org-babel-put-colnames (table colnames)
  "Add COLNAMES to TABLE if they exist."
  (if colnames (apply 'list colnames 'hline table) table))

(defun org-babel-put-rownames (table rownames)
  "Add ROWNAMES to TABLE if they exist."
  (if rownames
      (mapcar (lambda (row)
                (if (listp row)
                    (cons (or (pop rownames) "") row)
                  row)) table)
    table))

(defun org-babel-pick-name (names selector)
  "Select one out of an alist of row or column names.
SELECTOR can be either a list of names in which case those names
will be returned directly, or an index into the list NAMES in
which case the indexed names will be return."
  (if (listp selector)
      selector
    (when names
      (if (and selector (symbolp selector) (not (equal t selector)))
	  (cdr (assoc selector names))
	(if (integerp selector)
	    (nth (- selector 1) names)
	  (cdr (car (last names))))))))

(defun org-babel-disassemble-tables (vars hlines colnames rownames)
  "Parse tables for further processing.
Process the variables in VARS according to the HLINES,
ROWNAMES and COLNAMES header arguments.  Return a list consisting
of the vars, cnames and rnames."
  (let (cnames rnames)
    (list
     (mapcar
      (lambda (var)
        (when (listp (cdr var))
          (when (and (not (equal colnames "no"))
                     (or colnames (and (equal (nth 1 (cdr var)) 'hline)
                                       (not (member 'hline (cddr (cdr var)))))))
            (let ((both (org-babel-get-colnames (cdr var))))
              (setq cnames (cons (cons (car var) (cdr both))
                                 cnames))
              (setq var (cons (car var) (car both)))))
          (when (and rownames (not (equal rownames "no")))
            (let ((both (org-babel-get-rownames (cdr var))))
              (setq rnames (cons (cons (car var) (cdr both))
                                 rnames))
              (setq var (cons (car var) (car both)))))
          (when (and hlines (not (equal hlines "yes")))
            (setq var (cons (car var) (org-babel-del-hlines (cdr var))))))
        var)
      vars)
     cnames rnames)))

(defun org-babel-reassemble-table (table colnames rownames)
  "Add column and row names to a table.
Given a TABLE and set of COLNAMES and ROWNAMES add the names
to the table for reinsertion to org-mode."
  (if (listp table)
      ((lambda (table)
         (if (and colnames (listp (car table)) (= (length (car table))
                                                  (length colnames)))
             (org-babel-put-colnames table colnames) table))
       (if (and rownames (= (length table) (length rownames)))
           (org-babel-put-rownames table rownames) table))
    table))

(defun org-babel-where-is-src-block-head ()
  "Find where the current source block begins.
Return the point at the beginning of the current source
block.  Specifically at the beginning of the #+BEGIN_SRC line.
If the point is not on a source block then return nil."
  (let ((initial (point)) top bottom)
    (or
     (save-excursion ;; on a source name line
       (beginning-of-line 1)
       (and (looking-at org-babel-src-name-regexp) (forward-line 1)
            (looking-at org-babel-src-block-regexp)
            (point)))
     (save-excursion ;; on a #+begin_src line
       (beginning-of-line 1)
       (and (looking-at org-babel-src-block-regexp)
            (point)))
     (save-excursion ;; inside a src block
       (and
        (re-search-backward "^[ \t]*#\\+begin_src" nil t) (setq top (point))
        (re-search-forward "^[ \t]*#\\+end_src" nil t) (setq bottom (point))
        (< top initial) (< initial bottom)
        (progn (goto-char top) (beginning-of-line 1)
	       (looking-at org-babel-src-block-regexp))
        (point))))))

;;;###autoload
(defun org-babel-goto-src-block-head ()
  "Go to the beginning of the current code block."
  (interactive)
  ((lambda (head)
     (if head (goto-char head) (error "not currently in a code block")))
   (org-babel-where-is-src-block-head)))

;;;###autoload
(defun org-babel-goto-named-src-block (name)
  "Go to a named source-code block."
  (interactive
   (let ((completion-ignore-case t))
     (list (org-icompleting-read "source-block name: "
				 (org-babel-src-block-names) nil t))))
  (let ((point (org-babel-find-named-block name)))
    (if point
        ;; taken from `org-open-at-point'
        (progn (goto-char point) (org-show-context))
      (message "source-code block '%s' not found in this buffer" name))))

(defun org-babel-find-named-block (name)
  "Find a named source-code block.
Return the location of the source block identified by source
NAME, or nil if no such block exists. Set match data according to
org-babel-named-src-block-regexp."
  (save-excursion
    (let ((case-fold-search t)
	  (regexp (org-babel-named-src-block-regexp-for-name name)) msg)
      (goto-char (point-min))
      (when (or (re-search-forward regexp nil t)
                (re-search-backward regexp nil t))
        (match-beginning 0)))))

(defun org-babel-src-block-names (&optional file)
  "Returns the names of source blocks in FILE or the current buffer."
  (save-excursion
    (when file (find-file file)) (goto-char (point-min))
    (let (names)
      (while (re-search-forward org-babel-src-name-w-name-regexp nil t)
	(setq names (cons (org-babel-clean-text-properties (match-string 3))
			  names)))
      names)))

;;;###autoload
(defun org-babel-goto-named-result (name)
  "Go to a named result."
  (interactive
   (let ((completion-ignore-case t))
     (list (org-icompleting-read "source-block name: "
				 (org-babel-result-names) nil t))))
  (let ((point (org-babel-find-named-result name)))
    (if point
        ;; taken from `org-open-at-point'
        (progn (goto-char point) (org-show-context))
      (message "result '%s' not found in this buffer" name))))

(defun org-babel-find-named-result (name)
  "Find a named result.
Return the location of the result named NAME in the current
buffer or nil if no such result exists."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (concat org-babel-result-regexp
                   "[ \t]" (regexp-quote name) "[ \t\n\f\v\r]") nil t)
      (beginning-of-line 0) (point))))

(defun org-babel-result-names (&optional file)
  "Returns the names of results in FILE or the current buffer."
  (save-excursion
    (when file (find-file file)) (goto-char (point-min))
    (let (names)
      (while (re-search-forward org-babel-result-w-name-regexp nil t)
	(setq names (cons (org-babel-clean-text-properties (match-string 4))
			  names)))
      names)))

;;;###autoload
(defun org-babel-next-src-block (&optional arg)
  "Jump to the next source block.
With optional prefix argument ARG, jump forward ARG many source blocks."
  (interactive "P")
  (when (looking-at org-babel-src-block-regexp) (forward-char 1))
  (condition-case nil
      (re-search-forward org-babel-src-block-regexp nil nil (or arg 1))
    (error (error "No further code blocks")))
  (goto-char (match-beginning 0)) (org-show-context))

;;;###autoload
(defun org-babel-previous-src-block (&optional arg)
  "Jump to the previous source block.
With optional prefix argument ARG, jump backward ARG many source blocks."
  (interactive "P")
  (condition-case nil
      (re-search-backward org-babel-src-block-regexp nil nil (or arg 1))
    (error (error "No previous code blocks")))
  (goto-char (match-beginning 0)) (org-show-context))

(defvar org-babel-load-languages)

;;;###autoload
(defun org-babel-mark-block ()
  "Mark current src block"
  (interactive)
  ((lambda (head)
     (when head
       (save-excursion
	 (goto-char head)
	 (looking-at org-babel-src-block-regexp))
       (push-mark (match-end 5) nil t)
       (goto-char (match-beginning 5))))
   (org-babel-where-is-src-block-head)))

(defun org-babel-demarcate-block (&optional arg)
  "Wrap or split the code in the region or on the point.
When called from inside of a code block the current block is
split.  When called from outside of a code block a new code block
is created.  In both cases if the region is demarcated and if the
region is not active then the point is demarcated."
  (interactive "P")
  (let ((info (org-babel-get-src-block-info 'light))
	(stars (concat (make-string (or (org-current-level) 1) ?*) " ")))
    (if info
        (mapc
         (lambda (place)
           (save-excursion
             (goto-char place)
             (let ((lang (nth 0 info))
                   (indent (make-string (nth 5 info) ? )))
	       (when (string-match "^[[:space:]]*$"
				   (buffer-substring (point-at-bol)
						     (point-at-eol)))
		 (delete-region (point-at-bol) (point-at-eol)))
               (insert (concat (if (looking-at "^") "" "\n")
                               indent "#+end_src\n"
                               (if arg stars indent) "\n"
                               indent "#+begin_src " lang
                               (if (looking-at "[\n\r]") "" "\n")))))
	   (move-end-of-line 2))
         (sort (if (region-active-p) (list (mark) (point)) (list (point))) #'>))
      (let ((start (point))
	    (lang (org-icompleting-read "Lang: "
					(mapcar (lambda (el) (symbol-name (car el)))
						org-babel-load-languages)))
	    (body (delete-and-extract-region
		   (if (region-active-p) (mark) (point)) (point))))
	(insert (concat (if (looking-at "^") "" "\n")
			(if arg (concat stars "\n") "")
			"#+begin_src " lang "\n"
			body
			(if (or (= (length body) 0)
				(string-match "[\r\n]$" body)) "" "\n")
			"#+end_src\n"))
	(goto-char start) (move-end-of-line 1)))))

(defvar org-babel-lob-one-liner-regexp)
(defun org-babel-where-is-src-block-result (&optional insert info hash indent)
  "Find where the current source block results begin.
Return the point at the beginning of the result of the current
source block.  Specifically at the beginning of the results line.
If no result exists for this block then create a results line
following the source block."
  (save-excursion
    (let* ((on-lob-line (progn (beginning-of-line 1)
			       (looking-at org-babel-lob-one-liner-regexp)))
	   (name (if on-lob-line
		     (nth 0 (org-babel-lob-get-info))
		   (nth 4 (or info (org-babel-get-src-block-info)))))
	   (head (unless on-lob-line (org-babel-where-is-src-block-head)))
	   found beg end)
      (when head (goto-char head))
      (setq
       found ;; was there a result (before we potentially insert one)
       (or
	(and
	 ;; named results:
	 ;; - return t if it is found, else return nil
	 ;; - if it does not need to be rebuilt, then don't set end
	 ;; - if it does need to be rebuilt then do set end
	 name (setq beg (org-babel-find-named-result name))
	 (prog1 beg
	   (when (and hash (not (string= hash (match-string 3))))
	     (goto-char beg) (setq end beg) ;; beginning of result
	     (forward-line 1)
	     (delete-region end (org-babel-result-end)) nil)))
	(and
	 ;; unnamed results:
	 ;; - return t if it is found, else return nil
	 ;; - if it is found, and the hash doesn't match, delete and set end
	 (or on-lob-line (re-search-forward "^[ \t]*#\\+end_src" nil t))
	 (progn (end-of-line 1)
		(if (eobp) (insert "\n") (forward-char 1))
		(setq end (point))
		(or (and (not name)
			 (progn ;; unnamed results line already exists
			   (re-search-forward "[^ \f\t\n\r\v]" nil t)
			   (beginning-of-line 1)
			   (looking-at
			    (concat org-babel-result-regexp "\n")))
			 (prog1 (point)
			   ;; must remove and rebuild if hash!=old-hash
			   (if (and hash (not (string= hash (match-string 3))))
			       (prog1 nil
				 (forward-line 1)
				 (delete-region
				  end (org-babel-result-end)))
			     (setq end nil)))))))))
      (if (and insert end)
	  (progn
	    (goto-char end)
	    (unless beg
	      (if (looking-at "[\n\r]") (forward-char 1) (insert "\n")))
	    (insert (concat
		     (if indent
			 (mapconcat
			  (lambda (el) " ")
			  (org-number-sequence 1 indent) "")
		       "")
		     "#+results"
		     (when hash (concat "["hash"]"))
		     ":"
		     (when name (concat " " name)) "\n"))
	    (unless beg (insert "\n") (backward-char))
	    (beginning-of-line 0)
	    (if hash (org-babel-hide-hash))
	    (point))
	found))))

(defvar org-block-regexp)
(defun org-babel-read-result ()
  "Read the result at `point' into emacs-lisp."
  (let ((case-fold-search t) result-string)
    (cond
     ((org-at-table-p) (org-babel-read-table))
     ((org-in-item-p) (org-babel-read-list))
     ((looking-at org-bracket-link-regexp) (org-babel-read-link))
     ((looking-at org-block-regexp) (org-babel-trim (match-string 4)))
     ((looking-at "^[ \t]*: ")
      (setq result-string
	    (org-babel-trim
	     (mapconcat (lambda (line)
                          (if (and (> (length line) 1)
                                   (string-match "^[ \t]*: \\(.+\\)" line))
                              (match-string 1 line)
                            line))
			(split-string
			 (buffer-substring
                          (point) (org-babel-result-end)) "[\r\n]+")
			"\n")))
      (or (org-babel-number-p result-string) result-string))
     ((looking-at org-babel-result-regexp)
      (save-excursion (forward-line 1) (org-babel-read-result))))))

(defun org-babel-read-table ()
  "Read the table at `point' into emacs-lisp."
  (mapcar (lambda (row)
            (if (and (symbolp row) (equal row 'hline)) row
              (mapcar #'org-babel-read row)))
          (org-table-to-lisp)))

(defun org-babel-read-list ()
  "Read the list at `point' into emacs-lisp."
  (mapcar #'org-babel-read (cdr (org-list-parse-list))))

(defvar org-link-types-re)
(defun org-babel-read-link ()
  "Read the link at `point' into emacs-lisp.
If the path of the link is a file path it is expanded using
`expand-file-name'."
  (let* ((case-fold-search t)
         (raw (and (looking-at org-bracket-link-regexp)
                   (org-babel-clean-text-properties (match-string 1))))
         (type (and (string-match org-link-types-re raw)
                    (match-string 1 raw))))
    (cond
     ((not type) (expand-file-name raw))
     ((string= type "file")
      (and (string-match "file\\(.*\\):\\(.+\\)" raw)
           (expand-file-name (match-string 2 raw))))
     (t raw))))

(defun org-babel-insert-result
  (result &optional result-params info hash indent lang)
  "Insert RESULT into the current buffer.
By default RESULT is inserted after the end of the
current source block.  With optional argument RESULT-PARAMS
controls insertion of results in the org-mode file.
RESULT-PARAMS can take the following values...

replace - (default option) insert results after the source block
          replacing any previously inserted results

silent -- no results are inserted

file ---- the results are interpreted as a file path, and are
          inserted into the buffer using the Org-mode file syntax

list ---- the results are interpreted as an Org-mode list.

raw ----- results are added directly to the Org-mode file.  This
          is a good option if you code block will output org-mode
          formatted text.

org ----- similar in effect to raw, only the results are wrapped
          in an org code block.  Similar to the raw option, on
          export the results will be interpreted as org-formatted
          text, however by wrapping the results in an org code
          block they can be replaced upon re-execution of the
          code block.

html ---- results are added inside of a #+BEGIN_HTML block.  This
          is a good option if you code block will output html
          formatted text.

latex --- results are added inside of a #+BEGIN_LATEX block.
          This is a good option if you code block will output
          latex formatted text.

code ---- the results are extracted in the syntax of the source
          code of the language being evaluated and are added
          inside of a #+BEGIN_SRC block with the source-code
          language set appropriately.  Note this relies on the
          optional LANG argument."
  (if (stringp result)
      (progn
        (setq result (org-babel-clean-text-properties result))
        (when (member "file" result-params)
          (setq result (org-babel-result-to-file result))))
    (unless (listp result) (setq result (format "%S" result))))
  (if (and result-params (member "silent" result-params))
      (progn
	(message (replace-regexp-in-string "%" "%%" (format "%S" result)))
	result)
    (when (and (stringp result) ;; ensure results end in a newline
	       (> (length result) 0)
	       (not (or (string-equal (substring result -1) "\n")
			(string-equal (substring result -1) "\r"))))
      (setq result (concat result "\n")))
    (save-excursion
      (let ((existing-result (org-babel-where-is-src-block-result
			      t info hash indent))
	    (results-switches
	     (cdr (assoc :results_switches (nth 2 info))))
	    beg end)
	(if (not existing-result)
	    (setq beg (point))
	  (goto-char existing-result)
	  (save-excursion
	    (re-search-forward "#" nil t)
	    (setq indent (- (current-column) 1)))
	  (forward-line 1)
	  (setq beg (point))
	  (cond
	   ((member "replace" result-params)
	    (delete-region (point) (org-babel-result-end)))
	   ((member "append" result-params)
	    (goto-char (org-babel-result-end)) (setq beg (point-marker)))
	   ((member "prepend" result-params)))) ; already there
	(setq results-switches
	      (if results-switches (concat " " results-switches) ""))
	;; insert results based on type
	(cond
	 ;; do nothing for an empty result
	 ((= (length result) 0))
	 ;; insert a list if preferred
	 ((member "list" result-params)
	  (insert
	   (org-babel-trim
	    (org-list-to-generic (cons 'unordered
				       (if (listp result) result (list result)))
				 '(:splicep nil :istart "- " :iend "\n")))))
	 ;; assume the result is a table if it's not a string
	 ((not (stringp result))
	  (goto-char beg)
	  (insert (concat (orgtbl-to-orgtbl
			   (if (or (eq 'hline (car result))
				   (and (listp (car result))
					(listp (cdr (car result)))))
			       result (list result))
			   '(:fmt (lambda (cell) (format "%s" cell)))) "\n"))
	  (goto-char beg) (when (org-at-table-p) (org-table-align)))
	 ((member "file" result-params)
	  (insert result))
	 (t (goto-char beg) (insert result)))
	(when (listp result) (goto-char (org-table-end)))
	(setq end (point-marker))
	;; possibly wrap result
	(flet ((wrap (start finish)
		     (goto-char beg) (insert start)
		     (goto-char end) (insert finish)
		     (setq end (point-marker))))
	  (cond
	   ((member "html" result-params)
	    (wrap "#+BEGIN_HTML\n" "#+END_HTML"))
	   ((member "latex" result-params)
	    (wrap "#+BEGIN_LaTeX\n" "#+END_LaTeX"))
	   ((member "code" result-params)
	    (wrap (format "#+BEGIN_SRC %s%s\n" (or lang "none") results-switches)
		  "#+END_SRC"))
	   ((member "org" result-params)
	    (wrap "#+BEGIN_ORG\n" "#+END_ORG"))
	   ((member "raw" result-params)
	    (goto-char beg) (if (org-at-table-p) (org-cycle)))
	   ((member "wrap" result-params)
	    (when (and (stringp result) (not (member "file" result-params)))
	      (org-babel-examplize-region beg end results-switches))
	    (wrap "#+BEGIN_RESULT\n" "#+END_RESULT"))
	   ((and (stringp result) (not (member "file" result-params)))
	    (org-babel-examplize-region beg end results-switches)
	    (setq end (point)))))
	;; possibly indent the results to match the #+results line
	(when (and indent (> indent 0)
		   ;; in this case `table-align' does the work for us
		   (not (and (listp result)
			     (member "append" result-params))))
	  (indent-rigidly beg end indent))))
    (if (= (length result) 0)
	(if (member "value" result-params)
	    (message "Code block returned no value.")
	  (message "Code block produced no output."))
      (message "Code block evaluation complete."))))

(defun org-babel-remove-result (&optional info)
  "Remove the result of the current source block."
  (interactive)
  (let ((location (org-babel-where-is-src-block-result nil info)) start)
    (when location
      (save-excursion
        (goto-char location) (setq start (point)) (forward-line 1)
        (delete-region start (org-babel-result-end))))))

(defun org-babel-result-end ()
  "Return the point at the end of the current set of results"
  (save-excursion
    (cond
     ((org-at-table-p) (progn (goto-char (org-table-end)) (point)))
     ((org-in-item-p) (- (org-list-bottom-point) 1))
     (t
      (let ((case-fold-search t)
	    (blocks-re (regexp-opt
			(list "latex" "html" "example" "src" "result"))))
	(if (looking-at (concat "[ \t]*#\\+begin_" blocks-re))
	    (re-search-forward (concat "[ \t]*#\\+end_" blocks-re) nil t)
	  (while (looking-at "[ \t]*\\(: \\|\\[\\[\\)")
	    (forward-line 1))))
      (point)))))

(defun org-babel-result-to-file (result)
  "Convert RESULT into an `org-mode' link.
If the `default-directory' is different from the containing
file's directory then expand relative links."
  (format
   "[[file:%s]]"
   (if (and default-directory
            buffer-file-name
            (not (string= (expand-file-name default-directory)
                          (expand-file-name
                           (file-name-directory buffer-file-name)))))
       (expand-file-name result default-directory)
     result)))

(defun org-babel-examplize-region (beg end &optional results-switches)
  "Comment out region using the ': ' org example quote."
  (interactive "*r")
  (let ((size (count-lines beg end)))
    (save-excursion
      (cond ((= size 0))	      ; do nothing for an empty result
	    ((< size org-babel-min-lines-for-block-output)
	     (goto-char beg)
	     (dotimes (n size)
	       (beginning-of-line 1) (insert ": ") (forward-line 1)))
	    (t
	     (goto-char beg)
	     (insert (if results-switches
                         (format "#+begin_example%s\n" results-switches)
                       "#+begin_example\n"))
	     (if (markerp end) (goto-char end) (forward-char (- end beg)))
	     (insert "#+end_example\n"))))))

(defun org-babel-update-block-body (new-body)
  "Update the body of the current code block to NEW-BODY."
  (if (not (org-babel-where-is-src-block-head))
      (error "not in source block")
    (save-match-data
      (replace-match (concat (org-babel-trim new-body) "\n") nil nil nil 5))
    (indent-rigidly (match-beginning 5) (match-end 5) 2)))

(defun org-babel-merge-params (&rest plists)
  "Combine all parameter association lists in PLISTS.
Later elements of PLISTS override the values of previous element.
This takes into account some special considerations for certain
parameters when merging lists."
  (let ((results-exclusive-groups
	 '(("file" "list" "vector" "table" "scalar" "raw" "org"
            "html" "latex" "code" "pp" "wrap")
	   ("replace" "silent" "append" "prepend")
	   ("output" "value")))
	(exports-exclusive-groups
	 '(("code" "results" "both" "none")))
	params results exports tangle noweb cache vars shebang comments)
    (flet ((e-merge (exclusive-groups &rest result-params)
             ;; maintain exclusivity of mutually exclusive parameters
             (let (output)
               (mapc (lambda (new-params)
                       (mapc (lambda (new-param)
                               (mapc (lambda (exclusive-group)
                                       (when (member new-param exclusive-group)
                                         (mapcar (lambda (excluded-param)
                                                   (setq output
                                                         (delete
                                                          excluded-param
                                                          output)))
                                                 exclusive-group)))
                                     exclusive-groups)
                               (setq output (org-uniquify
                                             (cons new-param output))))
                             new-params))
                     result-params)
               output)))
      (mapc
       (lambda (plist)
	 (mapc
	  (lambda (pair)
	    (case (car pair)
	      (:var
	       (let ((name (if (listp (cdr pair))
			       (cadr pair)
			     (and (string-match "^\\([^= \f\t\n\r\v]+\\)[ \t]*="
						(cdr pair))
				  (intern (match-string 1 (cdr pair)))))))
		 (when name
		   (setq vars
			 (cons (cons name pair)
			       (if (member name (mapcar #'car vars))
				   (delq nil
					 (mapcar
					  (lambda (p) (unless (equal (car p) name) p))
					  vars))
				 vars))))))
	      (:results
	       (setq results (e-merge results-exclusive-groups
				      results (split-string (cdr pair)))))
	      (:file
	       (when (cdr pair)
		 (setq results (e-merge results-exclusive-groups
					results '("file")))
		 (unless (or (member "both" exports)
			     (member "none" exports)
			     (member "code" exports))
		   (setq exports (e-merge exports-exclusive-groups
					  exports '("results"))))
		 (setq params (cons pair (assq-delete-all (car pair) params)))))
	      (:exports
	       (setq exports (e-merge exports-exclusive-groups
				      exports (split-string (cdr pair)))))
	      (:tangle ;; take the latest -- always overwrite
	       (setq tangle (or (list (cdr pair)) tangle)))
	      (:noweb
	       (setq noweb (e-merge '(("yes" "no" "tangle")) noweb
				    (split-string (or (cdr pair) "")))))
	      (:cache
	       (setq cache (e-merge '(("yes" "no")) cache
				    (split-string (or (cdr pair) "")))))
	      (:shebang ;; take the latest -- always overwrite
	       (setq shebang (or (list (cdr pair)) shebang)))
	      (:comments
	       (setq comments (e-merge '(("yes" "no")) comments
				       (split-string (or (cdr pair) "")))))
	      (t ;; replace: this covers e.g. :session
	       (setq params (cons pair (assq-delete-all (car pair) params))))))
	  plist))
       plists))
    (while vars (setq params (cons (cons :var (cddr (pop vars))) params)))
    (cons (cons :comments (mapconcat 'identity comments " "))
          (cons (cons :shebang (mapconcat 'identity shebang " "))
                (cons (cons :cache (mapconcat 'identity cache " "))
                      (cons (cons :noweb (mapconcat 'identity noweb " "))
                            (cons (cons :tangle (mapconcat 'identity tangle " "))
                                  (cons (cons :exports
                                              (mapconcat 'identity exports " "))
                                        (cons
                                         (cons :results
                                               (mapconcat 'identity results " "))
                                         params)))))))))

(defun org-babel-expand-noweb-references (&optional info parent-buffer)
  "Expand Noweb references in the body of the current source code block.

For example the following reference would be replaced with the
body of the source-code block named 'example-block'.

<<example-block>>

Note that any text preceding the <<foo>> construct on a line will
be interposed between the lines of the replacement text.  So for
example if <<foo>> is placed behind a comment, then the entire
replacement text will also be commented.

This function must be called from inside of the buffer containing
the source-code block which holds BODY.

In addition the following syntax can be used to insert the
results of evaluating the source-code block named 'example-block'.

<<example-block()>>

Any optional arguments can be passed to example-block by placing
the arguments inside the parenthesis following the convention
defined by `org-babel-lob'.  For example

<<example-block(a=9)>>

would set the value of argument \"a\" equal to \"9\".  Note that
these arguments are not evaluated in the current source-code
block but are passed literally to the \"example-block\"."
  (let* ((parent-buffer (or parent-buffer (current-buffer)))
         (info (or info (org-babel-get-src-block-info)))
         (lang (nth 0 info))
         (body (nth 1 info))
         (new-body "") index source-name evaluate prefix)
    (flet ((nb-add (text)
                   (setq new-body (concat new-body text))))
      (with-temp-buffer
        (insert body) (goto-char (point-min))
        (setq index (point))
        (while (and (re-search-forward "<<\\(.+?\\)>>" nil t))
          (save-match-data (setf source-name (match-string 1)))
          (save-match-data (setq evaluate (string-match "\(.*\)" source-name)))
          (save-match-data
            (setq prefix
                  (buffer-substring (match-beginning 0)
                                    (save-excursion
                                      (beginning-of-line 1) (point)))))
          ;; add interval to new-body (removing noweb reference)
          (goto-char (match-beginning 0))
          (nb-add (buffer-substring index (point)))
          (goto-char (match-end 0))
          (setq index (point))
          (nb-add (with-current-buffer parent-buffer
		    (mapconcat ;; interpose PREFIX between every line
                     #'identity
                     (split-string
                      (if evaluate
                          (let ((raw (org-babel-ref-resolve source-name)))
                            (if (stringp raw) raw (format "%S" raw)))
			(save-restriction
			  (widen)
			  (let ((point (org-babel-find-named-block
					source-name)))
			    (if point
				(save-excursion
				  (goto-char point)
				  (org-babel-trim
				   (org-babel-expand-noweb-references
				    (org-babel-get-src-block-info))))
			      ;; optionally raise an error if named
			      ;; source-block doesn't exist
			      (if (member lang org-babel-noweb-error-langs)
				  (error "%s"
					 (concat
					  "<<" source-name ">> "
					  "could not be resolved (see "
					  "`org-babel-noweb-error-langs')"))
				"")))))
		      "[\n\r]") (concat "\n" prefix)))))
        (nb-add (buffer-substring index (point-max)))))
    new-body))

(defun org-babel-clean-text-properties (text)
  "Strip all properties from text return."
  (when text
    (set-text-properties 0 (length text) nil text) text))

(defun org-babel-strip-protective-commas (body)
  "Strip protective commas from bodies of source blocks."
  (replace-regexp-in-string "^,#" "#" body))

(defun org-babel-script-escape (str)
  "Safely convert tables into elisp lists."
  (let (in-single in-double out)
    (org-babel-read
     (if (and (stringp str) (string-match "^\\[.+\\]$" str))
	 (org-babel-read
	  (concat
	   "'"
	   (progn
	     (mapc
	      (lambda (ch)
		(setq
		 out
		 (case ch
		   (91 (if (or in-double in-single) ; [
			   (cons 91 out)
			 (cons 40 out)))
		   (93 (if (or in-double in-single) ; ]
			   (cons 93 out)
			 (cons 41 out)))
		   (44 (if (or in-double in-single) (cons 44 out) out)) ; ,
		   (39 (if in-double	; '
			   (cons 39 out)
			 (setq in-single (not in-single)) (cons 34 out)))
		   (34 (if in-single	; "
			   (append (list 34 32) out)
			 (setq in-double (not in-double)) (cons 34 out)))
		   (t  (cons ch out)))))
	      (string-to-list str))
	     (apply #'string (reverse out)))))
       str))))

(defun org-babel-read (cell)
  "Convert the string value of CELL to a number if appropriate.
Otherwise if cell looks like lisp (meaning it starts with a
\"(\" or a \"'\") then read it as lisp, otherwise return it
unmodified as a string.

This is taken almost directly from `org-read-prop'."
  (if (and (stringp cell) (not (equal cell "")))
      (or (org-babel-number-p cell)
          (if (or (equal "(" (substring cell 0 1))
                  (equal "'" (substring cell 0 1))
                  (equal "`" (substring cell 0 1)))
              (eval (read cell))
            (progn (set-text-properties 0 (length cell) nil cell) cell)))
    cell))

(defun org-babel-number-p (string)
  "If STRING represents a number return it's value."
  (if (and (string-match "^-?[0-9]*\\.?[0-9]*$" string)
           (= (length (substring string (match-beginning 0)
				 (match-end 0)))
	      (length string)))
      (string-to-number string)))

(defun org-babel-import-elisp-from-file (file-name &optional separator)
  "Read the results located at FILE-NAME into an elisp table.
If the table is trivial, then return it as a scalar."
  (let (result)
    (save-window-excursion
      (with-temp-buffer
	(condition-case nil
	    (progn
	      (org-table-import file-name separator)
	      (delete-file file-name)
	      (setq result (mapcar (lambda (row)
				     (mapcar #'org-babel-string-read row))
				   (org-table-to-lisp))))
	  (error nil)))
      (if (null (cdr result)) ;; if result is trivial vector, then scalarize it
	  (if (consp (car result))
	      (if (null (cdr (car result)))
		  (caar result)
		result)
	    (car result))
	result))))

(defun org-babel-string-read (cell)
  "Strip nested \"s from around strings."
  (org-babel-read (or (and (stringp cell)
                           (string-match "\\\"\\(.+\\)\\\"" cell)
                           (match-string 1 cell))
                      cell)))

(defun org-babel-reverse-string (string)
  "Return the reverse of STRING."
  (apply 'string (reverse (string-to-list string))))

(defun org-babel-chomp (string &optional regexp)
  "Strip trailing spaces and carriage returns from STRING.
Default regexp used is \"[ \f\t\n\r\v]\" but can be
overwritten by specifying a regexp as a second argument."
  (let ((regexp (or regexp "[ \f\t\n\r\v]")))
    (while (and (> (length string) 0)
                (string-match regexp (substring string -1)))
      (setq string (substring string 0 -1)))
    string))

(defun org-babel-trim (string &optional regexp)
  "Strip leading and trailing spaces and carriage returns from STRING.
Like `org-babel-chomp' only it runs on both the front and back
of the string."
  (org-babel-chomp (org-babel-reverse-string
                    (org-babel-chomp (org-babel-reverse-string string) regexp))
                   regexp))

(defvar org-babel-org-babel-call-process-region-original nil)
(defun org-babel-tramp-handle-call-process-region
  (start end program &optional delete buffer display &rest args)
  "Use tramp to handle call-process-region.
Fixes a bug in `tramp-handle-call-process-region'."
  (if (and (featurep 'tramp) (file-remote-p default-directory))
      (let ((tmpfile (tramp-compat-make-temp-file "")))
	(write-region start end tmpfile)
	(when delete (delete-region start end))
	(unwind-protect
	    ;;	(apply 'call-process program tmpfile buffer display args)
            ;; bug in tramp
	    (apply 'process-file program tmpfile buffer display args)
	  (delete-file tmpfile)))
    ;; org-babel-call-process-region-original is the original emacs
    ;; definition. It is in scope from the let binding in
    ;; org-babel-execute-src-block
    (apply org-babel-call-process-region-original
           start end program delete buffer display args)))

(defun org-babel-local-file-name (file)
  "Return the local name component of FILE."
  (if (file-remote-p file)
      (let (localname)
	(with-parsed-tramp-file-name file nil
	  localname))
    file))

(defun org-babel-process-file-name (name &optional no-quote-p)
  "Prepare NAME to be used in an external process.
If NAME specifies a remote location, the remote portion of the
name is removed, since in that case the process will be executing
remotely. The file name is then processed by
`expand-file-name'. Unless second argument NO-QUOTE-P is non-nil,
the file name is additionally processed by
`shell-quote-argument'"
  ((lambda (f) (if no-quote-p f (shell-quote-argument f)))
   (expand-file-name (org-babel-local-file-name name))))

(defvar org-babel-temporary-directory)
(unless (or noninteractive (boundp 'org-babel-temporary-directory))
  (defvar org-babel-temporary-directory
    (or (and (boundp 'org-babel-temporary-directory)
	     (file-exists-p org-babel-temporary-directory)
	     org-babel-temporary-directory)
	(make-temp-file "babel-" t))
    "Directory to hold temporary files created to execute code blocks.
Used by `org-babel-temp-file'.  This directory will be removed on
Emacs shutdown."))

(defun org-babel-temp-file (prefix &optional suffix)
  "Create a temporary file in the `org-babel-temporary-directory'.
Passes PREFIX and SUFFIX directly to `make-temp-file' with the
value of `temporary-file-directory' temporarily set to the value
of `org-babel-temporary-directory'."
  (if (file-remote-p default-directory)
      (make-temp-file
       (concat (file-remote-p default-directory)
	       (expand-file-name
		prefix temporary-file-directory)
	       nil suffix))
    (let ((temporary-file-directory
	   (or (and (file-exists-p org-babel-temporary-directory)
		    org-babel-temporary-directory)
	       temporary-file-directory)))
      (make-temp-file prefix nil suffix))))

(defun org-babel-remove-temporary-directory ()
  "Remove `org-babel-temporary-directory' on Emacs shutdown."
  (when (and (boundp 'org-babel-temporary-directory)
	     (file-exists-p org-babel-temporary-directory))
    ;; taken from `delete-directory' in files.el
    (condition-case nil
	(progn
	  (mapc (lambda (file)
		  ;; This test is equivalent to
		  ;; (and (file-directory-p fn) (not (file-symlink-p fn)))
		  ;; but more efficient
		  (if (eq t (car (file-attributes file)))
		      (delete-directory file)
		    (delete-file file)))
		;; We do not want to delete "." and "..".
		(directory-files org-babel-temporary-directory 'full
				 "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"))
	  (delete-directory org-babel-temporary-directory))
      (error
       (message "Failed to remove temporary Org-babel directory %s"
		org-babel-temporary-directory)))))

(add-hook 'kill-emacs-hook 'org-babel-remove-temporary-directory)

(provide 'ob)

;; arch-tag: 01a7ebee-06c5-4ee4-a709-e660d28c0af1

;;; ob.el ends here
