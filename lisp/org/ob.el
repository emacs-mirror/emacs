;;; ob.el --- working with code blocks in org-mode

;; Copyright (C) 2009, 2010  Free Software Foundation, Inc.

;; Author: Eric Schulte, Dan Davison
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

;; See the online documentation for more information
;; 
;;   http://orgmode.org/worg/org-contrib/babel/

;;; Code:
(eval-when-compile (require 'cl))
(require 'org-macs)

(defvar org-babel-call-process-region-original)
(declare-function show-all "outline" ())
(declare-function tramp-compat-make-temp-file "tramp" (filename &optional dir-flag))
(declare-function tramp-dissect-file-name "tramp" (name &optional nodefault))
(declare-function tramp-file-name-user "tramp" (vec))
(declare-function tramp-file-name-host "tramp" (vec))
(declare-function org-icompleting-read "org" (&rest args))
(declare-function org-edit-src-code "org" (context code edit-buffer-name))
(declare-function org-open-at-point "org" (&optional in-emacs reference-buffer))
(declare-function org-save-outline-visibility "org" (use-markers &rest body))
(declare-function org-narrow-to-subtree "org" ())
(declare-function org-entry-get "org" (pom property &optional inherit))
(declare-function org-make-options-regexp "org" (kwds &optional extra))
(declare-function org-match-string-no-properties "org" (num &optional string))
(declare-function org-do-remove-indentation "org" (&optional n))
(declare-function org-show-context "org" (&optional key))
(declare-function org-at-table-p "org" (&optional table-type))
(declare-function org-cycle "org" (&optional arg))
(declare-function org-uniquify "org" (list))
(declare-function org-table-import "org" (file arg))
(declare-function org-add-hook "org-compat" (hook function &optional append local))
(declare-function org-table-align "org-table" ())
(declare-function org-table-end "org-table" (&optional table-type))
(declare-function orgtbl-to-generic "org-table" (table params))
(declare-function orgtbl-to-orgtbl "org-table" (table params))
(declare-function org-babel-lob-get-info "ob-lob" nil)
(declare-function org-babel-ref-split-args "ob-ref" (arg-string))
(declare-function org-babel-ref-variables "ob-ref" (params))
(declare-function org-babel-ref-resolve-reference "ob-ref" (ref &optional params))

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

(defvar org-babel-src-name-w-name-regexp
  (concat org-babel-src-name-regexp
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

(defun org-babel-get-src-block-info (&optional header-vars-only)
  "Get information on the current source block.

Returns a list
 (language body header-arguments-alist switches name function-args indent).
Unless HEADER-VARS-ONLY is non-nil, any variable
references provided in 'function call style' (i.e. in a
parenthesised argument list following the src block name) are
added to the header-arguments-alist."
  (let ((case-fold-search t) head info args indent)
    (if (setq head (org-babel-where-is-src-block-head))
        (save-excursion
	  (goto-char head)
	  (setq info (org-babel-parse-src-block-match))
	  (setq indent (car (last info)))
	  (setq info (butlast info))
	  (forward-line -1)
	  (if (and (looking-at org-babel-src-name-w-name-regexp)
		   (match-string 2))
	      (progn
		(setq info (append info (list (org-babel-clean-text-properties
					       (match-string 2)))))
		;; Note that e.g. "name()" and "name( )" result in
		;; ((:var . "")).  We maintain that behaviour, and the
		;; resulting non-nil sixth element is relied upon in
		;; org-babel-exp-code to detect a functional-style
		;; block in those cases. However, "name" without any
		;; parentheses would result in the same thing, so we
		;; explicitly avoid that.
		(if (setq args (match-string 4))
		    (setq info
			  (append info (list
					(mapcar
					 (lambda (ref) (cons :var ref))
					 (org-babel-ref-split-args args))))))
		(unless header-vars-only
		  (setf (nth 2 info)
			(org-babel-merge-params (nth 5 info) (nth 2 info)))))
	    (setq info (append info (list nil nil))))
	  (append info (list indent)))
      (if (save-excursion ;; inline source block
            (re-search-backward "[ \f\t\n\r\v]" nil t)
            (looking-at org-babel-inline-src-block-regexp))
          (org-babel-parse-inline-src-block-match)
        nil))))

(defun org-babel-confirm-evaluate (info)
  "Confirm evaluation of the code block INFO.
This behavior can be suppressed by setting the value of
`org-confirm-babel-evaluate' to nil, in which case all future
interactive code block evaluations will proceed without any
confirmation from the user.

Note disabling confirmation may result in accidental evaluation
of potentially harmful code."
  (let* ((eval (cdr (assoc :eval (nth 2 info))))
	 (query (or (equal eval "query")
		    (and (functionp org-confirm-babel-evaluate)
			 (funcall org-confirm-babel-evaluate
				  (nth 0 info) (nth 1 info)))
		    org-confirm-babel-evaluate)))
    (when (or (equal eval "never")
	      (and query
		   (not (yes-or-no-p
			 (format "Evaluate this%scode on your system? "
				 (if info (format " %s " (nth 0 info)) " "))))))
      (error "evaluation aborted"))))

;;;###autoload
(defun org-babel-execute-src-block-maybe ()
  "Conditionally execute a source block.
Detect if this is context for a Babel src-block and if so
then run `org-babel-execute-src-block'."
  (interactive)
  (if (not org-babel-no-eval-on-ctrl-c-ctrl-c)
      (let ((info (org-babel-get-src-block-info)))
	(if info
	    (progn (org-babel-execute-src-block current-prefix-arg info) t) nil))
    nil))
(add-hook 'org-ctrl-c-ctrl-c-hook 'org-babel-execute-src-block-maybe)

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
	  session tangle var noeval comments)
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

(defvar org-babel-current-buffer-properties)
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

Optionally supply a value for INFO in the form returned by
`org-babel-get-src-block-info'.

Optionally supply a value for PARAMS which will be merged with
the header arguments specified at the front of the source code
block."
  (interactive)
  (let* ((info (or info (org-babel-get-src-block-info)))
	 ;; note the `evaluation-confirmed' variable is currently not
	 ;; used, but could be used later to avoid the need for
	 ;; chaining confirmations
	 (evaluation-confirmed (org-babel-confirm-evaluate info))
         (lang (nth 0 info))
	 (params (setf (nth 2 info)
                       (sort (org-babel-merge-params (nth 2 info) params)
                             (lambda (el1 el2) (string< (symbol-name (car el1))
                                                   (symbol-name (car el2)))))))
         (new-hash
          (if (and (cdr (assoc :cache params))
                   (string= "yes" (cdr (assoc :cache params))))
              (org-babel-sha1-hash info)))
         (old-hash (org-babel-result-hash info))
         (body (setf (nth 1 info)
		     (if (and (cdr (assoc :noweb params))
                              (string= "yes" (cdr (assoc :noweb params))))
                         (org-babel-expand-noweb-references info)
		       (nth 1 info))))
         (result-params (split-string (or (cdr (assoc :results params)) "")))
         (result-type (cond ((member "output" result-params) 'output)
			    ((member "value" result-params) 'value)
			    (t 'value)))
         (cmd (intern (concat "org-babel-execute:" lang)))
	 (dir (cdr (assoc :dir params)))
	 (default-directory
	   (or (and dir (file-name-as-directory dir)) default-directory))
	 (org-babel-call-process-region-original
	  (if (boundp 'org-babel-call-process-region-original) org-babel-call-process-region-original
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
                (message (replace-regexp-in-string "%" "%%"
                                                   (format "%S" result))) result)
            (message "executing %s code block%s..."
		     (capitalize lang)
		     (if (nth 4 info) (format " (%s)" (nth 4 info)) ""))
	    (setq result (funcall cmd body params))
            (if (eq result-type 'value)
                (setq result (if (and (or (member "vector" result-params)
                                          (member "table" result-params))
                                      (not (listp result)))
                                 (list (list result))
                               result)))
            (org-babel-insert-result
	     result result-params info new-hash indent lang)
            (run-hooks 'org-babel-after-execute-hook)
            result))
      (setq call-process-region 'org-babel-call-process-region-original))))

(defun org-babel-expand-body:generic (body params &optional processed-params)
  "Expand BODY with PARAMS.
Expand a block of code with org-babel according to it's header
arguments.  This generic implementation of body expansion is
called for languages which have not defined their own specific
org-babel-expand-body:lang function." body)

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
         (cmd (intern (concat "org-babel-expand-body:" lang)))
         (expanded (funcall (if (fboundp cmd) cmd 'org-babel-expand-body:generic)
                            body params)))
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
         (body (nth 1 info))
         (params (nth 2 info))
         (session (cdr (assoc :session params)))
	 (cmd (intern (concat "org-babel-load-session:" lang))))
    (unless (fboundp cmd)
      (error "No org-babel-load-session function for %s!" lang))
    (pop-to-buffer (funcall cmd session body params))
    (end-of-line 1)))

;;;###autoload
(defun org-babel-switch-to-session (&optional arg info)
  "Switch to the session of the current source-code block.
If called with a prefix argument then evaluate the header arguments
for the source block before entering the session. Copy the body
of the source block to the kill ring."
  (interactive)
  (let* ((info (or info (org-babel-get-src-block-info)))
         (lang (nth 0 info))
         (body (nth 1 info))
         (params (nth 2 info))
         (session (cdr (assoc :session params)))
	 (dir (cdr (assoc :dir params)))
	 (default-directory
	   (or (and dir (file-name-as-directory dir)) default-directory))
	 (cmd (intern (format "org-babel-%s-initiate-session" lang)))
	 (cmd2 (intern (concat "org-babel-prep-session:" lang))))
    (unless (fboundp cmd)
      (error "No org-babel-initiate-session function for %s!" lang))
    ;; copy body to the kill ring
    (with-temp-buffer (insert (org-babel-trim body))
                      (copy-region-as-kill (point-min) (point-max)))
    ;; if called with a prefix argument, then process header arguments
    (unless (fboundp cmd2)
      (error "No org-babel-prep-session function for %s!" lang))
    (when arg (funcall cmd2 session params))
    ;; just to the session using pop-to-buffer
    (pop-to-buffer (funcall cmd session params))
    (end-of-line 1)))

(defalias 'org-babel-pop-to-session 'org-babel-switch-to-session)

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
(defun org-babel-execute-buffer (&optional arg)
  "Execute source code blocks in a buffer.
Call `org-babel-execute-src-block' on every source block in
the current buffer."
  (interactive "P")
  (save-excursion
    (org-save-outline-visibility t
      (goto-char (point-min))
      (show-all)
      (while (re-search-forward org-babel-src-block-regexp nil t)
	(let ((pos-end (match-end 0)))
	  (goto-char (match-beginning 0))
	  (org-babel-execute-src-block arg)
	  (goto-char pos-end))))))

;;;###autoload
(defun org-babel-execute-subtree (&optional arg)
  "Execute source code blocks in a subtree.
Call `org-babel-execute-src-block' on every source block in
the current subtree."
  (interactive "P")
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (org-babel-execute-buffer)
      (widen))))

;;;###autoload
(defun org-babel-sha1-hash (&optional info)
  "Generate an sha1 hash based on the value of info."
  (interactive)
  (let* ((info (or info (org-babel-get-src-block-info)))
         (hash (sha1 (format "%s-%s" (mapconcat (lambda (arg) (format "%S" arg))
                                                (nth 2 info) ":")
                             (nth 1 info)))))
    (when (interactive-p) (message hash))
    hash))

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

(defmacro org-babel-map-src-blocks (file &rest body)
  "Evaluate BODY forms on each source-block in FILE."
  (declare (indent 1))
  `(let ((visited-p (get-file-buffer (expand-file-name ,file)))
	 to-be-removed)
     (save-window-excursion
       (find-file ,file)
       (setq to-be-removed (current-buffer))
       (goto-char (point-min))
       (while (re-search-forward org-babel-src-block-regexp nil t)
         (goto-char (match-beginning 0))
         (save-match-data ,@body)
         (goto-char (match-end 0))))
     (unless visited-p
       (kill-buffer to-be-removed))))

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
		    (cons (intern (concat ":" header-arg)) val)))
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
  (if (> (length arg-string) 0)
      (delq nil
	    (mapcar
	     (lambda (arg)
	       (if (string-match
                    "\\([^ \f\t\n\r\v]+\\)[ \f\t\n\r\v]+\\([^ \f\t\n\r\v]+.*\\)"
                    arg)
		   (cons (intern (concat ":" (match-string 1 arg)))
                         (let ((raw (org-babel-chomp (match-string 2 arg))))
                           (if (org-babel-number-p raw)
                               raw (org-babel-read raw))))
		 (cons (intern (concat ":" arg)) nil)))
	     (split-string (concat " " arg-string) "[ \f\t\n\r\v]+:" t)))))

(defun org-babel-process-params (params)
  "Parse params and resolve references.

Return a list (session vars result-params result-type colnames rownames)."
  (let* ((session (cdr (assoc :session params)))
         (vars-and-names (org-babel-disassemble-tables
                          (org-babel-ref-variables params)
                          (cdr (assoc :hlines params))
                          (cdr (assoc :colnames params))
                          (cdr (assoc :rownames params))))
         (vars     (car   vars-and-names))
         (colnames (cadr  vars-and-names))
         (rownames (caddr vars-and-names))
	 (result-params (split-string (or (cdr (assoc :results params)) "")))
	 (result-type (cond ((member "output" result-params) 'output)
			    ((member "value" result-params) 'value)
			    (t 'value))))
    (list session vars result-params result-type colnames rownames)))

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
    (let* ((width (apply 'max (mapcar (lambda (el) (if (listp el) (length el) 0)) table)))
           (table (trans (mapcar (lambda (row)
                                   (if (not (equal row 'hline))
                                       row
                                     (setq row '())
                                     (dotimes (n width) (setq row (cons 'hline row)))
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
  "Select one out of an alist of row or column names."
  (when names
    (if (and selector (symbolp selector) (not (equal t selector)))
        (cdr (assoc selector names))
      (if (integerp selector)
          (nth (- selector 1) names)
        (cdr (car (last names)))))))

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
	(setq names (cons (org-babel-clean-text-properties (match-string 2))
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
  (re-search-forward org-babel-src-block-regexp nil nil (or arg 1))
  (goto-char (match-beginning 0)) (org-show-context))

;;;###autoload
(defun org-babel-previous-src-block (&optional arg)
  "Jump to the previous source block.
With optional prefix argument ARG, jump backward ARG many source blocks."
  (interactive "P")
  (re-search-backward org-babel-src-block-regexp nil nil (or arg 1))
  (goto-char (match-beginning 0)) (org-show-context))

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
			  (number-sequence 1 indent) "")
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

raw ----- results are added directly to the org-mode file.  This
          is a good option if you code block will output org-mode
          formatted text.

org ----- this is the same as the 'raw' option

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
  (if (= (length result) 0)
      (if (member "value" result-params)
	  (message "No result returned by source block")
	(message "Source block produced no output"))
    (if (and result-params (member "silent" result-params))
        (progn
	  (message (replace-regexp-in-string "%" "%%" (format "%S" result)))
	  result)
      (when (and (stringp result) ;; ensure results end in a newline
                 (not (or (string-equal (substring result -1) "\n")
                          (string-equal (substring result -1) "\r"))))
        (setq result (concat result "\n")))
      (save-excursion
	(let ((existing-result (org-babel-where-is-src-block-result
				t info hash indent))
	      (results-switches
               (cdr (assoc :results_switches (nth 2 info))))
	      beg end)
	  (when existing-result
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
	      (goto-char (org-babel-result-end)) (setq beg (point)))
	     ((member "prepend" result-params) ;; already there
	      )))
	  (setq results-switches
                (if results-switches (concat " " results-switches) ""))
	  (cond
	   ;; assume the result is a table if it's not a string
	   ((not (stringp result))
	    (insert (concat (orgtbl-to-orgtbl
			     (if (or (eq 'hline (car result))
				     (and (listp (car result))
					  (listp (cdr (car result)))))
				 result (list result))
			     '(:fmt (lambda (cell) (format "%s" cell)))) "\n"))
	    (goto-char beg) (when (org-at-table-p) (org-table-align)))
	   ((member "file" result-params)
	    (insert result))
	   ((member "html" result-params)
	    (insert (format "#+BEGIN_HTML%s\n%s#+END_HTML\n"
                            results-switches result)))
	   ((member "latex" result-params)
	    (insert (format "#+BEGIN_LaTeX%s\n%s#+END_LaTeX\n"
                            results-switches result)))
	   ((member "code" result-params)
	    (insert (format "#+BEGIN_SRC %s%s\n%s#+END_SRC\n"
                            (or lang "none") results-switches result)))
	   ((or (member "raw" result-params) (member "org" result-params))
	    (save-excursion (insert result)) (if (org-at-table-p) (org-cycle)))
	   (t
	    (org-babel-examplize-region
	     (point) (progn (insert result) (point)) results-switches)))
	  ;; possibly indent the results to match the #+results line
	  (setq end (if (listp result) (org-table-end) (point)))
	  (when (and indent (> indent 0)
		     ;; in this case `table-align' does the work for us
		     (not (and (listp result)
			       (member "append" result-params))))
	    (indent-rigidly beg end indent))))
      (message "finished"))))

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
    (if (org-at-table-p)
        (progn (goto-char (org-table-end)) (point))
      (let ((case-fold-search t))
        (cond
         ((looking-at "[ \t]*#\\+begin_latex")
          (re-search-forward "[ \t]*#\\+end_latex" nil t)
          (forward-line 1))
         ((looking-at "[ \t]*#\\+begin_html")
          (re-search-forward "[ \t]*#\\+end_html" nil t)
          (forward-line 1))
         ((looking-at "[ \t]*#\\+begin_example")
          (re-search-forward "[ \t]*#\\+end_example" nil t)
          (forward-line 1))
         ((looking-at "[ \t]*#\\+begin_src")
          (re-search-forward "[ \t]*#\\+end_src" nil t)
          (forward-line 1))
         (t (progn (while (looking-at "[ \t]*\\(: \\|\\[\\[\\)")
                     (forward-line 1))))))
      (point))))

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
      (cond ((= size 0)
	     (error (concat "This should be impossible:"
                            "a newline was appended to result if missing")))
	    ((< size org-babel-min-lines-for-block-output)
	     (goto-char beg)
	     (dotimes (n size)
	       (beginning-of-line 1) (insert ": ") (forward-line 1)))
	    (t
	     (goto-char beg)
	     (insert (if results-switches
                         (format "#+begin_example%s\n" results-switches)
                       "#+begin_example\n"))
	     (forward-char (- end beg))
	     (insert "#+end_example\n"))))))

(defun org-babel-merge-params (&rest plists)
  "Combine all parameter association lists in PLISTS.
Later elements of PLISTS override the values of previous element.
This takes into account some special considerations for certain
parameters when merging lists."
  (let ((results-exclusive-groups
	 '(("file" "vector" "table" "scalar" "raw" "org"
            "html" "latex" "code" "pp")
	   ("replace" "silent" "append" "prepend")
	   ("output" "value")))
	(exports-exclusive-groups
	 '(("code" "results" "both" "none")))
	params results exports tangle noweb cache vars var ref shebang comments)
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
      (mapc (lambda (plist)
              (mapc (lambda (pair)
                      (case (car pair)
                        (:var
                         ;; we want only one specification per variable
                         (when (string-match
                                (concat "^\\([^= \f\t\n\r\v]+\\)[ \t]*="
                                        "[ \t]*\\([^\f\n\r\v]+\\)$") (cdr pair))
                           ;; TODO: When is this not true?
                           (setq var (intern (match-string 1 (cdr pair)))
                                 ref (match-string 2 (cdr pair))
                                 vars (cons (cons var ref)
                                            (assq-delete-all var vars)))))
                        (:results
                         (setq results
			       (e-merge results-exclusive-groups
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
			   (setq params
                                 (cons pair
                                       (assq-delete-all (car pair) params)))))
                        (:exports
                         (setq exports
                               (e-merge exports-exclusive-groups
                                        exports (split-string (cdr pair)))))
                        (:tangle ;; take the latest -- always overwrite
                         (setq tangle (or (list (cdr pair)) tangle)))
                        (:noweb
                         (setq noweb
                               (e-merge '(("yes" "no")) noweb
                                        (split-string (or (cdr pair) "")))))
                        (:cache
                         (setq cache
                               (e-merge '(("yes" "no")) cache
                                        (split-string (or (cdr pair) "")))))
                        (:shebang ;; take the latest -- always overwrite
                         (setq shebang (or (list (cdr pair)) shebang)))
                        (:comments
                         (setq comments
                               (e-merge '(("yes" "no")) comments
                                        (split-string (or (cdr pair) "")))))
                        (t ;; replace: this covers e.g. :session
                         (setq params
                               (cons pair
                                     (assq-delete-all (car pair) params))))))
                    plist))
            plists))
    (setq vars (mapcar (lambda (pair) (format "%s=%s" (car pair) (cdr pair))) vars))
    (while vars (setq params (cons (cons :var (pop vars)) params)))
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
                          (let ((raw (org-babel-ref-resolve-reference
                                      source-name nil)))
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
  "Return t if STRING represents a number."
  (if (and (string-match "^-?[0-9]*\\.?[0-9]*$" string)
           (= (length (substring string (match-beginning 0)
				 (match-end 0)))
	      (length string)))
      (string-to-number string)))

(defun org-babel-import-elisp-from-file (file-name)
  "Read the results located at FILE-NAME into an elisp table.
If the table is trivial, then return it as a scalar."
  (let (result)
    (save-window-excursion
      (with-temp-buffer
	(condition-case nil
	    (progn
	      (org-table-import file-name nil)
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
    ;; org-babel-call-process-region-original is the original emacs definition. It
    ;; is in scope from the let binding in org-babel-execute-src-block
    (apply org-babel-call-process-region-original
           start end program delete buffer display args)))

(defun org-babel-maybe-remote-file (file)
  "Conditionally parse information on a remote connnection.
If FILE specifies a remove file, then parse the information on
the remote connection."
  (if (file-remote-p default-directory)
      (let* ((vec (tramp-dissect-file-name default-directory))
             (user (tramp-file-name-user vec))
             (host (tramp-file-name-host vec)))
        (concat "/" user (when user "@") host ":" file))
    file))

(provide 'ob)

;; arch-tag: 01a7ebee-06c5-4ee4-a709-e660d28c0af1

;;; ob.el ends here
