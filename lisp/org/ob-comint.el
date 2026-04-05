;;; ob-comint.el --- Babel Functions for Interaction with Comint Buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2026 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research, comint
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

;; These functions build on comint to ease the sending and receiving
;; of commands and results from comint buffers.

;; Note that the buffers in this file are analogous to sessions in
;; org-babel at large.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ob-core)
(require 'org-compat)
(require 'org-element-ast)

(require 'comint)

(declare-function org-element-context "org-element" (&optional element))
(defvar org-element-inline-src-block-regexp)

(defun org-babel-comint-buffer-livep (buffer)
  "Check if BUFFER is a comint buffer with a live process."
  (let ((buffer (when buffer (get-buffer buffer))))
    (and buffer (buffer-live-p buffer) (get-buffer-process buffer) buffer)))

(defmacro org-babel-comint-in-buffer (buffer &rest body)
  "Check BUFFER and execute BODY.
BUFFER is checked with `org-babel-comint-buffer-livep'.  BODY is
executed inside the protection of `save-excursion' and
`save-match-data'."
  (declare (indent 1) (debug t))
  `(progn
     (unless (org-babel-comint-buffer-livep ,buffer)
       (error "Buffer %s does not exist or has no process" ,buffer))
     (save-match-data
       (with-current-buffer ,buffer
	 (save-excursion
	   (let ((comint-input-filter (lambda (_input) nil)))
	     ,@body))))))

(defvaralias 'org-babel-comint-prompt-regexp-old 'org-babel-comint-prompt-regexp-fallback)
(defvar org-babel-comint-prompt-regexp-fallback nil
  "Fallback regexp used to detect prompt.")

(defcustom org-babel-comint-fallback-regexp-threshold 5.0
  "Waiting time until trying to use fallback regexp to detect prompt.
This is useful when prompt unexpectedly changes."
  :type 'float
  :group 'org-babel
  :package-version '(Org . "9.7"))

(defun org-babel-comint--set-fallback-prompt ()
  "Swap `comint-prompt-regexp' and `org-babel-comint-prompt-regexp-fallback'."
  (when org-babel-comint-prompt-regexp-fallback
    (let ((tmp comint-prompt-regexp))
      (setq comint-prompt-regexp org-babel-comint-prompt-regexp-fallback
            org-babel-comint-prompt-regexp-fallback tmp))))

(defun org-babel-comint--prompt-filter (string &optional prompt-regexp)
  "Remove PROMPT-REGEXP from STRING.

PROMPT-REGEXP defaults to `comint-prompt-regexp'."
  (let* ((prompt-regexp (or prompt-regexp comint-prompt-regexp))
         ;; We need newline in case if we do progressive replacement
         ;; of agglomerated comint prompts with `comint-prompt-regexp'
         ;; containing ^.
         (separator "org-babel-comint--prompt-filter-separator\n"))
    (while (string-match-p prompt-regexp string)
      (setq string
            (replace-regexp-in-string
             (format "\\(?:%s\\)?\\(?:%s\\)[ \t]*" separator prompt-regexp)
             separator string)))
    (delete "" (split-string string separator))))

(defun org-babel-comint--echo-filter (string &optional echo)
  "Remove ECHO from STRING."
  (and echo string
       (string-match
        (replace-regexp-in-string "\n" "[\r\n]+" (regexp-quote echo))
        string)
       (setq string (substring string (match-end 0))))
  string)

(defun org-babel-comint--remove-prompts-p (prompt-handling)
  "Helper to decide whether to remove prompts from comint output.
Parses the symbol in PROMPT-HANDLING, which can be
`filter-prompts', in which case prompts should be removed; or
`disable-prompt-filtering', in which case prompt filtering is
skipped.  For backward-compatibility, the default value of `nil'
is equivalent to `filter-prompts'."
  (cond
   ((eq prompt-handling 'disable-prompt-filtering) nil)
   ((eq prompt-handling 'filter-prompts) t)
   ((eq prompt-handling nil) t)
   (t (error (format "Unrecognized prompt handling behavior %s"
                     prompt-handling)))))

(defmacro org-babel-comint-with-output (meta &rest body)
  "Evaluate BODY in BUFFER and return process output.
Will wait until EOE-INDICATOR appears in the output, then return
all process output.  If REMOVE-ECHO and FULL-BODY are present and
non-nil, then strip echo'd body from the returned output.
PROMPT-HANDLING may be either of the symbols `filter-prompts', in
which case the output is split by `comint-prompt-regexp' and
returned as a list; or, `disable-prompt-filtering', which
suppresses this behavior and returns the full output as a string.
META should be a list containing the following where the last
three elements are optional.

 (BUFFER EOE-INDICATOR REMOVE-ECHO FULL-BODY PROMPT-HANDLING)

This macro ensures that the filter is removed in case of an error
or user `keyboard-quit' during execution of body."
  (declare (indent 1) (debug (sexp body)))
  (let ((buffer (nth 0 meta))
	(eoe-indicator (nth 1 meta))
	(remove-echo (nth 2 meta))
	(full-body (nth 3 meta))
        (prompt-handling (nth 4 meta)))
    `(org-babel-comint-in-buffer ,buffer
       (let* ((string-buffer "")
	      (comint-output-filter-functions
	       (cons (lambda (text)
                       (setq string-buffer (concat string-buffer text)))
		     comint-output-filter-functions))
	      dangling-text)
         ;; got located, and save dangling text
	 (goto-char (process-mark (get-buffer-process (current-buffer))))
	 (let ((start (point))
	       (end (point-max)))
	   (setq dangling-text (buffer-substring start end))
	   (delete-region start end))
	 ;; pass FULL-BODY to process
	 ,@body
	 ;; wait for end-of-evaluation indicator
         (let ((start-time (current-time)))
	   (while (not (save-excursion
		         (and (string-match
			       (regexp-quote ,eoe-indicator) string-buffer)
			      (string-match
			       comint-prompt-regexp string-buffer))))
	     (accept-process-output
              (get-buffer-process (current-buffer))
              org-babel-comint-fallback-regexp-threshold)
             (when (and org-babel-comint-prompt-regexp-fallback
                        (> (float-time (time-since start-time))
                           org-babel-comint-fallback-regexp-threshold)
                        (progn
		          (goto-char comint-last-input-end)
		          (save-excursion
                            (and
                             (string-match
			      (regexp-quote ,eoe-indicator) string-buffer)
			     (string-match
                              org-babel-comint-prompt-regexp-fallback string-buffer)))))
               (org-babel-comint--set-fallback-prompt))))
	 ;; replace cut dangling text
         (goto-char (process-mark (get-buffer-process (current-buffer))))
	 (insert dangling-text)

         ;; remove echo'd FULL-BODY from input
         (and ,remove-echo ,full-body
              (setq string-buffer (org-babel-comint--echo-filter string-buffer ,full-body)))

         (if (org-babel-comint--remove-prompts-p ,prompt-handling)
             (org-babel-comint--prompt-filter string-buffer)
           string-buffer)))))

(defun org-babel-comint-input-command (buffer cmd)
  "Pass CMD to BUFFER.
The input will not be echoed."
  (org-babel-comint-in-buffer buffer
    (goto-char (process-mark (get-buffer-process buffer)))
    (insert cmd)
    (comint-send-input)
    (org-babel-comint-wait-for-output buffer)))

(defun org-babel-comint-wait-for-output (buffer)
  "Wait until output arrives from BUFFER.
Note: this is only safe when waiting for the result of a single
statement (not large blocks of code)."
  (org-babel-comint-in-buffer buffer
    (let ((start-time (current-time)))
      (while (progn
               (goto-char comint-last-input-end)
               (not (and (re-search-forward comint-prompt-regexp nil t)
                       (goto-char (match-beginning 0)))))
        (accept-process-output
         (get-buffer-process buffer)
         org-babel-comint-fallback-regexp-threshold)
        (when (and org-babel-comint-prompt-regexp-fallback
                   (> (float-time (time-since start-time))
                      org-babel-comint-fallback-regexp-threshold)
                   (progn
		     (goto-char comint-last-input-end)
		     (save-excursion
		       (re-search-forward
                        org-babel-comint-prompt-regexp-fallback nil t))))
          (org-babel-comint--set-fallback-prompt))))))

(defun org-babel-comint-eval-invisibly-and-wait-for-file
    (buffer file string &optional period)
  "Evaluate STRING in BUFFER invisibly.
Don't return until FILE exists.  Code in STRING must ensure that
FILE exists at end of evaluation."
  (unless (org-babel-comint-buffer-livep buffer)
    (error "Buffer %s does not exist or has no process" buffer))
  (when (file-exists-p file) (delete-file file))
  (process-send-string
   (get-buffer-process buffer)
   (if (= (aref string (1- (length string))) ?\n) string (concat string "\n")))
  (while (not (file-exists-p file)) (sit-for (or period 0.25))))


;;; Async evaluation

(defvar-local org-babel-comint-async-indicator nil
  "Regular expression that `org-babel-comint-async-filter' scans for.
It should have 2 parenthesized expressions,
e.g. \"org_babel_async_\\(start\\|end\\|file\\)_\\(.*\\)\".  The
first parenthesized expression determines whether the token is
delimiting a result block, or whether the result is in a file.
If delimiting a block, the second expression gives a UUID for the
location to insert the result.  Otherwise, the result is in a tmp
file, and the second expression gives the file name.")

(defvar-local org-babel-comint-async-buffers nil
  "List of Org mode buffers to check for Babel async output results.")

(defvar-local org-babel-comint-async-file-callback nil
  "Callback to clean and insert Babel async results from a temp file.
The callback function takes two arguments: the alist of params of the Babel
source block, and the name of the temp file.")

(defvar-local org-babel-comint-async-chunk-callback nil
  "Callback function to clean Babel async output results before insertion.
Its single argument is a string consisting of output from the
comint process.  It should return a string that will be passed
to `org-babel-insert-result'.")

(defvar-local org-babel-comint-async-remove-prompts-p t
  "Whether prompts should be detected and removed from async output.")

(defvar-local org-babel-comint-async-dangling nil
  "Dangling piece of the last process output, as a string.
Used when `org-babel-comint-async-indicator' is spread across multiple
comint outputs due to buffering.")

(defun org-babel-comint-use-async (params)
  "Determine whether to use session async evaluation.
PARAMS are the header arguments as passed to
`org-babel-execute:lang'."
  (let ((async (assq :async params))
        (session (assq :session params)))
    (and async
	 (not org-babel-exp-reference-buffer)
         (not (equal (cdr async) "no"))
         (not (equal (cdr session) "none")))))

(defun org-babel-comint-async-filter (string)
  "Captures Babel async output from comint buffer back to Org mode buffers.
This function is added as a hook to `comint-output-filter-functions'.
STRING contains the output originally inserted into the comint buffer."
  ;; Remove outdated Org mode buffers
  (setq org-babel-comint-async-buffers
	(cl-loop for buf in org-babel-comint-async-buffers
	         if (buffer-live-p buf)
	         collect buf))
  (let* ((indicator org-babel-comint-async-indicator)
	 (org-buffers org-babel-comint-async-buffers)
	 (file-callback org-babel-comint-async-file-callback)
	 (combined-string (concat org-babel-comint-async-dangling string))
	 (new-dangling combined-string)
         ;; Assumes comint filter called with session buffer current
         (session-dir default-directory)
	 ;; list of UUID's matched by `org-babel-comint-async-indicator'
	 uuid-list)
    (with-temp-buffer
      (insert combined-string)
      (goto-char (point-min))
      (while (re-search-forward indicator nil t)
	;; update dangling
	(setq new-dangling (buffer-substring (point) (point-max)))
	(cond ((equal (match-string 1) "end")
	       ;; save UUID for insertion later
	       (push (match-string 2) uuid-list))
	      ((equal (match-string 1) "file")
	       ;; insert results from tmp-file
	       (let ((tmp-file (match-string 2)))
		 (cl-loop for buf in org-buffers
		          until
		          (with-current-buffer buf
			    (save-excursion
			      (goto-char (point-min))
			      (when (org-babel-comint-async--find-src tmp-file)
                                (let* ((info (org-babel-get-src-block-info))
                                       (params (nth 2 info))
                                       (result-params
                                        (cdr (assq :result-params params)))
                                       (default-directory session-dir))
                                  (org-babel-insert-result
                                   (funcall file-callback
                                            (nth
                                             2 (org-babel-get-src-block-info))
                                            tmp-file)
                                   result-params info))
			        t))))))))
      ;; Truncate dangling to only the most recent output
      (when (> (length new-dangling) (length string))
	(setq new-dangling string)))
    (setq-local org-babel-comint-async-dangling new-dangling)
    (when uuid-list
      ;; Search for results in the comint buffer
      (save-excursion
	(goto-char (point-max))
	(while uuid-list
	  (re-search-backward indicator)
	  (when (equal (match-string 1) "end")
	    (let* ((uuid (match-string-no-properties 2))
		   (res-str-raw
		    (buffer-substring
		     ;; move point to beginning of indicator
                     (match-beginning 0)
		     ;; find the matching start indicator
		     (cl-loop
                      do (re-search-backward indicator)
		      until (and (equal (match-string 1) "start")
				 (equal (match-string 2) uuid))
		      finally return (+ 1 (match-end 0)))))
		   ;; Apply user callback
		   (res-str (funcall org-babel-comint-async-chunk-callback
                                     (if org-babel-comint-async-remove-prompts-p
                                         (org-trim (string-join
                                                    (mapcar #'org-trim
                                                            (org-babel-comint--prompt-filter
                                                             res-str-raw))
                                                    "\n")
                                                   t)
                                       res-str-raw))))
	      ;; Search for uuid in associated org-buffers to insert results
	      (cl-loop for buf in org-buffers
		       until (with-current-buffer buf
			       (save-excursion
			         (goto-char (point-min))
			         (when (org-babel-comint-async--find-src uuid)
                                   (let* ((info (org-babel-get-src-block-info))
                                          (params (nth 2 info))
                                          (result-params
                                           (cdr (assq :result-params params)))
                                          (default-directory session-dir))
				     (org-babel-insert-result
                                      res-str result-params info))
				   t))))
	      ;; Remove uuid from the list to search for
	      (setq uuid-list (delete uuid uuid-list)))))))))

(defun org-babel-comint-async--find-src (uuid-or-tmpfile)
  "Find source block associated with an async comint result.
UUID-OR-TMPFILE is the uuid or tmpfile associated with the result.
Returns non-nil if the source block is succesfully found, and moves
point there.

This function assumes that UUID-OR-TMPFILE was previously inserted as
the source block's result, as a placeholder until the true result
becomes ready.  It may fail to find the source block if the buffer was
modified so that UUID-OR-TMPFILE is no longer the result of the source
block, or if it has been copied elsewhere into the buffer (this is a
limitation of the current async implementation)."
  (goto-char (point-min))
  (when (search-forward uuid-or-tmpfile nil t)
    (let ((uuid-pos (point)))
      (and (re-search-backward
            ;; find the nearest preceding src or inline-src block
            (rx (or (regexp org-babel-src-block-regexp)
                    (regexp org-element-inline-src-block-regexp)))
            nil t)
           ;; check it's actually a src block and not verbatim text
           (org-element-type-p (org-element-context)
                               '(inline-src-block src-block))
           ;; Check result contains the uuid. There isn't a simple way
           ;; to extract the result value that works in all cases
           ;; (e.g. inline blocks or results drawers), so instead
           ;; check the result region contains the found uuid position
           (let ((result-where (org-babel-where-is-src-block-result)))
             (when result-where
               (save-excursion
                 (goto-char result-where)
                 (and
                  (>= uuid-pos (org-element-property :begin (org-element-context)))
                  (< uuid-pos (org-element-property :end (org-element-context)))))))))))

(defun org-babel-comint-async-register
    (session-buffer org-buffer indicator-regexp
		    chunk-callback file-callback
                    &optional prompt-handling)
  "Set local org-babel-comint-async variables in SESSION-BUFFER.
ORG-BUFFER is added to `org-babel-comint-async-buffers' if not
present.  `org-babel-comint-async-indicator',
`org-babel-comint-async-chunk-callback', and
`org-babel-comint-async-file-callback' are set to
INDICATOR-REGEXP, CHUNK-CALLBACK, and FILE-CALLBACK respectively.
PROMPT-HANDLING may be either of the symbols `filter-prompts', in
which case prompts matching `comint-prompt-regexp' are filtered
from output before it is passed to CHUNK-CALLBACK, or
`disable-prompt-filtering', in which case this behavior is
disabled.  For backward-compatibility, the default value of `nil'
is equivalent to `filter-prompts'."
  (org-babel-comint-in-buffer session-buffer
    (setq org-babel-comint-async-indicator indicator-regexp
	  org-babel-comint-async-chunk-callback chunk-callback
	  org-babel-comint-async-file-callback file-callback)
    (setq org-babel-comint-async-remove-prompts-p
          (org-babel-comint--remove-prompts-p prompt-handling))
    (unless (memq org-buffer org-babel-comint-async-buffers)
      (setq org-babel-comint-async-buffers
	    (cons org-buffer org-babel-comint-async-buffers)))
    (add-hook 'comint-output-filter-functions
	      'org-babel-comint-async-filter nil t)))

(defmacro org-babel-comint-async-delete-dangling-and-eval
    (session-buffer &rest body)
  "Remove dangling text in SESSION-BUFFER and evaluate BODY.
This is analogous to `org-babel-comint-with-output', but meant
for asynchronous output, and much shorter because inserting the
result is delegated to `org-babel-comint-async-filter'."
  (declare (indent 1) (debug t))
  `(org-babel-comint-in-buffer ,session-buffer
     (goto-char (process-mark (get-buffer-process (current-buffer))))
     (delete-region (point) (point-max))
     ,@body))

(provide 'ob-comint)



;;; ob-comint.el ends here
