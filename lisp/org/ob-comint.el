;;; ob-comint.el --- Babel Functions for Interaction with Comint Buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2022 Free Software Foundation, Inc.

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
(require 'comint)

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

(defmacro org-babel-comint-with-output (meta &rest body)
  "Evaluate BODY in BUFFER and return process output.
Will wait until EOE-INDICATOR appears in the output, then return
all process output.  If REMOVE-ECHO and FULL-BODY are present and
non-nil, then strip echo'd body from the returned output.  META
should be a list containing the following where the last two
elements are optional.

 (BUFFER EOE-INDICATOR REMOVE-ECHO FULL-BODY)

This macro ensures that the filter is removed in case of an error
or user `keyboard-quit' during execution of body."
  (declare (indent 1) (debug (sexp body)))
  (let ((buffer (nth 0 meta))
	(eoe-indicator (nth 1 meta))
	(remove-echo (nth 2 meta))
	(full-body (nth 3 meta))
        (org-babel-comint-prompt-separator
         "org-babel-comint-prompt-separator"))
    `(org-babel-comint-in-buffer ,buffer
       (let* ((string-buffer "")
	      (comint-output-filter-functions
	       (cons (lambda (text)
                       (setq string-buffer
                             (concat
                              string-buffer
                              ;; Upon concatenation, the prompt may no
                              ;; longer match `comint-prompt-regexp'.
                              ;; In particular, when the regexp has ^
                              ;; and the output does not contain
                              ;; trailing newline.  Use more reliable
                              ;; match to split the output later.
                              (replace-regexp-in-string
                               comint-prompt-regexp
                               ,org-babel-comint-prompt-separator
                               text))))
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
	 (while (progn
		  (goto-char comint-last-input-end)
		  (not (save-excursion
			 (and (re-search-forward
			       (regexp-quote ,eoe-indicator) nil t)
			      (re-search-forward
			       comint-prompt-regexp nil t)))))
	   (accept-process-output (get-buffer-process (current-buffer))))
	 ;; replace cut dangling text
	 (goto-char (process-mark (get-buffer-process (current-buffer))))
	 (insert dangling-text)

	 ;; remove echo'd FULL-BODY from input
	 (when (and ,remove-echo ,full-body
		    (string-match
		     (replace-regexp-in-string
		      "\n" "[\r\n]+" (regexp-quote (or ,full-body "")))
		     string-buffer))
	   (setq string-buffer (substring string-buffer (match-end 0))))
         (delete "" (split-string
                     string-buffer
                     ,org-babel-comint-prompt-separator))))))

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
    (while (progn
             (goto-char comint-last-input-end)
             (not (and (re-search-forward comint-prompt-regexp nil t)
                     (goto-char (match-beginning 0)))))
      (accept-process-output (get-buffer-process buffer)))))

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

(defvar-local org-babel-comint-async-dangling nil
  "Dangling piece of the last process output, in case
`org-babel-comint-async-indicator' is spread across multiple
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
			      (when (search-forward tmp-file nil t)
			        (org-babel-previous-src-block)
                                (let* ((info (org-babel-get-src-block-info))
                                       (params (nth 2 info))
                                       (result-params
                                        (cdr (assq :result-params params))))
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
                     (- (match-beginning 0) 1)
		     ;; find the matching start indicator
		     (cl-loop
                      do (re-search-backward indicator)
		      until (and (equal (match-string 1) "start")
				 (equal (match-string 2) uuid))
		      finally return (+ 1 (match-end 0)))))
		   ;; Apply callback to clean up the result
		   (res-str (funcall org-babel-comint-async-chunk-callback
                                     res-str-raw)))
	      ;; Search for uuid in associated org-buffers to insert results
	      (cl-loop for buf in org-buffers
		       until (with-current-buffer buf
			       (save-excursion
			         (goto-char (point-min))
			         (when (search-forward uuid nil t)
				   (org-babel-previous-src-block)
                                   (let* ((info (org-babel-get-src-block-info))
                                          (params (nth 2 info))
                                          (result-params
                                           (cdr (assq :result-params params))))
				     (org-babel-insert-result
                                      res-str result-params info))
				   t))))
	      ;; Remove uuid from the list to search for
	      (setq uuid-list (delete uuid uuid-list)))))))))

(defun org-babel-comint-async-register
    (session-buffer org-buffer indicator-regexp
		    chunk-callback file-callback)
  "Set local org-babel-comint-async variables in SESSION-BUFFER.
ORG-BUFFER is added to `org-babel-comint-async-buffers' if not
present.  `org-babel-comint-async-indicator',
`org-babel-comint-async-chunk-callback', and
`org-babel-comint-async-file-callback' are set to
INDICATOR-REGEXP, CHUNK-CALLBACK, and FILE-CALLBACK
respectively."
  (org-babel-comint-in-buffer session-buffer
    (setq org-babel-comint-async-indicator indicator-regexp
	  org-babel-comint-async-chunk-callback chunk-callback
	  org-babel-comint-async-file-callback file-callback)
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
