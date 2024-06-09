;;; ob-python.el --- Babel Functions for Python      -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2024 Free Software Foundation, Inc.

;; Authors: Eric Schulte
;;	 Dan Davison
;; Maintainer: Jack Kamm <jackkamm@gmail.com>
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

;; Org-Babel support for evaluating python source code.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ob)
(require 'org-macs)
(require 'python)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("python" . "py"))

(defvar org-babel-default-header-args:python '())

(defconst org-babel-header-args:python
  '((return . :any)
    (python . :any)
    (async . ((yes no))))
  "Python-specific header arguments.")

(defcustom org-babel-python-command 'auto
  "Command (including arguments) for interactive and non-interactive Python code.
When not `auto', it overrides `org-babel-python-command-session'
and `org-babel-python-command-nonsession'."
  :package-version '(Org . "9.7")
  :group 'org-babel
  :type '(choice string (const auto)))

(defcustom org-babel-python-command-session 'auto
  "Command (including arguments) for starting interactive Python sessions.
If `auto' (the default), uses the values from
`python-shell-interpreter' and `python-shell-interpreter-args'.
If `org-babel-python-command' is set, then it overrides this
option."
  :package-version '(Org . "9.7")
  :group 'org-babel
  :type '(choice string (const auto)))

(defcustom org-babel-python-command-nonsession "python"
  "Command (including arguments) for executing non-interactive Python code.
If `org-babel-python-command' is set, then it overrides this option."
  :package-version '(Org . "9.7")
  :group 'org-babel
  :type 'string)

(defcustom org-babel-python-hline-to "None"
  "Replace hlines in incoming tables with this when translating to python."
  :group 'org-babel
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-babel-python-None-to 'hline
  "Replace `None' in python tables with this before returning."
  :group 'org-babel
  :package-version '(Org . "8.0")
  :type 'symbol)

(defun org-babel-python-associate-session (session)
  "Associate Python code buffer with an Python session.
Make SESSION without earmuffs be the Python buffer name."
  (setq-local python-shell-buffer-name
              (org-babel-python-without-earmuffs session)))

(defun org-babel-execute:python (body params)
  "Execute Python BODY according to PARAMS.
This function is called by `org-babel-execute-src-block'."
  (let* ((org-babel-python-command
	  (or (cdr (assq :python params))
	      org-babel-python-command))
	 (session (org-babel-python-initiate-session
		   (cdr (assq :session params))))
	 (graphics-file (and (member "graphics" (assq :result-params params))
			     (org-babel-graphical-output-file params)))
         (result-params (cdr (assq :result-params params)))
         (result-type (cdr (assq :result-type params)))
	 (return-val (when (eq result-type 'value)
		       (cdr (assq :return params))))
	 (preamble (cdr (assq :preamble params)))
	 (async (org-babel-comint-use-async params))
         (full-body
	  (concat
	   (org-babel-expand-body:generic
	    body params
	    (org-babel-variable-assignments:python params))
	   (when return-val
	     (format (if session "\n%s" "\nreturn %s") return-val))))
         (result (org-babel-python-evaluate
		  session full-body result-type
		  result-params preamble async graphics-file)))
    (org-babel-reassemble-table
     result
     (org-babel-pick-name (cdr (assq :colname-names params))
			  (cdr (assq :colnames params)))
     (org-babel-pick-name (cdr (assq :rowname-names params))
			  (cdr (assq :rownames params))))))

(defun org-babel-prep-session:python (session params)
  "Prepare SESSION according to the header arguments in PARAMS.
VARS contains resolved variable references."
  (let* ((session (org-babel-python-initiate-session session))
	 (var-lines
	  (org-babel-variable-assignments:python params)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (end-of-line 1) (insert var) (comint-send-input)
              (org-babel-comint-wait-for-output session))
	    var-lines))
    session))

(defun org-babel-load-session:python (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:python session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))

;; helper functions

(defconst org-babel-python--output-graphics-wrapper "\
import matplotlib.pyplot
matplotlib.pyplot.gcf().clear()
%s
matplotlib.pyplot.savefig('%s')"
  "Format string for saving Python graphical output.
Has two %s escapes, for the Python code to be evaluated, and the
file to save the graphics to.")

(defconst org-babel-python--def-format-value "\
def __org_babel_python_format_value(result, result_file, result_params):
    with open(result_file, 'w') as f:
        if 'graphics' in result_params:
            result.savefig(result_file)
        elif 'pp' in result_params:
            import pprint
            f.write(pprint.pformat(result))
        elif 'list' in result_params and isinstance(result, dict):
            f.write(str(['{} :: {}'.format(k, v) for k, v in result.items()]))
        else:
            if not set(result_params).intersection(\
['scalar', 'verbatim', 'raw']):
                def dict2table(res):
                    if isinstance(res, dict):
                        return [(k, dict2table(v)) for k, v in res.items()]
                    elif isinstance(res, list) or isinstance(res, tuple):
                        return [dict2table(x) for x in res]
                    else:
                        return res
                if 'table' in result_params:
                    result = dict2table(result)
                try:
                    import pandas
                except ImportError:
                    pass
                else:
                    if isinstance(result, pandas.DataFrame) and 'table' in result_params:
                        result = [[result.index.name or ''] + list(result.columns)] + \
[None] + [[i] + list(row) for i, row in result.iterrows()]
                    elif isinstance(result, pandas.Series) and 'table' in result_params:
                        result = list(result.items())
                try:
                    import numpy
                except ImportError:
                    pass
                else:
                    if isinstance(result, numpy.ndarray):
                        if 'table' in result_params:
                            result = result.tolist()
                        else:
                            result = repr(result)
            f.write(str(result))"
  "Python function to format value result and save it to file.")

(defun org-babel-variable-assignments:python (params)
  "Return a list of Python statements assigning the block's variables.
The assignments are defined in PARAMS."
  (mapcar
   (lambda (pair)
     (format "%s=%s"
	     (car pair)
	     (org-babel-python-var-to-python (cdr pair))))
   (org-babel--get-vars params)))

(defun org-babel-python-var-to-python (var)
  "Convert an elisp value to a python variable.
Convert an elisp value, VAR, into a string of python source code
specifying a variable of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-python-var-to-python var ", ") "]")
    (if (eq var 'hline)
	org-babel-python-hline-to
      (format
       (if (and (stringp var) (string-match "[\n\r]" var)) "\"\"%S\"\"" "%S")
       (if (stringp var) (substring-no-properties var) var)))))

(defun org-babel-python-table-or-string (results)
  "Convert RESULTS into an appropriate elisp value.
If the results look like a list or tuple (but not a dict), then
convert them into an Emacs-lisp table.  Otherwise return the
results as a string."
  (let ((res (if (and (> (length results) 0)
                      (string-equal "{" (substring results 0 1)))
                 results ;don't covert dicts to elisp
               (org-babel-script-escape results))))
    (if (listp res)
        (mapcar (lambda (el) (if (eq el 'None)
                                 org-babel-python-None-to el))
                res)
      res)))

(defvar org-babel-python-buffers '((:default . "*Python*")))

(defun org-babel-python-session-buffer (session)
  "Return the buffer associated with SESSION."
  (cdr (assoc session org-babel-python-buffers)))

(defun org-babel-python-with-earmuffs (session)
  "Return SESSION name as string, ensuring *...* around."
  (let ((name (if (stringp session) session (format "%s" session))))
    (if (and (string= "*" (substring name 0 1))
	     (string= "*" (substring name (- (length name) 1))))
	name
      (format "*%s*" name))))

(defun org-babel-python-without-earmuffs (session)
  "Return SESSION name as string, without *...* around."
  (let ((name (if (stringp session) session (format "%s" session))))
    (if (and (string= "*" (substring name 0 1))
	     (string= "*" (substring name (- (length name) 1))))
	(substring name 1 (- (length name) 1))
      name)))

(defun org-babel-session-buffer:python (session &optional _)
  "Return session buffer name for SESSION."
  (or (org-babel-python-session-buffer session)
      (org-babel-python-with-earmuffs session)))

(defun org-babel-python--python-util-comint-end-of-output-p ()
  "Return non-nil if the last prompt matches input prompt.
Backport of `python-util-comint-end-of-output-p' to emacs28.  To
be removed after minimum supported version reaches emacs29."
  (when-let ((prompt (python-util-comint-last-prompt)))
    (python-shell-comint-end-of-output-p
     (buffer-substring-no-properties
      (car prompt) (cdr prompt)))))

(defun org-babel-python--command (is-session)
  "Helper function to return the Python command.
This checks `org-babel-python-command', and then
`org-babel-python-command-session' (if IS-SESSION) or
`org-babel-python-command-nonsession' (if not IS-SESSION).  If
IS-SESSION, this might return `nil', which means to use
`python-shell-calculate-command'."
  (or (unless (eq org-babel-python-command 'auto)
        org-babel-python-command)
      (if is-session
          (unless (eq org-babel-python-command-session 'auto)
            org-babel-python-command-session)
        org-babel-python-command-nonsession)))

(defvar-local org-babel-python--initialized nil
  "Flag used to mark that python session has been initialized.")
(defun org-babel-python--setup-session ()
  "Babel Python session setup code, to be run once per session.
Function should be run from within the Python session buffer.
This is often run as a part of `python-shell-first-prompt-hook',
unless the Python session was created outside Org."
  (python-shell-send-string-no-output org-babel-python--def-format-value)
  (setq-local org-babel-python--initialized t))
(defun org-babel-python-initiate-session-by-key (&optional session)
  "Initiate a python session.
If there is not a current inferior-process-buffer matching
SESSION then create it. If inferior process already
exists (e.g. if it was manually started with `run-python'), make
sure it's configured to work with ob-python.  If session has
already been configured as such, do nothing.  Return the
initialized session."
  (save-window-excursion
    (let* ((session (if session (intern session) :default))
           (py-buffer (org-babel-session-buffer:python session))
           (python-shell-buffer-name
	    (org-babel-python-without-earmuffs py-buffer))
           (existing-session-p (comint-check-proc py-buffer))
           (cmd (org-babel-python--command t)))
      (if cmd
          (let* ((cmd-split (split-string-and-unquote cmd))
                 (python-shell-interpreter (car cmd-split))
                 (python-shell-interpreter-args
                  (combine-and-quote-strings
                   (append (cdr cmd-split)
                           (when (member system-type
                                         '(cygwin windows-nt ms-dos))
                             (list "-i"))))))
            (run-python))
        (run-python))
      (with-current-buffer py-buffer
        (if existing-session-p
            ;; Session was created outside Org.  Assume first prompt
            ;; already happened; run session setup code directly
            (unless org-babel-python--initialized
              ;; Ensure first prompt. Based on python-tests.el
              ;; (`python-tests-shell-wait-for-prompt')
              (while (not (org-babel-python--python-util-comint-end-of-output-p))
                (sit-for 0.1))
              (org-babel-python--setup-session))
          ;; Adding to `python-shell-first-prompt-hook' immediately
          ;; after `run-python' should be safe from race conditions,
          ;; because subprocess output only arrives when Emacs is
          ;; waiting (see elisp manual, "Output from Processes")
          (add-hook
           'python-shell-first-prompt-hook
           #'org-babel-python--setup-session
           nil 'local)))
      ;; Wait until Python initializes
      ;; This is more reliable compared to
      ;; `org-babel-comint-wait-for-output' as python may emit
      ;; multiple prompts during initialization.
      (with-current-buffer py-buffer
        (while (not org-babel-python--initialized)
          (sleep-for 0.010)))
      (setq org-babel-python-buffers
	    (cons (cons session py-buffer)
		  (assq-delete-all session org-babel-python-buffers)))
      session)))

(defun org-babel-python-initiate-session (&optional session _params)
  "Initiate Python session named SESSION according to PARAMS.
If there is not a current inferior-process-buffer matching
SESSION then create it. If inferior process already
exists (e.g. if it was manually started with `run-python'), make
sure it's configured to work with ob-python.  If session has
already been configured as such, do nothing."
  (unless (string= session "none")
    (org-babel-python-session-buffer
     (org-babel-python-initiate-session-by-key session))))

(defvar org-babel-python-eoe-indicator "org_babel_python_eoe"
  "A string to indicate that evaluation has completed.")

(defun org-babel-python-format-session-value
    (src-file result-file result-params)
  "Return Python code to evaluate SRC-FILE and write result to RESULT-FILE.
RESULT-PARAMS defines the result type."
  (format "\
import ast
with open('%s') as __org_babel_python_tmpfile:
    __org_babel_python_ast = ast.parse(__org_babel_python_tmpfile.read())
__org_babel_python_final = __org_babel_python_ast.body[-1]
if isinstance(__org_babel_python_final, ast.Expr):
    __org_babel_python_ast.body = __org_babel_python_ast.body[:-1]
    exec(compile(__org_babel_python_ast, '<string>', 'exec'))
    __org_babel_python_final = eval(compile(ast.Expression(
        __org_babel_python_final.value), '<string>', 'eval'))
else:
    exec(compile(__org_babel_python_ast, '<string>', 'exec'))
    __org_babel_python_final = None
__org_babel_python_format_value(__org_babel_python_final, '%s', %s)"
	  (org-babel-process-file-name src-file 'noquote)
	  (org-babel-process-file-name result-file 'noquote)
	  (org-babel-python-var-to-python result-params)))

(defun org-babel-python-evaluate
    (session body &optional result-type result-params preamble async graphics-file)
  "Evaluate BODY as Python code."
  (if session
      (if async
	  (org-babel-python-async-evaluate-session
	   session body result-type result-params graphics-file)
	(org-babel-python-evaluate-session
	 session body result-type result-params graphics-file))
    (org-babel-python-evaluate-external-process
     body result-type result-params preamble graphics-file)))

(defun org-babel-python--shift-right (body &optional count)
  (with-temp-buffer
    (python-mode)
    (insert body)
    (goto-char (point-min))
    (while (not (eobp))
      (unless (python-syntax-context 'string)
	(python-indent-shift-right (line-beginning-position)
				   (line-end-position)
				   count))
      (forward-line 1))
    (buffer-string)))

(defun org-babel-python-evaluate-external-process
    (body &optional result-type result-params preamble graphics-file)
  "Evaluate BODY in external python process.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of
the last statement in BODY, as elisp.  If GRAPHICS-FILE is
non-nil, then save graphical results to that file instead."
  (let ((raw
         (pcase result-type
           (`output (org-babel-eval (org-babel-python--command nil)
				    (concat preamble (and preamble "\n")
                                            (if graphics-file
                                                (format org-babel-python--output-graphics-wrapper
                                                        body graphics-file)
                                              body))))
           (`value (let ((results-file (or graphics-file
				           (org-babel-temp-file "python-"))))
		     (org-babel-eval (org-babel-python--command nil)
		      (concat
		       preamble (and preamble "\n")
		       (format
			(concat org-babel-python--def-format-value "
def main():
%s

__org_babel_python_format_value(main(), '%s', %s)")
                        (org-babel-python--shift-right body)
			(org-babel-process-file-name results-file 'noquote)
			(org-babel-python-var-to-python result-params))))
		     (org-babel-eval-read-file results-file))))))
    (org-babel-result-cond result-params
      raw
      (org-babel-python-table-or-string raw))))

(defun org-babel-python-send-string (session body)
  "Pass BODY to the Python process in SESSION.
Return output."
  (with-current-buffer session
    (let* ((string-buffer "")
	   (comint-output-filter-functions
	    (cons (lambda (text) (setq string-buffer
				       (concat string-buffer text)))
		  comint-output-filter-functions))
	   (body (format "\
try:
%s
except:
    raise
finally:
    print('%s')"
			 (org-babel-python--shift-right body 4)
			 org-babel-python-eoe-indicator)))
      (let ((python-shell-buffer-name
	     (org-babel-python-without-earmuffs session)))
	(python-shell-send-string body))
      ;; same as `python-shell-comint-end-of-output-p' in emacs-25.1+
      (while (not (and (python-shell-comint-end-of-output-p string-buffer)
                       (string-match
		        org-babel-python-eoe-indicator
		        string-buffer)))
	(accept-process-output (get-buffer-process (current-buffer))))
      (org-babel-chomp (substring string-buffer 0 (match-beginning 0))))))

(defun org-babel-python-evaluate-session
    (session body &optional result-type result-params graphics-file)
  "Pass BODY to the Python process in SESSION.
If RESULT-TYPE equals `output' then return standard output as a
string.  If RESULT-TYPE equals `value' then return the value of
the last statement in BODY, as elisp.  If GRAPHICS-FILE is
non-nil, then save graphical results to that file instead."
  (let* ((tmp-src-file (org-babel-temp-file "python-"))
         (results
	  (progn
	    (with-temp-file tmp-src-file
              (insert (if (and graphics-file (eq result-type 'output))
                          (format org-babel-python--output-graphics-wrapper
                                  body graphics-file)
                        body)))
            (pcase result-type
	      (`output
	       (let ((body (format "\
with open('%s') as f:
    exec(compile(f.read(), f.name, 'exec'))"
				   (org-babel-process-file-name
				    tmp-src-file 'noquote))))
		 (org-babel-python-send-string session body)))
              (`value
               (let* ((results-file (or graphics-file
					(org-babel-temp-file "python-")))
		      (body (org-babel-python-format-session-value
			     tmp-src-file results-file result-params)))
		 (org-babel-python-send-string session body)
		 (sleep-for 0.010)
		 (org-babel-eval-read-file results-file)))))))
    (org-babel-result-cond result-params
      results
      (org-babel-python-table-or-string results))))

(defun org-babel-python-read-string (string)
  "Strip \\='s from around Python STRING."
  (if (and (string-prefix-p "'" string)
	   (string-suffix-p "'" string))
      (substring string 1 -1)
    string))

;; Async session eval

(defconst org-babel-python-async-indicator "print ('ob_comint_async_python_%s_%s')")

(defun org-babel-python-async-value-callback (params tmp-file)
  (let ((result-params (cdr (assq :result-params params)))
	(results (org-babel-eval-read-file tmp-file)))
    (org-babel-result-cond result-params
      results
      (org-babel-python-table-or-string results))))

(defun org-babel-python-async-evaluate-session
    (session body &optional result-type result-params graphics-file)
  "Asynchronously evaluate BODY in SESSION.
Returns a placeholder string for insertion, to later be replaced
by `org-babel-comint-async-filter'."
  (org-babel-comint-async-register
   session (current-buffer)
   "ob_comint_async_python_\\(.+\\)_\\(.+\\)"
   'org-babel-chomp 'org-babel-python-async-value-callback)
  (pcase result-type
    (`output
     (let ((uuid (org-id-uuid)))
       (with-temp-buffer
         (insert (format org-babel-python-async-indicator "start" uuid))
         (insert "\n")
         (insert (if graphics-file
                     (format org-babel-python--output-graphics-wrapper
                             body graphics-file)
                   body))
         (insert "\n")
         (insert (format org-babel-python-async-indicator "end" uuid))
         (let ((python-shell-buffer-name
                (org-babel-python-without-earmuffs session)))
           (python-shell-send-buffer)))
       uuid))
    (`value
     (let ((results-file (or graphics-file
			     (org-babel-temp-file "python-")))
           (tmp-src-file (org-babel-temp-file "python-")))
       (with-temp-file tmp-src-file (insert body))
       (with-temp-buffer
         (insert (org-babel-python-format-session-value
                  tmp-src-file results-file result-params))
         (insert "\n")
         (unless graphics-file
           (insert (format org-babel-python-async-indicator "file" results-file)))
         (let ((python-shell-buffer-name
                (org-babel-python-without-earmuffs session)))
           (python-shell-send-buffer)))
       results-file))))

(provide 'ob-python)

;;; ob-python.el ends here
