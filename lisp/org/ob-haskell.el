;;; ob-haskell.el --- Babel Functions for Haskell    -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2022 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Maintainer: Lawrence Bottorff <borgauf@gmail.com>
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

;; Org Babel support for evaluating Haskell source code.
;; Haskell programs must be compiled before
;; they can be run, but haskell code can also be run through an
;; interactive interpreter.
;;
;; By default we evaluate using the Haskell interpreter.
;; To use the compiler, specify :compile yes in the header.

;;; Requirements:

;; - haskell-mode: https://www.iro.umontreal.ca/~monnier/elisp/#haskell-mode
;; - inf-haskell: https://www.iro.umontreal.ca/~monnier/elisp/#haskell-mode
;; - (optionally) lhs2tex: https://people.cs.uu.nl/andres/lhs2tex/

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ob)
(require 'org-macs)
(require 'comint)

(declare-function haskell-mode "ext:haskell-mode" ())
(declare-function run-haskell "ext:inf-haskell" (&optional arg))
(declare-function inferior-haskell-load-file
		  "ext:inf-haskell" (&optional reload))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("haskell" . "hs"))

(defvar org-babel-default-header-args:haskell
  '((:padlines . "no")))

(defvar org-babel-haskell-lhs2tex-command "lhs2tex")

(defvar org-babel-haskell-eoe "\"org-babel-haskell-eoe\"")

(defvar haskell-prompt-regexp)

(defcustom org-babel-haskell-compiler "ghc"
  "Command used to compile a Haskell source code file into an executable.
May be either a command in the path, like \"ghc\" or an absolute
path name, like \"/usr/local/bin/ghc\".  The command can include
a parameter, such as \"ghc -v\"."
  :group 'org-babel
  :package-version '(Org "9.4")
  :type 'string)

(defconst org-babel-header-args:haskell '((compile . :any))
  "Haskell-specific header arguments.")

(defun org-babel-haskell-execute (body params)
  "This function should only be called by `org-babel-execute:haskell'."
  (let* ((tmp-src-file (org-babel-temp-file "Haskell-src-" ".hs"))
         (tmp-bin-file
          (org-babel-process-file-name
           (org-babel-temp-file "Haskell-bin-" org-babel-exeext)))
         (cmdline (cdr (assq :cmdline params)))
         (cmdline (if cmdline (concat " " cmdline) ""))
         (flags (cdr (assq :flags params)))
         (flags (mapconcat #'identity
		           (if (listp flags)
                               flags
                             (list flags))
			   " "))
         (libs (org-babel-read
	        (or (cdr (assq :libs params))
	            (org-entry-get nil "libs" t))
	        nil))
         (libs (mapconcat #'identity
		          (if (listp libs) libs (list libs))
		          " ")))
    (with-temp-file tmp-src-file (insert body))
    (org-babel-eval
     (format "%s -o %s %s %s %s"
             org-babel-haskell-compiler
	     tmp-bin-file
	     flags
	     (org-babel-process-file-name tmp-src-file)
	     libs)
     "")
    (let ((results (org-babel-eval (concat tmp-bin-file cmdline) "")))
      (when results
        (setq results (org-trim (org-remove-indentation results)))
        (org-babel-reassemble-table
         (org-babel-result-cond (cdr (assq :result-params params))
	   (org-babel-read results t)
	   (let ((tmp-file (org-babel-temp-file "Haskell-")))
	     (with-temp-file tmp-file (insert results))
	     (org-babel-import-elisp-from-file tmp-file)))
         (org-babel-pick-name
	  (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
         (org-babel-pick-name
	  (cdr (assq :rowname-names params)) (cdr (assq :rownames params))))))))

(defun org-babel-interpret-haskell (body params)
  (require 'inf-haskell)
  (add-hook 'inferior-haskell-hook
            (lambda ()
              (setq-local comint-prompt-regexp
                          (concat haskell-prompt-regexp "\\|^λ?> "))))
  (let* ((session (cdr (assq :session params)))
         (result-type (cdr (assq :result-type params)))
         (full-body (org-babel-expand-body:generic
		     body params
		     (org-babel-variable-assignments:haskell params)))
         (session (org-babel-haskell-initiate-session session params))
	 (comint-preoutput-filter-functions
	  (cons 'ansi-color-filter-apply comint-preoutput-filter-functions))
         (raw (org-babel-comint-with-output
		  (session org-babel-haskell-eoe nil full-body)
                (insert (org-trim full-body))
                (comint-send-input nil t)
                (insert org-babel-haskell-eoe)
                (comint-send-input nil t)))
         (results (mapcar #'org-strip-quotes
			  (cdr (member org-babel-haskell-eoe
                                       (reverse (mapcar #'org-trim raw)))))))
    (org-babel-reassemble-table
     (let ((result
            (pcase result-type
              (`output (mapconcat #'identity (reverse results) "\n"))
              (`value (car results)))))
       (org-babel-result-cond (cdr (assq :result-params params))
	 result (when result (org-babel-script-escape result))))
     (org-babel-pick-name (cdr (assq :colname-names params))
			  (cdr (assq :colname-names params)))
     (org-babel-pick-name (cdr (assq :rowname-names params))
			  (cdr (assq :rowname-names params))))))

(defun org-babel-execute:haskell (body params)
  "Execute a block of Haskell code."
  (let ((compile (string= "yes" (cdr (assq :compile params)))))
    (if (not compile)
	(org-babel-interpret-haskell body params)
      (org-babel-haskell-execute body params))))

(defun org-babel-haskell-initiate-session (&optional _session _params)
  "Initiate a haskell session.
If there is not a current inferior-process-buffer in SESSION
then create one.  Return the initialized session."
  (require 'inf-haskell)
  (or (get-buffer "*haskell*")
      (save-window-excursion (run-haskell) (sleep-for 0.25) (current-buffer))))

(defun org-babel-load-session:haskell (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let* ((buffer (org-babel-prep-session:haskell session params))
           (load-file (concat (org-babel-temp-file "haskell-load-") ".hs")))
      (with-temp-buffer
        (insert body) (write-file load-file)
        (haskell-mode) (inferior-haskell-load-file))
      buffer)))

(defun org-babel-prep-session:haskell (session params)
  "Prepare SESSION according to the header arguments in PARAMS."
  (save-window-excursion
    (let ((buffer (org-babel-haskell-initiate-session session)))
      (org-babel-comint-in-buffer buffer
      	(mapc (lambda (line)
		(insert line)
		(comint-send-input nil t))
	      (org-babel-variable-assignments:haskell params)))
      (current-buffer))))

(defun org-babel-variable-assignments:haskell (params)
  "Return list of haskell statements assigning the block's variables."
  (mapcar (lambda (pair)
	    (format "let %s = %s"
		    (car pair)
		    (org-babel-haskell-var-to-haskell (cdr pair))))
	  (org-babel--get-vars params)))

(defun org-babel-haskell-var-to-haskell (var)
  "Convert an elisp value VAR into a haskell variable.
The elisp VAR is converted to a string of haskell source code
specifying a variable of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-haskell-var-to-haskell var ", ") "]")
    (format "%S" var)))

(defvar org-export-copy-to-kill-ring)
(declare-function org-export-to-file "ox"
		  (backend file
			   &optional async subtreep visible-only body-only
			   ext-plist post-process))
(defun org-babel-haskell-export-to-lhs (&optional arg)
  "Export to a .lhs file with all haskell code blocks escaped.
When called with a prefix argument the resulting
.lhs file will be exported to a .tex file.  This function will
create two new files, base-name.lhs and base-name.tex where
base-name is the name of the current Org file.

Note that all standard Babel literate programming
constructs (header arguments, no-web syntax etc...) are ignored."
  (interactive "P")
  (let* ((contents (buffer-string))
         (haskell-regexp
          (concat "^\\([ \t]*\\)#\\+begin_src[ \t]haskell*\\(.*\\)[\r\n]"
                  "\\([^\000]*?\\)[\r\n][ \t]*#\\+end_src.*"))
         (base-name (file-name-sans-extension (buffer-file-name)))
         (tmp-file (org-babel-temp-file "haskell-"))
         (tmp-org-file (concat tmp-file ".org"))
         (tmp-tex-file (concat tmp-file ".tex"))
         (lhs-file (concat base-name ".lhs"))
         (tex-file (concat base-name ".tex"))
         (command (concat org-babel-haskell-lhs2tex-command
			  " " (org-babel-process-file-name lhs-file)
			  " > " (org-babel-process-file-name tex-file)))
         (preserve-indentp org-src-preserve-indentation)
         indentation)
    ;; escape haskell source-code blocks
    (with-temp-file tmp-org-file
      (insert contents)
      (goto-char (point-min))
      (while (re-search-forward haskell-regexp nil t)
        (save-match-data (setq indentation (length (match-string 1))))
        (replace-match (save-match-data
                         (concat
                          "#+begin_export latex\n\\begin{code}\n"
                          (if (or preserve-indentp
                                  (string-match "-i" (match-string 2)))
                              (match-string 3)
                            (org-remove-indentation (match-string 3)))
                          "\n\\end{code}\n#+end_export\n"))
                       t t)
        (indent-code-rigidly (match-beginning 0) (match-end 0) indentation)))
    (save-excursion
      ;; export to latex w/org and save as .lhs
      (require 'ox-latex)
      (find-file tmp-org-file)
      ;; Ensure we do not clutter kill ring with incomplete results.
      (let (org-export-copy-to-kill-ring)
	(org-export-to-file 'latex tmp-tex-file))
      (kill-buffer nil)
      (delete-file tmp-org-file)
      (find-file tmp-tex-file)
      (goto-char (point-min)) (forward-line 2)
      (insert "%include polycode.fmt\n")
      ;; ensure all \begin/end{code} statements start at the first column
      (while (re-search-forward "^[ \t]+\\\\begin{code}[^\000]+\\\\end{code}" nil t)
        (replace-match (save-match-data (org-remove-indentation (match-string 0)))
                       t t))
      (setq contents (buffer-string))
      (save-buffer) (kill-buffer nil))
    (delete-file tmp-tex-file)
    ;; save org exported latex to a .lhs file
    (with-temp-file lhs-file (insert contents))
    (if (not arg)
        (find-file lhs-file)
      ;; process .lhs file with lhs2tex
      (message "running %s" command) (shell-command command) (find-file tex-file))))

(provide 'ob-haskell)

;;; ob-haskell.el ends here
