;;; ob-clojure.el --- org-babel functions for clojure evaluation

;; Copyright (C) 2009, 2010  Free Software Foundation, Inc.

;; Author: Joel Boehland
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 7.3

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

;;; ob support for evaluating clojure code

;;; Requirements:

;;; A working clojure install. This also implies a working java executable
;;; clojure-mode
;;; slime
;;; swank-clojure

;;; By far, the best way to install these components is by following
;;; the directions as set out by Phil Hagelberg (Technomancy) on the
;;; web page: http://technomancy.us/126

;;; Code:
(require 'ob)
(require 'ob-eval)
(eval-when-compile (require 'cl))

(declare-function slime-eval-async "ext:slime" (sexp &optional cont package))
(declare-function slime-eval "ext:slime" (sexp &optional package))
(declare-function swank-clojure-concat-paths "ext:slime" (paths))
(declare-function slime "ext:slime" (&optional command coding-system))
(declare-function slime-output-buffer "ext:slime" (&optional noprompt))
(declare-function slime-filter-buffers "ext:slime" (predicate))

(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

(defvar org-babel-default-header-args:clojure '())

(defvar org-babel-clojure-wrapper-method
  "
(defn spit
  [f content]
  (with-open [#^java.io.PrintWriter w
                 (java.io.PrintWriter.
                   (java.io.BufferedWriter.
                     (java.io.OutputStreamWriter.
                       (java.io.FileOutputStream.
                         (java.io.File. f)))))]
      (.print w content)))

(defn main
  []
  %s)

(spit \"%s\" (str (main)))")
;;";; <-- syntax highlighting is messed without this double quote

;;taken mostly from clojure-test-mode.el
(defun org-babel-clojure-clojure-slime-eval (string &optional handler)
  "Evaluate a STRING of clojure code using `slime-eval-async'."
  (slime-eval-async `(swank:eval-and-grab-output ,string)
                    (or handler #'identity)))

(defun org-babel-clojure-slime-eval-sync (string)
  "Evaluate a STRING of clojure code using `slime-eval'."
  (slime-eval `(swank:eval-and-grab-output ,string)))

;;taken from swank-clojure.el
(defvar swank-clojure-binary)
(defvar swank-clojure-classpath)
(defvar swank-clojure-java-path)
(defvar swank-clojure-extra-vm-args)
(defvar swank-clojure-library-paths)
(defvar swank-clojure-extra-classpaths)
(defun org-babel-clojure-babel-clojure-cmd ()
  "Create the command to start clojure according to current settings."
  (or (when swank-clojure-binary
	(if (listp swank-clojure-binary)
	    swank-clojure-binary
	  (list swank-clojure-binary)))
      (when swank-clojure-classpath
	(delq
	 nil
	 (append
	  (list swank-clojure-java-path)
	  swank-clojure-extra-vm-args
	  (list
	   (when swank-clojure-library-paths
	     (concat "-Djava.library.path="
		     (swank-clojure-concat-paths swank-clojure-library-paths)))
	   "-classpath"
	   (swank-clojure-concat-paths
	    (append
	     swank-clojure-classpath
	     swank-clojure-extra-classpaths))
	   "clojure.main"))))
      (error "%s" (concat "You must specifiy either a `swank-clojure-binary' "
			  "or a `swank-clojure-classpath'"))))

(defun org-babel-clojure-table-or-string (results)
  "Convert RESULTS to an elisp value.
If RESULTS looks like a table, then convert to an Emacs-lisp
table, otherwise return the results as a string."
  (org-babel-read
   (if (string-match "^\\[.+\\]$" results)
       (org-babel-read
        (concat "'"
                (replace-regexp-in-string
                 "\\[" "(" (replace-regexp-in-string
                            "\\]" ")" (replace-regexp-in-string
                                       ", " " " (replace-regexp-in-string
                                                 "'" "\"" results))))))
     results)))

(defun org-babel-clojure-var-to-clojure (var)
  "Convert an elisp value into a clojure variable.
The elisp value VAR is converted into a string of clojure source
code specifying a variable of the same value."
  (if (listp var)
      (format "'%s" var)
    (format "%S" var)))

(defun org-babel-clojure-build-full-form (body vars)
  "Construct a clojure let form with VARS as the let variables."
  (let ((vars-forms
	 (mapconcat ;; define any variables
	  (lambda (pair)
	    (format "%s %s"
		    (car pair) (org-babel-clojure-var-to-clojure (cdr pair))))
	  vars "\n      "))
	(body (org-babel-trim body)))
    (if (> (length vars-forms) 0)
	(format "(let [%s]\n  %s)" vars-forms body)
      body)))

(defun org-babel-prep-session:clojure (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (require 'slime) (require 'swank-clojure)
  (let* ((session-buf (org-babel-clojure-initiate-session session))
         (vars (mapcar #'cdr (org-babel-get-header params :var)))
         (var-lines (mapcar ;; define any top level session variables
                     (lambda (pair)
                       (format "(def %s %s)\n" (car pair)
                               (org-babel-clojure-var-to-clojure (cdr pair))))
                     vars)))
    session-buf))

(defun org-babel-load-session:clojure (session body params)
  "Load BODY into SESSION."
  (require 'slime) (require 'swank-clojure)
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:clojure session params)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert (org-babel-chomp body)))
      buffer)))

(defvar org-babel-clojure-buffers '())
(defvar org-babel-clojure-pending-sessions '())

(defun org-babel-clojure-session-buffer (session)
  "Return the buffer associated with SESSION."
  (cdr (assoc session org-babel-clojure-buffers)))

(defun org-babel-clojure-initiate-session-by-key (&optional session)
  "Initiate a clojure session in an inferior-process-buffer.
If there is not a current inferior-process-buffer in SESSION
then create one.  Return the initialized session."
  (save-window-excursion
    (let* ((session (if session
                        (if (stringp session) (intern session)
                          session)
		      :default))
           (clojure-buffer (org-babel-clojure-session-buffer session)))
      (unless (and clojure-buffer (buffer-live-p clojure-buffer))
        (setq org-babel-clojure-buffers
	      (assq-delete-all session org-babel-clojure-buffers))
        (push session org-babel-clojure-pending-sessions)
        (slime)
        ;; we are waiting to finish setting up which will be done in
        ;; org-babel-clojure-session-connected-hook below.
        (let ((timeout 9))
          (while (and (not (org-babel-clojure-session-buffer session))
                      (< 0 timeout))
            (message "Waiting for clojure repl for session: %s ... %i"
		     session timeout)
            (sit-for 1)
            (decf timeout)))
        (setq org-babel-clojure-pending-sessions
              (remove session org-babel-clojure-pending-sessions))
        (unless (org-babel-clojure-session-buffer session)
          (error "Couldn't create slime clojure process"))
        (setq clojure-buffer (org-babel-clojure-session-buffer session)))
      session)))

(defun org-babel-clojure-initiate-session (&optional session params)
  "Return the slime-clojure repl buffer bound to SESSION.
Returns nil if \"none\" is specified."
  (require 'slime) (require 'swank-clojure)
  (unless (and (stringp session) (string= session "none"))
    (org-babel-clojure-session-buffer
     (org-babel-clojure-initiate-session-by-key session))))

(defun org-babel-clojure-session-connected-hook ()
  "Finish  binding an org-babel session to a slime-clojure repl."
  (let ((pending-session (pop org-babel-clojure-pending-sessions)))
    (when pending-session
      (save-excursion
        (switch-to-buffer (slime-output-buffer))
        (rename-buffer
	 (if (stringp pending-session)
	     pending-session (symbol-name pending-session)))
        (org-babel-clojure-bind-session-to-repl-buffer
	 pending-session (slime-output-buffer))))))

(add-hook 'slime-connected-hook 'org-babel-clojure-session-connected-hook)

(defun org-babel-clojure-bind-session-to-repl-buffer (session repl-buffer)
  "Associate SESSION with REPL-BUFFER."
  (when (stringp session) (setq session (intern session)))
  (setq org-babel-clojure-buffers
        (cons (cons session repl-buffer)
              (assq-delete-all session org-babel-clojure-buffers))))

(defun org-babel-clojure-repl-buffer-pred ()
  "Test whether the current buffer is an active slime-clojure
repl buffer."
  (and (buffer-live-p (current-buffer)) (eq major-mode 'slime-repl-mode)))

(defun org-babel-clojure-bind-session-to-repl (session)
  "Bind SESSION to a clojure repl."
  (interactive "sEnter session name: ")
  (let ((repl-bufs (slime-filter-buffers 'org-babel-clojure-repl-buffer-pred)))
    (unless repl-bufs (error "No existing slime-clojure repl buffers exist"))
    (let ((repl-buf (read-buffer "Choose slime-clojure repl: " repl-bufs t)))
      (org-babel-clojure-bind-session-to-repl-buffer session repl-buf))))

(defun org-babel-clojure-evaluate-external-process
  (buffer body &optional result-type)
  "Evaluate the body in an external process."
  (let ((cmd (format "%s -" (mapconcat #'identity
				       (org-babel-clojure-babel-clojure-cmd)
				       " "))))
    (case result-type
      (output (org-babel-eval cmd body))
      (value (let* ((tmp-file (org-babel-temp-file "clojure-")))
	       (org-babel-eval
		cmd
		(format
		 org-babel-clojure-wrapper-method
		 body
		 (org-babel-process-file-name tmp-file 'noquote)))
	       (org-babel-clojure-table-or-string
		(org-babel-eval-read-file tmp-file)))))))

(defun org-babel-clojure-evaluate-session (buffer body &optional result-type)
  "Evaluate the body in the context of a clojure session."
  (require 'slime) (require 'swank-clojure)
  (let ((raw nil)
        (results nil))
    (with-current-buffer buffer
      (setq raw (org-babel-clojure-slime-eval-sync body))
      (setq results (reverse (mapcar #'org-babel-trim raw)))
      (cond
       ((equal result-type 'output)
	(mapconcat #'identity (reverse (cdr results)) "\n"))
       ((equal result-type 'value)
	(org-babel-clojure-table-or-string (car results)))))))

(defun org-babel-clojure-evaluate (buffer body &optional result-type)
  "Pass BODY to the Clojure process in BUFFER.
If RESULT-TYPE equals 'output then return a list of the outputs
of the statements in BODY, if RESULT-TYPE equals 'value then
return the value of the last statement in BODY as elisp."
  (if buffer
      (org-babel-clojure-evaluate-session buffer body result-type)
    (org-babel-clojure-evaluate-external-process buffer body result-type)))

(defun org-babel-expand-body:clojure (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (org-babel-clojure-build-full-form
   body (mapcar #'cdr (org-babel-get-header params :var))))

(defun org-babel-execute:clojure (body params)
  "Execute a block of Clojure code."
  (require 'slime) (require 'swank-clojure)
  (let* ((body (org-babel-expand-body:clojure body params))
         (session (org-babel-clojure-initiate-session
		   (cdr (assoc :session params)))))
    (org-babel-reassemble-table
     (org-babel-clojure-evaluate session body (cdr (assoc :result-type params)))
     (org-babel-pick-name
      (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
     (org-babel-pick-name
      (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))

(provide 'ob-clojure)

;; arch-tag: a43b33f2-653e-46b1-ac56-2805cf05b7d1

;;; ob-clojure.el ends here
