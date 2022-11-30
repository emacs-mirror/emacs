;;; ob-clojure.el --- Babel Functions for Clojure    -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2022 Free Software Foundation, Inc.

;; Author: Joel Boehland, Eric Schulte, Oleh Krehel, Frederick Giasson
;; Maintainer: Daniel Kraus <daniel@kraus.my>
;;
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

;; Support for evaluating Clojure code

;; Requirements:

;; - Clojure (at least 1.2.0)
;; - clojure-mode
;; - inf-clojure, Cider, SLIME, babashka or nbb

;; For clojure-mode, see https://github.com/clojure-emacs/clojure-mode
;; For inf-clojure, see https://github.com/clojure-emacs/inf-clojure
;; For Cider, see https://github.com/clojure-emacs/cider
;; For SLIME, see https://slime.common-lisp.dev
;; For babashka, see https://github.com/babashka/babashka
;; For nbb, see https://github.com/babashka/nbb

;; For SLIME, the best way to install its components is by following
;; the directions as set out by Phil Hagelberg (Technomancy) on the
;; web page: https://technomancy.us/126

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ob)

(declare-function cider-current-connection "ext:cider-client" (&optional type))
(declare-function cider-current-ns "ext:cider-client" ())
(declare-function inf-clojure "ext:inf-clojure" (cmd))
(declare-function inf-clojure-cmd "ext:inf-clojure" (project-type))
(declare-function inf-clojure-eval-string "ext:inf-clojure" (code))
(declare-function inf-clojure-project-type "ext:inf-clojure" ())
(declare-function nrepl-dict-get "ext:nrepl-client" (dict key))
(declare-function nrepl-sync-request:eval "ext:nrepl-client" (input connection &optional ns tooling))
(declare-function sesman-start-session "ext:sesman" (system))
(declare-function slime-eval "ext:slime" (sexp &optional package))

(defvar cider-buffer-ns)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))
(add-to-list 'org-babel-tangle-lang-exts '("clojurescript" . "cljs"))

(defvar org-babel-default-header-args:clojure '())
(defvar org-babel-header-args:clojure
  '((ns . :any)
    (package . :any)
    (backend . ((inf-clojure cider slime babashka nbb)))))
(defvar org-babel-default-header-args:clojurescript '())
(defvar org-babel-header-args:clojurescript '((package . :any)))

(defcustom org-babel-clojure-backend (cond
                                      ((executable-find "bb") 'babashka)
                                      ((executable-find "nbb") 'nbb)
                                      ((featurep 'cider) 'cider)
                                      ((featurep 'inf-clojure) 'inf-clojure)
                                      ((featurep 'slime) 'slime)
				      (t nil))
  "Backend used to evaluate Clojure code blocks."
  :group 'org-babel
  :package-version '(Org . "9.6")
  :type '(choice
	  (const :tag "inf-clojure" inf-clojure)
	  (const :tag "cider" cider)
	  (const :tag "slime" slime)
	  (const :tag "babashka" babashka)
	  (const :tag "nbb" nbb)
	  (const :tag "Not configured yet" nil)))

(defcustom org-babel-clojure-default-ns "user"
  "Default Clojure namespace for source block when finding ns failed."
  :type 'string
  :group 'org-babel)

(defcustom ob-clojure-babashka-command (executable-find "bb")
  "Path to the babashka executable."
  :type 'file
  :group 'org-babel
  :package-version '(Org . "9.6"))

(defcustom ob-clojure-nbb-command (executable-find "nbb")
  "Path to the nbb executable."
  :type 'file
  :group 'org-babel
  :package-version '(Org . "9.6"))

(defun org-babel-expand-body:clojure (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let* ((vars (org-babel--get-vars params))
         (backend-override (cdr (assq :backend params)))
         (org-babel-clojure-backend
          (cond
           (backend-override (intern backend-override))
           (org-babel-clojure-backend org-babel-clojure-backend)
           (t (user-error "You need to customize `org-babel-clojure-backend'
or set the `:backend' header argument"))))
	 (ns (or (cdr (assq :ns params))
		 (if (eq org-babel-clojure-backend 'cider)
		     (or cider-buffer-ns
			 (let ((repl-buf (cider-current-connection)))
			   (and repl-buf (buffer-local-value
					  'cider-buffer-ns repl-buf))))
		   org-babel-clojure-default-ns)))
	 (result-params (cdr (assq :result-params params)))
	 (print-level nil)
	 (print-length nil)
	 ;; Remove comments, they break (let [...] ...) bindings
	 (body (replace-regexp-in-string "^[ 	]*;+.*$" "" body))
	 (body (org-trim
		(concat
		 ;; Source block specified namespace :ns.
		 (and (cdr (assq :ns params)) (format "(ns %s)\n" ns))
		 ;; Variables binding.
		 (if (null vars) (org-trim body)
		   (format "(let [%s]\n%s)"
			   (mapconcat
			    (lambda (var)
			      (format "%S '%S" (car var) (cdr var)))
			    vars
			    "\n      ")
			   body))))))
    (if (or (member "code" result-params)
	    (member "pp" result-params))
	(format "(clojure.pprint/pprint (do %s))" body)
      body)))

(defvar ob-clojure-inf-clojure-filter-out)
(defvar ob-clojure-inf-clojure-tmp-output)
(defun ob-clojure-inf-clojure-output (s)
  "Store a trimmed version of S in a variable and return S."
  (let ((s0 (org-trim
	     (replace-regexp-in-string
	      ob-clojure-inf-clojure-filter-out "" s))))
    (push s0 ob-clojure-inf-clojure-tmp-output))
  s)

(defmacro ob-clojure-with-temp-expanded (expanded params &rest body)
  "Run BODY on EXPANDED code block with PARAMS."
  (declare (debug (body)) (indent 2))
  `(with-temp-buffer
     (insert ,expanded)
     (goto-char (point-min))
     (while (not (looking-at "\\s-*\\'"))
       (let* ((beg (point))
	      (end (progn (forward-sexp) (point)))
	      (exp (org-babel-expand-body:clojure
		    (buffer-substring beg end) ,params)))
	 (sit-for .1)
	 ,@body))))

(defsubst ob-clojure-string-or-list (l)
  "Convert list L into a string or a list of list."
  (if (and (listp l) (= (length l) 1))
      (car l)
    (mapcar #'list l)))

(defvar inf-clojure-buffer)
(defvar comint-prompt-regexp)
(defvar inf-clojure-comint-prompt-regexp)
(defun ob-clojure-eval-with-inf-clojure (expanded params)
  "Evaluate EXPANDED code block with PARAMS using inf-clojure."
  (condition-case nil (require 'inf-clojure)
    (user-error "inf-clojure not available"))
  ;; Maybe initiate the inf-clojure session
  (unless (and inf-clojure-buffer
	       (buffer-live-p (get-buffer inf-clojure-buffer)))
    (save-window-excursion
      (let* ((alias (cdr (assq :alias params)))
	     (cmd0 (inf-clojure-cmd (inf-clojure-project-type)))
	     (cmd (if alias (replace-regexp-in-string
			     "clojure" (format "clojure -A%s" alias)
			     cmd0)
		    cmd0)))
	(setq comint-prompt-regexp inf-clojure-comint-prompt-regexp)
	(funcall-interactively #'inf-clojure cmd)
	(goto-char (point-max))))
    (sit-for 1))
  ;; Now evaluate the code
  (setq ob-clojure-inf-clojure-filter-out
	(concat "^nil\\|nil$\\|\\s-*"
		(or (cdr (assq :ns params))
		    org-babel-clojure-default-ns)
		"=>\\s-*"))
  (add-hook 'comint-preoutput-filter-functions
	    #'ob-clojure-inf-clojure-output)
  (setq ob-clojure-inf-clojure-tmp-output nil)
  (ob-clojure-with-temp-expanded expanded nil
    (inf-clojure-eval-string exp))
  (sit-for .5)
  (remove-hook 'comint-preoutput-filter-functions
	       #'ob-clojure-inf-clojure-output)
  ;; And return the result
  (ob-clojure-string-or-list
   (delete nil
	   (mapcar
	    (lambda (s)
	      (unless (or (equal "" s)
			  (string-match-p "^Clojure" s))
		s))
	    (reverse ob-clojure-inf-clojure-tmp-output)))))

(defun ob-clojure-eval-with-cider (expanded params)
  "Evaluate EXPANDED code block with PARAMS using cider."
  (condition-case nil (require 'cider)
    (user-error "cider not available"))
  (let ((connection (cider-current-connection (cdr (assq :target params))))
	(result-params (cdr (assq :result-params params)))
	result0)
    (unless connection (sesman-start-session 'CIDER))
    (if (not connection)
	;; Display in the result instead of using `user-error'
	(setq result0 "Please reevaluate when nREPL is connected")
      (ob-clojure-with-temp-expanded expanded params
	(let ((response (nrepl-sync-request:eval exp connection)))
	  (push (or (nrepl-dict-get response "root-ex")
		    (nrepl-dict-get response "ex")
		    (nrepl-dict-get
		     response (if (or (member "output" result-params)
				      (member "pp" result-params))
				  "out"
				"value")))
		result0)))
      (ob-clojure-string-or-list
       ;; Filter out s-expressions that return `nil' (string "nil"
       ;; from nrepl eval) or comment forms (actual `nil' from nrepl)
       (reverse (delete "" (mapcar (lambda (r)
				     (replace-regexp-in-string "nil" "" (or r "")))
				   result0)))))))

(defun ob-clojure-eval-with-slime (expanded params)
  "Evaluate EXPANDED code block with PARAMS using slime."
  (condition-case nil (require 'slime)
    (user-error "slime not available"))
  (with-temp-buffer
    (insert expanded)
    (slime-eval
     `(swank:eval-and-grab-output
       ,(buffer-substring-no-properties (point-min) (point-max)))
     (cdr (assq :package params)))))

(defun ob-clojure-eval-with-babashka (bb expanded)
  "Evaluate EXPANDED code block using BB (babashka or nbb)."
  (let ((script-file (org-babel-temp-file "clojure-bb-script-" ".clj")))
    (with-temp-file script-file
      (insert expanded))
    (org-babel-eval
     (format "%s %s" bb (org-babel-process-file-name script-file))
     "")))

(defun org-babel-execute:clojure (body params)
  "Execute the BODY block of Clojure code with PARAMS using Babel."
  (let* ((backend-override (cdr (assq :backend params)))
         (org-babel-clojure-backend
          (cond
           (backend-override (intern backend-override))
           (org-babel-clojure-backend org-babel-clojure-backend)
           (t (user-error "You need to customize `org-babel-clojure-backend'
or set the `:backend' header argument")))))
    (let* ((expanded (org-babel-expand-body:clojure body params))
	   (result-params (cdr (assq :result-params params)))
	   result)
      (setq result
	    (cond
	     ((eq org-babel-clojure-backend 'inf-clojure)
	      (ob-clojure-eval-with-inf-clojure expanded params))
             ((eq org-babel-clojure-backend 'babashka)
	      (ob-clojure-eval-with-babashka ob-clojure-babashka-command expanded))
             ((eq org-babel-clojure-backend 'nbb)
	      (ob-clojure-eval-with-babashka ob-clojure-nbb-command expanded))
	     ((eq org-babel-clojure-backend 'cider)
	      (ob-clojure-eval-with-cider expanded params))
	     ((eq org-babel-clojure-backend 'slime)
	      (ob-clojure-eval-with-slime expanded params))))
      (org-babel-result-cond result-params
        result
        (condition-case nil (org-babel-script-escape result)
	  (error result))))))

(defun org-babel-execute:clojurescript (body params)
  "Evaluate BODY with PARAMS as ClojureScript code."
  (org-babel-execute:clojure body (cons '(:target . "cljs") params)))

(provide 'ob-clojure)

;;; ob-clojure.el ends here
