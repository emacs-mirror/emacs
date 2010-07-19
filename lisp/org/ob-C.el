;;; ob-C.el --- org-babel functions for C and similar languages

;; Copyright (C) 2010  Free Software Foundation, Inc.

;; Author: Eric Schulte
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

;; Org-Babel support for evaluating C code.
;;
;; very limited implementation:
;; - currently only support :results output
;; - not much in the way of error feedback

;;; Code:
(require 'ob)
(require 'ob-eval)
(require 'org)
(require 'cc-mode)

(declare-function org-entry-get "org"
		  (pom property &optional inherit literal-nil))

(add-to-list 'org-babel-tangle-lang-exts '("c++" . "cpp"))

(defvar org-babel-default-header-args:C '())

(defvar org-babel-C-compiler "gcc"
  "Command used to compile a C source code file into an
  executable.")

(defvar org-babel-c++-compiler "g++"
  "Command used to compile a c++ source code file into an
  executable.")

(defvar org-babel-c-variant nil
  "Internal variable used to hold which type of C (e.g. C or C++)
is currently being evaluated.")

(defun org-babel-execute:cpp (body params)
  "Execute BODY according to PARAMS.  This function calls
`org-babel-execute:C'."
  (org-babel-execute:C body params))

(defun org-babel-execute:c++ (body params)
    "Execute a block of C++ code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (let ((org-babel-c-variant 'cpp)) (org-babel-C-execute body params)))

(defun org-babel-expand-body:c++ (body params &optional processed-params)
  "Expand a block of C++ code with org-babel according to it's
header arguments (calls `org-babel-C-expand')."
  (let ((org-babel-c-variant 'cpp)) (org-babel-C-expand body params processed-params)))

(defun org-babel-execute:C (body params)
  "Execute a block of C code with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (let ((org-babel-c-variant 'c)) (org-babel-C-execute body params)))

(defun org-babel-expand-body:c (body params &optional processed-params)
  "Expand a block of C code with org-babel according to it's
header arguments (calls `org-babel-C-expand')."
  (let ((org-babel-c-variant 'c)) (org-babel-C-expand body params processed-params)))

(defun org-babel-C-execute (body params)
  "This function should only be called by `org-babel-execute:C'
or `org-babel-execute:c++'."
  (let* ((processed-params (org-babel-process-params params))
         (tmp-src-file (make-temp-file "org-babel-C-src" nil
                                       (cond
					((equal org-babel-c-variant 'c) ".c")
					((equal org-babel-c-variant 'cpp) ".cpp"))))
         (tmp-bin-file (make-temp-file "org-babel-C-bin"))
         (tmp-out-file (make-temp-file "org-babel-C-out"))
         (cmdline (cdr (assoc :cmdline params)))
         (flags (cdr (assoc :flags params)))
         (full-body (org-babel-C-expand body params))
         (compile
	  (progn
	    (with-temp-file tmp-src-file (insert full-body))
	    (org-babel-eval
	     (format "%s -o %s %s %s"
		     (cond
		      ((equal org-babel-c-variant 'c) org-babel-C-compiler)
		      ((equal org-babel-c-variant 'cpp) org-babel-c++-compiler))
		     tmp-bin-file
		     (mapconcat 'identity
				(if (listp flags) flags (list flags)) " ")
		     tmp-src-file) ""))))
    ((lambda (results)
       (org-babel-reassemble-table
	(if (member "vector" (nth 2 processed-params))
	    (let ((tmp-file (make-temp-file "ob-c")))
	      (with-temp-file tmp-file (insert results))
	      (org-babel-import-elisp-from-file tmp-file))
	  (org-babel-read results))
	(org-babel-pick-name
	 (nth 4 processed-params) (cdr (assoc :colnames params)))
	(org-babel-pick-name
	 (nth 5 processed-params) (cdr (assoc :rownames params)))))
     (org-babel-trim
       (org-babel-eval
	(concat tmp-bin-file (if cmdline (concat " " cmdline) "")) "")))))

(defun org-babel-C-expand (body params &optional processed-params)
  "Expand a block of C or C++ code with org-babel according to
it's header arguments."
  (let ((vars (nth 1 (or processed-params
                          (org-babel-process-params params))))
        (main-p (not (string= (cdr (assoc :main params)) "no")))
        (includes (or (cdr (assoc :includes params))
                      (org-babel-read (org-entry-get nil "includes" t))))
        (defines (org-babel-read
                  (or (cdr (assoc :defines params))
                      (org-babel-read (org-entry-get nil "defines" t))))))
    (org-babel-trim
     (mapconcat 'identity
		(list
		 ;; includes
		 (mapconcat
		  (lambda (inc) (format "#include %s" inc))
		  (if (listp includes) includes (list includes)) "\n")
		 ;; defines
		 (mapconcat
		  (lambda (inc) (format "#define %s" inc))
		  (if (listp defines) defines (list defines)) "\n")
		 ;; variables
		 (mapconcat 'org-babel-C-var-to-C vars "\n")
		 ;; body
		 (if main-p
		     (org-babel-C-ensure-main-wrap body)
		   body) "\n") "\n"))))

(defun org-babel-C-ensure-main-wrap (body)
  "Wrap body in a \"main\" function call if none exists."
  (if (string-match "^[ \t]*[intvod]+[ \t]*main[ \t]*(.*)" body)
      body
    (format "int main() {\n%s\n}\n" body)))

(defun org-babel-prep-session:C (session params)
  "This function does nothing as C is a compiled language with no
support for sessions"
  (error "C is a compiled languages -- no support for sessions"))

(defun org-babel-load-session:C (session body params)
  "This function does nothing as C is a compiled language with no
support for sessions"
  (error "C is a compiled languages -- no support for sessions"))

;; helper functions

(defun org-babel-C-var-to-C (pair)
  "Convert an elisp val into a string of C code specifying a var
of the same value."
  ;; TODO list support
  (let ((var (car pair))
        (val (cdr pair)))
    (when (symbolp val)
      (setq val (symbol-name val))
      (when (= (length val) 1)
        (setq val (string-to-char val))))
    (cond
     ((integerp val)
      (format "int %S = %S;" var val))
     ((floatp val)
      (format "double %S = %S;" var val))
     ((or (characterp val))
      (format "char %S = '%S';" var val))
     ((stringp val)
      (format "char %S[%d] = \"%s\";"
              var (+ 1 (length val)) val))
     (t
      (format "u32 %S = %S;" var val)))))


(provide 'ob-C)

;; arch-tag: 8f49e462-54e3-417b-9a8d-423864893b37

;;; ob-C.el ends here
