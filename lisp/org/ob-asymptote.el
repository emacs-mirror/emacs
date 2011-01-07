;;; ob-asymptote.el --- org-babel functions for asymptote evaluation

;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.

;; Author: Eric Schulte
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

;; Org-Babel support for evaluating asymptote source code.
;;
;; This differs from most standard languages in that
;;
;; 1) there is no such thing as a "session" in asymptote
;;
;; 2) we are generally only going to return results of type "file"
;;
;; 3) we are adding the "file" and "cmdline" header arguments, if file
;;    is omitted then the -V option is passed to the asy command for
;;    interactive viewing

;;; Requirements:

;; - The asymptote program :: http://asymptote.sourceforge.net/
;;
;; - asy-mode :: Major mode for editing asymptote files

;;; Code:
(require 'ob)
(eval-when-compile (require 'cl))

(declare-function orgtbl-to-generic "org-table" (table params))
(declare-function org-combine-plists "org" (&rest plists))

(add-to-list 'org-babel-tangle-lang-exts '("asymptote" . "asy"))

(defvar org-babel-default-header-args:asymptote
  '((:results . "file") (:exports . "results"))
  "Default arguments when evaluating an Asymptote source block.")

(defun org-babel-execute:asymptote (body params)
  "Execute a block of Asymptote code.
This function is called by `org-babel-execute-src-block'."
  (let* ((result-params (split-string (or (cdr (assoc :results params)) "")))
         (out-file (cdr (assoc :file params)))
         (format (or (and out-file
                          (string-match ".+\\.\\(.+\\)" out-file)
                          (match-string 1 out-file))
                     "pdf"))
         (cmdline (cdr (assoc :cmdline params)))
         (in-file (org-babel-temp-file "asymptote-"))
         (cmd
	  (concat "asy "
		  (if out-file
		      (concat
		       "-globalwrite -f " format
		       " -o " (org-babel-process-file-name out-file))
		    "-V")
		  " " cmdline
		  " " (org-babel-process-file-name in-file))))
    (with-temp-file in-file
      (insert (org-babel-expand-body:generic
	       body params
	       (org-babel-variable-assignments:asymptote params))))
    (message cmd) (shell-command cmd)
    out-file))

(defun org-babel-prep-session:asymptote (session params)
  "Return an error if the :session header argument is set.
Asymptote does not support sessions"
  (error "Asymptote does not support sessions"))

(defun org-babel-variable-assignments:asymptote (params)
  "Return list of asymptote statements assigning the block's variables"
  (mapcar #'org-babel-asymptote-var-to-asymptote
	  (mapcar #'cdr (org-babel-get-header params :var))))

(defun org-babel-asymptote-var-to-asymptote (pair)
  "Convert an elisp value into an Asymptote variable.
The elisp value PAIR is converted into Asymptote code specifying
a variable of the same value."
  (let ((var (car pair))
        (val (if (symbolp (cdr pair))
                 (symbol-name (cdr pair))
               (cdr pair))))
    (cond
     ((integerp val)
      (format "int %S=%S;" var val))
     ((floatp val)
      (format "real %S=%S;" var val))
     ((stringp val)
      (format "string %S=\"%s\";" var val))
     ((listp val)
      (let* ((dimension-2-p (not (null (cdr val))))
             (dim (if dimension-2-p "[][]" "[]"))
             (type (org-babel-asymptote-define-type val))
             (array (org-babel-asymptote-table-to-array
                     val
                     (if dimension-2-p '(:lstart "{" :lend "}," :llend "}")))))
        (format "%S%s %S=%s;" type dim var array))))))

(defun org-babel-asymptote-table-to-array (table params)
  "Convert values of an elisp table into a string of an asymptote array.
Empty cells are ignored."
  (labels ((atom-to-string (table)
                           (cond
                            ((null table) '())
                            ((not (listp (car table)))
                             (cons (if (and (stringp (car table))
                                            (not (string= (car table) "")))
                                       (format "\"%s\"" (car table))
                                     (format "%s" (car table)))
                                   (atom-to-string (cdr table))))
                            (t
                             (cons (atom-to-string (car table))
                                   (atom-to-string (cdr table))))))
           ;; Remove any empty row
           (fix-empty-lines (table)
                            (delq nil (mapcar (lambda (l) (delq "" l)) table))))
    (orgtbl-to-generic
     (fix-empty-lines (atom-to-string table))
     (org-combine-plists '(:hline nil :sep "," :tstart "{" :tend "}") params))))

(defun org-babel-asymptote-define-type (data)
  "Determine type of DATA.
DATA is a list. Type symbol is returned as 'symbol. The type is
usually the type of the first atom encountered, except for arrays
of int, where every cell must be of int type."
  (labels ((anything-but-int (el)
                             (cond
                              ((null el) nil)
                              ((not (listp (car el)))
                               (cond
                                ((floatp (car el)) 'real)
                                ((stringp (car el)) 'string)
                                (t
                                 (anything-but-int (cdr el)))))
                              (t
                               (or (anything-but-int (car el))
                                   (anything-but-int (cdr el)))))))
    (or (anything-but-int data) 'int)))

(provide 'ob-asymptote)

;; arch-tag: f2f5bd0d-78e8-412b-8e6c-6dadc94cc06b

;;; ob-asymptote.el ends here
