;;; pp.el --- pretty printer for Emacs Lisp  -*- lexical-binding: t -*-

;; Copyright (C) 1989, 1993, 2001-2024 Free Software Foundation, Inc.

;; Author: Randal Schwartz <merlyn@stonehenge.com>
;; Keywords: lisp

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

;;; Code:

(require 'cl-lib)
(defvar font-lock-verbose)

(defgroup pp nil
  "Pretty printer for Emacs Lisp."
  :prefix "pp-"
  :group 'lisp)

(defcustom pp-escape-newlines t
  "Value of `print-escape-newlines' used by pp-* functions."
  :type 'boolean)

(defcustom pp-max-width t
  "Max width to use when formatting.
If nil, there's no max width.  If t, use the window width.
Otherwise this should be a number."
  :type '(choice (const :tag "none" nil)
                 (const :tag "window width" t)
                 number)
  :version "29.1")

(defcustom pp-use-max-width nil
  "If non-nil, `pp'-related functions will try to fold lines.
The target width is given by the `pp-max-width' variable.
Note that this could slow down `pp' considerably when formatting
large lists."
  :type 'boolean
  :version "29.1")

(defvar pp--inhibit-function-formatting nil)

;;;###autoload
(defun pp-to-string (object)
  "Return a string containing the pretty-printed representation of OBJECT.
OBJECT can be any Lisp object.  Quoting characters are used as needed
to make output that `read' can handle, whenever this is possible."
  (if pp-use-max-width
      (let ((pp--inhibit-function-formatting t))
        (with-temp-buffer
          (pp-emacs-lisp-code object)
          (buffer-string)))
    (with-temp-buffer
      (lisp-mode-variables nil)
      (set-syntax-table emacs-lisp-mode-syntax-table)
      (let ((print-escape-newlines pp-escape-newlines)
            (print-quoted t))
        (prin1 object (current-buffer)))
      (pp-buffer)
      (buffer-string))))

;;;###autoload
(defun pp-buffer ()
  "Prettify the current buffer with printed representation of a Lisp object."
  (interactive)
  (goto-char (point-min))
  (while (not (eobp))
    (cond
     ((ignore-errors (down-list 1) t)
      (save-excursion
        (backward-char 1)
        (skip-chars-backward "'`#^")
        (when (and (not (bobp)) (memq (char-before) '(?\s ?\t ?\n)))
          (delete-region
           (point)
           (progn (skip-chars-backward " \t\n") (point)))
          (insert "\n"))))
     ((ignore-errors (up-list 1) t)
      (skip-syntax-forward ")")
      (delete-region
       (point)
       (progn (skip-chars-forward " \t\n") (point)))
      (insert ?\n))
     (t (goto-char (point-max)))))
  (goto-char (point-min))
  (indent-sexp))

;;;###autoload
(defun pp (object &optional stream)
  "Output the pretty-printed representation of OBJECT, any Lisp object.
Quoting characters are printed as needed to make output that `read'
can handle, whenever this is possible.

This function does not apply special formatting rules for Emacs
Lisp code.  See `pp-emacs-lisp-code' instead.

By default, this function won't limit the line length of lists
and vectors.  Bind `pp-use-max-width' to a non-nil value to do so.

Output stream is STREAM, or value of `standard-output' (which see)."
  (princ (pp-to-string object) (or stream standard-output)))

;;;###autoload
(defun pp-display-expression (expression out-buffer-name &optional lisp)
  "Prettify and display EXPRESSION in an appropriate way, depending on length.
If LISP, format with `pp-emacs-lisp-code'; use `pp' otherwise.

If a temporary buffer is needed for representation, it will be named
after OUT-BUFFER-NAME."
  (let* ((old-show-function temp-buffer-show-function)
	 ;; Use this function to display the buffer.
	 ;; This function either decides not to display it at all
	 ;; or displays it in the usual way.
	 (temp-buffer-show-function
          (lambda (buf)
            (with-current-buffer buf
              (goto-char (point-min))
              (end-of-line 1)
              (if (or (< (1+ (point)) (point-max))
                      (>= (- (point) (point-min)) (frame-width)))
                  (let ((temp-buffer-show-function old-show-function)
                        (old-selected (selected-window))
                        (window (display-buffer buf)))
                    (goto-char (point-min)) ; expected by some hooks ...
                    (make-frame-visible (window-frame window))
                    (unwind-protect
                        (progn
                          (select-window window)
                          (run-hooks 'temp-buffer-show-hook))
                      (when (window-live-p old-selected)
                        (select-window old-selected))))
                (message "%s" (buffer-substring (point-min) (point))))))))
    (with-output-to-temp-buffer out-buffer-name
      (if lisp
          (with-current-buffer standard-output
            (pp-emacs-lisp-code expression))
        (pp expression))
      (with-current-buffer standard-output
	(emacs-lisp-mode)
	(setq buffer-read-only nil)
        (setq-local font-lock-verbose nil)))))

;;;###autoload
(defun pp-eval-expression (expression)
  "Evaluate EXPRESSION and pretty-print its value.
Also add the value to the front of the list in the variable `values'."
  (interactive
   (list (read--expression "Eval: ")))
  (message "Evaluating...")
  (let ((result (eval expression lexical-binding)))
    (values--store-value result)
    (pp-display-expression result "*Pp Eval Output*" pp-use-max-width)))

;;;###autoload
(defun pp-macroexpand-expression (expression)
  "Macroexpand EXPRESSION and pretty-print its value."
  (interactive
   (list (read--expression "Macroexpand: ")))
  (pp-display-expression (macroexpand-1 expression) "*Pp Macroexpand Output*"
                         pp-use-max-width))

(defun pp-last-sexp ()
  "Read sexp before point.  Ignore leading comment characters."
  (with-syntax-table emacs-lisp-mode-syntax-table
    (let ((pt (point)))
      (save-excursion
        (forward-sexp -1)
        ;; Make `pp-eval-last-sexp' work the same way `eval-last-sexp'
        ;; does.
        (when (looking-at ",@?")
          (goto-char (match-end 0)))
        (read
         ;; If first line is commented, ignore all leading comments:
         (if (save-excursion (beginning-of-line) (looking-at-p "[ \t]*;"))
             (let ((exp (buffer-substring (point) pt))
                   (start nil))
               (while (string-match "\n[ \t]*;+" exp start)
                 (setq start (1+ (match-beginning 0))
                       exp (concat (substring exp 0 start)
                                   (substring exp (match-end 0)))))
               exp)
           (current-buffer)))))))

;;;###autoload
(defun pp-eval-last-sexp (arg)
  "Run `pp-eval-expression' on sexp before point.
With ARG, pretty-print output into current buffer.
Ignores leading comment characters."
  (interactive "P")
  (if arg
      (insert (pp-to-string (eval (elisp--eval-defun-1
                                   (macroexpand (pp-last-sexp)))
                                  lexical-binding)))
    (pp-eval-expression (elisp--eval-defun-1
                         (macroexpand (pp-last-sexp))))))

;;;###autoload
(defun pp-macroexpand-last-sexp (arg)
  "Run `pp-macroexpand-expression' on sexp before point.
With ARG, pretty-print output into current buffer.
Ignores leading comment characters."
  (interactive "P")
  (if arg
      (insert (pp-to-string (macroexpand-1 (pp-last-sexp))))
    (pp-macroexpand-expression (pp-last-sexp))))

;;;###autoload
(defun pp-emacs-lisp-code (sexp)
  "Insert SEXP into the current buffer, formatted as Emacs Lisp code.
Use the `pp-max-width' variable to control the desired line length.
Note that this could be slow for large SEXPs."
  (require 'edebug)
  (let ((obuf (current-buffer)))
    (with-temp-buffer
      (emacs-lisp-mode)
      (pp--insert-lisp sexp)
      (insert "\n")
      (goto-char (point-min))
      (indent-sexp)
      (while (re-search-forward " +$" nil t)
        (replace-match ""))
      (insert-into-buffer obuf))))

(defun pp--insert-lisp (sexp)
  (cl-case (type-of sexp)
    (vector (pp--format-vector sexp))
    (cons (cond
           ((consp (cdr sexp))
            (if (and (length= sexp 2)
                     (memq (car sexp) '(quote function)))
                (cond
                 ((symbolp (cadr sexp))
                  (let ((print-quoted t))
                    (prin1 sexp (current-buffer))))
                 ((consp (cadr sexp))
                  (insert (if (eq (car sexp) 'quote)
                              "'" "#'"))
                  (pp--format-list (cadr sexp)
                                   (set-marker (make-marker) (1- (point))))))
              (pp--format-list sexp)))
           (t
            (prin1 sexp (current-buffer)))))
    ;; Print some of the smaller integers as characters, perhaps?
    (integer
     (if (<= ?0 sexp ?z)
         (let ((print-integers-as-characters t))
           (princ sexp (current-buffer)))
       (princ sexp (current-buffer))))
    (string
     (let ((print-escape-newlines t))
       (prin1 sexp (current-buffer))))
    (otherwise (princ sexp (current-buffer)))))

(defun pp--format-vector (sexp)
  (insert "[")
  (cl-loop for i from 0
           for element across sexp
           do (pp--insert (and (> i 0) " ") element))
  (insert "]"))

(defun pp--format-list (sexp &optional start)
  (if (and (symbolp (car sexp))
           (not pp--inhibit-function-formatting)
           (not (keywordp (car sexp))))
      (pp--format-function sexp)
    (insert "(")
    (pp--insert start (pop sexp))
    (while sexp
      (if (consp sexp)
          (pp--insert " " (pop sexp))
        (pp--insert " . " sexp)
        (setq sexp nil)))
    (insert ")")))

(defun pp--format-function (sexp)
  (let* ((sym (car sexp))
         (edebug (get sym 'edebug-form-spec))
         (indent (get sym 'lisp-indent-function))
         (doc (get sym 'doc-string-elt)))
    (when (eq indent 'defun)
      (setq indent 2))
    ;; We probably want to keep all the elements before the doc string
    ;; on a single line.
    (when doc
      (setq indent (1- doc)))
    ;; Special-case closures -- these shouldn't really exist in actual
    ;; source code, so there's no indentation information.  But make
    ;; them output slightly better.
    (when (and (not indent)
               (eq sym 'closure))
      (setq indent 0))
    (pp--insert "(" sym)
    (pop sexp)
    ;; Get the first entries on the first line.
    (if indent
        (pp--format-definition sexp indent edebug)
      (let ((prev 0))
        (while sexp
          (let ((start (point)))
            ;; Don't put sexps on the same line as a multi-line sexp
            ;; preceding it.
            (pp--insert (if (> prev 1) "\n" " ")
                        (pop sexp))
            (setq prev (count-lines start (point)))))))
    (insert ")")))

(defun pp--format-definition (sexp indent edebug)
  (while (and (cl-plusp indent)
              sexp)
    (insert " ")
    ;; We don't understand all the edebug specs.
    (unless (consp edebug)
      (setq edebug nil))
    (if (and (consp (car edebug))
             (eq (caar edebug) '&rest))
        (pp--insert-binding (pop sexp))
      (if (null (car sexp))
          (insert "()")
        (pp--insert-lisp (car sexp)))
      (pop sexp))
    (pop edebug)
    (cl-decf indent))
  (when (stringp (car sexp))
    (insert "\n")
    (prin1 (pop sexp) (current-buffer)))
  ;; Then insert the rest with line breaks before each form.
  (while sexp
    (insert "\n")
    (if (keywordp (car sexp))
        (progn
          (pp--insert-lisp (pop sexp))
          (when sexp
            (pp--insert " " (pop sexp))))
      (pp--insert-lisp (pop sexp)))))

(defun pp--insert-binding (sexp)
  (insert "(")
  (while sexp
    (if (consp (car sexp))
        ;; Newlines after each (...) binding.
        (progn
          (pp--insert-lisp (car sexp))
          (when (cdr sexp)
            (insert "\n")))
      ;; Keep plain symbols on the same line.
      (pp--insert " " (car sexp)))
    (pop sexp))
  (insert ")"))

(defun pp--insert (delim &rest things)
  (let ((start (if (markerp delim)
                   (prog1
                       delim
                     (setq delim nil))
                 (point-marker))))
    (when delim
      (insert delim))
    (dolist (thing things)
      (pp--insert-lisp thing))
    ;; We need to indent what we have so far to see if we have to fold.
    (pp--indent-buffer)
    (when (> (current-column) (pp--max-width))
      (save-excursion
        (goto-char start)
        (unless (looking-at "[ \t]+$")
          (insert "\n"))
        (pp--indent-buffer)
        (goto-char (point-max))
        ;; If we're still too wide, then go up one step and try to
        ;; insert a newline there.
        (when (> (current-column) (pp--max-width))
          (condition-case ()
              (backward-up-list 1)
            (:success (when (and (not (bobp)) (looking-back " " 2))
                        (insert "\n")))
            (error nil)))))))

(defun pp--max-width ()
  (cond ((numberp pp-max-width)
         pp-max-width)
        ((null pp-max-width)
         most-positive-fixnum)
        ((eq pp-max-width t)
         (window-width))
        (t
         (error "Invalid pp-max-width value: %s" pp-max-width))))

(defun pp--indent-buffer ()
  (goto-char (point-min))
  (while (not (eobp))
    (lisp-indent-line)
    (forward-line 1)))

(provide 'pp)				; so (require 'pp) works

;;; pp.el ends here
