;;; subr-x.el --- extra Lisp functions  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2020 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: convenience
;; Package: emacs

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

;; Less commonly used functions that complement basic APIs, often implemented in
;; C code (like hash-tables and strings), and are not eligible for inclusion
;; in subr.el.

;; Do not document these functions in the lispref.
;; https://lists.gnu.org/r/emacs-devel/2014-01/msg01006.html

;; NB If you want to use this library, it's almost always correct to use:
;; (eval-when-compile (require 'subr-x))

;;; Code:

(eval-when-compile (require 'cl-lib))


(defmacro internal--thread-argument (first? &rest forms)
  "Internal implementation for `thread-first' and `thread-last'.
When Argument FIRST? is non-nil argument is threaded first, else
last.  FORMS are the expressions to be threaded."
  (pcase forms
    (`(,x (,f . ,args) . ,rest)
     `(internal--thread-argument
       ,first? ,(if first? `(,f ,x ,@args) `(,f ,@args ,x)) ,@rest))
    (`(,x ,f . ,rest) `(internal--thread-argument ,first? (,f ,x) ,@rest))
    (_ (car forms))))

(defmacro thread-first (&rest forms)
  "Thread FORMS elements as the first argument of their successor.
Example:
    (thread-first
      5
      (+ 20)
      (/ 25)
      -
      (+ 40))
Is equivalent to:
    (+ (- (/ (+ 5 20) 25)) 40)
Note how the single `-' got converted into a list before
threading."
  (declare (indent 1)
           (debug (form &rest [&or symbolp (sexp &rest form)])))
  `(internal--thread-argument t ,@forms))

(defmacro thread-last (&rest forms)
  "Thread FORMS elements as the last argument of their successor.
Example:
    (thread-last
      5
      (+ 20)
      (/ 25)
      -
      (+ 40))
Is equivalent to:
    (+ 40 (- (/ 25 (+ 20 5))))
Note how the single `-' got converted into a list before
threading."
  (declare (indent 1) (debug thread-first))
  `(internal--thread-argument nil ,@forms))

(defsubst internal--listify (elt)
  "Wrap ELT in a list if it is not one.
If ELT is of the form ((EXPR)), listify (EXPR) with a dummy symbol."
  (cond
   ((symbolp elt) (list elt elt))
   ((null (cdr elt))
    (list (make-symbol "s") (car elt)))
   (t elt)))

(defsubst internal--check-binding (binding)
  "Check BINDING is properly formed."
  (when (> (length binding) 2)
    (signal
     'error
     (cons "`let' bindings can have only one value-form" binding)))
  binding)

(defsubst internal--build-binding-value-form (binding prev-var)
  "Build the conditional value form for BINDING using PREV-VAR."
  (let ((var (car binding)))
    `(,var (and ,prev-var ,(cadr binding)))))

(defun internal--build-binding (binding prev-var)
  "Check and build a single BINDING with PREV-VAR."
  (thread-first
      binding
    internal--listify
    internal--check-binding
    (internal--build-binding-value-form prev-var)))

(defun internal--build-bindings (bindings)
  "Check and build conditional value forms for BINDINGS."
  (let ((prev-var t))
    (mapcar (lambda (binding)
              (let ((binding (internal--build-binding binding prev-var)))
                (setq prev-var (car binding))
                binding))
            bindings)))

(defmacro if-let* (varlist then &rest else)
  "Bind variables according to VARLIST and evaluate THEN or ELSE.
This is like `if-let' but doesn't handle a VARLIST of the form
\(SYMBOL SOMETHING) specially."
  (declare (indent 2)
           (debug ((&rest [&or symbolp (symbolp form) (form)])
                   form body)))
  (if varlist
      `(let* ,(setq varlist (internal--build-bindings varlist))
         (if ,(caar (last varlist))
             ,then
           ,@else))
    `(let* () ,then)))

(defmacro when-let* (varlist &rest body)
  "Bind variables according to VARLIST and conditionally evaluate BODY.
This is like `when-let' but doesn't handle a VARLIST of the form
\(SYMBOL SOMETHING) specially."
  (declare (indent 1) (debug if-let*))
  (list 'if-let* varlist (macroexp-progn body)))

(defmacro and-let* (varlist &rest body)
  "Bind variables according to VARLIST and conditionally evaluate BODY.
Like `when-let*', except if BODY is empty and all the bindings
are non-nil, then the result is non-nil."
  (declare (indent 1)
           (debug ((&rest [&or symbolp (symbolp form) (form)])
                   body)))
  (let (res)
    (if varlist
        `(let* ,(setq varlist (internal--build-bindings varlist))
           (when ,(setq res (caar (last varlist)))
             ,@(or body `(,res))))
      `(let* () ,@(or body '(t))))))

(defmacro if-let (spec then &rest else)
  "Bind variables according to SPEC and evaluate THEN or ELSE.
Evaluate each binding in turn, as in `let*', stopping if a
binding value is nil.  If all are non-nil return the value of
THEN, otherwise the last form in ELSE.

Each element of SPEC is a list (SYMBOL VALUEFORM) that binds
SYMBOL to the value of VALUEFORM.  An element can additionally be
of the form (VALUEFORM), which is evaluated and checked for nil;
i.e. SYMBOL can be omitted if only the test result is of
interest.  It can also be of the form SYMBOL, then the binding of
SYMBOL is checked for nil.

As a special case, interprets a SPEC of the form \(SYMBOL SOMETHING)
like \((SYMBOL SOMETHING)).  This exists for backward compatibility
with an old syntax that accepted only one binding."
  (declare (indent 2)
           (debug ([&or (&rest [&or symbolp (symbolp form) (form)])
                        (symbolp form)]
                   form body)))
  (when (and (<= (length spec) 2)
             (not (listp (car spec))))
    ;; Adjust the single binding case
    (setq spec (list spec)))
  (list 'if-let* spec then (macroexp-progn else)))

;;;###autoload
(defmacro when-let (spec &rest body)
  "Bind variables according to SPEC and conditionally evaluate BODY.
Evaluate each binding in turn, stopping if a binding value is nil.
If all are non-nil, return the value of the last form in BODY.

The variable list SPEC is the same as in `if-let'."
  (declare (indent 1) (debug if-let))
  (list 'if-let spec (macroexp-progn body)))

(defsubst hash-table-empty-p (hash-table)
  "Check whether HASH-TABLE is empty (has 0 elements)."
  (zerop (hash-table-count hash-table)))

(defsubst hash-table-keys (hash-table)
  "Return a list of keys in HASH-TABLE."
  (cl-loop for k being the hash-keys of hash-table collect k))

(defsubst hash-table-values (hash-table)
  "Return a list of values in HASH-TABLE."
  (cl-loop for v being the hash-values of hash-table collect v))

(defsubst string-empty-p (string)
  "Check whether STRING is empty."
  (string= string ""))

(defsubst string-join (strings &optional separator)
  "Join all STRINGS using SEPARATOR."
  (mapconcat #'identity strings separator))

(define-obsolete-function-alias 'string-reverse 'reverse "25.1")

(defsubst string-trim-left (string &optional regexp)
  "Trim STRING of leading string matching REGEXP.

REGEXP defaults to \"[ \\t\\n\\r]+\"."
  (if (string-match (concat "\\`\\(?:" (or regexp "[ \t\n\r]+") "\\)") string)
      (substring string (match-end 0))
    string))

(defsubst string-trim-right (string &optional regexp)
  "Trim STRING of trailing string matching REGEXP.

REGEXP defaults to  \"[ \\t\\n\\r]+\"."
  (let ((i (string-match-p (concat "\\(?:" (or regexp "[ \t\n\r]+") "\\)\\'")
                           string)))
    (if i (substring string 0 i) string)))

(defsubst string-trim (string &optional trim-left trim-right)
  "Trim STRING of leading and trailing strings matching TRIM-LEFT and TRIM-RIGHT.

TRIM-LEFT and TRIM-RIGHT default to \"[ \\t\\n\\r]+\"."
  (string-trim-left (string-trim-right string trim-right) trim-left))

(defsubst string-blank-p (string)
  "Check whether STRING is either empty or only whitespace.
The following characters count as whitespace here: space, tab, newline and
carriage return."
  (string-match-p "\\`[ \t\n\r]*\\'" string))

(defsubst string-remove-prefix (prefix string)
  "Remove PREFIX from STRING if present."
  (if (string-prefix-p prefix string)
      (substring string (length prefix))
    string))

(defsubst string-remove-suffix (suffix string)
  "Remove SUFFIX from STRING if present."
  (if (string-suffix-p suffix string)
      (substring string 0 (- (length string) (length suffix)))
    string))

(defun replace-region-contents (beg end replace-fn
                                    &optional max-secs max-costs)
  "Replace the region between BEG and END using REPLACE-FN.
REPLACE-FN runs on the current buffer narrowed to the region.  It
should return either a string or a buffer replacing the region.

The replacement is performed using `replace-buffer-contents'
which also describes the MAX-SECS and MAX-COSTS arguments and the
return value.

Note: If the replacement is a string, it'll be placed in a
temporary buffer so that `replace-buffer-contents' can operate on
it.  Therefore, if you already have the replacement in a buffer,
it makes no sense to convert it to a string using
`buffer-substring' or similar."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((repl (funcall replace-fn)))
	(if (bufferp repl)
	    (replace-buffer-contents repl max-secs max-costs)
	  (let ((source-buffer (current-buffer)))
	    (with-temp-buffer
	      (insert repl)
	      (let ((tmp-buffer (current-buffer)))
		(set-buffer source-buffer)
		(replace-buffer-contents tmp-buffer max-secs max-costs)))))))))

(provide 'subr-x)

;;; subr-x.el ends here
