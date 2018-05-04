;;; thunk.el --- Lazy form evaluation  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 Free Software Foundation, Inc.

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: sequences
;; Version: 1.0
;; Package: thunk

;; Maintainer: emacs-devel@gnu.org

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
;;
;; Thunk provides functions and macros to delay the evaluation of
;; forms.
;;
;; Use `thunk-delay' to delay the evaluation of a form (requires
;; lexical-binding), and `thunk-force' to evaluate it. The result of
;; the evaluation is cached, and only happens once.
;;
;; Here is an example of a form which evaluation is delayed:
;;
;;     (setq delayed (thunk-delay (message "this message is delayed")))
;;
;; `delayed' is not evaluated until `thunk-force' is called, like the
;; following:
;;
;;    (thunk-force delayed)
;;
;; This file also defines macros `thunk-let' and `thunk-let*' that are
;; analogous to `let' and `let*' but provide lazy evaluation of
;; bindings by using thunks implicitly (i.e. in the expansion).

;;; Code:

(require 'cl-lib)

(defmacro thunk-delay (&rest body)
  "Delay the evaluation of BODY."
  (declare (debug t))
  (cl-assert lexical-binding)
  (let ((forced (make-symbol "forced"))
        (val (make-symbol "val")))
    `(let (,forced ,val)
       (lambda (&optional check)
         (if check
             ,forced
           (unless ,forced
             (setf ,val (progn ,@body))
             (setf ,forced t))
           ,val)))))

(defun thunk-force (delayed)
  "Force the evaluation of DELAYED.
The result is cached and will be returned on subsequent calls
with the same DELAYED argument."
  (funcall delayed))

(defun thunk-evaluated-p (delayed)
  "Return non-nil if DELAYED has been evaluated."
  (funcall delayed t))

(defmacro thunk-let (bindings &rest body)
  "Like `let' but create lazy bindings.

BINDINGS is a list of elements of the form (SYMBOL EXPRESSION).
Any binding EXPRESSION is not evaluated before the variable
SYMBOL is used for the first time when evaluating the BODY.

It is not allowed to set `thunk-let' or `thunk-let*' bound
variables.

Using `thunk-let' and `thunk-let*' requires `lexical-binding'."
  (declare (indent 1) (debug let))
  (cl-callf2 mapcar
      (lambda (binding)
        (pcase binding
          (`(,(pred symbolp) ,_) binding)
          (_ (signal 'error (cons "Bad binding in thunk-let"
                                  (list binding))))))
      bindings)
  (cl-callf2 mapcar
      (pcase-lambda (`(,var ,binding))
        (list (make-symbol (concat (symbol-name var) "-thunk"))
              var binding))
      bindings)
  `(let ,(mapcar
          (pcase-lambda (`(,thunk-var ,_var ,binding))
            `(,thunk-var (thunk-delay ,binding)))
          bindings)
     (cl-symbol-macrolet
         ,(mapcar (pcase-lambda (`(,thunk-var ,var ,_binding))
                    `(,var (thunk-force ,thunk-var)))
                  bindings)
       ,@body)))

(defmacro thunk-let* (bindings &rest body)
  "Like `let*' but create lazy bindings.

BINDINGS is a list of elements of the form (SYMBOL EXPRESSION).
Any binding EXPRESSION is not evaluated before the variable
SYMBOL is used for the first time when evaluating the BODY.

It is not allowed to set `thunk-let' or `thunk-let*' bound
variables.

Using `thunk-let' and `thunk-let*' requires `lexical-binding'."
  (declare (indent 1) (debug let))
  (cl-reduce
   (lambda (expr binding) `(thunk-let (,binding) ,expr))
   (nreverse bindings)
   :initial-value (macroexp-progn body)))

;; (defalias 'lazy-let  #'thunk-let)
;; (defalias 'lazy-let* #'thunk-let*)


(provide 'thunk)
;;; thunk.el ends here
