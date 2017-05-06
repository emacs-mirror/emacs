;;; assess-call.el --- Call and Return -*- lexical-binding: t -*-

;;; Header:

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@russet.org.uk>
;; Maintainer: Phillip Lord <phillip.lord@russet.org.uk>

;; The contents of this file are subject to the GPL License, Version 3.0.

;; Copyright (C) 2016, Phillip Lord

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Capture calls to functions, checking parameters and return values.

;;; Code:

;; ** Call Capture

;; Here we provide a function for tracing calls to a particular function. This
;; can be a direct or indirect call; parameters and return values are available
;; for inspection afterwards. For example:

;; #+begin_src elisp
;;   (assess-call-capture
;;     '+
;;     (lambda()
;;       (+ 1 1)))
;;   ;; => (((1 1) . 2))
;; #+end_src

;; The return value is a list of cons cells, one for each invocation, of the
;; parameters and return values.


;; #+begin_src emacs-lisp
(defun assess-call--capture-lambda ()
  "Return function which captures args and returns of another.

The returned function takes FN the function to call, and any
number of ARGS to call the function with. In the special case,
that FN is equal to `:return`, then all previous args and return
values of FN are returned instead."
  (let ((capture-store nil))
    (lambda (fn &rest args)
      (if (eq fn :return)
          capture-store
        (let ((rtn (apply fn args)))
          (setq capture-store
                (cons (cons args rtn)
                      capture-store))
          rtn)))))

(defun assess-call-capture (sym-fn fn)
  "Trace all calls to SYM-FN when FN is called with no args.

The return value is a list of cons cells, with car being the
parameters of the calls, and the cdr being the return value."
  (let ((capture-lambda
         (assess-call--capture-lambda)))
    (unwind-protect
        (progn (advice-add sym-fn :around capture-lambda)
               (funcall fn)
               (funcall capture-lambda :return))
      (advice-remove sym-fn capture-lambda))))

(defun assess-call--hook-capture-lambda ()
  "Returns a function which captures all of its args.

The returned function takes any number of ARGS. In the special
case that the first arg is `:return` then it returns all previous
args."
  (let ((capture-store nil))
    (lambda (&rest args)
      (if (eq (car-safe args) :return)
          capture-store
        (setq capture-store
              (cons
               args
               capture-store))))))

(defun assess-call-capture-hook (hook-var fn &optional append local)
  "Trace all calls to HOOK-VAR when FN is called with no args.
APPEND and LOCAL are passed to `add-hook` and documented there."
  (let ((capture-lambda
         (assess-call--hook-capture-lambda)))
    (unwind-protect
        (progn
          (add-hook hook-var
                    capture-lambda
                    append local)
          (funcall fn)
          (funcall capture-lambda :return))
      (remove-hook hook-var
                   capture-lambda
                   local))))

(provide 'assess-call)
;;; assess-call.el ends here
;; #+end_src
