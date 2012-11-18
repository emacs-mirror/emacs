;;; advice-tests.el --- Test suite for the new advice thingy.

;; Copyright (C) 2012 Free Software Foundation, Inc.

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

;;; Code:

(defvar advice-tests--data
  '(((defun sm-test1 (x) (+ x 4))
     (sm-test1 6) 10)
    ((advice-add 'sm-test1 :around (lambda (f y) (* (funcall f y) 5)))
     (sm-test1 6) 50)
    ((defun sm-test1 (x) (+ x 14))
     (sm-test1 6) 100)
    ((null (get 'sm-test1 'defalias-fset-function)) nil)
    ((advice-remove 'sm-test1 (lambda (f y) (* (funcall f y) 5)))
     (sm-test1 6) 20)
    ((null (get 'sm-test1 'defalias-fset-function)) t)

    ((defun sm-test2 (x) (+ x 4))
     (sm-test2 6) 10)
    ((defadvice sm-test2 (around sm-test activate)
       ad-do-it (setq ad-return-value (* ad-return-value 5)))
     (sm-test2 6) 50)
    ((ad-deactivate 'sm-test2)
     (sm-test2 6) 10)
    ((ad-activate 'sm-test2)
     (sm-test2 6) 50)
    ((defun sm-test2 (x) (+ x 14))
     (sm-test2 6) 100)
    ((null (get 'sm-test2 'defalias-fset-function)) nil)
    ((ad-remove-advice 'sm-test2 'around 'sm-test)
     (sm-test2 6) 100)
    ((ad-activate 'sm-test2)
     (sm-test2 6) 20)
    ((null (get 'sm-test2 'defalias-fset-function)) t)

    ((advice-add 'sm-test3 :around
		 (lambda (f &rest args) `(toto ,(apply f args)))
		 '((name . wrap-with-toto)))
     (defmacro sm-test3 (x) `(call-test3 ,x))
     (macroexpand '(sm-test3 56)) (toto (call-test3 56)))

    ((defadvice sm-test4 (around wrap-with-toto activate)
       ad-do-it (setq ad-return-value `(toto ,ad-return-value)))
     (defmacro sm-test4 (x) `(call-test4 ,x))
     (macroexpand '(sm-test4 56)) (toto (call-test4 56)))
    ((defmacro sm-test4 (x) `(call-testq ,x))
     (macroexpand '(sm-test4 56)) (toto (call-testq 56)))

    ;; Combining old style and new style advices.
    ((defun sm-test5 (x) (+ x 4))
     (sm-test5 6) 10)
    ((advice-add 'sm-test5 :around (lambda (f y) (* (funcall f y) 5)))
     (sm-test5 6) 50)
    ((defadvice sm-test5 (around test activate)
       ad-do-it (setq ad-return-value (+ ad-return-value 0.1)))
     (sm-test5 5) 45.1)
    ((ad-deactivate 'sm-test5)
     (sm-test5 6) 50)
    ((ad-activate 'sm-test5)
     (sm-test5 6) 50.1)
    ((defun sm-test5 (x) (+ x 14))
     (sm-test5 6) 100.1)
    ((advice-remove 'sm-test5 (lambda (f y) (* (funcall f y) 5)))
     (sm-test5 6) 20.1)

    ;; This used to signal an error (bug#12858).
    ((autoload 'sm-test6 "foo")
     (defadvice sm-test6 (around test activate)
       ad-do-it)
     t t)

    ))

(ert-deftest advice-tests ()
  "Test advice code."
  (with-temp-buffer
    (dolist (test advice-tests--data)
      (let ((res (eval `(progn ,@(butlast test)))))
        (should (equal (car (last test)) res))))))

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; advice-tests.el ends here.
