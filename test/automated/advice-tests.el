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
