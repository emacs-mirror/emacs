;;; advice-tests.el --- Test suite for the new advice thingy.

;; Copyright (C) 2012-2013 Free Software Foundation, Inc.

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

(ert-deftest advice-tests ()
  "Test advice code."
  (with-temp-buffer
    (defun sm-test1 (x) (+ x 4))
    (should (equal (sm-test1 6) 10))
    (advice-add 'sm-test1 :around (lambda (f y) (* (funcall f y) 5)))
    (should (equal (sm-test1 6) 50))
    (defun sm-test1 (x) (+ x 14))
    (should (equal (sm-test1 6) 100))
    (should (equal (null (get 'sm-test1 'defalias-fset-function)) nil))
    (advice-remove 'sm-test1 (lambda (f y) (* (funcall f y) 5)))
    (should (equal (sm-test1 6) 20))
    (should (equal (null (get 'sm-test1 'defalias-fset-function)) t))

    (defun sm-test2 (x) (+ x 4))
    (should (equal (sm-test2 6) 10))
    (defadvice sm-test2 (around sm-test activate)
       ad-do-it (setq ad-return-value (* ad-return-value 5)))
    (should (equal (sm-test2 6) 50))
    (ad-deactivate 'sm-test2)
    (should (equal (sm-test2 6) 10))
    (ad-activate 'sm-test2)
    (should (equal (sm-test2 6) 50))
    (defun sm-test2 (x) (+ x 14))
    (should (equal (sm-test2 6) 100))
    (should (equal (null (get 'sm-test2 'defalias-fset-function)) nil))
    (ad-remove-advice 'sm-test2 'around 'sm-test)
    (should (equal (sm-test2 6) 100))
    (ad-activate 'sm-test2)
    (should (equal (sm-test2 6) 20))
    (should (equal (null (get 'sm-test2 'defalias-fset-function)) t))

    (advice-add 'sm-test3 :around
		 (lambda (f &rest args) `(toto ,(apply f args)))
		 '((name . wrap-with-toto)))
     (defmacro sm-test3 (x) `(call-test3 ,x))
    (should (equal (macroexpand '(sm-test3 56)) '(toto (call-test3 56))))

    (defadvice sm-test4 (around wrap-with-toto activate)
       ad-do-it (setq ad-return-value `(toto ,ad-return-value)))
     (defmacro sm-test4 (x) `(call-test4 ,x))
    (should (equal (macroexpand '(sm-test4 56)) '(toto (call-test4 56))))
    (defmacro sm-test4 (x) `(call-testq ,x))
    (should (equal (macroexpand '(sm-test4 56)) '(toto (call-testq 56))))

    ;; Combining old style and new style advices.
    (defun sm-test5 (x) (+ x 4))
    (should (equal (sm-test5 6) 10))
    (advice-add 'sm-test5 :around (lambda (f y) (* (funcall f y) 5)))
    (should (equal (sm-test5 6) 50))
    (defadvice sm-test5 (around test activate)
       ad-do-it (setq ad-return-value (+ ad-return-value 0.1)))
    (should (equal (sm-test5 5) 45.1))
    (ad-deactivate 'sm-test5)
    (should (equal (sm-test5 6) 50))
    (ad-activate 'sm-test5)
    (should (equal (sm-test5 6) 50.1))
    (defun sm-test5 (x) (+ x 14))
    (should (equal (sm-test5 6) 100.1))
    (advice-remove 'sm-test5 (lambda (f y) (* (funcall f y) 5)))
    (should (equal (sm-test5 6) 20.1))

    ;; This used to signal an error (bug#12858).
    (autoload 'sm-test6 "foo")
     (defadvice sm-test6 (around test activate)
       ad-do-it)

    ;; Check interaction between advice and called-interactively-p.
    (defun sm-test7 (&optional x) (interactive) (+ (or x 7) 4))
    (advice-add 'sm-test7 :around
                (lambda (f &rest args)
                  (list (cons 1 (called-interactively-p)) (apply f args))))
    (should (equal (sm-test7) '((1 . nil) 11)))
    (should (equal (call-interactively 'sm-test7) '((1 . t) 11)))
    (let ((smi 7))
      (advice-add 'sm-test7 :before
                  (lambda (&rest args)
                    (setq smi (called-interactively-p))))
      (should (equal (list (sm-test7) smi)
                     '(((1 . nil) 11) nil)))
      (should (equal (list (call-interactively 'sm-test7) smi)
                      '(((1 . t) 11) t))))
    (advice-add 'sm-test7 :around
                (lambda (f &rest args)
                  (cons (cons 2 (called-interactively-p)) (apply f args))))
    (should (equal (call-interactively 'sm-test7) '((2 . t) (1 . t) 11)))

    ;; Check handling of interactive spec.
    (defun sm-test8 (a) (interactive "p") a)
    (defadvice sm-test8 (before adv1 activate) nil)
    (defadvice sm-test8 (before adv2 activate) (interactive "P") nil)
    (should (equal (interactive-form 'sm-test8) '(interactive "P")))
    ))

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; advice-tests.el ends here.
