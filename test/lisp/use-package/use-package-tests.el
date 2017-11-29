;;; use-package-tests.el --- Tests for use-package.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 


;;; Code:

(require 'ert)
(require 'use-package)

(ert-deftest use-package-normalize-binder ()
  (let ((good-values '(:map map-sym
                       ("str" . sym) ("str" . "str")
                       ([vec] . sym) ([vec] . "str"))))
    (should (equal (use-package-normalize-binder
                    'foopkg :bind good-values)
                   good-values)))
  (should-error (use-package-normalize-binder
                 'foopkg :bind '("foo")))
  (should-error (use-package-normalize-binder
                 'foopkg :bind '("foo" . 99)))
  (should-error (use-package-normalize-binder
                 'foopkg :bind '(99 . sym))))

(ert-deftest use-package-normalize-mode ()
  (should (equal (use-package-normalize-mode 'foopkg :mode '(".foo"))
                 '((".foo" . foopkg))))
  (should (equal (use-package-normalize-mode 'foopkg :mode '(".foo" ".bar"))
                 '((".foo" . foopkg) (".bar" . foopkg))))
  (should (equal (use-package-normalize-mode 'foopkg :mode '((".foo" ".bar")))
                 '((".foo" . foopkg) (".bar" . foopkg))))
  (should (equal (use-package-normalize-mode 'foopkg :mode '((".foo")))
                 '((".foo" . foopkg))))
  (should (equal (use-package-normalize-mode 'foopkg :mode '((".foo" . foo) (".bar" . bar)))
                 '((".foo" . foo) (".bar" . bar)))))

(ert-deftest use-package-normalize-delight ()
  (should (equal `((foo-mode nil foo))
                 (use-package-normalize/:delight 'foo :delight nil)))
  (should (equal `((foo-mode nil foo-mode))
                 (use-package-normalize/:delight 'foo-mode :delight nil)))
  (should (equal `((bar-mode nil foo))
                 (use-package-normalize/:delight 'foo :delight '(bar-mode))))
  (should (equal `((bar-mode nil :major))
                 (use-package-normalize/:delight 'foo :delight '((bar-mode nil :major)))))
  (should (equal `((foo-mode "abc" foo))
                 (use-package-normalize/:delight 'foo :delight '("abc"))))
  (should (equal `((foo-mode (:eval 1) foo))
                 (use-package-normalize/:delight 'foo :delight '('(:eval 1)))))
  (should (equal `((a-mode nil foo)
                   (b-mode " b" foo))
                 (use-package-normalize/:delight 'foo :delight '((a-mode)
                                                                 (b-mode " b")))))
  (should-error (use-package-normalize/:delight 'foo :delight '((:eval 1))))

  )

(ert-deftest use-package-normalize-diminish ()
  (should (equal (use-package-normalize-diminish 'foopkg :diminish nil)
                 '(foopkg-mode)))
  (should (equal (use-package-normalize-diminish 'foopkg :diminish 'bar)
                 '(bar)))
  (should (equal (use-package-normalize-diminish 'foopkg :diminish "bar")
                 '((foopkg-mode . "bar"))))
  (should (equal (use-package-normalize-diminish 'foopkg :diminish 'foo-mode)
                 '(foo-mode)))
  (should (equal (use-package-normalize-diminish 'foopkg :diminish '(foo . "bar"))
                 '((foo . "bar")))))

(ert-deftest use-package--recognize-function-test ()
  (should (use-package--recognize-function 'sym))
  (should (use-package--recognize-function #'sym))
  (should (use-package--recognize-function (lambda () ...)))
  (should (use-package--recognize-function '(lambda () ...)))
  (should (use-package--recognize-function #'(lambda () ...))))

(ert-deftest use-package--normalize-function-test ()
  (should (equal (use-package--normalize-function 'sym) 'sym))
  (should (equal (use-package--normalize-function #'sym) 'sym))
  (should (equal (use-package--normalize-function (lambda () ...)) (lambda () ...)))
  (should (equal (use-package--normalize-function '(lambda () ...)) (lambda () ...)))
  (should (equal (use-package--normalize-function #'(lambda () ...)) (lambda () ...))))

;; Local Variables:
;; indent-tabs-mode: nil
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; use-package-tests.el ends here
