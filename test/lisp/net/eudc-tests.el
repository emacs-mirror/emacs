;;; eudc-tests.el --- tests for eudc.el -*- lexical-binding: t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

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

;;; Code:

(require 'eudc)

(ert-deftest eudc--plist-member ()
  "Test `eudc--plist-member' behavior."
  (dolist (obj '(a (a . a) (a a . a)))
    (should-error (eudc--plist-member obj nil) :type 'wrong-type-argument))
  (dolist (plist '((nil) (a) (a a a)))
    (let ((err `(wrong-type-argument plistp ,(copy-sequence plist))))
      (dolist (key '(nil a))
        (should (equal err (should-error (eudc--plist-member plist key)))))))
  (let ((-nil (string ?n ?i ?l))
        (-a (string ?a)))
    (should-not (eudc--plist-member () nil))
    (should-not (eudc--plist-member () 'a))
    (should-not (eudc--plist-member '(nil nil) 'a))
    (should-not (eudc--plist-member '(nil a) 'a))
    (should-not (eudc--plist-member '(a nil) nil))
    (should-not (eudc--plist-member '(a a) nil))
    (should-not (eudc--plist-member '("nil" a) nil))
    (should-not (eudc--plist-member '("nil" a) -nil))
    (should-not (eudc--plist-member '("a" a) nil))
    (should-not (eudc--plist-member '("a" a) -a))
    (should-not (eudc--plist-member '(nil a nil a) 'a))
    (should-not (eudc--plist-member '(nil a "a" a) -a))
    (should (equal (eudc--plist-member '(nil nil) nil) '(nil nil)))
    (should (equal (eudc--plist-member '(nil a) nil) '(nil a)))
    (should (equal (eudc--plist-member '(a nil) 'a) '(a nil)))
    (should (equal (eudc--plist-member '(a a) 'a) '(a a)))
    (should (equal (eudc--plist-member '(nil nil a nil) 'a) '(a nil)))
    (should (equal (eudc--plist-member '(nil a a a) 'a) '(a a)))
    (should (equal (eudc--plist-member '(a a a a) 'a) '(a a a a)))))

(ert-deftest eudc-plist-member ()
  "Test `eudc-plist-member' behavior."
  (dolist (obj '(a (a . a) (a a . a)))
    (should-error (eudc-plist-member obj nil) :type 'wrong-type-argument))
  (dolist (plist '((nil) (a) (a a a)))
    (let ((err `(wrong-type-argument plistp ,(copy-sequence plist))))
      (dolist (key '(nil a))
        (should (equal err (should-error (eudc-plist-member plist key)))))))
  (let ((-nil (string ?n ?i ?l))
        (-a (string ?a)))
    (should-not (eudc-plist-member () nil))
    (should-not (eudc-plist-member () 'a))
    (should-not (eudc-plist-member '(nil nil) 'a))
    (should-not (eudc-plist-member '(nil a) 'a))
    (should-not (eudc-plist-member '(a nil) nil))
    (should-not (eudc-plist-member '(a a) nil))
    (should-not (eudc-plist-member '("nil" a) nil))
    (should-not (eudc-plist-member '("nil" a) -nil))
    (should-not (eudc-plist-member '("a" a) nil))
    (should-not (eudc-plist-member '("a" a) -a))
    (should-not (eudc-plist-member '(nil a nil a) 'a))
    (should-not (eudc-plist-member '(nil a "a" a) -a))
    (should (eq t (eudc-plist-member '(nil nil) nil)))
    (should (eq t (eudc-plist-member '(nil a) nil)))
    (should (eq t (eudc-plist-member '(a nil) 'a)))
    (should (eq t (eudc-plist-member '(a a) 'a)))
    (should (eq t (eudc-plist-member '(nil nil a nil) 'a)))
    (should (eq t (eudc-plist-member '(nil a a a) 'a)))
    (should (eq t (eudc-plist-member '(a a a a) 'a)))))

(ert-deftest eudc-plist-get ()
  "Test `eudc-plist-get' behavior."
  (dolist (obj '(a (a . a) (a a . a)))
    (should-error (eudc-plist-get obj nil) :type 'wrong-type-argument))
  (dolist (plist '((nil) (a) (a a a)))
    (let ((err `(wrong-type-argument plistp ,(copy-sequence plist))))
      (dolist (key '(nil a))
        (should (equal err (should-error (eudc-plist-get plist key)))))))
  (let ((-nil (string ?n ?i ?l))
        (-a (string ?a)))
    (should-not (eudc-plist-get () nil))
    (should-not (eudc-plist-get () 'a))
    (should-not (eudc-plist-get '(nil nil) nil))
    (should-not (eudc-plist-get '(nil nil) 'a))
    (should-not (eudc-plist-get '(nil a) 'a))
    (should-not (eudc-plist-get '(a nil) nil))
    (should-not (eudc-plist-get '(a nil) 'a))
    (should-not (eudc-plist-get '(a a) nil))
    (should-not (eudc-plist-get '("nil" a) nil))
    (should-not (eudc-plist-get '("nil" a) -nil))
    (should-not (eudc-plist-get '("a" a) nil))
    (should-not (eudc-plist-get '("a" a) -a))
    (should-not (eudc-plist-get '(nil nil nil a) nil))
    (should-not (eudc-plist-get '(nil a nil a) 'a))
    (should-not (eudc-plist-get '(nil a "a" a) -a))
    (should-not (eudc-plist-get '(a nil a a) 'a))
    (should (eq 'a (eudc-plist-get '(nil a) nil)))
    (should (eq 'a (eudc-plist-get '(a a) 'a)))
    (should (eq 'a (eudc-plist-get '(a a a nil) 'a)))
    (should (eq 'b (eudc-plist-get () nil 'b)))
    (should (eq 'b (eudc-plist-get () 'a 'b)))
    (should (eq 'b (eudc-plist-get '(nil a "a" a) -a 'b)))
    (should (eq 'b (eudc-plist-get '(a nil "nil" nil) -nil 'b)))))

(ert-deftest eudc-lax-plist-get ()
  "Test `eudc-lax-plist-get' behavior."
  (dolist (obj '(a (a . a) (a a . a)))
    (should-error (eudc-lax-plist-get obj nil) :type 'wrong-type-argument))
  (dolist (plist '((nil) (a) (a a a)))
    (let ((err `(wrong-type-argument plistp ,(copy-sequence plist))))
      (dolist (key '(nil a))
        (should (equal err (should-error (eudc-lax-plist-get plist key)))))))
  (let ((-nil (string ?n ?i ?l))
        (-a (string ?a)))
    (should-not (eudc-lax-plist-get () nil))
    (should-not (eudc-lax-plist-get () 'a))
    (should-not (eudc-lax-plist-get '(nil nil) nil))
    (should-not (eudc-lax-plist-get '(nil nil) 'a))
    (should-not (eudc-lax-plist-get '(nil a) 'a))
    (should-not (eudc-lax-plist-get '(a nil) nil))
    (should-not (eudc-lax-plist-get '(a nil) 'a))
    (should-not (eudc-lax-plist-get '(a a) nil))
    (should-not (eudc-lax-plist-get '("nil" a) nil))
    (should-not (eudc-lax-plist-get '("nil" a) 'a))
    (should-not (eudc-lax-plist-get '("a" a) nil))
    (should-not (eudc-lax-plist-get '("a" a) 'a))
    (should-not (eudc-lax-plist-get '(nil nil nil a) nil))
    (should-not (eudc-lax-plist-get '(nil a nil a) 'a))
    (should-not (eudc-lax-plist-get '(nil a "a" a) 'a))
    (should-not (eudc-lax-plist-get '(a nil a a) 'a))
    (should (eq 'a (eudc-lax-plist-get '(nil a) nil)))
    (should (eq 'a (eudc-lax-plist-get '(a a) 'a)))
    (should (eq 'a (eudc-lax-plist-get '(a a a nil) 'a)))
    (should (eq 'b (eudc-lax-plist-get () nil 'b)))
    (should (eq 'b (eudc-lax-plist-get () 'a 'b)))
    (should (eq 'a (eudc-lax-plist-get '("nil" a) -nil)))
    (should (eq 'a (eudc-lax-plist-get '("a" a) -a)))
    (should (eq 'a (eudc-lax-plist-get '(nil a "a" a) -a)))
    (should (eq 'b (eudc-lax-plist-get '(nil a "a" a) 'a 'b)))
    (should (eq 'b (eudc-lax-plist-get '(a nil "nil" nil) nil 'b)))))

;;; eudc-tests.el ends here
