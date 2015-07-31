;;; stream-tests.el --- Unit tests for stream.el  -*- lexical-binding: t -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Nicolas Petton <nicolas@petton.fr>

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'ert)
(require 'stream)

(defun stream-to-list (stream)
  "Eagerly traverse STREAM and return a list of its elements."
  (let (result)
    (stream-do (lambda (elt)
                 (push elt result))
               stream)
    (reverse result)))

(ert-deftest stream-empty-test ()
  (should (stream-p (stream-empty)))
  (should (stream-empty-p (stream-empty))))

(ert-deftest stream-make-test ()
  (should (stream-p (stream-range)))
  (should (not (stream-empty-p (stream-range))))) ;; Should use stream-list or something

(ert-deftest stream-first-test ()
  (should (= 3 (stream-first (stream-range 3))))
  (should (null (stream-first (stream-empty)))))

(ert-deftest stream-rest-test ()
  (should (= 4 (stream-first (stream-rest (stream-range 3)))))
  (should (= 5 (stream-first (stream-rest (stream-rest (stream-range 3)))))))

(ert-deftest stream-take-test ()
  (should (stream-p (stream-take (stream-range) 2)))
  (should (= 0 (stream-first (stream-take (stream-range) 2))))
  (should (= 1 (stream-first (stream-rest (stream-take (stream-range) 2)))))
  (should (null (stream-first (stream-rest (stream-rest (stream-take (stream-range) 2))))))
  (should (stream-empty-p (stream-rest (stream-rest (stream-take (stream-range) 2))))))

(ert-deftest stream-drop-test ()
  (should (stream-p (stream-drop (stream-range) 2)))
  (should (= 2 (stream-first (stream-drop (stream-range) 2))))
  (should (= 3 (stream-first (stream-rest (stream-drop (stream-range) 2)))))
  (should (stream-empty-p (stream-drop (stream-empty) 2))))

(ert-deftest stream-take-while-test ()
  (let ((stream (stream-list '(1 3 2 5))))
    (should (stream-empty-p (stream-take-while #'identity (stream-empty))))
    (should (stream-p (stream-take-while #'oddp stream)))
    (should (= 1 (stream-first (stream-take-while #'oddp stream))))
    (should (= 3 (stream-first (stream-rest (stream-take-while #'oddp stream)))))
    (should (stream-empty-p (stream-rest (stream-rest (stream-take-while #'oddp stream)))))))

(ert-deftest stream-drop-while-test ()
  (let ((stream (stream-list '(1 3 2 5))))
    (should (stream-p (stream-drop-while #'evenp stream)))
    (should (stream-empty-p (stream-drop-while #'identity (stream-empty))))
    (should (= 2 (stream-first (stream-drop-while #'evenp stream))))
    (should (= 5 (stream-first (stream-rest (stream-drop-while #'evenp stream)))))
    (should (stream-empty-p (stream-rest (stream-rest (stream-drop-while #'evenp stream)))))))

(ert-deftest stream-map-test ()
  (should (stream-empty-p (stream-map #'- (stream-empty))))
  (should (= -1 (stream-first (stream-map #'- (stream-range 1)))))
  (should (= -2 (stream-first (stream-rest (stream-map #'- (stream-range 1)))))))

(ert-deftest stream-do-test ()
  (let ((result '()))
    (stream-do
     (lambda (elt)
       (push elt result))
     (stream-range 0 5))
    (should (equal result '(4 3 2 1 0)))))

(ert-deftest stream-filter-test ()
  (should (stream-empty-p (stream-filter #'oddp (stream-empty))))
  (should (stream-empty-p (stream-filter #'oddp (stream-range 0 4 2))))
  (should (= 1 (stream-first (stream-filter #'oddp (stream-range 0 4)))))
  (should (= 3 (stream-first (stream-rest (stream-filter #'oddp (stream-range 0 4))))))
  (should (stream-empty-p (stream-rest (stream-rest (stream-filter #'oddp (stream-range 0 4)))))))

(ert-deftest stream-range-test ()
  (should (stream-empty-p (stream-range 0 0)))
  (should (stream-empty-p (stream-range 3 3)))
  (should-error (stream-range 3 2))
  (should (= 0 (stream-first (stream-range 0 6 2))))
  (should (= 2 (stream-first (stream-rest (stream-range 0 6 2)))))
  (should (= 4 (stream-first (stream-rest (stream-rest (stream-range 0 6 2))))))
  (should (stream-empty-p (stream-rest (stream-rest (stream-rest (stream-range 0 6 2)))))))

(ert-deftest stream-list-test ()
  (dolist (list '(nil '(1 2 3) '(a . b)))
    (should (equal list (stream-to-list (stream-list list))))))

(provide 'stream-tests)
;;; stream-tests.el ends here
