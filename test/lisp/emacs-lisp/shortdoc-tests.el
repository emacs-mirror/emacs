;;; shortdoc-tests.el --- tests for shortdoc.el   -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

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

(require 'ert)
(require 'shortdoc)

(defun shortdoc-tests--tree-contains (tree fun)
  "Whether TREE contains a call to FUN."
  (and (proper-list-p tree)
       (or (eq (car tree) fun)
           (cl-some (lambda (x) (shortdoc-tests--tree-contains x fun)) tree))))

(ert-deftest shortdoc-examples ()
  "Check that each example actually contains the corresponding form."
  (dolist (group shortdoc--groups)
    (dolist (item group)
      (when (consp item)
        (let ((fun (car item))
              (props (cdr item)))
          (while props
            (when (memq (car props) '(:eval :no-eval :no-eval* :no-value))
              (let* ((example (cadr props))
                     (expr (cond
                            ((consp example) example)
                            ((stringp example) (read example)))))
                (should (shortdoc-tests--tree-contains expr fun))))
            (setq props (cddr props))))))))

(provide 'shortdoc-tests)

;;; shortdoc-tests.el ends here
