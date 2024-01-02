;;; shortdoc-tests.el --- tests for shortdoc.el   -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

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
(require 'subr-x) ; `string-pad' in shortdoc group needed at run time
(require 'regexp-opt)    ; `regexp-opt-charset' not autoloaded

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

(ert-deftest shortdoc-all-functions-fboundp ()
  "Check that all functions listed in shortdoc groups are `fboundp'."
  (dolist (group shortdoc--groups)
    (dolist (item group)
      (when (consp item)
        (let ((fun (car item)))
          (should (fboundp fun)))))))

(ert-deftest shortdoc-all-groups-work ()
  "Test that all defined shortdoc groups display correctly."
  (dolist (group (mapcar (lambda (x) (car x)) shortdoc--groups))
    (let ((buf-name (format "*Shortdoc %s*" group)) buf)
      (unwind-protect
          (progn
            (shortdoc-display-group group)
            (should (setq buf (get-buffer buf-name))))
        (when buf
          (kill-buffer buf))))))

(provide 'shortdoc-tests)

;;; shortdoc-tests.el ends here
