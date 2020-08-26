;;; shorthand-tests.el --- Tests for shorthand.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords:

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

;;; Code:

(require 'shorthand)
(require 'cl-lib)
(require 'ert)

(ert-deftest shorthand-read-buffer ()
  (let* ((gsym (downcase (symbol-name (cl-gensym "sh-"))))
         (shorthand-sname (format "s-%s" gsym))
         (expected (intern (format "shorthand-longhand-%s" gsym))))
    (cl-assert (not (intern-soft shorthand-sname)))
    (should (equal (let ((shorthand-shorthands
                          '(("^s-" . "shorthand-longhand-"))))
                     (with-temp-buffer
                       (insert shorthand-sname)
                       (goto-char (point-min))
                       (read (current-buffer))))
                   expected))
    (should (not (intern-soft shorthand-sname)))))

(ert-deftest shorthand-read-from-string ()
  (let* ((gsym (downcase (symbol-name (cl-gensym "sh-"))))
         (shorthand-sname (format "s-%s" gsym))
         (expected (intern (format "shorthand-longhand-%s" gsym))))
    (cl-assert (not (intern-soft shorthand-sname)))
    (should (equal (let ((shorthand-shorthands
                          '(("^s-" . "shorthand-longhand-"))))
                     (car (read-from-string shorthand-sname)))
                   expected))
    (should (not (intern-soft shorthand-sname)))))


(provide 'shorthand-tests)
;;; shorthand-tests.el ends here
