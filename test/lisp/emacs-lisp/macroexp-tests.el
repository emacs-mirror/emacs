;;; macroexp-tests.el --- Tests for macroexp.el      -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(ert-deftest macroexp--tests-fgrep ()
  (should (equal (macroexp--fgrep '((x) (y)) '([x] z ((u))))
                 '((x))))
  (should (equal (macroexp--fgrep '((x) (y)) '#2=([y] ((y #2#))))
                 '((y))))
  (should (equal (macroexp--fgrep '((x) (y)) '#2=([r] ((a x)) a b c d . #2#))
                 '((x)))))

(defconst macroexp--tests-filename (macroexp-file-name))

(defmacro macroexp--test-get-file-name () (macroexp-file-name))

(ert-deftest macroexp--tests-file-name ()
  (should (string-match
           "\\`macroexp-tests.elc?\\'"
           (file-name-nondirectory macroexp--tests-filename)))
  (let ((rsrc-dir (expand-file-name
                   "macroexp-resources"
                   (file-name-directory macroexp--tests-filename))))
    (with-current-buffer
        (find-file-noselect (expand-file-name "m1.el" rsrc-dir))
      (defvar macroexp--m1-tests-filename)
      (declare-function macroexp--m1-tests-file-name "m1" ())
      ;; `macroexp-file-name' should work with `eval-buffer'.
      (eval-buffer)
      (should (equal "m1.el"
                     (file-name-nondirectory macroexp--m1-tests-filename)))
      (should (equal "m1.el"
                     (file-name-nondirectory (macroexp--m1-tests-file-name))))
      (search-forward "macroexp--m1-tests-filename")
      (makunbound 'macroexp--m1-tests-filename)
      ;; `macroexp-file-name' should also work with `eval-defun'.
      (eval-defun nil)
      (should (equal "m1.el"
                     (file-name-nondirectory macroexp--m1-tests-filename))))

    ;; Test the case where we load a file which byte-compiles another.
    (defvar macroexp--m1-tests-comp-filename)
    (makunbound 'macroexp--m1-tests-comp-filename)
    (load (expand-file-name "m2.el" rsrc-dir))
    (should (equal "m1.el"
                   (file-name-nondirectory macroexp--m1-tests-comp-filename)))))


(provide 'macroexp-tests)
;;; macroexp-tests.el ends here
