;;; vtable-tests.el --- Tests for vtable.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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

(require 'vtable)
(require 'ert)
(require 'ert-x)

(defun vtable-tests--make-no-header-2-object-table ()
  (make-vtable :columns '("a" "b" "c")
               :objects '(("foo" 1 2)
                          ("bar" 3 :zot))
               :insert nil))

(ert-deftest test-vtable-compute-columns ()
  (should
   (equal (mapcar
           (lambda (column)
             (vtable-column-align column))
           (vtable--compute-columns
            (vtable-tests--make-no-header-2-object-table)))
          '(left right left))))

(ert-deftest test-vtable-insert-object ()
  (should
   (equal (let ((buffer (get-buffer-create " *vtable-test*")))
            (pop-to-buffer buffer)
            (erase-buffer)
            (let* ((object1 '("Foo" 3))
                   (object2 '("Gazonk" 8))
                   (table (make-vtable
                           :columns '("Name" (:name "Rank" :width 5))
                           :objects (list object1 object2))))
              (mapc (lambda (args)
                      (pcase-let ((`(,object ,location ,before) args))
                        (vtable-insert-object table object location before)))
                    `( ; Some correct inputs.
                      ;; object    location        before
                      (("Fizz" 4)  ,object1        nil)
                      (("Bop"  7)  ,object2        t)
                      (("Zat"  5)  2               nil)
                      (("Dib"  6)  3               t)
                      (("Wup"  9)  nil             nil)
                      (("Quam" 2)  nil             t)
                      ;; And some faulty inputs.
                      (("Yat"  1)  -1              nil) ; non-existing index, `before' is ignored.
                      (("Vop"  10) 100             t)   ; non-existing index, `before' is ignored.
                      (("Jib"  11) ("Bleh"  0)     nil) ; non-existing object.
                      (("Nix"  0)  ("Ugh"   0)     t)   ; non-existing object.
                      ))
              (mapcar #'cadr (vtable-objects table))))
          (number-sequence 0 11))))

(ert-deftest test-vtable-unique-buffer ()
  (let ((table (vtable-tests--make-no-header-2-object-table)))
    (with-temp-buffer
      (vtable-insert table)
      (with-temp-buffer
        (should-error (vtable-insert table)))
      (with-temp-buffer
        (vtable-set-buffer table (current-buffer))
        (vtable-insert table)))))

(ert-deftest test-vtable-read-only-buffer ()
  (let ((table (vtable-tests--make-no-header-2-object-table)))
    (with-temp-buffer
      (setq buffer-read-only t)
      (vtable-insert table))))

(ert-deftest test-vtable-non-current-buffer-insert-object ()
  (let ((table (vtable-tests--make-no-header-2-object-table))
        (obj '("baz" 4 5)))
    (with-temp-buffer
      (vtable-insert table)
      (should (= (count-lines (point-min) (point-max)) 2))
      (with-temp-buffer
        (vtable-insert-object table obj))
      (should (= (count-lines (point-min) (point-max)) 3)))))

(ert-deftest test-vtable-non-current-buffer-remove-object ()
  (let ((table (vtable-tests--make-no-header-2-object-table))
        (obj '("baz" 4 5)))
    (with-temp-buffer
      (vtable-insert table)
      (vtable-insert-object table obj)
      (should (= (count-lines (point-min) (point-max)) 3))
      (with-temp-buffer
        (vtable-remove-object table obj))
      (should (= (count-lines (point-min) (point-max)) 2)))))

(ert-deftest test-vtable-non-current-buffer-update-object ()
  (let ((table (vtable-tests--make-no-header-2-object-table))
        (obj '("baz" 4 5))
        (obj-2 '("qux" 6 7)))
    (with-temp-buffer
      (vtable-insert table)
      (vtable-insert-object table obj)
      (should (= (count-lines (point-min) (point-max)) 3))
      (let ((line-2 (progn
                      (goto-char (point-min))
                      (forward-line 2)
                      (buffer-substring (point) (point-max)))))
        (with-temp-buffer
          (vtable-update-object table obj-2 obj))
        (let ((line-2-new (progn
                            (goto-char (point-min))
                            (forward-line 2)
                            (buffer-substring (point) (point-max)))))
          (should (= (count-lines (point-min) (point-max)) 3))
          (should (not (string= line-2 line-2-new))))))))

(ert-deftest test-vtable--limit-string-with-face-remapped-buffer ()
  (with-temp-buffer
    (let ((text (propertize "XXXXX"
                            'face 'variable-pitch)))
      (face-remap-add-relative 'default :height 1.5)
      ;; TODO: Remove the pre-31 test, eventually.
      (cond ((eval-when-compile (< emacs-major-version 31))
             (let* ((x-width (string-pixel-width (substring text 0 1)))
                    (char-limit 2)
                    (pixel-limit (* char-limit x-width)))
               (should (eq
                        char-limit
                        (length (vtable--limit-string text pixel-limit))))))
            (t
             (let* ((x-width (string-pixel-width (substring text 0 1) (current-buffer)))
                    (char-limit 2)
                    (pixel-limit (* char-limit x-width)))
               (should (eq
                        char-limit
                        (length (vtable--limit-string text pixel-limit (current-buffer)))))))))))

;;; vtable-tests.el ends here
