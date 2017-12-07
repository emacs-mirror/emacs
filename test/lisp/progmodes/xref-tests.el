;;; xref-tests.el --- tests for xref

;; Copyright (C) 2016-2017 Free Software Foundation, Inc.

;; Author: Dmitry Gutov <dgutov@yandex.ru>

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

;;; Code:

(require 'xref)
(require 'cl-lib)

(defvar xref-tests-data-dir
  (expand-file-name "data/xref/"
                    (getenv "EMACS_TEST_DIRECTORY")))

(ert-deftest xref-collect-matches-finds-none-for-some-regexp ()
  (should (null (xref-collect-matches "zzz" "*" xref-tests-data-dir nil))))

(ert-deftest xref-collect-matches-finds-some-for-bar ()
  (let* ((matches (xref-collect-matches "bar" "*" xref-tests-data-dir nil))
         (locs (cl-sort (mapcar #'xref-item-location matches)
                        #'string<
                        :key #'xref-location-group)))
    (should (= 2 (length matches)))
    (should (string-match-p "file1\\.txt\\'" (xref-location-group (nth 0 locs))))
    (should (string-match-p "file2\\.txt\\'" (xref-location-group (nth 1 locs))))))

(ert-deftest xref-collect-matches-finds-two-matches-on-the-same-line ()
  (let* ((matches (xref-collect-matches "foo" "*" xref-tests-data-dir nil))
         (locs (mapcar #'xref-item-location matches)))
    (should (= 2 (length matches)))
    (should (string-match-p "file1\\.txt\\'" (xref-location-group (nth 0 locs))))
    (should (string-match-p "file1\\.txt\\'" (xref-location-group (nth 1 locs))))
    (should (equal 1 (xref-location-line (nth 0 locs))))
    (should (equal 1 (xref-location-line (nth 1 locs))))
    (should (equal 0 (xref-file-location-column (nth 0 locs))))
    (should (equal 4 (xref-file-location-column (nth 1 locs))))))

(ert-deftest xref-collect-matches-finds-an-empty-line-regexp-match ()
  (let* ((matches (xref-collect-matches "^$" "*" xref-tests-data-dir nil))
         (locs (mapcar #'xref-item-location matches)))
    (should (= 1 (length matches)))
    (should (string-match-p "file2\\.txt\\'" (xref-location-group (nth 0 locs))))
    (should (equal 1 (xref-location-line (nth 0 locs))))
    (should (equal 0 (xref-file-location-column (nth 0 locs))))))

(ert-deftest xref--buf-pairs-iterator-groups-markers-by-buffers-1 ()
  (let* ((xrefs (xref-collect-matches "foo" "*" xref-tests-data-dir nil))
         (iter (xref--buf-pairs-iterator xrefs))
         (cons (funcall iter :next)))
    (should (null (funcall iter :next)))
    (should (string-match "file1\\.txt\\'" (buffer-file-name (car cons))))
    (should (= 2 (length (cdr cons))))))

(ert-deftest xref--buf-pairs-iterator-groups-markers-by-buffers-2 ()
  (let* ((xrefs (xref-collect-matches "bar" "*" xref-tests-data-dir nil))
         (iter (xref--buf-pairs-iterator xrefs))
         (cons1 (funcall iter :next))
         (cons2 (funcall iter :next)))
    (should (null (funcall iter :next)))
    (should-not (equal (car cons1) (car cons2)))
    (should (= 1 (length (cdr cons1))))
    (should (= 1 (length (cdr cons2))))))

(ert-deftest xref--buf-pairs-iterator-cleans-up-markers ()
  (let* ((xrefs (xref-collect-matches "bar" "*" xref-tests-data-dir nil))
         (iter (xref--buf-pairs-iterator xrefs))
         (cons1 (funcall iter :next))
         (cons2 (funcall iter :next)))
    (funcall iter :cleanup)
    (should (null (marker-position (car (nth 0 (cdr cons1))))))
    (should (null (marker-position (cdr (nth 0 (cdr cons1))))))
    (should (null (marker-position (car (nth 0 (cdr cons2))))))
    (should (null (marker-position (cdr (nth 0 (cdr cons2))))))))
