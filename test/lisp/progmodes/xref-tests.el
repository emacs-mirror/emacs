;;; xref-tests.el --- tests for xref  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2021 Free Software Foundation, Inc.

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

(require 'ert)
(require 'xref)
(require 'cl-lib)

(defvar xref-tests--data-dir
  (expand-file-name "xref-resources/"
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(defun xref-tests--matches-in-data-dir (regexp &optional files)
  (xref-matches-in-directory regexp (or files "*") xref-tests--data-dir nil))

(defun xref-tests--locations-in-data-dir (regexp &optional files)
  (let ((matches (xref-tests--matches-in-data-dir regexp files)))
    ;; Sort in order to guarantee an order independent from the
    ;; filesystem traversal.
    (cl-sort (mapcar #'xref-item-location matches)
             #'string<
             :key #'xref-location-group)))

(ert-deftest xref-matches-in-directory-finds-none-for-some-regexp ()
  (should (null (xref-tests--matches-in-data-dir "zzz"))))

(ert-deftest xref-matches-in-directory-finds-some-for-bar ()
  (let ((locs (xref-tests--locations-in-data-dir "bar")))
    (should (= 2 (length locs)))
    (should (string-match-p "file1\\.txt\\'" (xref-location-group (nth 0 locs))))
    (should (string-match-p "file2\\.txt\\'" (xref-location-group (nth 1 locs))))))

(ert-deftest xref-matches-in-directory-finds-two-matches-on-the-same-line ()
  (let ((locs (xref-tests--locations-in-data-dir "foo")))
    (should (= 2 (length locs)))
    (should (string-match-p "file1\\.txt\\'" (xref-location-group (nth 0 locs))))
    (should (string-match-p "file1\\.txt\\'" (xref-location-group (nth 1 locs))))
    (should (equal 1 (xref-location-line (nth 0 locs))))
    (should (equal 1 (xref-location-line (nth 1 locs))))
    (should (equal 0 (xref-location-column (nth 0 locs))))
    (should (equal 4 (xref-location-column (nth 1 locs))))))

(ert-deftest xref-matches-in-directory-finds-an-empty-line-regexp-match ()
  (let ((locs (xref-tests--locations-in-data-dir "^$")))
    (should (= 1 (length locs)))
    (should (string-match-p "file2\\.txt\\'" (xref-location-group (nth 0 locs))))
    (should (equal 1 (xref-location-line (nth 0 locs))))
    (should (equal 0 (xref-location-column (nth 0 locs))))))

(ert-deftest xref--buf-pairs-iterator-groups-markers-by-buffers-1 ()
  (let* ((xrefs (xref-tests--matches-in-data-dir "foo"))
         (iter (xref--buf-pairs-iterator xrefs))
         (cons (funcall iter :next)))
    (should (null (funcall iter :next)))
    (should (string-match "file1\\.txt\\'" (buffer-file-name (car cons))))
    (should (= 2 (length (cdr cons))))))

(ert-deftest xref--buf-pairs-iterator-groups-markers-by-buffers-2 ()
  (let* ((xrefs (xref-tests--matches-in-data-dir "bar"))
         (iter (xref--buf-pairs-iterator xrefs))
         (cons1 (funcall iter :next))
         (cons2 (funcall iter :next)))
    (should (null (funcall iter :next)))
    (should-not (equal (car cons1) (car cons2)))
    (should (= 1 (length (cdr cons1))))
    (should (= 1 (length (cdr cons2))))))

(ert-deftest xref--buf-pairs-iterator-cleans-up-markers ()
  (let* ((xrefs (xref-tests--matches-in-data-dir "bar"))
         (iter (xref--buf-pairs-iterator xrefs))
         (cons1 (funcall iter :next))
         (cons2 (funcall iter :next)))
    (funcall iter :cleanup)
    (should (null (marker-position (car (nth 0 (cdr cons1))))))
    (should (null (marker-position (cdr (nth 0 (cdr cons1))))))
    (should (null (marker-position (car (nth 0 (cdr cons2))))))
    (should (null (marker-position (cdr (nth 0 (cdr cons2))))))))

(ert-deftest xref--xref-file-name-display-is-abs ()
  (let ((xref-file-name-display 'abs)
        ;; Some older BSD find versions can produce '//' in the output.
        (expected (list
                   (concat xref-tests--data-dir "/?file1.txt")
                   (concat xref-tests--data-dir "/?file2.txt")))
        (actual (delete-dups
                 (mapcar 'xref-location-group
                         (xref-tests--locations-in-data-dir "\\(bar\\|foo\\)")))))
    (should (and (= (length expected) (length actual))
                 (cl-every (lambda (e1 e2)
                             (string-match-p e1 e2))
                           expected actual)))))

(ert-deftest xref--xref-file-name-display-is-nondirectory ()
  (let ((xref-file-name-display 'nondirectory))
    (should (equal (delete-dups
                    (mapcar 'xref-location-group
                            (xref-tests--locations-in-data-dir "\\(bar\\|foo\\)")))
                   (list
                    "file1.txt"
                    "file2.txt")))))

(ert-deftest xref--xref-file-name-display-is-relative-to-project-root ()
  (let* ((data-parent-dir
          (file-name-directory (directory-file-name xref-tests--data-dir)))
         (project-find-functions
          #'(lambda (_) (cons 'transient data-parent-dir)))
         (xref-file-name-display 'project-relative)
         ;; Some older BSD find versions can produce '//' in the output.
         (expected (list
                    "xref-resources//?file1.txt"
                    "xref-resources//?file2.txt"))
         (actual (delete-dups
                  (mapcar 'xref-location-group
                          (xref-tests--locations-in-data-dir "\\(bar\\|foo\\)")))))
    (should (and (= (length expected) (length actual))
                 (cl-every (lambda (e1 e2)
                             (string-match-p e1 e2))
                           expected actual)))))
