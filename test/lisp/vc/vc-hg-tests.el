;;; vc-hg-tests.el --- tests for vc/vc-hg.el

;; Copyright (C) 2016-2017 Free Software Foundation, Inc.

;; Author: Dmitry Gutov <dgutov@yandex.ru>
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'vc-hg)
(require 'vc-annotate)

(ert-deftest vc-hg-annotate-extract-revision-at-line-with-filename ()
  ;; with filename
  (with-temp-buffer
    (save-excursion (insert "215 2007-06-20 CONTENTS:"))
    (should (equal (vc-hg-annotate-extract-revision-at-line)
                   (cons
                    "215"
                    (expand-file-name "CONTENTS"))))))

(ert-deftest vc-hg-annotate-extract-revision-at-line-with-user ()
  (with-temp-buffer
    (save-excursion (insert " gerv 107217 2012-09-17:"))
    (should (equal (vc-hg-annotate-extract-revision-at-line)
                   "107217"))))

(ert-deftest vc-hg-annotate-extract-revision-at-line-with-both ()
  (with-temp-buffer
    (save-excursion (insert "philringnalda 218075 2014-11-28   CLOBBER:"))
    (should (equal (vc-hg-annotate-extract-revision-at-line)
                   (cons
                    "218075"
                    (expand-file-name "CLOBBER"))))))

(ert-deftest vc-hg-annotate-time ()
  (with-temp-buffer
    (save-excursion (insert "philringnalda 218075 2014-11-28 CLOBBER:"))
    (should (floatp (vc-hg-annotate-time)))))

;;; vc-hg-tests.el ends here
