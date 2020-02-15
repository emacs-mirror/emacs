;;; nnrss-tests.el --- tests for gnus/nnrss.el    -*- lexical-binding:t -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

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
(require 'nnrss)

(ert-deftest test-nnrss-normalize ()
  (should (equal (nnrss-normalize-date "2004-09-17T05:09:49.001+00:00")
                 "Fri, 17 Sep 2004 05:09:49 +0000")))

;;; nnrss-tests.el ends here
