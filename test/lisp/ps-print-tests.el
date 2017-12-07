;;; ps-print-tests.el --- Test suite for ps-print.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2017 Free Software Foundation, Inc.

;; Author: Phillip Lord <phillip.lord@russet.org.uk>

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
(require 'ps-print)
(require 'ert)

;;; Autoload tests
(ert-deftest ps-mule-autoload ()
  "Tests to see whether ps-mule has been autoloaded"
  (should
   (fboundp 'ps-mule-initialize))
  (should
   (autoloadp
    (symbol-function
     'ps-mule-initialize))))
