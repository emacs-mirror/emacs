;;; cl-preloaded-tests.el --- unit tests for cl-preloaded.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.
;; Author: Philipp Stephani <phst@google.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit tests for lisp/emacs-lisp/cl-preloaded.el.

;;; Code:

(ert-deftest cl-struct-define/builtin-type ()
  (should-error
   (cl-struct-define 'hash-table nil nil 'record nil nil
                     'cl-preloaded-tests-tag 'cl-preloaded-tests nil)
   :type 'wrong-type-argument))

;;; cl-preloaded-tests.el ends here
