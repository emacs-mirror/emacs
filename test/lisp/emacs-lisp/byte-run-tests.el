;;; byte-run-tests.el --- Tests for byte-run.el  -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

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

(ert-deftest make-obsolete ()
  (should-error (make-obsolete nil 'foo "30.1"))
  (should-error (make-obsolete t 'foo "30.1") ))

(ert-deftest make-obsolete-variable ()
  (should-error (make-obsolete-variable nil 'foo "30.1"))
  (should-error (make-obsolete-variable t 'foo "30.1")))

;;; byte-run-tests.el ends here
