;;; ls-lisp-tests.el --- tests for ls-lisp.el  -*- lexical-binding: t-*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: Tino Calacha <tino.calancha@gmail.com>
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:


;;; Code:
(require 'ert)

(ert-deftest ls-lisp-unload ()
  "Test for http://debbugs.gnu.org/xxxxx ."
  (require 'ls-lisp)
  (should (advice-member-p 'ls-lisp--insert-directory 'insert-directory))
  (unload-feature 'ls-lisp 'force)
  (should-not (advice-member-p 'ls-lisp--insert-directory 'insert-directory)))

(provide 'ls-lisp-tests)
;;; ls-lisp-tests.el ends here
