;;; info-tests.el --- Tests for info.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

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

(require 'info)
(require 'ert)
(require 'ert-x)

(ert-deftest test-info-urls ()
  (should (equal (Info-url-for-node "(tramp)Top")
                 "https://www.gnu.org/software/emacs/manual/html_node/tramp/"))
  (should (equal (Info-url-for-node "(emacs)Minibuffer")
                 "https://www.gnu.org/software/emacs/manual/html_node/emacs/Minibuffer.html"))
  (should (equal (Info-url-for-node "(emacs)Minibuffer File")
                 "https://www.gnu.org/software/emacs/manual/html_node/emacs/Minibuffer-File.html"))
  (should (equal (Info-url-for-node "(elisp)Backups and Auto-Saving")
                 "https://www.gnu.org/software/emacs/manual/html_node/elisp/Backups-and-Auto_002dSaving.html"))
  (should (equal (Info-url-for-node "(eintr)car & cdr")
                 "https://www.gnu.org/software/emacs/manual/html_node/eintr/car-_0026-cdr.html"))
  (should (equal (Info-url-for-node "(emacs-mime)\tIndex")
                 "https://www.gnu.org/software/emacs/manual/html_node/emacs-mime/Index.html"))
  (should (equal (Info-url-for-node "(gnus) Don't Panic")
                 "https://www.gnu.org/software/emacs/manual/html_node/gnus/Don_0027t-Panic.html"))
  (should-error (Info-url-for-node "(nonexistent)Example")))

;;; info-tests.el ends here
