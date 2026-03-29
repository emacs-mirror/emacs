;;; winner-tests.el --- Tests for winner.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

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

(require 'winner)

(ert-deftest winner-dont-bind-my-keys ()
  (should (keymap-lookup winner-mode-map "C-c <left>"))
  (setopt winner-dont-bind-my-keys t)
  (should-not (keymap-lookup winner-mode-map "C-c <left>"))
  (setopt winner-dont-bind-my-keys nil)
  (should (keymap-lookup winner-mode-map "C-c <left>")))

(provide 'winner-tests)
;;; winner-tests.el ends here
