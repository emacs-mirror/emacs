;;; emacs-news-mode-tests.el --- Tests for emacs-news-mode.el  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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
(require 'ert-x)
(require 'emacs-news-mode)

(ert-deftest emacs-news-cycle-tag ()
  (ert-test-erts-file (ert-resource-file "cycle-tag.erts")
                      (lambda ()
                        (emacs-news-mode)
                        (emacs-news-cycle-tag))))

;;; emacs-news-mode-tests.el ends here
