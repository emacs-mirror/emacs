;;; faces-test-dark-theme.el --- A dark theme from tests ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(deftheme faces-test-dark
  "")

(custom-theme-set-faces
 'faces-test-dark
 '(spiff-added ((t (:foreground "Green" :extend t))))
 '(spiff-changed-face ((t (:foreground "Khaki"))))
 '(spiff-file-header-face ((t (:background "grey20" :foreground "ivory1")))))

(provide-theme 'faces-test-dark)

;;; faces-test-dark-theme.el ends here
