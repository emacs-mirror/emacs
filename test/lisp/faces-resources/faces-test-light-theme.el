;;; faces-test-light-theme.el --- A dark theme from tests ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023 Free Software Foundation, Inc.

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

(deftheme faces-test-light
  "Light test theme.")

(custom-theme-set-faces
 'faces-test-light
 '(spiff-added ((t (:inherit diff-changed :background "light green" :extend t))))
 '(spiff-changed ((t (:background "light steel blue")))))

(provide-theme 'faces-test-light)

;;; faces-test-light-theme.el ends here
