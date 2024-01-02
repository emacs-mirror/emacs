;;; hfy-cmap-tests.el --- tests for hfy-cmap.el -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

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
(require 'hfy-cmap)

(defconst hfy-cmap-tests--data
  (concat "255 250 250		snow\n"
          "248 248 255		ghost white\n"
          "248 248 255		GhostWhite\n"))

(defconst hfy-cmap-tests--parsed
  '(("GhostWhite" 248 248 255)
    ("ghost white" 248 248 255)
    ("snow" 255 250 250)))

(ert-deftest test-hfy-cmap--parse-buffer ()
  (with-temp-buffer
    (insert hfy-cmap-tests--data)
    (should (equal (hfy-cmap--parse-buffer (current-buffer))
                   hfy-cmap-tests--parsed))))

(ert-deftest test-htmlfontify-load-rgb-file ()
  :tags '(:expensive-test)
  (let (hfy-rgb-txt-color-map)
    (htmlfontify-load-rgb-file (ert-resource-file "rgb.txt"))
    (should (equal hfy-rgb-txt-color-map
                   hfy-cmap-tests--parsed))))

(ert-deftest test-htmlfontify-load-rgb-file/non-existent-file ()
  (let (hfy-rgb-txt-color-map)
    (htmlfontify-load-rgb-file "/non/existent/file")
    (should-not hfy-rgb-txt-color-map)))

(provide 'hfy-cmap-tests)
;;; hfy-cmap-tests.el ends here
