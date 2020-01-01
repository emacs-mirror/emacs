;;; url-misc-tests.el --- Test suite for url-misc. -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Free Software Foundation, Inc.

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

(require 'url-misc)
(require 'ert)

(ert-deftest url-misc-data ()
  "Test reading data: URL."
  (should (equal
           (with-current-buffer
               (url-data (url-generic-parse-url "data:;,some%20text"))
             (goto-char (point-min))
             (forward-paragraph)
             (forward-line)
             (prog1 (buffer-substring (point) (point-max))
               (kill-buffer)))
           "some text")))

(provide 'url-misc-tests)

;;; url-misc-tests.el ends here
