;;; fortune-tests.el --- Tests for fortune.el  -*- lexical-binding: t -*-

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

;;; Commentary:

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'fortune)

(defvar fortune-tests--regexp
  (rx (| "Embarrassed" "Embarrassingly")))

(ert-deftest test-fortune ()
  (skip-unless (executable-find "fortune"))
  (unwind-protect
      (let ((fortune-file (ert-resource-file "fortunes")))
        (fortune)
        (goto-char (point-min))
        (should (looking-at fortune-tests--regexp)))
    (kill-buffer fortune-buffer-name)))

(provide 'fortune-tests)
;;; fortune-tests.el ends here
