;;; search-tests.el --- tests for search.c functions -*- lexical-binding: t -*-

;; Copyright (C) 2015-2016, 2018-2024 Free Software Foundation, Inc.

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

(ert-deftest test-replace-match-modification-hooks ()
  (let ((ov-set nil))
    (with-temp-buffer
      (insert "1 abc")
      (setq ov-set (make-overlay 3 5))
      (overlay-put
       ov-set 'modification-hooks
       (list (lambda (_o after &rest _args)
	       (when after
		 (let ((inhibit-modification-hooks t))
		   (save-excursion
		     (goto-char 2)
		     (insert "234")))))))
      (goto-char 3)
      (if (search-forward "bc")
	  (replace-match "bcd"))
      (should (= (point) 10)))))

;;; search-tests.el ends here
