;;; makesum-tests.el --- Tests for makesum.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Simen Heggestøyl <simenheg@gmail.com>
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'makesum)

(ert-deftest makesum-tests-double-column-even-lines ()
  (with-temp-buffer
    (insert "a\nb\nc\nd\ne\nf")
    (double-column (point-min) (point-max))
    (should (string-match-p "a[ \t]+d\nb[ \t]+e\nc[ \t]+f" (buffer-string)))))

(ert-deftest makesum-tests-double-column-odd-lines ()
  (with-temp-buffer
    (insert "a\nb\nc\nd\ne")
    (double-column (point-min) (point-max))
    (should (string-match-p "a[ \t]+d\nb[ \t]+e\nc" (buffer-string)))))

(ert-deftest makesum-tests-double-column-noop ()
  (with-temp-buffer
    (insert "foo")
    (let ((prev-buffer-string (buffer-string)))
      (double-column (point-min) (point-max))
      (should (equal prev-buffer-string (buffer-string))))))

(ert-deftest makesum-tests-double-column-partial ()
  (with-temp-buffer
    (insert "a\nb\nc\nd\ne\nf")
    (double-column 3 10)
    (should (string-match-p "a\nb[ \t]+d\nc[ \t]+e\nf" (buffer-string)))))

(provide 'makesum-tests)
;;; makesum-tests.el ends here
