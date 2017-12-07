;;; decompress-tests.el --- Test suite for decompress.

;; Copyright (C) 2013-2017 Free Software Foundation, Inc.

;; Author: Lars Ingebrigtsen <larsi@gnus.org>

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

(defvar zlib-tests-data-directory
  (expand-file-name "data/decompress" (getenv "EMACS_TEST_DIRECTORY"))
  "Directory containing zlib test data.")

(ert-deftest zlib--decompress ()
  "Test decompressing a gzipped file."
  (when (and (fboundp 'zlib-available-p)
	     (zlib-available-p))
    (should (string=
	     (with-temp-buffer
	       (set-buffer-multibyte nil)
	       (insert-file-contents-literally
		(expand-file-name "foo.gz" zlib-tests-data-directory))
	       (zlib-decompress-region (point-min) (point-max))
	       (buffer-string))
	     "foo\n"))))

(provide 'decompress-tests)

;;; decompress-tests.el ends here.
