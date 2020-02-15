;;; url-file-tests.el --- Test suite for url-file. -*- lexical-binding: t -*-

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

(require 'url-file)
(require 'ert)

(defconst url-file-tests-data-directory
  (expand-file-name "lisp/url/url-file-resources"
                    (or (getenv "EMACS_TEST_DIRECTORY")
                        (expand-file-name "../../.."
                                          (or load-file-name
                                              buffer-file-name))))
  "Directory for url-file test files.")

(ert-deftest url-file ()
  "Test reading file via file:/// URL."
  (let* ((file (expand-file-name "file.txt" url-file-tests-data-directory))
         (uri-prefix (if (eq (aref file 0) ?/) "file://" "file:///")))
    (should (equal
             (with-current-buffer
                 (url-file (url-generic-parse-url (concat uri-prefix file))
                           #'ignore nil)
               (prog1 (buffer-substring (point) (point-max))
                 (kill-buffer)))
             (with-temp-buffer
               (insert-file-contents-literally file)
               (buffer-string))))))

(provide 'url-file-tests)

;;; url-file-tests.el ends here
