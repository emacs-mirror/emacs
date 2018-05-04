;;; faceup-test-files.el --- Self test of `faceup' using dummy major mode.

;; Copyright (C) 2014-2018 Free Software Foundation, Inc.

;; Author: Anders Lindgren
;; Keywords: languages, faces

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

;; Self test of `faceup' with a major mode that sets both the
;; `syntax-table' and the `echo-help' property.
;;
;; This file can also be seen as a blueprint of test cases for real
;; major modes.

;;; Code:

(require 'faceup)

;; Note: The byte compiler needs the value to load `faceup-test-mode',
;; hence the `eval-and-compile'.
(eval-and-compile
  (defvar faceup-test-files-dir (faceup-this-file-directory)
    "The directory of this file."))

(require 'faceup-test-mode
         (concat faceup-test-files-dir
                 "../faceup-resources/"
                 "faceup-test-mode.el"))

(defun faceup-test-files-check-one (file)
  "Test that FILE is fontified as the .faceup file describes.

FILE is interpreted as relative to this source directory."
  (let ((faceup-properties '(face syntax-table help-echo)))
    (faceup-test-font-lock-file 'faceup-test-mode
                                (concat
                                 faceup-test-files-dir
                                 "../faceup-resources/"
                                 file))))
(faceup-defexplainer faceup-test-files-check-one)

(ert-deftest faceup-files ()
  (should (faceup-test-files-check-one "files/test1.txt")))

(provide 'faceup-test-files)

;;; faceup-test-files.el ends here
