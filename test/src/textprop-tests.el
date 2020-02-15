;;; textprop-tests.el --- Test suite for text properties.

;; Copyright (C) 2015-2020 Free Software Foundation, Inc.

;; Author: Wolfgang Jenkner <wjenkner@inode.at>
;; Keywords: internal

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

(ert-deftest textprop-tests-format ()
  "Test `format' with text properties."
  ;; See Bug#21351.
  (should (equal-including-properties
           (format #("mouse-1, RET: %s -- w: copy %s"
                     12 20 (face minibuffer-prompt)
                     21 30 (face minibuffer-prompt))
                   "visit" "link")
           #("mouse-1, RET: visit -- w: copy link"
             12 23 (face minibuffer-prompt)
             24 35 (face minibuffer-prompt)))))

(ert-deftest textprop-tests-font-lock--remove-face-from-text-property ()
  "Test `font-lock--remove-face-from-text-property'."
  (let* ((string "foobar")
	 (stack (list string))
	 (faces '(bold (:foreground "red") underline)))
    ;; Build each string in `stack' by adding a face to the previous
    ;; string.
    (let ((faces (reverse faces)))
      (push (copy-sequence (car stack)) stack)
      (put-text-property 0 3 'font-lock-face (pop faces) (car stack))
      (push (copy-sequence (car stack)) stack)
      (put-text-property 3 6 'font-lock-face (pop faces) (car stack))
      (push (copy-sequence (car stack)) stack)
      (font-lock-prepend-text-property 2 5
				       'font-lock-face (pop faces) (car stack)))
    ;; Check that removing the corresponding face from each string
    ;; yields the previous string in `stack'.
    (while faces
      ;; (message "%S" (car stack))
      (should (equal-including-properties
	       (progn
		 (font-lock--remove-face-from-text-property 0 6
							    'font-lock-face
							    (pop faces)
							    (car stack))
		 (pop stack))
	       (car stack))))
    ;; Sanity check.
    ;; (message "%S" (car stack))
    (should (and (equal-including-properties (pop stack) string)
		 (null stack)))))

(provide 'textprop-tests)
;; textprop-tests.el ends here.
