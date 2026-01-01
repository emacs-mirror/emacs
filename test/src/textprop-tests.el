;;; textprop-tests.el --- Test suite for text properties. -*- lexical-binding: t -*-

;; Copyright (C) 2015-2026 Free Software Foundation, Inc.

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

(ert-deftest textprop-interval-immutability ()
  "Test modification of text with properties affecting mutability."
  (let ((template (concat
                   (propertize "12345" 'inhibit-read-only t) ; 1-5
                   (propertize "67890" 'read-only 'abcdefg)  ; 6-10
                   (propertize "ABCDE" 'inhibit-read-only t) ; 11-15
                   (propertize "FGHIJ" 'inhibit-read-only 'yes) ; 16-20
                   "KLMNO" ; 21-25
                   (propertize "PQRST" 'inhibit-read-only 't) ; 26-30
                   (propertize "UVWXYZ" 'read-only 'not-suppressed)))
        inhibit-read-only)
    (with-temp-buffer
      (insert template)
      (setq buffer-read-only t)
      ;; Delete an entire inhibit-read-only region.
      (progn (should (equal (delete-and-extract-region 1 6)
                            "12345"))
             (let ((inhibit-read-only t)) (erase-buffer)
                  (insert template)))
      ;; Delete multiple characters inside an inhibit-read-only section.
      (progn (should (equal (delete-and-extract-region 2 5)
                            "234"))
             (let ((inhibit-read-only t)) (erase-buffer)
                  (insert template)))
      ;; Attempt to delete characters across both an inhibit-read-only
      ;; and a read only region.
      (setq buffer-read-only nil)
      (should-error (delete-and-extract-region 4 7))
      (setq inhibit-read-only '(abcdefg))
      ;; Attempt the same, but with the read-only property of the second
      ;; section suppressed.
      (progn (should (equal (delete-and-extract-region 4 7) "456"))
             (let ((inhibit-read-only t)) (erase-buffer)
                  (insert template)))
      (setq buffer-read-only t)
      ;; Delete text across the suppressed read-only region and two
      ;; other inhibit-read-only regions each with distinct intervals.
      (progn (should (equal (delete-and-extract-region 7 17)
                            "7890ABCDEF"))
             (let ((inhibit-read-only t)) (erase-buffer)
                  (insert template)))
      (setq inhibit-read-only nil)
      ;; Attempt to delete text spanning two inhibit-read-only sections
      ;; separated by immutable text.
      (should-error (delete-and-extract-region 17 27))
      (setq inhibit-read-only '(abcdefg))
      ;; Attempt to delete text from the start of an inhibit-read-only
      ;; section extending into protected text exempt from
      ;; `inhibit-read-only''s influence towards the end of the buffer.
      (should-error (delete-and-extract-region 26 37)))))

(provide 'textprop-tests)
;;; textprop-tests.el ends here
