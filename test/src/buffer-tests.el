;;; buffer-tests.el --- tests for buffer.c functions -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017 Free Software Foundation, Inc.

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

(ert-deftest overlay-modification-hooks-message-other-buf ()
  "Test for bug#21824.
After a modification-hook has been run and there is an overlay in
the *Messages* buffer, the message coalescing [2 times] wrongly
runs the modification-hook of the overlay in the 1st buffer, but
with parameters from the *Messages* buffer modification."
  (let ((buf nil)
        (msg-ov nil))
    (with-temp-buffer
      (insert "123")
      (overlay-put (make-overlay 1 3)
                   'modification-hooks
                   (list (lambda (&rest _)
                           (setq buf (current-buffer)))))
      (goto-char 2)
      (insert "x")
      (unwind-protect
          (progn
            (setq msg-ov (make-overlay 1 1 (get-buffer-create "*Messages*")))
            (message "a message")
            (message "a message")
            (should (eq buf (current-buffer))))
        (when msg-ov (delete-overlay msg-ov))))))

(ert-deftest test-generate-new-buffer-name-bug27966 ()
  (should-not (string-equal "nil"
                            (progn (get-buffer-create "nil")
                                   (generate-new-buffer-name "nil")))))

;;; buffer-tests.el ends here
