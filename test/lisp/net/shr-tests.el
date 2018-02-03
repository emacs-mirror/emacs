;;; network-stream-tests.el --- tests for network processes       -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018 Free Software Foundation, Inc.

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

;;; Commentary:

;;; Code:

(require 'shr)

(defconst shr-tests--datadir
  (expand-file-name "test/data/shr" source-directory))

(defun shr-test (name)
  (with-temp-buffer
    (insert-file-contents (format (concat shr-tests--datadir "/%s.html") name))
    (let ((dom (libxml-parse-html-region (point-min) (point-max)))
          (shr-width 80)
          (shr-use-fonts nil))
      (erase-buffer)
      (shr-insert-document dom)
      (cons (buffer-substring-no-properties (point-min) (point-max))
            (with-temp-buffer
              (insert-file-contents
               (format (concat shr-tests--datadir "/%s.txt") name))
              (while (re-search-forward "%\\([0-9A-F][0-9A-F]\\)" nil t)
                (replace-match (string (string-to-number (match-string 1) 16))
                               t t))
              (buffer-string))))))

(ert-deftest rendering ()
  (skip-unless (fboundp 'libxml-parse-html-region))
  (dolist (file (directory-files shr-tests--datadir nil "\\.html\\'"))
    (let* ((name (replace-regexp-in-string "\\.html\\'" "" file))
           (result (shr-test name)))
      (unless (equal (car result) (cdr result))
        (should (not (list name (car result) (cdr result))))))))

(require 'shr)

;;; shr-stream-tests.el ends here
