;;; buff-menu-tests.el --- Test suite for buff-menu.el -*- lexical-binding: t -*-

;; Copyright (C) 2016 Free Software Foundation, Inc.

;; Author: Tino Calancha <tino.calancha@gmail.com>

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert)

(ert-deftest buff-menu-24962 ()
  "Test for http://debbugs.gnu.org/24962 ."
  (let ((file (expand-file-name "foo" temporary-file-directory))
        buf)
    (unwind-protect
        (progn
          (write-region "foo" nil file)
          (setq buf (find-file file))
          (rename-buffer " foo")
          (list-buffers)
          (with-current-buffer "*Buffer List*"
            (should (string= " foo" (buffer-name (Buffer-menu-buffer))))))
      (and (buffer-live-p buf) (kill-buffer buf))
      (and (file-exists-p file) (delete-file file)))))

(provide 'buff-menu-tests)

;;; buff-menu-tests.el ends here
