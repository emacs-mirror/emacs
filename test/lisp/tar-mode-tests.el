;;; tar-mode-tests.el --- Test suite for tar-mode. -*- lexical-binding: t -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

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
(require 'tar-mode)


(ert-deftest tar-mode-test-tar-grind-file-mode ()
  (let ((alist (list (cons 448 "rwx------")
                     (cons 420 "rw-r--r--")
                     (cons 292 "r--r--r--")
                     (cons 512 "--------T")
                     (cons 1024 "-----S---"))))
    (dolist (x alist)
      (should (equal (cdr x) (tar-grind-file-mode (car x)))))))

(provide 'tar-mode-tests)

;; tar-mode-tests.el ends here
