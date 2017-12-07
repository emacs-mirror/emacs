;;; arc-mode-tests.el --- Test suite for arc-mode. -*- lexical-binding: t -*-

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
(require 'arc-mode)


(ert-deftest arc-mode-test-archive-int-to-mode ()
  (let ((alist (list (cons 448 "-rwx------")
                     (cons 420 "-rw-r--r--")
                     (cons 292 "-r--r--r--")
                     (cons 512 "----------")
                     (cons 1024 "------S---") ; Bug#28092
                     (cons 2048 "---S------"))))
    (dolist (x alist)
      (should (equal (cdr x) (archive-int-to-mode (car x)))))))

(provide 'arc-mode-tests)

;; arc-mode-tests.el ends here
