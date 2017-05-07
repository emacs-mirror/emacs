;;; tutorial-tests.el --- Test suite for tutorial  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2017 Free Software Foundation, Inc.

;; Author: Phillip Lord <phillip.lord@russet.org.uk>
;; Keywords: abbrevs

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

;;; Code:

(require 'ert)
(require 'tutorial)
(require 'assess)

(ert-deftest tutorial--open-tutorial ()
  ;; We do not care about the return value (which happens to be nil),
  ;; but it should not error.
  (should
   (assess-with-preserved-buffer-list
    (let ((current-language-environment "Russian"))
      (help-with-tutorial)
      (remove-hook 'kill-buffer-hook 'tutorial--save-tutorial t)
      t))))

(ert-deftest tutorial--open-org-tutorial ()
  :expected-result :failed
  (should
   (assess-with-preserved-buffer-list
    (let ((current-language-environment "English"))
      (help-with-tutorial)
      (remove-hook 'kill-buffer-hook 'tutorial--save-tutorial t)
      t))))



(provide 'tutorial-tests)
;;; tutorial-tests.el ends here
