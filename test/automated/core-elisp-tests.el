;;; core-elisp-tests.el --- Testing some core Elisp rules

;; Copyright (C) 2013-2014 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(ert-deftest core-elisp-tests ()
  "Test some core Elisp rules."
  (with-temp-buffer
    ;; Check that when defvar is run within a let-binding, the toplevel default
    ;; is properly initialized.
    (should (equal (list (let ((c-e-x 1)) (defvar c-e-x 2) c-e-x) c-e-x)
                   '(1 2)))
    (should (equal (list (let ((c-e-x 1))
                           (defcustom c-e-x 2 "doc" :group 'blah) c-e-x)
                         c-e-x)
                   '(1 2)))))

(ert-deftest core-elisp-test-window-configurations ()
  "Test properties of window-configurations."
  (let ((wc (current-window-configuration)))
    (with-current-buffer (window-buffer (frame-selected-window))
      (push-mark)
      (activate-mark))
    (set-window-configuration wc)
    (should (or (not mark-active) (mark)))))

(provide 'core-elisp-tests)
;;; core-elisp-tests.el ends here
