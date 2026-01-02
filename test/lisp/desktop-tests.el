;;; desktop-tests.el --- Tests for desktop.el  -*- lexical-binding: t -*-

;; Copyright (C) 2020, 2023-2026 Free Software Foundation, Inc.

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

(require 'ert)
(require 'desktop)

(ert-deftest desktop-tests--emacs-pid-running-p ()
  (should (desktop--emacs-pid-running-p (emacs-pid)))
  (should-not (desktop--emacs-pid-running-p 1)))

(ert-deftest desktop-tests--load-locked-desktop-p ()
  (let ((desktop-load-locked-desktop t))
    (should (desktop--load-locked-desktop-p (emacs-pid)))))

(ert-deftest desktop-tests--load-locked-desktop-p-nil ()
  (let ((desktop-load-locked-desktop nil))
    (should-not (desktop--load-locked-desktop-p (emacs-pid)))))

(ert-deftest desktop-tests--load-locked-desktop-p-ask ()
 (let ((desktop-load-locked-desktop 'ask))
   (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
     (should (desktop--load-locked-desktop-p (emacs-pid))))
   (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
     (should-not (desktop--load-locked-desktop-p (emacs-pid))))))

(ert-deftest desktop-tests--load-locked-desktop-p-check ()
  (let ((desktop-load-locked-desktop 'check-pid))
    (desktop--load-locked-desktop-p (emacs-pid))))

(provide 'desktop-tests)
