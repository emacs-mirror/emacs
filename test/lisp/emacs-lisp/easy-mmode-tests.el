;;; easy-mmode-tests.el --- tests for easy-mmode.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Free Software Foundation, Inc.

;; Author: Philipp Stephani <phst@google.com>

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

;; Unit tests for lisp/emacs-lisp/easy-mmode.el.

;;; Code:

(define-minor-mode easy-mmode-tests--mode nil)

(ert-deftest easy-mmode-tests--modefun-nil ()
  (let (easy-mmode-tests--mode)
    (easy-mmode-tests--mode)
    (should easy-mmode-tests--mode)))

(ert-deftest easy-mmode-tests--modefun-0 ()
  (let ((easy-mmode-tests--mode nil))
    (easy-mmode-tests--mode)
    (easy-mmode-tests--mode 0)
    (should-not easy-mmode-tests--mode)))

(ert-deftest easy-mmode-tests--modefun-+1 ()
  (let ((easy-mmode-tests--mode nil))
    (easy-mmode-tests--mode 1)
    (should easy-mmode-tests--mode)))

(ert-deftest easy-mmode-tests--modefun--1 ()
  (let ((easy-mmode-tests--mode nil))
    (easy-mmode-tests--mode)
    (easy-mmode-tests--mode -1)
    (should-not easy-mmode-tests--mode)))

(ert-deftest easy-mmode-tests--modefun-toggle ()
  (let ((easy-mmode-tests--mode nil))
    (easy-mmode-tests--mode 'toggle)
    (should easy-mmode-tests--mode)
    (easy-mmode-tests--mode 'toggle)
    (should-not easy-mmode-tests--mode)))

(ert-deftest easy-mmode-tests--modefun-off ()
  (let ((easy-mmode-tests--mode nil))
    (easy-mmode-tests--mode 'off)
    (should easy-mmode-tests--mode)))

(ert-deftest easy-mmode-tests--modefun-t ()
  (let ((easy-mmode-tests--mode nil))
    (easy-mmode-tests--mode t)
    (should easy-mmode-tests--mode)))

;;; easy-mmode-tests.el ends here
