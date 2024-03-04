;;; mwheel-tests.el --- tests for mwheel.el  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

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
(require 'mwheel)

(ert-deftest mwheel-test-enable/disable ()
  (mouse-wheel-mode 1)
  (should (eq (lookup-key (current-global-map) `[,mouse-wheel-up-event]) 'mwheel-scroll))
  (mouse-wheel-mode -1)
  (should (eq (lookup-key (current-global-map) `[,mouse-wheel-up-event]) nil)))

(ert-deftest mwheel-test--create-scroll-keys ()
  (should (equal (mouse-wheel--create-scroll-keys 10 'mouse-4)
                 '([mouse-4]
                   [left-margin mouse-4] [right-margin mouse-4]
                   [left-fringe mouse-4] [right-fringe mouse-4]
                   [vertical-scroll-bar mouse-4] [horizontal-scroll-bar mouse-4]
                   [mode-line mouse-4] [header-line mouse-4])))
  ;; Don't bind modifiers outside of buffer area (e.g. for fringes).
  (should (equal (mouse-wheel--create-scroll-keys '((shift) . 1) 'mouse-4)
                 '([(shift mouse-4)])))
  (should (equal (mouse-wheel--create-scroll-keys '((control) . 9) 'mouse-7)
                 '([(control mouse-7)])))
  (should (equal (mouse-wheel--create-scroll-keys '((meta) . 5) 'mouse-5)
                 '([(meta mouse-5)]))))

;;; mwheel-tests.el ends here
