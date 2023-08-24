;;; em-hist-tests.el --- em-hist test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2017-2023 Free Software Foundation, Inc.

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
(require 'ert-x)
(require 'em-hist)
(require 'eshell)

(require 'eshell-tests-helpers
         (expand-file-name "eshell-tests-helpers"
                           (file-name-directory (or load-file-name
                                                    default-directory))))

;;; Tests:

(ert-deftest em-hist-test/write-readonly-history ()
  "Test that having read-only strings in history is okay."
  (ert-with-temp-file histfile
    (let ((eshell-history-ring (make-ring 2)))
      (ring-insert eshell-history-ring
                   (propertize "echo foo" 'read-only t))
      (ring-insert eshell-history-ring
                   (propertize "echo bar" 'read-only t))
      (eshell-write-history histfile))))

(ert-deftest em-hist-test/add-to-history/allow-dups ()
  "Test adding to history, allowing dups."
  (let ((eshell-hist-ignoredups nil))
    (with-temp-eshell
     (eshell-insert-command "echo hi")
     (eshell-insert-command "echo bye")
     (eshell-insert-command "echo bye")
     (eshell-insert-command "echo hi")
     (should (equal (ring-elements eshell-history-ring)
                    '("echo hi" "echo bye" "echo bye" "echo hi"))))))

(ert-deftest em-hist-test/add-to-history/no-consecutive-dups ()
  "Test adding to history, ignoring consecutive dups."
  (let ((eshell-hist-ignoredups t))
    (with-temp-eshell
     (eshell-insert-command "echo hi")
     (eshell-insert-command "echo bye")
     (eshell-insert-command "echo bye")
     (eshell-insert-command "echo hi")
     (should (equal (ring-elements eshell-history-ring)
                    '("echo hi" "echo bye" "echo hi"))))))

(ert-deftest em-hist-test/add-to-history/erase-dups ()
  "Test adding to history, erasing any old dups."
  (let ((eshell-hist-ignoredups 'erase))
    (with-temp-eshell
     (eshell-insert-command "echo hi")
     (eshell-insert-command "echo bye")
     (eshell-insert-command "echo bye")
     (eshell-insert-command "echo hi")
     (should (equal (ring-elements eshell-history-ring)
                    '("echo hi" "echo bye"))))))

(provide 'em-hist-test)

;;; em-hist-tests.el ends here
