;;; em-hist-tests.el --- em-hist test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2017-2026 Free Software Foundation, Inc.

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

(eval-when-compile
  (require 'cl-lib))

(require 'ert)
(require 'ert-x)
(require 'em-hist)
(require 'eshell)
(require 'ert-x)

(require 'eshell-tests-helpers
         (ert-resource-file "eshell-tests-helpers"))

(cl-defun em-hist-test/check-history-file (file-name expected &optional
                                                     (expected-ring t))
  "Check that the contents of FILE-NAME match the EXPECTED history entries.
Additionally, check that after loading the file, the history ring
matches too.  If EXPECTED-RING is a list, compare the ring
elements against that; if t (the default), check against EXPECTED."
  (when (eq expected-ring t) (setq expected-ring expected))
  ;; First check the actual file.
  (should (equal (with-temp-buffer
                   (insert-file-contents file-name)
                   (buffer-string))
                 (mapconcat (lambda (i) (concat i "\n")) expected)))
  ;; Now read the history ring and check that too.
  (let (eshell-history-ring eshell-history-index eshell-hist--new-items)
    (eshell-read-history file-name)
    (should (equal (nreverse (ring-elements eshell-history-ring))
                   expected-ring))))

;;; Tests:

(ert-deftest em-hist-test/write-history/append ()
  "Test appending new history to history file."
  (ert-with-temp-file histfile
    (with-temp-eshell
     (em-hist-test/check-history-file histfile nil)
     (eshell-insert-command "echo hi")
     (eshell-write-history histfile 'append)
     (em-hist-test/check-history-file histfile '("echo hi"))
     (eshell-insert-command "echo bye")
     (eshell-write-history histfile 'append)
     (em-hist-test/check-history-file histfile '("echo hi" "echo bye")))))

(ert-deftest em-hist-test/write-history/append-multiple-eshells ()
  "Test appending new history to history file from multiple Eshells."
  (ert-with-temp-file histfile
    (with-temp-eshell
     (with-temp-eshell
      ;; Enter some commands and save them.
      (eshell-insert-command "echo foo")
      (eshell-insert-command "echo bar")
      (eshell-write-history histfile 'append)
      (em-hist-test/check-history-file histfile '("echo foo" "echo bar")))
     ;; Now do the same in the first Eshell buffer.
     (eshell-insert-command "echo goat")
     (eshell-insert-command "echo panda")
     (eshell-write-history histfile 'append)
     (em-hist-test/check-history-file
      histfile '("echo foo" "echo bar" "echo goat" "echo panda")))))

(ert-deftest em-hist-test/write-history/overwrite ()
  "Test overwriting history file."
  (ert-with-temp-file histfile
    (with-temp-eshell
     (em-hist-test/check-history-file histfile nil)
     (eshell-insert-command "echo hi")
     (eshell-insert-command "echo bye")
     (eshell-insert-command "echo bye")
     (eshell-insert-command "echo hi")
     (eshell-write-history histfile)
     (em-hist-test/check-history-file
      histfile '("echo hi" "echo bye" "echo bye" "echo hi"))
     (let ((eshell-hist-ignoredups t))
       (em-hist-test/check-history-file
        histfile '("echo hi" "echo bye" "echo bye" "echo hi")
        '("echo hi" "echo bye" "echo hi")))
     (let ((eshell-hist-ignoredups 'erase))
       (em-hist-test/check-history-file
        histfile '("echo hi" "echo bye" "echo bye" "echo hi")
        '("echo bye" "echo hi"))))))

(ert-deftest em-hist-test/write-history/overwrite-multiple-shells ()
  "Test overwriting history file from multiple Eshells."
  (ert-with-temp-file histfile
    (with-temp-eshell
     (with-temp-eshell
      ;; Enter some commands and save them.
      (eshell-insert-command "echo foo")
      (eshell-insert-command "echo bar")
      (eshell-write-history histfile)
      (em-hist-test/check-history-file histfile '("echo foo" "echo bar")))
     ;; Now do the same in the first Eshell buffer.
     (eshell-insert-command "echo goat")
     (eshell-insert-command "echo panda")
     (eshell-write-history histfile)
     (em-hist-test/check-history-file
      histfile '("echo goat" "echo panda")))))

(ert-deftest em-hist-test/write-history/read-only ()
  "Test that having read-only strings in history is okay."
  (ert-with-temp-file histfile
    (let ((eshell-history-ring (make-ring 2)))
      (ring-insert eshell-history-ring
                   (propertize "echo foo" 'read-only t))
      (ring-insert eshell-history-ring
                   (propertize "echo bar" 'read-only t))
      (eshell-write-history histfile)
      (em-hist-test/check-history-file histfile '("echo foo" "echo bar")))))

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

(ert-deftest em-hist-test/add-to-history/erase-existing-dups ()
  "Test adding to history, erasing any old dups after switching to 'erase."
  (let ((eshell-hist-ignoredups nil))
    (with-temp-eshell
     (eshell-insert-command "echo hi")
     (eshell-insert-command "echo bye")
     (eshell-insert-command "echo bye")
     (eshell-insert-command "echo hi")
     (eshell-insert-command "echo bye")
     (setq eshell-hist-ignoredups 'erase)
     (eshell-insert-command "echo hi")
     (should (equal (ring-elements eshell-history-ring)
                    '("echo hi" "echo bye" "echo bye" "echo bye")))
     (eshell-insert-command "echo bye")
     (should (equal (ring-elements eshell-history-ring)
                    '("echo bye" "echo hi"))))))

(provide 'em-hist-test)

;;; em-hist-tests.el ends here
