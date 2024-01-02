;;; mh-thread-tests.el --- tests for mh-thread.el -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

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
(require 'mh-thread)
(eval-when-compile (require 'cl-lib))

(defun mh-thread-tests-before-from ()
  "Generate the fields of a scan line up to where the \"From\" field would start.
The exact contents are not important, but the number of characters is."
    (concat (make-string mh-cmd-note ?9)
            (make-string mh-scan-cmd-note-width ?A)
            (make-string mh-scan-destination-width ?t)
            (make-string mh-scan-date-width ?/)
            (make-string mh-scan-date-flag-width ?*)))

;;; Tests of support routines

(ert-deftest mh-thread-current-indentation-level ()
  "Test that `mh-thread-current-indentation-level' identifies the level."
  (with-temp-buffer
    (insert (mh-thread-tests-before-from) "[Sender One]  Subject of msg 1\n")
    (insert (mh-thread-tests-before-from) "  [Sender Two]  Subject of msg 2\n")
    (goto-char (point-min))
    (should (equal 0 (mh-thread-current-indentation-level)))
    (forward-line)
    (should (equal 2 (mh-thread-current-indentation-level)))))

(ert-deftest mh-thread-find-children ()
  "Test `mh-thread-find-children'."
  (let (expected-start expected-end)
    (with-temp-buffer
      (insert (mh-thread-tests-before-from) "[Sender One]  line 1\n")
      (setq expected-start (point))
      (insert (mh-thread-tests-before-from) "  [Sender Two]  line 2\n")
      (insert (mh-thread-tests-before-from) "    [Sender Three]  line 3\n")
      (insert (mh-thread-tests-before-from) "      [Sender Four]  line 4\n")
      (setq expected-end (1- (point)))
      (insert (mh-thread-tests-before-from) "  [Sender Five]  line 5\n")
      (goto-char (1+ expected-start))
      (should (equal (list expected-start expected-end)
                     (mh-thread-find-children))))))

(ert-deftest mh-thread-immediate-ancestor ()
  "Test that `mh-thread-immediate-ancestor' moves to the correct message."
  (with-temp-buffer
    (insert (mh-thread-tests-before-from) "[Sender Other]  line 1\n")
    (insert (mh-thread-tests-before-from) "[Sender One]  line 2\n")
    (insert (mh-thread-tests-before-from) "  [Sender Two]  line 3\n")
    (insert (mh-thread-tests-before-from) "    [Sender Three]  line 4\n")
    (insert (mh-thread-tests-before-from) "      [Sender Four]  line 5\n")
    (insert (mh-thread-tests-before-from) "      [Sender Five]  line 6\n")
    (forward-line -1)
    (should (equal (line-number-at-pos) 6))
    (mh-thread-immediate-ancestor)
    (should (equal (line-number-at-pos) 4)) ;skips over sibling
    (mh-thread-immediate-ancestor)
    (should (equal (line-number-at-pos) 3)) ;goes up only one level at a time
    (mh-thread-immediate-ancestor)
    (should (equal (line-number-at-pos) 2))
    (mh-thread-immediate-ancestor)
    (should (equal (line-number-at-pos) 2)))) ;no further motion at thread root

;;; Tests of MH-Folder Commands

(ert-deftest mh-thread-sibling-and-ancestor ()
  "Test motion by `mh-thread-ancestor' and `mh-thread-next-sibling'."
  (with-temp-buffer
    (insert (mh-thread-tests-before-from) "[Sender Other]  line 1\n")
    (insert (mh-thread-tests-before-from) "[Sender One]  line 2\n")
    (insert (mh-thread-tests-before-from) "  [Sender Two]  line 3\n")
    (insert (mh-thread-tests-before-from) "    [Sender Three]  line 4\n")
    (insert (mh-thread-tests-before-from) "      [Sender Four]  line 5\n")
    (insert (mh-thread-tests-before-from) "      [Sender Five]  line 6\n")
    (forward-line -1)
    (let ((mh-view-ops '(unthread))
          (show-count 0))
      (cl-letf (((symbol-function 'mh-maybe-show)
                 (lambda ()
                  (setq show-count (1+ show-count)))))
        (should (equal (line-number-at-pos) 6))
        ;; test mh-thread-ancestor
        (mh-thread-ancestor)
        (should (equal (line-number-at-pos) 4)) ;skips over sibling
        (should (equal show-count 1))
        (mh-thread-ancestor t)
        (should (equal (line-number-at-pos) 2)) ;root flag skips to root
        (should (equal show-count 2))
        (mh-thread-ancestor)
        (should (equal (line-number-at-pos) 2)) ;do not move from root
        (should (equal show-count 2))   ;do not re-show at root
        ;; test mh-thread-sibling
        (mh-thread-next-sibling)
        (should (equal (line-number-at-pos) 2)) ;no next sibling, no motion
        (should (equal show-count 2))   ;no sibling, no show
        (mh-thread-next-sibling t)
        (should (equal (line-number-at-pos) 1))
        (should (equal show-count 3))
        (mh-thread-next-sibling t)
        (should (equal (line-number-at-pos) 1)) ;no previous sibling
        (should (equal show-count 3))
        (goto-char (point-max))
        (forward-line -1)
        (should (equal (line-number-at-pos) 6))
        (mh-thread-next-sibling t)
        (should (equal (line-number-at-pos) 5))
        (should (equal show-count 4))
        (mh-thread-next-sibling t)
        (should (equal (line-number-at-pos) 5)) ;no previous sibling
        (should (equal show-count 4))
        ))))

;;; mh-thread-tests.el ends here
