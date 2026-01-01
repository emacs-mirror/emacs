;;; check-declare-tests.el --- Tests for check-declare.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

;; Author: Simen Heggestøyl <simenheg@gmail.com>
;; Keywords:

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

;;

;;; Code:

(require 'check-declare)
(require 'ert)
(require 'ert-x)
(eval-when-compile (require 'subr-x))

(ert-deftest check-declare-tests-locate ()
  (should (file-exists-p (check-declare-locate "check-declare" "")))
  (should
   (string-prefix-p "ext:" (check-declare-locate "ext:foo" ""))))

(ert-deftest check-declare-tests-scan ()
  (ert-with-temp-file file
    (with-temp-file file
      (insert
       (string-join
        '(";; foo comment"
          "(declare-function ring-insert \"ring\" (ring item))"
          "(let ((foo 'code)) foo)")
        "\n")))
    (let ((res (check-declare-scan file)))
      (should (= (length res) 1))
      (pcase-let ((`((,fnfile ,fn ,arglist ,fileonly)) res))
        (should (string-match-p "ring" fnfile))
        (should (equal "ring-insert" fn))
        (should (equal '(ring item) arglist))
        (should-not fileonly)))))

(ert-deftest check-declare-tests-verify ()
  (ert-with-temp-file file
    (with-temp-file file
      (insert
       (string-join
        '(";; foo comment"
          "(defun foo-fun ())"
          "(defun ring-insert (ring item)"
          "\"Insert onto ring RING the item ITEM.\""
          "nil)")
        "\n")))
    (should-not
     (check-declare-verify
      file '(("foo.el" "ring-insert" (ring item)))))))

(ert-deftest check-declare-tests-verify-mismatch ()
  (ert-with-temp-file file
    (with-temp-file file
      (insert
       (string-join
        '(";; foo comment"
          "(defun foo-fun ())"
          "(defun ring-insert (ring)"
          "\"Insert onto ring RING the item ITEM.\""
          "nil)")
        "\n")))
    (should
     (equal
      (check-declare-verify
       file '(("foo.el" "ring-insert" (ring item))))
      '(("foo.el" "ring-insert" "arglist mismatch"))))))

(ert-deftest check-declare-tests-sort ()
  (should-not (check-declare-sort '()))
  (should (equal (check-declare-sort '((a (1 a)) (b (2)) (d (1 d))))
                 '((2 (b)) (1 (a a) (d d))))))

(ert-deftest check-declare-tests-warn ()
  (with-temp-buffer
    (let ((check-declare-warning-buffer (buffer-name)))
      (check-declare-warn
       "foo-file" "foo-fun" "bar-file" "it wasn't" 999)
      (let ((res (buffer-string)))
        ;; Don't care too much about the format of the output, but
        ;; check that key information is present.
        (should (string-search "foo-file" res))
        (should (string-search "foo-fun" res))
        (should (string-search "bar-file" res))
        (should (string-search "it wasn't" res))
        (should (string-search "999" res))))))

(provide 'check-declare-tests)
;;; check-declare-tests.el ends here
