;;; find-cmd-tests.el --- tests for find-cmd.el.  -*- lexical-binding: t -*-

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
(require 'find-cmd)

(ert-deftest find-cmd-test-find-cmd ()
  (should
   (string-match
    (rx "find " (+ any)
        " \\( \\( -name .svn -or -name .git -or -name .CVS \\)"
        " -prune -or -true \\)"
        " \\( \\( \\(" " -name \\*.pl -or -name \\*.pm -or -name \\*.t \\)"
        " -or -mtime \\+1 \\) -and \\( -fstype nfs -or -fstype ufs \\) \\) ")
    (find-cmd '(prune (name ".svn" ".git" ".CVS"))
              '(and (or (name "*.pl" "*.pm" "*.t")
                        (mtime "+1"))
                    (fstype "nfs" "ufs"))))))

(ert-deftest find-cmd-test-find-cmd/error-unknown-atom ()
  (should-error (find-cmd '(unknown 123))))

(ert-deftest find-cmd-test-find-cmd/error-wrong-argnum ()
  (should-error (find-cmd '(name))))

(provide 'find-cmd-tests)
;;; find-cmd-tests.el ends here
