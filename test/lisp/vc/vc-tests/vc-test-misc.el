;;; vc-test-misc.el --- backend-agnostic VC tests  -*- lexical-binding:t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Sean Whitton <spwhitton@spwhitton.name>

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

(require 'ert-x)
(require 'vc)

(ert-deftest vc-test-buffer-sync-fileset ()
  "Test `vc-buffer-sync-fileset'."
  (cl-flet ((test-it (&rest args)
              (let (buffers)
                (cl-letf (((symbol-function 'vc-buffer-sync)
                           (lambda (&rest _)
                             (push (current-buffer) buffers))))
                  (apply #'vc-buffer-sync-fileset args)
                  (sort buffers)))))
    (ert-with-temp-directory temp
      (let* ((default-directory temp)
             (present (find-file-noselect "present"))
             (missing (find-file-noselect "missing"))
             (only-present (list present))
             (only-missing (list missing))
             (missing+present (list missing present)))
        (with-current-buffer present (basic-save-buffer))
        (with-temp-file "unvisited")
        ;; Regular behavior for files.
        (should (equal (test-it `(Git ("missing")))
                       only-missing))
        (should (equal (test-it `(Git ("present" "missing")))
                       missing+present))
        ;; Regular behavior for directories.
        (should (equal (test-it `(Git (,temp)))
                       only-present))
        ;; Two ways to override regular behavior for directories.
        (should (equal (test-it `(Git (,temp)) nil t)
                       missing+present))
        (should (equal (test-it `(Git (,temp "missing")))
                       missing+present))
        ;; Doesn't sync PRESENT twice.
        (should (equal (test-it `(Git ("present" ,temp)))
                       only-present))
        (should (equal (test-it `(Git ("missing" ,temp "present")))
                       missing+present))))))

(provide 'vc-test-misc)
;;; vc-test-misc.el ends here
