;;; pcmpl-linux-tests.el --- Tests for pcmpl-linux.el  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

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
(require 'ert-x)
(require 'pcmpl-linux)

(ert-deftest pcmpl-linux-test-fs-types ()
  (let ((pcmpl-linux-fs-modules-path-format (ert-resource-file "fs")))
    ;; FIXME: Shouldn't return "." and ".."
    (should (equal (pcmpl-linux-fs-types)
                   '("." ".." "ext4")))))

(ert-deftest pcmpl-linux-test-mounted-directories ()
  (let ((pcmpl-linux-mtab-file (ert-resource-file "mtab")))
    (should (equal (pcmpl-linux-mounted-directories)
                   '("/" "/dev" "/dev/pts" "/dev/shm" "/home/alice/.gvfs"
                     "/lib/modules/2.6.24-16-generic/volatile" "/proc" "/sys"
                     "/sys/kernel/security" "/var/lock" "/var/run")))))

(provide 'pcmpl-linux-tests)

;;; pcmpl-linux-tests.el ends here
