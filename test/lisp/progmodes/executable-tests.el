;;; executable-tests.el --- Tests for executable.el  -*- lexical-binding:t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

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
(require 'executable)

(ert-deftest executable-tests-set-magic ()
  (with-temp-buffer
    (insert "#!/foo/bar")
    (executable-set-magic "/bin/bash" nil t t)
    (should (equal (buffer-string) "#!/bin/bash"))))

(ert-deftest executable-tests-set-magic/with-argument ()
  (with-temp-buffer
    (insert "#!/foo/bar")
    (executable-set-magic "/bin/bash" "--norc" t t)
    (should (equal (buffer-string) "#!/bin/bash --norc"))))

(ert-deftest executable-tests-set-magic/executable-insert-nil ()
  (let ((executable-insert nil))
    (with-temp-buffer
      (insert "#!/foo/bar")
      (executable-set-magic "/bin/bash" nil t nil)
      (should (equal (buffer-string) "#!/foo/bar"))))
  (let ((executable-insert nil))
    (with-temp-buffer
      (insert "#!/foo/bar")
      (executable-set-magic "/bin/bash" nil t t)
      (should (equal (buffer-string) "#!/bin/bash")))))

;;; executable-tests.el ends here
