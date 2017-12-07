;;; ido-tests.el --- unit tests for ido.el           -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit tests for ido.el.

;;; Code:

(ert-deftest ido-tests--other-window-frame ()
  "Verifies that Bug#26360 is fixed."
  (should-not ido-mode)
  (unwind-protect
      (progn
        (ido-mode)
        (should (equal ido-mode 'both))
        (should (equal (key-binding [remap find-alternate-file-other-window])
                       #'ido-find-alternate-file-other-window))
        (should (commandp #'ido-find-alternate-file-other-window))
        (should (equal (key-binding (kbd "C-x 4 d")) #'ido-dired-other-window))
        (should (commandp #'ido-dired-other-window))
        (should (equal (key-binding (kbd "C-x 5 d")) #'ido-dired-other-frame))
        (should (commandp #'ido-dired-other-frame))
        (should (equal (key-binding (kbd "C-x 5 C-o"))
                       #'ido-display-buffer-other-frame))
        (should (commandp #'ido-display-buffer-other-frame)))
    (ido-mode 0)))

;;; ido-tests.el ends here
