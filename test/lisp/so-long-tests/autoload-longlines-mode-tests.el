;;; autoload-longlines-mode-tests.el --- Test suite for so-long.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Phil Sainty <psainty@orcon.net.nz>
;; Keywords: convenience

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
(load (expand-file-name "so-long-tests-helpers"
                        (file-name-directory (or load-file-name
                                                 default-directory))))

(declare-function so-long-tests-remember "so-long-tests-helpers")
(declare-function so-long-tests-assert-and-revert "so-long-tests-helpers")
(defvar so-long-action)

;; We're testing the initial state.  We could alternatively use
;; `unload-feature' to revert to that, but this option is simple.

(autoload 'so-long "so-long")
(autoload 'longlines-mode "longlines")

(ert-deftest so-long-tests-autoload-longlines-mode ()
  "File-local -*- so-long-action: longlines-mode; eval: (so-long) -*-"
  (with-temp-buffer
    (display-buffer (current-buffer))
    (so-long-tests-remember)
    (insert "-*- so-long-action: longlines-mode; eval: (so-long) -*-\n")
    (put 'so-long-action 'safe-local-variable #'symbolp)
    (push '(eval . (so-long)) safe-local-variable-values)
    (hack-local-variables)
    (should (eq so-long-action 'longlines-mode))
    (so-long-tests-assert-and-revert 'longlines-mode)))

;;; autoload-longlines-mode-tests.el ends here
