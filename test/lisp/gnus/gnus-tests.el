;;; gnus-tests.el --- Wrapper for the Gnus tests

;; Copyright (C) 2011-2018 Free Software Foundation, Inc.

;; Author: Teodor Zlatanov <tzz@lifelogs.com>

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

;; This file should contain nothing but requires for all the Gnus
;; tests that are not standalone.

;;; Code:
;; registry.el is required by gnus-registry.el but this way we're explicit.
(require 'registry)
(require 'gnus-registry)

(provide 'gnus-tests)
;;; gnus-tests.el ends here
