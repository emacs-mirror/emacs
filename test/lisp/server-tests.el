;;; server-tests.el --- Emacs server test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

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
(require 'server)

;;; Tests:

(ert-deftest server-test/server-start-sets-minor-mode ()
  "Ensure that calling `server-start' also sets `server-mode' properly."
  (server-start)
  (unwind-protect
      (progn
        ;; Make sure starting the server activates the minor mode.
        (should (eq server-mode t))
        (should (memq 'server-mode global-minor-modes)))
    ;; Always stop the server, even if the above checks fail.
    (server-start t))
  ;; Make sure stopping the server deactivates the minor mode.
  (should (eq server-mode nil))
  (should-not (memq 'server-mode global-minor-modes)))

;;; server-tests.el ends here
