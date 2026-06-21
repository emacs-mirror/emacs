;;; rmail-tests.el --- Test suite. -*- lexical-binding: t -*-

;; Copyright (C) 2015-2026 Free Software Foundation, Inc.

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
(require 'rmail)


(ert-deftest rmail-autoload ()
  "Test that `rmail-edit-current-message' has been autoloaded."
  (should
   (fboundp 'rmail-edit-current-message))
  (should
   (autoloadp
    (symbol-function
     'rmail-edit-current-message))))

(ert-deftest rmail-mode-makes-collection-buffer-unibyte ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert "From sender@example.fr Tue Jun  9 12:00:00 2026\n"
            "From: sender@example.fr\n"
            "To: recipient@example.com\n"
            "Subject: lunch\n"
            "Date: Tue, 09 Jun 2026 12:00:00 +0000\n"
            "\n"
            "Caf\xe9\n"
            "\n")
    (rmail-mode)
    (with-current-buffer (if (rmail-buffers-swapped-p)
                             rmail-view-buffer
                           rmail-buffer)
      (should-not enable-multibyte-characters))))

(provide 'rmail-tests)
;;; rmail-tests.el ends here
