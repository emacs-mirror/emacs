;;; indent-aux.el --- Autoloaded indentation commands for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Package: emacs

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

;; Autoloaded commands for making and changing indentation in text and
;; killed text

;;; Code:



;; Indent Filter mode.  When enabled, this minor mode filters all
;; killed text to remove leading indentation.

(defun kill-ring-deindent-buffer-substring-function (beg end delete)
  "Save the text within BEG and END to kill-ring, decreasing indentation.
Delete the saved text if DELETE is non-nil.

In the saved copy of the text, remove some of the indentation, such
that the buffer position at BEG will be at column zero when the text
is yanked."
  (let ((a beg)
        (b end))
    (setq beg (min a b)
          end (max a b)))
  (let ((indentation (save-excursion (goto-char beg)
                                     (current-column)))
        (i-t-m indent-tabs-mode)
        (text (if delete
                  (delete-and-extract-region beg end)
                (buffer-substring beg end))))
    (with-temp-buffer
      ;; We bind inhibit-read-only non-nil in case the copied text has
      ;; read-only properties.
      (let ((inhibit-read-only t))
        ;; Indent/deindent the same as the major mode in the original
        ;; buffer.
        (setq indent-tabs-mode i-t-m)
        (insert text)
        (indent-rigidly (point-min) (point-max)
                        (- indentation))
        (buffer-string)))))

;;;###autoload
(define-minor-mode kill-ring-deindent-mode
  "Toggle removal of indentation from text saved to the kill ring.

When this minor mode is enabled, text saved into the kill ring is
indented towards the left by the column number at the start of
that text."
  :global 't
  :group 'killing
  (if kill-ring-deindent-mode
      (add-function :override filter-buffer-substring-function
                    #'kill-ring-deindent-buffer-substring-function
                    '(:depth 100))
    (remove-function filter-buffer-substring-function
                     #'kill-ring-deindent-buffer-substring-function)))



(provide 'indent-aux)
;;; indent-aux.el ends here.
