;;; display-line-numbers.el --- interface for display-line-numbers -*- lexical-binding: t -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
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

;;; Commentary:

;; Provides a minor mode interface for `display-line-numbers'.
;;
;; Toggle display of line numbers with M-x display-line-numbers-mode.
;; To enable line numbering in all buffers, use M-x
;; global-display-line-numbers-mode.  To change the default type of
;; line numbers displayed, customize display-line-numbers-type.

;; NOTE: Customization variables for `display-line-numbers' itself are
;; defined in cus-start.el.

;;; Code:

(defgroup display-line-numbers nil
  "Display line numbers in the buffer."
  :group 'convenience
  :group 'display)

(defcustom display-line-numbers-type t
  "The default type of line numbers to use in `display-line-numbers-mode'.
See `display-line-numbers' for value options."
  :group 'display-line-numbers
  :type '(choice (const :tag "Relative line numbers" relative)
                 (const :tag "Relative visual line numbers" visual)
                 (other :tag "Absolute line numbers" t))
  :version "26.1")

(defcustom display-line-numbers-grow-only nil
  "If non-nil, do not shrink line number width."
  :group 'display-line-numbers
  :type 'boolean
  :version "26.1")

(defcustom display-line-numbers-width-start nil
  "If non-nil, count number of lines to use for line number width.
When `display-line-numbers-mode' is turned on,
`display-line-numbers-width' is set to the minimum width necessary
to display all line numbers in the buffer."
  :group 'display-line-numbers
  :type 'boolean
  :version "26.1")

(defun display-line-numbers-update-width ()
  "Prevent the line number width from shrinking."
  (let ((width (line-number-display-width)))
    (when (> width (or display-line-numbers-width 1))
      (setq display-line-numbers-width width))))

;;;###autoload
(define-minor-mode display-line-numbers-mode
  "Toggle display of line numbers in the buffer.
This uses `display-line-numbers' internally.

To change the type of line numbers displayed by default,
customize `display-line-numbers-type'.  To change the type while
the mode is on, set `display-line-numbers' directly."
  :lighter nil
  (if display-line-numbers-mode
      (progn
        (when display-line-numbers-width-start
          (setq display-line-numbers-width
                (length (number-to-string
                         (count-lines (point-min) (point-max))))))
        (when display-line-numbers-grow-only
          (add-hook 'pre-command-hook #'display-line-numbers-update-width nil t))
        (setq display-line-numbers display-line-numbers-type))
    (remove-hook 'pre-command-hook #'display-line-numbers-update-width t)
    (setq display-line-numbers nil)))

(defun display-line-numbers--turn-on ()
  "Turn on `display-line-numbers-mode'."
  (unless (or (minibufferp)
              ;; taken from linum.el
              (and (daemonp) (null (frame-parameter nil 'client))))
    (display-line-numbers-mode)))

;;;###autoload
(define-globalized-minor-mode global-display-line-numbers-mode
  display-line-numbers-mode display-line-numbers--turn-on)

(provide 'display-line-numbers)

;;; display-line-numbers.el ends here
