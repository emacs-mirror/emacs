;;; display-line-numbers.el --- interface for display-line-numbers -*- lexical-binding: t -*-

;; Copyright (C) 2017-2026 Free Software Foundation, Inc.

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
When `display-line-numbers-mode' is turned on, if this option is
non-nil, `display-line-numbers-width' is set up front to a width
necessary to display all line numbers in the buffer.  If the value
is a positive number, it is interpreted as extra lines to account
for when computing the required width; this should be set to the
number of lines in the tallest window in which you want to prevent
the line-number width from changing."
  :group 'display-line-numbers
  :type '(choice (boolean :tag "Minimum width for buffer's line count")
                 (integer :tag "Number of extra lines to account for"))
  :version "28.1")

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
                         (+ (count-lines (point-min) (point-max))
                            (if (and (numberp display-line-numbers-width-start)
                                     (> display-line-numbers-width-start 0))
                                display-line-numbers-width-start
                              0))))))
        (when display-line-numbers-grow-only
          (add-hook 'pre-command-hook #'display-line-numbers-update-width nil t))
        (setq display-line-numbers display-line-numbers-type))
    (remove-hook 'pre-command-hook #'display-line-numbers-update-width t)
    (setq display-line-numbers nil)))

(defun display-line-numbers--turn-on ()
  "Turn on `display-line-numbers-mode'."
  (unless (minibufferp)
    (display-line-numbers-mode)))

;;;###autoload
(define-globalized-minor-mode global-display-line-numbers-mode
  display-line-numbers-mode display-line-numbers--turn-on)



;;;###autoload
(defvar header-line-indent ""
  "String of spaces to indent the beginning of header-line due to line numbers.
This is intended to be used in `header-line-format', and requires
the `header-line-indent-mode' to be turned on, in order for the width
of this string to be kept updated when the line-number width changes
on display.  An example of a `header-line-format' that uses this
variable might look like this:

  (\"\" header-line-indent THE-REST...)

where THE-REST is the format string which produces the actual text
of the header-line.
Also see `header-line-indent-width'.")

;;;###autoload
(defvar header-line-indent-width 0
  "The width of the current line number display in the window.
This is measured in units of the frame's canonical columns.
This is updated when `header-line-indent-mode' is switched on,
and is intended for use in `:align-to' display specifications
that are part of `header-line-format', when portions of header-line
text should be aligned to respective parts of buffer text.
Also see `header-line-indent'.")

(defun header-line-indent--line-number-width ()
  "Return the width taken by `display-line-numbers' in the current buffer."
  ;; line-number-display-width returns the value for the selected
  ;; window, which might not be the window in which the current buffer
  ;; is displayed.
  (if (not display-line-numbers)
      0
    (let ((cbuf-window (get-buffer-window (current-buffer) t)))
      (if (window-live-p cbuf-window)
          (with-selected-window cbuf-window
            (truncate (line-number-display-width 'columns)))
        4))))

(defun header-line-indent--watch-line-number-width (_window)
  (let ((width (header-line-indent--line-number-width)))
    (setq header-line-indent-width width)
    (unless (= (length header-line-indent) width)
      (setq header-line-indent (make-string width ?\s)))))

(defun header-line-indent--window-scroll-function (window _start)
  (let ((width (with-selected-window window
                 (truncate (line-number-display-width 'columns)))))
    (setq header-line-indent-width width)
    (unless (= (length header-line-indent) width)
      (setq header-line-indent (make-string width ?\s)))))

;;;###autoload
(define-minor-mode header-line-indent-mode
  "Minor mode to help with alignment of header line when line numbers are shown.
This minor mode should be turned on in buffers which display header-line
that needs to be aligned with buffer text when `display-line-numbers-mode'
is turned on in the buffer.

Buffers that have this switched on should have a `header-line-format'
that uses the `header-line-indent' or the `header-line-indent-width'
variables, which this mode will keep up-to-date with the current
display of line numbers.  For example, a `header-line-format' that
looks like this:

  (\"\" header-line-indent THE-REST...)

will make sure the text produced by THE-REST (which should be
a header-line format string) is always indented to be aligned on
display with the first column of buffer text.

The `header-line-indent-width' variable is also kept updated,
and can be used, for instance, in `:align-to' specs as part
of `header-line-format', like this:

  (space :align-to (+ header-line-indent-width 10))

See also `line-number-display-width'."
  :lighter nil
  (if header-line-indent-mode
      (progn
        (setq-local header-line-indent ""
                    header-line-indent-width 0)
        (add-hook 'pre-redisplay-functions
                  #'header-line-indent--watch-line-number-width nil t)
        (add-hook 'window-scroll-functions
                  #'header-line-indent--window-scroll-function nil t))
    (setq-local header-line-indent ""
                header-line-indent-width 0)
    (remove-hook 'pre-redisplay-functions
                 #'header-line-indent--watch-line-number-width t)
    (remove-hook 'window-scroll-functions
                 #'header-line-indent--window-scroll-function t)))

(provide 'display-line-numbers)

;;; display-line-numbers.el ends here
