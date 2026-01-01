;;; ring-bell-fns.el --- Collection of functions for ring-bell  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; Author: Elijah Gabe Pérez <eg642616@gmail.com>
;; Keywords: faces

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

;; Collection of functions intended to be used with `ring-bell-function'.
;; as alternatives to `visible-bell'

;;; Code:
(require 'pulse)

(defgroup ring-bell nil
  "Customization options for ring bell."
  :version "31.1"
  :group 'emacs)

(defcustom flash-face-attributes
  '(:background "red" :foreground "white")
  "Face attributes to use in any function from `ring-bell-fns'.
This is intended to be used in any function from `ring-bell-fns' such as
`flash-face-bell-function' and `flash-echo-area-bell-function' to make
the flash face more noticeable."
  :type 'plist
  :version "31.1")

(defcustom flash-face-faces
  '(mode-line-active)
  "A list of faces to be flashed by `flash-face-bell-function'."
  :type '(repeat face)
  :version "31.1")

;;;###autoload
(defun flash-face-bell-function ()
  "Indicate ringing the bell by flashing some faces.
Intended to be used in `ring-bell-function'."
  (pulse-faces flash-face-faces flash-face-attributes))

;;;###autoload
(defun flash-echo-area-bell-function ()
  "Indicate ringing the bell by flashing the echo area.
Intended to be used in `ring-bell-function'."
  ;; pulse-faces uses run-with-timer if `pulse-face-duration'
  ;; is long, which makes the flashing in the echo area not visible.
  ;; for fix this then apply the flashing to *Echo Area 0*
  ;; and minibuffer buffer for the `run-with-timer',
  ;; and fallback to minibuffer buffer due performance.
  (if (> pulse-face-duration 0.1)
      (dolist (buf `(,(window-buffer (minibuffer-window))
                     ;; get or create the echo area for flash it too.
                     ,(get-buffer-create" *Echo Area 0*")))
        (redisplay)
        (with-current-buffer buf
          (pulse-faces '(default) flash-face-attributes)))
    (with-current-buffer (window-buffer (minibuffer-window))
      ;; For make the flash effect take effect in the
      ;; minibuffer/echo area, insert a space only if it is empty.
      (if (= (buffer-size) 0)
          (insert ?\s))
      (pulse-faces '(default) flash-face-attributes))))

(provide 'ring-bell-fns)
;;; ring-bell-fns.el ends here
