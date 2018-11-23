 ;;; kmacro-tasks.el --- Enhanced Keyboard Macros -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Gemini Lasswell <gazally@runbox.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'erb-task)
(require 'kmacro)

(erb-deftask kmacro-tasks-edit-lines ()
  "Edit lines of text using a keyboard macro."
  (:version "1.0" :discard-first-sample t)
  (with-temp-buffer
    (let ((last-kbd-macro (vconcat (kbd "C-s . ")
                                   [return]
                                   (kbd "C-f C-SPC C-a C-w C-e M-b")
                                   [backspace backspace]
                                   (kbd "C-SPC C-e C-w C-a C-y SPC C-n C-a"))))
      (dotimes (i 10) (insert (format "%s. Flintstone, Fred\n" i)))
      (pop-to-buffer (current-buffer))

      (erb-task-time
       (goto-char (point-min))
       (ignore-errors (kmacro-call-macro 0 nil))
       (goto-char (point-max))
       (forward-line -1)))))

(provide 'kmacro-tasks)
;;; kmacro-tasks.el ends here
