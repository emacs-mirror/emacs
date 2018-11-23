;;; cmds-tasks.el --- Simple Editing Commands -*- lexical-binding: t; -*-

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

(defun cmds-tasks-setup-buffer (count)
  (dotimes (i count)
      (insert (format "%s. Lorem ipsum dolor sit amet, consectetur adipiscing elit.\n" i)))
    (pop-to-buffer (current-buffer)))

(erb-deftask cmds-tasks-forward-line ()
  "Use forward-line to navigate through a buffer."
  (:version "1.0")
  (with-temp-buffer
    (cmds-tasks-setup-buffer 10000)
    (erb-task-time
     (goto-char (point-min))
     (while (< (point) (point-max))
       (forward-line)))))

(provide 'cmds-tasks)
;;; cmds-tasks.el ends here
