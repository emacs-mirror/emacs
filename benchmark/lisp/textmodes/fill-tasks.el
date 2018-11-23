;;; fill-tasks.el --- Fill Commands -*- lexical-binding: t; -*-

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

(erb-deftask fill-tasks-fill-paragraph ()
  "Create a long single line paragraph and use fill-paragraph."
  (:version "1.0")
  (with-temp-buffer
    (dotimes (_i 100)
      (insert "Lorem ipsum dolor sit amet, consectetur adipiscing elit.  "))
    (insert "\n")
    (pop-to-buffer (current-buffer))

    (erb-task-time
     (goto-char (point-min))
     (fill-paragraph))))

(provide 'fill-tasks)
;;; fill-tasks.el ends here
