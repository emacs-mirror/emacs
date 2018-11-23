;;; cus-theme-tasks.el --- Custom Themes -*- lexical-binding: t; -*-

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

(erb-deftask cus-theme-tasks-load-tango ()
  "Load the tango theme."
  (:version "1.0" :special own-process)
  (erb-task-time (load-theme 'tango)))

(provide 'cus-theme-tasks)
;;; cus-theme-tasks.el ends here
