;;; data-tasks.el --- Primitive Operations on Lisp Data Types -*- lexical-binding: t; -*-

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

(erb-deftask data-tasks-prime-p ()
  "Verify that a large prime number is prime."
  (:version "1.0")
  (erb-task-time (data-tasks-prime-p 2305843009)))

(defun data-tasks-prime-p (n)
  (or (= n 2)
      (and (> n 2)
           (= 1 (% n 2))
           (catch 'not-prime
             (dotimes (i (1+ (truncate (sqrt n))))
               (when (and (> i 2) (zerop (% n i)))
                 (throw 'not-prime nil)))
             t))))

(provide 'data-tasks)
;;; data-tasks.el ends here
