;;; compact.el --- compact buffers when idle

;; Copyright (C) 2012  Free Software Foundation, Inc.

;; Maintainer: FSF
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides the ability to compact buffers when Emacs is idle.
;; Initially written by Dmitry Antipov <dmantipov@yandex.ru>.

;;; Code:

(require 'timer)

(defun compact-buffers ()
  "Run `compact-buffer' for each buffer except current buffer.
Schedule next compaction if `compact-buffers-when-idle' is greater than zero."
  (mapc (lambda (buffer) 
	  (and (not (eq buffer (current-buffer)))
	       (compact-buffer buffer)))
	(buffer-list))
  (compact-buffers-idle))

(defun compact-buffers-idle ()
  "Compact buffers if `compact-buffers-when-idle' is greater than zero."
  (and (floatp compact-buffers-when-idle)
       (> compact-buffers-when-idle 0.0)
       (run-with-idle-timer compact-buffers-when-idle nil 'compact-buffers)))

(defcustom compact-buffers-when-idle 1.0
  "Compact all buffers when Emacs is idle more than this period of time.
Compaction is done by truncating `buffer-undo-list' and shrinking the gap.
Value less than or equal to zero disables idle compaction."
  :type 'float
  :group 'alloc
  :set (lambda (symbol value)
	 (progn (set-default symbol value)
		(compact-buffers-idle)))
  :version "24.2")

(provide 'compact)

;;; compact.el ends here
