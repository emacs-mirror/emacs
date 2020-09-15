;;; modern-mode.el --- collection of non invasive modern settings -*- lexical-binding: t -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Andrea Corallo <akrl@sdf.org>
;; Version: 0.1
;; Created: 2020-09-15

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
;; A minimal collection of non invasive 'modern' settings trying to
;; make Emacs more appealing/easy to new comers.

;;; Code:

(defgroup modern-mode nil
  "A conservative collection of modern settings."
  :group 'convenience)

;;;###autoload
(define-minor-mode modern-mode
  "Toggle modern mode in all buffers."
  :global t :group 'modern-mode
  (let ((arg (if modern-mode 1 -1)))
    (column-number-mode arg)
    (delete-selection-mode arg)
    (fido-mode arg)
    (global-auto-revert-mode arg)
    (show-paren-mode arg)
    (winner-mode arg))
  (if modern-mode
      (progn
	(windmove-default-keybindings '(shift))
	(global-set-key (kbd "C-x C-b") 'ibuffer))
    (windmove-remove-keybindings)
    (global-set-key (kbd "C-x C-b") 'list-all-buffers)))

(provide 'modern-mode)

;;; modern-mode.el ends here
