;;; -*- coding: iso-2022-7bit; -*-
;;; tramp-util.el --- Misc utility functions to use with Tramp

;; Copyright (C) 2001  Free Software Foundation, Inc.

;; Author: Kai Gro,A_(Bjohann <Kai.Grossjohann@CS.Uni-Dortmund.DE>
;; Keywords: comm, extensions, processes

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Some misc. utility functions that might go nicely with Tramp.
;; Mostly, these are kluges awaiting real solutions later on.

;;; Code:

(eval-when-compile (require 'cl))
(require 'compile)
(require 'tramp)

(defun tramp-compile (command)
  "Compile on remote host."
  (interactive
   (if (or compilation-read-command current-prefix-arg)
       (list (read-from-minibuffer "Compile command: "
                                   compile-command nil nil
                                   '(compile-history . 1)))
     (list compile-command)))
  (setq compile-command command)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (let ((d default-directory))
    (save-excursion
      (pop-to-buffer (get-buffer-create "*Compilation*") t)
      (erase-buffer)
      (setq default-directory d)))
  (tramp-handle-shell-command command (get-buffer "*Compilation*"))
  (pop-to-buffer (get-buffer "*Compilation*"))
  (compilation-minor-mode 1))

(provide 'tramp-util)

;;; arch-tag: 500f9992-a44e-46d0-83a7-980799251808
;;; tramp-util.el ends here
