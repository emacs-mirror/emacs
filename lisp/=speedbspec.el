;;; speedbspec --- Buffer specialized configurations for speedbar

;; Copyright (C) 1997, 1998 Free Software Foundation
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Version: 0.2
;; Keywords: file, tags, tools
;;
;; This file is part of GNU Emacs.
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;   Speedbar provides a frame in which files, and locations in
;; files are displayed.  These functions provide some mode-specific
;; displays for some existing emacs modes.
;;
;;   To provide special service to all the modes supported by this file,
;; put the following in your .emacs file.
;;
;; (require 'speedbspec)
;;
;;   This will load in the known functions, and the mode-enabling code
;; into 'change-major-mode-hook.
;;
;;   You can interactivly try to enable speedbar specialized modes by
;; calling the function `speedbar-add-localized-speedbar-support' and
;; disable it with `speedbar-remove-localized-speedbar-support'.
;;
;;   This file requires speedbar.

;;; Change log:
;;  0.1   - Initial revision requiring speedbar 0.5
;;  0.1.1 - `buffer-live-p' replacement on old emacsen
;;  0.2   - Moved actual work code into their own files.
;;          Check and load files that need loading before checking for the
;;             menu variable.
;;          Made the functions to turn on/off speedbar support interactive.
;;             It is *not* a minor-mode, it mearly enables special speedbar
;;             behaviors.
;;  0.2.1 - Fix for emacs 20 when checking for autoload functions.

;;; Code:
(require 'speedbar)

;;; Compatibility:
;;
;; Thanks: ptype@dra.hmg.gb
(if (fboundp 'buffer-live-p)
    nil
  (defun buffer-live-p (buffer)
    "Determine if the buffer is alive."
    (memq buffer (buffer-list))))


;;; Generic add-new-special-mode stuff
;;
(defvar speedbar-localized-buffer-queue nil
  "List of buffers to localize for speedbar.")

(defun speedbar-add-localized-speedbar-support-to-q ()
  "Add speedbar support to all buffers in `speedbar-localized-buffer-queue'."
  (remove-hook 'post-command-hook
	       'speedbar-add-localized-speedbar-support-to-q)
  (while speedbar-localized-buffer-queue
    (speedbar-add-localized-speedbar-support
     (car speedbar-localized-buffer-queue))
    (setq speedbar-localized-buffer-queue
	  (cdr speedbar-localized-buffer-queue))))

(defun speedbar-add-localized-speedbar-support (buffer)
  "Add localized speedbar support to BUFFER's mode if it is available."
  (interactive "bBuffer: ")
  (if (stringp buffer) (setq buffer (get-buffer buffer)))
  (if (not (buffer-live-p buffer))
      nil
    (save-excursion
      (set-buffer buffer)
      (save-match-data
	(let ((ms (symbol-name major-mode))
	      v tmp)
	  (if (not (string-match "-mode$" ms))
	      nil ;; do nothing to broken mode
	    (setq ms (substring ms 0 (match-beginning 0)))
	    (setq v (intern-soft (concat ms "-speedbar-buttons")))
	    (if (not v)
		nil ;; do nothing if not defined
	      ;; If it is autoloaded, we need to load it now so that
	      ;; we have access to the varialbe -speedbar-menu-items.
	      ;; Is this XEmacs safe?
	      (let ((sf (symbol-function v)))
		(if (and (listp sf) (eq (car sf) 'autoload))
		    (load-library (car (cdr sf)))))
	      (set (make-local-variable 'speedbar-special-mode-expansion-list)
		   (list v))
	      (setq v (intern-soft (concat ms "-speedbar-menu-items")))
	      (if (not v)
		  nil ;; don't add special menus
		(make-local-variable 'speedbar-easymenu-definition-special)
		(setq speedbar-easymenu-definition-special
		      (symbol-value v))))))))))

(defun speedbar-remove-localized-speedbar-support (buffer)
  "Remove any traces that BUFFER supports speedbar in a specialized way."
  (save-excursion
    (set-buffer buffer)
    (kill-local-variable 'speedbar-special-mode-expansion-list)
    (kill-local-variable 'speedbar-easymenu-definition-special)))
  
(defun speedbar-change-major-mode ()
  "Run when the major mode is changed."
  (setq speedbar-localized-buffer-queue
	(add-to-list 'speedbar-localized-buffer-queue (current-buffer)))
  (add-hook 'post-command-hook 'speedbar-add-localized-speedbar-support-to-q))

(add-hook 'change-major-mode-hook 'speedbar-change-major-mode)
(add-hook 'find-file-hooks 'speedbar-change-major-mode)

(provide 'speedbspec)
;;; speedbspec ends here
