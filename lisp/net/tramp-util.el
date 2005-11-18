;;; -*- coding: iso-2022-7bit; -*-
;;; tramp-util.el --- Misc utility functions to use with Tramp

;; Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

;; Author: kai.grossjohann@gmx.net
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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Some misc. utility functions that might go nicely with Tramp.
;; Mostly, these are kluges awaiting real solutions later on.

;;; Code:

(eval-when-compile (require 'cl))
(require 'compile)
(require 'tramp)

;; Define a Tramp minor mode. It's intention is to redefine some keys for Tramp
;; specific functions, like compilation.
;; The key remapping works since Emacs 22 only. Unknown for XEmacs.

;; Pacify byte-compiler
(eval-when-compile
  (unless (fboundp 'define-minor-mode)
      (defalias 'define-minor-mode 'identity)
      (defvar tramp-minor-mode))
  (unless (featurep 'xemacs)
      (defalias 'add-menu-button 'identity)))

(defvar tramp-minor-mode-map (make-sparse-keymap)
  "Keymap for Tramp minor mode.")

(define-minor-mode tramp-minor-mode "Tramp minor mode for utility functions."
  :group 'tramp
  :global nil
  :init-value nil
  :lighter " Tramp"
  :keymap tramp-minor-mode-map
  (setq tramp-minor-mode
	(and tramp-minor-mode (tramp-tramp-file-p default-directory))))

(add-hook 'find-file-hooks 'tramp-minor-mode t)
(add-hook 'dired-mode-hook 'tramp-minor-mode t)

(defun tramp-remap-command (old-command new-command)
  "Replaces bindings of OLD-COMMAND by NEW-COMMAND.
If remapping functionality for keymaps is defined, this happens for all
bindings.  Otherwise, only bindings active during invocation are taken
into account.  XEmacs menubar bindings are not changed by this."
  (if (functionp 'command-remapping)
      ;; Emacs 22
      (eval
       `(define-key tramp-minor-mode-map [remap ,old-command] new-command))
    ;; previous Emacs versions.
    (mapcar
     '(lambda (x)
	(define-key tramp-minor-mode-map x new-command))
     (where-is-internal old-command))))

(tramp-remap-command 'compile 'tramp-compile)
(tramp-remap-command 'recompile 'tramp-recompile)

;; XEmacs has an own mimic for menu entries
(when (fboundp 'add-menu-button)
  (funcall 'add-menu-button
   '("Tools" "Compile")
   ["Compile..."
    (command-execute (if tramp-minor-mode 'tramp-compile 'compile))
    :active (fboundp 'compile)])
  (funcall 'add-menu-button
   '("Tools" "Compile")
   ["Repeat Compilation"
    (command-execute (if tramp-minor-mode 'tramp-recompile 'recompile))
    :active (fboundp 'compile)]))

;; Utility functions.

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
  (tramp-minor-mode 1)
  (compilation-minor-mode 1))

(defun tramp-recompile ()
  "Re-compile on remote host."
  (interactive)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (tramp-handle-shell-command compile-command (get-buffer "*Compilation*"))
  (pop-to-buffer (get-buffer "*Compilation*"))
  (tramp-minor-mode 1)
  (compilation-minor-mode 1))

(provide 'tramp-util)

;;; arch-tag: 500f9992-a44e-46d0-83a7-980799251808
;;; tramp-util.el ends here
