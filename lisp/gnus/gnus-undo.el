;;; gnus-undo.el --- minor mode for undoing in Gnus  -*- lexical-binding: t; -*-

;; Copyright (C) 1996-2023 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

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

;; This package allows arbitrary undoing in Gnus buffers.  As all the
;; Gnus buffers aren't very text-oriented (what is in the buffers is
;; just some arbitrary representation of the actual data), normal Emacs
;; undoing doesn't work at all for Gnus.
;;
;; This package works by letting Gnus register functions for reversing
;; actions, and then calling these functions when the user pushes the
;; `undo' key.  As with normal `undo', there it is possible to set
;; undo boundaries and so on.
;;
;; Internally, the undo sequence is represented by the
;; `gnus-undo-actions' list, where each element is a list of functions
;; to be called, in sequence, to undo some action.  (An "action" is a
;; collection of functions.)
;;
;; For instance, a function for killing a group will call
;; `gnus-undo-register' with a function that un-kills the group.  This
;; package will put that function into an action.

;;; Code:

(require 'gnus-util)
(require 'gnus)

(defgroup gnus-undo nil
  "Undoing in Gnus buffers."
  :group 'gnus)

(defcustom gnus-undo-limit 2000
  "The number of undoable actions recorded."
  :type 'integer)

(defcustom gnus-undo-mode nil
  ;; FIXME: This is a buffer-local minor mode which requires running
  ;; code upon activation/deactivation, so defining it as a defcustom
  ;; doesn't seem very useful: setting it to non-nil via Customize
  ;; probably won't do the right thing.
  "Minor mode for undoing in Gnus buffers."
  :type 'boolean)

(defcustom gnus-undo-mode-hook nil
  "Hook called in all `gnus-undo-mode' buffers."
  :type 'hook)

;;; Internal variables.

(defvar gnus-undo-actions nil)
(defvar gnus-undo-boundary t)
(defvar gnus-undo-last nil)
(defvar gnus-undo-boundary-inhibit nil)

;;; Minor mode definition.

(defvar-keymap gnus-undo-mode-map
  "C-M-_" #'gnus-undo
  "C-_" #'gnus-undo
  "C-x u" #'gnus-undo
  ;; many people are used to type `C-/' on GUI frames and get `C-_'.
  "C-/" #'gnus-undo)

(defun gnus-undo-make-menu-bar ()
  ;; This is disabled for the time being.
  (when nil
    (define-key-after (current-local-map) [menu-bar file gnus-undo]
      (cons "Undo" 'gnus-undo-actions)
      [menu-bar file whatever])))

(define-minor-mode gnus-undo-mode
  "Minor mode for providing `undo' in Gnus buffers.

\\{gnus-undo-mode-map}"
  :keymap gnus-undo-mode-map
  (setq-local gnus-undo-actions nil)
  (setq-local gnus-undo-boundary t)
  (when gnus-undo-mode
    ;; Set up the menu.
    (when (gnus-visual-p 'undo-menu 'menu)
      (gnus-undo-make-menu-bar))
    (add-hook 'post-command-hook #'gnus-undo-boundary nil t)))

;;; Interface functions.

(defun gnus-disable-undo (&optional buffer)
  "Disable undoing in the current buffer."
  (interactive)
  (save-excursion
    (when buffer
      (set-buffer buffer))
    (gnus-undo-mode -1)))

(defun gnus-undo-boundary ()
  "Set Gnus undo boundary."
  (if gnus-undo-boundary-inhibit
      (setq gnus-undo-boundary-inhibit nil)
    (setq gnus-undo-boundary t)))

(defun gnus-undo-force-boundary ()
  "Set Gnus undo boundary."
  (setq gnus-undo-boundary-inhibit nil
	gnus-undo-boundary t))

(defun gnus-undo-register (form)
  "Register FORMS as something to be performed to undo a change."
  (when gnus-undo-mode
    (gnus-undo-register-1
     `(lambda () ,form))))

(defun gnus-undo-register-1 (function)
  "Register FUNCTION as something to be performed to undo a change."
  (when gnus-undo-mode
    (cond
     ;; We are on a boundary, so we create a new action.
     (gnus-undo-boundary
      (push (list function) gnus-undo-actions)
      (setq gnus-undo-boundary nil))
     ;; Prepend the function to an old action.
     (gnus-undo-actions
      (setcar gnus-undo-actions (cons function (car gnus-undo-actions))))
     ;; Initialize list.
     (t
      (setq gnus-undo-actions (list (list function)))))
    ;; Limit the length of the undo list.
    (let ((next (nthcdr gnus-undo-limit gnus-undo-actions)))
      (when next
	(setcdr next nil)))
    ;; We are not at a boundary...
    (setq gnus-undo-boundary-inhibit t)))

(defun gnus-undo (_n)
  "Undo some previous changes in Gnus buffers.
Repeat this command to undo more changes."
  ;; FIXME: A numeric argument should serve as a repeat count.
  (interactive "p")
  (unless gnus-undo-mode
    (error "Undoing is not enabled in this buffer"))
  (message "%s" last-command)
  (unless (and (eq last-command 'gnus-undo)
               gnus-undo-last)
    (setq gnus-undo-last gnus-undo-actions))
  (let ((action (pop gnus-undo-last)))
    (unless action
      (error "Nothing further to undo"))
    (setq gnus-undo-actions (delq action gnus-undo-actions))
    (setq gnus-undo-boundary t)
    (mapc #'funcall action)))

(provide 'gnus-undo)

;;; gnus-undo.el ends here
