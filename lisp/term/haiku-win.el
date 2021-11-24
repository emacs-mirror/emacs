;;; haiku-win.el --- set up windowing on Haiku -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

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

;; Support for using Haiku's BeOS derived windowing system.

;;; Code:

(eval-when-compile (require 'cl-lib))
(unless (featurep 'haiku)
  (error "%s: Loading haiku-win without having Haiku"
         invocation-name))

;; Documentation-purposes only: actually loaded in loadup.el.
(require 'frame)
(require 'mouse)
(require 'scroll-bar)
(require 'menu-bar)
(require 'fontset)
(require 'dnd)

(add-to-list 'display-format-alist '(".*" . haiku-win))

;;;; Command line argument handling.

(defvar x-invocation-args)
(defvar x-command-line-resources)

(defvar haiku-initialized)

(declare-function x-open-connection "haikufns.c")
(declare-function x-handle-args "common-win")
(declare-function haiku-selection-data "haikuselect.c")
(declare-function haiku-selection-put "haikuselect.c")
(declare-function haiku-put-resource "haikufns.c")

(defun haiku--handle-x-command-line-resources (command-line-resources)
  "Handle command line X resources specified with the option `-xrm'.
The resources should be a list of strings in COMMAND-LINE-RESOURCES."
  (dolist (s command-line-resources)
    (let ((components (split-string s ":")))
      (when (car components)
        (haiku-put-resource (car components)
                            (string-trim-left
                             (mapconcat #'identity (cdr components) ":")))))))

(cl-defmethod window-system-initialization (&context (window-system haiku)
                                                     &optional display)
  "Set up the window system.  WINDOW-SYSTEM must be HAIKU.
DISPLAY may be set to the name of a display that will be initialized."
  (cl-assert (not haiku-initialized))

  (create-default-fontset)
  (when x-command-line-resources
    (haiku--handle-x-command-line-resources
     (split-string x-command-line-resources "\n")))
  (x-open-connection (or display "be") x-command-line-resources t)
  (setq haiku-initialized t))

(cl-defmethod frame-creation-function (params &context (window-system haiku))
  (x-create-frame-with-faces params))

(cl-defmethod handle-args-function (args &context (window-system haiku))
  (x-handle-args args))

(defun haiku--selection-type-to-mime (type)
  "Convert symbolic selection type TYPE to its MIME equivalent.
If TYPE is nil, return \"text/plain\"."
  (cond
   ((memq type '(TEXT COMPOUND_TEXT STRING UTF8_STRING)) "text/plain")
   ((stringp type) type)
   ((symbolp type) (symbol-name type))
   (t "text/plain")))

(cl-defmethod gui-backend-get-selection (type data-type
                                              &context (window-system haiku))
  (if (eq data-type 'TARGETS)
      (apply #'vector (mapcar #'intern
                              (haiku-selection-targets type)))
    (haiku-selection-data type (haiku--selection-type-to-mime data-type))))

(cl-defmethod gui-backend-set-selection (type value
                                              &context (window-system haiku))
  (haiku-selection-put type "text/plain" value t))

(cl-defmethod gui-backend-selection-exists-p (selection
                                              &context (window-system haiku))
  (haiku-selection-data selection "text/plain"))

(cl-defmethod gui-backend-selection-owner-p (_
                                             &context (window-system haiku))
  t)

(declare-function haiku-read-file-name "haikufns.c")

(defun x-file-dialog (prompt dir default_filename mustmatch only_dir_p)
  "SKIP: real doc in xfns.c."
  (if (eq (framep-on-display (selected-frame)) 'haiku)
      (haiku-read-file-name prompt (selected-frame)
                            (or dir (and default_filename
                                         (file-name-directory default_filename)))
                            mustmatch only_dir_p
                            (file-name-nondirectory default_filename))
    (error "x-file-dialog on a tty frame")))

(defun haiku-dnd-handle-drag-n-drop-event (event)
  "Handle specified drag-n-drop EVENT."
  (interactive "e")
  (let* ((string (caddr event))
	 (window (posn-window (event-start event))))
    (with-selected-window window
      (raise-frame)
      (dnd-handle-one-url window 'private (concat "file:" string)))))

(define-key special-event-map [drag-n-drop]
            'haiku-dnd-handle-drag-n-drop-event)

(provide 'haiku-win)
(provide 'term/haiku-win)

;;; haiku-win.el ends here
