;;; x-win.el --- parse relevant switches and set up for Android  -*- lexical-binding:t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: FSF
;; Keywords: terminals, i18n, android

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

;; This file contains the support for initializing the Lisp side of
;; Android windowing.

;;; Code:


(unless (featurep 'android)
  (error "%s: Loading android-win without having Android"
         invocation-name))

;; Documentation-purposes only: actually loaded in loadup.el.
(require 'frame)
(require 'mouse)
(require 'fontset)
(require 'dnd)
(require 'touch-screen)

(add-to-list 'display-format-alist '(".*" . android))

;; Window system initialization.  This is extremely simple because all
;; initialization is done in android_term_init.

(cl-defmethod window-system-initialization (&context (window-system android)
                                                     &optional _ignored)
  "Set up the window system.  WINDOW-SYSTEM must be ANDROID.
DISPLAY is ignored on Android."
  ;; Create the default fontset.
  (create-default-fontset)
  ;; Just make sure the window system was initialized at startup.
  (android-get-connection))

(cl-defmethod frame-creation-function (params &context (window-system android))
  (x-create-frame-with-faces params))

(cl-defmethod handle-args-function (_ignored &context (window-system android))
  ;; Nothing to do here: Android has no command line to provide
  ;; arguments on.
  (ignore))


;;; Selection support.

(declare-function android-clipboard-exists-p "androidselect.c")
(declare-function android-get-clipboard "androidselect.c")
(declare-function android-set-clipboard "androidselect.c")
(declare-function android-clipboard-owner-p "androidselect.c")

(defvar android-primary-selection nil
  "The last string placed in the primary selection.
Nil if there was no such string.

Android does not have a primary selection of its own, so Emacs
emulates one inside Lisp.")

(defun android-get-clipboard-1 (data-type)
  "Return the clipboard data.
DATA-TYPE is a selection conversion target; only STRING and
TARGETS are supported."
  (or (and (eq data-type 'STRING)
           (android-get-clipboard))
      (and (eq data-type 'TARGETS)
           (android-clipboard-exists-p)
           [TARGETS STRING])))

(defun android-get-primary (data-type)
  "Return the last string placed in the primary selection, or nil.
Return nil if DATA-TYPE is anything other than STRING or TARGETS."
  (when android-primary-selection
    (or (and (eq data-type 'STRING)
             android-primary-selection)
        (and (eq data-type 'TARGETS)
             [TARGETS]))))

(defun android-selection-bounds (value)
  "Return bounds of selection value VALUE.
The return value is a list (BEG END BUF) if VALUE is a cons of
two markers or an overlay.  Otherwise, it is nil."
  (cond ((bufferp value)
	 (with-current-buffer value
	   (when (mark t)
	     (list (mark t) (point) value))))
	((and (consp value)
	      (markerp (car value))
	      (markerp (cdr value)))
	 (when (and (marker-buffer (car value))
		    (buffer-name (marker-buffer (car value)))
		    (eq (marker-buffer (car value))
			(marker-buffer (cdr value))))
	   (list (marker-position (car value))
		 (marker-position (cdr value))
		 (marker-buffer (car value)))))
	((overlayp value)
	 (when (overlay-buffer value)
	   (list (overlay-start value)
		 (overlay-end value)
		 (overlay-buffer value))))))

(defun android-encode-select-string (value)
  "Turn VALUE into a string suitable for placing in the clipboard.
VALUE should be something suitable for passing to
`gui-set-selection'."
  (unless (stringp value)
    (when-let ((bounds (android-selection-bounds value)))
      (setq value (ignore-errors
                    (with-current-buffer (nth 2 bounds)
                      (buffer-substring (nth 0 bounds)
                                        (nth 1 bounds)))))))
  value)

(cl-defmethod gui-backend-get-selection (type data-type
                                              &context (window-system android))
  (cond ((eq type 'CLIPBOARD)
         (android-get-clipboard-1 data-type))
        ((eq type 'PRIMARY)
         (android-get-primary data-type))))

(cl-defmethod gui-backend-selection-exists-p (selection
                                              &context (window-system android))
  (cond ((eq selection 'CLIPBOARD)
         (android-clipboard-exists-p))
        ((eq selection 'PRIMARY)
         (not (null android-primary-selection)))))

(cl-defmethod gui-backend-selection-owner-p (selection
                                             &context (window-system android))
  (cond ((eq selection 'CLIPBOARD)
         (let ((ownership (android-clipboard-owner-p)))
           ;; If ownership is `lambda', then Emacs couldn't determine
           ;; whether or not it owns the clipboard.
           (and (not (eq ownership 'lambda)) ownership)))
        ((eq selection 'PRIMARY)
         ;; Emacs always owns its own primary selection as long as it
         ;; exists.
         (not (null android-primary-selection)))))

(cl-defmethod gui-backend-set-selection (type value
                                              &context (window-system android))
  ;; First, try to turn value into a string.
  ;; Don't set anything if that did not work.
  (when-let ((string (android-encode-select-string value)))
    (cond ((eq type 'CLIPBOARD)
           (android-set-clipboard string))
          ((eq type 'PRIMARY)
           (setq android-primary-selection string)))))

(provide 'android-win)
;; android-win.el ends here.
