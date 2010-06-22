;;; font-setting.el --- Support dynamic font changes   -*- coding: utf-8 -*-

;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.

;; Author: Jan Djärv <jan.h.d@swipnet.se>
;; Maintainer: FSF
;; Keywords: font, system-font

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

;; This file provides the lisp part of the GConf and XSetting code in
;; xsetting.c.  But it is nothing that prevents it from being used by
;; other configuration schemes.

;;; Code:

;;; Customizable variables

(declare-function font-get-system-font "xsettings.c" ())

(defvar font-use-system-font)

(defun font-setting-change-default-font (display-or-frame set-font)
  "Change font and/or font settings for frames on display DISPLAY-OR-FRAME.
If DISPLAY-OR-FRAME is a frame, the display is the one for that frame.

If SET-FONT is non-nil, change the font for frames.  Otherwise re-apply the
current form for the frame (i.e. hinting or somesuch changed)."

  (let ((new-font (and (fboundp 'font-get-system-font)
		       (font-get-system-font))))
    (when new-font
      ;; Be careful here: when set-face-attribute is called for the
      ;; :font attribute, Emacs tries to guess the best matching font
      ;; by examining the other face attributes (Bug#2476).

      (clear-font-cache)
      ;; Set for current frames. Only change font for those that have
      ;; the old font now. If they don't have the old font, the user
      ;; probably changed it.
      (dolist (f (frames-on-display-list display-or-frame))
	(if (display-graphic-p f)
	    (let* ((frame-font
		    (or (font-get (face-attribute 'default :font f
						  'default) :user-spec)
			(frame-parameter f 'font-parameter)))
		   (font-to-set
		    (if set-font new-font
		      ;; else set font again, hinting etc. may have changed.
		      frame-font)))
	      (if font-to-set
		  (progn
		    (message "setting %s" font-to-set)
		    (set-frame-parameter f 'font-parameter font-to-set)
		    (set-face-attribute 'default f
					:width 'normal
					:weight 'normal
					:slant 'normal
					:font font-to-set))))))

      ;; Set for future frames.
      (set-face-attribute 'default t :font new-font)
      (let ((spec (list (list t (face-attr-construct 'default)))))
	(progn
	  (put 'default 'customized-face spec)
	  (custom-push-theme 'theme-face 'default 'user 'set spec)
	  (put 'default 'face-modified nil))))))

(defun font-setting-handle-config-changed-event (event)
  "Handle config-changed-event to change fonts on the display in EVENT.
If `font-use-system-font' is nil, the font is not changed."
  (interactive "e")
  (let ((type (nth 1 event)) ;; font-name or font-render
	(display-name (nth 2 event)))
    (if (or (not (eq type 'font-name))
	    font-use-system-font)
	(font-setting-change-default-font display-name
					  (eq type 'font-name)))))

(if (or (featurep 'system-font-setting) (featurep 'font-render-setting))
  (define-key special-event-map [config-changed-event]
    'font-setting-handle-config-changed-event))

(provide 'font-setting)

;; arch-tag: 3a57e78f-1cd6-48b6-ab75-98f160dcc017
