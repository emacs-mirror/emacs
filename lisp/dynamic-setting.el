;;; dynamic-setting.el --- Support dynamic changes

;; Copyright (C) 2009-2011 Free Software Foundation, Inc.

;; Author: Jan Djärv <jan.h.d@swipnet.se>
;; Maintainer: FSF
;; Keywords: font, system-font, tool-bar-style
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

(defun dynamic-setting-handle-config-changed-event (event)
  "Handle config-changed-event on the display in EVENT.
Changes can be
  The monospace font. If `font-use-system-font' is nil, the font
    is not changed.
  The normal font.
  Xft parameters, like DPI and hinting.
  The Gtk+ theme name.
  The tool bar style."
  (interactive "e")
  (let ((type (nth 1 event))
	(display-name (nth 2 event)))
    (cond ((and (eq type 'monospace-font-name) font-use-system-font)
	   (font-setting-change-default-font display-name t))

	  ((eq type 'font-render)
	   (font-setting-change-default-font display-name nil))

	  ;; This is a bit heavy, ideally we would just clear faces
	  ;; on the affected display, and perhaps only the relevant
	  ;; faces.  Oh well.
	  ((eq type 'theme-name) (clear-face-cache))

	  ((eq type 'tool-bar-style) (force-mode-line-update t)))))

(define-key special-event-map [config-changed-event]
  'dynamic-setting-handle-config-changed-event)

