;;; ps-def.el --- Emacs definitions for ps-print -*- lexical-binding: t -*-

;; Copyright (C) 2007-2020 Free Software Foundation, Inc.

;; Author: Vinicius Jose Latorre <viniciusjl.gnu@gmail.com>
;;	Kenichi Handa <handa@gnu.org> (multi-byte characters)
;; Keywords: wp, print, PostScript
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/ViniciusJoseLatorre
;; Package: ps-print

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

;; See ps-print.el for documentation.

;;; Code:

(declare-function ps-plot-with-face "ps-print" (from to face))
(declare-function ps-plot-string    "ps-print" (string))

(defvar ps-bold-faces)                  ; in ps-print.el
(defvar ps-italic-faces)




;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Definitions


(defun ps-mark-active-p ()
  mark-active)


(defun ps-face-foreground-name (face)
  (face-foreground face nil t))


(defun ps-face-background-name (face)
  (face-background face nil t))


(defalias 'ps-frame-parameter 'frame-parameter)

;; Return t if the device (which can be changed during an emacs session) can
;; handle colors.  This function is not yet implemented for GNU emacs.
(defun ps-color-device ()
  (if (fboundp 'color-values)
      (funcall 'color-values "Green")
    t))


(defun ps-color-values (x-color)
  (cond
   ((fboundp 'color-values)
    (funcall 'color-values x-color))
   ((fboundp 'x-color-values)
    (funcall 'x-color-values x-color))
   (t
    (error "No available function to determine X color values"))))


(defun ps-face-bold-p (face)
  (or (face-bold-p face)
      (memq face ps-bold-faces)))


(defun ps-face-italic-p (face)
  (or (face-italic-p face)
      (memq face ps-italic-faces)))


(defun ps-face-strikeout-p (face)
  (eq (face-attribute face :strike-through) t))


(defun ps-face-overline-p (face)
  (eq (face-attribute face :overline) t))


(defun ps-face-box-p (face)
  (not (memq (face-attribute face :box) '(nil unspecified))))


;; Emacs understands the %f format; we'll use it to limit color RGB values
;; to three decimals to cut down some on the size of the PostScript output.
(defvar ps-color-format "%0.3f %0.3f %0.3f")
(defvar ps-float-format "%0.3f ")


(defun ps-generate-postscript-with-faces1 (from to)
  ;; Generate some PostScript.
  (let ((face 'default)
	(position to)
	;; Emacs
	(property-change from)
	(overlay-change from)
	before-string after-string)
    (while (< from to)
      (and (< property-change to)  ; Don't search for property change
					; unless previous search succeeded.
	   (setq property-change (next-property-change from nil to)))
      (and (< overlay-change to)   ; Don't search for overlay change
					; unless previous search succeeded.
	   (setq overlay-change (min (next-overlay-change from)
				     to)))
      (setq position (min property-change overlay-change)
	    before-string nil
	    after-string nil)
      (setq face
	    (cond ((invisible-p from)
		   'emacs--invisible--face)
		  ((get-char-property from 'face))
		  (t 'default)))
      ;; Plot up to this record.
      (and before-string
	   (ps-plot-string before-string))
      (ps-plot-with-face from position face)
      (and after-string
	   (ps-plot-string after-string))
      (setq from position))
    (ps-plot-with-face from to face)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ps-def)

;;; ps-def.el ends here
