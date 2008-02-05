;;; ps-def.el --- XEmacs and Emacs definitions for ps-print

;; Copyright (C) 2007, 2008 Free Software Foundation, Inc.

;; Author: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;;	Kenichi Handa <handa@m17n.org> (multi-byte characters)
;; Maintainer: Kenichi Handa <handa@m17n.org> (multi-byte characters)
;;	Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Keywords: wp, print, PostScript
;; X-URL: http://www.emacswiki.org/cgi-bin/wiki/ViniciusJoseLatorre

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; GNU Emacs is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; See ps-print.el for documentation.

;;; Code:

(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r))))

(declare-function ps-plot-with-face "ps-print" (from to face))
(declare-function ps-plot-string    "ps-print" (string))

(defvar ps-bold-faces)                  ; in ps-print.el
(defvar ps-italic-faces)



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XEmacs Definitions


(cond
 ((featurep 'xemacs)			; XEmacs


  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ps-bdf

  (defvar installation-directory nil)
  (defvar coding-system-for-read)


  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ps-mule

  (defvar leading-code-private-22 157)

  (or (fboundp 'charset-bytes)
      (defun charset-bytes (charset) 1)) ; ascii

  (or (fboundp 'charset-dimension)
      (defun charset-dimension (charset) 1)) ; ascii

  (or (fboundp 'charset-id)
      (defun charset-id (charset) 0))	; ascii

  (or (fboundp 'charset-width)
      (defun charset-width (charset) 1)) ; ascii

  (or (fboundp 'find-charset-region)
      (defun find-charset-region (beg end &optional table)
	(list 'ascii)))

  (or (fboundp 'char-width)
      (defun char-width (char) 1))	; ascii

  (or (fboundp 'chars-in-region)
      (defun chars-in-region (beg end)
	(- (max beg end) (min beg end))))

  (or (fboundp 'forward-point)
      (defun forward-point (arg)
	(save-excursion
	  (let ((count (abs arg))
		(step  (if (zerop arg)
			   0
			 (/ arg arg))))
	    (while (and (> count 0)
			(< (point-min) (point)) (< (point) (point-max)))
	      (forward-char step)
	      (setq count (1- count)))
	    (+ (point) (* count step))))))

  (or (fboundp 'decompose-composite-char)
      (defun decompose-composite-char (char &optional type
					    with-composition-rule)
	nil))

  (or (fboundp 'encode-coding-string)
      (defun encode-coding-string (string coding-system &optional nocopy)
	(if nocopy
	    string
	  (copy-sequence string))))

  (or (fboundp 'coding-system-p)
      (defun coding-system-p (obj) nil))

  (or (fboundp 'ccl-execute-on-string)
      (defun ccl-execute-on-string (ccl-prog status str
					     &optional contin unibyte-p)
	str))

  (or (fboundp 'define-ccl-program)
      (defmacro define-ccl-program (name ccl-program &optional doc)
	`(defconst ,name nil ,doc)))

  (or (fboundp 'multibyte-string-p)
      (defun multibyte-string-p (str)
	(let ((len (length str))
	      (i 0)
	      multibyte)
	  (while (and (< i len) (not (setq multibyte (> (aref str i) 255))))
	    (setq i (1+ i)))
	  multibyte)))

  (or (fboundp 'string-make-multibyte)
      (defalias 'string-make-multibyte 'copy-sequence))

  (or (fboundp 'encode-char)
      (defun encode-char (ch ccs)
	ch))


  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ps-print

  ;; GNU Emacs
  (or (fboundp 'line-beginning-position)
      (defun line-beginning-position (&optional n)
	(save-excursion
	  (and n (/= n 1) (forward-line (1- n)))
	  (beginning-of-line)
	  (point))))


  ;; GNU Emacs
  (or (fboundp 'find-composition)
      (defalias 'find-composition 'ignore))


  (defun ps-xemacs-color-name (color)
    (if (color-specifier-p color)
	(color-name color)
      color))


  (defalias 'ps-mark-active-p 'region-active-p)


  (defun ps-face-foreground-name (face)
    (ps-xemacs-color-name (face-foreground face)))


  (defun ps-face-background-name (face)
    (ps-xemacs-color-name (face-background face)))


  (defalias 'ps-frame-parameter 'frame-property)


  ;; Return t if the device (which can be changed during an emacs session)
  ;; can handle colors.
  ;; XEmacs change: Need to check for emacs-major-version too.
  (if (or (> emacs-major-version 19)
	  (and (= emacs-major-version 19)
	       (>= emacs-minor-version 12)))
      ;; xemacs >= 19.12
      (defun ps-color-device ()
	(eq (device-class) 'color))
    ;; xemacs < 19.12
    (setq ps-print-color-p nil)
    (defalias 'ps-color-device 'ignore))


  (defun ps-mapper (extent list)
    (nconc list
	   (list (list (extent-start-position extent) 'push extent)
		 (list (extent-end-position extent) 'pull extent)))
    nil)


  (defun ps-extent-sorter (a b)
    (< (extent-priority a) (extent-priority b)))


  (defun ps-xemacs-face-kind-p (face kind kind-regex)
    (let* ((frame-font (or (face-font-instance face)
			   (face-font-instance 'default)))
	   (kind-cons
	    (and frame-font
		 (assq kind
		       (font-instance-properties frame-font))))
	   (kind-spec (cdr-safe kind-cons))
	   (case-fold-search t))
      (and kind-spec (string-match kind-regex kind-spec))))


  ;; to avoid XEmacs compilation gripes
  (defvar coding-system-for-write)
  (defvar buffer-file-coding-system)


  (and (fboundp 'find-coding-system)
       (or (funcall 'find-coding-system 'raw-text-unix)
	   (funcall 'copy-coding-system 'no-conversion-unix 'raw-text-unix)))


  (defun ps-color-values (x-color)
    (let ((color (ps-xemacs-color-name x-color)))
      (cond
       ((fboundp 'x-color-values)
	(funcall 'x-color-values color))
       ((and (fboundp 'color-instance-rgb-components)
	     (ps-color-device))
	(funcall 'color-instance-rgb-components
		 (if (color-instance-p x-color)
		     x-color
		   (make-color-instance color))))
       (t
	(error "No available function to determine X color values")))))


  (defun ps-face-bold-p (face)
    (or (ps-xemacs-face-kind-p face 'WEIGHT_NAME "bold\\|demibold")
	(memq face ps-bold-faces)))	; Kludge-compatible


  (defun ps-face-italic-p (face)
    (or (ps-xemacs-face-kind-p face 'ANGLE_NAME "i\\|o")
	(ps-xemacs-face-kind-p face 'SLANT "i\\|o")
	(memq face ps-italic-faces)))	; Kludge-compatible


  (defalias 'ps-face-strikeout-p 'ignore)


  (defalias 'ps-face-overline-p 'ignore)


  (defalias 'ps-face-box-p 'ignore)


  ;; XEmacs will have to make do with %s (princ) for floats.
  (defvar ps-color-format "%s %s %s")
  (defvar ps-float-format "%s ")


  (defun ps-generate-postscript-with-faces1 (from to)
    ;; Generate some PostScript.
    (let ((face 'default)
	  (position to)
	  ;; XEmacs
	  ;; Build the list of extents...
	  (a (cons 'dummy nil))
	  record type extent extent-list)
      (map-extents 'ps-mapper nil from to a)
      (setq a (sort (cdr a) 'car-less-than-car)
	    extent-list nil)

      ;; Loop through the extents...
      (while a
	(setq record (car a)
	      position (car record)

	      record (cdr record)
	      type (car record)

	      record (cdr record)
	      extent (car record))

	;; Plot up to this record.
	;; XEmacs 19.12: for some reason, we're getting into a
	;; situation in which some of the records have
	;; positions less than 'from'.  Since we've narrowed
	;; the buffer, this'll generate errors.  This is a hack,
	;; but don't call ps-plot-with-face unless from > point-min.
	(and (>= from (point-min))
	     (ps-plot-with-face from (min position (point-max)) face))

	(cond
	 ((eq type 'push)
	  (and (extent-face extent)
	       (setq extent-list (sort (cons extent extent-list)
				       'ps-extent-sorter))))

	 ((eq type 'pull)
	  (setq extent-list (sort (delq extent extent-list)
				  'ps-extent-sorter))))

	(setq face (if extent-list
		       (extent-face (car extent-list))
		     'default)
	      from position
	      a (cdr a)))

      (ps-plot-with-face from to face)))

  )
 (t					; Emacs
  ;; Do nothing
  ))					; end cond featurep



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Definitions


(cond
 ((featurep 'xemacs)			; XEmacs
  ;; Do nothing
  )
 (t					; Emacs


  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ps-print


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
	  (save-buffer-invisibility-spec buffer-invisibility-spec)
	  (buffer-invisibility-spec nil)
	  before-string after-string)
      (while (< from to)
	(and (< property-change to)	; Don't search for property change
					; unless previous search succeeded.
	     (setq property-change (next-property-change from nil to)))
	(and (< overlay-change to)	; Don't search for overlay change
					; unless previous search succeeded.
	     (setq overlay-change (min (next-overlay-change from)
				       to)))
	(setq position (min property-change overlay-change)
	      before-string nil
	      after-string nil)
	;; The code below is not quite correct,
	;; because a non-nil overlay invisible property
	;; which is inactive according to the current value
	;; of buffer-invisibility-spec nonetheless overrides
	;; a face text property.
	(setq face
	      (cond ((let ((prop (get-text-property from 'invisible)))
		       ;; Decide whether this invisible property
		       ;; really makes the text invisible.
		       (if (eq save-buffer-invisibility-spec t)
			   (not (null prop))
			 (or (memq prop save-buffer-invisibility-spec)
			     (assq prop save-buffer-invisibility-spec))))
		     'emacs--invisible--face)
		    ((get-text-property from 'face))
		    (t 'default)))
	(let ((overlays (overlays-at from))
	      (face-priority -1))	; text-property
	  (while (and overlays
		      (not (eq face 'emacs--invisible--face)))
	    (let* ((overlay (car overlays))
		   (overlay-invisible
		    (overlay-get overlay 'invisible))
		   (overlay-priority
		    (or (overlay-get overlay 'priority) 0)))
	      (and (> overlay-priority face-priority)
		   (setq before-string
			 (or (overlay-get overlay 'before-string)
			     before-string)
			 after-string
			 (or (and (<= (overlay-end overlay) position)
				  (overlay-get overlay 'after-string))
			     after-string)
			 face-priority overlay-priority
			 face
			 (cond
			  ((if (eq save-buffer-invisibility-spec t)
			       (not (null overlay-invisible))
			     (or (memq overlay-invisible
				       save-buffer-invisibility-spec)
				 (assq overlay-invisible
				       save-buffer-invisibility-spec)))
			   'emacs--invisible--face)
			  ((overlay-get overlay 'face))
			  (t face)
			  ))))
	    (setq overlays (cdr overlays))))
	;; Plot up to this record.
	(and before-string
	     (ps-plot-string before-string))
	(ps-plot-with-face from position face)
	(and after-string
	     (ps-plot-string after-string))
	(setq from position))
      (ps-plot-with-face from to face)))

  ))					; end cond featurep


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ps-def)

;; arch-tag: 4edde45b-af10-4685-b8ee-7cd0f951095a
;;; ps-def.el ends here
