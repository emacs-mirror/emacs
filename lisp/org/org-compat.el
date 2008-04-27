;;; org-compat.el --- Compatibility code for Org-mode

;; Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.02b
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains code needed for compatibility with XEmacs and older
;; versions of GNU Emacs.

;;; Code:

(defconst org-xemacs-p (featurep 'xemacs)) ; not used by org.el itself
(defconst org-format-transports-properties-p
  (let ((x "a"))
    (add-text-properties 0 1 '(test t) x)
    (get-text-property 0 'test (format "%s" x)))
  "Does format transport text properties?")

(defun org-compatible-face (inherits specs)
  "Make a compatible face specification.
If INHERITS is an existing face and if the Emacs version supports it,
just inherit the face.  If not, use SPECS to define the face.
XEmacs and Emacs 21 do not know about the `min-colors' attribute.
For them we convert a (min-colors 8) entry to a `tty' entry and move it
to the top of the list.  The `min-colors' attribute will be removed from
any other entries, and any resulting duplicates will be removed entirely."
  (cond
   ((and inherits (facep inherits)
	 (not (featurep 'xemacs)) (> emacs-major-version 22))
    ;; In Emacs 23, we use inheritance where possible.
    ;; We only do this in Emacs 23, because only there the outline
    ;; faces have been changed to the original org-mode-level-faces.
    (list (list t :inherit inherits)))
   ((or (featurep 'xemacs) (< emacs-major-version 22))
    ;; These do not understand the `min-colors' attribute.
    (let (r e a)
      (while (setq e (pop specs))
	(cond
	 ((memq (car e) '(t default)) (push e r))
	 ((setq a (member '(min-colors 8) (car e)))
	  (nconc r (list (cons (cons '(type tty) (delq (car a) (car e)))
			       (cdr e)))))
	 ((setq a (assq 'min-colors (car e)))
	  (setq e (cons (delq a (car e)) (cdr e)))
	  (or (assoc (car e) r) (push e r)))
	 (t (or (assoc (car e) r) (push e r)))))
      (nreverse r)))
   (t specs)))
(put 'org-compatible-face 'lisp-indent-function 1)

;;;; Emacs/XEmacs compatibility

;; Overlay compatibility functions
(defun org-make-overlay (beg end &optional buffer)
  (if (featurep 'xemacs)
      (make-extent beg end buffer)
    (make-overlay beg end buffer)))
(defun org-delete-overlay (ovl)
  (if (featurep 'xemacs) (progn (delete-extent ovl) nil) (delete-overlay ovl)))
(defun org-detach-overlay (ovl)
  (if (featurep 'xemacs) (detach-extent ovl) (delete-overlay ovl)))
(defun org-move-overlay (ovl beg end &optional buffer)
  (if (featurep 'xemacs)
      (set-extent-endpoints ovl beg end (or buffer (current-buffer)))
    (move-overlay ovl beg end buffer)))
(defun org-overlay-put (ovl prop value)
  (if (featurep 'xemacs)
      (set-extent-property ovl prop value)
    (overlay-put ovl prop value)))
(defun org-overlay-display (ovl text &optional face evap)
  "Make overlay OVL display TEXT with face FACE."
  (if (featurep 'xemacs)
      (let ((gl (make-glyph text)))
	(and face (set-glyph-face gl face))
	(set-extent-property ovl 'invisible t)
	(set-extent-property ovl 'end-glyph gl))
    (overlay-put ovl 'display text)
    (if face (overlay-put ovl 'face face))
    (if evap (overlay-put ovl 'evaporate t))))
(defun org-overlay-before-string (ovl text &optional face evap)
  "Make overlay OVL display TEXT with face FACE."
  (if (featurep 'xemacs)
      (let ((gl (make-glyph text)))
	(and face (set-glyph-face gl face))
	(set-extent-property ovl 'begin-glyph gl))
    (if face (org-add-props text nil 'face face))
    (overlay-put ovl 'before-string text)
    (if evap (overlay-put ovl 'evaporate t))))
(defun org-overlay-get (ovl prop)
  (if (featurep 'xemacs)
      (extent-property ovl prop)
    (overlay-get ovl prop)))
(defun org-overlays-at (pos)
  (if (featurep 'xemacs) (extents-at pos) (overlays-at pos)))
(defun org-overlays-in (&optional start end)
  (if (featurep 'xemacs)
      (extent-list nil start end)
    (overlays-in start end)))
(defun org-overlay-start (o)
  (if (featurep 'xemacs) (extent-start-position o) (overlay-start o)))
(defun org-overlay-end (o)
  (if (featurep 'xemacs) (extent-end-position o) (overlay-end o)))
(defun org-overlay-buffer (o)
  (if (featurep 'xemacs) (extent-buffer o) (overlay-buffer o)))
(defun org-find-overlays (prop &optional pos delete)
  "Find all overlays specifying PROP at POS or point.
If DELETE is non-nil, delete all those overlays."
  (let ((overlays (org-overlays-at (or pos (point))))
	ov found)
    (while (setq ov (pop overlays))
      (if (org-overlay-get ov prop)
          (if delete (org-delete-overlay ov) (push ov found))))
    found))

(defun org-add-hook (hook function &optional append local)
  "Add-hook, compatible with both Emacsen."
  (if (and local (featurep 'xemacs))
      (add-local-hook hook function append)
    (add-hook hook function append local)))

(defun org-add-props (string plist &rest props)
  "Add text properties to entire string, from beginning to end.
PLIST may be a list of properties, PROPS are individual properties and values
that will be added to PLIST.  Returns the string that was modified."
  (add-text-properties
   0 (length string) (if props (append plist props) plist) string)
  string)
(put 'org-add-props 'lisp-indent-function 2)

;; Region compatibility

(defvar org-ignore-region nil
  "To temporarily disable the active region.")

(defun org-region-active-p ()
  "Is `transient-mark-mode' on and the region active?
Works on both Emacs and XEmacs."
  (if org-ignore-region
      nil
    (if (featurep 'xemacs)
	(and zmacs-regions (region-active-p))
      (if (fboundp 'use-region-p)
	  (use-region-p)
	(and transient-mark-mode mark-active))))) ; Emacs 22 and before

;; Invisibility compatibility

(defun org-add-to-invisibility-spec (arg)
  "Add elements to `buffer-invisibility-spec'.
See documentation for `buffer-invisibility-spec' for the kind of elements
that can be added."
  (cond
   ((fboundp 'add-to-invisibility-spec)
    (add-to-invisibility-spec arg))
   ((or (null buffer-invisibility-spec) (eq buffer-invisibility-spec t))
	(setq buffer-invisibility-spec (list arg)))
   (t
    (setq buffer-invisibility-spec
	  (cons arg buffer-invisibility-spec)))))

(defun org-remove-from-invisibility-spec (arg)
  "Remove elements from `buffer-invisibility-spec'."
  (if (fboundp 'remove-from-invisibility-spec)
      (remove-from-invisibility-spec arg)
    (if (consp buffer-invisibility-spec)
	(setq buffer-invisibility-spec
	      (delete arg buffer-invisibility-spec)))))

(defun org-in-invisibility-spec-p (arg)
  "Is ARG a member of `buffer-invisibility-spec'?"
  (if (consp buffer-invisibility-spec)
      (member arg buffer-invisibility-spec)
    nil))

(defun org-indent-to-column (column &optional minimum buffer)
  "Work around a bug with extents with invisibility in XEmacs."
 (if (featurep 'xemacs)
     (let ((ext-inv (extent-list
                     nil (point-at-bol) (point-at-eol)
                     'all-extents-closed-open 'invisible))
           ext-inv-specs)
       (dolist (ext ext-inv)
         (when (extent-property ext 'invisible)
           (add-to-list 'ext-inv-specs (list ext (extent-property
						  ext 'invisible)))
           (set-extent-property ext 'invisible nil)))
       (indent-to-column column minimum buffer)
       (dolist (ext-inv-spec ext-inv-specs)
         (set-extent-property (car ext-inv-spec) 'invisible
			      (cadr ext-inv-spec))))
   (indent-to-column column minimum)))

(defun org-indent-line-to (column)
  "Work around a bug with extents with invisibility in XEmacs."
 (if (featurep 'xemacs)
     (let ((ext-inv (extent-list
                     nil (point-at-bol) (point-at-eol)
                     'all-extents-closed-open 'invisible))
           ext-inv-specs)
       (dolist (ext ext-inv)
         (when (extent-property ext 'invisible)
           (add-to-list 'ext-inv-specs (list ext (extent-property
						  ext 'invisible)))
           (set-extent-property ext 'invisible nil)))
       (indent-line-to column)
       (dolist (ext-inv-spec ext-inv-specs)
         (set-extent-property (car ext-inv-spec) 'invisible
			      (cadr ext-inv-spec))))
   (indent-line-to column)))

(defun org-move-to-column (column &optional force buffer)
 (if (featurep 'xemacs)
     (let ((ext-inv (extent-list
                     nil (point-at-bol) (point-at-eol)
                     'all-extents-closed-open 'invisible))
           ext-inv-specs)
       (dolist (ext ext-inv)
         (when (extent-property ext 'invisible)
           (add-to-list 'ext-inv-specs (list ext (extent-property ext
								  'invisible)))
           (set-extent-property ext 'invisible nil)))
       (move-to-column column force buffer)
       (dolist (ext-inv-spec ext-inv-specs)
         (set-extent-property (car ext-inv-spec) 'invisible
			      (cadr ext-inv-spec))))
   (move-to-column column force)))
 

(provide 'org-compat)

;;; org-compat.el ends here
