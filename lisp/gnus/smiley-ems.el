;;; smiley-ems.el --- displaying smiley faces

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: news mail multimedia

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A re-written, simplified version of Wes Hardaker's XEmacs smiley.el
;; which might be merged back to smiley.el if we get an assignment for
;; that.  We don't have assignments for the images smiley.el uses, but
;; I'm not sure we need that degree of rococoness and defaults like a
;; yellow background.  Also, using PBM means we can display the images
;; more generally.  -- fx

;;; Test smileys:  :-) :-\ :-( :-/

;;; Code:

(require 'nnheader)

(defgroup smiley nil
  "Turn :-)'s into real images."
  :group 'gnus-visual)

;; Maybe this should go.
(defcustom smiley-data-directory (nnheader-find-etc-directory "smilies")
  "*If non-nil, a directory to search for the smiley image files.
This is in addition to the normal image search path."
  :type '(choice directory
		 (const nil))
  :group 'smiley)

;; The XEmacs version has a baroque, if not rococo, set of these.
(defcustom smiley-regexp-alist
  ;; Perhaps :-) should be distinct -- it does appear in the Jargon File.
  '(("\\([:;]-?)\\)\\W" 1 "smile.pbm")
    ("\\(:-[/\\]\\)\\W" 1 "wry.pbm")
    ("\\(:-[({]\\)\\W" 1 "frown.pbm"))
  "*A list of regexps to map smilies to images.
The elements are (REGEXP MATCH FILE), where MATCH is the submatch in
rgexp to replace with IMAGE.  IMAGE is the name of a PBM file in
`smiley-data-directory' or the normal image search path."
  :type '(repeat (list regexp
		       (integer :tag "Regexp match number")
		       (string :tag "Image name")))
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (smiley-update-cache))
  :initialize 'custom-initialize-default
  :group 'smiley)

(defvar smiley-cached-regexp-alist nil)

(defun smiley-update-cache ()
  (dolist (elt smiley-regexp-alist)
    (let* ((data-directory smiley-data-directory)
	   (image (find-image (list (list :type 'pbm
					  :file (nth 2 elt)
					  :ascent 'center)))))
      (if image
	  (push (list (car elt) (cadr elt) image)
		smiley-cached-regexp-alist)))))

(defvar smiley-active nil
  "Non-nil means smilies in the buffer will be displayed.")
(make-variable-buffer-local 'smiley-active)

(defvar smiley-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-2] 'ignore) ; override widget
    (define-key map [mouse-2]
      'smiley-mouse-toggle-buffer)
    map))

;;;###autoload
(defun smiley-region (start end)
  "Display textual smileys as images.
START and END specify the region; interactively, use the values
of point and mark.  The value of `smiley-regexp-alist' determines
which smileys to operate on and which images to use for them."
  (interactive "r")
  (when (and (fboundp 'display-graphic-p)
	     (display-graphic-p))
    (mapc (lambda (o)
	    (if (eq 'smiley (overlay-get o 'smiley))
		(delete-overlay o)))
	  (overlays-in start end))
    (unless smiley-cached-regexp-alist
      (smiley-update-cache))
    (save-excursion
      (let ((beg (or start (point-min)))
	    group overlay image)
	(dolist (entry smiley-cached-regexp-alist)
	  (setq group (nth 1 entry)
		image (nth 2 entry))
	  (goto-char beg)
	  (while (re-search-forward (car entry) end t)
	    (when image
	      (setq overlay (make-overlay (match-beginning group)
					  (match-end group)))
	      (overlay-put overlay
			   'display `(when smiley-active ,@image))
	      (overlay-put overlay 'mouse-face 'highlight)
	      (overlay-put overlay 'smiley t)
	      (overlay-put overlay
			   'help-echo "mouse-2: toggle smilies in buffer")
	      (overlay-put overlay 'keymap smiley-mouse-map))))))
        (setq smiley-active t)))

(defun smiley-toggle-buffer (&optional arg)
  "Toggle displaying smiley faces.
With arg, turn displaying on if and only if arg is positive."
  (interactive "P")
  (if (numberp arg)
      (setq smiley-active (> arg 0))
    (setq smiley-active (not smiley-active))))

(defun smiley-mouse-toggle-buffer (event)
  "Toggle displaying smiley faces.
With arg, turn displaying on if and only if arg is positive."
  (interactive "e")
  (save-excursion
    (save-window-excursion
      (mouse-set-point event)
      (smiley-toggle-buffer))))

(eval-when-compile (defvar gnus-article-buffer))

(defun gnus-smiley-display (&optional arg)
  "Display textual emoticaons (\"smilies\") as small graphical icons.
With arg, turn displaying on if and only if arg is positive."
  (interactive "P")
  (save-excursion
    (set-buffer gnus-article-buffer)
    (save-restriction
      (widen)
      (article-goto-body)
      (smiley-region (point-min) (point-max))
      (if (and (numberp arg) (<= arg 0))
	  (smiley-toggle-buffer arg)))))

(provide 'smiley)

;;; smiley-ems.el ends here
