;;; smiley.el --- displaying smiley faces

;; Copyright (C) 2000, 2001, 2002, 2003 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; A re-written, simplified version of Wes Hardaker's XEmacs smiley.el
;; which might be merged back to smiley.el if we get an assignment for
;; that.  We don't have assignments for the images smiley.el uses, but
;; I'm not sure we need that degree of rococoness and defaults like a
;; yellow background.  Also, using PBM means we can display the images
;; more generally.  -- fx

;;; Test smileys:  :-) :-\ :-( :-/

;;; Code:

(eval-when-compile (require 'cl))
(require 'nnheader)
(require 'gnus-art)

(defgroup smiley nil
  "Turn :-)'s into real images."
  :group 'gnus-visual)

;; Maybe this should go.
(defcustom smiley-data-directory (nnheader-find-etc-directory "images/smilies")
  "*Location of the smiley faces files."
  :type 'directory
  :group 'smiley)

;; The XEmacs version has a baroque, if not rococo, set of these.
(defcustom smiley-regexp-alist
  '(("\\(:-?)\\)\\W" 1 "smile")
    ("\\(;-?)\\)\\W" 1 "blink")
    ("\\(:-]\\)\\W" 1 "forced")
    ("\\(8-)\\)\\W" 1 "braindamaged")
    ("\\(:-|\\)\\W" 1 "indifferent")
    ("\\(:-[/\\]\\)\\W" 1 "wry")
    ("\\(:-(\\)\\W" 1 "sad")
    ("\\(:-{\\)\\W" 1 "frown"))
  "*A list of regexps to map smilies to images.
The elements are (REGEXP MATCH FILE), where MATCH is the submatch in
regexp to replace with IMAGE.  IMAGE is the name of a PBM file in
`smiley-data-directory'."
  :type '(repeat (list regexp
		       (integer :tag "Regexp match number")
		       (string :tag "Image name")))
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (smiley-update-cache))
  :initialize 'custom-initialize-default
  :group 'smiley)

(defcustom gnus-smiley-file-types
  (let ((types (list "pbm")))
    (when (gnus-image-type-available-p 'xpm)
      (push "xpm" types))
    types)
  "*List of suffixes on picon file names to try."
  :version "22.1"
  :type '(repeat string)
  :group 'smiley)

(defvar smiley-cached-regexp-alist nil)

(defun smiley-update-cache ()
  (dolist (elt (if (symbolp smiley-regexp-alist)
		   (symbol-value smiley-regexp-alist)
		 smiley-regexp-alist))
    (let ((types gnus-smiley-file-types)
	  file type)
      (while (and (not file)
		  (setq type (pop types)))
	(unless (file-exists-p
		 (setq file (expand-file-name (concat (nth 2 elt) "." type)
					      smiley-data-directory)))
	  (setq file nil)))
      (when type
	(let ((image (gnus-create-image file (intern type) nil
					:ascent 'center)))
	  (when image
	    (push (list (car elt) (cadr elt) image)
		  smiley-cached-regexp-alist)))))))

(defvar smiley-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-2] 'ignore) ; override widget
    (define-key map [mouse-2]
      'smiley-mouse-toggle-buffer)
    map))

;;;###autoload
(defun smiley-region (start end)
  "Replace in the region `smiley-regexp-alist' matches with corresponding images.
A list of images is returned."
  (interactive "r")
  (when (gnus-graphic-display-p)
    (unless smiley-cached-regexp-alist
      (smiley-update-cache))
    (save-excursion
      (let ((beg (or start (point-min)))
	    group image images string)
	(dolist (entry smiley-cached-regexp-alist)
	  (setq group (nth 1 entry)
		image (nth 2 entry))
	  (goto-char beg)
	  (while (re-search-forward (car entry) end t)
	    (setq string (match-string group))
	    (goto-char (match-end group))
	    (delete-region (match-beginning group) (match-end group))
	    (when image
	      (push image images)
	      (gnus-add-wash-type 'smiley)
	      (gnus-add-image 'smiley image)
	      (gnus-put-image image string 'smiley))))
	images))))

;;;###autoload
(defun smiley-buffer (&optional buffer)
  "Run `smiley-region' at the buffer, specified in the argument or
interactively. If there's no argument, do it at the current buffer"
  (interactive "bBuffer to run smiley-region: ")
  (save-excursion
    (if buffer
	(set-buffer (get-buffer buffer)))
    (smiley-region (point-min) (point-max))))

(defun smiley-toggle-buffer (&optional arg)
  "Toggle displaying smiley faces in article buffer.
With arg, turn displaying on if and only if arg is positive."
  (interactive "P")
  (gnus-with-article-buffer
    (if (if (numberp arg)
	    (> arg 0)
	  (not (memq 'smiley gnus-article-wash-types)))
	(smiley-region (point-min) (point-max))
      (gnus-delete-images 'smiley))))

(defun smiley-mouse-toggle-buffer (event)
  "Toggle displaying smiley faces.
With arg, turn displaying on if and only if arg is positive."
  (interactive "e")
  (save-excursion
    (save-window-excursion
      (mouse-set-point event)
      (smiley-toggle-buffer))))

(provide 'smiley)

;;; arch-tag: 5beb161b-4321-40af-8ac9-876afb8ee818
;;; smiley.el ends here
