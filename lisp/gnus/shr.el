;;; shr.el --- Simple HTML Renderer

;; Copyright (C) 2010 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: html

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

;; This package takes a HTML parse tree (as provided by
;; libxml-parse-html-region) and renders it in the current buffer.  It
;; does not do CSS, JavaScript or anything advanced: It's geared
;; towards rendering typical short snippets of HTML, like what you'd
;; find in HTML email and the like.

;;; Code:

(defgroup shr nil
  "Simple HTML Renderer"
  :group 'mail)

(defcustom shr-max-image-proportion 0.9
  "How big pictures displayed are in relation to the window they're in.
A value of 0.7 means that they are allowed to take up 70% of the
width and height of the window.  If they are larger than this,
and Emacs supports it, then the images will be rescaled down to
fit these criteria."
  :version "24.1"
  :group 'shr
  :type 'float)

(defcustom shr-blocked-images nil
  "Images that have URLs matching this regexp will be blocked."
  :version "24.1"
  :group 'shr
  :type 'regexp)

(defvar shr-folding-mode nil)
(defvar shr-state nil)
(defvar shr-start nil)
(defvar shr-indentation 0)

(defvar shr-width 70)

(defun shr-transform-dom (dom)
  (let ((result (list (pop dom))))
    (dolist (arg (pop dom))
      (push (cons (intern (concat ":" (symbol-name (car arg))) obarray)
		  (cdr arg))
	    result))
    (dolist (sub dom)
      (if (stringp sub)
	  (push (cons :text sub) result)
	(push (shr-transform-dom sub) result)))
    (nreverse result)))

;;;###autoload
(defun shr-insert-document (dom)
  (let ((shr-state nil)
	(shr-start nil))
    (shr-descend (shr-transform-dom dom))))

(defun shr-descend (dom)
  (let ((function (intern (concat "shr-tag-" (symbol-name (car dom))) obarray)))
    (if (fboundp function)
	(funcall function (cdr dom))
      (shr-generic (cdr dom)))))

(defun shr-generic (cont)
  (dolist (sub cont)
    (cond
     ((eq (car sub) :text)
      (shr-insert (cdr sub)))
     ((listp (cdr sub))
      (shr-descend sub)))))

(defun shr-tag-p (cont)
  (shr-ensure-paragraph)
  (shr-generic cont)
  (shr-ensure-paragraph))

(defun shr-ensure-paragraph ()
  (unless (bobp)
    (if (bolp)
	(unless (eql (char-after (- (point) 2)) ?\n)
	  (insert "\n"))
      (if (save-excursion
	    (beginning-of-line)
	    (looking-at " *"))
	  (insert "\n")
	(insert "\n\n")))))

(defun shr-tag-b (cont)
  (shr-fontize-cont cont 'bold))

(defun shr-tag-i (cont)
  (shr-fontize-cont cont 'italic))

(defun shr-tag-u (cont)
  (shr-fontize-cont cont 'underline))

(defun shr-tag-s (cont)
  (shr-fontize-cont cont 'strike-through))

(defun shr-fontize-cont (cont &rest types)
  (let (shr-start)
    (shr-generic cont)
    (dolist (type types)
      (shr-add-font (or shr-start (point)) (point) type))))

(defun shr-add-font (start end type)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face type)))

(defun shr-tag-a (cont)
  (let ((url (cdr (assq :href cont)))
	shr-start)
    (shr-generic cont)
    (widget-convert-button
     'link shr-start (point)
     :action 'shr-browse-url
     :url url
     :keymap widget-keymap
     :help-echo url)))

(defun shr-browse-url (widget &rest stuff)
  (browse-url (widget-get widget :url)))

(defun shr-tag-img (cont)
  (when (and (> (current-column) 0)
	     (not (eq shr-state 'image)))
    (insert "\n"))
  (let ((start (point-marker)))
    (let ((alt (cdr (assq :alt cont)))
	  (url (cdr (assq :src cont))))
      (when (zerop (length alt))
	(setq alt "[img]"))
      (cond
       ((and shr-blocked-images
	     (string-match shr-blocked-images url))
	(insert alt))
       ((url-is-cached (browse-url-url-encode-chars url "[&)$ ]"))
	(shr-put-image (shr-get-image-data url) (point) alt))
       (t
	(insert alt)
	(url-retrieve url 'shr-image-fetched
		      (list (current-buffer) start (point-marker))
		      t)))
      (insert " ")
      (setq shr-state 'image))))

(defun shr-image-fetched (status buffer start end)
  (when (and (buffer-name buffer)
	     (not (plist-get status :error)))
    (url-store-in-cache (current-buffer))
    (when (or (search-forward "\n\n" nil t)
	      (search-forward "\r\n\r\n" nil t))
      (let ((data (buffer-substring (point) (point-max))))
        (with-current-buffer buffer
          (let ((alt (buffer-substring start end))
		(inhibit-read-only t))
	    (delete-region start end)
	    (shr-put-image data start alt))))))
  (kill-buffer (current-buffer)))

(defun shr-put-image (data point alt)
  (if (not (display-graphic-p))
      (insert alt)
    (let ((image (ignore-errors
		   (shr-rescale-image data))))
      (when image
	(put-image image point alt)))))

(defun shr-rescale-image (data)
  (if (or (not (fboundp 'imagemagick-types))
	  (not (get-buffer-window (current-buffer))))
      (create-image data nil t)
    (let* ((image (create-image data nil t))
	   (size (image-size image t))
	   (width (car size))
	   (height (cdr size))
	   (edges (window-inside-pixel-edges
		   (get-buffer-window (current-buffer))))
	   (window-width (truncate (* shr-max-image-proportion
				      (- (nth 2 edges) (nth 0 edges)))))
	   (window-height (truncate (* shr-max-image-proportion
				       (- (nth 3 edges) (nth 1 edges)))))
	   scaled-image)
      (when (> height window-height)
	(setq image (or (create-image data 'imagemagick t
				      :height window-height)
			image))
	(setq size (image-size image t)))
      (when (> (car size) window-width)
	(setq image (or
		     (create-image data 'imagemagick t
				   :width window-width)
		     image)))
      image)))

(defun shr-tag-pre (cont)
  (let ((shr-folding-mode nil))
    (shr-ensure-newline)
    (shr-generic cont)
    (shr-ensure-newline)))

(defun shr-tag-blockquote (cont)
  (let ((shr-indentation (+ shr-indentation 4)))
    (shr-tag-pre cont)))

(defun shr-ensure-newline ()
  (unless (zerop (current-column))
    (insert "\n")))

(defun shr-insert (text)
  (when (eq shr-state 'image)
    (insert "\n")
    (setq shr-state nil))
  (cond
   ((eq shr-folding-mode 'none)
    (insert t))
   (t
    (let ((first t)
	  column)
      (when (and (string-match "\\`[ \t\n]" text)
		 (not (bolp)))
	(insert " "))
      (dolist (elem (split-string text))
	(setq column (current-column))
	(when (> column 0)
	  (cond
	   ((> (+ column (length elem) 1) shr-width)
	    (insert "\n"))
	   ((not first)
	    (insert " "))))
	(setq first nil)
	(when (and (bolp)
		   (> shr-indentation 0))
	  (insert (make-string shr-indentation ? )))
	;; The shr-start is a special variable that is used to pass
	;; upwards the first point in the buffer where the text really
	;; starts.
	(unless shr-start
	  (setq shr-start (point)))
	(insert elem))
      (when (and (string-match "[ \t\n]\\'" text)
		 (not (bolp)))
	(insert " "))))))

(defun shr-get-image-data (url)
  "Get image data for URL.
Return a string with image data."
  (with-temp-buffer
    (mm-disable-multibyte)
    (url-cache-extract (url-cache-create-filename url))
    (when (or (search-forward "\n\n" nil t)
              (search-forward "\r\n\r\n" nil t))
      (buffer-substring (point) (point-max)))))

(defvar shr-list-mode nil)

(defun shr-tag-ul (cont)
  (shr-ensure-paragraph)
  (let ((shr-list-mode 'ul))
    (shr-generic cont)))

(defun shr-tag-ol (cont)
  (let ((shr-list-mode 1))
    (shr-generic cont)))

(defun shr-tag-li (cont)
  (shr-ensure-newline)
  (if (numberp shr-list-mode)
      (progn
	(insert (format "%d " shr-list-mode))
	(setq shr-list-mode (1+ shr-list-mode)))
    (insert "* "))
  (shr-generic cont))

(defun shr-tag-br (cont)
  (shr-ensure-newline)
  (shr-generic cont))

(defun shr-tag-h1 (cont)
  (shr-heading cont 'bold 'underline))

(defun shr-tag-h2 (cont)
  (shr-heading cont 'bold))

(defun shr-tag-h3 (cont)
  (shr-heading cont 'italic))

(defun shr-tag-h4 (cont)
  (shr-heading cont))

(defun shr-tag-h5 (cont)
  (shr-heading cont))

(defun shr-tag-h6 (cont)
  (shr-heading cont))

(defun shr-heading (cont &rest types)
  (shr-ensure-paragraph)
  (apply #'shr-fontize-cont cont types)
  (shr-ensure-paragraph))

(provide 'shr)

;;; shr.el ends here
