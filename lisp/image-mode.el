;;; image-mode.el --- support for visiting image files
;;
;; Copyright (C) 2005, 2006, 2007, 2008 Free Software Foundation, Inc.
;;
;; Author: Richard Stallman <rms@gnu.org>
;; Keywords: multimedia

;; This file is part of GNU Emacs.

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

;;; Commentary:

;; Defines a major mode for visiting image files
;; that allows conversion between viewing the text of the file
;; and viewing the file as an image.  Viewing the image
;; works by putting a `display' text-property on the
;; image data, with the image-data still present underneath; if the
;; resulting buffer file is saved to another name it will correctly save
;; the image data to the new file.

;;; Code:

(require 'image)
(eval-when-compile (require 'cl))

;;;###autoload (push '("\\.jpe?g\\'"    . image-mode) auto-mode-alist)
;;;###autoload (push '("\\.png\\'"      . image-mode) auto-mode-alist)
;;;###autoload (push '("\\.gif\\'"      . image-mode) auto-mode-alist)
;;;###autoload (push '("\\.tiff?\\'"    . image-mode) auto-mode-alist)
;;;###autoload (push '("\\.p[bpgn]m\\'" . image-mode) auto-mode-alist)

;;;###autoload (push '("\\.x[bp]m\\'"   . c-mode)     auto-mode-alist)
;;;###autoload (push '("\\.x[bp]m\\'"   . image-mode-maybe) auto-mode-alist)

;;;###autoload (push '("\\.svgz?\\'"    . xml-mode)   auto-mode-alist)
;;;###autoload (push '("\\.svgz?\\'"    . image-mode-maybe) auto-mode-alist)

;;; Image mode window-info management.

(defvar image-mode-winprops-alist t)
(make-variable-buffer-local 'image-mode-winprops-alist)

(defvar image-mode-new-window-functions nil
  "Special hook run when image data is requested in a new window.
It is called with one argument, the initial WINPROPS.")

(defun image-mode-winprops (&optional window)
  "Return winprops of WINDOW.
A winprops object has the shape (WINDOW . ALIST)."
  (unless window (setq window (selected-window)))
  (let ((winprops (assq window image-mode-winprops-alist)))
    ;; For new windows, set defaults from the latest.
    (unless winprops
      (setq winprops (cons window
                           (copy-alist (cdar image-mode-winprops-alist))))
      (run-hook-with-args 'image-mode-new-window-functions winprops))
    ;; Move window to front.
    (setq image-mode-winprops-alist
          (cons winprops (delq winprops image-mode-winprops-alist)))
    winprops))

(defun image-mode-window-get (prop &optional winprops)
  (unless (consp winprops) (setq winprops (image-mode-winprops winprops)))
  (cdr (assq prop (cdr winprops))))

(defsetf image-mode-window-get (prop &optional winprops) (val)
  `(image-mode-window-put ,prop ,val ,winprops))

(defun image-mode-window-put (prop val &optional winprops)
  (unless (consp winprops) (setq winprops (image-mode-winprops winprops)))
  (setcdr winprops (cons (cons prop val)
                         (delq (assq prop (cdr winprops)) (cdr winprops)))))

(defun image-set-window-vscroll (vscroll)
  (setf (image-mode-window-get 'vscroll) vscroll)
  (set-window-vscroll (selected-window) vscroll))

(defun image-set-window-hscroll (ncol)
  (setf (image-mode-window-get 'hscroll) ncol)
  (set-window-hscroll (selected-window) ncol))

(defun image-mode-reapply-winprops ()
  ;; When set-window-buffer, set hscroll and vscroll to what they were
  ;; last time the image was displayed in this window.
  (when (listp image-mode-winprops-alist)
    (let* ((winprops (image-mode-winprops))
           (hscroll (image-mode-window-get 'hscroll winprops))
           (vscroll (image-mode-window-get 'vscroll winprops)))
      (if hscroll (set-window-hscroll (selected-window) hscroll))
      (if vscroll (set-window-vscroll (selected-window) vscroll)))))

(defun image-mode-setup-winprops ()
  ;; Record current scroll settings.
  (unless (listp image-mode-winprops-alist)
    (setq image-mode-winprops-alist nil))
  (add-hook 'window-configuration-change-hook
 	    'image-mode-reapply-winprops nil t))

;;; Image scrolling functions

(defun image-get-display-property ()
  (get-char-property (point-min) 'display
                     ;; There might be different images for different displays.
                     (if (eq (window-buffer) (current-buffer))
                         (selected-window))))

(defun image-forward-hscroll (&optional n)
  "Scroll image in current window to the left by N character widths.
Stop if the right edge of the image is reached."
  (interactive "p")
  (cond ((= n 0) nil)
	((< n 0)
	 (image-set-window-hscroll (max 0 (+ (window-hscroll) n))))
	(t
	 (let* ((image (image-get-display-property))
		(edges (window-inside-edges))
		(win-width (- (nth 2 edges) (nth 0 edges)))
		(img-width (ceiling (car (image-size image)))))
	   (image-set-window-hscroll (min (max 0 (- img-width win-width))
					  (+ n (window-hscroll))))))))

(defun image-backward-hscroll (&optional n)
  "Scroll image in current window to the right by N character widths.
Stop if the left edge of the image is reached."
  (interactive "p")
  (image-forward-hscroll (- n)))

(defun image-next-line (&optional n)
  "Scroll image in current window upward by N lines.
Stop if the bottom edge of the image is reached."
  (interactive "p")
  (cond ((= n 0) nil)
	((< n 0)
	 (image-set-window-vscroll (max 0 (+ (window-vscroll) n))))
	(t
	 (let* ((image (image-get-display-property))
		(edges (window-inside-edges))
		(win-height (- (nth 3 edges) (nth 1 edges)))
		(img-height (ceiling (cdr (image-size image)))))
	   (image-set-window-vscroll (min (max 0 (- img-height win-height))
					  (+ n (window-vscroll))))))))

(defun image-previous-line (&optional n)
  "Scroll image in current window downward by N lines.
Stop if the top edge of the image is reached."
  (interactive "p")
  (image-next-line (- n)))

(defun image-scroll-up (&optional n)
  "Scroll image in current window upward by N lines.
Stop if the bottom edge of the image is reached.
If ARG is omitted or nil, scroll upward by a near full screen.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll downward.
If ARG is the atom `-', scroll downward by nearly full screen.
When calling from a program, supply as argument a number, nil, or `-'."
  (interactive "P")
  (cond ((null n)
	 (let* ((edges (window-inside-edges))
		(win-height (- (nth 3 edges) (nth 1 edges))))
	   (image-next-line
	    (max 0 (- win-height next-screen-context-lines)))))
	((eq n '-)
	 (let* ((edges (window-inside-edges))
		(win-height (- (nth 3 edges) (nth 1 edges))))
	   (image-next-line
	    (min 0 (- next-screen-context-lines win-height)))))
	(t (image-next-line (prefix-numeric-value n)))))

(defun image-scroll-down (&optional n)
  "Scroll image in current window downward by N lines.
Stop if the top edge of the image is reached.
If ARG is omitted or nil, scroll downward by a near full screen.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll upward.
If ARG is the atom `-', scroll upward by nearly full screen.
When calling from a program, supply as argument a number, nil, or `-'."
  (interactive "P")
  (cond ((null n)
	 (let* ((edges (window-inside-edges))
		(win-height (- (nth 3 edges) (nth 1 edges))))
	   (image-next-line
	    (min 0 (- next-screen-context-lines win-height)))))
	((eq n '-)
	 (let* ((edges (window-inside-edges))
		(win-height (- (nth 3 edges) (nth 1 edges))))
	   (image-next-line
	    (max 0 (- win-height next-screen-context-lines)))))
	(t (image-next-line (- (prefix-numeric-value n))))))

(defun image-bol (arg)
  "Scroll horizontally to the left edge of the image in the current window.
With argument ARG not nil or 1, move forward ARG - 1 lines first,
stopping if the top or bottom edge of the image is reached."
  (interactive "p")
  (and arg
       (/= (setq arg (prefix-numeric-value arg)) 1)
       (image-next-line (- arg 1)))
  (image-set-window-hscroll 0))

(defun image-eol (arg)
  "Scroll horizontally to the right edge of the image in the current window.
With argument ARG not nil or 1, move forward ARG - 1 lines first,
stopping if the top or bottom edge of the image is reached."
  (interactive "p")
  (and arg
       (/= (setq arg (prefix-numeric-value arg)) 1)
       (image-next-line (- arg 1)))
  (let* ((image (image-get-display-property))
	 (edges (window-inside-edges))
	 (win-width (- (nth 2 edges) (nth 0 edges)))
	 (img-width (ceiling (car (image-size image)))))
    (image-set-window-hscroll (max 0 (- img-width win-width)))))

(defun image-bob ()
  "Scroll to the top-left corner of the image in the current window."
  (interactive)
  (image-set-window-hscroll 0)
  (image-set-window-vscroll 0))

(defun image-eob ()
  "Scroll to the bottom-right corner of the image in the current window."
  (interactive)
  (let* ((image (image-get-display-property))
	 (edges (window-inside-edges))
	 (win-width (- (nth 2 edges) (nth 0 edges)))
	 (img-width (ceiling (car (image-size image))))
	 (win-height (- (nth 3 edges) (nth 1 edges)))
	 (img-height (ceiling (cdr (image-size image)))))
    (image-set-window-hscroll (max 0 (- img-width win-width)))
    (image-set-window-vscroll (max 0 (- img-height win-height)))))

;; Adjust frame and image size.

(defun image-mode-fit-frame ()
  "Fit the frame to the current image.
This function assumes the current frame has only one window."
  ;; FIXME: This does not take into account decorations like mode-line,
  ;; minibuffer, header-line, ...
  (interactive)
  (let* ((saved (frame-parameter nil 'image-mode-saved-size))
         (display (image-get-display-property))
         (size (image-size display)))
    (if (and saved
             (eq (caar saved) (frame-width))
             (eq (cdar saved) (frame-height)))
        (progn ;; Toggle back to previous non-fitted size.
          (set-frame-parameter nil 'image-mode-saved-size nil)
          (setq size (cdr saved)))
      ;; Round up size, and save current size so we can toggle back to it.
      (setcar size (ceiling (car size)))
      (setcdr size (ceiling (cdr size)))
      (set-frame-parameter nil 'image-mode-saved-size
                           (cons size (cons (frame-width) (frame-height)))))
    (set-frame-width  (selected-frame) (car size))
    (set-frame-height (selected-frame) (cdr size))))

;;; Image Mode setup

(defvar image-type nil
  "Current image type.
This variable is used to display the current image type in the mode line.")
(make-variable-buffer-local 'image-type)

(defvar image-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'image-toggle-display)
    (define-key map [remap forward-char] 'image-forward-hscroll)
    (define-key map [remap backward-char] 'image-backward-hscroll)
    (define-key map [remap previous-line] 'image-previous-line)
    (define-key map [remap next-line] 'image-next-line)
    (define-key map [remap scroll-up] 'image-scroll-up)
    (define-key map [remap scroll-down] 'image-scroll-down)
    (define-key map [remap move-beginning-of-line] 'image-bol)
    (define-key map [remap move-end-of-line] 'image-eol)
    (define-key map [remap beginning-of-buffer] 'image-bob)
    (define-key map [remap end-of-buffer] 'image-eob)
    map)
  "Major mode keymap for viewing images in Image mode.")

(defvar image-mode-text-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'image-toggle-display)
    map)
  "Major mode keymap for viewing images as text in Image mode.")

(defvar bookmark-make-record-function)

;;;###autoload
(defun image-mode ()
  "Major mode for image files.
You can use \\<image-mode-map>\\[image-toggle-display]
to toggle between display as an image and display as text."
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Image[text]")
  (setq major-mode 'image-mode)
  ;; Use our own bookmarking function for images.
  (set (make-local-variable 'bookmark-make-record-function)
       'image-bookmark-make-record)

  ;; Keep track of [vh]scroll when switching buffers
  (image-mode-setup-winprops)

  (add-hook 'change-major-mode-hook 'image-toggle-display-text nil t)
  (if (and (display-images-p)
	   (not (image-get-display-property)))
      (image-toggle-display)
    ;; Set next vars when image is already displayed but local
    ;; variables were cleared by kill-all-local-variables
    (use-local-map image-mode-map)
    (setq cursor-type nil truncate-lines t))
  (run-mode-hooks 'image-mode-hook)
  (if (display-images-p)
      (message "%s" (concat
		     (substitute-command-keys
		      "Type \\[image-toggle-display] to view as ")
		     (if (image-get-display-property)
			 "text" "an image") "."))))

;;;###autoload
(define-minor-mode image-minor-mode
  "Toggle Image minor mode.
With arg, turn Image minor mode on if arg is positive, off otherwise.
See the command `image-mode' for more information on this mode."
  nil (:eval (format " Image[%s]" image-type)) image-mode-text-map
  :group 'image
  :version "22.1"
  (if (not image-minor-mode)
      (image-toggle-display-text)
    (if (image-get-display-property)
	(setq cursor-type nil truncate-lines t)
      (setq image-type "text"))
    (add-hook 'change-major-mode-hook (lambda () (image-minor-mode -1)) nil t)
    (message "%s" (concat (substitute-command-keys
			   "Type \\[image-toggle-display] to view the image as ")
			  (if (image-get-display-property)
			      "text" "an image") "."))))

;;;###autoload
(defun image-mode-maybe ()
  "Set major or minor mode for image files.
Set Image major mode only when there are no other major modes
associated with a filename in `auto-mode-alist'.  When an image
filename matches another major mode in `auto-mode-alist' then
set that major mode and Image minor mode.

See commands `image-mode' and `image-minor-mode' for more
information on these modes."
  (interactive)
  (let* ((mode-alist
	  (delq nil (mapcar
		     (lambda (elt)
		       (unless (memq (or (car-safe (cdr elt)) (cdr elt))
				     '(image-mode image-mode-maybe))
			 elt))
		     auto-mode-alist))))
    (if (assoc-default buffer-file-name mode-alist 'string-match)
	(let ((auto-mode-alist mode-alist)
	      (magic-mode-alist nil))
	  (set-auto-mode)
	  (image-minor-mode t))
      (image-mode))))

(defun image-toggle-display-text ()
  "Showing the text of the image file."
  (if (image-get-display-property)
      (image-toggle-display)))

(defvar archive-superior-buffer)
(defvar tar-superior-buffer)

(defun image-toggle-display ()
  "Start or stop displaying an image file as the actual image.
This command toggles between showing the text of the image file
and showing the image as an image."
  (interactive)
  (if (image-get-display-property)
      (let ((inhibit-read-only t)
	    (buffer-undo-list t)
	    (modified (buffer-modified-p)))
	(remove-list-of-text-properties (point-min) (point-max)
					'(display intangible read-nonsticky
						  read-only front-sticky))
	(set-buffer-modified-p modified)
	(kill-local-variable 'cursor-type)
	(kill-local-variable 'truncate-lines)
	(kill-local-variable 'auto-hscroll-mode)
	(use-local-map image-mode-text-map)
	(setq image-type "text")
	(if (eq major-mode 'image-mode)
	    (setq mode-name "Image[text]"))
	(if (called-interactively-p)
	    (message "Repeat this command to go back to displaying the image")))
    ;; Turn the image data into a real image, but only if the whole file
    ;; was inserted
    (let* ((filename (buffer-file-name))
	   (data-p (not (and filename
			     (file-readable-p filename)
			     (not (file-remote-p filename))
			     (not (buffer-modified-p))
			     (not (and (boundp 'archive-superior-buffer)
				       archive-superior-buffer))
			     (not (and (boundp 'tar-superior-buffer)
				       tar-superior-buffer)))))
	   (file-or-data (if data-p
			     (string-make-unibyte
			      (buffer-substring-no-properties (point-min) (point-max)))
			   filename))
	   (type (image-type file-or-data nil data-p))
	   (image (create-image file-or-data type data-p))
	   (props
	    `(display ,image
		      intangible ,image
		      rear-nonsticky (display intangible)
		      read-only t front-sticky (read-only)))
	   (inhibit-read-only t)
	   (buffer-undo-list t)
	   (modified (buffer-modified-p)))
      (image-refresh image)
      (add-text-properties (point-min) (point-max) props)
      (set-buffer-modified-p modified)
      ;; Inhibit the cursor when the buffer contains only an image,
      ;; because cursors look very strange on top of images.
      (setq cursor-type nil)
      ;; This just makes the arrow displayed in the right fringe
      ;; area look correct when the image is wider than the window.
      (setq truncate-lines t)
      ;; Allow navigation of large images
      (set (make-local-variable 'auto-hscroll-mode) nil)
      (use-local-map image-mode-map)
      (setq image-type type)
      (if (eq major-mode 'image-mode)
	  (setq mode-name (format "Image[%s]" type)))
      (if (called-interactively-p)
	  (message "Repeat this command to go back to displaying the file as text")))))

;;; Support for bookmark.el

(defun image-bookmark-make-record (annotation)
  (let ((the-record
         `((filename   . ,(buffer-file-name))
	   (image-type . ,image-type)
	   (position   . ,(point))
	   (handler    . image-bookmark-jump))))

    ;; Take no chances with text properties
    (set-text-properties 0 (length annotation) nil annotation)

    (when annotation
      (nconc the-record (list (cons 'annotation annotation))))

    ;; Finally, return the completed record.
    the-record))

(declare-function bookmark-get-filename        "bookmark" (bookmark))
(declare-function bookmark-get-bookmark-record "bookmark" (bookmark))
(declare-function bookmark-get-position        "bookmark" (bookmark))

;;;###autoload
(defun image-bookmark-jump (bmk)
  ;; This implements the `handler' function interface for record type
  ;; returned by `bookmark-make-record-function', which see.
  (save-window-excursion
    (let ((filename (bookmark-get-filename bmk))
	  (type (cdr (assq 'image-type (bookmark-get-bookmark-record bmk))))
	  (pos  (bookmark-get-position bmk)))
      (find-file filename)
      (when (not (string= image-type type))
	(image-toggle-display))
      (when (string= image-type "text")
	(goto-char pos))
      `((buffer ,(current-buffer)) (position ,(point))))))

(provide 'image-mode)

;; arch-tag: b5b2b7e6-26a7-4b79-96e3-1546b5c4c6cb
;;; image-mode.el ends here
