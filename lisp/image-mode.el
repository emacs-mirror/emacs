;;; image-mode.el --- support for visiting image files  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2005-2018 Free Software Foundation, Inc.
;;
;; Author: Richard Stallman <rms@gnu.org>
;; Keywords: multimedia
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Defines a major mode for visiting image files
;; that allows conversion between viewing the text of the file,
;; hex of the file and viewing the file as an image.  Viewing the image
;; works by putting a `display' text-property on the
;; image data, with the image-data still present underneath; if the
;; resulting buffer file is saved to another name it will correctly save
;; the image data to the new file.

;; Todo:

;; Consolidate with doc-view to make them work on directories of images or on
;; image files containing various "pages".

;;; Code:

(require 'image)
(eval-when-compile (require 'cl-lib))

;;; Image mode window-info management.

(defvar-local image-mode-winprops-alist t
  "Alist of windows to window properties.
Each element has the form (WINDOW . ALIST).
See `image-mode-winprops'.")

(defvar image-mode-new-window-functions nil
  "Special hook run when image data is requested in a new window.
It is called with one argument, the initial WINPROPS.")

;; FIXME this doesn't seem mature yet. Document in manual when it is.
(defvar image-transform-resize nil
  "The image resize operation.
Its value should be one of the following:
 - nil, meaning no resizing.
 - `fit-height', meaning to fit the image to the window height.
 - `fit-width', meaning to fit the image to the window width.
 - A number, which is a scale factor (the default size is 1).")

(defvar image-transform-scale 1.0
  "The scale factor of the image being displayed.")

(defvar image-transform-rotation 0.0
  "Rotation angle for the image in the current Image mode buffer.")

(defvar image-transform-right-angle-fudge 0.0001
  "Snap distance to a multiple of a right angle.
There's no deep theory behind the default value, it should just
be somewhat larger than ImageMagick's MagickEpsilon.")

(defun image-mode-winprops (&optional window cleanup)
  "Return winprops of WINDOW.
A winprops object has the shape (WINDOW . ALIST).
WINDOW defaults to `selected-window' if it displays the current buffer, and
otherwise it defaults to t, used for times when the buffer is not displayed."
  (cond ((null window)
         (setq window
               (if (eq (current-buffer) (window-buffer)) (selected-window) t)))
        ((eq window t))
	((not (windowp window))
	 (error "Not a window: %s" window)))
  (when cleanup
    (setq image-mode-winprops-alist
  	  (delq nil (mapcar (lambda (winprop)
			      (let ((w (car-safe winprop)))
				(if (or (not (windowp w)) (window-live-p w))
				    winprop)))
  			    image-mode-winprops-alist))))
  (let ((winprops (assq window image-mode-winprops-alist)))
    ;; For new windows, set defaults from the latest.
    (if winprops
        ;; Move window to front.
        (setq image-mode-winprops-alist
              (cons winprops (delq winprops image-mode-winprops-alist)))
      (setq winprops (cons window
                           (copy-alist (cdar image-mode-winprops-alist))))
      ;; Add winprops before running the hook, to avoid inf-loops if the hook
      ;; triggers window-configuration-change-hook.
      (setq image-mode-winprops-alist
            (cons winprops image-mode-winprops-alist))
      (run-hook-with-args 'image-mode-new-window-functions winprops))
    winprops))

(defun image-mode-window-get (prop &optional winprops)
  (declare (gv-setter (lambda (val)
                        `(image-mode-window-put ,prop ,val ,winprops))))
  (unless (consp winprops) (setq winprops (image-mode-winprops winprops)))
  (cdr (assq prop (cdr winprops))))

(defun image-mode-window-put (prop val &optional winprops)
  (unless (consp winprops) (setq winprops (image-mode-winprops winprops)))
  (unless (eq t (car winprops))
    (image-mode-window-put prop val t))
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
    ;; Beware: this call to image-mode-winprops can't be optimized away,
    ;; because it not only gets the winprops data but sets it up if needed
    ;; (e.g. it's used by doc-view to display the image in a new window).
    (let* ((winprops (image-mode-winprops nil t))
           (hscroll (image-mode-window-get 'hscroll winprops))
           (vscroll (image-mode-window-get 'vscroll winprops)))
      (when (image-get-display-property) ;Only do it if we display an image!
	(if hscroll (set-window-hscroll (selected-window) hscroll))
	(if vscroll (set-window-vscroll (selected-window) vscroll))))))

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

(declare-function image-size "image.c" (spec &optional pixels frame))
(declare-function xwidget-info "xwidget.c" (xwidget))
(declare-function xwidget-at "xwidget.el" (pos))

(defun image-display-size (spec &optional pixels frame)
  "Wrapper around `image-size', handling slice display properties.
Like `image-size', the return value is (WIDTH . HEIGHT).
WIDTH and HEIGHT are in canonical character units if PIXELS is
nil, and in pixel units if PIXELS is non-nil.

If SPEC is an image display property, this function is equivalent to
`image-size'.  If SPEC represents an xwidget object, defer to `xwidget-info'.
If SPEC is a list of properties containing `image' and `slice' properties,
return the display size taking the slice property into account.  If the list
contains `image' but not `slice', return the `image-size' of the specified
image."
  (cond ((eq (car spec) 'xwidget)
         (let ((xwi (xwidget-info (xwidget-at (point-min)))))
           (cons (aref xwi 2) (aref xwi 3))))
        ((eq (car spec) 'image)
         (image-size spec pixels frame))
        (t (let ((image (assoc 'image spec))
                 (slice (assoc 'slice spec)))
             (cond ((and image slice)
                    (if pixels
                        (cons (nth 3 slice) (nth 4 slice))
                      (cons (/ (float (nth 3 slice)) (frame-char-width frame))
                            (/ (float (nth 4 slice))
                               (frame-char-height frame)))))
                   (image
                    (image-size image pixels frame))
                   (t
                    (error "Invalid image specification: %s" spec)))))))

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
		(img-width (ceiling (car (image-display-size image)))))
	   (image-set-window-hscroll (min (max 0 (- img-width win-width))
					  (+ n (window-hscroll))))))))

(defun image-backward-hscroll (&optional n)
  "Scroll image in current window to the right by N character widths.
Stop if the left edge of the image is reached."
  (interactive "p")
  (image-forward-hscroll (- n)))

(defun image-next-line (n)
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
		(img-height (ceiling (cdr (image-display-size image)))))
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

(defun image-scroll-left (&optional n)
  "Scroll image in current window leftward by N character widths.
Stop if the right edge of the image is reached.
If ARG is omitted or nil, scroll leftward by a near full screen.
A near full screen is 2 columns less than a full screen.
Negative ARG means scroll rightward.
If ARG is the atom `-', scroll rightward by nearly full screen.
When calling from a program, supply as argument a number, nil, or `-'."
  (interactive "P")
  (cond ((null n)
	 (let* ((edges (window-inside-edges))
		(win-width (- (nth 2 edges) (nth 0 edges))))
	   (image-forward-hscroll
	    (max 0 (- win-width 2)))))
	((eq n '-)
	 (let* ((edges (window-inside-edges))
		(win-width (- (nth 2 edges) (nth 0 edges))))
	   (image-forward-hscroll
	    (min 0 (- 2 win-width)))))
	(t (image-forward-hscroll (prefix-numeric-value n)))))

(defun image-scroll-right (&optional n)
  "Scroll image in current window rightward by N character widths.
Stop if the left edge of the image is reached.
If ARG is omitted or nil, scroll downward by a near full screen.
A near full screen is 2 less than a full screen.
Negative ARG means scroll leftward.
If ARG is the atom `-', scroll leftward by nearly full screen.
When calling from a program, supply as argument a number, nil, or `-'."
  (interactive "P")
  (cond ((null n)
	 (let* ((edges (window-inside-edges))
		(win-width (- (nth 2 edges) (nth 0 edges))))
	   (image-forward-hscroll
	    (min 0 (- 2 win-width)))))
	((eq n '-)
	 (let* ((edges (window-inside-edges))
		(win-width (- (nth 2 edges) (nth 0 edges))))
	   (image-forward-hscroll
	    (max 0 (- win-width 2)))))
	(t (image-forward-hscroll (- (prefix-numeric-value n))))))

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
	 (img-width (ceiling (car (image-display-size image)))))
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
	 (img-width (ceiling (car (image-display-size image))))
	 (win-height (- (nth 3 edges) (nth 1 edges)))
	 (img-height (ceiling (cdr (image-display-size image)))))
    (image-set-window-hscroll (max 0 (- img-width win-width)))
    (image-set-window-vscroll (max 0 (- img-height win-height)))))

;; Adjust frame and image size.

(defun image-mode-fit-frame (&optional frame toggle)
  "Fit FRAME to the current image.
If FRAME is omitted or nil, it defaults to the selected frame.
All other windows on the frame are deleted.

If called interactively, or if TOGGLE is non-nil, toggle between
fitting FRAME to the current image and restoring the size and
window configuration prior to the last `image-mode-fit-frame'
call."
  (interactive (list nil t))
  (let* ((buffer (current-buffer))
         (display (image-get-display-property))
         (size (image-display-size display))
	 (saved (frame-parameter frame 'image-mode-saved-params))
	 (window-configuration (current-window-configuration frame))
	 (width  (frame-width  frame))
	 (height (frame-height frame)))
    (with-selected-frame (or frame (selected-frame))
      (if (and toggle saved
	       (= (caar saved) width)
	       (= (cdar saved) height))
	  (progn
	    (set-frame-width  frame (car (nth 1 saved)))
	    (set-frame-height frame (cdr (nth 1 saved)))
	    (set-window-configuration (nth 2 saved))
	    (set-frame-parameter frame 'image-mode-saved-params nil))
	(delete-other-windows)
	(switch-to-buffer buffer t t)
	(let* ((edges (window-inside-edges))
	       (inner-width  (- (nth 2 edges) (nth 0 edges)))
	       (inner-height (- (nth 3 edges) (nth 1 edges))))
	  (set-frame-width  frame (+ (ceiling (car size))
				     width (- inner-width)))
	  (set-frame-height frame (+ (ceiling (cdr size))
				     height (- inner-height)))
	  ;; The frame size after the above `set-frame-*' calls may
	  ;; differ from what we specified, due to window manager
	  ;; interference.  We have to call `frame-width' and
	  ;; `frame-height' to get the actual results.
	  (set-frame-parameter frame 'image-mode-saved-params
			       (list (cons (frame-width)
					   (frame-height))
				     (cons width height)
				     window-configuration)))))))

;;; Image Mode setup

(defvar-local image-type nil
  "The image type for the current Image mode buffer.")

(defvar-local image-multi-frame nil
  "Non-nil if image for the current Image mode buffer has multiple frames.")

(defvar image-mode-previous-major-mode nil
  "Internal variable to keep the previous non-image major mode.")

(defvar image-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'image-toggle-display)
    (define-key map "\C-c\C-x" 'image-toggle-hex-display)
    (define-key map (kbd "SPC")       'image-scroll-up)
    (define-key map (kbd "S-SPC")     'image-scroll-down)
    (define-key map (kbd "DEL")       'image-scroll-down)
    (define-key map (kbd "RET")       'image-toggle-animation)
    (define-key map "F" 'image-goto-frame)
    (define-key map "f" 'image-next-frame)
    (define-key map "b" 'image-previous-frame)
    (define-key map "n" 'image-next-file)
    (define-key map "p" 'image-previous-file)
    (define-key map "a+" 'image-increase-speed)
    (define-key map "a-" 'image-decrease-speed)
    (define-key map "a0" 'image-reset-speed)
    (define-key map "ar" 'image-reverse-speed)
    (define-key map "k" 'image-kill-buffer)
    (define-key map [remap forward-char] 'image-forward-hscroll)
    (define-key map [remap backward-char] 'image-backward-hscroll)
    (define-key map [remap right-char] 'image-forward-hscroll)
    (define-key map [remap left-char] 'image-backward-hscroll)
    (define-key map [remap previous-line] 'image-previous-line)
    (define-key map [remap next-line] 'image-next-line)
    (define-key map [remap scroll-up] 'image-scroll-up)
    (define-key map [remap scroll-down] 'image-scroll-down)
    (define-key map [remap scroll-up-command] 'image-scroll-up)
    (define-key map [remap scroll-down-command] 'image-scroll-down)
    (define-key map [remap scroll-left] 'image-scroll-left)
    (define-key map [remap scroll-right] 'image-scroll-right)
    (define-key map [remap move-beginning-of-line] 'image-bol)
    (define-key map [remap move-end-of-line] 'image-eol)
    (define-key map [remap beginning-of-buffer] 'image-bob)
    (define-key map [remap end-of-buffer] 'image-eob)
    (easy-menu-define image-mode-menu map "Menu for Image mode."
      '("Image"
	["Show as Text" image-toggle-display :active t
	 :help "Show image as text"]
    ["Show as Hex" image-toggle-hex-display :active t
     :help "Show image as hex"]
	"--"
	["Fit to Window Height" image-transform-fit-to-height
	 :visible (eq image-type 'imagemagick)
	 :help "Resize image to match the window height"]
	["Fit to Window Width" image-transform-fit-to-width
	 :visible (eq image-type 'imagemagick)
	 :help "Resize image to match the window width"]
	["Rotate Image..." image-transform-set-rotation
	 :visible (eq image-type 'imagemagick)
	 :help "Rotate the image"]
	["Reset Transformations" image-transform-reset
	 :visible (eq image-type 'imagemagick)
	 :help "Reset all image transformations"]
	"--"
	["Show Thumbnails"
	 (lambda ()
	   (interactive)
	   (image-dired default-directory))
	 :active default-directory
	 :help "Show thumbnails for all images in this directory"]
	["Next Image" image-next-file :active buffer-file-name
         :help "Move to next image in this directory"]
	["Previous Image" image-previous-file :active buffer-file-name
         :help "Move to previous image in this directory"]
	"--"
	["Fit Frame to Image" image-mode-fit-frame :active t
	 :help "Resize frame to match image"]
	"--"
	["Animate Image" image-toggle-animation :style toggle
	 :selected (let ((image (image-get-display-property)))
		     (and image (image-animate-timer image)))
	 :active image-multi-frame
         :help "Toggle image animation"]
	["Loop Animation"
	 (lambda () (interactive)
	   (setq image-animate-loop (not image-animate-loop))
	   ;; FIXME this is a hacky way to make it affect a currently
	   ;; animating image.
	   (when (let ((image (image-get-display-property)))
		   (and image (image-animate-timer image)))
	     (image-toggle-animation)
	     (image-toggle-animation)))
	 :style toggle :selected image-animate-loop
	 :active image-multi-frame
	 :help "Animate images once, or forever?"]
	["Reverse Animation" image-reverse-speed
	 :style toggle :selected (let ((image (image-get-display-property)))
				   (and image (<
					       (image-animate-get-speed image)
					       0)))
	 :active image-multi-frame
	 :help "Reverse direction of this image's animation?"]
	["Speed Up Animation" image-increase-speed
	 :active image-multi-frame
	 :help "Speed up this image's animation"]
	["Slow Down Animation" image-decrease-speed
	 :active image-multi-frame
	 :help "Slow down this image's animation"]
	["Reset Animation Speed" image-reset-speed
	 :active image-multi-frame
	 :help "Reset the speed of this image's animation"]
	["Next Frame" image-next-frame :active image-multi-frame
	 :help "Show the next frame of this image"]
	["Previous Frame" image-previous-frame :active image-multi-frame
	 :help "Show the previous frame of this image"]
	["Goto Frame..." image-goto-frame :active image-multi-frame
	 :help "Show a specific frame of this image"]
	))
    (make-composed-keymap (list map image-map) special-mode-map))
  "Mode keymap for `image-mode'.")

(defvar image-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'image-toggle-display)
    (define-key map "\C-c\C-x" 'image-toggle-hex-display)
    map)
  "Mode keymap for `image-minor-mode'.")

(defvar bookmark-make-record-function)

(put 'image-mode 'mode-class 'special)

;;;###autoload
(defun image-mode ()
  "Major mode for image files.
You can use \\<image-mode-map>\\[image-toggle-display] or \\<image-mode-map>\\[image-toggle-hex-display]
to toggle between display as an image and display as text or hex.

Key bindings:
\\{image-mode-map}"
  (interactive)
  (condition-case err
      (progn
	(unless (display-images-p)
	  (error "Display does not support images"))

	(kill-all-local-variables)
	(setq major-mode 'image-mode)

	(if (not (image-get-display-property))
	    (progn
	      (image-toggle-display-image)
	      ;; If attempt to display the image fails.
	      (if (not (image-get-display-property))
		  (error "Invalid image")))
	  ;; Set next vars when image is already displayed but local
	  ;; variables were cleared by kill-all-local-variables
	  (setq cursor-type nil truncate-lines t
		image-type (plist-get (cdr (image-get-display-property)) :type)))

	(setq mode-name (if image-type (format "Image[%s]" image-type) "Image"))
	(use-local-map image-mode-map)

	;; Use our own bookmarking function for images.
	(setq-local bookmark-make-record-function
                    #'image-bookmark-make-record)

	;; Keep track of [vh]scroll when switching buffers
	(image-mode-setup-winprops)

	(add-hook 'change-major-mode-hook 'image-toggle-display-text nil t)
	(add-hook 'after-revert-hook 'image-after-revert-hook nil t)
	(run-mode-hooks 'image-mode-hook)
	(let ((image (image-get-display-property))
	      (msg1 (substitute-command-keys
             "Type \\[image-toggle-display] or \\[image-toggle-hex-display] to view the image as "))
	      animated)
	  (cond
	   ((null image)
	    (message "%s" (concat msg1 "an image.")))
	   ((setq animated (image-multi-frame-p image))
	    (setq image-multi-frame t
		  mode-line-process
		  `(:eval
		    (concat " "
			    (propertize
			     (format "[%s/%s]"
				     (1+ (image-current-frame ',image))
				     ,(car animated))
			     'help-echo "Frames
mouse-1: Next frame
mouse-3: Previous frame"
			     'mouse-face 'mode-line-highlight
			     'local-map
			     '(keymap
			       (mode-line
				keymap
				(down-mouse-1 . image-next-frame)
				(down-mouse-3 . image-previous-frame)))))))
	    (message "%s"
		     (concat msg1 "text.  This image has multiple frames.")))
;;;			     (substitute-command-keys
;;;			      "\\[image-toggle-animation] to animate."))))
	   (t
        (message "%s" (concat msg1 "text or hex."))))))

    (error
     (image-mode-as-text)
     (funcall
      (if (called-interactively-p 'any) 'error 'message)
      "Cannot display image: %s" (cdr err)))))

;;;###autoload
(define-minor-mode image-minor-mode
  "Toggle Image minor mode in this buffer.
With a prefix argument ARG, enable Image minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Image minor mode provides the key \\<image-mode-map>\\[image-toggle-display],
to switch back to `image-mode' and display an image file as the
actual image."
  nil (:eval (if image-type (format " Image[%s]" image-type) " Image"))
  image-minor-mode-map
  :group 'image
  :version "22.1"
  (if image-minor-mode
      (add-hook 'change-major-mode-hook (lambda () (image-minor-mode -1)) nil t)))

;;;###autoload
(defun image-mode-to-text ()
  "Set a non-image mode as major mode in combination with image minor mode.
A non-mage major mode found from `auto-mode-alist' or fundamental mode
displays an image file as text."
  ;; image-mode-as-text = normal-mode + image-minor-mode
  (let ((previous-image-type image-type)) ; preserve `image-type'
    (if image-mode-previous-major-mode
	;; Restore previous major mode that was already found by this
	;; function and cached in `image-mode-previous-major-mode'
	(funcall image-mode-previous-major-mode)
      (let ((auto-mode-alist
	     (delq nil (mapcar
			(lambda (elt)
			  (unless (memq (or (car-safe (cdr elt)) (cdr elt))
					'(image-mode image-mode-maybe image-mode-as-text))
			    elt))
			auto-mode-alist)))
	    (magic-fallback-mode-alist
	     (delq nil (mapcar
			(lambda (elt)
			  (unless (memq (or (car-safe (cdr elt)) (cdr elt))
					'(image-mode image-mode-maybe image-mode-as-text))
			    elt))
			magic-fallback-mode-alist))))
	(normal-mode)
	(setq-local image-mode-previous-major-mode major-mode)))
    ;; Restore `image-type' after `kill-all-local-variables' in `normal-mode'.
    (setq image-type previous-image-type)
    ;; Enable image minor mode with `C-c C-c'.
    (image-minor-mode 1)
    ;; Show the image file as text.
    (image-toggle-display-text)))

(defun image-mode-as-hex ()
  "Set a non-image mode as major mode in combination with image minor mode.
A non-mage major mode found from `auto-mode-alist' or fundamental mode
displays an image file as hex.  `image-minor-mode' provides the key
\\<image-mode-map>\\[image-toggle-hex-display] to switch back to `image-mode'
to display an image file as the actual image.

You can use `image-mode-as-hex' in `auto-mode-alist' when you want to
display an image file as hex initially.

See commands `image-mode' and `image-minor-mode' for more information
on these modes."
  (interactive)
  (image-mode-to-text)
  ;; Turn on hexl-mode
  (hexl-mode)
  (message "%s" (concat
                 (substitute-command-keys
                  "Type \\[image-toggle-hex-display] or \\[image-toggle-display] to view the image as ")
                 (if (image-get-display-property)
                     "hex" "an image or text") ".")))

(defun image-mode-as-text ()
  "Set a non-image mode as major mode in combination with image minor mode.
A non-image major mode found from `auto-mode-alist' or Fundamental mode
displays an image file as text.  `image-minor-mode' provides the key
\\<image-mode-map>\\[image-toggle-display] to switch back to `image-mode'
to display an image file as the actual image.

You can use `image-mode-as-text' in `auto-mode-alist' when you want
to display an image file as text initially.

See commands `image-mode' and `image-minor-mode' for more information
on these modes."
  (interactive)
  (image-mode-to-text)
  (message "%s" (concat
                 (substitute-command-keys
                  "Type \\[image-toggle-display] or \\[image-toggle-hex-display] to view the image as ")
                 (if (image-get-display-property)
                     "text" "an image or hex") ".")))

(define-obsolete-function-alias 'image-mode-maybe 'image-mode "23.2")

(defun image-toggle-display-text ()
  "Show the image file as text.
Remove text properties that display the image."
  (let ((inhibit-read-only t)
	(buffer-undo-list t)
	(modified (buffer-modified-p)))
    (remove-list-of-text-properties (point-min) (point-max)
				    '(display read-nonsticky ;; intangible
					      read-only front-sticky))
    (set-buffer-modified-p modified)
    (if (called-interactively-p 'any)
	(message "Repeat this command to go back to displaying the image"))))

(defvar archive-superior-buffer)
(defvar tar-superior-buffer)
(declare-function image-flush "image.c" (spec &optional frame))

(defun image-toggle-display-image ()
  "Show the image of the image file.
Turn the image data into a real image, but only if the whole file
was inserted."
  (unless (derived-mode-p 'image-mode)
    (error "The buffer is not in Image mode"))
  (let* ((filename (buffer-file-name))
	 (data-p (not (and filename
			   (file-readable-p filename)
			   (not (file-remote-p filename))
			   (not (buffer-modified-p))
			   (not (and (boundp 'archive-superior-buffer)
				     archive-superior-buffer))
			   (not (and (boundp 'tar-superior-buffer)
				     tar-superior-buffer))
                           ;; This means the buffer holds the
                           ;; decrypted content (bug#21870).
                           (not (and (boundp 'epa-file-encrypt-to)
                                     (local-variable-p
                                      'epa-file-encrypt-to))))))
	 (file-or-data (if data-p
			   (string-make-unibyte
			    (buffer-substring-no-properties (point-min) (point-max)))
			 filename))
	 ;; If we have a `fit-width' or a `fit-height', don't limit
	 ;; the size of the image to the window size.
	 (edges (and (null image-transform-resize)
		     (window-inside-pixel-edges
		      (get-buffer-window (current-buffer)))))
	 (type (if (image--imagemagick-wanted-p filename)
		   'imagemagick
		 (image-type file-or-data nil data-p)))
	 (image (if (not edges)
		    (create-image file-or-data type data-p)
		  (create-image file-or-data type data-p
				:max-width (- (nth 2 edges) (nth 0 edges))
				:max-height (- (nth 3 edges) (nth 1 edges)))))
	 (inhibit-read-only t)
	 (buffer-undo-list t)
	 (modified (buffer-modified-p))
	 props)

    ;; Discard any stale image data before looking it up again.
    (image-flush image)
    (setq image (append image (image-transform-properties image)))
    (setq props
	  `(display ,image
		    ;; intangible ,image
		    rear-nonsticky (display) ;; intangible
		    read-only t front-sticky (read-only)))

    (let ((create-lockfiles nil)) ; avoid changing dir mtime by lock_file
      (add-text-properties (point-min) (point-max) props)
      (restore-buffer-modified-p modified))
    ;; Inhibit the cursor when the buffer contains only an image,
    ;; because cursors look very strange on top of images.
    (setq cursor-type nil)
    ;; This just makes the arrow displayed in the right fringe
    ;; area look correct when the image is wider than the window.
    (setq truncate-lines t)
    ;; Disable adding a newline at the end of the image file when it
    ;; is written with, e.g., C-x C-w.
    (if (coding-system-equal (coding-system-base buffer-file-coding-system)
			     'no-conversion)
	(setq-local find-file-literally t))
    ;; Allow navigation of large images.
    (setq-local auto-hscroll-mode nil)
    (setq image-type type)
    (if (eq major-mode 'image-mode)
	(setq mode-name (format "Image[%s]" type)))
    (image-transform-check-size)
    (if (called-interactively-p 'any)
	(message "Repeat this command to go back to displaying the file as text"))))

(defun image--imagemagick-wanted-p (filename)
  (and (fboundp 'imagemagick-types)
       (not (eq imagemagick-types-inhibit t))
       (not (memq (intern (upcase (file-name-extension filename)) obarray)
                  imagemagick-types-inhibit))))

(defun image-toggle-hex-display ()
  "Toggle between image and hex display."
  (interactive)
  (if (image-get-display-property)
      (image-mode-as-hex)
    (if (eq major-mode 'fundamental-mode)
        (image-mode-as-hex)
      (image-mode))))

(defun image-toggle-display ()
  "Toggle between image and text display.

If the current buffer is displaying an image file as an image,
call `image-mode-as-text' to switch to text or hex display.
Otherwise, display the image by calling `image-mode'"
  (interactive)
  (if (image-get-display-property)
      (image-mode-as-text)
    (if (eq major-mode 'hexl-mode)
        (image-mode-as-text)
      (image-mode))))

(defun image-kill-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun image-after-revert-hook ()
  ;; Fixes bug#21598
  (when (not (image-get-display-property))
    (image-toggle-display-image))
  (when (image-get-display-property)
    (image-toggle-display-text)
    ;; Update image display.
    (mapc (lambda (window) (redraw-frame (window-frame window)))
          (get-buffer-window-list (current-buffer) 'nomini 'visible))
    (image-toggle-display-image)))


;;; Animated images

(defcustom image-animate-loop nil
  "Non-nil means animated images loop forever, rather than playing once."
  :type 'boolean
  :version "24.1"
  :group 'image)

(defun image-toggle-animation ()
  "Start or stop animating the current image.
If `image-animate-loop' is non-nil, animation loops forever.
Otherwise it plays once, then stops."
  (interactive)
  (let ((image (image-get-display-property))
	animation)
    (cond
     ((null image)
      (error "No image is present"))
     ((null (setq animation (image-multi-frame-p image)))
      (message "No image animation."))
     (t
      (let ((timer (image-animate-timer image)))
	(if timer
	    (cancel-timer timer)
	  (let ((index (plist-get (cdr image) :index)))
	    ;; If we're at the end, restart.
	    (and index
		 (>= index (1- (car animation)))
		 (setq index nil))
	    (image-animate image index
			   (if image-animate-loop t)))))))))

(defun image--set-speed (speed &optional multiply)
  "Set speed of an animated image to SPEED.
If MULTIPLY is non-nil, treat SPEED as a multiplication factor.
If SPEED is `reset', reset the magnitude of the speed to 1."
  (let ((image (image-get-display-property)))
    (cond
     ((null image)
      (error "No image is present"))
     ((null image-multi-frame)
      (message "No image animation."))
     (t
      (if (eq speed 'reset)
	  (setq speed (if (< (image-animate-get-speed image) 0)
			  -1 1)
		multiply nil))
      (image-animate-set-speed image speed multiply)
      ;; FIXME Hack to refresh an active image.
      (when (image-animate-timer image)
	(image-toggle-animation)
	(image-toggle-animation))
      (message "Image speed is now %s" (image-animate-get-speed image))))))

(defun image-increase-speed ()
  "Increase the speed of current animated image by a factor of 2."
  (interactive)
  (image--set-speed 2 t))

(defun image-decrease-speed ()
  "Decrease the speed of current animated image by a factor of 2."
  (interactive)
  (image--set-speed 0.5 t))

(defun image-reverse-speed ()
  "Reverse the animation of the current image."
  (interactive)
  (image--set-speed -1 t))

(defun image-reset-speed ()
  "Reset the animation speed of the current image."
  (interactive)
  (image--set-speed 'reset))

(defun image-goto-frame (n &optional relative)
  "Show frame N of a multi-frame image.
Optional argument OFFSET non-nil means interpret N as relative to the
current frame.  Frames are indexed from 1."
  (interactive
   (list (or current-prefix-arg
	     (read-number "Show frame number: "))))
  (let ((image (image-get-display-property)))
    (cond
     ((null image)
      (error "No image is present"))
     ((null image-multi-frame)
      (message "No image animation."))
     (t
      (image-show-frame image
			(if relative
			    (+ n (image-current-frame image))
			  (1- n)))))))

(defun image-next-frame (&optional n)
  "Switch to the next frame of a multi-frame image.
With optional argument N, switch to the Nth frame after the current one.
If N is negative, switch to the Nth frame before the current one."
  (interactive "p")
  (image-goto-frame n t))

(defun image-previous-frame (&optional n)
  "Switch to the previous frame of a multi-frame image.
With optional argument N, switch to the Nth frame before the current one.
If N is negative, switch to the Nth frame after the current one."
  (interactive "p")
  (image-next-frame (- n)))


;;; Switching to the next/previous image

(defun image-next-file (&optional n)
  "Visit the next image in the same directory as the current image file.
With optional argument N, visit the Nth image file after the
current one, in cyclic alphabetical order.

This command visits the specified file via `find-alternate-file',
replacing the current Image mode buffer."
  (interactive "p")
  (unless (derived-mode-p 'image-mode)
    (error "The buffer is not in Image mode"))
  (unless buffer-file-name
    (error "The current image is not associated with a file"))
  (let* ((file (file-name-nondirectory buffer-file-name))
	 (images (image-mode--images-in-directory file))
	 (idx 0))
    (catch 'image-visit-next-file
      (dolist (f images)
	(if (string= f file)
	    (throw 'image-visit-next-file (1+ idx)))
	(setq idx (1+ idx))))
    (setq idx (mod (+ idx (or n 1)) (length images)))
    (find-alternate-file (nth idx images))))

(defun image-previous-file (&optional n)
  "Visit the preceding image in the same directory as the current file.
With optional argument N, visit the Nth image file preceding the
current one, in cyclic alphabetical order.

This command visits the specified file via `find-alternate-file',
replacing the current Image mode buffer."
  (interactive "p")
  (image-next-file (- n)))

(defun image-mode--images-in-directory (file)
  (let* ((dir (file-name-directory buffer-file-name))
	 (files (directory-files dir nil
				 (image-file-name-regexp) t)))
    ;; Add the current file to the list of images if necessary, in
    ;; case it does not match `image-file-name-regexp'.
    (unless (member file files)
      (push file files))
    (sort files 'string-lessp)))


;;; Support for bookmark.el
(declare-function bookmark-make-record-default
                  "bookmark" (&optional no-file no-context posn))
(declare-function bookmark-prop-get "bookmark" (bookmark prop))
(declare-function bookmark-default-handler "bookmark" (bmk))

(defun image-bookmark-make-record ()
  `(,@(bookmark-make-record-default nil 'no-context 0)
      (image-type . ,image-type)
      (handler    . image-bookmark-jump)))

;;;###autoload
(defun image-bookmark-jump (bmk)
  ;; This implements the `handler' function interface for record type
  ;; returned by `bookmark-make-record-function', which see.
  (prog1 (bookmark-default-handler bmk)
    (when (not (string= image-type (bookmark-prop-get bmk 'image-type)))
      (image-toggle-display))))


;; Not yet implemented.
;; (defvar image-transform-minor-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     ;; (define-key map  [(control ?+)] 'image-scale-in)
;;     ;; (define-key map  [(control ?-)] 'image-scale-out)
;;     ;; (define-key map  [(control ?=)] 'image-scale-none)
;;     ;; (define-key map "c f h" 'image-scale-fit-height)
;;     ;; (define-key map "c ]" 'image-rotate-right)
;;     map)
;;   "Minor mode keymap `image-transform-mode'.")
;;
;; (define-minor-mode image-transform-mode
;;   "Minor mode for scaling and rotating images.
;; With a prefix argument ARG, enable the mode if ARG is positive,
;; and disable it otherwise.  If called from Lisp, enable the mode
;; if ARG is omitted or nil.  This minor mode requires Emacs to have
;; been compiled with ImageMagick support."
;;   nil "image-transform" image-transform-minor-mode-map)


(defsubst image-transform-width (width height)
  "Return the bounding box width of a rotated WIDTH x HEIGHT rectangle.
The rotation angle is the value of `image-transform-rotation' in degrees."
  (let ((angle (degrees-to-radians image-transform-rotation)))
    ;; Assume, w.l.o.g., that the vertices of the rectangle have the
    ;; coordinates (+-w/2, +-h/2) and that (0, 0) is the center of the
    ;; rotation by the angle A.  The projections onto the first axis
    ;; of the vertices of the rotated rectangle are +- (w/2) cos A +-
    ;; (h/2) sin A, and the difference between the largest and the
    ;; smallest of the four values is the expression below.
    (+ (* width (abs (cos angle))) (* height (abs (sin angle))))))

;; The following comment and code snippet are from
;; ImageMagick-6.7.4-4/magick/distort.c

;;    /* Set the output image geometry to calculated 'best fit'.
;;       Yes this tends to 'over do' the file image size, ON PURPOSE!
;;       Do not do this for DePolar which needs to be exact for virtual tiling.
;;    */
;;    if ( fix_bounds ) {
;;      geometry.x = (ssize_t) floor(min.x-0.5);
;;      geometry.y = (ssize_t) floor(min.y-0.5);
;;      geometry.width=(size_t) ceil(max.x-geometry.x+0.5);
;;      geometry.height=(size_t) ceil(max.y-geometry.y+0.5);
;;    }

;; Other parts of the same file show that here the origin is in the
;; left lower corner of the image rectangle, the center of the
;; rotation is the center of the rectangle and min.x and max.x
;; (resp. min.y and max.y) are the smallest and the largest of the
;; projections of the vertices onto the first (resp. second) axis.

(defun image-transform-fit-width (width height length)
  "Return (w . h) so that a rotated w x h image has exactly width LENGTH.
The rotation angle is the value of `image-transform-rotation'.
Write W for WIDTH and H for HEIGHT.  Then the w x h rectangle is
an \"approximately uniformly\" scaled W x H rectangle, which
currently means that w is one of floor(s W) + {0, 1, -1} and h is
floor(s H), where s can be recovered as the value of `image-transform-scale'.
The value of `image-transform-rotation' may be replaced by
a slightly different angle.  Currently this is done for values
close to a multiple of 90, see `image-transform-right-angle-fudge'."
  (cond ((< (abs (- (mod (+ image-transform-rotation 90) 180) 90))
	    image-transform-right-angle-fudge)
	 (cl-assert (not (zerop width)) t)
	 (setq image-transform-rotation
	       (float (round image-transform-rotation))
	       image-transform-scale (/ (float length) width))
	 (cons length nil))
	((< (abs (- (mod (+ image-transform-rotation 45) 90) 45))
	    image-transform-right-angle-fudge)
	 (cl-assert (not (zerop height)) t)
	 (setq image-transform-rotation
	       (float (round image-transform-rotation))
	       image-transform-scale (/ (float length) height))
	 (cons nil length))
	(t
	 (cl-assert (not (and (zerop width) (zerop height))) t)
	 (setq image-transform-scale
	       (/ (float (1- length)) (image-transform-width width height)))
	 ;; Assume we have a w x h image and an angle A, and let l =
	 ;; l(w, h) = w |cos A| + h |sin A|, which is the actual width
	 ;; of the bounding box of the rotated image, as calculated by
	 ;; `image-transform-width'.  The code snippet quoted above
	 ;; means that ImageMagick puts the rotated image in
	 ;; a bounding box of width L = 2 ceil((w+l+1)/2) - w.
	 ;; Elementary considerations show that this is equivalent to
	 ;; L - w being even and L-3 < l(w, h) <= L-1.  In our case, L is
	 ;; the given `length' parameter and our job is to determine
	 ;; reasonable values for w and h which satisfy these
	 ;; conditions.
	 (let ((w (floor (* image-transform-scale width)))
	       (h (floor (* image-transform-scale height))))
	   ;; Let w and h as bound above.  Then l(w, h) <= l(s W, s H)
	   ;; = L-1 < l(w+1, h+1) = l(w, h) + l(1, 1) <= l(w, h) + 2,
	   ;; hence l(w, h) > (L-1) - 2 = L-3.
	   (cons
	    (cond ((= (mod w 2) (mod length 2))
		   w)
		  ;; l(w+1, h) >= l(w, h) > L-3, but does l(w+1, h) <=
		  ;; L-1 hold?
		  ((<= (image-transform-width (1+ w) h) (1- length))
		   (1+ w))
		  ;; No, it doesn't, but this implies that l(w-1, h) =
		  ;; l(w+1, h) - l(2, 0) >= l(w+1, h) - 2 > (L-1) -
		  ;; 2 = L-3.  Clearly, l(w-1, h) <= l(w, h) <= L-1.
		  (t
		   (1- w)))
	    h)))))

(defun image-transform-check-size ()
  "Check that the image exactly fits the width/height of the window.

Do this for an image of type `imagemagick' to make sure that the
elisp code matches the way ImageMagick computes the bounding box
of a rotated image."
  (when (and (not (numberp image-transform-resize))
	     (boundp 'image-type)
	     (eq image-type 'imagemagick))
    (let ((size (image-display-size (image-get-display-property) t)))
      (cond ((eq image-transform-resize 'fit-width)
	     (cl-assert (= (car size)
			(- (nth 2 (window-inside-pixel-edges))
			   (nth 0 (window-inside-pixel-edges))))
		     t))
	    ((eq image-transform-resize 'fit-height)
	     (cl-assert (= (cdr size)
			(- (nth 3 (window-inside-pixel-edges))
			   (nth 1 (window-inside-pixel-edges))))
		     t))))))

(defun image-transform-properties (spec)
  "Return rescaling/rotation properties for image SPEC.
These properties are determined by the Image mode variables
`image-transform-resize' and `image-transform-rotation'.  The
return value is suitable for appending to an image spec.

Rescaling and rotation properties only take effect if Emacs is
compiled with ImageMagick support."
  (setq image-transform-scale 1.0)
  (when (or image-transform-resize
	    (/= image-transform-rotation 0.0))
    ;; Note: `image-size' looks up and thus caches the untransformed
    ;; image.  There's no easy way to prevent that.
    (let* ((size (image-size spec t))
	   (resized
	    (cond
	     ((numberp image-transform-resize)
	      (unless (= image-transform-resize 1)
		(setq image-transform-scale image-transform-resize)
		(cons nil (floor (* image-transform-resize (cdr size))))))
	     ((eq image-transform-resize 'fit-width)
	      (image-transform-fit-width
	       (car size) (cdr size)
	       (- (nth 2 (window-inside-pixel-edges))
		  (nth 0 (window-inside-pixel-edges)))))
	     ((eq image-transform-resize 'fit-height)
	      (let ((res (image-transform-fit-width
			  (cdr size) (car size)
			  (- (nth 3 (window-inside-pixel-edges))
			     (nth 1 (window-inside-pixel-edges))))))
		(cons (cdr res) (car res)))))))
      `(,@(when (car resized)
	    (list :width (car resized)))
	,@(when (cdr resized)
	    (list :height (cdr resized)))
	,@(unless (= 0.0 image-transform-rotation)
	    (list :rotation image-transform-rotation))))))

(defun image-transform-set-scale (scale)
  "Prompt for a number, and resize the current image by that amount.
This command has no effect unless Emacs is compiled with
ImageMagick support."
  (interactive "nScale: ")
  (setq image-transform-resize scale)
  (image-toggle-display-image))

(defun image-transform-fit-to-height ()
  "Fit the current image to the height of the current window.
This command has no effect unless Emacs is compiled with
ImageMagick support."
  (interactive)
  (setq image-transform-resize 'fit-height)
  (image-toggle-display-image))

(defun image-transform-fit-to-width ()
  "Fit the current image to the width of the current window.
This command has no effect unless Emacs is compiled with
ImageMagick support."
  (interactive)
  (setq image-transform-resize 'fit-width)
  (image-toggle-display-image))

(defun image-transform-set-rotation (rotation)
  "Prompt for an angle ROTATION, and rotate the image by that amount.
ROTATION should be in degrees.  This command has no effect unless
Emacs is compiled with ImageMagick support."
  (interactive "nRotation angle (in degrees): ")
  (setq image-transform-rotation (float (mod rotation 360)))
  (image-toggle-display-image))

(defun image-transform-reset ()
  "Display the current image with the default size and rotation.
This command has no effect unless Emacs is compiled with
ImageMagick support."
  (interactive)
  (setq image-transform-resize nil
	image-transform-rotation 0.0
	image-transform-scale 1)
  (image-toggle-display-image))

(provide 'image-mode)

;;; image-mode.el ends here
