;;; shr.el --- Simple HTML Renderer -*- lexical-binding: t -*-

;; Copyright (C) 2010-2026 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package takes a HTML parse tree (as provided by
;; libxml-parse-html-region) and renders it in the current buffer.  It
;; does not do CSS, JavaScript or anything advanced: It's geared
;; towards rendering typical short snippets of HTML, like what you'd
;; find in HTML email and the like.

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'url))      ;For url-filename's setf handler.
(require 'browse-url)
(eval-when-compile (require 'subr-x))
(require 'dom)
(require 'seq)
(require 'svg)
(require 'image)
(require 'puny)
(require 'url-cookie)
(require 'url-file)
(require 'pixel-fill)
(require 'text-property-search)

(defgroup shr nil
  "Simple HTML Renderer."
  :version "25.1"
  :group 'web)

(defcustom shr-max-image-proportion 0.9
  "How big pictures displayed are in relation to the window they're in.
A value of 0.7 means that they are allowed to take up 70% of the
width and height of the window.  If they are larger than this,
and Emacs supports it, then the images will be rescaled down to
fit these criteria."
  :version "24.1"
  :type 'float)

(defcustom shr-sliced-image-height 0.9
  "How tall images can be before slicing in relation to the window they're in.
A value of 0.7 means that images are allowed to take up 70% of the
height of the window before being sliced by `insert-sliced-image'.  If
nil, never slice images.

Sliced images allow for more intuitive scrolling up/down by letting you
scroll past each slice, instead of jumping past the entire image.
Alternately, you can use `pixel-scroll-precision-mode' to scroll
pixel-wise past images, in which case you can set this option to nil."
  :version "31.1"
  :type '(choice (const :tag "Never slice images")
                 float))

(defcustom shr-allowed-images nil
  "If non-nil, only images that match this regexp are displayed.
If nil, all URLs are allowed.  Also see `shr-blocked-images'."
  :version "29.1"
  :type '(choice (const nil) regexp))

(defcustom shr-blocked-images nil
  "Images that have URLs matching this regexp will be blocked.
If nil, no images are blocked.  Also see `shr-allowed-images'."
  :version "24.1"
  :type '(choice (const nil) regexp))

(defcustom shr-use-fonts t
  "If non-nil, use proportional fonts for text."
  :version "25.1"
  :type 'boolean)

(defcustom shr-discard-aria-hidden nil
  "If non-nil, don't render tags with `aria-hidden=\"true\"'.
This attribute is meant to tell screen readers to ignore a tag."
  :version "27.1"
  :type 'boolean)

(defcustom shr-use-colors t
  "If non-nil, respect color specifications in the HTML."
  :version "26.1"
  :type 'boolean)

(defcustom shr-table-horizontal-line nil
  "Character used to draw horizontal table lines.
If nil, don't draw horizontal table lines."
  :type '(choice (const nil) character))

(defcustom shr-table-vertical-line ?\s
  "Character used to draw vertical table lines."
  :type 'character)

(defcustom shr-table-corner ?\s
  "Character used to draw table corners."
  :type 'character)

(defcustom shr-hr-line ?-
  "Character used to draw hr lines."
  :type 'character)

(defcustom shr-width nil
  "Window width to use for HTML rendering.
May either be an integer specifying a fixed width in characters,
or nil, meaning use the full width of the window.
If `shr-use-fonts' is set, the value is interpreted as a multiple
of the mean character width of the default face's font.

Also see `shr-max-width'."
  :version "25.1"
  :type '(choice (integer :tag "Fixed width in characters")
		 (const   :tag "Use the width of the window" nil)))

(defcustom shr-max-width 120
  "Maximum text width to use for HTML rendering.
May either be an integer specifying a fixed width in characters,
or nil, meaning that there is no width limit.

If `shr-use-fonts' is set, the value of this variable is
interpreted as a multiple of the mean character width of the
default face's font.

If `shr-width' is non-nil, it overrides this variable."
  :version "28.1"
  :type '(choice (integer :tag "Fixed width in characters")
		 (const :tag "No width limit" nil)))

(defcustom shr-bullet "* "
  "Bullet used for unordered lists.
Alternative suggestions are:
- \"  \"
- \"  \""
  :version "24.4"
  :type 'string)

(defcustom shr-cookie-policy 'same-origin
  "When to use cookies when fetching dependent data like images.
If t, always use cookies.  If nil, never use cookies.  If
`same-origin', use cookies if the dependent data comes from the
same domain as the main data."
  :type '(choice (const :tag "Always use cookies" t)
                 (const :tag "Never use cookies" nil)
                 (const :tag "Use cookies for same domain" same-origin))
  :version "27.1")

(define-obsolete-variable-alias 'shr-external-browser
  'browse-url-secondary-browser-function "27.1")

(defcustom shr-image-animate t
  "Non-nil means that images that can be animated will be."
  :version "24.4"
  :type 'boolean)

(defcustom shr-offer-extend-specpdl t
  "Non-nil means offer to extend the specpdl if the HTML nests deeply.
Complicated HTML can require more nesting than the current specpdl
size permits.  If this variable is t, ask the user whether to increase
the specpdl size.  If nil, just give up."
  :version "28.1"
  :type 'boolean)

(defcustom shr-fill-text t
  "Non-nil means to fill the text according to the width of the window.
If nil, text is not filled, and `visual-line-mode' can be used to reflow text."
  :version "30.1"
  :type 'boolean)


(defcustom shr-sup-raise-factor 0.2
  "The value of raise property for superscripts.
Should be a non-negative float number between 0 and 1."
  :version "30.1"
  :type 'float)

(defcustom shr-sub-raise-factor -0.2
  "The value of raise property for subscripts.
Should be a non-positive float number between 0 and 1."
  :version "30.1"
  :type 'float)

(defcustom shr-image-ascent 100
  "The value to be used for :ascent property when inserting images."
  :version "30.1"
  :type 'integer)

(defcustom shr-max-inline-image-size nil
  "If non-nil, determines when the images can be displayed inline.
If nil, images are never displayed inline.

It non-nil, it should be cons (WIDTH . HEIGHT).

WIDTH can be an integer which is interpreted as number of pixels.  If the width
of an image exceeds this amount, the image is displayed on a separate line.
WIDTH can also be floating point number, in which case the image is displayed
inline if it occupies less than this fraction of window width.

HEIGHT can be also be an integer or a floating point number.  If it is an
integer and the pixel height of an image exceeds it, the image image is
displayed on a separate line.  If it is a float number , the limit is
interpreted as a multiple of the height of default font."
  :version "30.1"
  :type '(choice (const nil) (cons number number)))

(defcustom shr-image-zoom-levels '(fit original fill-height)
  "A list of image zoom levels to cycle through with `shr-zoom-image'.
The first element in the list is the initial zoom level.  Each element
can be one of the following symbols:

* `fit': Display the image at its original size as requested by the
  page, shrinking it to fit in the current window if necessary.
* `original': Display the image at its original size as requested by the
  page.
* `image': Display the image at its full size (ignoring the width/height
  specified by the HTML).
* `fill-height': Display the image zoomed to fill the height of the
current window."
  :version "31.1"
  :type '(set (const :tag "Fit to window size" fit)
              (const :tag "Original size" original)
              (const :tag "Full image size" image)
              (const :tag "Fill window height" fill-height)))

(defvar shr-content-function nil
  "If bound, this should be a function that will return the content.
This is used for cid: URLs, and the function is called with the
cid: URL as the argument.")

(defvar shr-put-image-function #'shr-put-image
  "Function called to put image and alt string.")

(defface shr-text '((t :inherit variable-pitch-text))
  "Face used for rendering text."
  :version "29.1")

(defface shr-strike-through '((t :strike-through t))
  "Face for <s> elements."
  :version "24.1")

(defface shr-link
  '((t :inherit link))
  "Face for link elements."
  :version "24.1")

(defface shr-selected-link
  '((t :inherit shr-link :background "red"))
  "Temporary face for externally visited link elements.
When a link is visited with an external browser, the link
temporarily blinks with this face."
  :version "27.1")

(defface shr-abbreviation
  '((t :inherit underline :underline (:style wave)))
  "Face for <abbr> elements."
  :version "27.1")

(defface shr-sup
  '((t :height 0.8))
  "Face for <sup> and <sub> elements."
  :version "29.1")

(defface shr-h1
  '((t :height 1.3 :weight bold))
  "Face for <h1> elements."
  :version "28.1")

(defface shr-h2
  '((t :weight bold))
  "Face for <h2> elements."
  :version "28.1")

(defface shr-h3
  '((t :slant italic))
  "Face for <h3> elements."
  :version "28.1")

(defface shr-h4
  '((t (:inherit default)))
  "Face for <h4> elements."
  :version "28.1")

(defface shr-h5
  '((t (:inherit default)))
  "Face for <h5> elements."
  :version "28.1")

(defface shr-h6
  '((t (:inherit default)))
  "Face for <h6> elements."
  :version "28.1")

(defface shr-code '((t :inherit fixed-pitch))
  "Face used for rendering <code> blocks."
  :version "29.1")

(defface shr-mark
  '((t :background "yellow" :foreground "black"))
  "Face used for <mark> elements."
  :version "29.1")

(defface shr-sliced-image
  '((t :underline nil :overline nil))
  "Face used for sliced images.
This face should remove any unsightly decorations from sliced images.
Otherwise, decorations like underlines from links would normally show on
every slice."
  :version "30.1")

(defcustom shr-inhibit-images nil
  "If non-nil, inhibit loading images."
  :version "28.1"
  :type 'boolean)

(defvar shr-external-rendering-functions nil
  "Alist of tag/function pairs used to alter how shr renders certain tags.
For instance, eww uses this to alter rendering of title, forms
and other things:
\((title . eww-tag-title)
 (form . eww-tag-form)
 ...)")

;;; Internal variables.

(defvar shr-folding-mode nil)
(defvar shr-start nil)
(defvar shr-indentation 0)
(defvar shr-internal-width nil)
(defvar shr-list-mode nil)
(defvar shr-content-cache nil)
(defvar shr-table-depth 0)
(defvar shr-stylesheet nil)
(defvar shr-base nil)
(defvar shr-depth 0)
(defvar shr-warning nil)
(defvar shr-ignore-cache nil)
(defvar shr-table-separator-length 1)
(defvar shr-table-separator-pixel-width 0)
(defvar shr-table-id nil)
(defvar shr-current-font nil)
(defvar shr-internal-bullet nil)

(defvar shr-target-id nil
  "Target fragment identifier anchor.")
(defvar shr--link-targets nil)

(defvar-keymap shr-map
  "a" #'shr-show-alt-text
  "M-i" #'shr-browse-image
  "z" #'shr-zoom-image
  "TAB" #'shr-next-link
  "C-M-i" #'shr-previous-link
  "<follow-link>" 'mouse-face
  "<mouse-2>" #'shr-browse-url
  "C-<down-mouse-1>" #'shr-mouse-browse-url-new-window
  "I" #'shr-insert-image
  "w" #'shr-maybe-probe-and-copy-url
  "u" #'shr-maybe-probe-and-copy-url
  "v" #'shr-browse-url
  "O" #'shr-save-contents
  "RET" #'shr-browse-url)

(defvar-keymap shr-image-map
  :parent (if (boundp 'image-map)
              (make-composed-keymap shr-map image-map)
            shr-map))

(defvar shr-url-transformer #'identity
  "Function to transform URLs.
It's called with the URL as the parameter, and should return the
 URL to use.")

;; Public functions and commands.
(declare-function libxml-parse-html-region "xml.c"
		  (start end &optional base-url discard-comments))

(defun shr-render-buffer (buffer)
  "Display the HTML rendering of the current buffer."
  (interactive (list (current-buffer)))
  (or (fboundp 'libxml-parse-html-region)
      (error "This function requires Emacs to be compiled with libxml2"))
  (pop-to-buffer "*html*")
  (erase-buffer)
  (shr-insert-document
   (with-current-buffer buffer
     (libxml-parse-html-region (point-min) (point-max))))
  (goto-char (point-min)))

;;;###autoload
(defun shr-render-region (begin end &optional buffer)
  "Display the HTML rendering of the region between BEGIN and END."
  (interactive "r")
  (unless (fboundp 'libxml-parse-html-region)
    (error "This function requires Emacs to be compiled with libxml2"))
  (with-current-buffer (or buffer (current-buffer))
    (let ((dom (libxml-parse-html-region begin end)))
      (delete-region begin end)
      (goto-char begin)
      (shr-insert-document dom))))

(defun shr--have-one-fringe-p ()
  "Return non-nil if we know at least one of the fringes has non-zero width."
  (and (fboundp 'fringe-columns)
       (or (not (zerop (fringe-columns 'right)))
           (not (zerop (fringe-columns 'left))))))

(defun shr--window-width ()
  ;; Compute the width based on the window width.  We need to
  ;; adjust the available width for when the user disables
  ;; the fringes, which will cause the display engine usurp
  ;; one column for the continuation glyph.
  (if (not shr-use-fonts)
      (- (window-body-width) 1
         (if (shr--have-one-fringe-p)
             1
           0))
    (pixel-fill-width)))

(defmacro shr-string-pixel-width (string)
  `(if (not shr-use-fonts)
       (length ,string)
     (string-pixel-width ,string)))

;;;###autoload
(defun shr-insert-document (dom)
  "Render the parsed document DOM into the current buffer.
DOM should be a parse tree as generated by
`libxml-parse-html-region' or similar."
  (setq shr-content-cache nil)
  (let ((start (point))
	(shr-start nil)
	(shr-base nil)
	(shr-depth 0)
	(shr-table-id 0)
	(shr-warning nil)
	(shr-table-separator-pixel-width (shr-string-pixel-width "-"))
	(shr-internal-bullet (cons shr-bullet
				   (shr-string-pixel-width shr-bullet)))
	(shr-internal-width
         (if shr-width
             ;; Specified width; use it.
	     (if (not shr-use-fonts)
		 shr-width
	       (* shr-width (frame-char-width)))
           (shr--window-width)))
        (shr--link-targets nil)
        (hscroll (window-hscroll))
        ;; `bidi-display-reordering' is supposed to be only used for
        ;; debugging purposes, but Shr's naïve filling algorithm
        ;; cannot cope with the complexity of RTL text in an LTR
        ;; paragraph, when a long line has been continued, and for
        ;; most scripts the character metrics don't change when they
        ;; are reordered, so...  this is the best we could do :-(
        bidi-display-reordering)
    ;; Adjust for max width specification.
    (when (and shr-max-width
               (not shr-width))
      (setq shr-internal-width
            (min shr-internal-width
                 (if shr-use-fonts
                     (* shr-max-width (frame-char-width))
                   shr-max-width))))
    ;; If the window was hscrolled for some reason, shr-fill-lines
    ;; below will misbehave, because it silently assumes that it
    ;; starts with a non-hscrolled window (vertical-motion will move
    ;; to a wrong place otherwise).
    (unwind-protect
        (progn
          (set-window-hscroll nil 0)
          (shr-descend dom)
          (shr-fill-lines start (point))
          (shr--remove-blank-lines-at-the-end start (point))
          (shr--set-target-ids shr--link-targets))
      (set-window-hscroll nil hscroll))
    (when shr-warning
      (message "%s" shr-warning))))

(defun shr--set-target-ids (ids)
  ;; If the buffer is empty, there's no point in setting targets.
  (unless (zerop (- (point-max) (point-min)))
    ;; We may have several targets in the same place (if you have
    ;; several <span id='foo'> things after one another).  So group
    ;; them by position.
    (dolist (group (seq-group-by #'cdr ids))
      (let ((point (min (1- (point-max)) (car group))))
        (put-text-property point (1+ point)
                           'shr-target-id
                           (mapcar #'car (cdr group)))))))

(defun shr--remove-blank-lines-at-the-end (start end)
  (save-restriction
    (save-excursion
      (narrow-to-region start end)
      (goto-char end)
      (when (and (re-search-backward "[^ \n]" nil t)
                 (not (eobp)))
        (forward-line 1)
        (delete-region (point) (point-max))))))

(defun shr-url-at-point (image-url)
  "Return the URL under point as a string.
If IMAGE-URL is non-nil, or there is no link under point, but
there is an image under point then copy the URL of the image
under point instead."
  (if image-url
      (get-text-property (point) 'image-url)
    (or (get-text-property (point) 'shr-url)
        (get-text-property (point) 'image-url))))

(defun shr-copy-url (url)
  "Copy the URL under point to the kill ring.
With a prefix argument, or if there is no link under point, but
there is an image under point then copy the URL of the image
under point instead."
  (interactive (list (shr-url-at-point current-prefix-arg)))
  (if (not url)
      (message "No URL under point")
    (setq url (url-encode-url url))
    (kill-new url)
    (message "Copied %s" url)))

(defun shr-probe-url (url cont)
  "Pass URL's redirect destination to CONT, if it has one.
CONT should be a function of one argument, the redirect
destination URL.  If URL is not redirected, then CONT is never
called."
  (interactive "P")
  (url-retrieve
   url (lambda (a)
         (pcase a
           (`(:redirect ,destination . ,_)
            ;; Remove common tracking junk from the URL.
            (funcall cont (replace-regexp-in-string
                           ".utm_.*" "" destination)))))
   nil t t))

(defun shr-probe-and-copy-url (url)
  "Copy the URL under point to the kill ring.
Like `shr-copy-url', but additionally fetch URL and use its
redirection destination if it has one."
  (interactive (list (shr-url-at-point current-prefix-arg)))
  (if url (shr-probe-url url #'shr-copy-url)
    (shr-copy-url url)))

(defun shr-maybe-probe-and-copy-url (url)
  "Copy the URL under point to the kill ring.
If the URL is already at the front of the kill ring act like
`shr-probe-and-copy-url', otherwise like `shr-copy-url'."
  (interactive (list (shr-url-at-point current-prefix-arg)))
  (if (equal url (car kill-ring))
      (shr-probe-and-copy-url url)
    (shr-copy-url url)))

(defun shr--current-link-region ()
  "Return the start and end positions of the URL at point, if any.
Value is a pair of positions (START . END) if there is a non-nil
`shr-url' text property at point; otherwise nil."
  (when (get-text-property (point) 'shr-url)
    (let* ((end (or (next-single-property-change (point) 'shr-url)
                    (point-max)))
           (beg (or (previous-single-property-change end 'shr-url)
                    (point-min))))
      (cons beg end))))

(defun shr--blink-link ()
  "Briefly fontify URL at point with the face `shr-selected-link'."
  (when-let* ((region  (shr--current-link-region))
              (overlay (make-overlay (car region) (cdr region))))
    (overlay-put overlay 'face 'shr-selected-link)
    (run-at-time 1 nil (lambda ()
                         (delete-overlay overlay)))))

(defun shr-next-link ()
  "Skip to the next link."
  (interactive)
  (let ((match (text-property-search-forward 'shr-tab-stop nil nil t)))
    (if (not match)
        (message "No next link")
      (goto-char (prop-match-beginning match))
      (message "%s" (get-text-property (point) 'help-echo)))))

(defun shr-previous-link ()
  "Skip to the previous link."
  (interactive)
  (if (not (text-property-search-backward 'shr-tab-stop nil nil t))
      (message "No previous link")
    (message "%s" (get-text-property (point) 'help-echo))))

(defun shr-show-alt-text ()
  "Show the ALT text of the image under point."
  (declare (completion (lambda (_ b) (command-completion-button-p 'shr b))))
  (interactive)
  (let ((text (get-text-property (point) 'shr-alt)))
    (if (not text)
	(message "No image under point")
      (message "%s" (shr-fill-text text)))))

(defun shr-browse-image (&optional copy-url)
  "Browse the image under point.
If COPY-URL (the prefix if called interactively) is non-nil, copy
the URL of the image to the kill buffer instead."
  (interactive "P")
  (let ((url (get-text-property (point) 'image-url)))
    (cond
     ((not url)
      (message "No image under point"))
     (copy-url
      (with-temp-buffer
	(insert url)
	(copy-region-as-kill (point-min) (point-max))
	(message "Copied %s" url)))
     (t
      (message "Browsing %s..." url)
      (browse-url url)))))

(defun shr-insert-image ()
  "Insert the image under point into the buffer."
  (interactive)
  (let ((url (get-text-property (point) 'image-url)))
    (if (not url)
	(message "No image under point")
      (message "Inserting %s..." url)
      (url-retrieve url #'shr-image-fetched
		    (list (current-buffer) (1- (point)) (point-marker))
		    t))))

(defvar shr-image-zoom-level-alist
  `((fit         "Zoom to fit"                shr-rescale-image)
    (original    "Zoom to original size"      shr--image-zoom-original-size)
    (image       "Zoom to full image size"    shr--image-zoom-image-size)
    (fill-height "Zoom to fill window height" shr--image-zoom-fill-height))
  "An alist of possible image zoom levels.
Each element is of the form (SYMBOL DESC FUNCTION).  SYMBOL is the
symbol identifying this level, as used by `shr-image-zoom-levels' (which
see).  DESC is a string describing the level.

FUNCTION is a function that returns a properly-zoomed image; it takes
the following arguments:

* DATA: The image data in string form.
* CONTENT-TYPE: The content-type of the image, if any.
* WIDTH: The width as specified by the HTML \"width\" attribute, if any.
* HEIGHT: The height as specified by the HTML \"height\" attribute, if
  any.")

(defun shr-zoom-image (&optional position zoom-level)
  "Change the zoom level of the image at POSITION.

The size will cycle through the default size, the original size, and
full-buffer size."
  (interactive "d")
  (unless position (setq position (point)))
  (let ((url (get-text-property position 'image-url)))
    (if (not url)
	(message "No image under point")
      (unless zoom-level
        (let ((last-zoom (get-text-property position 'image-zoom)))
          (setq zoom-level (or (cadr (memq last-zoom shr-image-zoom-levels))
                               (car shr-image-zoom-levels)))))
      (let* ((end (or (next-single-property-change position 'image-url)
                      (point-max)))
             (start (or (previous-single-property-change end 'image-url)
                        (point-min)))
             (dom-size (get-text-property position 'image-dom-size))
             (flags `( :zoom   ,zoom-level
                       :width  ,(car dom-size)
                       :height ,(cdr dom-size)))
             (buffer-read-only nil))
        ;; Delete the old picture.
        (put-text-property start end 'display nil)
        (message "%s" (cadr (assq zoom-level shr-image-zoom-level-alist)))
        (if (and (not shr-ignore-cache)
                 (url-is-cached url))
            (shr-replace-image (shr-get-image-data url) start
                               (set-marker (make-marker) end) flags)
          (url-retrieve url #'shr-image-fetched
                        `(,(current-buffer) ,start
                          ,(set-marker (make-marker) end)
                          ,flags)
                        t))))))

;;; Utility functions.

(defsubst shr-generic (dom)
  (dolist (sub (dom-children dom))
    (if (stringp sub)
	(shr-insert sub)
      (shr-descend sub))))

(defun shr-image-blocked-p (url)
  (or (and shr-blocked-images
           (string-match shr-blocked-images url))
      (and shr-allowed-images
           (not (string-match shr-allowed-images url)))))

(defun shr-indirect-call (tag-name dom &rest args)
  (let ((function (intern (concat "shr-tag-" (symbol-name tag-name)) obarray))
	;; Allow other packages to override (or provide) rendering
	;; of elements.
	(external (cdr (assq tag-name shr-external-rendering-functions))))
    (cond (external
	   (apply external dom args))
	  ((fboundp function)
	   (apply function dom args))
	  (t
           (apply #'shr-generic dom args)))))

(defun shr-descend (dom)
  (let ((function
         (intern (concat "shr-tag-" (symbol-name (dom-tag dom))) obarray))
        ;; Allow other packages to override (or provide) rendering
        ;; of elements.
        (external (cdr (assq (dom-tag dom) shr-external-rendering-functions)))
	(style (dom-attr dom 'style))
	(shr-stylesheet shr-stylesheet)
	(shr-depth (1+ shr-depth))
	(start (point)))
    (when style
      (if (string-match-p "color\\|display\\|border-collapse" style)
	  (setq shr-stylesheet (nconc (shr-parse-style style)
				      shr-stylesheet))
	(setq style nil)))
    ;; If we have a display:none, then just ignore this part of the DOM.
    (unless (or (equal (cdr (assq 'display shr-stylesheet)) "none")
                (and shr-discard-aria-hidden
                     (equal (dom-attr dom 'aria-hidden) "true")))
      ;; We don't use shr-indirect-call here, since shr-descend is
      ;; the central bit of shr.el, and should be as fast as
      ;; possible.  Having one more level of indirection with its
      ;; negative effect on performance is deemed unjustified in
      ;; this case.
      (cond (external
             (funcall external dom))
            ((fboundp function)
             (funcall function dom))
            (t
             (shr-generic dom)))
      (when-let* ((id (dom-attr dom 'id)))
        (push (cons id (set-marker (make-marker) start)) shr--link-targets))
      ;; If style is set, then this node has set the color.
      (when style
	(shr-colorize-region
	 start (point)
	 (cdr (assq 'color shr-stylesheet))
	 (cdr (assq 'background-color shr-stylesheet)))))))

(defun shr-fill-text (text)
  (if (zerop (length text))
      text
    (with-temp-buffer
      (let ((shr-indentation 0)
	    (shr-start nil)
	    (shr-internal-width (shr--window-width)))
	(shr-insert text)
	(shr-fill-lines (point-min) (point-max))
	(buffer-string)))))

(defun shr-pixel-column ()
  (if (not shr-use-fonts)
      (current-column)
    (if (not (get-buffer-window (current-buffer)))
	(save-window-excursion
          ;; Avoid errors if the selected window is a dedicated one,
          ;; and they just want to insert a document into it.
          (set-window-dedicated-p nil nil)
	  (set-window-buffer nil (current-buffer))
	  (car (window-text-pixel-size nil (line-beginning-position) (point))))
      (car (window-text-pixel-size nil (line-beginning-position) (point))))))

(defun shr-pixel-region ()
  (declare (obsolete nil "29.1"))
  (- (shr-pixel-column)
     (save-excursion
       (goto-char (mark))
       (shr-pixel-column))))

(defsubst shr--translate-insertion-chars ()
  ;; Remove soft hyphens.
  (goto-char (point-min))
  (while (search-forward "­" nil t)
    (replace-match "" t t))
  ;; Translate non-breaking spaces into real spaces.
  (goto-char (point-min))
  (while (search-forward " " nil t)
    (replace-match " " t t)))

(defun shr-insert (text)
  (when (and (not shr-max-inline-image-size)
	     (not (bolp))
	     (get-text-property (1- (point)) 'image-url))
    (insert "\n"))
  (cond
   ((eq shr-folding-mode 'none)
    (let ((start (point)))
      (insert text)
      (save-restriction
	(narrow-to-region start (point))
        (shr--translate-insertion-chars)
	(goto-char (point-max)))))
   (t
    (let ((font-start (point)))
      (when (and (string-match-p "\\`[ \t\n\r]" text)
		 (not (bolp))
		 (not (eq (char-after (1- (point))) ? )))
	(insert " "))
      (let ((start (point))
	    (bolp (bolp)))
	(insert text)
	(save-restriction
	  (narrow-to-region start (point))
	  (goto-char start)
	  (when (looking-at "[ \t\n\r]+")
	    (replace-match "" t t))
	  (while (re-search-forward "[\t\n\r]+" nil t)
	    (replace-match " " t t))
	  (goto-char start)
          (while (re-search-forward "  +" nil t)
            (replace-match " " t t))
          (shr--translate-insertion-chars)
	  (goto-char (point-max)))
	;; We may have removed everything we inserted if it was just
	;; spaces.
	(unless (= font-start (point))
	  ;; Mark all lines that should possibly be folded afterwards.
	  (when bolp
	    (shr-mark-fill start))
	  (when shr-use-fonts
	    (put-text-property font-start (point)
			       'face
			       (or shr-current-font 'shr-text)))))))))

(defun shr-fill-lines (start end)
  "Indent and fill text from START to END.
When `shr-fill-text' is nil, only indent."
  (unless (<= shr-internal-width 0)
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (when (get-text-property (point) 'shr-indentation)
	(shr-fill-line))
      (while (setq start (next-single-property-change start 'shr-indentation))
	(goto-char start)
	(when (bolp)
	  (shr-fill-line)))
      (goto-char (point-max)))))

(defun shr-vertical-motion (column)
  (if (not shr-use-fonts)
      (move-to-column column)
    (unless (eolp)
      (forward-char 1))
    (vertical-motion (cons (/ column (frame-char-width)) 0))
    (unless (eolp)
      (forward-char 1))))

(defun shr-fill-line ()
  "Indent and fill the current line.
When `shr-fill-text' is nil, only indent."
  (let ((shr-indentation (or (get-text-property (point) 'shr-indentation)
                             shr-indentation))
	(continuation (get-text-property
		       (point) 'shr-continuation-indentation))
	start)
    (put-text-property (point) (1+ (point)) 'shr-indentation nil)
    (let ((face (get-text-property (point) 'face))
	  (background-start (point)))
      (shr-indent)
      (when face
	(put-text-property background-start (point) 'face
			   `,(shr-face-background face))))
    (setq start (point))
    (setq shr-indentation (or continuation shr-indentation))
    ;; Fill the current line, unless `shr-fill-text' is unset, or we
    ;; have an indentation that's wider than the width we're trying to
    ;; fill to.
    (when (and shr-fill-text
               (< shr-indentation shr-internal-width))
      (shr-vertical-motion shr-internal-width)
      (when (looking-at " $")
        (delete-region (point) (line-end-position)))
      (while (not (eolp))
        ;; We have to do some folding.  First find the first
        ;; previous point suitable for folding.
        (if (or (not (pixel-fill-find-fill-point (line-beginning-position)))
	        (= (point) start))
	    ;; We had unbreakable text (for this width), so just go to
	    ;; the first space and carry on.
	    (progn
	      (beginning-of-line)
	      (skip-chars-forward " ")
	      (search-forward " " (line-end-position) 'move)))
        ;; Success; continue.
        (when (= (preceding-char) ?\s)
	  (delete-char -1))
        (let ((gap-start (point))
              (face (get-text-property (point) 'face)))
          ;; Extend the background to the end of the line.
          (insert ?\n)
	  (shr-indent)
          (when face
            (put-text-property gap-start (point)
                               'face (shr-face-background face)))
          (when (and (> (1- gap-start) (point-min))
                     (get-text-property (point) 'shr-url)
                     ;; The link on both sides of the newline are the
                     ;; same...
                     (equal (get-text-property (point) 'shr-url)
                            (get-text-property (1- gap-start) 'shr-url)))
            ;; ... so we join the two bits into one link logically, but
            ;; not visually.  This makes navigation between links work
            ;; well, but avoids underscores before the link on the next
            ;; line when indented.
            (let* ((props (copy-sequence (text-properties-at (point))))
                   (face (plist-get props 'face)))
              ;; We don't want to use the faces on the indentation, because
              ;; that's ugly, but we do want to use the background color.
              (when face
                (setq props (plist-put props 'face (shr-face-background face))))
	      (add-text-properties gap-start (point) props))))
        (setq start (point))
        (shr-vertical-motion shr-internal-width)
        (when (looking-at " $")
	  (delete-region (point) (line-end-position)))))))

(defun shr-adaptive-fill-function ()
  "Return a fill prefix for the paragraph at point."
  (when-let* ((prefix (get-text-property (point) 'shr-prefix-length)))
    (buffer-substring (point) (+ (point) prefix))))

(defun shr-parse-base (url)
  ;; Always chop off anchors.
  (when (string-match "#.*" url)
    (setq url (substring url 0 (match-beginning 0))))
  ;; NB: <base href=""> URI may itself be relative to the document's URI.
  (setq url (shr-expand-url url))
  (let* ((parsed (url-generic-parse-url url))
	 (local (or (url-filename parsed) "")))
    (setf (url-filename parsed) "")
    ;; Chop off the bit after the last slash.
    (when (string-match "\\`\\(.*/\\)[^/]+\\'" local)
      (setq local (match-string 1 local)))
    ;; Always make the local bit end with a slash.
    (when (and (not (zerop (length local)))
	       (not (eq (aref local (1- (length local))) ?/)))
      (setq local (concat local "/")))
    (list (url-recreate-url parsed)
	  local
	  (url-type parsed)
	  url)))

(autoload 'url-expand-file-name "url-expand")

;; FIXME This needs some tests writing.
;; Does it even need to exist, given that url-expand-file-name does?
(defun shr-expand-url (url &optional base)
  (setq base
	(if base
	    ;; shr-parse-base should never call this with non-nil base!
	    (shr-parse-base base)
	  ;; Bound by the parser.
	  shr-base))
  (when (zerop (length url))
    (setq url nil))
  ;; Strip leading/trailing whitespace.
  (when url
    (setq url (string-trim url)))
  (cond ((zerop (length url))
         (nth 3 base))
        ((or (not base)
	     (string-match-p "\\`[a-z]*:" url))
	 ;; Absolute or empty URI
	 url)
	((eq (aref url 0) ?/)
	 (if (and (> (length url) 1)
		  (eq (aref url 1) ?/))
	     ;; //host...; just use the protocol
	     (concat (nth 2 base) ":" url)
	   ;; Just use the host name part.
	   (concat (car base) url)))
	((eq (aref url 0) ?#)
	 ;; A link to an anchor.
	 (concat (nth 3 base) url))
	(t
	 ;; Totally relative.  Allow Tramp file names if we're
	 ;; rendering a file:// URL.
         (let ((url-allow-non-local-files (equal (nth 2 base) "file")))
	   (url-expand-file-name url (concat (car base) (cadr base)))))))

(defun shr-ensure-newline ()
  (unless (bobp)
    (let ((prefix (get-text-property (line-beginning-position)
				     'shr-prefix-length)))
      (unless (or (zerop (current-column))
                  (and prefix
                       (= prefix (- (point) (line-beginning-position)))))
        (insert "\n")))))

(defun shr-ensure-paragraph ()
  (unless (bobp)
    (let ((prefix (get-text-property (line-beginning-position)
				     'shr-prefix-length)))
      (cond
       ((and (bolp)
	     (save-excursion
	       (forward-line -1)
	       (looking-at " *$")))
	;; We're already at a new paragraph; do nothing.
	)
       ((and prefix
	     (= prefix (- (point) (line-beginning-position))))
	;; Do nothing; we're at the start of a <li>.
	)
       ((save-excursion
	  (beginning-of-line)
	  ;; If the current line is totally blank, and doesn't even
	  ;; have any face properties set, then delete the blank
	  ;; space.
	  (and (looking-at " *$")
	       (not (get-text-property (point) 'face))
	       (not (= (next-single-property-change (point) 'face nil
						    (line-end-position))
		       (line-end-position)))))
	(delete-region (match-beginning 0) (match-end 0)))
       ;; We have a single blank line.
       ((and (eolp) (bolp))
        (insert "\n"))
       ;; Insert new paragraph.
       (t
	(insert "\n\n"))))))

(defun shr-indent ()
  (when (> shr-indentation 0)
    (let ((start (point))
          (prefix (or (get-text-property (point) 'shr-prefix-length) 0)))
      (if (not shr-use-fonts)
          (insert-char ?\s shr-indentation)
        (insert ?\s)
        ;; Set the specified space width in units of the average-width
        ;; of the current face, like (N . width).  That way, the
        ;; indentation is calculated correctly when using
        ;; `text-scale-adjust'.
        (let ((avg-space (propertize (buffer-substring (1- (point)) (point))
                                     'display '(space :width (1 . width)))))
          (put-text-property
           (1- (point)) (point) 'display
           `(space :width (,(/ (float shr-indentation)
                               (string-pixel-width avg-space))
                           . width)))))
      (put-text-property start (+ (point) prefix)
                         'shr-prefix-length (+ prefix (- (point) start))))))

(defun shr-fontize-dom (dom &rest types)
  (let ((start (point)))
    (shr-generic dom)
    (dolist (type types)
      (shr-add-font start (point) type))))

;; Add face to the region, but avoid putting the font properties on
;; blank text at the start of the line, and the newline at the end, to
;; avoid ugliness.
(defun shr-add-font (start end type)
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (when (bolp)
        (skip-chars-forward " "))
      (add-face-text-property (point) (min (line-end-position) end) type t)
      (if (< (line-end-position) end)
          (forward-line 1)
        (goto-char end)))))

(defun shr-mouse-browse-url (ev)
  "Browse the URL under the mouse cursor."
  (interactive "e")
  (mouse-set-point ev)
  (shr-browse-url))

(defun shr-mouse-browse-url-new-window (ev)
  "Browse the URL under the mouse cursor in a new window."
  (interactive "e")
  (mouse-set-point ev)
  (shr-browse-url nil nil t))

(defun shr-browse-url (&optional secondary mouse-event new-window)
  "Browse the URL at point using `browse-url'.
If SECONDARY is non-nil (interactively, the prefix argument), browse
the URL using `browse-url-secondary-browser-function'.
If this function is invoked by a mouse click, it will browse the URL
at the position of the click.  Optional argument MOUSE-EVENT describes
the mouse click event."
  (interactive (list current-prefix-arg last-nonmenu-event))
  (mouse-set-point mouse-event)
  (let ((url (get-text-property (point) 'shr-url)))
    (cond
     ((not url)
      (message "No link under point"))
     (secondary
      (let ((browse-url-browser-function browse-url-secondary-browser-function))
        (browse-url url))
      (shr--blink-link))
     (t
      (browse-url url (xor new-window browse-url-new-window-flag))))))

(defun shr-save-contents (directory)
  "Save the contents from URL in a file."
  (interactive "DSave contents of URL to directory: ")
  (let ((url (get-text-property (point) 'shr-url)))
    (if (not url)
	(message "No link under point")
      (url-retrieve url #'shr-store-contents (list url directory)))))

(defun shr-store-contents (status url directory)
  (unless (plist-get status :error)
    (when (or (search-forward "\n\n" nil t)
	      (search-forward "\r\n\r\n" nil t))
      (write-region (point) (point-max)
		    (expand-file-name (file-name-nondirectory url)
				      directory)))))

(defun shr-replace-image (data start end &optional flags)
  (save-excursion
    (save-restriction
      (widen)
      (let ((alt (buffer-substring start end))
	    (properties (text-properties-at start))
            ;; We don't want to record these changes.
            (buffer-undo-list t)
	    (inhibit-read-only t))
        (remove-overlays start end)
	(delete-region start end)
	(goto-char start)
	(funcall shr-put-image-function data alt flags)
	(while properties
	  (let ((type (pop properties))
		(value (pop properties)))
	    (unless (memq type '(display image-zoom))
	      (put-text-property start (point) type value))))))))

(defun shr-image-fetched (status buffer start end &optional flags)
  (let ((image-buffer (current-buffer)))
    (when (and (buffer-name buffer)
	       (not (plist-get status :error)))
      (url-store-in-cache image-buffer)
      (goto-char (point-min))
      (when (or (search-forward "\n\n" nil t)
		(search-forward "\r\n\r\n" nil t))
	(let ((data (shr-parse-image-data)))
	  (with-current-buffer buffer
	    (shr-replace-image data start end flags)))))
    (kill-buffer image-buffer)))

(defun shr-image-from-data (data)
  "Return an image from the data: URI content DATA."
  (when (string-match
	 "\\(\\([^/;,]+\\(/[^;,]+\\)?\\)\\(;[^;,]+\\)*\\)?,\\(.*\\)"
	 data)
    (let ((param (match-string 4 data))
	  (payload (url-unhex-string (match-string 5 data))))
      (when (and param
                 (string-match-p "^.*\\(;[ \t]*base64\\)$" param))
	(setq payload (ignore-errors
                        (base64-decode-string payload))))
      payload)))

;; Behind display-graphic-p test.
(declare-function image-size "image.c" (spec &optional pixels frame))
(declare-function image-animate "image" (image &optional index limit position))

(defun shr--inline-image-p (image)
  "Return non-nil if IMAGE should be displayed inline."
  (when shr-max-inline-image-size
    (let ((size (image-size image t))
	  (max-width (car shr-max-inline-image-size))
	  (max-height (cdr shr-max-inline-image-size)))
      (unless (integerp max-width)
	(setq max-width (* max-width (window-width nil t))))
      (unless (integerp max-height)
	(setq max-height (* max-height (frame-char-height))))
      (and (< (car size) max-width)
	   (< (cdr size) max-height)))))

(defun shr-put-image (spec alt &optional flags)
  "Insert image SPEC with a string ALT.  Return image.
SPEC is either an image data blob, or a list where the first
element is the data blob and the second element is the content-type.

FLAGS is a property list specifying optional parameters for the image.
You can specify the following optional properties:

* `:zoom': The zoom level for the image.  One of `default', `original',
  or `full'.
* `:width': The width of the image as specified by the HTML \"width\"
  attribute.
* `:height': The height of the image as specified by the HTML
  \"height\" attribute."
  (if (display-graphic-p)
      (let* ((zoom (or (plist-get flags :zoom)
                       (car shr-image-zoom-levels)))
             (zoom-function (or (nth 2 (assq zoom shr-image-zoom-level-alist))
                                (error "Unrecognized zoom level %s" zoom)))
	     (data (if (consp spec)
		       (car spec)
		     spec))
	     (content-type (and (consp spec)
				(cadr spec)))
	     (start (point))
	     (image (ignore-errors
                      (funcall zoom-function data content-type
                               (plist-get flags :width)
                               (plist-get flags :height)))))
        (when image
          ;; The trailing space can confuse shr-insert into not
          ;; putting any space after inline images.
          ;; ALT may be nil when visiting image URLs in eww
          ;; (bug#67764).
          (setq alt (string-trim (or alt "")))
          (when (length= alt 0) (setq alt "*"))
	  ;; When inserting big-ish pictures, put them at the
	  ;; beginning of the line.
	  (let ((inline (shr--inline-image-p image)))
	    (when (and (> (current-column) 0)
		     (not inline))
		(insert "\n"))
	    (let ((image-pos (point))
                  image-height body-height)
	      (if (and shr-sliced-image-height
                       (setq image-height (cdr (image-size image t))
                             body-height (window-body-height
                                          (get-buffer-window (current-buffer))
                                          t))
                       (> (/ image-height body-height 1.0)
                          shr-sliced-image-height))
                  ;; Normally, we try to keep the buffer text the same
                  ;; by preserving ALT.  With a sliced image, we have to
                  ;; repeat the text for each line, so we can't do that.
                  ;; Just use "*" for the string to insert instead.
                  (progn
                    (insert-sliced-image
                     image "*" nil (/ image-height (default-line-height)) 1)
                    (let ((overlay (make-overlay start (point))))
                      ;; Avoid displaying unsightly decorations on the
                      ;; image slices.
                      (overlay-put overlay 'face 'shr-sliced-image)))
		(insert-image image alt))
	      (put-text-property start (point) 'image-zoom zoom)
	      (when (and (not inline) shr-max-inline-image-size)
		(insert "\n"))
	      (when (and shr-image-animate
			 (cdr (image-multi-frame-p image)))
		(image-animate image nil 60 image-pos)))))
	image)
    (insert (or alt ""))))

(defun shr--image-type ()
  "Emacs image type to use when displaying images.
If Emacs has native image scaling support, that's used, but if
not, `imagemagick' is preferred if it's present."
  (if (or (and (fboundp 'image-transforms-p)
	       (image-transforms-p))
	  (not (fboundp 'imagemagick-types)))
      nil
    'imagemagick))

(defvar image-scaling-factor)
(defun shr-rescale-image (data content-type width height
                               &optional max-width max-height)
  "Rescale DATA, if too big, to fit the current buffer.
WIDTH and HEIGHT are the sizes given in the HTML data, if any.

The size of the displayed image will not exceed
MAX-WIDTH/MAX-HEIGHT.  If not given, use the current window
width/height instead."
  (if (not (get-buffer-window (current-buffer) t))
      (create-image data nil t :ascent shr-image-ascent)
    (let* ((edges (window-inside-pixel-edges
                   (get-buffer-window (current-buffer))))
           (max-width (truncate (* shr-max-image-proportion
                                   (or max-width
                                       (- (nth 2 edges) (nth 0 edges))))))
           (max-height (truncate (* shr-max-image-proportion
                                    (or max-height
                                        (- (nth 3 edges) (nth 1 edges))))))
           (scaling (image-compute-scaling-factor image-scaling-factor)))
      (when (and width (> (* width scaling) max-width))
        (setq width nil))
      (when (and height (> (* height scaling) max-height))
        (setq height nil))
      (create-image
       data (shr--image-type) t
       :ascent shr-image-ascent
       :width width
       :height height
       :max-width max-width
       :max-height max-height
       :format content-type))))

(defun shr--image-zoom-original-size (data content-type width height)
  (create-image data (shr--image-type) t :ascent shr-image-ascent
                :width width :height height :format content-type))

(defun shr--image-zoom-image-size (data content-type _width _height)
  (create-image data nil t :ascent shr-image-ascent :format content-type))

(defun shr--image-zoom-fill-height (data content-type _width _height)
  (let* ((edges (window-inside-pixel-edges
                 (get-buffer-window (current-buffer))))
         (height (truncate (* shr-max-image-proportion
                              (- (nth 3 edges) (nth 1 edges))))))
    (create-image data (shr--image-type) t :ascent shr-image-ascent
                  :height height :format content-type)))

;; url-cache-extract autoloads url-cache.
(declare-function url-cache-create-filename "url-cache" (url))

(defun shr-get-image-data (url)
  "Get image data for URL.
Return a string with image data."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (when (ignore-errors
	    (url-cache-extract (url-cache-create-filename url))
	    t)
      (when (re-search-forward "\r?\n\r?\n" nil t)
	(shr-parse-image-data)))))

(declare-function libxml-parse-xml-region "xml.c"
		  (start end &optional base-url discard-comments))

(defun shr-parse-image-data ()
  (let ((data (buffer-substring (point) (point-max)))
	(content-type
	 (save-excursion
	   (save-restriction
	     (narrow-to-region (point-min) (point))
	     (let ((content-type (mail-fetch-field "content-type")))
	       (and content-type
		    ;; Remove any comments in the type string.
		    (intern (replace-regexp-in-string ";.*" "" content-type)
			    obarray)))))))
    ;; SVG images may contain references to further images that we may
    ;; want to block.  So special-case these by parsing the XML data
    ;; and remove anything that looks like a blocked bit.
    (when (and (or shr-allowed-images shr-blocked-images)
               (eq content-type 'image/svg+xml))
      (setq data
            ;; Note that libxml2 doesn't parse everything perfectly,
            ;; so glitches may occur during this transformation.  And
            ;; encode as utf-8: There may be text (and other elements)
            ;; that are non-ASCII.
	    (shr-dom-to-xml
	     (libxml-parse-xml-region (point) (point-max)) 'utf-8)))
    (list data content-type)))

(defun shr-image-displayer (content-function)
  "Return a function to display an image.
CONTENT-FUNCTION is a function to retrieve an image for a cid url that
is an argument.  The function to be returned takes three arguments URL,
START, and END.  Note that START and END should be markers."
  (lambda (url start end)
    (when url
      (if (string-match "\\`cid:" url)
	  (when content-function
	    (let ((image (funcall content-function
				  (substring url (match-end 0)))))
	      (when image
		(goto-char start)
		(funcall shr-put-image-function
			 image (buffer-substring start end))
		(delete-region (point) end))))
        (url-retrieve url #'shr-image-fetched
		      (list (current-buffer) start end)
		      t t)))))

(defun shr-heading (dom &rest types)
  (shr-ensure-paragraph)
  (let ((start (point))
	(level (string-to-number
		(string-remove-prefix "shr-h" (symbol-name (car types))))))
   (apply #'shr-fontize-dom dom types)
   (put-text-property start (pos-eol) 'outline-level level))
  (shr-ensure-paragraph))

(defun shr-urlify (start url &optional title)
  (shr-add-font start (point) 'shr-link)
  (add-text-properties
   start (point)
   (list 'shr-url url
         'button t
         'category 'shr                ; For button.el button buffers.
	 'help-echo (let ((parsed (url-generic-parse-url
                                   (or (ignore-errors
				         (decode-coding-string
				          (url-unhex-string url)
				          'utf-8 t))
				       url)))
                          iri)
                      ;; If we have an IDNA domain, then show the
                      ;; decoded version in the mouseover to let the
                      ;; user know that there's something possibly
                      ;; fishy.
                      (when (url-host parsed)
                        (setf (url-host parsed)
                              (puny-encode-domain (url-host parsed))))
                      (setq iri (url-recreate-url parsed))
		      (if title
                          (format "%s (%s)" iri title)
                        iri))
	 'follow-link t
         ;; Make separate regions not `eq' so that they'll get
         ;; separate mouse highlights.
	 'mouse-face (list 'highlight)))
  (when (< start (point))
    (add-text-properties start (1+ start) '(shr-tab-stop t)))
  ;; Don't overwrite any keymaps that are already in the buffer (i.e.,
  ;; image keymaps).
  (while (and start
              (< start (point)))
    (let ((next (next-single-property-change start 'keymap nil (point))))
      (if (get-text-property start 'keymap)
          (setq start next)
        (put-text-property start (or next (point)) 'keymap shr-map)))))

(defun shr-encode-url (url)
  "Encode URL."
  (declare (obsolete nil "29.1"))
  (browse-url-url-encode-chars url "[)$ ]"))

(autoload 'shr-color-visible "shr-color")
(autoload 'shr-color->hexadecimal "shr-color")

(defun shr-color-check (fg bg)
  "Check that FG is visible on BG.
Returns (fg bg) with corrected values.
Returns nil if the colors that would be used are the default
ones, in case fg and bg are nil."
  (when (or fg bg)
    (let ((fixed (cond ((null fg) 'fg)
                       ((null bg) 'bg))))
      ;; Convert colors to hexadecimal, or set them to default.
      (let ((fg (or (shr-color->hexadecimal fg)
                    (frame-parameter nil 'foreground-color)))
            (bg (or (shr-color->hexadecimal bg)
                    (frame-parameter nil 'background-color))))
        (cond ((eq fixed 'bg)
               ;; Only return the new fg
               (list nil (cadr (shr-color-visible bg fg t))))
              ((eq fixed 'fg)
               ;; Invert args and results and return only the new bg
               (list (cadr (shr-color-visible fg bg t)) nil))
              (t
               (shr-color-visible bg fg)))))))

(defun shr-colorize-region (start end fg &optional bg)
  (when (and shr-use-colors
             (or fg bg)
             (>= (display-color-cells) 88))
    (let ((new-colors (shr-color-check fg bg)))
      (when new-colors
	(when fg
	  (add-face-text-property start end
				  (list :foreground (cadr new-colors))
				  t))
	(when bg
	  (add-face-text-property start end
				  (list :background (car new-colors) :extend t)
				  t)))
      new-colors)))

;;; Tag-specific rendering rules.

(defun shr-tag-html (dom)
  (let ((dir (dom-attr dom 'dir)))
    (cond
     ((equal dir "ltr")
      (setq bidi-paragraph-direction 'left-to-right))
     ((equal dir "rtl")
      (setq bidi-paragraph-direction 'right-to-left))
     ((equal dir "auto")
      (setq bidi-paragraph-direction nil))))
  (shr-generic dom))

(defun shr-tag-body (dom)
  (let* ((start (point))
	 (fgcolor (or (dom-attr dom 'fgcolor) (dom-attr dom 'text)))
	 (bgcolor (dom-attr dom 'bgcolor))
	 (shr-stylesheet (list (cons 'color fgcolor)
			       (cons 'background-color bgcolor))))
    (shr-generic dom)
    (shr-colorize-region start (point) fgcolor bgcolor)))

(defun shr-tag-style (_dom)
  )

(defun shr-tag-script (_dom)
  )

(defun shr-tag-comment (_dom)
  )

;; Introduced in HTML5.  For text browsers, functionally similar to a
;; comment.
(defun shr-tag-template (_dom)
  )

(defun shr-dom-to-xml (dom &optional charset)
  (with-temp-buffer
    (shr-dom-print dom)
    (when charset
      (encode-coding-region (point-min) (point-max) charset)
      (goto-char (point-min))
      (insert (format "<?xml version=\"1.0\" encoding=\"%s\"?>\n"
                      charset)))
    (buffer-string)))

(defun shr-dom-print (dom)
  "Convert DOM into a string containing the xml representation."
  (insert (format "<%s" (dom-tag dom)))
  (dolist (attr (dom-attributes dom))
    ;; Ignore attributes that start with a colon because they are
    ;; private elements.
    (unless (= (aref (format "%s" (car attr)) 0) ?:)
      (insert (format " %s=\"%s\"" (car attr) (cdr attr)))))
  (insert ">")
  (let (url)
    (dolist (elem (dom-children dom))
      (cond
       ((stringp elem)
	(insert elem))
       ((eq (dom-tag elem) 'comment)
	)
       ((or (not (eq (dom-tag elem) 'image))
	    ;; Filter out blocked elements inside the SVG image.
	    (not (setq url (dom-attr elem ':xlink:href)))
	    (not (shr-image-blocked-p url)))
	(insert " ")
	(shr-dom-print elem)))))
  (insert (format "</%s>" (dom-tag dom))))

(defconst shr-correct-attribute-case
  '((attributename . attributeName)
    (attributetype . attributeType)
    (basefrequency . baseFrequency)
    (baseprofile . baseProfile)
    (calcmode . calcMode)
    (clippathunits . clipPathUnits)
    (diffuseconstant . diffuseConstant)
    (edgemode . edgeMode)
    (filterunits . filterUnits)
    (glyphref . glyphRef)
    (gradienttransform . gradientTransform)
    (gradientunits . gradientUnits)
    (kernelmatrix . kernelMatrix)
    (kernelunitlength . kernelUnitLength)
    (keypoints . keyPoints)
    (keysplines . keySplines)
    (keytimes . keyTimes)
    (lengthadjust . lengthAdjust)
    (limitingconeangle . limitingConeAngle)
    (markerheight . markerHeight)
    (markerunits . markerUnits)
    (markerwidth . markerWidth)
    (maskcontentunits . maskContentUnits)
    (maskunits . maskUnits)
    (numoctaves . numOctaves)
    (pathlength . pathLength)
    (patterncontentunits . patternContentUnits)
    (patterntransform . patternTransform)
    (patternunits . patternUnits)
    (pointsatx . pointsAtX)
    (pointsaty . pointsAtY)
    (pointsatz . pointsAtZ)
    (preservealpha . preserveAlpha)
    (preserveaspectratio . preserveAspectRatio)
    (primitiveunits . primitiveUnits)
    (refx . refX)
    (refy . refY)
    (repeatcount . repeatCount)
    (repeatdur . repeatDur)
    (requiredextensions . requiredExtensions)
    (requiredfeatures . requiredFeatures)
    (specularconstant . specularConstant)
    (specularexponent . specularExponent)
    (spreadmethod . spreadMethod)
    (startoffset . startOffset)
    (stddeviation . stdDeviation)
    (stitchtiles . stitchTiles)
    (surfacescale . surfaceScale)
    (systemlanguage . systemLanguage)
    (tablevalues . tableValues)
    (targetx . targetX)
    (targety . targetY)
    (textlength . textLength)
    (viewbox . viewBox)
    (viewtarget . viewTarget)
    (xchannelselector . xChannelSelector)
    (ychannelselector . yChannelSelector)
    (zoomandpan . zoomAndPan))
  "Attributes for correcting the case in SVG and MathML.
Based on https://html.spec.whatwg.org/multipage/parsing.html#parsing-main-inforeign .")

(defun shr-correct-dom-case (dom)
  "Correct the case for SVG segments."
  (dolist (attr (dom-attributes dom))
    (when-let* ((rep (assoc-default (car attr) shr-correct-attribute-case)))
      (setcar attr rep)))
  (dolist (child (dom-children dom))
    (when (consp child)
      (shr-correct-dom-case child)))
  dom)

(defun shr-tag-svg (dom)
  (when (and (image-type-available-p 'svg)
	     (not shr-inhibit-images)
             (dom-attr dom 'width)
             (dom-attr dom 'height))
    (funcall shr-put-image-function
	     (list (shr-dom-to-xml (shr-correct-dom-case dom) 'utf-8)
                   'image/svg+xml)
	     "SVG Image")))

(defun shr-tag-sup (dom)
  (let ((start (point)))
    (shr-generic dom)
    (put-text-property start (point) 'display `(raise ,shr-sup-raise-factor))
    (add-face-text-property start (point) 'shr-sup)))

(defun shr-tag-sub (dom)
  ;; Why would a subscript be at the beginning of a line?  It does
  ;; happen sometimes because of a <br> tag and the intent seems to be
  ;; alignment of subscript and superscript but I don't think that is
  ;; possible in Emacs.  So we remove the newline in that case.
  (when (and (bolp) (not (bobp)))
    (forward-char -1)
    (delete-char 1))
  (let ((start (point)))
    (shr-generic dom)
    (put-text-property start (point) 'display `(raise ,shr-sub-raise-factor))
    (add-face-text-property start (point) 'shr-sup)))

(defun shr-tag-p (dom)
  (shr-ensure-paragraph)
  (shr-generic dom)
  (shr-ensure-paragraph))

(defun shr-tag-div (dom)
  (let ((display (cdr (assq 'display shr-stylesheet))))
    (if (or (equal display "inline")
            (equal display "inline-block"))
        (shr-generic dom)
      (shr-ensure-newline)
      (shr-generic dom)
      (shr-ensure-newline))))

(defun shr-tag-s (dom)
  (shr-fontize-dom dom 'shr-strike-through))

(defun shr-tag-b (dom)
  (shr-fontize-dom dom 'bold))

(defun shr-tag-i (dom)
  (shr-fontize-dom dom 'italic))

(defun shr-tag-em (dom)
  (shr-fontize-dom dom 'italic))

(defun shr-tag-strong (dom)
  (shr-fontize-dom dom 'bold))

(defun shr-tag-u (dom)
  (shr-fontize-dom dom 'underline))

(defun shr-tag-code (dom)
  (let ((shr-current-font 'shr-code))
    (shr-generic dom)))

(defun shr-tag-tt (dom)
  ;; The `tt' tag is deprecated in favor of `code'.
  (shr-tag-code dom))

(defun shr-tag-mark (dom)
  (when (and (not (bobp))
             (not (= (char-after (1- (point))) ?\s)))
    (insert " "))
  (let ((start (point)))
    (shr-generic dom)
    (shr-add-font start (point) 'shr-mark)))

(defun shr-tag-ins (cont)
  (let* ((start (point))
         (color "green")
         (shr-stylesheet (nconc (list (cons 'color color))
				shr-stylesheet)))
    (shr-generic cont)
    (shr-colorize-region start (point) color
                         (cdr (assq 'background-color shr-stylesheet)))))

(defun shr-tag-del (cont)
  (let* ((start (point))
         (color "red")
         (shr-stylesheet (nconc (list (cons 'color color))
				shr-stylesheet)))
    (shr-fontize-dom cont 'shr-strike-through)
    (shr-colorize-region start (point) color
                         (cdr (assq 'background-color shr-stylesheet)))))

(defun shr-parse-style (style)
  (when style
    (setq style (replace-regexp-in-string "\n" " " style))
    (let ((plist nil))
      (dolist (elem (split-string style ";"))
	(when elem
	  (setq elem (split-string elem ":"))
	  (when (and (car elem)
		     (cadr elem))
	    (let ((name (replace-regexp-in-string "^ +\\| +$" "" (car elem)))
		  (value (replace-regexp-in-string "^ +\\| +$" "" (cadr elem))))
	      (when (string-match " *!important\\'" value)
		(setq value (substring value 0 (match-beginning 0))))
	      (unless (equal value "inherit")
		(push (cons (intern name obarray)
			    value)
		      plist))))))
      plist)))

(defun shr-tag-base (dom)
  (let ((base (dom-attr dom 'href)))
    (when (> (length base) 0)
      (setq shr-base (shr-parse-base base))))
  (shr-generic dom))

(defun shr-tag-a (dom)
  (let ((url (dom-attr dom 'href))
        (title (dom-attr dom 'title))
	(start (point))
	shr-start)
    (shr-generic dom)
    (when-let* ((id (and (not (dom-attr dom 'id)) ; Handled by `shr-descend'.
                         (dom-attr dom 'name)))) ; Obsolete since HTML5.
      (push (cons id (set-marker (make-marker) start)) shr--link-targets))
    (when url
      (shr-urlify (or shr-start start)
                  (funcall shr-url-transformer (shr-expand-url url))
                  title)
      ;; Check whether the URL is suspicious.
      (when-let* ((warning (or (textsec-suspicious-p
                                (shr-expand-url url) 'url)
                               (textsec-suspicious-p
                                (cons (shr-expand-url url)
                                      (buffer-substring (or shr-start start)
                                                        (point)))
                                'link))))
        (add-text-properties (or shr-start start) (point)
                             (list 'face '(shr-link textsec-suspicious)))
        (insert (propertize "⚠️" 'help-echo warning))))))

(defun shr-tag-abbr (dom)
  (let ((title (dom-attr dom 'title))
	(start (point)))
    (shr-generic dom)
    (shr-add-font start (point) 'shr-abbreviation)
    (when title
      (add-text-properties start (point)
                           (list 'help-echo title
                                 'mouse-face 'highlight)))))

(defun shr-tag-acronym (dom)
  ;; `acronym' is deprecated in favor of `abbr'.
  (shr-tag-abbr dom))

(defun shr-tag-object (dom)
  (unless shr-inhibit-images
    (let ((start (point))
	  url multimedia image)
      (when-let* ((type (dom-attr dom 'type)))
	(when (string-match-p "\\`image/svg" type)
	  (setq url (dom-attr dom 'data)
		image t)))
      (dolist (child (dom-non-text-children dom))
	(cond
	 ((eq (dom-tag child) 'embed)
	  (setq url (or url (dom-attr child 'src))
		multimedia t))
	 ((and (eq (dom-tag child) 'param)
	       (equal (dom-attr child 'name) "movie"))
	  (setq url (or url (dom-attr child 'value))
		multimedia t))))
      (when url
	(cond
	 (image
	  (shr-indirect-call 'img dom url)
	  (setq dom nil))
	 (multimedia
	  (shr-insert " [multimedia] ")
	  (shr-urlify start (shr-expand-url url)))))
      (when dom
	(shr-generic dom)))))

(defcustom shr-prefer-media-type-alist '(("webm" . 1.0)
                                         ("ogv"  . 1.0)
                                         ("ogg"  . 1.0)
                                         ("opus" . 1.0)
                                         ("flac" . 0.9)
                                         ("wav"  . 0.5))
  "Preferences for media types.
The key element should be a regexp matched against the type of the source or
url if no type is specified.  The value should be a float in the range 0.0 to
1.0.  Media elements with higher value are preferred."
  :version "24.4"
  :type '(alist :key-type regexp :value-type float))

(defcustom shr-use-xwidgets-for-media nil
  "If non-nil, use xwidgets to display video and audio elements.
This also depends on Emacs being built with xwidgets capability.
Note that this is experimental, and may lead to instability on
some platforms."
  :type 'boolean
  :version "29.1")

(defun shr--get-media-pref (elem)
  "Determine the preference for ELEM.
The preference is a float determined from `shr-prefer-media-type'."
  (let ((type (dom-attr elem 'type))
        (p 0.0))
    (unless type
      (setq type (dom-attr elem 'src)))
    (when type
      (dolist (pref shr-prefer-media-type-alist)
        (when (and
               (> (cdr pref) p)
               (string-match-p (car pref) type))
          (setq p (cdr pref)))))
    p))

(defun shr--extract-best-source (dom &optional url pref)
  "Extract the best `:src' property from <source> blocks in DOM."
  (setq pref (or pref -1.0))
  (let (new-pref)
    (dolist (elem (dom-non-text-children dom))
      (when (and (eq (dom-tag elem) 'source)
		 (< pref
		    (setq new-pref
			  (shr--get-media-pref elem))))
	(setq pref new-pref
	      url (dom-attr elem 'src))
        ;; libxml's html parser isn't HTML5 compliant and non terminated
        ;; source tags might end up as children.  So recursion it is...
        (dolist (child (dom-non-text-children elem))
          (when (eq (dom-tag child) 'source)
            (let ((ret (shr--extract-best-source (list child) url pref)))
              (when (< pref (cdr ret))
                (setq url (car ret)
                      pref (cdr ret)))))))))
  (cons url pref))

(declare-function xwidget-webkit-execute-script "xwidget.c"
                  (xwidget script &optional callback))

(defun shr-tag-video (dom)
  (let ((image (dom-attr dom 'poster))
        (url (dom-attr dom 'src))
        (start (point)))
    (unless url
      (setq url (car (shr--extract-best-source dom))))
    (if (and shr-use-xwidgets-for-media
             (fboundp 'make-xwidget))
        ;; Play the video.
        (progn
          (require 'xwidget)
          (let ((widget (make-xwidget
                         'webkit
			 "Video"
                         (truncate (* (window-pixel-width) 0.8))
                         (truncate (* (window-pixel-width) 0.8 0.75)))))
            (insert
             (propertize
              " [video] "
              'display (list 'xwidget :xwidget widget)))
            (xwidget-webkit-execute-script
             widget (format "document.body.innerHTML = %S;"
                            (format
                             "<style>body { margin: 0px; }</style><div style='background: black; height: 100%%; display: flex; align-items: center; justify-content: center;'><video autoplay loop muted controls style='max-width: 100%%; max-height: 100%%;'><source src=%S type='video/mp4'></source></video></div>"
                             url)))))
      ;; No xwidgets.
      (if (> (length image) 0)
	  (shr-indirect-call 'img nil image)
        (shr-insert " [video] "))
      (shr-urlify start (shr-expand-url url)))))

(defun shr-tag-audio (dom)
  (let ((url (dom-attr dom 'src))
        (start (point)))
    (unless url
      (setq url (car (shr--extract-best-source dom))))
    (shr-insert " [audio] ")
    (shr-urlify start (shr-expand-url url))))

(defun shr-tag-img (dom &optional url)
  (when (or url
	    (and dom
		 (or (> (length (dom-attr dom 'src)) 0)
                     (> (length (dom-attr dom 'srcset)) 0))))
    (when (and (not shr-max-inline-image-size)
	       (> (current-column) 0))
      (insert "\n"))
    (let ((alt (dom-attr dom 'alt))
          (width (shr-string-number (dom-attr dom 'width)))
          (height (shr-string-number (dom-attr dom 'height)))
	  (url (shr-expand-url (or url (shr--preferred-image dom)))))
      (let ((start (point-marker)))
	(when (zerop (length alt))
	  (setq alt "*"))
	(cond
         ((null url)
          ;; After further expansion, there turned out to be no valid
          ;; src in the img after all.
          )
	 ((or (member (dom-attr dom 'height) '("0" "1"))
	      (member (dom-attr dom 'width) '("0" "1")))
	  ;; Ignore zero-sized or single-pixel images.
	  )
	 ((and (not shr-inhibit-images)
	       (string-match "\\`data:" url))
	  (let ((image (shr-image-from-data (substring url (match-end 0)))))
	    (if image
		(funcall shr-put-image-function image alt
                         (list :width width :height height))
	      (insert alt))))
	 ((and (not shr-inhibit-images)
	       (string-match "\\`cid:" url))
	  (let ((url (substring url (match-end 0)))
		image)
	    (if (or (not shr-content-function)
		    (not (setq image (funcall shr-content-function url))))
		(insert alt)
	      (funcall shr-put-image-function image alt
                       (list :width width :height height)))))
	 ((or shr-inhibit-images
	      (shr-image-blocked-p url))
	  (setq shr-start (point))
          (shr-insert alt))
	 ((and (not shr-ignore-cache)
	       (url-is-cached url))
	  (funcall shr-put-image-function (shr-get-image-data url) alt
                   (list :width width :height height)))
	 (t
	  (when (and shr-ignore-cache
		     (url-is-cached url))
	    (let ((file (url-cache-create-filename url)))
	      (when (file-exists-p file)
		(delete-file file))))
          (if (image-type-available-p 'svg)
              (insert-image
               (shr-make-placeholder-image dom)
               (or (string-trim alt) ""))
            ;; No SVG support.  Just use a space as our placeholder.
            (insert " "))
	  (url-queue-retrieve
           url #'shr-image-fetched
	   (list (current-buffer) start (set-marker (make-marker) (point))
                 (list :width width :height height))
	   t
           (not (shr--use-cookies-p url shr-base)))))
	(when (zerop shr-table-depth) ;; We are not in a table.
	  (put-text-property start (point) 'keymap shr-image-map)
	  (put-text-property start (point) 'shr-alt alt)
	  (put-text-property start (point) 'image-url url)
	  (put-text-property start (point) 'image-dom-size (cons width height))
	  (put-text-property start (point) 'image-displayer
			     (shr-image-displayer shr-content-function))
	  (put-text-property start (point) 'help-echo
			     (shr-fill-text
			      (or (dom-attr dom 'title) alt))))))))

(defun shr--use-cookies-p (url base)
  "Say whether to use cookies when fetching URL (typically an image).
BASE is the URL of the HTML being rendered."
  (cond
   ((null base)
    ;; Disallow cookies if we don't know what the base is.
    nil)
   ((eq shr-cookie-policy 'same-origin)
    (let ((url-host (url-host (url-generic-parse-url url)))
          (base-host (split-string
                      (url-host (url-generic-parse-url (car base)))
                      "\\.")))
      ;; We allow cookies if it's for any of the sibling domains (that
      ;; we're allowed to set cookies for).  Determine that by going
      ;; "upwards" in the base domain name.
      (cl-loop while base-host
               when (url-cookie-host-can-set-p
                     url-host (mapconcat #'identity base-host "."))
               return t
               do (pop base-host)
               finally (return nil))))
   (t
    shr-cookie-policy)))

(defun shr--preferred-image (dom)
  (let* ((srcset (and (dom-attr dom 'srcset)
                      (shr--parse-srcset (dom-attr dom 'srcset)
                                         (and (dom-attr dom 'width)
                                              (string-to-number
                                               (dom-attr dom 'width))))))
         (frame-width (frame-pixel-width))
         candidate)
    (when srcset
      ;; Choose the smallest picture that's bigger than the current
      ;; frame.
      (setq candidate (caar srcset))
      (while (and srcset
                  (> (cadr (car srcset)) frame-width))
        (setq candidate (caar srcset))
        (pop srcset)))
    (or candidate (dom-attr dom 'src))))

(defun shr--parse-srcset (srcset &optional width)
  (setq srcset (string-trim srcset)
        width (or width 100))
  (when (> (length srcset) 0)
    ;; srcset consists of a series of URL/size specifications separated
    ;; by the " ," string.
    (sort (mapcar
           (lambda (elem)
             (let ((spec (split-string elem "[\t\n\r ]+")))
               (cond
                ((= (length spec) 1)
                 ;; Make sure it's well formed.
                 (list (car spec) 0))
                ((string-match "\\([0-9]+\\)x\\'" (cadr spec))
                 ;; If we have an "x" form, then use the width
                 ;; spec to compute the real width.
                 (list (car spec)
                       (* width (string-to-number
                                 (match-string 1 (cadr spec))))))
                (t
                 (list (car spec)
                       (string-to-number (cadr spec)))))))
           (with-temp-buffer
             (insert srcset)
             (goto-char (point-min))
             (let ((bits nil))
               (while (re-search-forward "[^\t\n\r ]+[\t\n\r ]+[^\t\n\r ,]+"
                                         nil t)
                 (push (match-string 0) bits)
                 (if (looking-at "[\t\n\r ]*,[\t\n\r ]*")
                     (goto-char (match-end 0))
                   (goto-char (point-max))))
               bits)))
          (lambda (e1 e2)
            (> (cadr e1) (cadr e2))))))

(defun shr-string-number (string)
  (if (null string)
      nil
    (setq string (replace-regexp-in-string "[^0-9]" "" string))
    (if (zerop (length string))
        nil
      (string-to-number string))))

(defun shr-make-placeholder-image (dom)
  (let* ((edges (and
                 (get-buffer-window (current-buffer))
                 (window-inside-pixel-edges
                  (get-buffer-window (current-buffer)))))
         (scaling (image-compute-scaling-factor image-scaling-factor))
         (width (truncate
                 (* (or (shr-string-number (dom-attr dom 'width)) 100)
                    scaling)))
         (height (truncate
                  (* (or (shr-string-number (dom-attr dom 'height)) 100)
                     scaling)))
         (max-width
          (and edges
               (truncate (* shr-max-image-proportion
                            (- (nth 2 edges) (nth 0 edges))))))
         (max-height (and edges
                          (truncate (* shr-max-image-proportion
                                       (- (nth 3 edges) (nth 1 edges))))))
         svg)
    (when (and max-width
               (> width max-width))
      (setq height (truncate (* (/ (float max-width) width) height))
            width max-width))
    (when (and max-height
               (> height max-height))
      (setq width (truncate (* (/ (float max-height) height) width))
            height max-height))
    (setq svg (svg-create width height))
    (svg-gradient svg "background" 'linear '((0 . "#b0b0b0") (100 . "#808080")))
    (svg-rectangle svg 0 0 width height :gradient "background"
                   :stroke-width 2 :stroke-color "black")
    (let ((image (svg-image svg :scale 1)))
      (setf (image-property image :ascent) shr-image-ascent)
      image)))

(defun shr-tag-pre (dom)
  (let ((shr-folding-mode 'none)
	(shr-current-font 'default))
    (shr-ensure-newline)
    (shr-generic dom)
    (shr-ensure-newline)))

(defun shr-tag-blockquote (dom)
  (shr-ensure-paragraph)
  (let ((start (point))
	(shr-indentation (+ shr-indentation
			    (* 4 shr-table-separator-pixel-width))))
    (shr-generic dom)
    (shr-ensure-paragraph)
    (shr-mark-fill start)))

(defun shr-tag-dl (dom)
  (shr-ensure-paragraph)
  (shr-generic dom)
  (shr-ensure-paragraph))

(defun shr-tag-dt (dom)
  (shr-ensure-newline)
  (shr-generic dom)
  (shr-ensure-newline))

(defun shr-tag-dd (dom)
  (shr-ensure-newline)
  (let ((shr-indentation (+ shr-indentation
			    (* 4 shr-table-separator-pixel-width))))
    (shr-generic dom)))

(defun shr-tag-ul (dom)
  (shr-ensure-paragraph)
  (let ((shr-list-mode 'ul))
    (shr-generic dom))
  ;; If we end on an empty <li>, then make sure we really end on a new
  ;; paragraph.
  (unless (bolp)
    (insert "\n"))
  (shr-ensure-paragraph))

(defun shr-tag-ol (dom)
  (shr-ensure-paragraph)
  (let* ((attrs (dom-attributes dom))
         (start-attr (alist-get 'start attrs))
         ;; Start at 1 if there is no start attribute
         ;; or if start can't be parsed as an integer.
         (start-index (condition-case _
                          (cl-parse-integer start-attr)
                        (t 1)))
         (shr-list-mode start-index))
    (shr-generic dom))
  (shr-ensure-paragraph))

(defun shr-tag-li (dom)
  (shr-ensure-newline)
  (let ((start (point)))
    (let* ((bullet
	    (if (numberp shr-list-mode)
		(prog1
		    (format "%d " shr-list-mode)
		  (setq shr-list-mode (1+ shr-list-mode)))
	      (car shr-internal-bullet)))
	   (width (if (numberp shr-list-mode)
		      (shr-string-pixel-width bullet)
		    (cdr shr-internal-bullet))))
      (insert bullet)
      (shr-mark-fill start)
      (let ((shr-indentation (+ shr-indentation width)))
	(put-text-property start (1+ start)
			   'shr-continuation-indentation shr-indentation)
	(put-text-property start (1+ start) 'shr-prefix-length (length bullet))
	(shr-generic dom))))
  (unless (bolp)
    (insert "\n")))

(defun shr-mark-fill (start)
  ;; We may not have inserted any text to fill.
  (when (and (/= start (point))
             ;; Tables insert themselves with the correct indentation,
             ;; so don't do anything if we're at the start of a table.
             (not (get-text-property start 'shr-table-id)))
    (put-text-property start (1+ start)
		       'shr-indentation shr-indentation)))

(defun shr-tag-br (dom)
  (when (and (not (bobp))
	     ;; Only add a newline if we break the current line, or
	     ;; the previous line isn't a blank line.
	     (or (not (bolp))
		 (and (> (- (point) 2) (point-min))
		      (not (= (char-after (- (point) 2)) ?\n)))))
    (insert "\n"))
  (shr-generic dom))

(defun shr-tag-span (dom)
  (shr-generic dom))

(defun shr-tag-h1 (dom)
  (shr-heading dom 'shr-h1))

(defun shr-tag-h2 (dom)
  (shr-heading dom 'shr-h2))

(defun shr-tag-h3 (dom)
  (shr-heading dom 'shr-h3))

(defun shr-tag-h4 (dom)
  (shr-heading dom 'shr-h4))

(defun shr-tag-h5 (dom)
  (shr-heading dom 'shr-h5))

(defun shr-tag-h6 (dom)
  (shr-heading dom 'shr-h6))

(defun shr-tag-hr (_dom)
  (shr-ensure-newline)
  (insert (make-string (if (not shr-use-fonts)
			   shr-internal-width
			 (1+ (/ shr-internal-width
				shr-table-separator-pixel-width)))
		       shr-hr-line)
	  "\n"))

(defun shr-tag-title (dom)
  (shr-heading dom 'bold 'underline))

(defun shr-tag-font (dom)
  (let* ((start (point))
         (color (dom-attr dom 'color))
         (shr-stylesheet (nconc (list (cons 'color color))
				shr-stylesheet)))
    (shr-generic dom)
    (when color
      (shr-colorize-region start (point) color
			   (cdr (assq 'background-color shr-stylesheet))))))

(defun shr-tag-bdo (dom)
  (let* ((direction (dom-attr dom 'dir))
         (char (cond
                ((equal direction "ltr")
                 ?\N{LEFT-TO-RIGHT OVERRIDE})
                ((equal direction "rtl")
                 ?\N{RIGHT-TO-LEFT OVERRIDE}))))
    (when char
      (insert ?\N{FIRST STRONG ISOLATE} char))
    (shr-generic dom)
    (when char
      (insert ?\N{POP DIRECTIONAL FORMATTING} ?\N{POP DIRECTIONAL ISOLATE}))))

(defun shr-tag-bdi (dom)
  (insert ?\N{FIRST STRONG ISOLATE})
  (shr-generic dom)
  (insert ?\N{POP DIRECTIONAL ISOLATE}))

(defun shr-tag-math (dom)
  ;; Sometimes a math element contains a plain text annotation
  ;; (typically TeX notation) in addition to MathML markup.  If we pass
  ;; that to `dom-generic', the formula is printed twice.  So we select
  ;; only the annotation if available.
  (shr-generic
   (thread-first
     dom
     (dom-child-by-tag 'semantics)
     (dom-child-by-tag 'annotation)
     (or dom))))

;;; Outline Support
(defun shr-outline-search (&optional bound move backward looking-at)
  "A function that can be used as `outline-search-function' for rendered html.
See `outline-search-function' for BOUND, MOVE, BACKWARD and LOOKING-AT."
  (if looking-at
      (get-text-property (point) 'outline-level)
    (let ((heading-found nil)
	  (bound (or bound
		     (if backward (point-min) (point-max)))))
      (save-excursion
	(when (and (not (bolp))
		   (get-text-property (point) 'outline-level))
	  (forward-line (if backward -1 1)))
	(if backward
	    (unless (get-text-property (point) 'outline-level)
	      (goto-char (or (previous-single-property-change
			      (point) 'outline-level nil bound)
			     bound)))
	  (goto-char (or (text-property-not-all (point) bound 'outline-level nil)
			 bound)))
	(goto-char (pos-bol))
	(when (get-text-property (point) 'outline-level)
	  (setq heading-found (point))))
      (if heading-found
	  (progn
	    (set-match-data (list heading-found heading-found))
	    (goto-char heading-found))
	(when move
	  (goto-char bound)
	  nil)))))

(defun shr-outline-level ()
  "Function to be used as `outline-level' with `shr-outline-search'."
  (get-text-property (point) 'outline-level))

;;; Table rendering algorithm.

;; Table rendering is the only complicated thing here.  We do this by
;; first counting how many TDs there are in each TR, and registering
;; how wide they think they should be ("width=45%", etc).  Then we
;; render each TD separately (this is done in temporary buffers, so
;; that we can use all the rendering machinery as if we were in the
;; main buffer).  Now we know how much space each TD really takes, so
;; we then render everything again with the new widths, and finally
;; insert all these boxes into the main buffer.
(defun shr-tag-table-1 (dom)
  (setq dom (or (dom-child-by-tag dom 'tbody) dom))
  (let* ((shr-inhibit-images t)
	 (shr-table-depth (1+ shr-table-depth))
         ;; Fill hard in CJK languages.
	 (pixel-fill-respect-kinsoku nil)
	 ;; Find all suggested widths.
	 (columns (shr-column-specs dom))
	 ;; Compute how many pixels wide each TD should be.
	 (suggested-widths (shr-pro-rate-columns columns))
	 ;; Do a "test rendering" to see how big each TD is (this can
	 ;; be smaller (if there's little text) or bigger (if there's
	 ;; unbreakable text).
	 (elems (or (dom-attr dom 'shr-suggested-widths)
		    (shr-make-table dom suggested-widths nil
				    'shr-suggested-widths)))
	 (sketch (cl-loop for line in elems
		          collect (mapcar #'car line)))
	 (natural (cl-loop for line in elems
			   collect (mapcar #'cdr line)))
	 (sketch-widths (shr-table-widths sketch natural suggested-widths)))
    ;; This probably won't work very well.
    (when (> (+ (cl-loop for width across sketch-widths
		         summing (1+ width))
		shr-indentation shr-table-separator-pixel-width)
	     (frame-width))
      (setq truncate-lines t))
    ;; Then render the table again with these new "hard" widths.
    (shr-insert-table (shr-make-table dom sketch-widths t) sketch-widths)))

(defun shr-table-body (dom)
  (let ((tbodies (seq-filter (lambda (child)
                               (eq (dom-tag child) 'tbody))
                             (dom-non-text-children dom))))
    (cond
     ((null tbodies)
      dom)
     ((null (cdr tbodies))
      (car tbodies))
     (t
      ;; Table with multiple tbodies.  Convert into a single tbody.
      `(tbody nil ,@(mapcan #'dom-non-text-children tbodies))))))

(defun shr--fix-tbody (tbody)
  (nconc (list 'tbody (dom-attributes tbody))
         (cl-loop for child in (dom-children tbody)
		  for tag = (and (not (stringp child)) (dom-tag child))
		  unless (or (eq tag 'thead) (eq tag 'tfoot))
		  collect (if (not (eq tag 'tr))
                              (list 'tr nil (list 'td nil child))
                            child))))

(defun shr--fix-table (dom caption header footer)
  (let* ((body (dom-non-text-children (shr--fix-tbody (shr-table-body dom))))
         (nheader (if header (shr-max-columns header)))
	 (nbody (if body (shr-max-columns body) 0))
         (nfooter (if footer (shr-max-columns footer))))
    (nconc
     (list 'table nil)
     (if caption `((tr nil (td nil ,@caption))))
     (cond
      (header
       (if footer
	   ;; header + body + footer
	   (if (= nheader nbody)
	       (if (= nbody nfooter)
		   `((tr nil (td nil (table nil
					    (tbody nil ,@header
						   ,@body ,@footer)))))
	         (nconc `((tr nil (td nil (table nil
					         (tbody nil ,@header
						        ,@body)))))
		        (if (= nfooter 1)
			    footer
			  `((tr nil (td nil (table
					     nil (tbody
						  nil ,@footer))))))))
	     (nconc `((tr nil (td nil (table nil (tbody
						  nil ,@header)))))
		    (if (= nbody nfooter)
		        `((tr nil (td nil (table
					   nil (tbody nil ,@body
						      ,@footer)))))
		      (nconc `((tr nil (td nil (table
					        nil (tbody nil
							   ,@body)))))
			     (if (= nfooter 1)
			         footer
			       `((tr nil (td nil (table
						  nil
						  (tbody
						   nil
						   ,@footer))))))))))
         ;; header + body
         (if (= nheader nbody)
	     `((tr nil (td nil (table nil (tbody nil ,@header
					         ,@body)))))
	   (if (= nheader 1)
	       `(,@header (tr nil (td nil (table
					   nil (tbody nil ,@body)))))
	     `((tr nil (td nil (table nil (tbody nil ,@header))))
	       (tr nil (td nil (table nil (tbody nil ,@body)))))))))
      (footer
       ;; body + footer
       (if (= nbody nfooter)
	   `((tr nil (td nil (table
			      nil (tbody nil ,@body ,@footer)))))
         (nconc `((tr nil (td nil (table nil (tbody nil ,@body)))))
	        (if (= nfooter 1)
		    footer
		  `((tr nil (td nil (table
				     nil (tbody nil ,@footer)))))))))
      (caption
       `((tr nil (td nil (table nil (tbody nil ,@body))))))
      (body)))))

(defun shr-tag-table (dom)
  (shr-ensure-paragraph)
  (let* ((caption (dom-children (dom-child-by-tag dom 'caption)))
	 (header (dom-non-text-children (dom-child-by-tag dom 'thead)))
	 (footer (dom-non-text-children (dom-child-by-tag dom 'tfoot))))
    (if (and (not caption)
	     (not header)
	     (not (dom-child-by-tag dom 'tbody))
	     (not (dom-child-by-tag dom 'tr))
	     (not footer))
	;; The table is totally invalid and just contains random junk.
	;; Try to output it anyway.
	(shr-generic dom)
      ;; It's a real table, so render it.
      (if (dom-attr dom 'shr-fixed-table)
	  (shr-tag-table-1 dom)
	;; Only fix up the table once.
	(let ((table (shr--fix-table dom caption header footer)))
	  (dom-set-attribute table 'shr-fixed-table t)
	  (setcdr dom (cdr table))
	  (shr-tag-table-1 dom)))
      (let* ((bgcolor (dom-attr dom 'bgcolor))
	     (start (point))
	     (shr-stylesheet (nconc (list (cons 'background-color bgcolor))
				    shr-stylesheet)))
        (when bgcolor
          (shr-colorize-region start (point) (cdr (assq 'color shr-stylesheet))
			       bgcolor))
        ;; Finally, insert all the images after the table.  The Emacs buffer
        ;; model isn't strong enough to allow us to put the images actually
        ;; into the tables.  It inserts also non-td/th objects.
        (when (zerop shr-table-depth)
          (save-excursion
	    (shr-expand-alignments start (point)))
          (let ((strings (shr-collect-extra-strings-in-table dom)))
	    (when strings
	      (save-restriction
	        (narrow-to-region (point) (point))
	        (insert (mapconcat #'identity strings "\n"))
	        (shr-fill-lines (point-min) (point-max))))))))))

(defun shr-collect-extra-strings-in-table (dom &optional flags)
  "Return extra strings in DOM of which the root is a table clause.
Render <img>s and <object>s, and strings and child <table>s of which
the parent <td> or <th> is lacking.  FLAGS is a cons of two boolean
flags that control whether to collect or render objects."
  ;; This function runs recursively and collects strings if the cdr of
  ;; FLAGS is nil and the car is not nil, and it renders also child
  ;; <table>s if the cdr is nil.  Note: FLAGS may be nil, not a cons.
  ;; FLAGS becomes (t . nil) if a <tr> clause is found in the children
  ;; of DOM, and becomes (t . t) if a <td> or a <th> clause is found
  ;; and the car is t then.  When a <table> clause is found, FLAGS
  ;; becomes nil if the cdr is t then.  But if FLAGS is (t . nil) then,
  ;; it renders the <table>.
  (cl-loop for child in (dom-children dom) with recurse with tag
	   do (setq recurse nil)
	   if (stringp child)
	     unless (or (not (car flags)) (cdr flags))
	       when (string-match "\\(?:[^\t\n\r ]+[\t\n\r ]+\\)*[^\t\n\r ]+"
				  child)
		 collect (match-string 0 child)
	       end end
	   else if (consp child)
	     do (setq tag (dom-tag child)) and
	     unless (memq tag '(comment style))
	       if (eq tag 'img)
		 do (shr-indirect-call 'img child)
	       else if (eq tag 'object)
		 do (shr-indirect-call 'object child)
	       else
		 do (setq recurse t) and
		 if (eq tag 'tr)
		   do (setq flags '(t . nil))
		 else if (memq tag '(td th))
		   when (car flags)
		     do (setq flags '(t . t))
		   end
		 else if (eq tag 'table)
		   if (cdr flags)
		     do (setq flags nil)
		   else if (car flags)
		     do (setq recurse nil)
			(shr-indirect-call 'table child)
		   end end end end end end end end end end
	   when recurse
	     append (shr-collect-extra-strings-in-table child flags)))

(defun shr-insert-table (table widths)
  (let* ((collapse (equal (cdr (assq 'border-collapse shr-stylesheet))
			  "collapse"))
	 (shr-table-separator-length (if collapse 0 1))
	 (shr-table-vertical-line (if collapse "" shr-table-vertical-line))
	 (start (point)))
    (setq shr-table-id (1+ shr-table-id))
    (unless collapse
      (shr-insert-table-ruler widths))
    (dolist (row table)
      (let ((start (point))
	    (align 0)
	    (column-number 0)
	    (height (let ((max 0))
		      (dolist (column row)
			(setq max (max max (nth 2 column))))
		      max)))
	(dotimes (_ (max height 1))
          (when (bolp)
	    (shr-indent))
	  (insert shr-table-vertical-line "\n"))
	(dolist (column row)
	  (when (> (nth 2 column) -1)
	    (goto-char start)
	    ;; Sum up all the widths from the column.  (There may be
	    ;; more than one if this is a "colspan" column.)
	    (dotimes (_ (nth 4 column))
	      ;; The colspan directive may be wrong and there may not be
	      ;; that number of columns.
	      (when (<= column-number (1- (length widths)))
		(setq align (+ align
			       (aref widths column-number)
			       (* 2 shr-table-separator-pixel-width))))
	      (setq column-number (1+ column-number)))
	    (let ((lines (nth 3 column))
		  (pixel-align (if (not shr-use-fonts)
				   (* align (frame-char-width))
				 align)))
	      (dolist (line lines)
		(end-of-line)
		(let ((start (point))
                      (background (and (> (length line) 0)
                                       (shr-face-background
                                        (get-text-property
                                         (1- (length line)) 'face line))))
                      (space (propertize
                              " "
                              'display `(space :align-to (,pixel-align))
                              'shr-table-indent shr-table-id)))
                  (when background
                    (setq space (propertize space 'face background)))
		  (insert line space shr-table-vertical-line)
		  (shr-colorize-region
		   start (1- (point)) (nth 5 column) (nth 6 column)))
		(forward-line 1))
	      ;; Add blank lines at padding at the bottom of the TD,
	      ;; possibly.
	      (dotimes (_ (- height (length lines)))
		(end-of-line)
		(let ((start (point)))
		  (insert (propertize " "
				      'display `(space :align-to (,pixel-align))
				      'shr-table-indent shr-table-id)
			  shr-table-vertical-line)
		  (shr-colorize-region
		   start (1- (point)) (nth 5 column) (nth 6 column)))
		(forward-line 1))))))
      (unless collapse
	(shr-insert-table-ruler widths)))
    (unless (= start (point))
      (put-text-property start (1+ start) 'shr-table-id shr-table-id))))

(defun shr-face-background (face)
  (and (consp face)
       (or (and (plist-get face :background)
                (list :background (plist-get face :background)))
           (let ((background nil))
             (dolist (elem face)
               (when (and (consp elem)
                          (eq (car elem) :background)
                          (not background))
                 (setq background (cadr elem))))
             (and background
                  (list :background background :extend t))))))

(defun shr-expand-alignments (start end)
  (while (< (setq start (next-single-property-change
			 start 'shr-table-id nil end))
	    end)
    (goto-char start)
    (let* ((shr-use-fonts t)
	   (id (get-text-property (point) 'shr-table-id))
	   (base (shr-pixel-column))
	   elem)
      (when id
	(save-excursion
	  (while (setq elem (text-property-any
			     (point) end 'shr-table-indent id))
	    (goto-char elem)
	    (let ((align (get-text-property (point) 'display)))
	      (put-text-property (point) (1+ (point)) 'display
				 `(space :align-to (,(+ (car (nth 2 align))
							base)))))
	    (forward-char 1)))))
    (setq start (1+ start))))

(defun shr-insert-table-ruler (widths)
  (when shr-table-horizontal-line
    (when (and (bolp)
	       (> shr-indentation 0))
      (shr-indent))
    (insert shr-table-corner)
    (let ((total-width 0))
      (dotimes (i (length widths))
	(setq total-width (+ total-width (aref widths i)
			     (* shr-table-separator-pixel-width 2)))
	(insert (make-string (1+ (/ (aref widths i)
				    shr-table-separator-pixel-width))
			     shr-table-horizontal-line)
		(propertize " "
			    'display `(space :align-to (,total-width))
			    'shr-table-indent shr-table-id)
		shr-table-corner)))
    (insert "\n")))

(defun shr-table-widths (table natural-table suggested-widths)
  (let* ((length (length suggested-widths))
	 (widths (make-vector length 0))
	 (natural-widths (make-vector length 0)))
    (dolist (row table)
      (let ((i 0))
	(dolist (column row)
	  (aset widths i (max (aref widths i) column))
	  (setq i (1+ i)))))
    (dolist (row natural-table)
      (let ((i 0))
	(dolist (column row)
	  (aset natural-widths i (max (aref natural-widths i) column))
	  (setq i (1+ i)))))
    (let ((extra (- (apply #'+ (append suggested-widths nil))
                    (apply #'+ (append widths nil))
		    (* shr-table-separator-pixel-width (1+ (length widths)))))
	  (expanded-columns 0))
      ;; We have extra, unused space, so divide this space amongst the
      ;; columns.
      (when (> extra 0)
	;; If the natural width is wider than the rendered width, we
	;; want to allow the column to expand.
	(dotimes (i length)
	  (when (> (aref natural-widths i) (aref widths i))
	    (setq expanded-columns (1+ expanded-columns))))
	(dotimes (i length)
	  (when (> (aref natural-widths i) (aref widths i))
	    (aset widths i (min
			    (aref natural-widths i)
			    (+ (/ extra expanded-columns)
			       (aref widths i))))))))
    widths))

(defun shr-make-table (dom widths &optional fill storage-attribute)
  (or (cadr (assoc (list dom widths fill) shr-content-cache))
      (let ((data (shr-make-table-1 dom widths fill)))
	(push (list (list dom widths fill) data)
	      shr-content-cache)
	(when storage-attribute
	  (dom-set-attribute dom storage-attribute data))
	data)))

(defun shr-make-table-1 (dom widths &optional fill)
  (let ((trs nil)
	(rowspans (make-vector (length widths) 0))
	(colspan-remaining 0)
	colspan-width colspan-count
	width colspan)
    (dolist (row (dom-non-text-children dom))
      (when (eq (dom-tag row) 'tr)
	(let ((tds nil)
	      (columns (dom-non-text-children row))
	      (i 0)
	      (width-column 0)
	      column)
	  (while (< i (length widths))
	    ;; If we previously had a rowspan definition, then that
	    ;; means that we now have a "missing" td/th element here.
	    ;; So just insert a dummy, empty one to (sort of) emulate
	    ;; rowspan.
	    (setq column
		  (if (zerop (aref rowspans i))
		      (pop columns)
		    (aset rowspans i (1- (aref rowspans i)))
		    '(td)))
	    (when (and (not (stringp column))
		       (or (memq (dom-tag column) '(td th))
			   (not column)))
	      (when-let* ((span (dom-attr column 'rowspan)))
		(aset rowspans i (+ (aref rowspans i)
				    (1- (string-to-number span)))))
	      ;; Sanity check for invalid column-spans.
	      (when (>= width-column (length widths))
		(setq width-column 0))
	      (setq width
		    (if column
			(aref widths width-column)
		      (* 10 shr-table-separator-pixel-width)))
	      (when (setq colspan (dom-attr column 'colspan))
		(setq colspan (min (string-to-number colspan)
				   ;; The colspan may be wrong, so
				   ;; truncate it to the length of the
				   ;; remaining columns.
				   (- (length widths) i)))
		(dotimes (j (1- colspan))
		  (setq width
			(if (> (+ i 1 j) (1- (length widths)))
			    ;; If we have a colspan spec that's longer
			    ;; than the table is wide, just use the last
			    ;; width as the width.
			    (aref widths (1- (length widths)))
			  ;; Sum up the widths of the columns we're
			  ;; spanning.
			  (+ width
			     shr-table-separator-length
			     (aref widths (+ i 1 j))))))
		(setq width-column (+ width-column (1- colspan))
		      colspan-count colspan
		      colspan-remaining colspan))
	      (when column
		(let ((data (shr-render-td column width fill)))
		  (if (and (not fill)
			   (> colspan-remaining 0))
		      (progn
			(setq colspan-width (car data))
			(let ((this-width (/ colspan-width colspan-count)))
			  (push (cons this-width (cadr data)) tds)
			  (setq colspan-remaining (1- colspan-remaining))))
		    (if (not fill)
			(push (cons (car data) (cadr data)) tds)
		      (push data tds)))))
	      (when (and colspan
			 (> colspan 1))
		(dotimes (_ (1- colspan))
		  (setq i (1+ i))
		  (push
		   (if fill
		       (list 0 0 -1 nil 1 nil nil)
		     '(0 . 0))
		   tds)))
	      (setq i (1+ i)
		    width-column (1+ width-column))))
	  (push (nreverse tds) trs))))
    (nreverse trs)))

(defun shr-pixel-buffer-width ()
  (if (not shr-use-fonts)
      (save-excursion
	(goto-char (point-min))
	(let ((max 0))
	  (while (not (eobp))
	    (end-of-line)
	    (setq max (max max (current-column)))
	    (forward-line 1))
	  max))
    (if (get-buffer-window)
	(car (window-text-pixel-size nil (point-min) (point-max)))
      (save-window-excursion
        ;; Avoid errors if the selected window is a dedicated one,
        ;; and they just want to insert a document into it.
        (set-window-dedicated-p nil nil)
	(set-window-buffer nil (current-buffer))
	(car (window-text-pixel-size nil (point-min) (point-max)))))))

(defun shr-render-td (dom width fill)
  (let ((cache (intern (format "shr-td-cache-%s-%s" width fill))))
    (or (dom-attr dom cache)
	(and fill
	     (let (result)
	       (dolist (attr (dom-attributes dom))
		 (let ((name (symbol-name (car attr))))
		   (when (string-match "shr-td-cache-\\([0-9]+\\)-nil" name)
		     (let ((cache-width (string-to-number
					 (match-string 1 name))))
		       (when (and (>= cache-width width)
				  (<= (car (cdr attr)) width))
			 (setq result (cdr attr)))))))
	       result))
	(let* ((pt (point))
               (result (shr-render-td-1 dom width fill)))
	  (dom-set-attribute dom cache result)
          (goto-char pt)
	  result))))

(defun shr-render-td-1 (dom width fill)
  (with-temp-buffer
    (let ((bgcolor (dom-attr dom 'bgcolor))
	  (fgcolor (dom-attr dom 'fgcolor))
	  (style (dom-attr dom 'style))
	  (shr-stylesheet shr-stylesheet)
	  (max-width 0)
          (shr--link-targets nil)
	  natural-width)
      (when style
	(setq style (and (string-search "color" style)
			 (shr-parse-style style))))
      (when bgcolor
	(setq style (nconc (list (cons 'background-color bgcolor))
			   style)))
      (when fgcolor
	(setq style (nconc (list (cons 'color fgcolor)) style)))
      (when style
	(setq shr-stylesheet (append style shr-stylesheet)))
      (let ((shr-internal-width width)
	    (shr-indentation 0))
	(shr-descend dom))
      (save-window-excursion
        ;; Avoid errors if the selected window is a dedicated one,
        ;; and they just want to insert a document into it.
        (set-window-dedicated-p nil nil)
	(set-window-buffer nil (current-buffer))
	(unless fill
	  (setq natural-width
		(or (dom-attr dom 'shr-td-cache-natural)
		    (let ((natural (max (shr-pixel-buffer-width)
					(shr-dom-max-natural-width dom))))
		      (dom-set-attribute dom 'shr-td-cache-natural natural)
		      natural))))
	(if (and natural-width
		 (<= natural-width width))
	    (setq max-width natural-width)
	  (let ((shr-internal-width width))
	    (shr-fill-lines (point-min) (point-max))
	    (setq max-width (shr-pixel-buffer-width)))))
      (goto-char (point-max))
      ;; Delete padding at the bottom of the TDs.
      (delete-region
       (point)
       (progn
	 (skip-chars-backward " \t\n")
	 (end-of-line)
	 (point)))
      (goto-char (point-min))
      (shr--set-target-ids shr--link-targets)
      (list max-width
	    natural-width
	    (count-lines (point-min) (point-max))
	    (split-string (buffer-string) "\n")
	    (if (dom-attr dom 'colspan)
		(string-to-number (dom-attr dom 'colspan))
	      1)
	    (cdr (assq 'color shr-stylesheet))
	    (cdr (assq 'background-color shr-stylesheet))))))

(defun shr-dom-max-natural-width (dom)
  (or (if (eq (dom-tag dom) 'table)
          (cl-loop for line in (dom-attr dom 'shr-suggested-widths)
	           maximize (+ shr-table-separator-length
		               (cl-loop for elem in line
			                summing
			                (+ (cdr elem)
			                   (* 2 shr-table-separator-length)))))
        (cl-loop for child in (dom-children dom)
                 unless (stringp child)
                 maximize (shr-dom-max-natural-width child)))
      0))

(defun shr-buffer-width ()
  (goto-char (point-min))
  (let ((max 0))
    (while (not (eobp))
      (end-of-line)
      (setq max (max max (current-column)))
      (forward-line 1))
    max))

(defun shr-pro-rate-columns (columns)
  (let ((total-percentage 0)
	(widths (make-vector (length columns) 0)))
    (dotimes (i (length columns))
      (setq total-percentage (+ total-percentage (aref columns i))))
    (setq total-percentage (/ 1.0 total-percentage))
    (dotimes (i (length columns))
      (aset widths i (max (truncate (* (aref columns i)
				       total-percentage
				       (- shr-internal-width
                                          (* (1+ (length columns))
					     shr-table-separator-pixel-width))))
			  10)))
    widths))

;; Return a summary of the number and shape of the TDs in the table.
(defun shr-column-specs (dom)
  (let ((columns (make-vector (shr-max-columns dom) 1)))
    (dolist (row (dom-non-text-children dom))
      (when (eq (dom-tag row) 'tr)
	(let ((i 0))
	  (dolist (column (dom-non-text-children row))
	    (when (memq (dom-tag column) '(td th))
	      (let ((width (dom-attr column 'width)))
		(when (and width
			   (string-match "\\([0-9]+\\)%" width)
			   (not (zerop (setq width (string-to-number
						    (match-string 1 width))))))
		  (aset columns i (/ width 100.0))))
	      (setq i (1+ i)))))))
    columns))

(defun shr-count (dom elem)
  ;; This is faster than `seq-count', and shr can use it.
  (let ((i 0))
    (dolist (sub (dom-children dom))
      (when (and (not (stringp sub))
                 (eq (dom-tag sub) elem))
        (setq i (1+ i))))
    i))

(defun shr-max-columns (dom)
  (let ((max 0)
        (this 0)
        (rowspans nil))
    (dolist (row (dom-children dom))
      (when (and (not (stringp row))
		 (eq (dom-tag row) 'tr))
        (setq this 0)
        (dolist (column (dom-children row))
          (when (and (not (stringp column))
                     (memq (dom-tag column) '(td th)))
            (setq this (+ 1 this (length rowspans)))
            ;; We have a rowspan, which we emulate later in rendering
            ;; by adding an extra column to the following rows.
            (when-let* ((span (dom-attr column 'rowspan)))
              (push (string-to-number span) rowspans))))
	(setq max (max max this)))
      ;; Count down the rowspans in effect.
      (let ((new nil))
        (dolist (span rowspans)
          (when (> span 1)
            (push (1- span) new)))
        (setq rowspans new)))
    max))

(provide 'shr)

;;; shr.el ends here
