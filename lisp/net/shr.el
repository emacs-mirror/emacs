;;; shr.el --- Simple HTML Renderer

;; Copyright (C) 2010-2015 Free Software Foundation, Inc.

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

(eval-when-compile (require 'cl))
(eval-when-compile (require 'url))      ;For url-filename's setf handler.
(require 'browse-url)
(require 'subr-x)
(require 'dom)

(defgroup shr nil
  "Simple HTML Renderer"
  :version "25.1"
  :group 'web)

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
  :type '(choice (const nil) regexp))

(defcustom shr-use-fonts t
  "If non-nil, use proportional fonts for text."
  :version "25.1"
  :group 'shr
  :type 'boolean)

(defcustom shr-table-horizontal-line nil
  "Character used to draw horizontal table lines.
If nil, don't draw horizontal table lines."
  :group 'shr
  :type '(choice (const nil) character))

(defcustom shr-table-vertical-line ?\s
  "Character used to draw vertical table lines."
  :group 'shr
  :type 'character)

(defcustom shr-table-corner ?\s
  "Character used to draw table corners."
  :group 'shr
  :type 'character)

(defcustom shr-hr-line ?-
  "Character used to draw hr lines."
  :group 'shr
  :type 'character)

(defcustom shr-width nil
  "Frame width to use for rendering.
May either be an integer specifying a fixed width in characters,
or nil, meaning that the full width of the window should be
used."
  :version "25.1"
  :type '(choice (integer :tag "Fixed width in characters")
		 (const   :tag "Use the width of the window" nil))
  :group 'shr)

(defcustom shr-bullet "* "
  "Bullet used for unordered lists.
Alternative suggestions are:
- \"  \"
- \"  \""
  :version "24.4"
  :type 'string
  :group 'shr)

(defcustom shr-external-browser 'browse-url-default-browser
  "Function used to launch an external browser."
  :version "24.4"
  :group 'shr
  :type 'function)

(defcustom shr-image-animate t
  "Non nil means that images that can be animated will be."
  :version "24.4"
  :group 'shr
  :type 'boolean)

(defvar shr-content-function nil
  "If bound, this should be a function that will return the content.
This is used for cid: URLs, and the function is called with the
cid: URL as the argument.")

(defvar shr-put-image-function 'shr-put-image
  "Function called to put image and alt string.")

(defface shr-strike-through '((t (:strike-through t)))
  "Font for <s> elements."
  :group 'shr)

(defface shr-link
  '((t (:inherit link)))
  "Font for link elements."
  :group 'shr)

(defvar shr-inhibit-images nil
  "If non-nil, inhibit loading images.")

;;; Internal variables.

(defvar shr-folding-mode nil)
(defvar shr-start nil)
(defvar shr-indentation 0)
(defvar shr-internal-width nil)
(defvar shr-list-mode nil)
(defvar shr-content-cache nil)
(defvar shr-kinsoku-shorten nil)
(defvar shr-table-depth 0)
(defvar shr-stylesheet nil)
(defvar shr-base nil)
(defvar shr-depth 0)
(defvar shr-warning nil)
(defvar shr-ignore-cache nil)
(defvar shr-external-rendering-functions nil)
(defvar shr-target-id nil)
(defvar shr-table-separator-length 1)
(defvar shr-table-separator-pixel-width 0)
(defvar shr-table-id nil)
(defvar shr-current-font nil)
(defvar shr-internal-bullet nil)

(defvar shr-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'shr-show-alt-text)
    (define-key map "i" 'shr-browse-image)
    (define-key map "z" 'shr-zoom-image)
    (define-key map [?\t] 'shr-next-link)
    (define-key map [?\M-\t] 'shr-previous-link)
    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2] 'shr-browse-url)
    (define-key map "I" 'shr-insert-image)
    (define-key map "w" 'shr-copy-url)
    (define-key map "u" 'shr-copy-url)
    (define-key map "v" 'shr-browse-url)
    (define-key map "o" 'shr-save-contents)
    (define-key map "\r" 'shr-browse-url)
    map))

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
	(shr-internal-width (or (and shr-width
				     (if (not shr-use-fonts)
					 shr-width
				       (* shr-width (frame-char-width))))
				(if (not shr-use-fonts)
				    (- (window-width) 2)
				  (- (window-pixel-width)
				     (* (frame-fringe-width) 2))))))
    (shr-descend dom)
    (shr-fill-lines start (point))
    (shr-remove-trailing-whitespace start (point))
    (when shr-warning
      (message "%s" shr-warning))))

(defun shr-remove-trailing-whitespace (start end)
  (let ((width (window-width)))
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (not (eobp))
	(end-of-line)
	(when (> (shr-previous-newline-padding-width (current-column)) width)
	  (dolist (overlay (overlays-at (point)))
	    (when (overlay-get overlay 'before-string)
	      (overlay-put overlay 'before-string nil))))
	(forward-line 1)))))

(defun shr-copy-url (&optional image-url)
  "Copy the URL under point to the kill ring.
If IMAGE-URL (the prefix) is non-nil, or there is no link under
point, but there is an image under point then copy the URL of the
image under point instead.
If called twice, then try to fetch the URL and see whether it
redirects somewhere else."
  (interactive "P")
  (let ((url (or (get-text-property (point) 'shr-url)
		 (get-text-property (point) 'image-url))))
    (cond
     ((not url)
      (message "No URL under point"))
     ;; Resolve redirected URLs.
     ((equal url (car kill-ring))
      (url-retrieve
       url
       (lambda (a)
	 (when (and (consp a)
		    (eq (car a) :redirect))
	   (with-temp-buffer
	     (insert (cadr a))
	     (goto-char (point-min))
	     ;; Remove common tracking junk from the URL.
	     (when (re-search-forward ".utm_.*" nil t)
	       (replace-match "" t t))
	     (message "Copied %s" (buffer-string))
	     (copy-region-as-kill (point-min) (point-max)))))
       nil t))
     ;; Copy the URL to the kill ring.
     (t
      (with-temp-buffer
	(insert (url-encode-url url))
	(copy-region-as-kill (point-min) (point-max))
	(message "Copied %s" (buffer-string)))))))

(defun shr-next-link ()
  "Skip to the next link."
  (interactive)
  (let ((skip (text-property-any (point) (point-max) 'help-echo nil)))
    (if (or (eobp)
	    (not (setq skip (text-property-not-all skip (point-max)
						   'help-echo nil))))
	(message "No next link")
      (goto-char skip)
      (message "%s" (get-text-property (point) 'help-echo)))))

(defun shr-previous-link ()
  "Skip to the previous link."
  (interactive)
  (let ((start (point))
	(found nil))
    ;; Skip past the current link.
    (while (and (not (bobp))
		(get-text-property (point) 'help-echo))
      (forward-char -1))
    ;; Find the previous link.
    (while (and (not (bobp))
		(not (setq found (get-text-property (point) 'help-echo))))
      (forward-char -1))
    (if (not found)
	(progn
	  (message "No previous link")
	  (goto-char start))
      ;; Put point at the start of the link.
      (while (and (not (bobp))
		  (get-text-property (point) 'help-echo))
	(forward-char -1))
      (forward-char 1)
      (message "%s" (get-text-property (point) 'help-echo)))))

(defun shr-show-alt-text ()
  "Show the ALT text of the image under point."
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
      (url-retrieve url 'shr-image-fetched
		    (list (current-buffer) (1- (point)) (point-marker))
		    t t))))

(defun shr-zoom-image ()
  "Toggle the image size.
The size will be rotated between the default size, the original
size, and full-buffer size."
  (interactive)
  (let ((url (get-text-property (point) 'image-url))
	(size (get-text-property (point) 'image-size))
	(buffer-read-only nil))
    (if (not url)
	(message "No image under point")
      ;; Delete the old picture.
      (while (get-text-property (point) 'image-url)
	(forward-char -1))
      (forward-char 1)
      (let ((start (point)))
	(while (get-text-property (point) 'image-url)
	  (forward-char 1))
	(forward-char -1)
	(put-text-property start (point) 'display nil)
	(when (> (- (point) start) 2)
	  (delete-region start (1- (point)))))
      (message "Inserting %s..." url)
      (url-retrieve url 'shr-image-fetched
		    (list (current-buffer) (1- (point)) (point-marker)
			  (list (cons 'size
				      (cond ((or (eq size 'default)
						 (null size))
					     'original)
					    ((eq size 'original)
					     'full)
					    ((eq size 'full)
					     'default)))))
		    t))))

;;; Utility functions.

(defsubst shr-generic (dom)
  (dolist (sub (dom-children dom))
    (if (stringp sub)
	(shr-insert sub)
      (shr-descend sub))))

(defun shr-descend (dom)
  (let ((function
	 (or
	  ;; Allow other packages to override (or provide) rendering
	  ;; of elements.
	  (cdr (assq (dom-tag dom) shr-external-rendering-functions))
	  (intern (concat "shr-tag-" (symbol-name (dom-tag dom))) obarray)))
	(style (dom-attr dom 'style))
	(shr-stylesheet shr-stylesheet)
	(shr-depth (1+ shr-depth))
	(start (point)))
    ;; shr uses about 12 frames per nested node.
    (if (> shr-depth (/ max-specpdl-size 12))
	(setq shr-warning "Too deeply nested to render properly; consider increasing `max-specpdl-size'")
      (when style
	(if (string-match "color\\|display\\|border-collapse" style)
	    (setq shr-stylesheet (nconc (shr-parse-style style)
					shr-stylesheet))
	  (setq style nil)))
      ;; If we have a display:none, then just ignore this part of the DOM.
      (unless (equal (cdr (assq 'display shr-stylesheet)) "none")
	(if (fboundp function)
	    (funcall function dom)
	  (shr-generic dom))
	(when (and shr-target-id
		   (equal (dom-attr dom 'id) shr-target-id))
	  ;; If the element was empty, we don't have anything to put the
	  ;; anchor on.  So just insert a dummy character.
	  (when (= start (point))
	    (insert "*"))
	  (put-text-property start (1+ start) 'shr-target-id shr-target-id))
	;; If style is set, then this node has set the color.
	(when style
	  (shr-colorize-region
	   start (point)
	   (cdr (assq 'color shr-stylesheet))
	   (cdr (assq 'background-color shr-stylesheet))))))))

(defun shr-fill-text (text)
  (if (zerop (length text))
      text
    (with-temp-buffer
      (let ((shr-indentation 0)
	    (shr-start nil)
	    (shr-internal-width (- (window-pixel-width)
				   (* (frame-fringe-width) 2))))
	(shr-insert text)
	(buffer-string)))))

(define-inline shr-char-breakable-p (char)
  "Return non-nil if a line can be broken before and after CHAR."
  (inline-quote (aref fill-find-break-point-function-table ,char)))
(define-inline shr-char-nospace-p (char)
  "Return non-nil if no space is required before and after CHAR."
  (inline-quote (aref fill-nospace-between-words-table ,char)))

;; KINSOKU is a Japanese word meaning a rule that should not be violated.
;; In Emacs, it is a term used for characters, e.g. punctuation marks,
;; parentheses, and so on, that should not be placed in the beginning
;; of a line or the end of a line.
(define-inline shr-char-kinsoku-bol-p (char)
  "Return non-nil if a line ought not to begin with CHAR."
  (inline-letevals (char)
    (inline-quote (and (not (eq ,char ?'))
                       (aref (char-category-set ,char) ?>)))))
(define-inline shr-char-kinsoku-eol-p (char)
  "Return non-nil if a line ought not to end with CHAR."
  (inline-quote (aref (char-category-set ,char) ?<)))
(unless (shr-char-kinsoku-bol-p (make-char 'japanese-jisx0208 33 35))
  (load "kinsoku" nil t))

(defun shr-pixel-column ()
  (if (not shr-use-fonts)
      (current-column)
    (if (not (get-buffer-window (current-buffer)))
	(save-window-excursion
	  (set-window-buffer nil (current-buffer))
	  (car (window-text-pixel-size nil (line-beginning-position) (point))))
      (car (window-text-pixel-size nil (line-beginning-position) (point))))))

(defun shr-pixel-region ()
  (- (shr-pixel-column)
     (save-excursion
       (goto-char (mark))
       (shr-pixel-column))))

(defun shr-string-pixel-width (string)
  (if (not shr-use-fonts)
      (length string)
    (with-temp-buffer
      (insert string)
      (shr-pixel-column))))

(defun shr-insert (text)
  (when (and (not (bolp))
	     (get-text-property (1- (point)) 'image-url))
    (insert "\n"))
  (cond
   ((eq shr-folding-mode 'none)
    (let ((start (point)))
      (insert text)
      (save-restriction
	(narrow-to-region start (point))
	;; Remove soft hyphens.
	(goto-char (point-min))
	(while (search-forward "­" nil t)
	  (replace-match "" t t))
	(goto-char (point-max)))))
   (t
    (let ((font-start (point)))
      (when (and (string-match "\\`[ \t\n\r ]" text)
		 (not (bolp))
		 (not (eq (char-after (1- (point))) ? )))
	(insert " "))
      (let ((start (point))
	    (bolp (bolp)))
	(insert text)
	(save-restriction
	  (narrow-to-region start (point))
	  (goto-char start)
	  (when (looking-at "[ \t\n\r ]+")
	    (replace-match "" t t))
	  (while (re-search-forward "[ \t\n\r ]+" nil t)
	    (replace-match " " t t))
	  ;; Remove soft hyphens.
	  (goto-char (point-min))
	  (while (search-forward "­" nil t)
	    (replace-match "" t t))
	  (goto-char (point-max)))
	;; We may have removed everything we inserted if if was just
	;; spaces.
	(unless (= font-start (point))
	  ;; Mark all lines that should possibly be folded afterwards.
	  (when bolp
	    (shr-mark-fill start))
	  (when shr-use-fonts
	    (put-text-property font-start (point)
			       'face
			       (or shr-current-font 'variable-pitch)))))))))

(defun shr-fill-lines (start end)
  (if (<= shr-internal-width 0)
      nil
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
  (let ((shr-indentation (get-text-property (point) 'shr-indentation))
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
    (shr-vertical-motion shr-internal-width)
    (when (looking-at " $")
      (delete-region (point) (line-end-position)))
    (while (not (eolp))
      ;; We have to do some folding.  First find the first
      ;; previous point suitable for folding.
      (if (or (not (shr-find-fill-point (line-beginning-position)))
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
      (let ((face (get-text-property (point) 'face))
	    (background-start (point)))
	(insert "\n")
	(shr-indent)
	(when face
	  (put-text-property background-start (point) 'face
			     `,(shr-face-background face))))
      (setq start (point))
      (shr-vertical-motion shr-internal-width)
      (when (looking-at " $")
	(delete-region (point) (line-end-position))))))

(defun shr-find-fill-point (start)
  (let ((bp (point))
	(end (point))
	failed)
    (while (not (or (setq failed (<= (point) start))
		    (eq (preceding-char) ? )
		    (eq (following-char) ? )
		    (shr-char-breakable-p (preceding-char))
		    (shr-char-breakable-p (following-char))
		    (and (shr-char-kinsoku-bol-p (preceding-char))
			 (shr-char-breakable-p (following-char))
			 (not (shr-char-kinsoku-bol-p (following-char))))
		    (shr-char-kinsoku-eol-p (following-char))
		    (bolp)))
      (backward-char 1))
    (if failed
	;; There's no breakable point, so we give it up.
	(let (found)
	  (goto-char bp)
	  (unless shr-kinsoku-shorten
	    (while (setq found (re-search-forward
				"\\(\\c>\\)\\| \\|\\c<\\|\\c|"
				(line-end-position) 'move)))
	    (if (and found
		     (not (match-beginning 1)))
		(goto-char (match-beginning 0)))))
      (or
       (eolp)
       ;; Don't put kinsoku-bol characters at the beginning of a line,
       ;; or kinsoku-eol characters at the end of a line.
       (cond
	(shr-kinsoku-shorten
	 (while (and (not (memq (preceding-char) (list ?\C-@ ?\n ? )))
		     (shr-char-kinsoku-eol-p (preceding-char)))
	   (backward-char 1))
	 (when (setq failed (<= (point) start))
	   ;; There's no breakable point that doesn't violate kinsoku,
	   ;; so we look for the second best position.
	   (while (and (progn
			 (forward-char 1)
			 (<= (point) end))
		       (progn
			 (setq bp (point))
			 (shr-char-kinsoku-eol-p (following-char)))))
	   (goto-char bp)))
	((shr-char-kinsoku-eol-p (preceding-char))
	 ;; Find backward the point where kinsoku-eol characters begin.
	 (let ((count 4))
	   (while
	       (progn
		 (backward-char 1)
		 (and (> (setq count (1- count)) 0)
		      (not (memq (preceding-char) (list ?\C-@ ?\n ? )))
		      (or (shr-char-kinsoku-eol-p (preceding-char))
			  (shr-char-kinsoku-bol-p (following-char)))))))
	 (when (setq failed (<= (point) start))
	   ;; There's no breakable point that doesn't violate kinsoku,
	   ;; so we go to the second best position.
	   (if (looking-at "\\(\\c<+\\)\\c<")
	       (goto-char (match-end 1))
	     (forward-char 1))))
	((shr-char-kinsoku-bol-p (following-char))
	 ;; Find forward the point where kinsoku-bol characters end.
	 (let ((count 4))
	   (while (progn
		    (forward-char 1)
		    (and (>= (setq count (1- count)) 0)
			 (shr-char-kinsoku-bol-p (following-char))
			 (shr-char-breakable-p (following-char))))))))
       (when (eq (following-char) ? )
	 (forward-char 1))))
    (not failed)))

(defun shr-parse-base (url)
  ;; Always chop off anchors.
  (when (string-match "#.*" url)
    (setq url (substring url 0 (match-beginning 0))))
  ;; NB: <base href="" > URI may itself be relative to the document s URI
  (setq url (shr-expand-url url))
  (let* ((parsed (url-generic-parse-url url))
	 (local (url-filename parsed)))
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
  ;; Strip leading whitespace
  (and url (string-match "\\`\\s-+" url)
       (setq url (substring url (match-end 0))))
  (cond ((or (not url)
	     (not base)
	     (string-match "\\`[a-z]*:" url))
	 ;; Absolute or empty URI
	 (or url (nth 3 base)))
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
	 ;; Totally relative.
	 (url-expand-file-name url (concat (car base) (cadr base))))))

(defun shr-ensure-newline ()
  (unless (zerop (current-column))
    (insert "\n")))

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
       (t
	(insert "\n\n"))))))

(defun shr-indent ()
  (when (> shr-indentation 0)
    (insert
     (if (not shr-use-fonts)
	 (make-string shr-indentation ?\s)
       (propertize " "
		   'display
		   `(space :width (,shr-indentation)))))))

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

(defun shr-browse-url (&optional external mouse-event)
  "Browse the URL under point.
If EXTERNAL, browse the URL using `shr-external-browser'."
  (interactive (list current-prefix-arg last-nonmenu-event))
  (mouse-set-point mouse-event)
  (let ((url (get-text-property (point) 'shr-url)))
    (cond
     ((not url)
      (message "No link under point"))
     ((string-match "^mailto:" url)
      (browse-url-mail url))
     (t
      (if external
	  (funcall shr-external-browser url)
	(browse-url url))))))

(defun shr-save-contents (directory)
  "Save the contents from URL in a file."
  (interactive "DSave contents of URL to directory: ")
  (let ((url (get-text-property (point) 'shr-url)))
    (if (not url)
	(message "No link under point")
      (url-retrieve (shr-encode-url url)
		    'shr-store-contents (list url directory)
		    nil t))))

(defun shr-store-contents (status url directory)
  (unless (plist-get status :error)
    (when (or (search-forward "\n\n" nil t)
	      (search-forward "\r\n\r\n" nil t))
      (write-region (point) (point-max)
		    (expand-file-name (file-name-nondirectory url)
				      directory)))))

(defun shr-image-fetched (status buffer start end &optional flags)
  (let ((image-buffer (current-buffer)))
    (when (and (buffer-name buffer)
	       (not (plist-get status :error)))
      (url-store-in-cache image-buffer)
      (when (or (search-forward "\n\n" nil t)
		(search-forward "\r\n\r\n" nil t))
	(let ((data (shr-parse-image-data)))
	  (with-current-buffer buffer
	    (save-excursion
	      (let ((alt (buffer-substring start end))
		    (properties (text-properties-at start))
		    (inhibit-read-only t))
		(delete-region start end)
		(goto-char start)
		(funcall shr-put-image-function data alt flags)
		(while properties
		  (let ((type (pop properties))
			(value (pop properties)))
		    (unless (memq type '(display image-size))
		      (put-text-property start (point) type value))))))))))
    (kill-buffer image-buffer)))

(defun shr-image-from-data (data)
  "Return an image from the data: URI content DATA."
  (when (string-match
	 "\\(\\([^/;,]+\\(/[^;,]+\\)?\\)\\(;[^;,]+\\)*\\)?,\\(.*\\)"
	 data)
    (let ((param (match-string 4 data))
	  (payload (url-unhex-string (match-string 5 data))))
      (when (string-match "^.*\\(;[ \t]*base64\\)$" param)
	(setq payload (base64-decode-string payload)))
      payload)))

;; Behind display-graphic-p test.
(declare-function image-size "image.c" (spec &optional pixels frame))
(declare-function image-animate "image" (image &optional index limit))

(defun shr-put-image (spec alt &optional flags)
  "Insert image SPEC with a string ALT.  Return image.
SPEC is either an image data blob, or a list where the first
element is the data blob and the second element is the content-type."
  (if (display-graphic-p)
      (let* ((size (cdr (assq 'size flags)))
	     (data (if (consp spec)
		       (car spec)
		     spec))
	     (content-type (and (consp spec)
				(cadr spec)))
	     (start (point))
	     (image (cond
		     ((eq size 'original)
		      (create-image data nil t :ascent 100
				    :format content-type))
		     ((eq content-type 'image/svg+xml)
		      (create-image data 'svg t :ascent 100))
		     ((eq size 'full)
		      (ignore-errors
			(shr-rescale-image data content-type)))
		     (t
		      (ignore-errors
			(shr-rescale-image data content-type))))))
        (when image
	  ;; When inserting big-ish pictures, put them at the
	  ;; beginning of the line.
	  (when (and (> (current-column) 0)
		     (> (car (image-size image t)) 400))
	    (insert "\n"))
	  (if (eq size 'original)
	      (insert-sliced-image image (or alt "*") nil 20 1)
	    (insert-image image (or alt "*")))
	  (put-text-property start (point) 'image-size size)
	  (when (and shr-image-animate
                     (cond ((fboundp 'image-multi-frame-p)
		       ;; Only animate multi-frame things that specify a
		       ;; delay; eg animated gifs as opposed to
		       ;; multi-page tiffs.  FIXME?
                            (cdr (image-multi-frame-p image)))
                           ((fboundp 'image-animated-p)
                            (image-animated-p image))))
            (image-animate image nil 60)))
	image)
    (insert alt)))

(defun shr-rescale-image (data &optional content-type)
  "Rescale DATA, if too big, to fit the current buffer."
  (if (not (and (fboundp 'imagemagick-types)
                (get-buffer-window (current-buffer))))
      (create-image data nil t :ascent 100)
    (let ((edges (window-inside-pixel-edges
		  (get-buffer-window (current-buffer)))))
      (create-image
       data 'imagemagick t
       :ascent 100
       :max-width (truncate (* shr-max-image-proportion
			       (- (nth 2 edges) (nth 0 edges))))
       :max-height (truncate (* shr-max-image-proportion
				(- (nth 3 edges) (nth 1 edges))))
       :format content-type))))

;; url-cache-extract autoloads url-cache.
(declare-function url-cache-create-filename "url-cache" (url))
(autoload 'mm-disable-multibyte "mm-util")
(autoload 'browse-url-mail "browse-url")

(defun shr-get-image-data (url)
  "Get image data for URL.
Return a string with image data."
  (with-temp-buffer
    (mm-disable-multibyte)
    (when (ignore-errors
	    (url-cache-extract (url-cache-create-filename (shr-encode-url url)))
	    t)
      (when (or (search-forward "\n\n" nil t)
		(search-forward "\r\n\r\n" nil t))
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
    ;; and remove the blocked bits.
    (when (eq content-type 'image/svg+xml)
      (setq data
	    (shr-dom-to-xml
	     (libxml-parse-xml-region (point) (point-max)))))
    (list data content-type)))

(defun shr-image-displayer (content-function)
  "Return a function to display an image.
CONTENT-FUNCTION is a function to retrieve an image for a cid url that
is an argument.  The function to be returned takes three arguments URL,
START, and END.  Note that START and END should be markers."
  `(lambda (url start end)
     (when url
       (if (string-match "\\`cid:" url)
	   ,(when content-function
	      `(let ((image (funcall ,content-function
				     (substring url (match-end 0)))))
		 (when image
		   (goto-char start)
		   (funcall shr-put-image-function
			    image (buffer-substring start end))
		   (delete-region (point) end))))
	 (url-retrieve url 'shr-image-fetched
		       (list (current-buffer) start end)
		       t t)))))

(defun shr-heading (dom &rest types)
  (shr-ensure-paragraph)
  (apply #'shr-fontize-dom dom types)
  (shr-ensure-paragraph))

(defun shr-urlify (start url &optional title)
  (shr-add-font start (point) 'shr-link)
  (add-text-properties
   start (point)
   (list 'shr-url url
	 'help-echo (let ((iri (or (ignore-errors
				     (decode-coding-string
				      (url-unhex-string url)
				      'utf-8 t))
				   url)))
		      (if title (format "%s (%s)" iri title) iri))
	 'follow-link t
	 'mouse-face 'highlight
	 'keymap shr-map)))

(defun shr-encode-url (url)
  "Encode URL."
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
  (when (or fg bg)
    (let ((new-colors (shr-color-check fg bg)))
      (when new-colors
	(when fg
	  (add-face-text-property start end
				  (list :foreground (cadr new-colors))
				  t))
	(when bg
	  (add-face-text-property start end
				  (list :background (car new-colors))
				  t)))
      new-colors)))

(defun shr-previous-newline-padding-width (width)
  (let ((overlays (overlays-at (point)))
	(previous-width 0))
    (if (null overlays)
	width
      (dolist (overlay overlays)
	(setq previous-width
	      (+ previous-width
		 (length (plist-get (overlay-properties overlay)
				    'before-string)))))
      (+ width previous-width))))

;;; Tag-specific rendering rules.

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

(defun shr-dom-to-xml (dom)
  (with-temp-buffer
    (shr-dom-print dom)
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
	    (not shr-blocked-images)
	    (not (string-match shr-blocked-images url)))
	(insert " ")
	(shr-dom-print elem)))))
  (insert (format "</%s>" (dom-tag dom))))

(defun shr-tag-svg (dom)
  (when (and (image-type-available-p 'svg)
	     (not shr-inhibit-images))
    (funcall shr-put-image-function (list (shr-dom-to-xml dom) 'image/svg+xml)
	     "SVG Image")))

(defun shr-tag-sup (dom)
  (let ((start (point)))
    (shr-generic dom)
    (put-text-property start (point) 'display '(raise 0.5))))

(defun shr-tag-sub (dom)
  (let ((start (point)))
    (shr-generic dom)
    (put-text-property start (point) 'display '(raise -0.5))))

(defun shr-tag-label (dom)
  (shr-generic dom)
  (shr-ensure-paragraph))

(defun shr-tag-p (dom)
  (shr-ensure-paragraph)
  (shr-generic dom)
  (shr-ensure-paragraph))

(defun shr-tag-div (dom)
  (shr-ensure-newline)
  (shr-generic dom)
  (shr-ensure-newline))

(defun shr-tag-s (dom)
  (shr-fontize-dom dom 'shr-strike-through))

(defun shr-tag-del (dom)
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

(defun shr-tag-tt (dom)
  (let ((shr-current-font 'default))
    (shr-generic dom)))

(defun shr-parse-style (style)
  (when style
    (save-match-data
      (when (string-match "\n" style)
        (setq style (replace-match " " t t style))))
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
  (when-let (base (dom-attr dom 'href))
    (setq shr-base (shr-parse-base base)))
  (shr-generic dom))

(defun shr-tag-a (dom)
  (let ((url (dom-attr dom 'href))
        (title (dom-attr dom 'title))
	(start (point))
	shr-start)
    (shr-generic dom)
    (when (and shr-target-id
	       (equal (dom-attr dom 'name) shr-target-id))
      ;; We have a zero-length <a name="foo"> element, so just
      ;; insert...  something.
      (when (= start (point))
	(shr-ensure-newline)
	(insert " "))
      (put-text-property start (1+ start) 'shr-target-id shr-target-id))
    (when url
      (shr-urlify (or shr-start start) (shr-expand-url url) title))))

(defun shr-tag-object (dom)
  (unless shr-inhibit-images
    (let ((start (point))
	  url multimedia image)
      (when-let (type (dom-attr dom 'type))
	(when (string-match "\\`image/svg" type)
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
	  (shr-tag-img dom url)
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
  :group 'shr
  :type '(alist :key-type regexp :value-type float))

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

(defun shr-tag-video (dom)
  (let ((image (dom-attr dom 'poster))
        (url (dom-attr dom 'src))
        (start (point)))
    (unless url
      (setq url (car (shr--extract-best-source dom))))
    (if image
        (shr-tag-img nil image)
      (shr-insert " [video] "))
    (shr-urlify start (shr-expand-url url))))

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
		 (> (length (dom-attr dom 'src)) 0)))
    (when (> (current-column) 0)
      (insert "\n"))
    (let ((alt (dom-attr dom 'alt))
	  (url (shr-expand-url (or url (dom-attr dom 'src)))))
      (let ((start (point-marker)))
	(when (zerop (length alt))
	  (setq alt "*"))
	(cond
	 ((or (member (dom-attr dom 'height) '("0" "1"))
	      (member (dom-attr dom 'width) '("0" "1")))
	  ;; Ignore zero-sized or single-pixel images.
	  )
	 ((and (not shr-inhibit-images)
	       (string-match "\\`data:" url))
	  (let ((image (shr-image-from-data (substring url (match-end 0)))))
	    (if image
		(funcall shr-put-image-function image alt)
	      (insert alt))))
	 ((and (not shr-inhibit-images)
	       (string-match "\\`cid:" url))
	  (let ((url (substring url (match-end 0)))
		image)
	    (if (or (not shr-content-function)
		    (not (setq image (funcall shr-content-function url))))
		(insert alt)
	      (funcall shr-put-image-function image alt))))
	 ((or shr-inhibit-images
	      (and shr-blocked-images
		   (string-match shr-blocked-images url)))
	  (setq shr-start (point))
	  (if (> (string-width alt) 8)
	      (shr-insert (truncate-string-to-width alt 8))
	    (shr-insert alt)))
	 ((and (not shr-ignore-cache)
	       (url-is-cached (shr-encode-url url)))
	  (funcall shr-put-image-function (shr-get-image-data url) alt))
	 (t
	  (insert alt " ")
	  (when (and shr-ignore-cache
		     (url-is-cached (shr-encode-url url)))
	    (let ((file (url-cache-create-filename (shr-encode-url url))))
	      (when (file-exists-p file)
		(delete-file file))))
	  (url-queue-retrieve
	   (shr-encode-url url) 'shr-image-fetched
	   (list (current-buffer) start (set-marker (make-marker) (1- (point))))
	   t t)))
	(when (zerop shr-table-depth) ;; We are not in a table.
	  (put-text-property start (point) 'keymap shr-map)
	  (put-text-property start (point) 'shr-alt alt)
	  (put-text-property start (point) 'image-url url)
	  (put-text-property start (point) 'image-displayer
			     (shr-image-displayer shr-content-function))
	  (put-text-property start (point) 'help-echo
			     (shr-fill-text
			      (or (dom-attr dom 'title) alt))))))))

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
  (shr-ensure-paragraph))

(defun shr-tag-ol (dom)
  (shr-ensure-paragraph)
  (let ((shr-list-mode 1))
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
	(shr-generic dom)))))

(defun shr-mark-fill (start)
  ;; We may not have inserted any text to fill.
  (unless (= start (point))
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
  (shr-heading dom (if shr-use-fonts
		       '(variable-pitch (:height 1.3 :weight bold))
		     'bold)))

(defun shr-tag-h2 (dom)
  (shr-heading dom 'bold))

(defun shr-tag-h3 (dom)
  (shr-heading dom 'italic))

(defun shr-tag-h4 (dom)
  (shr-heading dom))

(defun shr-tag-h5 (dom)
  (shr-heading dom))

(defun shr-tag-h6 (dom)
  (shr-heading dom))

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
	 (shr-kinsoku-shorten t)
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
	 (sketch (loop for line in elems
		       collect (mapcar #'car line)))
	 (natural (loop for line in elems
			collect (mapcar #'cdr line)))
	 (sketch-widths (shr-table-widths sketch natural suggested-widths)))
    ;; This probably won't work very well.
    (when (> (+ (loop for width across sketch-widths
		      summing (1+ width))
		shr-indentation shr-table-separator-pixel-width)
	     (frame-width))
      (setq truncate-lines t))
    ;; Then render the table again with these new "hard" widths.
    (shr-insert-table (shr-make-table dom sketch-widths t) sketch-widths)))

(defun shr-tag-table (dom)
  (shr-ensure-paragraph)
  (let* ((caption (dom-children (dom-child-by-tag dom 'caption)))
	 (header (dom-non-text-children (dom-child-by-tag dom 'thead)))
	 (body (dom-non-text-children (or (dom-child-by-tag dom 'tbody)
					  dom)))
	 (footer (dom-non-text-children (dom-child-by-tag dom 'tfoot)))
         (bgcolor (dom-attr dom 'bgcolor))
	 (start (point))
	 (shr-stylesheet (nconc (list (cons 'background-color bgcolor))
				shr-stylesheet))
	 (nheader (if header (shr-max-columns header)))
	 (nbody (if body (shr-max-columns body)))
	 (nfooter (if footer (shr-max-columns footer))))
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
	(let ((table
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
	  (dom-set-attribute table 'shr-fixed-table t)
	  (setcdr dom (cdr table))
	  (shr-tag-table-1 dom))))
    (when bgcolor
      (shr-colorize-region start (point) (cdr (assq 'color shr-stylesheet))
			   bgcolor))
    ;; Finally, insert all the images after the table.  The Emacs buffer
    ;; model isn't strong enough to allow us to put the images actually
    ;; into the tables.
    (when (zerop shr-table-depth)
      (save-excursion
	(shr-expand-alignments start (point)))
      (dolist (elem (dom-by-tag dom 'object))
	(shr-tag-object elem))
      (dolist (elem (dom-by-tag dom 'img))
	(shr-tag-img elem)))))

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
	(dotimes (i (max height 1))
	  (shr-indent)
	  (insert shr-table-vertical-line "\n"))
	(dolist (column row)
	  (when (> (nth 2 column) -1)
	    (goto-char start)
	    ;; Sum up all the widths from the column.  (There may be
	    ;; more than one if this is a "colspan" column.)
	    (dotimes (i (nth 4 column))
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
		(let ((start (point)))
		  (insert
		   line
		   (propertize " "
			       'display `(space :align-to (,pixel-align))
			       'face (and (> (length line) 0)
					  (shr-face-background
					   (get-text-property
					    (1- (length line)) 'face line)))
			       'shr-table-indent shr-table-id)
		   shr-table-vertical-line)
		  (shr-colorize-region
		   start (1- (point)) (nth 5 column) (nth 6 column)))
		(forward-line 1))
	      ;; Add blank lines at padding at the bottom of the TD,
	      ;; possibly.
	      (dotimes (i (- height (length lines)))
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
       (let ((background nil))
	 (dolist (elem face)
	   (when (and (consp elem)
		      (eq (car elem) :background))
	     (setq background (cadr elem))))
	 (and background
	      (list :background background)))))

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
    (let ((extra (- (apply '+ (append suggested-widths nil))
		    (apply '+ (append widths nil))
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
	      (when-let (span (dom-attr column 'rowspan))
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
		(dotimes (c (1- colspan))
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
	(let ((result (shr-render-td-1 dom width fill)))
	  (dom-set-attribute dom cache result)
	  result))))

(defun shr-render-td-1 (dom width fill)
  (with-temp-buffer
    (let ((bgcolor (dom-attr dom 'bgcolor))
	  (fgcolor (dom-attr dom 'fgcolor))
	  (style (dom-attr dom 'style))
	  (shr-stylesheet shr-stylesheet)
	  (max-width 0)
	  natural-width)
      (when style
	(setq style (and (string-match "color" style)
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
	(set-window-buffer nil (current-buffer))
	(unless fill
	  (setq natural-width
		(or (dom-attr dom 'shr-td-cache-natural)
		    (let ((natural (max (shr-pixel-buffer-width)
					(shr-dom-max-natural-width dom 0))))
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
      (list max-width
	    natural-width
	    (count-lines (point-min) (point-max))
	    (split-string (buffer-string) "\n")
	    (if (dom-attr dom 'colspan)
		(string-to-number (dom-attr dom 'colspan))
	      1)
	    (cdr (assq 'color shr-stylesheet))
	    (cdr (assq 'background-color shr-stylesheet))))))

(defun shr-dom-max-natural-width (dom max)
  (if (eq (dom-tag dom) 'table)
      (max max (or
		(loop for line in (dom-attr dom 'shr-suggested-widths)
		      maximize (+
				shr-table-separator-length
				(loop for elem in line
				      summing
				      (+ (cdr elem)
					 (* 2 shr-table-separator-length)))))
		0))
    (dolist (child (dom-children dom))
      (unless (stringp child)
	(setq max (max (shr-dom-max-natural-width child max)))))
    max))

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
  (let ((i 0))
    (dolist (sub (dom-children dom))
      (when (and (not (stringp sub))
		 (eq (dom-tag sub) elem))
	(setq i (1+ i))))
    i))

(defun shr-max-columns (dom)
  (let ((max 0))
    (dolist (row (dom-children dom))
      (when (and (not (stringp row))
		 (eq (dom-tag row) 'tr))
	(setq max (max max (+ (shr-count row 'td)
			      (shr-count row 'th))))))
    max))

(provide 'shr)

;; Local Variables:
;; coding: utf-8
;; End:

;;; shr.el ends here
