;;; gnus-html.el --- Render HTML in a buffer.

;; Copyright (C) 2010  Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: html, web

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

;; The idea is to provide a simple, fast and pretty minimal way to
;; render HTML (including links and images) in a buffer, based on an
;; external HTML renderer (i.e., w3m).

;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (require 'mm-decode))
(require 'mm-url)

(defcustom gnus-html-cache-directory (nnheader-concat gnus-directory "html-cache/")
  "Where Gnus will cache images it downloads from the web."
  :version "24.1"
  :group 'gnus-art
  :type 'directory)

(defcustom gnus-html-cache-size 500000000
  "The size of the Gnus image cache."
  :version "24.1"
  :group 'gnus-art
  :type 'integer)

(defcustom gnus-html-frame-width 70
  "What width to use when rendering HTML."
  :version "24.1"
  :group 'gnus-art
  :type 'integer)

(defcustom gnus-blocked-images "."
  "Images that have URLs matching this regexp will be blocked."
  :version "24.1"
  :group 'gnus-art
  :type 'regexp)

(defcustom gnus-max-image-proportion 0.7
  "How big pictures displayed are in relation to the window they're in.
A value of 0.7 means that they are allowed to take up 70% of the
width and height of the window.  If they are larger than this,
and Emacs supports it, then the images will be rescaled down to
fit these criteria."
  :version "24.1"
  :group 'gnus-art
  :type 'float)

(defvar gnus-html-image-map
  (let ((map (make-sparse-keymap)))
    (define-key map "u" 'gnus-article-copy-string)
    (define-key map "i" 'gnus-html-insert-image)
    map))

(defvar gnus-html-displayed-image-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'gnus-html-show-alt-text)
    (define-key map "i" 'gnus-html-browse-image)
    (define-key map "\r" 'gnus-html-browse-url)
    (define-key map "u" 'gnus-article-copy-string)
    (define-key map [tab] 'widget-forward)
    map))

;;;###autoload
(defun gnus-article-html (&optional handle)
  (let ((article-buffer (current-buffer)))
    (unless handle
      (setq handle (mm-dissect-buffer t)))
    (save-restriction
      (narrow-to-region (point) (point))
      (save-excursion
	(mm-with-part handle
	  (let* ((coding-system-for-read 'utf-8)
		 (coding-system-for-write 'utf-8)
		 (default-process-coding-system
		   (cons coding-system-for-read coding-system-for-write))
		 (charset (mail-content-type-get (mm-handle-type handle)
						 'charset)))
	    (when (and charset
		       (setq charset (mm-charset-to-coding-system charset))
		       (not (eq charset 'ascii)))
	      (insert (prog1
			  (mm-decode-coding-string (buffer-string) charset)
			(erase-buffer)
			(mm-enable-multibyte))))
	    (call-process-region (point-min) (point-max)
				 "w3m"
				 nil article-buffer nil
				 "-halfdump"
				 "-no-cookie"
				 "-I" "UTF-8"
				 "-O" "UTF-8"
				 "-o" "ext_halfdump=1"
				 "-o" "pre_conv=1"
				 "-t" (format "%s" tab-width)
				 "-cols" (format "%s" gnus-html-frame-width)
				 "-o" "display_image=on"
				 "-T" "text/html"))))
      (gnus-html-wash-tags))))

(defvar gnus-article-mouse-face)

(defun gnus-html-pre-wash ()
  (goto-char (point-min))
  (while (re-search-forward " *<pre_int> *</pre_int> *\n" nil t)
    (replace-match "" t t))
  (goto-char (point-min))
  (while (re-search-forward "<a name[^\n>]+>" nil t)
    (replace-match "" t t)))

(defun gnus-html-wash-images ()
  (let (tag parameters string start end images url)
    (goto-char (point-min))
    ;; Search for all the images first.
    (while (re-search-forward "<img_alt \\([^>]*\\)>" nil t)
      (setq parameters (match-string 1)
	    start (match-beginning 0))
      (delete-region start (point))
      (when (search-forward "</img_alt>" (line-end-position) t)
	(delete-region (match-beginning 0) (match-end 0)))
      (setq end (point))
      (when (string-match "src=\"\\([^\"]+\\)" parameters)
	(setq url (match-string 1 parameters))
	(gnus-message 8 "gnus-html-wash-tags: fetching image URL %s" url)
	(if (string-match "^cid:\\(.*\\)" url)
	    ;; URLs with cid: have their content stashed in other
	    ;; parts of the MIME structure, so just insert them
	    ;; immediately.
	    (let ((handle (mm-get-content-id
			   (setq url (match-string 1 url))))
		  image)
	      (when handle
		(mm-with-part handle
		  (setq image (gnus-create-image (buffer-string)
						 nil t))))
	      (when image
		(let ((string (buffer-substring start end)))
		  (delete-region start end)
		  (gnus-put-image image (gnus-string-or string "*") 'cid)
		  (gnus-add-image 'cid image))))
	  ;; Normal, external URL.
	  (if (gnus-html-image-url-blocked-p
	       url
	       (if (buffer-live-p gnus-summary-buffer)
		   (with-current-buffer gnus-summary-buffer
		     gnus-blocked-images)
		 gnus-blocked-images))
	      (progn
		(widget-convert-button
		 'link start end
		 :action 'gnus-html-insert-image
		 :help-echo url
		 :keymap gnus-html-image-map
		 :button-keymap gnus-html-image-map)
		(let ((overlay (gnus-make-overlay start end))
		      (spec (list url
				  (set-marker (make-marker) start)
				  (set-marker (make-marker) end))))
		  (gnus-overlay-put overlay 'local-map gnus-html-image-map)
		  (gnus-overlay-put overlay 'gnus-image spec)
		  (gnus-put-text-property
		   start end
		   'gnus-image spec)))
	    (let ((file (gnus-html-image-id url))
		  width height alt-text)
	      (when (string-match "height=\"?\\([0-9]+\\)" parameters)
		(setq height (string-to-number (match-string 1 parameters))))
	      (when (string-match "width=\"?\\([0-9]+\\)" parameters)
		(setq width (string-to-number (match-string 1 parameters))))
	      (when (string-match "\\(alt\\|title\\)=\"\\([^\"]+\\)"
				  parameters)
		(setq alt-text (match-string 2 parameters)))
	      ;; Don't fetch images that are really small.  They're
	      ;; probably tracking pictures.
	      (when (and (or (null height)
			     (> height 4))
			 (or (null width)
			     (> width 4)))
		(if (file-exists-p file)
		    ;; It's already cached, so just insert it.
		    (let ((string (buffer-substring start end)))
		      ;; Delete the IMG text.
		      (delete-region start end)
		      (gnus-html-put-image file (point) string url alt-text))
		  ;; We don't have it, so schedule it for fetching
		  ;; asynchronously.
		  (push (list url
			      (set-marker (make-marker) start)
			      (point-marker))
			images))))))))
    (when images
      (gnus-html-schedule-image-fetching (current-buffer) (nreverse images)))))

(defun gnus-html-wash-tags ()
  (let (tag parameters string start end images url)
    (gnus-html-pre-wash)
    (gnus-html-wash-images)

    (goto-char (point-min))
    ;; Then do the other tags.
    (while (re-search-forward "<\\([^ />]+\\)\\([^>]*\\)>" nil t)
      (setq tag (match-string 1)
	    parameters (match-string 2)
	    start (match-beginning 0))
      (when (plusp (length parameters))
	(set-text-properties 0 (1- (length parameters)) nil parameters))
      (delete-region start (point))
      (when (search-forward (concat "</" tag ">") nil t)
	(delete-region (match-beginning 0) (match-end 0)))
      (setq end (point))
      (cond
       ;; Fetch and insert a picture.
       ((equal tag "img_alt"))
       ;; Add a link.
       ((or (equal tag "a")
	    (equal tag "A"))
	(when (string-match "href=\"\\([^\"]+\\)" parameters)
	  (setq url (match-string 1 parameters))
          (gnus-message 8 "gnus-html-wash-tags: fetching link URL %s" url)
	  (gnus-article-add-button start end
				   'browse-url url
				   url)
	  (let ((overlay (gnus-make-overlay start end)))
	    (gnus-overlay-put overlay 'evaporate t)
	    (gnus-overlay-put overlay 'gnus-button-url url)
	    (gnus-put-text-property start end 'gnus-string url)
	    (when gnus-article-mouse-face
	      (gnus-overlay-put overlay 'mouse-face gnus-article-mouse-face)))))
       ;; The upper-case IMG_ALT is apparently just an artifact that
       ;; should be deleted.
       ((equal tag "IMG_ALT")
	(delete-region start end))
       ;; Whatever.  Just ignore the tag.
       (t
	))
      (goto-char start))
    (goto-char (point-min))
    ;; The output from -halfdump isn't totally regular, so strip
    ;; off any </pre_int>s that were left over.
    (while (re-search-forward "</pre_int>\\|</internal>" nil t)
      (replace-match "" t t))
    (mm-url-decode-entities)))

(defun gnus-html-insert-image ()
  "Fetch and insert the image under point."
  (interactive)
  (gnus-html-schedule-image-fetching
   (current-buffer) (list (get-text-property (point) 'gnus-image))))

(defun gnus-html-show-alt-text ()
  "Show the ALT text of the image under point."
  (interactive)
  (message "%s" (get-text-property (point) 'gnus-alt-text)))

(defun gnus-html-browse-image ()
  "Browse the image under point."
  (interactive)
  (browse-url (get-text-property (point) 'gnus-image)))

(defun gnus-html-browse-url ()
  "Browse the image under point."
  (interactive)
  (let ((url (get-text-property (point) 'gnus-string)))
    (if (not url)
	(message "No URL at point")
      (browse-url url))))

(defun gnus-html-schedule-image-fetching (buffer images)
  (gnus-message 8 "gnus-html-schedule-image-fetching: buffer %s, images %s"
                buffer images)
  (let* ((url (caar images))
	 (process (start-process
		   "images" nil "curl"
		   "-s" "--create-dirs"
		   "--location"
		   "--max-time" "60"
		   "-o" (gnus-html-image-id url)
		   (mm-url-decode-entities-string url))))
    (process-kill-without-query process)
    (set-process-sentinel process 'gnus-html-curl-sentinel)
    (gnus-set-process-plist process (list 'images images
					  'buffer buffer))))

(defun gnus-html-image-id (url)
  (expand-file-name (sha1 url) gnus-html-cache-directory))

(defun gnus-html-curl-sentinel (process event)
  (when (string-match "finished" event)
    (let* ((images (gnus-process-get process 'images))
	   (buffer (gnus-process-get process 'buffer))
	   (spec (pop images))
	   (file (gnus-html-image-id (car spec))))
      (when (and (buffer-live-p buffer)
		 ;; If the position of the marker is 1, then that
		 ;; means that the text it was in has been deleted;
		 ;; i.e., that the user has selected a different
		 ;; article before the image arrived.
		 (not (= (marker-position (cadr spec)) (point-min))))
	(with-current-buffer buffer
	  (let ((inhibit-read-only t)
		(string (buffer-substring (cadr spec) (caddr spec))))
	    (delete-region (cadr spec) (caddr spec))
	    (gnus-html-put-image file (cadr spec) string))))
      (when images
	(gnus-html-schedule-image-fetching buffer images)))))

(defun gnus-html-put-image (file point string &optional url alt-text)
  (when (gnus-graphic-display-p)
    (let* ((image (ignore-errors
		   (gnus-create-image file)))
	  (size (and image
		     (if (featurep 'xemacs)
			 (cons (glyph-width image) (glyph-height image))
		       (image-size image t)))))
      (save-excursion
	(goto-char point)
	(if (and image
		 ;; Kludge to avoid displaying 30x30 gif images, which
		 ;; seems to be a signal of a broken image.
		 (not (and (if (featurep 'xemacs)
			       (glyphp image)
			     (listp image))
			   (eq (if (featurep 'xemacs)
				   (let ((data (cdadar (specifier-spec-list
							(glyph-image image)))))
				     (and (vectorp data)
					  (aref data 0)))
				 (plist-get (cdr image) :type))
			       'gif)
			   (= (car size) 30)
			   (= (cdr size) 30))))
	    (let ((start (point)))
	      (setq image (gnus-html-rescale-image image file size))
	      (gnus-put-image image
			      (gnus-string-or string "*")
			      'external)
	      (let ((overlay (gnus-make-overlay start (point))))
		(gnus-overlay-put overlay 'local-map
				  gnus-html-displayed-image-map)
		(gnus-put-text-property start (point) 'gnus-alt-text alt-text)
		(when url
		  (gnus-put-text-property start (point) 'gnus-image url)))
	      (gnus-add-image 'external image)
	      t)
	  (insert string)
	  (when (fboundp 'find-image)
	    (setq image (find-image '((:type xpm :file "lock-broken.xpm"))))
	    (gnus-put-image image
			    (gnus-string-or string "*")
			    'internal)
	    (gnus-add-image 'internal image))
	  nil)))))

(defun gnus-html-rescale-image (image file size)
  (if (or (not (fboundp 'imagemagick-types))
	  (not (get-buffer-window (current-buffer))))
      image
    (let* ((width (car size))
	   (height (cdr size))
	   (edges (window-pixel-edges (get-buffer-window (current-buffer))))
	   (window-width (truncate (* gnus-max-image-proportion
				      (- (nth 2 edges) (nth 0 edges)))))
	   (window-height (truncate (* gnus-max-image-proportion
				       (- (nth 3 edges) (nth 1 edges)))))
	   scaled-image)
      (when (> height window-height)
	(setq image (or (create-image file 'imagemagick nil
				      :height window-height)
			image))
	(setq size (image-size image t)))
      (when (> (car size) window-width)
	(setq image (or
		     (create-image file 'imagemagick nil
				   :width window-width)
		     image)))
      image)))

(defun gnus-html-prune-cache ()
  (let ((total-size 0)
	files)
    (dolist (file (directory-files gnus-html-cache-directory t nil t))
      (let ((attributes (file-attributes file)))
	(unless (nth 0 attributes)
	  (incf total-size (nth 7 attributes))
	  (push (list (time-to-seconds (nth 5 attributes))
		      (nth 7 attributes) file)
		files))))
    (when (> total-size gnus-html-cache-size)
      (setq files (sort files (lambda (f1 f2)
				(< (car f1) (car f2)))))
      (dolist (file files)
	(when (> total-size gnus-html-cache-size)
	  (decf total-size (cadr file))
	  (delete-file (nth 2 file)))))))

(defun gnus-html-image-url-blocked-p (url blocked-images)
  "Find out if URL is blocked by BLOCKED-IMAGES."
  (let ((ret (and blocked-images
                  (string-match blocked-images url))))
    (if ret
        (gnus-message 8 "gnus-html-image-url-blocked-p: %s blocked by regex %s"
                      url blocked-images)
      (gnus-message 9 "gnus-html-image-url-blocked-p: %s passes regex %s"
                    url blocked-images))
    ret))

(defun gnus-html-show-images ()
  "Show any images that are in the HTML-rendered article buffer.
This only works if the article in question is HTML."
  (interactive)
  (gnus-with-article-buffer
    (let ((overlays (overlays-in (point-min) (point-max)))
	  overlay images)
      (while (setq overlay (pop overlays))
	(when (overlay-get overlay 'gnus-image)
	  (push (overlay-get overlay 'gnus-image) images)))
      (if (not images)
	  (message "No images to show")
	(gnus-html-schedule-image-fetching (current-buffer) images)))))

;;;###autoload
(defun gnus-html-prefetch-images (summary)
  (let (blocked-images urls)
    (when (buffer-live-p summary)
      (with-current-buffer summary
	(setq blocked-images gnus-blocked-images))
      (save-match-data
	(while (re-search-forward "<img.*src=[\"']\\([^\"']+\\)" nil t)
	  (let ((url (match-string 1)))
	    (unless (gnus-html-image-url-blocked-p url blocked-images)
              (unless (file-exists-p (gnus-html-image-id url))
                (push (mm-url-decode-entities-string url) urls)
                (push (gnus-html-image-id url) urls)
                (push "-o" urls)))))
	(let ((process
	       (apply 'start-process
		      "images" nil "curl"
		      "-s" "--create-dirs"
		      "--location"
		      "--max-time" "60"
		      urls)))
	  (process-kill-without-query process))))))

(provide 'gnus-html)

;;; gnus-html.el ends here
