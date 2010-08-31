;;; gnus-html.el --- Quoted-Printable functions

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

(defcustom gnus-html-cache-directory (nnheader-concat gnus-directory "html-cache/")
  "Where Gnus will cache images it downloads from the web."
  :group 'gnus-art
  :type 'directory)

(defcustom gnus-html-cache-size 500000000
  "The size of the Gnus image cache."
  :group 'gnus-art
  :type 'integer)

(defcustom gnus-html-frame-width 70
  "What width to use when rendering HTML."
  :group 'gnus-art
  :type 'integer)

;;;###autoload
(defun gnus-article-html (handle)
  (let ((article-buffer (current-buffer)))
    (save-restriction
      (narrow-to-region (point) (point))
      (save-excursion
	(mm-with-part handle
	  (let* ((coding-system-for-read 'utf-8)
		 (coding-system-for-write 'utf-8)
		 (default-process-coding-system
		   (cons coding-system-for-read coding-system-for-write)))
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
				 "-o" "display_image=off"
				 "-T" "text/html"))))
      (gnus-html-wash-tags))))

(defun gnus-html-wash-tags ()
  (let (tag parameters string start end images)
    (mm-url-decode-entities)
    (goto-char (point-min))
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
       ((equal tag "img_alt")
	(when (string-match "src=\"\\([^\"]+\\)" parameters)
	  (setq parameters (match-string 1 parameters))
	  (when (or (null mm-w3m-safe-url-regexp)
		    (string-match mm-w3m-safe-url-regexp parameters))
	    (let ((file (gnus-html-image-id parameters)))
	      (if (file-exists-p file)
		  ;; It's already cached, so just insert it.
		  (when (gnus-html-put-image file (point))
		    ;; Delete the ALT text.
		    (delete-region start end))
		;; We don't have it, so schedule it for fetching
		;; asynchronously.
		(push (list parameters
			    (set-marker (make-marker) start)
			    (point-marker))
		      images))))))
       ;; Add a link.
       ((equal tag "a")
	(when (string-match "href=\"\\([^\"]+\\)" parameters)
	  (setq parameters (match-string 1 parameters))
	  (gnus-article-add-button start end
				   'browse-url parameters
				   parameters)
	  (let ((overlay (gnus-make-overlay start end)))
	    (gnus-overlay-put overlay 'evaporate t)
	    (gnus-overlay-put overlay 'gnus-button-url parameters)
	    (when gnus-article-mouse-face
	      (gnus-overlay-put overlay 'mouse-face gnus-article-mouse-face)))))
       ;; Whatever.  Just ignore the tag.
       (t
	))
      (goto-char start))
    (goto-char (point-min))
    ;; The output from -halfdump isn't totally regular, so strip
    ;; off any </pre_int>s that were left over.
    (while (re-search-forward "</pre_int>" nil t)
      (replace-match "" t t))
    (when images
      (gnus-html-schedule-image-fetching (current-buffer) (nreverse images)))))

(defun gnus-html-schedule-image-fetching (buffer images)
  (let* ((url (caar images))
	 (process (start-process
		   "images" nil "curl"
		   "-s" "--create-dirs"
		   "--location"
		   "--max-time" "60"
		   "-o" (gnus-html-image-id url)
		   url)))
    (process-kill-without-query process)
    (set-process-sentinel process 'gnus-html-curl-sentinel)
    (set-process-plist process (list 'images images
				     'buffer buffer))))

(defun gnus-html-image-id (url)
  (expand-file-name (sha1 url) gnus-html-cache-directory))

(defun gnus-html-curl-sentinel (process event)
  (when (string-match "finished" event)
    (let* ((images (getf (process-plist process) 'images))
	   (buffer (getf (process-plist process) 'buffer))
	   (spec (pop images))
	   (file (gnus-html-image-id (car spec))))
      (when (and (buffer-live-p buffer)
		 ;; If the position of the marker is 1, then that
		 ;; means that the text is was in has been deleted;
		 ;; i.e., that the user has selected a different
		 ;; article before the image arrived.
		 (not (= (marker-position (cadr spec)) 1)))
	(save-excursion
	  (set-buffer buffer)
	  (let ((buffer-read-only nil))
	    (when (gnus-html-put-image file (cadr spec))
	      (delete-region (1+ (cadr spec)) (caddr spec))))))
      (when images
	(gnus-html-schedule-image-fetching buffer images)))))

(defun gnus-html-put-image (file point)
  (when (display-graphic-p)
    (let ((image (ignore-errors
		   (gnus-create-image file))))
      (save-excursion
	(goto-char point)
	(if (and image
		 ;; Kludge to avoid displaying 30x30 gif images, which
		 ;; seems to be a signal of a broken image.
		 (not (and (eq (getf (cdr image) :type) 'gif)
			   (= (car (image-size image t)) 30)
			   (= (cdr (image-size image t)) 30))))
	    (progn
	      (gnus-put-image image)
	      t)
	  (when (fboundp 'find-image)
	    (gnus-put-image (find-image
			     '((:type xpm :file "lock-broken.xpm")))))
	  nil)))))

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

;;;###autoload
(defun gnus-html-prefetch-images (summary)
  (let (safe-url-regexp urls)
    (when (buffer-live-p summary)
      (save-excursion
	(set-buffer summary)
	(setq safe-url-regexp mm-w3m-safe-url-regexp))
      (save-match-data
	(while (re-search-forward "<img.*src=[\"']\\([^\"']+\\)" nil t)
	  (let ((url (match-string 1)))
	    (when (or (null safe-url-regexp)
		      (string-match safe-url-regexp url))
	      (unless (file-exists-p (gnus-html-image-id url))
		(push url urls)
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
