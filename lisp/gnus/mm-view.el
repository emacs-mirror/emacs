;;; mm-view.el --- functions for viewing MIME objects
;; Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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

;;; Code:

(eval-when-compile (require 'cl))
(require 'mail-parse)
(require 'mailcap)
(require 'mm-bodies)
(require 'mm-decode)

(eval-and-compile
  (autoload 'gnus-article-prepare-display "gnus-art")
  (autoload 'vcard-parse-string "vcard")
  (autoload 'vcard-format-string "vcard")
  (autoload 'fill-flowed "flow-fill")
  (unless (fboundp 'diff-mode)
    (autoload 'diff-mode "diff-mode" "" t nil)))

;;;
;;; Functions for displaying various formats inline
;;;
(defun mm-inline-image-emacs (handle)
  (let ((b (point-marker))
	buffer-read-only)
    (insert "\n")
    (put-image (mm-get-image handle) b)
    (mm-handle-set-undisplayer
     handle
     `(lambda () (remove-images ,b (1+ ,b))))))

(defun mm-inline-image-xemacs (handle)
  (insert "\n")
  (forward-char -1)
  (let ((b (point))
	(annot (make-annotation (mm-get-image handle) nil 'text))
	buffer-read-only)
    (mm-handle-set-undisplayer
     handle
     `(lambda ()
	(let (buffer-read-only)
	  (delete-annotation ,annot)
	  (delete-region ,(set-marker (make-marker) b)
			 ,(set-marker (make-marker) (point))))))
    (set-extent-property annot 'mm t)
    (set-extent-property annot 'duplicable t)))

(eval-and-compile
  (if (featurep 'xemacs)
      (defalias 'mm-inline-image 'mm-inline-image-xemacs)
    (defalias 'mm-inline-image 'mm-inline-image-emacs)))

(defvar mm-w3-setup nil)
(defun mm-setup-w3 ()
  (unless mm-w3-setup
    (require 'w3)
    (w3-do-setup)
    (require 'url)
    (require 'w3-vars)
    (require 'url-vars)
    (setq mm-w3-setup t)))

(defun mm-inline-text (handle)
  (let ((type (mm-handle-media-subtype handle))
	text buffer-read-only)
    (cond
     ((equal type "html")
      (mm-setup-w3)
      (setq text (mm-get-part handle))
      (let ((b (point))
	    (url-standalone-mode t)
	    (url-gateway-unplugged t)
	    (url-current-object
	     (url-generic-parse-url (format "cid:%s" (mm-handle-id handle))))
	    (width (window-width))
	    (charset (mail-content-type-get
		      (mm-handle-type handle) 'charset)))
	(save-excursion
	  (insert text)
	  (save-restriction
	    (narrow-to-region b (point))
	    (goto-char (point-min))
	    (if (or (and (boundp 'w3-meta-content-type-charset-regexp)
			 (re-search-forward
			  w3-meta-content-type-charset-regexp nil t))
		    (and (boundp 'w3-meta-charset-content-type-regexp)
			 (re-search-forward
			  w3-meta-charset-content-type-regexp nil t)))
		(setq charset
		      (or (let ((bsubstr (buffer-substring-no-properties
					  (match-beginning 2)
					  (match-end 2))))
			    (if (fboundp 'w3-coding-system-for-mime-charset)
				(w3-coding-system-for-mime-charset bsubstr)
			      (mm-charset-to-coding-system bsubstr)))
			  charset)))
	    (delete-region (point-min) (point-max))
	    (insert (mm-decode-string text charset))
	    (save-window-excursion
	      (save-restriction
		(let ((w3-strict-width width)
		      ;; Don't let w3 set the global version of
		      ;; this variable.
		      (fill-column fill-column))
		  (condition-case var
		      (w3-region (point-min) (point-max))
		    (error
		     (delete-region (point-min) (point-max))
		     (let ((b (point))
			   (charset (mail-content-type-get
				     (mm-handle-type handle) 'charset)))
		       (if (or (eq charset 'gnus-decoded)
			       (eq mail-parse-charset 'gnus-decoded))
			   (save-restriction
			     (narrow-to-region (point) (point))
			     (mm-insert-part handle)
			     (goto-char (point-max)))
			 (insert (mm-decode-string (mm-get-part handle)
						   charset))))
		     (message
		      "Error while rendering html; showing as text/plain"))))))
	    (mm-handle-set-undisplayer
	     handle
	     `(lambda ()
		(let (buffer-read-only)
		  (if (functionp 'remove-specifier)
		      (mapcar (lambda (prop)
				(remove-specifier
				 (face-property 'default prop)
				 (current-buffer)))
			      '(background background-pixmap foreground)))
		  (delete-region ,(point-min-marker)
				 ,(point-max-marker)))))))))
     ((equal type "x-vcard")
      (mm-insert-inline
       handle
       (concat "\n-- \n"
	       (ignore-errors
		 (if (fboundp 'vcard-pretty-print)
		     (vcard-pretty-print (mm-get-part handle))
		   (vcard-format-string
		    (vcard-parse-string (mm-get-part handle)
					'vcard-standard-filter)))))))
     (t
      (let ((b (point))
	    (charset (mail-content-type-get
		      (mm-handle-type handle) 'charset)))
	(if (or (eq charset 'gnus-decoded)
		;; This is probably not entirely correct, but
		;; makes rfc822 parts with embedded multiparts work.
		(eq mail-parse-charset 'gnus-decoded))
	    (save-restriction
	      (narrow-to-region (point) (point))
	      (mm-insert-part handle)
	      (goto-char (point-max)))
	  (insert (mm-decode-string (mm-get-part handle) charset)))
	(when (and (equal type "plain")
		   (equal (cdr (assoc 'format (mm-handle-type handle)))
			  "flowed"))
	  (save-restriction
	    (narrow-to-region b (point))
	    (goto-char b)
	    (fill-flowed)
	    (goto-char (point-max))))
	(save-restriction
	  (narrow-to-region b (point))
	  (set-text-properties (point-min) (point-max) nil)
	  (when (or (equal type "enriched")
		    (equal type "richtext"))
	    (enriched-decode (point-min) (point-max)))
	  (mm-handle-set-undisplayer
	   handle
	   `(lambda ()
	      (let (buffer-read-only)
		(delete-region ,(point-min-marker)
			       ,(point-max-marker)))))))))))

(defun mm-insert-inline (handle text)
  "Insert TEXT inline from HANDLE."
  (let ((b (point)))
    (insert text)
    (mm-handle-set-undisplayer
     handle
     `(lambda ()
	(let (buffer-read-only)
	  (delete-region ,(set-marker (make-marker) b)
			 ,(set-marker (make-marker) (point))))))))

(defun mm-inline-audio (handle)
  (message "Not implemented"))

(defun mm-view-sound-file ()
  (message "Not implemented"))

(defun mm-w3-prepare-buffer ()
  (require 'w3)
  (let ((url-standalone-mode t)
	(url-gateway-unplugged t))
    (w3-prepare-buffer)))

(defun mm-view-message ()
  (mm-enable-multibyte)
  (let (handles)
    (let (gnus-article-mime-handles)
      ;; Double decode problem may happen.  See mm-inline-message.
      (run-hooks 'gnus-article-decode-hook)
      (gnus-article-prepare-display)
      (setq handles gnus-article-mime-handles))
    (when handles
      (setq gnus-article-mime-handles
	    (nconc gnus-article-mime-handles 
		   (if (listp (car handles)) 
		       handles (list handles))))))
  (fundamental-mode)
  (goto-char (point-min)))

(defun mm-inline-message (handle)
  (let ((b (point))
	(bolp (bolp))
	(charset (mail-content-type-get
		  (mm-handle-type handle) 'charset))
	gnus-displaying-mime handles)
    (when (and charset
	       (stringp charset))
      (setq charset (intern (downcase charset)))
      (when (eq charset 'us-ascii)
	(setq charset nil)))
    (save-excursion
      (save-restriction
	(narrow-to-region b b)
	(mm-insert-part handle)
	(let (gnus-article-mime-handles
	      ;; disable prepare hook
	      gnus-article-prepare-hook
	      (gnus-newsgroup-charset
	       (or charset gnus-newsgroup-charset)))
	  (run-hooks 'gnus-article-decode-hook)
	  (gnus-article-prepare-display)
	  (setq handles gnus-article-mime-handles))
	(goto-char (point-min))
	(unless bolp
	  (insert "\n"))
	(goto-char (point-max))
	(unless (bolp)
	  (insert "\n"))
	(insert "----------\n\n")
	(when handles
	  (setq gnus-article-mime-handles
		(nconc gnus-article-mime-handles 
		       (if (listp (car handles)) 
			   handles (list handles)))))
	(mm-handle-set-undisplayer
	 handle
	 `(lambda ()
	    (let (buffer-read-only)
	      (if (fboundp 'remove-specifier)
		  ;; This is only valid on XEmacs.
		  (mapcar (lambda (prop)
			    (remove-specifier
			     (face-property 'default prop) (current-buffer)))
			  '(background background-pixmap foreground)))
	      (delete-region ,(point-min-marker) ,(point-max-marker)))))))))

(defun mm-display-inline-fontify (handle mode)
  (let (text)
    (with-temp-buffer
      (mm-insert-part handle)
      (funcall mode)
      (font-lock-fontify-buffer)
      (when (fboundp 'extent-list)
	(map-extents (lambda (ext ignored)
		       (set-extent-property ext 'duplicable t)
		       nil)
		     nil nil nil nil nil 'text-prop))
      (setq text (buffer-string)))
    (mm-insert-inline handle text)))

(defun mm-display-patch-inline (handle)
  (mm-display-inline-fontify handle 'diff-mode))

(defun mm-display-elisp-inline (handle)
  (mm-display-inline-fontify handle 'emacs-lisp-mode))

(provide 'mm-view)

;;; mm-view.el ends here
