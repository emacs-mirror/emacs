;;; mm-view.el --- functions for viewing MIME objects

;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

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
  (autoload 'html2text "html2text" nil t)
  (unless (fboundp 'diff-mode)
    (autoload 'diff-mode "diff-mode" "" t nil)))

(defvar gnus-article-mime-handles)
(defvar gnus-newsgroup-charset)
(defvar smime-keys)
(defvar w3m-cid-retrieve-function-alist)
(defvar w3m-current-buffer)
(defvar w3m-display-inline-images)
(defvar w3m-minor-mode-map)

(defvar mm-text-html-renderer-alist
  '((w3  . mm-inline-text-html-render-with-w3)
    (w3m . mm-inline-text-html-render-with-w3m)
    (w3m-standalone . mm-inline-text-html-render-with-w3m-standalone)
    (links mm-inline-render-with-file
	   mm-links-remove-leading-blank
	   "links" "-dump" file)
    (lynx  mm-inline-render-with-stdin nil
	   "lynx" "-dump" "-force_html" "-stdin" "-nolist")
    (html2text  mm-inline-render-with-function html2text))
  "The attributes of renderer types for text/html.")

(defvar mm-text-html-washer-alist
  '((w3  . gnus-article-wash-html-with-w3)
    (w3m . gnus-article-wash-html-with-w3m)
    (w3m-standalone . gnus-article-wash-html-with-w3m-standalone)
    (links mm-inline-wash-with-file
	   mm-links-remove-leading-blank
	   "links" "-dump" file)
    (lynx  mm-inline-wash-with-stdin nil
	   "lynx" "-dump" "-force_html" "-stdin" "-nolist")
    (html2text  html2text))
  "The attributes of washer types for text/html.")

(defcustom mm-fill-flowed t
  "If non-nil an format=flowed article will be displayed flowed."
  :type 'boolean
  :version "22.1"
  :group 'mime-display)

;;; Internal variables.

;;;
;;; Functions for displaying various formats inline
;;;

(defun mm-inline-image-emacs (handle)
  (let ((b (point-marker))
	buffer-read-only)
    (put-image (mm-get-image handle) b)
    (insert "\n\n")
    (mm-handle-set-undisplayer
     handle
     `(lambda ()
	(let ((b ,b)
	      buffer-read-only)
	  (remove-images b b)
	  (delete-region b (+ b 2)))))))

(defun mm-inline-image-xemacs (handle)
  (insert "\n\n")
  (forward-char -2)
  (let ((annot (make-annotation (mm-get-image handle) nil 'text))
	buffer-read-only)
    (mm-handle-set-undisplayer
     handle
     `(lambda ()
	(let ((b ,(point-marker))
	      buffer-read-only)
	  (delete-annotation ,annot)
	  (delete-region (- b 2) b))))
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

(defun mm-inline-text-html-render-with-w3 (handle)
  (mm-setup-w3)
  (let ((text (mm-get-part handle))
	(b (point))
	(url-standalone-mode t)
	(url-gateway-unplugged t)
	(w3-honor-stylesheets nil)
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
	      (if (or debug-on-error debug-on-quit)
		  (w3-region (point-min) (point-max))
		(condition-case ()
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
		    "Error while rendering html; showing as text/plain")))))))
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

(defvar mm-w3m-setup nil
  "Whether gnus-article-mode has been setup to use emacs-w3m.")

(defun mm-setup-w3m ()
  "Setup gnus-article-mode to use emacs-w3m."
  (unless mm-w3m-setup
    (require 'w3m)
    (unless (assq 'gnus-article-mode w3m-cid-retrieve-function-alist)
      (push (cons 'gnus-article-mode 'mm-w3m-cid-retrieve)
	    w3m-cid-retrieve-function-alist))
    (setq mm-w3m-setup t))
  (setq w3m-display-inline-images mm-inline-text-html-with-images))

(defun mm-w3m-cid-retrieve-1 (url handle)
  (dolist (elem handle)
    (when (consp elem)
      (when (equal url (mm-handle-id elem))
	(mm-insert-part elem)
	(throw 'found-handle (mm-handle-media-type elem)))
      (when (and (stringp (car elem))
		 (equal "multipart" (mm-handle-media-supertype elem)))
	(mm-w3m-cid-retrieve-1 url elem)))))

(defun mm-w3m-cid-retrieve (url &rest args)
  "Insert a content pointed by URL if it has the cid: scheme."
  (when (string-match "\\`cid:" url)
    (or (catch 'found-handle
	  (mm-w3m-cid-retrieve-1
	   (setq url (concat "<" (substring url (match-end 0)) ">"))
	   (with-current-buffer w3m-current-buffer
	     gnus-article-mime-handles)))
	(prog1
	    nil
	  (message "Failed to find \"Content-ID: %s\"" url)))))

(defun mm-inline-text-html-render-with-w3m (handle)
  "Render a text/html part using emacs-w3m."
  (mm-setup-w3m)
  (let ((text (mm-get-part handle))
	(b (point))
	(charset (or (mail-content-type-get (mm-handle-type handle) 'charset)
		     mail-parse-charset)))
    (save-excursion
      (insert (if charset (mm-decode-string text charset) text))
      (save-restriction
	(narrow-to-region b (point))
	(unless charset
	  (goto-char (point-min))
	  (when (setq charset (w3m-detect-meta-charset))
	    (delete-region (point-min) (point-max))
	    (insert (mm-decode-string text charset))))
	(let ((w3m-safe-url-regexp mm-w3m-safe-url-regexp)
	      w3m-force-redisplay)
	  (w3m-region (point-min) (point-max) nil charset))
	(when (and mm-inline-text-html-with-w3m-keymap
		   (boundp 'w3m-minor-mode-map)
		   w3m-minor-mode-map)
	  (add-text-properties
	   (point-min) (point-max)
	   (list 'keymap w3m-minor-mode-map
		 ;; Put the mark meaning this part was rendered by emacs-w3m.
		 'mm-inline-text-html-with-w3m t)))
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

(defvar mm-w3m-standalone-supports-m17n-p (if (featurep 'mule) 'undecided)
  "*T means the w3m command supports the m17n feature.")

(defun mm-w3m-standalone-supports-m17n-p ()
  "Say whether the w3m command supports the m17n feature."
  (cond ((eq mm-w3m-standalone-supports-m17n-p t) t)
	((eq mm-w3m-standalone-supports-m17n-p nil) nil)
	((not (featurep 'mule)) (setq mm-w3m-standalone-supports-m17n-p nil))
	((condition-case nil
	     (let ((coding-system-for-write 'iso-2022-jp)
		   (coding-system-for-read 'iso-2022-jp)
		   (str (mm-decode-coding-string "\
\e$B#D#o#e#s!!#w#3#m!!#s#u#p#p#o#r#t#s!!#m#1#7#n!)\e(B" 'iso-2022-jp)))
	       (mm-with-multibyte-buffer
		 (insert str)
		 (call-process-region
		  (point-min) (point-max) "w3m" t t nil "-dump"
		  "-T" "text/html" "-I" "iso-2022-jp" "-O" "iso-2022-jp")
		 (goto-char (point-min))
		 (search-forward str nil t)))
	   (error nil))
	 (setq mm-w3m-standalone-supports-m17n-p t))
	(t
	 ;;(message "You had better upgrade your w3m command")
	 (setq mm-w3m-standalone-supports-m17n-p nil))))

(defun mm-inline-text-html-render-with-w3m-standalone (handle)
  "Render a text/html part using w3m."
  (if (mm-w3m-standalone-supports-m17n-p)
      (let ((source (mm-get-part handle))
	    (charset (or (mail-content-type-get (mm-handle-type handle)
						'charset)
			 (symbol-name mail-parse-charset)))
	    cs)
	(unless (and charset
		     (setq cs (mm-charset-to-coding-system charset))
		     (not (eq cs 'ascii)))
	  ;; The default.
	  (setq charset "iso-8859-1"
		cs 'iso-8859-1))
	(mm-insert-inline
	 handle
	 (mm-with-unibyte-buffer
	   (insert source)
	   (mm-enable-multibyte)
	   (let ((coding-system-for-write 'binary)
		 (coding-system-for-read cs))
	     (call-process-region
	      (point-min) (point-max)
	      "w3m" t t nil "-dump" "-T" "text/html"
	      "-I" charset "-O" charset))
	   (buffer-string))))
    (mm-inline-render-with-stdin handle nil "w3m" "-dump" "-T" "text/html")))

(defun mm-links-remove-leading-blank ()
  ;; Delete the annoying three spaces preceding each line of links
  ;; output.
  (goto-char (point-min))
  (while (re-search-forward "^   " nil t)
    (delete-region (match-beginning 0) (match-end 0))))

(defun mm-inline-wash-with-file (post-func cmd &rest args)
  (let ((file (mm-make-temp-file
	       (expand-file-name "mm" mm-tmp-directory))))
    (let ((coding-system-for-write 'binary))
      (write-region (point-min) (point-max) file nil 'silent))
    (delete-region (point-min) (point-max))
    (unwind-protect
	(apply 'call-process cmd nil t nil (mapcar 'eval args))
      (delete-file file))
    (and post-func (funcall post-func))))

(defun mm-inline-wash-with-stdin (post-func cmd &rest args)
  (let ((coding-system-for-write 'binary))
    (apply 'call-process-region (point-min) (point-max)
	   cmd t t nil args))
  (and post-func (funcall post-func)))

(defun mm-inline-render-with-file (handle post-func cmd &rest args)
  (let ((source (mm-get-part handle)))
    (mm-insert-inline
     handle
     (mm-with-unibyte-buffer
       (insert source)
       (apply 'mm-inline-wash-with-file post-func cmd args)
       (buffer-string)))))

(defun mm-inline-render-with-stdin (handle post-func cmd &rest args)
  (let ((source (mm-get-part handle)))
    (mm-insert-inline
     handle
     (mm-with-unibyte-buffer
       (insert source)
       (apply 'mm-inline-wash-with-stdin post-func cmd args)
       (buffer-string)))))

(defun mm-inline-render-with-function (handle func &rest args)
  (let ((source (mm-get-part handle))
	(charset (or (mail-content-type-get (mm-handle-type handle) 'charset)
		     mail-parse-charset)))
    (mm-insert-inline
     handle
     (mm-with-multibyte-buffer
       (insert (if charset
		   (mm-decode-string source charset)
		 source))
       (apply func args)
       (buffer-string)))))

(defun mm-inline-text-html (handle)
  (let* ((func (or mm-inline-text-html-renderer mm-text-html-renderer))
	 (entry (assq func mm-text-html-renderer-alist))
	 buffer-read-only)
    (if entry
	(setq func (cdr entry)))
    (cond
     ((functionp func)
      (funcall func handle))
     (t
      (apply (car func) handle (cdr func))))))

(defun mm-inline-text-vcard (handle)
  (let (buffer-read-only)
    (mm-insert-inline
     handle
     (concat "\n-- \n"
	     (ignore-errors
	       (if (fboundp 'vcard-pretty-print)
		   (vcard-pretty-print (mm-get-part handle))
		 (vcard-format-string
		  (vcard-parse-string (mm-get-part handle)
				      'vcard-standard-filter))))))))

(defun mm-inline-text (handle)
  (let ((b (point))
	(type (mm-handle-media-subtype handle))
	(charset (mail-content-type-get
		  (mm-handle-type handle) 'charset))
	buffer-read-only)
    (if (or (eq charset 'gnus-decoded)
	    ;; This is probably not entirely correct, but
	    ;; makes rfc822 parts with embedded multiparts work.
	    (eq mail-parse-charset 'gnus-decoded))
	(save-restriction
	  (narrow-to-region (point) (point))
	  (mm-insert-part handle)
	  (goto-char (point-max)))
      (insert (mm-decode-string (mm-get-part handle) charset)))
    (when (and mm-fill-flowed
	       (equal type "plain")
	       (equal (cdr (assoc 'format (mm-handle-type handle)))
		      "flowed"))
      (save-restriction
	(narrow-to-region b (point))
	(goto-char b)
	(fill-flowed)
	(goto-char (point-max))))
    (save-restriction
      (narrow-to-region b (point))
      (when (or (equal type "enriched")
		(equal type "richtext"))
	(set-text-properties (point-min) (point-max) nil)
	(ignore-errors
	  (enriched-decode (point-min) (point-max))))
      (mm-handle-set-undisplayer
       handle
       `(lambda ()
	  (let (buffer-read-only)
	    (delete-region ,(point-min-marker)
			   ,(point-max-marker))))))))

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
	(url-gateway-unplugged t)
	(w3-honor-stylesheets nil))
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
	    (mm-merge-handles gnus-article-mime-handles handles))))
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
	       (unless (eq charset 'gnus-decoded) ;; mm-uu might set it.
		 (or charset gnus-newsgroup-charset))))
	  (let ((gnus-original-article-buffer (mm-handle-buffer handle)))
	    (run-hooks 'gnus-article-decode-hook))
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
		(mm-merge-handles gnus-article-mime-handles handles)))
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
    ;; XEmacs @#$@ version of font-lock refuses to fully turn itself
    ;; on for buffers whose name begins with " ".  That's why we use
    ;; save-current-buffer/get-buffer-create rather than
    ;; with-temp-buffer.
    (save-current-buffer
      (set-buffer (generate-new-buffer "*fontification*"))
      (unwind-protect
	  (progn
	    (buffer-disable-undo)
	    (mm-insert-part handle)
	    (require 'font-lock)
	    (let ((font-lock-maximum-size nil)
		  ;; Disable support modes, e.g., jit-lock, lazy-lock, etc.
		  (font-lock-mode-hook nil)
		  (font-lock-support-mode nil)
		  ;; I find font-lock a bit too verbose.
		  (font-lock-verbose nil))
	      (funcall mode)
	      ;; The mode function might have already turned on font-lock.
	      (unless (symbol-value 'font-lock-mode)
		(font-lock-fontify-buffer)))
	    ;; By default, XEmacs font-lock uses non-duplicable text
	    ;; properties.  This code forces all the text properties
	    ;; to be copied along with the text.
	    (when (fboundp 'extent-list)
	      (map-extents (lambda (ext ignored)
			     (set-extent-property ext 'duplicable t)
			     nil)
			   nil nil nil nil nil 'text-prop))
	    (setq text (buffer-string)))
	(kill-buffer (current-buffer))))
    (mm-insert-inline handle text)))

;; Shouldn't these functions check whether the user even wants to use
;; font-lock?  At least under XEmacs, this fontification is pretty
;; much unconditional.  Also, it would be nice to change for the size
;; of the fontified region.

(defun mm-display-patch-inline (handle)
  (mm-display-inline-fontify handle 'diff-mode))

(defun mm-display-elisp-inline (handle)
  (mm-display-inline-fontify handle 'emacs-lisp-mode))

;;      id-signedData OBJECT IDENTIFIER ::= { iso(1) member-body(2)
;;          us(840) rsadsi(113549) pkcs(1) pkcs7(7) 2 }
(defvar mm-pkcs7-signed-magic
  (mm-string-as-unibyte
   (apply 'concat
	  (mapcar 'char-to-string
		  (list ?\x30 ?\x5c ?\x28 ?\x80 ?\x5c ?\x7c ?\x81 ?\x2e ?\x5c
			?\x7c ?\x82 ?\x2e ?\x2e ?\x5c ?\x7c ?\x83 ?\x2e ?\x2e
			?\x2e ?\x5c ?\x29 ?\x06 ?\x09 ?\x5c ?\x2a ?\x86 ?\x48
			?\x86 ?\xf7 ?\x0d ?\x01 ?\x07 ?\x02)))))

;;      id-envelopedData OBJECT IDENTIFIER ::= { iso(1) member-body(2)
;;          us(840) rsadsi(113549) pkcs(1) pkcs7(7) 3 }
(defvar mm-pkcs7-enveloped-magic
  (mm-string-as-unibyte
   (apply 'concat
	  (mapcar 'char-to-string
		  (list ?\x30 ?\x5c ?\x28 ?\x80 ?\x5c ?\x7c ?\x81 ?\x2e ?\x5c
			?\x7c ?\x82 ?\x2e ?\x2e ?\x5c ?\x7c ?\x83 ?\x2e ?\x2e
			?\x2e ?\x5c ?\x29 ?\x06 ?\x09 ?\x5c ?\x2a ?\x86 ?\x48
			?\x86 ?\xf7 ?\x0d ?\x01 ?\x07 ?\x03)))))

(defun mm-view-pkcs7-get-type (handle)
  (mm-with-unibyte-buffer
    (mm-insert-part handle)
    (cond ((looking-at mm-pkcs7-enveloped-magic)
	   'enveloped)
	  ((looking-at mm-pkcs7-signed-magic)
	   'signed)
	  (t
	   (error "Could not identify PKCS#7 type")))))

(defun mm-view-pkcs7 (handle)
  (case (mm-view-pkcs7-get-type handle)
    (enveloped (mm-view-pkcs7-decrypt handle))
    (signed (mm-view-pkcs7-verify handle))
    (otherwise (error "Unknown or unimplemented PKCS#7 type"))))

(defun mm-view-pkcs7-verify (handle)
  ;; A bogus implementation of PKCS#7. FIXME::
  (mm-insert-part handle)
  (goto-char (point-min))
  (if (search-forward "Content-Type: " nil t)
      (delete-region (point-min) (match-beginning 0)))
  (goto-char (point-max))
  (if (re-search-backward "--\r?\n?" nil t)
      (delete-region (match-end 0) (point-max)))
  (goto-char (point-min))
  (while (search-forward "\r\n" nil t)
    (replace-match "\n"))
  (message "Verify signed PKCS#7 message is unimplemented.")
  (sit-for 1)
  t)

(autoload 'gnus-completing-read-maybe-default "gnus-util" nil nil 'macro)

(defun mm-view-pkcs7-decrypt (handle)
  (insert-buffer-substring (mm-handle-buffer handle))
  (goto-char (point-min))
  (insert "MIME-Version: 1.0\n")
  (mm-insert-headers "application/pkcs7-mime" "base64" "smime.p7m")
  (smime-decrypt-region
   (point-min) (point-max)
   (if (= (length smime-keys) 1)
       (cadar smime-keys)
     (smime-get-key-by-email
      (gnus-completing-read-maybe-default
       (concat "Decipher using key"
	       (if smime-keys
		   (concat " (default " (caar smime-keys) "): ")
		 ": "))
       smime-keys nil nil nil nil (car-safe (car-safe smime-keys))))))
  (goto-char (point-min))
  (while (search-forward "\r\n" nil t)
    (replace-match "\n"))
  (goto-char (point-min)))

(provide 'mm-view)

;;; arch-tag: b60e749a-d05c-47f2-bccd-bdaa59327cb2
;;; mm-view.el ends here
