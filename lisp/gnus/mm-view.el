;;; mm-view.el --- functions for viewing MIME objects  -*- lexical-binding: t; -*-

;; Copyright (C) 1998-2022 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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

;;; Code:

(require 'cl-lib)
(require 'mail-parse)
(require 'mailcap)
(require 'mm-bodies)
(require 'mm-decode)
(require 'smime)
(require 'mml-smime)

(autoload 'gnus-completing-read "gnus-util")
(autoload 'gnus-article-prepare-display "gnus-art")
(autoload 'vcard-parse-string "vcard")
(autoload 'vcard-format-string "vcard")
(autoload 'fill-flowed "flow-fill")
(autoload 'html2text "html2text" nil t)

(defvar gnus-article-mime-handles)
(defvar gnus-newsgroup-charset)
(defvar smime-keys)
(defvar w3m-cid-retrieve-function-alist)
(defvar w3m-current-buffer)
(defvar w3m-display-inline-images)
(defvar w3m-minor-mode-map)

(defvar mm-text-html-renderer-alist
  '((shr . mm-shr)
    (w3m . mm-inline-text-html-render-with-w3m)
    (w3m-standalone . mm-inline-text-html-render-with-w3m-standalone)
    (gnus-w3m . gnus-article-html)
    (links . mm-inline-render-with-links)
    (lynx mm-inline-render-with-stdin nil
	  "lynx" "-dump" "-force_html" "-stdin" "-nolist")
    (html2text mm-inline-render-with-function html2text))
  "The attributes of renderer types for text/html.")

(defcustom mm-fill-flowed t
  "If non-nil, format=flowed articles will be displayed flowed."
  :type 'boolean
  :version "22.1"
  :group 'mime-display)

;; Not a defcustom, since it's usually overridden by the callers of
;; the mm functions.
(defvar mm-inline-font-lock t
  "If non-nil, do font locking of inline media types that support it.")

(defcustom mm-inline-large-images-proportion 0.9
  "Maximum proportion large images can occupy in the buffer.
This is only used if `mm-inline-large-images' is set to
`resize'."
  :type 'float
  :version "24.1"
  :group 'mime-display)

;;; Internal variables.

;;;
;;; Functions for displaying various formats inline
;;;

(autoload 'gnus-rescale-image "gnus-util")

(defun mm-inline-image (handle)
  (let ((b (point-marker))
	(inhibit-read-only t))
    (put-image
     (let ((image (mm-get-image handle)))
       (if (eq mm-inline-large-images 'resize)
           (gnus-rescale-image
	    image
	    (let ((edges (window-inside-pixel-edges
			  (get-buffer-window (current-buffer)))))
	      (cons (truncate (* mm-inline-large-images-proportion
				 (- (nth 2 edges) (nth 0 edges))))
		    (truncate (* mm-inline-large-images-proportion
				 (- (nth 3 edges) (nth 1 edges)))))))
         image))
     b)
    (insert "\n")
    (mm-handle-set-undisplayer
     handle
     (lambda ()
       (let ((inhibit-read-only t))
	 (remove-images b b)
	 (delete-region b (1+ b)))))))

(defvar mm-w3m-setup nil
  "Whether gnus-article-mode has been setup to use emacs-w3m.")

;; External.
(declare-function w3m-detect-meta-charset "ext:w3m" ())
(declare-function w3m-region "ext:w3m" (start end &optional url charset))

(defun mm-setup-w3m ()
  "Setup gnus-article-mode to use emacs-w3m."
  (unless mm-w3m-setup
    (require 'w3m)
    (unless (assq 'gnus-article-mode w3m-cid-retrieve-function-alist)
      (push (cons 'gnus-article-mode 'mm-w3m-cid-retrieve)
	    w3m-cid-retrieve-function-alist))
    (setq mm-w3m-setup t))
  (setq w3m-display-inline-images (not mm-html-inhibit-images)))

(defun mm-w3m-cid-retrieve-1 (url handle)
  (dolist (elem handle)
    (when (consp elem)
      (when (equal url (mm-handle-id elem))
	(mm-insert-part elem)
	(throw 'found-handle (mm-handle-media-type elem)))
      (when (and (stringp (car elem))
		 (equal "multipart" (mm-handle-media-supertype elem)))
	(mm-w3m-cid-retrieve-1 url elem)))))

(defun mm-w3m-cid-retrieve (url &rest _args)
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

(defvar w3m-force-redisplay)
(defvar w3m-safe-url-regexp)

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
	;; Put the mark meaning this part was rendered by emacs-w3m.
	(put-text-property (point-min) (point-max)
			   'mm-inline-text-html-with-w3m t)
	(when (and mm-inline-text-html-with-w3m-keymap
		   (boundp 'w3m-minor-mode-map)
		   w3m-minor-mode-map)
	  (if (and (boundp 'w3m-link-map)
		   w3m-link-map)
	      (let* ((start (point-min))
		     (end (point-max))
		     (on (get-text-property start 'w3m-href-anchor))
		     (map (copy-keymap w3m-link-map))
		     next)
		(set-keymap-parent map w3m-minor-mode-map)
		(while (< start end)
		  (if on
		      (progn
			(setq next (or (text-property-any start end
							  'w3m-href-anchor nil)
				       end))
			(put-text-property start next 'keymap map))
		    (setq next (or (text-property-not-all start end
							  'w3m-href-anchor nil)
				   end))
		    (put-text-property start next 'keymap w3m-minor-mode-map))
		  (setq start next
			on (not on))))
	    (put-text-property (point-min) (point-max)
			       'keymap w3m-minor-mode-map)))
	(mm-handle-set-undisplayer
	 handle
	 (let ((beg (point-min-marker))
	       (end (point-max-marker)))
	   (lambda ()
	     (let ((inhibit-read-only t))
	       (delete-region beg end)))))))))

(defcustom mm-w3m-standalone-supports-m17n-p 'undecided
  "T means the w3m command supports the m17n feature."
  :type '(choice (const nil) (const t) (other :tag "detect" undecided))
  :group 'mime-display)

(defun mm-w3m-standalone-supports-m17n-p ()
  "Say whether the w3m command supports the m17n feature."
  (cond ((eq mm-w3m-standalone-supports-m17n-p t) t)
	((eq mm-w3m-standalone-supports-m17n-p nil) nil)
	((condition-case nil
	     (let ((coding-system-for-write 'iso-2022-jp)
		   (coding-system-for-read 'iso-2022-jp)
		   (str (decode-coding-string "\
\e$B#D#o#e#s!!#w#3#m!!#s#u#p#p#o#r#t!!#m#1#7#n!)\e(B" 'iso-2022-jp)))
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
	(if (and charset
		 (setq cs (mm-charset-to-coding-system charset nil t))
		 (not (eq cs 'ascii)))
	    (setq charset (format "%s" (mm-coding-system-to-mime-charset cs)))
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
  (declare (obsolete nil "28.1"))
  ;; Delete the annoying three spaces preceding each line of links
  ;; output.
  (goto-char (point-min))
  (while (re-search-forward "^   " nil t)
    (delete-region (match-beginning 0) (match-end 0))))

(defun mm-inline-wash-with-file (post-func cmd &rest args)
  (declare (obsolete nil "28.1"))
  (with-suppressed-warnings ((lexical file))
    (dlet ((file (make-temp-file
	          (expand-file-name "mm" mm-tmp-directory))))
      (let ((coding-system-for-write 'binary))
        (write-region (point-min) (point-max) file nil 'silent))
      (delete-region (point-min) (point-max))
      (unwind-protect
	  (apply #'call-process cmd nil t nil
                 (mapcar (lambda (e) (eval e t)) args))
        (delete-file file))
      (and post-func (funcall post-func)))))

(defun mm-inline-wash-with-stdin (post-func cmd &rest args)
  (let ((coding-system-for-write 'binary))
    (apply #'call-process-region (point-min) (point-max)
	   cmd t t nil args))
  (and post-func (funcall post-func)))

(defun mm-inline-render-with-file (handle post-func cmd &rest args)
  (declare (obsolete nil "28.1"))
  (let ((source (mm-get-part handle)))
    (mm-insert-inline
     handle
     (mm-with-unibyte-buffer
       (insert source)
       (with-suppressed-warnings ((obsolete mm-inline-wash-with-file))
         (apply #'mm-inline-wash-with-file post-func cmd args))
       (buffer-string)))))

(defun mm-inline-render-with-links (handle)
  (let ((source (mm-get-part handle))
        file charset)
    (mm-insert-inline
     handle
     (with-temp-buffer
       (setq charset (mail-content-type-get (mm-handle-type handle) 'charset))
       (insert source)
       (unwind-protect
           (progn
             (setq file (make-temp-file (expand-file-name
                                         "mm" mm-tmp-directory)))
             (let ((coding-system-for-write 'binary))
               (write-region (point-min) (point-max) file nil 'silent))
             (delete-region (point-min) (point-max))
             (if charset
                 (with-environment-variables (("LANG" (format "en-US.%s"
                                                              charset)))
	           (call-process "links" nil t nil "-dump" file))
               (call-process "links" nil t nil "-dump" file))
             (goto-char (point-min))
             (while (re-search-forward "^   " nil t)
               (delete-region (match-beginning 0) (match-end 0))))
         (when (and file (file-exists-p file))
           (delete-file file)))
       (buffer-string)))))

(defun mm-inline-render-with-stdin (handle post-func cmd &rest args)
  (let ((source (mm-get-part handle)))
    (mm-insert-inline
     handle
     (mm-with-unibyte-buffer
       (insert source)
       (apply #'mm-inline-wash-with-stdin post-func cmd args)
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
  (if (stringp (car handle))
      (mapcar #'mm-inline-text-html (cdr handle))
    (let* ((func mm-text-html-renderer)
	   (entry (assq func mm-text-html-renderer-alist))
	   (inhibit-read-only t))
      (if entry
	  (setq func (cdr entry)))
      (cond
       ((null func)
	(mm-insert-inline handle (mm-get-part handle)))
       ((functionp func)
	(funcall func handle))
       (t
	(apply (car func) handle (cdr func)))))))

(defun mm-inline-text-vcard (handle)
  (let ((inhibit-read-only t))
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
	(inhibit-read-only t))
    (if (or (eq charset 'gnus-decoded)
	    ;; This is probably not entirely correct, but
	    ;; makes rfc822 parts with embedded multiparts work.
	    (eq mail-parse-charset 'gnus-decoded))
	(save-restriction
	  (narrow-to-region (point) (point))
	  (mm-insert-part handle)
	  (goto-char (point-max)))
      (mm-display-inline-fontify handle))
    (when (and mm-fill-flowed
	       (equal type "plain")
	       (equal (cdr (assoc 'format (mm-handle-type handle)))
		      "flowed"))
      (save-restriction
	(narrow-to-region b (point))
	(goto-char b)
	(fill-flowed nil (cl-equalp (cdr (assoc 'delsp (mm-handle-type handle)))
				    "yes"))
	(goto-char (point-max))))
    (save-restriction
      (narrow-to-region b (point))
      (when (member type '("enriched" "richtext"))
        (set-text-properties (point-min) (point-max) nil)
	(ignore-errors
	  (enriched-decode (point-min) (point-max))))
      (mm-handle-set-undisplayer
       handle
       (if (= (point-min) (point-max))
	   #'ignore
	 (let ((beg (copy-marker (point-min) t))
	       (end (point-max-marker)))
	   (lambda ()
	     (let ((inhibit-read-only t))
	       (delete-region beg end)))))))))

(defun mm-insert-inline (handle text)
  "Insert TEXT inline from HANDLE."
  (let ((b (point)))
    (insert text)
    (unless (bolp)
      (insert "\n"))
    (mm-handle-set-undisplayer
     handle
     (let ((beg (copy-marker b t))
           (end (point-marker)))
       (lambda ()
	 (let ((inhibit-read-only t))
	   (delete-region beg end)))))))

(defun mm-inline-audio (_handle)
  (message "Not implemented"))

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

(defvar mm-inline-message-prepare-function nil
  "Function called by `mm-inline-message' to do client specific setup.
It is called with two parameters -- the MIME handle and the charset.")

(defun mm-inline-message (handle)
  "Insert HANDLE (a message/rfc822 part) into the current buffer.
This function will call `mm-inline-message-prepare-function'
after inserting the part."
  (let ((b (point))
	(bolp (bolp))
	(charset (mail-content-type-get
		  (mm-handle-type handle) 'charset)))
    (when (and charset
	       (stringp charset))
      (setq charset (intern (downcase charset)))
      (when (eq charset 'us-ascii)
	(setq charset nil)))
    (save-excursion
      (save-restriction
	(narrow-to-region b b)
	(mm-insert-part handle)
        (when mm-inline-message-prepare-function
	  (funcall mm-inline-message-prepare-function handle charset))
	(goto-char (point-min))
	(unless bolp
	  (insert "\n"))
	(goto-char (point-max))
	(unless (bolp)
	  (insert "\n"))
	(insert "----------\n\n")
	(mm-handle-set-undisplayer
	 handle
	 (let ((beg (point-min-marker))
	       (end (point-max-marker)))
	   (lambda ()
	     (let ((inhibit-read-only t))
	       (delete-region beg end)))))))))

;; Shut up byte-compiler.
(defvar font-lock-mode-hook)
(defun mm-display-inline-fontify (handle &optional mode)
  "Insert HANDLE inline fontifying with MODE.
If MODE is not set, try to find mode automatically."
  (let ((charset (mail-content-type-get (mm-handle-type handle) 'charset))
	text coding-system ovs)
    (unless (eq charset 'gnus-decoded)
      (mm-with-unibyte-buffer
	(mm-insert-part handle)
	(mm-decompress-buffer
         (mm-handle-filename handle)
	 t t)
	(unless charset
	  (setq coding-system (mm-find-buffer-file-coding-system)))
	(setq text (buffer-string))))
    (with-temp-buffer
      (buffer-disable-undo)
      (mm-enable-multibyte)
      (insert (cond ((eq charset 'gnus-decoded)
		     (with-current-buffer (mm-handle-buffer handle)
		       (buffer-string)))
		    (coding-system
		     (decode-coding-string text coding-system))
                    (t
                     (mm-decode-string text (or charset 'undecided)))))
      (let ((font-lock-verbose nil)     ; font-lock is a bit too verbose.
	    (enable-local-variables nil))
        ;; We used to set font-lock-mode-hook to nil to avoid enabling
        ;; support modes, but now that we use font-lock-ensure, support modes
        ;; aren't a problem any more.  So we could probably get rid of this
        ;; setting now, but it seems harmless and potentially still useful.
	(setq-local font-lock-mode-hook nil)
        (setq buffer-file-name (mm-handle-filename handle))
	(with-demoted-errors
	    (if mode
                (save-window-excursion
                  ;; According to Katsumi Yamaoka <yamaoka@jpl.org>, org-mode
                  ;; requires the buffer to be temporarily displayed here, but
                  ;; I could not reproduce this problem.  Furthermore, if
                  ;; there's such a problem, we should fix org-mode rather than
                  ;; use switch-to-buffer which can have undesirable
                  ;; side-effects!
                  ;;(switch-to-buffer (current-buffer))
	          (funcall mode))
	    (let ((auto-mode-alist
		   (delq (rassq 'doc-view-mode-maybe auto-mode-alist)
			 (copy-sequence auto-mode-alist))))
	      ;; Don't run hooks that might assume buffer-file-name
	      ;; really associates buffer with a file (bug#39190).
	      (delay-mode-hooks (set-auto-mode))
	      (setq mode major-mode)))
	  ;; Do not fontify if the guess mode is fundamental.
	  (when (and (not (eq major-mode 'fundamental-mode))
		     mm-inline-font-lock)
	    (font-lock-ensure))))
      (setq text (buffer-string))
      (when (eq mode 'diff-mode)
	(setq ovs (mapcar (lambda (ov) (list ov (overlay-start ov)
	                                        (overlay-end ov)))
	                  (overlays-in (point-min) (point-max)))))
      ;; Set buffer unmodified to avoid confirmation when killing the
      ;; buffer.
      (set-buffer-modified-p nil))
    (let ((b (- (point) (save-restriction (widen) (point-min)))))
      (mm-insert-inline handle text)
      (dolist (ov ovs)
	(move-overlay (nth 0 ov) (+ (nth 1 ov) b)
	                         (+ (nth 2 ov) b) (current-buffer))))))

;; Shouldn't these functions check whether the user even wants to use
;; font-lock?  Also, it would be nice to change for the size of the
;; fontified region.

(defun mm-display-patch-inline (handle)
  (mm-display-inline-fontify handle 'diff-mode))

(defun mm-display-elisp-inline (handle)
  (mm-display-inline-fontify handle 'emacs-lisp-mode))

(defun mm-display-dns-inline (handle)
  (mm-display-inline-fontify handle 'dns-mode))

(defun mm-display-org-inline (handle)
  "Show an Org mode text from HANDLE inline."
  (mm-display-inline-fontify handle 'org-mode))

(defun mm-display-shell-script-inline (handle)
  "Show a shell script from HANDLE inline."
  (mm-display-inline-fontify handle 'shell-script-mode))

(defun mm-display-javascript-inline (handle)
  "Show JavaScript code from HANDLE inline."
  (mm-display-inline-fontify handle 'javascript-mode))

;;      id-signedData OBJECT IDENTIFIER ::= { iso(1) member-body(2)
;;          us(840) rsadsi(113549) pkcs(1) pkcs7(7) 2 }
(defvar mm-pkcs7-signed-magic
  (concat
    "0"
    "\\(\\(\x80\\)"
    "\\|\\(\x81\\(.\\|\n\\)\\{1\\}\\)"
    "\\|\\(\x82\\(.\\|\n\\)\\{2\\}\\)"
    "\\|\\(\x83\\(.\\|\n\\)\\{3\\}\\)"
    "\\)"
    "\x06\x09\\*\x86H\x86\xf7\x0d\x01\x07\x02"))

;;      id-envelopedData OBJECT IDENTIFIER ::= { iso(1) member-body(2)
;;          us(840) rsadsi(113549) pkcs(1) pkcs7(7) 3 }
(defvar mm-pkcs7-enveloped-magic
  (concat
    "0"
    "\\(\\(\x80\\)"
    "\\|\\(\x81\\(.\\|\n\\)\\{1\\}\\)"
    "\\|\\(\x82\\(.\\|\n\\)\\{2\\}\\)"
    "\\|\\(\x83\\(.\\|\n\\)\\{3\\}\\)"
    "\\)"
    "\x06\x09\\*\x86H\x86\xf7\x0d\x01\x07\x03"))

(defun mm-view-pkcs7-get-type (handle)
  (mm-with-unibyte-buffer
    (mm-insert-part handle)
    (cond ((looking-at mm-pkcs7-enveloped-magic)
	   'enveloped)
	  ((looking-at mm-pkcs7-signed-magic)
	   'signed)
	  (t
	   (error "Could not identify PKCS#7 type")))))

(defun mm-view-pkcs7 (handle &optional from)
  (cl-case (mm-view-pkcs7-get-type handle)
    (enveloped (mm-view-pkcs7-decrypt handle from))
    (signed (mm-view-pkcs7-verify handle))
    (otherwise (error "Unknown or unimplemented PKCS#7 type"))))

(defun mm-view-pkcs7-verify (handle)
  (let ((verified nil))
    (if (eq mml-smime-use 'epg)
	;; Use EPG/gpgsm
	(insert
	 (with-temp-buffer
	   (insert-buffer-substring (mm-handle-buffer handle))
	   (goto-char (point-min))
	   (let ((part (base64-decode-string (buffer-string)))
		 (context (epg-make-context 'CMS)))
	     (prog1
		 (epg-verify-string context part)
	       (let ((result (car (epg-context-result-for context 'verify))))
		 (mm-sec-status
		  'gnus-info (epg-signature-status result)
		  'gnus-details
		  (format "%s:%s" (epg-signature-validity result)
			  (epg-signature-key-id result))))))))
      (with-temp-buffer
	(insert "MIME-Version: 1.0\n")
	(mm-insert-headers "application/pkcs7-mime" "base64" "smime.p7m")
	(insert-buffer-substring (mm-handle-buffer handle))
	(setq verified (smime-verify-region (point-min) (point-max))))
      (if verified
	  (insert verified)
	(insert-buffer-substring smime-details-buffer)))
    t))

(autoload 'epg-decrypt-string "epg")

(defun mm-view-pkcs7-decrypt (handle &optional from)
  (insert-buffer-substring (mm-handle-buffer handle))
  (goto-char (point-min))
  (if (eq mml-smime-use 'epg)
      ;; Use EPG/gpgsm
      (let ((part (base64-decode-string (buffer-string))))
	(erase-buffer)
	(insert (epg-decrypt-string (epg-make-context 'CMS) part)))
    ;; Use openssl
    (insert "MIME-Version: 1.0\n")
    (mm-insert-headers "application/pkcs7-mime" "base64" "smime.p7m")
    (smime-decrypt-region
     (point-min) (point-max)
     (if (= (length smime-keys) 1)
	 (cadar smime-keys)
       (smime-get-key-by-email
	(gnus-completing-read
	 "Decipher using key"
	 smime-keys nil nil nil (car-safe (car-safe smime-keys)))))
     from))
  (goto-char (point-min))
  (while (search-forward "\r\n" nil t)
    (replace-match "\n"))
  (goto-char (point-min)))

(provide 'mm-view)

;;; mm-view.el ends here
