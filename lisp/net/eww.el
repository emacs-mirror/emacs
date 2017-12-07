;;; eww.el --- Emacs Web Wowser  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2017 Free Software Foundation, Inc.

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

;;; Code:

(require 'cl-lib)
(require 'format-spec)
(require 'shr)
(require 'url)
(require 'url-queue)
(require 'url-util)			; for url-get-url-at-point
(require 'mm-url)
(require 'puny)
(eval-when-compile (require 'subr-x)) ;; for string-trim

(defgroup eww nil
  "Emacs Web Wowser"
  :version "25.1"
  :link '(custom-manual "(eww) Top")
  :group 'web
  :prefix "eww-")

(defcustom eww-header-line-format "%t: %u"
  "Header line format.
- %t is replaced by the title.
- %u is replaced by the URL."
  :version "24.4"
  :group 'eww
  :type 'string)

(defcustom eww-search-prefix "https://duckduckgo.com/html/?q="
  "Prefix URL to search engine."
  :version "24.4"
  :group 'eww
  :type 'string)

(defcustom eww-download-directory "~/Downloads/"
  "Directory where files will downloaded."
  :version "24.4"
  :group 'eww
  :type 'directory)

;;;###autoload
(defcustom eww-suggest-uris
  '(eww-links-at-point
    url-get-url-at-point
    eww-current-url)
  "List of functions called to form the list of default URIs for `eww'.
Each of the elements is a function returning either a string or a list
of strings.  The results will be joined into a single list with
duplicate entries (if any) removed."
  :version "25.1"
  :group 'eww
  :type 'hook
  :options '(eww-links-at-point
             url-get-url-at-point
             eww-current-url))

(defcustom eww-bookmarks-directory user-emacs-directory
  "Directory where bookmark files will be stored."
  :version "25.1"
  :group 'eww
  :type 'directory)

(defcustom eww-desktop-remove-duplicates t
  "Whether to remove duplicates from the history when saving desktop data.
If non-nil, repetitive EWW history entries (comprising of the URI, the
title, and the point position) will not be saved as part of the Emacs
desktop.  Otherwise, such entries will be retained."
  :version "25.1"
  :group 'eww
  :type 'boolean)

(defcustom eww-restore-desktop nil
  "How to restore EWW buffers on `desktop-restore'.
If t or `auto', the buffers will be reloaded automatically.
If nil, buffers will require manual reload, and will contain the text
specified in `eww-restore-reload-prompt' instead of the actual Web
page contents."
  :version "25.1"
  :group 'eww
  :type '(choice (const :tag "Restore all automatically" t)
                 (const :tag "Require manual reload" nil)))

(defcustom eww-restore-reload-prompt
  "\n\n *** Use \\[eww-reload] to reload this buffer. ***\n"
  "The string to put in the buffers not reloaded on `desktop-restore'.
This prompt will be used if `eww-restore-desktop' is nil.

The string will be passed through `substitute-command-keys'."
  :version "25.1"
  :group 'eww
  :type 'string)

(defcustom eww-history-limit 50
  "Maximum number of entries to retain in the history."
  :version "25.1"
  :group 'eww
  :type '(choice (const :tag "Unlimited" nil)
                 integer))

(defcustom eww-use-external-browser-for-content-type
  "\\`\\(video/\\|audio/\\|application/ogg\\)"
  "Always use external browser for specified content-type."
  :version "24.4"
  :group 'eww
  :type '(choice (const :tag "Never" nil)
                 regexp))

(defcustom eww-after-render-hook nil
  "A hook called after eww has finished rendering the buffer."
  :version "25.1"
  :group 'eww
  :type 'hook)

(defcustom eww-form-checkbox-selected-symbol "[X]"
  "Symbol used to represent a selected checkbox.
See also `eww-form-checkbox-symbol'."
  :version "24.4"
  :group 'eww
  :type '(choice (const "[X]")
                 (const "☒")            ; Unicode BALLOT BOX WITH X
                 (const "☑")            ; Unicode BALLOT BOX WITH CHECK
                 string))

(defcustom eww-form-checkbox-symbol "[ ]"
  "Symbol used to represent a checkbox.
See also `eww-form-checkbox-selected-symbol'."
  :version "24.4"
  :group 'eww
  :type '(choice (const "[ ]")
                 (const "☐")            ; Unicode BALLOT BOX
                 string))

(defface eww-form-submit
  '((((type x w32 ns) (class color))	; Like default mode line
     :box (:line-width 2 :style released-button)
     :background "#808080" :foreground "black"))
  "Face for eww buffer buttons."
  :version "24.4"
  :group 'eww)

(defface eww-form-file
  '((((type x w32 ns) (class color))	; Like default mode line
     :box (:line-width 2 :style released-button)
     :background "#808080" :foreground "black"))
  "Face for eww buffer buttons."
  :version "25.1"
  :group 'eww)

(defface eww-form-checkbox
  '((((type x w32 ns) (class color))	; Like default mode line
     :box (:line-width 2 :style released-button)
     :background "lightgrey" :foreground "black"))
  "Face for eww buffer buttons."
  :version "24.4"
  :group 'eww)

(defface eww-form-select
  '((((type x w32 ns) (class color))	; Like default mode line
     :box (:line-width 2 :style released-button)
     :background "lightgrey" :foreground "black"))
  "Face for eww buffer buttons."
  :version "24.4"
  :group 'eww)

(defface eww-form-text
  '((t (:background "#505050"
		    :foreground "white"
		    :box (:line-width 1))))
  "Face for eww text inputs."
  :version "24.4"
  :group 'eww)

(defface eww-form-textarea
  '((t (:background "#C0C0C0"
		    :foreground "black"
		    :box (:line-width 1))))
  "Face for eww textarea inputs."
  :version "24.4"
  :group 'eww)

(defface eww-invalid-certificate
  '((default :weight bold)
    (((class color)) :foreground "red"))
  "Face for web pages with invalid certificates."
  :version "25.1"
  :group 'eww)

(defface eww-valid-certificate
  '((default :weight bold)
    (((class color)) :foreground "ForestGreen"))
  "Face for web pages with valid certificates."
  :version "25.1"
  :group 'eww)

(defvar eww-data nil)
(defvar eww-history nil)
(defvar eww-history-position 0)

(defvar eww-local-regex "localhost"
  "When this regex is found in the URL, it's not a keyword but an address.")

(defvar eww-link-keymap
  (let ((map (copy-keymap shr-image-map)))
    (define-key map "\r" 'eww-follow-link)
    map))

(defun eww-suggested-uris nil
  "Return the list of URIs to suggest at the `eww' prompt.
This list can be customized via `eww-suggest-uris'."
  (let ((obseen (make-vector 42 0))
	(uris nil))
    (dolist (fun eww-suggest-uris)
      (let ((ret (funcall fun)))
	(dolist (uri (if (stringp ret) (list ret) ret))
	  (when (and uri (not (intern-soft uri obseen)))
	    (intern uri obseen)
	    (push   uri uris)))))
    (nreverse uris)))

;;;###autoload
(defun eww (url)
  "Fetch URL and render the page.
If the input doesn't look like an URL or a domain name, the
word(s) will be searched for via `eww-search-prefix'."
  (interactive
   (let* ((uris (eww-suggested-uris))
	  (prompt (concat "Enter URL or keywords"
			  (if uris (format " (default %s)" (car uris)) "")
			  ": ")))
     (list (read-string prompt nil nil uris))))
  (setq url (eww--dwim-expand-url url))
  (pop-to-buffer-same-window
   (if (eq major-mode 'eww-mode)
       (current-buffer)
     (get-buffer-create "*eww*")))
  (eww-setup-buffer)
  ;; Check whether the domain only uses "Highly Restricted" Unicode
  ;; IDNA characters.  If not, transform to punycode to indicate that
  ;; there may be funny business going on.
  (let ((parsed (url-generic-parse-url url)))
    (unless (puny-highly-restrictive-domain-p (url-host parsed))
      (setf (url-host parsed) (puny-encode-domain (url-host parsed)))
      (setq url (url-recreate-url parsed))))
  (plist-put eww-data :url url)
  (plist-put eww-data :title "")
  (eww-update-header-line-format)
  (let ((inhibit-read-only t))
    (insert (format "Loading %s..." url))
    (goto-char (point-min)))
  (url-retrieve url 'eww-render
		(list url nil (current-buffer))))

(defun eww--dwim-expand-url (url)
  (setq url (string-trim url))
  (cond ((string-match-p "\\`file:/" url))
	;; Don't mangle file: URLs at all.
        ((string-match-p "\\`ftp://" url)
         (user-error "FTP is not supported"))
        (t
	 ;; Anything that starts with something that vaguely looks
	 ;; like a protocol designator is interpreted as a full URL.
         (if (or (string-match "\\`[A-Za-z]+:" url)
		 ;; Also try to match "naked" URLs like
		 ;; en.wikipedia.org/wiki/Free software
		 (string-match "\\`[A-Za-z_]+\\.[A-Za-z._]+/" url)
		 (and (= (length (split-string url)) 1)
		      (or (and (not (string-match-p "\\`[\"'].*[\"']\\'" url))
			       (> (length (split-string url "[.:]")) 1))
			  (string-match eww-local-regex url))))
             (progn
               (unless (string-match-p "\\`[a-zA-Z][-a-zA-Z0-9+.]*://" url)
                 (setq url (concat "http://" url)))
               ;; Some sites do not redirect final /
               (when (string= (url-filename (url-generic-parse-url url)) "")
                 (setq url (concat url "/"))))
           (setq url (concat eww-search-prefix
                             (mapconcat
                              #'url-hexify-string (split-string url) "+"))))))
  url)

;;;###autoload (defalias 'browse-web 'eww)

;;;###autoload
(defun eww-open-file (file)
  "Render FILE using EWW."
  (interactive "fFile: ")
  (eww (concat "file://"
	       (and (memq system-type '(windows-nt ms-dos))
		    "/")
	       (expand-file-name file))))

;;;###autoload
(defun eww-search-words ()
  "Search the web for the text between BEG and END.
If region is active (and not whitespace), search the web for
the text between BEG and END.  Else, prompt the user for a search
string.  See the `eww-search-prefix' variable for the search
engine used."
  (interactive)
  (if (use-region-p)
      (let ((region-string (buffer-substring (region-beginning) (region-end))))
        (if (not (string-match-p "\\`[ \n\t\r\v\f]*\\'" region-string))
            (eww region-string)
          (call-interactively 'eww)))
    (call-interactively 'eww)))

(defun eww-open-in-new-buffer ()
  "Fetch link at point in a new EWW buffer."
  (interactive)
  (let ((url (eww-suggested-uris)))
    (if (null url) (user-error "No link at point")
      ;; clone useful to keep history, but
      ;; should not clone from non-eww buffer
      (with-current-buffer
          (if (eq major-mode 'eww-mode) (clone-buffer)
            (generate-new-buffer "*eww*"))
        (unless (equal url (eww-current-url))
          (eww-mode)
          (eww (if (consp url) (car url) url)))))))

(defun eww-html-p (content-type)
  "Return non-nil if CONTENT-TYPE designates an HTML content type.
Currently this means either text/html or application/xhtml+xml."
  (member content-type '("text/html"
			 "application/xhtml+xml")))

(defun eww-render (status url &optional point buffer encode)
  (let ((redirect (plist-get status :redirect)))
    (when redirect
      (setq url redirect)))
  (let* ((headers (eww-parse-headers))
	 (content-type
	  (mail-header-parse-content-type
           (if (zerop (length (cdr (assoc "content-type" headers))))
	       "text/plain"
             (cdr (assoc "content-type" headers)))))
	 (charset (intern
		   (downcase
		    (or (cdr (assq 'charset (cdr content-type)))
			(eww-detect-charset (eww-html-p (car content-type)))
			"utf-8"))))
	 (data-buffer (current-buffer))
	 last-coding-system-used)
    (with-current-buffer buffer
      ;; Save the https peer status.
      (plist-put eww-data :peer (plist-get status :peer))
      ;; Make buffer listings more informative.
      (setq list-buffers-directory url))
    (unwind-protect
	(progn
	  (cond
           ((and eww-use-external-browser-for-content-type
                 (string-match-p eww-use-external-browser-for-content-type
                                 (car content-type)))
            (erase-buffer)
            (insert "<title>Unsupported content type</title>")
            (insert (format "<h1>Content-type %s is unsupported</h1>"
                            (car content-type)))
            (insert (format "<a href=%S>Direct link to the document</a>"
                            url))
            (goto-char (point-min))
	    (eww-display-html charset url nil point buffer encode))
	   ((eww-html-p (car content-type))
	    (eww-display-html charset url nil point buffer encode))
	   ((equal (car content-type) "application/pdf")
	    (eww-display-pdf))
	   ((string-match-p "\\`image/" (car content-type))
	    (eww-display-image buffer))
	   (t
	    (eww-display-raw buffer (or encode charset 'utf-8))))
	  (with-current-buffer buffer
	    (plist-put eww-data :url url)
	    (eww-update-header-line-format)
	    (setq eww-history-position 0)
	    (and last-coding-system-used
		 (set-buffer-file-coding-system last-coding-system-used))
	    (run-hooks 'eww-after-render-hook)))
      (kill-buffer data-buffer))))

(defun eww-parse-headers ()
  (let ((headers nil))
    (goto-char (point-min))
    (while (and (not (eobp))
		(not (eolp)))
      (when (looking-at "\\([^:]+\\): *\\(.*\\)")
	(push (cons (downcase (match-string 1))
		    (match-string 2))
	      headers))
      (forward-line 1))
    (unless (eobp)
      (forward-line 1))
    headers))

(defun eww-detect-charset (html-p)
  (let ((case-fold-search t)
	(pt (point)))
    (or (and html-p
	     (re-search-forward
	      "<meta[\t\n\r ]+[^>]*charset=\"?\\([^\t\n\r \"/>]+\\)[\\\"'.*]" nil t)
	     (goto-char pt)
	     (match-string 1))
	(and (looking-at
	      "[\t\n\r ]*<\\?xml[\t\n\r ]+[^>]*encoding=\"\\([^\"]+\\)")
	     (match-string 1)))))

(declare-function libxml-parse-html-region "xml.c"
		  (start end &optional base-url discard-comments))

(defun eww-display-html (charset url &optional document point buffer encode)
  (unless (fboundp 'libxml-parse-html-region)
    (error "This function requires Emacs to be compiled with libxml2"))
  (unless (buffer-live-p buffer)
    (error "Buffer %s doesn't exist" buffer))
  ;; There should be a better way to abort loading images
  ;; asynchronously.
  (setq url-queue nil)
  (let ((document
	 (or document
	     (list
	      'base (list (cons 'href url))
	      (progn
		(setq encode (or encode charset 'utf-8))
		(condition-case nil
		    (decode-coding-region (point) (point-max) encode)
		  (coding-system-error nil))
                (save-excursion
                  ;; Remove CRLF before parsing.
                  (while (re-search-forward "\r$" nil t)
                    (replace-match "" t t)))
		(libxml-parse-html-region (point) (point-max))))))
	(source (and (null document)
		     (buffer-substring (point) (point-max)))))
    (with-current-buffer buffer
      (setq bidi-paragraph-direction nil)
      (plist-put eww-data :source source)
      (plist-put eww-data :dom document)
      (let ((inhibit-read-only t)
	    (inhibit-modification-hooks t)
	    (shr-target-id (url-target (url-generic-parse-url url)))
	    (shr-external-rendering-functions
             (append
              shr-external-rendering-functions
              '((title . eww-tag-title)
                (form . eww-tag-form)
                (input . eww-tag-input)
                (button . eww-form-submit)
                (textarea . eww-tag-textarea)
                (select . eww-tag-select)
                (link . eww-tag-link)
                (meta . eww-tag-meta)
                (a . eww-tag-a)))))
	(erase-buffer)
	(shr-insert-document document)
	(cond
	 (point
	  (goto-char point))
	 (shr-target-id
	  (goto-char (point-min))
	  (let ((point (next-single-property-change
			(point-min) 'shr-target-id)))
	    (when point
	      (goto-char point))))
	 (t
	  (goto-char (point-min))
	  ;; Don't leave point inside forms, because the normal eww
	  ;; commands aren't available there.
	  (while (and (not (eobp))
		      (get-text-property (point) 'eww-form))
	    (forward-line 1)))))
      (eww-size-text-inputs))))

(defun eww-handle-link (dom)
  (let* ((rel (dom-attr dom 'rel))
	 (href (dom-attr dom 'href))
	 (where (assoc
		 ;; The text associated with :rel is case-insensitive.
		 (if rel (downcase rel))
		 '(("next" . :next)
		   ;; Texinfo uses "previous", but HTML specifies
		   ;; "prev", so recognize both.
		   ("previous" . :previous)
		   ("prev" . :previous)
		   ;; HTML specifies "start" but also "contents",
		   ;; and Gtk seems to use "home".  Recognize
		   ;; them all; but store them in different
		   ;; variables so that we can readily choose the
		   ;; "best" one.
		   ("start" . :start)
		   ("home" . :home)
		   ("contents" . :contents)
		   ("up" . :up)))))
    (and href
	 where
	 (plist-put eww-data (cdr where) href))))

(defvar eww-redirect-level 1)

(defun eww-tag-meta (dom)
  (when (and (cl-equalp (dom-attr dom 'http-equiv) "refresh")
             (< eww-redirect-level 5))
    (when-let* ((refresh (dom-attr dom 'content)))
      (when (or (string-match "^\\([0-9]+\\) *;.*url=\"\\([^\"]+\\)\"" refresh)
                (string-match "^\\([0-9]+\\) *;.*url='\\([^']+\\)'" refresh)
                (string-match "^\\([0-9]+\\) *;.*url=\\([^ ]+\\)" refresh))
        (let ((timeout (match-string 1 refresh))
              (url (match-string 2 refresh))
              (eww-redirect-level (1+ eww-redirect-level)))
          (if (equal timeout "0")
              (eww (shr-expand-url url))
            (eww-tag-a
             (dom-node 'a `((href . ,(shr-expand-url url)))
                       (format "Auto refresh in %s second%s disabled"
                               timeout
                               (if (equal timeout "1")
                                   ""
                                 "s"))))))))))

(defun eww-tag-link (dom)
  (eww-handle-link dom)
  (shr-generic dom))

(defun eww-tag-a (dom)
  (eww-handle-link dom)
  (let ((start (point)))
    (shr-tag-a dom)
    (put-text-property start (point) 'keymap eww-link-keymap)))

(defun eww-update-header-line-format ()
  (setq header-line-format
	(and eww-header-line-format
	     (let ((title (plist-get eww-data :title))
		   (peer (plist-get eww-data :peer)))
	       (when (zerop (length title))
		 (setq title "[untitled]"))
	       ;; This connection has is https.
	       (when peer
		 (setq title
		       (propertize title 'face
				   (if (plist-get peer :warnings)
				       'eww-invalid-certificate
				     'eww-valid-certificate))))
	       (replace-regexp-in-string
		"%" "%%"
		(format-spec
		 eww-header-line-format
		 `((?u . ,(or (plist-get eww-data :url) ""))
		   (?t . ,title))))))))

(defun eww-tag-title (dom)
  (plist-put eww-data :title
	     (replace-regexp-in-string
	      "^ \\| $" ""
	      (replace-regexp-in-string "[ \t\r\n]+" " " (dom-text dom))))
  (eww-update-header-line-format))

(defun eww-display-raw (buffer &optional encode)
  (let ((data (buffer-substring (point) (point-max))))
    (unless (buffer-live-p buffer)
      (error "Buffer %s doesn't exist" buffer))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert data)
	(condition-case nil
	    (decode-coding-region (point-min) (1+ (length data)) encode)
	  (coding-system-error nil)))
      (goto-char (point-min)))))

(defun eww-display-image (buffer)
  (let ((data (shr-parse-image-data)))
    (unless (buffer-live-p buffer)
      (error "Buffer %s doesn't exist" buffer))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(shr-put-image data nil))
      (goto-char (point-min)))))

(declare-function mailcap-view-mime "mailcap" (type))
(defun eww-display-pdf ()
  (let ((data (buffer-substring (point) (point-max))))
    (pop-to-buffer-same-window (get-buffer-create "*eww pdf*"))
    (let ((coding-system-for-write 'raw-text)
	  (inhibit-read-only t))
      (erase-buffer)
      (insert data)
      (mailcap-view-mime "application/pdf")))
  (goto-char (point-min)))

(defun eww-setup-buffer ()
  (when (or (plist-get eww-data :url)
            (plist-get eww-data :dom))
    (eww-save-history))
  (let ((inhibit-read-only t))
    (remove-overlays)
    (erase-buffer))
  (setq bidi-paragraph-direction nil)
  (unless (eq major-mode 'eww-mode)
    (eww-mode)))

(defun eww-current-url nil
  "Return URI of the Web page the current EWW buffer is visiting."
  (plist-get eww-data :url))

(defun eww-links-at-point ()
  "Return list of URIs, if any, linked at point."
  (remq nil
	(list (get-text-property (point) 'shr-url)
	      (get-text-property (point) 'image-url))))

(defun eww-view-source ()
  "View the HTML source code of the current page."
  (interactive)
  (let ((buf (get-buffer-create "*eww-source*"))
        (source (plist-get eww-data :source)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
	(delete-region (point-min) (point-max))
	(insert (or source "no source"))
	(goto-char (point-min))
        ;; Decode the source and set the buffer's encoding according
        ;; to what the HTML source specifies in its 'charset' header,
        ;; if any.
        (let ((cs (find-auto-coding "" (point-max))))
          (when (consp cs)
            (setq cs (car cs))
            (when (coding-system-p cs)
              (decode-coding-region (point-min) (point-max) cs)
              (setq buffer-file-coding-system last-coding-system-used))))
        (cond
         ((fboundp 'mhtml-mode)
          (mhtml-mode))
         ((fboundp 'html-mode)
	  (html-mode)))))
    (view-buffer buf)))

(defun eww-toggle-paragraph-direction ()
  "Cycle the paragraph direction between left-to-right, right-to-left and auto."
  (interactive)
  (setq bidi-paragraph-direction
        (cond ((eq bidi-paragraph-direction 'left-to-right)
               nil)
              ((eq bidi-paragraph-direction 'right-to-left)
               'left-to-right)
              (t
               'right-to-left)))
  (message "The paragraph direction is now %s"
           (if (null bidi-paragraph-direction)
               "automatic"
             bidi-paragraph-direction)))

(defun eww-readable ()
  "View the main \"readable\" parts of the current web page.
This command uses heuristics to find the parts of the web page that
contains the main textual portion, leaving out navigation menus and
the like."
  (interactive)
  (let* ((old-data eww-data)
	 (dom (with-temp-buffer
		(insert (plist-get old-data :source))
		(condition-case nil
		    (decode-coding-region (point-min) (point-max) 'utf-8)
		  (coding-system-error nil))
		(libxml-parse-html-region (point-min) (point-max))))
         (base (plist-get eww-data :url)))
    (eww-score-readability dom)
    (eww-save-history)
    (eww-display-html nil nil
                      (list 'base (list (cons 'href base))
                            (eww-highest-readability dom))
		      nil (current-buffer))
    (dolist (elem '(:source :url :title :next :previous :up))
      (plist-put eww-data elem (plist-get old-data elem)))
    (eww-update-header-line-format)))

(defun eww-score-readability (node)
  (let ((score -1))
    (cond
     ((memq (dom-tag node) '(script head comment))
      (setq score -2))
     ((eq (dom-tag node) 'meta)
      (setq score -1))
     ((eq (dom-tag node) 'img)
      (setq score 2))
     ((eq (dom-tag node) 'a)
      (setq score (- (length (split-string (dom-text node))))))
     (t
      (dolist (elem (dom-children node))
	(cond
         ((stringp elem)
          (setq score (+ score (length (split-string elem)))))
         ((consp elem)
	  (setq score (+ score
			 (or (cdr (assoc :eww-readability-score (cdr elem)))
			     (eww-score-readability elem)))))))))
    ;; Cache the score of the node to avoid recomputing all the time.
    (dom-set-attribute node :eww-readability-score score)
    score))

(defun eww-highest-readability (node)
  (let ((result node)
	highest)
    (dolist (elem (dom-non-text-children node))
      (when (> (or (dom-attr
		    (setq highest (eww-highest-readability elem))
		    :eww-readability-score)
		   most-negative-fixnum)
	       (or (dom-attr result :eww-readability-score)
		   most-negative-fixnum))
	(setq result highest)))
    result))

(defvar eww-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'eww-reload) ;FIXME: revert-buffer-function instead!
    (define-key map "G" 'eww)
    (define-key map [?\M-\r] 'eww-open-in-new-buffer)
    (define-key map [?\t] 'shr-next-link)
    (define-key map [?\M-\t] 'shr-previous-link)
    (define-key map [backtab] 'shr-previous-link)
    (define-key map [delete] 'scroll-down-command)
    (define-key map "l" 'eww-back-url)
    (define-key map "r" 'eww-forward-url)
    (define-key map "n" 'eww-next-url)
    (define-key map "p" 'eww-previous-url)
    (define-key map "u" 'eww-up-url)
    (define-key map "t" 'eww-top-url)
    (define-key map "&" 'eww-browse-with-external-browser)
    (define-key map "d" 'eww-download)
    (define-key map "w" 'eww-copy-page-url)
    (define-key map "C" 'url-cookie-list)
    (define-key map "v" 'eww-view-source)
    (define-key map "R" 'eww-readable)
    (define-key map "H" 'eww-list-histories)
    (define-key map "E" 'eww-set-character-encoding)
    (define-key map "s" 'eww-switch-to-buffer)
    (define-key map "S" 'eww-list-buffers)
    (define-key map "F" 'eww-toggle-fonts)
    (define-key map "D" 'eww-toggle-paragraph-direction)
    (define-key map [(meta C)] 'eww-toggle-colors)

    (define-key map "b" 'eww-add-bookmark)
    (define-key map "B" 'eww-list-bookmarks)
    (define-key map [(meta n)] 'eww-next-bookmark)
    (define-key map [(meta p)] 'eww-previous-bookmark)

    (easy-menu-define nil map ""
      '("Eww"
	["Exit" quit-window t]
	["Close browser" quit-window t]
	["Reload" eww-reload t]
	["Follow URL in new buffer" eww-open-in-new-buffer]
	["Back to previous page" eww-back-url
	 :active (not (zerop (length eww-history)))]
	["Forward to next page" eww-forward-url
	 :active (not (zerop eww-history-position))]
	["Browse with external browser" eww-browse-with-external-browser t]
	["Download" eww-download t]
	["View page source" eww-view-source]
	["Copy page URL" eww-copy-page-url t]
	["List histories" eww-list-histories t]
	["Switch to buffer" eww-switch-to-buffer t]
	["List buffers" eww-list-buffers t]
	["Add bookmark" eww-add-bookmark t]
	["List bookmarks" eww-list-bookmarks t]
	["List cookies" url-cookie-list t]
	["Toggle fonts" eww-toggle-fonts t]
	["Toggle colors" eww-toggle-colors t]
        ["Character Encoding" eww-set-character-encoding]
        ["Toggle Paragraph Direction" eww-toggle-paragraph-direction]))
    map))

(defvar eww-tool-bar-map
  (let ((map (make-sparse-keymap)))
    (dolist (tool-bar-item
             '((quit-window . "close")
               (eww-reload . "refresh")
               (eww-back-url . "left-arrow")
               (eww-forward-url . "right-arrow")
               (eww-view-source . "show")
               (eww-copy-page-url . "copy")
               (eww-add-bookmark . "bookmark_add"))) ;; ...
      (tool-bar-local-item-from-menu
       (car tool-bar-item) (cdr tool-bar-item) map eww-mode-map))
    map)
  "Tool bar for `eww-mode'.")

;; Autoload cookie needed by desktop.el.
;;;###autoload
(define-derived-mode eww-mode special-mode "eww"
  "Mode for browsing the web."
  (setq-local eww-data (list :title ""))
  (setq-local browse-url-browser-function #'eww-browse-url)
  (add-hook 'after-change-functions #'eww-process-text-input nil t)
  (setq-local eww-history nil)
  (setq-local eww-history-position 0)
  (when (boundp 'tool-bar-map)
    (setq-local tool-bar-map eww-tool-bar-map))
  ;; desktop support
  (setq-local desktop-save-buffer #'eww-desktop-misc-data)
  ;; multi-page isearch support
  (setq-local multi-isearch-next-buffer-function #'eww-isearch-next-buffer)
  (setq truncate-lines t)
  (buffer-disable-undo)
  (setq buffer-read-only t))

;;;###autoload
(defun eww-browse-url (url &optional new-window)
  (when new-window
    (pop-to-buffer-same-window
     (generate-new-buffer
      (format "*eww-%s*" (url-host (url-generic-parse-url
                                    (eww--dwim-expand-url url))))))
    (eww-mode))
  (eww url))

(defun eww-back-url ()
  "Go to the previously displayed page."
  (interactive)
  (when (>= eww-history-position (length eww-history))
    (user-error "No previous page"))
  (eww-save-history)
  (setq eww-history-position (+ eww-history-position 2))
  (eww-restore-history (elt eww-history (1- eww-history-position))))

(defun eww-forward-url ()
  "Go to the next displayed page."
  (interactive)
  (when (zerop eww-history-position)
    (user-error "No next page"))
  (eww-save-history)
  (eww-restore-history (elt eww-history (1- eww-history-position))))

(defun eww-restore-history (elem)
  (let ((inhibit-read-only t)
	(inhibit-modification-hooks t)
	(text (plist-get elem :text)))
    (setq eww-data elem)
    (if (null text)
	(eww-reload)			; FIXME: restore :point?
      (erase-buffer)
      (insert text)
      (goto-char (plist-get elem :point))
      ;; Make buffer listings more informative.
      (setq list-buffers-directory (plist-get elem :url))
      (eww-update-header-line-format))))

(defun eww-next-url ()
  "Go to the page marked `next'.
A page is marked `next' if rel=\"next\" appears in a <link>
or <a> tag."
  (interactive)
  (if (plist-get eww-data :next)
      (eww-browse-url (shr-expand-url (plist-get eww-data :next)
				      (plist-get eww-data :url)))
    (user-error "No `next' on this page")))

(defun eww-previous-url ()
  "Go to the page marked `previous'.
A page is marked `previous' if rel=\"previous\" appears in a <link>
or <a> tag."
  (interactive)
  (if (plist-get eww-data :previous)
      (eww-browse-url (shr-expand-url (plist-get eww-data :previous)
				      (plist-get eww-data :url)))
    (user-error "No `previous' on this page")))

(defun eww-up-url ()
  "Go to the page marked `up'.
A page is marked `up' if rel=\"up\" appears in a <link>
or <a> tag."
  (interactive)
  (if (plist-get eww-data :up)
      (eww-browse-url (shr-expand-url (plist-get eww-data :up)
				      (plist-get eww-data :url)))
    (user-error "No `up' on this page")))

(defun eww-top-url ()
  "Go to the page marked `top'.
A page is marked `top' if rel=\"start\", rel=\"home\", or rel=\"contents\"
appears in a <link> or <a> tag."
  (interactive)
  (let ((best-url (or (plist-get eww-data :start)
		      (plist-get eww-data :contents)
		      (plist-get eww-data :home))))
    (if best-url
	(eww-browse-url (shr-expand-url best-url (plist-get eww-data :url)))
      (user-error "No `top' for this page"))))

(defun eww-reload (&optional local encode)
  "Reload the current page.
If LOCAL is non-nil (interactively, the command was invoked with
a prefix argument), don't reload the page from the network, but
just re-display the HTML already fetched."
  (interactive "P")
  (let ((url (plist-get eww-data :url)))
    (if local
	(if (null (plist-get eww-data :dom))
	    (error "No current HTML data")
	  (eww-display-html 'utf-8 url (plist-get eww-data :dom)
			    (point) (current-buffer)))
      (url-retrieve url 'eww-render
		    (list url (point) (current-buffer) encode)))))

;; Form support.

(defvar eww-form nil)

(defvar eww-submit-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'eww-submit)
    (define-key map [(control c) (control c)] 'eww-submit)
    map))

(defvar eww-submit-file
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'eww-select-file)
    (define-key map [(control c) (control c)] 'eww-submit)
    map))

(defvar eww-checkbox-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'eww-toggle-checkbox)
    (define-key map "\r" 'eww-toggle-checkbox)
    (define-key map [(control c) (control c)] 'eww-submit)
    map))

(defvar eww-text-map
  (let ((map (make-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "\r" 'eww-submit)
    (define-key map [(control a)] 'eww-beginning-of-text)
    (define-key map [(control c) (control c)] 'eww-submit)
    (define-key map [(control e)] 'eww-end-of-text)
    (define-key map [?\t] 'shr-next-link)
    (define-key map [?\M-\t] 'shr-previous-link)
    map))

(defvar eww-textarea-map
  (let ((map (make-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "\r" 'forward-line)
    (define-key map [(control c) (control c)] 'eww-submit)
    (define-key map [?\t] 'shr-next-link)
    (define-key map [?\M-\t] 'shr-previous-link)
    map))

(defvar eww-select-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'eww-change-select)
    (define-key map [(control c) (control c)] 'eww-submit)
    map))

(defun eww-beginning-of-text ()
  "Move to the start of the input field."
  (interactive)
  (goto-char (eww-beginning-of-field)))

(defun eww-end-of-text ()
  "Move to the end of the text in the input field."
  (interactive)
  (goto-char (eww-end-of-field))
  (let ((start (eww-beginning-of-field)))
    (while (and (equal (following-char) ? )
		(> (point) start))
      (forward-char -1))
    (when (> (point) start)
      (forward-char 1))))

(defun eww-beginning-of-field ()
  (cond
   ((bobp)
    (point))
   ((not (eq (get-text-property (point) 'eww-form)
	     (get-text-property (1- (point)) 'eww-form)))
    (point))
   (t
    (previous-single-property-change
     (point) 'eww-form nil (point-min)))))

(defun eww-end-of-field ()
  (1- (next-single-property-change
       (point) 'eww-form nil (point-max))))

(defun eww-tag-form (dom)
  (let ((eww-form (list (cons :method (dom-attr dom 'method))
			(cons :action (dom-attr dom 'action))))
	(start (point)))
    (insert "\n")
    (shr-ensure-paragraph)
    (shr-generic dom)
    (unless (bolp)
      (insert "\n"))
    (insert "\n")
    (when (> (point) start)
      (put-text-property start (1+ start)
			 'eww-form eww-form))))

(defun eww-form-submit (dom)
  (let ((start (point))
	(value (dom-attr dom 'value)))
    (setq value
	  (if (zerop (length value))
	      "Submit"
	    value))
    (insert value)
    (add-face-text-property start (point) 'eww-form-submit)
    (put-text-property start (point) 'eww-form
		       (list :eww-form eww-form
			     :value value
			     :type "submit"
			     :name (dom-attr dom 'name)))
    (put-text-property start (point) 'keymap eww-submit-map)
    (insert " ")))

(defun eww-form-checkbox (dom)
  (let ((start (point)))
    (if (dom-attr dom 'checked)
	(insert eww-form-checkbox-selected-symbol)
      (insert eww-form-checkbox-symbol))
    (add-face-text-property start (point) 'eww-form-checkbox)
    (put-text-property start (point) 'eww-form
		       (list :eww-form eww-form
			     :value (dom-attr dom 'value)
			     :type (downcase (dom-attr dom 'type))
			     :checked (dom-attr dom 'checked)
			     :name (dom-attr dom 'name)))
    (put-text-property start (point) 'keymap eww-checkbox-map)
    (insert " ")))

(defun eww-form-file (dom)
  (let ((start (point))
	(value (dom-attr dom 'value)))
    (setq value
	  (if (zerop (length value))
	      " No file selected"
	    value))
    (insert "Browse")
    (add-face-text-property start (point) 'eww-form-file)
    (insert value)
    (put-text-property start (point) 'eww-form
		       (list :eww-form eww-form
			     :value (dom-attr dom 'value)
			     :type (downcase (dom-attr dom 'type))
			     :name (dom-attr dom 'name)))
    (put-text-property start (point) 'keymap eww-submit-file)
    (insert " ")))

(defun eww-select-file ()
  "Change the value of the upload file menu under point."
  (interactive)
  (let*  ((input (get-text-property (point) 'eww-form)))
    (let ((filename
	   (let ((insert-default-directory t))
	     (read-file-name "filename:  "))))
      (eww-update-field filename (length "Browse"))
              (plist-put input :filename filename))))

(defun eww-form-text (dom)
  (let ((start (point))
	(type (downcase (or (dom-attr dom 'type) "text")))
	(value (or (dom-attr dom 'value) ""))
	(width (string-to-number (or (dom-attr dom 'size) "40")))
        (readonly-property (if (or (dom-attr dom 'disabled)
				   (dom-attr dom 'readonly))
                               'read-only
                             'inhibit-read-only)))
    (insert value)
    (when (< (length value) width)
      (insert (make-string (- width (length value)) ? )))
    (put-text-property start (point) 'face 'eww-form-text)
    (put-text-property start (point) 'inhibit-read-only t)
    (put-text-property start (point) 'local-map eww-text-map)
    (put-text-property start (point) readonly-property t)
    (put-text-property start (point) 'eww-form
                       (list :eww-form eww-form
                             :value value
                             :type type
                             :name (dom-attr dom 'name)))
    (insert " ")))

(defconst eww-text-input-types '("text" "password" "textarea"
                                 "color" "date" "datetime" "datetime-local"
                                 "email" "month" "number" "search" "tel"
                                 "time" "url" "week")
  "List of input types which represent a text input.
See URL `https://developer.mozilla.org/en-US/docs/Web/HTML/Element/Input'.")

(defun eww-process-text-input (beg end replace-length)
  (when-let* ((pos (and (< (1+ end) (point-max))
		        (> (1- end) (point-min))
		        (cond
		         ((get-text-property (1+ end) 'eww-form)
			  (1+ end))
		         ((get-text-property (1- end) 'eww-form)
			  (1- end))))))
    (let* ((form (get-text-property pos 'eww-form))
	   (properties (text-properties-at pos))
           (buffer-undo-list t)
	   (inhibit-read-only t)
	   (length (- end beg replace-length))
	   (type (plist-get form :type)))
      (when (and form
		 (member type eww-text-input-types))
	(cond
	 ((> length 0)
	  ;; Delete some space at the end.
	  (save-excursion
	    (goto-char
	     (if (equal type "textarea")
		 (1- (line-end-position))
	       (eww-end-of-field)))
	    (while (and (> length 0)
			(eql (char-after (1- (point))) ? ))
	      (delete-region (1- (point)) (point))
	      (cl-decf length))))
	 ((< length 0)
	  ;; Add padding.
	  (save-excursion
	    (goto-char end)
	    (goto-char
	     (if (equal type "textarea")
		 (1- (line-end-position))
	       (1+ (eww-end-of-field))))
	    (let ((start (point)))
              (insert (make-string (abs length) ? ))
	      (set-text-properties start (point) properties))
	    (goto-char (1- end)))))
	(set-text-properties (cdr (assq :start form))
                             (cdr (assq :end form))
			     properties)
	(let ((value (buffer-substring-no-properties
		      (eww-beginning-of-field)
		      (eww-end-of-field))))
	  (when (string-match " +\\'" value)
	    (setq value (substring value 0 (match-beginning 0))))
	  (plist-put form :value value)
	  (when (equal type "password")
	    ;; Display passwords as asterisks.
	    (let ((start (eww-beginning-of-field)))
	      (put-text-property
               start (+ start (length value))
               'display (make-string (length value) ?*)))))))))

(defun eww-tag-textarea (dom)
  (let ((start (point))
	(value (or (dom-attr dom 'value) ""))
	(lines (string-to-number (or (dom-attr dom 'rows) "10")))
	(width (string-to-number (or (dom-attr dom 'cols) "10")))
	end)
    (shr-ensure-newline)
    (insert value)
    (shr-ensure-newline)
    (when (< (count-lines start (point)) lines)
      (dotimes (_ (- lines (count-lines start (point))))
	(insert "\n")))
    (setq end (point-marker))
    (goto-char start)
    (while (< (point) end)
      (end-of-line)
      (let ((pad (- width (- (point) (line-beginning-position)))))
	(when (> pad 0)
	  (insert (make-string pad ? ))))
      (add-face-text-property (line-beginning-position)
			      (point) 'eww-form-textarea)
      (put-text-property (line-beginning-position) (point) 'inhibit-read-only t)
      (put-text-property (line-beginning-position) (point)
			 'local-map eww-textarea-map)
      (forward-line 1))
    (put-text-property start (point) 'eww-form
		       (list :eww-form eww-form
			     :value value
			     :type "textarea"
			     :name (dom-attr dom 'name)))))

(defun eww-tag-input (dom)
  (let ((type (downcase (or (dom-attr dom 'type) "text")))
	(start (point)))
    (cond
     ((or (equal type "checkbox")
	  (equal type "radio"))
      (eww-form-checkbox dom))
     ((equal type "file")
      (eww-form-file dom))
     ((equal type "submit")
      (eww-form-submit dom))
     ((equal type "hidden")
      (let ((form eww-form)
	    (name (dom-attr dom 'name)))
	;; Don't add <input type=hidden> elements repeatedly.
	(while (and form
		    (or (not (consp (car form)))
			(not (eq (caar form) 'hidden))
			(not (equal (plist-get (cdr (car form)) :name)
				    name))))
	  (setq form (cdr form)))
	(unless form
	  (nconc eww-form (list
			   (list 'hidden
				 :name name
				 :value (or (dom-attr dom 'value) "")))))))
     (t
      (eww-form-text dom)))
    (unless (= start (point))
      (put-text-property start (1+ start) 'help-echo "Input field")
      ;; Mark this as an element we can TAB to.
      (put-text-property start (1+ start) 'shr-url dom))))

(defun eww-tag-select (dom)
  (shr-ensure-paragraph)
  (let ((menu (list :name (dom-attr dom 'name)
		    :eww-form eww-form))
	(options nil)
	(start (point))
	(max 0)
	opelem)
    (if (eq (dom-tag dom) 'optgroup)
	(dolist (groupelem (dom-children dom))
	  (unless (dom-attr groupelem 'disabled)
	    (setq opelem (append opelem (list groupelem)))))
      (setq opelem (list dom)))
    (dolist (elem opelem)
      (when (eq (dom-tag elem) 'option)
	(when (dom-attr elem 'selected)
	  (nconc menu (list :value (dom-attr elem 'value))))
	(let ((display (dom-text elem)))
	  (setq max (max max (length display)))
	  (push (list 'item
		      :value (dom-attr elem 'value)
		      :display display)
		options))))
    (when options
      (setq options (nreverse options))
      ;; If we have no selected values, default to the first value.
      (unless (plist-get menu :value)
	(nconc menu (list :value (nth 2 (car options)))))
      (nconc menu options)
      (let ((selected (eww-select-display menu)))
	(insert selected
		(make-string (- max (length selected)) ? )))
      (put-text-property start (point) 'eww-form menu)
      (add-face-text-property start (point) 'eww-form-select)
      (put-text-property start (point) 'keymap eww-select-map)
      (unless (= start (point))
       (put-text-property start (1+ start) 'help-echo "select field"))
      (shr-ensure-paragraph))))

(defun eww-select-display (select)
  (let ((value (plist-get select :value))
	display)
    (dolist (elem select)
      (when (and (consp elem)
		 (eq (car elem) 'item)
		 (equal value (plist-get (cdr elem) :value)))
	(setq display (plist-get (cdr elem) :display))))
    display))

(defun eww-change-select ()
  "Change the value of the select drop-down menu under point."
  (interactive)
  (let* ((input (get-text-property (point) 'eww-form))
	 (completion-ignore-case t)
	 (options
	  (delq nil
		(mapcar (lambda (elem)
			  (and (consp elem)
			       (eq (car elem) 'item)
			       (cons (plist-get (cdr elem) :display)
				     (plist-get (cdr elem) :value))))
			input)))
	 (display
	  (completing-read "Change value: " options nil 'require-match))
	 (inhibit-read-only t))
    (plist-put input :value (cdr (assoc-string display options t)))
    (goto-char
     (eww-update-field display))))

(defun eww-update-field (string &optional offset)
  (unless offset
    (setq offset 0))
  (let ((properties (text-properties-at (point)))
	(start (+ (eww-beginning-of-field) offset))
	(current-end (1+ (eww-end-of-field)))
	(new-end (+ (eww-beginning-of-field) (length string)))
        (inhibit-read-only t))
    (delete-region start current-end)
    (forward-char offset)
    (insert string
	    (make-string (- (- (+ new-end offset) start) (length string)) ? ))
    (when (= 0 offset)
      (set-text-properties start new-end properties))
    start))

(defun eww-toggle-checkbox ()
  "Toggle the value of the checkbox under point."
  (interactive)
  (let* ((input (get-text-property (point) 'eww-form))
	 (type (plist-get input :type)))
    (if (equal type "checkbox")
	(goto-char
	 (1+
	  (if (plist-get input :checked)
	      (progn
		(plist-put input :checked nil)
		(eww-update-field eww-form-checkbox-symbol))
	    (plist-put input :checked t)
	    (eww-update-field eww-form-checkbox-selected-symbol))))
      ;; Radio button.  Switch all other buttons off.
      (let ((name (plist-get input :name)))
	(save-excursion
	  (dolist (elem (eww-inputs (plist-get input :eww-form)))
	    (when (equal (plist-get (cdr elem) :name) name)
	      (goto-char (car elem))
	      (if (not (eq (cdr elem) input))
		  (progn
		    (plist-put input :checked nil)
		    (eww-update-field eww-form-checkbox-symbol))
		(plist-put input :checked t)
		(eww-update-field eww-form-checkbox-selected-symbol)))))
	(forward-char 1)))))

(defun eww-inputs (form)
  (let ((start (point-min))
	(inputs nil))
    (while (and start
		(< start (point-max)))
      (when (or (get-text-property start 'eww-form)
		(setq start (next-single-property-change start 'eww-form)))
	(when (eq (plist-get (get-text-property start 'eww-form) :eww-form)
		  form)
	  (push (cons start (get-text-property start 'eww-form))
		inputs))
	(setq start (next-single-property-change start 'eww-form))))
    (nreverse inputs)))

(defun eww-size-text-inputs ()
  (let ((start (point-min)))
    (while (and start
		(< start (point-max)))
      (when (or (get-text-property start 'eww-form)
		(setq start (next-single-property-change start 'eww-form)))
	(let ((props (get-text-property start 'eww-form)))
          (nconc props (list (cons :start start)))
          (setq start (next-single-property-change
                       start 'eww-form nil (point-max)))
          (nconc props (list (cons :end start))))))))

(defun eww-input-value (input)
  (let ((type (plist-get input :type))
	(value (plist-get input :value)))
    (cond
     ((equal type "textarea")
      (with-temp-buffer
	(insert value)
	(goto-char (point-min))
	(while (re-search-forward "^ +\n\\| +$" nil t)
	  (replace-match "" t t))
	(buffer-string)))
     (t
      (if (string-match " +\\'" value)
	  (substring value 0 (match-beginning 0))
	value)))))

(defun eww-submit ()
  "Submit the current form."
  (interactive)
  (let* ((this-input (get-text-property (point) 'eww-form))
	 (form (plist-get this-input :eww-form))
	 values next-submit)
    (dolist (elem (sort (eww-inputs form)
			(lambda (o1 o2)
			  (< (car o1) (car o2)))))
      (let* ((input (cdr elem))
	     (input-start (car elem))
	     (name (plist-get input :name)))
	(when name
	  (cond
	   ((member (plist-get input :type) '("checkbox" "radio"))
	    (when (plist-get input :checked)
	      (push (cons name (plist-get input :value))
		    values)))
	   ((equal (plist-get input :type) "file")
	    (push (cons "file"
			(list (cons "filedata"
				    (with-temp-buffer
				      (insert-file-contents
				       (plist-get input :filename))
				      (buffer-string)))
			      (cons "name" (plist-get input :name))
			      (cons "filename" (plist-get input :filename))))
		  values))
	   ((equal (plist-get input :type) "submit")
	    ;; We want the values from buttons if we hit a button if
	    ;; we hit enter on it, or if it's the first button after
	    ;; the field we did hit return on.
	    (when (or (eq input this-input)
		      (and (not (eq input this-input))
			   (null next-submit)
			   (> input-start (point))))
	      (setq next-submit t)
	      (push (cons name (plist-get input :value))
		    values)))
	   (t
	    (push (cons name (eww-input-value input))
		  values))))))
    (dolist (elem form)
      (when (and (consp elem)
		 (eq (car elem) 'hidden))
	(push (cons (plist-get (cdr elem) :name)
		    (or (plist-get (cdr elem) :value) ""))
	      values)))
    (if (and (stringp (cdr (assq :method form)))
	     (equal (downcase (cdr (assq :method form))) "post"))
	(let ((mtype))
	  (dolist (x values mtype)
	    (if (equal (car x) "file")
		(progn
		  (setq mtype "multipart/form-data"))))
	  (cond ((equal mtype "multipart/form-data")
		 (let ((boundary (mml-compute-boundary '())))
		   (let ((url-request-method "POST")
			 (url-request-extra-headers
			  (list (cons "Content-Type"
				      (concat "multipart/form-data; boundary="
					      boundary))))
			 (url-request-data
			  (mm-url-encode-multipart-form-data values boundary)))
		     (eww-browse-url (shr-expand-url
				      (cdr (assq :action form))
				      (plist-get eww-data :url))))))
		(t
		 (let ((url-request-method "POST")
		       (url-request-extra-headers
			'(("Content-Type" .
			   "application/x-www-form-urlencoded")))
		       (url-request-data
			(mm-url-encode-www-form-urlencoded values)))
		   (eww-browse-url (shr-expand-url
				    (cdr (assq :action form))
				    (plist-get eww-data :url)))))))
      (eww-browse-url
       (concat
	(if (cdr (assq :action form))
	    (shr-expand-url (cdr (assq :action form)) (plist-get eww-data :url))
	  (plist-get eww-data :url))
	"?"
	(mm-url-encode-www-form-urlencoded values))))))

(defun eww-browse-with-external-browser (&optional url)
  "Browse the current URL with an external browser.
The browser to used is specified by the `shr-external-browser' variable."
  (interactive)
  (funcall shr-external-browser (or url (plist-get eww-data :url))))

(defun eww-follow-link (&optional external mouse-event)
  "Browse the URL under point.
If EXTERNAL is single prefix, browse the URL using `shr-external-browser'.
If EXTERNAL is double prefix, browse in new buffer."
  (interactive (list current-prefix-arg last-nonmenu-event))
  (mouse-set-point mouse-event)
  (let ((url (get-text-property (point) 'shr-url)))
    (cond
     ((not url)
      (message "No link under point"))
     ((string-match "^mailto:" url)
      (browse-url-mail url))
     ((and (consp external) (<= (car external) 4))
      (funcall shr-external-browser url))
     ;; This is a #target url in the same page as the current one.
     ((and (url-target (url-generic-parse-url url))
	   (eww-same-page-p url (plist-get eww-data :url)))
      (let ((dom (plist-get eww-data :dom)))
	(eww-save-history)
	(eww-display-html 'utf-8 url dom nil (current-buffer))))
     (t
      (eww-browse-url url external)))))

(defun eww-same-page-p (url1 url2)
  "Return non-nil if URL1 and URL2 represent the same page.
Differences in #targets are ignored."
  (let ((obj1 (url-generic-parse-url url1))
	(obj2 (url-generic-parse-url url2)))
    (setf (url-target obj1) nil)
    (setf (url-target obj2) nil)
    (equal (url-recreate-url obj1) (url-recreate-url obj2))))

(defun eww-copy-page-url ()
  "Copy the URL of the current page into the kill ring."
  (interactive)
  (message "%s" (plist-get eww-data :url))
  (kill-new (plist-get eww-data :url)))

(defun eww-download ()
  "Download URL under point to `eww-download-directory'."
  (interactive)
  (access-file eww-download-directory "Download failed")
  (let ((url (get-text-property (point) 'shr-url)))
    (if (not url)
        (message "No URL under point")
      (url-retrieve url 'eww-download-callback (list url)))))

(defun eww-download-callback (status url)
  (unless (plist-get status :error)
    (let* ((obj (url-generic-parse-url url))
           (path (car (url-path-and-query obj)))
           (file (eww-make-unique-file-name
                  (eww-decode-url-file-name (file-name-nondirectory path))
                  eww-download-directory)))
      (goto-char (point-min))
      (re-search-forward "\r?\n\r?\n")
      (write-region (point) (point-max) file)
      (message "Saved %s" file))))

(defun eww-decode-url-file-name (string)
  (let* ((binary (url-unhex-string string))
         (decoded
          (decode-coding-string
           binary
           ;; Possibly set by `universal-coding-system-argument'.
           (or coding-system-for-read
               ;; RFC 3986 says that %AB stuff is utf-8.
               (if (equal (decode-coding-string binary 'utf-8)
                          '(unicode))
                   'utf-8
                 ;; But perhaps not.
                 (car (detect-coding-string binary))))))
         (encodes (find-coding-systems-string decoded)))
    (if (or (equal encodes '(undecided))
            (memq (coding-system-base (or file-name-coding-system
                                          default-file-name-coding-system))
                  encodes))
        decoded
      ;; If we can't encode the decoded file name (due to language
      ;; environment settings), then we return the original, hexified
      ;; string.
      string)))

(defun eww-make-unique-file-name (file directory)
  (cond
   ((zerop (length file))
    (setq file "!"))
   ((string-match "\\`[.]" file)
    (setq file (concat "!" file))))
  (let ((count 1)
        (stem file)
        (suffix ""))
    (when (string-match "\\`\\(.*\\)\\([.][^.]+\\)" file)
      (setq stem (match-string 1 file)
            suffix (match-string 2)))
    (while (file-exists-p (expand-file-name file directory))
      (setq file (format "%s(%d)%s" stem count suffix))
      (setq count (1+ count)))
    (expand-file-name file directory)))

(defun eww-set-character-encoding (charset)
  "Set character encoding to CHARSET.
If CHARSET is nil then use UTF-8."
  (interactive "zUse character set (default utf-8): ")
  (if (null charset)
      (eww-reload nil 'utf-8)
    (eww-reload nil charset)))

(defun eww-switch-to-buffer ()
  "Prompt for an EWW buffer to display in the selected window."
  (interactive)
  (let ((completion-extra-properties
         '(:annotation-function (lambda (buf)
                                  (with-current-buffer buf
                                    (format " %s" (eww-current-url)))))))
    (pop-to-buffer-same-window
     (read-buffer "Switch to EWW buffer: "
                  (cl-loop for buf in (nreverse (buffer-list))
                           if (with-current-buffer buf (derived-mode-p 'eww-mode))
                           return buf)
                  t
                  (lambda (bufn)
                    (with-current-buffer
                        (if (consp bufn) (cdr bufn) (get-buffer bufn))
                      (derived-mode-p 'eww-mode)))))))

(defun eww-toggle-fonts ()
  "Toggle whether to use monospaced or font-enabled layouts."
  (interactive)
  (setq shr-use-fonts (not shr-use-fonts))
  (eww-reload)
  (message "Proportional fonts are now %s"
           (if shr-use-fonts "on" "off")))

(defun eww-toggle-colors ()
  "Toggle whether to use HTML-specified colors or not."
  (interactive)
  (message "Colors are now %s"
	   (if (setq shr-use-colors (not shr-use-colors))
	       "on"
	     "off"))
  (eww-reload))

;;; Bookmarks code

(defvar eww-bookmarks nil)

(defun eww-add-bookmark ()
  "Bookmark the current page."
  (interactive)
  (eww-read-bookmarks)
  (dolist (bookmark eww-bookmarks)
    (when (equal (plist-get eww-data :url) (plist-get bookmark :url))
      (user-error "Already bookmarked")))
  (when (y-or-n-p "Bookmark this page?")
    (let ((title (replace-regexp-in-string "[\n\t\r]" " "
					   (plist-get eww-data :title))))
      (setq title (replace-regexp-in-string "\\` +\\| +\\'" "" title))
      (push (list :url (plist-get eww-data :url)
		  :title title
		  :time (current-time-string))
	    eww-bookmarks))
    (eww-write-bookmarks)
    (message "Bookmarked %s (%s)" (plist-get eww-data :url)
	     (plist-get eww-data :title))))

(defun eww-write-bookmarks ()
  (with-temp-file (expand-file-name "eww-bookmarks" eww-bookmarks-directory)
    (insert ";; Auto-generated file; don't edit\n")
    (pp eww-bookmarks (current-buffer))))

(defun eww-read-bookmarks ()
  (let ((file (expand-file-name "eww-bookmarks" eww-bookmarks-directory)))
    (setq eww-bookmarks
	  (unless (zerop (or (nth 7 (file-attributes file)) 0))
	    (with-temp-buffer
	      (insert-file-contents file)
	      (read (current-buffer)))))))

;;;###autoload
(defun eww-list-bookmarks ()
  "Display the bookmarks."
  (interactive)
  (pop-to-buffer "*eww bookmarks*")
  (eww-bookmark-prepare))

(defun eww-bookmark-prepare ()
  (eww-read-bookmarks)
  (unless eww-bookmarks
    (user-error "No bookmarks are defined"))
  (set-buffer (get-buffer-create "*eww bookmarks*"))
  (eww-bookmark-mode)
  (let* ((width (/ (window-width) 2))
	 (format (format "%%-%ds %%s" width))
	 (inhibit-read-only t)
	 start title)
    (erase-buffer)
    (setq header-line-format (concat " " (format format "Title" "URL")))
    (dolist (bookmark eww-bookmarks)
      (setq start (point)
	    title (plist-get bookmark :title))
      (when (> (length title) width)
	(setq title (truncate-string-to-width title width)))
      (insert (format format title (plist-get bookmark :url)) "\n")
      (put-text-property start (1+ start) 'eww-bookmark bookmark))
    (goto-char (point-min))))

(defvar eww-bookmark-kill-ring nil)

(defun eww-bookmark-kill ()
  "Kill the current bookmark."
  (interactive)
  (let* ((start (line-beginning-position))
	 (bookmark (get-text-property start 'eww-bookmark))
	 (inhibit-read-only t))
    (unless bookmark
      (user-error "No bookmark on the current line"))
    (forward-line 1)
    (push (buffer-substring start (point)) eww-bookmark-kill-ring)
    (delete-region start (point))
    (setq eww-bookmarks (delq bookmark eww-bookmarks))
    (eww-write-bookmarks)))

(defun eww-bookmark-yank ()
  "Yank a previously killed bookmark to the current line."
  (interactive)
  (unless eww-bookmark-kill-ring
    (user-error "No previously killed bookmark"))
  (beginning-of-line)
  (let ((inhibit-read-only t)
	(start (point))
	bookmark)
    (insert (pop eww-bookmark-kill-ring))
    (setq bookmark (get-text-property start 'eww-bookmark))
    (if (= start (point-min))
	(push bookmark eww-bookmarks)
      (let ((line (count-lines start (point))))
	(setcdr (nthcdr (1- line) eww-bookmarks)
		(cons bookmark (nthcdr line eww-bookmarks)))))
    (eww-write-bookmarks)))

(defun eww-bookmark-browse ()
  "Browse the bookmark under point in eww."
  (interactive)
  (let ((bookmark (get-text-property (line-beginning-position) 'eww-bookmark)))
    (unless bookmark
      (user-error "No bookmark on the current line"))
    (quit-window)
    (eww-browse-url (plist-get bookmark :url))))

(defun eww-next-bookmark ()
  "Go to the next bookmark in the list."
  (interactive)
  (let ((first nil)
	bookmark)
    (unless (get-buffer "*eww bookmarks*")
      (setq first t)
      (eww-bookmark-prepare))
    (with-current-buffer (get-buffer "*eww bookmarks*")
      (when (and (not first)
		 (not (eobp)))
	(forward-line 1))
      (setq bookmark (get-text-property (line-beginning-position)
					'eww-bookmark))
      (unless bookmark
	(user-error "No next bookmark")))
    (eww-browse-url (plist-get bookmark :url))))

(defun eww-previous-bookmark ()
  "Go to the previous bookmark in the list."
  (interactive)
  (let ((first nil)
	bookmark)
    (unless (get-buffer "*eww bookmarks*")
      (setq first t)
      (eww-bookmark-prepare))
    (with-current-buffer (get-buffer "*eww bookmarks*")
      (if first
	  (goto-char (point-max))
	(beginning-of-line))
      ;; On the final line.
      (when (eolp)
	(forward-line -1))
      (if (bobp)
	  (user-error "No previous bookmark")
	(forward-line -1))
      (setq bookmark (get-text-property (line-beginning-position)
					'eww-bookmark)))
    (eww-browse-url (plist-get bookmark :url))))

(defvar eww-bookmark-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control k)] 'eww-bookmark-kill)
    (define-key map [(control y)] 'eww-bookmark-yank)
    (define-key map "\r" 'eww-bookmark-browse)

    (easy-menu-define nil map
      "Menu for `eww-bookmark-mode-map'."
      '("Eww Bookmark"
        ["Exit" quit-window t]
        ["Browse" eww-bookmark-browse
         :active (get-text-property (line-beginning-position) 'eww-bookmark)]
        ["Kill" eww-bookmark-kill
         :active (get-text-property (line-beginning-position) 'eww-bookmark)]
        ["Yank" eww-bookmark-yank
         :active eww-bookmark-kill-ring]))
    map))

(define-derived-mode eww-bookmark-mode special-mode "eww bookmarks"
  "Mode for listing bookmarks.

\\{eww-bookmark-mode-map}"
  (buffer-disable-undo)
  (setq truncate-lines t))

;;; History code

(defun eww-save-history ()
  (plist-put eww-data :point (point))
  (plist-put eww-data :text (buffer-string))
  (push eww-data eww-history)
  (setq eww-data (list :title ""))
  ;; Don't let the history grow infinitely.  We store quite a lot of
  ;; data per page.
  (when-let* ((tail (and eww-history-limit
		         (nthcdr eww-history-limit eww-history))))
    (setcdr tail nil)))

(defvar eww-current-buffer)

(defun eww-list-histories ()
  "List the eww-histories."
  (interactive)
  (when (null eww-history)
    (error "No eww-histories are defined"))
  (let ((eww-history-trans eww-history)
	(buffer (current-buffer)))
    (set-buffer (get-buffer-create "*eww history*"))
    (eww-history-mode)
    (setq-local eww-current-buffer buffer)
    (let ((inhibit-read-only t)
	  (domain-length 0)
	  (title-length 0)
	  url title format start)
      (erase-buffer)
      (dolist (history eww-history-trans)
	(setq start (point))
	(setq domain-length (max domain-length (length (plist-get history :url))))
	(setq title-length (max title-length (length (plist-get history :title)))))
      (setq format (format "%%-%ds %%-%ds" title-length domain-length)
	    header-line-format
	    (concat " " (format format "Title" "URL")))
      (dolist (history eww-history-trans)
	(setq start (point))
	(setq url (plist-get history :url))
	(setq title (plist-get history :title))
	(insert (format format title url))
	(insert "\n")
	(put-text-property start (1+ start) 'eww-history history))
      (goto-char (point-min)))
    (pop-to-buffer "*eww history*")))

(defun eww-history-browse ()
  "Browse the history under point in eww."
  (interactive)
  (let ((history (get-text-property (line-beginning-position) 'eww-history)))
    (unless history
      (error "No history on the current line"))
    (let ((buffer eww-current-buffer))
      (quit-window)
      (when buffer
	(pop-to-buffer-same-window buffer)))
    (eww-restore-history history)))

(defvar eww-history-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'eww-history-browse)
;;    (define-key map "n" 'next-error-no-select)
;;    (define-key map "p" 'previous-error-no-select)

    (easy-menu-define nil map
      "Menu for `eww-history-mode-map'."
      '("Eww History"
        ["Exit" quit-window t]
        ["Browse" eww-history-browse
         :active (get-text-property (line-beginning-position) 'eww-history)]))
    map))

(define-derived-mode eww-history-mode special-mode "eww history"
  "Mode for listing eww-histories.

\\{eww-history-mode-map}"
  (buffer-disable-undo)
  (setq truncate-lines t))

;;; eww buffers list

(defun eww-list-buffers ()
  "Enlist eww buffers."
  (interactive)
  (let (buffers-info
        (current (current-buffer)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'eww-mode)
          (push (vector buffer (plist-get eww-data :title)
                        (plist-get eww-data :url))
                buffers-info))))
    (unless buffers-info
      (error "No eww buffers"))
    (setq buffers-info (nreverse buffers-info)) ;more recent on top
    (set-buffer (get-buffer-create "*eww buffers*"))
    (eww-buffers-mode)
    (let ((inhibit-read-only t)
          (domain-length 0)
          (title-length 0)
          url title format start)
      (erase-buffer)
      (dolist (buffer-info buffers-info)
        (setq title-length (max title-length
                                (length (elt buffer-info 1)))
              domain-length (max domain-length
                                 (length (elt buffer-info 2)))))
      (setq format (format "%%-%ds %%-%ds" title-length domain-length)
            header-line-format
            (concat " " (format format "Title" "URL")))
      (let ((line 0)
            (current-buffer-line 1))
        (dolist (buffer-info buffers-info)
          (setq start (point)
                title (elt buffer-info 1)
                url (elt buffer-info 2)
                line (1+ line))
          (insert (format format title url))
          (insert "\n")
          (let ((buffer (elt buffer-info 0)))
            (put-text-property start (1+ start) 'eww-buffer
                               buffer)
            (when (eq current buffer)
              (setq current-buffer-line line))))
        (goto-char (point-min))
        (forward-line (1- current-buffer-line)))))
  (pop-to-buffer "*eww buffers*"))

(defun eww-buffer-select ()
  "Switch to eww buffer."
  (interactive)
  (let ((buffer (get-text-property (line-beginning-position)
                                   'eww-buffer)))
    (unless buffer
      (error "No buffer on current line"))
    (quit-window)
    (pop-to-buffer-same-window buffer)))

(defun eww-buffer-show ()
  "Display buffer under point in eww buffer list."
  (let ((buffer (get-text-property (line-beginning-position)
                                   'eww-buffer)))
    (unless buffer
      (error "No buffer on current line"))
    (other-window -1)
    (pop-to-buffer-same-window buffer)
    (other-window 1)))

(defun eww-buffer-show-next ()
  "Move to next eww buffer in the list and display it."
  (interactive)
  (forward-line)
  (when (eobp)
    (goto-char (point-min)))
  (eww-buffer-show))

(defun eww-buffer-show-previous ()
  "Move to previous eww buffer in the list and display it."
  (interactive)
  (beginning-of-line)
  (when (bobp)
    (goto-char (point-max)))
  (forward-line -1)
  (eww-buffer-show))

(defun eww-buffer-kill ()
  "Kill buffer from eww list."
  (interactive)
  (let* ((start (line-beginning-position))
	 (buffer (get-text-property start 'eww-buffer))
	 (inhibit-read-only t))
    (unless buffer
      (user-error "No buffer on the current line"))
    (kill-buffer buffer)
    (forward-line 1)
    (delete-region start (point)))
  (when (eobp)
    (forward-line -1))
  (eww-buffer-show))

(defvar eww-buffers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control k)] 'eww-buffer-kill)
    (define-key map "\r" 'eww-buffer-select)
    (define-key map "n" 'eww-buffer-show-next)
    (define-key map "p" 'eww-buffer-show-previous)

    (easy-menu-define nil map
      "Menu for `eww-buffers-mode-map'."
      '("Eww Buffers"
        ["Exit" quit-window t]
        ["Select" eww-buffer-select
         :active (get-text-property (line-beginning-position) 'eww-buffer)]
        ["Kill" eww-buffer-kill
         :active (get-text-property (line-beginning-position) 'eww-buffer)]))
    map))

(define-derived-mode eww-buffers-mode special-mode "eww buffers"
  "Mode for listing buffers.

\\{eww-buffers-mode-map}"
  (buffer-disable-undo)
  (setq truncate-lines t))

;;; Desktop support

(defvar eww-desktop-data-save
  '(:url :title :point)
  "List of `eww-data' properties to preserve in the desktop file.
Also used when saving `eww-history'.")

(defun eww-desktop-data-1 (alist)
  (let ((acc  nil)
        (tail alist))
    (while tail
      (let ((k (car  tail))
            (v (cadr tail)))
        (when (memq k eww-desktop-data-save)
          (setq acc (cons k (cons v acc)))))
      (setq tail  (cddr tail)))
    acc))

(defun eww-desktop-history-duplicate (a b)
  (let ((tail a) (r t))
    (while tail
      (if (or (memq (car tail) '(:point)) ; ignore :point
	      (equal (cadr tail)
		     (plist-get b (car tail))))
	  (setq tail (cddr tail))
	(setq tail nil
	      r    nil)))
    ;; .
    r))

(defun eww-desktop-misc-data (_directory)
  "Return a property list with data used to restore eww buffers.
This list will contain, as :history, the list, whose first element is
the value of `eww-data', and the tail is `eww-history'.

If `eww-desktop-remove-duplicates' is non-nil, duplicate
entries (if any) will be removed from the list.

Only the properties listed in `eww-desktop-data-save' are included.
Generally, the list should not include the (usually overly large)
:dom, :source and :text properties."
  (let ((history  (mapcar 'eww-desktop-data-1
			  (cons eww-data eww-history))))
    (list :history  (if eww-desktop-remove-duplicates
			(cl-remove-duplicates
			 history :test 'eww-desktop-history-duplicate)
		      history))))

(defun eww-restore-desktop (file-name buffer-name misc-data)
  "Restore an eww buffer from its desktop file record.
If `eww-restore-desktop' is t or `auto', this function will also
initiate the retrieval of the respective URI in the background.
Otherwise, the restored buffer will contain a prompt to do so by using
\\[eww-reload]."
  (with-current-buffer (get-buffer-create buffer-name)
    (eww-mode)
    ;; NB: eww-history, eww-data are buffer-local per (eww-mode)
    (setq eww-history       (cdr (plist-get misc-data :history))
	  eww-data      (or (car (plist-get misc-data :history))
			    ;; backwards compatibility
			    (list :url (plist-get misc-data :uri))))
    (unless file-name
      (when (plist-get eww-data :url)
	(cl-case eww-restore-desktop
	  ((t auto) (eww (plist-get eww-data :url)))
	  ((zerop (buffer-size))
	   (let ((inhibit-read-only t))
	     (insert (substitute-command-keys
		      eww-restore-reload-prompt)))))))
    ;; .
    (current-buffer)))

(add-to-list 'desktop-locals-to-save
	     'eww-history-position)
(add-to-list 'desktop-buffer-mode-handlers
             '(eww-mode . eww-restore-desktop))

;;; Isearch support

(defun eww-isearch-next-buffer (&optional _buffer wrap)
  "Go to the next page to search using `rel' attribute for navigation."
  (if wrap
      (condition-case nil
	  (eww-top-url)
	(error nil))
    (if isearch-forward
	(eww-next-url)
      (eww-previous-url)))
  (current-buffer))

(provide 'eww)

;;; eww.el ends here
