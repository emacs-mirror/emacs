;;; eww.el --- Emacs Web Wowser

;; Copyright (C) 2013 Free Software Foundation, Inc.

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

;;; Code:

(eval-when-compile (require 'cl))
(require 'format-spec)
(require 'shr)
(require 'url)
(require 'mm-url)

(defgroup eww nil
  "Emacs Web Wowser"
  :version "24.4"
  :group 'hypermedia
  :prefix "eww-")

(defcustom eww-header-line-format "%t: %u"
  "Header line format.
- %t is replaced by the title.
- %u is replaced by the URL."
  :group 'eww
  :type 'string)

(defface eww-button
  '((((type x w32 ns) (class color))	; Like default mode line
     :box (:line-width 2 :style released-button)
     :background "lightgrey" :foreground "black"))
  "Face for eww buffer buttons."
  :version "24.4"
  :group 'eww)

(defvar eww-current-url nil)
(defvar eww-current-title ""
  "Title of current page.")
(defvar eww-history nil)

(defvar eww-next-url nil)
(defvar eww-previous-url nil)
(defvar eww-up-url nil)
(defvar eww-top-url nil)

;;;###autoload
(defun eww (url)
  "Fetch URL and render the page."
  (interactive "sUrl: ")
  (unless (string-match-p "\\`[a-zA-Z][-a-zA-Z0-9+.]*://" url)
    (setq url (concat "http://" url)))
  (url-retrieve url 'eww-render (list url)))

;;;###autoload
(defun eww-open-file (file)
  "Render a file using EWW."
  (interactive "fFile: ")
  (eww (concat "file://" (expand-file-name file))))

(defun eww-render (status url &optional point)
  (let ((redirect (plist-get status :redirect)))
    (when redirect
      (setq url redirect)))
  (set (make-local-variable 'eww-next-url) nil)
  (set (make-local-variable 'eww-previous-url) nil)
  (set (make-local-variable 'eww-up-url) nil)
  (set (make-local-variable 'eww-top-url) nil)
  (let* ((headers (eww-parse-headers))
	 (shr-target-id
	  (and (string-match "#\\(.*\\)" url)
	       (match-string 1 url)))
	 (content-type
	  (mail-header-parse-content-type
	   (or (cdr (assoc "content-type" headers))
	       "text/plain")))
	 (charset (intern
		   (downcase
		    (or (cdr (assq 'charset (cdr content-type)))
			(eww-detect-charset (equal (car content-type)
						   "text/html"))
			"utf8"))))
	 (data-buffer (current-buffer)))
    (unwind-protect
	(progn
	  (cond
	   ((equal (car content-type) "text/html")
	    (eww-display-html charset url))
	   ((string-match "^image/" (car content-type))
	    (eww-display-image))
	   (t
	    (eww-display-raw charset)))
	  (cond
	   (point
	    (goto-char point))
	   (shr-target-id
	    (let ((point (next-single-property-change
			  (point-min) 'shr-target-id)))
	      (when point
		(goto-char (1+ point)))))))
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
	      "<meta[\t\n\r ]+[^>]*charset=\"?\\([^\t\n\r \"/>]+\\)" nil t)
	     (goto-char pt)
	     (match-string 1))
	(and (looking-at
	      "[\t\n\r ]*<\\?xml[\t\n\r ]+[^>]*encoding=\"\\([^\"]+\\)")
	     (match-string 1)))))

(defun eww-display-html (charset url)
  (unless (eq charset 'utf8)
    (decode-coding-region (point) (point-max) charset))
  (let ((document
	 (list
	  'base (list (cons 'href url))
	  (libxml-parse-html-region (point) (point-max)))))
    (eww-setup-buffer)
    (setq eww-current-url url)
    (eww-update-header-line-format)
    (let ((inhibit-read-only t)
	  (shr-width nil)
	  (shr-external-rendering-functions
	   '((title . eww-tag-title)
	     (form . eww-tag-form)
	     (input . eww-tag-input)
	     (textarea . eww-tag-textarea)
	     (body . eww-tag-body)
	     (select . eww-tag-select)
	     (link . eww-tag-link)
	     (a . eww-tag-a))))
      (shr-insert-document document)
      (eww-convert-widgets))
    (goto-char (point-min))))

(defun eww-handle-link (cont)
  (let* ((rel (assq :rel cont))
  	(href (assq :href cont))
	(where (assoc (cdr rel)
		      '(("next" . eww-next-url)
			("previous" . eww-previous-url)
			("start" . eww-top-url)
			("up" . eww-up-url)))))
    (and href
	 where
	 (set (cdr where) (cdr href)))))

(defun eww-tag-link (cont)
  (eww-handle-link cont)
  (shr-generic cont))

(defun eww-tag-a (cont)
  (eww-handle-link cont)
  (shr-tag-a cont))

(defun eww-update-header-line-format ()
  (if eww-header-line-format
      (setq header-line-format (format-spec eww-header-line-format
                                            `((?u . ,eww-current-url)
                                              (?t . ,eww-current-title))))
    (setq header-line-format nil)))

(defun eww-tag-title (cont)
  (setq eww-current-title "")
  (dolist (sub cont)
    (when (eq (car sub) 'text)
      (setq eww-current-title (concat eww-current-title (cdr sub)))))
  (eww-update-header-line-format))

(defun eww-tag-body (cont)
  (let* ((start (point))
	 (fgcolor (cdr (or (assq :fgcolor cont)
                           (assq :text cont))))
	 (bgcolor (cdr (assq :bgcolor cont)))
	 (shr-stylesheet (list (cons 'color fgcolor)
			       (cons 'background-color bgcolor))))
    (shr-generic cont)
    (eww-colorize-region start (point) fgcolor bgcolor)))

(defun eww-colorize-region (start end fg &optional bg)
  (when (or fg bg)
    (let ((new-colors (shr-color-check fg bg)))
      (when new-colors
	(when fg
	  (add-face-text-property start end
				  (list :foreground (cadr new-colors))))
	(when bg
	  (add-face-text-property start end
				  (list :background (car new-colors))))))))

(defun eww-display-raw (charset)
  (let ((data (buffer-substring (point) (point-max))))
    (eww-setup-buffer)
    (let ((inhibit-read-only t))
      (insert data))
    (goto-char (point-min))))

(defun eww-display-image ()
  (let ((data (buffer-substring (point) (point-max))))
    (eww-setup-buffer)
    (let ((inhibit-read-only t))
      (shr-put-image data nil))
    (goto-char (point-min))))

(defun eww-setup-buffer ()
  (pop-to-buffer (get-buffer-create "*eww*"))
  (remove-overlays)
  (setq widget-field-list nil)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (eww-mode))

(defvar eww-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'eww-quit)
    (define-key map "g" 'eww-reload)
    (define-key map [tab] 'shr-next-link)
    (define-key map [backtab] 'shr-previous-link)
    (define-key map [delete] 'scroll-down-command)
    (define-key map "\177" 'scroll-down-command)
    (define-key map " " 'scroll-up-command)
    (define-key map "l" 'eww-back-url)
    (define-key map "n" 'eww-next-url)
    (define-key map "p" 'eww-previous-url)
    (define-key map "u" 'eww-up-url)
    (define-key map "t" 'eww-top-url)
    map))

(define-derived-mode eww-mode nil "eww"
  "Mode for browsing the web.

\\{eww-mode-map}"
  (set (make-local-variable 'eww-current-url) 'author)
  (set (make-local-variable 'browse-url-browser-function) 'eww-browse-url))

(defun eww-browse-url (url &optional new-window)
  (push (list eww-current-url (point))
	eww-history)
  (eww url))

(defun eww-quit ()
  "Exit the Emacs Web Wowser."
  (interactive)
  (setq eww-history nil)
  (kill-buffer (current-buffer)))

(defun eww-back-url ()
  "Go to the previously displayed page."
  (interactive)
  (when (zerop (length eww-history))
    (error "No previous page"))
  (let ((prev (pop eww-history)))
    (url-retrieve (car prev) 'eww-render (list (car prev) (cadr prev)))))

(defun eww-next-url ()
  "Go to the page marked `next'.
A page is marked `next' if rel=\"next\" appears in a <link>
or <a> tag."
  (interactive)
  (if eww-next-url
      (eww-browse-url (shr-expand-url eww-next-url eww-current-url))
    (error "No `next' on this page")))

(defun eww-previous-url ()
  "Go to the page marked `previous'.
A page is marked `previous' if rel=\"previous\" appears in a <link>
or <a> tag."
  (interactive)
  (if eww-previous-url
      (eww-browse-url (shr-expand-url eww-previous-url eww-current-url))
    (error "No `previous' on this page")))

(defun eww-up-url ()
  "Go to the page marked `up'.
A page is marked `up' if rel=\"up\" appears in a <link>
or <a> tag."
  (interactive)
  (if eww-up-url
      (eww-browse-url (shr-expand-url eww-up-url eww-current-url))
    (error "No `up' on this page")))

(defun eww-top-url ()
  "Go to the page marked `top'.
A page is marked `top' if rel=\"start\" appears in a <link>
or <a> tag."
  (interactive)
  (if eww-top-url
      (eww-browse-url (shr-expand-url eww-top-url eww-current-url))
    (error "No `top' on this page")))

(defun eww-reload ()
  "Reload the current page."
  (interactive)
  (url-retrieve eww-current-url 'eww-render
		(list eww-current-url (point))))

;; Form support.

(defvar eww-form nil)

(defun eww-tag-form (cont)
  (let ((eww-form
	 (list (assq :method cont)
	       (assq :action cont)))
	(start (point)))
    (shr-ensure-paragraph)
    (shr-generic cont)
    (unless (bolp)
      (insert "\n"))
    (insert "\n")
    (when (> (point) start)
      (put-text-property start (1+ start)
			 'eww-form eww-form))))

(defun eww-tag-input (cont)
  (let* ((start (point))
	 (type (downcase (or (cdr (assq :type cont))
			     "text")))
	 (value (cdr (assq :value cont)))
	 (widget
	  (cond
	   ((or (equal type "submit")
		(equal type "image"))
	    (list 'push-button
		  :notify 'eww-submit
		  :name (cdr (assq :name cont))
		  :value (if (zerop (length value))
			     "Submit"
			   value)
		  :eww-form eww-form
		  (or (if (zerop (length value))
			  "Submit"
			value))))
	   ((or (equal type "radio")
		(equal type "checkbox"))
	    (list 'checkbox
		  :notify 'eww-click-radio
		  :name (cdr (assq :name cont))
		  :checkbox-value value
		  :checkbox-type type
		  :eww-form eww-form
		  (cdr (assq :checked cont))))
	   ((equal type "hidden")
	    (list 'hidden
		  :name (cdr (assq :name cont))
		  :value value))
	   (t
	    (list 'editable-field
		  :size (string-to-number
			 (or (cdr (assq :size cont))
			     "40"))
		  :value (or value "")
		  :secret (and (equal type "password") ?*)
		  :action 'eww-submit
		  :name (cdr (assq :name cont))
		  :eww-form eww-form)))))
    (nconc eww-form (list widget))
    (unless (eq (car widget) 'hidden)
      (apply 'widget-create widget)
      (put-text-property start (point) 'eww-widget widget)
      (insert " "))))

(defun eww-tag-textarea (cont)
  (let* ((start (point))
	 (widget
	  (list 'text
		:size (string-to-number
		       (or (cdr (assq :cols cont))
			   "40"))
		:value (or (cdr (assq 'text cont)) "")
		:action 'eww-submit
		:name (cdr (assq :name cont))
		:eww-form eww-form)))
    (nconc eww-form (list widget))
    (apply 'widget-create widget)
    (put-text-property start (point) 'eww-widget widget)))

(defun eww-tag-select (cont)
  (shr-ensure-paragraph)
  (let ((menu (list 'menu-choice
		    :name (cdr (assq :name cont))
		    :eww-form eww-form))
	(options nil)
	(start (point)))
    (dolist (elem cont)
      (when (eq (car elem) 'option)
	(when (cdr (assq :selected (cdr elem)))
	  (nconc menu (list :value
			    (cdr (assq :value (cdr elem))))))
	(push (list 'item
		    :value (cdr (assq :value (cdr elem)))
		    :tag (cdr (assq 'text (cdr elem))))
	      options)))
    (when options
      ;; If we have no selected values, default to the first value.
      (unless (plist-get (cdr menu) :value)
	(nconc menu (list :value (nth 2 (car options)))))
      (nconc menu options)
      (apply 'widget-create menu)
      (put-text-property start (point) 'eww-widget menu)
      (shr-ensure-paragraph))))

(defun eww-click-radio (widget &rest ignore)
  (let ((form (plist-get (cdr widget) :eww-form))
	(name (plist-get (cdr widget) :name)))
    (when (equal (plist-get (cdr widget) :type) "radio")
      (if (widget-value widget)
	  ;; Switch all the other radio buttons off.
	  (dolist (overlay (overlays-in (point-min) (point-max)))
	    (let ((field (plist-get (overlay-properties overlay) 'button)))
	      (when (and (eq (plist-get (cdr field) :eww-form) form)
			 (equal name (plist-get (cdr field) :name)))
		(unless (eq field widget)
		  (widget-value-set field nil)))))
	(widget-value-set widget t)))
    (eww-fix-widget-keymap)))

(defun eww-submit (widget &rest ignore)
  (let ((form (plist-get (cdr widget) :eww-form))
	values)
    (dolist (overlay (sort (overlays-in (point-min) (point-max))
			   (lambda (o1 o2)
			     (< (overlay-start o1) (overlay-start o2)))))
      (let ((field (or (plist-get (overlay-properties overlay) 'field)
		       (plist-get (overlay-properties overlay) 'button))))
	(when (eq (plist-get (cdr field) :eww-form) form)
	  (let ((name (plist-get (cdr field) :name)))
	    (when name
	      (cond
	       ((eq (car field) 'checkbox)
		(when (widget-value field)
		  (push (cons name (plist-get (cdr field) :checkbox-value))
			values)))
	       ((eq (car field) 'push-button)
		;; We want the values from buttons if we hit a button,
		;; if it's the first button in the DOM after the field
		;; hit ENTER on.
		(when (and (eq (car widget) 'push-button)
			   (eq widget field))
		  (push (cons name (widget-value field))
			values)))
	       (t
		(push (cons name (widget-value field))
		      values))))))))
    (dolist (elem form)
      (when (and (consp elem)
		 (eq (car elem) 'hidden))
	(push (cons (plist-get (cdr elem) :name)
		    (plist-get (cdr elem) :value))
	      values)))
    ;; If we hit ENTER in a non-button field, include the value of the
    ;; first submit button after it.
    (unless (eq (car widget) 'push-button)
      (let ((rest form)
	    (name (plist-get (cdr widget) :name)))
	(when rest
	  (while (and rest
		      (or (not (consp (car rest)))
			  (not (equal name (plist-get (cdar rest) :name)))))
	    (pop rest)))
	(while rest
	  (let ((elem (pop rest)))
	    (when (and (consp (car rest))
		       (eq (car elem) 'push-button))
	      (push (cons (plist-get (cdr elem) :name)
			  (plist-get (cdr elem) :value))
		    values)
	      (setq rest nil))))))
    (if (and (stringp (cdr (assq :method form)))
	     (equal (downcase (cdr (assq :method form))) "post"))
	(let ((url-request-method "POST")
	      (url-request-extra-headers
	       '(("Content-Type" . "application/x-www-form-urlencoded")))
	      (url-request-data (mm-url-encode-www-form-urlencoded values)))
	  (eww-browse-url (shr-expand-url (cdr (assq :action form))
					  eww-current-url)))
      (eww-browse-url
       (concat
	(if (cdr (assq :action form))
	    (shr-expand-url (cdr (assq :action form))
			    eww-current-url)
	  eww-current-url)
	"?"
	(mm-url-encode-www-form-urlencoded values))))))

(defun eww-convert-widgets ()
  (let ((start (point-min))
	widget)
    ;; Some widgets come from different buffers (rendered for tables),
    ;; so we need to nix out the list of widgets and recreate them.
    (setq widget-field-list nil
	  widget-field-new nil)
    (while (setq start (if (get-text-property start 'eww-widget)
			   start
			 (next-single-property-change start 'eww-widget)))
      (setq widget (get-text-property start 'eww-widget))
      (goto-char start)
      (let ((end (next-single-property-change start 'eww-widget)))
	(dolist (overlay (overlays-in start end))
	  (when (or (plist-get (overlay-properties overlay) 'button)
		    (plist-get (overlay-properties overlay) 'field))
	    (delete-overlay overlay)))
	(delete-region start end))
      (when (and widget
		 (not (eq (car widget) 'hidden)))
	(apply 'widget-create widget)
	(put-text-property start (point) 'help-echo
			   (if (memq (car widget) '(text editable-field))
			       "Input field"
			     "Button"))
	(when (eq (car widget) 'push-button)
	  (add-face-text-property start (point) 'eww-button t))))
    (widget-setup)
    (eww-fix-widget-keymap)))

(defun eww-fix-widget-keymap ()
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (plist-get (overlay-properties overlay) 'button)
      (overlay-put overlay 'local-map widget-keymap))))

(provide 'eww)

;;; eww.el ends here
