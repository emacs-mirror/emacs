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
(require 'shr)
(require 'url)
(require 'mm-url)

(defvar eww-current-url nil)
(defvar eww-history nil)

;;;###autoload
(defun eww (url)
  "Fetch URL and render the page."
  (interactive "sUrl: ")
  (unless (string-match-p "\\`[a-zA-Z][-a-zA-Z0-9+.]*://" url)
    (setq url (concat "http://" url)))
  (url-retrieve url 'eww-render (list url)))

(defun eww-detect-charset (html-p)
  (let ((case-fold-search t)
	(pt (point)))
    (or (and html-p
	     (re-search-forward
	      "<meta[\t\n\r ]+[^>]*charset=\\([^\t\n\r \"/>]+\\)" nil t)
	     (goto-char pt)
	     (match-string 1))
	(and (looking-at
	      "[\t\n\r ]*<\\?xml[\t\n\r ]+[^>]*encoding=\"\\([^\"]+\\)")
	     (match-string 1)))))

(defun eww-render (status url &optional point)
  (let* ((headers (eww-parse-headers))
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
	  (when point
	    (goto-char point)))
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

(defun eww-display-html (charset url)
  (unless (eq charset 'utf8)
    (decode-coding-region (point) (point-max) charset))
  (let ((document
	 (list
	  'base (list (cons 'href url))
	  (libxml-parse-html-region (point) (point-max)))))
    (eww-setup-buffer)
    (setq eww-current-url url)
    (let ((inhibit-read-only t)
	  (shr-external-rendering-functions
	   '((form . eww-tag-form)
	     (input . eww-tag-input)
	     (select . eww-tag-select))))
      (shr-insert-document document)
      (eww-convert-widgets))
    (goto-char (point-min))))

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
    (define-key map [tab] 'widget-forward)
    (define-key map [backtab] 'widget-backward)
    (define-key map [delete] 'scroll-down-command)
    (define-key map "\177" 'scroll-down-command)
    (define-key map " " 'scroll-up-command)
    (define-key map "p" 'eww-previous-url)
    ;;(define-key map "n" 'eww-next-url)
    map))

(define-derived-mode eww-mode nil "eww"
  "Mode for browsing the web.

\\{eww-mode-map}"
  (set (make-local-variable 'eww-current-url) 'author)
  (set (make-local-variable 'browse-url-browser-function) 'eww-browse-url))

(defun eww-browse-url (url &optional new-window)
  (let ((url-request-extra-headers
	 (append '(("User-Agent" . "eww/1.0"))
		 url-request-extra-headers)))
    (push (list eww-current-url (point))
	  eww-history)
    (eww url)))

(defun eww-quit ()
  "Exit the Emacs Web Wowser."
  (interactive)
  (setq eww-history nil)
  (kill-buffer (current-buffer)))

(defun eww-previous-url ()
  "Go to the previously displayed page."
  (interactive)
  (when (zerop (length eww-history))
    (error "No previous page"))
  (let ((prev (pop eww-history)))
    (url-retrieve (car prev) 'eww-render (list (car prev) (cadr prev)))))

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
	 (widget
	  (cond
	   ((equal type "submit")
	    (list 'push-button
		  :notify 'eww-submit
		  :name (cdr (assq :name cont))
		  :value (cdr (assq :value cont))
		  :eww-form eww-form
		  (or (cdr (assq :value cont)) "Submit")))
	   ((or (equal type "radio")
		(equal type "checkbox"))
	    (list 'checkbox
		  :notify 'eww-click-radio
		  :name (cdr (assq :name cont))
		  :checkbox-value (cdr (assq :value cont))
		  :checkbox-type type
		  :eww-form eww-form
		  (cdr (assq :checked cont))))
	   ((equal type "hidden")
	    (list 'hidden
		  :name (cdr (assq :name cont))
		  :value (cdr (assq :value cont))))
	   (t
	    (list 'editable-field
		  :size (string-to-number
			 (or (cdr (assq :size cont))
			     "40"))
		  :value (or (cdr (assq :value cont)) "")
		  :secret (and (equal type "password") ?*)
		  :action 'eww-submit
		  :name (cdr (assq :name cont))
		  :eww-form eww-form)))))
    (nconc eww-form (list widget))
    (unless (eq (car widget) 'hidden)
      (apply 'widget-create widget)
      (put-text-property start (point) 'eww-widget widget))))

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
    ;; If we have no selected values, default to the first value.
    (unless (plist-get (cdr menu) :value)
      (nconc menu (list :value (nth 2 (car options)))))
    (nconc menu options)
    (apply 'widget-create menu)
    (put-text-property start (point) 'eww-widget menu)
    (shr-ensure-paragraph)))

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
    (debug values)
    (let ((shr-base eww-current-url))
      (if (and (stringp (cdr (assq :method form)))
	       (equal (downcase (cdr (assq :method form))) "post"))
	  (let ((url-request-method "POST")
		(url-request-extra-headers
		 '(("Content-Type" . "application/x-www-form-urlencoded")))
		(url-request-data (mm-url-encode-www-form-urlencoded values)))
	    (eww-browse-url (shr-expand-url (cdr (assq :action form)))))
	(eww-browse-url
	 (concat
	  (if (cdr (assq :action form))
	      (shr-expand-url (cdr (assq :action form)))
	    eww-current-url)
	  "?"
	  (mm-url-encode-www-form-urlencoded values)))))))

(defun eww-convert-widgets ()
  (let ((start (point-min))
	widget)
    ;; Some widgets come from different buffers (rendered for tables),
    ;; so we need to nix out the list of widgets and recreate them.
    (setq widget-field-list nil
	  widget-field-new nil)
    (while (setq start (next-single-property-change start 'eww-widget))
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
	(apply 'widget-create widget)))
    (widget-setup)
    (eww-fix-widget-keymap)))

(defun eww-fix-widget-keymap ()
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (plist-get (overlay-properties overlay) 'button)
      (overlay-put overlay 'local-map widget-keymap))))

(provide 'eww)

;;; eww.el ends here
