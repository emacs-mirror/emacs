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

(defun eww (url)
  "Fetch URL and render the page."
  (interactive "sUrl: ")
  (url-retrieve url 'eww-render (list url)))

(defun eww-render (status url &optional point)
  (let* ((headers (eww-parse-headers))
	 (content-type
	  (mail-header-parse-content-type
	   (or (cdr (assoc "content-type" headers))
	       "text/plain")))
	 (charset (intern
		   (downcase
		    (or (cdr (assq 'charset (cdr content-type)))
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
	     (submit . eww-tag-submit))))
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
    (define-key map [tab] 'widget-forward)
    (define-key map [backtab] 'widget-backward)
    (define-key map [delete] 'scroll-down-command)
    (define-key map "\177" 'scroll-down-command)
    (define-key map " " 'scroll-up-command)
    (define-key map "p" 'eww-previous-url)
    ;;(define-key map "n" 'eww-next-url)
    map))

(defun eww-mode ()
  "Mode for browsing the web.

\\{eww-mode-map}"
  (interactive)
  (setq major-mode 'eww-mode
	mode-name "eww")
  (set (make-local-variable 'eww-current-url) 'author)
  (set (make-local-variable 'browse-url-browser-function) 'eww-browse-url)
  ;;(setq buffer-read-only t)
  (use-local-map eww-mode-map))

(defun eww-browse-url (url &optional new-window)
  (push (list eww-current-url (point))
	eww-history)
  (eww url))

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

;; Form support.

(defvar eww-form nil)

(defun eww-tag-form (cont)
  (let ((eww-form
	 (list (assq :method cont)
	       (assq :action cont)))
	(start (point)))
    (shr-ensure-paragraph)
    (shr-generic cont)
    (shr-ensure-paragraph)
    (put-text-property start (1+ start)
		       'eww-form eww-form)))

(defun eww-tag-input (cont)
  (let ((start (point))
	(widget (list
		 'editable-field
		 :size (string-to-number
			(or (cdr (assq :size cont))
			    "40"))
		 :value (or (cdr (assq :value cont)) "")
		 :action 'eww-submit
		 :name (cdr (assq :name cont))
		 :eww-form eww-form)))
    (apply 'widget-create widget)
    (shr-generic cont)
    (put-text-property start (point) 'eww-widget widget)))

(defun eww-submit (widget dummy)
  (let ((form (getf (cdr widget) :eww-form))
	values)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (let ((field (getf (overlay-properties overlay) 'field)))
	(when (eq (getf (cdr field) :eww-form) form)
	  (let ((name (getf (cdr field) :name)))
	    (when name
	      (push (cons name (widget-value field))
		    values))))))
    (let ((shr-base eww-current-url))
      (if (and (stringp (getf form :method))
	       (equal (downcase (getf form :method)) "post"))
	  (let ((url-request-method "POST")
		(url-request-data (mm-url-encode-www-form-urlencoded values)))
	    (eww-browse-url (shr-expand-url (getf form :action))))
	(eww-browse-url
	 (shr-expand-url
	  (concat
	   (getf form :action)
	   "?"
	   (mm-url-encode-www-form-urlencoded values))))))))

(defun eww-convert-widgets ()
  (let ((start (point-min))
	widget)
    (while (setq start (next-single-property-change start 'eww-widget))
      (setq widget (get-text-property start 'eww-widget))
      (goto-char start)
      (delete-region start (next-single-property-change start 'eww-widget))
      (apply 'widget-create widget)
      (put-text-property start (point) 'not-read-only t))
    (widget-setup)))

(provide 'eww)

;;; eww.el ends here
