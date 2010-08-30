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

;;;###autoload
(defun gnus-article-html (handle)
  (let ((article-buffer (current-buffer)))
    (save-restriction
      (narrow-to-region (point) (point))
      (save-excursion
	(set-buffer (car handle))
	(call-process-region (point-min) (point-max)
			     "w3m" 
			     nil article-buffer nil
			     "-halfdump"
			     "-T" "text/html"))
      (gnus-html-wash-tags))))

(defun gnus-html-wash-tags ()
  (let (tag parameters string start end)
    ;;(subst-char-in-region (point-min) (point-max) ?_ ? )
    (goto-char (point-min))
    (while (re-search-forward "<\\([^ ]+\\)\\([^>]*\\)>\\([^<]*\\)<[^>]*>" nil t)
      (setq tag (match-string 1)
	    parameters (match-string 2)
	    string (match-string 3)
	    start (match-beginning 0)
	    end (+ start (length string)))
      (replace-match string)
      (cond
       ;; Fetch and insert a picture.
       ((equal tag "img_alt")
	;;
	)
       ;; Add a link.
       ((equal tag "a")
	(when (string-match "href=\"\\([^\"]+\\)" parameters)
	  (setq parameters (match-string 1 parameters))
	  (gnus-article-add-button start end
				   'browse-url parameters)
	  (let ((overlay (gnus-make-overlay start end)))
	    (gnus-overlay-put overlay 'evaporate t)
	    (gnus-overlay-put overlay 'gnus-button-url parameters)
	    (when gnus-article-mouse-face
	      (gnus-overlay-put overlay 'mouse-face gnus-article-mouse-face)))))
       ;; Whatever.  Just ignore the tag.
       (t
	(replace-match string))))))

;;; gnus-html.el ends here
