;;; gnus-bcklg.el --- backlog functions for Gnus

;; Copyright (C) 1996-2020 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

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

;; The backlog caches the text of a certain number of read articles in
;; a separate buffer, so they can be retrieved quickly if the user
;; opens them again.  Also see `gnus-keep-backlog'.

;;; Code:

(require 'gnus)

(defvar gnus-backlog-buffer " *Gnus Backlog*")
(defvar gnus-backlog-articles '())

(defun gnus-backlog-buffer ()
  "Return the backlog buffer."
  (or (get-buffer gnus-backlog-buffer)
      (with-current-buffer (gnus-get-buffer-create gnus-backlog-buffer)
	(buffer-disable-undo)
	(setq buffer-read-only t)
	(get-buffer gnus-backlog-buffer))))

(gnus-add-shutdown 'gnus-backlog-shutdown 'gnus)

(defun gnus-backlog-shutdown ()
  "Clear all backlog variables and buffers."
  (interactive)
  (gnus-kill-buffer gnus-backlog-buffer)
  (setq gnus-backlog-articles nil))

(defun gnus-backlog-enter-article (group number buffer)
  (when (and (numberp number)
	     (not (gnus-virtual-group-p group)))
    (let ((ident (format "%s:%d" group number))
	  b)
     (unless (member ident gnus-backlog-articles) ; It's already kept.
       ;; Remove the oldest article, if necessary.
       (and (numberp gnus-keep-backlog)
	    (>= (length gnus-backlog-articles) gnus-keep-backlog)
	    (gnus-backlog-remove-oldest-article))
       (push ident gnus-backlog-articles)
       ;; Insert the new article.
       (with-current-buffer (gnus-backlog-buffer)
	 (let (buffer-read-only)
	   (goto-char (point-max))
	   (unless (bolp)
	     (insert "\n"))
	   (setq b (point))
	   (insert-buffer-substring buffer)
	   ;; Tag the beginning of the article with the ident.
	   (if (> (point-max) b)
	       (put-text-property b (1+ b) 'gnus-backlog ident)
	     (gnus-error 3 "Article %d is blank" number))))))))

(defun gnus-backlog-remove-oldest-article ()
  (with-current-buffer (gnus-backlog-buffer)
    (goto-char (point-min))
    (unless (zerop (buffer-size)) ; The buffer is empty.
      (let ((ident (get-text-property (point) 'gnus-backlog))
	    buffer-read-only)
	;; Remove the ident from the list of articles.
	(when ident
	  (setq gnus-backlog-articles
		(delete ident gnus-backlog-articles)))
	;; Delete the article itself.
	(delete-region
	 (point) (next-single-property-change
		  (1+ (point)) 'gnus-backlog nil (point-max)))))))

(defun gnus-backlog-remove-article (group number)
  "Remove article NUMBER in GROUP from the backlog."
  (when (numberp number)
    (let ((ident (format "%s:%d" group number))
	  beg)
      (when (member ident gnus-backlog-articles)
	;; It was in the backlog.
	(with-current-buffer (gnus-backlog-buffer)
	  (save-excursion
	    (let (buffer-read-only)
	      (goto-char (point-min))
	      (when (setq beg (gnus-text-property-search
			       'gnus-backlog ident))
		;; Find the end (i. e., the beginning of the next article).
		(goto-char
		 (next-single-property-change
		  (1+ beg) 'gnus-backlog (current-buffer) (point-max)))
		(delete-region beg (point))
		;; Return success.
		t)))
	  (setq gnus-backlog-articles
		(delete ident gnus-backlog-articles)))))))

(defun gnus-backlog-request-article (group number &optional buffer)
  (when (and (numberp number)
	     (not (gnus-virtual-group-p group)))
    (let ((ident (format "%s:%d" group number))
	  beg end)
      (when (member ident gnus-backlog-articles)
	;; It was in the backlog.
	(with-current-buffer (gnus-backlog-buffer)
	  (if (not (setq beg (gnus-text-property-search
			      'gnus-backlog ident)))
	      ;; It wasn't in the backlog after all.
	      (ignore
	       (setq gnus-backlog-articles
		     (delete ident gnus-backlog-articles)))
	    ;; Find the end (i. e., the beginning of the next article).
	    (setq end
		  (next-single-property-change
		   (1+ beg) 'gnus-backlog (current-buffer) (point-max)))))
	(with-current-buffer (or buffer (current-buffer))
	  (let ((inhibit-read-only t))
	    (erase-buffer)
	    (insert-buffer-substring gnus-backlog-buffer beg end)))
	t))))

(provide 'gnus-bcklg)

;;; gnus-bcklg.el ends here
