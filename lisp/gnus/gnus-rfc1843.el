;;; gnus-rfc1843.el --- HZ (rfc1843) decoding interface functions for Gnus

;; Copyright (C) 1998-2020 Free Software Foundation, Inc.

;; Author: Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: news HZ HZ+ mail i18n

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

;; Usage:
;; (require 'gnus-rfc1843)
;; (rfc1843-gnus-setup)

;;; Code:

(require 'rfc1843)
(require 'gnus-sum)
(require 'gnus-art)
(require 'message)

(defun rfc1843-decode-article-body ()
  "Decode HZ encoded text in the article body."
  (if (string-match (concat "\\<\\(" rfc1843-newsgroups-regexp "\\)\\>")
		    (or gnus-newsgroup-name ""))
      (save-excursion
	(save-restriction
	  (message-narrow-to-head)
	  (let* ((inhibit-point-motion-hooks t)
		 (case-fold-search t)
		 (ct (message-fetch-field "Content-Type" t))
		 (ctl (and ct (mail-header-parse-content-type ct))))
	    (if (and ctl (not (string-match "/" (car ctl))))
		(setq ctl nil))
	    (goto-char (point-max))
	    (widen)
	    (forward-line 1)
	    (narrow-to-region (point) (point-max))
	    (when (or (not ctl)
		      (equal (car ctl) "text/plain"))
	      (rfc1843-decode-region (point) (point-max))))))))

(defun rfc1843-gnus-setup ()
  "Setup HZ decoding for Gnus."
  (add-hook 'gnus-article-decode-hook 'rfc1843-decode-article-body t)
  (setq gnus-decode-encoded-word-function
	'gnus-multi-decode-encoded-word-string
	gnus-decode-header-function
	'gnus-multi-decode-header
	gnus-decode-encoded-word-methods
	(nconc gnus-decode-encoded-word-methods
	       (list
		(cons (concat "\\<\\(" rfc1843-newsgroups-regexp "\\)\\>")
		      'rfc1843-decode-string)))
	gnus-decode-header-methods
	(nconc gnus-decode-header-methods
	       (list
		(cons (concat "\\<\\(" rfc1843-newsgroups-regexp "\\)\\>")
		      'rfc1843-decode-region)))))

(provide 'gnus-rfc1843)

;;; gnus-rfc1843.el ends here
