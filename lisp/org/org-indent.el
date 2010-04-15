;;; org-indent.el --- Dynamic indentation for  Org-mode
;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.35i
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This is an implementation of dynamic virtual indentation.  It works
;; by adding text properties to a buffer to make sure lines are
;; indented according to outline structure.

(require 'org-macs)
(require 'org-compat)
(require 'org)
(eval-when-compile
  (require 'cl))


(defgroup org-indent nil
  "Options concerning dynamic virtual outline indentation."
  :tag "Org Indent"
  :group 'org)

(defconst org-indent-max 40
  "Maximum indentation in characters")
(defconst org-indent-max-levels 40
  "Maximum indentation in characters")

(defvar org-indent-strings nil
  "Vector with all indentation strings.
It will be set in `org-indent-initialize'.")
(defvar org-indent-stars nil
  "Vector with all indentation star strings.
It will be set in `org-indent-initialize'.")
(defvar org-hide-leading-stars-before-indent-mode nil
  "Used locally")

(defcustom org-indent-boundary-char ?\   ; comment to protect space char
  "The end of the virtual indentation strings, a single-character string.
The default is just a space, but if you wish, you can use \"|\" or so.
This can be useful on a terminal window - under a windowing system,
it may be prettier to customize the org-indent face."
  :group 'org-indent
  :set (lambda (var val)
	 (set var val)
	 (and org-indent-strings (org-indent-initialize)))
  :type 'character)

(defcustom org-indent-mode-turns-off-org-adapt-indentation t
  "Non-nil means turning on `org-indent-mode' turns off indentation adaptation.
For details see the variable `org-adapt-indentation'."
  :group 'org-indent
  :type 'boolean)

(defcustom org-indent-mode-turns-on-hiding-stars t
  "Non-nil means turning on `org-indent-mode' turns on `org-hide-leading-stars'."
  :group 'org-indent
  :type 'boolean)

(defcustom org-indent-indentation-per-level 2
  "Indentation per level in number of characters."
  :group 'org-indent
  :type 'integer)

(defcustom org-indent-fix-section-after-idle-time 0.2
  "Seconds of idle time before fixing virtual indentation of section.
The hooking-in of virtual indentation is not yet perfect.  Occasionally,
a change does not trigger to proper change of indentation.  For this we
have a timer action that fixes indentation in the current section after
a short amount idle time.  If we ever get the integration to work perfectly,
this variable can be set to nil to get rid of the timer."
  :group 'org-indent
  :type '(choice
	  (const "Do not install idle timer" nil)
	  (number :tag "Idle time")))

(defun org-indent-initialize ()
  "Initialize the indentation strings and set the idle timer."
  ;; We use an idle timer to "repair" the current section, because the
  ;; redisplay seems to have some problems.
  (unless org-indent-strings
    (when org-indent-fix-section-after-idle-time
      (run-with-idle-timer
       org-indent-fix-section-after-idle-time
       t 'org-indent-refresh-section)))
  ;; Initialize the indentation and star vectors
  (setq org-indent-strings (make-vector (1+ org-indent-max) nil))
  (setq org-indent-stars (make-vector (1+ org-indent-max) nil))
  (aset org-indent-strings 0 nil)
  (aset org-indent-stars 0 nil)
  (loop for i from 1 to org-indent-max do
	(aset org-indent-strings i
	      (org-add-props
		  (concat (make-string (1- i) ?\ )
			  (char-to-string org-indent-boundary-char))
		  nil 'face 'org-indent)))
  (loop for i from 1 to org-indent-max-levels do
	(aset org-indent-stars i
	      (org-add-props (make-string i ?*)
		  nil 'face 'org-hide))))

;;;###autoload
(define-minor-mode org-indent-mode
  "When active, indent text according to outline structure.

Internally this works by adding `line-prefix' properties to all non-headlines.
These properties are updated locally in idle time.
FIXME:  How to update when broken?"
  nil " Ind" nil
  (if (org-bound-and-true-p org-inhibit-startup)
      (setq org-indent-mode nil)
    (if org-indent-mode
	(progn
	  (or org-indent-strings (org-indent-initialize))
	  (when org-indent-mode-turns-off-org-adapt-indentation
	    (org-set-local 'org-adapt-indentation nil))
	  (when org-indent-mode-turns-on-hiding-stars
	    (org-set-local 'org-hide-leading-stars-before-indent-mode
			   org-hide-leading-stars)
	    (org-set-local 'org-hide-leading-stars t))
	  (make-local-variable 'buffer-substring-filters)
	  (add-to-list 'buffer-substring-filters
		       'org-indent-remove-properties-from-string)
	  (org-add-hook 'org-after-demote-entry-hook
			'org-indent-refresh-section nil 'local)
	  (org-add-hook 'org-after-promote-entry-hook
			'org-indent-refresh-section nil 'local)
	  (org-add-hook 'org-font-lock-hook
			'org-indent-refresh-to nil 'local)
	  (and font-lock-mode (org-restart-font-lock))
	  )
      (save-excursion
	(save-restriction
	  (org-indent-remove-properties (point-min) (point-max))
	  (kill-local-variable 'org-adapt-indentation)
	  (when (boundp 'org-hide-leading-stars-before-indent-mode)
	    (org-set-local 'org-hide-leading-stars
			   org-hide-leading-stars-before-indent-mode))
	  (setq buffer-substring-filters
		(delq 'org-indent-remove-properties-from-string
		      buffer-substring-filters))
	  (remove-hook 'org-after-promote-entry-hook
		       'org-indent-refresh-section 'local)
	  (remove-hook 'org-after-demote-entry-hook
		       'org-indent-refresh-section 'local)
	  (and font-lock-mode (org-restart-font-lock))
	  (redraw-display))))))


(defface org-indent
  (org-compatible-face nil nil)
  "Face for outline indentation.
The default is to make it look like whitespace.  But you may find it
useful to make it ever so slightly different."
  :group 'org-faces)

(defun org-indent-indent-buffer ()
  "Add indentation properties for the whole buffer."
  (interactive)
  (when org-indent-mode
    (save-excursion
      (save-restriction
	(widen)
	(org-indent-remove-properties (point-min) (point-max))
	(org-indent-add-properties (point-min) (point-max))))))

(defun org-indent-remove-properties (beg end)
  "Remove indentations between BEG and END."
  (org-unmodified
   (remove-text-properties beg end '(line-prefix nil wrap-prefix nil))))

(defun org-indent-remove-properties-from-string (string)
  "Remove indentations between BEG and END."
  (remove-text-properties 0 (length string)
			  '(line-prefix nil wrap-prefix nil) string)
  string)

(defvar org-indent-outline-re (concat "^" org-outline-regexp)
  "Outline heading regexp.")

(defun org-indent-add-properties (beg end)
  "Add indentation properties between BEG and END.
Assumes that BEG is at the beginning of a line."
  (when (or t org-indent-mode)
    (let (ov b e n level exit nstars)
      (org-unmodified
       (save-excursion
	 (goto-char beg)
	 (while (not exit)
	   (setq e end)
	   (if (not (re-search-forward org-indent-outline-re nil t))
	       (setq e (point-max) exit t)
	     (setq e (match-beginning 0))
	     (if (>= e end) (setq exit t))
	     (setq level (- (match-end 0) (match-beginning 0) 1))
	     (setq nstars (- (* (1- level) org-indent-indentation-per-level)
			     (1- level)))
	     (add-text-properties
	      (point-at-bol) (point-at-eol)
	      (list 'line-prefix
		    (aref org-indent-stars nstars)
		    'wrap-prefix
		    (aref org-indent-strings
			  (* level org-indent-indentation-per-level)))))
	   (when (and b (> e b))
	     (add-text-properties
	      b  e (list 'line-prefix (aref org-indent-strings n)
			 'wrap-prefix (aref org-indent-strings n))))
	   (setq b (1+ (point-at-eol))
		 n (* (or level 0) org-indent-indentation-per-level))))))))

(defun org-indent-refresh-section ()
  "Refresh indentation properties in the current outline section.
Point is assumed to be at the beginning of a headline."
  (interactive)
  (when org-indent-mode
    (let (beg end)
      (save-excursion
	(when (ignore-errors (org-back-to-heading))
	  (setq beg (point))
	  (setq end (or (save-excursion (or (outline-next-heading) (point)))))
	  (org-indent-remove-properties beg end)
	  (org-indent-add-properties beg end))))))

(defun org-indent-refresh-to (limit)
  "Refresh indentation properties in the current outline section.
Point is assumed to be at the beginning of a headline."
  (interactive)
  (when org-indent-mode
    (let ((beg (point)) (end limit))
      (save-excursion
	(and (ignore-errors (org-back-to-heading t))
	     (setq beg (point))))
      (org-indent-remove-properties beg end)
      (org-indent-add-properties beg end)))
  (goto-char limit))

(defun org-indent-refresh-subtree ()
  "Refresh indentation properties in the current outline subtree.
Point is assumed to be at the beginning of a headline."
  (interactive)
  (when org-indent-mode
    (save-excursion
      (let (beg end)
	(setq beg (point))
	(setq end (save-excursion (org-end-of-subtree t t)))
	(org-indent-remove-properties beg end)
	(org-indent-add-properties beg end)))))

(defun org-indent-refresh-buffer ()
  "Refresh indentation properties in the current outline subtree.
Point is assumed to be at the beginning of a headline."
  (interactive)
  (when org-indent-mode
    (org-indent-mode -1)
    (org-indent-mode 1)))

(provide 'org-indent)

;; arch-tag: b76736bc-9f4a-43cd-977c-ecfd6689846a
;;; org-indent.el ends here
