;;; gnus-cite.el --- parse citations in articles for Gnus  -*- coding: iso-latin-1 -*-

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002
;;        Free Software Foundation, Inc.

;; Author: Per Abhiddenware

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(require 'gnus)
(require 'gnus-art)
(require 'gnus-range)

;;; Customization:

(defgroup gnus-cite nil
  "Citation."
  :prefix "gnus-cite-"
  :link '(custom-manual "(gnus)Article Highlighting")
  :group 'gnus-article)

(defcustom gnus-cite-reply-regexp
  "^\\(Subject: Re\\|In-Reply-To\\|References\\):"
  "*If headers match this regexp it is reasonable to believe that
article has citations."
  :group 'gnus-cite
  :type 'string)

(defcustom gnus-cite-always-check nil
  "Check article always for citations.  Set it t to check all articles."
  :group 'gnus-cite
  :type '(choice (const :tag "no" nil)
		 (const :tag "yes" t)))

(defcustom gnus-cited-opened-text-button-line-format "%(%{[-]%}%)\n"
  "Format of opened cited text buttons."
  :group 'gnus-cite
  :type 'string)

(defcustom gnus-cited-closed-text-button-line-format "%(%{[+]%}%)\n"
  "Format of closed cited text buttons."
  :group 'gnus-cite
  :type 'string)

(defcustom gnus-cited-lines-visible nil
  "The number of lines of hidden cited text to remain visible.
Or a pair (cons) of numbers which are the number of lines at the top
and bottom of the text, respectively, to remain visible."
  :group 'gnus-cite
  :type '(choice (const :tag "none" nil)
		 integer
		 (cons :tag "Top and Bottom" integer integer)))

(defcustom gnus-cite-parse-max-size 25000
  "Maximum article size (in bytes) where parsing citations is allowed.
Set it to nil to parse all articles."
  :group 'gnus-cite
  :type '(choice (const :tag "all" nil)
		 integer))

(defcustom gnus-cite-prefix-regexp
  ;; The Latin-1 angle quote looks pretty dubious.  -- fx
  "^[]>»|:}+ ]*[]>»|:}+]\\(.*>»\\)?\\|^.*>"
  "*Regexp matching the longest possible citation prefix on a line."
  :group 'gnus-cite
  :type 'regexp)

(defcustom gnus-cite-max-prefix 20
  "Maximum possible length for a citation prefix."
  :group 'gnus-cite
  :type 'integer)

(defcustom gnus-supercite-regexp
  (concat "^\\(" gnus-cite-prefix-regexp "\\)? *"
	  ">>>>> +\"\\([^\"\n]+\\)\" +==")
  "*Regexp matching normal Supercite attribution lines.
The first grouping must match prefixes added by other packages."
  :group 'gnus-cite
  :type 'regexp)

(defcustom gnus-supercite-secondary-regexp "^.*\"\\([^\"\n]+\\)\" +=="
  "Regexp matching mangled Supercite attribution lines.
The first regexp group should match the Supercite attribution."
  :group 'gnus-cite
  :type 'regexp)

(defcustom gnus-cite-minimum-match-count 2
  "Minimum number of identical prefixes before we believe it's a citation."
  :group 'gnus-cite
  :type 'integer)

(defcustom gnus-cite-attribution-prefix
  "In article\\|in <\\|On \\(Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\),\\|-----Original Message-----"
  "*Regexp matching the beginning of an attribution line."
  :group 'gnus-cite
  :type 'regexp)

(defcustom gnus-cite-attribution-suffix
  "\\(\\(wrote\\|writes\\|said\\|says\\|>\\)\\(:\\|\\.\\.\\.\\)\\|-----Original Message-----\\)[ \t]*$"
  "*Regexp matching the end of an attribution line.
The text matching the first grouping will be used as a button."
  :group 'gnus-cite
  :type 'regexp)

(defface gnus-cite-attribution-face '((t
				       (:slant italic)))
  "Face used for attribution lines.")

(defcustom gnus-cite-attribution-face 'gnus-cite-attribution-face
  "Face used for attribution lines.
It is merged with the face for the cited text belonging to the attribution."
  :group 'gnus-cite
  :type 'face)

(defface gnus-cite-face-1 '((((class color)
			      (background dark))
			     (:foreground "light blue"))
			    (((class color)
			      (background light))
			     (:foreground "MidnightBlue"))
			    (t
			     (:slant italic)))
  "Citation face.")

(defface gnus-cite-face-2 '((((class color)
			      (background dark))
			     (:foreground "light cyan"))
			    (((class color)
			      (background light))
			     (:foreground "firebrick"))
			    (t
			     (:slant italic)))
  "Citation face.")

(defface gnus-cite-face-3 '((((class color)
			      (background dark))
			     (:foreground "light yellow"))
			    (((class color)
			      (background light))
			     (:foreground "dark green"))
			    (t
			     (:slant italic)))
  "Citation face.")

(defface gnus-cite-face-4 '((((class color)
			      (background dark))
			     (:foreground "light pink"))
			    (((class color)
			      (background light))
			     (:foreground "OrangeRed"))
			    (t
			     (:slant italic)))
  "Citation face.")

(defface gnus-cite-face-5 '((((class color)
			      (background dark))
			     (:foreground "pale green"))
			    (((class color)
			      (background light))
			     (:foreground "dark khaki"))
			    (t
			     (:slant italic)))
  "Citation face.")

(defface gnus-cite-face-6 '((((class color)
			      (background dark))
			     (:foreground "beige"))
			    (((class color)
			      (background light))
			     (:foreground "dark violet"))
			    (t
			     (:slant italic)))
  "Citation face.")

(defface gnus-cite-face-7 '((((class color)
			      (background dark))
			     (:foreground "orange"))
			    (((class color)
			      (background light))
			     (:foreground "SteelBlue4"))
			    (t
			     (:slant italic)))
  "Citation face.")

(defface gnus-cite-face-8 '((((class color)
			      (background dark))
			     (:foreground "magenta"))
			    (((class color)
			      (background light))
			     (:foreground "magenta"))
			    (t
			     (:slant italic)))
  "Citation face.")

(defface gnus-cite-face-9 '((((class color)
			      (background dark))
			     (:foreground "violet"))
			    (((class color)
			      (background light))
			     (:foreground "violet"))
			    (t
			     (:slant italic)))
  "Citation face.")

(defface gnus-cite-face-10 '((((class color)
			       (background dark))
			      (:foreground "medium purple"))
			     (((class color)
			       (background light))
			      (:foreground "medium purple"))
			     (t
			      (:slant italic)))
  "Citation face.")

(defface gnus-cite-face-11 '((((class color)
			       (background dark))
			      (:foreground "turquoise"))
			     (((class color)
			       (background light))
			      (:foreground "turquoise"))
			     (t
			      (:slant italic)))
  "Citation face.")

(defcustom gnus-cite-face-list
  '(gnus-cite-face-1 gnus-cite-face-2 gnus-cite-face-3 gnus-cite-face-4
		     gnus-cite-face-5 gnus-cite-face-6 gnus-cite-face-7 gnus-cite-face-8
		     gnus-cite-face-9 gnus-cite-face-10 gnus-cite-face-11)
  "*List of faces used for highlighting citations.

When there are citations from multiple articles in the same message,
Gnus will try to give each citation from each article its own face.
This should make it easier to see who wrote what."
  :group 'gnus-cite
  :type '(repeat face))

(defcustom gnus-cite-hide-percentage 50
  "Only hide excess citation if above this percentage of the body."
  :group 'gnus-cite
  :type 'number)

(defcustom gnus-cite-hide-absolute 10
  "Only hide excess citation if above this number of lines in the body."
  :group 'gnus-cite
  :type 'integer)

(defcustom gnus-cite-blank-line-after-header t
  "If non-nil, put a blank line between the citation header and the button."
  :group 'gnus-cite
  :type 'boolean)

;;; Internal Variables:

(defvar gnus-cite-article nil)
(defvar gnus-cite-overlay-list nil)

(defvar gnus-cite-prefix-alist nil)
;; Alist of citation prefixes.
;; The cdr is a list of lines with that prefix.

(defvar gnus-cite-attribution-alist nil)
;; Alist of attribution lines.
;; The car is a line number.
;; The cdr is the prefix for the citation started by that line.

(defvar gnus-cite-loose-prefix-alist nil)
;; Alist of citation prefixes that have no matching attribution.
;; The cdr is a list of lines with that prefix.

(defvar gnus-cite-loose-attribution-alist nil)
;; Alist of attribution lines that have no matching citation.
;; Each member has the form (WROTE IN PREFIX TAG), where
;; WROTE: is the attribution line number
;; IN: is the line number of the previous line if part of the same attribution,
;; PREFIX: Is the citation prefix of the attribution line(s), and
;; TAG: Is a Supercite tag, if any.

(defvar gnus-cited-opened-text-button-line-format-alist
  `((?b (marker-position beg) ?d)
    (?e (marker-position end) ?d)
    (?n (count-lines beg end) ?d)
    (?l (- end beg) ?d)))
(defvar gnus-cited-opened-text-button-line-format-spec nil)
(defvar gnus-cited-closed-text-button-line-format-alist
  gnus-cited-opened-text-button-line-format-alist)
(defvar gnus-cited-closed-text-button-line-format-spec nil)


;;; Commands:

(defun gnus-article-highlight-citation (&optional force)
  "Highlight cited text.
Each citation in the article will be highlighted with a different face.
The faces are taken from `gnus-cite-face-list'.
Attribution lines are highlighted with the same face as the
corresponding citation merged with `gnus-cite-attribution-face'.

Text is considered cited if at least `gnus-cite-minimum-match-count'
lines matches `gnus-cite-prefix-regexp' with the same prefix.

Lines matching `gnus-cite-attribution-suffix' and perhaps
`gnus-cite-attribution-prefix' are considered attribution lines."
  (interactive (list 'force))
  (save-excursion
    (set-buffer gnus-article-buffer)
    (gnus-cite-parse-maybe force)
    (let ((buffer-read-only nil)
	  (alist gnus-cite-prefix-alist)
	  (faces gnus-cite-face-list)
	  (inhibit-point-motion-hooks t)
	  face entry prefix skip numbers number face-alist)
      ;; Loop through citation prefixes.
      (while alist
	(setq entry (car alist)
	      alist (cdr alist)
	      prefix (car entry)
	      numbers (cdr entry)
	      face (car faces)
	      faces (or (cdr faces) gnus-cite-face-list)
	      face-alist (cons (cons prefix face) face-alist))
	(while numbers
	  (setq number (car numbers)
		numbers (cdr numbers))
	  (and (not (assq number gnus-cite-attribution-alist))
	       (not (assq number gnus-cite-loose-attribution-alist))
	       (gnus-cite-add-face number prefix face))))
      ;; Loop through attribution lines.
      (setq alist gnus-cite-attribution-alist)
      (while alist
	(setq entry (car alist)
	      alist (cdr alist)
	      number (car entry)
	      prefix (cdr entry)
	      skip (gnus-cite-find-prefix number)
	      face (cdr (assoc prefix face-alist)))
	;; Add attribution button.
	(goto-char (point-min))
	(forward-line (1- number))
	(when (re-search-forward gnus-cite-attribution-suffix
				 (save-excursion (end-of-line 1) (point))
				 t)
	  (gnus-article-add-button (match-beginning 1) (match-end 1)
				   'gnus-cite-toggle prefix))
	;; Highlight attribution line.
	(gnus-cite-add-face number skip face)
	(gnus-cite-add-face number skip gnus-cite-attribution-face))
      ;; Loop through attribution lines.
      (setq alist gnus-cite-loose-attribution-alist)
      (while alist
	(setq entry (car alist)
	      alist (cdr alist)
	      number (car entry)
	      skip (gnus-cite-find-prefix number))
	(gnus-cite-add-face number skip gnus-cite-attribution-face)))))

(defun gnus-dissect-cited-text ()
  "Dissect the article buffer looking for cited text."
  (save-excursion
    (set-buffer gnus-article-buffer)
    (gnus-cite-parse-maybe nil t)
    (let ((alist gnus-cite-prefix-alist)
	  prefix numbers number marks m)
      ;; Loop through citation prefixes.
      (while alist
	(setq numbers (pop alist)
	      prefix (pop numbers))
	(while numbers
	  (setq number (pop numbers))
	  (goto-char (point-min))
	  (forward-line number)
	  (push (cons (point-marker) "") marks)
	  (while (and numbers
		      (= (1- number) (car numbers)))
	    (setq number (pop numbers)))
	  (goto-char (point-min))
	  (forward-line (1- number))
	  (push (cons (point-marker) prefix) marks)))
      ;; Skip to the beginning of the body.
      (article-goto-body)
      (push (cons (point-marker) "") marks)
      ;; Find the end of the body.
      (goto-char (point-max))
      (gnus-article-search-signature)
      (push (cons (point-marker) "") marks)
      ;; Sort the marks.
      (setq marks (sort marks 'car-less-than-car))
      (let ((omarks marks))
	(setq marks nil)
	(while (cdr omarks)
	  (if (= (caar omarks) (caadr omarks))
	      (progn
		(unless (equal (cdar omarks) "")
		  (push (car omarks) marks))
		(unless (equal (cdadr omarks) "")
		  (push (cadr omarks) marks))
		(unless (and (equal (cdar omarks) "")
			     (equal (cdadr omarks) "")
			     (not (cddr omarks)))
		  (setq omarks (cdr omarks))))
	    (push (car omarks) marks))
	  (setq omarks (cdr omarks)))
	(when (car omarks)
	  (push (car omarks) marks))
	(setq marks (setq m (nreverse marks)))
	(while (cddr m)
	  (if (and (equal (cdadr m) "")
		   (equal (cdar m) (cdaddr m))
		   (goto-char (caadr m))
		   (forward-line 1)
		   (= (point) (caaddr m)))
	      (setcdr m (cdddr m))
	    (setq m (cdr m))))
	marks))))

(defun gnus-article-fill-cited-article (&optional force width)
  "Do word wrapping in the current article.
If WIDTH (the numerical prefix), use that text width when filling."
  (interactive (list t current-prefix-arg))
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t)
	  (marks (gnus-dissect-cited-text))
	  (adaptive-fill-mode nil)
	  (filladapt-mode nil)
	  (fill-column (if width (prefix-numeric-value width) fill-column)))
      (save-restriction
	(while (cdr marks)
	  (narrow-to-region (caar marks) (caadr marks))
	  (let ((adaptive-fill-regexp
		 (concat "^" (regexp-quote (cdar marks)) " *"))
		(fill-prefix (cdar marks)))
	    (fill-region (point-min) (point-max)))
	  (set-marker (caar marks) nil)
	  (setq marks (cdr marks)))
	(when marks
	  (set-marker (caar marks) nil))
	;; All this information is now incorrect.
	(setq gnus-cite-prefix-alist nil
	      gnus-cite-attribution-alist nil
	      gnus-cite-loose-prefix-alist nil
	      gnus-cite-loose-attribution-alist nil
	      gnus-cite-article nil)))))

(defun gnus-article-hide-citation (&optional arg force)
  "Toggle hiding of all cited text except attribution lines.
See the documentation for `gnus-article-highlight-citation'.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (append (gnus-article-hidden-arg) (list 'force)))
  (gnus-set-format 'cited-opened-text-button t)
  (gnus-set-format 'cited-closed-text-button t)
  (save-excursion
    (set-buffer gnus-article-buffer)
      (let ((buffer-read-only nil)
	    marks
	    (inhibit-point-motion-hooks t)
	    (props (nconc (list 'article-type 'cite)
			  gnus-hidden-properties))
	    (point (point-min))
	    found beg end start)
	(while (setq point 
		     (text-property-any point (point-max) 
					'gnus-callback
					'gnus-article-toggle-cited-text))
	  (setq found t)
	  (goto-char point)
	  (gnus-article-toggle-cited-text
	   (get-text-property point 'gnus-data) arg)
	  (forward-line 1)
	  (setq point (point)))
	(unless found
	  (setq marks (gnus-dissect-cited-text))
	  (while marks
	    (setq beg nil
		  end nil)
	    (while (and marks (string= (cdar marks) ""))
	      (setq marks (cdr marks)))
	    (when marks
	      (setq beg (caar marks)))
	    (while (and marks (not (string= (cdar marks) "")))
	      (setq marks (cdr marks)))
	    (when marks
	    (setq end (caar marks)))
	    ;; Skip past lines we want to leave visible.
	    (when (and beg end gnus-cited-lines-visible)
	      (goto-char beg)
	      (forward-line (if (consp gnus-cited-lines-visible)
				(car gnus-cited-lines-visible)
			      gnus-cited-lines-visible))
	      (if (>= (point) end)
		  (setq beg nil)
		(setq beg (point-marker))
		(when (consp gnus-cited-lines-visible)
		  (goto-char end)
		  (forward-line (- (cdr gnus-cited-lines-visible)))
		  (if (<= (point) beg)
		      (setq beg nil)
		  (setq end (point-marker))))))
	    (when (and beg end)
	      ;; We use markers for the end-points to facilitate later
	      ;; wrapping and mangling of text.
	      (setq beg (set-marker (make-marker) beg)
		    end (set-marker (make-marker) end))
	      (gnus-add-text-properties-when 'article-type nil beg end props)
	      (goto-char beg)
	      (when (and gnus-cite-blank-line-after-header
			 (not (save-excursion (search-backward "\n\n" nil t))))
		(insert "\n"))
	      (put-text-property
	       (setq start (point-marker))
	       (progn
	       (gnus-article-add-button
		(point)
		(progn (eval gnus-cited-closed-text-button-line-format-spec)
		       (point))
		`gnus-article-toggle-cited-text
		(list (cons beg end) start))
	       (point))
	       'article-type 'annotation)
	      (set-marker beg (point))))))))

(defun gnus-article-toggle-cited-text (args &optional arg)
  "Toggle hiding the text in REGION.
ARG can be nil or a number.  Positive means hide, negative
means show, nil means toggle."
  (let* ((region (car args))
	 (beg (car region))
	 (end (cdr region))
	 (start (cadr args))
	 (hidden
	  (text-property-any beg (1- end) 'article-type 'cite))
	 (inhibit-point-motion-hooks t)
	 buffer-read-only)
    (when (or (null arg)
	      (zerop arg)
	      (and (> arg 0) (not hidden))
	      (and (< arg 0) hidden))
      (if hidden
	  (gnus-remove-text-properties-when
	   'article-type 'cite beg end 
	   (cons 'article-type (cons 'cite
				     gnus-hidden-properties)))
	(gnus-add-text-properties-when
	 'article-type nil beg end 
	 (cons 'article-type (cons 'cite
				   gnus-hidden-properties))))
      (save-excursion
	(goto-char start)
	(gnus-delete-line)
	(put-text-property
	 (point)
	 (progn
	   (gnus-article-add-button
	    (point)
	    (progn (eval
		    (if hidden
			gnus-cited-opened-text-button-line-format-spec
		      gnus-cited-closed-text-button-line-format-spec))
		   (point))
	    `gnus-article-toggle-cited-text
	    args)
	   (point))
	 'article-type 'annotation)))))

(defun gnus-article-hide-citation-maybe (&optional arg force)
  "Toggle hiding of cited text that has an attribution line.
If given a negative prefix, always show; if given a positive prefix,
always hide.
This will do nothing unless at least `gnus-cite-hide-percentage'
percent and at least `gnus-cite-hide-absolute' lines of the body is
cited text with attributions.  When called interactively, these two
variables are ignored.
See also the documentation for `gnus-article-highlight-citation'."
  (interactive (append (gnus-article-hidden-arg) '(force)))
  (unless (gnus-article-check-hidden-text 'cite arg)
    (save-excursion
      (set-buffer gnus-article-buffer)
      (gnus-cite-parse-maybe force)
      (article-goto-body)
      (let ((start (point))
	    (atts gnus-cite-attribution-alist)
	    (buffer-read-only nil)
	    (inhibit-point-motion-hooks t)
	    (hidden 0)
	    total)
	(goto-char (point-max))
	(gnus-article-search-signature)
	(setq total (count-lines start (point)))
	(while atts
	  (setq hidden (+ hidden (length (cdr (assoc (cdar atts)
						     gnus-cite-prefix-alist))))
		atts (cdr atts)))
	(when (or force
		  (and (> (* 100 hidden) (* gnus-cite-hide-percentage total))
		       (> hidden gnus-cite-hide-absolute)))
	  (setq atts gnus-cite-attribution-alist)
	  (while atts
	    (setq total (cdr (assoc (cdar atts) gnus-cite-prefix-alist))
		  atts (cdr atts))
	    (while total
	      (setq hidden (car total)
		    total (cdr total))
	      (goto-char (point-min))
	      (forward-line (1- hidden))
	      (unless (assq hidden gnus-cite-attribution-alist)
		(gnus-add-text-properties
		 (point) (progn (forward-line 1) (point))
		 (nconc (list 'article-type 'cite)
			gnus-hidden-properties))))))))))

(defun gnus-article-hide-citation-in-followups ()
  "Hide cited text in non-root articles."
  (interactive)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (let ((article (cdr gnus-article-current)))
      (unless (save-excursion
		(set-buffer gnus-summary-buffer)
		(gnus-article-displayed-root-p article))
	(gnus-article-hide-citation)))))

;;; Internal functions:

(defun gnus-cite-parse-maybe (&optional force no-overlay)
  "Always parse the buffer."
  (gnus-cite-localize)
  ;;Reset parser information.
  (setq gnus-cite-prefix-alist nil
	gnus-cite-attribution-alist nil
	gnus-cite-loose-prefix-alist nil
	gnus-cite-loose-attribution-alist nil)
  (unless no-overlay
    (gnus-cite-delete-overlays))
  ;; Parse if not too large.
  (if (and gnus-cite-parse-max-size
	   (> (buffer-size) gnus-cite-parse-max-size))
      ()
    (setq gnus-cite-article (cons (car gnus-article-current)
				  (cdr gnus-article-current)))
    (gnus-cite-parse-wrapper)))

(defun gnus-cite-delete-overlays ()
  (dolist (overlay gnus-cite-overlay-list)
    (when (or (not (gnus-overlay-end overlay))
	      (and (>= (gnus-overlay-end overlay) (point-min))
		   (<= (gnus-overlay-end overlay) (point-max))))
      (setq gnus-cite-overlay-list (delete overlay gnus-cite-overlay-list))
      (gnus-delete-overlay overlay))))

(defun gnus-cite-parse-wrapper ()
  ;; Wrap chopped gnus-cite-parse.
  (article-goto-body)
  (let ((inhibit-point-motion-hooks t))
    (save-excursion
      (gnus-cite-parse-attributions))
    (save-excursion
      (gnus-cite-parse))
    (save-excursion
      (gnus-cite-connect-attributions))))

(defun gnus-cite-parse ()
  ;; Parse and connect citation prefixes and attribution lines.

  ;; Parse current buffer searching for citation prefixes.
  (let ((line (1+ (count-lines (point-min) (point))))
	(case-fold-search t)
	(max (save-excursion
	       (goto-char (point-max))
	       (gnus-article-search-signature)
	       (point)))
	alist entry start begin end numbers prefix)
    ;; Get all potential prefixes in `alist'.
    (while (< (point) max)
      ;; Each line.
      (setq begin (point)
	    end (progn (beginning-of-line 2) (point))
	    start end)
      (goto-char begin)
      ;; Ignore standard Supercite attribution prefix.
      (when (looking-at gnus-supercite-regexp)
	(if (match-end 1)
	    (setq end (1+ (match-end 1)))
	  (setq end (1+ begin))))
      ;; Ignore very long prefixes.
      (when (> end (+ (point) gnus-cite-max-prefix))
	(setq end (+ (point) gnus-cite-max-prefix)))
      (while (re-search-forward gnus-cite-prefix-regexp (1- end) t)
	;; Each prefix.
	(setq end (match-end 0)
	      prefix (buffer-substring begin end))
	(gnus-set-text-properties 0 (length prefix) nil prefix)
	(setq entry (assoc prefix alist))
	(if entry
	    (setcdr entry (cons line (cdr entry)))
	  (push (list prefix line) alist))
	(goto-char begin))
      (goto-char start)
      (setq line (1+ line)))
    ;; We got all the potential prefixes.  Now create
    ;; `gnus-cite-prefix-alist' containing the oldest prefix for each
    ;; line that appears at least gnus-cite-minimum-match-count
    ;; times.  First sort them by length.  Longer is older.
    (setq alist (sort alist (lambda (a b)
			      (> (length (car a)) (length (car b))))))
    (while alist
      (setq entry (car alist)
	    prefix (car entry)
	    numbers (cdr entry)
	    alist (cdr alist))
      (cond ((null numbers)
	     ;; No lines with this prefix that wasn't also part of
	     ;; a longer prefix.
	     )
	    ((< (length numbers) gnus-cite-minimum-match-count)
	     ;; Too few lines with this prefix.  We keep it a bit
	     ;; longer in case it is an exact match for an attribution
	     ;; line, but we don't remove the line from other
	     ;; prefixes.
	     (push entry gnus-cite-prefix-alist))
	    (t
	     (push entry
		   gnus-cite-prefix-alist)
	     ;; Remove articles from other prefixes.
	     (let ((loop alist)
		   current)
	       (while loop
		 (setq current (car loop)
		       loop (cdr loop))
		 (setcdr current
			 (gnus-set-difference (cdr current) numbers)))))))))

(defun gnus-cite-parse-attributions ()
  (let (al-alist)
    ;; Parse attributions
    (while (re-search-forward gnus-cite-attribution-suffix (point-max) t)
      (let* ((start (match-beginning 0))
	     (end (match-end 0))
	     (wrote (count-lines (point-min) end))
	     (prefix (gnus-cite-find-prefix wrote))
	     ;; Check previous line for an attribution leader.
	     (tag (progn
		    (beginning-of-line 1)
		    (when (looking-at gnus-supercite-secondary-regexp)
		      (buffer-substring (match-beginning 1)
					(match-end 1)))))
	     (in (progn
		   (goto-char start)
		   (and (re-search-backward gnus-cite-attribution-prefix
					    (save-excursion
					      (beginning-of-line 0)
					      (point))
					    t)
			(not (re-search-forward gnus-cite-attribution-suffix
						start t))
			(count-lines (point-min) (1+ (point)))))))
	(when (eq wrote in)
	  (setq in nil))
	(goto-char end)
	;; don't add duplicates
	(let ((al (buffer-substring (save-excursion (beginning-of-line 0)
						    (1+ (point)))
				    end)))
	  (if (not (assoc al al-alist))
	      (progn
		(push (list wrote in prefix tag)
		      gnus-cite-loose-attribution-alist)
		(push (cons al t) al-alist))))))))

(defun gnus-cite-connect-attributions ()
  ;; Connect attributions to citations

  ;; No citations have been connected to attribution lines yet.
  (setq gnus-cite-loose-prefix-alist (append gnus-cite-prefix-alist nil))

  ;; Parse current buffer searching for attribution lines.
  ;; Find exact supercite citations.
  (gnus-cite-match-attributions 'small nil
				(lambda (prefix tag)
				  (when tag
				    (concat "\\`"
					    (regexp-quote prefix) "[ \t]*"
					    (regexp-quote tag) ">"))))
  ;; Find loose supercite citations after attributions.
  (gnus-cite-match-attributions 'small t
				(lambda (prefix tag)
				  (when tag
				    (concat "\\<"
					    (regexp-quote tag)
					    "\\>"))))
  ;; Find loose supercite citations anywhere.
  (gnus-cite-match-attributions 'small nil
				(lambda (prefix tag)
				  (when tag
				    (concat "\\<"
					    (regexp-quote tag)
					    "\\>"))))
  ;; Find nested citations after attributions.
  (gnus-cite-match-attributions 'small-if-unique t
				(lambda (prefix tag)
				  (concat "\\`" (regexp-quote prefix) ".+")))
  ;; Find nested citations anywhere.
  (gnus-cite-match-attributions 'small nil
				(lambda (prefix tag)
				  (concat "\\`" (regexp-quote prefix) ".+")))
  ;; Remove loose prefixes with too few lines.
  (let ((alist gnus-cite-loose-prefix-alist)
	entry)
    (while alist
      (setq entry (car alist)
	    alist (cdr alist))
      (when (< (length (cdr entry)) gnus-cite-minimum-match-count)
	(setq gnus-cite-prefix-alist
	      (delq entry gnus-cite-prefix-alist)
	      gnus-cite-loose-prefix-alist
	      (delq entry gnus-cite-loose-prefix-alist)))))
  ;; Find flat attributions.
  (gnus-cite-match-attributions 'first t nil)
  ;; Find any attributions (are we getting desperate yet?).
  (gnus-cite-match-attributions 'first nil nil))

(defun gnus-cite-match-attributions (sort after fun)
  ;; Match all loose attributions and citations (SORT AFTER FUN) .
  ;;
  ;; If SORT is `small', the citation with the shortest prefix will be
  ;; used, if it is `first' the first prefix will be used, if it is
  ;; `small-if-unique' the shortest prefix will be used if the
  ;; attribution line does not share its own prefix with other
  ;; loose attribution lines, otherwise the first prefix will be used.
  ;;
  ;; If AFTER is non-nil, only citations after the attribution line
  ;; will be considered.
  ;;
  ;; If FUN is non-nil, it will be called with the arguments (WROTE
  ;; PREFIX TAG) and expected to return a regular expression.  Only
  ;; citations whose prefix matches the regular expression will be
  ;; considered.
  ;;
  ;; WROTE is the attribution line number.
  ;; PREFIX is the attribution line prefix.
  ;; TAG is the Supercite tag on the attribution line.
  (let ((atts gnus-cite-loose-attribution-alist)
	(case-fold-search t)
	att wrote in prefix tag regexp limit smallest best size)
    (while atts
      (setq att (car atts)
	    atts (cdr atts)
	    wrote (nth 0 att)
	    in (nth 1 att)
	    prefix (nth 2 att)
	    tag (nth 3 att)
	    regexp (if fun (funcall fun prefix tag) "")
	    size (cond ((eq sort 'small) t)
		       ((eq sort 'first) nil)
		       (t (< (length (gnus-cite-find-loose prefix)) 2)))
	    limit (if after wrote -1)
	    smallest 1000000
	    best nil)
      (let ((cites gnus-cite-loose-prefix-alist)
	    cite candidate numbers first compare)
	(while cites
	  (setq cite (car cites)
		cites (cdr cites)
		candidate (car cite)
		numbers (cdr cite)
		first (apply 'min numbers)
		compare (if size (length candidate) first))
	  (and (> first limit)
	       regexp
	       (string-match regexp candidate)
	       (< compare smallest)
	       (setq best cite
		     smallest compare))))
      (if (null best)
	  ()
	(setq gnus-cite-loose-attribution-alist
	      (delq att gnus-cite-loose-attribution-alist))
	(push (cons wrote (car best)) gnus-cite-attribution-alist)
	(when in
	  (push (cons in (car best)) gnus-cite-attribution-alist))
	(when (memq best gnus-cite-loose-prefix-alist)
	  (let ((loop gnus-cite-prefix-alist)
		(numbers (cdr best))
		current)
	    (setq gnus-cite-loose-prefix-alist
		  (delq best gnus-cite-loose-prefix-alist))
	    (while loop
	      (setq current (car loop)
		    loop (cdr loop))
	      (if (eq current best)
		  ()
		(setcdr current (gnus-set-difference (cdr current) numbers))
		(when (null (cdr current))
		  (setq gnus-cite-loose-prefix-alist
			(delq current gnus-cite-loose-prefix-alist)
			atts (delq current atts)))))))))))

(defun gnus-cite-find-loose (prefix)
  ;; Return a list of loose attribution lines prefixed by PREFIX.
  (let* ((atts gnus-cite-loose-attribution-alist)
	 att line lines)
    (while atts
      (setq att (car atts)
	    line (car att)
	    atts (cdr atts))
      (when (string-equal (gnus-cite-find-prefix line) prefix)
	(push line lines)))
    lines))

(defun gnus-cite-add-face (number prefix face)
  ;; At line NUMBER, ignore PREFIX and add FACE to the rest of the line.
  (when face
    (let ((inhibit-point-motion-hooks t)
	  from to overlay)
      (goto-char (point-min))
      (when (zerop (forward-line (1- number)))
	(forward-char (length prefix))
	(skip-chars-forward " \t")
	(setq from (point))
	(end-of-line 1)
	(skip-chars-backward " \t")
	(setq to (point))
	(when (< from to)
	  (push (setq overlay (gnus-make-overlay from to))
		gnus-cite-overlay-list)
	  (gnus-overlay-put overlay 'face face))))))

(defun gnus-cite-toggle (prefix)
  (save-excursion
    (set-buffer gnus-article-buffer)
    (gnus-cite-parse-maybe nil t)
    (let ((buffer-read-only nil)
	  (numbers (cdr (assoc prefix gnus-cite-prefix-alist)))
	  (inhibit-point-motion-hooks t)
	  number)
      (while numbers
	(setq number (car numbers)
	      numbers (cdr numbers))
	(goto-char (point-min))
	(forward-line (1- number))
	(cond ((get-text-property (point) 'invisible)
	       (remove-text-properties (point) (progn (forward-line 1) (point))
				       gnus-hidden-properties))
	      ((assq number gnus-cite-attribution-alist))
	      (t
	       (gnus-add-text-properties
		(point) (progn (forward-line 1) (point))
		(nconc (list 'article-type 'cite)
		       gnus-hidden-properties))))))))

(defun gnus-cite-find-prefix (line)
  ;; Return citation prefix for LINE.
  (let ((alist gnus-cite-prefix-alist)
	(prefix "")
	entry)
    (while alist
      (setq entry (car alist)
	    alist (cdr alist))
      (when (memq line (cdr entry))
	(setq prefix (car entry))))
    prefix))

(defun gnus-cite-localize ()
  "Make the citation variables local to the article buffer."
  (let ((vars '(gnus-cite-article
		gnus-cite-overlay-list gnus-cite-prefix-alist
		gnus-cite-attribution-alist gnus-cite-loose-prefix-alist
		gnus-cite-loose-attribution-alist)))
    (while vars
      (make-local-variable (pop vars)))))

(gnus-ems-redefine)

(provide 'gnus-cite)

;; Local Variables:
;; coding: iso-8859-1
;; End:

;;; gnus-cite.el ends here
